#include "qemu/osdep.h"

#include "qapi/error.h"
#include "hw/boards.h"
#include "hw/loader.h"
#include "system/memory.h"
#include "qemu/error-report.h"
#include "qemu/units.h"
#include "qemu/bswap.h"
#include "target/slow32/cpu.h"
#include "target/slow32/mmio.h"

#define TYPE_SLOW32_MACHINE "slow32-tcg-machine"
OBJECT_DECLARE_SIMPLE_TYPE(Slow32MachineState, SLOW32_MACHINE)

typedef struct Slow32MachineState {
    MachineState parent_obj;
    MemoryRegion ram;
} Slow32MachineState;


typedef struct QEMU_PACKED Slow32XHeader {
    uint32_t magic;
    uint16_t version;
    uint8_t endian;
    uint8_t machine;
    uint32_t entry;
    uint32_t nsections;
    uint32_t sec_offset;
    uint32_t str_offset;
    uint32_t str_size;
    uint32_t flags;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t stack_base;
    uint32_t mem_size;
    uint32_t heap_base;
    uint32_t checksum;
    uint32_t mmio_base;
} Slow32XHeader;

typedef struct QEMU_PACKED Slow32XSection {
    uint32_t name_offset;
    uint32_t type;
    uint32_t vaddr;
    uint32_t offset;
    uint32_t size;
    uint32_t mem_size;
    uint32_t flags;
} Slow32XSection;

#define S32X_MAGIC 0x53333258
#define S32_MACHINE_SLOW32 0x32

static bool slow32_load_flat(MachineState *machine, Slow32CPU *cpu,
                             Error **errp)
{
    int64_t sz = load_image_targphys(machine->kernel_filename, 0,
                                     machine->ram_size, errp);
    if (sz <= 0) {
        error_setg(errp, "Failed to load flat image '%s'",
                   machine->kernel_filename);
        return false;
    }

    CPUSlow32State *env = &cpu->env;
    env->pc = 0;
    env->next_pc = 0;
    env->regs[SLOW32_REG_SP] = SLOW32_DEFAULT_STACK_TOP;
    env->stack_top = SLOW32_DEFAULT_STACK_TOP;
    env->data_limit = machine->ram_size;
    env->heap_base = 0;
    env->mem_size = machine->ram_size;
    slow32_mmio_reconfigure(cpu);
    return true;
}

static bool slow32_load_s32x(Slow32MachineState *sms, MachineState *machine,
                             Slow32CPU *cpu, FILE *f, Error **errp)
{
    Slow32XHeader hdr;
    if (fread(&hdr, sizeof(hdr), 1, f) != 1) {
        error_setg(errp, "Unable to read .s32x header");
        return false;
    }

    if (hdr.version != 1 || hdr.machine != S32_MACHINE_SLOW32) {
        error_setg(errp, "Unsupported .s32x header (version=%u machine=0x%02x)",
                   hdr.version, hdr.machine);
        return false;
    }

    if (hdr.mem_size == 0 || hdr.mem_size > machine->ram_size) {
        error_setg(errp,
                   "Executable requests %u bytes but machine has %zu bytes",
                   hdr.mem_size, (size_t)machine->ram_size);
        return false;
    }

    uint8_t *ram = memory_region_get_ram_ptr(&sms->ram);
    memset(ram, 0, machine->ram_size);

    g_autofree char *strtab = NULL;
    if (hdr.str_size) {
        strtab = g_malloc(hdr.str_size);
        if (fseek(f, hdr.str_offset, SEEK_SET) != 0 ||
            fread(strtab, 1, hdr.str_size, f) != hdr.str_size) {
            error_setg(errp, "Unable to read .s32x string table");
            return false;
        }
    }

    if (fseek(f, hdr.sec_offset, SEEK_SET) != 0) {
        error_setg(errp, "Unable to seek to .s32x section table");
        return false;
    }

    for (uint32_t i = 0; i < hdr.nsections; ++i) {
        Slow32XSection sec;
        if (fread(&sec, sizeof(sec), 1, f) != 1) {
            error_setg(errp, "Unable to read .s32x section %u", i);
            return false;
        }

        if (sec.mem_size == 0) {
            continue;
        }

        if (sec.vaddr > hdr.mem_size ||
            sec.mem_size > hdr.mem_size - sec.vaddr ||
            sec.mem_size < sec.size) {
            error_setg(errp, "Section %u exceeds memory layout", i);
            return false;
        }

        long table_pos = ftell(f);
        if (sec.size && sec.offset) {
            if (fseek(f, sec.offset, SEEK_SET) != 0) {
                error_setg(errp, "Unable to seek to section %u payload", i);
                return false;
            }
            uint8_t *dest = ram + sec.vaddr;
            if (fread(dest, 1, sec.size, f) != sec.size) {
                error_setg(errp, "Unable to read section %u payload", i);
                return false;
            }
        }
        if (sec.mem_size > sec.size) {
            memset(ram + sec.vaddr + sec.size, 0, sec.mem_size - sec.size);
        }
        if (table_pos >= 0 && fseek(f, table_pos, SEEK_SET) != 0) {
            error_setg(errp, "Unable to resume section table walk");
            return false;
        }
    }

    CPUSlow32State *env = &cpu->env;
    env->pc = hdr.entry;
    env->next_pc = hdr.entry;
    env->stack_top = hdr.stack_base ? hdr.stack_base : SLOW32_DEFAULT_STACK_TOP;
    env->regs[SLOW32_REG_SP] = env->stack_top;
    env->mmio_base = hdr.mmio_base;
    env->data_limit = hdr.data_limit;
    env->heap_base = hdr.heap_base ? hdr.heap_base : hdr.data_limit;
    env->mem_size = hdr.mem_size ? hdr.mem_size : machine->ram_size;
    slow32_mmio_reconfigure(cpu);

    return true;
}

static bool slow32_load_kernel(Slow32MachineState *sms, MachineState *machine,
                               Slow32CPU *cpu, Error **errp)
{
    if (!machine->kernel_filename) {
        return true;
    }

    FILE *f = fopen(machine->kernel_filename, "rb");
    if (!f) {
        error_setg(errp, "Unable to open kernel image '%s'",
                   machine->kernel_filename);
        return false;
    }

    uint32_t magic;
    if (fread(&magic, sizeof(magic), 1, f) != 1) {
        fclose(f);
        error_setg(errp, "Unable to read kernel header");
        return false;
    }
    fseek(f, 0, SEEK_SET);

    bool ok;
    if (magic == cpu_to_le32(S32X_MAGIC)) {
        ok = slow32_load_s32x(sms, machine, cpu, f, errp);
        fclose(f);
    } else {
        fclose(f);
        ok = slow32_load_flat(machine, cpu, errp);
    }

    return ok;
}

static void slow32_machine_init(MachineState *machine)
{
    Slow32MachineState *sms = SLOW32_MACHINE(machine);
    MemoryRegion *system_memory = get_system_memory();
    Error *err = NULL;

    memory_region_init_ram(&sms->ram, NULL,
                           "slow32.ram", machine->ram_size, &error_fatal);
    memory_region_add_subregion(system_memory, 0, &sms->ram);

    if (!machine->cpu_type) {
        machine->cpu_type = TYPE_SLOW32_CPU;
    }
    CPUState *cs = cpu_create(machine->cpu_type);
    Slow32CPU *cpu = SLOW32_CPU(cs);
    cpu->env.run_start_us = g_get_monotonic_time();

    if (!slow32_load_kernel(sms, machine, cpu, &err)) {
        error_report_err(err);
        exit(EXIT_FAILURE);
    }
}

static void slow32_machine_class_init(ObjectClass *oc, const void *data)
{
    MachineClass *mc = MACHINE_CLASS(oc);

    mc->desc = "Minimal Slow32 TCG reference machine";
    mc->init = slow32_machine_init;
    mc->default_cpu_type = TYPE_SLOW32_CPU;
    mc->default_ram_size = 256 * MiB;
    mc->ignore_memory_transaction_failures = true;
}

static const TypeInfo slow32_machine_type = {
    .name = TYPE_SLOW32_MACHINE,
    .parent = TYPE_MACHINE,
    .instance_size = sizeof(Slow32MachineState),
    .class_init = slow32_machine_class_init,
};

static void slow32_machine_register_types(void)
{
    type_register_static(&slow32_machine_type);
}

type_init(slow32_machine_register_types);
