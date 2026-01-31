#include "qemu/osdep.h"

#include "qapi/error.h"
#include "hw/core/boards.h"
#include "hw/core/loader.h"
#include "system/memory.h"
#include "qemu/error-report.h"
#include "qemu/units.h"
#include "qemu/bswap.h"
#include "qemu/log.h"
#include "target/slow32/cpu.h"
#include "target/slow32/mmio.h"
#include "qom/object.h"

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
    uint32_t stack_end;
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
#define S32X_FLAG_W_XOR_X    0x0001u
#define S32X_FLAG_MMIO       0x0080u

#define S32_SEC_SYMTAB       0x0021u
#define S32_SEC_STRTAB       0x0022u

/* Symbol entry (16 bytes), matches s32o_symbol_t in s32_formats.h */
typedef struct QEMU_PACKED Slow32Symbol {
    uint32_t name_offset;
    uint32_t value;
    uint16_t section;
    uint8_t  type;
    uint8_t  binding;
    uint32_t size;
} Slow32Symbol;

/*
 * Scan the .s32x section table for SYMTAB/STRTAB sections, parse the
 * symbol table, and populate env->intrinsic_* fields for known library
 * functions.  This enables the TCG translator to emit native helper
 * calls instead of translating the guest implementation.
 */
static void slow32_load_intrinsics(Slow32CPU *cpu, FILE *f,
                                   const Slow32XHeader *hdr)
{
    CPUSlow32State *env = &cpu->env;
    uint32_t symtab_offset = 0, symtab_size = 0;
    uint32_t sym_strtab_offset = 0, sym_strtab_size = 0;

    if (fseek(f, hdr->sec_offset, SEEK_SET) != 0) {
        return;
    }

    for (uint32_t i = 0; i < hdr->nsections; i++) {
        Slow32XSection sec;
        if (fread(&sec, sizeof(sec), 1, f) != 1) {
            return;
        }
        if (sec.type == S32_SEC_SYMTAB) {
            symtab_offset = sec.offset;
            symtab_size = sec.size;
        } else if (sec.type == S32_SEC_STRTAB) {
            /* Distinguish symbol STRTAB from section name STRTAB */
            if (sec.vaddr == 0 && sec.mem_size == 0 &&
                sec.offset != hdr->str_offset) {
                sym_strtab_offset = sec.offset;
                sym_strtab_size = sec.size;
            }
        }
    }

    if (symtab_offset == 0 || sym_strtab_offset == 0 || symtab_size == 0) {
        return;
    }

    /* Read symbol string table */
    g_autofree char *sym_str = g_malloc(sym_strtab_size);
    if (fseek(f, sym_strtab_offset, SEEK_SET) != 0 ||
        fread(sym_str, 1, sym_strtab_size, f) != sym_strtab_size) {
        return;
    }

    /* Read symbol entries */
    uint32_t nsyms = symtab_size / sizeof(Slow32Symbol);
    g_autofree Slow32Symbol *syms = g_malloc(symtab_size);
    if (fseek(f, symtab_offset, SEEK_SET) != 0 ||
        fread(syms, sizeof(Slow32Symbol), nsyms, f) != nsyms) {
        return;
    }

    /* Name variants to match (same as the DBT) */
    static const char *memcpy_names[] = {
        "memcpy", "llvm.memcpy.p0.p0.i32", "llvm.memcpy.p0.p0.i64", NULL
    };
    static const char *memset_names[] = {
        "memset", "llvm.memset.p0.i32", "llvm.memset.p0.i64", NULL
    };
    static const char *memmove_names[] = { "memmove", NULL };
    static const char *strlen_names[] = { "strlen", NULL };
    static const char *memswap_names[] = { "memswap", NULL };

    struct {
        uint32_t *dest;
        const char **names;
    } lookups[] = {
        { &env->intrinsic_memcpy,  memcpy_names },
        { &env->intrinsic_memset,  memset_names },
        { &env->intrinsic_memmove, memmove_names },
        { &env->intrinsic_strlen,  strlen_names },
        { &env->intrinsic_memswap, memswap_names },
    };

    for (size_t k = 0; k < G_N_ELEMENTS(lookups); k++) {
        for (const char **n = lookups[k].names; *n; n++) {
            for (uint32_t i = 0; i < nsyms; i++) {
                if (syms[i].name_offset < sym_strtab_size &&
                    strcmp(&sym_str[syms[i].name_offset], *n) == 0 &&
                    syms[i].value != 0) {
                    *lookups[k].dest = syms[i].value;
                    goto next_lookup;
                }
            }
        }
next_lookup:;
    }

    if (env->intrinsic_memcpy || env->intrinsic_memset ||
        env->intrinsic_memmove || env->intrinsic_strlen ||
        env->intrinsic_memswap) {
        qemu_log("slow32: native intrinsics detected:\n");
        if (env->intrinsic_memcpy) {
            qemu_log("  memcpy:  0x%08x\n", env->intrinsic_memcpy);
        }
        if (env->intrinsic_memset) {
            qemu_log("  memset:  0x%08x\n", env->intrinsic_memset);
        }
        if (env->intrinsic_memmove) {
            qemu_log("  memmove: 0x%08x\n", env->intrinsic_memmove);
        }
        if (env->intrinsic_strlen) {
            qemu_log("  strlen:  0x%08x\n", env->intrinsic_strlen);
        }
        if (env->intrinsic_memswap) {
            qemu_log("  memswap: 0x%08x\n", env->intrinsic_memswap);
        }
    }
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
    env->stack_end = hdr.stack_end;
    env->regs[SLOW32_REG_SP] = env->stack_top;
    env->code_limit = hdr.code_limit;
    env->rodata_limit = hdr.rodata_limit;
    env->data_limit = hdr.data_limit;
    env->heap_base = hdr.heap_base ? hdr.heap_base : hdr.data_limit;
    env->mem_size = hdr.mem_size ? hdr.mem_size : machine->ram_size;
    env->layout_defined = true;
    env->mmio_base = (hdr.flags & S32X_FLAG_MMIO) ? hdr.mmio_base : 0;
    slow32_mmio_reconfigure(cpu);

    return true;
}

static void slow32_parse_cmdline(MachineState *machine, Slow32CPU *cpu)
{
    const char *cmdline = machine->kernel_cmdline;
    const char *kernel = machine->kernel_filename;

    /*
     * Build argv[] for the guest. The kernel filename becomes argv[0],
     * and the -append arguments become argv[1..n].
     */
    int extra_argc = 0;
    char **extra_argv = NULL;
    GError *gerr = NULL;

    if (cmdline && *cmdline) {
        if (!g_shell_parse_argv(cmdline, &extra_argc, &extra_argv, &gerr)) {
            warn_report("Failed to parse kernel cmdline: %s", gerr->message);
            g_error_free(gerr);
            extra_argc = 0;
            extra_argv = NULL;
        }
    }

    /* Build combined argv: [kernel_filename, extra_argv...] */
    int argc = 1 + extra_argc;
    char **argv = g_new0(char *, argc + 1);

    argv[0] = g_strdup(kernel ? kernel : "");
    for (int i = 0; i < extra_argc; i++) {
        argv[1 + i] = g_strdup(extra_argv[i]);
    }
    argv[argc] = NULL;

    slow32_mmio_set_args(cpu, argc, argv);

    g_strfreev(argv);
    g_strfreev(extra_argv);
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

    if (magic != cpu_to_le32(S32X_MAGIC)) {
        fclose(f);
        error_setg(errp, "Invalid .s32x magic: 0x%08x (expected 0x%08x)",
                   le32_to_cpu(magic), S32X_MAGIC);
        return false;
    }

    fseek(f, 0, SEEK_SET);
    bool ok = slow32_load_s32x(sms, machine, cpu, f, errp);

    if (ok) {
        /* Re-read header for intrinsic symbol scanning */
        Slow32XHeader hdr;
        fseek(f, 0, SEEK_SET);
        if (fread(&hdr, sizeof(hdr), 1, f) == 1) {
            slow32_load_intrinsics(cpu, f, &hdr);
        }
    }

    fclose(f);

    if (ok) {
        slow32_parse_cmdline(machine, cpu);
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

static void slow32_machine_instance_init(Object *obj)
{
}

static const TypeInfo slow32_machine_type = {
    .name = TYPE_SLOW32_MACHINE,
    .parent = TYPE_MACHINE,
    .instance_size = sizeof(Slow32MachineState),
    .instance_init = slow32_machine_instance_init,
    .class_init = slow32_machine_class_init,
};

static void slow32_machine_register_types(void)
{
    type_register_static(&slow32_machine_type);
}

type_init(slow32_machine_register_types);
