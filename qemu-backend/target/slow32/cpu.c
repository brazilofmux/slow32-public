#include "qemu/osdep.h"

#include <inttypes.h>

#include "qapi/error.h"
#include "cpu.h"
#include "accel/tcg/cpu-ops.h"
#include "hw/core/cpu.h"
#include "hw/core/sysemu-cpu-ops.h"
#include "exec/cputlb.h"
#include "exec/target_page.h"
#include "exec/translation-block.h"
#include "exec/gdbstub.h"
#include "gdbstub/helpers.h"
#include "tcg/tcg.h"
#include "qemu/log.h"
#include "mmio.h"

static void slow32_cpu_set_pc(CPUState *cs, vaddr value)
{
    CPUSlow32State *env = cpu_env(cs);

    env->pc = value;
    env->next_pc = value;
}

static vaddr slow32_cpu_get_pc(CPUState *cs)
{
    return cpu_env(cs)->pc;
}

static TCGTBCPUState slow32_get_tb_cpu_state(CPUState *cs)
{
    CPUSlow32State *env = cpu_env(cs);

    return (TCGTBCPUState){
        .pc = env->pc,
        .flags = 0,
        .cflags = 0,
        .cs_base = 0,
    };
}

static void slow32_cpu_synchronize_from_tb(CPUState *cs,
                                           const TranslationBlock *tb)
{
    slow32_cpu_set_pc(cs, tb->pc);
}

static void slow32_restore_state_to_opc(CPUState *cs,
                                        const TranslationBlock *tb,
                                        const uint64_t *data)
{
    slow32_cpu_set_pc(cs, tb->pc);
}

static bool slow32_cpu_has_work(CPUState *cs)
{
    return !cpu_env(cs)->halted;
}

static int slow32_cpu_mmu_index(CPUState *cs, bool ifetch)
{
    return 0;
}

static hwaddr slow32_cpu_get_phys_page_debug(CPUState *cs, vaddr addr)
{
    return addr;
}

static bool slow32_cpu_tlb_fill(CPUState *cs, vaddr addr, int size,
                                MMUAccessType access_type, int mmu_idx,
                                bool probe, uintptr_t retaddr)
{
    CPUSlow32State *env = cpu_env(cs);
    Slow32CPU *cpu = SLOW32_CPU(cs);
    vaddr page = addr & TARGET_PAGE_MASK;
    uint32_t mem_limit = env->mem_size;
    bool have_mem_limit = mem_limit != 0;
    int prot = 0;

    if (env->layout_defined && env->code_limit &&
        page < env->code_limit) {
        prot = PAGE_READ | PAGE_EXEC;
    } else if (env->layout_defined && env->rodata_limit &&
               page < env->rodata_limit) {
        prot = PAGE_READ;
    } else {
        if (have_mem_limit && page >= mem_limit) {
            goto fault;
        }
        prot = PAGE_READ | PAGE_WRITE;
        if (!env->layout_defined) {
            prot |= PAGE_EXEC;
        }
    }

    if (env->mmio_base) {
        uint32_t mmio_start = env->mmio_base & TARGET_PAGE_MASK;
        uint64_t mmio_end =
            (uint64_t)env->mmio_base + S32_MMIO_WINDOW_SIZE;
        if (page >= mmio_start && (uint64_t)page < mmio_end) {
            prot = PAGE_READ | PAGE_WRITE;
        }
    }

    tlb_set_page(cs, page, page, prot, mmu_idx, TARGET_PAGE_SIZE);
    return true;

fault:
    qemu_log_mask(LOG_GUEST_ERROR,
                  "slow32: invalid %s at 0x%08" PRIx64 "\n",
                  access_type == MMU_INST_FETCH ? "fetch" :
                  access_type == MMU_DATA_LOAD ? "load" : "store",
                  (uint64_t)addr);
    env->halted = 1;
    slow32_cpu_complete_halt(cpu);
    cpu_loop_exit_restore(cs, retaddr);
}

static void slow32_cpu_do_interrupt(CPUState *cs)
{
    cpu_abort(cs, "Slow32 interrupt delivery is not implemented yet");
}

static bool slow32_cpu_exec_interrupt(CPUState *cs, int interrupt_request)
{
    /* Interrupts are not implemented yet. */
    return false;
}

static void slow32_cpu_reset_hold(Object *obj, ResetType type)
{
    Slow32CPU *cpu = SLOW32_CPU(obj);
    Slow32CPUClass *scc = SLOW32_CPU_GET_CLASS(obj);
    CPUSlow32State *env = &cpu->env;

    if (scc->parent_phases.hold) {
        scc->parent_phases.hold(obj, type);
    }

    memset(env, 0, sizeof(*env));
    env->stack_top = SLOW32_DEFAULT_STACK_TOP;
    env->regs[SLOW32_REG_SP] = env->stack_top;
    env->pc = 0;
    env->next_pc = 0;
    env->data_limit = 0;
    env->code_limit = 0;
    env->rodata_limit = 0;
    env->heap_base = 0;
    env->mem_size = 0;
    env->halted = 0;
    env->layout_defined = false;

    slow32_mmio_reset(cpu);
}

static void slow32_cpu_realizefn(DeviceState *dev, Error **errp)
{
    Slow32CPU *cpu = SLOW32_CPU(dev);
    CPUState *cs = CPU(dev);
    Slow32CPUClass *scc = SLOW32_CPU_GET_CLASS(dev);
    Error *local_err = NULL;

    slow32_mmio_context_init(cpu);

    cpu->env.mmio_base = 0;

    cpu_exec_realizefn(cs, &local_err);
    if (local_err) {
        error_propagate(errp, local_err);
        return;
    }

    qemu_init_vcpu(cs);
    cpu_reset(cs);

    if (scc->parent_realize) {
        scc->parent_realize(dev, errp);
    }
}

static void slow32_cpu_set_irq(void *opaque, int irq, int level)
{
    Slow32CPU *cpu = SLOW32_CPU(opaque);

    if (level) {
        cpu->env.halted = 0;
    }
}

void slow32_handle_debug(uint32_t value)
{
    putchar(value & 0xFF);
    fflush(stdout);
}

void slow32_handle_yield(Slow32CPU *cpu)
{
    if (cpu && cpu->mmio) {
        slow32_mmio_process(cpu);
    }
}

static int slow32_cpu_gdb_read_register(CPUState *cs, GByteArray *buf, int n)
{
    CPUSlow32State *env = cpu_env(cs);

    if (n < SLOW32_NUM_GPRS) {
        gdb_get_reg32(buf, slow32_get_reg(env, n));
        return 4;
    }

    if (n == SLOW32_NUM_GPRS) {
        gdb_get_reg32(buf, env->pc);
        return 4;
    }

    return 0;
}

static int slow32_cpu_gdb_write_register(CPUState *cs, uint8_t *mem_buf, int n)
{
    CPUSlow32State *env = cpu_env(cs);

    if (n < SLOW32_NUM_GPRS) {
        slow32_set_reg(env, n, ldl_p(mem_buf));
        return 4;
    }

    if (n == SLOW32_NUM_GPRS) {
        slow32_cpu_set_pc(cs, ldl_p(mem_buf));
        return 4;
    }

    return 0;
}

static ObjectClass *slow32_cpu_class_by_name(const char *cpu_model)
{
    char *typename = g_strdup_printf(SLOW32_CPU_TYPE_NAME("%s"), cpu_model);
    ObjectClass *oc = object_class_by_name(typename);

    g_free(typename);
    return oc;
}

#ifndef CONFIG_USER_ONLY
static const struct SysemuCPUOps slow32_sysemu_ops = {
    .has_work = slow32_cpu_has_work,
    .get_phys_page_debug = slow32_cpu_get_phys_page_debug,
};
#endif

static const TCGCPUOps slow32_tcg_ops = {
    .guest_default_memory_order = TCG_MO_ALL,
    .mttcg_supported = false,
    .initialize = slow32_translate_init,
    .translate_code = slow32_translate_code,
    .get_tb_cpu_state = slow32_get_tb_cpu_state,
    .synchronize_from_tb = slow32_cpu_synchronize_from_tb,
    .restore_state_to_opc = slow32_restore_state_to_opc,
    .mmu_index = slow32_cpu_mmu_index,
    .pointer_wrap = cpu_pointer_wrap_uint32,
#ifndef CONFIG_USER_ONLY
    .tlb_fill = slow32_cpu_tlb_fill,
    .cpu_exec_interrupt = slow32_cpu_exec_interrupt,
    .cpu_exec_halt = slow32_cpu_has_work,
    .cpu_exec_reset = cpu_reset,
    .do_interrupt = slow32_cpu_do_interrupt,
#endif
};

static void slow32_cpu_initfn(Object *obj)
{
#ifndef CONFIG_USER_ONLY
    qdev_init_gpio_in_named(DEVICE(obj), slow32_cpu_set_irq, "IRQ", 1);
#endif
}

static void slow32_cpu_class_init(ObjectClass *oc, const void *data)
{
    Slow32CPUClass *scc = SLOW32_CPU_CLASS(oc);
    CPUClass *cc = CPU_CLASS(scc);
    DeviceClass *dc = DEVICE_CLASS(oc);
    ResettableClass *rc = RESETTABLE_CLASS(oc);

    device_class_set_parent_realize(dc, slow32_cpu_realizefn,
                                    &scc->parent_realize);
    resettable_class_set_parent_phases(rc, NULL, slow32_cpu_reset_hold, NULL,
                                       &scc->parent_phases);

    cc->class_by_name = slow32_cpu_class_by_name;
    cc->dump_state = slow32_cpu_dump_state;
    cc->set_pc = slow32_cpu_set_pc;
    cc->get_pc = slow32_cpu_get_pc;
    cc->gdb_read_register = slow32_cpu_gdb_read_register;
    cc->gdb_write_register = slow32_cpu_gdb_write_register;
    cc->gdb_num_core_regs = SLOW32_NUM_GPRS + 1;
#ifndef CONFIG_USER_ONLY
    cc->sysemu_ops = &slow32_sysemu_ops;
#endif
    cc->disas_set_info = slow32_disas_set_info;
    cc->tcg_ops = &slow32_tcg_ops;
}

static void slow32_cpu_finalize(Object *obj)
{
    Slow32CPU *cpu = SLOW32_CPU(obj);

    slow32_mmio_context_destroy(cpu);
}

static const TypeInfo slow32_cpu_type_info = {
    .name = TYPE_SLOW32_CPU,
    .parent = TYPE_CPU,
    .instance_size = sizeof(Slow32CPU),
    .instance_align = __alignof(Slow32CPU),
    .instance_init = slow32_cpu_initfn,
    .abstract = false,
    .class_size = sizeof(Slow32CPUClass),
    .class_init = slow32_cpu_class_init,
    .instance_finalize = slow32_cpu_finalize,
};

static void slow32_cpu_register_types(void)
{
    type_register_static(&slow32_cpu_type_info);
}

type_init(slow32_cpu_register_types)
