/*
 * Slow32 virtual CPU header.
 *
 * SPDX-License-Identifier: LGPL-2.1-or-later
 */

#ifndef SLOW32_CPU_H
#define SLOW32_CPU_H

#include <stdbool.h>

#include "cpu-qom.h"
#include "exec/cpu-common.h"
#include "exec/cpu-defs.h"
#include "exec/cpu-interrupt.h"

#define CPU_RESOLVING_TYPE TYPE_SLOW32_CPU

#define SLOW32_NUM_GPRS 32
#define SLOW32_REG_ZERO 0
#define SLOW32_REG_SP   29
#define SLOW32_DEFAULT_STACK_TOP 0x0FFFFFF0u

typedef struct Slow32MMIOContext Slow32MMIOContext;

struct Slow32CPUClass {
    CPUClass parent_class;

    DeviceRealize parent_realize;
    ResettablePhases parent_phases;
};

typedef struct CPUArchState CPUSlow32State;

struct CPUArchState {
    uint32_t regs[SLOW32_NUM_GPRS];
    uint32_t pc;
    uint32_t next_pc;
    uint32_t mmio_base;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    uint32_t heap_base;
    uint32_t mem_size;
    uint32_t stack_top;
    uint32_t halted;
    uint64_t insn_retired;
    uint64_t tb_translated;
    uint64_t tb_translated_bytes;
    uint64_t tb_translated_insns;
    uint64_t tb_exec_count;
    uint64_t tb_exec_insns;
    uint64_t translate_time_us;
    int64_t run_start_us;
    bool layout_defined;
    bool stats_enabled;
};

static inline uint32_t slow32_get_reg(const CPUSlow32State *env, int idx)
{
    if (idx == SLOW32_REG_ZERO) {
        return 0;
    }
    return env->regs[idx];
}

static inline void slow32_set_reg(CPUSlow32State *env, int idx, uint32_t value)
{
    if (idx == SLOW32_REG_ZERO) {
        return;
    }
    env->regs[idx] = value;
}

struct ArchCPU {
    CPUState parent_obj;

    CPUSlow32State env;
    Slow32MMIOContext *mmio;
    bool stats_enabled;
};

void slow32_translate_init(void);
void slow32_translate_code(CPUState *cs, TranslationBlock *tb, int *max_insns,
                           vaddr pc, void *host_pc);
void slow32_cpu_dump_state(CPUState *cpu, FILE *f, int flags);
void slow32_disas_set_info(CPUState *cpu, disassemble_info *info);
void slow32_handle_debug(uint32_t value);
void slow32_handle_yield(Slow32CPU *cpu);
void slow32_cpu_complete_halt(Slow32CPU *cpu);

#endif /* SLOW32_CPU_H */
