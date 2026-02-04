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

/* Signature types for math function interception */
enum {
    SLOW32_SIG_F32_F32,        /* float fn(float) */
    SLOW32_SIG_F64_F64,        /* double fn(double) */
    SLOW32_SIG_F32_F32_F32,    /* float fn(float, float) */
    SLOW32_SIG_F64_F64_F64,    /* double fn(double, double) */
    SLOW32_SIG_F64_F64_I32,    /* double fn(double, int) — ldexp */
    SLOW32_SIG_F32_F32_I32,    /* float fn(float, int) — ldexpf */
    SLOW32_SIG_F64_F64_DPTR,   /* double fn(double, double*) — modf */
    SLOW32_SIG_F64_F64_IPTR,   /* double fn(double, int*) — frexp */
    SLOW32_SIG_F32_F32_FPTR,   /* float fn(float, float*) — modff */
    SLOW32_SIG_F32_F32_IPTR,   /* float fn(float, int*) — frexpf */
    SLOW32_SIG_I32_F64,        /* int fn(double) — isnan, isinf, isfinite */
};

#define SLOW32_MAX_MATH_INTERCEPTS 64

typedef struct {
    uint32_t guest_addr;   /* 0 = unused */
    void    *host_fn;      /* pointer to host libm function */
    uint8_t  sig;          /* SLOW32_SIG_* */
} Slow32MathIntercept;

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
    uint32_t stack_end;
    uint32_t halted;
    bool layout_defined;

    /* Intrinsic recognition: guest addresses of known functions (0 = none) */
    uint32_t intrinsic_memcpy;
    uint32_t intrinsic_memset;
    uint32_t intrinsic_memmove;
    uint32_t intrinsic_strlen;
    uint32_t intrinsic_memswap;

    /* Math function interception table (populated at load time) */
    Slow32MathIntercept math_intercepts[SLOW32_MAX_MATH_INTERCEPTS];
    int num_math_intercepts;
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
};

void slow32_translate_init(void);
void slow32_translate_code(CPUState *cs, TranslationBlock *tb, int *max_insns,
                           vaddr pc, void *host_pc);
void slow32_cpu_dump_state(CPUState *cpu, FILE *f, int flags);
void slow32_disas_set_info(const CPUState *cpu, disassemble_info *info);
void slow32_handle_debug(uint32_t value);
void slow32_handle_yield(Slow32CPU *cpu);
void slow32_cpu_complete_halt(Slow32CPU *cpu);

#endif /* SLOW32_CPU_H */
