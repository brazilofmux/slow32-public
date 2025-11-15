#include "qemu/osdep.h"

#include <inttypes.h>

#include "cpu.h"
#include "exec/helper-proto.h"
#include "system/runstate.h"

void HELPER(slow32_debug)(CPUSlow32State *env, uint32_t value)
{
    slow32_handle_debug(value);
}

void HELPER(slow32_yield)(CPUSlow32State *env)
{
    slow32_handle_yield(SLOW32_CPU(env_cpu(env)));
}

void slow32_cpu_complete_halt(Slow32CPU *cpu)
{
    int64_t now_us = g_get_monotonic_time();
    double wall_ms = 0.0;
    CPUSlow32State *env = &cpu->env;
    CPUState *cs = CPU(cpu);

    if (env->run_start_us) {
        wall_ms = (now_us - env->run_start_us) / 1000.0;
    }

    double translate_ms = env->translate_time_us / 1000.0;
    double exec_ms = wall_ms - translate_ms;
    if (exec_ms < 0) {
        exec_ms = 0;
    }

    printf("Slow32 stats: guest_insns=%" PRIu64
           " wall_ms=%.3f translate_ms=%.3f exec_ms=%.3f tb_count=%" PRIu64 "\n",
           env->insn_retired, wall_ms, translate_ms, exec_ms,
           env->tb_translated);
    fflush(stdout);

    cs->halted = 1;
    qemu_system_shutdown_request(SHUTDOWN_CAUSE_GUEST_SHUTDOWN);
}

void HELPER(slow32_halt)(CPUSlow32State *env)
{
    Slow32CPU *cpu = SLOW32_CPU(env_cpu(env));

    slow32_handle_yield(cpu);
    slow32_cpu_complete_halt(cpu);
}
