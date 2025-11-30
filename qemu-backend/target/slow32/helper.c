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
    CPUState *cs = CPU(cpu);

    cs->halted = 1;
    qemu_system_shutdown_request(SHUTDOWN_CAUSE_GUEST_SHUTDOWN);
}

void HELPER(slow32_halt)(CPUSlow32State *env)
{
    CPUState *cs = env_cpu(env);
    Slow32CPU *cpu = SLOW32_CPU(cs);

    slow32_handle_yield(cpu);
    slow32_cpu_complete_halt(cpu);
    cpu_exit(cs);
}
