#include "qemu/osdep.h"

#include <inttypes.h>

#include "cpu.h"
#include "exec/helper-proto.h"
#include "exec/target_page.h"
#include "accel/tcg/cpu-ldst.h"
#include "accel/tcg/probe.h"
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

/*
 * Native intrinsic helpers.
 *
 * Each helper reads arguments from guest registers (r3=arg0, r4=arg1,
 * r5=arg2) per the SLOW-32 calling convention, performs the operation
 * using host-native routines where possible, writes the return value
 * to r1, and sets PC = r31 (link register) to return to the caller.
 *
 * We chunk operations at page boundaries and use probe_write()/probe_read()
 * to get host pointers.  If a probe returns NULL (MMIO region), we fall
 * back to byte-by-byte access via cpu_ldub/cpu_stb.
 */

void HELPER(slow32_native_memcpy)(CPUSlow32State *env)
{
    uint32_t dst = env->regs[3];
    uint32_t src = env->regs[4];
    uint32_t len = env->regs[5];
    uintptr_t ra = GETPC();

    env->regs[1] = dst;

    while (len > 0) {
        uint32_t page_remain_dst = TARGET_PAGE_SIZE - (dst & ~TARGET_PAGE_MASK);
        uint32_t page_remain_src = TARGET_PAGE_SIZE - (src & ~TARGET_PAGE_MASK);
        uint32_t chunk = MIN(len, MIN(page_remain_dst, page_remain_src));

        void *host_dst = probe_write(env, dst, chunk, 0, ra);
        void *host_src = probe_read(env, src, chunk, 0, ra);

        if (likely(host_dst && host_src)) {
            memcpy(host_dst, host_src, chunk);
        } else {
            for (uint32_t i = 0; i < chunk; i++) {
                uint8_t b = cpu_ldub_mmuidx_ra(env, src + i, 0, ra);
                cpu_stb_mmuidx_ra(env, dst + i, b, 0, ra);
            }
        }

        dst += chunk;
        src += chunk;
        len -= chunk;
    }

    env->pc = env->next_pc = env->regs[31];
}

void HELPER(slow32_native_memset)(CPUSlow32State *env)
{
    uint32_t dst = env->regs[3];
    uint32_t val = env->regs[4] & 0xFF;
    uint32_t len = env->regs[5];
    uintptr_t ra = GETPC();

    env->regs[1] = dst;

    while (len > 0) {
        uint32_t chunk = MIN(len, TARGET_PAGE_SIZE - (dst & ~TARGET_PAGE_MASK));
        void *host = probe_write(env, dst, chunk, 0, ra);

        if (likely(host)) {
            memset(host, val, chunk);
        } else {
            for (uint32_t i = 0; i < chunk; i++) {
                cpu_stb_mmuidx_ra(env, dst + i, val, 0, ra);
            }
        }

        dst += chunk;
        len -= chunk;
    }

    env->pc = env->next_pc = env->regs[31];
}

void HELPER(slow32_native_memmove)(CPUSlow32State *env)
{
    uint32_t dst = env->regs[3];
    uint32_t src = env->regs[4];
    uint32_t len = env->regs[5];
    uintptr_t ra = GETPC();

    env->regs[1] = dst;

    if (dst < src || dst >= src + len) {
        /* No overlap or dst before src: forward copy */
        uint32_t d = dst, s = src, remaining = len;
        while (remaining > 0) {
            uint32_t page_d = TARGET_PAGE_SIZE - (d & ~TARGET_PAGE_MASK);
            uint32_t page_s = TARGET_PAGE_SIZE - (s & ~TARGET_PAGE_MASK);
            uint32_t chunk = MIN(remaining, MIN(page_d, page_s));

            void *host_d = probe_write(env, d, chunk, 0, ra);
            void *host_s = probe_read(env, s, chunk, 0, ra);

            if (likely(host_d && host_s)) {
                memcpy(host_d, host_s, chunk);
            } else {
                for (uint32_t i = 0; i < chunk; i++) {
                    uint8_t b = cpu_ldub_mmuidx_ra(env, s + i, 0, ra);
                    cpu_stb_mmuidx_ra(env, d + i, b, 0, ra);
                }
            }

            d += chunk;
            s += chunk;
            remaining -= chunk;
        }
    } else {
        /* Overlap with dst > src: backward copy byte-by-byte for correctness */
        for (uint32_t i = len; i > 0; i--) {
            uint8_t b = cpu_ldub_mmuidx_ra(env, src + i - 1, 0, ra);
            cpu_stb_mmuidx_ra(env, dst + i - 1, b, 0, ra);
        }
    }

    env->pc = env->next_pc = env->regs[31];
}

void HELPER(slow32_native_strlen)(CPUSlow32State *env)
{
    uint32_t str = env->regs[3];
    uintptr_t ra = GETPC();
    uint32_t len = 0;

    while (true) {
        uint32_t page_remain = TARGET_PAGE_SIZE - ((str + len) & ~TARGET_PAGE_MASK);
        uint32_t chunk = page_remain;

        void *host = probe_read(env, str + len, chunk, 0, ra);
        if (likely(host)) {
            const uint8_t *p = host;
            for (uint32_t i = 0; i < chunk; i++) {
                if (p[i] == 0) {
                    env->regs[1] = len + i;
                    env->pc = env->next_pc = env->regs[31];
                    return;
                }
            }
            len += chunk;
        } else {
            for (uint32_t i = 0; i < chunk; i++) {
                if (cpu_ldub_mmuidx_ra(env, str + len, 0, ra) == 0) {
                    env->regs[1] = len;
                    env->pc = env->next_pc = env->regs[31];
                    return;
                }
                len++;
            }
        }
    }
}

void HELPER(slow32_native_memswap)(CPUSlow32State *env)
{
    uint32_t a = env->regs[3];
    uint32_t b = env->regs[4];
    uint32_t len = env->regs[5];
    uintptr_t ra = GETPC();

    env->regs[1] = a;

    while (len > 0) {
        uint32_t page_a = TARGET_PAGE_SIZE - (a & ~TARGET_PAGE_MASK);
        uint32_t page_b = TARGET_PAGE_SIZE - (b & ~TARGET_PAGE_MASK);
        uint32_t chunk = MIN(len, MIN(page_a, page_b));

        void *host_a = probe_write(env, a, chunk, 0, ra);
        void *host_b = probe_write(env, b, chunk, 0, ra);

        if (likely(host_a && host_b)) {
            uint8_t *pa = host_a;
            uint8_t *pb = host_b;
            for (uint32_t i = 0; i < chunk; i++) {
                uint8_t tmp = pa[i];
                pa[i] = pb[i];
                pb[i] = tmp;
            }
        } else {
            for (uint32_t i = 0; i < chunk; i++) {
                uint8_t va = cpu_ldub_mmuidx_ra(env, a + i, 0, ra);
                uint8_t vb = cpu_ldub_mmuidx_ra(env, b + i, 0, ra);
                cpu_stb_mmuidx_ra(env, a + i, vb, 0, ra);
                cpu_stb_mmuidx_ra(env, b + i, va, 0, ra);
            }
        }

        a += chunk;
        b += chunk;
        len -= chunk;
    }

    env->pc = env->next_pc = env->regs[31];
}
