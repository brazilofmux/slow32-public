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

/*
 * Floating-point helper.
 *
 * The raw instruction word is passed in; we decode rd/rs1/rs2 and dispatch
 * to the appropriate C float operation.  This avoids needing 38 separate
 * DEF_HELPER macros.
 */
#include <math.h>

static inline void qemu_load_f64(CPUSlow32State *env, int reg, double *out)
{
    uint64_t bits = ((uint64_t)env->regs[reg + 1] << 32) | env->regs[reg];
    memcpy(out, &bits, 8);
}

static inline void qemu_store_f64(CPUSlow32State *env, int reg, double val)
{
    uint64_t bits;
    memcpy(&bits, &val, 8);
    env->regs[reg] = (uint32_t)bits;
    env->regs[reg + 1] = (uint32_t)(bits >> 32);
}

void HELPER(slow32_fp_op)(CPUSlow32State *env, uint32_t raw)
{
    uint32_t opcode = raw & 0x7F;
    int rd  = (raw >> 7)  & 0x1F;
    int rs1 = (raw >> 15) & 0x1F;
    int rs2 = (raw >> 20) & 0x1F;
    uint32_t *r = env->regs;

    switch (opcode) {
    /* f32 arithmetic */
    case 0x53: { float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); res = a + b; memcpy(&r[rd], &res, 4); break; }
    case 0x54: { float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); res = a - b; memcpy(&r[rd], &res, 4); break; }
    case 0x55: { float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); res = a * b; memcpy(&r[rd], &res, 4); break; }
    case 0x56: { float a, b, res; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); res = a / b; memcpy(&r[rd], &res, 4); break; }
    case 0x57: { float a, res; memcpy(&a, &r[rs1], 4); res = sqrtf(a); memcpy(&r[rd], &res, 4); break; }
    case 0x58: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a == b) ? 1 : 0; break; }
    case 0x59: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a < b) ? 1 : 0; break; }
    case 0x5A: { float a, b; memcpy(&a, &r[rs1], 4); memcpy(&b, &r[rs2], 4); r[rd] = (a <= b) ? 1 : 0; break; }
    case 0x5B: { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)(int32_t)a; break; }
    case 0x5C: { float a; memcpy(&a, &r[rs1], 4); r[rd] = (uint32_t)a; break; }
    case 0x5D: { float res = (float)(int32_t)r[rs1]; memcpy(&r[rd], &res, 4); break; }
    case 0x5E: { float res = (float)r[rs1]; memcpy(&r[rd], &res, 4); break; }
    case 0x5F: r[rd] = r[rs1] ^ 0x80000000u; break;
    case 0x60: r[rd] = r[rs1] & 0x7FFFFFFFu; break;

    /* f64 arithmetic */
    case 0x61: { double a, b, res; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); res = a + b; qemu_store_f64(env, rd, res); break; }
    case 0x62: { double a, b, res; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); res = a - b; qemu_store_f64(env, rd, res); break; }
    case 0x63: { double a, b, res; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); res = a * b; qemu_store_f64(env, rd, res); break; }
    case 0x64: { double a, b, res; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); res = a / b; qemu_store_f64(env, rd, res); break; }
    case 0x65: { double a, res; qemu_load_f64(env, rs1, &a); res = sqrt(a); qemu_store_f64(env, rd, res); break; }
    case 0x66: { double a, b; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); r[rd] = (a == b) ? 1 : 0; break; }
    case 0x67: { double a, b; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); r[rd] = (a < b) ? 1 : 0; break; }
    case 0x68: { double a, b; qemu_load_f64(env, rs1, &a); qemu_load_f64(env, rs2, &b); r[rd] = (a <= b) ? 1 : 0; break; }
    case 0x69: { double a; qemu_load_f64(env, rs1, &a); r[rd] = (uint32_t)(int32_t)a; break; }
    case 0x6A: { double a; qemu_load_f64(env, rs1, &a); r[rd] = (uint32_t)a; break; }
    case 0x6B: qemu_store_f64(env, rd, (double)(int32_t)r[rs1]); break;
    case 0x6C: qemu_store_f64(env, rd, (double)r[rs1]); break;
    case 0x6D: { float a; memcpy(&a, &r[rs1], 4); qemu_store_f64(env, rd, (double)a); break; }
    case 0x6E: { double a; qemu_load_f64(env, rs1, &a); float res = (float)a; memcpy(&r[rd], &res, 4); break; }
    case 0x6F: r[rd] = r[rs1]; r[rd + 1] = r[rs1 + 1] ^ 0x80000000u; break;
    case 0x70: r[rd] = r[rs1]; r[rd + 1] = r[rs1 + 1] & 0x7FFFFFFFu; break;

    /* float <-> int64 */
    case 0x71: { float a; memcpy(&a, &r[rs1], 4); int64_t v = (int64_t)a; r[rd] = (uint32_t)v; r[rd+1] = (uint32_t)((uint64_t)v >> 32); break; }
    case 0x72: { float a; memcpy(&a, &r[rs1], 4); uint64_t v = (uint64_t)a; r[rd] = (uint32_t)v; r[rd+1] = (uint32_t)(v >> 32); break; }
    case 0x73: { int64_t v = (int64_t)(((uint64_t)r[rs1+1] << 32) | r[rs1]); float res = (float)v; memcpy(&r[rd], &res, 4); break; }
    case 0x74: { uint64_t v = ((uint64_t)r[rs1+1] << 32) | r[rs1]; float res = (float)v; memcpy(&r[rd], &res, 4); break; }
    case 0x75: { double a; qemu_load_f64(env, rs1, &a); int64_t v = (int64_t)a; r[rd] = (uint32_t)v; r[rd+1] = (uint32_t)((uint64_t)v >> 32); break; }
    case 0x76: { double a; qemu_load_f64(env, rs1, &a); uint64_t v = (uint64_t)a; r[rd] = (uint32_t)v; r[rd+1] = (uint32_t)(v >> 32); break; }
    case 0x77: { int64_t v = (int64_t)(((uint64_t)r[rs1+1] << 32) | r[rs1]); qemu_store_f64(env, rd, (double)v); break; }
    case 0x78: { uint64_t v = ((uint64_t)r[rs1+1] << 32) | r[rs1]; qemu_store_f64(env, rd, (double)v); break; }
    }

    r[0] = 0;  /* r0 stays zero */
}
