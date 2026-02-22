// SLOW-32 DBT: Main Entry Point
// Stage 4 - Superblock extension

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#ifndef _POSIX_C_SOURCE
#define _POSIX_C_SOURCE 199309L
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <time.h>
#include <inttypes.h>
#include <math.h>
#include <signal.h>
#include <sys/time.h>
#include <unistd.h>
#include <ucontext.h>

#include "cpu_state.h"
#include "translate.h"
#include "block_cache.h"
#include "stage5_burg.h"
#include "stage5_codegen.h"

// Reuse components from the emulator
#include "../emulator/s32x_loader.h"
#include "../emulator/mmio_ring.h"

#define STAGE1_CODE_BUFFER_SIZE (64 * 1024)    // 64KB for Stage 1 translated code
#define GUEST_MEM_SIZE   (256 * 1024 * 1024)  // 256MB guest memory (must cover stack at 0x0FFFFFF0)

// Profiling accumulators (nanoseconds)
static bool profile_timing = false;
static uint32_t profile_sample_rate = 1000;
static uint64_t profile_translate_cycles = 0;
static uint64_t profile_exec_cycles = 0;
static uint64_t profile_tsc_start = 0;
static uint64_t profile_tsc_end = 0;
static bool profile_side_exits = false;
static bool avoid_backedge_extend = false;
static bool peephole_enabled = true;
static bool superblock_enabled = true;
static bool reg_cache_enabled = true;
static bool strict_carry_enabled = false;
static bool stage5_burg_hook_enabled = false;
static bool stage5_emit_hook_enabled = false;
static bool align_traps_enabled = false;
static bool intrinsics_disabled = false;
static bool bounds_checks_disabled = false;
static bool trace_branch_exit_enabled = false;
static int trace_branch_exit_budget = 0;
static uint32_t trace_branch_exit_pc_filter = 0;
static bool trace_block_exits_enabled = false;
static int trace_block_exits_budget = 0;
static uint32_t trace_block_exits_pc = 0;
static bool trace_block_regs_enabled = false;
static int trace_block_regs_budget = 0;
static uint32_t trace_block_regs_pc = 0;

static void apply_stage5_native_bench_profile(int *stage_out,
                                              bool *two_pass_out,
                                              bool *two_pass_forced_out,
                                              bool *show_stats_out) {
    if (stage_out) *stage_out = 5;
    if (two_pass_out) *two_pass_out = false;
    if (two_pass_forced_out) *two_pass_forced_out = true;
    (void)show_stats_out;
    // Stable Stage5-native baseline for apples-to-apples benchmarking.
    strict_carry_enabled = true;
    stage5_burg_hook_enabled = true;
    stage5_emit_hook_enabled = true;
    superblock_enabled = true;
    reg_cache_enabled = true;
    peephole_enabled = true;
    profile_side_exits = false;
    // Benchmark profile defaults should exercise Stage5-owned side exits.
    // Use overwrite=0 so explicit caller env still wins.
    setenv("SLOW32_DBT_STAGE5_SIDE_EXIT", "1", 0);
    setenv("SLOW32_DBT_STAGE5_SIDE_EXIT_MODE", "eqne_u", 0);
    setenv("SLOW32_DBT_STAGE5_BENCH_PROFILE", "1", 1);
}

// MMIO state
static mmio_ring_state_t mmio_state;
static bool mmio_initialized = false;

// Host argc/argv for passing to guest
static int host_argc = 0;
static char **host_argv = NULL;

// ============================================================================
// Signal-based probe/debugging support
// ============================================================================

#define PROBE_RING_SIZE 8

typedef struct {
    volatile sig_atomic_t flag;        // Set by SIGALRM, cleared by dispatch loop
    volatile sig_atomic_t sigint_flag; // Set by SIGINT
    uint32_t pc_ring[PROBE_RING_SIZE]; // Ring buffer for loop detection
    int ring_pos, ring_count;
    struct timespec start_time;
    int probe_interval_sec;
    int probe_count;
    bool enabled;
} dbt_probe_state_t;

static volatile dbt_cpu_state_t *g_dbt_cpu = NULL;
static dbt_probe_state_t *g_dbt_probe = NULL;
static volatile uintptr_t g_dbt_code_base = 0;
static volatile uintptr_t g_dbt_code_limit = 0;
static volatile uint32_t g_dbt_block_pc = 0;
static volatile uintptr_t g_dbt_block_code = 0;
static volatile uint32_t g_dbt_block_size = 0;

static void dbt_init_branch_trace_from_env(void) {
    static bool inited = false;
    if (inited) return;
    inited = true;

    const char *v = getenv("SLOW32_DBT_TRACE_BRANCH_EXIT");
    trace_branch_exit_enabled = (v && v[0] != '\0' && strcmp(v, "0") != 0);
    if (trace_branch_exit_enabled) {
        const char *maxv = getenv("SLOW32_DBT_TRACE_BRANCH_EXIT_MAX");
        trace_branch_exit_budget = (maxv && maxv[0] != '\0') ? atoi(maxv) : 64;
        if (trace_branch_exit_budget < 0) trace_branch_exit_budget = 0;

        const char *pcv = getenv("SLOW32_DBT_TRACE_BRANCH_PC");
        if (pcv && pcv[0] != '\0') {
            trace_branch_exit_pc_filter = (uint32_t)strtoul(pcv, NULL, 0);
        }
    }

    const char *bx = getenv("SLOW32_DBT_TRACE_BLOCK_EXITS_PC");
    if (bx && bx[0] != '\0') {
        trace_block_exits_enabled = true;
        trace_block_exits_pc = (uint32_t)strtoul(bx, NULL, 0);
        const char *bmax = getenv("SLOW32_DBT_TRACE_BLOCK_EXITS_MAX");
        trace_block_exits_budget = (bmax && bmax[0] != '\0') ? atoi(bmax) : 8;
        if (trace_block_exits_budget < 0) trace_block_exits_budget = 0;
    }

    const char *br = getenv("SLOW32_DBT_TRACE_BLOCK_REGS_PC");
    if (br && br[0] != '\0') {
        trace_block_regs_enabled = true;
        trace_block_regs_pc = (uint32_t)strtoul(br, NULL, 0);
        const char *rmax = getenv("SLOW32_DBT_TRACE_BLOCK_REGS_MAX");
        trace_block_regs_budget = (rmax && rmax[0] != '\0') ? atoi(rmax) : 8;
        if (trace_block_regs_budget < 0) trace_block_regs_budget = 0;
    }
}

static void dbt_trace_branch_exit(block_cache_t *cache,
                                  translated_block_t *block,
                                  dbt_cpu_state_t *cpu) {
    (void)cache;
    if (!trace_branch_exit_enabled) return;
    if (trace_branch_exit_budget == 0) return;
    if (cpu->exit_reason != EXIT_BRANCH &&
        cpu->exit_reason != EXIT_INDIRECT &&
        cpu->exit_reason != EXIT_BLOCK_END) {
        return;
    }
    if (trace_branch_exit_pc_filter != 0 && cpu->pc != trace_branch_exit_pc_filter) {
        return;
    }

    int exit_idx = -1;
    uint32_t branch_pc = 0;
    uint32_t target_pc = 0;
    if (block) {
        for (int i = 0; i < block->exit_count; i++) {
            if (block->exits[i].target_pc == cpu->pc) {
                exit_idx = i;
                branch_pc = block->exits[i].branch_pc;
                target_pc = block->exits[i].target_pc;
                break;
            }
        }
    }

    fprintf(stderr,
            "dbt-branch-exit reason=%u next_pc=0x%08X block_pc=0x%08X exit_idx=%d branch_pc=0x%08X target_pc=0x%08X info=0x%08X\n",
            cpu->exit_reason, cpu->pc, block ? block->guest_pc : 0, exit_idx,
            branch_pc, target_pc, cpu->exit_info);
    trace_branch_exit_budget--;
}

static void dbt_trace_block_exits(translated_block_t *block) {
    if (!trace_block_exits_enabled || !block) return;
    if (trace_block_exits_budget == 0) return;
    if (trace_block_exits_pc != 0 && block->guest_pc != trace_block_exits_pc) return;

    uint32_t guest_end = block->guest_pc + block->guest_size;
    fprintf(stderr,
            "dbt-block-exits block_pc=0x%08X block_end=0x%08X host_size=%u exits=%u side_exits=%u\n",
            block->guest_pc, guest_end, block->host_size, block->exit_count, block->side_exit_count);
    for (uint8_t i = 0; i < block->exit_count; i++) {
        bool side_owned = false;
        for (uint8_t s = 0; s < block->side_exit_count; s++) {
            if (block->side_exit_pcs[s] == block->exits[i].branch_pc) {
                side_owned = true;
                break;
            }
        }
        fprintf(stderr,
                "  exit[%u] target=0x%08X branch_pc=0x%08X chained=%u side_owned=%u\n",
                i, block->exits[i].target_pc, block->exits[i].branch_pc,
                block->exits[i].chained ? 1u : 0u, side_owned ? 1u : 0u);
    }
    for (uint8_t i = 0; i < block->side_exit_count; i++) {
        fprintf(stderr, "  side_exit_pc[%u]=0x%08X\n", i, block->side_exit_pcs[i]);
    }
    trace_block_exits_budget--;
}

static void dbt_trace_block_regs(dbt_cpu_state_t *cpu, translated_block_t *block) {
    if (!trace_block_regs_enabled || !cpu || !block) return;
    if (trace_block_regs_budget == 0) return;
    if (trace_block_regs_pc != 0 && block->guest_pc != trace_block_regs_pc) return;

    fprintf(stderr,
            "dbt-block-regs block_pc=0x%08X pc=0x%08X r7=%08X r10=%08X r12=%08X r14=%08X r15=%08X r16=%08X r18=%08X r19=%08X r20=%08X r21=%08X r23=%08X r28=%08X\n",
            block->guest_pc, cpu->pc,
            cpu->regs[7], cpu->regs[10], cpu->regs[12], cpu->regs[14], cpu->regs[15],
            cpu->regs[16], cpu->regs[18], cpu->regs[19], cpu->regs[20], cpu->regs[21],
            cpu->regs[23], cpu->regs[28]);
    trace_block_regs_budget--;
}

// Async-signal-safe hex printer: writes "0xNNNNNNNN" to buf, returns length
static int probe_hex32(char *buf, uint32_t val) {
    static const char hex[] = "0123456789ABCDEF";
    buf[0] = '0'; buf[1] = 'x';
    int i;
    for (i = 0; i < 8; i++) {
        buf[2 + i] = hex[(val >> (28 - i * 4)) & 0xF];
    }
    return 10;
}

// Async-signal-safe hex printer for 64-bit values: writes "0xNN..NN"
static int probe_hex64(char *buf, uint64_t val) {
    static const char hex[] = "0123456789ABCDEF";
    buf[0] = '0'; buf[1] = 'x';
    for (int i = 0; i < 16; i++) {
        buf[2 + i] = hex[(val >> (60 - i * 4)) & 0xFULL];
    }
    return 18;
}

static void dbt_sigfault_handler(int sig, siginfo_t *si, void *ucontext) {
    char buf[320];
    int pos = 0;
    const char prefix[] = "[dbt-fault] signal=";
    for (int i = 0; prefix[i]; i++) buf[pos++] = prefix[i];
    if (sig >= 0 && sig < 100) {
        if (sig >= 10) buf[pos++] = (char)('0' + (sig / 10));
        buf[pos++] = (char)('0' + (sig % 10));
    } else {
        buf[pos++] = '?';
    }
    const char a[] = " addr=";
    for (int i = 0; a[i]; i++) buf[pos++] = a[i];
    pos += probe_hex64(buf + pos, (uint64_t)(uintptr_t)(si ? si->si_addr : NULL));

    if (g_dbt_cpu) {
        const char p[] = " guest_pc=";
        for (int i = 0; p[i]; i++) buf[pos++] = p[i];
        pos += probe_hex32(buf + pos, g_dbt_cpu->pc);

        const char sp[] = " sp=";
        for (int i = 0; sp[i]; i++) buf[pos++] = sp[i];
        pos += probe_hex32(buf + pos, g_dbt_cpu->regs[29]);

        const char fp[] = " fp=";
        for (int i = 0; fp[i]; i++) buf[pos++] = fp[i];
        pos += probe_hex32(buf + pos, g_dbt_cpu->regs[30]);

        const char lr[] = " lr=";
        for (int i = 0; lr[i]; i++) buf[pos++] = lr[i];
        pos += probe_hex32(buf + pos, g_dbt_cpu->regs[31]);
    }
#if defined(__x86_64__)
    if (ucontext) {
        ucontext_t *uc = (ucontext_t *)ucontext;
        uint64_t ripv = (uint64_t)uc->uc_mcontext.gregs[REG_RIP];
        const char rip[] = " host_rip=";
        for (int i = 0; rip[i]; i++) buf[pos++] = rip[i];
        pos += probe_hex64(buf + pos, ripv);
        const char rsp[] = " host_rsp=";
        for (int i = 0; rsp[i]; i++) buf[pos++] = rsp[i];
        pos += probe_hex64(buf + pos, (uint64_t)uc->uc_mcontext.gregs[REG_RSP]);
        if (g_dbt_code_base != 0 && g_dbt_code_limit > g_dbt_code_base &&
            ripv >= g_dbt_code_base && ripv < g_dbt_code_limit) {
            const char off[] = " host_off=";
            for (int i = 0; off[i]; i++) buf[pos++] = off[i];
            pos += probe_hex64(buf + pos, ripv - g_dbt_code_base);
        }
        if (g_dbt_block_code != 0) {
            const char bpc[] = " block_pc=";
            for (int i = 0; bpc[i]; i++) buf[pos++] = bpc[i];
            pos += probe_hex32(buf + pos, g_dbt_block_pc);
            const char boff[] = " block_off=";
            for (int i = 0; boff[i]; i++) buf[pos++] = boff[i];
            if (ripv >= g_dbt_block_code &&
                ripv < g_dbt_block_code + g_dbt_block_size) {
                pos += probe_hex64(buf + pos, ripv - g_dbt_block_code);
            } else {
                pos += probe_hex64(buf + pos, UINT64_MAX);
            }
        }
    }
#endif

    buf[pos++] = '\n';
    if (write(STDERR_FILENO, buf, pos)) {/* async-signal-safe */}
    _exit(128 + sig);
}

static void dbt_sigint_handler(int sig) {
    (void)sig;
    if (g_dbt_probe) g_dbt_probe->sigint_flag = 1;
    if (g_dbt_cpu) ((dbt_cpu_state_t *)g_dbt_cpu)->halted = true;
}

static void dbt_sigalrm_handler(int sig) {
    (void)sig;
    if (!g_dbt_cpu || !g_dbt_probe) return;
    g_dbt_probe->flag = 1;

    // Async-signal-safe: print PC and key registers using write()
    char buf[128];
    int pos = 0;
    // "[probe] PC="
    const char prefix[] = "[probe] PC=";
    int j;
    for (j = 0; prefix[j]; j++) buf[pos++] = prefix[j];
    pos += probe_hex32(buf + pos, g_dbt_cpu->pc);
    // " SP="
    buf[pos++] = ' '; buf[pos++] = 'S'; buf[pos++] = 'P'; buf[pos++] = '=';
    pos += probe_hex32(buf + pos, g_dbt_cpu->regs[29]);
    // " FP="
    buf[pos++] = ' '; buf[pos++] = 'F'; buf[pos++] = 'P'; buf[pos++] = '=';
    pos += probe_hex32(buf + pos, g_dbt_cpu->regs[30]);
    // " LR="
    buf[pos++] = ' '; buf[pos++] = 'L'; buf[pos++] = 'R'; buf[pos++] = '=';
    pos += probe_hex32(buf + pos, g_dbt_cpu->regs[31]);
    // " r1="
    buf[pos++] = ' '; buf[pos++] = 'r'; buf[pos++] = '1'; buf[pos++] = '=';
    pos += probe_hex32(buf + pos, g_dbt_cpu->regs[1]);
    buf[pos++] = '\n';
    if (write(STDERR_FILENO, buf, pos)) {/* async-signal-safe */}
}

static void dbt_install_sigint(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = dbt_sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGINT, &sa, NULL);
}

static void dbt_install_sigfault(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_sigaction = dbt_sigfault_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = SA_SIGINFO;
    sigaction(SIGSEGV, &sa, NULL);
    sigaction(SIGBUS, &sa, NULL);
    sigaction(SIGILL, &sa, NULL);
}

static void dbt_start_probe_timer(int interval_sec) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = dbt_sigalrm_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    sigaction(SIGALRM, &sa, NULL);

    struct itimerval itv;
    itv.it_value.tv_sec = interval_sec;
    itv.it_value.tv_usec = 0;
    itv.it_interval.tv_sec = interval_sec;
    itv.it_interval.tv_usec = 0;
    setitimer(ITIMER_REAL, &itv, NULL);
}

static void dbt_stop_probe_timer(void) {
    struct itimerval itv = {{0, 0}, {0, 0}};
    setitimer(ITIMER_REAL, &itv, NULL);
    signal(SIGALRM, SIG_DFL);
}

// Detailed probe processing (called from dispatch loop when flag is set)
static void dbt_process_probe(dbt_probe_state_t *probe, dbt_cpu_state_t *cpu) {
    probe->flag = 0;
    uint32_t pc = cpu->pc;

    // Add to ring buffer
    probe->pc_ring[probe->ring_pos % PROBE_RING_SIZE] = pc;
    probe->ring_pos++;
    if (probe->ring_count < PROBE_RING_SIZE) probe->ring_count++;
    probe->probe_count++;

    // Calculate elapsed time
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    double elapsed = (now.tv_sec - probe->start_time.tv_sec)
                   + (now.tv_nsec - probe->start_time.tv_nsec) / 1e9;

    // Loop detection: check if all ring entries are in same 4KB region
    bool looping = false;
    if (probe->ring_count >= PROBE_RING_SIZE) {
        uint32_t base = probe->pc_ring[0] & ~0xFFFu;
        looping = true;
        int i;
        for (i = 1; i < PROBE_RING_SIZE; i++) {
            if ((probe->pc_ring[i] & ~0xFFFu) != base) {
                looping = false;
                break;
            }
        }
    }

    fprintf(stderr, "[%7.1fs] PC=0x%08X SP=0x%08X FP=0x%08X LR=0x%08X r1=0x%08X%s\n",
            elapsed, pc, cpu->regs[29], cpu->regs[30], cpu->regs[31],
            cpu->regs[1], looping ? " (LOOPING)" : "");

    // Full register dump every 10 probes
    if ((probe->probe_count % 10) == 0) {
        fprintf(stderr, "  r1-r10:");
        int i;
        for (i = 1; i <= 10; i++)
            fprintf(stderr, " %08X", cpu->regs[i]);
        fprintf(stderr, "\n");
    }
}

#if defined(__aarch64__)
static inline uint64_t rdtsc(void) {
    uint64_t val;
    __asm__ __volatile__("mrs %0, cntvct_el0" : "=r"(val));
    return val;
}
#elif defined(__x86_64__) || defined(__i386__)
static inline uint64_t rdtsc(void) {
    uint32_t lo, hi;
    __asm__ __volatile__("lfence\nrdtsc" : "=a"(lo), "=d"(hi) :: "memory");
    return ((uint64_t)hi << 32) | lo;
}
#endif

// ============================================================================
// CPU initialization
// ============================================================================

void dbt_cpu_init(dbt_cpu_state_t *cpu) {
    memset(cpu, 0, sizeof(*cpu));

    // Initialize stack pointer
    cpu->regs[REG_SP] = 0x0FFFFFF0;

    // Allocate executable code buffer (for Stage 1 mode)
    cpu->code_buffer = mmap(NULL, STAGE1_CODE_BUFFER_SIZE,
                            PROT_READ | PROT_WRITE | PROT_EXEC,
                            MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (cpu->code_buffer == MAP_FAILED) {
        perror("mmap code buffer");
        exit(1);
    }
    cpu->code_buffer_size = STAGE1_CODE_BUFFER_SIZE;

    // Allocate guest memory
    cpu->mem_base = mmap(NULL, GUEST_MEM_SIZE,
                         PROT_READ | PROT_WRITE,
                         MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (cpu->mem_base == MAP_FAILED) {
        perror("mmap guest memory");
        munmap(cpu->code_buffer, STAGE1_CODE_BUFFER_SIZE);
        exit(1);
    }
    cpu->mem_size = GUEST_MEM_SIZE;
}

void dbt_cpu_destroy(dbt_cpu_state_t *cpu) {
    if (cpu->code_buffer && cpu->code_buffer != MAP_FAILED) {
        munmap(cpu->code_buffer, cpu->code_buffer_size);
    }
    if (cpu->mem_base && cpu->mem_base != MAP_FAILED) {
        munmap(cpu->mem_base, cpu->mem_size);
    }
    // Reset MMIO state so it gets re-initialized with new memory addresses
    mmio_initialized = false;
}

// ============================================================================
// MMIO support
// ============================================================================

// Initialize MMIO for DBT - called after loading s32x if MMIO is enabled
static void dbt_init_mmio(dbt_cpu_state_t *cpu) {
    if (mmio_initialized || !cpu->mmio_enabled) return;

    // Check that MMIO base is within guest memory
    if (cpu->mmio_base > cpu->mem_size ||
        cpu->mem_size - cpu->mmio_base < 0x10000) {
        fprintf(stderr, "DBT: MMIO base 0x%08X exceeds guest memory\n", cpu->mmio_base);
        cpu->mmio_enabled = false;
        return;
    }

    mmio_ring_init(&mmio_state);

    // Configure direct access to guest memory (for zero-copy I/O)
    mmio_state.guest_mem_base = cpu->mem_base;
    mmio_state.guest_mem_size = cpu->mem_size;

    // Point ring buffers directly at guest memory
    uint8_t *mmio_mem = cpu->mem_base + cpu->mmio_base;
    mmio_state.req_ring = (io_descriptor_t*)(mmio_mem + S32_MMIO_REQ_RING_OFFSET);
    mmio_state.resp_ring = (io_descriptor_t*)(mmio_mem + S32_MMIO_RESP_RING_OFFSET);
    mmio_state.data_buffer = mmio_mem + S32_MMIO_DATA_BUFFER_OFFSET;
    mmio_state.base_addr = cpu->mmio_base;

    // Set up host args if provided
    if (host_argc > 0 && host_argv) {
        mmio_ring_set_args(&mmio_state, host_argc, host_argv);
    }

    // Pass environment to guest
    extern char **environ;
    mmio_ring_set_envp(&mmio_state, environ);

    mmio_initialized = true;
}

// Sync MMIO indices from guest memory to host state
static inline void mmio_sync_from_guest(dbt_cpu_state_t *cpu) {
    if (!mmio_initialized) return;

    uint32_t *mmio_mem = (uint32_t*)(cpu->mem_base + cpu->mmio_base);
    mmio_state.req_head = mmio_mem[S32_MMIO_REQ_HEAD_OFFSET / 4];
    mmio_state.req_tail = mmio_mem[S32_MMIO_REQ_TAIL_OFFSET / 4];
    mmio_state.resp_head = mmio_mem[S32_MMIO_RESP_HEAD_OFFSET / 4];
    mmio_state.resp_tail = mmio_mem[S32_MMIO_RESP_TAIL_OFFSET / 4];
}

// Sync MMIO indices from host state back to guest memory
static inline void mmio_sync_to_guest(dbt_cpu_state_t *cpu) {
    if (!mmio_initialized) return;

    uint32_t *mmio_mem = (uint32_t*)(cpu->mem_base + cpu->mmio_base);
    mmio_mem[S32_MMIO_REQ_HEAD_OFFSET / 4] = mmio_state.req_head;
    mmio_mem[S32_MMIO_REQ_TAIL_OFFSET / 4] = mmio_state.req_tail;
    mmio_mem[S32_MMIO_RESP_HEAD_OFFSET / 4] = mmio_state.resp_head;
    mmio_mem[S32_MMIO_RESP_TAIL_OFFSET / 4] = mmio_state.resp_tail;
}

// Handle YIELD instruction - process MMIO ring buffers
static uint32_t yield_spin_count = 0;
static uint32_t last_req_head = 0, last_req_tail = 0;

static void dbt_handle_yield(dbt_cpu_state_t *cpu) {
    if (!cpu->mmio_enabled) return;

    if (!mmio_initialized) return;

    // Sync indices from guest memory
    mmio_sync_from_guest(cpu);

    // Detect spin (no new requests)
    if (mmio_state.req_head == last_req_head && mmio_state.req_tail == last_req_tail) {
        yield_spin_count++;
        if (yield_spin_count == 3) {
            uint32_t *mmio_mem = (uint32_t*)(cpu->mem_base + cpu->mmio_base);
            fprintf(stderr, "DBT: YIELD spin detected! (3 yields with no new requests)\n");
            fprintf(stderr, "  host:  req h=%u t=%u, resp h=%u t=%u\n",
                    mmio_state.req_head, mmio_state.req_tail,
                    mmio_state.resp_head, mmio_state.resp_tail);
            fprintf(stderr, "  guest: req h=%u t=%u, resp h=%u t=%u\n",
                    mmio_mem[S32_MMIO_REQ_HEAD_OFFSET / 4],
                    mmio_mem[S32_MMIO_REQ_TAIL_OFFSET / 4],
                    mmio_mem[S32_MMIO_RESP_HEAD_OFFSET / 4],
                    mmio_mem[S32_MMIO_RESP_TAIL_OFFSET / 4]);
            fprintf(stderr, "  guest PC=0x%08x\n", cpu->pc);
        }
    } else {
        yield_spin_count = 0;
        last_req_head = mmio_state.req_head;
        last_req_tail = mmio_state.req_tail;
    }

    // Process pending requests
    mmio_cpu_iface_t iface = {
        .halted = &cpu->halted,
        .exit_status = &cpu->regs[1]
    };
    mmio_ring_process(&mmio_state, &iface);

    // Sync indices back to guest memory
    mmio_sync_to_guest(cpu);
}

// ============================================================================
// s32x loader (uses shared s32x_loader.h)
// ============================================================================

// Write callback for flat guest memory
static int dbt_write_callback(void *user_data, uint32_t addr, const void *data, uint32_t size) {
    dbt_cpu_state_t *cpu = (dbt_cpu_state_t *)user_data;
    if (size > cpu->mem_size || addr > cpu->mem_size - size) return -1;
    memcpy(cpu->mem_base + addr, data, size);
    return 0;
}

// Wrapper functions for C99 classification macros (not addressable as function pointers)
static int host_isnan(double x) { return isnan(x) ? 1 : 0; }
static int host_isinf(double x) { return isinf(x) ? 1 : 0; }
static int host_isfinite(double x) { return isfinite(x) ? 1 : 0; }

// Math function interception table: maps symbol names to host libm functions
static const struct {
    const char *name;
    void       *host_fn;
    uint8_t     sig;
} math_intercepts[] = {
    // f32 unary: float fn(float)
    { "sqrtf",    (void*)(uintptr_t)sqrtf,    SIG_F32_F32 },
    { "fabsf",    (void*)(uintptr_t)fabsf,    SIG_F32_F32 },
    { "sinf",     (void*)(uintptr_t)sinf,     SIG_F32_F32 },
    { "cosf",     (void*)(uintptr_t)cosf,     SIG_F32_F32 },
    { "tanf",     (void*)(uintptr_t)tanf,     SIG_F32_F32 },
    { "asinf",    (void*)(uintptr_t)asinf,    SIG_F32_F32 },
    { "acosf",    (void*)(uintptr_t)acosf,    SIG_F32_F32 },
    { "atanf",    (void*)(uintptr_t)atanf,    SIG_F32_F32 },
    { "sinhf",    (void*)(uintptr_t)sinhf,    SIG_F32_F32 },
    { "coshf",    (void*)(uintptr_t)coshf,    SIG_F32_F32 },
    { "tanhf",    (void*)(uintptr_t)tanhf,    SIG_F32_F32 },
    { "expf",     (void*)(uintptr_t)expf,     SIG_F32_F32 },
    { "logf",     (void*)(uintptr_t)logf,     SIG_F32_F32 },
    { "log10f",   (void*)(uintptr_t)log10f,   SIG_F32_F32 },
    { "ceilf",    (void*)(uintptr_t)ceilf,    SIG_F32_F32 },
    { "floorf",   (void*)(uintptr_t)floorf,   SIG_F32_F32 },
    { "roundf",   (void*)(uintptr_t)roundf,   SIG_F32_F32 },
    { "truncf",   (void*)(uintptr_t)truncf,   SIG_F32_F32 },
    // f64 unary: double fn(double)
    { "sqrt",     (void*)(uintptr_t)sqrt,     SIG_F64_F64 },
    { "fabs",     (void*)(uintptr_t)fabs,     SIG_F64_F64 },
    { "sin",      (void*)(uintptr_t)sin,      SIG_F64_F64 },
    { "cos",      (void*)(uintptr_t)cos,      SIG_F64_F64 },
    { "tan",      (void*)(uintptr_t)tan,      SIG_F64_F64 },
    { "asin",     (void*)(uintptr_t)asin,     SIG_F64_F64 },
    { "acos",     (void*)(uintptr_t)acos,     SIG_F64_F64 },
    { "atan",     (void*)(uintptr_t)atan,     SIG_F64_F64 },
    { "sinh",     (void*)(uintptr_t)sinh,     SIG_F64_F64 },
    { "cosh",     (void*)(uintptr_t)cosh,     SIG_F64_F64 },
    { "tanh",     (void*)(uintptr_t)tanh,     SIG_F64_F64 },
    { "exp",      (void*)(uintptr_t)exp,      SIG_F64_F64 },
    { "log",      (void*)(uintptr_t)log,      SIG_F64_F64 },
    { "log10",    (void*)(uintptr_t)log10,    SIG_F64_F64 },
    { "ceil",     (void*)(uintptr_t)ceil,     SIG_F64_F64 },
    { "floor",    (void*)(uintptr_t)floor,    SIG_F64_F64 },
    { "round",    (void*)(uintptr_t)round,    SIG_F64_F64 },
    { "trunc",    (void*)(uintptr_t)trunc,    SIG_F64_F64 },
    // f32 binary: float fn(float, float)
    { "fmodf",    (void*)(uintptr_t)fmodf,    SIG_F32_F32_F32 },
    { "powf",     (void*)(uintptr_t)powf,     SIG_F32_F32_F32 },
    { "atan2f",   (void*)(uintptr_t)atan2f,   SIG_F32_F32_F32 },
    { "copysignf",(void*)(uintptr_t)copysignf,SIG_F32_F32_F32 },
    // f64 binary: double fn(double, double)
    { "fmod",     (void*)(uintptr_t)fmod,     SIG_F64_F64_F64 },
    { "pow",      (void*)(uintptr_t)pow,      SIG_F64_F64_F64 },
    { "atan2",    (void*)(uintptr_t)atan2,    SIG_F64_F64_F64 },
    { "copysign", (void*)(uintptr_t)copysign, SIG_F64_F64_F64 },
    // float/int mixed
    { "ldexpf",   (void*)(uintptr_t)ldexpf,   SIG_F32_F32_I32 },
    { "ldexp",    (void*)(uintptr_t)ldexp,    SIG_F64_F64_I32 },
    // Pointer-out
    { "frexpf",   (void*)(uintptr_t)frexpf,   SIG_F32_F32_IPTR2 },
    { "frexp",    (void*)(uintptr_t)frexp,    SIG_F64_F64_IPTR },
    { "modff",    (void*)(uintptr_t)modff,    SIG_F32_F32_FPTR },
    { "modf",     (void*)(uintptr_t)modf,     SIG_F64_F64_DPTR },
    // Int-returning: int fn(double) — use wrappers since C99 macros aren't addressable
    { "isnan",    (void*)(uintptr_t)host_isnan,    SIG_I32_F64 },
    { "isinf",    (void*)(uintptr_t)host_isinf,    SIG_I32_F64 },
    { "isfinite", (void*)(uintptr_t)host_isfinite, SIG_I32_F64 },
    { NULL, NULL, 0 }
};

bool dbt_load_s32x(dbt_cpu_state_t *cpu, const char *filename) {
    // Read and validate header
    s32x_load_result_t hdr = load_s32x_header(filename);
    if (!hdr.success) {
        fprintf(stderr, "%s\n", hdr.error_msg);
        return false;
    }

    // Resize guest memory to match the executable's declared memory size.
    if (hdr.mem_size != 0 && hdr.mem_size != cpu->mem_size) {
        if (cpu->mem_base && cpu->mem_base != MAP_FAILED) {
            munmap(cpu->mem_base, cpu->mem_size);
        }
        cpu->mem_base = mmap(NULL, hdr.mem_size,
                             PROT_READ | PROT_WRITE,
                             MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (cpu->mem_base == MAP_FAILED) {
            perror("mmap guest memory");
            return false;
        }
        cpu->mem_size = hdr.mem_size;
        mmio_initialized = false;
    }

    // Store memory layout
    cpu->code_limit = hdr.code_limit;
    cpu->rodata_limit = hdr.rodata_limit;
    cpu->data_limit = hdr.data_limit;
    cpu->stack_base = hdr.stack_base;

    // Check for flags
    cpu->wxorx_enabled = hdr.has_wxorx;
    cpu->mmio_base = hdr.mmio_base;
    cpu->mmio_enabled = hdr.has_mmio != 0;
    if (cpu->mmio_enabled) {
        if (cpu->mmio_base == 0 ||
            cpu->mmio_base > cpu->mem_size ||
            cpu->mem_size - cpu->mmio_base < 0x10000) {
            fprintf(stderr, "DBT: Invalid MMIO base 0x%08X for mem_size 0x%08X\n",
                    cpu->mmio_base, cpu->mem_size);
            return false;
        }
    }

    // Load sections via shared loader
    s32x_loader_config_t cfg = {
        .write_cb = dbt_write_callback,
        .user_data = cpu,
        .mem_size = cpu->mem_size,
        .verbose = 0
    };

    s32x_load_result_t result = load_s32x_file(filename, &cfg);
    if (!result.success) {
        fprintf(stderr, "%s\n", result.error_msg);
        return false;
    }

    // Initialize entry point and stack pointer from the executable header.
    cpu->pc = result.entry_point;
    cpu->regs[REG_SP] = result.stack_base;
    if (cpu->pc >= result.code_limit) {
        fprintf(stderr, "Invalid entry point 0x%08X outside code segment [0, 0x%08X)\n",
                cpu->pc, result.code_limit);
        return false;
    }

    // Make code section read-only (after loading)
    if (cpu->code_limit > 0 && cpu->wxorx_enabled) {
        mprotect(cpu->mem_base, cpu->code_limit, PROT_READ);
    }

    if (cpu->mmio_enabled && !mmio_initialized) {
        dbt_init_mmio(cpu);
        if (!mmio_initialized) {
            fprintf(stderr, "DBT: Failed to initialize MMIO\n");
            return false;
        }
    }

    // Load symbol table for intrinsic recognition
    cpu->intrinsics_enabled = !intrinsics_disabled;
    if (cpu->intrinsics_enabled) {
        s32x_symtab_result_t st = load_s32x_symtab(filename);
        if (st.success && st.num_symbols > 0) {
            // Look up known intrinsic functions (try multiple name variants)
            static const char *memcpy_names[] = {
                "memcpy", "llvm.memcpy.p0.p0.i32", "llvm.memcpy.p0.p0.i64", NULL
            };
            static const char *memset_names[] = {
                "memset", "llvm.memset.p0.i32", "llvm.memset.p0.i64", NULL
            };
            static const char *memmove_names[] = { "memmove", NULL };
            static const char *strlen_names[] = { "strlen", NULL };
            static const char *memswap_names[] = { "memswap", NULL };

            for (const char **n = memcpy_names; *n; n++) {
                uint32_t addr = s32x_symtab_lookup(&st, *n);
                if (addr) { cpu->intrinsic_memcpy = addr; break; }
            }
            for (const char **n = memset_names; *n; n++) {
                uint32_t addr = s32x_symtab_lookup(&st, *n);
                if (addr) { cpu->intrinsic_memset = addr; break; }
            }
            for (const char **n = memmove_names; *n; n++) {
                uint32_t addr = s32x_symtab_lookup(&st, *n);
                if (addr) { cpu->intrinsic_memmove = addr; break; }
            }
            for (const char **n = strlen_names; *n; n++) {
                uint32_t addr = s32x_symtab_lookup(&st, *n);
                if (addr) { cpu->intrinsic_strlen = addr; break; }
            }
            for (const char **n = memswap_names; *n; n++) {
                uint32_t addr = s32x_symtab_lookup(&st, *n);
                if (addr) { cpu->intrinsic_memswap = addr; break; }
            }

            // Populate math function intercept table
            cpu->num_intercepts = 0;
            for (int i = 0; math_intercepts[i].name != NULL; i++) {
                if (cpu->num_intercepts >= MAX_INTERCEPTS) break;
                uint32_t addr = s32x_symtab_lookup(&st, math_intercepts[i].name);
                if (addr) {
                    cpu->intercepts[cpu->num_intercepts].guest_addr = addr;
                    cpu->intercepts[cpu->num_intercepts].host_fn = math_intercepts[i].host_fn;
                    cpu->intercepts[cpu->num_intercepts].sig = math_intercepts[i].sig;
                    cpu->num_intercepts++;
                }
            }

            s32x_symtab_free(&st);
        } else {
            cpu->intrinsics_enabled = false;
        }
    }

    return true;
}

// ============================================================================
// Dispatcher
// ============================================================================

// Assembly trampoline to call translated code
// Sets up: rbp = &cpu, r14 = cpu->mem_base, r15 = cpu->lookup_table
//
// The translated code expects:
//   rbp = pointer to dbt_cpu_state_t
//   r14 = cpu->mem_base (guest memory base pointer)
//   r15 = cpu->lookup_table (block cache base)
#ifdef __aarch64__
__attribute__((noinline))
static void execute_translated(dbt_cpu_state_t *cpu, translated_block_fn block) {
    uint8_t *mem_base = cpu->mem_base;
    void *lookup_table = cpu->lookup_table;

    // AArch64 trampoline:
    //   x20 = cpu_state pointer (callee-saved)
    //   x21 = mem_base pointer (callee-saved)
    //   x22 = lookup_table pointer (callee-saved)
    //
    // We capture the inputs into caller-saved registers (x0-x3) first,
    // then save callee-saved regs, then set up x20/x21/x22 from the
    // captured values. This avoids the problem of the compiler putting
    // inputs into callee-saved registers that get clobbered by the saves.
    register uint64_t r_cpu     __asm__("x0") = (uint64_t)cpu;
    register uint64_t r_mem     __asm__("x1") = (uint64_t)mem_base;
    register uint64_t r_block   __asm__("x2") = (uint64_t)block;
    register uint64_t r_lookup  __asm__("x3") = (uint64_t)lookup_table;

    __asm__ __volatile__(
        // Save callee-saved registers (x19-x28, fp, lr)
        "stp x29, x30, [sp, #-16]!\n\t"
        "stp x27, x28, [sp, #-16]!\n\t"
        "stp x25, x26, [sp, #-16]!\n\t"
        "stp x23, x24, [sp, #-16]!\n\t"
        "stp x21, x22, [sp, #-16]!\n\t"
        "stp x19, x20, [sp, #-16]!\n\t"

        // Set up translated code register convention from x0-x3
        "mov x20, x0\n\t"       // x20 = cpu
        "mov x21, x1\n\t"       // x21 = mem_base
        "mov x22, x3\n\t"       // x22 = lookup_table

        // Call translated block (address in x2)
        "blr x2\n\t"

        // Restore callee-saved registers
        "ldp x19, x20, [sp], #16\n\t"
        "ldp x21, x22, [sp], #16\n\t"
        "ldp x23, x24, [sp], #16\n\t"
        "ldp x25, x26, [sp], #16\n\t"
        "ldp x27, x28, [sp], #16\n\t"
        "ldp x29, x30, [sp], #16\n\t"

        : // No outputs
        : "r" (r_cpu),          // x0 = cpu
          "r" (r_mem),          // x1 = mem_base
          "r" (r_block),        // x2 = block
          "r" (r_lookup)        // x3 = lookup_table
        : "x4", "x5", "x6", "x7",
          "x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
          "x16", "x17", "x18",
          "memory", "cc"
    );
}
#else  // x86-64
__attribute__((noinline))
static void execute_translated(dbt_cpu_state_t *cpu, translated_block_fn block) {
    // Get mem_base into a local before inline asm
    uint8_t *mem_base = cpu->mem_base;
    void *lookup_table = cpu->lookup_table;

    // Use specific registers for inputs to avoid conflicts
    // rax = cpu, rcx = mem_base, rdx = block
    // These are caller-saved so we don't need to preserve them
    __asm__ __volatile__(
        // Save callee-saved registers that we'll clobber
        "push %%rbp\n\t"
        "push %%rbx\n\t"
        "push %%r12\n\t"
        "push %%r13\n\t"
        "push %%r14\n\t"
        "push %%r15\n\t"

        // Set up our calling convention (inputs are in rax, rcx, rdx)
        "mov %%rax, %%rbp\n\t"   // rbp = cpu
        "mov %%rcx, %%r14\n\t"   // r14 = mem_base
        "mov %%rsi, %%r15\n\t"   // r15 = lookup_table

        // Call translated block (address in rdx)
        "call *%%rdx\n\t"

        // Restore callee-saved registers
        "pop %%r15\n\t"
        "pop %%r14\n\t"
        "pop %%r13\n\t"
        "pop %%r12\n\t"
        "pop %%rbx\n\t"
        "pop %%rbp\n\t"

        : // No outputs
        : "a" (cpu),          // rax = cpu
          "c" (mem_base),     // rcx = mem_base
          "d" (block),        // rdx = block
          "S" (lookup_table)  // rsi = lookup_table
        : "rdi", "r8", "r9", "r10", "r11",
          "memory", "cc"
    );
}
#endif // __aarch64__

// Stage 1 dispatcher (no caching)
static void run_dbt_stage1(dbt_cpu_state_t *cpu) {
    translate_ctx_t ctx;
    translate_init(&ctx, cpu);
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Translate block at current PC
        translated_block_fn block;
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                block = translate_block(&ctx);
                profile_translate_cycles += rdtsc() - t0;
            } else {
                block = translate_block(&ctx);
            }
#else
            block = translate_block(&ctx);
#endif
        } else {
            block = translate_block(&ctx);
        }

        // Execute translated code
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_translated(cpu, block);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_translated(cpu, block);
            }
#else
            execute_translated(cpu, block);
#endif
        } else {
            execute_translated(cpu, block);
        }

        dispatch_iter++;

        // Check probe flag
        if (__builtin_expect(g_dbt_probe && g_dbt_probe->flag, 0)) {
            dbt_process_probe(g_dbt_probe, cpu);
        }

        // Handle exit reason
        switch (cpu->exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
            case EXIT_BLOCK_END:
                // Continue to next block
                break;

            case EXIT_HALT:
                // Process any final MMIO before halting
                dbt_handle_yield(cpu);
                cpu->halted = true;
                break;

            case EXIT_DEBUG:
                putchar(cpu->exit_info & 0xFF);
                fflush(stdout);
                break;

            case EXIT_YIELD:
                dbt_handle_yield(cpu);
                break;

            case EXIT_FAULT_FETCH:
            case EXIT_FAULT_LOAD:
            case EXIT_FAULT_STORE:
                fprintf(stderr, "DBT: Memory fault at PC=0x%08X, addr=0x%08X\n",
                        cpu->pc, cpu->exit_info);
                cpu->halted = true;
                break;

            case EXIT_ASSERT_FAIL:
                fprintf(stderr, "DBT: ASSERT_EQ failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;

            default:
                fprintf(stderr, "DBT: Unknown exit reason %u at PC=0x%08X\n",
                        cpu->exit_reason, cpu->pc);
                cpu->halted = true;
                break;
        }
    }
}

static inline void execute_cached_block(dbt_cpu_state_t *cpu, translated_block_t *block) {
    g_dbt_block_pc = block ? block->guest_pc : 0;
    g_dbt_block_code = (uintptr_t)(block ? block->host_code : NULL);
    g_dbt_block_size = block ? block->host_size : 0;
    execute_translated(cpu, (translated_block_fn)(block ? block->host_code : NULL));
}

// Stage 2 dispatcher (with block cache and chaining)
static void run_dbt_stage2(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    translate_ctx_t ctx;
    translate_init_cached(&ctx, cpu, cache);
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Look up block in cache
        translated_block_t *block = cache_lookup(cache, cpu->pc);

        if (!block) {
            // Translate and cache new block
            if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
                bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
                if (sample_now) {
                    uint64_t t0 = rdtsc();
                    block = translate_block_cached(&ctx, cpu->pc);
                    profile_translate_cycles += rdtsc() - t0;
                } else {
                    block = translate_block_cached(&ctx, cpu->pc);
                }
#else
                block = translate_block_cached(&ctx, cpu->pc);
#endif
            } else {
                block = translate_block_cached(&ctx, cpu->pc);
            }
            if (!block) {
                fprintf(stderr, "DBT: Translation failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;
            }
        }

        // Execute translated code
        // With chaining, this may execute multiple blocks before returning
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_cached_block(cpu, block);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_cached_block(cpu, block);
            }
#else
            execute_cached_block(cpu, block);
#endif
        } else {
            execute_cached_block(cpu, block);
        }

        dbt_trace_branch_exit(cache, block, cpu);

        dispatch_iter++;

        // Check probe flag
        if (__builtin_expect(g_dbt_probe && g_dbt_probe->flag, 0)) {
            dbt_process_probe(g_dbt_probe, cpu);
        }

        // Handle exit reason
        switch (cpu->exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
            case EXIT_BLOCK_END:
                // Continue to next block
                break;

            case EXIT_HALT:
                // Process any final MMIO before halting
                dbt_handle_yield(cpu);
                cpu->halted = true;
                break;

            case EXIT_DEBUG:
                putchar(cpu->exit_info & 0xFF);
                fflush(stdout);
                break;

            case EXIT_YIELD:
                dbt_handle_yield(cpu);
                break;

            case EXIT_FAULT_FETCH:
            case EXIT_FAULT_LOAD:
            case EXIT_FAULT_STORE:
                fprintf(stderr, "DBT: Memory fault at PC=0x%08X, addr=0x%08X\n",
                        cpu->pc, cpu->exit_info);
                cpu->halted = true;
                break;

            case EXIT_ASSERT_FAIL:
                fprintf(stderr, "DBT: ASSERT_EQ failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;

            default:
                fprintf(stderr, "DBT: Unknown exit reason %u at PC=0x%08X\n",
                        cpu->exit_reason, cpu->pc);
                cpu->halted = true;
                break;
        }
    }
}

// Stage 3 dispatcher (with inline indirect branch lookup)
static void run_dbt_stage3(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    translate_ctx_t ctx;
    translate_init_cached(&ctx, cpu, cache);
    ctx.inline_lookup_enabled = true;  // Enable Stage 3 inline lookup
    ctx.ras_enabled = true;             // Enable Stage 3 Phase 2 RAS
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Look up block in cache
        translated_block_t *block = cache_lookup(cache, cpu->pc);

        if (!block) {
            // Translate and cache new block
            if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
                bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
                if (sample_now) {
                    uint64_t t0 = rdtsc();
                    block = translate_block_cached(&ctx, cpu->pc);
                    profile_translate_cycles += rdtsc() - t0;
                } else {
                    block = translate_block_cached(&ctx, cpu->pc);
                }
#else
                block = translate_block_cached(&ctx, cpu->pc);
#endif
            } else {
                block = translate_block_cached(&ctx, cpu->pc);
            }
            if (!block) {
                fprintf(stderr, "DBT: Translation failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;
            }
        }

        // Execute translated code
        // With chaining + inline lookup, this may execute many blocks
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_cached_block(cpu, block);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_cached_block(cpu, block);
            }
#else
            execute_cached_block(cpu, block);
#endif
        } else {
            execute_cached_block(cpu, block);
        }

        dbt_trace_branch_exit(cache, block, cpu);

        dispatch_iter++;

        // Check probe flag
        if (__builtin_expect(g_dbt_probe && g_dbt_probe->flag, 0)) {
            dbt_process_probe(g_dbt_probe, cpu);
        }

        // Track indirect branch statistics when we return to dispatcher
        if (cpu->exit_reason == EXIT_INDIRECT) {
            cache->inline_miss_count++;
        }
        if (block->side_exit_count > 0) {
            for (int i = 0; i < block->side_exit_count; i++) {
                uint32_t pc = block->side_exit_pcs[i];
                uint32_t idx = cache_hash(pc);
                cache->side_exit_total_profile[idx]++;
                cache->side_exit_pc_hint[idx] = pc;
                // Track unique PCs for reporting
                bool seen = false;
                for (uint32_t j = 0; j < cache->side_exit_pc_count; j++) {
                    if (cache->side_exit_pc_list[j] == pc) {
                        seen = true;
                        break;
                    }
                }
                if (!seen && cache->side_exit_pc_count < 1024) {
                    cache->side_exit_pc_list[cache->side_exit_pc_count++] = pc;
                }
            }
        }
        if (cpu->exit_reason == EXIT_BRANCH && cpu->exit_info != 0) {
            uint32_t pc = cpu->exit_info;
            uint32_t idx = cache_hash(pc);
            cache->side_exit_taken_profile[idx]++;
            cache->side_exit_pc_hint[idx] = pc;
            bool seen = false;
            for (uint32_t j = 0; j < cache->side_exit_pc_count; j++) {
                if (cache->side_exit_pc_list[j] == pc) {
                    seen = true;
                    break;
                }
            }
            if (!seen && cache->side_exit_pc_count < 1024) {
                cache->side_exit_pc_list[cache->side_exit_pc_count++] = pc;
            }
            cpu->exit_info = 0;
        }

        // Handle exit reason
        switch (cpu->exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
            case EXIT_BLOCK_END:
                // Continue to next block
                break;

            case EXIT_HALT:
                // Process any final MMIO before halting
                dbt_handle_yield(cpu);
                cpu->halted = true;
                break;

            case EXIT_DEBUG:
                putchar(cpu->exit_info & 0xFF);
                fflush(stdout);
                break;

            case EXIT_YIELD:
                dbt_handle_yield(cpu);
                break;

            case EXIT_FAULT_FETCH:
            case EXIT_FAULT_LOAD:
            case EXIT_FAULT_STORE:
                fprintf(stderr, "DBT: Memory fault at PC=0x%08X, addr=0x%08X\n",
                        cpu->pc, cpu->exit_info);
                if (trace_branch_exit_enabled && block) {
                    uint32_t guest_end = block->guest_pc + block->guest_size;
                    fprintf(stderr,
                            "dbt-fault-context block_pc=0x%08X block_end=0x%08X host_size=%u exits=%u side_exits=%u\n",
                            block->guest_pc, guest_end, block->host_size,
                            block->exit_count, block->side_exit_count);
                }
                cpu->halted = true;
                break;

            case EXIT_ASSERT_FAIL:
                fprintf(stderr, "DBT: ASSERT_EQ failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;

            default:
                fprintf(stderr, "DBT: Unknown exit reason %u at PC=0x%08X\n",
                        cpu->exit_reason, cpu->pc);
                cpu->halted = true;
                break;
        }
    }
}

// Stage 4/5 dispatcher (with superblock extension)
static void run_dbt_stage4plus(dbt_cpu_state_t *cpu, block_cache_t *cache,
                               bool strict_carry, bool stage5_mode) {
    translate_ctx_t ctx;
    translate_init_cached(&ctx, cpu, cache);
    ctx.inline_lookup_enabled = true;   // Keep Stage 3 inline lookup
    ctx.ras_enabled = true;              // Keep Stage 3 RAS
    ctx.superblock_enabled = superblock_enabled; // Enable Stage 4 superblock extension
    ctx.profile_side_exits = profile_side_exits;
    ctx.side_exit_info_enabled = profile_side_exits;   // Study-only diagnostics
    ctx.avoid_backedge_extend = avoid_backedge_extend;
    ctx.peephole_enabled = peephole_enabled;
    ctx.reg_cache_enabled = reg_cache_enabled;
    ctx.strict_carry = strict_carry;
    ctx.stage5_burg_enabled = stage5_mode && stage5_burg_hook_enabled;
    ctx.stage5_emit_enabled = stage5_mode && stage5_emit_hook_enabled;
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Look up block in cache
        translated_block_t *block = cache_lookup(cache, cpu->pc);

        if (!block) {
            // Reset superblock depth for new translation
            ctx.superblock_depth = 0;

            // Translate and cache new block (may be a superblock)
            if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
                bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
                if (sample_now) {
                    uint64_t t0 = rdtsc();
                    block = translate_block_cached(&ctx, cpu->pc);
                    profile_translate_cycles += rdtsc() - t0;
                } else {
                    block = translate_block_cached(&ctx, cpu->pc);
                }
#else
                block = translate_block_cached(&ctx, cpu->pc);
#endif
            } else {
                block = translate_block_cached(&ctx, cpu->pc);
            }
            if (!block) {
                fprintf(stderr, "DBT: Translation failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;
            }
        }

        dbt_trace_block_exits(block);
        dbt_trace_block_regs(cpu, block);

        // Execute translated code
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_cached_block(cpu, block);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_cached_block(cpu, block);
            }
#else
            execute_cached_block(cpu, block);
#endif
        } else {
            execute_cached_block(cpu, block);
        }

        dbt_trace_branch_exit(cache, block, cpu);

        dispatch_iter++;

        // Check probe flag
        if (__builtin_expect(g_dbt_probe && g_dbt_probe->flag, 0)) {
            dbt_process_probe(g_dbt_probe, cpu);
        }

        // Track indirect branch statistics when we return to dispatcher
        if (cpu->exit_reason == EXIT_INDIRECT) {
            cache->inline_miss_count++;
        }
        if (profile_side_exits && block->side_exit_count > 0) {
            for (int i = 0; i < block->side_exit_count; i++) {
                uint32_t pc = block->side_exit_pcs[i];
                cache->side_exit_total_profile[cache_hash(pc)]++;
            }
            // Identify which side exit was taken by matching cpu->pc
            // against the block's exit targets (no longer stored in exit_info)
            if (cpu->exit_reason == EXIT_BRANCH) {
                for (int i = 0; i < block->exit_count; i++) {
                    if (block->exits[i].target_pc == cpu->pc &&
                        block->exits[i].branch_pc != 0) {
                        uint32_t bpc = block->exits[i].branch_pc;
                        cache->side_exit_taken_profile[cache_hash(bpc)]++;
                        break;
                    }
                }
            }
        }

        // Handle exit reason
        switch (cpu->exit_reason) {
            case EXIT_BRANCH:
            case EXIT_INDIRECT:
            case EXIT_BLOCK_END:
                // Continue to next block
                break;

            case EXIT_HALT:
                // Process any final MMIO before halting
                dbt_handle_yield(cpu);
                cpu->halted = true;
                break;

            case EXIT_DEBUG:
                putchar(cpu->exit_info & 0xFF);
                fflush(stdout);
                break;

            case EXIT_YIELD:
                dbt_handle_yield(cpu);
                break;

            case EXIT_FAULT_FETCH:
            case EXIT_FAULT_LOAD:
            case EXIT_FAULT_STORE:
                fprintf(stderr, "DBT: Memory fault at PC=0x%08X, addr=0x%08X\n",
                        cpu->pc, cpu->exit_info);
                if (trace_branch_exit_enabled && block) {
                    uint32_t guest_end = block->guest_pc + block->guest_size;
                    fprintf(stderr,
                            "dbt-fault-context block_pc=0x%08X block_end=0x%08X host_size=%u exits=%u side_exits=%u\n",
                            block->guest_pc, guest_end, block->host_size,
                            block->exit_count, block->side_exit_count);
                }
                cpu->halted = true;
                break;

            case EXIT_ASSERT_FAIL:
                fprintf(stderr, "DBT: ASSERT_EQ failed at PC=0x%08X\n", cpu->pc);
                cpu->halted = true;
                break;

            default:
                fprintf(stderr, "DBT: Unknown exit reason %u at PC=0x%08X\n",
                        cpu->exit_reason, cpu->pc);
                cpu->halted = true;
                break;
        }
    }
}

static void run_dbt_stage4(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    run_dbt_stage4plus(cpu, cache, strict_carry_enabled, false);
}

static void run_dbt_stage5(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    // Stage 5 starts from Stage 4 pipeline with strict carry-flag guardrails.
    run_dbt_stage4plus(cpu, cache, true, true);
}

// ============================================================================
// Main
// ============================================================================

static void usage(const char *prog) {
    fprintf(stderr, "SLOW-32 Dynamic Binary Translator\n");
    fprintf(stderr, "Usage: %s [options] <program.s32x>\n", prog);
    fprintf(stderr, "\nOptions:\n");
    fprintf(stderr, "  -h        Show this help\n");
    fprintf(stderr, "  -v        Verbose output\n");
    fprintf(stderr, "  -s        Show statistics on exit\n");
    fprintf(stderr, "  -p        Profile timing breakdown (implies -s)\n");
    fprintf(stderr, "  -E        Study-only: enable side-exit bookkeeping\n");
    fprintf(stderr, "  -B        Study-only: avoid extending across back-edges\n");
    fprintf(stderr, "  -P        Disable peephole optimization on emitted x86-64\n");
    fprintf(stderr, "  -S        Toggle superblock expansion (default: on)\n");
    fprintf(stderr, "  -R        Toggle fixed register cache (default: on)\n");
    fprintf(stderr, "  -C        Enable strict carry guardrails (disable unsigned cmp-branch fusion)\n");
    fprintf(stderr, "  -G        Enable Stage 5 lift/BURG hook telemetry (default: off)\n");
    fprintf(stderr, "  -W        Enable Stage 5 pilot emission (small terminal families)\n");
    fprintf(stderr, "  -N        Enable Stage 5 native benchmark profile\n");
    fprintf(stderr, "  -t        Two-pass superblock profiling (Stage 4/5 only)\n");
    fprintf(stderr, "  -M <n>    Min samples before using side-exit rate\n");
    fprintf(stderr, "  -T <pct>  Max taken %% to allow superblock extension\n");
    fprintf(stderr, "  -D        Dump top offender blocks\n");
    fprintf(stderr, "  -O        Disassemble offender host blocks (uses objdump)\n");
    fprintf(stderr, "  -X <pc>   Dump block containing guest PC\n");
    fprintf(stderr, "  -I        Disable intrinsic recognition (memcpy/memset native stubs)\n");
    fprintf(stderr, "  -U        UNSAFE: Disable all bounds/W^X checks (for benchmarking)\n");
    fprintf(stderr, "  -q [N]    Probe: sample PC every N seconds (default 1) via SIGALRM\n");
    fprintf(stderr, "  -1        Use Stage 1 mode (no caching)\n");
    fprintf(stderr, "  -2        Use Stage 2 mode (block cache + chaining)\n");
    fprintf(stderr, "  -3        Use Stage 3 mode (inline indirect lookup)\n");
    fprintf(stderr, "  -4        Use Stage 4 mode (superblock extension, default)\n");
    fprintf(stderr, "  -5        Use Stage 5 mode (Stage 4 + strict carry guardrails)\n");
    fprintf(stderr, "  --allow <list>  Only allow these MMIO services (comma-separated)\n");
    fprintf(stderr, "  --deny <list>   Deny these MMIO services (comma-separated)\n");
    fprintf(stderr, "\nEnvironment:\n");
    fprintf(stderr, "  SLOW32_DBT_ALIGN_TRAP=1  Trap on unaligned LD/ST/fetch\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_VALIDATE_LIFT=1  Validate lift semantics against decoded steps\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_VALIDATE_ABORT=1 Abort on first validation mismatch\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_VALIDATE_REQUIRE=1 Return non-zero if validator reports mismatches\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_EMIT_CALLS=1  Allow Stage5 emit for terminal JAL call regions\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_CODEGEN_CMP_RR=1  Allow Stage5 native codegen for compare rr ops\n");
    fprintf(stderr, "  SLOW32_DBT_STAGE5_CODEGEN_CMP_RI=1  Allow Stage5 native codegen for compare imm ops\n");
}

static void parse_service_list(const char *list, char names[][S32_MAX_SVC_NAME], int *count, int max) {
    char buf[256];
    strncpy(buf, list, sizeof(buf) - 1);
    buf[sizeof(buf) - 1] = '\0';
    char *saveptr = NULL;
    char *tok = strtok_r(buf, ",", &saveptr);
    while (tok && *count < max) {
        strncpy(names[*count], tok, S32_MAX_SVC_NAME - 1);
        names[*count][S32_MAX_SVC_NAME - 1] = '\0';
        (*count)++;
        tok = strtok_r(NULL, ",", &saveptr);
    }
}

static void print_top_opcode_hist(const char *label, const uint32_t hist[128]) {
    uint32_t top_count[3] = {0, 0, 0};
    uint32_t top_opcode[3] = {0, 0, 0};
    for (uint32_t op = 0; op < 128; op++) {
        uint32_t c = hist[op];
        if (c == 0) continue;
        for (int k = 0; k < 3; k++) {
            if (c > top_count[k]) {
                for (int m = 2; m > k; m--) {
                    top_count[m] = top_count[m - 1];
                    top_opcode[m] = top_opcode[m - 1];
                }
                top_count[k] = c;
                top_opcode[k] = op;
                break;
            }
        }
    }
    for (int k = 0; k < 3; k++) {
        if (top_count[k] == 0) break;
        fprintf(stderr, "  %s top%d: op=0x%02X count=%" PRIu32 "\n",
                label, k + 1, top_opcode[k], top_count[k]);
    }
}

int main(int argc, char **argv) {
    /* Endianness check: SLOW-32 is Little-Endian and this emulator 
     * relies on host endianness for code translation performance. */
    {
        uint32_t test = 1;
        if (*(uint8_t *)&test != 1) {
            fprintf(stderr, "Error: This emulator only supports Little-Endian host platforms.\n");
            return 1;
        }
    }

    bool verbose = false;
    bool show_stats = false;
    bool two_pass = false;
    bool two_pass_forced = false;
    bool dump_offenders = false;
    bool disassemble_offenders = false;
    uint32_t dump_pc = 0;
    bool stage5_native_bench_profile = false;
    int stage = 4;  // Default to Stage 4 now
    const char *filename = NULL;
    dbt_probe_state_t probe = {0};
    probe.probe_interval_sec = 1;
    const char *trace_env = getenv("SLOW32_DBT_EMIT_TRACE");
    const char *trace_pc_env = getenv("SLOW32_DBT_EMIT_TRACE_PC");
    const char *align_env = getenv("SLOW32_DBT_ALIGN_TRAP");
    const char *validate_require_env = getenv("SLOW32_DBT_STAGE5_VALIDATE_REQUIRE");
    bool validate_require_clean =
        (validate_require_env && validate_require_env[0] != '\0' &&
         strcmp(validate_require_env, "0") != 0);
    bool emit_trace = (trace_env && atoi(trace_env) != 0);
    if (align_env && atoi(align_env) != 0) {
        align_traps_enabled = true;
    }
    uint32_t emit_trace_pc = 0;
    if (trace_pc_env && trace_pc_env[0] != '\0') {
        emit_trace_pc = (uint32_t)strtoul(trace_pc_env, NULL, 0);
    }

    // Pre-scan for --allow/--deny (strip before single-char parser)
    svc_policy_t svc_policy = { .default_allow = true };
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--allow") == 0 && i + 1 < argc) {
            parse_service_list(argv[i + 1], svc_policy.allow_list, &svc_policy.allow_count, S32_MAX_SERVICES);
            memmove(&argv[i], &argv[i + 2], (argc - i - 2) * sizeof(char *));
            argc -= 2;
            i--;
        } else if (strcmp(argv[i], "--deny") == 0 && i + 1 < argc) {
            parse_service_list(argv[i + 1], svc_policy.deny_list, &svc_policy.deny_count, S32_MAX_SERVICES);
            memmove(&argv[i], &argv[i + 2], (argc - i - 2) * sizeof(char *));
            argc -= 2;
            i--;
        }
    }

    // Parse arguments
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            switch (argv[i][1]) {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'v':
                    verbose = true;
                    break;
                case 's':
                    show_stats = true;
                    break;
                case 'p':
                    profile_timing = true;
                    show_stats = true;
                    break;
                case 'E':
                    profile_side_exits = true;
                    break;
                case 'B':
                    avoid_backedge_extend = true;
                    break;
                case 'P':
                    peephole_enabled = false;
                    break;
                case 'S':
                    superblock_enabled = !superblock_enabled;
                    break;
                case 'R':
                    reg_cache_enabled = !reg_cache_enabled;
                    break;
                case 'C':
                    strict_carry_enabled = true;
                    break;
                case 'G':
                    stage5_burg_hook_enabled = true;
                    break;
                case 'W':
                    stage5_emit_hook_enabled = true;
                    break;
                case 'N':
                    stage5_native_bench_profile = true;
                    apply_stage5_native_bench_profile(&stage, &two_pass, &two_pass_forced, &show_stats);
                    break;
                case 'I':
                    intrinsics_disabled = true;
                    break;
                case 'U':
                    bounds_checks_disabled = true;
                    break;
                case 'q':
                    probe.enabled = true;
                    // Optional numeric argument for interval
                    if (i + 1 < argc && argv[i + 1][0] >= '0' && argv[i + 1][0] <= '9') {
                        probe.probe_interval_sec = atoi(argv[++i]);
                        if (probe.probe_interval_sec < 1) probe.probe_interval_sec = 1;
                    }
                    break;
                case 't':
                    two_pass = true;
                    two_pass_forced = true;
                    break;
                case 'M':
                    if (i + 1 < argc) {
                        superblock_profile_min_samples = (uint32_t)atoi(argv[++i]);
                    } else {
                        fprintf(stderr, "Option -M requires a value\n");
                        return 1;
                    }
                    break;
                case 'T':
                    if (i + 1 < argc) {
                        superblock_taken_pct_threshold = (uint32_t)atoi(argv[++i]);
                    } else {
                        fprintf(stderr, "Option -T requires a value\n");
                        return 1;
                    }
                    break;
                case 'D':
                    dump_offenders = true;
                    break;
                case 'O':
                    disassemble_offenders = true;
                    break;
                case 'X':
                    if (i + 1 < argc) {
                        dump_pc = (uint32_t)strtoul(argv[++i], NULL, 0);
                    } else {
                        fprintf(stderr, "Option -X requires a value\n");
                        return 1;
                    }
                    break;
                case '1':
                    stage = 1;
                    break;
                case '2':
                    stage = 2;
                    break;
                case '3':
                    stage = 3;
                    break;
                case '4':
                    stage = 4;
                    break;
                case '5':
                    stage = 5;
                    break;
                default:
                    fprintf(stderr, "Unknown option: %s\n", argv[i]);
                    usage(argv[0]);
                    return 1;
            }
        } else {
            // First non-option is the filename, rest are guest args
            filename = argv[i];
            // Capture remaining args for guest program (including filename as argv[0])
            host_argc = argc - i;
            host_argv = &argv[i];
            break;  // Stop parsing options
        }
    }

    if (!filename) {
        usage(argv[0]);
        return 1;
    }

    if (emit_trace && emit_trace_pc == 0 && dump_pc != 0) {
        emit_trace_pc = dump_pc;
    }
    dbt_set_emit_trace(emit_trace, emit_trace_pc);
    dbt_init_branch_trace_from_env();

    // Initialize CPU
    dbt_cpu_state_t cpu;
    dbt_cpu_init(&cpu);
    cpu.align_traps_enabled = align_traps_enabled;
    cpu.bounds_checks_disabled = bounds_checks_disabled;

    // Load program
    if (!dbt_load_s32x(&cpu, filename)) {
        dbt_cpu_destroy(&cpu);
        return 1;
    }

    // Apply service policy
    if (cpu.mmio_enabled && mmio_initialized &&
        (svc_policy.allow_count > 0 || svc_policy.deny_count > 0)) {
        mmio_set_policy(&mmio_state, &svc_policy);
    }

    if (verbose) {
        fprintf(stderr, "Loaded: %s\n", filename);
        fprintf(stderr, "  Code limit:   0x%08X\n", cpu.code_limit);
        fprintf(stderr, "  Rodata limit: 0x%08X\n", cpu.rodata_limit);
        fprintf(stderr, "  Data limit:   0x%08X\n", cpu.data_limit);
        fprintf(stderr, "  Stack base:   0x%08X\n", cpu.stack_base);
        if (cpu.mmio_enabled) {
            fprintf(stderr, "  MMIO base:    0x%08X\n", cpu.mmio_base);
        }
        fprintf(stderr, "  Stage:        %d\n", stage);
        if (stage >= 4 || strict_carry_enabled) {
            fprintf(stderr, "  Strict carry: %s\n",
                    (stage == 5 || strict_carry_enabled) ? "enabled" : "disabled");
        }
        if (stage == 5) {
            fprintf(stderr, "  Stage5 hook:  %s\n",
                    stage5_burg_hook_enabled ? "enabled" : "disabled");
            fprintf(stderr, "  Stage5 emit:  %s\n",
                    stage5_emit_hook_enabled ? "enabled" : "disabled");
            fprintf(stderr, "  Stage5 bench: %s\n",
                    stage5_native_bench_profile ? "enabled" : "disabled");
        }
        if (cpu.intrinsics_enabled) {
            fprintf(stderr, "  Intrinsics:   enabled\n");
            if (cpu.intrinsic_memcpy)  fprintf(stderr, "    memcpy:  0x%08X\n", cpu.intrinsic_memcpy);
            if (cpu.intrinsic_memset)  fprintf(stderr, "    memset:  0x%08X\n", cpu.intrinsic_memset);
            if (cpu.intrinsic_memmove) fprintf(stderr, "    memmove: 0x%08X\n", cpu.intrinsic_memmove);
            if (cpu.intrinsic_strlen)  fprintf(stderr, "    strlen:  0x%08X\n", cpu.intrinsic_strlen);
            if (cpu.intrinsic_memswap) fprintf(stderr, "    memswap: 0x%08X\n", cpu.intrinsic_memswap);
            if (cpu.num_intercepts > 0)
                fprintf(stderr, "    math intercepts: %d functions\n", cpu.num_intercepts);
        }
    }

    // Initialize block cache for Stage 2 and 3
    static block_cache_t cache;
    if (stage >= 2) {
        if (!cache_init(&cache)) {
            fprintf(stderr, "Failed to initialize block cache\n");
            dbt_cpu_destroy(&cpu);
            return 1;
        }
        g_dbt_code_base = (uintptr_t)cache.code_buffer;
        g_dbt_code_limit = (uintptr_t)(cache.code_buffer + cache.code_buffer_size);
        // Stage 3: Set up inline lookup table pointer in CPU state
        cpu.lookup_table = cache.blocks;
        cpu.lookup_mask = BLOCK_CACHE_MASK;
    }

    // Run
    struct timespec start, end;

    if (two_pass && stage != 4 && stage != 5 && !two_pass_forced) {
        two_pass = false;
    }

    if (two_pass) {
        if (stage != 4 && stage != 5) {
            fprintf(stderr, "Two-pass profiling is only supported for Stage 4/5.\n");
            dbt_cpu_destroy(&cpu);
            if (stage >= 2) {
                cache_destroy(&cache);
            }
            return 1;
        }

        if (verbose) {
            fprintf(stderr, "Two-pass profiling: pass 1 (collect side-exit hotness)\n");
        }
        // Pass 1: collect side-exit hotness without timing overhead
        bool saved_profile = profile_timing;
        profile_timing = false;
        profile_side_exits = true;
        if (stage == 5) {
            run_dbt_stage5(&cpu, &cache);
        } else {
            run_dbt_stage4(&cpu, &cache);
        }
        profile_side_exits = false;
        profile_timing = saved_profile;
        if (verbose) {
            uint32_t max_total = 0;
            uint32_t max_taken = 0;
            for (uint32_t i = 0; i < BLOCK_CACHE_SIZE; i++) {
                if (cache.side_exit_total_profile[i] > max_total) {
                    max_total = cache.side_exit_total_profile[i];
                }
                if (cache.side_exit_taken_profile[i] > max_taken) {
                    max_taken = cache.side_exit_taken_profile[i];
                }
            }
            fprintf(stderr, "Two-pass profiling: max side-exit totals=%u taken=%u\n",
                    max_total, max_taken);
            fprintf(stderr, "Two-pass profiling: side-exit PC count=%u\n",
                    cache.side_exit_pc_count);
        }

        // Reset CPU and reload program for pass 2
        dbt_cpu_destroy(&cpu);
        dbt_cpu_init(&cpu);
        cpu.align_traps_enabled = align_traps_enabled;
        cpu.bounds_checks_disabled = bounds_checks_disabled;
        if (!dbt_load_s32x(&cpu, filename)) {
            if (stage >= 2) {
                cache_destroy(&cache);
            }
            dbt_cpu_destroy(&cpu);
            return 1;
        }

        // Flush cache but keep side-exit profile
        uint32_t saved_side_exit_pc_count = cache.side_exit_pc_count;
        uint32_t saved_side_exit_pc_list[1024];
        if (saved_side_exit_pc_count > 0) {
            memcpy(saved_side_exit_pc_list, cache.side_exit_pc_list,
                   sizeof(uint32_t) * saved_side_exit_pc_count);
        }

        cache_flush(&cache);
        cache_reset_stats(&cache);
        cache.side_exit_pc_count = saved_side_exit_pc_count;
        if (saved_side_exit_pc_count > 0) {
            memcpy(cache.side_exit_pc_list, saved_side_exit_pc_list,
                   sizeof(uint32_t) * saved_side_exit_pc_count);
        }
        cpu.lookup_table = cache.blocks;
        cpu.lookup_mask = BLOCK_CACHE_MASK;

        if (verbose) {
            fprintf(stderr, "Two-pass profiling: pass 2 (profile-guided)\n");
        }
    }

    profile_translate_cycles = 0;
    profile_exec_cycles = 0;
    profile_tsc_start = 0;
    profile_tsc_end = 0;

    // Install SIGINT handler (always — Ctrl+C dumps registers and halts)
    g_dbt_cpu = &cpu;
    g_dbt_probe = &probe;
    dbt_install_sigint();
    dbt_install_sigfault();

    // Start probe timer if requested
    if (probe.enabled) {
        fprintf(stderr, "DBT probe enabled: sampling every %d second(s)\n", probe.probe_interval_sec);
        clock_gettime(CLOCK_MONOTONIC, &probe.start_time);
        dbt_start_probe_timer(probe.probe_interval_sec);
    }

    clock_gettime(CLOCK_MONOTONIC, &start);
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
    if (profile_timing) {
        profile_tsc_start = rdtsc();
    }
#endif

    if (stage == 1) {
        run_dbt_stage1(&cpu);
    } else if (stage == 2) {
        run_dbt_stage2(&cpu, &cache);
    } else if (stage == 3) {
        run_dbt_stage3(&cpu, &cache);
    } else if (stage == 5) {
        run_dbt_stage5(&cpu, &cache);
    } else {
        run_dbt_stage4(&cpu, &cache);
    }

    // Stop probe timer before any output
    if (probe.enabled) {
        dbt_stop_probe_timer();
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
    if (profile_timing) {
        profile_tsc_end = rdtsc();
    }
#endif

    // SIGINT register dump
    if (probe.sigint_flag) {
        fprintf(stderr, "\nInterrupted (SIGINT) — register dump:\n");
        fprintf(stderr, "  PC=0x%08X  SP=0x%08X  FP=0x%08X  LR=0x%08X\n",
                cpu.pc, cpu.regs[29], cpu.regs[30], cpu.regs[31]);
        int ri;
        for (ri = 0; ri < 32; ri += 4) {
            fprintf(stderr, "  r%-2d=%08X  r%-2d=%08X  r%-2d=%08X  r%-2d=%08X\n",
                    ri, cpu.regs[ri], ri+1, cpu.regs[ri+1],
                    ri+2, cpu.regs[ri+2], ri+3, cpu.regs[ri+3]);
        }
    }

    g_dbt_cpu = NULL;
    g_dbt_probe = NULL;
    g_dbt_code_base = 0;
    g_dbt_code_limit = 0;
    g_dbt_block_pc = 0;
    g_dbt_block_code = 0;
    g_dbt_block_size = 0;

    // Results
    int exit_code = cpu.regs[REG_RV];
    if (validate_require_clean && stage5_validate_mismatch > 0) {
        fprintf(stderr, "Stage5 validate require-clean failed: mismatches=%" PRIu32 "\n",
                stage5_validate_mismatch);
        if (exit_code == 0) {
            exit_code = 2;
        }
    }

    if (show_stats || verbose) {
        double elapsed = (end.tv_sec - start.tv_sec) +
                        (end.tv_nsec - start.tv_nsec) / 1e9;
        fprintf(stderr, "\n--- Statistics ---\n");
        fprintf(stderr, "Stage: %d\n", stage);
        fprintf(stderr, "Exit code: %d (0x%08X)\n", exit_code, (uint32_t)exit_code);
        fprintf(stderr, "Time: %.3f seconds\n", elapsed);
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__) || defined(__aarch64__)
            uint64_t total_tsc = profile_tsc_end - profile_tsc_start;
            double tsc_hz = 0.0;
            if (elapsed > 0.0) {
                tsc_hz = total_tsc / elapsed;
            }
            double translate_s = 0.0;
            double exec_s = 0.0;
            double dispatch_s = 0.0;
            if (tsc_hz > 0.0) {
                uint64_t scaled_translate = profile_translate_cycles * profile_sample_rate;
                uint64_t scaled_exec = profile_exec_cycles * profile_sample_rate;
                translate_s = (double)scaled_translate / tsc_hz;
                exec_s = (double)scaled_exec / tsc_hz;
                dispatch_s = elapsed - translate_s - exec_s;
                if (dispatch_s < 0.0) {
                    dispatch_s = 0.0;
                }
            }
            fprintf(stderr,
                    "Time breakdown (sampled 1/%u): translate %.3f s, exec %.3f s, dispatch %.3f s\n",
                    profile_sample_rate, translate_s, exec_s, dispatch_s);
#else
            fprintf(stderr, "Time breakdown: unavailable on this architecture\n");
#endif
        }
        if (cpu.side_exit_taken > 0) {
            fprintf(stderr, "Superblock side exits taken: %" PRIu32 "\n", cpu.side_exit_taken);
        }

        if (cmp_branch_fusion_count > 0) {
            fprintf(stderr, "Compare-branch fusions: %" PRIu32 "\n", cmp_branch_fusion_count);
        }
        if (cmp_branch_fusion_carry_skipped > 0) {
            fprintf(stderr, "Unsigned fusion skips (strict carry): %" PRIu32 "\n",
                    cmp_branch_fusion_carry_skipped);
        }

        if (cbz_peephole_count > 0) {
            fprintf(stderr, "CBZ/CBNZ peepholes: %" PRIu32 "\n", cbz_peephole_count);
        }

        if (native_stub_count > 0) {
            fprintf(stderr, "Native intrinsic stubs: %" PRIu32 "\n", native_stub_count);
        }

        if (stage5_lift_attempted > 0 || stage5_burg_attempted > 0 ||
            stage5_fallback_total > 0 || stage5_emit_prefilter_skip > 0) {
            fprintf(stderr, "Stage5 lift attempted: %" PRIu32 "\n", stage5_lift_attempted);
            fprintf(stderr, "Stage5 lift success:   %" PRIu32 "\n", stage5_lift_success);
            fprintf(stderr, "Stage5 BURG attempted: %" PRIu32 "\n", stage5_burg_attempted);
            fprintf(stderr, "Stage5 BURG selected:  %" PRIu32 "\n", stage5_burg_selected);
            if (stage5_burg_selected_guest_insts > 0) {
                fprintf(stderr, "Stage5 BURG sel guest insts: %" PRIu64 "\n",
                        stage5_burg_selected_guest_insts);
            }
            if (stage5_select_calls > 0 && stage5_select_time_ns > 0) {
                double sel_ms = (double)stage5_select_time_ns / 1e6;
                double sel_us_per_call = (double)stage5_select_time_ns /
                                         (double)stage5_select_calls / 1e3;
                fprintf(stderr, "Stage5 select calls: %" PRIu32 "\n", stage5_select_calls);
                fprintf(stderr, "Stage5 select time: %.3f ms (avg %.3f us/call)\n",
                        sel_ms, sel_us_per_call);
            }
            if (stage5_burg_selected > 0) {
                for (int p = 1; p < STAGE5_BURG_PATTERN_COUNT; p++) {
                    if (stage5_burg_pattern_hist[p] == 0) continue;
                    fprintf(stderr, "  burg pattern %-15s %" PRIu32 "\n",
                            stage5_burg_pattern_str((stage5_burg_pattern_t)p),
                            stage5_burg_pattern_hist[p]);
                }
            }
            fprintf(stderr, "Stage5 fallback total: %" PRIu32 "\n", stage5_fallback_total);
            if (stage5_fallback_lift_not_implemented > 0) {
                fprintf(stderr, "  lift not_implemented: %" PRIu32 "\n",
                        stage5_fallback_lift_not_implemented);
            }
            if (stage5_fallback_lift_unsupported_opcode > 0) {
                fprintf(stderr, "  lift unsupported_opcode: %" PRIu32 "\n",
                        stage5_fallback_lift_unsupported_opcode);
            }
            if (stage5_fallback_lift_region_too_large > 0) {
                fprintf(stderr, "  lift region_too_large: %" PRIu32 "\n",
                        stage5_fallback_lift_region_too_large);
            }
            if (stage5_fallback_lift_invalid_cfg > 0) {
                fprintf(stderr, "  lift invalid_cfg: %" PRIu32 "\n",
                        stage5_fallback_lift_invalid_cfg);
            }
            if (stage5_fallback_lift_internal > 0) {
                fprintf(stderr, "  lift internal: %" PRIu32 "\n",
                        stage5_fallback_lift_internal);
            }
            if (stage5_fallback_burg_not_implemented > 0) {
                fprintf(stderr, "  burg not_implemented: %" PRIu32 "\n",
                        stage5_fallback_burg_not_implemented);
            }
            if (stage5_fallback_burg_no_cover > 0) {
                fprintf(stderr, "  burg no_cover: %" PRIu32 "\n",
                        stage5_fallback_burg_no_cover);
            }
            if (stage5_fallback_burg_illegal_cover > 0) {
                fprintf(stderr, "  burg illegal_cover: %" PRIu32 "\n",
                        stage5_fallback_burg_illegal_cover);
            }
            if (stage5_fallback_burg_internal > 0) {
                fprintf(stderr, "  burg internal: %" PRIu32 "\n",
                        stage5_fallback_burg_internal);
            }
            if (stage5_fallback_lift_unsupported_opcode > 0) {
                uint32_t top_count[3] = {0, 0, 0};
                uint32_t top_opcode[3] = {0, 0, 0};
                for (uint32_t op = 0; op < 128; op++) {
                    uint32_t c = stage5_fallback_unsupported_opcode_hist[op];
                    if (c == 0) continue;
                    for (int k = 0; k < 3; k++) {
                        if (c > top_count[k]) {
                            for (int m = 2; m > k; m--) {
                                top_count[m] = top_count[m - 1];
                                top_opcode[m] = top_opcode[m - 1];
                            }
                            top_count[k] = c;
                            top_opcode[k] = op;
                            break;
                        }
                    }
                }
                for (int k = 0; k < 3; k++) {
                    if (top_count[k] == 0) break;
                    fprintf(stderr, "  lift unsupported top%d: op=0x%02X count=%" PRIu32 "\n",
                            k + 1, top_opcode[k], top_count[k]);
                }
            }
            if (stage5_emit_attempted > 0 || stage5_emit_success > 0 ||
                stage5_emit_fallback > 0 || stage5_emit_prefilter_skip > 0) {
                fprintf(stderr, "Stage5 emit attempted: %" PRIu32 "\n", stage5_emit_attempted);
                fprintf(stderr, "Stage5 emit success:   %" PRIu32 "\n", stage5_emit_success);
                fprintf(stderr, "Stage5 emit fallback:  %" PRIu32 "\n", stage5_emit_fallback);
                if (stage5_emit_success_guest_insts > 0) {
                    fprintf(stderr, "Stage5 emit guest insts: %" PRIu64 "\n",
                            stage5_emit_success_guest_insts);
                }
                if (stage5_emit_success_host_bytes > 0) {
                    fprintf(stderr, "Stage5 emit host bytes: %" PRIu64 "\n",
                            stage5_emit_success_host_bytes);
                }
                if (stage5_emit_success_guest_insts > 0 && stage5_emit_success_host_bytes > 0) {
                    double bpg = (double)stage5_emit_success_host_bytes /
                                 (double)stage5_emit_success_guest_insts;
                    fprintf(stderr, "Stage5 emit bytes/guest-inst: %.2f\n", bpg);
                }
                if (stage5_emit_calls > 0 && stage5_emit_time_ns > 0) {
                    double emit_ms = (double)stage5_emit_time_ns / 1e6;
                    double emit_us_per_call = (double)stage5_emit_time_ns /
                                              (double)stage5_emit_calls / 1e3;
                    fprintf(stderr, "Stage5 emit calls: %" PRIu32 "\n", stage5_emit_calls);
                    fprintf(stderr, "Stage5 emit time: %.3f ms (avg %.3f us/call)\n",
                            emit_ms, emit_us_per_call);
                }
                if (stage5_emit_success > 0 && stage5_emit_success_time_ns > 0) {
                    double succ_ms = (double)stage5_emit_success_time_ns / 1e6;
                    double succ_us_per = (double)stage5_emit_success_time_ns /
                                         (double)stage5_emit_success / 1e3;
                    fprintf(stderr, "  emit success time: %.3f ms (avg %.3f us/success)\n",
                            succ_ms, succ_us_per);
                }
                if (stage5_emit_fallback > 0 && stage5_emit_fallback_time_ns > 0) {
                    double fb_ms = (double)stage5_emit_fallback_time_ns / 1e6;
                    double fb_us_per = (double)stage5_emit_fallback_time_ns /
                                       (double)stage5_emit_fallback / 1e3;
                    fprintf(stderr, "  emit fallback time: %.3f ms (avg %.3f us/fallback)\n",
                            fb_ms, fb_us_per);
                }
                for (int p = 0; p < STAGE5_BURG_PATTERN_COUNT; p++) {
                    if (stage5_emit_pattern_success[p] == 0) continue;
                    double pbpg = 0.0;
                    double avg_g = 0.0;
                    double avg_h = 0.0;
                    if (stage5_emit_pattern_guest_insts[p] > 0 &&
                        stage5_emit_pattern_host_bytes[p] > 0) {
                        pbpg = (double)stage5_emit_pattern_host_bytes[p] /
                               (double)stage5_emit_pattern_guest_insts[p];
                    }
                    if (stage5_emit_pattern_success[p] > 0) {
                        avg_g = (double)stage5_emit_pattern_guest_insts[p] /
                                (double)stage5_emit_pattern_success[p];
                        avg_h = (double)stage5_emit_pattern_host_bytes[p] /
                                (double)stage5_emit_pattern_success[p];
                    }
                    fprintf(stderr,
                            "  emit pattern %-15s n=%" PRIu32 " ginst=%" PRIu64
                            " hbytes=%" PRIu64 " bpg=%.2f avg_g=%.2f avg_h=%.2f\n",
                            stage5_burg_pattern_str((stage5_burg_pattern_t)p),
                            stage5_emit_pattern_success[p],
                            stage5_emit_pattern_guest_insts[p],
                            stage5_emit_pattern_host_bytes[p],
                            pbpg, avg_g, avg_h);
                }
                if (stage5_emit_fallback_non_terminal > 0) {
                    fprintf(stderr, "  emit non_terminal: %" PRIu32 "\n",
                            stage5_emit_fallback_non_terminal);
                }
                if (stage5_emit_fallback_shape > 0) {
                    fprintf(stderr, "  emit shape: %" PRIu32 "\n",
                            stage5_emit_fallback_shape);
                }
                if (stage5_emit_fallback_superblock_policy > 0) {
                    fprintf(stderr, "  emit superblock_policy: %" PRIu32 "\n",
                            stage5_emit_fallback_superblock_policy);
                    if (stage5_emit_fallback_policy_guardrail > 0) {
                        fprintf(stderr, "  emit policy guardrail: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_guardrail);
                    }
                    if (stage5_emit_fallback_policy_jalr_indirect > 0) {
                        fprintf(stderr, "  emit policy jalr_indirect: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_jalr_indirect);
                    }
                    if (stage5_emit_fallback_policy_bench_jal_jump > 0) {
                        fprintf(stderr, "  emit policy bench_jal_jump: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_bench_jal_jump);
                        if (stage5_emit_fallback_policy_bench_jal_jump_backedge > 0) {
                            fprintf(stderr, "    bench_jal_jump backedge: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_bench_jal_jump_backedge);
                        }
                    }
                    if (stage5_emit_fallback_policy_bench_direct_branch > 0) {
                        fprintf(stderr, "  emit policy bench_direct_branch: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_bench_direct_branch);
                    }
                    if (stage5_emit_fallback_policy_bench_block_end > 0) {
                        fprintf(stderr, "  emit policy bench_block_end: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_bench_block_end);
                    }
                    if (stage5_emit_fallback_policy_bench_jal_call > 0) {
                        fprintf(stderr, "  emit policy bench_jal_call: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_bench_jal_call);
                    }
                    if (stage5_emit_fallback_policy_bench_jalr_ret > 0) {
                        fprintf(stderr, "  emit policy bench_jalr_ret: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_bench_jalr_ret);
                        if (stage5_emit_fallback_policy_bench_jalr_ret_short > 0) {
                            fprintf(stderr, "    bench_jalr_ret short: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_bench_jalr_ret_short);
                        }
                        if (stage5_emit_fallback_policy_bench_jalr_ret_long > 0) {
                            fprintf(stderr, "    bench_jalr_ret long: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_bench_jalr_ret_long);
                        }
                    }
                    if (stage5_emit_fallback_policy_call_return > 0) {
                        fprintf(stderr, "  emit policy call_return: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_call_return);
                        if (stage5_emit_fallback_policy_call_return_jal_call_disabled > 0) {
                            fprintf(stderr, "    call_return jal_call_disabled: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_call_return_jal_call_disabled);
                        }
                        if (stage5_emit_fallback_policy_call_return_jal_call_long > 0) {
                            fprintf(stderr, "    call_return jal_call_long: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_call_return_jal_call_long);
                        }
                        if (stage5_emit_fallback_policy_call_return_jalr_ret > 0) {
                            fprintf(stderr, "    call_return jalr_ret: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_call_return_jalr_ret);
                            if (stage5_emit_fallback_policy_call_return_jalr_ret_short > 0) {
                                fprintf(stderr, "      jalr_ret short: %" PRIu32 "\n",
                                        stage5_emit_fallback_policy_call_return_jalr_ret_short);
                            }
                            if (stage5_emit_fallback_policy_call_return_jalr_ret_long > 0) {
                                fprintf(stderr, "      jalr_ret long: %" PRIu32 "\n",
                                        stage5_emit_fallback_policy_call_return_jalr_ret_long);
                            }
                        }
                    }
                    if (stage5_emit_fallback_policy_direct_branch > 0) {
                        fprintf(stderr, "  emit policy direct_branch: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_direct_branch);
                    }
                    if (stage5_emit_fallback_policy_regflow > 0) {
                        fprintf(stderr, "  emit policy regflow: %" PRIu32 "\n",
                                stage5_emit_fallback_policy_regflow);
                        if (stage5_emit_fallback_policy_regflow_cross > 0) {
                            fprintf(stderr, "    regflow cross: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_regflow_cross);
                        }
                        if (stage5_emit_fallback_policy_regflow_span > 0) {
                            fprintf(stderr, "    regflow span: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_regflow_span);
                        }
                        if (stage5_emit_fallback_policy_regflow_live > 0) {
                            fprintf(stderr, "    regflow live: %" PRIu32 "\n",
                                    stage5_emit_fallback_policy_regflow_live);
                        }
                    }
                }
                if (stage5_emit_policy_allow_call > 0) {
                    fprintf(stderr, "  emit policy allow_call: %" PRIu32 "\n",
                            stage5_emit_policy_allow_call);
                    if (stage5_emit_policy_allow_call_bench > 0) {
                        fprintf(stderr, "    allow_call bench: %" PRIu32 "\n",
                                stage5_emit_policy_allow_call_bench);
                    }
                }
                if (stage5_emit_policy_allow_jalr_ret_bench > 0) {
                    fprintf(stderr, "  emit policy allow_jalr_ret_bench: %" PRIu32 "\n",
                            stage5_emit_policy_allow_jalr_ret_bench);
                    if (stage5_emit_policy_allow_jalr_ret_bench_short > 0) {
                        fprintf(stderr, "    allow_jalr_ret short: %" PRIu32 "\n",
                                stage5_emit_policy_allow_jalr_ret_bench_short);
                    }
                    if (stage5_emit_policy_allow_jalr_ret_bench_long > 0) {
                        fprintf(stderr, "    allow_jalr_ret long: %" PRIu32 "\n",
                                stage5_emit_policy_allow_jalr_ret_bench_long);
                    }
                }
                if (stage5_emit_regflow_retry_attempted > 0) {
                    fprintf(stderr, "  emit regflow_retry attempted: %" PRIu32 "\n",
                            stage5_emit_regflow_retry_attempted);
                    fprintf(stderr, "  emit regflow_retry accepted:  %" PRIu32 "\n",
                            stage5_emit_regflow_retry_accepted);
                    if (stage5_emit_regflow_retry_emit_success > 0) {
                        fprintf(stderr, "  emit regflow_retry success:   %" PRIu32 "\n",
                                stage5_emit_regflow_retry_emit_success);
                    }
                    if (stage5_emit_regflow_retry_cfg_attempted > 0) {
                        fprintf(stderr, "    cfg_head attempt/accept/success: %" PRIu32 "/%" PRIu32 "/%" PRIu32 "\n",
                                stage5_emit_regflow_retry_cfg_attempted,
                                stage5_emit_regflow_retry_cfg_accepted,
                                stage5_emit_regflow_retry_cfg_emit_success);
                    }
                    if (stage5_emit_regflow_retry_term_attempted > 0) {
                        fprintf(stderr, "    terminal attempt/accept/success: %" PRIu32 "/%" PRIu32 "/%" PRIu32 "\n",
                                stage5_emit_regflow_retry_term_attempted,
                                stage5_emit_regflow_retry_term_accepted,
                                stage5_emit_regflow_retry_term_emit_success);
                    }
                    if (stage5_emit_regflow_retry_half_attempted > 0) {
                        fprintf(stderr, "    half     attempt/accept/success: %" PRIu32 "/%" PRIu32 "/%" PRIu32 "\n",
                                stage5_emit_regflow_retry_half_attempted,
                                stage5_emit_regflow_retry_half_accepted,
                                stage5_emit_regflow_retry_half_emit_success);
                    }
                    if (stage5_emit_regflow_retry_accepted > 0 &&
                        stage5_emit_regflow_retry_guest_before_total > 0 &&
                        stage5_emit_regflow_retry_guest_after_total > 0) {
                        double avg_before = (double)stage5_emit_regflow_retry_guest_before_total /
                                            (double)stage5_emit_regflow_retry_accepted;
                        double avg_after = (double)stage5_emit_regflow_retry_guest_after_total /
                                           (double)stage5_emit_regflow_retry_accepted;
                        fprintf(stderr, "    retry avg guest insts: %.2f -> %.2f (max drop=%" PRIu32 ")\n",
                                avg_before, avg_after, stage5_emit_regflow_retry_max_reduction);
                    }
                    if (stage5_emit_regflow_retry_explore_events > 0) {
                        fprintf(stderr,
                                "    retry exploration events: %" PRIu32 " (cfg=%" PRIu32 ", term=%" PRIu32 ", half=%" PRIu32 ")\n",
                                stage5_emit_regflow_retry_explore_events,
                                stage5_emit_regflow_retry_explore_cfg_first,
                                stage5_emit_regflow_retry_explore_term_first,
                                stage5_emit_regflow_retry_explore_half_first);
                    }
                    if (stage5_emit_regflow_retry_decay_events > 0) {
                        fprintf(stderr,
                                "    retry decay events: %" PRIu32 " recent cfg=%" PRIu32 "/%" PRIu32
                                " term=%" PRIu32 "/%" PRIu32 " half=%" PRIu32 "/%" PRIu32 "\n",
                                stage5_emit_regflow_retry_decay_events,
                                stage5_emit_regflow_retry_recent_cfg_success,
                                stage5_emit_regflow_retry_recent_cfg_attempted,
                                stage5_emit_regflow_retry_recent_term_success,
                                stage5_emit_regflow_retry_recent_term_attempted,
                                stage5_emit_regflow_retry_recent_half_success,
                                stage5_emit_regflow_retry_recent_half_attempted);
                    }
                    if (stage5_emit_regflow_retry_cooldown_events > 0) {
                        fprintf(stderr,
                                "    retry cooldown events: %" PRIu32 " (cfg=%" PRIu32 ", term=%" PRIu32 ", half=%" PRIu32 ")\n",
                                stage5_emit_regflow_retry_cooldown_events,
                                stage5_emit_regflow_retry_cooldown_cfg,
                                stage5_emit_regflow_retry_cooldown_term,
                                stage5_emit_regflow_retry_cooldown_half);
                    }
                }
                if (stage5_emit_prefilter_skip > 0) {
                    fprintf(stderr, "  emit prefilter_skip: %" PRIu32 "\n",
                            stage5_emit_prefilter_skip);
                    if (stage5_emit_prefilter_skip_branch_head > 0) {
                        fprintf(stderr, "  emit prefilter branch_head: %" PRIu32 "\n",
                                stage5_emit_prefilter_skip_branch_head);
                    }
                    if (stage5_emit_prefilter_skip_noncmp_head > 0) {
                        fprintf(stderr, "  emit prefilter noncmp_head: %" PRIu32 "\n",
                                stage5_emit_prefilter_skip_noncmp_head);
                    }
                    if (stage5_emit_prefilter_branch_probe > 0) {
                        fprintf(stderr, "  emit prefilter branch_probe: %" PRIu32 "\n",
                                stage5_emit_prefilter_branch_probe);
                    }
                }
                if (stage5_emit_fallback_side_exit_unowned > 0) {
                    fprintf(stderr, "  emit side_exit_unowned: %" PRIu32 "\n",
                            stage5_emit_fallback_side_exit_unowned);
                    if (stage5_emit_fallback_side_exit_disabled > 0) {
                        fprintf(stderr, "  emit side_exit disabled: %" PRIu32 "\n",
                                stage5_emit_fallback_side_exit_disabled);
                    }
                    if (stage5_emit_fallback_side_exit_unsupported > 0) {
                        fprintf(stderr, "  emit side_exit unsupported: %" PRIu32 "\n",
                                stage5_emit_fallback_side_exit_unsupported);
                    }
                    if (stage5_emit_fallback_side_exit_call_guard > 0) {
                        fprintf(stderr, "  emit side_exit call_guard: %" PRIu32 "\n",
                                stage5_emit_fallback_side_exit_call_guard);
                        if (stage5_emit_fallback_side_exit_call_guard_jal > 0 ||
                            stage5_emit_fallback_side_exit_call_guard_jalr > 0) {
                            fprintf(stderr,
                                    "    call_guard split: jal=%" PRIu32 " jalr=%" PRIu32 "\n",
                                    stage5_emit_fallback_side_exit_call_guard_jal,
                                    stage5_emit_fallback_side_exit_call_guard_jalr);
                        }
                    }
                }
                if (stage5_emit_fallback_single_unhandled > 0) {
                    fprintf(stderr, "  emit single_unhandled: %" PRIu32 "\n",
                            stage5_emit_fallback_single_unhandled);
                }
                if (stage5_emit_fallback_cmp_branch_miss > 0) {
                    fprintf(stderr, "  emit cmp_branch_miss: %" PRIu32 "\n",
                            stage5_emit_fallback_cmp_branch_miss);
                }
                if (stage5_emit_fallback_not_ended > 0) {
                    fprintf(stderr, "  emit not_ended: %" PRIu32 "\n",
                            stage5_emit_fallback_not_ended);
                    if (stage5_emit_not_ended_reason_single_terminal > 0) {
                        fprintf(stderr, "    not_ended single_terminal: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_single_terminal);
                    }
                    if (stage5_emit_not_ended_reason_cmp_branch_fused > 0) {
                        fprintf(stderr, "    not_ended cmp_branch_fused: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_cmp_branch_fused);
                    }
                    if (stage5_emit_not_ended_reason_familyb_cmp_rd > 0) {
                        fprintf(stderr, "    not_ended familyb_cmp_rd: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_familyb_cmp_rd);
                    }
                    if (stage5_emit_not_ended_reason_familyb_branch_first > 0) {
                        fprintf(stderr, "    not_ended familyb_branch_first: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_familyb_branch_first);
                    }
                    if (stage5_emit_not_ended_reason_familyb_prefix_terminal > 0) {
                        fprintf(stderr, "    not_ended familyb_prefix_terminal: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_familyb_prefix_terminal);
                    }
                    if (stage5_emit_not_ended_reason_familyc_terminal > 0) {
                        fprintf(stderr, "    not_ended familyc_terminal: %" PRIu32 "\n",
                                stage5_emit_not_ended_reason_familyc_terminal);
                    }
                    uint8_t top_op[3] = {0};
                    uint32_t top_ct[3] = {0};
                    for (uint32_t op = 0; op < 128; op++) {
                        uint32_t c = stage5_emit_not_ended_opcode_hist[op];
                        if (c == 0) continue;
                        for (int i = 0; i < 3; i++) {
                            if (c > top_ct[i]) {
                                for (int k = 2; k > i; k--) {
                                    top_ct[k] = top_ct[k - 1];
                                    top_op[k] = top_op[k - 1];
                                }
                                top_ct[i] = c;
                                top_op[i] = (uint8_t)op;
                                break;
                            }
                        }
                    }
                    for (int i = 0; i < 3; i++) {
                        if (top_ct[i] == 0) break;
                        fprintf(stderr,
                                "  emit not_ended top%d: op=0x%02X count=%" PRIu32 "\n",
                                i + 1, top_op[i], top_ct[i]);
                    }
                }
                if (stage5_emit_fused_cmp_branch > 0) {
                    fprintf(stderr, "  emit fused_cmp_branch: %" PRIu32 "\n",
                            stage5_emit_fused_cmp_branch);
                }
                if (stage5_emit_side_exits > 0) {
                    fprintf(stderr, "  emit side_exits: %" PRIu32 "\n",
                            stage5_emit_side_exits);
                }
                if (stage_emit_inblock_backedge_total > 0) {
                    fprintf(stderr,
                            "  emit inblock_backedge: total=%" PRIu32 " with_side_exit=%" PRIu32 "\n",
                            stage_emit_inblock_backedge_total,
                            stage_emit_inblock_backedge_with_side_exit);
                    if (stage5_backedge_dirty_promotions > 0) {
                        fprintf(stderr,
                                "  emit backedge_dirty_promotions: total=%" PRIu32
                                " r15=%" PRIu32 "\n",
                                stage5_backedge_dirty_promotions,
                                stage5_backedge_dirty_promotions_r15);
                    }
                }
                if (stage5_deferred_exit_flush_full > 0 ||
                    stage5_deferred_exit_flush_dirty > 0) {
                    fprintf(stderr,
                            "  emit deferred_flush: full=%" PRIu32 " dirty=%" PRIu32
                            " pending_r15=%" PRIu32 " snap_r15_alloc=%" PRIu32
                            " snap_r15_dirty=%" PRIu32 "\n",
                            stage5_deferred_exit_flush_full,
                            stage5_deferred_exit_flush_dirty,
                            stage5_deferred_exit_pending_write_r15,
                            stage5_deferred_exit_snapshot_r15_allocated,
                            stage5_deferred_exit_snapshot_r15_dirty);
                }
                if (stage5_emit_region_side_exit_total > 0) {
                    fprintf(stderr,
                            "  emit side_exit_regions: total=%" PRIu32
                            " owned=%" PRIu32
                            " unsupported=%" PRIu32
                            " disabled=%" PRIu32
                            " call_guard=%" PRIu32 "\n",
                            stage5_emit_region_side_exit_total,
                            stage5_emit_region_side_exit_owned,
                            stage5_emit_region_side_exit_unsupported,
                            stage5_emit_region_side_exit_disabled,
                            stage5_emit_region_side_exit_call_guard);
                    if (stage5_emit_region_side_exit_call_guard_jal > 0 ||
                        stage5_emit_region_side_exit_call_guard_jalr > 0) {
                        fprintf(stderr,
                                "    side_exit call_guard split: jal=%" PRIu32
                                " jalr=%" PRIu32 "\n",
                                stage5_emit_region_side_exit_call_guard_jal,
                                stage5_emit_region_side_exit_call_guard_jalr);
                    }
                    if (stage5_emit_region_side_exit_call_guard_relaxed_after_only > 0) {
                        fprintf(stderr,
                                "    side_exit call_guard relaxed_after_only: %" PRIu32 "\n",
                                stage5_emit_region_side_exit_call_guard_relaxed_after_only);
                    }
                    if (stage5_emit_side_exit_forced_family_c_unsigned > 0) {
                        fprintf(stderr,
                                "  emit side_exit forced_family_c_unsigned: %" PRIu32 "\n",
                                stage5_emit_side_exit_forced_family_c_unsigned);
                    }
                    if (stage5_emit_side_exit_forced_family_c_b_only > 0) {
                        fprintf(stderr,
                                "  emit side_exit forced_family_c_b_only: %" PRIu32 "\n",
                                stage5_emit_side_exit_forced_family_c_b_only);
                    }
                    if (stage5_emit_side_exit_auto_backedge_retry_unsigned > 0) {
                        fprintf(stderr,
                                "  emit side_exit auto_backedge_retry_unsigned: %" PRIu32 "\n",
                                stage5_emit_side_exit_auto_backedge_retry_unsigned);
                    }
                    if (stage5_emit_region_side_exit_unsupported > 0) {
                        uint32_t top_count[3] = {0, 0, 0};
                        uint32_t top_opcode[3] = {0, 0, 0};
                        for (uint32_t op = 0; op < 128; op++) {
                            uint32_t c = stage5_emit_side_exit_unsupported_opcode_hist[op];
                            if (c == 0) continue;
                            for (int k = 0; k < 3; k++) {
                                if (c > top_count[k]) {
                                    for (int m = 2; m > k; m--) {
                                        top_count[m] = top_count[m - 1];
                                        top_opcode[m] = top_opcode[m - 1];
                                    }
                                    top_count[k] = c;
                                    top_opcode[k] = op;
                                    break;
                                }
                            }
                        }
                        for (int k = 0; k < 3; k++) {
                            if (top_count[k] == 0) break;
                            fprintf(stderr,
                                    "  emit side_exit unsupported top%d: op=0x%02X count=%" PRIu32 "\n",
                                    k + 1, top_opcode[k], top_count[k]);
                        }
                    }
                    if (stage5_emit_side_exits > 0) {
                        uint32_t top_count[3] = {0, 0, 0};
                        uint32_t top_opcode[3] = {0, 0, 0};
                        for (uint32_t op = 0; op < 128; op++) {
                            uint32_t c = stage5_emit_side_exit_emitted_opcode_hist[op];
                            if (c == 0) continue;
                            for (int k = 0; k < 3; k++) {
                                if (c > top_count[k]) {
                                    for (int m = 2; m > k; m--) {
                                        top_count[m] = top_count[m - 1];
                                        top_opcode[m] = top_opcode[m - 1];
                                    }
                                    top_count[k] = c;
                                    top_opcode[k] = op;
                                    break;
                                }
                            }
                        }
                        for (int k = 0; k < 3; k++) {
                            if (top_count[k] == 0) break;
                            fprintf(stderr,
                                    "  emit side_exit emitted top%d: op=0x%02X count=%" PRIu32 "\n",
                                    k + 1, top_opcode[k], top_count[k]);
                        }
                    }
                }
                if (stage5_emit_fallback > 0) {
                    uint32_t top_count[3] = {0, 0, 0};
                    uint32_t top_opcode[3] = {0, 0, 0};
                    for (uint32_t op = 0; op < 128; op++) {
                        uint32_t c = stage5_emit_unhandled_opcode_hist[op];
                        if (c == 0) continue;
                        for (int k = 0; k < 3; k++) {
                            if (c > top_count[k]) {
                                for (int m = 2; m > k; m--) {
                                    top_count[m] = top_count[m - 1];
                                    top_opcode[m] = top_opcode[m - 1];
                                }
                                top_count[k] = c;
                                top_opcode[k] = op;
                                break;
                            }
                        }
                    }
                    for (int k = 0; k < 3; k++) {
                        if (top_count[k] == 0) break;
                        fprintf(stderr, "  emit unhandled top%d: op=0x%02X count=%" PRIu32 "\n",
                                k + 1, top_opcode[k], top_count[k]);
                    }
                }
            }
        }
        if (stage5_validate_attempted > 0 || stage5_validate_mismatch > 0) {
            fprintf(stderr, "Stage5 validate attempted: %" PRIu32 "\n", stage5_validate_attempted);
            fprintf(stderr, "Stage5 validate eligible:  %" PRIu32 "\n", stage5_validate_eligible);
            fprintf(stderr, "Stage5 validate ok:        %" PRIu32 "\n", stage5_validate_ok);
            fprintf(stderr, "Stage5 validate mismatch:  %" PRIu32 "\n", stage5_validate_mismatch);
            if (stage5_validate_eligible > 0) {
                double avg_len = (double)stage5_validate_eligible_guest_insts /
                                 (double)stage5_validate_eligible;
                fprintf(stderr, "  validate eligible avg guest insts: %.2f\n", avg_len);
                fprintf(stderr,
                        "  validate eligible len buckets: 1-2=%" PRIu32
                        " 3-4=%" PRIu32 " 5-8=%" PRIu32 " 9-16=%" PRIu32 " 17+=%" PRIu32 "\n",
                        stage5_validate_eligible_len_hist[0],
                        stage5_validate_eligible_len_hist[1],
                        stage5_validate_eligible_len_hist[2],
                        stage5_validate_eligible_len_hist[3],
                        stage5_validate_eligible_len_hist[4]);
                print_top_opcode_hist("validate eligible terminal",
                                      stage5_validate_eligible_terminal_opcode_hist);
            }
            if (stage5_validate_skipped_load_store > 0) {
                fprintf(stderr, "  validate skipped unsupported: %" PRIu32 "\n",
                        stage5_validate_skipped_load_store);
            }
            if (stage5_validate_skipped_call_indirect > 0) {
                fprintf(stderr, "  validate skipped call/indirect: %" PRIu32 "\n",
                        stage5_validate_skipped_call_indirect);
                print_top_opcode_hist("validate skip call/indirect",
                                      stage5_validate_skip_call_indirect_opcode_hist);
            }
            if (stage5_validate_skipped_terminal > 0) {
                fprintf(stderr, "  validate skipped terminal: %" PRIu32 "\n",
                        stage5_validate_skipped_terminal);
                print_top_opcode_hist("validate skip terminal",
                                      stage5_validate_skip_terminal_opcode_hist);
            }
            if (stage5_validate_skipped_mem_mmio > 0) {
                fprintf(stderr, "  validate skipped mem-mmio: %" PRIu32 "\n",
                        stage5_validate_skipped_mem_mmio);
            }
            if (stage5_validate_skipped_mem_oob > 0) {
                fprintf(stderr, "  validate skipped mem-oob: %" PRIu32 "\n",
                        stage5_validate_skipped_mem_oob);
            }
            if (stage5_validate_skipped_mem_capacity > 0) {
                fprintf(stderr, "  validate skipped mem-capacity: %" PRIu32 "\n",
                        stage5_validate_skipped_mem_capacity);
            }
            if (stage5_validate_skipped_mem_mmio > 0 ||
                stage5_validate_skipped_mem_oob > 0 ||
                stage5_validate_skipped_mem_capacity > 0) {
                print_top_opcode_hist("validate skip mem",
                                      stage5_validate_skip_mem_opcode_hist);
            }
        }
        if (stage5_cfg_regions > 0) {
            double avg_blocks = (double)stage5_cfg_blocks_total / (double)stage5_cfg_regions;
            double avg_iters = (double)stage5_cfg_liveness_iterations_total / (double)stage5_cfg_regions;
            fprintf(stderr, "Stage5 cfg regions: %" PRIu32 "\n", stage5_cfg_regions);
            fprintf(stderr, "  cfg avg blocks/region: %.2f\n", avg_blocks);
            fprintf(stderr, "  cfg avg liveness iters: %.2f\n", avg_iters);
            fprintf(stderr, "  cfg spill_likely regions: %" PRIu32 "\n",
                    stage5_cfg_spill_likely_regions);
            fprintf(stderr, "  cfg max live seen: %" PRIu32 "\n", stage5_cfg_max_live_seen);
            if (stage5_reg_flow_regions > 0) {
                double avg_cross_regs = (double)stage5_reg_flow_cross_block_regs_total /
                                        (double)stage5_reg_flow_regions;
                fprintf(stderr, "  reg-flow avg cross-block regs/region: %.2f\n", avg_cross_regs);
                fprintf(stderr, "  reg-flow max span seen: %" PRIu32 "\n",
                        stage5_reg_flow_max_span_seen);
            }
        }

        if (stage5_codegen_attempted > 0 || stage5_codegen_success > 0) {
            fprintf(stderr, "Stage5 codegen attempted: %" PRIu32 "\n", stage5_codegen_attempted);
            fprintf(stderr, "Stage5 codegen success:   %" PRIu32 "\n", stage5_codegen_success);
            fprintf(stderr, "Stage5 codegen fallback:  %" PRIu32 "\n", stage5_codegen_fallback);
            if (stage5_codegen_fallback_unsupported_op > 0) {
                fprintf(stderr, "  codegen unsupported op: %" PRIu32 "\n",
                        stage5_codegen_fallback_unsupported_op);
            }
            if (stage5_codegen_guest_insts > 0) {
                fprintf(stderr, "Stage5 codegen guest insts: %" PRIu64 "\n",
                        stage5_codegen_guest_insts);
            }
            if (stage5_codegen_host_bytes > 0) {
                fprintf(stderr, "Stage5 codegen host bytes: %" PRIu64 "\n",
                        stage5_codegen_host_bytes);
            }
            if (stage5_codegen_guest_insts > 0 && stage5_codegen_host_bytes > 0) {
                double bpg = (double)stage5_codegen_host_bytes /
                             (double)stage5_codegen_guest_insts;
                fprintf(stderr, "Stage5 codegen bytes/guest-inst: %.2f\n", bpg);
            }
            fprintf(stderr, "  codegen regs allocated histogram:");
            for (int h = 0; h <= REG_ALLOC_SLOTS; h++) {
                if (stage5_codegen_regs_allocated_hist[h] > 0) {
                    fprintf(stderr, " [%d]=%" PRIu32, h,
                            stage5_codegen_regs_allocated_hist[h]);
                }
            }
            fprintf(stderr, "\n");
        }

        if (stage >= 2) {
            cache_print_stats(&cache);
            if (dump_offenders) {
                cache_dump_offender_blocks(&cache, cpu.mem_base, disassemble_offenders);
            }
            if (dump_pc != 0) {
                cache_dump_block_for_pc(&cache, cpu.mem_base, dump_pc, disassemble_offenders);
            }
        }
    }

    // Cleanup
    if (stage >= 2) {
        cache_destroy(&cache);
    }
    dbt_cpu_destroy(&cpu);
    return exit_code;
}
