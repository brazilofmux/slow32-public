// SLOW-32 DBT: Main Entry Point
// Stage 4 - Superblock extension

#define _GNU_SOURCE
#define _POSIX_C_SOURCE 199309L

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <sys/mman.h>
#include <time.h>
#include <inttypes.h>

#include "cpu_state.h"
#include "translate.h"
#include "block_cache.h"

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
static bool peephole_enabled = false;
static bool reg_cache_enabled = false;
static bool align_traps_enabled = false;

// MMIO state
static mmio_ring_state_t mmio_state;
static bool mmio_initialized = false;

// Host argc/argv for passing to guest
static int host_argc = 0;
static char **host_argv = NULL;

#if defined(__x86_64__) || defined(__i386__)
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
    if (cpu->mmio_base + 0x10000 > cpu->mem_size) {
        fprintf(stderr, "DBT: MMIO base 0x%08X exceeds guest memory\n", cpu->mmio_base);
        cpu->mmio_enabled = false;
        return;
    }

    // Initialize MMIO state with heap after data limit
    uint32_t heap_base = (cpu->data_limit + 0xFFF) & ~0xFFF;
    uint32_t heap_size = cpu->mmio_base - heap_base;  // Heap up to MMIO
    mmio_ring_init(&mmio_state, heap_base, heap_size);

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
static void dbt_handle_yield(dbt_cpu_state_t *cpu) {
    if (!cpu->mmio_enabled) return;

    if (!mmio_initialized) return;

    // Sync indices from guest memory
    mmio_sync_from_guest(cpu);

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
    if (addr + size > cpu->mem_size) return -1;
    memcpy(cpu->mem_base + addr, data, size);
    return 0;
}

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
        if (cpu->mmio_base == 0 || cpu->mmio_base + 0x10000 > cpu->mem_size) {
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

// Stage 1 dispatcher (no caching)
static void run_dbt_stage1(dbt_cpu_state_t *cpu) {
    translate_ctx_t ctx;
    translate_init(&ctx, cpu);
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Translate block at current PC
        translated_block_fn block;
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__)
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
#if defined(__x86_64__) || defined(__i386__)
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
#if defined(__x86_64__) || defined(__i386__)
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
#if defined(__x86_64__) || defined(__i386__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_translated(cpu, (translated_block_fn)block->host_code);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_translated(cpu, (translated_block_fn)block->host_code);
            }
#else
            execute_translated(cpu, (translated_block_fn)block->host_code);
#endif
        } else {
            execute_translated(cpu, (translated_block_fn)block->host_code);
        }

        dispatch_iter++;

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
#if defined(__x86_64__) || defined(__i386__)
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
#if defined(__x86_64__) || defined(__i386__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_translated(cpu, (translated_block_fn)block->host_code);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_translated(cpu, (translated_block_fn)block->host_code);
            }
#else
            execute_translated(cpu, (translated_block_fn)block->host_code);
#endif
        } else {
            execute_translated(cpu, (translated_block_fn)block->host_code);
        }

        dispatch_iter++;

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

// Stage 4 dispatcher (with superblock extension)
static void run_dbt_stage4(dbt_cpu_state_t *cpu, block_cache_t *cache) {
    translate_ctx_t ctx;
    translate_init_cached(&ctx, cpu, cache);
    ctx.inline_lookup_enabled = true;   // Keep Stage 3 inline lookup
    ctx.ras_enabled = true;              // Keep Stage 3 RAS
    ctx.superblock_enabled = true;       // Enable Stage 4 superblock extension
    ctx.profile_side_exits = profile_side_exits;
    ctx.side_exit_info_enabled = profile_side_exits;   // Study-only diagnostics
    ctx.avoid_backedge_extend = avoid_backedge_extend;
    ctx.peephole_enabled = peephole_enabled;
    ctx.reg_cache_enabled = reg_cache_enabled;
    uint64_t dispatch_iter = 0;

    while (!cpu->halted) {
        // Look up block in cache
        translated_block_t *block = cache_lookup(cache, cpu->pc);

        if (!block) {
            // Reset superblock depth for new translation
            ctx.superblock_depth = 0;

            // Translate and cache new block (may be a superblock)
            if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__)
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
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__)
            bool sample_now = (dispatch_iter % profile_sample_rate) == 0;
            if (sample_now) {
                uint64_t t0 = rdtsc();
                execute_translated(cpu, (translated_block_fn)block->host_code);
                profile_exec_cycles += rdtsc() - t0;
            } else {
                execute_translated(cpu, (translated_block_fn)block->host_code);
            }
#else
            execute_translated(cpu, (translated_block_fn)block->host_code);
#endif
        } else {
            execute_translated(cpu, (translated_block_fn)block->host_code);
        }

        dispatch_iter++;

        // Track indirect branch statistics when we return to dispatcher
        if (cpu->exit_reason == EXIT_INDIRECT) {
            cache->inline_miss_count++;
        }
        if (profile_side_exits) {
            if (block->side_exit_count > 0) {
                for (int i = 0; i < block->side_exit_count; i++) {
                    uint32_t pc = block->side_exit_pcs[i];
                    cache->side_exit_total_profile[cache_hash(pc)]++;
                }
            }
            if (cpu->exit_reason == EXIT_BRANCH && cpu->exit_info != 0) {
                uint32_t pc = cpu->exit_info;
                cache->side_exit_taken_profile[cache_hash(pc)]++;
                cpu->exit_info = 0;
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
    fprintf(stderr, "  -P        Study-only: peephole optimize emitted x86-64\n");
    fprintf(stderr, "  -R        Enable fixed register cache (Stage 4)\n");
    fprintf(stderr, "  -t        Two-pass superblock profiling (Stage 4 only)\n");
    fprintf(stderr, "  -M <n>    Min samples before using side-exit rate\n");
    fprintf(stderr, "  -T <pct>  Max taken %% to allow superblock extension\n");
    fprintf(stderr, "  -D        Dump top offender blocks\n");
    fprintf(stderr, "  -O        Disassemble offender host blocks (uses objdump)\n");
    fprintf(stderr, "  -X <pc>   Dump block containing guest PC\n");
    fprintf(stderr, "  -1        Use Stage 1 mode (no caching)\n");
    fprintf(stderr, "  -2        Use Stage 2 mode (block cache + chaining)\n");
    fprintf(stderr, "  -3        Use Stage 3 mode (inline indirect lookup)\n");
    fprintf(stderr, "  -4        Use Stage 4 mode (superblock extension, default)\n");
    fprintf(stderr, "\nEnvironment:\n");
    fprintf(stderr, "  SLOW32_DBT_ALIGN_TRAP=1  Trap on unaligned LD/ST/fetch\n");
}

int main(int argc, char **argv) {
    bool verbose = false;
    bool show_stats = false;
    bool two_pass = false;
    bool two_pass_forced = false;
    bool dump_offenders = false;
    bool disassemble_offenders = false;
    uint32_t dump_pc = 0;
    int stage = 4;  // Default to Stage 4 now
    const char *filename = NULL;
    const char *trace_env = getenv("SLOW32_DBT_EMIT_TRACE");
    const char *trace_pc_env = getenv("SLOW32_DBT_EMIT_TRACE_PC");
    const char *align_env = getenv("SLOW32_DBT_ALIGN_TRAP");
    bool emit_trace = (trace_env && atoi(trace_env) != 0);
    if (align_env && atoi(align_env) != 0) {
        align_traps_enabled = true;
    }
    uint32_t emit_trace_pc = 0;
    if (trace_pc_env && trace_pc_env[0] != '\0') {
        emit_trace_pc = (uint32_t)strtoul(trace_pc_env, NULL, 0);
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
                    peephole_enabled = true;
                    break;
                case 'R':
                    reg_cache_enabled = true;
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

    // Initialize CPU
    dbt_cpu_state_t cpu;
    dbt_cpu_init(&cpu);
    cpu.align_traps_enabled = align_traps_enabled;

    // Load program
    if (!dbt_load_s32x(&cpu, filename)) {
        dbt_cpu_destroy(&cpu);
        return 1;
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
    }

    // Initialize block cache for Stage 2 and 3
    block_cache_t cache;
    if (stage >= 2) {
        if (!cache_init(&cache)) {
            fprintf(stderr, "Failed to initialize block cache\n");
            dbt_cpu_destroy(&cpu);
            return 1;
        }
        // Stage 3: Set up inline lookup table pointer in CPU state
        cpu.lookup_table = cache.blocks;
        cpu.lookup_mask = BLOCK_CACHE_MASK;
    }

    // Run
    struct timespec start, end;

    if (two_pass && stage != 4 && !two_pass_forced) {
        two_pass = false;
    }

    if (two_pass) {
        if (stage != 4) {
            fprintf(stderr, "Two-pass profiling is only supported for Stage 4.\n");
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
        run_dbt_stage4(&cpu, &cache);
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
    clock_gettime(CLOCK_MONOTONIC, &start);
#if defined(__x86_64__) || defined(__i386__)
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
    } else {
        run_dbt_stage4(&cpu, &cache);
    }

    clock_gettime(CLOCK_MONOTONIC, &end);
#if defined(__x86_64__) || defined(__i386__)
    if (profile_timing) {
        profile_tsc_end = rdtsc();
    }
#endif

    // Results
    int exit_code = cpu.regs[REG_RV];

    if (show_stats || verbose) {
        double elapsed = (end.tv_sec - start.tv_sec) +
                        (end.tv_nsec - start.tv_nsec) / 1e9;
        fprintf(stderr, "\n--- Statistics ---\n");
        fprintf(stderr, "Stage: %d\n", stage);
        fprintf(stderr, "Exit code: %d (0x%08X)\n", exit_code, (uint32_t)exit_code);
        fprintf(stderr, "Time: %.3f seconds\n", elapsed);
        if (profile_timing) {
#if defined(__x86_64__) || defined(__i386__)
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
