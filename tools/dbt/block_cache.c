// SLOW-32 DBT: Block Cache Implementation
// Stage 2 - Cache translated blocks and support direct chaining

#include "block_cache.h"
#include "cpu_state.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/mman.h>

// Maximum number of blocks we can store
#define MAX_BLOCKS 8192

// ============================================================================
// Cache management
// ============================================================================

bool cache_init(block_cache_t *cache) {
    memset(cache, 0, sizeof(*cache));

    // Allocate block metadata pool
    cache->block_pool = calloc(MAX_BLOCKS, sizeof(translated_block_t));
    if (!cache->block_pool) {
        return false;
    }
    cache->block_pool_size = MAX_BLOCKS;
    cache->block_pool_used = 0;

    // Allocate executable code buffer
    cache->code_buffer = mmap(NULL, CODE_BUFFER_SIZE,
                              PROT_READ | PROT_WRITE | PROT_EXEC,
                              MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (cache->code_buffer == MAP_FAILED) {
        free(cache->block_pool);
        cache->block_pool = NULL;
        return false;
    }
    cache->code_buffer_size = CODE_BUFFER_SIZE;
    cache->code_buffer_used = 0;

    // Emit dispatcher stub at start of code buffer
    // This is where unchained exits jump to - just 'ret' to C dispatcher
    cache->dispatcher_stub = cache->code_buffer;
    cache->code_buffer[0] = 0xC3;  // ret
    cache->code_buffer_used = 16;  // Align next code

    // Emit shared branch exit stub:
    //   mov dword [rbp + CPU_EXIT_REASON_OFFSET], EXIT_BRANCH  (10 bytes)
    //   jmp native_dispatcher                                   (5 bytes)
    // Total: 15 bytes
    {
        uint8_t *p = cache->code_buffer + cache->code_buffer_used;
        cache->shared_branch_exit = p;
        int32_t disp = (int32_t)CPU_EXIT_REASON_OFFSET;
        // mov dword [rbp + disp32], imm32
        // C7 85 <disp32> <imm32>
        *p++ = 0xC7;
        *p++ = 0x85;
        *p++ = (disp >> 0) & 0xFF;
        *p++ = (disp >> 8) & 0xFF;
        *p++ = (disp >> 16) & 0xFF;
        *p++ = (disp >> 24) & 0xFF;
        uint32_t reason = EXIT_BRANCH;
        *p++ = (reason >> 0) & 0xFF;
        *p++ = (reason >> 8) & 0xFF;
        *p++ = (reason >> 16) & 0xFF;
        *p++ = (reason >> 24) & 0xFF;
        // jmp rel32 (placeholder — patched after native_dispatcher is emitted)
        uint8_t *jmp_patch = p;
        *p++ = 0xE9;
        *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00;
        cache->code_buffer_used += (uint32_t)(p - cache->shared_branch_exit);
    }

    // Emit native dispatcher trampoline
    // Stays in generated code — does full linear-probe cache lookup.
    // On hit: jumps to block->host_code (keeps return address on stack).
    // On miss: ret to C dispatcher (execute_translated).
    //
    // Register usage (all free at any exit path):
    //   RBP = &cpu (preserved), R14 = mem_base (preserved), R15 = lookup_table (preserved)
    //   RAX, RCX, RDX, R8, R10 = scratch
    {
        uint8_t *p = cache->code_buffer + cache->code_buffer_used;
        cache->native_dispatcher = p;

        // Patch the jmp in shared_branch_exit to point here
        {
            uint8_t *jmp_site = cache->shared_branch_exit + 10; // after mov dword
            int32_t rel = (int32_t)(p - (jmp_site + 5));
            jmp_site[1] = (rel >> 0) & 0xFF;
            jmp_site[2] = (rel >> 8) & 0xFF;
            jmp_site[3] = (rel >> 16) & 0xFF;
            jmp_site[4] = (rel >> 24) & 0xFF;
        }

        int32_t pc_off = (int32_t)CPU_PC_OFFSET;
        int32_t mask_off = (int32_t)CPU_LOOKUP_MASK_OFFSET;

        // mov ecx, [rbp + pc_off]          ; ecx = cpu->pc
        *p++ = 0x8B; *p++ = 0x8D;
        *p++ = (pc_off >> 0) & 0xFF; *p++ = (pc_off >> 8) & 0xFF;
        *p++ = (pc_off >> 16) & 0xFF; *p++ = (pc_off >> 24) & 0xFF;

        // mov eax, ecx                     ; eax = pc
        *p++ = 0x89; *p++ = 0xC8;

        // shr eax, 2                       ; eax = pc >> 2
        *p++ = 0xC1; *p++ = 0xE8; *p++ = 0x02;

        // mov r11d, ecx                    ; r11d = pc
        *p++ = 0x44; *p++ = 0x89; *p++ = 0xCB;

        // shr r11d, 12                     ; r11d = pc >> 12
        *p++ = 0x41; *p++ = 0xC1; *p++ = 0xEB; *p++ = 0x0C;

        // xor eax, r11d                    ; eax ^= r11d
        *p++ = 0x44; *p++ = 0x31; *p++ = 0xD8;

        // mov r10d, [rbp + mask_off]       ; r10d = BLOCK_CACHE_MASK
        *p++ = 0x44; *p++ = 0x8B; *p++ = 0x95;
        *p++ = (mask_off >> 0) & 0xFF; *p++ = (mask_off >> 8) & 0xFF;
        *p++ = (mask_off >> 16) & 0xFF; *p++ = (mask_off >> 24) & 0xFF;

        // and eax, r10d                    ; hash &= mask
        *p++ = 0x44; *p++ = 0x21; *p++ = 0xD0;

        // mov r8d, eax                     ; r8d = start index
        *p++ = 0x41; *p++ = 0x89; *p++ = 0xC0;

        // .probe:
        uint8_t *probe_label = p;

        // mov rdx, [r15 + rax*8]           ; rdx = blocks[hash]
        *p++ = 0x49; *p++ = 0x8B; *p++ = 0x14; *p++ = 0xC7;

        // test rdx, rdx                    ; NULL check
        *p++ = 0x48; *p++ = 0x85; *p++ = 0xD2;

        // jz .miss (rel32)
        *p++ = 0x0F; *p++ = 0x84;
        uint8_t *jz_miss_patch = p;
        *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; // placeholder

        // cmp [rdx], ecx                   ; block->guest_pc == target?
        *p++ = 0x39; *p++ = 0x0A;

        // je .hit (rel32)
        *p++ = 0x0F; *p++ = 0x84;
        uint8_t *je_hit_patch = p;
        *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; *p++ = 0x00; // placeholder

        // inc eax
        *p++ = 0xFF; *p++ = 0xC0;

        // and eax, r10d                    ; wrap around
        *p++ = 0x44; *p++ = 0x21; *p++ = 0xD0;

        // cmp eax, r8d                     ; full circle?
        *p++ = 0x44; *p++ = 0x39; *p++ = 0xC0;

        // jne .probe
        *p++ = 0x75;
        *p = (uint8_t)(probe_label - (p + 1));
        p++;

        // .miss:
        {
            int32_t rel = (int32_t)(p - (jz_miss_patch + 4));
            jz_miss_patch[0] = (rel >> 0) & 0xFF;
            jz_miss_patch[1] = (rel >> 8) & 0xFF;
            jz_miss_patch[2] = (rel >> 16) & 0xFF;
            jz_miss_patch[3] = (rel >> 24) & 0xFF;
        }

        // ret                               ; return to C dispatcher
        *p++ = 0xC3;

        // .hit:
        {
            int32_t rel = (int32_t)(p - (je_hit_patch + 4));
            je_hit_patch[0] = (rel >> 0) & 0xFF;
            je_hit_patch[1] = (rel >> 8) & 0xFF;
            je_hit_patch[2] = (rel >> 16) & 0xFF;
            je_hit_patch[3] = (rel >> 24) & 0xFF;
        }

        // inc dword [rdx + 24]             ; exec_count++ (offset 24 in translated_block_t)
        *p++ = 0xFF; *p++ = 0x42; *p++ = 0x18;

        // jmp [rdx + 8]                    ; jmp block->host_code (offset 8)
        *p++ = 0xFF; *p++ = 0x62; *p++ = 0x08;

        cache->code_buffer_used += (uint32_t)(p - cache->native_dispatcher);
        // Align
        cache->code_buffer_used = (cache->code_buffer_used + 15) & ~15;
    }

    // Record where stubs end (for cache_flush)
    cache->stubs_end = cache->code_buffer_used;

    // Initialize chain pending index
    memset(cache->chain_pending_head, -1, sizeof(cache->chain_pending_head));
    cache->chain_pending_used = 0;

    return true;
}

void cache_destroy(block_cache_t *cache) {
    if (cache->code_buffer && cache->code_buffer != MAP_FAILED) {
        munmap(cache->code_buffer, cache->code_buffer_size);
    }
    if (cache->block_pool) {
        free(cache->block_pool);
    }
    memset(cache, 0, sizeof(*cache));
}

void cache_flush(block_cache_t *cache) {
    // Clear hash table
    memset(cache->blocks, 0, sizeof(cache->blocks));
    cache->block_count = 0;

    // Reset block pool
    memset(cache->block_pool, 0, cache->block_pool_used * sizeof(translated_block_t));
    cache->block_pool_used = 0;

    // Reset code buffer - keep dispatcher stub, shared_branch_exit, and native_dispatcher
    cache->code_buffer_used = cache->stubs_end;

    // Reset chain pending index
    memset(cache->chain_pending_head, -1, sizeof(cache->chain_pending_head));
    cache->chain_pending_used = 0;

    cache->flush_count++;
}

// ============================================================================
// Side-exit profiling
// ============================================================================

uint32_t cache_side_exit_taken_count(block_cache_t *cache, uint32_t branch_pc) {
    uint32_t idx = cache_hash(branch_pc);
    return cache->side_exit_taken_profile[idx];
}

uint32_t cache_side_exit_total_count(block_cache_t *cache, uint32_t branch_pc) {
    uint32_t idx = cache_hash(branch_pc);
    return cache->side_exit_total_profile[idx];
}

// ============================================================================
// Block lookup and insertion
// ============================================================================

translated_block_t *cache_lookup(block_cache_t *cache, uint32_t guest_pc) {
    cache->lookup_count++;

    uint32_t idx = cache_hash(guest_pc);
    uint32_t start = idx;

    do {
        translated_block_t *b = cache->blocks[idx];

        if (b == NULL) {
            // Empty slot - block not in cache
            return NULL;
        }

        if (b->guest_pc == guest_pc) {
            // Found it
            cache->lookup_hit_count++;
            b->exec_count++;
            return b;
        }

        // Linear probe to next slot
        idx = (idx + 1) & BLOCK_CACHE_MASK;
    } while (idx != start);

    // Table full and not found (shouldn't happen with proper sizing)
    return NULL;
}

translated_block_t *cache_alloc_block(block_cache_t *cache, uint32_t guest_pc) {
    // Check if pool is full
    if (cache->block_pool_used >= cache->block_pool_size) {
        return NULL;  // Caller should flush
    }

    // Allocate from pool
    translated_block_t *block = &cache->block_pool[cache->block_pool_used++];
    memset(block, 0, sizeof(*block));
    block->guest_pc = guest_pc;

    return block;
}

void cache_insert(block_cache_t *cache, translated_block_t *block) {
    uint32_t idx = cache_hash(block->guest_pc);

    // Find empty slot (linear probe)
    while (cache->blocks[idx] != NULL) {
        idx = (idx + 1) & BLOCK_CACHE_MASK;
    }

    cache->blocks[idx] = block;
    cache->block_count++;
}

// ============================================================================
// Code buffer management
// ============================================================================

uint8_t *cache_alloc_code(block_cache_t *cache, uint32_t size) {
    // Align to 16 bytes for performance
    uint32_t aligned_offset = (cache->code_buffer_used + 15) & ~15;

    if (aligned_offset + size > cache->code_buffer_size) {
        return NULL;  // Caller should flush
    }

    return cache->code_buffer + aligned_offset;
}

uint8_t *cache_get_code_ptr(block_cache_t *cache) {
    // Return next aligned position
    uint32_t aligned_offset = (cache->code_buffer_used + 15) & ~15;
    return cache->code_buffer + aligned_offset;
}

void cache_commit_code(block_cache_t *cache, uint32_t size) {
    uint32_t aligned_offset = (cache->code_buffer_used + 15) & ~15;
    cache->code_buffer_used = aligned_offset + size;
}

// ============================================================================
// Direct chaining
// ============================================================================

void cache_record_exit(block_cache_t *cache, translated_block_t *block,
                       int exit_idx, uint32_t target_pc, uint8_t *patch_site) {
    (void)cache;  // Unused for now

    if (exit_idx >= MAX_BLOCK_EXITS) {
        return;  // Shouldn't happen
    }

    block->exits[exit_idx].target_pc = target_pc;
    block->exits[exit_idx].patch_site = patch_site;
    block->exits[exit_idx].chained = false;
    // Preserve branch_pc if already set by the translator.

    if (exit_idx >= block->exit_count) {
        block->exit_count = exit_idx + 1;
    }
}

void cache_chain_incoming(block_cache_t *cache, translated_block_t *target) {
    // Use the O(1) pending chain index if available
    cache_chain_pending(cache, target);
}

void cache_record_pending_chain(block_cache_t *cache, translated_block_t *block,
                                int exit_idx, uint32_t target_pc) {
    if (cache->chain_pending_used >= CHAIN_PENDING_POOL_SIZE) {
        return;  // Pool exhausted
    }
    uint32_t hash = cache_hash(target_pc);
    uint32_t entry_idx = cache->chain_pending_used++;
    uint32_t block_idx = (uint32_t)(block - cache->block_pool);

    cache->chain_pending_entries[entry_idx].target_pc = target_pc;
    cache->chain_pending_entries[entry_idx].block_idx = block_idx;
    cache->chain_pending_entries[entry_idx].exit_idx = (uint8_t)exit_idx;
    cache->chain_pending_entries[entry_idx].next = cache->chain_pending_head[hash];
    cache->chain_pending_head[hash] = (int32_t)entry_idx;
}

void cache_chain_pending(block_cache_t *cache, translated_block_t *target) {
    uint32_t target_pc = target->guest_pc;
    uint32_t hash = cache_hash(target_pc);
    int32_t prev = -1;
    int32_t idx = cache->chain_pending_head[hash];

    while (idx >= 0) {
        int32_t next = cache->chain_pending_entries[idx].next;

        if (cache->chain_pending_entries[idx].target_pc == target_pc) {
            uint32_t block_idx = cache->chain_pending_entries[idx].block_idx;
            uint8_t exit_idx = cache->chain_pending_entries[idx].exit_idx;
            translated_block_t *b = &cache->block_pool[block_idx];

            if (b->host_code != NULL &&
                exit_idx < b->exit_count &&
                !b->exits[exit_idx].chained &&
                b->exits[exit_idx].patch_site != NULL &&
                b->exits[exit_idx].target_pc == target_pc) {

                cache_patch_jmp(b->exits[exit_idx].patch_site, target->host_code);
                b->exits[exit_idx].chained = true;
                cache->chain_count++;
            }

            // Remove from list
            if (prev >= 0) {
                cache->chain_pending_entries[prev].next = next;
            } else {
                cache->chain_pending_head[hash] = next;
            }
            // Don't update prev - it stays the same since we removed current
        } else {
            prev = idx;
        }

        idx = next;
    }
}

void cache_patch_jmp(uint8_t *patch_site, uint8_t *target) {
    // patch_site points to the rel32 offset of a jmp or jcc instruction
    // Calculate the relative offset from end of instruction
    // jmp rel32: opcode(1) + rel32(4) = 5 bytes total, so rel = target - (patch_site + 4)
    // jcc rel32: 0F opcode(2) + rel32(4) = 6 bytes, but patch_site is after opcode
    //            so rel = target - (patch_site + 4)

    int64_t rel = (int64_t)(target - (patch_site + 4));

    // Check if offset fits in 32 bits (should always be true with our buffer sizes)
    if (rel < INT32_MIN || rel > INT32_MAX) {
        fprintf(stderr, "cache_patch_jmp: offset out of range!\n");
        return;
    }

    // Write the new offset (little-endian)
    int32_t rel32 = (int32_t)rel;
    patch_site[0] = (rel32 >> 0) & 0xFF;
    patch_site[1] = (rel32 >> 8) & 0xFF;
    patch_site[2] = (rel32 >> 16) & 0xFF;
    patch_site[3] = (rel32 >> 24) & 0xFF;
}

// ============================================================================
// Statistics
// ============================================================================

void cache_print_stats(block_cache_t *cache) {
    fprintf(stderr, "\n=== Block Cache Statistics ===\n");
    fprintf(stderr, "Blocks cached: %u\n", cache->block_count);
    fprintf(stderr, "Code buffer:   %u / %u bytes (%.1f%%)\n",
            cache->code_buffer_used, cache->code_buffer_size,
            100.0 * cache->code_buffer_used / cache->code_buffer_size);
    fprintf(stderr, "Block pool:    %u / %u (%.1f%%)\n",
            cache->block_pool_used, cache->block_pool_size,
            100.0 * cache->block_pool_used / cache->block_pool_size);

    if (cache->lookup_count > 0) {
        fprintf(stderr, "Cache lookups: %lu (%.1f%% hit rate)\n",
                cache->lookup_count,
                100.0 * cache->lookup_hit_count / cache->lookup_count);
    }

    fprintf(stderr, "Chains made:   %lu\n", cache->chain_count);
    fprintf(stderr, "Cache flushes: %lu\n", cache->flush_count);

    // Stage 3 inline lookup stats
    if (cache->inline_miss_count > 0) {
        fprintf(stderr, "Inline lookup misses: %lu\n", cache->inline_miss_count);
    }

    if (cache->superblock_count > 0) {
        fprintf(stderr, "Superblocks:   %lu (side exits emitted: %lu)\n",
                cache->superblock_count, cache->side_exit_emitted);
        double avg_insts = (double)cache->superblock_inst_total /
                           (double)cache->superblock_count;
        double avg_exits = (double)cache->side_exit_emitted /
                           (double)cache->superblock_count;
        double host_bytes_per_inst = 0.0;
        if (cache->superblock_inst_total > 0) {
            host_bytes_per_inst =
                (double)cache->superblock_host_bytes_total /
                (double)cache->superblock_inst_total;
        }
        fprintf(stderr, "Superblock avg: insts=%.1f exits=%.2f hostB/inst=%.2f\n",
                avg_insts, avg_exits, host_bytes_per_inst);
        fprintf(stderr, "Superblock diagnostics:\n");
        for (uint32_t i = 0; i < cache->block_pool_used; i++) {
            translated_block_t *b = &cache->block_pool[i];
            if (b->host_code == NULL || b->side_exit_count == 0) {
                continue;
            }
            const char *avoid_note = "";
            uint32_t back_edges = 0;
            uint32_t max_taken = 0;
            uint32_t max_total = 0;
            uint32_t max_pc = 0;
            for (uint32_t e = 0; e < b->exit_count; e++) {
                block_exit_t *ex = &b->exits[e];
                if (ex->branch_pc == 0) {
                    continue;
                }
                if (ex->target_pc < ex->branch_pc) {
                    back_edges++;
                }
                uint32_t idx = cache_hash(ex->branch_pc);
                uint32_t total = cache->side_exit_total_profile[idx];
                uint32_t taken = cache->side_exit_taken_profile[idx];
                if (total > 0 && taken >= max_taken) {
                    max_taken = taken;
                    max_total = total;
                    max_pc = ex->branch_pc;
                }
            }
            uint32_t end_pc = b->guest_pc + b->guest_size - 4;
            if (back_edges > 0 && b->exec_count > 1000) {
                avoid_note = " avoid(back_edge+hot)";
            }
            if (max_total > 0) {
                if (max_taken <= max_total) {
                    double pct = (double)max_taken * 100.0 / (double)max_total;
                    fprintf(stderr,
                            "  SB 0x%08X-0x%08X exits=%u back_edges=%u max_taken=%.2f%% (pc 0x%08X t=%u/%u) execs=%u%s\n",
                            b->guest_pc, end_pc, b->side_exit_count, back_edges,
                            pct, max_pc, max_taken, max_total, b->exec_count, avoid_note);
                } else {
                    fprintf(stderr,
                            "  SB 0x%08X-0x%08X exits=%u back_edges=%u max_taken=na (pc 0x%08X t=%u/%u) execs=%u%s\n",
                            b->guest_pc, end_pc, b->side_exit_count, back_edges,
                            max_pc, max_taken, max_total, b->exec_count, avoid_note);
                }
            } else {
                fprintf(stderr,
                        "  SB 0x%08X-0x%08X exits=%u back_edges=%u max_taken=n/a execs=%u%s\n",
                        b->guest_pc, end_pc, b->side_exit_count, back_edges,
                        b->exec_count, avoid_note);
            }
        }
    }
    if (cache->peephole_hits > 0) {
        fprintf(stderr, "Peephole rewrites: %lu\n", cache->peephole_hits);
    }

    // Side-exit profile summary (debug)
    uint32_t max_total = 0;
    uint32_t max_taken = 0;
    for (uint32_t i = 0; i < BLOCK_CACHE_SIZE; i++) {
        if (cache->side_exit_total_profile[i] > max_total) {
            max_total = cache->side_exit_total_profile[i];
        }
        if (cache->side_exit_taken_profile[i] > max_taken) {
            max_taken = cache->side_exit_taken_profile[i];
        }
    }
    if (max_total > 0 || max_taken > 0) {
        fprintf(stderr, "Side-exit profile max: total=%u taken=%u\n",
                max_total, max_taken);
        fprintf(stderr, "Top side-exit offenders (by taken rate):\n");

        struct {
            uint32_t pc;
            uint32_t total;
            uint32_t taken;
        } top[5] = {0};

        if (cache->side_exit_pc_count > 0) {
            for (uint32_t i = 0; i < cache->side_exit_pc_count; i++) {
                uint32_t pc = cache->side_exit_pc_list[i];
                uint32_t idx = cache_hash(pc);
                uint32_t total = cache->side_exit_total_profile[idx];
                uint32_t taken = cache->side_exit_taken_profile[idx];
                if (total == 0) {
                    continue;
                }
                for (int j = 0; j < 5; j++) {
                    uint32_t top_total = top[j].total;
                    uint32_t top_taken = top[j].taken;
                    uint32_t lhs = taken * (top_total ? top_total : 1);
                    uint32_t rhs = top_taken * total;
                    if (top[j].total == 0 || lhs > rhs) {
                        for (int k = 4; k > j; k--) {
                            top[k] = top[k - 1];
                        }
                        top[j].pc = pc;
                        top[j].total = total;
                        top[j].taken = taken;
                        break;
                    }
                }
            }
        } else {
            for (uint32_t i = 0; i < BLOCK_CACHE_SIZE; i++) {
                uint32_t total = cache->side_exit_total_profile[i];
                uint32_t taken = cache->side_exit_taken_profile[i];
                uint32_t pc = cache->side_exit_pc_hint[i];
                if (total == 0 || pc == 0) {
                    continue;
                }
                for (int j = 0; j < 5; j++) {
                    uint32_t top_total = top[j].total;
                    uint32_t top_taken = top[j].taken;
                    uint32_t lhs = taken * (top_total ? top_total : 1);
                    uint32_t rhs = top_taken * total;
                    if (top[j].total == 0 || lhs > rhs) {
                        for (int k = 4; k > j; k--) {
                            top[k] = top[k - 1];
                        }
                        top[j].pc = pc;
                        top[j].total = total;
                        top[j].taken = taken;
                        break;
                    }
                }
            }
        }

        for (int i = 0; i < 5 && top[i].total > 0; i++) {
            uint32_t pct = (top[i].taken * 100) / top[i].total;
            fprintf(stderr, "  0x%08X: taken=%u total=%u (%u%%)\n",
                    top[i].pc, top[i].taken, top[i].total, pct);
        }
    }

    // Find hottest blocks
    fprintf(stderr, "\nTop 5 hottest blocks:\n");
    translated_block_t *hot[5] = {0};

    for (uint32_t i = 0; i < cache->block_pool_used; i++) {
        translated_block_t *b = &cache->block_pool[i];
        if (b->host_code == NULL) continue;

        // Insert into sorted hot list
        for (int j = 0; j < 5; j++) {
            if (hot[j] == NULL || b->exec_count > hot[j]->exec_count) {
                // Shift others down
                for (int k = 4; k > j; k--) {
                    hot[k] = hot[k-1];
                }
                hot[j] = b;
                break;
            }
        }
    }

    for (int i = 0; i < 5 && hot[i]; i++) {
        if (hot[i]->reg_cache_hits > 0 || hot[i]->reg_cache_misses > 0) {
            uint32_t total = hot[i]->reg_cache_hits + hot[i]->reg_cache_misses;
            uint32_t pct = total ? (hot[i]->reg_cache_hits * 100) / total : 0;
            fprintf(stderr,
                    "  0x%08X: %u executions, %u bytes host code, regcache=%u/%u (%u%%)\n",
                    hot[i]->guest_pc, hot[i]->exec_count, hot[i]->host_size,
                    hot[i]->reg_cache_hits, total, pct);
        } else {
            fprintf(stderr, "  0x%08X: %u executions, %u bytes host code\n",
                    hot[i]->guest_pc, hot[i]->exec_count, hot[i]->host_size);
        }
    }
}

void cache_reset_stats(block_cache_t *cache) {
    cache->lookup_count = 0;
    cache->lookup_hit_count = 0;
    cache->chain_count = 0;
    cache->flush_count = 0;
    cache->indirect_count = 0;
    cache->inline_hit_count = 0;
    cache->inline_miss_count = 0;
    cache->ras_push_count = 0;
    cache->ras_hit_count = 0;
    cache->ras_miss_count = 0;
    cache->superblock_count = 0;
    cache->side_exit_emitted = 0;
    cache->superblock_inst_total = 0;
    cache->superblock_guest_bytes_total = 0;
    cache->superblock_host_bytes_total = 0;
    cache->peephole_hits = 0;
    cache->side_exit_pc_count = 0;
}

static void dump_bytes(const uint8_t *data, uint32_t len, uint32_t max_len) {
    uint32_t n = len < max_len ? len : max_len;
    for (uint32_t i = 0; i < n; i++) {
        fprintf(stderr, "%02X", data[i]);
        if (i + 1 < n) {
            fputc(' ', stderr);
        }
    }
    if (len > max_len) {
        fprintf(stderr, " ...");
    }
    fputc('\n', stderr);
}

static void dump_objdump(const uint8_t *data, uint32_t len, uint32_t pc) {
    char path[256];
    snprintf(path, sizeof(path), "/tmp/slow32-dbt-host-0x%08X.bin", pc);
    FILE *f = fopen(path, "wb");
    if (!f) {
        fprintf(stderr, "  objdump: failed to open %s\n", path);
        return;
    }
    fwrite(data, 1, len, f);
    fclose(f);

    char cmd[512];
    snprintf(cmd, sizeof(cmd),
             "objdump -D -b binary -m i386:x86-64 %s 2>/dev/null",
             path);
    int rc = system(cmd);
    if (rc != 0) {
        fprintf(stderr, "  objdump: failed for %s\n", path);
    }
}

void cache_dump_block_for_pc(block_cache_t *cache, uint8_t *mem_base, uint32_t pc,
                             bool disassemble) {
    translated_block_t *owner = NULL;
    for (uint32_t b = 0; b < cache->block_pool_used; b++) {
        translated_block_t *blk = &cache->block_pool[b];
        if (!blk->host_code) {
            continue;
        }
        uint32_t start = blk->guest_pc;
        uint32_t end = blk->guest_pc + blk->guest_size;
        if (pc >= start && pc < end) {
            owner = blk;
            break;
        }
    }

    if (!owner) {
        fprintf(stderr, "No owning block found for 0x%08X.\n", pc);
        return;
    }

    fprintf(stderr, "\n=== Block Dump for 0x%08X ===\n", pc);
    fprintf(stderr, "  Block guest: 0x%08X (size %u bytes), host size %u bytes\n",
            owner->guest_pc, owner->guest_size, owner->host_size);
    fprintf(stderr, "  Guest bytes: ");
    dump_bytes(mem_base + owner->guest_pc, owner->guest_size, 64);
    fprintf(stderr, "  Host bytes:  ");
    dump_bytes(owner->host_code, owner->host_size, 128);
    if (disassemble) {
        fprintf(stderr, "  Host disassembly:\n");
        dump_objdump(owner->host_code, owner->host_size, owner->guest_pc);
    }
}

void cache_dump_offender_blocks(block_cache_t *cache, uint8_t *mem_base, bool disassemble) {
    struct {
        uint32_t pc;
        uint32_t total;
        uint32_t taken;
    } top[5] = {0};

    if (cache->side_exit_pc_count == 0) {
        fprintf(stderr, "No side-exit offenders recorded.\n");
        return;
    }

    for (uint32_t i = 0; i < cache->side_exit_pc_count; i++) {
        uint32_t pc = cache->side_exit_pc_list[i];
        uint32_t idx = cache_hash(pc);
        uint32_t total = cache->side_exit_total_profile[idx];
        uint32_t taken = cache->side_exit_taken_profile[idx];
        if (total == 0) {
            continue;
        }
        for (int j = 0; j < 5; j++) {
            uint32_t top_total = top[j].total;
            uint32_t top_taken = top[j].taken;
            uint32_t lhs = taken * (top_total ? top_total : 1);
            uint32_t rhs = top_taken * total;
            if (top[j].total == 0 || lhs > rhs) {
                for (int k = 4; k > j; k--) {
                    top[k] = top[k - 1];
                }
                top[j].pc = pc;
                top[j].total = total;
                top[j].taken = taken;
                break;
            }
        }
    }

    fprintf(stderr, "\n=== Offender Block Dump ===\n");
    for (int i = 0; i < 5 && top[i].total > 0; i++) {
        uint32_t pc = top[i].pc;
        uint32_t pct = (top[i].taken * 100) / top[i].total;
        fprintf(stderr, "Offender %d: 0x%08X (taken=%u total=%u %u%%)\n",
                i + 1, pc, top[i].taken, top[i].total, pct);

        translated_block_t *owner = NULL;
        for (uint32_t b = 0; b < cache->block_pool_used; b++) {
            translated_block_t *blk = &cache->block_pool[b];
            if (!blk->host_code) {
                continue;
            }
            uint32_t start = blk->guest_pc;
            uint32_t end = blk->guest_pc + blk->guest_size;
            if (pc >= start && pc < end) {
                owner = blk;
                break;
            }
        }

        if (!owner) {
            fprintf(stderr, "  No owning block found.\n");
            continue;
        }

        fprintf(stderr, "  Block guest: 0x%08X (size %u bytes), host size %u bytes\n",
                owner->guest_pc, owner->guest_size, owner->host_size);
        fprintf(stderr, "  Guest bytes: ");
        dump_bytes(mem_base + owner->guest_pc, owner->guest_size, 64);
        fprintf(stderr, "  Host bytes:  ");
        dump_bytes(owner->host_code, owner->host_size, 128);
        if (disassemble) {
            fprintf(stderr, "  Host disassembly:\n");
            dump_objdump(owner->host_code, owner->host_size, owner->guest_pc);
        }
    }
}
