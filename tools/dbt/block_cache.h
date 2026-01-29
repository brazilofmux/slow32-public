// SLOW-32 DBT: Block Cache
// Stage 2 - Cache translated blocks and support direct chaining

#ifndef DBT_BLOCK_CACHE_H
#define DBT_BLOCK_CACHE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#include "dbt_limits.h"

// Forward declaration
typedef struct translated_block translated_block_t;

// Block cache size (power of 2 for fast modulo)
#define BLOCK_CACHE_SIZE 4096
#define BLOCK_CACHE_MASK (BLOCK_CACHE_SIZE - 1)

// Code buffer size for translated code
#define CODE_BUFFER_SIZE (4 * 1024 * 1024)  // 4MB

// Block flags
#define BLOCK_FLAG_DIRECT    0x01   // All exits are direct (can be chained)
#define BLOCK_FLAG_INDIRECT  0x02   // Has indirect exit (JALR)
#define BLOCK_FLAG_CALL      0x04   // Ends with JAL (call)
#define BLOCK_FLAG_RETURN    0x08   // Ends with JR r31 (return)

// Exit slot for tracking chain targets
typedef struct {
    uint32_t target_pc;         // Guest target address (0 if indirect)
    uint8_t *patch_site;        // Address of rel32 in the jmp/jcc instruction
    bool chained;               // Already patched to target?
    uint32_t branch_pc;         // Guest PC of branch that produced this exit (0 if none)
} block_exit_t;

// Translated block metadata
struct translated_block {
    uint32_t guest_pc;          // Guest start address (hash key)
    uint32_t guest_size;        // Bytes of guest code (inst_count * 4)
    uint8_t *host_code;         // Pointer to translated x86-64 code
    uint32_t host_size;         // Bytes of host code
    uint32_t flags;             // BLOCK_FLAG_*
    uint32_t exec_count;        // Execution counter (for profiling)
    uint32_t reg_cache_hits;    // Reg-cache hits during translation
    uint32_t reg_cache_misses;  // Reg-cache misses during translation

    // Exit tracking for chaining
    uint8_t exit_count;         // Number of exits (0-MAX_BLOCK_EXITS)
    block_exit_t exits[MAX_BLOCK_EXITS];

    // Stage 4: Side-exit profiling (per-block)
    uint8_t side_exit_count;
    uint32_t side_exit_pcs[MAX_BLOCK_EXITS];
};

// Block cache
struct block_cache {
    // Hash table of blocks (open addressing with linear probing)
    translated_block_t *blocks[BLOCK_CACHE_SIZE];
    uint32_t block_count;

    // Memory pool for block metadata
    translated_block_t *block_pool;
    uint32_t block_pool_size;
    uint32_t block_pool_used;

    // Code buffer for translated x86-64 code
    uint8_t *code_buffer;
    uint32_t code_buffer_size;
    uint32_t code_buffer_used;

    // Dispatcher stub address (for unchained exits)
    uint8_t *dispatcher_stub;

    // Shared exit stubs (emitted once in code buffer)
    uint8_t *shared_branch_exit;    // mov [rbp+EXIT_REASON], EXIT_BRANCH; ret

    // Statistics
    uint64_t lookup_count;
    uint64_t lookup_hit_count;
    uint64_t chain_count;       // Successful chains
    uint64_t flush_count;       // Cache flushes

    // Stage 3: Inline lookup statistics
    uint64_t indirect_count;    // Total indirect branches executed
    uint64_t inline_hit_count;  // Inline lookup hits
    uint64_t inline_miss_count; // Inline lookup misses (fell back to dispatcher)

    // Stage 3 Phase 2: RAS statistics
    uint64_t ras_push_count;    // RAS pushes (calls)
    uint64_t ras_hit_count;     // RAS prediction hits
    uint64_t ras_miss_count;    // RAS prediction misses

    // Stage 4: Superblock statistics
    uint64_t superblock_count;          // Blocks that emitted side exits
    uint64_t side_exit_emitted;         // Total side exits emitted
    uint64_t superblock_inst_total;     // Total guest insts in superblocks
    uint64_t superblock_guest_bytes_total; // Total guest bytes in superblocks
    uint64_t superblock_host_bytes_total;  // Total host bytes in superblocks
    uint64_t peephole_hits;        // Number of peephole rewrites (study only)

    // Chain pending index: reverse lookup for O(1) chaining
    // chain_pending[hash] = index into chain_pending_entries[], or -1
    #define CHAIN_PENDING_POOL_SIZE 4096
    int32_t chain_pending_head[BLOCK_CACHE_SIZE];  // head of linked list per target_pc hash
    struct {
        uint32_t target_pc;         // guest PC this exit wants to reach
        uint32_t block_idx;         // index into block_pool
        uint8_t exit_idx;           // which exit slot
        int32_t next;               // next entry in chain, or -1
    } chain_pending_entries[CHAIN_PENDING_POOL_SIZE];
    uint32_t chain_pending_used;

    // Stage 4: Side-exit profile (indexed by cache_hash(guest_pc))
    uint32_t side_exit_taken_profile[BLOCK_CACHE_SIZE];
    uint32_t side_exit_total_profile[BLOCK_CACHE_SIZE];
    uint32_t side_exit_pc_hint[BLOCK_CACHE_SIZE];
    uint32_t side_exit_pc_list[1024];
    uint32_t side_exit_pc_count;
};
typedef struct block_cache block_cache_t;

// ============================================================================
// Cache management
// ============================================================================

// Initialize block cache
// Returns true on success, false on allocation failure
bool cache_init(block_cache_t *cache);

// Destroy block cache and free resources
void cache_destroy(block_cache_t *cache);

// Flush all blocks (reset cache)
void cache_flush(block_cache_t *cache);

// ============================================================================
// Block lookup and insertion
// ============================================================================

// Look up a block by guest PC
// Returns NULL if not found
translated_block_t *cache_lookup(block_cache_t *cache, uint32_t guest_pc);

// Allocate a new block for translation
// Returns NULL if cache is full (triggers flush)
translated_block_t *cache_alloc_block(block_cache_t *cache, uint32_t guest_pc);

// Insert a block into the cache after translation
void cache_insert(block_cache_t *cache, translated_block_t *block);

// ============================================================================
// Code buffer management
// ============================================================================

// Allocate space in code buffer
// Returns NULL if full (triggers flush)
uint8_t *cache_alloc_code(block_cache_t *cache, uint32_t size);

// Get current code buffer pointer (for emitting)
uint8_t *cache_get_code_ptr(block_cache_t *cache);

// Commit code after emission (updates used count)
void cache_commit_code(block_cache_t *cache, uint32_t size);

// ============================================================================
// Direct chaining
// ============================================================================

// Record an exit from a block
// Called during translation when emitting a branch/jump
void cache_record_exit(block_cache_t *cache, translated_block_t *block,
                       int exit_idx, uint32_t target_pc, uint8_t *patch_site);

// Chain incoming blocks to a newly translated block
// Patches all blocks that have unchained exits to target_pc
void cache_chain_incoming(block_cache_t *cache, translated_block_t *target);

// Patch a jmp rel32 instruction to point to a new target
// patch_site points to the first byte of the rel32 offset
void cache_patch_jmp(uint8_t *patch_site, uint8_t *target);

// Record a pending chain entry (for O(1) chaining)
void cache_record_pending_chain(block_cache_t *cache, translated_block_t *block,
                                int exit_idx, uint32_t target_pc);

// Chain incoming blocks using the pending index (O(1) per target)
void cache_chain_pending(block_cache_t *cache, translated_block_t *target);

// ============================================================================
// Statistics
// ============================================================================

// Print cache statistics
void cache_print_stats(block_cache_t *cache);
void cache_reset_stats(block_cache_t *cache);
void cache_dump_offender_blocks(block_cache_t *cache, uint8_t *mem_base, bool disassemble);
void cache_dump_block_for_pc(block_cache_t *cache, uint8_t *mem_base, uint32_t pc,
                             bool disassemble);

// Side-exit profiling helpers
uint32_t cache_side_exit_taken_count(block_cache_t *cache, uint32_t branch_pc);
uint32_t cache_side_exit_total_count(block_cache_t *cache, uint32_t branch_pc);

// ============================================================================
// Hash function
// ============================================================================

static inline uint32_t cache_hash(uint32_t guest_pc) {
    // SLOW-32 instructions are 4-byte aligned
    // Shift out low 2 bits and mask to table size
    return (guest_pc >> 2) & BLOCK_CACHE_MASK;
}

#endif // DBT_BLOCK_CACHE_H
