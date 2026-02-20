// SLOW-32 Fast Emulator - Optimized with pre-decode and function pointer dispatch
// Metadata-driven memory layout from s32x headers

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <inttypes.h>
#include <signal.h>
#include <sys/time.h>
#include <sys/mman.h>
#include <math.h>
#include "slow32.h"
#include "s32x_loader.h"
#include "memory_manager.h"
#include "mmio_ring.h"

// Enable to trap on unaligned LD/ST (strict ISA mode)
// Disabled by default: SLOW-32 runs on x86-64 which handles unaligned access,
// and the toolchain doesn't yet enforce alignment for all data items.
#ifdef S32_STRICT_ALIGNMENT
#define S32_TRAP_ON_UNALIGNED 1
#else
#define S32_TRAP_ON_UNALIGNED 0
#endif

// Enable to trap on odd/out-of-range register numbers for f64 operations.
// Disabled by default: x86-64 won't crash, but regs[31+1] silently corrupts PC.
#ifdef S32_STRICT_ALIGNMENT
#define S32_CHECK_F64_REGS 1
#else
#define S32_CHECK_F64_REGS 0
#endif

#define S32_MMIO_WINDOW_SIZE \
    (S32_MMIO_DATA_BUFFER_OFFSET + S32_MMIO_DATA_CAPACITY)
#define S32_MMIO_REGION_SIZE 0x00010000u
#define UNUSED(x) (void)(x)

// ============================================================
// Signal-based probe/debugging support
// ============================================================

// Sorted function symbol table for PC → function name lookup
typedef struct { uint32_t addr; const char *name; } func_sym_t;
typedef struct {
    func_sym_t *syms;
    uint32_t count;
    s32x_symtab_result_t raw;  // Owns the memory (symbols + strtab)
} sorted_symtab_t;

// Probe state (ring buffer + timing)
#define PROBE_RING_SIZE 8
typedef struct {
    volatile sig_atomic_t flag;        // Set by SIGALRM, cleared by main loop
    volatile sig_atomic_t sigint_flag; // Set by SIGINT
    uint32_t pc_ring[PROBE_RING_SIZE]; // Ring buffer for loop detection
    int ring_pos, ring_count;
    struct timespec start_time;
    uint64_t last_inst_count;
    int probe_interval_sec;
    int probe_count;
    int full_dump_interval;            // Full dump every N probes
    bool enabled;
} probe_state_t;

// Forward declarations
typedef struct fast_cpu_state fast_cpu_state_t;
typedef struct decoded_inst decoded_inst_t;

// Handler function type
typedef void (*handler_fn)(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc);

// Pre-decoded instruction with handler pointer
struct decoded_inst {
    handler_fn handler;
    int32_t imm;
    uint8_t rd;
    uint8_t rs1;
    uint8_t rs2;
    uint8_t pad;  // keeps the struct 16 bytes and available for future flags
};

typedef struct {
    uint32_t start;
    uint32_t end;
    memory_region_t *region;
} region_cache_entry_t;

struct fast_cpu_state {
    uint32_t regs[32];
    uint32_t pc;
    memory_manager_t mm;  // Memory manager instead of raw pointer
    decoded_inst_t *decoded_code;
    uint32_t code_words;
    bool halted;
    uint64_t cycle_count;
    uint64_t inst_count;
    
    // Memory protection limits from .s32x loader
    bool wx_enabled;
    uint32_t code_limit;
    uint32_t rodata_limit;
    uint32_t data_limit;
    
    // MMIO support
    mmio_device_t mmio;
    
    region_cache_entry_t region_cache[2];
    uint8_t region_cache_next;
};

// Globals for signal handler access
static fast_cpu_state_t *g_cpu = NULL;
static probe_state_t *g_probe = NULL;
static sorted_symtab_t g_symtab = {0};


// MMIO initialization
static void cpu_init_mmio(fast_cpu_state_t *cpu) {
    if (cpu->mmio.initialized || !cpu->mmio.enabled) {
        return;
    }

    // Allocate MMIO state
    cpu->mmio.state = calloc(1, sizeof(mmio_ring_state_t));
    if (!cpu->mmio.state) {
        fprintf(stderr, "Failed to allocate MMIO state\n");
        cpu->mmio.enabled = false;
        return;
    }

    mmio_ring_init(cpu->mmio.state);

    // Map MMIO window into host memory and register with the memory manager
    cpu->mmio.mem = mmio_ring_map(cpu->mmio.state);
    if (!cpu->mmio.mem) {
        fprintf(stderr, "Failed to map MMIO memory\n");
        mmio_ring_clear_args(cpu->mmio.state);
        free(cpu->mmio.state);
        cpu->mmio.state = NULL;
        cpu->mmio.enabled = false;
        return;
    }

    memory_region_t *mmio_region = mm_allocate_region(&cpu->mm, cpu->mmio.base,
                                                      S32_MMIO_REGION_SIZE,
                                                      PROT_READ | PROT_WRITE);
    if (!mmio_region) {
        fprintf(stderr, "Failed to allocate MMIO region\n");
        munmap(cpu->mmio.mem, S32_MMIO_WINDOW_SIZE);
        cpu->mmio.mem = NULL;
        mmio_ring_clear_args(cpu->mmio.state);
        free(cpu->mmio.state);
        cpu->mmio.state = NULL;
        cpu->mmio.enabled = false;
        return;
    }

    mmio_region->host_addr = cpu->mmio.mem;
    cpu->mmio.state->base_addr = cpu->mmio.base;
    cpu->mmio.initialized = true;
}

// Adapter function to process MMIO without cpu_state
static void mmio_process_fast(fast_cpu_state_t *cpu) {
    mmio_cpu_iface_t iface = { .halted = &cpu->halted, .exit_status = &cpu->regs[1] };
    mmio_ring_process(cpu->mmio.state, &iface);
}

// MMIO write wrapper for fast CPU
static void mmio_write_fast(fast_cpu_state_t *cpu, uint32_t addr, uint32_t value, int size) {
    if (size != 4) {
        uint32_t word_addr = addr & ~0x3u;
        uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
        int shift = (addr & 0x3u) * 8;

        if (size == 1) {
            word = (word & ~(0xFFu << shift)) | ((value & 0xFFu) << shift);
        } else if (size == 2) {
            word = (word & ~(0xFFFFu << shift)) | ((value & 0xFFFFu) << shift);
        } else {
            return;  // Unsupported width
        }

        addr = word_addr;
        value = word;
        size = 4;
    }

    mmio_ring_write(cpu->mmio.state, NULL, addr, value, size);
}

// Write callback for s32x loader
static int mm_write_callback(void *user_data, uint32_t addr, const void *data, uint32_t size) {
    fast_cpu_state_t *cpu = (fast_cpu_state_t *)user_data;
    return mm_write(&cpu->mm, addr, data, size);
}


static inline bool aligned(uint32_t addr, uint32_t size) {
#if S32_TRAP_ON_UNALIGNED
    return (addr & (size - 1)) == 0;
#else
    UNUSED(addr);
    UNUSED(size);
    return true;
#endif
}

static inline memory_region_t *region_cache_lookup(fast_cpu_state_t *cpu, uint32_t addr, uint32_t size, int prot_flags) {
    uint32_t end = addr + size;
    if (end < addr) {
        return NULL;  // overflow
    }

    for (int i = 0; i < 2; ++i) {
        region_cache_entry_t *entry = &cpu->region_cache[i];
        memory_region_t *cached = entry->region;
        if (!cached) {
            continue;
        }
        if (addr >= entry->start && end <= entry->end) {
            if ((cached->prot_flags & prot_flags) == prot_flags) {
                return cached;
            }
            break;  // permissions don't match; fall through to fresh lookup
        }
    }

    memory_region_t *region = mm_find_region(&cpu->mm, addr);
    if (!region) {
        return NULL;
    }
    if (end > region->vaddr_end) {
        return NULL;
    }
    if ((region->prot_flags & prot_flags) != prot_flags) {
        return NULL;
    }

    region_cache_entry_t *slot = &cpu->region_cache[cpu->region_cache_next & 1u];
    slot->start = region->vaddr_start;
    slot->end = region->vaddr_end;
    slot->region = region;
    cpu->region_cache_next ^= 1u;
    return region;
}

static inline uint8_t *region_host_ptr(memory_region_t *region, uint32_t addr) {
    return (uint8_t *)region->host_addr + (addr - region->vaddr_start);
}

static inline bool mm_load_bytes_fast(fast_cpu_state_t *cpu, uint32_t addr, void *dest, uint32_t size) {
    memory_region_t *region = region_cache_lookup(cpu, addr, size, PROT_READ);
    if (!region) {
        return false;
    }
    memcpy(dest, region_host_ptr(region, addr), size);
    return true;
}

static inline bool mm_store_bytes_fast(fast_cpu_state_t *cpu, uint32_t addr, const void *src, uint32_t size) {
    memory_region_t *region = region_cache_lookup(cpu, addr, size, PROT_WRITE);
    if (!region) {
        return false;
    }
    memcpy(region_host_ptr(region, addr), src, size);
    return true;
}

static inline bool is_mmio_addr(fast_cpu_state_t *cpu, uint32_t addr) {
    return cpu->mmio.initialized &&
           __builtin_expect((uint32_t)(addr - cpu->mmio.base) < S32_MMIO_REGION_SIZE, 0);
}

// Handler implementations
static void op_add(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] + cpu->regs[inst->rs2];
}

static void op_sub(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] - cpu->regs[inst->rs2];
}

static void op_xor(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] ^ cpu->regs[inst->rs2];
}

static void op_or(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] | cpu->regs[inst->rs2];
}

static void op_and(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] & cpu->regs[inst->rs2];
}

static void op_sll(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] << (cpu->regs[inst->rs2] & 0x1F);
}

static void op_srl(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] >> (cpu->regs[inst->rs2] & 0x1F);
}

static void op_sra(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] >> (cpu->regs[inst->rs2] & 0x1F);
}

static void op_slt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] < (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sltu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] < cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_mul(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] * cpu->regs[inst->rs2];
    cpu->cycle_count += 31;
}

static void op_mulh(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    int64_t result = (int64_t)(int32_t)cpu->regs[inst->rs1] * (int64_t)(int32_t)cpu->regs[inst->rs2];
    cpu->regs[inst->rd] = result >> 32;
    cpu->cycle_count += 31;
}

static void op_mulhu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint64_t result = (uint64_t)cpu->regs[inst->rs1] * (uint64_t)cpu->regs[inst->rs2];
    cpu->regs[inst->rd] = result >> 32;
    cpu->cycle_count += 31;
}

static void op_div(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t divisor = cpu->regs[inst->rs2];
    if (divisor == 0) {
        cpu->regs[inst->rd] = -1;
    } else if (cpu->regs[inst->rs1] == 0x80000000 && divisor == 0xFFFFFFFF) {
        cpu->regs[inst->rd] = 0x80000000;  // INT32_MIN / -1 = INT32_MIN
    } else {
        cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] / (int32_t)divisor;
    }
    cpu->cycle_count += 63;
}

static void op_rem(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t divisor = cpu->regs[inst->rs2];
    if (divisor == 0) {
        cpu->regs[inst->rd] = cpu->regs[inst->rs1];
    } else if (cpu->regs[inst->rs1] == 0x80000000 && divisor == 0xFFFFFFFF) {
        cpu->regs[inst->rd] = 0;  // INT32_MIN % -1 = 0
    } else {
        cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] % (int32_t)divisor;
    }
    cpu->cycle_count += 63;
}

static void op_seq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] == cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sne(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] > (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgtu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] > cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sle(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] <= (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sleu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] <= cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sge(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] >= (int32_t)cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_sgeu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] >= cpu->regs[inst->rs2]) ? 1 : 0;
}

static void op_addi(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] + inst->imm;
}

static void op_ori(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] | inst->imm;
}

static void op_andi(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] & inst->imm;
}

static void op_xori(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] ^ inst->imm;
}

static void op_slli(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] << (inst->imm & 0x1F);
}

static void op_srli(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] >> (inst->imm & 0x1F);
}

static void op_srai(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (int32_t)cpu->regs[inst->rs1] >> (inst->imm & 0x1F);
}

static void op_slti(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = ((int32_t)cpu->regs[inst->rs1] < inst->imm) ? 1 : 0;
}

static void op_sltiu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = (cpu->regs[inst->rs1] < (uint32_t)inst->imm) ? 1 : 0;
}

static void op_lui(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = inst->imm;
}

static void op_ldw(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!aligned(addr,4)) {
        fprintf(stderr, "Error: Unaligned read at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    
    // Check for MMIO access
    if (is_mmio_addr(cpu, addr)) {
        uint32_t value = mmio_ring_read(cpu->mmio.state, addr, 4);
        cpu->regs[inst->rd] = value;
        cpu->cycle_count += 3;
        return;
    }

    uint32_t value;
    if (!mm_load_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = value;
    cpu->cycle_count += 3;
}

static void op_ldh(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!aligned(addr,2)) {
        fprintf(stderr, "Error: Unaligned read at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (is_mmio_addr(cpu, addr)) {
        uint32_t word_addr = addr & ~0x3u;
        uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
        int shift = (addr & 0x3u) * 8;
        uint16_t value = (word >> shift) & 0xFFFFu;
        cpu->regs[inst->rd] = (int32_t)(int16_t)value;
        cpu->cycle_count += 3;
        return;
    }
    uint16_t value;
    if (!mm_load_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = (int32_t)(int16_t)value;
    cpu->cycle_count += 3;
}

static void op_ldhu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!aligned(addr,2)) {
        fprintf(stderr, "Error: Unaligned read at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (is_mmio_addr(cpu, addr)) {
        uint32_t word_addr = addr & ~0x3u;
        uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
        int shift = (addr & 0x3u) * 8;
        uint16_t value = (word >> shift) & 0xFFFFu;
        cpu->regs[inst->rd] = value;
        cpu->cycle_count += 3;
        return;
    }
    uint16_t value;
    if (!mm_load_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = value;
    cpu->cycle_count += 3;
}

static void op_ldb(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (is_mmio_addr(cpu, addr)) {
        uint32_t word_addr = addr & ~0x3u;
        uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
        int shift = (addr & 0x3u) * 8;
        uint8_t value = (word >> shift) & 0xFFu;
        cpu->regs[inst->rd] = (int32_t)(int8_t)value;
        cpu->cycle_count += 3;
        return;
    }
    uint8_t value;
    if (!mm_load_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = (int32_t)(int8_t)value;
    cpu->cycle_count += 3;
}

static void op_ldbu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (is_mmio_addr(cpu, addr)) {
        uint32_t word_addr = addr & ~0x3u;
        uint32_t word = mmio_ring_read(cpu->mmio.state, word_addr, 4);
        int shift = (addr & 0x3u) * 8;
        uint8_t value = (word >> shift) & 0xFFu;
        cpu->regs[inst->rd] = value;
        cpu->cycle_count += 3;
        return;
    }
    uint8_t value;
    if (!mm_load_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Read out of bounds at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->regs[inst->rd] = value;
    cpu->cycle_count += 3;
}

static void op_stw(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!aligned(addr,4)) {
        fprintf(stderr, "Error: Unaligned write at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    
    // Check for MMIO access
    if (is_mmio_addr(cpu, addr)) {
        mmio_write_fast(cpu, addr, cpu->regs[inst->rs2], 4);
        // MMIO processing happens only on YIELD
        cpu->cycle_count += 3;
        return;
    }

    uint32_t value = cpu->regs[inst->rs2];
    if (!mm_store_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Write out of bounds or to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->cycle_count += 3;
}

static void op_sth(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    if (!aligned(addr,2)) {
        fprintf(stderr, "Error: Unaligned write at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    if (is_mmio_addr(cpu, addr)) {
        mmio_write_fast(cpu, addr, cpu->regs[inst->rs2] & 0xFFFFu, 2);
        cpu->cycle_count += 3;
        return;
    }
    uint16_t value = cpu->regs[inst->rs2] & 0xFFFF;
    if (!mm_store_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Write out of bounds or to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->cycle_count += 3;
}

static void op_stb(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint32_t addr = cpu->regs[inst->rs1] + inst->imm;
    
    // Check for MMIO access
    if (is_mmio_addr(cpu, addr)) {
        mmio_write_fast(cpu, addr, cpu->regs[inst->rs2] & 0xFF, 1);
        cpu->cycle_count += 3;
        return;
    }
    
    uint8_t value = cpu->regs[inst->rs2] & 0xFF;
    if (!mm_store_bytes_fast(cpu, addr, &value, sizeof(value))) {
        fprintf(stderr, "Error: Write out of bounds or to protected memory at 0x%08x\n", addr);
        cpu->halted = true;
        return;
    }
    cpu->cycle_count += 3;
}

static inline uint32_t branch_target(fast_cpu_state_t *cpu, decoded_inst_t *inst) {
    return cpu->pc + 4 + inst->imm;
}

static void op_beq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] == cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_bne(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_blt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if ((int32_t)cpu->regs[inst->rs1] < (int32_t)cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_bge(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if ((int32_t)cpu->regs[inst->rs1] >= (int32_t)cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_bltu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] < cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_bgeu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    if (cpu->regs[inst->rs1] >= cpu->regs[inst->rs2]) {
        *next_pc = branch_target(cpu, inst);
    }
}

static void op_jal(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    cpu->regs[inst->rd] = cpu->pc + 4;
    *next_pc = cpu->pc + inst->imm;
}

static void op_jalr(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    uint32_t target = (cpu->regs[inst->rs1] + inst->imm) & ~1;
    cpu->regs[inst->rd] = cpu->pc + 4;
    *next_pc = target;
}

static void op_halt(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    UNUSED(inst);
    // Process any final MMIO before halting
    if (cpu->mmio.initialized && cpu->mmio.state) {
        mmio_process_fast(cpu);
    }
    cpu->halted = true;
}

static void op_assert_eq(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    if (cpu->regs[inst->rs1] != cpu->regs[inst->rs2]) {
        fprintf(stderr, "ASSERT r%d==r%d failed @PC=%08x\n", 
                inst->rs1, inst->rs2, cpu->pc);
        cpu->halted = true;
        cpu->regs[1] = 0xDEADBEEF;
    }
}

static void op_debug(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    putchar(cpu->regs[inst->rs1] & 0xFF);
    fflush(stdout);
}

static void op_yield(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    UNUSED(inst);
    // Process MMIO if enabled and initialized
    if (cpu->mmio.initialized && cpu->mmio.state) {
        mmio_process_fast(cpu);
    }
}

static void op_nop(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    UNUSED(inst);
    UNUSED(cpu);
    // Do nothing
}

// ============================================================
// f32 (single-precision float) handlers
// ============================================================

#define F32_ARITH_OP(name, op) \
static void op_##name(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) { \
    UNUSED(next_pc); \
    float a, b, r; \
    memcpy(&a, &cpu->regs[inst->rs1], 4); \
    memcpy(&b, &cpu->regs[inst->rs2], 4); \
    r = a op b; \
    memcpy(&cpu->regs[inst->rd], &r, 4); \
}

F32_ARITH_OP(fadd_s, +)
F32_ARITH_OP(fsub_s, -)
F32_ARITH_OP(fmul_s, *)
F32_ARITH_OP(fdiv_s, /)
#undef F32_ARITH_OP

static void op_fsqrt_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a, r;
    memcpy(&a, &cpu->regs[inst->rs1], 4);
    r = sqrtf(a);
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

#define F32_CMP_OP(name, op) \
static void op_##name(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) { \
    UNUSED(next_pc); \
    float a, b; \
    memcpy(&a, &cpu->regs[inst->rs1], 4); \
    memcpy(&b, &cpu->regs[inst->rs2], 4); \
    cpu->regs[inst->rd] = (a op b) ? 1 : 0; \
}

F32_CMP_OP(feq_s, ==)
F32_CMP_OP(flt_s, <)
F32_CMP_OP(fle_s, <=)
#undef F32_CMP_OP

static void op_fcvt_w_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a; memcpy(&a, &cpu->regs[inst->rs1], 4);
    cpu->regs[inst->rd] = (uint32_t)(int32_t)a;
}

static void op_fcvt_wu_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a; memcpy(&a, &cpu->regs[inst->rs1], 4);
    cpu->regs[inst->rd] = (uint32_t)a;
}

static void op_fcvt_s_w(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float r = (float)(int32_t)cpu->regs[inst->rs1];
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

static void op_fcvt_s_wu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float r = (float)cpu->regs[inst->rs1];
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

static void op_fneg_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] ^ 0x80000000u;
}

static void op_fabs_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1] & 0x7FFFFFFFu;
}

// ============================================================
// f64 (double-precision float) handlers -- register pairs
// ============================================================

static inline void load_f64(fast_cpu_state_t *cpu, uint8_t reg, double *out) {
#if S32_CHECK_F64_REGS
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31) at PC=0x%08X\n",
                reg, cpu->pc);
        cpu->halted = true;
        return;
    }
#endif
    uint64_t bits = ((uint64_t)cpu->regs[reg + 1] << 32) | cpu->regs[reg];
    memcpy(out, &bits, 8);
}

static inline void store_f64(fast_cpu_state_t *cpu, uint8_t reg, double val) {
#if S32_CHECK_F64_REGS
    if (reg >= 31 || (reg & 1)) {
        fprintf(stderr, "f64 register fault: r%d is invalid (must be even, < 31) at PC=0x%08X\n",
                reg, cpu->pc);
        cpu->halted = true;
        return;
    }
#endif
    uint64_t bits;
    memcpy(&bits, &val, 8);
    cpu->regs[reg] = (uint32_t)bits;
    cpu->regs[reg + 1] = (uint32_t)(bits >> 32);
}

#define F64_ARITH_OP(name, op) \
static void op_##name(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) { \
    UNUSED(next_pc); \
    double a, b, r; \
    load_f64(cpu, inst->rs1, &a); load_f64(cpu, inst->rs2, &b); \
    r = a op b; \
    store_f64(cpu, inst->rd, r); \
}

F64_ARITH_OP(fadd_d, +)
F64_ARITH_OP(fsub_d, -)
F64_ARITH_OP(fmul_d, *)
F64_ARITH_OP(fdiv_d, /)
#undef F64_ARITH_OP

static void op_fsqrt_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a, r;
    load_f64(cpu, inst->rs1, &a);
    r = sqrt(a);
    store_f64(cpu, inst->rd, r);
}

#define F64_CMP_OP(name, op) \
static void op_##name(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) { \
    UNUSED(next_pc); \
    double a, b; \
    load_f64(cpu, inst->rs1, &a); load_f64(cpu, inst->rs2, &b); \
    cpu->regs[inst->rd] = (a op b) ? 1 : 0; \
}

F64_CMP_OP(feq_d, ==)
F64_CMP_OP(flt_d, <)
F64_CMP_OP(fle_d, <=)
#undef F64_CMP_OP

static void op_fcvt_w_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a; load_f64(cpu, inst->rs1, &a);
    cpu->regs[inst->rd] = (uint32_t)(int32_t)a;
}

static void op_fcvt_wu_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a; load_f64(cpu, inst->rs1, &a);
    cpu->regs[inst->rd] = (uint32_t)a;
}

static void op_fcvt_d_w(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    store_f64(cpu, inst->rd, (double)(int32_t)cpu->regs[inst->rs1]);
}

static void op_fcvt_d_wu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    store_f64(cpu, inst->rd, (double)cpu->regs[inst->rs1]);
}

static void op_fcvt_d_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a; memcpy(&a, &cpu->regs[inst->rs1], 4);
    store_f64(cpu, inst->rd, (double)a);
}

static void op_fcvt_s_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a; load_f64(cpu, inst->rs1, &a);
    float r = (float)a;
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

static void op_fneg_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1];
    cpu->regs[inst->rd + 1] = cpu->regs[inst->rs1 + 1] ^ 0x80000000u;
}

static void op_fabs_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    cpu->regs[inst->rd] = cpu->regs[inst->rs1];
    cpu->regs[inst->rd + 1] = cpu->regs[inst->rs1 + 1] & 0x7FFFFFFFu;
}

// float <-> int64 conversion handlers
static void op_fcvt_l_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a; memcpy(&a, &cpu->regs[inst->rs1], 4);
    int64_t r = (int64_t)a;
    cpu->regs[inst->rd] = (uint32_t)r;
    cpu->regs[inst->rd + 1] = (uint32_t)((uint64_t)r >> 32);
}

static void op_fcvt_lu_s(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    float a; memcpy(&a, &cpu->regs[inst->rs1], 4);
    uint64_t r = (uint64_t)a;
    cpu->regs[inst->rd] = (uint32_t)r;
    cpu->regs[inst->rd + 1] = (uint32_t)(r >> 32);
}

static void op_fcvt_s_l(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    int64_t val = (int64_t)(((uint64_t)cpu->regs[inst->rs1 + 1] << 32) | cpu->regs[inst->rs1]);
    float r = (float)val;
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

static void op_fcvt_s_lu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint64_t val = ((uint64_t)cpu->regs[inst->rs1 + 1] << 32) | cpu->regs[inst->rs1];
    float r = (float)val;
    memcpy(&cpu->regs[inst->rd], &r, 4);
}

static void op_fcvt_l_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a; load_f64(cpu, inst->rs1, &a);
    int64_t r = (int64_t)a;
    cpu->regs[inst->rd] = (uint32_t)r;
    cpu->regs[inst->rd + 1] = (uint32_t)((uint64_t)r >> 32);
}

static void op_fcvt_lu_d(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    double a; load_f64(cpu, inst->rs1, &a);
    uint64_t r = (uint64_t)a;
    cpu->regs[inst->rd] = (uint32_t)r;
    cpu->regs[inst->rd + 1] = (uint32_t)(r >> 32);
}

static void op_fcvt_d_l(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    int64_t val = (int64_t)(((uint64_t)cpu->regs[inst->rs1 + 1] << 32) | cpu->regs[inst->rs1]);
    store_f64(cpu, inst->rd, (double)val);
}

static void op_fcvt_d_lu(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    uint64_t val = ((uint64_t)cpu->regs[inst->rs1 + 1] << 32) | cpu->regs[inst->rs1];
    store_f64(cpu, inst->rd, (double)val);
}

static void op_invalid(fast_cpu_state_t *cpu, decoded_inst_t *inst, uint32_t *next_pc) {
    UNUSED(next_pc);
    UNUSED(inst);
    fprintf(stderr, "Invalid instruction at PC 0x%08x\n", cpu->pc);
    cpu->halted = true;
}

// Get handler for opcode
static handler_fn get_handler(uint8_t opcode) {
    switch (opcode) {
        case OP_ADD: return op_add;
        case OP_SUB: return op_sub;
        case OP_XOR: return op_xor;
        case OP_OR: return op_or;
        case OP_AND: return op_and;
        case OP_SLL: return op_sll;
        case OP_SRL: return op_srl;
        case OP_SRA: return op_sra;
        case OP_SLT: return op_slt;
        case OP_SLTU: return op_sltu;
        case OP_MUL: return op_mul;
        case OP_MULH: return op_mulh;
        case OP_MULHU: return op_mulhu;
        case OP_DIV: return op_div;
        case OP_REM: return op_rem;
        case OP_SEQ: return op_seq;
        case OP_SNE: return op_sne;
        case OP_SGT: return op_sgt;
        case OP_SGTU: return op_sgtu;
        case OP_SLE: return op_sle;
        case OP_SLEU: return op_sleu;
        case OP_SGE: return op_sge;
        case OP_SGEU: return op_sgeu;
        case OP_ADDI: return op_addi;
        case OP_ORI: return op_ori;
        case OP_ANDI: return op_andi;
        case OP_XORI: return op_xori;
        case OP_SLLI: return op_slli;
        case OP_SRLI: return op_srli;
        case OP_SRAI: return op_srai;
        case OP_SLTI: return op_slti;
        case OP_SLTIU: return op_sltiu;
        case OP_LUI: return op_lui;
        case OP_LDW: return op_ldw;
        case OP_LDH: return op_ldh;
        case OP_LDHU: return op_ldhu;
        case OP_LDB: return op_ldb;
        case OP_LDBU: return op_ldbu;
        case OP_STW: return op_stw;
        case OP_STH: return op_sth;
        case OP_STB: return op_stb;
        case OP_BEQ: return op_beq;
        case OP_BNE: return op_bne;
        case OP_BLT: return op_blt;
        case OP_BGE: return op_bge;
        case OP_BLTU: return op_bltu;
        case OP_BGEU: return op_bgeu;
        case OP_JAL: return op_jal;
        case OP_JALR: return op_jalr;
        case OP_NOP: return op_nop;
        case OP_YIELD: return op_yield;
        case OP_DEBUG: return op_debug;
        case OP_ASSERT_EQ: return op_assert_eq;
        case OP_HALT: return op_halt;
        // f32 instructions
        case OP_FADD_S: return op_fadd_s;
        case OP_FSUB_S: return op_fsub_s;
        case OP_FMUL_S: return op_fmul_s;
        case OP_FDIV_S: return op_fdiv_s;
        case OP_FSQRT_S: return op_fsqrt_s;
        case OP_FEQ_S: return op_feq_s;
        case OP_FLT_S: return op_flt_s;
        case OP_FLE_S: return op_fle_s;
        case OP_FCVT_W_S: return op_fcvt_w_s;
        case OP_FCVT_WU_S: return op_fcvt_wu_s;
        case OP_FCVT_S_W: return op_fcvt_s_w;
        case OP_FCVT_S_WU: return op_fcvt_s_wu;
        case OP_FNEG_S: return op_fneg_s;
        case OP_FABS_S: return op_fabs_s;
        // f64 instructions
        case OP_FADD_D: return op_fadd_d;
        case OP_FSUB_D: return op_fsub_d;
        case OP_FMUL_D: return op_fmul_d;
        case OP_FDIV_D: return op_fdiv_d;
        case OP_FSQRT_D: return op_fsqrt_d;
        case OP_FEQ_D: return op_feq_d;
        case OP_FLT_D: return op_flt_d;
        case OP_FLE_D: return op_fle_d;
        case OP_FCVT_W_D: return op_fcvt_w_d;
        case OP_FCVT_WU_D: return op_fcvt_wu_d;
        case OP_FCVT_D_W: return op_fcvt_d_w;
        case OP_FCVT_D_WU: return op_fcvt_d_wu;
        case OP_FCVT_D_S: return op_fcvt_d_s;
        case OP_FCVT_S_D: return op_fcvt_s_d;
        case OP_FNEG_D: return op_fneg_d;
        case OP_FABS_D: return op_fabs_d;
        // float <-> int64 conversions
        case OP_FCVT_L_S: return op_fcvt_l_s;
        case OP_FCVT_LU_S: return op_fcvt_lu_s;
        case OP_FCVT_S_L: return op_fcvt_s_l;
        case OP_FCVT_S_LU: return op_fcvt_s_lu;
        case OP_FCVT_L_D: return op_fcvt_l_d;
        case OP_FCVT_LU_D: return op_fcvt_lu_d;
        case OP_FCVT_D_L: return op_fcvt_d_l;
        case OP_FCVT_D_LU: return op_fcvt_d_lu;
        default: return op_invalid;
    }
}

// Pre-decode all instructions using CORRECT encoding
static void predecode_program(fast_cpu_state_t *cpu, uint32_t code_size) {
    cpu->code_words = code_size / 4;
    cpu->decoded_code = calloc(cpu->code_words, sizeof(decoded_inst_t));
    
    for (uint32_t i = 0; i < cpu->code_words; i++) {
        uint32_t pc = i * 4;
        uint32_t raw;
        if (mm_read(&cpu->mm, pc, &raw, 4) != 0) {
            fprintf(stderr, "Error: Failed to read instruction at 0x%08x\n", pc);
            cpu->halted = true;
            return;
        }
        decoded_inst_t *di = &cpu->decoded_code[i];
        
        // Extract opcode from lower 7 bits (RISC-V style)
        uint8_t opcode = raw & 0x7F;
        di->handler = get_handler(opcode);
        
        // Decode based on opcode ranges (matching slow32.c)
        switch (opcode) {
            case OP_ADD ... OP_SNE:
            case OP_SGT ... OP_SGEU:
            case OP_MULHU:
            case OP_FADD_S ... OP_FCVT_D_LU:
                // R-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = 0;
                break;
                
            case OP_ORI ... OP_ANDI:
            case OP_XORI:
            case OP_SLTIU:
                // I-format with zero-extended immediates
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = 0;
                di->imm = (raw >> 20) & 0xFFF;  // Zero-extend 12-bit immediate
                break;
                
            case OP_ADDI:
            case OP_SLLI ... OP_SLTI:
            case OP_LDB ... OP_LDHU:
            case OP_JALR:
                // I-format with sign-extended immediates
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = 0;
                di->imm = ((int32_t)raw) >> 20;  // Sign-extend
                break;
                
            case OP_STB ... OP_STW:
                // S-format
                di->rd = 0;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = ((raw >> 7) & 0x1F) | (((int32_t)raw >> 25) << 5);
                break;
                
            case OP_BEQ ... OP_BGEU:
                // B-format
                di->rd = 0;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = (((raw >> 8) & 0xF) << 1) | (((raw >> 25) & 0x3F) << 5) |
                         (((raw >> 7) & 0x1) << 11) | (((int32_t)raw >> 31) << 12);
                break;
                
            case OP_LUI:
                // U-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = 0;
                di->rs2 = 0;
                di->imm = raw & 0xFFFFF000;
                break;
                
            case OP_JAL:
                // J-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = 0;
                di->rs2 = 0;
                {
                    uint32_t imm20 = (raw >> 31) & 0x1;
                    uint32_t imm10_1 = (raw >> 21) & 0x3FF;
                    uint32_t imm11 = (raw >> 20) & 0x1;
                    uint32_t imm19_12 = (raw >> 12) & 0xFF;
                    di->imm = (imm20 << 20) | (imm19_12 << 12) | (imm11 << 11) | (imm10_1 << 1);
                    if (di->imm & 0x100000) di->imm |= 0xFFE00000;  // Sign extend
                }
                break;
                
            case OP_NOP:
            case OP_YIELD:
            case OP_DEBUG:
            case OP_ASSERT_EQ:
            case OP_HALT:
                // Special R-format
                di->rd = (raw >> 7) & 0x1F;
                di->rs1 = (raw >> 15) & 0x1F;
                di->rs2 = (raw >> 20) & 0x1F;
                di->imm = 0;
                break;
                
            default:
                // Unknown
                di->rd = 0;
                di->rs1 = 0;
                di->rs2 = 0;
                di->imm = 0;
                break;
        }
    }
}

// Fast execution loop
static inline void cpu_step_fast(fast_cpu_state_t *cpu) {
    // Single boundary check protects against wild jumps and executing data
    // This is the only check needed in the hot path
    if ((cpu->pc >> 2) >= cpu->code_words) {
        fprintf(stderr, "Execute fault: PC=0x%08x outside code segment [0, 0x%08x)\n", 
                cpu->pc, cpu->code_words << 2);
        cpu->halted = true;
        return;
    }
    
    decoded_inst_t *di = &cpu->decoded_code[cpu->pc >> 2];
    uint32_t next_pc = cpu->pc + 4;
    
    // Direct function call - no switch!
    di->handler(cpu, di, &next_pc);
    
    cpu->pc = next_pc;
    cpu->regs[0] = 0;  // r0 always zero
    cpu->cycle_count++;
    cpu->inst_count++;
}

// ============================================================
// Symbol table building and lookup
// ============================================================

static int func_sym_cmp(const void *a, const void *b) {
    uint32_t aa = ((const func_sym_t *)a)->addr;
    uint32_t bb = ((const func_sym_t *)b)->addr;
    return (aa > bb) - (aa < bb);
}

static void build_sorted_symtab(sorted_symtab_t *st, const char *filename) {
    memset(st, 0, sizeof(*st));
    st->raw = load_s32x_symtab(filename);
    if (!st->raw.success || st->raw.num_symbols == 0) return;

    // Count usable symbols: FUNC type, or NOTYPE with a non-zero address
    // (many linkers emit all globals as NOTYPE)
    uint32_t nfunc = 0;
    for (uint32_t i = 0; i < st->raw.num_symbols; i++) {
        uint8_t t = st->raw.symbols[i].type;
        if (t == S32O_SYM_FUNC ||
            (t == S32O_SYM_NOTYPE && st->raw.symbols[i].value != 0 &&
             st->raw.symbols[i].name && st->raw.symbols[i].name[0] != '\0'))
            nfunc++;
    }
    if (nfunc == 0) return;

    st->syms = malloc(nfunc * sizeof(func_sym_t));
    if (!st->syms) return;

    uint32_t j = 0;
    for (uint32_t i = 0; i < st->raw.num_symbols; i++) {
        uint8_t t = st->raw.symbols[i].type;
        if (t == S32O_SYM_FUNC ||
            (t == S32O_SYM_NOTYPE && st->raw.symbols[i].value != 0 &&
             st->raw.symbols[i].name && st->raw.symbols[i].name[0] != '\0')) {
            st->syms[j].addr = st->raw.symbols[i].value;
            st->syms[j].name = st->raw.symbols[i].name;
            j++;
        }
    }
    st->count = j;
    qsort(st->syms, st->count, sizeof(func_sym_t), func_sym_cmp);
}

// Binary search: find last symbol with addr <= pc
static const char *resolve_symbol(sorted_symtab_t *st, uint32_t pc, uint32_t *offset) {
    if (!st->syms || st->count == 0) {
        *offset = pc;
        return NULL;
    }
    uint32_t lo = 0, hi = st->count;
    while (lo < hi) {
        uint32_t mid = lo + (hi - lo) / 2;
        if (st->syms[mid].addr <= pc) lo = mid + 1;
        else hi = mid;
    }
    if (lo == 0) {
        *offset = pc;
        return NULL;
    }
    lo--;
    *offset = pc - st->syms[lo].addr;
    return st->syms[lo].name;
}

static void free_sorted_symtab(sorted_symtab_t *st) {
    free(st->syms);
    st->syms = NULL;
    s32x_symtab_free(&st->raw);
    st->count = 0;
}

// ============================================================
// Register dump
// ============================================================

static void dump_registers(FILE *out, fast_cpu_state_t *cpu, sorted_symtab_t *st) {
    static const char *rnames[32] = {
        "zero", "rv1 ", "rv2 ", "a0  ", "a1  ", "a2  ", "a3  ", "a4  ",
        "a5  ", "a6  ", "a7  ", "r11 ", "r12 ", "r13 ", "r14 ", "r15 ",
        "r16 ", "r17 ", "r18 ", "r19 ", "r20 ", "r21 ", "r22 ", "r23 ",
        "r24 ", "r25 ", "r26 ", "r27 ", "r28 ", "sp  ", "fp  ", "lr  "
    };

    fprintf(out, "--- Register Dump ---\n");
    uint32_t off;
    const char *sym = resolve_symbol(st, cpu->pc, &off);
    if (sym) {
        fprintf(out, "PC = 0x%08X  <%s+0x%X>\n", cpu->pc, sym, off);
    } else {
        fprintf(out, "PC = 0x%08X\n", cpu->pc);
    }

    for (int i = 0; i < 32; i += 4) {
        fprintf(out, "  r%-2d/%-4s = 0x%08X  r%-2d/%-4s = 0x%08X  "
                     "r%-2d/%-4s = 0x%08X  r%-2d/%-4s = 0x%08X\n",
                i,   rnames[i],   cpu->regs[i],
                i+1, rnames[i+1], cpu->regs[i+1],
                i+2, rnames[i+2], cpu->regs[i+2],
                i+3, rnames[i+3], cpu->regs[i+3]);
    }
    fprintf(out, "Instructions: %" PRIu64 "  Cycles: %" PRIu64 "\n", cpu->inst_count, cpu->cycle_count);
    fprintf(out, "---------------------\n");
}

// ============================================================
// Signal handlers (minimal, async-signal-safe)
// ============================================================

static void sigint_handler(int sig) {
    UNUSED(sig);
    if (g_probe) g_probe->sigint_flag = 1;
    if (g_cpu) g_cpu->halted = true;
}

static void sigalrm_handler(int sig) {
    UNUSED(sig);
    if (g_probe && g_cpu) {
        g_probe->flag = 1;
    }
}

static void install_sigint_handler(void) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigint_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;  // No SA_RESTART — we want to interrupt blocking calls
    sigaction(SIGINT, &sa, NULL);
}

static void start_probe_timer(int interval_sec) {
    struct sigaction sa;
    memset(&sa, 0, sizeof(sa));
    sa.sa_handler = sigalrm_handler;
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

static void stop_probe_timer(void) {
    struct itimerval itv = {{0, 0}, {0, 0}};
    setitimer(ITIMER_REAL, &itv, NULL);
    signal(SIGALRM, SIG_DFL);
}

// ============================================================
// Probe processing (called from main loop, not signal handler)
// ============================================================

static void process_probe(probe_state_t *probe, fast_cpu_state_t *cpu, sorted_symtab_t *st) {
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

    // Calculate delta MIPS
    uint64_t delta_inst = cpu->inst_count - probe->last_inst_count;
    double delta_mips = (double)delta_inst / ((double)probe->probe_interval_sec * 1e6);
    probe->last_inst_count = cpu->inst_count;

    // Resolve symbol
    uint32_t off;
    const char *sym = resolve_symbol(st, pc, &off);

    // Loop detection: check if all ring entries are in same 4KB-aligned region
    bool looping = false;
    if (probe->ring_count >= PROBE_RING_SIZE) {
        uint32_t base = probe->pc_ring[0] & ~0xFFFu;
        looping = true;
        for (int i = 1; i < PROBE_RING_SIZE; i++) {
            if ((probe->pc_ring[i] & ~0xFFFu) != base) {
                looping = false;
                break;
            }
        }
    }

    // Full dump every full_dump_interval probes (default 10)
    bool do_full_dump = (probe->probe_count % probe->full_dump_interval) == 0;

    // Print probe line
    if (sym) {
        fprintf(stderr, "[%7.1fs] PC=0x%08X <%s+0x%X>  %" PRIu64 " inst  %.1f MIPS%s\n",
                elapsed, pc, sym, off, cpu->inst_count, delta_mips,
                looping ? " (looping)" : "");
    } else {
        fprintf(stderr, "[%7.1fs] PC=0x%08X  %" PRIu64 " inst  %.1f MIPS%s\n",
                elapsed, pc, cpu->inst_count, delta_mips,
                looping ? " (looping)" : "");
    }

    if (do_full_dump) {
        dump_registers(stderr, cpu, st);
    }
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

static void print_usage(const char *progname) {
    fprintf(stderr, "Usage: %s [options] <binary> [-- <args...>]\n", progname);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -h, --help       Show this help message\n");
    fprintf(stderr, "  -p, --probe [N]  Periodic probe: sample PC every N seconds (default 1)\n");
    fprintf(stderr, "  --allow <list>   Only allow these services (comma-separated)\n");
    fprintf(stderr, "  --deny <list>    Deny these services (comma-separated)\n");
    fprintf(stderr, "\n");
    fprintf(stderr, "Ctrl+C always dumps full register state before exiting.\n");
}

int main(int argc, char **argv) {
    /* Endianness check: SLOW-32 is Little-Endian and this emulator 
     * relies on host endianness for memory access performance. */
    {
        uint32_t test = 1;
        if (*(uint8_t *)&test != 1) {
            fprintf(stderr, "Error: This emulator only supports Little-Endian host platforms.\n");
            return 1;
        }
    }

    if (argc < 2) {
        print_usage(argv[0]);
        return 1;
    }

    // Probe state (stack-allocated, pointed to by g_probe when enabled)
    probe_state_t probe = {0};
    probe.probe_interval_sec = 1;
    probe.full_dump_interval = 10;

    // Pre-scan for --help/--allow/--deny/-p/--probe
    svc_policy_t policy = { .default_allow = true };
    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
            print_usage(argv[0]);
            return 0;
        }
        if (strcmp(argv[i], "-p") == 0 || strcmp(argv[i], "--probe") == 0) {
            probe.enabled = true;
            // Check if next arg is a number (optional interval)
            int consume = 1;
            if (i + 1 < argc && argv[i + 1][0] >= '0' && argv[i + 1][0] <= '9') {
                probe.probe_interval_sec = atoi(argv[i + 1]);
                if (probe.probe_interval_sec < 1) probe.probe_interval_sec = 1;
                probe.full_dump_interval = 10 / probe.probe_interval_sec;
                if (probe.full_dump_interval < 1) probe.full_dump_interval = 1;
                consume = 2;
            }
            memmove(&argv[i], &argv[i + consume], (argc - i - consume) * sizeof(char *));
            argc -= consume;
            i--;
            continue;
        }
        if (i >= argc - 1) break;
        if (strcmp(argv[i], "--allow") == 0) {
            parse_service_list(argv[i + 1], policy.allow_list, &policy.allow_count, S32_MAX_SERVICES);
            memmove(&argv[i], &argv[i + 2], (argc - i - 2) * sizeof(char *));
            argc -= 2;
            i--;
        } else if (strcmp(argv[i], "--deny") == 0) {
            parse_service_list(argv[i + 1], policy.deny_list, &policy.deny_count, S32_MAX_SERVICES);
            memmove(&argv[i], &argv[i + 2], (argc - i - 2) * sizeof(char *));
            argc -= 2;
            i--;
        }
    }

    fast_cpu_state_t cpu = {0};
    mm_init(&cpu.mm, false);  // Initialize memory manager

    int guest_argc = argc - 1;
    char **guest_argv = &argv[1];

    // Read header to get memory layout before loading sections
    s32x_load_result_t hdr = load_s32x_header(argv[1]);
    if (!hdr.success) {
        fprintf(stderr, "Error: %s\n", hdr.error_msg);
        return 1;
    }

    // Check header flag for MMIO configuration
    if (hdr.has_mmio) {
        cpu.mmio.enabled = true;
        cpu.mmio.base = hdr.mmio_base;
    }

    // Set up memory regions based on header
    uint32_t mmio_size = hdr.has_mmio ? S32_MMIO_REGION_SIZE : 0u;
    if (mm_setup_from_s32x(&cpu.mm, hdr.code_limit, hdr.rodata_limit,
                          hdr.data_limit, hdr.stack_base, hdr.stack_end,
                          hdr.mmio_base, mmio_size) != 0) {
        fprintf(stderr, "Error: Failed to allocate memory regions\n");
        mm_destroy(&cpu.mm);
        return 1;
    }

    // Load sections directly into memory manager via write callback
    s32x_loader_config_t cfg = {
        .write_cb = mm_write_callback,
        .user_data = &cpu,
        .mem_size = hdr.mem_size,
        .verbose = 0
    };

    s32x_load_result_t lr = load_s32x_file(argv[1], &cfg);
    if (!lr.success) {
        fprintf(stderr, "%s\n", lr.error_msg);
        mm_destroy(&cpu.mm);
        return 1;
    }

    // Set W^X policy from loader result
    cpu.mm.wxorx_enabled = lr.has_wxorx;

    // Protect regions after loading
    if (mm_protect_regions(&cpu.mm, lr.code_limit, lr.rodata_limit) != 0) {
        fprintf(stderr, "Error: Failed to protect memory regions\n");
        mm_destroy(&cpu.mm);
        return 1;
    }

    cpu.pc = lr.entry_point;
    if (cpu.pc >= lr.code_limit) {
        fprintf(stderr, "Invalid entry point 0x%08x outside code segment [0, 0x%08x)\n",
                cpu.pc, lr.code_limit);
        mm_destroy(&cpu.mm);
        return 1;
    }
    cpu.regs[REG_SP] = lr.stack_base;
    cpu.regs[REG_FP] = lr.stack_base;
    cpu.code_words = lr.code_limit / 4;   // predecode [0, code_limit)
    cpu.wx_enabled = lr.has_wxorx;
    cpu.code_limit = lr.code_limit;
    cpu.rodata_limit = lr.rodata_limit;
    cpu.data_limit = lr.data_limit;

    if (cpu.mmio.enabled && !cpu.mmio.initialized) {
        // Final limits are in place; now the MMIO heap aligns with slow32
        cpu_init_mmio(&cpu);
    }

    // Apply service policy
    if (cpu.mmio.enabled && cpu.mmio.state && (policy.allow_count > 0 || policy.deny_count > 0)) {
        mmio_set_policy(cpu.mmio.state, &policy);
    }

    if (cpu.mmio.enabled && cpu.mmio.state) {
        if (mmio_ring_set_args(cpu.mmio.state, (uint32_t)guest_argc, guest_argv) != 0) {
            fprintf(stderr, "Error: unable to stage guest arguments (too many bytes?)\n");
            if (cpu.mmio.mem) {
                munmap(cpu.mmio.mem, S32_MMIO_WINDOW_SIZE);
                cpu.mmio.mem = NULL;
            }
            if (cpu.mmio.state) {
                mmio_ring_clear_args(cpu.mmio.state);
                free(cpu.mmio.state);
                cpu.mmio.state = NULL;
            }
            free(cpu.decoded_code);
            mm_destroy(&cpu.mm);
            return 1;
        }
        // Pass through host environment to guest
        extern char **environ;
        if (mmio_ring_set_envp(cpu.mmio.state, environ) != 0) {
            fprintf(stderr, "Warning: unable to stage host environment (too many bytes?)\n");
            // Non-fatal - continue without environment
        }
    } else if (guest_argc > 1) {
        fprintf(stderr, "Warning: guest arguments ignored because MMIO is disabled.\n");
    }

    predecode_program(&cpu, lr.code_limit);

    // Build sorted symbol table for PC → function name resolution
    build_sorted_symtab(&g_symtab, argv[1]);

    // Install SIGINT handler (always — Ctrl+C dumps registers)
    g_cpu = &cpu;
    g_probe = &probe;
    install_sigint_handler();

    // Start probe timer if requested
    if (probe.enabled) {
        fprintf(stderr, "Probe enabled: sampling every %d second(s)\n", probe.probe_interval_sec);
        clock_gettime(CLOCK_MONOTONIC, &probe.start_time);
        start_probe_timer(probe.probe_interval_sec);
    }

    // Time the execution
    printf("Starting execution at PC 0x%08x\n", cpu.pc);
    if (cpu.mmio.enabled) {
        printf("MMIO enabled for this program\n");
    }
    struct timespec start, end;
    clock_gettime(CLOCK_MONOTONIC, &start);

    uint64_t max_instructions = 10000000000;  // 10B instruction limit for debugging
    while (!cpu.halted && cpu.inst_count < max_instructions) {
        cpu_step_fast(&cpu);
        if (__builtin_expect(probe.flag, 0)) {
            process_probe(&probe, &cpu, &g_symtab);
        }
    }

    // Stop probe timer before any output
    if (probe.enabled) {
        stop_probe_timer();
    }

    // SIGINT register dump
    if (probe.sigint_flag) {
        fprintf(stderr, "\nInterrupted (SIGINT)\n");
        dump_registers(stderr, &cpu, &g_symtab);
    }

    if (cpu.inst_count >= max_instructions) {
        printf("WARNING: Execution limit reached (%" PRIu64 " instructions)\n", max_instructions);
        cpu.halted = true;
    }

    clock_gettime(CLOCK_MONOTONIC, &end);

    // Calculate elapsed time
    double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;

    printf("HALT at PC 0x%08x\n", cpu.pc);
    printf("\nProgram halted.\n");
    printf("Exit code: %d\n", cpu.regs[1]);
    printf("Instructions executed: %" PRIu64 "\n", cpu.inst_count);
    printf("Simulated cycles: %" PRIu64 "\n", cpu.cycle_count);
    printf("Wall time: %.6f seconds\n", elapsed);

    if (elapsed > 0) {
        double ips = cpu.inst_count / elapsed;
        printf("Performance: %.2f MIPS (actual)\n", ips / 1e6);
        printf("            %.2f instructions/second\n", ips);
    }

    // Cleanup
    g_cpu = NULL;
    g_probe = NULL;
    free_sorted_symtab(&g_symtab);
    if (cpu.mmio.state) {
        mmio_cleanup_services(cpu.mmio.state);
    }
    if (cpu.mmio.mem) {
        munmap(cpu.mmio.mem, S32_MMIO_WINDOW_SIZE);
    }
    if (cpu.mmio.state) {
        mmio_ring_clear_args(cpu.mmio.state);
        free(cpu.mmio.state);
    }
    free(cpu.decoded_code);
    mm_destroy(&cpu.mm);
    return cpu.regs[1];
}
