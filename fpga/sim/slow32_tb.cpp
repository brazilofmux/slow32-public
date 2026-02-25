// slow32_tb.cpp — Verilator testbench for SLOW-32 CPU
//
// Loads .s32x executables, provides flat memory model, captures DEBUG output.
// Usage: ./obj_dir/Vslow32_cpu <program.s32x> [--trace] [--max-cycles N]

#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdint>
#include <string>

#include "Vslow32_cpu.h"
#include "verilated.h"

// .s32x format constants
static constexpr uint32_t S32X_MAGIC = 0x53333258u;
static constexpr uint32_t SEC_CODE   = 0x0001;
static constexpr uint32_t SEC_DATA   = 0x0002;
static constexpr uint32_t SEC_RODATA = 0x0004;

// Memory
static uint8_t *mem = nullptr;
static uint32_t mem_size = 0;
static uint32_t entry_point = 0;
static uint32_t stack_base = 0;

static inline uint32_t rd32(const uint8_t *m, uint32_t a) {
    uint32_t v; memcpy(&v, m + a, 4); return v;
}

static bool load_s32x(const char *path) {
    FILE *f = fopen(path, "rb");
    if (!f) { perror(path); return false; }

    uint8_t hdr[64];
    if (fread(hdr, 1, 64, f) != 64) {
        fprintf(stderr, "%s: truncated header\n", path);
        fclose(f); return false;
    }

    if (rd32(hdr, 0x00) != S32X_MAGIC) {
        fprintf(stderr, "%s: bad magic\n", path);
        fclose(f); return false;
    }

    entry_point          = rd32(hdr, 0x08);
    uint32_t nsections   = rd32(hdr, 0x0C);
    uint32_t sec_offset  = rd32(hdr, 0x10);
    stack_base           = rd32(hdr, 0x2C);
    uint32_t raw_mem     = rd32(hdr, 0x30);

    uint64_t total64 = raw_mem;
    if ((uint64_t)stack_base > total64) total64 = stack_base;
    if (total64 == 0 || total64 > 0x10000000ULL) total64 = 0x10000000ULL;
    mem_size = (uint32_t)total64;

    mem = (uint8_t *)calloc(1, mem_size);
    if (!mem) { fprintf(stderr, "OOM\n"); fclose(f); return false; }

    fseek(f, sec_offset, SEEK_SET);
    for (uint32_t i = 0; i < nsections; i++) {
        uint8_t sec[28];
        if (fread(sec, 1, 28, f) != 28) break;
        uint32_t type    = rd32(sec, 0x04);
        uint32_t vaddr   = rd32(sec, 0x08);
        uint32_t foffset = rd32(sec, 0x0C);
        uint32_t size    = rd32(sec, 0x10);
        if (type != SEC_CODE && type != SEC_DATA && type != SEC_RODATA) continue;
        if (size == 0 || vaddr + size > mem_size) continue;
        long saved = ftell(f);
        fseek(f, foffset, SEEK_SET);
        fread(mem + vaddr, 1, size, f);
        fseek(f, saved, SEEK_SET);
    }

    fclose(f);
    return true;
}

static uint32_t mem_read_word(uint32_t addr) {
    addr &= ~3u;
    if (addr + 4 <= mem_size) return rd32(mem, addr);
    return 0;
}

static void mem_write(uint32_t addr, uint32_t data, uint8_t byte_en) {
    addr &= ~3u;
    if (addr + 4 > mem_size) return;
    if (byte_en & 1) mem[addr + 0] = (data >>  0) & 0xFF;
    if (byte_en & 2) mem[addr + 1] = (data >>  8) & 0xFF;
    if (byte_en & 4) mem[addr + 2] = (data >> 16) & 0xFF;
    if (byte_en & 8) mem[addr + 3] = (data >> 24) & 0xFF;
}

int main(int argc, char **argv) {
    Verilated::commandArgs(argc, argv);

    const char *s32x_path = nullptr;
    uint64_t max_cycles = 100000000ULL;
    bool trace = false;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "--trace") == 0) trace = true;
        else if (strcmp(argv[i], "--max-cycles") == 0 && i + 1 < argc)
            max_cycles = strtoull(argv[++i], nullptr, 0);
        else if (argv[i][0] != '-' && argv[i][0] != '+')
            s32x_path = argv[i];
    }

    if (!s32x_path) {
        fprintf(stderr, "Usage: %s <program.s32x> [--trace] [--max-cycles N]\n", argv[0]);
        return 1;
    }

    if (!load_s32x(s32x_path)) return 1;

    Vslow32_cpu *dut = new Vslow32_cpu;

    // Connect initialization inputs
    dut->init_pc = entry_point;
    dut->init_sp = stack_base;

    // Reset sequence
    dut->clk = 0;
    dut->rst_n = 0;
    dut->imem_rdata = 0;
    dut->dmem_rdata = 0;
    for (int i = 0; i < 4; i++) {
        dut->clk = 0; dut->eval();
        dut->clk = 1; dut->eval();
    }

    // Release reset
    dut->rst_n = 1;

    uint64_t cycle = 0;
    int exit_code = 0;

    while (cycle < max_cycles) {
        // Falling edge: drive memory inputs
        dut->clk = 0;

        // Instruction memory
        dut->imem_rdata = mem_read_word(dut->imem_addr);

        // Data memory read
        if (dut->dmem_ren)
            dut->dmem_rdata = mem_read_word(dut->dmem_addr);

        dut->eval();

        // Rising edge
        dut->clk = 1;
        dut->eval();

        // Data memory write (committed on rising edge)
        if (dut->dmem_wen)
            mem_write(dut->dmem_addr, dut->dmem_wdata, dut->dmem_byte_en);

        // Capture DEBUG output
        if (dut->debug_valid)
            putchar(dut->debug_char);

        // Check assert failure
        if (dut->assert_fail) {
            fprintf(stderr, "ASSERT_EQ failed: r%d(0x%08X) != r%d(0x%08X) at PC=0x%08X\n",
                    dut->assert_rs1, dut->assert_val1,
                    dut->assert_rs2, dut->assert_val2,
                    dut->assert_pc);
            exit_code = 1;
            break;
        }

        // Check halt
        if (dut->halted) {
            if (trace)
                fprintf(stderr, "HALT after %llu cycles\n", (unsigned long long)cycle);
            break;
        }

        cycle++;
    }

    if (cycle >= max_cycles) {
        fprintf(stderr, "Timeout after %llu cycles\n", (unsigned long long)max_cycles);
        exit_code = 124;
    }

    fflush(stdout);
    if (trace)
        fprintf(stderr, "Cycles: %llu\n", (unsigned long long)cycle);

    dut->final();
    delete dut;
    free(mem);
    return exit_code;
}
