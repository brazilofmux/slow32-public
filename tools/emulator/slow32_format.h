#ifndef SLOW32_FORMAT_H
#define SLOW32_FORMAT_H

#include <stdint.h>

// SLOW-32 v2 Binary Format
// Simple flat binary with header specifying code and data sections

#define SLOW32_MAGIC 0x32534C32  // "2SL2" in little-endian

typedef struct {
    uint32_t magic;       // Magic number: 0x32534C32 ("2SL2")
    uint32_t entry;       // Entry point (PC start address)
    uint32_t code_size;   // Size of code section in bytes
    uint32_t data_size;   // Size of data section in bytes
} slow32_header_t;

// After header:
// - code_size bytes of code (loaded at 0x00000000)
// - data_size bytes of data (loaded at 0x00100000)

#endif