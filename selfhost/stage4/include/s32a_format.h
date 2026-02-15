#ifndef S32A_FORMAT_H
#define S32A_FORMAT_H

#include <stdint.h>

// ============================================================================
// SLOW-32 Archive Format (.s32a)
// ============================================================================
// 
// Archives contain multiple .s32o object files with a symbol index for
// efficient linking. Structure:
//   1. Archive header
//   2. Symbol index (for ranlib functionality)
//   3. Member entries
//   4. Member data (actual .s32o files)

#define S32A_MAGIC 0x53333241  // "S32A" in little-endian

// Archive header (32 bytes)
typedef struct {
    uint32_t magic;         // 0x00: Magic number 0x53333241 ("S32A")
    uint16_t version;       // 0x04: Format version (1)
    uint8_t  endian;        // 0x06: Endianness (0x01=little)
    uint8_t  reserved;      // 0x07: Reserved (must be 0)
    uint32_t nmembers;      // 0x08: Number of archive members
    uint32_t mem_offset;    // 0x0C: Offset to member table
    uint32_t nsymbols;      // 0x10: Number of symbols in index
    uint32_t sym_offset;    // 0x14: Offset to symbol index
    uint32_t str_offset;    // 0x18: Offset to string table
    uint32_t str_size;      // 0x1C: Size of string table
} s32a_header_t;

// Symbol index entry (8 bytes)
// Maps global symbols to the members that define them
typedef struct {
    uint32_t name_offset;   // Offset into string table
    uint32_t member_index;  // Which member defines this symbol
} s32a_symbol_t;

// Member entry (24 bytes)
typedef struct {
    uint32_t name_offset;   // Offset into string table
    uint32_t offset;        // File offset to member data (.s32o file)
    uint32_t size;          // Size of member data
    uint32_t timestamp;     // Modification time (Unix timestamp)
    uint32_t uid;           // User ID
    uint32_t gid;           // Group ID
} s32a_member_t;

#endif // S32A_FORMAT_H