// Shared DBT limits

#ifndef DBT_LIMITS_H
#define DBT_LIMITS_H

// Superblocks can have multiple side exits; keep cache tracking in sync.
#define MAX_SUPERBLOCK_EXITS 8
#define MAX_BLOCK_EXITS MAX_SUPERBLOCK_EXITS

#endif  // DBT_LIMITS_H
