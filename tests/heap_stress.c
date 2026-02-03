#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <string.h>

// Deterministic PRNG
static uint32_t rng_state = 0xDEADBEEF;
static uint32_t rng_next(void) {
    rng_state = rng_state * 1664525u + 1013904223u;
    return rng_state;
}

// Fill memory with a pattern derived from pointer and size
static void fill_pattern(void *p, size_t size, uint32_t tag) {
    uint8_t *b = (uint8_t *)p;
    for (size_t i = 0; i < size; i++) {
        b[i] = (uint8_t)(tag ^ i ^ (i >> 8));
    }
}

// Verify pattern; returns 0 on success
static int check_pattern(void *p, size_t size, uint32_t tag) {
    uint8_t *b = (uint8_t *)p;
    for (size_t i = 0; i < size; i++) {
        uint8_t expected = (uint8_t)(tag ^ i ^ (i >> 8));
        if (b[i] != expected) return 1;
    }
    return 0;
}

// ---- Test 1: Many rounds of alloc/free with integrity checks ----
// Allocate N slots, then randomly free and reallocate in R rounds.
// After each round, verify all live blocks are uncorrupted.
#define SLOTS 64
#define ROUNDS 20

static int test_churn(void) {
    void *ptrs[SLOTS];
    size_t sizes[SLOTS];
    uint32_t tags[SLOTS];

    memset(ptrs, 0, sizeof(ptrs));

    // Initial fill
    for (int i = 0; i < SLOTS; i++) {
        sizes[i] = (rng_next() % 200) + 1;
        tags[i] = rng_next();
        ptrs[i] = malloc(sizes[i]);
        if (!ptrs[i]) { printf("  OOM at initial slot %d size %u\n", i, (unsigned)sizes[i]); return 1; }
        fill_pattern(ptrs[i], sizes[i], tags[i]);
    }

    for (int r = 0; r < ROUNDS; r++) {
        // Free ~half the slots at random
        for (int i = 0; i < SLOTS; i++) {
            if (ptrs[i] && (rng_next() & 1)) {
                free(ptrs[i]);
                ptrs[i] = NULL;
            }
        }

        // Verify survivors
        for (int i = 0; i < SLOTS; i++) {
            if (ptrs[i]) {
                if (check_pattern(ptrs[i], sizes[i], tags[i])) {
                    printf("  Corruption round %d slot %d\n", r, i);
                    return 1;
                }
            }
        }

        // Refill empty slots with new sizes
        for (int i = 0; i < SLOTS; i++) {
            if (!ptrs[i]) {
                sizes[i] = (rng_next() % 300) + 1;
                tags[i] = rng_next();
                ptrs[i] = malloc(sizes[i]);
                if (!ptrs[i]) { printf("  OOM round %d slot %d size %u\n", r, i, (unsigned)sizes[i]); return 1; }
                fill_pattern(ptrs[i], sizes[i], tags[i]);
            }
        }
    }

    // Final verify and cleanup
    for (int i = 0; i < SLOTS; i++) {
        if (ptrs[i]) {
            if (check_pattern(ptrs[i], sizes[i], tags[i])) {
                printf("  Corruption final slot %d\n", i);
                return 1;
            }
            free(ptrs[i]);
        }
    }
    return 0;
}

// ---- Test 2: Realloc churn ----
// Grow and shrink blocks repeatedly, check data preservation.
#define REALLOC_SLOTS 32
#define REALLOC_ROUNDS 30

static int test_realloc_churn(void) {
    void *ptrs[REALLOC_SLOTS];
    size_t sizes[REALLOC_SLOTS];
    uint32_t tags[REALLOC_SLOTS];

    // Initial small allocations
    for (int i = 0; i < REALLOC_SLOTS; i++) {
        sizes[i] = (rng_next() % 16) + 1;
        tags[i] = rng_next();
        ptrs[i] = malloc(sizes[i]);
        if (!ptrs[i]) { printf("  OOM realloc init %d\n", i); return 1; }
        fill_pattern(ptrs[i], sizes[i], tags[i]);
    }

    for (int r = 0; r < REALLOC_ROUNDS; r++) {
        int idx = rng_next() % REALLOC_SLOTS;
        size_t new_size;

        if (rng_next() & 1) {
            // Grow
            new_size = sizes[idx] + (rng_next() % 128) + 1;
        } else {
            // Shrink (but not to zero)
            new_size = (rng_next() % sizes[idx]) + 1;
        }

        // Check data before realloc
        if (check_pattern(ptrs[idx], sizes[idx], tags[idx])) {
            printf("  Pre-realloc corruption round %d idx %d\n", r, idx);
            return 1;
        }

        void *p = realloc(ptrs[idx], new_size);
        if (!p) { printf("  OOM realloc round %d idx %d size %u\n", r, idx, (unsigned)new_size); return 1; }

        // Check preserved prefix
        size_t check_len = new_size < sizes[idx] ? new_size : sizes[idx];
        if (check_pattern(p, check_len, tags[idx])) {
            printf("  Realloc data loss round %d idx %d\n", r, idx);
            return 1;
        }

        // Re-tag with full new size
        ptrs[idx] = p;
        sizes[idx] = new_size;
        tags[idx] = rng_next();
        fill_pattern(ptrs[idx], sizes[idx], tags[idx]);
    }

    for (int i = 0; i < REALLOC_SLOTS; i++) free(ptrs[i]);
    return 0;
}

// ---- Test 3: Calloc zeroing after reuse ----
// Allocate, fill with 0xFF, free, then calloc same size and verify zeros.
static int test_calloc_reuse(void) {
    for (int sz = 1; sz <= 256; sz += 7) {
        void *p = malloc(sz);
        if (!p) { printf("  OOM calloc_reuse malloc %d\n", sz); return 1; }
        memset(p, 0xFF, sz);
        free(p);

        int *q = calloc(1, sz);
        if (!q) { printf("  OOM calloc_reuse calloc %d\n", sz); return 1; }
        uint8_t *b = (uint8_t *)q;
        for (int i = 0; i < sz; i++) {
            if (b[i] != 0) {
                printf("  calloc not zeroed at byte %d (size %d)\n", i, sz);
                free(q);
                return 1;
            }
        }
        free(q);
    }
    return 0;
}

// ---- Test 4: Free-order coalescing ----
// Allocate A B C D in sequence, free in various orders,
// then allocate a block that requires full coalescing.
static int test_coalesce_orders(void) {
    // Six orderings of freeing 3 blocks (middle already free)
    int orders[][3] = {
        {0, 1, 2}, {0, 2, 1}, {1, 0, 2},
        {1, 2, 0}, {2, 0, 1}, {2, 1, 0}
    };

    for (int t = 0; t < 6; t++) {
        void *a = malloc(100);
        void *b = malloc(100);
        void *c = malloc(100);
        if (!a || !b || !c) { printf("  OOM coalesce test %d\n", t); return 1; }

        memset(a, 'A', 100);
        memset(b, 'B', 100);
        memset(c, 'C', 100);

        void *blks[3] = {a, b, c};
        free(blks[orders[t][0]]);
        free(blks[orders[t][1]]);
        free(blks[orders[t][2]]);

        // Should be able to get a block covering all three
        void *big = malloc(250);
        if (!big) { printf("  Coalesce failed order %d%d%d\n", orders[t][0], orders[t][1], orders[t][2]); return 1; }
        memset(big, 'X', 250);
        free(big);
    }
    return 0;
}

// ---- Test 5: Alignment for all sizes 1..128 ----
static int test_alignment(void) {
    for (int sz = 1; sz <= 128; sz++) {
        void *p = malloc(sz);
        if (!p) { printf("  OOM align %d\n", sz); return 1; }
        if (((uintptr_t)p) % 8 != 0) {
            printf("  Misaligned: malloc(%d) -> %p\n", sz, p);
            free(p);
            return 1;
        }
        // Write to every byte to trigger any overlap issues
        memset(p, sz & 0xFF, sz);
        free(p);
    }
    return 0;
}

// ---- Test 6: Large block reuse after fragmentation ----
// Create a fragmented heap, free everything, verify large alloc works.
static int test_defrag(void) {
    void *ptrs[100];
    // Allocate 100 small blocks
    for (int i = 0; i < 100; i++) {
        ptrs[i] = malloc(24);
        if (!ptrs[i]) { printf("  OOM defrag alloc %d\n", i); return 1; }
        memset(ptrs[i], i, 24);
    }
    // Free all
    for (int i = 0; i < 100; i++) free(ptrs[i]);

    // Should coalesce into one big block
    void *big = malloc(2000);
    if (!big) { printf("  Defrag failed: can't alloc 2000 after freeing 100x24\n"); return 1; }
    memset(big, 0xAA, 2000);
    free(big);
    return 0;
}

// ---- Test 7: Large calloc of int64_t structs (regal scenario) ----
// Reproduces the pattern that crashed regal on SLOW-32:
// 1. Churn the heap with small allocations
// 2. calloc a large array of structs containing int64_t fields
// 3. Write/read int64_t values and verify integrity
typedef struct {
    int64_t debit;
    int64_t credit;
} txn_totals_t;

static int test_large_calloc_int64(void) {
    // Phase 1: Churn the heap to create fragmentation
    void *churn[64];
    size_t churn_sz[64];
    for (int i = 0; i < 64; i++) {
        churn_sz[i] = (rng_next() % 120) + 8;
        churn[i] = malloc(churn_sz[i]);
        if (!churn[i]) { printf("  OOM churn alloc %d\n", i); return 1; }
        memset(churn[i], i, churn_sz[i]);
    }
    // Free odd slots
    for (int i = 1; i < 64; i += 2) {
        free(churn[i]);
        churn[i] = NULL;
    }
    // Free even slots
    for (int i = 0; i < 64; i += 2) {
        free(churn[i]);
        churn[i] = NULL;
    }

    // Phase 2: Large calloc of int64_t-containing structs
    // This mirrors regal's calloc(g_num_transactions, sizeof(TxnTotals))
    // where TxnTotals has two int64_t fields.
    int count = 1024;
    txn_totals_t *arr = (txn_totals_t *)calloc(count, sizeof(txn_totals_t));
    if (!arr) { printf("  OOM calloc %d x %u\n", count, (unsigned)sizeof(txn_totals_t)); return 1; }

    // Phase 3: Verify calloc zeroed everything
    for (int i = 0; i < count; i++) {
        if (arr[i].debit != 0 || arr[i].credit != 0) {
            printf("  calloc not zeroed at index %d: debit=%lld credit=%lld\n",
                   i, (long long)arr[i].debit, (long long)arr[i].credit);
            free(arr);
            return 1;
        }
    }

    // Phase 4: Write int64_t values and verify
    for (int i = 0; i < count; i++) {
        arr[i].debit  = (int64_t)i * 100000LL + 123456789LL;
        arr[i].credit = (int64_t)i * 200000LL + 987654321LL;
    }
    for (int i = 0; i < count; i++) {
        int64_t expected_d = (int64_t)i * 100000LL + 123456789LL;
        int64_t expected_c = (int64_t)i * 200000LL + 987654321LL;
        if (arr[i].debit != expected_d || arr[i].credit != expected_c) {
            printf("  int64 corruption at index %d\n", i);
            free(arr);
            return 1;
        }
    }

    // Phase 5: Check alignment of the allocation
    if (((uintptr_t)arr) % 8 != 0) {
        printf("  calloc result misaligned: %p\n", (void *)arr);
        free(arr);
        return 1;
    }

    free(arr);
    return 0;
}

// ---- Test 8: Interleaved int64_t heap and stack access ----
// Allocate multiple small structs with int64_t, interleave access.
static int test_multi_int64_blocks(void) {
    int n = 16;
    txn_totals_t *blocks[16];

    for (int i = 0; i < n; i++) {
        blocks[i] = (txn_totals_t *)malloc(sizeof(txn_totals_t) * 4);
        if (!blocks[i]) { printf("  OOM multi_int64 %d\n", i); return 1; }
        for (int j = 0; j < 4; j++) {
            blocks[i][j].debit  = (int64_t)(i * 100 + j);
            blocks[i][j].credit = (int64_t)(i * 100 + j + 50);
        }
    }

    // Verify all blocks
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < 4; j++) {
            if (blocks[i][j].debit != (int64_t)(i * 100 + j) ||
                blocks[i][j].credit != (int64_t)(i * 100 + j + 50)) {
                printf("  multi_int64 corruption block %d elem %d\n", i, j);
                return 1;
            }
        }
    }

    // Free in reverse order, then reallocate and verify fresh
    for (int i = n - 1; i >= 0; i--) free(blocks[i]);

    txn_totals_t *big = (txn_totals_t *)calloc(n * 4, sizeof(txn_totals_t));
    if (!big) { printf("  OOM multi_int64 big calloc\n"); return 1; }
    for (int i = 0; i < n * 4; i++) {
        if (big[i].debit != 0 || big[i].credit != 0) {
            printf("  multi_int64 big calloc not zeroed at %d\n", i);
            free(big);
            return 1;
        }
    }
    free(big);
    return 0;
}

int main(void) {
    int fail = 0;

    printf("Heap stress test\n");

    printf("1. Alignment:       ");
    if (test_alignment()) { printf("FAIL\n"); fail++; } else printf("PASS\n");

    printf("2. Calloc reuse:    ");
    if (test_calloc_reuse()) { printf("FAIL\n"); fail++; } else printf("PASS\n");

    printf("3. Coalesce orders: ");
    if (test_coalesce_orders()) { printf("FAIL\n"); fail++; } else printf("PASS\n");

    printf("4. Defragmentation: ");
    if (test_defrag()) { printf("FAIL\n"); fail++; } else printf("PASS\n");

    printf("5. Alloc/free churn:");
    if (test_churn()) { printf("FAIL\n"); fail++; } else printf(" PASS\n");

    printf("6. Realloc churn:   ");
    if (test_realloc_churn()) { printf("FAIL\n"); fail++; } else printf("PASS\n");

    printf("7. Large calloc i64:");
    if (test_large_calloc_int64()) { printf("FAIL\n"); fail++; } else printf(" PASS\n");

    printf("8. Multi int64 blks:");
    if (test_multi_int64_blocks()) { printf("FAIL\n"); fail++; } else printf(" PASS\n");

    if (fail) {
        printf("\n%d test(s) FAILED\n", fail);
        return 1;
    }
    printf("\nAll heap tests passed\n");
    return 0;
}
