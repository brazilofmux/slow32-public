/* host stress test for libcob/btree.h: random inserts, removals (by slot and exact), seeks and scans against a reference model, under a 16-page cache, with and without a payload; klen N ops on the command line.  The suite runs it as Gate 1b. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "btree.h"
static void fatal(const char *m) { fprintf(stderr, "FATAL: %s\n", m); exit(9); }
typedef struct { unsigned char ka[BT_KEYMAX + 4]; unsigned slot; } ref_e;
static ref_e *ref; static unsigned nref, KL;
static int ref_cmp(const void *a, const void *b) { return memcmp(((const ref_e *)a)->ka, ((const ref_e *)b)->ka, KL + 4); }
static unsigned rnd(void) { static unsigned s = 12345; s = s * 1103515245u + 12345u; return s >> 8; }
static void mkkey(unsigned char *k, unsigned v) { memset(k, 'a', KL); for (unsigned i = 0; i < 4 && i < KL; i++) k[KL - 1 - i] = (unsigned char)('0' + (v >> (3 * i)) % 8); }
static int check_scan(btf *b, const char *when) {
    unsigned page, ix; unsigned char zero[BT_KEYMAX + 4]; memset(zero, 0, sizeof zero);
    unsigned n = 0; unsigned char ka[BT_KEYMAX + 4];
    if (bt_first_ge(b, 0, zero, KL + 4, &page, &ix)) {
        do { unsigned slot = bt_read(b, 0, page, ix, ka);
            if (n >= nref || memcmp(ka, ref[n].ka, KL + 4) || slot != ref[n].slot) { printf("MISMATCH %s at entry %u (nref %u)\n", when, n, nref); return 0; }
            n++; } while (bt_step(b, &page, &ix));
    }
    if (n != nref) { printf("COUNT MISMATCH %s: scanned %u, ref %u\n", when, n, nref); return 0; }
    if (b->k[0].count != nref) { printf("HEADER COUNT MISMATCH %s: %u vs %u\n", when, b->k[0].count, nref); return 0; }
    /* backwards from the end */
    n = nref;
    if (bt_last(b, 0, &page, &ix)) { do { n--; bt_read(b, 0, page, ix, ka); if (memcmp(ka, ref[n].ka, KL + 4)) { printf("BACK MISMATCH %s at %u\n", when, n); return 0; } } while (bt_back(b, &page, &ix)); if (n) { printf("BACK COUNT %s: %u left\n", when, n); return 0; } }
    else if (nref) { printf("bt_last empty but nref=%u %s\n", nref, when); return 0; }
    return 1;
}
int main(int argc, char **argv) {
    KL = argc > 1 ? (unsigned)atoi(argv[1]) : 200; unsigned N = argc > 2 ? (unsigned)atoi(argv[2]) : 4000, ops = argc > 3 ? (unsigned)atoi(argv[3]) : 20000;
    btkey k = { 0, KL, 1, 0, 0, (unsigned)(getenv("EXTRA") ? atoi(getenv("EXTRA")) : 0) }; btf b;
    remove("t.key");
    if (!bt_create(&b, "t.key", 16, 100, 0, KL, &k, 1, fatal)) return 1;
    ref = calloc(N + ops, sizeof *ref);
    unsigned seq = 0, slots_live = 0;
    for (unsigned i = 0; i < N; i++) { unsigned char key[BT_KEYMAX]; mkkey(key, rnd() % (N / 2)); unsigned slot = bt_slot_alloc(&b); slots_live++;
        bt_insert(&b, 0, key, seq, slot); memcpy(ref[nref].ka, key, KL); bt_putbe(ref[nref].ka + KL, seq); ref[nref].slot = slot; nref++; seq++; }
    qsort(ref, nref, sizeof *ref, ref_cmp);
    if (!check_scan(&b, "after load")) return 1;
    printf("loaded %u (npages %u, root %u)\n", nref, b.npages, b.k[0].root);
    /* close and reopen: the file, not the cache, holds it */
    bt_close(&b, 1);
    if (!bt_open(&b, "t.key", 0, 16, fatal)) { printf("reopen failed\n"); return 1; }
    if (!check_scan(&b, "after reopen")) return 1;
    for (unsigned o = 0; o < ops; o++) {
        unsigned r = rnd() % 100;
        if (r < 45 && nref) {                                  /* remove a random entry by (key, slot) */
            unsigned i = rnd() % nref;
            int okr = (o & 1) ? bt_remove(&b, 0, ref[i].ka, ref[i].slot) : bt_remove_exact(&b, 0, ref[i].ka, bt_getbe(ref[i].ka + KL));
            if (!okr) { printf("remove failed at op %u\n", o); return 1; }
            bt_slot_set(&b, ref[i].slot, 0); slots_live--;
            memmove(ref + i, ref + i + 1, (nref - i - 1) * sizeof *ref); nref--;
        } else if (r < 90) {                                   /* insert, sometimes a duplicate key */
            unsigned char key[BT_KEYMAX]; mkkey(key, rnd() % (N / 2)); unsigned slot = bt_slot_alloc(&b); slots_live++;
            { unsigned char ex[64]; memset(ex, 0, sizeof ex); bt_put32(ex, slot * 3); bt_insert_x(&b, 0, key, seq, slot, b.k[0].extra ? ex : 0); }
            memcpy(ref[nref].ka, key, KL); bt_putbe(ref[nref].ka + KL, seq); ref[nref].slot = slot; nref++; seq++;
            qsort(ref, nref, sizeof *ref, ref_cmp);
        } else {                                               /* seek: first >= random key prefix, compare with ref */
            unsigned char key[BT_KEYMAX + 4]; memset(key, 0, sizeof key); mkkey(key, rnd() % (N / 2));
            unsigned page, ix; int got = bt_first_ge(&b, 0, key, KL + 4, &page, &ix);
            unsigned i = 0; while (i < nref && memcmp(ref[i].ka, key, KL + 4) < 0) i++;
            if (got != (i < nref)) { printf("seek presence mismatch at op %u\n", o); return 1; }
            if (got) { unsigned char ka[BT_KEYMAX + 4]; unsigned sl = bt_read(&b, 0, page, ix, ka); if (memcmp(ka, ref[i].ka, KL + 4)) { printf("seek mismatch at op %u\n", o); return 1; }
                if (b.k[0].extra >= 4) { unsigned char ex[64]; bt_extra_get(&b, 0, page, ix, ex); unsigned v = bt_get32(ex); if (v && v != sl * 3) { printf("extra mismatch at op %u\n", o); return 1; } } }
        }
        if (o % 997 == 0 && !check_scan(&b, "mid")) return 1;
    }
    if (!check_scan(&b, "at end")) return 1;
    /* bitmap: live count equals what we tracked */
    unsigned live = 0; for (unsigned s = 0; s < b.nslots; s++) live += bt_slot_live(&b, s);
    if (live != slots_live) { printf("bitmap live %u vs tracked %u\n", live, slots_live); return 1; }
    /* delete everything: the tree must collapse to one empty leaf, pages freed */
    while (nref) { unsigned i = rnd() % nref; if (!bt_remove(&b, 0, ref[i].ka, ref[i].slot)) { printf("final remove failed\n"); return 1; } memmove(ref + i, ref + i + 1, (nref - i - 1) * sizeof *ref); nref--; }
    if (!check_scan(&b, "empty")) return 1;
    unsigned char *root = bt_pin(&b, b.k[0].root); int rootleaf = root[0] == BT_LEAF && bt_count(root) == 0; bt_unpin(&b, b.k[0].root);
    unsigned freed = 0; for (unsigned f = b.free_head; f; ) { unsigned char *p = bt_pin(&b, f); unsigned nx = bt_get32(p + 4); bt_unpin(&b, f); f = nx; freed++; }
    printf("done: klen=%u ops=%u final nslots=%u npages=%u freed=%u root-is-empty-leaf=%d\n", KL, ops, b.nslots, b.npages, freed, rootleaf);
    bt_close(&b, 1);
    return rootleaf ? 0 : 1;
}
