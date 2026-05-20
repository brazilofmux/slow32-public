/* Regression test for the RMW-fold narrow-type bug in
 * selfhost/stage08-cross-x64/hir_codegen_x64.h.
 *
 * The RMW fold (STORE addr, ADD(LOAD(addr), ICONST)) used to fire for
 * any non-wide STORE, including TY_CHAR/TY_SHORT.  The emit path
 * (`x64_add_mi` → `addl r/m32, imm`) is a 32-bit memory add, so for a
 * uint8_t / uint16_t field the fold wrote 4 bytes — the intended byte
 * plus 1-3 garbage bytes into adjacent struct fields.
 *
 * The bug was silent in many cases because the overwritten bytes
 * happened to match what they'd be after a 32-bit `add` propagated
 * carries through them.  Detect it deterministically by setting up a
 * struct where the corruption is visible:
 *
 *   - Carry-test (byte_carry): the byte field is 0xFF before `+= 1`.
 *     A correct 1-byte add gives 0x00; a 32-bit add gives 0x00 too,
 *     BUT it also sets the next byte from 0x00 → 0x01 (carry into
 *     bits 8..15).  Adjacent guard byte exposes this.
 *
 *   - Adjacent-clobber: the byte field's value alone doesn't change
 *     visibly even with the buggy add — the corruption shows up in
 *     the byte after it.  Initialize the adjacent guard to a
 *     distinctive value that wouldn't naturally end up there after
 *     a correct add.
 *
 * Returns 0 on success, non-zero bitmap on failure.
 */

#include <stdint.h>

struct byte_struct {
    uint8_t before;
    uint8_t target;
    uint8_t after;
    uint8_t pad;
};

struct half_struct {
    uint16_t before;
    uint16_t target;
    uint16_t after;
};

void inc_byte(struct byte_struct *p) {
    p->target += 1;
}

void inc_half(struct half_struct *p) {
    p->target += 1;
}

int main(void) {
    int fail = 0;

    /* Case 1: byte_field = 0xFF + 1.  Correct: target=0x00, after unchanged.
     * Buggy: 32-bit add reads dword (LE = 0x..00 00 FF), +1 = 0x..00 01 00,
     * writes back: target=0x00, after=0x01 (corrupted from 0xCC). */
    struct byte_struct s;
    s.before = 0xAA;
    s.target = 0xFF;
    s.after  = 0xCC;
    s.pad    = 0xDD;
    inc_byte(&s);
    if (s.before != 0xAA) fail = fail | 0x01;
    if (s.target != 0x00) fail = fail | 0x02;
    if (s.after  != 0xCC) fail = fail | 0x04;  /* THIS catches the bug */
    if (s.pad    != 0xDD) fail = fail | 0x08;

    /* Case 2: byte_field = 0x00 + 1.  Correct: target=0x01, after unchanged.
     * Buggy 32-bit add of 0xDDCC0000 + 1 = 0xDDCC0001 → target=0x01,
     * after=0x00 (corrupted from 0xCC). */
    s.before = 0xAA;
    s.target = 0x00;
    s.after  = 0xCC;
    s.pad    = 0xDD;
    inc_byte(&s);
    if (s.before != 0xAA) fail = fail | 0x10;
    if (s.target != 0x01) fail = fail | 0x20;
    if (s.after  != 0xCC) fail = fail | 0x40;  /* THIS catches the bug */

    /* Case 3: half_field = 0xFFFF + 1.  Correct: target=0x0000, after unchanged.
     * Buggy 32-bit add of (after << 16 | 0xFFFF) + 1 = ((after+1) << 16) | 0
     * → target=0x0000, after=after+1 (corrupted). */
    struct half_struct h;
    h.before = 0xAAAA;
    h.target = 0xFFFF;
    h.after  = 0xCCCC;
    inc_half(&h);
    if (h.before != 0xAAAA) fail = fail | 0x100;
    if (h.target != 0x0000) fail = fail | 0x200;
    if (h.after  != 0xCCCC) fail = fail | 0x400;  /* THIS catches the bug */

    return fail;
}
