// .cfi_remember_state deeper than the assembler's old eight saved CFA
// offsets: an exception unwinds through `nest`, whose CFA after a restore
// from depth ten is only right if the tenth offset was kept.  The old
// assembler kept eight, so the .cfi_adjust_cfa_offset after that restore
// started from a stale offset, and the unwinder looked for the return
// address four bytes off.  Self-checking.
extern "C" int printf(const char *, ...);

struct Spill { int drops; };

extern "C" void spill(void) { throw Spill{7}; }
extern "C" void nest(void);

asm(
    "\t.text\n"
    "\t.globl nest\n"
    "\t.p2align 2\n"
    "nest:\n"
    "\t.cfi_startproc\n"
    "\taddi sp, sp, -16\n"
    "\t.cfi_def_cfa_offset 16\n"
    "\tstw sp+12, lr\n"
    "\t.cfi_offset lr, -4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\t.cfi_remember_state\n"
    "\taddi sp, sp, -4\n"
    "\t.cfi_adjust_cfa_offset 4\n"
    "\taddi sp, sp, 4\n"
    "\t.cfi_restore_state\n"
    "\taddi sp, sp, -8\n"
    "\t.cfi_adjust_cfa_offset 8\n"
    "\tjal lr, spill\n"
    "\taddi sp, sp, 8\n"
    "\t.cfi_adjust_cfa_offset -8\n"
    "\taddi sp, sp, 36\n"
    "\t.cfi_adjust_cfa_offset -36\n"
    "\tldw lr, sp+12\n"
    "\taddi sp, sp, 16\n"
    "\t.cfi_def_cfa_offset 0\n"
    "\tjalr r0, lr, 0\n"
    "\t.cfi_endproc\n"
);

int main() {
    try {
        nest();
        printf("nest returned: FAIL\n");
        return 1;
    } catch (Spill &s) {
        printf("caught through nest, drops %d: PASS\n", s.drops);
    }
    return 0;
}
