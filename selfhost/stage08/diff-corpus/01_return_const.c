/* Smoking gun for Issue #31: gen2 was observed dropping the
   `addi r1, r0, 0` constant load for a literal `return 0;`.
   Both compilers should emit identical asm for this trivial main. */
int main(void) {
    return 0;
}
