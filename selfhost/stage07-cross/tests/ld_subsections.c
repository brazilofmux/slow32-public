/* Exercise ld-x64 with multiple subsectioned input sections in one object.
 *
 * The linker needs to resolve symbols and relocations against the specific
 * input section that defined them, not just the last .text.* / .rodata.*
 * section seen in the object.
 */

const char alpha[] __attribute__((section(".rodata.alpha"))) = "A";
const char beta[] __attribute__((section(".rodata.beta"))) = "B";

__attribute__((noinline, section(".text.alpha")))
static int pick_alpha(void) {
    return ((volatile const char *)alpha)[0];
}

__attribute__((noinline, section(".text.beta")))
static int pick_beta(void) {
    return ((volatile const char *)beta)[0];
}

__attribute__((section(".text.hot")))
static int helper(void) {
    return pick_alpha() + pick_beta();
}

__attribute__((noreturn, section(".text.startup")))
void _start(void) {
    long code;

    code = helper() == ('A' + 'B') ? 0 : 7;

    __asm__ volatile(
        "mov $60, %%rax\n\t"
        "mov %0, %%rdi\n\t"
        "syscall\n\t"
        :
        : "r"(code)
        : "rax", "rdi", "rcx", "r11", "memory");

    __builtin_unreachable();
}
