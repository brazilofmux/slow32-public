// test_x0_x1.c — Verify x64_encode.h + elf_writer.h produce working binaries
//
// Build:  cc -O2 -o test_x0_x1 test_x0_x1.c
// Run:    ./test_x0_x1
//
// Generates several test ELF binaries and verifies them:
//   test_exit42   — exit(42)
//   test_hello    — write "Hello from SLOW-32 cross-compiler!\n" to stdout
//   test_arith    — compute 7*6, exit with result (42)
//   test_branch   — conditional branch, exit(42)
//   test_call     — function call, exit(42)

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/wait.h>

#include "../x64_encode.h"
#include "../elf_writer.h"

// Run a binary, return its exit code
static int run_binary(const char *path) {
    int status;
    pid_t pid = fork();
    if (pid == 0) {
        execl(path, path, NULL);
        _exit(127);
    }
    waitpid(pid, &status, 0);
    if (WIFEXITED(status))
        return WEXITSTATUS(status);
    return -1;
}

// Run a binary, capture stdout, return exit code
static int run_capture(const char *path, char *out, int maxlen) {
    int pipefd[2];
    pipe(pipefd);

    pid_t pid = fork();
    if (pid == 0) {
        close(pipefd[0]);
        dup2(pipefd[1], 1);
        close(pipefd[1]);
        execl(path, path, NULL);
        _exit(127);
    }
    close(pipefd[1]);
    int total = 0;
    int n;
    while ((n = read(pipefd[0], out + total, maxlen - total - 1)) > 0)
        total += n;
    out[total] = 0;
    close(pipefd[0]);

    int status;
    waitpid(pid, &status, 0);
    if (WIFEXITED(status))
        return WEXITSTATUS(status);
    return -1;
}

static int tests_passed = 0;
static int tests_failed = 0;

static void check(const char *name, int ok) {
    if (ok) {
        printf("  PASS: %s\n", name);
        tests_passed++;
    } else {
        printf("  FAIL: %s\n", name);
        tests_failed++;
    }
}

// ============================================================================
// Test 1: exit(42)
// ============================================================================

static void test_exit42(void) {
    x64_off = 0;

    // mov edi, 42
    x64_mov_ri(X64_RDI, 42);
    // mov eax, 60  (sys_exit)
    x64_mov_ri(X64_RAX, 60);
    // syscall
    x64_syscall();

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_exit42");

    int code = run_binary("./test_exit42");
    check("exit(42) returns 42", code == 42);
    unlink("test_exit42");
}

// ============================================================================
// Test 2: write("Hello...\n") to stdout, exit(0)
// ============================================================================

static void test_hello(void) {
    static const char msg[] = "Hello from SLOW-32 cross-compiler!\n";
    int msg_len = sizeof(msg) - 1;
    unsigned char rodata[64];
    memcpy(rodata, msg, msg_len);

    x64_off = 0;

    // We need the rodata virtual address. With text_len unknown yet,
    // we'll emit code first, then compute addresses.

    // Emit placeholder code — we'll fix the address after
    int code_start = x64_off;

    // mov eax, 1  (sys_write)
    x64_mov_ri(X64_RAX, 1);
    // mov edi, 1  (stdout)
    x64_mov_ri(X64_RDI, 1);
    // mov rsi, <rodata_addr>  — placeholder, will patch
    int rsi_patch = x64_off;
    x64_mov_ri64(X64_RSI, 0, 0);
    // mov edx, msg_len
    x64_mov_ri(X64_RDX, msg_len);
    // syscall
    x64_syscall();

    // mov edi, 0
    x64_mov_ri(X64_RDI, 0);
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    // syscall
    x64_syscall();

    int code_len = x64_off;

    // Now compute addresses and patch
    elf_init();
    elf_set_text(x64_buf, code_len);
    elf_set_rodata(rodata, msg_len);
    elf_set_entry(0);

    // Compute rodata vaddr and patch the MOV RSI instruction
    int rodata_addr = elf_rodata_vaddr();

    // The MOV r64, imm64 encoding is: REX.W B8+rd imm64
    // The immediate starts 2 bytes after rsi_patch
    x64_buf[rsi_patch + 2] = rodata_addr & 0xFF;
    x64_buf[rsi_patch + 3] = (rodata_addr >> 8) & 0xFF;
    x64_buf[rsi_patch + 4] = (rodata_addr >> 16) & 0xFF;
    x64_buf[rsi_patch + 5] = (rodata_addr >> 24) & 0xFF;
    // hi 32 bits are already 0

    elf_write_file("test_hello");

    char out[256];
    int code = run_capture("./test_hello", out, sizeof(out));
    check("hello writes correct message", strcmp(out, msg) == 0);
    check("hello exits 0", code == 0);
    unlink("test_hello");
}

// ============================================================================
// Test 3: compute 7 * 6 = 42, exit(result)
// ============================================================================

static void test_arith(void) {
    x64_off = 0;

    // mov eax, 7
    x64_mov_ri(X64_RAX, 7);
    // mov ecx, 6
    x64_mov_ri(X64_RCX, 6);
    // imul eax, ecx
    x64_imul_rr(X64_RAX, X64_RCX);
    // mov edi, eax
    x64_mov_rr(X64_RDI, X64_RAX);
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    // syscall
    x64_syscall();

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_arith");

    int code = run_binary("./test_arith");
    check("7 * 6 = 42", code == 42);
    unlink("test_arith");
}

// ============================================================================
// Test 4: conditional branch
// if (10 > 5) exit(42) else exit(1)
// ============================================================================

static void test_branch(void) {
    x64_off = 0;

    // mov eax, 10
    x64_mov_ri(X64_RAX, 10);
    // cmp eax, 5
    x64_cmp_ri(X64_RAX, 5);
    // jle fail
    int patch_fail = x64_jcc_placeholder(X64_CC_LE);
    // mov edi, 42
    x64_mov_ri(X64_RDI, 42);
    // jmp done
    int patch_done = x64_jmp_placeholder();
    // fail:
    int fail_off = x64_off;
    // mov edi, 1
    x64_mov_ri(X64_RDI, 1);
    // done:
    int done_off = x64_off;
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    // syscall
    x64_syscall();

    // Patch jumps
    x64_patch_rel32(patch_fail, fail_off);
    x64_patch_rel32(patch_done, done_off);

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_branch");

    int code = run_binary("./test_branch");
    check("branch: 10 > 5 → exit(42)", code == 42);
    unlink("test_branch");
}

// ============================================================================
// Test 5: function call
// add(20, 22) → 42
// ============================================================================

static void test_call(void) {
    x64_off = 0;

    // _start:
    //   mov edi, 20    (first arg)
    //   mov esi, 22    (second arg)
    //   call add_func
    //   mov edi, eax   (result → exit code)
    //   mov eax, 60
    //   syscall

    x64_mov_ri(X64_RDI, 20);
    x64_mov_ri(X64_RSI, 22);
    int call_patch = x64_call_placeholder();
    x64_mov_rr(X64_RDI, X64_RAX);
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    // add_func:
    //   lea eax, [rdi + rsi]     — we'll use add instead
    //   Actually: mov eax, edi; add eax, esi; ret
    int func_off = x64_off;
    x64_mov_rr(X64_RAX, X64_RDI);
    x64_add_rr(X64_RAX, X64_RSI);
    x64_ret();

    // Patch call
    x64_patch_rel32(call_patch, func_off);

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_call");

    int code = run_binary("./test_call");
    check("call: add(20, 22) = 42", code == 42);
    unlink("test_call");
}

// ============================================================================
// Test 6: stack frame — push/pop callee-saved, local variables
// ============================================================================

static void test_stack(void) {
    x64_off = 0;

    // _start:
    //   mov edi, 6
    //   call factorial
    //   mov edi, eax    ; 720 & 0xFF = 208
    //   mov eax, 60
    //   syscall

    x64_mov_ri(X64_RDI, 6);
    int call_patch = x64_call_placeholder();
    // 6! = 720, but exit code is modulo 256, so 720 % 256 = 208
    x64_mov_rr(X64_RDI, X64_RAX);
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    // factorial(n):
    //   push rbp
    //   mov rbp, rsp
    //   push rbx           ; callee-saved
    //   mov ebx, edi       ; n
    //   cmp ebx, 1
    //   jle base
    //   lea edi, [rbx - 1]
    //   call factorial
    //   imul eax, ebx
    //   jmp done
    // base:
    //   mov eax, 1
    // done:
    //   pop rbx
    //   pop rbp
    //   ret

    int func_off = x64_off;
    x64_push(X64_RBP);
    x64_mov_rr64(X64_RBP, X64_RSP);
    x64_push(X64_RBX);
    x64_mov_rr(X64_RBX, X64_RDI);
    x64_cmp_ri(X64_RBX, 1);
    int patch_base = x64_jcc_placeholder(X64_CC_LE);

    // recursive case
    x64_lea32(X64_RDI, X64_RBX, -1);
    int call2_patch = x64_call_placeholder();
    x64_imul_rr(X64_RAX, X64_RBX);
    int patch_done = x64_jmp_placeholder();

    // base case
    int base_off = x64_off;
    x64_mov_ri(X64_RAX, 1);

    // done
    int done_off = x64_off;
    x64_pop(X64_RBX);
    x64_pop(X64_RBP);
    x64_ret();

    // Patch
    x64_patch_rel32(call_patch, func_off);
    x64_patch_rel32(patch_base, base_off);
    x64_patch_rel32(call2_patch, func_off);
    x64_patch_rel32(patch_done, done_off);

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_stack");

    int code = run_binary("./test_stack");
    check("factorial(6) mod 256 = 208", code == 208);
    unlink("test_stack");
}

// ============================================================================
// Test 7: high registers (R8-R15)
// ============================================================================

static void test_high_regs(void) {
    x64_off = 0;

    // mov r12d, 10
    x64_mov_ri(X64_R12, 10);
    // mov r13d, 20
    x64_mov_ri(X64_R13, 20);
    // mov r14d, 12
    x64_mov_ri(X64_R14, 12);
    // add r12d, r13d  → 30
    x64_add_rr(X64_R12, X64_R13);
    // add r12d, r14d  → 42
    x64_add_rr(X64_R12, X64_R14);
    // mov edi, r12d
    x64_mov_rr(X64_RDI, X64_R12);
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    // syscall
    x64_syscall();

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_high_regs");

    int code = run_binary("./test_high_regs");
    check("R12+R13+R14 = 10+20+12 = 42", code == 42);
    unlink("test_high_regs");
}

// ============================================================================
// Test 8: division and remainder
// ============================================================================

static void test_divmod(void) {
    x64_off = 0;

    // 85 / 2 = 42, remainder 1
    // mov eax, 85
    x64_mov_ri(X64_RAX, 85);
    // cdq
    x64_cdq();
    // mov ecx, 2
    x64_mov_ri(X64_RCX, 2);
    // idiv ecx  → eax=42, edx=1
    x64_idiv(X64_RCX);
    // mov edi, eax
    x64_mov_rr(X64_RDI, X64_RAX);
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_divmod");

    int code = run_binary("./test_divmod");
    check("85 / 2 = 42", code == 42);
    unlink("test_divmod");
}

// ============================================================================
// Test 9: shifts
// ============================================================================

static void test_shifts(void) {
    x64_off = 0;

    // (5 << 3) + 2 = 42
    x64_mov_ri(X64_RAX, 5);
    x64_shl_ri(X64_RAX, 3);
    x64_add_ri(X64_RAX, 2);
    x64_mov_rr(X64_RDI, X64_RAX);
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    elf_init();
    elf_set_text(x64_buf, x64_off);
    elf_set_entry(0);
    elf_write_file("test_shifts");

    int code = run_binary("./test_shifts");
    check("(5 << 3) + 2 = 42", code == 42);
    unlink("test_shifts");
}

// ============================================================================
// Test 10: data section — read initialized global
// ============================================================================

static void test_data_section(void) {
    // Put the value 42 in .data, load it, exit
    unsigned char data[4] = { 42, 0, 0, 0 };

    x64_off = 0;

    // mov rsi, <data_vaddr>  — placeholder
    int mov_patch = x64_off;
    x64_mov_ri64(X64_RSI, 0, 0);
    // mov edi, [rsi]
    x64_mov_rm(X64_RDI, X64_RSI, 0);
    // mov eax, 60
    x64_mov_ri(X64_RAX, 60);
    x64_syscall();

    int code_len = x64_off;

    elf_init();
    elf_set_text(x64_buf, code_len);
    elf_set_data(data, 4);
    elf_set_entry(0);

    // Patch data address
    int daddr = elf_data_vaddr();
    x64_buf[mov_patch + 2] = daddr & 0xFF;
    x64_buf[mov_patch + 3] = (daddr >> 8) & 0xFF;
    x64_buf[mov_patch + 4] = (daddr >> 16) & 0xFF;
    x64_buf[mov_patch + 5] = (daddr >> 24) & 0xFF;

    elf_write_file("test_data");

    int code = run_binary("./test_data");
    check("load 42 from .data section", code == 42);
    unlink("test_data");
}

// ============================================================================
// Main
// ============================================================================

int main(void) {
    printf("Stage X0+X1: x86-64 encoder + ELF writer tests\n");
    printf("================================================\n");

    test_exit42();
    test_hello();
    test_arith();
    test_branch();
    test_call();
    test_stack();
    test_high_regs();
    test_divmod();
    test_shifts();
    test_data_section();

    printf("================================================\n");
    printf("Results: %d passed, %d failed\n", tests_passed, tests_failed);
    return tests_failed ? 1 : 0;
}
