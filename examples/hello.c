// Example C program for SLOW-32
// Compile with: clang --target slow32-unknown-none -S -emit-llvm -O1 hello.c -o hello.ll

void debug_char(char c) {
    // In assembly, this would use the DEBUG instruction
    // The compiler intrinsic or runtime library would handle this
    __asm__("debug %0" : : "r"(c));
}

void print_string(const char* str) {
    while (*str) {
        debug_char(*str);
        str++;
    }
}

int main() {
    print_string("Hello from C on SLOW-32!\n");
    return 0;
}