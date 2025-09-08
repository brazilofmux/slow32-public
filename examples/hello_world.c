void putchar(int c);

const char msg[] = "Hello, SLOW-32!\n";

int main() {
    for (int i = 0; msg[i] != 0; i++) {
        putchar(msg[i]);
    }
    return 0;
}
