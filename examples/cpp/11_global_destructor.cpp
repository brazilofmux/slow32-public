extern "C" int putchar(int ch);

struct Marker {
    Marker() { emit("CTOR\n"); }
    ~Marker() { emit("DTOR\n"); }

    static void emit(const char *text) {
        for (const char *p = text; *p != '\0'; ++p) {
            putchar(*p);
        }
    }
};

static Marker marker;

int main() {
    Marker::emit("MAIN\n");
    return 0;
}
