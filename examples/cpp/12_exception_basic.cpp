// Basic C++ exception handling test for SLOW-32
// Tests: throw int, catch int, catch-all

extern "C" int printf(const char *fmt, ...);

void throw_int(int value) {
    printf("Throwing %d\n", value);
    throw value;
}

int main() {
    printf("Exception test start\n");

    // Test 1: throw and catch int
    try {
        throw_int(42);
        printf("ERROR: should not reach here\n");
    } catch (int e) {
        printf("Caught int: %d\n", e);
    }

    printf("After first catch\n");

    // Test 2: catch-all
    try {
        throw 99;
    } catch (...) {
        printf("Caught with catch-all\n");
    }

    printf("Exception test done\n");
    return 0;
}
