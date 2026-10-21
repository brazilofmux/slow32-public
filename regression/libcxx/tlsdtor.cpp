// A thread_local object with a destructor: clang emits a call to
// __cxa_thread_atexit, which both libc++abi and the C runtime define.
// The destructor runs at exit, after the main thread's statics are made
// and before atexit's own (registered first) handler.
#include <cstdio>
#include <cstdlib>

struct Lamp {
    int watts;
    explicit Lamp(int w) : watts(w) { std::printf("lamp on %d\n", watts); }
    ~Lamp() { std::printf("lamp off %d\n", watts); }
};

thread_local Lamp desk(40);

static void last() { std::printf("atexit handler\n"); }

int main() {
    std::atexit(last);
    std::printf("desk lamp draws %d\n", desk.watts);
    return 0;
}
