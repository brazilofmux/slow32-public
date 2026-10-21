// A thread_local object with a destructor in freestanding C++: clang
// emits __cxa_thread_atexit, which libs32cxx.s32a provides (libc++abi in
// a libc++ build), over libc's exit list.  The destructor runs at exit.
extern "C" int printf(const char *, ...);

struct Kettle {
    int litres;
    explicit Kettle(int l) : litres(l) { printf("kettle filled %d\n", litres); }
    ~Kettle() { printf("kettle emptied %d\n", litres); }
};

thread_local Kettle pot(2);

int main() {
    printf("pot holds %d\n", pot.litres);
    return 0;
}
