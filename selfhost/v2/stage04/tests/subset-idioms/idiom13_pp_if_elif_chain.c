#define F_A 1
#define F_B 2

#if defined(F_A) && !defined(F_C)
#define PICKED 21
#elif F_B == 2
#define PICKED 22
#else
#define PICKED 23
#endif

int main(void) {
    return (PICKED == 21) ? 0 : 1;
}
