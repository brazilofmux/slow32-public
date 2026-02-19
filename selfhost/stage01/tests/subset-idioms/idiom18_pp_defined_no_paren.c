#define HAVE_ONE 1

#if defined HAVE_ONE
#define OUT 3
#else
#define OUT 4
#endif

int main(void) {
    return (OUT == 3) ? 0 : 1;
}
