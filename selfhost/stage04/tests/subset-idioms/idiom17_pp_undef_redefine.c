#define VALUE 1
#undef VALUE
#define VALUE 5

int main(void) {
    return (VALUE == 5) ? 0 : 1;
}
