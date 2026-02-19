int main(void) {
    int i;
    int j;
    int acc = 0;

    for (i = 0; i < 4; i++) {
        for (j = 0; j < 3; j++) {
            if (j == 2) break;
            acc = acc + i;
        }
    }

    return (acc == 12) ? 0 : 1;
}
