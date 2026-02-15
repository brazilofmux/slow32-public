int main(void) {
    int i;
    int j;
    int sum = 0;

    for (i = 0; i < 3; i++) {
        for (j = 0; j < 2; j++) {
            sum = sum + 1;
            if (j == 0) continue;
        }
    }

    return (sum == 6) ? 0 : 1;
}
