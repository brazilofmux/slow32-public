int main(void) {
    int i;
    int sum;
    i = 0;
    sum = 0;
    while (i < 20) {
        i = i + 1;
        if (i == 5) continue;
        if (i == 11) break;
        sum = sum + 1;
    }
    return sum;
}
