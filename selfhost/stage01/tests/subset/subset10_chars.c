int main(void) {
    char s[] = "a\t,";
    if (s[0] != 97) return 1;
    if (s[1] != 9) return 2;
    if (s[2] != 44) return 3;
    if (s[3] != 0) return 4;
    return 0;
}
