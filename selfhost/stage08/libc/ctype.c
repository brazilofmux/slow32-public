/* ctype.c -- character classification for selfhost libc
 * Written for cc-min subset-C compatibility.
 * Uses ASCII value comparisons (no lookup table). */

int isdigit(int c) {
    if (c >= 48 && c <= 57) return 1;
    return 0;
}

int isxdigit(int c) {
    if (c >= 48 && c <= 57) return 1;
    if (c >= 65 && c <= 70) return 1;
    if (c >= 97 && c <= 102) return 1;
    return 0;
}

int isalpha(int c) {
    if (c >= 65 && c <= 90) return 1;
    if (c >= 97 && c <= 122) return 1;
    return 0;
}

int isalnum(int c) {
    if (c >= 48 && c <= 57) return 1;
    if (c >= 65 && c <= 90) return 1;
    if (c >= 97 && c <= 122) return 1;
    return 0;
}

int isupper(int c) {
    if (c >= 65 && c <= 90) return 1;
    return 0;
}

int islower(int c) {
    if (c >= 97 && c <= 122) return 1;
    return 0;
}

int isspace(int c) {
    if (c == 32) return 1;
    if (c == 9) return 1;
    if (c == 10) return 1;
    if (c == 13) return 1;
    if (c == 11) return 1;
    if (c == 12) return 1;
    return 0;
}

int isprint(int c) {
    if (c >= 32 && c <= 126) return 1;
    return 0;
}

int ispunct(int c) {
    if (c >= 33 && c <= 47) return 1;
    if (c >= 58 && c <= 64) return 1;
    if (c >= 91 && c <= 96) return 1;
    if (c >= 123 && c <= 126) return 1;
    return 0;
}

int tolower(int c) {
    if (c >= 65 && c <= 90) return c + 32;
    return c;
}

int toupper(int c) {
    if (c >= 97 && c <= 122) return c - 32;
    return c;
}
