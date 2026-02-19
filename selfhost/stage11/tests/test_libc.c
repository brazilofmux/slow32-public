/* Test extended libc functions: ctype, string_more */

int isdigit(int c);
int isalpha(int c);
int isalnum(int c);
int isspace(int c);
int isupper(int c);
int islower(int c);
int tolower(int c);
int toupper(int c);
int strcmp(char *a, char *b);
int strlen(char *s);
int memcmp(char *a, char *b, int n);
char *memchr(char *s, int c, int n);
char *strdup(char *s);
char *strcat(char *dst, char *src);
char *strstr(char *hay, char *needle);
char *strrchr(char *s, int c);
char *strcpy(char *dst, char *src);
int atoi(char *s);
void free(char *p);

int test_ctype(void) {
    if (!isdigit(48)) return 1;
    if (!isdigit(57)) return 2;
    if (isdigit(47)) return 3;
    if (!isalpha(65)) return 4;
    if (!isalpha(122)) return 5;
    if (isalpha(64)) return 6;
    if (!isalnum(48)) return 7;
    if (!isalnum(65)) return 8;
    if (isalnum(32)) return 9;
    if (!isspace(32)) return 10;
    if (!isspace(10)) return 11;
    if (isspace(65)) return 12;
    if (tolower(65) != 97) return 13;
    if (toupper(97) != 65) return 14;
    if (tolower(97) != 97) return 15;
    return 0;
}

int test_memcmp(void) {
    char a[4];
    char b[4];
    a[0] = 1; a[1] = 2; a[2] = 3; a[3] = 0;
    b[0] = 1; b[1] = 2; b[2] = 3; b[3] = 0;
    if (memcmp(a, b, 3) != 0) return 1;
    b[1] = 9;
    if (memcmp(a, b, 3) == 0) return 2;
    return 0;
}

int test_memchr(void) {
    char buf[4];
    char *p;
    buf[0] = 10; buf[1] = 20; buf[2] = 30; buf[3] = 0;
    p = memchr(buf, 20, 4);
    if (!p) return 1;
    if (p != buf + 1) return 2;
    p = memchr(buf, 99, 4);
    if (p) return 3;
    return 0;
}

int test_strdup(void) {
    char *s;
    s = strdup("hello");
    if (!s) return 1;
    if (strcmp(s, "hello") != 0) return 2;
    if (strlen(s) != 5) return 3;
    free(s);
    return 0;
}

int test_strcat(void) {
    char buf[32];
    buf[0] = 0;
    strcat(buf, "hello");
    strcat(buf, " world");
    if (strcmp(buf, "hello world") != 0) return 1;
    return 0;
}

int test_strstr(void) {
    char *p;
    p = strstr("hello world", "world");
    if (!p) return 1;
    if (strcmp(p, "world") != 0) return 2;
    p = strstr("hello", "xyz");
    if (p) return 3;
    p = strstr("hello", "");
    if (!p) return 4;
    return 0;
}

int test_strrchr(void) {
    char *p;
    p = strrchr("a/b/c", 47);
    if (!p) return 1;
    if (strcmp(p, "/c") != 0) return 2;
    return 0;
}

int test_strcpy(void) {
    char buf[16];
    strcpy(buf, "test");
    if (strcmp(buf, "test") != 0) return 1;
    return 0;
}

int test_atoi(void) {
    if (atoi("42") != 42) return 1;
    if (atoi("-7") != -7) return 2;
    if (atoi("  123") != 123) return 3;
    if (atoi("0") != 0) return 4;
    return 0;
}

int main(void) {
    int rc;
    rc = test_ctype();
    if (rc) return rc;
    rc = test_memcmp();
    if (rc) return rc + 20;
    rc = test_memchr();
    if (rc) return rc + 30;
    rc = test_strdup();
    if (rc) return rc + 40;
    rc = test_strcat();
    if (rc) return rc + 50;
    rc = test_strstr();
    if (rc) return rc + 60;
    rc = test_strrchr();
    if (rc) return rc + 70;
    rc = test_strcpy();
    if (rc) return rc + 80;
    rc = test_atoi();
    if (rc) return rc + 90;
    return 0;
}
