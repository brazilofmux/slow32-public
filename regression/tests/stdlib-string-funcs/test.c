#include <stdio.h>
#include <string.h>
#include <stdlib.h>

int main() {
    char buf[256];
    char buf2[256];

    /* strlen */
    printf("%s: strlen empty\n", strlen("") == 0 ? "PASS" : "FAIL");
    printf("%s: strlen hello\n", strlen("hello") == 5 ? "PASS" : "FAIL");
    printf("%s: strlen long\n", strlen("abcdefghijklmnop") == 16 ? "PASS" : "FAIL");

    /* strnlen */
    printf("%s: strnlen within\n", strnlen("hello", 10) == 5 ? "PASS" : "FAIL");
    printf("%s: strnlen capped\n", strnlen("hello", 3) == 3 ? "PASS" : "FAIL");
    printf("%s: strnlen empty\n", strnlen("", 5) == 0 ? "PASS" : "FAIL");

    /* strcpy */
    strcpy(buf, "world");
    printf("%s: strcpy\n", strcmp(buf, "world") == 0 ? "PASS" : "FAIL");

    /* strncpy */
    memset(buf, 'X', 10);
    strncpy(buf, "hi", 5);
    printf("%s: strncpy content\n", strcmp(buf, "hi") == 0 ? "PASS" : "FAIL");
    printf("%s: strncpy null-pad\n", buf[2] == '\0' && buf[3] == '\0' && buf[4] == '\0' ? "PASS" : "FAIL");

    memset(buf, 'X', 10);
    strncpy(buf, "hello", 3);
    printf("%s: strncpy trunc\n", buf[0] == 'h' && buf[1] == 'e' && buf[2] == 'l' ? "PASS" : "FAIL");

    /* strcmp */
    printf("%s: strcmp equal\n", strcmp("abc", "abc") == 0 ? "PASS" : "FAIL");
    printf("%s: strcmp less\n", strcmp("abc", "abd") < 0 ? "PASS" : "FAIL");
    printf("%s: strcmp greater\n", strcmp("abd", "abc") > 0 ? "PASS" : "FAIL");
    printf("%s: strcmp prefix\n", strcmp("ab", "abc") < 0 ? "PASS" : "FAIL");

    /* strncmp */
    printf("%s: strncmp equal n\n", strncmp("abcXXX", "abcYYY", 3) == 0 ? "PASS" : "FAIL");
    printf("%s: strncmp diff\n", strncmp("abcXXX", "abdYYY", 3) < 0 ? "PASS" : "FAIL");
    printf("%s: strncmp zero\n", strncmp("abc", "xyz", 0) == 0 ? "PASS" : "FAIL");

    /* strcat */
    strcpy(buf, "hello");
    strcat(buf, " world");
    printf("%s: strcat\n", strcmp(buf, "hello world") == 0 ? "PASS" : "FAIL");

    /* strncat */
    strcpy(buf, "hi");
    strncat(buf, "XYZ123", 3);
    printf("%s: strncat\n", strcmp(buf, "hiXYZ") == 0 ? "PASS" : "FAIL");

    /* strchr */
    printf("%s: strchr found\n", strchr("hello", 'l') == &"hello"[2] ? "PASS" : "FAIL");
    printf("%s: strchr not found\n", strchr("hello", 'z') == NULL ? "PASS" : "FAIL");
    printf("%s: strchr null-term\n", strchr("hello", '\0') != NULL ? "PASS" : "FAIL");

    /* strrchr */
    printf("%s: strrchr found\n", strrchr("hello", 'l') == &"hello"[3] ? "PASS" : "FAIL");
    printf("%s: strrchr not found\n", strrchr("hello", 'z') == NULL ? "PASS" : "FAIL");

    /* strstr */
    const char *hay = "hello world";
    printf("%s: strstr found\n", strstr(hay, "world") == hay + 6 ? "PASS" : "FAIL");
    printf("%s: strstr not found\n", strstr(hay, "xyz") == NULL ? "PASS" : "FAIL");
    printf("%s: strstr empty needle\n", strstr(hay, "") == hay ? "PASS" : "FAIL");

    /* strpbrk */
    printf("%s: strpbrk found\n", strpbrk("hello", "ol") == &"hello"[2] ? "PASS" : "FAIL");
    printf("%s: strpbrk not found\n", strpbrk("hello", "xyz") == NULL ? "PASS" : "FAIL");

    /* strspn */
    printf("%s: strspn\n", strspn("aabbcc", "ab") == 4 ? "PASS" : "FAIL");
    printf("%s: strspn none\n", strspn("hello", "xyz") == 0 ? "PASS" : "FAIL");

    /* strcspn */
    printf("%s: strcspn\n", strcspn("hello", "lo") == 2 ? "PASS" : "FAIL");
    printf("%s: strcspn all\n", strcspn("hello", "xyz") == 5 ? "PASS" : "FAIL");

    /* strtok */
    strcpy(buf, "one,two,,three");
    char *tok = strtok(buf, ",");
    int tok_ok = (tok != NULL && strcmp(tok, "one") == 0);
    tok = strtok(NULL, ",");
    tok_ok = tok_ok && (tok != NULL && strcmp(tok, "two") == 0);
    tok = strtok(NULL, ",");
    tok_ok = tok_ok && (tok != NULL && strcmp(tok, "three") == 0);
    tok = strtok(NULL, ",");
    tok_ok = tok_ok && (tok == NULL);
    printf("%s: strtok\n", tok_ok ? "PASS" : "FAIL");

    /* strtok_r */
    strcpy(buf, "a:b:c");
    char *save;
    tok = strtok_r(buf, ":", &save);
    int tr_ok = (tok != NULL && strcmp(tok, "a") == 0);
    tok = strtok_r(NULL, ":", &save);
    tr_ok = tr_ok && (tok != NULL && strcmp(tok, "b") == 0);
    tok = strtok_r(NULL, ":", &save);
    tr_ok = tr_ok && (tok != NULL && strcmp(tok, "c") == 0);
    tok = strtok_r(NULL, ":", &save);
    tr_ok = tr_ok && (tok == NULL);
    printf("%s: strtok_r\n", tr_ok ? "PASS" : "FAIL");

    /* strdup */
    char *dup = strdup("test123");
    printf("%s: strdup\n", dup != NULL && strcmp(dup, "test123") == 0 ? "PASS" : "FAIL");
    free(dup);

    /* strcasecmp */
    printf("%s: strcasecmp equal\n", strcasecmp("Hello", "hello") == 0 ? "PASS" : "FAIL");
    printf("%s: strcasecmp diff\n", strcasecmp("abc", "abd") < 0 ? "PASS" : "FAIL");

    /* strncasecmp */
    printf("%s: strncasecmp\n", strncasecmp("HelloXX", "helloYY", 5) == 0 ? "PASS" : "FAIL");

    /* memcpy */
    memset(buf, 0, 16);
    memcpy(buf, "ABCDEF", 6);
    printf("%s: memcpy\n", memcmp(buf, "ABCDEF", 6) == 0 ? "PASS" : "FAIL");

    /* memmove non-overlapping */
    memset(buf, 0, 16);
    memmove(buf, "12345", 5);
    printf("%s: memmove\n", memcmp(buf, "12345", 5) == 0 ? "PASS" : "FAIL");

    /* memset */
    memset(buf, 'Z', 8);
    int ms_ok = 1;
    for (int i = 0; i < 8; i++) {
        if (buf[i] != 'Z') ms_ok = 0;
    }
    printf("%s: memset\n", ms_ok ? "PASS" : "FAIL");

    /* memcmp */
    printf("%s: memcmp equal\n", memcmp("abc", "abc", 3) == 0 ? "PASS" : "FAIL");
    printf("%s: memcmp less\n", memcmp("abc", "abd", 3) < 0 ? "PASS" : "FAIL");
    printf("%s: memcmp greater\n", memcmp("abd", "abc", 3) > 0 ? "PASS" : "FAIL");

    /* memchr */
    const char *mc = "abcdef";
    printf("%s: memchr found\n", memchr(mc, 'd', 6) == mc + 3 ? "PASS" : "FAIL");
    printf("%s: memchr not found\n", memchr(mc, 'z', 6) == NULL ? "PASS" : "FAIL");

    /* memrchr */
    const char *mr = "abcdcd";
    printf("%s: memrchr found\n", memrchr(mr, 'c', 6) == mr + 4 ? "PASS" : "FAIL");
    printf("%s: memrchr not found\n", memrchr(mr, 'z', 6) == NULL ? "PASS" : "FAIL");

    return 0;
}
