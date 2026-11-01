/* 7.1.4p2: a library function whose declaration needs no header type may be declared by hand and called without its header */
extern int toupper(int);
extern int isxdigit(int);
extern int abs(int);
extern int printf(const char *, ...);

int main(void) {
    printf("%c %d %d\n", toupper('m'), isxdigit('f') != 0, abs(-3));
    return 0;
}
