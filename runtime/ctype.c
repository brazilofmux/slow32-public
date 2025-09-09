// SLOW-32 ctype.h implementation
// Character classification and conversion functions

int isalnum(int c) {
    return (c >= '0' && c <= '9') || 
           (c >= 'A' && c <= 'Z') || 
           (c >= 'a' && c <= 'z');
}

int isalpha(int c) {
    return (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z');
}

int isblank(int c) {
    return c == ' ' || c == '\t';
}

int iscntrl(int c) {
    return (c >= 0 && c < 32) || c == 127;
}

int isdigit(int c) {
    return c >= '0' && c <= '9';
}

int isgraph(int c) {
    return c > 32 && c < 127;
}

int islower(int c) {
    return c >= 'a' && c <= 'z';
}

int isprint(int c) {
    return c >= 32 && c < 127;
}

int ispunct(int c) {
    return isgraph(c) && !isalnum(c);
}

int isspace(int c) {
    return c == ' ' || c == '\t' || c == '\n' || 
           c == '\v' || c == '\f' || c == '\r';
}

int isupper(int c) {
    return c >= 'A' && c <= 'Z';
}

int isxdigit(int c) {
    return (c >= '0' && c <= '9') || 
           (c >= 'A' && c <= 'F') || 
           (c >= 'a' && c <= 'f');
}

int tolower(int c) {
    if (c >= 'A' && c <= 'Z') {
        return c + ('a' - 'A');
    }
    return c;
}

int toupper(int c) {
    if (c >= 'a' && c <= 'z') {
        return c - ('a' - 'A');
    }
    return c;
}

// ASCII-only versions (same as regular for SLOW-32)
int isascii(int c) {
    return c >= 0 && c <= 127;
}

int toascii(int c) {
    return c & 0x7F;
}