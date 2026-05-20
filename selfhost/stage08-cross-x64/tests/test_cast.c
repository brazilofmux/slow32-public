/* Test type casts, sign extension, narrowing */

int sign_extend_byte(int v) {
    return (int)((char)v);
}

int zero_extend_byte(int v) {
    return (int)((unsigned char)v);
}

int main(int argc, char **argv) {
    int a;
    int b;
    /* 0xFF sign-extended = -1, zero-extended = 255 */
    a = sign_extend_byte(255);   /* -1 */
    b = zero_extend_byte(255);   /* 255 */
    return b + a;                /* 255 + (-1) = 254 */
}
