static int get2_from_void(const void *vp) {
    const unsigned char *p = (const unsigned char *)vp;
    return (int)p[2];
}

int main(void) {
    unsigned char a[4];
    a[0] = 10;
    a[1] = 20;
    a[2] = 30;
    a[3] = 40;
    return (get2_from_void(a) == 30) ? 0 : 1;
}
