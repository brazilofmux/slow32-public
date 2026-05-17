static int pickv(const unsigned char *p, int i, int prefer_next) {
    int a = (int)p[i];
    int b = (int)p[i + 1];
    return prefer_next ? b : a;
}

int main(void) {
    unsigned char rel[6];
    int x;
    int y;
    int z;
    rel[0] = 2;
    rel[1] = 9;
    rel[2] = 3;
    rel[3] = 9;
    rel[4] = 2;
    rel[5] = 8;

    x = pickv(rel, 0, rel[0] == 2 ? 1 : 0);
    y = pickv(rel, 2, rel[3] == 9 ? 0 : 1);
    z = ((int)rel[4] == 2 && (int)rel[5] == 8) ? 1 : 0;
    return (x == 9 && y == 3 && z == 1) ? 0 : 1;
}
