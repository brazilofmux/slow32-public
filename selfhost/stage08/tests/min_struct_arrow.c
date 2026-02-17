struct point {
    int x;
    int y;
};

void set_x(struct point *p, int val) {
    p->x = val;
}

int main(void) {
    struct point p;
    p.x = 0;
    p.y = 0;
    set_x(&p, 10);
    return p.x;
}
