struct point {
    int x;
    int y;
};

int main(void) {
    struct point arr[4];
    arr[0].x = 10;
    arr[1].x = 20;
    arr[2].x = 99;
    arr[3].x = 40;
    return arr[2].x;
}
