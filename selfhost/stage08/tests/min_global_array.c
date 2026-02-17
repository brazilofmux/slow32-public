int g_arr[4];

void fill(void) {
    g_arr[0] = 1;
    g_arr[1] = 2;
    g_arr[2] = 3;
    g_arr[3] = 4;
}

int sum4(int *a) {
    return a[0] + a[1] + a[2] + a[3];
}

int main(void) {
    fill();
    return sum4(g_arr) - 2;
}
