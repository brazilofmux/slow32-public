int g_value;
int g_arr[3];

int main(void) {
    g_value = 7;
    g_arr[0] = 1;
    g_arr[1] = 2;
    g_arr[2] = 3;
    return (g_value + g_arr[0] + g_arr[1] + g_arr[2] == 13) ? 0 : 1;
}
