int g_x;
int g_y;

void set_vals(void) {
    g_x = 4;
    g_y = 6;
}

int main(void) {
    set_vals();
    return g_x + g_y;
}
