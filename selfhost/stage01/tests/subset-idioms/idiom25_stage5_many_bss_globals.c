int g0;
int g1;
char g2[1536];
char g3[768];
char g4[32768];
char g5[32768];
char g6[32768];
char g7[65535];
int g8;

int main(void) {
    g0 = 1;
    g1 = 2;
    g8 = 3;
    return (g0 + g1 + g8 == 6) ? 0 : 1;
}
