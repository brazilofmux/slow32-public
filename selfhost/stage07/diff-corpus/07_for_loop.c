/* Nested for loops with fused compares. Multiple induction variables
   live across multiple BRCs. */
int main(void) {
    int s;
    int i;
    int j;
    s = 0;
    for (i = 0; i < 5; i = i + 1) {
        for (j = 0; j < 5; j = j + 1) {
            if (i == j) {
                s = s + 1;
            } else if (i < j) {
                s = s + 2;
            }
        }
    }
    return s - 15;
}
