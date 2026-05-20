/* While loop with fused compare-branch in the header. The induction
   variable's live range crosses the fusion. */
int sum_to(int n) {
    int s;
    int i;
    s = 0;
    i = 0;
    while (i <= n) {
        s = s + i;
        i = i + 1;
    }
    return s;
}

int main(void) {
    return sum_to(10) - 55;
}
