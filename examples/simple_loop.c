// Simple loop to test PHI handling
int sum_to_n(int n) {
    int sum = 0;
    int i = 0;
    while (i < n) {
        sum = sum + i;
        i = i + 1;
    }
    return sum;
}

int main() {
    return sum_to_n(5);  // Should return 0+1+2+3+4 = 10
}