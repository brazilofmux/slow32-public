// Large program to test memory efficiency
int data[10000] = {0};  // 40KB of data

int main() {
    int sum = 0;
    for (int i = 0; i < 10000; i++) {
        sum += i;
        data[i] = i;
    }
    return sum & 0xFF;
}
