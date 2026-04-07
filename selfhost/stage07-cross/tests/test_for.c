int factorial(int n) {
    int result;
    int i;
    result = 1;
    i = 2;
    while (i <= n) {
        result = result * i;
        i = i + 1;
    }
    return result;
}

int main(int argc, char **argv) {
    return factorial(6) % 256;
}
