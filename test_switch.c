int test_switch(int x) {
    switch (x) {
        case 1:
            return 10;
        case 2:
            return 20;
        case 3:
            return 30;
        case 5:
            return 50;
        default:
            return -1;
    }
}

int main() {
    int result = test_switch(3);
    return result;
}