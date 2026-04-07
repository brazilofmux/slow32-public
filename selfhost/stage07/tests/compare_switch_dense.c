int dense_switch(int x) {
    switch (x) {
    case 0: return 7;
    case 1: return 11;
    case 2: return 13;
    case 3: return 17;
    case 4: return 19;
    case 5: return 23;
    case 6: return 29;
    case 7: return 31;
    case 8: return 37;
    case 9: return 41;
    case 10: return 43;
    case 11: return 47;
    case 12: return 53;
    case 13: return 59;
    case 14: return 61;
    case 15: return 67;
    default: return -1;
    }
}

int main(void) {
    int i;
    int sum;
    sum = 0;
    for (i = 0; i < 20; i = i + 1) {
        sum = sum + dense_switch(i);
    }
    return sum;
}
