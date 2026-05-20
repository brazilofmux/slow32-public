/* Test global variables */

int counter;
int init_val = 42;

int get_counter() { return counter; }

void bump() {
    counter = counter + 1;
}

int main() {
    int i;
    if (init_val != 42) return 1;
    if (counter != 0) return 2;
    bump();
    bump();
    bump();
    if (get_counter() != 3) return 3;
    counter = 100;
    if (get_counter() != 100) return 4;
    return 0;
}
