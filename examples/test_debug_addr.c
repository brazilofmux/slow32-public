// Test to debug address calculation
void putint(int n);
void putchar(int c);

const char test_str[] = "ABC";

int main() {
    // Output the address of the string
    putint((int)test_str);
    putchar('\n');
    
    // Output each character
    putchar(test_str[0]);
    putchar(test_str[1]);  
    putchar(test_str[2]);
    putchar('\n');
    
    return 0;
}