// Test program for ring buffer MMIO
typedef unsigned int size_t;
#define NULL ((void*)0)

// Function prototypes from mmio_stdio.c
void putchar(int c);
int getchar(void);
void puts(const char *s);
void exit(int status);
void* malloc(size_t size);

// Simple string functions
int strlen(const char *s) {
    int len = 0;
    while (s[len]) len++;
    return len;
}

void strcpy(char *dest, const char *src) {
    while (*src) {
        *dest++ = *src++;
    }
    *dest = '\0';
}

// Print a number
void print_num(int n) {
    if (n < 0) {
        putchar('-');
        n = -n;
    }
    
    if (n >= 10) {
        print_num(n / 10);
    }
    
    putchar('0' + (n % 10));
}

int main() {
    // Test 1: Basic output
    puts("Ring Buffer MMIO Test");
    puts("====================");
    
    // Test 2: Character output
    const char *msg = "Testing putchar: ";
    for (int i = 0; msg[i]; i++) {
        putchar(msg[i]);
    }
    putchar('O');
    putchar('K');
    putchar('\n');
    
    // Test 3: Dynamic memory
    puts("\nTesting malloc...");
    char *buffer = (char*)malloc(100);
    if (buffer) {
        strcpy(buffer, "Dynamic memory works!");
        puts(buffer);
    } else {
        puts("malloc failed!");
    }
    
    // Test 4: Multiple allocations
    puts("\nAllocating more memory...");
    int *numbers = (int*)malloc(10 * sizeof(int));
    if (numbers) {
        for (int i = 0; i < 10; i++) {
            numbers[i] = i * i;
        }
        
        const char *prefix = "Square of ";
        for (int i = 0; i < 10; i++) {
            // Print "Square of N = N²"
            for (int j = 0; prefix[j]; j++) {
                putchar(prefix[j]);
            }
            print_num(i);
            putchar(' ');
            putchar('=');
            putchar(' ');
            print_num(numbers[i]);
            putchar('\n');
        }
    }
    
    // Test 5: Interactive input (commented out for automated testing)
    /*
    puts("\nEnter your name: ");
    char name[50];
    int i = 0;
    int ch;
    while ((ch = getchar()) != '\n' && i < 49) {
        name[i++] = ch;
    }
    name[i] = '\0';
    
    puts("Hello, ");
    puts(name);
    puts("!");
    */
    
    puts("\n*** All tests completed successfully! ***");
    
    return 0;
}