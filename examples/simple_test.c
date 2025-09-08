// Simple test to verify basic functionality
char global_array[8] = {1, 2, 3, 4, 5, 6, 7, 8};

char access_array(int index) {
    return global_array[index];  // This should use proper byte scaling
}

int main() {
    char result = access_array(3);  // Should get value 4
    return (int)result;
}