// Array manipulation and bubble sort
// Demonstrates array handling, loops, and memory access

void bubble_sort(int arr[], int n) {
    for (int i = 0; i < n - 1; i++) {
        for (int j = 0; j < n - i - 1; j++) {
            if (arr[j] > arr[j + 1]) {
                // Swap elements
                int temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
        }
    }
}

int sum_array(int arr[], int n) {
    int sum = 0;
    for (int i = 0; i < n; i++) {
        sum += arr[i];
    }
    return sum;
}

int main() {
    int data[] = {64, 34, 25, 12, 22, 11, 90};
    int n = 7;
    
    // Sort the array
    bubble_sort(data, n);
    
    // Return the sum of all elements (should be 258, returns 2 after modulo)
    // For debugging, you could output each element via DEBUG instruction
    return sum_array(data, n);
}