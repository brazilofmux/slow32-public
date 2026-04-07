/* Test local arrays, indexing, pointer arithmetic */

int sum_array(int *arr, int n) {
    int total;
    int i;
    total = 0;
    i = 0;
    while (i < n) {
        total = total + arr[i];
        i = i + 1;
    }
    return total;
}

int main(int argc, char **argv) {
    int nums[5];
    nums[0] = 10;
    nums[1] = 8;
    nums[2] = 12;
    nums[3] = 6;
    nums[4] = 6;
    return sum_array(nums, 5);  /* 42 */
}
