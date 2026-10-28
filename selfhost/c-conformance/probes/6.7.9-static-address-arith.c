// 6.7.9p4/6.6p9  address constant plus an integer offset: arr + 1, "lit" + 1
#include <stdio.h>
static int arr[5] = { 1, 2, 3, 4, 5 };
static int *q = arr + 1;
static const char *msg = "hello" + 1;
int main(void) { printf("%d %s\n", *q, msg); return 0; }
