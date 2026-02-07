#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int cmp_int_asc(const void *a, const void *b) {
    int ia = *(const int *)a;
    int ib = *(const int *)b;
    if (ia < ib) return -1;
    if (ia > ib) return 1;
    return 0;
}

int cmp_int_desc(const void *a, const void *b) {
    return cmp_int_asc(b, a);
}

int cmp_str(const void *a, const void *b) {
    const char *sa = *(const char *const *)a;
    const char *sb = *(const char *const *)b;
    return strcmp(sa, sb);
}

int main() {
    /* qsort ints ascending */
    int arr1[] = {5, 3, 8, 1, 9, 2, 7, 4, 6, 0};
    qsort(arr1, 10, sizeof(int), cmp_int_asc);
    int ok = 1;
    for (int i = 0; i < 10; i++) {
        if (arr1[i] != i) ok = 0;
    }
    printf("%s: qsort int asc\n", ok ? "PASS" : "FAIL");

    /* qsort ints descending */
    int arr2[] = {5, 3, 8, 1, 9, 2, 7, 4, 6, 0};
    qsort(arr2, 10, sizeof(int), cmp_int_desc);
    ok = 1;
    for (int i = 0; i < 10; i++) {
        if (arr2[i] != 9 - i) ok = 0;
    }
    printf("%s: qsort int desc\n", ok ? "PASS" : "FAIL");

    /* qsort strings */
    const char *strs[] = {"banana", "apple", "cherry", "date"};
    qsort(strs, 4, sizeof(const char *), cmp_str);
    printf("%s: qsort strings\n",
           strcmp(strs[0], "apple") == 0 &&
           strcmp(strs[1], "banana") == 0 &&
           strcmp(strs[2], "cherry") == 0 &&
           strcmp(strs[3], "date") == 0 ? "PASS" : "FAIL");

    /* qsort already sorted */
    int arr3[] = {1, 2, 3, 4, 5};
    qsort(arr3, 5, sizeof(int), cmp_int_asc);
    ok = 1;
    for (int i = 0; i < 5; i++) {
        if (arr3[i] != i + 1) ok = 0;
    }
    printf("%s: qsort sorted\n", ok ? "PASS" : "FAIL");

    /* qsort single element */
    int arr4[] = {42};
    qsort(arr4, 1, sizeof(int), cmp_int_asc);
    printf("%s: qsort single\n", arr4[0] == 42 ? "PASS" : "FAIL");

    /* qsort zero elements */
    int arr5[] = {99};
    qsort(arr5, 0, sizeof(int), cmp_int_asc);
    printf("%s: qsort zero\n", arr5[0] == 99 ? "PASS" : "FAIL");

    /* bsearch found */
    int sorted[] = {10, 20, 30, 40, 50};
    int key = 30;
    int *found = bsearch(&key, sorted, 5, sizeof(int), cmp_int_asc);
    printf("%s: bsearch found\n", found != NULL && *found == 30 ? "PASS" : "FAIL");

    /* bsearch first element */
    key = 10;
    found = bsearch(&key, sorted, 5, sizeof(int), cmp_int_asc);
    printf("%s: bsearch first\n", found != NULL && *found == 10 ? "PASS" : "FAIL");

    /* bsearch last element */
    key = 50;
    found = bsearch(&key, sorted, 5, sizeof(int), cmp_int_asc);
    printf("%s: bsearch last\n", found != NULL && *found == 50 ? "PASS" : "FAIL");

    /* bsearch not found */
    key = 25;
    found = bsearch(&key, sorted, 5, sizeof(int), cmp_int_asc);
    printf("%s: bsearch not found\n", found == NULL ? "PASS" : "FAIL");

    return 0;
}
