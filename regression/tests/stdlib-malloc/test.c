#include <stdlib.h>
#include <stdio.h>
#include <string.h>

int main() {
    printf("Starting Malloc Test...\n");

    // 1. Basic Allocation
    char *p1 = malloc(100);
    if (!p1) { printf("FAIL: malloc(100)\n"); return 1; }
    memset(p1, 'A', 100);
    printf("Allocated p1\n");

    char *p2 = malloc(200);
    if (!p2) { printf("FAIL: malloc(200)\n"); return 1; }
    memset(p2, 'B', 200);
    printf("Allocated p2\n");
    
    char *p3 = malloc(300);
    if (!p3) { printf("FAIL: malloc(300)\n"); return 1; }
    memset(p3, 'C', 300);
    printf("Allocated p3\n");

    // 2. Free Middle
    printf("Freeing p2\n");
    free(p2);
    
    // 3. Alloc that fits in middle
    char *p4 = malloc(150);
    if (!p4) { printf("FAIL: malloc(150)\n"); return 1; }
    printf("Allocated p4\n");
    
    if (p4 == p2) {
        printf("PASS: Reused freed block\n");
    } else {
        printf("INFO: Addr mismatch p2=%p p4=%p\n", p2, p4);
    }
    
    // 4. Coalescing Test
    free(p1);
    free(p4);
    free(p3);
    
    printf("Freed all. Allocating 500\n");
    
    char *big = malloc(500);
    if (!big) { 
        printf("FAIL: malloc(500)\n"); 
    } else {
        if (big == p1) {
             printf("PASS: Coalesced\n");
        } else {
             printf("INFO: Not coalesced at start? big=%p p1=%p\n", big, p1);
        }
        free(big);
    }
    
    // 5. Calloc
    int *arr = calloc(10, sizeof(int));
    if (!arr) { printf("FAIL: calloc\n"); return 1; }
    int zero = 1;
    for(int i=0; i<10; i++) {
        if (arr[i] != 0) zero = 0;
    }
    if(zero) printf("PASS: calloc zeroed\n");
    else printf("FAIL: calloc not zeroed\n");
    free(arr);

    // 6. Realloc
    char *r = malloc(10);
    strcpy(r, "test");
    r = realloc(r, 20);
    if (strcmp(r, "test") == 0) printf("PASS: realloc preserved\n");
    else printf("FAIL: realloc corrupted\n");
    free(r);

    return 0;
}
