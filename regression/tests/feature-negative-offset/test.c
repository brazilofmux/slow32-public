#include <stdio.h>
#include <string.h>

struct point {
    int x;
    int y;
    int z;
};

struct nested {
    int a;
    struct point p;
    int b;
};

union overlay {
    int i;
    unsigned char bytes[4];
};

int main() {
    /* Struct field access — tests positive offsets from base */
    struct point pt;
    pt.x = 10;
    pt.y = 20;
    pt.z = 30;
    printf("%s: struct fields\n",
           pt.x == 10 && pt.y == 20 && pt.z == 30 ? "PASS" : "FAIL");

    /* Nested struct — deeper offsets */
    struct nested n;
    n.a = 1;
    n.p.x = 2;
    n.p.y = 3;
    n.p.z = 4;
    n.b = 5;
    printf("%s: nested struct\n",
           n.a == 1 && n.p.x == 2 && n.p.y == 3 && n.p.z == 4 && n.b == 5
           ? "PASS" : "FAIL");

    /* Pointer to struct — access via pointer arithmetic */
    struct point *pp = &pt;
    pp->x = 100;
    pp->y = 200;
    pp->z = 300;
    printf("%s: struct via pointer\n",
           pt.x == 100 && pt.y == 200 && pt.z == 300 ? "PASS" : "FAIL");

    /* Pointer arithmetic with negative indices */
    int arr[10];
    for (int i = 0; i < 10; i++) arr[i] = i * 10;
    int *mid = &arr[5];
    printf("%s: negative index -1\n", mid[-1] == 40 ? "PASS" : "FAIL");
    printf("%s: negative index -5\n", mid[-5] == 0 ? "PASS" : "FAIL");
    printf("%s: positive index +4\n", mid[4] == 90 ? "PASS" : "FAIL");

    /* Stack-allocated arrays (negative fp offsets) */
    int stack_arr[8];
    for (int i = 0; i < 8; i++) stack_arr[i] = i + 100;
    int sum = 0;
    for (int i = 0; i < 8; i++) sum += stack_arr[i];
    /* sum = 100+101+...+107 = 828 */
    printf("%s: stack array\n", sum == 828 ? "PASS" : "FAIL");

    /* Large stack frame (many locals — forces negative offsets) */
    int a = 1, b = 2, c = 3, d = 4, e = 5;
    int f = 6, g = 7, h = 8;
    int large_sum = a + b + c + d + e + f + g + h;
    printf("%s: many locals\n", large_sum == 36 ? "PASS" : "FAIL");

    /* Union overlay */
    union overlay u;
    u.i = 0;
    u.bytes[0] = 0x78;
    u.bytes[1] = 0x56;
    u.bytes[2] = 0x34;
    u.bytes[3] = 0x12;
    printf("%s: union overlay\n", u.i == 0x12345678 ? "PASS" : "FAIL");

    /* Array of structs */
    struct point pts[3];
    pts[0].x = 1; pts[0].y = 2; pts[0].z = 3;
    pts[1].x = 4; pts[1].y = 5; pts[1].z = 6;
    pts[2].x = 7; pts[2].y = 8; pts[2].z = 9;
    sum = 0;
    for (int i = 0; i < 3; i++) {
        sum += pts[i].x + pts[i].y + pts[i].z;
    }
    printf("%s: array of structs\n", sum == 45 ? "PASS" : "FAIL");

    /* Pointer walking backward through array */
    int walk[5] = {10, 20, 30, 40, 50};
    int *p = &walk[4];
    sum = 0;
    while (p >= walk) {
        sum += *p;
        p--;
    }
    printf("%s: backward walk\n", sum == 150 ? "PASS" : "FAIL");

    /* Struct with char fields (non-word-aligned offsets) */
    struct packed {
        char c1;
        int val;
        char c2;
    };
    struct packed pk;
    pk.c1 = 'A';
    pk.val = 12345;
    pk.c2 = 'Z';
    printf("%s: char+int struct\n",
           pk.c1 == 'A' && pk.val == 12345 && pk.c2 == 'Z' ? "PASS" : "FAIL");

    return 0;
}
