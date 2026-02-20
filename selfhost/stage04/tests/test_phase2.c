/* Phase 2 test: strings, pointers, globals, arrays, for loops */

int write(int fd, char *buf, int len);

/* --- Global variables --- */
int g_val;
int g_arr[10];

/* --- String output helper --- */
int my_strlen(char *s) {
    int n;
    n = 0;
    while (*(s + n) != 0) {
        n = n + 1;
    }
    return n;
}

int puts(char *s) {
    int len;
    len = my_strlen(s);
    write(1, s, len);
    return 0;
}

/* --- Test: string literals --- */
int test_string(void) {
    char *s;
    s = "hello";
    if (*(s + 0) != 104) return 1;  /* 'h' */
    if (*(s + 1) != 101) return 2;  /* 'e' */
    if (*(s + 4) != 111) return 3;  /* 'o' */
    if (*(s + 5) != 0)   return 4;  /* null terminator */
    return 0;
}

/* --- Test: pointer dereference and address-of --- */
int test_pointer(void) {
    int x;
    int *p;
    x = 42;
    p = &x;
    if (*p != 42) return 1;
    *p = 99;
    if (x != 99) return 2;
    return 0;
}

/* --- Test: global variables --- */
int test_global(void) {
    g_val = 123;
    if (g_val != 123) return 1;
    g_val = g_val + 1;
    if (g_val != 124) return 2;
    return 0;
}

/* --- Test: array subscript --- */
int test_array(void) {
    int arr[5];
    int i;
    arr[0] = 10;
    arr[1] = 20;
    arr[2] = 30;
    arr[3] = 40;
    arr[4] = 50;
    if (arr[0] != 10) return 1;
    if (arr[4] != 50) return 2;
    /* Sum via pointer arithmetic */
    i = 0;
    g_val = 0;
    while (i < 5) {
        g_val = g_val + arr[i];
        i = i + 1;
    }
    if (g_val != 150) return 3;
    return 0;
}

/* --- Test: global array --- */
int test_global_array(void) {
    g_arr[0] = 100;
    g_arr[9] = 900;
    if (g_arr[0] != 100) return 1;
    if (g_arr[9] != 900) return 2;
    return 0;
}

/* --- Test: char pointer / string manipulation --- */
int test_char_ptr(void) {
    char buf[16];
    buf[0] = 65;  /* 'A' */
    buf[1] = 66;  /* 'B' */
    buf[2] = 67;  /* 'C' */
    buf[3] = 0;
    if (buf[0] != 65) return 1;
    if (buf[2] != 67) return 2;
    if (my_strlen(buf) != 3) return 3;
    return 0;
}

/* --- Test: for loop --- */
int test_for(void) {
    int sum;
    int i;
    sum = 0;
    for (i = 1; i <= 10; i = i + 1) {
        sum = sum + i;
    }
    if (sum != 55) return 1;
    return 0;
}

/* --- Test: sizeof --- */
int test_sizeof(void) {
    if (sizeof(int) != 4) return 1;
    if (sizeof(char) != 1) return 2;
    return 0;
}

/* --- Test: function taking and returning pointer --- */
int sum_array(int *arr, int n) {
    int s;
    int i;
    s = 0;
    i = 0;
    while (i < n) {
        s = s + arr[i];
        i = i + 1;
    }
    return s;
}

int test_ptr_param(void) {
    int data[4];
    data[0] = 1;
    data[1] = 2;
    data[2] = 3;
    data[3] = 4;
    if (sum_array(data, 4) != 10) return 1;
    return 0;
}

/* --- Main --- */
int main(void) {
    int rc;

    rc = test_string();
    if (rc) return rc;

    rc = test_pointer();
    if (rc) return rc + 10;

    rc = test_global();
    if (rc) return rc + 20;

    rc = test_array();
    if (rc) return rc + 30;

    rc = test_global_array();
    if (rc) return rc + 40;

    rc = test_char_ptr();
    if (rc) return rc + 50;

    rc = test_for();
    if (rc) return rc + 60;

    rc = test_sizeof();
    if (rc) return rc + 70;

    rc = test_ptr_param();
    if (rc) return rc + 80;

    puts("Phase 2: all tests passed\n");
    return 0;
}
