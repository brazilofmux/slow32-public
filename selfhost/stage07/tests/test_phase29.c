/* test_phase29.c -- automatic local designated initializers */

struct pair {
    int x;
    int y;
};

struct outer {
    struct pair p;
    int z;
};

struct local_record {
    int id;
    struct {
        int left;
        int right;
    };
    int nums[4];
    char tag[5];
    char *name;
};

int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

void test_local_arrays(void) {
    int sparse[6] = { [3] = 30, [1] = 10, 20 };
    int inferred[] = { [2] = 22, 33 };
    char letters[4] = { [2] = 'z' };
    struct pair slots[3] = {
        [2].y = 9,
        [0] = { .x = 1, .y = 2 },
        [2].x = 8
    };

    check(sparse[0] == 0, 1);
    check(sparse[1] == 10, 2);
    check(sparse[2] == 20, 3);
    check(sparse[3] == 30, 4);
    check(sparse[4] == 0, 5);

    check(sizeof(inferred) == 16, 6);
    check(inferred[0] == 0, 7);
    check(inferred[2] == 22, 8);
    check(inferred[3] == 33, 9);

    check(letters[0] == 0, 10);
    check(letters[2] == 'z', 11);
    check(letters[3] == 0, 12);

    check(slots[0].x == 1, 13);
    check(slots[0].y == 2, 14);
    check(slots[1].x == 0, 15);
    check(slots[2].x == 8, 16);
    check(slots[2].y == 9, 17);
}

void test_local_structs(void) {
    struct pair p = { .y = 7, .x = 6 };
    struct outer o = { .p.y = 5, .z = 6, .p.x = 4 };
    struct local_record rec = {
        .right = 4,
        .nums[2] = 12,
        .left = 3,
        .tag = "abc",
        .name = "xy",
        .id = 1
    };

    check(p.x == 6, 18);
    check(p.y == 7, 19);

    check(o.p.x == 4, 20);
    check(o.p.y == 5, 21);
    check(o.z == 6, 22);

    check(rec.id == 1, 23);
    check(rec.left == 3, 24);
    check(rec.right == 4, 25);
    check(rec.nums[0] == 0, 26);
    check(rec.nums[2] == 12, 27);
    check(rec.tag[0] == 'a', 28);
    check(rec.tag[1] == 'b', 29);
    check(rec.tag[3] == 0, 30);
    check(rec.tag[4] == 0, 31);
    check(rec.name[0] == 'x', 32);
    check(rec.name[1] == 'y', 33);
}

int main(void) {
    test_local_arrays();
    test_local_structs();
    return failed;
}
