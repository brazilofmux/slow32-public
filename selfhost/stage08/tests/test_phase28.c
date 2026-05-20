/* test_phase28.c -- designated initializers */

struct slot {
    int op;
    int val;
};

struct record {
    int id;
    struct {
        int left;
        int right;
    };
    int nums[4];
    char tag[5];
    char *name;
};

int sparse[7] = { [2] = 22, [5] = 55, 66 };
int inferred[] = { [3] = 7, 8 };

struct slot slots[4] = {
    [2].op = 7,
    [2].val = 8,
    [0] = { 1, 2 },
    [3] = { .val = 9, .op = 10 }
};

struct record rec = {
    .right = 4,
    .nums[2] = 12,
    .left = 3,
    .tag = "abc",
    .name = "xy",
    .id = 1
};

int failed;

void check(int cond, int code) {
    if (!cond && failed == 0) failed = code;
}

void test_globals(void) {
    check(sparse[0] == 0, 1);
    check(sparse[2] == 22, 2);
    check(sparse[5] == 55, 3);
    check(sparse[6] == 66, 4);

    check(sizeof(inferred) == 20, 5);
    check(inferred[0] == 0, 6);
    check(inferred[3] == 7, 7);
    check(inferred[4] == 8, 8);

    check(slots[0].op == 1, 9);
    check(slots[0].val == 2, 10);
    check(slots[1].op == 0, 11);
    check(slots[2].op == 7, 12);
    check(slots[2].val == 8, 13);
    check(slots[3].op == 10, 14);
    check(slots[3].val == 9, 15);

    check(rec.id == 1, 16);
    check(rec.left == 3, 17);
    check(rec.right == 4, 18);
    check(rec.nums[0] == 0, 19);
    check(rec.nums[2] == 12, 20);
    check(rec.tag[0] == 'a', 21);
    check(rec.tag[3] == 0, 22);
    check(rec.name[0] == 'x', 23);
    check(rec.name[1] == 'y', 24);
}

void test_static_locals(void) {
    static int local_sparse[5] = { [3] = 30, [1] = 10, 40 };
    static struct slot local_slots[3] = {
        [1].val = 5,
        [1].op = 4,
        [2] = { .val = 7 }
    };

    check(local_sparse[0] == 0, 25);
    check(local_sparse[1] == 10, 26);
    check(local_sparse[3] == 30, 27);
    check(local_sparse[2] == 40, 28);
    check(local_sparse[4] == 0, 29);

    check(local_slots[0].op == 0, 30);
    check(local_slots[1].op == 4, 31);
    check(local_slots[1].val == 5, 32);
    check(local_slots[2].op == 0, 33);
    check(local_slots[2].val == 7, 34);
}

int main(void) {
    test_globals();
    test_static_locals();
    return failed;
}
