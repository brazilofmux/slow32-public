/* Test: function pointer in struct, indirect call */

int write(int fd, char *buf, int len);

typedef void (*handler_fn)();

struct inst {
    handler_fn handler;
    int val;
};

static void op_hello(struct inst *i) {
    write(1, "hello\n", 6);
}

static void op_world(struct inst *i) {
    write(1, "world\n", 6);
}

int main(int argc, char **argv) {
    struct inst a;
    struct inst b;

    a.handler = op_hello;
    a.val = 1;
    b.handler = op_world;
    b.val = 2;

    a.handler(&a);
    b.handler(&b);
    return 0;
}
