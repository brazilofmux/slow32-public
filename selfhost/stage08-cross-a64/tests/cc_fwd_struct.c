typedef struct node node_t;

struct node {
    int value;
    node_t *next;
};

int main(void) {
    node_t a;
    node_t b;
    a.value = 19;
    a.next = &b;
    b.value = 23;
    b.next = 0;
    return a.value + a.next->value;
}
