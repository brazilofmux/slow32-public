/* Designated initializers for structs -- mixed .field, [idx].field,
 * nested braced inits, and out-of-order designators followed by
 * sequential continuations.  Static and automatic storage. */

struct entry {
    int key;
    int val;
};

struct heap {
    int          count;
    struct entry items[5];
    int          tail;
};

static struct heap g = {
    .tail            = 99,
    .items[1].val    = 11,
    .items[1].key    = 10,
    .items[3]        = { .val = 31, .key = 30 },
    .count           = 4,
    .items[4].key    = 40
};

static int score(struct heap *h) {
    int s = h->count * 1000 + h->tail;
    int i;
    for (i = 0; i < 5; i++) {
        s = s + h->items[i].key * 7 + h->items[i].val * 3;
    }
    return s;
}

int main(void) {
    struct heap local = {
        .items[2]        = { 12, 13 },
        .count           = 1,
        .tail            = 77,
        .items[0].key    = 1
    };
    int acc = 0;
    acc = acc * 17 + score(&g);
    acc = acc * 17 + score(&local);
    return acc & 0xff;
}
