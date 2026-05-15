/* C99 flexible array member -- the unsized trailing array contributes
 * zero to sizeof(struct), gets aligned correctly relative to the prior
 * members, and indexing through a pointer at a backing buffer must
 * land at offset(prior_members), not zero. */

struct packet {
    int           len;
    unsigned char data[];
};

struct aligned_packet {
    char tag;
    /* Trailing alignment of 4 means data[] starts at offset 4, not 1. */
    int  words[];
};

static int fill_packet(struct packet *p, int n, int seed) {
    int i;
    int sum;

    p->len = n;
    sum    = 0;
    for (i = 0; i < n; i++) {
        p->data[i] = (unsigned char)((i * 3 + seed) & 0xff);
        sum        = sum + (int)p->data[i];
    }
    return sum;
}

static int fill_aligned(struct aligned_packet *p, int n) {
    int i;
    int sum = 0;
    p->tag  = (char)n;
    for (i = 0; i < n; i++) {
        p->words[i] = i * i + (int)p->tag;
        sum         = sum + p->words[i];
    }
    return sum + (int)p->tag;
}

int main(void) {
    unsigned char buf1[ sizeof(struct packet)         + 32 ];
    unsigned char buf2[ sizeof(struct aligned_packet) + 32 ];
    struct packet         *p = (struct packet *)buf1;
    struct aligned_packet *q = (struct aligned_packet *)buf2;
    int acc = 0;

    acc = acc * 17 + fill_packet(p, 5, 1);
    acc = acc * 17 + fill_packet(p, 8, 7);
    acc = acc * 17 + (int)p->data[0] + (int)p->data[3] + (int)p->data[7];
    acc = acc * 17 + fill_aligned(q, 6);
    acc = acc * 17 + q->words[2] + q->words[5];

    /* Empty / header-only instances still have correct len. */
    {
        struct packet header_only = { 42 };
        acc = acc * 17 + header_only.len;
    }
    return acc & 0xff;
}
