/* cc_flex_array.c -- C99 flexible array member layout and access */
#include <stddef.h>

struct packet {
    int len;
    unsigned char data[];
};

struct aligned_packet {
    char tag;
    long long words[];
};

static struct packet global_packet = { 11 };

static int fill_packet(struct packet *p) {
    int i;
    int sum;

    p->len = 5;
    i = 0;
    sum = 0;
    while (i < p->len) {
        p->data[i] = (unsigned char)(i * 3 + 1);
        sum = sum + p->data[i];
        i = i + 1;
    }
    return sum;
}

static int header_only(void) {
    struct packet p = { 7 };
    return p.len;
}

int main(void) {
    int fails;
    char storage[sizeof(struct packet) + 8];
    struct packet *p;

    fails = 0;
    _Static_assert(sizeof(struct packet) == 4, "packet size");
    _Static_assert(offsetof(struct packet, data) == 4, "data offset");
    _Static_assert(sizeof(struct aligned_packet) == 8, "aligned size");
    _Static_assert(offsetof(struct aligned_packet, words) == 8, "words offset");

    p = (struct packet *)storage;
    if (fill_packet(p) != 35) fails = fails + 1;
    if (p->len != 5) fails = fails + 2;
    if (p->data[3] != 10) fails = fails + 4;
    if (header_only() != 7) fails = fails + 8;
    if (global_packet.len != 11) fails = fails + 16;
    return fails == 0;
}
