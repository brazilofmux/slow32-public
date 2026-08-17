#include <stdint.h>
#include "include/tube.h"

int tube_init(void) {
    return -1;
}

void tube_cleanup(void) {
}

int tube_open(uint32_t mode) {
    (void)mode;
    return -1;
}

int tube_close(void) {
    return -1;
}

int tube_present(const uint32_t *list, uint32_t words, uint32_t generation) {
    (void)list;
    (void)words;
    (void)generation;
    return -1;
}

uint32_t tube_info(void) {
    return 0;
}

uint32_t tube_status(void) {
    return 0;
}

int tube_keys(void *buf, uint32_t nbytes) {
    (void)buf;
    (void)nbytes;
    return 0;
}
