#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "include/tube.h"

static uint32_t tube_base_opcode = 0;
static int tube_initialized = 0;

int tube_init(void) {
    volatile unsigned char *data_buffer;
    const char name[] = "tube";
    unsigned int name_len = 5;
    unsigned int i;
    int result;
    uint32_t svc_result = 0;

    if (tube_initialized) {
        return 0;
    }

    data_buffer = S32_MMIO_DATA_BUFFER;
    for (i = 0; i < name_len; i++) {
        data_buffer[i] = (unsigned char)name[i];
    }

    result = s32_mmio_request(0xF0, name_len, 0, 0);
    if (result == (int)S32_MMIO_STATUS_ERR) {
        return -1;
    }

    memcpy(&svc_result, (const void *)data_buffer, 4);
    if (svc_result != 0) {
        return -1;
    }

    memcpy(&tube_base_opcode, (const void *)(data_buffer + 4), 4);
    tube_initialized = 1;
    return 0;
}

void tube_cleanup(void) {
    volatile unsigned char *data_buffer;
    const char name[] = "tube";
    unsigned int name_len = 5;
    unsigned int i;

    if (!tube_initialized) {
        return;
    }

    data_buffer = S32_MMIO_DATA_BUFFER;
    for (i = 0; i < name_len; i++) {
        data_buffer[i] = (unsigned char)name[i];
    }
    s32_mmio_request(0xF1, name_len, 0, 0);
    tube_initialized = 0;
    tube_base_opcode = 0;
}

int tube_open(uint32_t mode) {
    int r;
    if (!tube_initialized) {
        return -1;
    }
    r = s32_mmio_request(tube_base_opcode + 1, 0, 0, mode);
    return (r == (int)S32_MMIO_STATUS_ERR) ? -1 : 0;
}

int tube_close(void) {
    int r;
    if (!tube_initialized) {
        return -1;
    }
    r = s32_mmio_request(tube_base_opcode + 2, 0, 0, 0);
    return (r == (int)S32_MMIO_STATUS_ERR) ? -1 : 0;
}

int tube_present(const uint32_t *list, uint32_t words, uint32_t generation) {
    int r;
    if (!tube_initialized || !list) {
        return -1;
    }
    r = s32_mmio_request(tube_base_opcode + 3, words, generation,
                         (unsigned int)(uintptr_t)list);
    return (r == (int)S32_MMIO_STATUS_ERR) ? -1 : 0;
}

uint32_t tube_info(void) {
    int r;
    if (!tube_initialized) {
        return 0;
    }
    r = s32_mmio_request(tube_base_opcode + 0, 0, 0, 0);
    if (r == (int)S32_MMIO_STATUS_ERR) {
        return 0;
    }
    return (uint32_t)r;
}

uint32_t tube_status(void) {
    int r;
    if (!tube_initialized) {
        return 0;
    }
    r = s32_mmio_request(tube_base_opcode + 4, 0, 0, 0);
    if (r == (int)S32_MMIO_STATUS_ERR) {
        return 0;
    }
    return (uint32_t)r;
}

int tube_keys(void *buf, uint32_t nbytes) {
    int r;
    if (!tube_initialized) {
        return -1;
    }
    r = s32_mmio_request(tube_base_opcode + 5, nbytes, 0, 0);
    if (r == (int)S32_MMIO_STATUS_ERR) {
        return -1;
    }
    if (buf && r > 0) {
        memcpy(buf, (const void *)S32_MMIO_DATA_BUFFER, (unsigned)r * 4u);
    }
    return r;
}
