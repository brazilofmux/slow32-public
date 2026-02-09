#include <stdint.h>
#include <string.h>

#include "mmio_ring.h"
#include "include/term.h"

static uint32_t term_base_opcode = 0;
static int term_initialized = 0;

int term_init(void) {
    if (term_initialized) return 0;

    /* Write service name "term\0" into data buffer */
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    const char name[] = "term";
    unsigned int name_len = 5; /* including NUL */
    unsigned int i;
    for (i = 0; i < name_len; i++) {
        data_buffer[i] = (unsigned char)name[i];
    }

    /* SVC_REQUEST: opcode=0xF0, length=name_len, offset=0, status=0 */
    int result = s32_mmio_request(0xF0, name_len, 0, 0);
    if (result == (int)S32_MMIO_STATUS_ERR) {
        return -1;
    }

    /* Read response: [0]=result_code, [4]=base_opcode, [8]=count, [12]=version */
    uint32_t svc_result = 0;
    memcpy(&svc_result, (const void *)data_buffer, 4);

    if (svc_result != 0) {
        /* S32_SVC_OK == 0; anything else is an error */
        return -1;
    }

    memcpy(&term_base_opcode, (const void *)(data_buffer + 4), 4);
    term_initialized = 1;
    return 0;
}

void term_cleanup(void) {
    if (!term_initialized) return;

    /* Write service name into data buffer */
    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    const char name[] = "term";
    unsigned int name_len = 5;
    unsigned int i;
    for (i = 0; i < name_len; i++) {
        data_buffer[i] = (unsigned char)name[i];
    }

    /* SVC_RELEASE: opcode=0xF1 */
    s32_mmio_request(0xF1, name_len, 0, 0);
    term_initialized = 0;
}

void term_set_raw(int raw) {
    if (!term_initialized) return;
    s32_mmio_request(term_base_opcode + 0, 0, 0, (unsigned int)raw);
}

void term_get_size(int *rows, int *cols) {
    if (!term_initialized) {
        if (rows) *rows = 24;
        if (cols) *cols = 80;
        return;
    }
    s32_mmio_request(term_base_opcode + 1, 8, 0, 0);

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    uint32_t r = 0, c = 0;
    memcpy(&r, (const void *)data_buffer, 4);
    memcpy(&c, (const void *)(data_buffer + 4), 4);
    if (rows) *rows = (int)r;
    if (cols) *cols = (int)c;
}

void term_gotoxy(int row, int col) {
    if (!term_initialized) return;
    uint32_t packed = ((uint32_t)row << 16) | ((uint32_t)col & 0xFFFF);
    s32_mmio_request(term_base_opcode + 2, 0, 0, packed);
}

void term_clear(int mode) {
    if (!term_initialized) return;
    s32_mmio_request(term_base_opcode + 3, 0, 0, (unsigned int)mode);
}

void term_set_attr(int attr) {
    if (!term_initialized) return;
    s32_mmio_request(term_base_opcode + 4, 0, 0, (unsigned int)attr);
}

int term_getkey(void) {
    if (!term_initialized) return -1;
    int result = s32_mmio_request(term_base_opcode + 5, 0, 0, 0);
    if (result == (int)S32_MMIO_STATUS_EOF) return -1;
    return result;
}

int term_kbhit(void) {
    if (!term_initialized) return 0;
    return s32_mmio_request(term_base_opcode + 6, 0, 0, 0);
}

void term_set_color(int fg, int bg) {
    if (!term_initialized) return;
    uint32_t packed = ((uint32_t)(fg & 0xFF) << 8) | ((uint32_t)(bg & 0xFF));
    s32_mmio_request(term_base_opcode + 7, 0, 0, packed);
}
