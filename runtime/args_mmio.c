#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mmio_ring.h"
#include "slow32_args.h"

static char **captured_argv = NULL;
static char *captured_blob = NULL;
static int captured_argc = 0;

static void reset_cached_args(void) {
    if (captured_blob) {
        free(captured_blob);
        captured_blob = NULL;
    }
    if (captured_argv) {
        free(captured_argv);
        captured_argv = NULL;
    }
    captured_argc = 0;
}

static int allocate_argv_vector(uint32_t argc) {
    size_t count = (size_t)argc + 1u;
    captured_argv = (char **)malloc(count * sizeof(char *));
    if (!captured_argv) {
        return -1;
    }
    captured_argv[count - 1u] = NULL;
    captured_argc = (int)argc;
    return 0;
}

int __slow32_fetch_args(int *argc_out, char ***argv_out) {
    if (!argc_out || !argv_out) {
        return -1;
    }

    reset_cached_args();

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    s32_mmio_args_info_t info = {0};

    unsigned int status = (unsigned int)s32_mmio_request(
        S32_MMIO_OP_ARGS_INFO,
        (unsigned int)sizeof(info),
        0u,
        0u);

    if (status != S32_MMIO_STATUS_OK) {
        *argc_out = 0;
        *argv_out = NULL;
        return -1;
    }

    memcpy(&info, (const void *)data_buffer, sizeof(info));

    if (info.total_bytes > S32_MMIO_ARGS_MAX_BYTES) {
        *argc_out = 0;
        *argv_out = NULL;
        return -1;
    }

    if (info.argc > 0 && info.total_bytes == 0) {
        *argc_out = 0;
        *argv_out = NULL;
        return -1;
    }

    if (info.argc == 0) {
        if (allocate_argv_vector(0) != 0) {
            *argc_out = 0;
            *argv_out = NULL;
            return -1;
        }
        *argc_out = 0;
        *argv_out = captured_argv;
        return 0;
    }

    if (info.total_bytes > 0) {
        captured_blob = (char *)malloc(info.total_bytes);
        if (!captured_blob) {
            *argc_out = 0;
            *argv_out = NULL;
            return -1;
        }

        uint32_t copied = 0;
        while (copied < info.total_bytes) {
            uint32_t chunk = info.total_bytes - copied;
            if (chunk > S32_MMIO_DATA_CAPACITY) {
                chunk = S32_MMIO_DATA_CAPACITY;
            }

            status = (unsigned int)s32_mmio_request(
                S32_MMIO_OP_ARGS_DATA,
                chunk,
                0u,
                copied);
            if (status != S32_MMIO_STATUS_OK) {
                reset_cached_args();
                *argc_out = 0;
                *argv_out = NULL;
                return -1;
            }

            memcpy(captured_blob + copied, (const void *)data_buffer, chunk);
            copied += chunk;
        }
    }

    if (allocate_argv_vector(info.argc) != 0) {
        reset_cached_args();
        *argc_out = 0;
        *argv_out = NULL;
        return -1;
    }

    size_t offset = 0;
    for (uint32_t i = 0; i < info.argc; ++i) {
        if (captured_blob && offset < info.total_bytes) {
            captured_argv[i] = captured_blob + offset;
            while (offset < info.total_bytes && captured_blob[offset] != '\0') {
                offset++;
            }
            if (offset < info.total_bytes) {
                offset++;
            }
        } else {
            captured_argv[i] = "";
        }
    }

    *argc_out = captured_argc;
    *argv_out = captured_argv;
    return 0;
}

void __slow32_release_args(void) {
    reset_cached_args();
}
