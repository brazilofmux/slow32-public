#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "mmio_ring.h"

// Cached environment data
static char *envp_captured_blob = NULL;
static char **envp_captured_envp = NULL;
static char *envp_empty_environ[] = { NULL };
static int envp_captured_envc = 0;
static int envp_initialized = 0;  // 0 = not initialized, 1 = initialized

static void reset_cached_envp(void) {
    if (envp_captured_blob) {
        free(envp_captured_blob);
        envp_captured_blob = NULL;
    }
    if (envp_captured_envp) {
        free(envp_captured_envp);
        envp_captured_envp = NULL;
    }
    envp_captured_envc = 0;
    envp_initialized = 0;
}

static int allocate_envp_vector(uint32_t envc) {
    size_t count = (size_t)envc + 1u;
    envp_captured_envp = (char **)malloc(count * sizeof(char *));
    if (!envp_captured_envp) {
        return -1;
    }
    envp_captured_envp[count - 1u] = NULL;
    envp_captured_envc = (int)envc;
    return 0;
}

// Fetch the entire environment from host
static int fetch_envp_from_host(void) {
    if (envp_initialized) {
        return 0;  // Already fetched
    }

    reset_cached_envp();

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;
    s32_mmio_envp_info_t info = {0};

    unsigned int status = (unsigned int)s32_mmio_request(
        S32_MMIO_OP_ENVP_INFO,
        (unsigned int)sizeof(info),
        0u,
        0u);

    if (status != S32_MMIO_STATUS_OK) {
        envp_initialized = 1;  // Mark as initialized even on failure
        return -1;
    }

    memcpy(&info, (const void *)data_buffer, sizeof(info));

    if (info.total_bytes > S32_MMIO_ENVP_MAX_BYTES) {
        envp_initialized = 1;
        return -1;
    }

    if (info.envc > 0 && info.total_bytes == 0) {
        envp_initialized = 1;
        return -1;
    }

    if (info.envc == 0) {
        if (allocate_envp_vector(0) != 0) {
            envp_initialized = 1;
            return -1;
        }
        envp_initialized = 1;
        return 0;
    }

    if (info.total_bytes > 0) {
        envp_captured_blob = (char *)malloc(info.total_bytes);
        if (!envp_captured_blob) {
            envp_initialized = 1;
            return -1;
        }

        uint32_t copied = 0;
        while (copied < info.total_bytes) {
            uint32_t chunk = info.total_bytes - copied;
            if (chunk > S32_MMIO_DATA_CAPACITY) {
                chunk = S32_MMIO_DATA_CAPACITY;
            }

            status = (unsigned int)s32_mmio_request(
                S32_MMIO_OP_ENVP_DATA,
                chunk,
                0u,
                copied);
            if (status != S32_MMIO_STATUS_OK) {
                reset_cached_envp();
                envp_initialized = 1;
                return -1;
            }

            memcpy(envp_captured_blob + copied, (const void *)data_buffer, chunk);
            copied += chunk;
        }
    }

    if (allocate_envp_vector(info.envc) != 0) {
        reset_cached_envp();
        envp_initialized = 1;
        return -1;
    }

    // Parse the blob into envp[] pointers
    size_t offset = 0;
    for (uint32_t i = 0; i < info.envc; ++i) {
        if (envp_captured_blob && offset < info.total_bytes) {
            envp_captured_envp[i] = envp_captured_blob + offset;
            while (offset < info.total_bytes && envp_captured_blob[offset] != '\0') {
                offset++;
            }
            if (offset < info.total_bytes) {
                offset++;
            }
        } else {
            envp_captured_envp[i] = "";
        }
    }

    envp_initialized = 1;
    return 0;
}

// Look up an environment variable using the direct GETENV opcode
// This avoids fetching the entire environment if you only need one variable
char *getenv(const char *name) {
    if (!name || !*name) {
        return NULL;
    }

    // Check for invalid name (contains '=')
    for (const char *p = name; *p; ++p) {
        if (*p == '=') {
            return NULL;
        }
    }

    size_t name_len = strlen(name);
    if (name_len == 0 || name_len >= S32_MMIO_DATA_CAPACITY) {
        return NULL;
    }

    volatile unsigned char *data_buffer = S32_MMIO_DATA_BUFFER;

    // Copy name to data buffer (include NUL terminator)
    memcpy((void *)data_buffer, name, name_len + 1);

    // Request: length = name length + 1 (for NUL), status = 0
    // Response: status = value length (0 if not found), value in data buffer
    unsigned int status = (unsigned int)s32_mmio_request(
        S32_MMIO_OP_GETENV,
        (unsigned int)(name_len + 1),
        0u,
        0u);

    if (status == S32_MMIO_STATUS_ERR) {
        return NULL;  // Not found or error
    }

    // Value is in the data buffer, length is in status
    // We need to return a pointer that remains valid
    // Use a static buffer for simplicity (not thread-safe, but neither is getenv)
    static char *getenv_buffer = NULL;
    static size_t getenv_buffer_size = 0;

    size_t value_len = status;
    if (value_len >= getenv_buffer_size) {
        size_t new_size = value_len + 1;
        char *new_buf = (char *)realloc(getenv_buffer, new_size);
        if (!new_buf) {
            return NULL;
        }
        getenv_buffer = new_buf;
        getenv_buffer_size = new_size;
    }

    memcpy(getenv_buffer, (const void *)data_buffer, value_len);
    getenv_buffer[value_len] = '\0';

    return getenv_buffer;
}

// Get the full environment array (lazily fetched)
char **__slow32_get_environ(void) {
    if (!envp_initialized) {
        fetch_envp_from_host();
    }
    return envp_captured_envp ? envp_captured_envp : envp_empty_environ;
}

// Release cached environment data
void __slow32_release_envp(void) {
    reset_cached_envp();
}
