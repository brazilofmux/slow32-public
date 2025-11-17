#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Input type table maps byte -> column in the state transition tables
static inline uint8_t classify_byte(uint8_t b) {
    switch (b) {
    case '\n':
        return 1;
    case '\r':
        return 2;
    case ',':
        return 3;
    case '"':
        return 4;
    default:
        return 0;
    }
}

// State transition tables (rows: states 0..12, cols: Any, LF, CR, ',', '"')
static const uint8_t s32_stt_loose[13][5] = {
    {  6, 10,  3,  8,  1 },  // 0 - F0
    {  7,  7,  7,  7,  2 },  // 1 - F2
    {  6, 11,  4,  8,  5 },  // 2 - Q2
    { 12, 10, 12, 12, 12 },  // 3 - CR0
    { 12, 11, 12, 12, 12 },  // 4 - CR1
    {  7,  7,  7,  7,  2 },  // 5 - Save Quote
    {  6, 11,  4,  8,  6 },  // 6 - Save Byte
    {  7,  7,  7,  7,  2 },  // 7 - Save Byte
    {  6, 11,  9,  8,  1 },  // 8 - Save Field
    { 12, 10, 12, 12, 12 },  // 9 - Save Field
    {  6, 10,  3,  8,  1 },  // 10 - Save Record
    {  6, 10,  3,  8,  1 },  // 11 - Save Field And Record
    { 12, 12, 12, 12, 12 },  // 12 - Error
};

static const uint8_t s32_stt_strict[13][5] = {
    {  6, 10,  3,  8,  1 },  // 0 - F0
    {  7,  7,  7,  7,  2 },  // 1 - F2
    { 12, 11,  4,  8,  5 },  // 2 - Q2
    { 12, 10, 12, 12, 12 },  // 3 - CR0
    { 12, 11, 12, 12, 12 },  // 4 - CR1
    {  7,  7,  7,  7,  2 },  // 5 - Save Quote
    {  6, 11,  4,  8, 12 },  // 6 - Save Byte
    {  7,  7,  7,  7,  2 },  // 7 - Save Byte
    {  6, 11,  9,  8,  1 },  // 8 - Save Field
    { 12, 10, 12, 12, 12 },  // 9 - Save Field
    {  6, 10,  3,  8,  1 },  // 10 - Save Record
    {  6, 10,  3,  8,  1 },  // 11 - Save Field And Record
    { 12, 12, 12, 12, 12 },  // 12 - Error
};

typedef struct {
    const char *filename;
    bool is_valid;
    int expected_fields;
    int line_number;
} file_result_t;

static file_result_t validate_file(const char *filename, bool strict_mode) {
    file_result_t result = {filename, false, 0, 0};

    FILE *f = fopen(filename, "rb");
    if (!f) {
        printf("Unable to open file: %s\n", filename);
        return result;
    }

    uint8_t *bytes = NULL;
    size_t size = 0;
    size_t capacity = 1024;
    bytes = (uint8_t *)malloc(capacity);
    if (!bytes) {
        printf("Unable to allocate buffer for file: %s\n", filename);
        fclose(f);
        return result;
    }

    uint8_t chunk[256];
    while (1) {
        size_t n = fread(chunk, 1u, sizeof(chunk), f);
        if (n == 0) {
            break;
        }
        if (size + n > capacity) {
            while (size + n > capacity) {
                capacity *= 2u;
            }
            uint8_t *new_bytes = (uint8_t *)realloc(bytes, capacity);
            if (!new_bytes) {
                printf("Unable to grow buffer for file: %s\n", filename);
                free(bytes);
                fclose(f);
                return result;
            }
            bytes = new_bytes;
        }
        memcpy(bytes + size, chunk, n);
        size += n;
        if (n < sizeof(chunk)) {
            break;  // likely EOF
        }
    }
    fclose(f);

    if (size == 0) {
        free(bytes);
        result.is_valid = true;
        return result;
    }

    bool valid = true;
    bool have_expected = false;
    int expected_fields = 0;
    int fields = 0;
    int line_number = 1;
    uint8_t state = 0;

    bool abort_processing = false;

    for (size_t i = 0; i < size && !abort_processing; ++i) {
        uint8_t column = classify_byte(bytes[i]);
        uint8_t next_state = strict_mode ? s32_stt_strict[state][column]
                                         : s32_stt_loose[state][column];
        state = next_state;

        if (state <= 7) {
            continue;
        }

        switch (state) {
        case 8:
        case 9:
            ++fields;
            break;
        case 10:
        case 11:
            if (state == 11) {
                ++fields;
            }
            if (have_expected) {
                if (fields != expected_fields) {
                    valid = false;
                    printf("%s: Line %d contains %d fields instead of the expected %d fields.\n",
                           filename, line_number, fields, expected_fields);
                }
            } else {
                expected_fields = fields;
                have_expected = true;
            }
            fields = 0;
            ++line_number;
            break;
        case 12:
            valid = false;
            printf("%s: Unexpected character '%c' (decimal %d) on line %d.\n",
                   filename, (char)bytes[i], (int)bytes[i], line_number);
            abort_processing = true;
            break;
        default:
            break;
        }
    }

    if (valid) {
        switch (state) {
        case 6:
        case 7:
            ++fields;
            if (have_expected) {
                if (fields != expected_fields) {
                    valid = false;
                    printf("%s: Line %d contains %d fields instead of the expected %d fields.\n",
                           filename, line_number, fields, expected_fields);
                }
            } else {
                expected_fields = fields;
                have_expected = true;
            }
            fields = 0;
            ++line_number;
            break;
        case 8:
        case 9:
            ++fields;
            if (have_expected) {
                if (fields != expected_fields) {
                    valid = false;
                    printf("%s: Line %d contains %d fields instead of the expected %d fields.\n",
                           filename, line_number, fields, expected_fields);
                }
            } else {
                expected_fields = fields;
                have_expected = true;
            }
            fields = 0;
            ++line_number;
            break;
        case 0:
        case 10:
        case 11:
            break;
        default:
            valid = false;
            printf("%s: Unexpected end of file while parsing line %d.\n",
                   filename, line_number);
            break;
        }
    }

    free(bytes);

    result.is_valid = valid;
    result.expected_fields = expected_fields;
    result.line_number = line_number;
    return result;
}

static bool validate_csv(int argc, char **argv) {
    bool strict_mode = false;
    bool overall_valid = true;
    bool any_files = false;

    for (int i = 1; i < argc; ++i) {
        const char *arg = argv[i];
        if (strcmp(arg, "-s") == 0) {
            strict_mode = true;
            continue;
        }
        any_files = true;
        file_result_t res = validate_file(arg, strict_mode);
        if (!res.is_valid) {
            printf("%s: File is %s invalid with %d columns and %d rows.\n",
                   res.filename, strict_mode ? "strictly" : "loosely",
                   res.expected_fields, res.line_number);
            overall_valid = false;
        }
    }

    if (!any_files) {
        printf("Usage: validatecsv [-s] <filename1> [filename2] ...\n");
        return false;
    }

    return overall_valid;
}

int main(int argc, char **argv) {
    return validate_csv(argc, argv) ? 0 : 1;
}
