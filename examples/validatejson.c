#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_NESTING 2048

typedef struct {
    const char *filename;
    bool is_valid;
    int line;
    int column;
    const char *error_msg;
} json_result_t;

typedef struct {
    const unsigned char *buf;
    size_t len;
    size_t pos;
    int line;
    int column;
    json_result_t *result;
} json_parser_t;

static inline bool is_hex(unsigned char c) {
    return (c >= '0' && c <= '9') ||
           (c >= 'a' && c <= 'f') ||
           (c >= 'A' && c <= 'F');
}

static inline unsigned hex_val(unsigned char c) {
    if (c >= '0' && c <= '9') return (unsigned)(c - '0');
    if (c >= 'a' && c <= 'f') return (unsigned)(10 + (c - 'a'));
    return (unsigned)(10 + (c - 'A'));
}

static inline unsigned char peek_byte(const json_parser_t *p) {
    return (p->pos < p->len) ? p->buf[p->pos] : 0;
}

static inline bool at_end(const json_parser_t *p) {
    return p->pos >= p->len;
}

static void fail(json_parser_t *p, const char *msg) {
    if (!p->result->is_valid) return;
    p->result->is_valid = false;
    p->result->line = p->line;
    p->result->column = p->column;
    p->result->error_msg = msg;
}

static unsigned char consume_byte(json_parser_t *p) {
    unsigned char c = p->buf[p->pos++];
    if (c == '\n') {
        p->line++;
        p->column = 1;
    } else {
        p->column++;
    }
    return c;
}

static void skip_ws(json_parser_t *p) {
    while (!at_end(p)) {
        unsigned char c = peek_byte(p);
        if (c == ' ' || c == '\t' || c == '\n' || c == '\r') {
            consume_byte(p);
        } else {
            break;
        }
    }
}

static bool parse_value(json_parser_t *p, int depth);

static bool parse_unicode_escape(json_parser_t *p, unsigned *out) {
    if (p->len - p->pos < 4) {
        fail(p, "incomplete unicode escape");
        return false;
    }
    unsigned v = 0;
    for (int i = 0; i < 4; i++) {
        unsigned char c = consume_byte(p);
        if (!is_hex(c)) {
            fail(p, "invalid unicode escape");
            return false;
        }
        v = (v << 4) | hex_val(c);
    }
    *out = v;
    return true;
}

static bool consume_utf8_char(json_parser_t *p) {
    if (at_end(p)) {
        fail(p, "unexpected end of string");
        return false;
    }

    unsigned char b0 = peek_byte(p);
    if (b0 < 0x80) {
        consume_byte(p);
        return true;
    }

    int extra = 0;
    unsigned min_cp = 0;
    unsigned cp = 0;

    if ((b0 & 0xE0) == 0xC0) {
        extra = 1;
        min_cp = 0x80;
        cp = b0 & 0x1F;
    } else if ((b0 & 0xF0) == 0xE0) {
        extra = 2;
        min_cp = 0x800;
        cp = b0 & 0x0F;
    } else if ((b0 & 0xF8) == 0xF0) {
        extra = 3;
        min_cp = 0x10000;
        cp = b0 & 0x07;
    } else {
        fail(p, "invalid UTF-8 leading byte");
        return false;
    }

    consume_byte(p);
    for (int i = 0; i < extra; i++) {
        if (at_end(p)) {
            fail(p, "incomplete UTF-8 sequence");
            return false;
        }
        unsigned char bx = peek_byte(p);
        if ((bx & 0xC0) != 0x80) {
            fail(p, "invalid UTF-8 continuation byte");
            return false;
        }
        consume_byte(p);
        cp = (cp << 6) | (unsigned)(bx & 0x3F);
    }

    if (cp < min_cp) {
        fail(p, "overlong UTF-8 sequence");
        return false;
    }
    if (cp > 0x10FFFF) {
        fail(p, "invalid UTF-8 code point");
        return false;
    }
    if (cp >= 0xD800 && cp <= 0xDFFF) {
        fail(p, "invalid UTF-8 surrogate code point");
        return false;
    }

    return true;
}

static bool parse_string(json_parser_t *p) {
    if (at_end(p) || consume_byte(p) != '"') {
        fail(p, "expected string");
        return false;
    }

    while (!at_end(p)) {
        unsigned char c = peek_byte(p);

        if (c == '"') {
            consume_byte(p);
            return true;
        }

        if (c < 0x20) {
            fail(p, "control character in string");
            return false;
        }

        if (c == '\\') {
            consume_byte(p);
            if (at_end(p)) {
                fail(p, "unfinished escape sequence");
                return false;
            }
            unsigned char e = consume_byte(p);
            switch (e) {
            case '"':
            case '\\':
            case '/':
            case 'b':
            case 'f':
            case 'n':
            case 'r':
            case 't':
                break;
            case 'u': {
                unsigned u1;
                if (!parse_unicode_escape(p, &u1)) return false;
                if (u1 >= 0xD800 && u1 <= 0xDBFF) {
                    if (p->len - p->pos < 6) {
                        fail(p, "incomplete surrogate pair");
                        return false;
                    }
                    if (consume_byte(p) != '\\' || consume_byte(p) != 'u') {
                        fail(p, "missing low surrogate");
                        return false;
                    }
                    unsigned u2;
                    if (!parse_unicode_escape(p, &u2)) return false;
                    if (!(u2 >= 0xDC00 && u2 <= 0xDFFF)) {
                        fail(p, "invalid low surrogate");
                        return false;
                    }
                } else if (u1 >= 0xDC00 && u1 <= 0xDFFF) {
                    fail(p, "unexpected low surrogate");
                    return false;
                }
                break;
            }
            default:
                fail(p, "invalid escape sequence");
                return false;
            }
            continue;
        }

        if (!consume_utf8_char(p)) return false;
    }

    fail(p, "unterminated string");
    return false;
}

static bool parse_number(json_parser_t *p) {
    if (!at_end(p) && peek_byte(p) == '-') {
        consume_byte(p);
        if (at_end(p)) {
            fail(p, "incomplete number");
            return false;
        }
    }

    if (at_end(p)) {
        fail(p, "incomplete number");
        return false;
    }

    unsigned char c = peek_byte(p);
    if (c == '0') {
        consume_byte(p);
        if (!at_end(p)) {
            unsigned char d = peek_byte(p);
            if (d >= '0' && d <= '9') {
                fail(p, "leading zero in number");
                return false;
            }
        }
    } else if (c >= '1' && c <= '9') {
        consume_byte(p);
        while (!at_end(p)) {
            c = peek_byte(p);
            if (c >= '0' && c <= '9') {
                consume_byte(p);
            } else {
                break;
            }
        }
    } else {
        fail(p, "invalid number");
        return false;
    }

    if (!at_end(p) && peek_byte(p) == '.') {
        consume_byte(p);
        if (at_end(p) || peek_byte(p) < '0' || peek_byte(p) > '9') {
            fail(p, "invalid fraction in number");
            return false;
        }
        while (!at_end(p)) {
            c = peek_byte(p);
            if (c >= '0' && c <= '9') {
                consume_byte(p);
            } else {
                break;
            }
        }
    }

    if (!at_end(p) && (peek_byte(p) == 'e' || peek_byte(p) == 'E')) {
        consume_byte(p);
        if (!at_end(p) && (peek_byte(p) == '+' || peek_byte(p) == '-')) {
            consume_byte(p);
        }
        if (at_end(p) || peek_byte(p) < '0' || peek_byte(p) > '9') {
            fail(p, "invalid exponent in number");
            return false;
        }
        while (!at_end(p)) {
            c = peek_byte(p);
            if (c >= '0' && c <= '9') {
                consume_byte(p);
            } else {
                break;
            }
        }
    }

    return true;
}

static bool parse_literal(json_parser_t *p, const char *lit) {
    size_t start = p->pos;
    for (size_t i = 0; lit[i] != '\0'; i++) {
        if (at_end(p) || peek_byte(p) != (unsigned char)lit[i]) {
            p->pos = start;
            fail(p, "invalid literal");
            return false;
        }
        consume_byte(p);
    }
    return true;
}

static bool parse_array(json_parser_t *p, int depth) {
    consume_byte(p); /* '[' */
    skip_ws(p);

    if (!at_end(p) && peek_byte(p) == ']') {
        consume_byte(p);
        return true;
    }

    while (true) {
        if (!parse_value(p, depth + 1)) return false;
        skip_ws(p);
        if (at_end(p)) {
            fail(p, "unterminated array");
            return false;
        }
        unsigned char c = consume_byte(p);
        if (c == ']') return true;
        if (c != ',') {
            fail(p, "expected ',' or ']' in array");
            return false;
        }
        skip_ws(p);
    }
}

static bool parse_object(json_parser_t *p, int depth) {
    consume_byte(p); /* '{' */
    skip_ws(p);

    if (!at_end(p) && peek_byte(p) == '}') {
        consume_byte(p);
        return true;
    }

    while (true) {
        if (at_end(p) || peek_byte(p) != '"') {
            fail(p, "expected string key in object");
            return false;
        }
        if (!parse_string(p)) return false;
        skip_ws(p);

        if (at_end(p) || consume_byte(p) != ':') {
            fail(p, "expected ':' after object key");
            return false;
        }

        skip_ws(p);
        if (!parse_value(p, depth + 1)) return false;
        skip_ws(p);

        if (at_end(p)) {
            fail(p, "unterminated object");
            return false;
        }

        unsigned char c = consume_byte(p);
        if (c == '}') return true;
        if (c != ',') {
            fail(p, "expected ',' or '}' in object");
            return false;
        }
        skip_ws(p);
    }
}

static bool parse_value(json_parser_t *p, int depth) {
    if (depth > MAX_NESTING) {
        fail(p, "maximum nesting depth exceeded");
        return false;
    }

    skip_ws(p);
    if (at_end(p)) {
        fail(p, "expected JSON value");
        return false;
    }

    unsigned char c = peek_byte(p);
    if (c == '"') return parse_string(p);
    if (c == '{') return parse_object(p, depth);
    if (c == '[') return parse_array(p, depth);
    if (c == 't') return parse_literal(p, "true");
    if (c == 'f') return parse_literal(p, "false");
    if (c == 'n') return parse_literal(p, "null");
    if (c == '-' || (c >= '0' && c <= '9')) return parse_number(p);

    fail(p, "unexpected character while parsing value");
    return false;
}

static bool parse_json_document(json_parser_t *p) {
    /* Optional UTF-8 BOM */
    if (p->len >= 3 && p->buf[0] == 0xEF && p->buf[1] == 0xBB && p->buf[2] == 0xBF) {
        p->pos = 3;
        p->column = 4;
    }

    skip_ws(p);
    if (at_end(p)) {
        fail(p, "empty input");
        return false;
    }

    if (!parse_value(p, 0)) return false;

    skip_ws(p);
    if (!at_end(p)) {
        fail(p, "trailing characters after top-level value");
        return false;
    }

    return true;
}

static bool read_file_bytes(const char *filename, unsigned char **out_buf, size_t *out_len) {
    FILE *f = fopen(filename, "rb");
    if (!f) return false;

    size_t cap = 4096;
    size_t len = 0;
    unsigned char *buf = (unsigned char *)malloc(cap);
    if (!buf) {
        fclose(f);
        return false;
    }

    unsigned char tmp[1024];
    while (1) {
        size_t n = fread(tmp, 1u, sizeof(tmp), f);
        if (n == 0) break;
        if (len + n > cap) {
            while (len + n > cap) cap *= 2;
            unsigned char *nb = (unsigned char *)realloc(buf, cap);
            if (!nb) {
                free(buf);
                fclose(f);
                return false;
            }
            buf = nb;
        }
        memcpy(buf + len, tmp, n);
        len += n;
        if (n < sizeof(tmp)) break;
    }

    fclose(f);
    *out_buf = buf;
    *out_len = len;
    return true;
}

static json_result_t validate_file(const char *filename) {
    json_result_t r;
    r.filename = filename;
    r.is_valid = true;
    r.line = 1;
    r.column = 1;
    r.error_msg = NULL;

    unsigned char *buf = NULL;
    size_t len = 0;
    if (!read_file_bytes(filename, &buf, &len)) {
        r.is_valid = false;
        r.error_msg = "unable to read file";
        return r;
    }

    json_parser_t p;
    p.buf = buf;
    p.len = len;
    p.pos = 0;
    p.line = 1;
    p.column = 1;
    p.result = &r;

    (void)parse_json_document(&p);

    free(buf);
    return r;
}

int main(int argc, char **argv) {
    bool verbose = false;
    bool saw_file = false;
    bool all_valid = true;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-v") == 0 || strcmp(argv[i], "--verbose") == 0) {
            verbose = true;
            continue;
        }

        saw_file = true;
        json_result_t r = validate_file(argv[i]);
        if (!r.is_valid) {
            all_valid = false;
            printf("%s: invalid JSON at line %d, column %d: %s\n",
                   r.filename, r.line, r.column,
                   r.error_msg ? r.error_msg : "unknown error");
        } else if (verbose) {
            printf("%s: valid JSON\n", r.filename);
        }
    }

    if (!saw_file) {
        printf("Usage: validatejson [-v|--verbose] <filename1> [filename2] ...\n");
        return 1;
    }

    return all_valid ? 0 : 1;
}
