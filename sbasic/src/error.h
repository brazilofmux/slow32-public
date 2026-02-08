#ifndef SBASIC_ERROR_H
#define SBASIC_ERROR_H

typedef enum {
    ERR_NONE = 0,
    ERR_SYNTAX,
    ERR_TYPE_MISMATCH,
    ERR_UNDEFINED_VAR,
    ERR_DIVISION_BY_ZERO,
    ERR_OVERFLOW,
    ERR_OUT_OF_MEMORY,
    ERR_STRING_TOO_LONG,
    ERR_ILLEGAL_FUNCTION_CALL,
    ERR_NEXT_WITHOUT_FOR,
    ERR_FOR_WITHOUT_NEXT,
    ERR_WEND_WITHOUT_WHILE,
    ERR_WHILE_WITHOUT_WEND,
    ERR_EXPECTED_THEN,
    ERR_EXPECTED_END_IF,
    ERR_UNEXPECTED_TOKEN,
    ERR_UNTERMINATED_STRING,
    ERR_INPUT_PAST_END,
    ERR_BREAK,
    ERR_EXIT,
    ERR_INTERNAL,
    ERR_COUNT
} error_t;

/* Return the message string for an error code */
const char *error_message(error_t err);

/* Print an error with line number context */
void error_print(error_t err, int line);

/* Macro to check eval result and propagate errors */
#define EVAL_CHECK(expr) do { \
    error_t _err = (expr); \
    if (_err != ERR_NONE) return _err; \
} while (0)

#endif
