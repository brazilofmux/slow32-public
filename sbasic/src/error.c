#include "error.h"
#include <stdio.h>

static const char *error_messages[ERR_COUNT] = {
    "No error",
    "Syntax error",
    "Type mismatch",
    "Undefined variable",
    "Division by zero",
    "Overflow",
    "Out of memory",
    "String too long",
    "Illegal function call",
    "NEXT without FOR",
    "FOR without NEXT",
    "WEND without WHILE",
    "WHILE without WEND",
    "Expected THEN",
    "Expected END IF",
    "Unexpected token",
    "Unterminated string",
    "Input past end",
    "Break",
    "Exit",
    "Internal error",
};

const char *error_message(error_t err) {
    if (err >= 0 && err < ERR_COUNT)
        return error_messages[err];
    return "Unknown error";
}

void error_print(error_t err, int line) {
    if (err == ERR_NONE || err == ERR_EXIT)
        return;
    if (line > 0)
        printf("Error in line %d: %s\n", line, error_message(err));
    else
        printf("Error: %s\n", error_message(err));
}
