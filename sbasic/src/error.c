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
    "EXIT FOR",
    "EXIT WHILE",
    "EXIT DO",
    "EXIT SUB",
    "EXIT FUNCTION",
    "GOTO",
    "GOSUB",
    "RETURN",
    "RETURN without GOSUB",
    "Label not defined",
    "Subprogram not defined",
    "Duplicate label",
    "LOOP without DO",
    "DO without LOOP",
    "Expected END SELECT",
    "Cannot reassign CONST",
    "Subscript out of range",
    "Array already dimensioned",
    "Out of DATA",
    "File not found",
    "File already open",
    "File not open",
    "Bad file number",
    "Undefined TYPE",
    "Duplicate field name",
    "Undefined field",
    "RESUME without error",
    "ON ERROR GOTO",
    "Out of stack space",
    "Internal error",
    "Wrong number of dimensions",
    "Argument-count mismatch",
    "Function not defined",
    "Parameter type mismatch",
    "Advanced feature unavailable",
    "RUN",
};

int error_qb_code(error_t err) {
    switch (err) {
    case ERR_NEXT_WITHOUT_FOR:       return 1;
    case ERR_SYNTAX:                 return 2;
    case ERR_RETURN_WITHOUT_GOSUB:   return 3;
    case ERR_OUT_OF_DATA:            return 4;
    case ERR_ILLEGAL_FUNCTION_CALL:  return 5;
    case ERR_OVERFLOW:               return 6;
    case ERR_OUT_OF_MEMORY:          return 7;
    case ERR_UNDEFINED_LABEL:        return 8;
    case ERR_SUBSCRIPT_OUT_OF_RANGE: return 9;
    case ERR_DUPLICATE_DIM:          return 10;
    case ERR_DIVISION_BY_ZERO:       return 11;
    case ERR_TYPE_MISMATCH:
    case ERR_PARAM_MISMATCH:         return 13;
    case ERR_STRING_TOO_LONG:        return 15;
    case ERR_FUNC_NOT_DEFINED:       return 18;
    case ERR_RESUME_WITHOUT_ERROR:   return 20;
    case ERR_FOR_WITHOUT_NEXT:       return 26;
    case ERR_STACK_OVERFLOW:         return 28;
    case ERR_WHILE_WITHOUT_WEND:     return 29;
    case ERR_WEND_WITHOUT_WHILE:     return 30;
    case ERR_DUPLICATE_LABEL:        return 33;
    case ERR_UNDEFINED_PROC:         return 35;
    case ERR_ARG_COUNT:              return 37;
    case ERR_INTERNAL:               return 51;
    case ERR_BAD_FILE_NUMBER:
    case ERR_FILE_NOT_OPEN:          return 52;
    case ERR_FILE_NOT_FOUND:         return 53;
    case ERR_FILE_ALREADY_OPEN:      return 55;
    case ERR_INPUT_PAST_END:         return 62;
    case ERR_ADVANCED:               return 73;
    default:                         return 5;
    }
}

const char *error_message(error_t err) {
    if (err >= 0 && err < ERR_COUNT)
        return error_messages[err];
    return "Unknown error";
}

/* ERROR n's number, or the error a handler had when ON ERROR GOTO 0
 * stopped it: ERR_ON_ERROR_GOTO prints as QBasic prints that number --
 * its error's message, or "Unprintable error" */
int error_raised_code;

void error_print(error_t err, int line) {
    const char *m;
    if (err == ERR_NONE || err == ERR_EXIT ||
        (err >= ERR_EXIT_FOR && err <= ERR_RETURN))
        return;
    m = error_message(err);
    if (err == ERR_ON_ERROR_GOTO) {
        m = "Unprintable error";
        /* (5 is every unnumbered error's too: Illegal function call's own) */
        if (error_raised_code == 5) m = error_message(ERR_ILLEGAL_FUNCTION_CALL);
        else for (int e = ERR_NONE + 1; e < ERR_COUNT; e++)
            if (e != ERR_ON_ERROR_GOTO && error_qb_code((error_t)e) == error_raised_code) {
                m = error_message((error_t)e);
                break;
            }
    }
    if (line > 0)
        printf("Error in line %d: %s\n", line, m);
    else
        printf("Error: %s\n", m);
}
