#ifndef EXPR_H
#define EXPR_H

#include "dbf.h"

typedef enum { VAL_NIL, VAL_NUM, VAL_CHAR, VAL_DATE, VAL_LOGIC } val_type_t;

typedef struct {
    val_type_t type;
    union {
        char str[256];
        double num;
        int32_t date;   /* JDN */
        int logic;      /* 0 or 1 */
    };
} value_t;

typedef struct {
    dbf_t *db;
    int found;      /* last LOCATE result */
    int bof_flag;   /* set when SKIP goes before record 1 */
    int eof_flag;   /* set when SKIP goes past last record */
    const char *error;
} expr_ctx_t;

/* Evaluate expression, advance *pp past consumed input. Returns 0 on success. */
int expr_eval(expr_ctx_t *ctx, const char **pp, value_t *result);

/* Convenience: evaluate a whole string */
int expr_eval_str(expr_ctx_t *ctx, const char *str, value_t *result);

/* Format value for display */
void val_to_string(const value_t *v, char *buf, int size);

/* Value constructors */
value_t val_num(double n);
value_t val_str(const char *s);
value_t val_logic(int b);
value_t val_date(int32_t jdn);
value_t val_nil(void);

#endif
