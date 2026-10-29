#ifndef EXPR_H
#define EXPR_H

#include "dbf.h"

typedef enum { VAL_NIL, VAL_NUM, VAL_CHAR, VAL_DATE, VAL_LOGIC, VAL_ARRAY } val_type_t;

struct array_s;

typedef struct {
    val_type_t type;
    /* A number's display attributes, as Clipper and dBASE III keep them:
     * its decimals, and the width of its integer part (0: the automatic
     * width, ten columns or the digits and one more) */
    short ndec;
    short nwidth;
    union {
        char str[256];
        double num;
        int32_t date;   /* JDN */
        int logic;      /* 0 or 1 */
        struct array_s *array;
    };
} value_t;

struct memvar_store;
struct set_options;

typedef struct {
    dbf_t *db;
    struct memvar_store *vars;
    struct set_options *opts;
    dbf_t *(*area_lookup)(const char *alias);  /* resolve alias→dbf */
    int found;      /* last LOCATE result */
    int bof_flag;   /* set when SKIP goes before record 1 */
    int eof_flag;   /* set when SKIP goes past last record */
    const char *error;
    char err_msg[128]; /* persistent buffer for formatted errors */
} expr_ctx_t;

#define MAX_FUNC_ARGS 32

/* Evaluate expression, advance *pp past consumed input. Returns 0 on success. */
int expr_eval(expr_ctx_t *ctx, const char **pp, value_t *result);

/* Convenience: evaluate a whole string */
int expr_eval_str(expr_ctx_t *ctx, const char *str, value_t *result);

/* Format value for display */
void val_to_string(const value_t *v, char *buf, int size);
/* How ? and ?? show a value: a number right-justified in its width with
 * its decimals (SET FIXED: SET DECIMALS of them); anything else as
 * val_to_string does */
void val_to_display(const value_t *v, char *buf, int size);
/* A number with explicit decimals (and integer width, 0 = automatic) */
value_t val_num_dec(double n, int ndec, int nwidth);
/* A numeric literal: as many decimals as its text writes */
value_t val_num_literal(double n, const char *text);
/* SET DECIMALS and SET FIXED, which arithmetic and display read */
extern int val_set_decimals;
extern int val_set_fixed;

/* Value constructors */
value_t val_num(double n);
value_t val_str(const char *s);
value_t val_logic(int b);
value_t val_date(int32_t jdn);
value_t val_nil(void);

#endif
