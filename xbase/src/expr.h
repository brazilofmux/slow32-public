#ifndef EXPR_H
#define EXPR_H

#include <string.h>

#include "dbf.h"

typedef enum { VAL_NIL, VAL_NUM, VAL_CHAR, VAL_DATE, VAL_LOGIC, VAL_ARRAY, VAL_BLOCK, VAL_OBJECT } val_type_t;

struct array_s;
struct block_s;
struct object_s;

typedef struct {
    val_type_t type;
    /* A number's display attributes, as Clipper and dBASE III keep them:
     * its decimals, and the width of its integer part (0: the automatic
     * width, ten columns or the digits and one more) */
    short ndec;
    short nwidth;
    /* A character value longer than 255 bytes, or holding a NUL, lives in
     * ext (len bytes, NUL-terminated too); str then holds its first 255
     * characters for readers that still take the inline text.  val_text()
     * and val_len() see the whole value; val_strn() makes one. */
    char *ext;
    int32_t len;
    union {
        char str[256];
        double num;
        int32_t date;   /* JDN */
        int logic;      /* 0 or 1 */
        struct array_s *array;
        struct block_s *block;   /* VAL_BLOCK: a code block (ast.h) */
        struct object_s *obj;    /* VAL_OBJECT: shared by reference */
    };
} value_t;

/* An object (Clipper's Error class, so far): its class name and instance
 * variables, reached with the : send operator; shared by reference, as
 * arrays are, and not collected (the same debt) */
#define OBJECT_MAX_IVARS 32
typedef struct object_s {
    char cls[32];
    int n;
    char names[OBJECT_MAX_IVARS][32];
    value_t vals[OBJECT_MAX_IVARS];
} object_t;
object_t *object_new(const char *cls);
/* The variable's slot, made when create is set; NULL when it has none */
value_t *object_ivar(object_t *o, const char *name, int create);

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
/* dBASE III's widths (the interpreter's): every number carries the width
 * of its integer part, a literal its text's, a sum one more than the
 * wider, and so on (ast.c num_result); Clipper's, a compiled program's,
 * leave most at the automatic width */
int val_dbase_widths(void);
int val_int_width(const value_t *v);   /* its integer width; the automatic one is ten */
void val_unsign_zero(char *buf);        /* a formatted zero without its minus (dBASE) */
/* A numeric literal: as many decimals as its text writes */
value_t val_num_literal(double n, const char *text);
/* SET DECIMALS and SET FIXED, which arithmetic and display read */
extern int val_set_decimals;
extern int val_set_fixed;

/* Value constructors */
value_t val_num(double n);
value_t val_str(const char *s);
value_t val_strn(const char *s, int n);         /* n bytes, NULs among them allowed */
/* A character value's whole text, and its length, NULs counted.  The only
 * way to a value's text: nothing writes into it in place (a value's text
 * may be shared), so a function that changes text makes a new value. */
static inline const char *val_text(const value_t *v) {
    return (v->type == VAL_CHAR && v->ext) ? v->ext : v->str;
}
static inline int val_len(const value_t *v) {
    if (v->type != VAL_CHAR) return 0;
    return v->ext ? v->len : (int)strlen(v->str);
}
value_t val_logic(int b);
value_t val_date(int32_t jdn);
value_t val_nil(void);
value_t val_object(object_t *o);

#endif
