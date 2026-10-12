/* isl.h -- ISLISP (ISO/IEC 13816:2007) for SLOW-32: the object model.
 *
 * One translation unit per concern, all sharing this header.  The
 * interpreter is plain C99 so that three compilers build it: the host
 * cc for development, clang for SLOW-32, and stage08 cc for SLOW-32
 * again (the two SLOW-32 builds must print the same bytes: that is the
 * stage08 gate this program adds -- docs/plans/islisp.md).
 *
 * Values are tagged words.  Bit 0 set is a fixnum (31 bits of payload,
 * the same range on a 64-bit host so the two builds agree on when a
 * bignum appears); low bits 10 are a character; low bits 00 are a
 * pointer to a heap object whose first word is a header.
 */
#ifndef ISL_H
#define ISL_H

#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <setjmp.h>
#include <math.h>
#ifndef HUGE_VAL                     /* the self-hosted libc's math.h has no HUGE_VAL */
#define HUGE_VAL (1e300 * 1e300)
#endif

typedef uintptr_t val;

#define FIX_TAG      1u
#define IS_FIX(v)    (((v) & 1u) != 0)
#define MKFIX(n)     ((val)(((uintptr_t)(intptr_t)(n) << 1) | 1u))
#define FIXVAL(v)    ((long)((intptr_t)(v) >> 1))
#define FIXNUM_MAX   ((long)1073741823L)
#define FIXNUM_MIN   ((long)-1073741824L)
#define FIX_OK(n)    ((n) >= FIXNUM_MIN && (n) <= FIXNUM_MAX)

#define IS_CHR(v)    (((v) & 3u) == 2u)
#define MKCHR(c)     ((val)(((uintptr_t)(unsigned char)(c) << 2) | 2u))
#define CHRVAL(v)    ((int)((v) >> 2))

#define IS_PTR(v)    (((v) & 3u) == 0u && (v) != 0)

/* Heap object tags. */
enum {
    T_CONS = 1, T_SYMBOL, T_STRING, T_FLOAT, T_BIGNUM, T_VECTOR, T_ARRAY,
    T_CLOSURE, T_BUILTIN, T_MACRO, T_GENERIC, T_METHOD, T_CLASS, T_INSTANCE,
    T_STREAM, T_ENV, T_NTAGS
};

typedef struct obj {
    unsigned char tag;
    unsigned char mark;
    unsigned char imm;      /* immutable (literal, constant) */
    unsigned char aux;
    struct obj  *gcnext;    /* every object, for the sweep */
} obj;

#define TAG(v)       (((obj *)(v))->tag)
#define IS_OBJ(v, t) (IS_PTR(v) && TAG(v) == (t))

typedef struct { obj h; val car, cdr; } cons_t;
typedef struct {
    obj h;
    val name;           /* string */
    val fn;             /* function cell: closure/builtin/generic/macro, or UNBOUND */
    val value;          /* global/constant cell, or UNBOUND */
    val dyn;            /* dynamic cell, or UNBOUND */
    val plist;          /* property list: (ind val ind val ...) */
    val cls;            /* when the symbol names a class: the class object */
    int special;        /* special-operator id, 0 if none */
    int constant;       /* defconstant'd */
} symbol_t;
typedef struct { obj h; long len; char data[1]; } string_t;
typedef struct { obj h; double d; } flonum_t;
typedef struct { obj h; int sign; int n; uint32_t limb[1]; } bignum_t;   /* magnitude, little-endian limbs */
typedef struct { obj h; long n; val elt[1]; } vector_t;
typedef struct { obj h; int rank; val dims; val data; } array_t;        /* dims: fixnum vector; data: vector (rank>=2 or rank 0) */
typedef struct {
    obj h;
    val params;         /* list of symbols */
    val rest;           /* symbol or NIL */
    val body;           /* list of forms */
    val env;            /* ENV */
    val name;           /* symbol or NIL */
    int nparams;
} closure_t;
typedef val (*builtin_fn)(int argc, val *argv);
typedef struct { obj h; const char *name; builtin_fn fn; int minargs, maxargs; val sym; } builtin_t;
typedef struct { obj h; val expander; val name; } macro_t;
typedef struct { obj h; val name; val methods; int nreq; int rest; int combination; } generic_t;   /* combination: 1 standard, 0 nil */
enum { MK_FN = 0, MK_READER, MK_WRITER, MK_BOUNDP, MK_INIT, MK_CREATE };
typedef struct { obj h; val specs; val qualifier; val fn; val slot; int kind; int nreq; int rest; } method_t;
typedef struct {
    obj h;
    val name;           /* symbol */
    val supers;         /* direct superclasses, list */
    val cpl;            /* class precedence list, most specific first (includes self) */
    val slots;          /* list of slot specs: (name initargs initform readers writers accessors boundp initform-p) as vectors */
    val allslots;       /* effective slot names, list, in order */
    int builtin;        /* 1 for the predefined classes */
    int abstractp;
    int metaclass;      /* 0 standard-class, 1 built-in-class */
} class_t;
typedef struct { obj h; val cls; val slots; } instance_t;              /* slots: vector parallel to class allslots */
enum { S_FILE_IN = 1, S_FILE_OUT, S_FILE_IO, S_STR_IN, S_STR_OUT, S_STD_IN, S_STD_OUT, S_STD_ERR };
typedef struct {
    obj h;
    int kind;
    int open;
    FILE *fp;
    val str;            /* string input: the string; string output: growing buffer */
    long pos;           /* string input position; string output length */
    long cap;
    int elem;           /* 0 character stream, else byte-stream element size in bits */
    int col;            /* output column, for ~& and ~T */
    int peeked;         /* -2 none; else the pushed-back character */
    val name;           /* file name string, for messages */
} stream_t;
typedef struct { obj h; val vars; val funs; val tags; val parent; } env_t;   /* vars/funs/tags: alists of (sym . value); tags: block names and tagbody tags -> frame tag objects */

/* Distinguished objects. */
extern val NIL, T, UNBOUND;
extern val sym_quote, sym_quasiquote, sym_unquote, sym_splice, sym_function;
extern val sym_rest, sym_amp_rest, sym_lambda, sym_t, sym_nil;
extern val sym_before, sym_after, sym_around, sym_initform, sym_initarg;
extern val sym_reader, sym_writer, sym_accessor, sym_boundp, sym_abstractp, sym_metaclass;
extern val sym_format_string, sym_format_arguments, sym_name, sym_namespace;
extern val sym_variable, sym_function_ns, sym_dynamic_variable, sym_class_ns;
extern val sym_operation, sym_operands, sym_object, sym_expected_class, sym_string, sym_stream;

#define CAR(v)   (((cons_t *)(v))->car)
#define CDR(v)   (((cons_t *)(v))->cdr)
#define IS_CONS(v)   IS_OBJ(v, T_CONS)
#define IS_SYM(v)    IS_OBJ(v, T_SYMBOL)
#define IS_STR(v)    IS_OBJ(v, T_STRING)
#define IS_FLT(v)    IS_OBJ(v, T_FLOAT)
#define IS_BIG(v)    IS_OBJ(v, T_BIGNUM)
#define IS_VEC(v)    IS_OBJ(v, T_VECTOR)
#define IS_ARR(v)    IS_OBJ(v, T_ARRAY)
#define IS_LIST(v)   ((v) == NIL || IS_CONS(v))
#define IS_INT(v)    (IS_FIX(v) || IS_BIG(v))
#define IS_NUM(v)    (IS_INT(v) || IS_FLT(v))
#define IS_FUNC(v)   (IS_PTR(v) && (TAG(v) == T_CLOSURE || TAG(v) == T_BUILTIN || TAG(v) == T_GENERIC))
#define SYM(v)   ((symbol_t *)(v))
#define STR(v)   ((string_t *)(v))
#define FLT(v)   ((flonum_t *)(v))
#define BIG(v)   ((bignum_t *)(v))
#define VEC(v)   ((vector_t *)(v))
#define ARR(v)   ((array_t *)(v))
#define CLO(v)   ((closure_t *)(v))
#define BI(v)    ((builtin_t *)(v))
#define MAC(v)   ((macro_t *)(v))
#define GEN(v)   ((generic_t *)(v))
#define MET(v)   ((method_t *)(v))
#define CLS(v)   ((class_t *)(v))
#define INST(v)  ((instance_t *)(v))
#define STM(v)   ((stream_t *)(v))
#define ENV(v)   ((env_t *)(v))

/* heap.c */
void  heap_init(void);
obj  *heap_alloc(int tag, size_t bytes);
void  gc_collect(void);
extern val  *gc_roots;
extern val  *gc_stack_base;          /* the C stack's high end, recorded by main */
extern long  gc_sp;
extern long  gc_cap;
long  gc_push(val v);               /* returns the slot index */
void  gc_pop_to(long sp);
#define GC_PROTECT(v)  gc_push(v)
val   mk_cons(val a, val d);
val   mk_string(const char *s, long len);
val   mk_string_uninit(long len);
val   mk_float(double d);
val   mk_vector(long n, val fill);
val   mk_env(val parent);
val   mk_array_from_nested(int rank, val nested);
val   mk_closure(val params, val rest, val body, val env, val name);
val   mk_builtin(const char *name, builtin_fn fn, int minargs, int maxargs);
val   list1(val a);
val   list2(val a, val b);
val   list3(val a, val b, val c);
long  list_length(val l);           /* -1 if not a proper list */
val   list_nreverse(val l);
val   list_reverse(val l);
extern long heap_live, heap_allocs, gc_runs;

/* symbol.c */
void  symbol_init(void);
val   intern(const char *name, long len);
val   intern_c(const char *name);
val   mk_uninterned(const char *name);
val   gensym(void);
extern int sym_upper_neutral;        /* 1: the neutral alphabetic case is upper (ours, like the oracle) */

/* reader.c */
typedef struct {
    const char *src; long len; long pos; int line;
    stream_t *stm;                   /* when reading from a stream instead */
    int eof;
} reader_t;
val   read_from_string(const char *s, long len, long *consumed, int *got);
val   read_from_stream(val stream, int *got);
int   parse_number_text(const char *s, long len, val *out);   /* 1 if the lexeme is a number */

/* printer.c */
void  print_to_stream(val stream, val v, int escape);
val   print_to_string(val v, int escape);
void  stream_putc(val stream, int c);
void  stream_puts(val stream, const char *s);
void  stream_write(val stream, const char *s, long n);
void  float_to_text(double d, char *buf);

/* eval.c */
void  eval_init(void);
val   eval(val form, val env);
val   eval_body(val body, val env);
val   apply(val fn, int argc, val *argv);
val   funcall1(val fn, val a);
val   funcall2(val fn, val a, val b);
val   macroexpand_all(val form, val env);
extern val global_env;
extern val cur_handler;              /* the active handler function or NIL */
extern long eval_depth;

/* Non-local exits: a chain of frames with dynamic extent. */
enum { NLX_BLOCK = 1, NLX_CATCH, NLX_TAGBODY, NLX_UNWIND, NLX_HANDLER, NLX_CONTINUE, NLX_TOPLEVEL };
typedef struct nlx {
    jmp_buf jb;
    int kind;
    val tag;            /* block name / catch tag / tagbody tags list */
    val value;          /* the transferred value, or the tag for tagbody */
    struct nlx *prev;
    long dyn_sp;        /* dynamic-binding stack height to restore */
    long gc_sp;         /* root stack height to restore */
    long eval_depth;    /* evaluator depth to restore (a longjmp skips the decrements) */
    val handler;        /* handler in force when established */
    int active;         /* a block/tagbody frame that has been exited is dead */
    struct nlx *pending;/* unwind-protect: where to continue after the cleanup */
    val pending_value;
    val cleanup;        /* unwind-protect cleanup forms */
    val env;
} nlx_t;
extern nlx_t *nlx_top;
void  nlx_throw(nlx_t *target, val value);      /* never returns */
void  nlx_push(nlx_t *f, int kind, val tag);
void  nlx_pop(nlx_t *f);

/* Dynamic variables: a save stack unwound by non-local exits. */
extern val  *dyn_syms;
extern val  *dyn_olds;
extern long  dyn_sp;
void  dyn_bind(val sym, val value);
void  dyn_unwind_to(long sp);

/* Conditions (cond.c) */
void  cond_init(void);
val   signal_condition(val condition, int continuable);
val   mk_condition(val cls, int nargs, val *initargs);
void  isl_error(const char *clsname, ...);           /* signal a condition of class <clsname>; never returns */
void  isl_simple_error(const char *fmt, ...);        /* <simple-error> with a message; never returns */
void  err_domain(val obj, const char *expected_class);
void  err_arity(val fn, int argc);
void  err_undefined_function(val name);
void  err_unbound_variable(val name);
void  err_undefined_class(val name);
val   cond_class(const char *name);          /* a condition class object by name, e.g. "<domain-error>" */
void  err_undefined_dynamic(val name);
void  err_division_by_zero(val op, val operands);
void  err_float_overflow(val op, val operands);
void  err_float_underflow(val op, val operands);
void  err_parse(val string, const char *expected_class);
void  err_program(void);
void  err_control(void);
void  err_stream(val stream);
void  err_end_of_stream(val stream);
void  err_improper_list(val list);
void  err_index_out_of_range(val seq, val idx);
void  err_immutable(val obj);
val   class_by_name(const char *name);

/* class.c (ILOS) */
void  class_init(void);
val   ilos_call_next_method(val env);
val   ilos_next_method_p(val env);
val   instance_slot(val inst, val name);
val   class_create(val cls, int nargs, val *initargs);
val   class_of(val v);
int   subclassp(val c1, val c2);
int   instancep(val v, val cls);
val   generic_call(val gf, int argc, val *argv);
val   find_class_sym(val sym, int signal);

/* numbers (number.c) */
void  number_init(void);
val   num_add(val a, val b);
val   num_sub(val a, val b);
val   num_mul(val a, val b);
int   num_cmp(val a, val b);         /* -1 0 1; signals on non-numbers */
int   num_eq(val a, val b);
int   num_zerop(val v);
val   int_from_long(long n);
val   int_normalize(val big);        /* bignum -> fixnum when it fits */
double num_to_double(val v);
val   int_quotient(val a, val b, val *rem);   /* truncating */
val   int_div_floor(val a, val b);
val   int_mod_floor(val a, val b);
int   int_sign(val v);
val   int_neg(val v);
void  int_to_text(val v, int radix, char **out, long *len);   /* malloc'd */
val   parse_integer_text(const char *s, long len, int radix, int *ok);
val   float_to_integer(double d);

/* streams and format (stream.c) */
void  stream_init(void);
val   mk_stream(int kind);
extern val std_input, std_output, std_error;
extern val cur_stdin, cur_stdout, cur_stderr;
val   mk_string_output_stream(void);
val   string_output_contents(val stream);
val   mk_string_input_stream(val str);
int   stream_getc(val stream);       /* -1 at end */
void  stream_ungetc(val stream, int c);
int   stream_peekc(val stream);
val   format_to_stream(val stream, val fmtstr, int argc, val *argv);
void  stream_flush(val stream);

/* builtins (builtin*.c) */
void  builtins_init(void);
void  seq_init(void);
void  defbuiltin(const char *name, builtin_fn fn, int minargs, int maxargs);
void  defspecial(const char *name, int id);
val   symbol_function(val sym);      /* UNBOUND if none */
val   list_nth(val l, long n);
int   eql(val a, val b);
int   equal(val a, val b);
val   mk_bool(int b);

/* misc */
val   load_file(const char *path, int echo);
void  fatal(const char *msg);
extern int isl_argc;
extern char **isl_argv;
extern const char *isl_libdir;

#endif
