/* symbol.c -- the symbol table.
 *
 * Names are interned in a chained hash table.  The reader folds the
 * case of a symbol written without vertical bars to the neutral case,
 * which here is UPPER: §18.1.2 leaves the choice to the implementation
 * and the oracle (Easy-ISLisp) chose upper, so a byte diff against it
 * is meaningful.  |FOO| and |foo| are distinct symbols; nil and t are
 * ordinary symbols that happen to be self-evaluating.
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>

int sym_upper_neutral = 1;

#define SYMTAB_SIZE 4096
static val symtab[SYMTAB_SIZE];     /* each bucket: a list of symbols, kept as conses */
static long gensym_counter;

val NIL, T, UNBOUND;
val sym_quote, sym_quasiquote, sym_unquote, sym_splice, sym_function;
val sym_rest, sym_amp_rest, sym_lambda, sym_t, sym_nil;
val sym_before, sym_after, sym_around, sym_initform, sym_initarg;
val sym_reader, sym_writer, sym_accessor, sym_boundp, sym_abstractp, sym_metaclass;
val sym_format_string, sym_format_arguments, sym_name, sym_namespace;
val sym_variable, sym_function_ns, sym_dynamic_variable, sym_class_ns;
val sym_operation, sym_operands, sym_object, sym_expected_class, sym_string, sym_stream;

static unsigned hash_name(const char *s, long len) {
    unsigned h = 2166136261u;
    long i;
    for (i = 0; i < len; i++) { h ^= (unsigned char)s[i]; h *= 16777619u; }
    return h % SYMTAB_SIZE;
}

static val new_symbol(const char *name, long len) {
    symbol_t *s;
    val str;
    long sp = gc_sp;
    str = mk_string(name, len);
    gc_push(str);
    s = (symbol_t *)heap_alloc(T_SYMBOL, sizeof(symbol_t));
    s->name = str;
    s->fn = UNBOUND; s->value = UNBOUND; s->dyn = UNBOUND;
    s->plist = NIL; s->cls = NIL;
    gc_sp = sp;
    return (val)s;
}

val intern(const char *name, long len) {
    unsigned h = hash_name(name, len);
    val l, s;
    for (l = symtab[h]; l != 0 && l != NIL; l = CDR(l)) {
        s = CAR(l);
        if (STR(SYM(s)->name)->len == len && memcmp(STR(SYM(s)->name)->data, name, (size_t)len) == 0)
            return s;
    }
    s = new_symbol(name, len);
    symtab[h] = mk_cons(s, symtab[h] ? symtab[h] : NIL);
    return s;
}

val intern_c(const char *name) { return intern(name, (long)strlen(name)); }

val mk_uninterned(const char *name) { return new_symbol(name, (long)strlen(name)); }

val gensym(void) {
    char buf[32];
    gensym_counter++;
    snprintf(buf, sizeof buf, "G%ld", gensym_counter);
    return mk_uninterned(buf);
}

void symbol_mark_all(void (*mark)(val)) {
    int i;
    for (i = 0; i < SYMTAB_SIZE; i++)
        if (symtab[i]) mark(symtab[i]);
}

void symbol_init(void) {
    int i;
    for (i = 0; i < SYMTAB_SIZE; i++) symtab[i] = 0;
    /* NIL must exist before intern can build a bucket list; bootstrap it
     * by hand: the bucket cons is made with a provisional 0 tail. */
    NIL = new_symbol("NIL", 3);
    SYM(NIL)->plist = NIL; SYM(NIL)->cls = NIL;
    symtab[hash_name("NIL", 3)] = mk_cons(NIL, NIL);
    SYM(NIL)->value = NIL;
    SYM(NIL)->constant = 2;
    sym_nil = NIL;
    T = intern_c("T");
    SYM(T)->value = T;
    SYM(T)->constant = 2;
    sym_t = T;
    UNBOUND = mk_uninterned("#<unbound>");
    SYM(UNBOUND)->value = UNBOUND;
    /* intern() did not know UNBOUND yet when NIL and T were made. */
    SYM(NIL)->fn = UNBOUND; SYM(NIL)->dyn = UNBOUND;
    SYM(T)->fn = UNBOUND; SYM(T)->dyn = UNBOUND;

    sym_quote = intern_c("QUOTE");
    sym_quasiquote = intern_c("QUASIQUOTE");
    sym_unquote = intern_c("UNQUOTE");
    sym_splice = intern_c("UNQUOTE-SPLICING");
    sym_function = intern_c("FUNCTION");
    sym_rest = intern_c(":REST");
    sym_amp_rest = intern_c("&REST");
    sym_lambda = intern_c("LAMBDA");
    sym_before = intern_c(":BEFORE");
    sym_after = intern_c(":AFTER");
    sym_around = intern_c(":AROUND");
    sym_initform = intern_c(":INITFORM");
    sym_initarg = intern_c(":INITARG");
    sym_reader = intern_c(":READER");
    sym_writer = intern_c(":WRITER");
    sym_accessor = intern_c(":ACCESSOR");
    sym_boundp = intern_c(":BOUNDP");
    sym_abstractp = intern_c(":ABSTRACTP");
    sym_metaclass = intern_c(":METACLASS");
    sym_format_string = intern_c("FORMAT-STRING");
    sym_format_arguments = intern_c("FORMAT-ARGUMENTS");
    sym_name = intern_c("NAME");
    sym_namespace = intern_c("NAMESPACE");
    sym_variable = intern_c("VARIABLE");
    sym_function_ns = intern_c("FUNCTION");
    sym_dynamic_variable = intern_c("DYNAMIC-VARIABLE");
    sym_class_ns = intern_c("CLASS");
    sym_operation = intern_c("OPERATION");
    sym_operands = intern_c("OPERANDS");
    sym_object = intern_c("OBJECT");
    sym_expected_class = intern_c("EXPECTED-CLASS");
    sym_string = intern_c("STRING");
    sym_stream = intern_c("STREAM");
}
