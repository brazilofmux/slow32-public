/* cond.c -- the condition system (§29) and the error-id table (§29.4).
 *
 * A condition is an instance of one of the built-in condition classes
 * of Figure 1, created with the initargs §29.3 names.  Signalling calls
 * the innermost handler with the handler context rebound to what was in
 * force when that handler was established; a continuable signal
 * establishes a frame that continue-condition returns through.  With no
 * handler the condition is reported on the error output and the process
 * exits with status 1 -- the implementation-defined action §9.1 allows.
 */
#include "isl.h"
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

int handler_is_ignore_marker(val h);
nlx_t *find_catch_frame(val tag);
val instance_slot(val inst, val name);
val class_create(val cls, int nargs, val *initargs);

val class_by_name(const char *name) {
    char up[128];
    size_t i;
    val sym;
    for (i = 0; name[i] && i < 127; i++) up[i] = (char)((name[i] >= 'a' && name[i] <= 'z') ? name[i] - 32 : name[i]);
    up[i] = 0;
    sym = intern_c(up);
    if (SYM(sym)->cls == NIL) { fputs(up, stderr); fatal(": no such built-in class"); }
    return SYM(sym)->cls;
}

val cond_class(const char *name) { return class_by_name(name); }

val mk_condition(val cls, int nargs, val *initargs) {
    return class_create(cls, nargs, initargs);
}

static void report_condition(val c, val stream) {
    val cls = class_of(c);
    stream_puts(stream, "Error: ");
    print_to_stream(stream, CLS(cls)->name, 0);
    if (instancep(c, class_by_name("<SIMPLE-ERROR>"))) {
        val fs = instance_slot(c, sym_format_string);
        val fa = instance_slot(c, sym_format_arguments);
        stream_puts(stream, ": ");
        if (IS_STR(fs)) {
            long n = list_length(fa), i;
            long sp = gc_sp;
            long base = gc_sp;
            for (i = 0; i < n; i++, fa = CDR(fa)) gc_push(CAR(fa));
            format_to_stream(stream, fs, (int)n, &gc_roots[base]);
            gc_sp = sp;
        }
    } else if (instancep(c, class_by_name("<DOMAIN-ERROR>"))) {
        stream_puts(stream, ": ");
        print_to_stream(stream, instance_slot(c, sym_object), 1);
        stream_puts(stream, " is not of class ");
        print_to_stream(stream, instance_slot(c, sym_expected_class), 0);
    } else if (instancep(c, class_by_name("<UNDEFINED-ENTITY>"))) {
        stream_puts(stream, ": ");
        print_to_stream(stream, instance_slot(c, sym_name), 1);
    } else if (instancep(c, class_by_name("<ARITHMETIC-ERROR>"))) {
        stream_puts(stream, ": ");
        print_to_stream(stream, instance_slot(c, sym_operation), 0);
        stream_putc(stream, ' ');
        print_to_stream(stream, instance_slot(c, sym_operands), 1);
    } else if (instancep(c, class_by_name("<PARSE-ERROR>"))) {
        stream_puts(stream, ": ");
        print_to_stream(stream, instance_slot(c, sym_string), 1);
    }
    stream_putc(stream, '\n');
}

static void default_handler(val c) {
    nlx_t *f;
    stream_flush(cur_stdout);
    report_condition(c, cur_stderr);
    stream_flush(cur_stderr);
    for (f = nlx_top; f; f = f->prev)
        if (f->kind == NLX_TOPLEVEL) nlx_throw(f, c);
    exit(1);
}

val signal_condition_desc(val condition, val continuable_desc);
val signal_condition(val condition, int continuable) { return signal_condition_desc(condition, continuable ? T : NIL); }

val signal_condition_desc(val condition, val continuable_desc) {
    int continuable = continuable_desc != NIL;
    val handlers = cur_handler;
    val h;
    long sp = gc_sp;
    gc_push(condition);
    for (;;) {
        if (handlers == NIL) default_handler(condition);
        h = CAR(handlers);
        if (handler_is_ignore_marker(h)) {
            if (instancep(condition, class_by_name("<ERROR>"))) {
                nlx_t *f = find_catch_frame(h);
                if (f) nlx_throw(f, NIL);
            }
            handlers = CDR(handlers);
            continue;
        }
        break;
    }
    {
        nlx_t f;
        val saved = cur_handler;
        val r = NIL;
        gc_push(saved);
        if (continuable) {
            nlx_push(&f, NLX_CONTINUE, condition);
            f.cleanup = continuable_desc;
            if (setjmp(f.jb) == 0) {
                cur_handler = CDR(handlers);
                r = funcall1(h, condition);
                nlx_pop(&f);
                cur_handler = saved;
                /* The handler returned normally.  §29.2 leaves that undefined;
                 * for a continuable condition the useful reading, and the
                 * suite's (unwind.lsp: an inner handler re-signals to an outer
                 * one and returns what it got), is to continue with its value. */
            } else {
                nlx_pop(&f);
                cur_handler = saved;
                r = f.value;
            }
            gc_sp = sp;
            return r;
        }
        cur_handler = CDR(handlers);
        funcall1(h, condition);
        cur_handler = saved;
        /* §29.2: the consequences are undefined if the handler returns. */
        stream_flush(cur_stdout);
        stream_puts(cur_stderr, "Error: a handler returned from a non-continuable condition\n");
        default_handler(condition);
    }
    return NIL;
}


/* Build a condition of the named class from (symbol value)* pairs. */
void isl_error(const char *clsname, ...) {
    va_list ap;
    long sp = gc_sp;
    long base = gc_sp;
    int n = 0;
    val cls = class_by_name(clsname);
    val c;
    va_start(ap, clsname);
    for (;;) {
        val k = va_arg(ap, val);
        val v;
        if (k == 0) break;
        v = va_arg(ap, val);
        gc_push(k); gc_push(v);
        n += 2;
    }
    va_end(ap);
    c = mk_condition(cls, n, &gc_roots[base]);
    gc_sp = sp;
    signal_condition(c, 0);
    fatal("error returned");
}

void isl_simple_error(const char *fmt, ...) {
    val s = mk_string(fmt, (long)strlen(fmt));
    isl_error("<SIMPLE-ERROR>", sym_format_string, s, sym_format_arguments, NIL, NULL);
}

void err_domain(val obj, const char *expected) {
    val cls;
    char up[128];
    size_t i;
    for (i = 0; expected[i] && i < 127; i++) up[i] = (char)((expected[i] >= 'a' && expected[i] <= 'z') ? expected[i] - 32 : expected[i]);
    up[i] = 0;
    cls = SYM(intern_c(up))->cls;
    if (cls == NIL) cls = class_by_name("<OBJECT>");
    isl_error("<DOMAIN-ERROR>", sym_object, obj, sym_expected_class, cls, NULL);
}

void err_arity(val fn, int argc) {
    (void)argc;
    isl_error("<PROGRAM-ERROR>", sym_object, fn, NULL);
}

void err_undefined_function(val name) {
    isl_error("<UNDEFINED-FUNCTION>", sym_name, name, sym_namespace, sym_function_ns, NULL);
}

void err_unbound_variable(val name) {
    isl_error("<UNBOUND-VARIABLE>", sym_name, name, sym_namespace, sym_variable, NULL);
}

void err_undefined_class(val name) {
    isl_error("<UNDEFINED-ENTITY>", sym_name, name, sym_namespace, sym_class_ns, NULL);
}

void err_undefined_dynamic(val name) {
    isl_error("<UNBOUND-VARIABLE>", sym_name, name, sym_namespace, sym_dynamic_variable, NULL);
}

/* The operation of an arithmetic error is the function object (the suite
 * asks functionp of it and compares it with #'div). */
static val op_function(val op) {
    if (IS_SYM(op) && SYM(op)->fn != UNBOUND) return SYM(op)->fn;
    return op;
}

void err_division_by_zero(val op, val operands) {
    isl_error("<DIVISION-BY-ZERO>", sym_operation, op_function(op), sym_operands, operands, NULL);
}

void err_float_overflow(val op, val operands) {
    isl_error("<FLOATING-POINT-OVERFLOW>", sym_operation, op_function(op), sym_operands, operands, NULL);
}

void err_float_underflow(val op, val operands) {
    isl_error("<FLOATING-POINT-UNDERFLOW>", sym_operation, op_function(op), sym_operands, operands, NULL);
}

void err_parse(val string, const char *expected) {
    val cls = SYM(intern_c(expected))->cls;
    if (cls == NIL) cls = class_by_name("<OBJECT>");
    isl_error("<PARSE-ERROR>", sym_string, string, sym_expected_class, cls, NULL);
}

void err_program(void) { isl_error("<PROGRAM-ERROR>", NULL); }
void err_control(void) { isl_error("<CONTROL-ERROR>", NULL); }
void err_stream(val stream) { isl_error("<STREAM-ERROR>", sym_stream, stream, NULL); }
void err_end_of_stream(val stream) { isl_error("<END-OF-STREAM>", sym_stream, stream, NULL); }
void err_improper_list(val list) { isl_error("<PROGRAM-ERROR>", sym_object, list, NULL); }
void err_index_out_of_range(val seq, val idx) { (void)seq; isl_error("<PROGRAM-ERROR>", sym_object, idx, NULL); }
void err_immutable(val obj) { isl_error("<PROGRAM-ERROR>", sym_object, obj, NULL); }

/* ---------- builtins of §29 ---------- */

static val bi_error(int argc, val *argv) {
    val args = NIL;
    int i;
    long sp = gc_sp;
    long slot = gc_push(NIL);
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    for (i = argc - 1; i >= 1; i--) { args = mk_cons(argv[i], args); gc_roots[slot] = args; }
    {
        val c;
        long base = gc_sp;
        gc_push(sym_format_string); gc_push(argv[0]); gc_push(sym_format_arguments); gc_push(args);
        c = mk_condition(class_by_name("<SIMPLE-ERROR>"), 4, &gc_roots[base]);
        gc_sp = sp;
        signal_condition(c, 0);
    }
    return NIL;
}

static val bi_cerror(int argc, val *argv) {
    val args = NIL;
    int i;
    long sp = gc_sp;
    long slot = gc_push(NIL);
    val cont;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    if (!IS_STR(argv[1])) err_domain(argv[1], "<string>");
    for (i = argc - 1; i >= 2; i--) { args = mk_cons(argv[i], args); gc_roots[slot] = args; }
    gc_push(args);
    {
        val s = mk_string_output_stream();
        gc_push(s);
        format_to_stream(s, argv[0], argc - 2, argv + 2);
        cont = string_output_contents(s);
        gc_push(cont);
    }
    {
        val c;
        long base = gc_sp;
        val r;
        gc_push(sym_format_string); gc_push(argv[1]); gc_push(sym_format_arguments); gc_push(args);
        c = mk_condition(class_by_name("<SIMPLE-ERROR>"), 4, &gc_roots[base]);
        gc_push(c);
        r = signal_condition_desc(c, cont);
        gc_sp = sp;
        return r;
    }
}

static val bi_signal_condition(int argc, val *argv) {
    (void)argc;
    if (!instancep(argv[0], class_by_name("<SERIOUS-CONDITION>"))) err_domain(argv[0], "<serious-condition>");
    return signal_condition_desc(argv[0], argv[1]);
}

static val bi_condition_continuable(int argc, val *argv) {
    nlx_t *f;
    (void)argc;
    if (!instancep(argv[0], class_by_name("<SERIOUS-CONDITION>"))) err_domain(argv[0], "<serious-condition>");
    for (f = nlx_top; f; f = f->prev)
        if (f->kind == NLX_CONTINUE && f->tag == argv[0] && f->active) return f->cleanup;   /* the description */
    return NIL;
}

static val bi_continue_condition(int argc, val *argv) {
    nlx_t *f;
    val v = argc > 1 ? argv[1] : NIL;
    if (!instancep(argv[0], class_by_name("<SERIOUS-CONDITION>"))) err_domain(argv[0], "<serious-condition>");
    for (f = nlx_top; f; f = f->prev)
        if (f->kind == NLX_CONTINUE && f->tag == argv[0] && f->active) nlx_throw(f, v);
    /* Not continuable: the text says an error shall be signaled and names no
     * class; the suite expects exactly <error>, as the oracle signals. */
    isl_error("<ERROR>", NULL);
    return NIL;
}

static val slot_reader(val inst, const char *cls, val slot) {
    if (!instancep(inst, class_by_name(cls))) err_domain(inst, cls);
    return instance_slot(inst, slot);
}
static val bi_ae_operation(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<ARITHMETIC-ERROR>", sym_operation); }
static val bi_ae_operands(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<ARITHMETIC-ERROR>", sym_operands); }
static val bi_de_object(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<DOMAIN-ERROR>", sym_object); }
static val bi_de_expected(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<DOMAIN-ERROR>", sym_expected_class); }
static val bi_pe_string(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<PARSE-ERROR>", sym_string); }
static val bi_pe_expected(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<PARSE-ERROR>", sym_expected_class); }
static val bi_se_fs(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<SIMPLE-ERROR>", sym_format_string); }
static val bi_se_fa(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<SIMPLE-ERROR>", sym_format_arguments); }
static val bi_ste_stream(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<STREAM-ERROR>", sym_stream); }
static val bi_ue_name(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<UNDEFINED-ENTITY>", sym_name); }
static val bi_ue_namespace(int argc, val *argv) { (void)argc; return slot_reader(argv[0], "<UNDEFINED-ENTITY>", sym_namespace); }

void cond_init(void) {
    defbuiltin("ERROR", bi_error, 1, -1);
    defbuiltin("CERROR", bi_cerror, 2, -1);
    defbuiltin("SIGNAL-CONDITION", bi_signal_condition, 2, 2);
    defbuiltin("CONDITION-CONTINUABLE", bi_condition_continuable, 1, 1);
    defbuiltin("CONTINUE-CONDITION", bi_continue_condition, 1, 2);
    defbuiltin("ARITHMETIC-ERROR-OPERATION", bi_ae_operation, 1, 1);
    defbuiltin("ARITHMETIC-ERROR-OPERANDS", bi_ae_operands, 1, 1);
    defbuiltin("DOMAIN-ERROR-OBJECT", bi_de_object, 1, 1);
    defbuiltin("DOMAIN-ERROR-EXPECTED-CLASS", bi_de_expected, 1, 1);
    defbuiltin("PARSE-ERROR-STRING", bi_pe_string, 1, 1);
    defbuiltin("PARSE-ERROR-EXPECTED-CLASS", bi_pe_expected, 1, 1);
    defbuiltin("SIMPLE-ERROR-FORMAT-STRING", bi_se_fs, 1, 1);
    defbuiltin("SIMPLE-ERROR-FORMAT-ARGUMENTS", bi_se_fa, 1, 1);
    defbuiltin("STREAM-ERROR-STREAM", bi_ste_stream, 1, 1);
    defbuiltin("UNDEFINED-ENTITY-NAME", bi_ue_name, 1, 1);
    defbuiltin("UNDEFINED-ENTITY-NAMESPACE", bi_ue_namespace, 1, 1);
}
