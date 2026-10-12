/* stream.c -- streams (§26), input and output (§27), files (§28), and format.
 *
 * Standard streams wrap the C stdio FILE*s; string streams keep a
 * string and a cursor; file streams are FILE*s too, opened by name
 * through the libc (the MMIO file service on SLOW-32).  Output tracks
 * the column so ~nT and ~& can be honoured.
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>

val std_input, std_output, std_error;
val cur_stdin, cur_stdout, cur_stderr;

val mk_stream(int kind) {
    stream_t *s = (stream_t *)heap_alloc(T_STREAM, sizeof(stream_t));
    s->kind = kind; s->open = 1; s->fp = 0; s->str = NIL; s->pos = 0; s->cap = 0;
    s->elem = 0; s->col = 0; s->peeked = -2; s->name = NIL;
    return (val)s;
}

static int is_output(val s) {
    int k = STM(s)->kind;
    return k == S_FILE_OUT || k == S_FILE_IO || k == S_STR_OUT || k == S_STD_OUT || k == S_STD_ERR;
}
static int is_input(val s) {
    int k = STM(s)->kind;
    return k == S_FILE_IN || k == S_FILE_IO || k == S_STR_IN || k == S_STD_IN;
}

/* A character operation on a byte stream (or the reverse) is a domain
 * error; one on a closed stream is a stream error. */
static void check_output(val s) {
    if (!IS_OBJ(s, T_STREAM)) err_domain(s, "<stream>");
    if (!is_output(s)) err_domain(s, "<stream>");
    if (!STM(s)->open) err_stream(s);
}
static void check_input(val s) {
    if (!IS_OBJ(s, T_STREAM)) err_domain(s, "<stream>");
    if (!is_input(s)) err_domain(s, "<stream>");
    if (!STM(s)->open) err_stream(s);
}
static void check_char_output(val s) { check_output(s); if (STM(s)->elem) err_domain(s, "<stream>"); }
static void check_char_input(val s) { check_input(s); if (STM(s)->elem) err_domain(s, "<stream>"); }

void stream_write(val stream, const char *buf, long n) {
    stream_t *s;
    long i;
    check_output(stream);
    s = STM(stream);
    if (s->kind == S_STR_OUT) {
        if (s->pos + n > s->cap) {
            long ncap = s->cap ? s->cap : 64;
            val nstr;
            while (s->pos + n > ncap) ncap *= 2;
            nstr = mk_string_uninit(ncap);
            if (s->str != NIL) memcpy(STR(nstr)->data, STR(s->str)->data, (size_t)s->pos);
            s->str = nstr;
            s->cap = ncap;
        }
        memcpy(STR(s->str)->data + s->pos, buf, (size_t)n);
        s->pos += n;
    } else {
        fwrite(buf, 1, (size_t)n, s->fp);
    }
    for (i = 0; i < n; i++) { if (buf[i] == '\n') s->col = 0; else s->col++; }
}

void stream_flush(val stream) {
    if (IS_OBJ(stream, T_STREAM) && STM(stream)->fp) fflush(STM(stream)->fp);
}

val mk_string_output_stream(void) { return mk_stream(S_STR_OUT); }

val string_output_contents(val stream) {
    stream_t *s = STM(stream);
    val r = mk_string(s->str == NIL ? "" : STR(s->str)->data, s->pos);
    s->pos = 0;
    return r;
}

val mk_string_input_stream(val str) {
    val s;
    long sp = gc_sp;
    gc_push(str);
    s = mk_stream(S_STR_IN);
    STM(s)->str = str;
    gc_sp = sp;
    return s;
}

int stream_getc(val stream) {
    stream_t *s;
    check_input(stream);
    s = STM(stream);
    if (s->peeked != -2) { int c = s->peeked; s->peeked = -2; return c; }
    if (s->kind == S_STR_IN) {
        if (s->pos >= STR(s->str)->len) return -1;
        return (unsigned char)STR(s->str)->data[s->pos++];
    }
    if (s->kind == S_STD_IN) stream_flush(cur_stdout);
    {
        int c = fgetc(s->fp);
        return c == EOF ? -1 : c;
    }
}

void stream_ungetc(val stream, int c) { STM(stream)->peeked = c; }

int stream_peekc(val stream) {
    int c = stream_getc(stream);
    if (c >= 0) stream_ungetc(stream, c);
    return c;
}

/* ---------- format ---------- */

static void fmt_integer(val stream, val v, int radix) {
    char *txt; long n;
    if (!IS_INT(v)) err_domain(v, "<integer>");
    if (radix < 2 || radix > 36) err_domain(MKFIX(radix), "<integer>");
    int_to_text(v, radix, &txt, &n);
    stream_write(stream, txt, n);
    free(txt);
}

static void fmt_float(val stream, val v) {
    char buf[64];
    if (!IS_NUM(v)) err_domain(v, "<number>");
    float_to_text(num_to_double(v), buf);
    stream_puts(stream, buf);
}

static void fmt_tab(val stream, long column) {
    long col = STM(stream)->col;
    if (col >= column) { stream_putc(stream, ' '); return; }
    while (col++ < column) stream_putc(stream, ' ');
}

val format_to_stream(val stream, val fmtstr, int argc, val *argv) {
    const char *f;
    long n, i;
    int ai = 0;
    check_output(stream);
    if (!IS_STR(fmtstr)) err_domain(fmtstr, "<string>");
    f = STR(fmtstr)->data; n = STR(fmtstr)->len;
    for (i = 0; i < n; i++) {
        char c = f[i];
        long num = -1;
        if (c != '~') { stream_putc(stream, c); continue; }
        i++;
        if (i >= n) isl_error("<PROGRAM-ERROR>", sym_object, fmtstr, NULL);
        if (f[i] >= '0' && f[i] <= '9') {
            num = 0;
            while (i < n && f[i] >= '0' && f[i] <= '9') { num = num * 10 + (f[i] - '0'); i++; }
            if (i >= n) isl_error("<PROGRAM-ERROR>", sym_object, fmtstr, NULL);
        }
        c = f[i];
        if (c >= 'a' && c <= 'z') c = (char)(c - 32);
        switch (c) {
        case 'A': case 'S': case 'B': case 'D': case 'O': case 'X': case 'C': case 'G': case 'R':
            if (ai >= argc) isl_error("<PROGRAM-ERROR>", sym_object, fmtstr, NULL);
            break;
        default: break;
        }
        switch (c) {
        case 'A': print_to_stream(stream, argv[ai++], 0); break;
        case 'S': print_to_stream(stream, argv[ai++], 1); break;
        case 'B': fmt_integer(stream, argv[ai++], 2); break;
        case 'D': fmt_integer(stream, argv[ai++], 10); break;
        case 'O': fmt_integer(stream, argv[ai++], 8); break;
        case 'X': fmt_integer(stream, argv[ai++], 16); break;
        case 'R':
            if (num < 0) isl_error("<PROGRAM-ERROR>", sym_object, fmtstr, NULL);
            fmt_integer(stream, argv[ai++], (int)num);
            break;
        case 'C':
            if (!IS_CHR(argv[ai])) err_domain(argv[ai], "<character>");
            stream_putc(stream, CHRVAL(argv[ai++]));
            break;
        case 'G': fmt_float(stream, argv[ai++]); break;
        case 'T': fmt_tab(stream, num < 0 ? 0 : num); break;
        case '%': stream_putc(stream, '\n'); break;
        case '&': if (STM(stream)->col != 0) stream_putc(stream, '\n'); break;
        case '~': stream_putc(stream, '~'); break;
        default: isl_error("<PROGRAM-ERROR>", sym_object, fmtstr, NULL);
        }
    }
    return NIL;
}

/* ---------- builtins ---------- */

static val bi_format(int argc, val *argv) {
    check_char_output(argv[0]);
    return format_to_stream(argv[0], argv[1], argc - 2, argv + 2);
}
static val bi_format_char(int argc, val *argv) {
    (void)argc; check_char_output(argv[0]);
    if (!IS_CHR(argv[1])) err_domain(argv[1], "<character>");
    stream_putc(argv[0], CHRVAL(argv[1]));
    return NIL;
}
static val bi_format_float(int argc, val *argv) { (void)argc; check_char_output(argv[0]); fmt_float(argv[0], argv[1]); return NIL; }
static val bi_format_fresh_line(int argc, val *argv) { (void)argc; check_char_output(argv[0]); if (STM(argv[0])->col != 0) stream_putc(argv[0], '\n'); return NIL; }
static val bi_format_integer(int argc, val *argv) {
    (void)argc; check_char_output(argv[0]);
    if (!IS_FIX(argv[2])) err_domain(argv[2], "<integer>");
    fmt_integer(argv[0], argv[1], (int)FIXVAL(argv[2]));
    return NIL;
}
static val bi_format_object(int argc, val *argv) { (void)argc; check_char_output(argv[0]); print_to_stream(argv[0], argv[1], argv[2] != NIL); return NIL; }
static val bi_format_tab(int argc, val *argv) {
    (void)argc; check_char_output(argv[0]);
    if (!IS_FIX(argv[1]) || FIXVAL(argv[1]) < 0) err_domain(argv[1], "<integer>");
    fmt_tab(argv[0], FIXVAL(argv[1]));
    return NIL;
}
static val bi_standard_input(int argc, val *argv) { (void)argc; (void)argv; return cur_stdin; }
static val bi_standard_output(int argc, val *argv) { (void)argc; (void)argv; return cur_stdout; }
static val bi_error_output(int argc, val *argv) { (void)argc; (void)argv; return cur_stderr; }
static val bi_streamp(int argc, val *argv) { (void)argc; return mk_bool(IS_OBJ(argv[0], T_STREAM)); }
static val bi_open_stream_p(int argc, val *argv) {
    (void)argc;
    if (!IS_OBJ(argv[0], T_STREAM)) return NIL;
    return mk_bool(STM(argv[0])->open);
}
static val bi_input_stream_p(int argc, val *argv) { (void)argc; return mk_bool(IS_OBJ(argv[0], T_STREAM) && is_input(argv[0])); }
static val bi_output_stream_p(int argc, val *argv) { (void)argc; return mk_bool(IS_OBJ(argv[0], T_STREAM) && is_output(argv[0])); }
static val bi_create_string_input_stream(int argc, val *argv) {
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    return mk_string_input_stream(argv[0]);
}
static val bi_create_string_output_stream(int argc, val *argv) { (void)argc; (void)argv; return mk_string_output_stream(); }
static val bi_get_output_stream_string(int argc, val *argv) {
    (void)argc;
    if (!IS_OBJ(argv[0], T_STREAM) || STM(argv[0])->kind != S_STR_OUT) err_domain(argv[0], "<stream>");
    return string_output_contents(argv[0]);
}

static val open_file(int kind, val name, val elem) {
    FILE *fp;
    val s;
    const char *mode = kind == S_FILE_IN ? "rb" : kind == S_FILE_OUT ? "wb" : "r+b";
    if (!IS_STR(name)) err_domain(name, "<string>");
    if (elem != NIL && (!IS_FIX(elem) || FIXVAL(elem) <= 0 || FIXVAL(elem) % 8 != 0)) err_domain(elem, "<integer>");
    fp = fopen(STR(name)->data, mode);
    if (!fp && kind == S_FILE_IO) fp = fopen(STR(name)->data, "w+b");
    if (!fp) isl_error("<STREAM-ERROR>", sym_stream, name, NULL);
    s = mk_stream(kind);
    STM(s)->fp = fp;
    STM(s)->name = name;
    STM(s)->elem = elem == NIL ? 0 : (int)FIXVAL(elem);
    return s;
}
static val bi_open_input_file(int argc, val *argv) { return open_file(S_FILE_IN, argv[0], argc > 1 ? argv[1] : NIL); }
static val bi_open_output_file(int argc, val *argv) { return open_file(S_FILE_OUT, argv[0], argc > 1 ? argv[1] : NIL); }
static val bi_open_io_file(int argc, val *argv) { return open_file(S_FILE_IO, argv[0], argc > 1 ? argv[1] : NIL); }
static val bi_close(int argc, val *argv) {
    stream_t *s;
    (void)argc;
    if (!IS_OBJ(argv[0], T_STREAM)) err_domain(argv[0], "<stream>");
    s = STM(argv[0]);
    if (s->open && s->fp && s->kind != S_STD_IN && s->kind != S_STD_OUT && s->kind != S_STD_ERR) fclose(s->fp);
    s->open = 0;
    s->fp = 0;
    return NIL;
}

/* (with-open-input-file (name filename [element-class]) form*) */
val sf_with_open_file(int kind, val form, val env) {
    val spec;
    val var, fname, elem = NIL, s, r, newenv;
    nlx_t f;
    long sp = gc_sp;
    if (!IS_CONS(CDR(form))) isl_error("<PROGRAM-ERROR>", NULL);
    spec = CAR(CDR(form));
    if (!IS_CONS(spec) || !IS_CONS(CDR(spec))) isl_error("<PROGRAM-ERROR>", NULL);
    if (!IS_SYM(CAR(spec))) err_domain(CAR(spec), "<symbol>");
    if (list_length(CDR(CDR(form))) < 0) isl_error("<PROGRAM-ERROR>", NULL);
    var = CAR(spec);
    gc_push(form); gc_push(env);
    fname = eval(CAR(CDR(spec)), env);
    gc_push(fname);
    if (IS_CONS(CDR(CDR(spec)))) { elem = eval(CAR(CDR(CDR(spec))), env); gc_push(elem); }
    s = open_file(kind, fname, elem);
    gc_push(s);
    newenv = mk_env(env);
    gc_push(newenv);
    {
        val cell = mk_cons(var, s);
        ENV(newenv)->vars = mk_cons(cell, ENV(newenv)->vars);
    }
    f.kind = NLX_UNWIND; f.tag = NIL; f.value = NIL; f.prev = nlx_top; f.dyn_sp = dyn_sp; f.gc_sp = gc_sp;
    f.handler = cur_handler; f.active = 1; f.pending = 0; f.pending_value = NIL; f.cleanup = NIL; f.env = NIL;
    nlx_top = &f;
    if (setjmp(f.jb) == 0) {
        r = eval_body(CDR(CDR(form)), newenv);
        nlx_top = f.prev;
        bi_close(1, &s);
    } else {
        nlx_t *target = f.pending;
        val v = f.pending_value;
        nlx_top = f.prev;
        bi_close(1, &s);
        nlx_throw(target, v);
        r = NIL;
    }
    gc_sp = sp;
    return r;
}

static val bi_read_char(int argc, val *argv) {
    val s = argc > 0 ? argv[0] : cur_stdin;
    int eos_error = argc > 1 ? argv[1] != NIL : 1;
    val eos_value = argc > 2 ? argv[2] : NIL;
    int c;
    check_char_input(s);
    c = stream_getc(s);
    if (c < 0) { if (eos_error) err_end_of_stream(s); return eos_value; }
    return MKCHR(c);
}
static val bi_preview_char(int argc, val *argv) {
    val s = argc > 0 ? argv[0] : cur_stdin;
    int eos_error = argc > 1 ? argv[1] != NIL : 1;
    val eos_value = argc > 2 ? argv[2] : NIL;
    int c;
    check_char_input(s);
    c = stream_peekc(s);
    if (c < 0) { if (eos_error) err_end_of_stream(s); return eos_value; }
    return MKCHR(c);
}
static val bi_read_line(int argc, val *argv) {
    val s = argc > 0 ? argv[0] : cur_stdin;
    int eos_error = argc > 1 ? argv[1] != NIL : 1;
    val eos_value = argc > 2 ? argv[2] : NIL;
    long cap = 128, n = 0;
    char *buf;
    int c;
    val r;
    check_char_input(s);
    c = stream_getc(s);
    if (c < 0) { if (eos_error) err_end_of_stream(s); return eos_value; }
    buf = malloc((size_t)cap);
    while (c >= 0 && c != '\n') {
        if (n + 1 >= cap) { cap *= 2; buf = realloc(buf, (size_t)cap); }
        buf[n++] = (char)c;
        c = stream_getc(s);
    }
    r = mk_string(buf, n);
    free(buf);
    return r;
}
static val bi_read(int argc, val *argv) {
    val s = argc > 0 ? argv[0] : cur_stdin;
    int eos_error = argc > 1 ? argv[1] != NIL : 1;
    val eos_value = argc > 2 ? argv[2] : NIL;
    int got;
    val v;
    check_char_input(s);
    v = read_from_stream(s, &got);
    if (!got) { if (eos_error) err_end_of_stream(s); return eos_value; }
    return v;
}
static val bi_stream_ready_p(int argc, val *argv) {
    (void)argc;
    check_char_input(argv[0]);
    if (STM(argv[0])->kind == S_STR_IN) return mk_bool(STM(argv[0])->peeked != -2 || STM(argv[0])->pos < STR(STM(argv[0])->str)->len);
    return mk_bool(stream_peekc(argv[0]) >= 0);
}
static val bi_read_byte(int argc, val *argv) {
    val s = argv[0];
    int eos_error = argc > 1 ? argv[1] != NIL : 1;
    val eos_value = argc > 2 ? argv[2] : NIL;
    int c;
    check_input(s);
    if (STM(s)->elem == 0) err_domain(s, "<stream>");
    c = stream_getc(s);
    if (c < 0) { if (eos_error) err_end_of_stream(s); return eos_value; }
    return MKFIX(c);
}
static val bi_write_byte(int argc, val *argv) {
    char b;
    (void)argc;
    if (!IS_FIX(argv[0]) || FIXVAL(argv[0]) < 0 || FIXVAL(argv[0]) > 255) err_domain(argv[0], "<integer>");
    check_output(argv[1]);
    if (STM(argv[1])->elem == 0) err_domain(argv[1], "<stream>");
    b = (char)FIXVAL(argv[0]);
    stream_write(argv[1], &b, 1);
    return argv[0];
}
static val bi_probe_file(int argc, val *argv) {
    FILE *fp;
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    fp = fopen(STR(argv[0])->data, "rb");
    if (!fp) return NIL;
    fclose(fp);
    return T;
}
static val bi_file_position(int argc, val *argv) {
    stream_t *s;
    (void)argc;
    if (!IS_OBJ(argv[0], T_STREAM)) err_domain(argv[0], "<stream>");
    s = STM(argv[0]);
    if (s->kind == S_STR_IN || s->kind == S_STR_OUT) return MKFIX(s->pos);
    if (!s->fp) err_stream(argv[0]);
    return int_from_long(ftell(s->fp) - (s->peeked != -2 ? 1 : 0));
}
static val bi_set_file_position(int argc, val *argv) {
    stream_t *s;
    (void)argc;
    if (!IS_OBJ(argv[0], T_STREAM)) err_domain(argv[0], "<stream>");
    if (!IS_FIX(argv[1]) || FIXVAL(argv[1]) < 0) err_domain(argv[1], "<integer>");
    s = STM(argv[0]);
    s->peeked = -2;
    if (s->kind == S_STR_IN || s->kind == S_STR_OUT) { s->pos = FIXVAL(argv[1]); return argv[1]; }
    if (!s->fp) err_stream(argv[0]);
    fseek(s->fp, FIXVAL(argv[1]), SEEK_SET);
    return argv[1];
}
static val bi_file_length(int argc, val *argv) {
    FILE *fp;
    long len;
    (void)argc;
    if (!IS_STR(argv[0])) err_domain(argv[0], "<string>");
    if (!IS_FIX(argv[1]) || FIXVAL(argv[1]) <= 0) err_domain(argv[1], "<integer>");
    if (FIXVAL(argv[1]) % 8 != 0) err_domain(argv[1], "<integer>");
    fp = fopen(STR(argv[0])->data, "rb");
    if (!fp) return NIL;
    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    fclose(fp);
    return int_from_long(len / (FIXVAL(argv[1]) / 8));
}
static val bi_finish_output(int argc, val *argv) { (void)argc; stream_flush(argv[0]); return NIL; }

void stream_init(void) {
    std_input = mk_stream(S_STD_IN); STM(std_input)->fp = stdin;
    std_output = mk_stream(S_STD_OUT); STM(std_output)->fp = stdout;
    std_error = mk_stream(S_STD_ERR); STM(std_error)->fp = stderr;
    cur_stdin = std_input; cur_stdout = std_output; cur_stderr = std_error;
    defbuiltin("FORMAT", bi_format, 2, -1);
    defbuiltin("FORMAT-CHAR", bi_format_char, 2, 2);
    defbuiltin("FORMAT-FLOAT", bi_format_float, 2, 2);
    defbuiltin("FORMAT-FRESH-LINE", bi_format_fresh_line, 1, 1);
    defbuiltin("FORMAT-INTEGER", bi_format_integer, 3, 3);
    defbuiltin("FORMAT-OBJECT", bi_format_object, 3, 3);
    defbuiltin("FORMAT-TAB", bi_format_tab, 2, 2);
    defbuiltin("STANDARD-INPUT", bi_standard_input, 0, 0);
    defbuiltin("STANDARD-OUTPUT", bi_standard_output, 0, 0);
    defbuiltin("ERROR-OUTPUT", bi_error_output, 0, 0);
    defbuiltin("STREAMP", bi_streamp, 1, 1);
    defbuiltin("OPEN-STREAM-P", bi_open_stream_p, 1, 1);
    defbuiltin("INPUT-STREAM-P", bi_input_stream_p, 1, 1);
    defbuiltin("OUTPUT-STREAM-P", bi_output_stream_p, 1, 1);
    defbuiltin("CREATE-STRING-INPUT-STREAM", bi_create_string_input_stream, 1, 1);
    defbuiltin("CREATE-STRING-OUTPUT-STREAM", bi_create_string_output_stream, 0, 0);
    defbuiltin("GET-OUTPUT-STREAM-STRING", bi_get_output_stream_string, 1, 1);
    defbuiltin("OPEN-INPUT-FILE", bi_open_input_file, 1, 2);
    defbuiltin("OPEN-OUTPUT-FILE", bi_open_output_file, 1, 2);
    defbuiltin("OPEN-IO-FILE", bi_open_io_file, 1, 2);
    defbuiltin("CLOSE", bi_close, 1, 1);
    defbuiltin("READ", bi_read, 0, 3);
    defbuiltin("READ-CHAR", bi_read_char, 0, 3);
    defbuiltin("PREVIEW-CHAR", bi_preview_char, 0, 3);
    defbuiltin("READ-LINE", bi_read_line, 0, 3);
    defbuiltin("STREAM-READY-P", bi_stream_ready_p, 1, 1);
    defbuiltin("READ-BYTE", bi_read_byte, 1, 3);
    defbuiltin("WRITE-BYTE", bi_write_byte, 2, 2);
    defbuiltin("PROBE-FILE", bi_probe_file, 1, 1);
    defbuiltin("FILE-POSITION", bi_file_position, 1, 1);
    defbuiltin("SET-FILE-POSITION", bi_set_file_position, 2, 2);
    defbuiltin("FILE-LENGTH", bi_file_length, 2, 2);
    defbuiltin("FINISH-OUTPUT", bi_finish_output, 1, 1);
}
