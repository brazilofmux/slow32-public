/* main.c -- the driver.
 *
 *   islisp [-l libdir] file.lsp ...     run each file's toplevel forms in order
 *   islisp [-l libdir]                  a read-eval-print loop on standard input
 *
 * An unhandled error in a file ends the run with status 1 after the
 * report (§9.1 leaves this to the implementation); in the loop it
 * returns to the prompt.  ISLISP_LIB names the library directory the
 * `import` extension searches; -l overrides it.
 */
#include "isl.h"
#include <stdlib.h>
#include <string.h>

int isl_argc;
char **isl_argv;
const char *isl_libdir = "lib";

val load_file(const char *path, int echo) {
    FILE *fp = fopen(path, "rb");
    long len, pos = 0;
    char *src;
    val r = NIL;
    if (!fp) {
        val name = mk_string(path, (long)strlen(path));
        isl_error("<STREAM-ERROR>", sym_stream, name, NULL);
    }
    fseek(fp, 0, SEEK_END);
    len = ftell(fp);
    fseek(fp, 0, SEEK_SET);
    src = malloc((size_t)len + 1);
    if (fread(src, 1, (size_t)len, fp) != (size_t)len) { fclose(fp); free(src); fatal("short read"); }
    src[len] = 0;
    fclose(fp);
    while (pos < len) {
        long used;
        int got;
        val form = read_from_string(src + pos, len - pos, &used, &got);
        pos += used;
        if (!got) break;
        if (getenv("ISLISP_TRACE_TOP")) {   /* which toplevel form is running: for finding a stop */
            val str = print_to_string(form, 1);
            fputs("TOP ", stderr);
            fwrite(STR(str)->data, 1, STR(str)->len > 160 ? 160 : (size_t)STR(str)->len, stderr);
            fputc('\n', stderr);
        }
        r = eval(form, global_env);
        if (echo) { print_to_stream(cur_stdout, r, 1); stream_putc(cur_stdout, '\n'); }
    }
    free(src);
    return r;
}

static void repl(void) {
    for (;;) {
        nlx_t f;
        f.kind = NLX_TOPLEVEL; f.tag = NIL; f.value = NIL; f.prev = nlx_top; f.dyn_sp = dyn_sp;
        f.gc_sp = gc_sp; f.handler = cur_handler; f.active = 1; f.pending = 0;
        f.pending_value = NIL; f.cleanup = NIL; f.env = NIL;
        nlx_top = &f;
        if (setjmp(f.jb) == 0) {
            int got;
            val form, r;
            stream_puts(cur_stdout, "> ");
            stream_flush(cur_stdout);
            form = read_from_stream(cur_stdin, &got);
            if (!got) { nlx_top = f.prev; break; }
            r = eval(form, global_env);
            print_to_stream(cur_stdout, r, 1);
            stream_putc(cur_stdout, '\n');
            nlx_top = f.prev;
        } else {
            nlx_top = f.prev;
            cur_handler = NIL;
        }
    }
}

int main(int argc, char **argv) {
    int i;
    int ran = 0;
    volatile uintptr_t stack_top = 0;
    const char *env_lib = getenv("ISLISP_LIB");
    gc_stack_base = (val *)&stack_top;
    isl_argc = argc; isl_argv = argv;
    if (env_lib) isl_libdir = env_lib;
    if (getenv("ISLISP_LINEBUF")) setvbuf(stdout, NULL, _IOLBF, 0);   /* for finding where a run stops */
    heap_init();
    symbol_init();
    eval_init();
    stream_init();
    class_init();
    cond_init();
    builtins_init();
    seq_init();
    number_init();
    for (i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-l") == 0 && i + 1 < argc) { isl_libdir = argv[++i]; continue; }
        load_file(argv[i], 0);
        ran = 1;
    }
    if (!ran) repl();
    stream_flush(cur_stdout);
    stream_flush(cur_stderr);
    return 0;
}
