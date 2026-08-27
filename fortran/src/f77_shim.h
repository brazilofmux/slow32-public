/* f77_shim.h -- the small surface the copied SLOW-32 backend expects
 * from a frontend.  The backend is language-neutral apart from four
 * references to the C compiler's AST node (fn->locals_size, fn->name,
 * fn->next, prog->body); this supplies a Fortran-side equivalent so
 * the copied files stay byte-comparable with their selfhost originals.
 */
#ifndef F77_SHIM_H
#define F77_SHIM_H

/* A compiled unit: PROGRAM, SUBROUTINE or FUNCTION.  Field names match
 * what hir_codegen.h reads off the C AST. */
typedef struct Node {
    char        *name;         /* external symbol name */
    int          locals_size;  /* frame bytes for named locals */
    int          nparams;      /* dummy-argument count */
    int          is_varargs;   /* always 0: F77 has no varargs */
    struct Node *next;         /* next unit */
    struct Node *body;         /* on the program node: first unit */
} Node;

#endif
