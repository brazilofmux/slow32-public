/* builtin_names.c -- the builtins' names and argument counts, without
 * their functions (builtins.def): enough for the checker (strict.c),
 * and for s32-bc, which links sbasic's front end on the host without
 * the interpreter. */
#include <string.h>
#include "builtin.h"

static const struct { const char *name; int min, max; } names[] = {
#define BUILTIN(name, fn, min, max) { name, min, max },
#include "builtins.def"
#undef BUILTIN
};

static int name_index(const char *name) {
    for (int i = 0; i < (int)(sizeof names / sizeof names[0]); i++)
        if (strcmp(names[i].name, name) == 0) return i;
    return -1;
}

int builtin_exists(const char *name) {
    return name_index(name) >= 0;
}

/* the arguments a builtin takes, at least *min and at most *max (-1:
 * any number); 0 when name is no builtin.  The counts are the ones each
 * function checks when it runs; strict.c holds a call to them at load
 * time, as QuickBASIC does (NBS P143-P155). */
int builtin_arity(const char *name, int *min, int *max) {
    int i = name_index(name);
    if (i < 0) return 0;
    *min = names[i].min;
    *max = names[i].max;
    return 1;
}
