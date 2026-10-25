/* __has_include(<name>) and __has_include("name") are 1 exactly when
 * #include would find the file, by the same search; __has_include is
 * itself "defined", so the usual guard works, and it composes with the
 * rest of an #if expression. */
#if defined(__has_include)
#  if __has_include(<termios.h>)
#    include <termios.h>
#    define GOT_TERMIOS 1
#  else
#    define GOT_TERMIOS 0
#  endif
#else
#  define GOT_TERMIOS -1
#endif

#ifdef __has_include
#  define GUARD_IFDEF 1
#else
#  define GUARD_IFDEF 0
#endif

#ifndef __has_include
#  define GUARD_IFNDEF 1
#else
#  define GUARD_IFNDEF 0
#endif

#if __has_include(<no_such_header_here.h>)
#  define MISSING_ANGLE 1
#else
#  define MISSING_ANGLE 0
#endif

#if __has_include("no_such_local_file.h")
#  define MISSING_QUOTED 1
#else
#  define MISSING_QUOTED 0
#endif

/* this file itself, by the quoted search */
#if __has_include( "test_has_include.c" ) && GOT_TERMIOS == 1
#  define SELF 1
#else
#  define SELF 0
#endif

#if !__has_include(<stdio.h>)
#  define STDIO_GONE 1
#elif __has_include(<stdio.h>) || 0
#  define STDIO_GONE 0
#else
#  define STDIO_GONE 2
#endif

int main(void) {
    int r;
    r = 0;
#if GOT_TERMIOS == 1
    {
        struct termios t;
        t.c_lflag = ECHO | ICANON;
        if (t.c_lflag != 0x11) r = 1;
    }
#endif
    if (GOT_TERMIOS != 1) return 2;
    if (GUARD_IFDEF != 1 || GUARD_IFNDEF != 0) return 3;
    if (MISSING_ANGLE != 0 || MISSING_QUOTED != 0) return 4;
    if (SELF != 1) return 5;
    if (STDIO_GONE != 0) return 6;
    return r;
}
