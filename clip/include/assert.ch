// assert.ch -- ASSERT( condition [, message] ): the program stops with
// the place and the message when the condition fails; nothing under NDEBUG
#ifdef NDEBUG
#command ASSERT( <exp> [, <msg>] ) =>
#else
#command ASSERT( <exp> [, <msg>] ) => IF !( <exp> ) ; OUTSTD( CHR(13) + CHR(10) + PROCNAME() + "(" + LTRIM( STR( PROCLINE() ) ) + ")  Assertion failed: " + IIF( <.msg.>, <msg>, <"exp"> ) ) ; QUIT ; END
#endif
