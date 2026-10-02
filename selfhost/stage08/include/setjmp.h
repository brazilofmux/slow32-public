/* setjmp.h -- for SLOW-32 (runtime/setjmp.s, the one both libraries
 * assemble): the registers a function keeps for its caller, r11-r28,
 * then sp, fp and lr.  21 words.  The cross compilers' libraries have
 * no setjmp. */
#ifndef _SETJMP_H
#define _SETJMP_H

typedef int jmp_buf[21];

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

#endif
