#ifndef _SETJMP_H
#define _SETJMP_H

#ifdef __cplusplus
extern "C" {
#endif

/* jmp_buf stores callee-saved registers R11-R28 (18), SP (R29), FP (R30), LR (R31) = 21 words */
typedef int jmp_buf[21];

int setjmp(jmp_buf env);
void longjmp(jmp_buf env, int val);

#ifdef __cplusplus
}
#endif

#endif
