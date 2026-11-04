#ifndef SBASIC_MACHINE_H
#define SBASIC_MACHINE_H
/* machine.c: the PC's devices at their addresses and ports (no CPU) */
#include "error.h"

extern int def_seg;             /* DEF SEG's segment */

/* PEEK and POKE at DEF SEG's segment, offset (-32768..65535) */
error_t pc_peek(int offset, int *out);
error_t pc_poke(int offset, int v);
/* INP, OUT, WAIT */
error_t pc_inp(int port, int *out);
error_t pc_out(int port, int v);
error_t pc_wait(int port, int and_mask, int xor_mask);
/* a program's start */
void pc_reset(void);

#endif
