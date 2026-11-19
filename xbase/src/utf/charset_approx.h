/* The DFAs in charset_approx.c (xbase/sync-libutf.sh): run over a
 * character's UTF-8 bytes from the start state, the state reached at or
 * past ACCEPTING_STATES_START, less that, is the byte in the codepage. */
#ifndef XBASE_CHARSET_APPROX_H
#define XBASE_CHARSET_APPROX_H
#define TR_CP437_START_STATE (0)
#define TR_CP437_ACCEPTING_STATES_START (114)
#define TR_LATIN1_START_STATE (0)
#define TR_LATIN1_ACCEPTING_STATES_START (101)
extern const unsigned char  tr_cp437_itt[256];
extern const unsigned short tr_cp437_sot[114];
extern const unsigned short tr_cp437_sbt[4065];
extern const unsigned char  tr_latin1_itt[256];
extern const unsigned short tr_latin1_sot[101];
extern const unsigned short tr_latin1_sbt[3488];
#endif
