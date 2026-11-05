#ifndef SBASIC_MUSIC_H
#define SBASIC_MUSIC_H
#include "error.h"
/* PLAY s$, silently: Illegal function call for what a string may not say */
error_t music_play(const char *s, int len);
/* PLAY(n): the notes not yet begun */
int music_count(void);
void music_reset(void);
#endif
