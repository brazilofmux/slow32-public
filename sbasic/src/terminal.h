#ifndef SBASIC_TERMINAL_H
#define SBASIC_TERMINAL_H

int sb_term_init(void);
void sb_term_shutdown(void);
void sb_term_cls(void);
void sb_term_locate(int row, int col);
void sb_term_color(int fg, int bg, int has_fg, int has_bg);
/* the attribute COLOR makes: its foreground, and 16 times its background */
int sb_term_attr(void);
/* the character ch in attribute attr at row, col (a POKE at B800), the
 * cursor put back at crow, ccol and COLOR's colors kept */
void sb_term_putat(int row, int col, int ch, int attr, int crow, int ccol);
int sb_term_inkey(char out[2]);

#endif
