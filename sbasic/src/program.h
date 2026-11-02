#ifndef SBASIC_PROGRAM_H
#define SBASIC_PROGRAM_H

/* The program as loaded: its lines, $INCLUDE files spliced in.  Both
 * the interpreter (main.c) and the compiler (s32-sbc) load a program
 * through here, so they see the same text. */

void program_clear(void);
void program_add_line(const char *line);
/* the line, or the lines of the file a $INCLUDE on it names */
void program_add_line_with_includes(const char *line, int depth);
/* a file's lines, after program_clear; -1 when it cannot be read */
int program_load_file(const char *filename);
int program_line_count(void);
const char *program_line(int i);
void program_delete_lines(int from, int to);   /* 1-based, inclusive */
/* the lines joined, each ended by \n (malloc'd) */
char *program_get_text(void);

#endif
