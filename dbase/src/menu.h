#ifndef MENU_H
#define MENU_H

#define MAX_POPUPS 16
#define MAX_BARS   32

typedef struct {
    int number;           /* bar number (1-based, user-assigned) */
    char prompt[80];
    char message[160];
    int skip;             /* 1 = always skip */
    char skip_expr[256];  /* SKIP FOR expression (empty = no skip) */
} bar_entry_t;

typedef struct {
    char name[32];        /* uppercased */
    int from_row, from_col;
    int to_row, to_col;   /* 0,0 = auto-size */
    int defined;
    bar_entry_t bars[MAX_BARS];
    int nbar;
} popup_t;

/* Initialize popup subsystem */
void menu_init(void);

/* Define a popup menu. Returns pointer or NULL on error. */
popup_t *menu_define_popup(const char *name, int r1, int c1, int r2, int c2);

/* Define a bar within a named popup. Returns 0 on success, -1 on error. */
int menu_define_bar(const char *popup_name, int bar_num, const char *prompt,
                    const char *message, const char *skip_expr);

/* Activate (display and run lightbar) a named popup. Returns bar# or 0=Esc. */
int menu_activate_popup(const char *name);

/* Deactivate the currently active popup (exit from ON SELECTION handler). */
void menu_deactivate_popup(void);

/* Release a named popup. */
void menu_release_popup(const char *name);

/* Release all popups. */
void menu_release_all(void);

/* Last selected bar number (0 if none). */
int menu_last_bar(void);

/* Last selected prompt text (empty string if none). */
const char *menu_last_prompt(void);

#endif
