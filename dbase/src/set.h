#ifndef SET_H
#define SET_H

typedef enum {
    DATE_AMERICAN,   /* MM/DD/YY */
    DATE_ANSI,       /* YY.MM.DD */
    DATE_BRITISH,    /* DD/MM/YY */
    DATE_FRENCH,     /* DD/MM/YY */
    DATE_GERMAN,     /* DD.MM.YY */
    DATE_ITALIAN,    /* DD-MM-YY */
    DATE_JAPAN       /* YY/MM/DD */
} date_format_t;

typedef struct set_options {
    int talk;
    int deleted;
    int exact;
    int heading;
    int confirm;
    int bell;
    int safety;
    int console;
    int decimals;
    date_format_t date_format;
    int device;       /* 0 = SCREEN, 1 = PRINT */
    int century;      /* 0=OFF (2-digit year), 1=ON (4-digit year) */
    int escape;       /* 1=ON (Esc interrupts programs) */
    int echo;         /* 1=ON (echo lines during DO) */
    int margin;       /* left margin for printer output (0-254) */
} set_options_t;

void set_init(set_options_t *opts);
void set_execute(set_options_t *opts, const char *arg);
void set_display(const set_options_t *opts);

#endif
