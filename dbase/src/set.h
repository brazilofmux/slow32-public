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
} set_options_t;

void set_init(set_options_t *opts);
void set_execute(set_options_t *opts, const char *arg);
void set_display(const set_options_t *opts);

#endif
