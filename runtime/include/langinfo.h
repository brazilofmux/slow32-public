/* runtime/include/langinfo.h (SLOW-32)
 * nl_langinfo: a locale's names and formats (locale.c). */
#ifndef _LANGINFO_H
#define _LANGINFO_H

#include <locale.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef int nl_item;

enum {
    CODESET,
    D_T_FMT, D_FMT, T_FMT, T_FMT_AMPM, AM_STR, PM_STR,
    DAY_1, DAY_2, DAY_3, DAY_4, DAY_5, DAY_6, DAY_7,
    ABDAY_1, ABDAY_2, ABDAY_3, ABDAY_4, ABDAY_5, ABDAY_6, ABDAY_7,
    MON_1, MON_2, MON_3, MON_4, MON_5, MON_6,
    MON_7, MON_8, MON_9, MON_10, MON_11, MON_12,
    ABMON_1, ABMON_2, ABMON_3, ABMON_4, ABMON_5, ABMON_6,
    ABMON_7, ABMON_8, ABMON_9, ABMON_10, ABMON_11, ABMON_12,
    ERA, ERA_D_FMT, ALT_DIGITS, ERA_D_T_FMT, ERA_T_FMT,
    RADIXCHAR, THOUSEP, YESEXPR, NOEXPR, CRNCYSTR,
    __S32_NL_ITEMS          /* how many there are */
};

char *nl_langinfo(nl_item item);
char *nl_langinfo_l(nl_item item, locale_t locale);

#ifdef __cplusplus
}
#endif

#endif /* _LANGINFO_H */
