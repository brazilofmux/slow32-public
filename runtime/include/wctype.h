/* wctype.h -- C11 7.30, for both SLOW-32 C libraries (runtime/include and
 * selfhost/stage08/include hold the same file).  The classes and case
 * mappings are ASCII's in the "C" locale and Unicode's in the UTF-8 ones
 * (runtime/wctype.c; the tables are runtime/utf/wctype_tables.h). */
#ifndef _SLOW32_WCTYPE_H_
#define _SLOW32_WCTYPE_H_

#ifdef __cplusplus
extern "C" {
#endif

#ifndef _WINT_T_DEFINED
#define _WINT_T_DEFINED
typedef unsigned int wint_t;
#endif
typedef unsigned int wctype_t;      /* a set of classes; 0 is none */
typedef unsigned int wctrans_t;     /* 1 toupper, 2 tolower; 0 is none */

#ifndef WEOF
#define WEOF ((wint_t)-1)
#endif

int iswalnum(wint_t wc);
int iswalpha(wint_t wc);
int iswblank(wint_t wc);
int iswcntrl(wint_t wc);
int iswdigit(wint_t wc);
int iswgraph(wint_t wc);
int iswlower(wint_t wc);
int iswprint(wint_t wc);
int iswpunct(wint_t wc);
int iswspace(wint_t wc);
int iswupper(wint_t wc);
int iswxdigit(wint_t wc);
int iswctype(wint_t wc, wctype_t desc);
wctype_t wctype(const char *property);
wint_t towlower(wint_t wc);
wint_t towupper(wint_t wc);
wint_t towctrans(wint_t wc, wctrans_t desc);
wctrans_t wctrans(const char *property);

#ifdef __cplusplus
}
#endif

#endif
