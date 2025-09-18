#ifndef CONVERT_H
#define CONVERT_H

#include <stddef.h>
#include <stdbool.h>

// Integer to string conversion functions
size_t ltoa(int val, char *buf);
size_t utoa(unsigned int uval, char *buf);
size_t utoo(unsigned int uval, char *buf);
size_t utox(unsigned int uval, char *buf, bool uppercase);

// Hex digit tables
extern const char Digits16U[17];
extern const char Digits16L[17];

#endif // CONVERT_H