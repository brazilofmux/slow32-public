#ifndef _CTYPE_H
#define _CTYPE_H

// Character classification functions (ISO 8859-1 / Latin-1)
int isalnum(int c);   // alphanumeric character
int isalpha(int c);   // alphabetic character
int isblank(int c);   // blank character (space, tab, nbsp)
int iscntrl(int c);   // control character
int isdigit(int c);   // decimal digit
int isgraph(int c);   // printing character except space
int islower(int c);   // lowercase letter
int isprint(int c);   // printing character including space
int ispunct(int c);   // punctuation character
int isspace(int c);   // white-space character
int isupper(int c);   // uppercase letter
int isxdigit(int c);  // hexadecimal digit

// Character conversion functions (Latin-1 aware)
int tolower(int c);   // convert to lowercase
int toupper(int c);   // convert to uppercase

// Non-standard but common
int isascii(int c);   // ASCII character (0-127)
int toascii(int c);   // mask to 7 bits

// Classification table (for direct access)
extern const unsigned char __ctype_table[256];
extern const unsigned char __toupper_table[256];
extern const unsigned char __tolower_table[256];

#endif
