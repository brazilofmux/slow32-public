#ifndef _CTYPE_H
#define _CTYPE_H

// Character classification functions
int isalnum(int c);   // alphanumeric character
int isalpha(int c);   // alphabetic character
int isblank(int c);   // blank character (space or tab)
int iscntrl(int c);   // control character
int isdigit(int c);   // decimal digit
int isgraph(int c);   // printing character except space
int islower(int c);   // lowercase letter
int isprint(int c);   // printing character including space
int ispunct(int c);   // punctuation character
int isspace(int c);   // white-space character
int isupper(int c);   // uppercase letter
int isxdigit(int c);  // hexadecimal digit

// Character conversion functions
int tolower(int c);   // convert to lowercase
int toupper(int c);   // convert to uppercase

// Non-standard but common
int isascii(int c);   // ASCII character
int toascii(int c);   // convert to ASCII

#endif