/* string.h -- s12cc-compatible stub */
#ifndef _STRING_H
#define _STRING_H

#include <stddef.h>

size_t strlen(char *s);
char *strcpy(char *dest, char *src);
char *strncpy(char *dest, char *src, size_t n);
int strcmp(char *s1, char *s2);
int strncmp(char *s1, char *s2, size_t n);
char *strcat(char *dest, char *src);
char *strncat(char *dest, char *src, size_t n);
char *strchr(char *s, int c);
char *strrchr(char *s, int c);
char *strstr(char *haystack, char *needle);
char *strdup(char *s);

void *memcpy(void *dest, void *src, size_t n);
void *memmove(void *dest, void *src, size_t n);
void *memset(void *s, int c, size_t n);
int memcmp(void *s1, void *s2, size_t n);
void *memchr(void *s, int c, size_t n);

#endif
