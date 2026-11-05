#ifndef SBASIC_CHAIN_H
#define SBASIC_CHAIN_H
#include "ast.h"
/* the program in the file at path; NULL with its error said */
stmt_t *chain_parse(const char *path);
#endif
