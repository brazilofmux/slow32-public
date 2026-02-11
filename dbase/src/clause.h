#ifndef CLAUSE_H
#define CLAUSE_H

#include <stdint.h>

typedef enum { 
    SCOPE_DEFAULT, 
    SCOPE_ALL_DEFAULT, 
    SCOPE_ALL, 
    SCOPE_NEXT, 
    SCOPE_RECORD, 
    SCOPE_REST 
} scope_type_t;

typedef struct { 
    scope_type_t type; 
    uint32_t count; 
} scope_t;

typedef struct {
    scope_t scope;
    int has_scope;
    char for_cond[256];
    char while_cond[256];
    int to_print;
    char to_file[64];
    int off;            /* OFF keyword (suppress recno) */
    char heading[256];  /* HEADING keyword */
    int summary;        /* SUMMARY keyword */
    int plain;          /* PLAIN keyword */
    int noeject;        /* NOEJECT keyword */
} clause_t;

#endif
