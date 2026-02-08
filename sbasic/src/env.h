#ifndef SBASIC_ENV_H
#define SBASIC_ENV_H

#include "value.h"
#include "error.h"

/* Hash table entry for variable storage */
typedef struct var_entry {
    char name[64];
    value_t value;
    int is_const;           /* 1 if CONST variable */
    struct var_entry *next;
} var_entry_t;

#define ENV_HASH_SIZE 127

/* Environment (scope) */
typedef struct env {
    var_entry_t *table[ENV_HASH_SIZE];
    struct env *parent;    /* enclosing scope (NULL for global) */
} env_t;

/* Create/destroy environment */
env_t *env_create(env_t *parent);
void env_destroy(env_t *env);

/* Get variable value. Auto-creates with default if not found in any scope.
   Returns pointer to stored value (valid until next env operation). */
value_t *env_get(env_t *env, const char *name);

/* Set variable in current scope */
void env_set(env_t *env, const char *name, const value_t *val);

/* Set variable in global (root) scope */
void env_set_global(env_t *env, const char *name, const value_t *val);

/* Check if variable exists in this scope (not parent) */
int env_has_local(env_t *env, const char *name);

/* Set variable as const (creates and marks const) */
void env_set_const(env_t *env, const char *name, const value_t *val);

/* Check if a variable is const (searches scope chain) */
int env_is_const(env_t *env, const char *name);

#endif
