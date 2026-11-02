#include "env.h"
#include <stdlib.h>
#include <string.h>

val_type_t (*env_deftype_hook)(char first_letter) = NULL;

static unsigned int hash_name(const env_t *env, const char *name) {
    unsigned int h = 0;
    while (*name) {
        h = h * 31 + (unsigned char)*name;
        name++;
    }
    return h % env->nbuckets;
}

env_t *env_create_sized(env_t *parent, unsigned nbuckets) {
    env_t *env = calloc(1, sizeof(env_t) + nbuckets * sizeof(var_entry_t *));
    if (!env) return NULL;
    env->parent = parent;
    env->nbuckets = nbuckets;
    return env;
}

env_t *env_create(env_t *parent) {
    return env_create_sized(parent, ENV_HASH_SIZE);
}

void env_destroy(env_t *env) {
    if (!env) return;
    env_clear(env);
    free(env);
}

unsigned env_generation = 0;

void env_clear(env_t *env) {
    if (!env) return;
    env_generation++;
    for (unsigned i = 0; i < env->nbuckets; i++) {
        var_entry_t *e = env->table[i];
        while (e) {
            var_entry_t *next = e->next;
            if (!e->link) val_clear(&e->value);
            free(e);
            e = next;
        }
        env->table[i] = NULL;
    }
}

/* Find entry in a single scope */
static var_entry_t *find_entry(env_t *env, const char *name) {
    unsigned int h = hash_name(env, name);
    var_entry_t *e = env->table[h];
    while (e) {
        if (strcmp(e->name, name) == 0)
            return e;
        e = e->next;
    }
    return NULL;
}

/* Create entry in a scope */
static var_entry_t *create_entry(env_t *env, const char *name) {
    unsigned int h = hash_name(env, name);
    var_entry_t *e = malloc(sizeof(var_entry_t));
    if (!e) return NULL;
    /* the fields set one by one: an entry is made for every variable of
     * every call, and clearing the whole name buffer twice was a fifth
     * of a FUNCTION call */
    int len = (int)strlen(name);
    if (len > 63) len = 63;
    memcpy(e->name, name, len);
    e->name[len] = '\0';
    e->is_const = 0;
    e->link = NULL;
    len = (int)strlen(name);
    /* Default value based on suffix, or DEFTYPE for bare names */
    if (len > 0 && name[len - 1] == '$')
        e->value = val_string_cstr("");
    else if (len > 0 && name[len - 1] == '%')
        e->value = val_integer(0);
    else if (len > 0 && name[len - 1] == '&')
        e->value = val_long(0);
    else if (len > 0 && name[len - 1] == '#')
        e->value = val_double(0.0);
    else if (len > 0 && env_deftype_hook)
        e->value = val_default(env_deftype_hook(name[0]));
    else
        e->value = val_single(0.0);
    e->next = env->table[h];
    env->table[h] = e;
    return e;
}

value_t *env_lookup(env_t *env, const char *name) {
    for (env_t *scope = env; scope; scope = scope->parent) {
        var_entry_t *e = find_entry(scope, name);
        if (e) return e->link ? e->link : &e->value;
    }
    return NULL;
}

value_t *env_get(env_t *env, const char *name) {
    /* Search up the scope chain */
    env_t *scope = env;
    while (scope) {
        var_entry_t *e = find_entry(scope, name);
        if (e) return e->link ? e->link : &e->value;
        scope = scope->parent;
    }
    /* Auto-create in current (innermost) scope */
    var_entry_t *e = create_entry(env, name);
    return e ? &e->value : NULL;
}

void env_set(env_t *env, const char *name, const value_t *val) {
    var_entry_t *e = find_entry(env, name);
    if (!e)
        e = create_entry(env, name);
    if (e) {
        if (e->link) val_assign(e->link, val);
        else val_assign(&e->value, val);
    }
}

void env_set_global(env_t *env, const char *name, const value_t *val) {
    /* Walk to root */
    while (env->parent)
        env = env->parent;
    env_set(env, name, val);
}

int env_has_local(env_t *env, const char *name) {
    return find_entry(env, name) != NULL;
}

int env_exists(env_t *env, const char *name) {
    env_t *scope = env;
    while (scope) {
        if (find_entry(scope, name)) return 1;
        scope = scope->parent;
    }
    return 0;
}

void env_set_const(env_t *env, const char *name, const value_t *val) {
    var_entry_t *e = find_entry(env, name);
    if (!e)
        e = create_entry(env, name);
    if (e) {
        if (e->link) val_assign(e->link, val);
        else val_assign(&e->value, val);
        e->is_const = 1;
    }
}

int env_is_const(env_t *env, const char *name) {
    env_t *scope = env;
    while (scope) {
        var_entry_t *e = find_entry(scope, name);
        if (e) return e->is_const;
        scope = scope->parent;
    }
    return 0;
}

void env_link(env_t *env, const char *name, value_t *target) {
    var_entry_t *e = find_entry(env, name);
    if (!e) e = create_entry(env, name);
    if (e) {
        val_clear(&e->value);
        e->link = target;
    }
}
