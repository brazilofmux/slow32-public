#include <stdio.h>
#include <string.h>
#include "memvar.h"
#include "util.h"

void memvar_init(memvar_store_t *store) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++)
        store->vars[i].used = 0;
    store->count = 0;
}

int memvar_find(const memvar_store_t *store, const char *name, value_t *val) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (store->vars[i].used && str_icmp(store->vars[i].name, name) == 0) {
            *val = store->vars[i].val;
            return 0;
        }
    }
    return -1;
}

int memvar_set(memvar_store_t *store, const char *name, const value_t *val) {
    int i;
    char uname[MEMVAR_NAMELEN];

    str_copy(uname, name, MEMVAR_NAMELEN);
    str_upper(uname);

    /* Update existing */
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (store->vars[i].used && str_icmp(store->vars[i].name, uname) == 0) {
            store->vars[i].val = *val;
            return 0;
        }
    }

    /* Find free slot */
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) {
            str_copy(store->vars[i].name, uname, MEMVAR_NAMELEN);
            store->vars[i].val = *val;
            store->vars[i].used = 1;
            store->count++;
            return 0;
        }
    }

    printf("Too many memory variables.\n");
    return -1;
}

int memvar_release(memvar_store_t *store, const char *name) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++) {
        if (store->vars[i].used && str_icmp(store->vars[i].name, name) == 0) {
            store->vars[i].used = 0;
            store->count--;
            return 0;
        }
    }
    return -1;
}

void memvar_release_all(memvar_store_t *store) {
    int i;
    for (i = 0; i < MEMVAR_MAX; i++)
        store->vars[i].used = 0;
    store->count = 0;
}

void memvar_display(const memvar_store_t *store) {
    int i;
    char buf[256];
    int shown = 0;

    for (i = 0; i < MEMVAR_MAX; i++) {
        if (!store->vars[i].used) continue;
        val_to_string(&store->vars[i].val, buf, sizeof(buf));
        switch (store->vars[i].val.type) {
        case VAL_NUM:
            printf("%-10s  N  %s\n", store->vars[i].name, buf);
            break;
        case VAL_CHAR:
            printf("%-10s  C  \"%s\"\n", store->vars[i].name, buf);
            break;
        case VAL_DATE:
            printf("%-10s  D  {%s}\n", store->vars[i].name, buf);
            break;
        case VAL_LOGIC:
            printf("%-10s  L  %s\n", store->vars[i].name, buf);
            break;
        default:
            printf("%-10s  U  %s\n", store->vars[i].name, buf);
            break;
        }
        shown++;
    }
    printf("%d variable(s) defined.\n", shown);
}
