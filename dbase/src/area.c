#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "area.h"
#include "util.h"

static work_area_t areas[MAX_AREAS];
static int current_area_idx = 0;

void area_init_all(void) {
    int i;
    memset(areas, 0, sizeof(areas));
    for (i = 0; i < MAX_AREAS; i++) {
        dbf_init(&areas[i].db);
        areas[i].relation_target = -1;
    }
    current_area_idx = 0;
}

int area_get_current_idx(void) {
    return current_area_idx;
}

void area_set_current_idx(int idx) {
    if (idx >= 0 && idx < MAX_AREAS) {
        current_area_idx = idx;
    }
}

work_area_t *area_get(int idx) {
    if (idx >= 0 && idx < MAX_AREAS) {
        return &areas[idx];
    }
    return NULL;
}

work_area_t *area_get_current(void) {
    return &areas[current_area_idx];
}

int area_resolve_alias(const char *alias) {
    int i;
    char c;

    if (!alias || !alias[0]) return -1;

    /* 1. Try numeric 1-10 */
    if (alias[0] >= '1' && alias[0] <= '9') {
        int val = atoi(alias);
        if (val >= 1 && val <= MAX_AREAS)
            return val - 1;
    }
    if (strcmp(alias, "10") == 0) return 9;

    /* 2. Try single letter A-J */
    c = alias[0];
    if (c >= 'a' && c <= 'z') c -= 32;
    if (c >= 'A' && c <= 'J' && alias[1] == '\0')
        return c - 'A';

    /* 3. Try named alias */
    for (i = 0; i < MAX_AREAS; i++) {
        if (areas[i].alias[0] && str_icmp(areas[i].alias, alias) == 0)
            return i;
    }

    return -1;
}

dbf_t *area_lookup_dbf(const char *alias) {
    int idx = area_resolve_alias(alias);
    if (idx >= 0) return &areas[idx].db;
    return NULL;
}
