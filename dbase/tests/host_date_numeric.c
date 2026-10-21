/* Host checks for date_to_jdn / field_format_numeric (GitHub issue 112). */
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "date.h"
#include "field.h"

static int fail;

static void expect_jdn0(const char *label, int32_t jdn) {
    if (jdn != 0) {
        fprintf(stderr, "FAIL %s: jdn=%d want 0\n", label, (int)jdn);
        fail = 1;
    }
}

static void expect_jdn_ok(const char *label, int y, int m, int d) {
    int32_t jdn = date_to_jdn(y, m, d);
    int y2, m2, d2;
    if (jdn == 0) {
        fprintf(stderr, "FAIL %s: blank\n", label);
        fail = 1;
        return;
    }
    date_from_jdn(jdn, &y2, &m2, &d2);
    if (y2 != y || m2 != m || d2 != d) {
        fprintf(stderr, "FAIL %s: roundtrip %d-%d-%d\n", label, y2, m2, d2);
        fail = 1;
    }
}

int main(void) {
    char buf[32];

    expect_jdn0("Feb 31 1986", date_to_jdn(1986, 2, 31));
    expect_jdn0("Apr 31", date_to_jdn(1986, 4, 31));
    expect_jdn0("month 0", date_to_jdn(1986, 0, 1));
    expect_jdn0("Feb 29 1985", date_to_jdn(1985, 2, 29));
    expect_jdn_ok("Feb 29 1984", 1984, 2, 29);
    expect_jdn_ok("Jan 15 1986", 1986, 1, 15);
    expect_jdn0("CTOD 02/31/86", date_from_mdy("02/31/86"));

    field_format_numeric(buf, 10, 0, "3000000000");
    if (strcmp(buf, "3000000000") != 0) {
        fprintf(stderr, "FAIL N 3000000000 got '%s'\n", buf);
        fail = 1;
    }
    field_format_numeric(buf, 5, 0, "3000000000");
    if (strcmp(buf, "*****") != 0) {
        fprintf(stderr, "FAIL N overflow stars got '%s'\n", buf);
        fail = 1;
    }
    field_format_numeric(buf, 10, 2, "12.5");
    if (strcmp(buf, "     12.50") != 0) {
        fprintf(stderr, "FAIL N 12.50 got '%s'\n", buf);
        fail = 1;
    }

    if (fail) {
        return 1;
    }
    printf("ok\n");
    return 0;
}
