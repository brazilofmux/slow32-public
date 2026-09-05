/* GitHub #31: a file-scope declarator list mixing scalars and arrays.
 * Each shape used to stop the parser at the comma after an array. */
int ga[3], gb[2];
extern char ha[], hb[];
char ha[4] = "abc", hb[4] = "xyz";
int s1, s2[2], *p1, s3;
static long la[2][3], lb;
int t2[2][2] = { { 1, 2 }, { 3, 4 } }, t3[3];
int main(void) {
    int rc = 0;
    ga[2] = 7; gb[1] = 9; if (ga[2] + gb[1] != 16) rc |= 1;
    if (ha[1] != 'b' || hb[2] != 'z') rc |= 2;
    s1 = 5; s2[1] = 6; p1 = &s1; s3 = *p1 + s2[1]; if (s3 != 11) rc |= 4;
    la[1][2] = 3; lb = la[1][2] * 2; if (lb != 6) rc |= 8;
    t3[0] = t2[1][1]; if (t3[0] != 4) rc |= 16;
    return rc;
}
