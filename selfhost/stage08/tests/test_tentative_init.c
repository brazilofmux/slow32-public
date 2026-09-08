/* GitHub issue 46: a file-scope definition followed by a tentative
 * declaration must keep the initializer.  `int x; int x = 7;` (the
 * SQLite order) already worked; `int x = 7; int x;` zeroed it because
 * add_defined_global always called ps_reset_global_init. */
int keep_v = 7;
int keep_v;

int tent_v;
int tent_v = 7;

int zero_v = 0;
int zero_v;

long long keep_ll = 9;
long long keep_ll;

char *keep_s = "ok";
char *keep_s;

int keep_a[3] = { 1, 2, 3 };
int keep_a[3];

int main(void) {
    if (keep_v != 7) return 1;
    if (tent_v != 7) return 2;
    if (zero_v != 0) return 3;
    if (keep_ll != 9) return 4;
    if (keep_s[0] != 'o' || keep_s[1] != 'k' || keep_s[2] != 0) return 5;
    if (keep_a[0] != 1 || keep_a[1] != 2 || keep_a[2] != 3) return 6;
    return 0;
}
