/* Static and global mutable state.  Tests addressing of
 * globals/statics (HI_GADDR / HI_SADDR), which take a different
 * regalloc path from stack-local addressing.  The state is
 * deliberately observable (returned via accumulator) so a wrong
 * load/store ordering would change the output. */

static int s_counter = 0;
static int s_history[16];
static int s_history_pos = 0;

int g_total = 0;

static int bump(int delta) {
    s_counter = s_counter + delta;
    s_history[s_history_pos & 15] = s_counter;
    s_history_pos = s_history_pos + 1;
    g_total = g_total + delta;
    return s_counter;
}

static int summarize(void) {
    int acc = 0;
    int i;
    for (i = 0; i < 16; i++) {
        acc = acc * 7 + s_history[i];
    }
    return acc ^ s_counter ^ g_total ^ s_history_pos;
}

int main(void) {
    int i;
    for (i = 0; i < 50; i++) {
        bump(i);
        bump(-i / 2);
        bump(i * 3);
    }
    return summarize() & 0xff;
}
