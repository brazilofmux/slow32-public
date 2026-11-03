/* A frame over 2KB keeps fp and is reached from both ends: a slot out of
 * fp's 12-bit reach is addressed from sp (hcg_dual_base).  The base goes
 * with the last offset asked for, so storing the address of a local near
 * sp into a slot near fp must form the value before the store's offset:
 * xBase's cmd_join stored to sp + the fp offset, and its JOIN wrote
 * nothing. */
int look(char **slots, char *big, char *small) {
    if (slots[0] != small) return 1;
    if (slots[1] != big + 2000) return 2;
    if (small[3] != 'z' || big[2999] != 'y') return 3;
    return 0;
}

int join(int n) {
    char *slots[4];          /* first declared: nearest fp */
    char big[3000];
    char small[16];          /* nearest sp, past fp's reach */
    small[3] = 'z';
    big[2999] = 'y';
    slots[0] = small;        /* STORE(slot near fp, address near sp) */
    slots[1] = big + 2000;
    slots[2] = 0;
    slots[3] = 0;
    return look(slots, big, small) + n;
}

int main(void) {
    return join(0);
}
