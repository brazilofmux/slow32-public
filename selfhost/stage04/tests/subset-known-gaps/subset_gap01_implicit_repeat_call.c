/*
 * Known-gap repro: cc.fth implicit-function long-call bug.
 *
 * Uses fopen/fclose without declarations (implicit auto-declare path).
 * The second call to the same function may be miscompiled as variable access,
 * causing runtime execute faults.
 */
int main(int argc, char **argv) {
    write(1, "a", 1);
    write(1, "b", 1);
    return 0;
}
