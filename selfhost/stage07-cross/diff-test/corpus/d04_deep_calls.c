/* Deep call chain with two-arg leaf functions.  Each leaf is the
 * exact shape of an s32fast h_X handler: e->r[di->rd] = ...
 * Exercises the PARAM-natural-home path that the two-pass color fix
 * targets.  Also stresses the prologue need_temp logic. */
struct emu  { unsigned int r[8]; unsigned int pc; };
struct dinst { unsigned int rd, rs1, rs2; int imm; };

static void h_add(struct emu *e, struct dinst *di) {
    e->r[di->rd] = e->r[di->rs1] + e->r[di->rs2];
    e->pc += 4;
}
static void h_sub(struct emu *e, struct dinst *di) {
    e->r[di->rd] = e->r[di->rs1] - e->r[di->rs2];
    e->pc += 4;
}
static void h_xor(struct emu *e, struct dinst *di) {
    e->r[di->rd] = e->r[di->rs1] ^ e->r[di->rs2];
    e->pc += 4;
}
static void h_sll(struct emu *e, struct dinst *di) {
    e->r[di->rd] = e->r[di->rs1] << (e->r[di->rs2] & 31);
    e->pc += 4;
}

int main(void) {
    struct emu e;
    struct dinst di;
    int i;
    for (i = 0; i < 8; i++) e.r[i] = i + 1;
    e.pc = 0;
    di.rd = 0; di.rs1 = 1; di.rs2 = 2;
    for (i = 0; i < 100; i++) {
        h_add(&e, &di); di.rd = (di.rd + 1) & 7; di.rs1 = (di.rs1 + 1) & 7; di.rs2 = (di.rs2 + 1) & 7;
        h_sub(&e, &di); di.rd = (di.rd + 1) & 7; di.rs1 = (di.rs1 + 1) & 7; di.rs2 = (di.rs2 + 1) & 7;
        h_xor(&e, &di); di.rd = (di.rd + 1) & 7; di.rs1 = (di.rs1 + 1) & 7; di.rs2 = (di.rs2 + 1) & 7;
        h_sll(&e, &di); di.rd = (di.rd + 1) & 7; di.rs1 = (di.rs1 + 1) & 7; di.rs2 = (di.rs2 + 1) & 7;
    }
    unsigned int acc = 0;
    for (i = 0; i < 8; i++) acc = acc * 31u + e.r[i];
    acc ^= e.pc;
    return acc & 0xff;
}
