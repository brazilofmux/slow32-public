typedef struct {
    char *mnemonic;
    unsigned int opcode;
    int fmt;
} info_t;

static info_t infos[] = {
    {"add", 0, 1},
    {"sub", 1, 2}
};

int main(void) {
    if (infos[0].mnemonic[1] != 'd') return 2;
    if (infos[1].opcode != 1) return 3;
    if (infos[1].fmt != 2) return 4;
    return 1;
}
