int write(int fd, char *buf, int len);

static void print_int(int v) {
    char buf[12];
    int i;
    i = 11;
    buf[i] = '\n';
    if (v == 0) { i = i - 1; buf[i] = '0'; }
    while (v > 0) { i = i - 1; buf[i] = '0' + (v % 10); v = v / 10; }
    write(1, buf + i, 12 - i);
}

struct emu {
    unsigned int r[32];
    unsigned int pc;
    unsigned char *mem;
    unsigned int mem_total;
    unsigned int code_limit;
    unsigned int mmio_base;
    int halted;
    int host_fds[128];
};

int main(int argc, char **argv) {
    struct emu e;
    char *base;
    base = (char *)&e;

    write(1, "r: ", 3); print_int((int)((char *)&e.r - base));
    write(1, "pc: ", 4); print_int((int)((char *)&e.pc - base));
    write(1, "mem: ", 5); print_int((int)((char *)&e.mem - base));
    write(1, "mem_total: ", 11); print_int((int)((char *)&e.mem_total - base));
    write(1, "code_limit: ", 12); print_int((int)((char *)&e.code_limit - base));
    write(1, "mmio_base: ", 11); print_int((int)((char *)&e.mmio_base - base));
    write(1, "halted: ", 8); print_int((int)((char *)&e.halted - base));
    write(1, "host_fds: ", 10); print_int((int)((char *)&e.host_fds - base));
    return 0;
}
