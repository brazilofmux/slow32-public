static unsigned int r32(const unsigned char *p) {
    unsigned int v = 0u;
    v = v + (unsigned int)p[0];
    v = v + ((unsigned int)p[1] << 8);
    v = v + ((unsigned int)p[2] << 16);
    v = v + ((unsigned int)p[3] << 24);
    return v;
}

static int validate_view(const unsigned char *hdr, unsigned int file_size) {
    unsigned int in_nmembers;
    unsigned int in_mem_off;
    unsigned int in_str_off;
    unsigned int in_str_sz;

    if (hdr[0] != 'A' || hdr[1] != '2' || hdr[2] != '3' || hdr[3] != 'S') return 0;
    in_nmembers = r32(hdr + 8);
    in_mem_off = r32(hdr + 12);
    in_str_off = r32(hdr + 24);
    in_str_sz = r32(hdr + 28);

    if (in_nmembers > 64u) return 0;
    if (in_str_sz > 32768u) return 0;
    if (in_mem_off + in_nmembers * 24u > file_size) return 0;
    if (in_str_off + in_str_sz > file_size) return 0;
    return 1;
}

static int scan_layout(unsigned int n, unsigned int base) {
    unsigned int i = 0u;
    unsigned int acc = base;
    while (i < n) {
        if ((i & 1u) == 0u) {
            acc = acc + i + 3u;
        } else if ((i & 3u) == 1u) {
            acc = acc + i + 5u;
        } else if ((i & 7u) == 3u) {
            acc = acc + i + 7u;
        } else {
            acc = acc + i + 11u;
        }
        i = i + 1u;
    }
    return (int)(acc & 255u);
}

int main(void) {
    unsigned char hdr[32];
    int ok;
    int sig;

    hdr[0] = 'A'; hdr[1] = '2'; hdr[2] = '3'; hdr[3] = 'S';
    hdr[4] = 1; hdr[5] = 0; hdr[6] = 1; hdr[7] = 0;
    hdr[8] = 2; hdr[9] = 0; hdr[10] = 0; hdr[11] = 0;
    hdr[12] = 32; hdr[13] = 0; hdr[14] = 0; hdr[15] = 0;
    hdr[16] = 0; hdr[17] = 0; hdr[18] = 0; hdr[19] = 0;
    hdr[20] = 32; hdr[21] = 0; hdr[22] = 0; hdr[23] = 0;
    hdr[24] = 80; hdr[25] = 0; hdr[26] = 0; hdr[27] = 0;
    hdr[28] = 27; hdr[29] = 0; hdr[30] = 0; hdr[31] = 0;

    ok = validate_view(hdr, 136u);
    if (!ok) return 1;
    hdr[28] = 120;
    if (validate_view(hdr, 136u)) return 2;

    sig = scan_layout(17u, 9u);
    if (sig != 228) return 3;
    return 0;
}
