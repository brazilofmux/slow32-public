#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static long file_size_for(FILE *f) {
    long pos = ftell(f);
    if (pos < 0) return -1;
    if (fseek(f, 0, SEEK_END) != 0) return -1;
    long size = ftell(f);
    fseek(f, pos, SEEK_SET);
    return size;
}

int main(int argc, char **argv) {
    const char *path;
    FILE *f;
    long sz;
    uint8_t *data;

    if (argc < 4) {
        printf("need args\n");
        return 1;
    }

    path = argv[3];
    printf("path=%s\n", path);
    f = fopen(path, "rb");
    if (!f) {
        printf("open fail\n");
        return 1;
    }

    sz = file_size_for(f);
    printf("size=%ld\n", sz);
    if (sz <= 0) {
        fclose(f);
        return 1;
    }
    if (fseek(f, 0, SEEK_SET) != 0) {
        fclose(f);
        return 1;
    }

    data = (uint8_t *)malloc((size_t)sz);
    if (!data) {
        printf("malloc fail\n");
        fclose(f);
        return 1;
    }

    if (fread(data, 1, (size_t)sz, f) != (size_t)sz) {
        printf("read fail\n");
        free(data);
        fclose(f);
        return 1;
    }
    fclose(f);

    printf("magic chars=%c%c%c%c\n", data[0], data[1], data[2], data[3]);
    printf("magic hex=%02x %02x %02x %02x\n", data[0], data[1], data[2], data[3]);
    free(data);
    return 0;
}
