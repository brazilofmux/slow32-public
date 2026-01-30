#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SIZE (64 * 1024)

int main() {
    int errors = 0;
    
    // Test large fwrite (exceeds MMIO buffer)
    char *buf = malloc(SIZE);
    if (!buf) return 1;
    
    for (int i = 0; i < SIZE; i++) buf[i] = (char)(i & 0xFF);
    
    // We can't easily verify the output content in this test harness without file I/O readback.
    // But we can check return values and ensuring it doesn't crash.
    // Also, we can write to a temporary file and read it back.
    
    FILE *f = fopen("test_large.dat", "w");
    if (!f) { printf("fopen w failed\n"); return 1; }
    
    size_t written = fwrite(buf, 1, SIZE, f);
    if (written != SIZE) { printf("fwrite large failed: %d\n", (int)written); errors++; }
    
    if (fclose(f) != 0) { printf("fclose w failed\n"); errors++; }
    
    // Read back
    f = fopen("test_large.dat", "r");
    if (!f) { printf("fopen r failed\n"); return 1; }
    
    char *buf2 = malloc(SIZE);
    size_t read = fread(buf2, 1, SIZE, f);
    if (read != SIZE) { printf("fread large failed: %d\n", (int)read); errors++; }
    
    if (memcmp(buf, buf2, SIZE) != 0) { printf("data mismatch\n"); errors++; }
    
    fclose(f);
    free(buf2);
    
    // Test buffering logic
    // Write small chunks, check if file is created but maybe not fully flushed until fclose?
    // Hard to test flush timing from within the guest without observing the host.
    // But we can test correctness.
    
    f = fopen("test_buf.dat", "w");
    // Write 1 byte
    fputc('A', f);
    // Write 4096 bytes (fill buffer)
    for (int i = 0; i < 4096; i++) fputc('x', f);
    // Should have flushed at least once
    fclose(f);
    
    f = fopen("test_buf.dat", "r");
    if (fgetc(f) != 'A') { printf("fgetc failed\n"); errors++; }
    for (int i = 0; i < 4096; i++) {
        if (fgetc(f) != 'x') { printf("fgetc loop failed at %d\n", i); errors++; break; }
    }
    if (fgetc(f) != EOF) { printf("expected EOF\n"); errors++; }
    fclose(f);
    
    // Clean up
    remove("test_large.dat");
    remove("test_buf.dat");
    
    if (errors == 0) printf("All stdio buffering tests passed\n");
    free(buf);
    
    return errors != 0;
}
