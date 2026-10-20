#ifndef SLOW32_OPENSSL_RAND_H
#define SLOW32_OPENSSL_RAND_H

/* Random bytes from the host platform's generator (runtime/crypto_mmio.c):
 * 1, or 0 when the host offers none.  RAND_status says which. */

#ifdef __cplusplus
extern "C" {
#endif

int RAND_bytes(unsigned char *buf, int num);
int RAND_priv_bytes(unsigned char *buf, int num);
int RAND_status(void);

#ifdef __cplusplus
}
#endif

#endif
