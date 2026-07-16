// Emulated TLS runtime for SLOW-32.
//
// LLVM lowers thread_local variables to __emutls_v.<name> control blocks
// (the triple defaults to -femulated-tls) and routes every access through
// __emutls_get_address. SLOW-32 is single-threaded today, so "per-thread"
// storage is simply allocated once on first touch and cached in the control
// block. When preemptive threads arrive, replace the object cache with a
// per-thread index table (see compiler-rt's emutls.c for the shape).
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

typedef struct {
  unsigned int size;   // size of the variable in bytes
  unsigned int align;  // alignment of the variable
  void *object;        // storage, allocated on first access
  void *templ;         // initial image, or NULL to zero-initialize
} __emutls_control;

void *__emutls_get_address(__emutls_control *ctl) {
  void *p = ctl->object;
  if (!p) {
    unsigned int align = ctl->align;
    if (align < sizeof(void *))
      align = sizeof(void *);
    // Over-allocate and round up; the block is never freed, so the raw
    // pointer does not need to be retained.
    char *raw = malloc(ctl->size + align - 1);
    p = (void *)(((uintptr_t)raw + align - 1) & ~(uintptr_t)(align - 1));
    if (ctl->templ)
      memcpy(p, ctl->templ, ctl->size);
    else
      memset(p, 0, ctl->size);
    ctl->object = p;
  }
  return p;
}
