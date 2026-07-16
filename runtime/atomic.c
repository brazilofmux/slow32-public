// Generic __atomic_* libcall support for SLOW-32.
//
// Atomics up to 64 bits are lowered inline to plain loads/stores by the
// backend (single-hart machine: program order IS memory order). Clang only
// emits these size-generic libcalls for oversized atomic objects (larger
// than 8 bytes, e.g. std::atomic<big struct>). On a single-hart machine a
// plain copy is atomic with respect to every observer, so these are simple
// memcpy wrappers. Revisit if thread service routines become preemptive.
//
// Clang treats the __atomic_* names as builtins and refuses direct
// definitions, so define *_c names and rename them at the object level,
// the same trick compiler-rt's atomic.c uses.
#include <stdbool.h>
#include <stddef.h>
#include <string.h>

#pragma redefine_extname __atomic_load_c __atomic_load
#pragma redefine_extname __atomic_store_c __atomic_store
#pragma redefine_extname __atomic_exchange_c __atomic_exchange
#pragma redefine_extname __atomic_compare_exchange_c __atomic_compare_exchange
#pragma redefine_extname __atomic_is_lock_free_c __atomic_is_lock_free

void __atomic_load_c(size_t size, const volatile void *src, void *dest,
                     int model) {
  (void)model;
  memcpy(dest, (const void *)src, size);
}

void __atomic_store_c(size_t size, volatile void *dest, void *src, int model) {
  (void)model;
  memcpy((void *)dest, src, size);
}

void __atomic_exchange_c(size_t size, volatile void *ptr, void *val, void *old,
                         int model) {
  (void)model;
  memcpy(old, (void *)ptr, size);
  memcpy((void *)ptr, val, size);
}

bool __atomic_compare_exchange_c(size_t size, volatile void *ptr,
                                 void *expected, void *desired, int success,
                                 int failure) {
  (void)success;
  (void)failure;
  if (memcmp((void *)ptr, expected, size) == 0) {
    memcpy((void *)ptr, desired, size);
    return true;
  }
  memcpy(expected, (void *)ptr, size);
  return false;
}

bool __atomic_is_lock_free_c(size_t size, const volatile void *ptr) {
  (void)ptr;
  return size <= 8;
}
