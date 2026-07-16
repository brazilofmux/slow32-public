// Probe: C11 atomics and _Thread_local through the SLOW-32 toolchain.
#include <stdatomic.h>
#include <stdio.h>

_Thread_local int tls_counter = 41;       // template path (__emutls_t)
_Thread_local char tls_buf[8];            // zero-init path
_Atomic int ai = 5;
_Atomic long long all = 0x100000001LL;
typedef struct { int a[4]; } Big;
_Atomic Big big;                          // oversized -> __atomic_* libcalls

int main(void) {
  tls_counter += 1;
  tls_buf[0] = 'T';
  printf("tls %d %c%d\n", tls_counter, tls_buf[0], (int)tls_buf[7]);

  atomic_fetch_add(&ai, 10);
  printf("ai %d\n", atomic_load(&ai));

  int expected = 15;
  int ok = atomic_compare_exchange_strong(&ai, &expected, 99);
  printf("cas %d %d\n", ok, atomic_load(&ai));
  expected = 100;
  ok = atomic_compare_exchange_strong(&ai, &expected, 7);
  printf("cas2 %d %d %d\n", ok, expected, atomic_load(&ai));

  int old = atomic_exchange(&ai, 123);
  printf("xchg %d %d\n", old, atomic_load(&ai));

  atomic_fetch_add(&all, 0xFFFFFFFFLL);
  long long v = atomic_load(&all);
  printf("all %08lx%08lx\n", (unsigned long)(v >> 32), (unsigned long)v);

  atomic_thread_fence(memory_order_seq_cst);
  atomic_flag f = ATOMIC_FLAG_INIT;
  int was = atomic_flag_test_and_set(&f);
  printf("flag %d %d\n", was, atomic_flag_test_and_set(&f));

  Big b1 = {{1, 2, 3, 4}};
  atomic_store(&big, b1);
  Big b2 = atomic_load(&big);
  printf("big %d %d\n", b2.a[0], b2.a[3]);

  printf("lockfree int %d big %d\n", (int)atomic_is_lock_free(&ai),
         (int)atomic_is_lock_free(&big));
  return 0;
}
