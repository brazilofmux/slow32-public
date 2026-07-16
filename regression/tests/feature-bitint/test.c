// Probe: exercise _BitInt at several widths through SLOW32 codegen.
// Operands come from volatile globals so -O2 cannot constant-fold the math.
#include <stdio.h>

typedef unsigned _BitInt(37) u37;
typedef _BitInt(66) s66;
typedef unsigned _BitInt(66) u66;
typedef unsigned _BitInt(128) u128;
typedef _BitInt(128) s128;
typedef unsigned _BitInt(200) u200;

volatile unsigned int seed_hi = 0x01234567u;
volatile unsigned int seed_lo = 0x89abcdefu;
volatile unsigned int three = 3u;
volatile unsigned int seven = 7u;

static void dump128(const char *tag, u128 v) {
  printf("%s %08lx%08lx%08lx%08lx\n", tag,
         (unsigned long)(v >> 96), (unsigned long)(v >> 64),
         (unsigned long)(v >> 32), (unsigned long)v);
}

int main(void) {
  // 37-bit: wraps mod 2^37
  u37 a37 = (u37)0x1FFFFFFFFFuwb;
  a37 += (u37)(three - 2u);
  printf("u37 wrap %02lx%08lx\n", (unsigned long)(a37 >> 32), (unsigned long)a37);

  // 66-bit mul/div/rem with runtime operands
  u66 b66 = ((u66)seed_hi << 32) | seed_lo;         // 0x0123456789abcdef
  u66 m66 = b66 * (u66)three;
  printf("u66 mul %01lx%08lx%08lx\n", (unsigned long)(m66 >> 64),
         (unsigned long)(m66 >> 32), (unsigned long)m66);
  u66 q66 = m66 / (u66)three;
  u66 r66 = m66 % (u66)seven;
  printf("u66 divrem %01lx%08lx%08lx r=%lu\n", (unsigned long)(q66 >> 64),
         (unsigned long)(q66 >> 32), (unsigned long)q66, (unsigned long)r66);

  s66 n66 = -(s66)b66;
  printf("s66 div %ld rem %ld\n", (long)(n66 / (s66)three % 1000000000wb),
         (long)(n66 % (s66)three));

  // 128-bit
  u128 x = ((u128)seed_hi << 96) | ((u128)seed_lo << 64) |
           ((u128)~seed_hi << 32) | (u128)~seed_lo;
  u128 y = ((u128)seven << 60) | (u128)three;
  dump128("u128 x", x);
  u128 p = x * (u128)three;
  dump128("u128 x*3", p);
  u128 q = x / y;
  u128 r = x % y;
  dump128("u128 q", q);
  dump128("u128 r", r);
  s128 sx = -(s128)x;
  dump128("s128 -x", (u128)sx);
  s128 sq = (s128)x / -(s128)y;
  dump128("s128 x/-y", (u128)sq);
  printf("cmp %d %d\n", (int)(x > y), (int)(sx < (s128)0));

  // shifts by runtime amounts
  dump128("u128 shl", x << (three * 5u + 2u));   // << 17
  dump128("u128 shr", x >> (seven * 3u + 2u));   // >> 23

  // 200-bit add/mul/div
  u200 big = 1uwb;
  for (unsigned i = 0; i < 24u; i++) big *= (u200)seven;   // 7^24
  big <<= 100;
  u200 bq = big / (u200)(seven * seven);
  u200 br = big % (u200)(seven * seven);
  printf("u200 q %08lx%08lx%08lx r=%lu\n", (unsigned long)(bq >> 164),
         (unsigned long)(bq >> 132), (unsigned long)(bq >> 100),
         (unsigned long)br);

  // fp conversions through 128 bits
  double d = (double)x;
  u128 back = (u128)d;
  dump128("u128 fpback", back);
  printf("fp %d\n", (int)(d > 0.0));

  return 0;
}
