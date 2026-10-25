/* Usual arithmetic conversions (C99 6.3.1.8): an unsigned operand makes
 * the result unsigned only when its rank reaches the other operand's.
 * unsigned int with long long is long long; unsigned char or short
 * promotes to int.  The parser and sema both said "unsigned if either
 * side is", so SQLite's SMALLEST_INT64 -- (long long)-1 minus
 * (0xffffffff | hi<<32) -- was unsigned, its divisions went to
 * __udivdi3, and every integer multiply overflowed into floating point
 * (selfhost ISSUES-95).  Each line checks a value; a wrong one fails
 * the run. */
#include <stdio.h>
typedef long long i64;
#define LARGEST_INT64  (0xffffffff|(((i64)0x7fffffff)<<32))
#define SMALLEST_INT64 (((i64)-1) - LARGEST_INT64)

static int fails = 0;
static void chk(const char *what, i64 got, i64 want) {
    if (got != want) {
        printf("FAIL %s: got %lld want %lld\n", what, got, want);
        fails = fails + 1;
    }
}

int mul_ovf(i64 *pA, i64 iB){
  i64 iA = *pA;
  if( iB>0 ){
    if( iA>LARGEST_INT64/iB ) return 1;
    if( iA<SMALLEST_INT64/iB ) return 1;
  }else if( iB<0 ){
    if( iA>0 ){
      if( iB<SMALLEST_INT64/iA ) return 1;
    }else if( iA<0 ){
      if( iB==SMALLEST_INT64 ) return 1;
      if( iA==SMALLEST_INT64 ) return 1;
      if( -iA>LARGEST_INT64/-iB ) return 1;
    }
  }
  *pA = iA*iB;
  return 0;
}

i64 sdiv(i64 b){ return SMALLEST_INT64 / b; }
i64 smod(i64 b){ return SMALLEST_INT64 % b; }

int main(void){
  unsigned int u = 0xffffffffu;
  unsigned char uc = 200;
  unsigned short us = 60000;
  i64 x = 7;
  i64 a;
  int r;
  /* unsigned int meets long long: long long, signed compare */
  chk("-1LL < (0xffffffffU|0LL)", (-1LL < (0xffffffffU | 0LL)) ? 1 : 0, 1);
  chk("-1LL < (u|x)", (-1LL < (u | x)) ? 1 : 0, 1);
  chk("-1LL < (u+x)", (-1LL < (u + x)) ? 1 : 0, 1);
  /* int meets unsigned int: unsigned; unsigned long long stays unsigned */
  chk("-1 < 0xffffffffU", (-1 < 0xffffffffU) ? 1 : 0, 0);
  chk("-1LL < ULLONG_MAX", (-1LL < 0xffffffffffffffffULL) ? 1 : 0, 0);
  chk("-1 < u", (-1 < u) ? 1 : 0, 0);
  /* unsigned char and short promote to int */
  chk("uc < -1", (uc < -1) ? 1 : 0, 0);
  chk("us < -1", (us < -1) ? 1 : 0, 0);
  chk("uc / -2", uc / -2, -100);
  chk("us / -3", us / -3, -20000);
  chk("(uc+100) / -2", (uc + 100) / -2, -150);
  /* division and remainder with the common type long long */
  chk("(0xffffffffU|0LL) / -1LL", (0xffffffffU | 0LL) / -1LL, -4294967295LL);
  chk("(u*1LL) / -5", (u * 1LL) / -5, -858993459LL);
  chk("SMALLEST/5", SMALLEST_INT64 / 5, -1844674407370955161LL);
  chk("SMALLEST/-3", SMALLEST_INT64 / -3, 3074457345618258602LL);
  chk("SMALLEST%7", SMALLEST_INT64 % 7, -1);
  chk("sdiv(5)", sdiv(5), -1844674407370955161LL);
  chk("sdiv(-3)", sdiv(-3), 3074457345618258602LL);
  chk("smod(7)", smod(7), -1);
  chk("smod(-2^32)", smod(-4294967296LL), 0);
  chk("(u|x) >> 33", (u | x) >> 33, 0);
  chk("((i64)u << 4) >> 2", ((i64)u << 4) >> 2, 17179869180LL);
  /* SQLite's overflow test, the shape that found this */
  a = 5; r = mul_ovf(&a, 7919); chk("5*7919 ovf", r, 0); chk("5*7919", a, 39595);
  a = -5; r = mul_ovf(&a, 7919); chk("-5*7919 ovf", r, 0); chk("-5*7919", a, -39595);
  a = 3037000500LL; r = mul_ovf(&a, 3037000500LL); chk("big*big ovf", r, 1);
  a = -3037000499LL; r = mul_ovf(&a, 3037000499LL); chk("-big*big ovf", r, 0);
  chk("-big*big", a, -9223372030926249001LL);
  a = SMALLEST_INT64; r = mul_ovf(&a, -1); chk("SMALLEST*-1 ovf", r, 1);
  if (fails) return 1;
  printf("usual-conv ok\n");
  return 0;
}
