*> The binary-* usages hold their minimum ranges (2023 13.18.60.4 rule
*> 12), SIGNED by default, UNSIGNED when written (13.18.60.2):
*> -128..127 and 0..255, -32768..32767 and 0..65535, -2**31..2**31-1
*> and 0..2**32-1.  docs/conformance/usage.md
identification division.
program-id. binranges.
data division.
working-storage section.
01 c1 binary-char.
01 c2 binary-char unsigned.
01 s1 binary-short.
01 s2 binary-short unsigned.
01 s3 binary-short signed.
01 l1 binary-long.
01 l2 binary-long unsigned.
01 l3 binary-long signed.
procedure division.
    move -128 to c1 display c1
    move 127 to c1 display c1
    move 255 to c2 display c2
    move -32768 to s1 display s1
    move 32767 to s1 display s1
    move 65535 to s2 display s2
    move -1 to s3 display s3
    move -2147483648 to l1 display l1
    move 2147483647 to l1 display l1
    move 4294967295 to l2 display l2
    move -5 to l3 display l3
    compute l2 = l2 - 1 display l2
    compute c2 = c2 - 55 display c2
    stop run.
