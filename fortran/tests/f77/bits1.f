C     MIL-STD-1753 bit functions, compiled in line, on constants and
C     on values the compiler cannot see; MVBITS; and hexadecimal and
C     octal constants in DATA.  Bit 0 is the rightmost.
      PROGRAM BITS1
      INTEGER KA, KB, KC, KD, KE, KF, KG, N, I, Z9
      INTEGER W(4)
      DATA KA /Z'0000F0F0'/, KB /O'377'/, KC /Z'7FFFFFFF'/
      DATA KD /Z'FFFFFFFF'/
      DATA W /Z'1', Z'A5', O'7', Z'C0DE'/
      IF (KA .NE. 61680 .OR. KB .NE. 255) STOP 1
      IF (KC .NE. 2147483647 .OR. KD .NE. -1) STOP 2
      IF (W(1)+W(2)+W(3)+W(4) .NE. 1+165+7+49374) STOP 3
C     constants
      IF (IAND(12, 10) .NE. 8 .OR. IOR(12, 10) .NE. 14) STOP 4
      IF (IEOR(12, 10) .NE. 6 .OR. NOT(0) .NE. -1) STOP 5
      IF (ISHFT(3, 4) .NE. 48 .OR. ISHFT(48, -4) .NE. 3) STOP 6
      IF (ISHFT(-1, -28) .NE. 15 .OR. ISHFT(5, 32) .NE. 0) STOP 7
      IF (IBITS(KA, 4, 8) .NE. 15 .OR. IBITS(KA, 0, 0) .NE. 0) STOP 8
      IF (.NOT. BTEST(KA, 4) .OR. BTEST(KA, 3)) STOP 9
      IF (IBSET(0, 31) .NE. -2147483647-1) STOP 10
      IF (IBCLR(KD, 0) .NE. -2) STOP 11
      IF (ISHFTC(1, 1, 3) .NE. 2 .OR. ISHFTC(4, 1, 3) .NE. 1) STOP 12
      IF (ISHFTC(KB, -4, 8) .NE. 255) STOP 13
      IF (ISHFTC(Z9(6), 2, 4) .NE. 9) STOP 14
C     values from a loop, so nothing folds
      N = 0
      DO 10 I = 0, 31
         IF (BTEST(KA, I)) N = N + 1
   10 CONTINUE
      IF (N .NE. 8) STOP 15
      N = 0
      DO 20 I = -32, 32
         N = IEOR(N, ISHFT(KC, I))
   20 CONTINUE
      DO 25 I = -32, 32, 64
         IF (ISHFT(KC, I) .NE. 0) STOP 19
   25 CONTINUE
      KE = 0
      DO 30 I = 0, 3
         KE = IOR(KE, ISHFT(IBITS(KA, 4 * I, 4), 4 * (3 - I)))
   30 CONTINUE
      IF (KE .NE. 3855) STOP 16
      KF = 4
      KG = -1
      CALL MVBITS(KF, 1, 2, KG, 8)
      IF (KG .NE. -257) STOP 17
      KG = 0
      CALL MVBITS(KA, 4, 0, KG, 3)
      IF (KG .NE. 0) STOP 18
      WRITE (6, 100) N, KE, KG
  100 FORMAT (' BITS ', 3I12)
      STOP 0
      END
      INTEGER FUNCTION Z9(K)
      Z9 = K
      END
