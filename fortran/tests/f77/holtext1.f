C     ** Text in numeric variables, as FORTRAN 66 and IBM's compilers
C     keep it: Hollerith data in INTEGER, REAL and DOUBLE PRECISION
C     items, written and read with A editing.  Aw takes the first w
C     bytes of the item as it lies in storage, or pads on the left if w
C     is larger; on input a larger w keeps the rightmost bytes.  The
C     runtime had handed such an item to the number writer, which
C     printed stars, and stopped on input.  Found by fort370 in a pi
C     spigot from H390-MVS.  Self-checking; success reaches STOP 0.
      INTEGER D/1H5/, E(2)/1HA,4HWXYZ/, R, S
      REAL X/4HABCD/
      DOUBLE PRECISION Y/8HABCDEFGH/
      CHARACTER*10 C/'QRSTUVWXYZ'/
      CHARACTER*40 L
      READ (C,'(A4,A6)') R, S
      WRITE (L,100) D, E, E(2), X, Y, R, R, S
  100 FORMAT (A1,A1,A4,A2,1X,A4,1X,A8,1X,A4,1X,A6,1X,A4)
      IF (L .NE. '5AWXYZWX ABCD ABCDEFGH QRST   QRST WXYZ') STOP 1
      WRITE (6,'(1X,A)') L
      STOP 0
      END
