C     ** A character or Hollerith constant continued from a line that
C     ends short of column 72.  The line is blank out to 72, as its card
C     was, and the blanks belong to the constant; the reader had joined
C     the continuation right after the last character, so a file with
C     trimmed lines compiled differently from the same deck.  'AB' ends
C     in column 13, so 59 blanks follow and 'CD' starts at position 62.
C     'XY' ends in column 22, so the Hollerith count 53 takes 'XY', 50
C     blanks and 'Z'.  Found by fort370.  The continued lines below
C     must stay trimmed.  Self-checking; success reaches STOP 0.
      CHARACTER*80 S
      S = 'AB
     1CD'
      IF (INDEX(S,'CD') .NE. 62) STOP 1
      WRITE (6,100) INDEX(S,'CD')
  100 FORMAT (1X,'CONTSTR',I4)
      WRITE (6,200)
  200 FORMAT (1X,53HXY
     1Z,'<')
      STOP 0
      END
