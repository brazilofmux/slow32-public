      $SET SOURCEFORMAT"FREE" NOLIST
*> A Micro Focus directive line (BP-E30): $SET SOURCEFORMAT"FREE" in the
*> indicator column switches to free form for the rest of the text, as
*> >>SOURCE FORMAT FREE does; NOLIST, a listing directive, has no effect.
*> A line that begins $$ is a picture going on, not a directive.
*> abrignoli_COBSOFT's 45 programs in X-COBOL begin so (ISSUES 120).
identification division.
program-id. dollarset.
data division.
working-storage section.
01 amount pic
$$$,$$9.99 value " $1,234.50".
procedure division.
display "free form after $SET"
display amount
$set sourceformat(fixed)
           DISPLAY "FIXED AGAIN".
           STOP RUN.
