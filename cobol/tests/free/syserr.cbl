*> DISPLAY UPON SYSERR, STDERR, or a mnemonic-name for SYSERR, writes to
*> the error stream; before 2026-09-30 it went to stdout with the rest.
*> The harness keeps stdout only, so the lines that must not appear here
*> are the "err" ones -- including a SYSERR DISPLAY written WITH NO
*> ADVANCING, and one in an IF.  GnuCOBOL sends SYSERR to stderr too.
identification division.
program-id. syserr.
environment division.
configuration section.
special-names.
    syserr is err-out.
data division.
working-storage section.
01 t pic x(3) value "abc".
01 n pic 9 value 1.
procedure division.
    display "out 1 " t
    display "err 1 " t(2:1) upon syserr
    display "err 2" upon err-out
    display "err 3 " upon syserr with no advancing
    display "tail" upon syserr
    if n = 1 display "out 2" else display "err 4" upon syserr end-if
    display "out 3"
    stop run.
