identification division.
program-id. rwclass.
*> A reserved word never names a SPECIAL-NAMES class; cobol ISSUES-43.
environment division.
configuration section.
special-names.
    class space is 'A' thru 'Z'.
procedure division.
    stop run.
