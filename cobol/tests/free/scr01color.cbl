*> Colours on the 01 screen entry, inherited by every entry below it as
*> a group's are (2023 13.18.4.4 rule 3, 13.18.23.4 rule 3): a field
*> that sets none takes both; one that sets its foreground keeps the
*> 01's background; a nested group's own colour composes over the 01's.
*> ACAS (cobol ISSUES-124) colours whole menus this way.  The ANSI stream
*> is the expected output.
*> No oracle: GnuCOBOL's screens need a real tty.
identification division.
program-id. scr01color.
data division.
working-storage section.
screen section.
01  menu  background-color 1 foreground-color 2.
    03  value "TITLE"   line 1 col 1.
    03  value "WHITE"   line 2 col 1 foreground-color 7.
    03  grp foreground-color 6.
        05  value "YELLOW" line 3 col 1.
procedure division.
    display menu
    stop run.
