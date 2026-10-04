*> SET ENVIRONMENT name TO value (GnuCOBOL's; BP-E31), read back by
*> ACCEPT ... FROM ENVIRONMENT, from a literal and from items with
*> trailing spaces (GnuCOBOL drops them from both).  ACAS (cobol
*> ISSUES-124) sets COB_SCREEN_ESC and friends this way.
identification division.
program-id. setenv.
data division.
working-storage section.
01  ws-got     pic x(20).
01  ws-name    pic x(30) value "S32_SETENV_TEST".
01  ws-val     pic x(10) value "abc".
procedure division.
    set environment "S32_SETENV_LIT" to "Y".
    accept ws-got from environment "S32_SETENV_LIT".
    display "[" ws-got "]".
    set environment ws-name to ws-val.
    accept ws-got from environment "S32_SETENV_TEST".
    display "[" ws-got "]".
    set environment "S32_SETENV_TEST" to "second".
    accept ws-got from environment ws-name.
    display "[" ws-got "]".
    goback.
