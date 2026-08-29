*>*****************************************************************
*> hello-free - majesty-shaped free-format source                 *
*>*****************************************************************
identification division.
program-id. hello-free.

environment division.
configuration section.
data division.

working-storage section.
77  bar61             pic x(61) value '-------------------------------------------------------------'.
77  option            pic xx value spaces.
01  ws-title          pic x(20) value 'General Ledger'.  *> a trailing comment

procedure division.
main-logic section.
begin.
    display bar61.
    display ws-title '[' option ']'.
    display "it's " 'a "quoted" thing'.
    display 'done' with no advancing
    display '.'.
    stop run.
end program hello-free.
