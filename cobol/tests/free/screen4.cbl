*> Nested screen groups: a group's look (colours, attributes) reaches
*> its children, its LINE/COLUMN anchor the first of them, and a named
*> group is a screen of its own for DISPLAY and ACCEPT -- a window
*> into the parent's slots. The group's AUTO reaches only the input
*> field, not the VALUE beside it. No oracle: screens need a tty.
identification division.
program-id. screen4.
environment division.
configuration section.
special-names.
    crt status is ws-crt.
data division.
working-storage section.
77  ws-crt  pic 9(4) value 0.
77  nm      pic x(4) value 'bob '.
77  am      pic 99 value 7.
screen section.
01  s.
    05  blank screen.
    05  line 1 column 5 value 'HDR' highlight.
    05  grp line 3 column 10 foreground-color 2 reverse-video auto.
        10  value 'Name:'.
        10  column plus 2 pic x(4) using nm.
        10  ln2 line plus 1 underline.
            15  column 10 value 'Amt:'.
            15  column plus 3 pic 99 using am.
    05  line 6 column 1 value 'tail'.
procedure division.
    display s.
    accept grp.
    display 'crt=' ws-crt ' nm=[' nm '] am=' am.
    accept ln2.
    display 'crt=' ws-crt ' am=' am.
    stop run.
end program screen4.
