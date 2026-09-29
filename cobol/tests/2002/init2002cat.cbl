*> INITIALIZE category TO VALUE (2023 14.9.20.4 GR 5c1): only items of
*> the named category with a VALUE clause take their value; with no
*> REPLACING or DEFAULT phrase the others are left alone, and a
*> REPLACING phrase applies to the items the VALUE phrase does not take.
*> ALL TO VALUE sets a data pointer to NULL (GR 6a1); REPLACING
*> DATA-POINTER BY is a SET.  docs/conformance/initialize.md
*> No oracle: GnuCOBOL restores every VALUE whatever the category named,
*> leaves a pointer alone under TO VALUE, and has no DATA-POINTER phrase.
identification division.
program-id. init2002cat.
data division.
working-storage section.
01 g.
   05 a  pic x(3) value "abc".
   05 n  pic 9(3) value 7.
   05 e  pic zz9 value " 42".
   05 m  pic 9(2).
   05 t  pic x(2) value "tt".
01 w  pic 9 value 1.
01 p  usage pointer.
01 p2 usage pointer.
procedure division.
    move all "#" to g
    initialize g numeric to value
    display "numeric:  [" g "]"
    move all "#" to g
    initialize g numeric-edited to value
    display "edited:   [" g "]"
    move all "#" to g
    initialize g numeric to value then replacing numeric data by 5 alphanumeric data by "k"
    display "num+rep:  [" g "]"
    move all "#" to g
    initialize g alphanumeric to value then to default
    display "alnum+def:[" g "]"
    set p to address of w
    initialize p all to value
    if p = null display "pointer:  null" else display "pointer:  set" end-if
    set p to address of w
    initialize p
    if p = null display "default:  null" else display "default:  set" end-if
    set p2 to address of g
    initialize p replacing data-pointer by p2
    if p = address of g display "replaced: g" else display "replaced: other" end-if
    stop run.
