*> SET condition-name TO FALSE (2023 14.9.39, format 4): the conditional
*> variable takes the literal of the condition-name's FALSE phrase
*> (13.18.63 format 3: [WHEN SET TO] FALSE IS literal-4), numeric and
*> alphanumeric; the condition is false afterwards.
*> docs/conformance/value.md
identification division.
program-id. condfalse.
data division.
working-storage section.
01 st   pic x value "Y".
   88 is-on  value "Y" "y" when set to false is "N".
01 lvl  pic 99 value 5.
   88 low    value 1 thru 9 false 50.
procedure division.
    display st " " lvl
    set is-on to false
    set low to false
    display st " " lvl
    if is-on display "on" else display "off" end-if
    if low display "low" else display "not low" end-if
    set is-on low to true
    display st " " lvl
    stop run.
