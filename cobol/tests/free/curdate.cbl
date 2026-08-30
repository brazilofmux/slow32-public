*> FUNCTION CURRENT-DATE through the guest clock: the shape is checked,
*> not the value.
identification division.
program-id. curdate.
data division.
working-storage section.
01  ts.
    05  ts-year  pic 9999.
    05  ts-rest  pic x(12).
    05  ts-sign  pic x.
    05  ts-off   pic 9999.
01  y   pic 9999.
procedure division.
    move function current-date to ts.
    move ts-year to y.
    if y >= 2026 and y < 2100 display 'year ok' else display 'year? ' y end-if.
    if ts-rest is numeric display 'rest ok' else display 'rest? ' ts-rest end-if.
    if ts-sign = '+' or ts-sign = '-' display 'sign ok' else display 'sign? ' ts-sign end-if.
    if ts-off is numeric display 'offset ok' else display 'offset? ' ts-off end-if.
    display function length(function current-date).
    stop run.
