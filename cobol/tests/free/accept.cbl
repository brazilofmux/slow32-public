identification division.
program-id. acceptline.
*> ACCEPT identifier: a line from standard input, moved as text.
*> The harness types the .keys file into standard input for us and for
*> the oracle alike.
data division.
working-storage section.
01  ws-name          pic x(10).
01  ws-num           pic 9(4).
01  ws-short         pic x(3).
01  ws-amount        pic 9(3)v99.
procedure division.
main.
    accept ws-name.
    display 'name: [' ws-name ']'.
    accept ws-num.
    display 'num: ' ws-num.
    accept ws-short.
    display 'short: [' ws-short ']'.
    accept ws-amount.
    display 'amount: ' ws-amount.
    add 1 to ws-num.
    display 'num+1: ' ws-num.
    stop run.
