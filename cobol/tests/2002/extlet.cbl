*> Extended letters in user-defined words (2023 8.1.3, Annex B; docs/plans/
*> standard-queue.md item 47): Latin-1, Greek, Cyrillic and CJK names, a
*> hyphenated one, a paragraph; matched without regard to case through the
*> simple pairs (Ö/ö, Π/π, Ч/ч), ß and a final sigma being pairs of nothing.
*> No oracle: GnuCOBOL 4 refuses the words.
identification division.
program-id. extlet.
data division.
working-storage section.
01 Größe pic 9(3) value 42.
01 ποσό pic 9(2) value 7.
01 Число pic 9(2) value 3.
01 数量 pic 9(2) value 5.
01 café-prix pic 9v99 value 1.5.
procedure division.
    add 1 to GRÖßE.
    add 1 to größe.
    display "größe=" größe.
    add ΠΟΣΌ to число.
    display "число=" число " ποσό=" ποσό.
    display "数量=" 数量.
    display "café-prix=" CAFÉ-PRIX.
    perform Δοκιμή.
    stop run.
Δοκιμή.
    display "in Δοκιμή".
