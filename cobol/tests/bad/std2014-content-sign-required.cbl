identification division.
program-id. p-content-sign-required.
*> SET CONTENT OF a two's-complement item TO FARTHEST-FROM-ZERO without SIGN:
*> its positive and negative extremes differ in magnitude (2023 14.9.39.3
*> rule 31a).
data division.
working-storage section.
01 c5 pic s9(4) comp-5.
procedure division.
    set content of c5 to farthest-from-zero
    goback.
