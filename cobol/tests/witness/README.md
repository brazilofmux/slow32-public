# Programs for the old witnesses

Programs written in the fixed-format, 74-era style that the old
compilers accept, for asking them the questions of docs/oracles.md.
They are not run by run-tests.sh. Each NAME.cbl has:

- `NAME.expected`: this compiler, `-std=85`.
- `NAME.ansmvt`: IBM ANS COBOL (IKFCBL00) on MVS 3.8j, where it compiles.
  Run with `../mvscheck.sh`, after `tk5-up` and before `tk5-down`.
- `NAME.ms465`: Microsoft MS-COBOL 4.65 on CP/M, where it compiles. Run
  with `../cpmcheck.sh`.

What each compiler refuses:

| program | ANS COBOL (MVT) | MS-COBOL 4.65 |
|---|---|---|
| `editins` | `/` in a PICTURE (a 74 addition) | runs |
| `editinb` | runs (`editins` with `B` for `/`) | not run |
| `divremu` | runs | ignores REMAINDER and ON SIZE ERROR |
| `inspord` | no INSPECT (a 74 addition) | its compiler stops on a multi-phrase REPLACING |
| `negcmp` | runs | runs |

ANS COBOL DISPLAYs a signed item zoned: the last digit carries the sign,
`J` to `R` for -1 to -9 and `}` for -0. So `R=J` is -1 and `R=01R` is
-0.19 in a `S9V99` item. Like MS COBOL 5.0 it shows no decimal point:
`Q=24` is 2.4.
