identification division.
program-id. alouter.
*> -warn-extensions under -std=2002: ANY LENGTH in an outermost
*> program, which 2023 13.18.2.3 rule 2 excludes (BP-E28); X-COBOL's
*> logger and command-line-parser are such programs.
data division.
linkage section.
01 l pic x any length.
procedure division using l.
    display l
    goback.
