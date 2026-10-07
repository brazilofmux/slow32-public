identification division.
program-id. p-std2014-options-initialize.
*> The OPTIONS INITIALIZE clause is COBOL 2023 (11.9.10).
options.
    initialize all section to spaces.
data division.
working-storage section.
01 x pic x.
procedure division.
    display x
    stop run.
