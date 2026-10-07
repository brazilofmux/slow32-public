identification division.
program-id. p-std2023-options-init-fill.
*> OPTIONS INITIALIZE ... TO takes a one-byte hexadecimal literal (2023 11.9.10.3 rule 1).
options.
    initialize all section to "ab".
data division.
working-storage section.
01 x pic x.
procedure division.
    display x
    stop run.
