identification division.
program-id. p-std2023-copy-word.
*> COBOL 2023 removed COPY REPLACING operands that are not pseudo-text (Annex E.2 item 1; BP-R4).
data division.
working-storage section.
copy "removed2023.cpy" replacing TAG by my-rec LEN by 5.
procedure division.
    display my-rec
    stop run.
