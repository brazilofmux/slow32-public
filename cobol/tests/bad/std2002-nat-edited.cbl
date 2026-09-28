identification division.
program-id. natedit.
*> An UNSTRING receiver of national data is national or numeric, not
*> national-edited (2023 14.9.48.3 rule 4).
data division.
working-storage section.
01  s pic n(8) value n"ab,cd".
01  e pic nn/nn.
procedure division.
    unstring s delimited by n"," into e
    stop run.
