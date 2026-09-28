identification division.
program-id. natinspect.
*> INSPECT of national items (2023 14.9.22; cobol ISSUES-68): positions
*> are characters, two bytes each, and every operand is national, a
*> figurative constant one national character (syntax rules 3 and 4).
*> TALLYING, REPLACING and CONVERTING, with BEFORE and AFTER, on a whole
*> item and a reference-modified one.  A match is whole characters only:
*> N"AB" is the bytes 00 41 00 42, which hold 41 00 at an odd offset,
*> but not the character U+4100.  No oracle (docs/national.md).
data division.
working-storage section.
01  n        pic n(10) value n"日本の日本語だ日本".
01  m        pic n(6).
01  ab       pic n(2) value n"AB".
01  c        pic 99.
01  d        pic 99.
procedure division.
main.
    move 0 to c d
    inspect n tallying c for all n"日本" d for characters
    display "all 日本: " c ", characters: " d
    move 0 to c
    inspect n tallying c for all n"日本" before initial n"語"
    display "before 語: " c
    move 0 to c
    inspect n tallying c for all n"日本" after initial n"語"
    display "after 語: " c
    move 0 to c
    inspect n tallying c for all spaces
    display "spaces: " c
    move 0 to c
    inspect ab tallying c for all nx"4100"
    display "misaligned bytes: " c
    move n"  abc  " to m
    move 0 to c
    inspect m tallying c for leading space
    display "leading spaces: " c
    inspect n replacing all n"日本" by n"にほ" first n"語" by n"ご"
    display "replacing: [" n "]"
    inspect n replacing leading n"に" by n"ニ"
    display "leading: [" n "]"
    inspect n replacing characters by n"*" after initial n"だ"
    display "characters after だ: [" n "]"
    move n"日本の日本語だ日本" to n
    inspect n converting n"日本" to n"ab" before initial n"語"
    display "converting: [" n "]"
    inspect n converting n"語だ" to space
    display "converting to space: [" n "]"
    move n"xxxx" to m
    inspect m(2:2) replacing all n"x" by n"y"
    display "reference-modified: [" m "]"
    stop run.
