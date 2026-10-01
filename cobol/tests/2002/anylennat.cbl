*> A national ANY LENGTH parameter (2023 13.18.2.3 rule 1: PICTURE N):
*> its length in characters, two bytes each, and a part of it.
*> No oracle: GnuCOBOL 4.0-early-dev has no DISPLAY-OF.
identification division.
program-id. anylennat.
data division.
working-storage section.
01 nat-s   pic n(4)  value n"wxyz".
01 nat-t   pic n(2)  value n"pq".
procedure division.
    call "natlen" using nat-s
    call "natlen" using nat-t
    stop run.

identification division.
program-id. natlen.
data division.
linkage section.
01 l-n pic n any length.
procedure division using l-n.
    display "national " function length(l-n) " " function byte-length(l-n) " [" function display-of(l-n(2:)) "]"
    goback.
end program natlen.
end program anylennat.
