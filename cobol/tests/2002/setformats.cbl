identification division.
program-id. setformats.
*> SET formats 1, 2 and 4 as X3.23-1985 6.23 and COBOL 2002 14.8.35 have
*> them: an index-name from an integer, an index-name of another table
*> (the occurrence number carried over), an index data item (no
*> conversion), an integer item from an index-name; UP BY and DOWN BY an
*> integer item; condition-names TO TRUE (the first literal) and TO FALSE
*> (the FALSE phrase's), several at once.
data division.
working-storage section.
01 t1.
   05 a pic x(3) occurs 5 indexed by i1 j1.
01 t2.
   05 b pic x(7) occurs 9 indexed by i2.
01 n    pic 99.
01 m    pic 99.
01 ixd  usage index.
01 sw   pic x value "a".
   88 s-yes values "Y" "y".
   88 s-no  value "N" false "?".
   88 s-rng value "0" thru "5".
01 st2  pic 9 value 0.
   88 s2-on value 1.
procedure division.
    move "aaabbbcccdddeee" to t1
    set i1 to 3 display a(i1)
    set i2 to i1 set n to i2 display "i2 from i1: " n
    set i1 up by 2 display a(i1)
    subtract 2 from n set i1 down by n display a(i1)
    set ixd to i1 set j1 to ixd display "via index item: " a(j1)
    set n m to i1 display n " " m
    move 2 to n
    set i1 j1 to n display a(i1) a(j1)
    set s-yes to true display "[" sw "]"
    set s-no to false display "[" sw "]"
    set s-rng to true display "[" sw "]"
    set s-no s2-on to true display "[" sw "] " st2
    stop run.
end program setformats.
