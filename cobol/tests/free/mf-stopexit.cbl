*> Micro Focus's dialect (-dialect=mf): STOP RUN followed by more
*> statements of its sentence (BP-D3) -- they never run -- and EXIT that is
*> not a sentence alone in its paragraph (BP-D4) -- it does nothing.
*> abrignoli_COBSOFT ends its subprograms "exit program stop run exit.":
*> called, EXIT PROGRAM returns and the rest is never reached.
identification division.
program-id. mf-stopexit.
data division.
working-storage section.
01 n pic 9 value 0.
procedure division.
main-para.
    call "sub"
    display "back in main"
    perform add-one
    perform add-one
    display "n = " n
    display "stopping" stop run display "never".
add-one.
    add 1 to n exit.

identification division.
program-id. sub.
procedure division.
sub-main.
    display "in sub".
sub-exit.
    exit program
    stop run
    exit.
end program sub.
end program mf-stopexit.
