! A CHARACTER actual that is an expression -- TRIM(S), (S) -- is a value:
! the callee may define the variable it came from through another
! argument, or as a host variable, and still read what was passed.
! gfortran passes (S) as S itself and prints blanks for the last line;
! the .expected file has what the standard says (12.4.1.1: an expression
! is not the variable).
program trimact
  character(len=10) :: word
  word = 'abcdefgh'
  call head_of(trim(word), word)
  print *, '<', word, '>'
  word = 'abcdefgh'
  call via_host(trim(word))
  print *, '<', word, '>'
  word = 'abcdefgh'
  call head_of((word), word)
  print *, '<', word, '>'
contains
  subroutine head_of(src, dst)
    character(len=*), intent(in) :: src
    character(len=*), intent(out) :: dst
    dst = ' '
    dst = src(1:4) // '!'
  end subroutine head_of
  subroutine via_host(src)
    character(len=*), intent(in) :: src
    word = ' '
    word(2:4) = src(6:8)
  end subroutine via_host
end program trimact
