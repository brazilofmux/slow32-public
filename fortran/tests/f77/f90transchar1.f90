! TRANSFER with CHARACTER (Fortran 90 13.13.110), as an actual argument:
! an INTEGER to a CHARACTER MOLD and back, a CHARACTER scalar and an
! array to INTEGER and CHARACTER array MOLDs, a concatenation as SOURCE;
! the result's length the MOLD's.
program f90transchar1
  implicit none
  character(len=4) :: w(2)
  character(len=8) :: s
  w = (/ 'ABCD', 'EFGH' /)
  s = 'abcdefgh'
  call showc(transfer(1094861636, 'abcd'))
  call showi(transfer('DCBA', 0))
  call showi2(transfer(s, (/ 0 /)), 2)
  call showc(transfer(w, s))
  call showc2(transfer(s(3:6) // 'xy', (/ 'ab' /)), 3)
contains
  subroutine showc(c)
    character(len=*) :: c
    print *, '[', c, ']', len(c)
  end subroutine
  subroutine showi(i)
    integer :: i
    print *, i
  end subroutine
  subroutine showi2(k, n)
    integer :: n, k(n)
    print *, k
  end subroutine
  subroutine showc2(c, n)
    integer :: n
    character(len=*) :: c(n)
    print *, '[', c, ']', len(c)
  end subroutine
end program
