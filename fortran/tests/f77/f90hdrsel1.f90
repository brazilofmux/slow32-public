! A FUNCTION statement whose type has a selector in parentheses (Fortran
! 90): REAL(8), INTEGER(KIND=4), CHARACTER(LEN=3), CHARACTER(3),
! CHARACTER(LEN=2, KIND=1), COMPLEX(KIND=8), with prefixes around it.
program f90hdrsel1
  implicit none
  print *, f1(2.0d0), f2(3), f3(1), f4(2), f5(3), f6(.true.), f7((1.0, 2.0))
contains
  real(8) function f1(x)
    real(8) :: x
    f1 = x * 2
  end function
  integer(kind=4) function f2(k)
    integer :: k
    f2 = k + 1
  end function
  character(len=3) function f3(k)
    integer :: k
    f3 = 'abc'
  end function
  character(4) function f4(k)
    integer :: k
    f4 = 'wxyz'
  end function
  character(len=2, kind=1) function f5(k)
    integer :: k
    f5 = 'pq'
  end function
  logical(4) function f6(l)
    logical :: l
    f6 = .not. l
  end function
  complex(kind=8) function f7(z)
    complex :: z
    f7 = z * 2
  end function
end program
