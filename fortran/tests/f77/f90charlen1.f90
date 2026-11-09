! A CHARACTER FUNCTION whose length names a constant (Fortran 90): the
! constant its host's, its host's USEs', or its own USEs'; the length on
! the FUNCTION statement or in a type statement; referenced before the
! function is compiled and after.
module f90charlen1_k
  implicit none
  integer, parameter :: n = 4, neg = -2
end module
module f90charlen1_m
  implicit none
  integer, parameter :: n = 4
contains
  subroutine show
    print *, '[', f1(1), '][', f2(0), '][', f3(1), '][', f4(1), ']'
  end subroutine
  character*(n) function f1(k)
    integer :: k
    f1 = 'abcdefgh'
    if (k > 0) f1(1:1) = 'Z'
  end function
  character(len=n) function f2(k)
    integer :: k
    f2 = 'ABCDEFGH'
    if (k > 0) f2(1:1) = 'z'
  end function
  character(len=n+1) function f3(k)
    integer :: k
    f3 = 'ABCDEFGH'
    if (k > 0) f3(1:1) = 'z'
  end function
  function f4(k)
    integer :: k
    character(len=n*2) :: f4
    f4 = '123456789'
    if (k > 0) f4(1:1) = 'z'
  end function
  integer function g(k)
    integer :: k
    character(len=n) :: tmp
    character*(n+1) :: t2
    tmp = 'abcdefgh'
    t2 = tmp
    g = len(tmp) + ichar(tmp(k:k)) + len(t2)
  end function
end module
function ext(k)
  use f90charlen1_k
  implicit none
  integer :: k
  character(len=n) :: ext
  ext = 'abcdefgh'
  if (k > 0) ext(1:1) = 'Z'
end function
character(len=2*n+neg) function ext2(k)
  use f90charlen1_k
  implicit none
  integer :: k
  ext2 = 'abcdefgh'
  if (k > 0) ext2(1:1) = 'Z'
end function
program f90charlen1
  use f90charlen1_m
  use f90charlen1_k, only: neg
  implicit none
  character(len=n) :: ext
  character(len=6) :: ext2
  call show
  print *, '[', f1(1), '][', f2(0), '][', f3(1), '][', f4(1), ']'
  print *, len(f1(1)), len(f2(1)), len(f3(1)), len(f4(0)), g(3)
  print *, '[', ext(1), '][', ext(0), '][', ext2(1), ']'
  call sub
contains
  subroutine sub
    print *, '[', g1(1), ']', len(g1(0)), '[', g2(1), ']', len(g2(0))
  end subroutine
  character(len=n-1) function g1(k)
    integer :: k
    g1 = 'ABCDEFGH'
    if (k > 0) g1(1:1) = 'z'
  end function
  function g2(k)
    integer :: k
    character(len=n+neg) :: g2
    g2 = 'ABCDEFGH'
    if (k > 0) g2(1:1) = 'z'
  end function
end program
