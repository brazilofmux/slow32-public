! CHARACTER array actuals (Fortran 90): sections -- contiguous by address,
! strided and reversed copied in and back out -- and a substring of each
! element of a section, to assumed-length explicit-shape dummies; to
! CHARACTER assumed-shape dummies (module and internal, rank 1 and 2) a
! whole array, a section, a row of a matrix and substrings of a section,
! LEN and SIZE of the dummy as the actual makes them.
module f90cam
  implicit none
contains
  subroutine upper(x)
    character(len=*), intent(inout) :: x(:)
    integer :: i, j
    do i = 1, size(x)
      do j = 1, len(x(i))
        if (x(i)(j:j) >= 'a' .and. x(i)(j:j) <= 'z') x(i)(j:j) = char(ichar(x(i)(j:j)) - 32)
      end do
    end do
  end subroutine
end module
program f90charact1
  use f90cam
  implicit none
  character(len=4) :: c(6), m(2,3)
  integer :: i
  do i = 1, 6
    c(i) = char(96 + i) // char(96 + i) // char(64 + i) // '_'
  end do
  call show(c(2:5), 4)
  call show(c(6:1:-2), 3)
  call bump(c(1:6:2), 3)
  print *, c
  call bump(c(:)(2:3), 6)
  print *, c
  call tail(c(4:6))
  print *, c
  m = c(1)
  m(1,:) = c(1:3)
  call show(m(1,:), 3)
  call show(m(:,2), 2)
  call upper(c(2:4))
  print *, c
  call upper(m(2, 2:3))
  print *, m
  call upper(c(5:6)(1:2))
  print *, c
  call upper(c)
  print *, c
  print *, lens(m), lens(m(1:2,2:3)(2:2))
contains
  integer function lens(x)
    character(len=*) :: x(:,:)
    lens = len(x) * 100 + size(x)
  end function
  subroutine show(x, n)
    integer :: n
    character(len=*) :: x(n)
    print *, n, len(x), '[', x, ']'
  end subroutine
  subroutine bump(x, n)
    integer :: n
    character(len=*) :: x(n)
    x(1) = '####'
    x(n)(1:1) = '!'
  end subroutine
  subroutine tail(x)
    character(len=*) :: x(:)
    x(size(x)) = 'END'
    print *, size(x), len(x), x
  end subroutine
end program
