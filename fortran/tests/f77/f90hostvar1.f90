! Host association of variables (Fortran 90 12.1.2.2.1): internal
! procedures of a main program, of a module procedure and of a function
! using their host's scalars, arrays, CHARACTER, an ALLOCATABLE, a local
! first seen after them, adjustable and assumed-shape dummies, an
! assumed-length CHARACTER dummy, and the host FUNCTION's result;
! siblings calling each other.  The host's locals they name are static,
! its dummies reached through a table whose address they take hidden.
module f90hvm
contains
subroutine work(m, n, tag, v)
  implicit none
  integer :: n
  integer :: m(n, 2)
  character(len=*) :: tag
  real :: v(:)
  integer :: hits
  hits = 0
  call fill()
  call fill()
  print *, 'hits', hits, tag, len(tag), sum(v)
contains
  subroutine fill()
    integer :: i
    do i = 1, n
      m(i, 2) = m(i, 2) + i * 10
    end do
    hits = hits + 1
    tag(1:1) = 'X'
    v(1) = v(1) + 1
  end subroutine
end subroutine work
end module
program f90hostvar1
  use f90hvm
  implicit none
  integer :: outer_f
  integer :: count, grid(3, 2), i, later
  double precision :: acc
  character(len=5) :: word
  real, allocatable :: buf(:)
  count = 0
  acc = 0.5d0
  word = 'abc'
  grid = 0
  call step(3)
  call step(4)
  print *, count, acc, grid, '[', word, ']'
  allocate(buf(4))
  buf = 1.0
  call scale(2.5)
  print *, buf, total()
  later = 11
  call show_later()
  call work(grid, 3, word, buf(1:3))
  print *, grid
  print *, outer_f(5)
contains
  subroutine step(k)
    integer :: k
    count = count + k
    acc = acc * 2
    grid(mod(k, 3) + 1, 1) = k
    word(4:5) = 'zz'
    call bump2()
  end subroutine
  subroutine bump2()
    count = count + 100
  end subroutine
  subroutine scale(f)
    real :: f
    buf = buf * f
  end subroutine
  real function total()
    total = sum(buf)
  end function
  subroutine show_later()
    print *, 'later', later
  end subroutine
end program


integer function outer_f(n)
  integer :: n
  outer_f = 0
  call add(n)
  call add(1)
contains
  subroutine add(k)
    integer :: k
    outer_f = outer_f + k * n
  end subroutine
end function
