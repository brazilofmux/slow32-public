! FORALL on a CHARACTER(*) dummy's substrings, on sections passed to
! assumed-shape dummies, through a POINTER to a structure and on a
! structure dummy, on substrings of an array of structures' components;
! and 20000 turns of a masked FORALL inside a masked FORALL (the lists
! of their combinations are freed).
module f95forall9m
  implicit none
  type pt
    integer :: tag(4)
    character(len=4) :: nm
  end type
contains
  subroutine flip(s)
    character(len=*) :: s
    integer :: i
    forall (i = 1:len(s)) s(i:i) = s(len(s) + 1 - i:len(s) + 1 - i)
  end subroutine
  subroutine strided(a)
    integer :: a(:), i
    forall (i = 2:size(a)) a(i) = a(i - 1) + 1
  end subroutine
  subroutine two(a)
    integer :: a(:, :), i, j
    forall (i = 1:size(a, 1), j = 1:size(a, 2)) a(i, j) = a(size(a, 1) + 1 - i, size(a, 2) + 1 - j)
  end subroutine
  subroutine viaptr(p)
    type(pt), pointer :: p
    integer :: i
    forall (i = 1:4) p%tag(i) = p%tag(5 - i)
    forall (i = 1:4) p%nm(i:i) = p%nm(5 - i:5 - i)
  end subroutine
  subroutine dum(d)
    type(pt) :: d
    integer :: i
    forall (i = 1:3) d%tag(i + 1) = d%tag(i)
  end subroutine
end module
program f95forall9
  use f95forall9m
  implicit none
  character(len=7) :: s
  integer :: v(9), i, j, k, m(4, 6), it, acc
  type(pt), target :: t
  type(pt), pointer :: tp
  type(pt) :: arr(3)
  s = 'abcdefg'
  call flip(s)
  print *, s
  call flip(s(2:5))
  print *, s
  v = (/ (i, i = 1, 9) /)
  call strided(v(1:9:2))
  print *, v
  call strided(v(9:1:-3))
  print *, v
  forall (i = 1:4, j = 1:6) m(i, j) = 10 * i + j
  call two(m(1:4:3, 2:6:2))
  print *, m
  t = pt((/ 1, 2, 3, 4 /), 'wxyz')
  tp => t
  call viaptr(tp)
  print *, t
  call dum(t)
  print *, t
  forall (i = 1:3) arr(i) = pt((/ i, i, i, i /), 'abcd')
  forall (i = 1:3) arr(i)%nm(i:i) = arr(4 - i)%nm(4:4)
  forall (i = 1:3, j = 1:4) arr(i)%tag(j) = arr(j - (j - 1) / 3 * 3)%tag(i) + j
  print *, arr
  ! a DO's terminal statement
  do k = 1, 3
    forall (i = 1:9, v(i) > k) v(i) = v(i) - k
  end do
  print *, v
  ! lists made and freed many times
  acc = 0
  do it = 1, 20000
    forall (i = 1:4, m(i, 1) >= 0)
      forall (j = 1:6, i /= j) m(i, j) = mod(m(j - (j - 1) / 4 * 4, i) + it, 97)
    end forall
    acc = mod(acc + m(2, 3), 1009)
  end do
  print *, acc
end program
