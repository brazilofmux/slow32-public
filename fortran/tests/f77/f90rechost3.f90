! A RECURSIVE host's locals in its internal procedures: structures,
! pointers, arrays, CHARACTER, a recursion through the internal procedure.
module m2c
  implicit none
  type node
    integer :: v
    real :: w(2)
  end type
contains
  recursive subroutine tree(depth, total)
    integer, intent(in) :: depth
    integer, intent(inout) :: total
    integer :: here, arr(3), i
    integer, save :: calls = 0
    character(5) :: tag
    type(node) :: nd
    real(8) :: d
    integer, allocatable :: al(:)
    logical :: flag
    complex :: z
    here = depth * 10
    arr = (/ depth, depth + 1, depth + 2 /)
    tag = 'lv' // achar(48 + depth)
    nd%v = depth; nd%w = (/ 1.5, 2.5 /) * depth
    d = 0.25d0 * depth
    allocate (al(depth + 1))
    al = (/ (i * depth, i = 1, depth + 1) /)
    flag = mod(depth, 2) == 0
    z = cmplx(depth, -depth)
    call note()
    if (depth < 3) call tree(depth + 1, total)
    call note()
    total = total + here + inner()
    deallocate (al)
  contains
    subroutine note()
      calls = calls + 1
      here = here + 1
      arr(2) = arr(2) + 100
      nd%v = nd%v + 1000
      d = d + 1
      print *, depth, here, arr, tag, nd%v, nd%w, d, al, flag, z, calls
      tag(5:5) = '*'
      flag = .not. flag
      z = z + (1.0, 1.0)
      al(1) = al(1) - 1
    end subroutine
    integer function inner()
      inner = sum(arr) + size(al) + len_trim(tag)
    end function
  end subroutine
  recursive function fib(n) result(r)
    integer, intent(in) :: n
    integer :: r, a, b
    if (n < 2) then
      r = n
      return
    end if
    a = fib(n - 1)
    b = fib(n - 2)
    call add()
  contains
    subroutine add()
      r = a + b
    end subroutine
  end function
end module
program p2c
  use m2c
  implicit none
  integer :: t
  t = 0
  call tree(1, t)
  print *, t
  print *, fib(15)
end program
