! A linked list (Fortran 90): a type with a POINTER component of its own
! type, a module's POINTER variable as its head, nodes ALLOCATEd and
! linked, walked, reversed, reached through two pointers at once; array
! POINTER dummies associated and reassociated by their procedures.
module f90ptrlist
  implicit none
  type node
    integer :: val
    type(node), pointer :: next
  end type
  type(node), pointer :: head
contains
  subroutine push(v)
    integer :: v
    type(node), pointer :: n
    allocate(n)
    n%val = v
    n%next => head
    head => n
  end subroutine
  integer function total()
    type(node), pointer :: c
    total = 0
    c => head
    do while (associated(c))
      total = total + c%val
      c => c%next
    end do
  end function
  subroutine reverse()
    type(node), pointer :: prev, cur, nxt
    nullify(prev)
    cur => head
    do while (associated(cur))
      nxt => cur%next
      cur%next => prev
      prev => cur
      cur => nxt
    end do
    head => prev
  end subroutine
  subroutine show()
    type(node), pointer :: c
    integer :: k, buf(20)
    k = 0
    c => head
    do while (associated(c))
      k = k + 1
      buf(k) = c%val
      c => c%next
    end do
    print *, buf(1:k)
  end subroutine
end module
program f90ptr2
  use f90ptrlist
  implicit none
  integer :: i
  real, pointer :: pp(:)
  real, target :: data(6)
  nullify(head)
  do i = 1, 5
    call push(i * i)
  end do
  call show()
  print *, total(), head%val, head%next%val, head%next%next%val
  call reverse()
  call show()
  head%next%val = -4
  print *, total()
  data = (/ 1., 2., 3., 4., 5., 6. /)
  call setp(pp, data)
  print *, size(pp), pp
  call grow(pp)
  print *, size(pp), pp
contains
  subroutine setp(p, d)
    real, pointer :: p(:)
    real, target :: d(:)
    p => d(2::2)
  end subroutine
  subroutine grow(p)
    real, pointer :: p(:)
    real, pointer :: t(:)
    allocate(t(size(p) + 1))
    t(1:size(p)) = p
    t(size(p) + 1) = 100.0
    p => t
  end subroutine
end program
