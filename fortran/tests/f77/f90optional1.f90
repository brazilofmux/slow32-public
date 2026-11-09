! OPTIONAL and keyword arguments (Fortran 90 12.4.1, 13.13.85): absent
! actuals in any position, PRESENT, keywords in any order and after
! positional ones, OPTIONAL scalars, assumed-shape arrays and CHARACTER
! (assumed length) of module and internal procedures; an absent dummy
! passed on to another OPTIONAL dummy.
module f90opm
  implicit none
contains
  real function area(w, h, scale)
    real, intent(in) :: w
    real, intent(in), optional :: h
    real, intent(in), optional :: scale
    if (present(h)) then
      area = w * h
    else
      area = w * w
    end if
    if (present(scale)) area = area * scale
  end function
  subroutine report(label, n, v, unit_name)
    character(len=*), intent(in) :: label
    integer, intent(in) :: n
    real, intent(in), optional :: v(:)
    character(len=*), intent(in), optional :: unit_name
    if (present(v)) then
      print *, label, n, sum(v)
    else
      print *, label, n, 'no values'
    end if
    if (present(unit_name)) print *, '  in ', unit_name, len(unit_name)
  end subroutine
end module
program f90optional1
  use f90opm
  implicit none
  real :: a(3)
  integer :: total
  a = (/ 1.0, 2.0, 3.0 /)
  print *, area(2.0), area(2.0, 3.0), area(2.0, scale=10.0), area(h=4.0, w=1.5)
  print *, area(scale=2.0, w=3.0, h=1.0)
  call report('a', 1)
  call report('b', 2, a)
  call report(n=3, label='c', unit_name='kg')
  call report('d', 4, unit_name='m', v=a(1:2))
  total = 0
  call acc(total, 5)
  call acc(total)
  call acc(by=7, t=total)
  print *, total
  call pass_on(a)
  call pass_on()
contains
  subroutine acc(t, by)
    integer :: t
    integer, optional :: by
    if (present(by)) then
      t = t + by
    else
      t = t + 1
    end if
  end subroutine
  subroutine pass_on(x)
    real, optional :: x(:)
    call report('p', 9, x)
  end subroutine
end program
