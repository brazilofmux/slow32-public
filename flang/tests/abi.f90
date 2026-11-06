! abi.f90 with abi-driver.c: Fortran's idea of the C types against clang's, and
! the derived-type tables flang lays out against the runtime that reads them.
! Each failure stops with its own code; success prints abi-ok.
module abi_types
  use iso_c_binding
  implicit none
  type, bind(c) :: mixed
    integer(c_signed_char) :: a
    real(c_double) :: b
    integer(c_int) :: c
    integer(c_long_long) :: d
    type(c_ptr) :: e
    type(c_funptr) :: f
    integer(c_long) :: g
    integer(c_size_t) :: h
  end type
  type :: base
    integer(4) :: tag = 71
    real(8) :: weight = 2.5d0
    integer(8) :: big = 90000000000_8
    character(3) :: name = 'abc'
  contains
    procedure :: show => base_show
    final :: base_final
  end type
  type, extends(base) :: child
    real(8) :: extra = 7.5d0
    integer(4), allocatable :: list(:)
  contains
    procedure :: show => child_show
  end type
  integer :: finals = 0
  interface
    integer(c_size_t) function c_sizes(which) bind(c, name='c_sizes')
      import
      integer(c_int), value :: which
    end function
    integer(c_int) function c_check(m, n, k, last) bind(c, name='c_check')
      import
      type(mixed), intent(in) :: m
      integer(c_size_t), value :: n
      integer(c_long), value :: k
      integer(c_int), value :: last
    end function
    integer(c_int) function c_apply(f, p, x) bind(c, name='c_apply')
      import
      type(c_funptr), value :: f
      type(c_ptr), value :: p
      integer(c_int), value :: x
    end function
  end interface
contains
  integer function base_show(self)
    class(base), intent(in) :: self
    base_show = self%tag
  end function
  integer function child_show(self)
    class(child), intent(in) :: self
    child_show = self%tag + int(self%extra)
  end function
  subroutine base_final(self)
    type(base), intent(inout) :: self
    finals = finals + 1
  end subroutine
  integer(c_int) function plus(p, x) bind(c)
    type(c_ptr), value :: p
    integer(c_int), value :: x
    integer(c_int), pointer :: q
    call c_f_pointer(p, q)
    plus = q + x
  end function
end module

program abi
  use abi_types
  implicit none
  type(mixed), target :: m
  integer(c_int), target :: v
  type(child) :: c1, c2
  class(base), allocatable :: poly
  type(mixed) :: arr(3)

  ! sizes: the C side says what they are
  if (c_sizeof(m) /= c_sizes(0)) stop 1
  if (c_sizeof(0_c_long) /= c_sizes(1)) stop 2
  if (c_sizeof(0_c_size_t) /= c_sizes(2)) stop 3
  if (c_sizeof(m%e) /= c_sizes(3)) stop 4
  if (c_sizeof(0_c_intptr_t) /= c_sizes(3)) stop 5
  if (c_sizeof(arr) /= 3 * c_sizes(0)) stop 6
  if (kind(c_sizeof(m)) /= c_size_t) stop 7

  ! every field where C looks, with later scalar arguments intact behind them
  m%a = 11; m%b = 1.5d0; m%c = 33; m%d = 4400000000_c_long_long
  v = 40
  m%e = c_loc(v); m%f = c_funloc(plus); m%g = 77; m%h = 88
  if (c_check(m, 1234_c_size_t, 5678_c_long, 91) /= 0) stop 8

  ! a procedure pointer and a data pointer through C
  if (c_apply(c_funloc(plus), c_loc(v), 2) /= 42) stop 9

  ! initialization from the type tables: every field at its offset
  if (c1%tag /= 71 .or. c1%weight /= 2.5d0 .or. c1%big /= 90000000000_8) stop 10
  if (c1%name /= 'abc' .or. c1%extra /= 7.5d0) stop 11

  ! the runtime walks components to copy an allocatable, by offset
  allocate(c1%list(3)); c1%list = [5, 6, 7]
  c1%tag = 72
  c2 = c1
  c1%list(2) = 0
  if (c2%tag /= 72 .or. c2%list(2) /= 6 .or. c2%extra /= 7.5d0) stop 12

  ! bindings through the table
  allocate(poly, source=c2)
  if (poly%show() /= 79) stop 13
  select type (poly)
  type is (child)
    if (poly%list(3) /= 7) stop 14
  class default
    stop 15
  end select

  ! finalization through a special binding
  finals = 0
  deallocate(poly)
  if (finals /= 1) stop 16

  print '(a)', 'abi-ok'
end program
