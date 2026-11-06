! wide.f90: what is wider than this machine's index.  An INTEGER(8) DO variable
! whose bounds do not fit in 32 bits, the same in an array constructor, NINT to
! INTEGER(8), and types holding a descriptor, sized by the compiler and then
! walked by the runtime.  Each failure stops with its own code; success prints
! wide-ok.
module wide_types
  implicit none
  type :: holder
    integer(4) :: first = 3
    integer(4), allocatable :: list(:)
    integer(4) :: second = 4
    real(8), pointer :: ref(:,:) => null()
    procedure(), pointer, nopass :: action => null()
    integer(4) :: third = 5
  end type
  type :: outer
    type(holder) :: inner(2)
    character(5) :: label = 'tail!'
  end type
contains
  integer function total(v)
    integer(8), intent(in) :: v(:)
    total = size(v)
  end function
end module

program wide
  use wide_types
  implicit none
  integer(8) :: i, top, down, n, last
  integer(4) :: low, c, j
  integer(8) :: asked_recl, wide_recl
  integer(8), allocatable :: built(:)
  real(8) :: x
  real(4) :: y
  type(holder) :: h(3), g(3)
  type(outer) :: o1, o2

  ! a DO variable above 2**32, counting down
  top = 7000000000_8 * 3 + 11
  low = 11
  down = -7000000000_8
  n = 0
  do i = top, low + 0_8, down
    n = n + 1
    if (n == 2 .and. i /= 14000000011_8) stop 1
  end do
  if (n /= 4) stop 2
  if (i /= 11_8 - 7000000000_8) stop 3

  ! counting up across 2**32, and a step that fits with bounds that do not
  n = 0
  do i = 4294967290_8, 4294967300_8, 3_8
    n = n + 1
    last = i
  end do
  if (n /= 4 .or. last /= 4294967299_8) stop 4
  if (i /= 4294967302_8) stop 5

  ! an empty loop keeps its first value
  n = 0
  do i = 9000000000_8, 8000000000_8
    n = n + 1
  end do
  if (n /= 0 .or. i /= 9000000000_8) stop 6

  ! INTEGER(4) as wide as index: the usual trip count formula overflows on these
  c = 0
  do j = 0, 1, huge(0)
    c = c + 1
  end do
  if (c /= 1 .or. j /= huge(0)) stop 22
  c = 0
  do j = 1, 0, -huge(0) - 1
    c = c + 1
  end do
  if (c /= 1 .or. j /= -huge(0)) stop 23
  c = 0
  do j = -huge(0), huge(0), huge(0)
    c = c + 1
  end do
  if (c /= 3) stop 24
  c = 0
  do j = huge(0) - 2, huge(0) - 1
    c = c + 1
    if (c == 2) exit   ! an exit makes it the other kind of loop
  end do
  if (c /= 2 .or. j /= huge(0) - 1) stop 25

  ! a record length past 4 GB survives the trip through the runtime
  asked_recl = 7777777777_8
  open(9, status='scratch', recl=asked_recl, form='unformatted')
  inquire(9, recl=wide_recl)
  close(9)
  if (wide_recl /= asked_recl) stop 26

  ! array constructors, in the two ways they are lowered
  if (total([(i, i = top, low + 0_8, down)]) /= 4) stop 7
  built = [(i * 2, i = top, low + 0_8, down)]
  if (size(built) /= 4) stop 8
  if (built(1) /= 2 * top .or. built(4) /= 22_8) stop 9
  built = [(i, i = 1_8, 2_8), (i + 5000000000_8, i = 1_8, 2_8)]
  if (built(3) /= 5000000001_8 .or. size(built) /= 4) stop 10

  ! NINT: values that do not fold, and do not fit a 32-bit long
  x = 6755399441055751.0_8   ! odd, above 2**52: exact, and no room for a fraction
  if (nint(x, kind=8) /= 6755399441055751_8) stop 11
  x = -3000000000.5_8
  if (nint(x, kind=8) /= -3000000001_8) stop 12
  y = 5000000000.0
  if (nint(y, kind=8) /= int(y, kind=8)) stop 13
  x = 2.5_8
  if (nint(x) /= 3 .or. nint(x, kind=8) /= 3_8) stop 14

  ! the compiler's size of a type is where the next element really is
  if (storage_size(h) / 8 /= loc(h(2)) - loc(h(1))) stop 15
  if (storage_size(o1) / 8 < storage_size(h) / 4) stop 16

  ! default initialization and copies, by the type tables' offsets
  if (h(2)%first /= 3 .or. h(2)%second /= 4 .or. h(2)%third /= 5) stop 17
  allocate(h(2)%list(2)); h(2)%list = [21, 22]
  h(2)%third = 6
  g = h
  h(2)%list(1) = 0
  if (g(2)%list(1) /= 21 .or. g(2)%third /= 6 .or. g(3)%second /= 4) stop 18
  if (allocated(g(3)%list)) stop 19
  allocate(o1%inner(2)%list(1)); o1%inner(2)%list = 33
  o2 = o1
  if (o2%inner(2)%list(1) /= 33 .or. o2%label /= 'tail!') stop 20
  if (o2%inner(2)%third /= 5) stop 21

  print '(a)', 'wide-ok'
end program
