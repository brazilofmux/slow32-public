! Derived types, continued: a module's types and procedures with
! structure dummies; LOGICAL, DOUBLE PRECISION and array components;
! arrays of structures assigned whole, by section and element; a
! structure array dummy; formatted WRITE and READ of components into and
! from an internal file; constructors as actuals.
module f90dtshapes
  implicit none
  type vec
    double precision :: x, y, z
  end type
  type body
    character(len=5) :: name
    logical :: alive
    integer :: id
    type(vec) :: pos
    real :: mass(2)
  end type
contains
  function dot(a, b)
    type(vec), intent(in) :: a, b
    double precision :: dot
    dot = a%x * b%x + a%y * b%y + a%z * b%z
  end function
  subroutine shift(bs, n, d)
    integer :: n
    type(body) :: bs(n)
    type(vec) :: d
    integer :: i
    do i = 1, n
      bs(i)%pos%x = bs(i)%pos%x + d%x
      bs(i)%pos%y = bs(i)%pos%y + d%y
      bs(i)%pos%z = bs(i)%pos%z + d%z
    end do
  end subroutine
end module
program f90dtype2
  use f90dtshapes
  implicit none
  type(body) :: b(3), c(3), one
  type(vec) :: v
  character(len=40) :: line
  integer :: i
  b = body('none', .false., 0, vec(0d0, 0d0, 0d0), 0.0)
  do i = 1, 3
    b(i)%name = char(64 + i) // 'x'
    b(i)%alive = mod(i, 2) == 1
    b(i)%id = int(i * 10)
    b(i)%pos = vec(dble(i), 2d0 * i, -dble(i))
    b(i)%mass = (/ real(i), real(i) / 2 /)
  end do
  c = b
  c(2:3) = b(1:2)
  print *, c(1)%name, c(2)%name, c(3)%name, c(3)%pos, c(2)%alive
  call shift(b, 3, vec(1d0, 1d0, 1d0))
  print *, b(2)%pos, dot(b(1)%pos, b(2)%pos)
  one = b(3)
  print *, one
  write (line, '(A5,L2,I4,3F6.1)') one%name, one%alive, one%id, one%pos
  print *, line
  read (line, '(A5,L2,I4,3F6.1)') c(1)%name, c(1)%alive, c(1)%id, c(1)%pos
  print *, c(1)%name, c(1)%alive, c(1)%id, c(1)%pos
  v = vec(1d0, 2d0, 3d0)
  print *, dot(v, v), dot(vec(1d0, 0d0, 0d0), v)
  print *, b(1)%mass, sum(b(3)%mass)
end program
