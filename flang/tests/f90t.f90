module shapes
  implicit none
  type :: point
    real(8) :: x = 0.0d0, y = 0.0d0
  end type
  type, extends(point) :: named
    character(len=8) :: tag = 'none'
  end type
  interface operator(+)
    module procedure addp
  end interface
contains
  pure function addp(a, b) result(c)
    type(point), intent(in) :: a, b
    type(point) :: c
    c%x = a%x + b%x; c%y = a%y + b%y
  end function
  recursive integer function fact(n) result(r)
    integer, intent(in) :: n
    if (n <= 1) then; r = 1; else; r = n * fact(n - 1); end if
  end function
  subroutine stats(v, mean, big)
    real(8), intent(in) :: v(:)
    real(8), intent(out) :: mean
    real(8), intent(out), optional :: big
    mean = sum(v) / size(v)
    if (present(big)) big = maxval(v)
  end subroutine
end module
program t
  use shapes
  implicit none
  real(8), allocatable :: a(:), m(:,:)
  type(point) :: p
  type(named) :: nn
  character(len=40) :: buf
  character(len=:), allocatable :: s
  integer :: i, u, k, ios
  real(8) :: mean, big
  complex(8) :: z
  allocate(a(10), m(3,3))
  a = [(real(i,8)**2, i=1,10)]
  call stats(a, mean, big)
  write(*,'(A,F8.3,1X,F8.3)') 'stats', mean, big
  m = reshape([(real(i,8), i=1,9)], [3,3])
  write(*,'(3F6.1)') matmul(m, transpose(m))
  print *, 'where', count(a > 20.0d0), sum(a, mask=a > 20.0d0)
  p = point(1.5d0, 2.5d0) + point(1.0d0, 1.0d0)
  write(*,'(2ES12.4)') p%x, p%y
  nn%tag = 'abc'; nn%x = 7
  write(*,'(A,"|",F4.1)') trim(nn%tag), nn%x
  print *, fact(10), trim(adjustl('  left')), len_trim('abc   ')
  s = 'alloc' // '-' // 'string'
  print *, s, len(s), index(s, '-')
  write(buf,'(I5,A,E12.5)') 42, ' x ', 3.25d0
  print *, buf
  read(buf,*) k
  print *, k + 1
  z = cmplx(1.0d0, 2.0d0, 8) * cmplx(3.0d0, -1.0d0, 8)
  print *, real(z), aimag(z), abs(cmplx(3.0d0, 4.0d0, 8))
  print *, sqrt(2.0d0), sin(1.0d0), exp(1.0d0), atan2(1.0d0, 1.0d0), 2.0**0.5
  open(newunit=u, file='f90t.tmp', status='replace')
  do i = 1, 3; write(u,'(I3,F6.2)') i, i*1.5; end do
  close(u)
  open(newunit=u, file='f90t.tmp', status='old')
  mean = 0
  do
    read(u,*,iostat=ios) k, big
    if (ios /= 0) exit
    mean = mean + big
  end do
  close(u, status='delete')
  print *, 'filesum', mean
  deallocate(a, m)
  print *, allocated(a), huge(1), epsilon(1.0d0), selected_real_kind(10)
end program
