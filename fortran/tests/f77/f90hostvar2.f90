! Host association, continued: a subroutine's ALLOCATABLE local used by
! its internal procedure is deallocated at each return (it is static for
! them, not SAVEd); a DATA-initialized local (SAVEd); a COMMON member; a
! CHARACTER function's result set from its internal procedure.
program f90hostvar2
  implicit none
  integer :: i
  character(len=6) :: s, label
  do i = 1, 3
    call f90hv2twice(i)
  end do
  s = label(4)
  print *, '[', s, ']'
end program
subroutine f90hv2twice(k)
  integer :: k
  real, allocatable :: w(:)
  integer :: calls = 0
  common /blk/ shared
  integer :: shared
  logical :: was
  was = allocated(w)
  allocate(w(k))
  call fillw()
  calls = calls + 1
  print *, k, was, sum(w), calls, shared
contains
  subroutine fillw()
    w = real(k)
    shared = shared + k
  end subroutine
end subroutine
function label(n)
  character(len=6) :: label
  integer :: n
  label = 'L'
  call mark()
contains
  subroutine mark()
    integer :: j
    do j = 2, n + 1
      label(j:j) = char(48 + j)
    end do
  end subroutine
end function
