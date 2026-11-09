module m
  implicit none
  interface what
    module procedure wi, wl
  end interface
  interface operator(.o.)
    module procedure oi, ol
  end interface
contains
  subroutine early(i, l)
    integer, intent(in) :: i
    logical, intent(in) :: l
    call what(i)
    call what(l)
    call what(i > 0)
    print *, i .o. i, l .o. l, (i > 0) .o. l
  end subroutine
  subroutine wi(k)
    integer, intent(in) :: k
    print *, 'wi', k
  end subroutine
  subroutine wl(k)
    logical(kind=4), intent(in) :: k
    print *, 'wl', k
  end subroutine
  integer function oi(a, b)
    integer, intent(in) :: a, b
    oi = a - b
  end function
  logical function ol(a, b)
    logical, intent(in) :: a, b
    ol = a .and. b
  end function
end module
subroutine xi(k)
  integer k
  print *, 'xi', k
end
subroutine xl(k)
  logical k
  print *, 'xl', k
end
subroutine xil(k, l)
  implicit logical (l)
  print *, 'xil', k, l
end
subroutine xli(lf, kf)
  implicit logical (l)
  print *, 'xli', lf, kf
end
program p
  use m
  implicit none
  interface ext
    subroutine xi(k)
      integer k
    end subroutine
    subroutine xl(k)
      logical k
    end subroutine
    subroutine xil(k, l)
      implicit logical (l)
    end subroutine
    subroutine xli(lf, kf)
      implicit logical (l)
    end subroutine
  end interface
  logical :: t
  t = .true.
  call early(3, .false.)
  call early(-3, t)
  call ext(1)
  call ext(t)
  call ext(1, t)
  call ext(t, 1)
  call ext(2 > 1, 5)
  call ext(k = 9, l = .false.)
  call ext(l = .false., k = 9)
  call ext(kf = 9, lf = .false.)
end program
