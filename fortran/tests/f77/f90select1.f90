! SELECT CASE (Fortran 90 8.1.3): INTEGER selectors with values, open and
! closed ranges, PARAMETER bounds and CASE DEFAULT in the middle;
! CHARACTER ones with ranges and blank-padded comparison, construct names
! on some CASEs and not others; LOGICAL ones; CYCLE and EXIT of a DO
! from inside one; one inside another's CASE DEFAULT; one with no CASE.
program f90select1
  implicit none
  integer :: i, n
  character(len=6) :: w
  character(len=1) :: ch
  logical :: f
  integer, parameter :: lim = 7
  do i = -3, 12
    select case (i)
    case (:-1)
      print *, i, 'negative'
    case (0)
      print *, i, 'zero'
    case (1, 3, 5:lim)
      print *, i, 'odd-ish'
    case default
      print *, i, 'other'
    case (lim + 1:9)
      print *, i, 'eight-nine'
    end select
  end do
  do i = 1, 5
    w = 'x'
    if (i == 1) w = 'apple'
    if (i == 2) w = 'banana'
    if (i == 3) w = 'cherry'
    if (i == 4) w = 'zebra'
    sel: select case (w)
    case ('apple', 'cherry') sel
      print *, w, 1
    case ('b':'bz')
      print *, w, 2
    case ('y':) sel
      print *, w, 3
    end select sel
  end do
  do n = 1, 4
    f = mod(n, 2) == 0
    select case (f)
    case (.true.)
      print *, n, 'even'
    case (.false.)
      print *, n, 'odd'
    end select
  end do
  outer: do i = 1, 10
    select case (i)
    case (3)
      cycle outer
    case (6:)
      exit outer
    end select
    print *, 'loop', i
  end do outer
  ch = 'm'
  select case (ch)
  case ('a':'l')
    print *, 'first half'
  case default
    select case (ichar(ch))
    case (109)
      print *, 'inner m'
    end select
  end select
  select case (2 * 3)
  end select
  print *, 'done'
end program
