! Fortran 95's WHERE: a WHERE inside a WHERE, ELSEWHERE with a mask, the
! construct's name.  Each mask is evaluated once, before its assignments,
! and only where the masks around it hold.
program w1
  implicit none
  integer :: a(6), b(6), i
  integer :: m(3,4), r(3,4)
  real :: x(5), y(5)
  logical :: l(6)
  a = (/ 3, -1, 4, -1, 5, -9 /)
  b = 0
  where (a > 0)
    b = 1
    where (a > 3)
      b = 2
    elsewhere
      b = 3
    end where
  elsewhere (a < -5)
    b = -2
  elsewhere
    b = -1
  end where
  print *, b
  ! three masks, one after another; the last part takes what is left
  b = 0
  where (a == 3)
    b = 30
  elsewhere (a == 4)
    b = 40
  elsewhere (a == 5)
    b = 50
  elsewhere (a == -1)
    b = 10
  end where
  print *, b
  ! the mask is the one of the WHERE statement, whatever its assignments do
  where (a > 0)
    a = -a
    b = a * 2
  elsewhere
    a = a - 100
    b = 7
  end where
  print *, a
  print *, b
  ! named, three deep, rank 2
  m = reshape((/ (i, i = 1, 12) /), (/ 3, 4 /))
  r = 0
  outer: where (m > 2)
    r = 1
    mid: where (mod(m, 2) == 0)
      r = 2
      inner: where (m > 8)
        r = 3
      elsewhere inner
        r = 4
      end where inner
    elsewhere (m > 6) mid
      r = 5
    elsewhere mid
      r = 6
    end where mid
  elsewhere outer
    r = -1
  end where outer
  do i = 1, 3
    print *, r(i, :)
  end do
  ! a WHERE statement inside the construct, and sections
  r = 0
  where (m(:, 1:3) > 4)
    where (m(:, 2:4) < 10) r(:, 1:3) = m(:, 2:4)
    r(:, 2:4) = r(:, 2:4) + 100
  end where
  do i = 1, 3
    print *, r(i, :)
  end do
  ! the inner mask's elements are computed only where the outer one holds
  x = (/ 4.0, -1.0, 0.0, 9.0, -16.0 /)
  y = -1.0
  where (x >= 0.0)
    where (sqrt(x) > 1.5)
      y = sqrt(x)
    elsewhere
      y = 0.5
    end where
  end where
  print '(5F6.2)', y
  ! an array-valued function in the masks
  l = (/ .true., .false., .true., .true., .false., .true. /)
  b = 0
  where (cshift(l, 1))
    b = 1
  elsewhere (eoshift(l, -1))
    b = 2
  elsewhere
    b = 3
  end where
  print *, b
end program
