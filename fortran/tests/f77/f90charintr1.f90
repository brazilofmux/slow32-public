! Fortran 90's CHARACTER intrinsics (13.13): ADJUSTL and ADJUSTR, scalar
! and elementwise; REPEAT with a run-time count; SCAN and VERIFY, with
! BACK, elementwise too; INDEX's BACK, constant and keyword; ACHAR and
! IACHAR.
program f90charintr1
  implicit none
  character(len=8) :: s, t(3)
  character(len=20) :: r
  character(len=*), parameter :: abc = 'abcabc'
  integer :: n, k(3)
  s = '  hi  '
  print *, '[', adjustl(s), '][', adjustr(s), ']', len(adjustl(s))
  t = (/ '  a     ', 'bb      ', '     ccc' /)
  print *, '[', adjustr(t), ']'
  print *, '[', adjustl(t), ']'
  n = 3
  r = repeat('ab', n)
  print *, '[', r, ']', len(repeat('xyz', 4)), '[', repeat(s(3:4), 2), ']'
  print *, scan(abc, 'cb'), scan(abc, 'cb', back=.true.), scan(abc, 'z'), verify(abc, 'ab'), verify('aabb', 'ab')
  print *, verify(abc, 'abc'), verify(abc, 'c', .true.), index(abc, 'bc'), index(abc, 'bc', back=.true.)
  print *, index(abc, 'bc', .false.), index(abc, 'x', .true.)
  k = scan(t, 'c')
  print *, k, verify(t, ' ')
  print *, achar(65), iachar('a'), achar(iachar('x') - 32)
end program
