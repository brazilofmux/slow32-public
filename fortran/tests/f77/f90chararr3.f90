! CHARACTER arrays (Fortran 90), continued: constructors with implied-DOs
! and to a substring of each element; a section assigned from itself
! reversed; rank 2 with substrings of row sections; a READ into a
! reversed section's substrings; comparison of two sections; LEN of a
! whole array; elements, sections and expressions in one PRINT list.
program f90chararr3
  implicit none
  character(len=2) :: c(6), m(2,3)
  character(len=5) :: rec
  integer :: i
  c = (/ (char(64+i) // char(96+i), i = 1, 6) /)
  print '(6(A,1X))', c
  c(6:1:-1) = c
  print '(6(A,1X))', c
  c(2:6:2) = (/ 'xx', c(1), 'zz' /)
  print '(6(A,1X))', c
  c(:)(2:2) = (/ ('#', i = 1, 6) /)
  print '(6(A,1X))', c
  m(1,:) = (/ 'pq', 'rs', 'tu' /)
  m(2,:) = (/ 'PQ', 'RS', 'TU' /)
  print '(6(A,1X))', m
  m(1,:) = m(2,:)(2:2) // m(2,:)(1:1)
  print '(6(A,1X))', m
  rec = 'ABCDE'
  read (rec, '(5A1)') c(5:1:-1)(1:1)
  print '(6(A,1X))', c
  print '(3L2)', m(1,:) < m(2,:)
  print '(I3)', len(c)
  print *, (c(i) // '|', i = 1, 3), c(4:6)(1:1) // '/'
end program
