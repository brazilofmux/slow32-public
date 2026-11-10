! FORALL, the construct: a name, two statements under one header (the
! second sees the first's stores), a logical IF's FORALL, three levels,
! masks on the inner levels, vector subscripts, the index variable
! keeping its own value, internal procedures.
program f95forall3
  implicit none
  integer :: a(5), i, j, k, m(3, 3), cnt(3), t(3, 3, 2)
  logical :: flag
  a = (/ 5, 4, 3, 2, 1 /)
  flag = .true.
  i = -1
  ! named construct, several statements: each is done in full first
  outer: forall (i = 1:5, a(i) > 1)
    a(i) = a(i) - 1
    a(6 - i) = a(6 - i) * 10 + i
  end forall outer
  print *, a
  ! logical IF's FORALL
  if (flag) forall (i = 1:5) a(i) = i
  print *, a
  if (.not. flag) forall (i = 1:5) a(i) = 0
  print *, a
  ! three levels
  m = 0
  t = 0
  forall (i = 1:3)
    forall (j = 1:3, j /= i)
      forall (k = 1:2, i + j > 3) t(i, j, k) = i * 10 + j + k
    end forall
    m(i, i) = -i
  end forall
  print *, t
  forall (i = 1:3, j = 1:3, i /= j) m(i, j) = t(i, j, 1)
  print *, m
  ! an inner mask naming the assigned: evaluated before any store of the statement
  forall (i = 1:3)
    forall (j = 1:3, m(j, i) < 0) m(i, j) = m(i, j) * 2
    forall (j = 1:3, m(i, j) == 0) m(j, i) = 7
  end forall
  print *, m
  ! vector subscript and index in a section
  cnt = (/ 3, 1, 2 /)
  forall (i = 1:3) m(cnt(i), i) = 100 + i
  print *, m
  forall (i = 1:3) m(i, cnt) = m(i, :)
  print *, m
  ! index name's variable untouched
  print *, i
  ! implicit typing of index
  call sub
contains
  subroutine sub
    integer :: b(4), i
    i = 42
    forall (i = 1:4) b(i) = 2 * i
    print *, b, i
    forall (i = 1:4) b(i) = b(5 - i) + helper(i)
    print *, b
  end subroutine
  pure integer function helper(n)
    integer, intent(in) :: n
    helper = n + a(1)
  end function
end program
