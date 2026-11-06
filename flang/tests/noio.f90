function addup(n) result(s)
  integer, intent(in) :: n
  integer :: s, i
  s = 0
  do i = 1, n
    s = s + i
  end do
end function addup
