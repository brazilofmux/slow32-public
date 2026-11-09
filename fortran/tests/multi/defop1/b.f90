subroutine show(label, a)
  use money
  implicit none
  character(len=*), intent(in) :: label
  type(cents), intent(in) :: a
  character(len=16) :: s
  s = a
  print '(A,1X,A)', label, trim(s)
end subroutine
