! OPTIONAL and keyword arguments across files: the .mod file gives each
! dummy's name and whether it is OPTIONAL (and CHARACTER, whose hidden
! length an absent one still takes).
module optm
contains
   subroutine line(width, fill, label, n)
      integer, intent(in) :: width
      character(len=1), intent(in), optional :: fill
      character(len=*), intent(in), optional :: label
      integer, intent(out), optional :: n
      character(len=20) :: s
      character(len=1) :: f
      integer :: i
      f = '-'
      if (present(fill)) f = fill
      s = ' '
      do i = 1, width
         s(i:i) = f
      end do
      if (present(label)) then
         print *, s(1:width), ' ', label
      else
         print *, s(1:width)
      end if
      if (present(n)) n = width
   end subroutine
end module
