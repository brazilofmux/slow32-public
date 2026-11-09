! Interface bodies (Fortran 90 12.3.2.1): externals of another file
! described in the caller -- an assumed-shape dummy (a descriptor
! passed), OPTIONAL and keyword arguments, an array-valued function, a
! CHARACTER function -- and in a module, for its users.
subroutine total(v, s, scale)
   real, intent(in) :: v(:)
   real, intent(out) :: s
   real, intent(in), optional :: scale
   s = sum(v)
   if (present(scale)) s = s * scale
end subroutine
function evens(n)
   integer, intent(in) :: n
   integer :: evens(n)
   integer :: i
   do i = 1, n
      evens(i) = 2 * i
   end do
end function
function tag(i)
   integer, intent(in) :: i
   character(len=6) :: tag
   write (tag, '(A,I3.3)') 'id', i
end function
real function twice(x)
   real, intent(in) :: x
   twice = 2.0 * x
end function
