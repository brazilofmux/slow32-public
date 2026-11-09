module ifm
   interface
      real function twice(x)
         real, intent(in) :: x
      end function
   end interface
end module
program ifuse
   use ifm
   implicit none
   interface
      subroutine total(v, s, scale)
         real, intent(in) :: v(:)
         real, intent(out) :: s
         real, intent(in), optional :: scale
      end subroutine
      function evens(n)
         integer, intent(in) :: n
         integer :: evens(n)
      end function
      function tag(i)
         integer, intent(in) :: i
         character(len=6) :: tag
      end function
   end interface
   real :: a(2, 3), s
   integer :: k(4)
   character(len=6) :: t
   a = reshape((/ 1., 2., 3., 4., 5., 6. /), (/ 2, 3 /))
   call total(a(2, :), s)
   print *, s
   call total(scale=10.0, s=s, v=a(:, 1))
   print *, s
   k = evens(4)
   print *, k, sum(evens(3))
   t = tag(7)
   print *, '[', t, ']', twice(1.5)
end program
