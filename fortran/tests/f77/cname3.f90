! A construct name is optional on ELSE IF, ELSE and CASE (F90 8.1.2.1,
! 8.1.3.1); only the END needs it.  ELSE and ELSE IF without it had been
! refused in a named IF.
program cname3
   implicit none
   integer :: i
   do i = -1, 1
      a: if (i > 0) then
         print *, i, 'positive'
      else if (i < 0) then
         print *, i, 'negative'
      else a
         print *, i, 'zero'
      end if a
      b: select case (i)
      case (-1) b
         print *, 'minus one'
      case default
         print *, 'not minus one'
      end select b
   end do
end program
