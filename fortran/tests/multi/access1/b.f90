program accuse
   use accm
   implicit none
   integer :: step
   step = -1
   call bump()
   print *, twice(3), twice(1.5), level, step
end program
