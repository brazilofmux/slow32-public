program optuse
   use optm
   implicit none
   integer :: got
   got = 0
   call line(5)
   call line(3, '*')
   call line(4, label='four')
   call line(n=got, width=6, fill='=')
   print *, got
   call line(2, '+', 'two', got)
   print *, got
end program
