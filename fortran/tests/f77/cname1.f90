! Construct names (Fortran 90): named DO, DO WHILE, DO forever and block
! IF; EXIT and CYCLE of an outer loop by name; ELSE, ELSE IF and END
! carrying the name.
program cname1
   implicit none
   integer :: i, j, k, n, tot
   tot = 0
   outer: do i = 1, 5
      inner: do j = 1, 5
         if (j > i) cycle outer
         if (i * j == 12) exit outer
         tot = tot + i * j
      end do inner
   end do outer
   print *, 'tot', tot, i, j
   n = 0
   k = 0
   loop: do
      k = k + 1
      w: do while (n < 100)
         n = n + k
         if (mod(n, 7) == 0) cycle loop
         if (n > 50) exit w
      end do w
      if (k >= 6) exit loop
   end do loop
   print *, 'n', n, k
   tot = 0
   do i = 1, 6
      chk: if (i < 2) then
         tot = tot + 1
      else if (i < 4) then chk
         tot = tot + 10
      else chk
         tot = tot + 100
      end if chk
   end do
   print *, 'if', tot
   k = 0
   do 20 i = 1, 3
      lab: do 10 j = 1, 3
         if (j == 2) cycle lab
         k = k + 1
10    end do lab
20 continue
   print *, 'lab', k
   k = 0
   do
      k = k + 1
      if (k == 4) exit
   end do
   print *, 'forever', k
   k = 0
   a: do i = 1, 3
      b: do j = 1, 3
         c: do n = 1, 3
            if (n == 2) cycle b
            if (i == 3) exit a
            k = k + 100 * i + 10 * j + n
         end do c
      end do b
   end do a
   print *, 'three', k, i
end program
