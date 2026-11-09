program mvuse
   use mvstate, only: count => hits, grid, label, work
   implicit none
   print *, count, '[', label, ']', allocated(work)
   call fill(5)
   print *, count, '[', label, ']', sum(grid), grid(2, 3), work, size(work)
end program
