! s32f77 links with a heap a Fortran program can use (s32-ld's own default
! is 1 MB, and ALLOCATE of 720,000 bytes failed).
program p
  real, allocatable :: a(:,:)
  allocate (a(600, 600))
  a = 1.5
  print *, sum(a)
  deallocate (a)
end
