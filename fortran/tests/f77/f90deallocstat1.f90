program p
  integer i
  real, allocatable :: b4(:), b5(:), b6(:)
  real, pointer :: a1(:), a2(:)
  allocate(b5(2), b6(2), a2(3))
  nullify(a1)
  i = 13
  deallocate(b4, b5, stat=i); print *, i /= 0, allocated(b5)
  deallocate(b6, b4, stat=i); print *, i /= 0, allocated(b6)
  deallocate(a1, a2, stat=i); print *, i /= 0
  if (allocated(b5)) deallocate(b5)
  if (allocated(b6)) deallocate(b6)
  allocate(b4(2), b5(2))
  deallocate(b4, b5, stat=i); print *, i, allocated(b4), allocated(b5)
  allocate(b4(2), stat=i); print *, i
  allocate(b4(2), b5(3), stat=i); print *, i /= 0
end
