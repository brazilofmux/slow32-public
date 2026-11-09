subroutine sub
  use ma
  implicit none
  type(t) :: l
  type(z) :: zz
  print *, l%n, l%w, l%c, l%in, associated(l%p), l%tail(40)
  print *, zz
  l%n = 0
  zz%k = 9
  call bump
end subroutine
program b
  use ma
  implicit none
  type(t) :: x(2)
  type(t), allocatable :: al(:)
  type(t), pointer :: pt, pa(:)
  call sub
  call sub
  print *, mv%n, marr(3)%in%a, marr(1)%in%a, mv%tail(1), marr(2)%tail(40)
  print *, x(2)%n, x(2)%w, x(2)%c, x(2)%in
  allocate(al(3), pt, pa(2))
  print *, al(3)%n, al(1)%w, al(2)%c, al(3)%in, associated(al(2)%p)
  print *, pt%n, pt%w, pt%c, pt%in, associated(pt%p), pt%tail(7)
  print *, pa(2)%n, pa(1)%w, pa(2)%c, pa(2)%in, associated(pa(1)%p)
  deallocate(al, pt, pa)
end program
