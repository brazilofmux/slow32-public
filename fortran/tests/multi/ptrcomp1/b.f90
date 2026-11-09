program p
  use lst
  type(node), pointer :: h, q
  type(node), target :: n1, n2
  type(buf) :: b
  n2%v = 2; nullify(n2%next)
  n1%v = 1; n1%next => n2
  h => n1
  do while (associated(h))
    print *, h%v
    h => h%next
  end do
  allocate(b%d(3))
  b%d = (/ 1.5, 2.5, 3.5 /)
  print *, sum(b%d), size(b%d)
end program
