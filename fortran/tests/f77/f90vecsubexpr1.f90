program p
  integer :: a(20), b(5), iv(5) = (/5,1,3,2,4/), i
  a = (/ (i*10, i=1,20) /)
  b = a(2*iv)
  print *, b
  b = a(iv+1) + a(iv(5:1:-1))
  print *, b
  print *, a(2*iv)
  print *, sum(a(iv*2-1))
  a(2*iv) = b
  print *, a(1:12)
  call s(a(iv+10))
contains
  subroutine s(x)
    integer x(:)
    print *, x
  end subroutine
end
