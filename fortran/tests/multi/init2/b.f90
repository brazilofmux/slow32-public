program t
  use shapes
  implicit none
  integer :: k
  integer v(3)
  parameter (v = (/ 7, 8, 9 /))
  type(rgb) :: mine = rgb(9, 8, 7)
  print *, red, black
  print *, base%name, base%c, base%w, base%on
  print *, palette(1)%name, palette(1)%c, palette(2)%w, palette(2)%on
  print *, lut, lut(2), lbound(lut)
  print *, v, sum(v)
  print *, mine
  call bump()
  call bump()
  palette(1)%c%g = 99
  print *, palette(1)%c
contains
  subroutine bump()
    type(rgb) :: cnt = rgb(0, 0, 0)
    cnt%r = cnt%r + 1
    print *, cnt%r
  end subroutine
end program
