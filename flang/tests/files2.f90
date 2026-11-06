subroutine fill
  implicit none
  integer :: all(4), blank(1000)
  common /shared/ all
  common blank
  all = [31, 32, 33, 34]
  blank = 0
  blank(1:2) = [41, 42]
  blank(1000) = 71
end subroutine

subroutine check
  implicit none
  integer :: all(4)
  common /shared/ all
  if (all(2) /= 61) stop 5
end subroutine

block data filldata
  implicit none
  integer :: preset(4)
  common /preset/ preset
  data preset /51, 52, 53, 54/
end block data
