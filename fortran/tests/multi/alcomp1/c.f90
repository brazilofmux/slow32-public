program main
  use keeper
  implicit none
  type(bag) :: mine
  print *, allocated(shelf%names)
  call fill(3)
  print *, shelf%n, shelf%names, allocated(shelf%grid)
  print *, shelf%grid(2, :), shape(shelf%grid)
  mine = shelf
  call fill(2)
  print *, mine%names, '|', shelf%names
  print *, sum(mine%grid), sum(shelf%grid)
end program
