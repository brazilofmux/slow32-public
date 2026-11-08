program b
  use m1, only: dp => wp, half, twice
  real(dp) :: d
  d = half
  write (*, '(2f8.3, f8.3)') twice(d), twice(1.25), d
end program b
