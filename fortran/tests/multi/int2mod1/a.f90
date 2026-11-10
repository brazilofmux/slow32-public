module m
contains
  integer*2 function hf(k)
    integer*2 k
    hf = k * 2
  end function
  logical*1 function lf(k)
    integer k
    lf = k > 0
  end function
end module
