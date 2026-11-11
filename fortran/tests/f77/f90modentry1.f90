! ENTRY in module procedures: each entry is a module procedure of its own,
! for the module's other procedures and for its users -- a subroutine's, a
! function's with a result of its own, a CHARACTER function's.
module entries
contains
  subroutine pair(x, y)
    integer x, y
    print *, 'pair', x, y
    return
  entry single(x)
    print *, 'single', x
  end subroutine
  subroutine both
    call pair(21, 22)
    call single(23)
  end subroutine
  function squared(x)
    integer :: squared, halved, x, y
    squared = x * x
    return
  entry halved(y)
    halved = y / 2
  end function squared
  function upper3(s)
    character(3) :: upper3, tagged, s, t
    upper3 = s
    if (s .eq. 'abc') upper3 = 'ABC'
    return
  entry tagged(t)
    tagged = t
    tagged(3:3) = '#'
  end function upper3
end module
program modentry
  use entries
  call both
  call single(24)
  print *, squared(9), halved(9)
  print *, upper3('abc'), upper3('xyz'), tagged('pqr')
end
