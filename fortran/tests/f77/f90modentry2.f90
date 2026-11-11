! An ENTRY of a module function with RESULT, each with a dummy of a
! derived type that has default initialization: INTENT(OUT) gives the
! dummy its default, INTENT(INOUT) leaves it.
module counters
  type counter
    integer :: n = 40
  end type counter
contains
  function reset_and_read(fresh) result(seen)
    integer :: seen
    type(counter), intent(out) :: fresh
    type(counter), intent(inout) :: kept
    seen = fresh%n
    fresh%n = -1
    return
  entry read_and_clear(kept) result(seen)
    seen = kept%n
    kept%n = -2
  end function reset_and_read
end module counters
program modentry2
  use counters
  type(counter) :: c
  c = counter(5)
  print *, reset_and_read(c), c%n
  c = counter(5)
  print *, read_and_clear(c), c%n
end
