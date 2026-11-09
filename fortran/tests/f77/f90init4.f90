module mq
type c
integer :: v
end type
type(c), parameter :: one = c(1)
end module
program p
use mq
type(c) :: z = one
print *, z
end
