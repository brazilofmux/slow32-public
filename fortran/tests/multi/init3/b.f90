program p
use mq
type(c) :: z = one
type(c), parameter :: two(2) = (/ one, c(2, "cd") /)
print *, z, two(2)%s, two(1)%v
end
