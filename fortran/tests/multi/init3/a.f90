module mq
type c
integer :: v
character(len=2) :: s
end type
type(c), parameter :: one = c(1, "ab")
end module
