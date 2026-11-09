module store
  implicit none
  type bag
    integer :: n
    character(len=5), allocatable :: names(:)
    integer, allocatable :: grid(:,:)
  end type
end module
