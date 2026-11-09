module lst
  type node
    integer :: v
    type(node), pointer :: next
  end type
  type buf
    integer :: n
    real, pointer :: d(:)
  end type
end module
