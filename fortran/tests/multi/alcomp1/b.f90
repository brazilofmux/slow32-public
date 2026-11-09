module keeper
  use store
  implicit none
  type(bag), save :: shelf
contains
  subroutine fill(k)
    integer, intent(in) :: k
    integer :: i, j
    type(bag) :: tmp
    tmp%n = k
    allocate(tmp%names(k), tmp%grid(k, 2))
    do i = 1, k
      write (tmp%names(i), '(A,I1)') 'item', i
      do j = 1, 2
        tmp%grid(i, j) = 10*i + j
      end do
    end do
    shelf = tmp
  end subroutine
end module
