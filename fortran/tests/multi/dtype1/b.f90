module shapes2
   use geom
   implicit none
   type box
      type(pt) :: lo, hi
      character(len=3) :: label
   end type
contains
   real function area(b)
      type(box), intent(in) :: b
      area = (b%hi%x - b%lo%x) * (b%hi%y - b%lo%y)
   end function
end module
