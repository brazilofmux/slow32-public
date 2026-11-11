! An OPTIONAL dummy that is absent, as an intrinsic's own optional
! argument, is absent there (12.4.1.6): CMPLX's Y, MAX and MIN's later
! arguments, SIZE's DIM, and BACK of SCAN, INDEX and VERIFY.
program optintr
  implicit none
  integer :: grid(4,5)
  logical :: yes = .true.
  print *, pair(2.5, -1.0), pair(7.0)
  print *, most(3, 9, 6), most(3, 9, d=12), most(8, 2), least(4.5, 6.5), least(4.5, 6.5, 0.5)
  print *, extent(grid), extent(grid, 1), extent(grid, 2)
  print *, any_of('to be or not', 'nb'), any_of('to be or not', 'nb', yes), any_of('to be or not', 'nb', .false.)
  print *, where_is('tick tock tick', 'ick'), where_is('tick tock tick', 'ick', yes)
  print *, not_in('zzazz', 'z'), not_in('zzazzq', 'z', yes)
contains
  complex function pair(re, im)
    real :: re
    real, optional :: im
    pair = cmplx(re, im)
  end function
  integer function most(a, b, c, d)
    integer :: a, b
    integer, optional :: c, d
    most = max(a, b, c, d)
  end function
  real function least(a, b, c)
    real :: a, b
    real, optional :: c
    least = min(a, b, c)
  end function
  integer function extent(arr, along)
    integer :: arr(:,:)
    integer, optional :: along
    extent = size(arr, dim=along)
  end function
  integer function any_of(text, set, back)
    character(*) :: text, set
    logical, optional :: back
    any_of = scan(text, set, back)
  end function
  integer function where_is(text, piece, back)
    character(*) :: text, piece
    logical, optional :: back
    where_is = index(text, piece, back)
  end function
  integer function not_in(text, set, back)
    character(*) :: text, set
    logical, optional :: back
    not_in = verify(text, set, back=back)
  end function
end
