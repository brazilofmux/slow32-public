module shapes
  implicit none
  type rgb
    integer :: r, g, b
  end type
  type swatch
    character(len=6) :: name
    type(rgb) :: c
    real :: w(3)
    logical :: on
  end type
  type(rgb), parameter :: red = rgb(255, 0, 0), black = rgb(0, 0, 0)
  type(swatch), parameter :: base = swatch('base', rgb(1, 2, 3), (/ 0.5, 1.0, 1.5 /), .true.)
  type(swatch) :: palette(2) = (/ swatch('one', red, 0.0, .false.), swatch('two', black, (/ 1., 2., 3. /), .true.) /)
  integer, parameter :: lut(0:3) = (/ 10, 20, 30, 40 /)
end module
