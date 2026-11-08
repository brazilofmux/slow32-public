! Fortran 90 free-form source: lower case, ! comments anywhere, &
! continuation with and without the leading &, a character constant
! continued, ; between statements, labels, long names with underscores,
! blanks that matter (and squeezed), FORMAT, and both quote forms.
program free_form_one
  implicit none
  integer, parameter :: n_items = 6
  double precision :: running_total, values(n_items)
  integer :: i, j; integer :: count_even = 0
  character(len=40) :: banner
  character(len=*), parameter :: greeting = "it's ""free"" form"

  banner = 'a constant continued &
           &across lines'          ! the & pair joins it
  write (*, '(a)') trim(banner)
  write (*, '(a)') greeting
  running_total = 0.0d0
  do i = 1, n_items
     values(i) = dble(i) * &
                 1.5d0 &          ! a comment after the &
                 + 0.25d0
     running_total = running_total + values(i)
     if (mod(i, 2) == 0) count_even = count_even + 1
  end do
  write (*, 100) running_total, count_even
100 format (1x, 'total', f10.3, ' even', i3)
  j = 0; i = 3; j = j + i ;  ; j = j * 2
  write (*, *) j
  call show_pairs( &
       n_items,    &
       values)
  if (running_total > 10.0d0 .and. &
      count_even /= 0) then
     write (*, '(a)') "big"
  else if (running_total <= 0d0) then
     write (*, '(a)') 'none'
  end if
  goto 200
  write (*, *) 'skipped'
200 continue
  write (*, '(a,i0)') 'label reached, j=', j
end program free_form_one

subroutine show_pairs(n, v)
  integer, intent(in) :: n
  double precision, intent(in) :: v(n)
  integer :: k
  do k = 1, n, 2
     write (*, '(2f8.2)') v(k), v(k + 1)
  end do
end subroutine show_pairs
