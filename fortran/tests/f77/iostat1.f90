! IOSTAT= is defined, with zero, by every I/O statement that completes:
! REWIND, WRITE and READ unformatted (into a vector subscript and a
! stride), a formatted WRITE, BACKSPACE, ENDFILE and CLOSE.
program iost
  real :: out(6), back(6)
  integer :: perm(6) = (/ 4, 6, 1, 3, 2, 5 /)
  integer :: ios
  out(perm) = 0.5 * perm
  back = -1.0
  open (unit = 31, status = 'scratch', form = 'unformatted')
  ios = 77
  rewind (31, iostat = ios)
  print *, ios
  ios = 77
  write (31, iostat = ios) out
  print *, ios
  ios = 77
  rewind (31, iostat = ios)
  read (31, iostat = ios) back(perm)
  print *, ios
  print *, back
  rewind 31
  back = -2.0
  read (31, iostat = ios) back(2:6:2)
  print *, ios, back
  ios = 77
  write (*, '(a)', iostat = ios) 'line'
  print *, ios
  ios = 77
  backspace (31, iostat = ios)
  print *, ios
  ios = 77
  endfile (31, iostat = ios)
  print *, ios
  ios = 77
  close (31, iostat = ios)
  print *, ios
end
