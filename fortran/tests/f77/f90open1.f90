program p
  character(len=20) :: line
  integer :: ios, n
  real :: x
  integer :: iv(3)
  namelist /cfg/ n, x, iv
  open (11, file='a2.txt', status='replace', action='write')
  write (11, '(A)') 'first'
  close (11)
  open (11, file='a2.txt', position='append', status='old')
  write (11, '(A)') 'second'
  close (11)
  open (11, file='a2.txt', status='old', action='read', pad='yes')
  do
    read (11, '(A)', iostat=ios) line
    if (ios /= 0) exit
    print *, trim(line)
  end do
  close (11)
  open (12, file='a3.txt', status='replace', delim='apostrophe')
  write (12, *) 'it''s', 'ok', 3
  close (12)
  open (12, file='a3.txt', status='old')
  read (12, '(A)') line
  print *, line
  close (12, status='delete')
  open (13, file='a4.txt', status='replace')
  write (13, '(A)') '&cfg n=5, x=2.5, iv=1,2,3 /'
  close (13)
  open (13, file='a4.txt', status='old')
  read (13, nml=cfg)
  print *, n, x, iv
  close (13, status='delete')
  open (11, file='a2.txt', status='old')
  close (11, status='delete')
end program
