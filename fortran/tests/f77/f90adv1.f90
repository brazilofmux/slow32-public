program p
  character(len=1) :: ch
  character(len=8) :: w
  integer :: n, ios, k, total
  open (10, file='adv.txt', status='replace')
  write (10, '(A)') 'hello world'
  write (10, '(A)') 'ab'
  write (10, '(I3,I3,I3)') 1, 2, 3
  close (10)
  open (10, file='adv.txt', status='old')
  n = 0
  do
    read (10, '(A1)', advance='no', eor=100, iostat=ios) ch
    n = n + 1
    write (*, '(A)', advance='no') ch
  end do
100 continue
  print *, ' n=', n
  read (10, '(A8)', advance='no', size=k, eor=200) w
  print *, 'no eor?'
200 print *, 'size', k, '[', w, ']'
  total = 0
  do k = 1, 3
    read (10, '(I3)', advance='no') n
    total = total + n
  end do
  print *, total
  close (10, status='delete')
  write (*, '(A)', advance='no') 'abc'
  write (*, '(A)', advance='NO') 'def'
  write (*, '(I4)') 42
end program
