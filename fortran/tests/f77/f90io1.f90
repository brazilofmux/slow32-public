program p
  integer :: i, j, k
  character(len=40) :: s
  print '(Z5,1X,O5,1X,Z8.6,1X,B10,1X,B12.10)', 255, 8, 255, 5, 5
  print '(Z9,1X,O12)', -1, -1
  print '(ES12.4,EN12.3,1X,B0)', 1234.5, 1234.5, 6
  print '(B4.0,"|",Z3.0,"|")', 0, 0
  s = '  ff 777 1011'
  read (s, '(Z4,O4,B5)') i, j, k
  print *, i, j, k
  write (s, '(3Z4)') 10, 11, 12
  print *, '[', trim(s), ']'
end program
