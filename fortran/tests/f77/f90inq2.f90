program p
  character(len=12) :: act, pos, rd, wr, rw, dl, pd
  open (15, file='q2.txt', status='replace', action='write', position='append', delim='quote')
  inquire (unit=15, action=act, position=pos, read=rd, write=wr, readwrite=rw, delim=dl, pad=pd)
  print *, trim(act), ' ', trim(pos), ' ', trim(rd), ' ', trim(wr), ' ', trim(rw), ' ', trim(dl), ' ', trim(pd)
  close (15, status='delete')
  inquire (unit=16, action=act, position=pos, delim=dl, pad=pd)
  print *, trim(act), ' ', trim(pos), ' ', trim(dl), ' ', trim(pd)
  open (17, file='q3.txt', status='replace', action='read')
  inquire (unit=17, action=act, position=pos, read=rd, write=wr, readwrite=rw)
  print *, trim(act), ' ', trim(pos), ' ', trim(rd), ' ', trim(wr), ' ', trim(rw)
  close (17, status='delete')
end program
