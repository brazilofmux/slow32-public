program p
  logical :: ex, op
  integer :: nr, rl
  character(len=12) :: act, pos, rd, wr, rw, dl, pd, nm, fm, acc, seqq
  open (14, file='q1.txt', status='replace')
  inquire (unit=14, exist=ex, opened=op, name=nm, form=fm, access=acc, sequential=seqq)
  print *, ex, op, trim(nm), ' ', trim(fm), ' ', trim(acc), ' ', trim(seqq)
  inquire (unit=14, action=act, position=pos, read=rd, write=wr, readwrite=rw, delim=dl, pad=pd)
  print *, trim(act), ' ', trim(pos), ' ', trim(rd), ' ', trim(wr), ' ', trim(rw), ' ', trim(dl), ' ', trim(pd)
  inquire (iolength=rl) nr, ex
  print *, rl
  close (14, status='delete')
end program
