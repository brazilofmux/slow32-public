! T and TL move back over what is written: what follows overwrites it
! from there, and what was beyond stays.
program tfield
  write (*,'(a,t1,a,a)') '----------', 'left', 'mid'
  write (*,'(a,t1,a,''/'',a)') '----------', 'left', 'mid'
  write (*,'(a,t1,2a)') '----------', 'left', 'mid'
  write (*,'(a,t3,a,i3)') '----------', 'in', 42
  write (*,'(a,tl4,a,a)') '----------', 'x', 'y'
end
