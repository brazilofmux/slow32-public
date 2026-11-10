! A routine with a SAVEd, initialized variable keeps it from call to
! call: spliced into its caller it had none, and every call began again.
integer function ticket(step)
  integer step
  integer, save :: last = 5
  last = last - step
  ticket = last
end function ticket
program savesplice
  integer ticket, t
  t = ticket(2)
  print *, t
  t = ticket(2)
  print *, t
  if (ticket(1)) 100, 200, 300
100 print *, 'below'
  goto 400
200 print *, 'none left'
  goto 400
300 print *, 'above'
400 print *, ticket(1)
end
