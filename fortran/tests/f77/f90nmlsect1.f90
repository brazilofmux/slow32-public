! NAMELIST input of array sections and elements -- (:), (lo:hi) with a
! repeat count, single elements -- with each way of ending a group, a
! name in either case, and a comment in the input.
program nmlsect
  implicit none
  integer, dimension(6) :: slots = -1
  integer :: pass, ios, depth = 0
  real :: scale = 0.0
  character(6) :: label = ' '
  logical :: ready = .false.
  namelist /setup/ slots, depth, scale, label, ready
  open (12, status = 'scratch')
  write (12,'(A)') '&setup slots(:)=11,12,13,14,15,16 /'
  write (12,'(A)') '&setup slots(1:6)=6*3 &end'
  write (12,'(A)') '&SETUP slots(4:5)=2*8, DEPTH = 9,'
  write (12,'(A)') '  scale=0.75, label=''o''''k'', ready=.true. /'
  write (12,'(A)') '$setup slots(1)=100 slots(6) = 600 $end'
  write (12,'(A)') ' &setup slots = 21, 22, depth=4 ! the rest stay'
  write (12,'(A)') ' label = ''end'' /'
  rewind (12)
  do pass = 1, 5
    read (12, nml = setup, iostat = ios)
    print *, ios, slots, depth, scale, label, ready
  end do
  close (12)
end
