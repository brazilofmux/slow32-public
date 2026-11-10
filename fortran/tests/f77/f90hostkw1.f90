! A host's variable that an internal procedure names only where, with the
! blanks gone, a statement keyword runs into it: DO X = ..., PRINT FMT,
! READ FMT, REWIND U, ASSIGN 20 TO LAB, GO TO LAB, RETURN M.  The scan
! for the host's candidates saw DOX and PRINTFMT, one name each, and the
! procedure was refused: "host association with a variable is not
! supported yet".
program p
  implicit none
  character(len=20) :: fmt
  integer :: u, k, lab, n, i, j, m, c
  real :: x
  fmt = '(1x, i5)'
  u = 9
  k = 0
  n = 2
  c = 0
  open (u, status='scratch')
  call loops
  print *, i, x, c
  call wr
  call rd
  call br
  call cg
  print *, k
  fmt = '(i5)'
  call rdin
  call alt(*10, *20)
  print *, 'zero'
10 print *, 'one'
20 print *, 'two', j, m
  if (k /= 42 .or. j /= 77 .or. c /= 3) stop 1
  stop 0
contains
  subroutine loops
    do i = 1, 2
      c = c + i
    end do
    do x = 1.0, 2.0
    end do
  end subroutine loops
  subroutine wr
    integer :: j
    j = 41
    print fmt, j
    write (u, fmt) j + 1
    endfile u
    rewind u
  end subroutine wr
  subroutine rd
    integer :: j
    read (u, fmt) j
    print *, j
    backspace u
    read (u, fmt) k
    rewind u
  end subroutine rd
  subroutine br
    assign 20 to lab
    go to lab
    print *, 'not here'
20  print *, 'assigned'
  end subroutine br
  subroutine cg
    go to (30, 40) n
30  print *, 'thirty'
40  print *, 'forty'
  end subroutine cg
  subroutine rdin
    read fmt, j
    m = 2
  end subroutine rdin
  subroutine alt(*, *)
    return m
  end subroutine alt
end program p
