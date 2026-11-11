! MVBITS is elemental: its FROM may be the TO array through a vector
! subscript, a stride or a pointer, and every FROM element is the one from
! before the call.
program mvvec
  integer :: src(8) = (/ 3, 12, 48, 5, 80, 17, 255, 64 /)
  integer :: wide(16)
  integer, target :: tgt(8)
  integer, pointer :: p(:)
  integer, parameter :: order(8) = (/ 8, 1, 1, 5, 3, 8, 2, 6 /)
  wide = 0
  wide(1:15:2) = src
  tgt = src
  call mvbits(src(order), 1, 5, src, 2)
  write (*,'(8I5)') src
  write (*,'(8I5)') wide(2*order - 1)
  call mvbits(wide(2*order - 1), 1, 5, wide(1:15:2), 2)
  write (*,'(8I5)') wide(1:15:2)
  p => tgt
  call mvbits(tgt(order), 1, 5, p, 2)
  write (*,'(8I5)') tgt
end
