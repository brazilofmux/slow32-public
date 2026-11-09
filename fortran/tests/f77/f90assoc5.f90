! ASSOCIATED of two array pointers compares strides too; a zero-sized
! target is associated with nothing; rank 2; a section of a pointer array.
program p4i
  implicit none
  integer, target :: a(8), z(0), m(3,4)
  integer, pointer :: p(:), q(:), s, s2, p2(:,:), q2(:,:)
  integer :: i
  a = (/ (i, i = 1, 8) /)
  p => a(1:6:2)
  q => a(1:3)
  print *, associated(p, q), associated(q, p), associated(p, p), associated(q, q)
  q => a(1:5:2)
  print *, associated(p, q), associated(q, p)
  q => p
  print *, associated(p, q), associated(q, p(:)), associated(q, p(1:3)), associated(q, p(1:2)), associated(q, p(2:3))
  s => p(2)
  s2 => a(3)
  print *, associated(s, s2), associated(s, a(3)), associated(s, p(2)), associated(s, p(1)), associated(s2, q(2))
  p => z
  print *, associated(p), associated(p, z), size(p)
  p => a(5:4)
  print *, associated(p), associated(p, a(5:4)), size(p)
  q => a(5:4)
  print *, associated(p, q)
  p2 => m
  q2 => m(:, 2:3)
  print *, associated(p2, m), associated(p2, q2), associated(q2, m(:, 2:3)), associated(q2, m(1:3, 2:3)), associated(q2, m(:, 2:4))
  q2 => p2(2:3, :)
  print *, associated(q2, m(2:3, :)), associated(q2, p2(2:3, :)), associated(q2, p2(2:3, 1:4)), associated(q2, p2(1:2, :))
  q => p2(2, :)
  print *, associated(q, m(2, :)), associated(q, p2(2, :)), associated(q, m(:, 2)), associated(q, p2(3, :))
end program
