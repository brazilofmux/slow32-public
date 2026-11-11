! ASSOCIATED(pointer, target): a dimension of one element has no stride
! to compare.
program p
  implicit none
  real, target :: t(2,3)
  real, pointer :: a(:,:), b(:,:)
  t = 0.0
  a => t(2:1:-2, 3:1:-2)
  b => a
  print *, shape(a)
  print *, associated(a), associated(a, b), associated(a, t)
  print *, associated(b, t(2:2, 3:1:-2)), associated(b, t(2:2, 1:3:2)), associated(b, t(1:1, 3:1:-2))
  a => t(1:2, 1:3:2)
  print *, associated(a, t(1:2, 1:3:2)), associated(a, t(2:1:-1, 1:3:2)), associated(a, t(:, 1:3))
end
