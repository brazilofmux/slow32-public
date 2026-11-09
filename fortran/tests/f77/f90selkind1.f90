! SELECTED_REAL_KIND and SELECTED_INT_KIND (Fortran 90 13.13.94-95), in
! PARAMETERs and in executable expressions, by position and by keyword
! (P=, R=), and the kinds they give used.  Only what any processor with
! REAL(4), REAL(8) and INTEGER(4) gives the same: a request beyond them
! is the processor's to answer.
program f90selkind1
   implicit none
   integer, parameter :: dp = selected_real_kind(15, 307)
   integer, parameter :: sp = selected_real_kind(p=6)
   integer, parameter :: ik = selected_int_kind(r=9)
   real(dp) :: x
   real(sp) :: y
   integer(ik) :: n
   x = 1.0_dp / 3.0_dp
   y = 1.0_sp / 3.0_sp
   n = 123456789_ik
   print *, dp, sp, ik, x, y, n, kind(x), kind(y), kind(n)
   print *, selected_real_kind(p=10), selected_real_kind(r=100), selected_real_kind(6, 50)
   print *, selected_real_kind(r=30, p=5), selected_int_kind(9) == kind(1)
end program
