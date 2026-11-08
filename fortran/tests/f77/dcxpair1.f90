! SPDX-License-Identifier: BSD-3-Clause-Open-MPI
! Reduced from reference LAPACK's SRC/zlartg.f90, under its modified BSD
! licence (see NOTICE).
! The fp64 pair split at calls (HIR register allocator, all four copies):
! CONJG(GS) * X pairs GS's imaginary lo word with a sign-flipped hi word
! computed late, and that lo word, an argument of an earlier call
! (ABSSQ(GS)), took the flipped word along -- a copy of it planted before
! it existed.  S came out (2E-315, 5E-315), not (0.229, 0.458).  Reduced
! from LAPACK's ZLARTG, which the ZEC, ZGG and ZGD suites found.
subroutine zz( f, g, c, s, r )
   integer, parameter :: wp = kind(1.d0)
   real(wp), parameter :: zero = 0, safmin = real(radix(0._wp), wp)**max( &
      minexponent(0._wp) - 1, 1 - maxexponent(0._wp)), safmax = 1 / safmin
   complex(wp), parameter :: czero = (0, 0)
   real(wp)           c
   complex(wp)        f, g, r, s
   real(wp) :: d, f1, f2, g1, g2, h2, u, rtmin, rtmax
   complex(wp) :: fs, gs, t
   real(wp) :: ABSSQ
   ABSSQ( t ) = real( t )**2 + aimag( t )**2
   rtmin = sqrt( safmin )
   if( g == czero ) then
      c = 1
   else
      f1 = max( abs(real(f)), abs(aimag(f)) )
      g1 = max( abs(real(g)), abs(aimag(g)) )
      rtmax = sqrt( safmax/4 )
      if( f1 > rtmin .and. f1 < rtmax .and. &
          g1 > rtmin .and. g1 < rtmax ) then
         c = 2
      else
         u = min( safmax, max( safmin, f1, g1 ) )
         gs = g / u
         g2 = ABSSQ( gs )
         if( f1 / u < rtmin ) then
            c = 3
         else
            fs = f / u
            f2 = ABSSQ( fs )
            h2 = f2 + g2
         end if
         if( f2 >= h2 * safmin ) then
            c = sqrt( f2 / h2 )
            r = fs / c
            d = sqrt( f2 * h2 )
            s = conjg( gs ) * ( fs / d )
         end if
         r = r * u
      end if
   end if
end subroutine
program m3
   complex(kind(1.d0)) :: f, g, s, r
   double precision :: c
   f = (-1.5d-300, 0.75d-300)
   g = (0d0, -1d-300)
   call zz(f, g, c, s, r)
   write (*, '(5es24.16e3)') c, s, r
end program m3
