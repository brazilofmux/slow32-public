program cfuse
   use cfm
   implicit none
   complex(kind=8) :: z
   character(len=4) :: a, b
   print *, '[', greet('bob'), ']', len(greet('x'))
   a = shout(42)
   b = shout('ab')
   print *, '[', a, '][', b, ']'
   z = rot((1d0, 2d0))
   print *, z, rot(rot(z))
end program
