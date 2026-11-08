C     THE PREPROCESSOR'S CONDITIONALS IN A .F FILE, AS LAPACK'S XCHKEE
C     HAS THEM: NO MACRO IS DEFINED, SO DEFINED(_OPENMP) IS FALSE AND
C     #IFNDEF TRUE; #ELIF, #ELSE, NESTING, AND && || ! IN EXPRESSIONS.
      PROGRAM CPP1
      INTEGER K
      K = 0
#if defined(_OPENMP)
      K = K + 1000
#else
      K = K + 1
#endif
#if defined(_OPENMP) && _OPENMP >= 201307
      K = K + 2000
#endif
#ifdef USE_IEEE_INTRINSIC
      K = K + 4000
#elif USE_ISNAN
      K = K + 8000
#else
      K = K + 10
#  ifndef NOTHING
      K = K + 100
#    if !defined(X) || (1 + 1 == 3)
      K = K + 1000
#    endif
#  endif
#endif
#if 0
      THIS IS NOT FORTRAN, AND IS NEVER SEEN
#endif
      WRITE (*, '(1X, I6)') K
      END
