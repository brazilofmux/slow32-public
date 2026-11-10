module m1
contains
  integer function s1()
    s1 = 0
  end function
  subroutine t1(k)
    k = 10
  end subroutine
end module
module m2
  use m1
contains
  subroutine s2
    integer k
    print *, s1()
    call t1(k)
    print *, k
    call inner
  contains
    integer function s1()
      s1 = 1
    end function
    subroutine t1(k)
      k = 11
    end subroutine
    subroutine inner
      integer k
      print *, s1()
      call t1(k)
      print *, k
    end subroutine
  end subroutine
  subroutine s3
    integer k
    print *, s1()
    call t1(k)
    print *, k
  end subroutine
end module
program p
  use m2
  call s2
  call s3
end
