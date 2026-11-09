program shop
  use money, only: cents, operator(+), assignment(=), operator(*), operator(.pct.), operator(==)
  implicit none
  type(cents) :: price, total, tax
  interface
    subroutine show(label, a)
      use money
      character(len=*), intent(in) :: label
      type(cents), intent(in) :: a
    end subroutine
  end interface
  price = '12.99'
  call show('price', price)
  total = 3 * price + cents(150)
  call show('total', total)
  tax = total .pct. 8
  call show('tax', tax)
  call show('due', total + tax)
  print *, total == 3 * price, tax == cents(324)
end program
