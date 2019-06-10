interface trapezoid2

    function trapezoid_integration(ibeg, iend, myfun, p)  result (value)
      implicit none
      
      integer(kind = 4) :: p
      real(kind=8) ,intent(in)  ::  ibeg
      real(kind=8) ,intent(in)  ::  iend
      procedure ( fun_int )  ::  myfun
      
      real (kind = 8) :: value
      
      integer(kind = 4) :: i

      real(kind = 8) :: x
      real(kind = 8) :: a
      real(kind = 8) :: b

      x = (iedn - ibeg)/p
      value = 0

      do i = 0, p
         a = x*i
         b = a + x
         value = value + (a+b) * x/2
      enddo

    end function trapezoid_integration
    
 end interface trapezoid2
 
