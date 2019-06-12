program main
  implicit none
  
  interface func
   function quadratic_integration(ibeg ,  iend , myfun , p)    result (value)
   
        real(kind = 8), intent(in) ::  ibeg
        real(kind = 8), intent(in) ::  iend
        procedure (fun_int), pointer ::  myfun
        integer(kind = 4),intent(in) ::  p
        
        real(kind = 8) ::  value
      end function quadratic_integration
      
    function trapezoid_integration(ibeg, iend, myfun, p)  result (value)
  
      integer(kind = 4) :: p
      real(kind=8) ,intent(in)  ::  ibeg
      real(kind=8) ,intent(in)  ::  iend
      procedure (fun_int), pointer :: myfun
      
      real (kind = 8) :: value
    end function trapezoid_integration

    function fsin (x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8):: y
    end function fsin

    function fe (x) result(y)
      real(kind = 8), intent (in) :: x
      real(kind = 8):: y
    end function fe

    function fa1x1(x) result (y)
      real(kind =8), intent (in) :: x
      real(kind =8) :: y
    end function fa1x1

    function fa3x3(x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8) :: y
    end function fa3x3

    function fa5x5(x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8) :: y
    end function fa5x5
 end interface func

  
   integer(kind = 4) :: p = 10
   real(kind = 8) ::  ibeg = 0
   real(kind = 8) ::  iend = 2
   real(kind = 8) ::  f
   
  open(unit=10, file = "/home/anna/Fortran/zad3/res/integration", action='write')
 
  write(10, *) "Trapezoid Integration"
  write(10, *) "Function sin(x)"
  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10"

  procedure (func), pointer :: f_ptr = null()
  f_ptr=> fsin
  f = & trapezoid_integration(ibeg, iend, f_ptr, p)
  write(10, *) 'result',  &trapezoid_integration(ibeg, iend, f_ptr, p)

    f = trapezoid_integration(ibeg, iend, f_ptr, p)
  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=100"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 10*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=1000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 100*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 1000*p)
!  write(10, *) f
!
!  write(10, *) "Quadratic Integration"
!  write(10, *) "Function sin(x)"
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10"
!  f = call trapezoid_integration(ibeg, iend, @fsin, p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=100"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 10*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=1000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 100*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 1000*p)
!  write(10, *) f
!
!  write(10, *) "Gauss Integration"
!  write(10, *) "Function sin(x)"
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10"
!  f = call trapezoid_integration(ibeg, iend, @fsin, p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=100"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 10*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=1000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 100*p)
!  write(10, *) f
!  write(10, *) "Parameters: ibeg = 0, iend = 2, p=10000"
!  f = call trapezoid_integration(ibeg, iend, @fsin, 1000*p)
!  write(10, *) f

  close(10)
  
end program main

   function quadratic_integration(ibeg ,  iend , myfun , p)    result (value)
        implicit  none
      
        real(kind = 8), intent(in) ::  ibeg
        real(kind = 8), intent(in) ::  iend
        procedure (fun_int), pointer ::  myfun
        integer(kind = 4),intent(in) ::  p
        
        real(kind = 8) ::  value = 0

        real(kind = 8) :: x
        real(kind = 8) :: y
        real(kind = 8) :: middle1
        
        integer(kind = 4) :: i


        x=(iend-ibeg)/p
        middle1 = ibeg + (iend - ibeg)/(2 * p)
        
        do i=0,(p-1)
           y= &myfun(middle1 + i*x)
           value = value +  x*y
        end do
        
      end function quadratic_integration
      
    function trapezoid_integration(ibeg, iend, myfun, p)  result (value)
      implicit none
      
      integer(kind = 4) :: p
      real(kind=8) ,intent(in)  ::  ibeg
      real(kind=8) ,intent(in)  ::  iend
      procedure (fun_int), pointer :: myfun
      
      real (kind = 8) :: value=0
      
      integer(kind = 4) :: i

      real(kind = 8) :: x
      real(kind = 8) :: a
      real(kind = 8) :: b

      x = (iend - ibeg)/p

      do i = 0, p
         a = call myfun(x*i)
         b = call myfun(x*i + x)
         value = value + (a+b) * x/2
      end do

    end function trapezoid_integration


    function fsin (x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8):: y
      y = sin(x)
    end function fsin

    function fe (x) result(y)
      real(kind = 8) intent (in) :: x
      real(kind = 8):: y
      y = e^x
    end function fe

    function fa1x1(x) result (y)
      real(kind =8), intent (in) :: x
      real(kind=8), parameter :: a = 4.23
      real(kind =8) :: y
      y = a * x^1
    end function fa1x1

    function fa3x3(x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8), parameter :: a = 6.12
      real(kind = 8) :: y
      y = a * x^1 + a * x^2 + a * x^3
    end function fa3x3

    function fa5x5(x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8) :: y
      real(kind = 8), parameter :: a = 5.67
      y = a*x^1 + a*x^2 + a*x^3 + a*x^4 + a*x^5
    end function fa5x5
