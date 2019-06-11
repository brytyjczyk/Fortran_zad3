module integration
contains
  implicit none
  
  interface trapezoid2
     function trapezoid_integration(ibeg, iend, myfun, p)  result (value)
    !  implicit none
      
      integer(kind = 4) :: p
      real(kind=8) ,intent(in)  ::  ibeg
      real(kind=8) ,intent(in)  ::  iend
      procedure (fun_int), pointer :: myfun
      
      real (kind = 8) :: value
      
      integer(kind = 4) :: i

      real(kind = 8) :: x
      real(kind = 8) :: a
      real(kind = 8) :: b
    end function trapezoid_integration
    
    function fsin (x) result (y)
      real(kind = 8), intent (in) :: x
      real(kind = 8):: y
    end function fsin

    function fe (x) result(y)
      real(kind = 8), intent (in) :: x
      real(kind = 8):: y
    end function fe

    function faixi(x, i, a) result (y)
      real(kind =8), intent (in) :: x
      integer(kind=4), intent (in) :: i
      real(kind=8), intent (in) :: a !wszystkie wyrazy ai są takie same
      real(kind =8) :: y
      integer(kind=4) :: j
    end function faixi

 end interface trapezoid2
  
    function trapezoid_integration(ibeg, iend, myfun, p)  result (value)
   !   implicit none
      
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
         a = myfun(x*i)
         b = myfun(x*i + x)
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

    function faixi(x, i, a) result (y)
      real(kind =8), intent (in) :: x
      integer(kind=4), intent (in) :: i
      real(kind=8), intent (in) :: a !wszystkie wyrazy ai są takie same
      real(kind =8) :: y
      integer(kind=4) :: j
      do j=1,i
         y= y + a * x^i
      end do
    end function faixi

  end module integration

!end interface trapezoid2
!https://stackoverflow.com/questions/12181888/fortran-passing-functions-as-arguments-in-other-functions 
