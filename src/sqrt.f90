interface
    function integrate  ( ibeg ,  iend , myfun , p)    result ( value )
        implicit  none
      
        real(kind = 8), intent(in) ::  ibeg
        real(kind = 8), intent(in) ::  iend
        procedure (fun_int) ::  myfun
        integer(kind = 4),intent(in) ::  p
        
        real(kind = 8) ::  value = 0

        real(kind = 8) :: x
        real(kind = 8) :: y
        real(kind = 8) :: middle1
        
        integer(kind = 4) :: i


        x=(iend-ibeg)/p
        middle1 = ibeg + (iend - ibeg)/(2 * p)
        
        do i=0,(p-1)
           y=myfun(middle1 + i*x)
           value = value +  x*y
        end do
        
    end function integrate
end  interface
