SUBROUTINE graphics(xx, yy, Action ) 
  real :: xx, yy
  integer :: Action
  if (xx > 1.0 .and. yy > 1.0 .and. Action > 4) return
  return
END SUBROUTINE graphics