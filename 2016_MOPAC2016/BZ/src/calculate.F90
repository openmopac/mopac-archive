
  
  
  subroutine Details_of_cursor_point(unit, mouseevent, keystate, MouseXpos,MouseYpos)
    use common_common, only : line, xscale, yscale, xoffset, yoffset, &
      top_l, top_r, bottom_l
   USE IFQWIN
    INTEGER unit
    INTEGER mouseevent
    INTEGER keystate
    INTEGER MouseXpos
    INTEGER MouseYpos
    double precision :: xx, yy
    integer :: status
    xx = (MouseXpos - xoffset)/xscale 
    yy = (MouseYpos - yoffset)/yscale 
    write(line,'(a,f6.2,",",f6.2,",",f6.2,a,f6.2)') &
    "Point (", xx, yy, yy, ") Value:", xx*yy
    status = SETACTIVEQQ (6)
    write(6,*)trim(line)
   return
  end subroutine Details_of_cursor_point
  