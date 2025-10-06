Program Make_Date_Stamp
 use ifport
 character  :: todays_date*24, julian*8
integer :: ioutput=16
todays_date = " "
call fdate (todays_date)
julian = jdate()
ijulian =  (ichar(julian(2:2)) - ichar('6'))*365 + &
             (ichar(julian(3:3)) - ichar('0'))*100 + &
             (ichar(julian(4:4)) - ichar('0'))*10 + &
             (ichar(julian(5:5)) - ichar('0')) 
open (unit=ioutput, &
& file="M:\PARAM\Source Code\MOPAC and PARAM source code\MOPAC_Source_code\GetDateStamp.F90", status="UNKNOWN") 
write(ioutput,*)"subroutine GetDateStamp(date_time, version)"
write(ioutput,*)"  character :: date_time*24, version*7"
write(ioutput,*)"  date_time = """//todays_date//""""
write(ioutput,'(5a)')'   version = "',julian(1:2),'.',julian(3:5),'"'
write(ioutput,*)"  return"
write(ioutput,*)"end subroutine GetDateStamp"
end program Make_Date_Stamp
