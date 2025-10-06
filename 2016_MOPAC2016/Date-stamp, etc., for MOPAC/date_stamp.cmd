REM
REM  Write the short FORTRAN 90 subroutine "GetDateStamp.F90" into the MOPAC source files, 
REM  and compile it.
REM
REM
REM   First, run Make_Date_Stamp.  This writes out a FORTRAN 90 file called GetDateStamp.F90
REM   into the MOPAC Source Code directory.  This file contains the current time as a character string.
REM
 cd /d M:\utility 
 call Make_Date_stamp.exe

