      SUBROUTINE TIMOUT(NOUT,TIM,*)                                     CSTP
C
C     CONVERT THE TIME FROM SECONDS TO DAYS, HOURS, MINUTES, AND SECONDS
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C
      DOUBLE PRECISION MINS, MINPHR
CSAV         SAVE                                                           GL0892
C
C
      DATA HRSPD /24.0D0/, MINPHR /60.0D0/
      DATA SECPD /86400.0D0/, SECPHR /3600.0D0/, SECPMI /60.0D0/
C
      DAYS = TIM / SECPD
      IDAYS = INT(DAYS)
      HOURS = (DAYS - FLOAT(IDAYS)) * HRSPD
      IHOURS = INT(HOURS)
      MINS = (HOURS - FLOAT(IHOURS)) * MINPHR
      IMINS = INT(MINS)
      SECS = (MINS - FLOAT(IMINS)) * SECPMI
C
      IF (DOPRNT) THEN                                                  CIO
       IF (IDAYS .GT. 1) THEN
         WRITE (NOUT,10) IDAYS,IHOURS,IMINS,SECS
       ELSE IF (IDAYS .EQ. 1) THEN
         WRITE (NOUT,20) IDAYS,IHOURS,IMINS,SECS
       ELSE IF (IHOURS .GT. 0) THEN
         WRITE (NOUT,30) IHOURS,IMINS,SECS
       ELSE IF (IMINS .GT. 0) THEN
         WRITE (NOUT,40) IMINS,SECS
       ELSE
         WRITE (NOUT,50) SECS
       END IF
      ENDIF                                                             CIO
C
   10 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'DAYS',2X,I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   20 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'DAY',2X,I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   30 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'HOURS',
     1        1X,I2,1X,'MINUTES AND',1X,F7.3,1X,'SECONDS')
   40 FORMAT (10X,'COMPUTATION TIME = ',I2,1X,'MINUTES AND',
     1        1X,F7.3,1X,'SECONDS')
   50 FORMAT (10X,'COMPUTATION TIME = ',F7.3,1X,'SECONDS')
      RETURN
 9999 RETURN 1                                                          CSTP
      END
