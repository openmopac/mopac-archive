      SUBROUTINE SWAP(C,N,MDIM,NOCC,IFILL,*)
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION C(MDIM,MDIM)
C******************************************************************
C
C        SWAP ENSURES THAT A NAMED MOLECULAR ORBITAL IFILL IS FILLED
C ON INPUT
C          C = EIGENVECTORS IN A MDIM*MDIM MATRIX
C          N = NUMBER OF ORBITALS
C          NOCC = NUMBER OF OCCUPIED ORBITALS
C          IFILL = FILLED ORBITAL
C******************************************************************
      COMMON /SWAP0/ PSI(MAXORB), STDPSI(MAXORB)
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
CSAV         SAVE                                                           GL0892
      IF(IFILL.GT.0) GOTO 20
C
C     WE NOW DEFINE THE FILLED ORBITAL
C
      IFILL=-IFILL
      DO 10 I=1,N
         STDPSI(I)=C(I,IFILL)
   10 PSI(I)=C(I,IFILL)
      RETURN
   20 CONTINUE
C
C     FIRST FIND THE LOCATION OF IFILL
C
      SUM=0.D0
      DO 30 I=1,N
   30 SUM=SUM+PSI(I)*C(I,IFILL)
      IF(ABS(SUM).GT.0.7071D0) GOTO 90
C
C     IFILL HAS MOVED!
C
      SUMMAX=0.D0
      DO 50 IFILL=1,N
         SUM=0.D0
         DO 40 I=1,N
   40    SUM=SUM+STDPSI(I)*C(I,IFILL)
         SUM=ABS(SUM)
         IF(SUM.GT.SUMMAX)JFILL=IFILL
         IF(SUM.GT.SUMMAX)SUMMAX=SUM
         IF(SUM.GT.0.7071D0) GOTO 90
   50 CONTINUE
      DO 70 IFILL=1,N
         SUM=0.D0
         DO 60 I=1,N
   60    SUM=SUM+PSI(I)*C(I,IFILL)
         SUM=ABS(SUM)
         IF(SUM.GT.SUMMAX)JFILL=IFILL
         IF(SUM.GT.SUMMAX)SUMMAX=SUM
         IF(SUM.GT.0.7071D0) GOTO 90
   70 CONTINUE
      IF (DOPRNT) WRITE(6,80)SUMMAX,JFILL                               CIO
   80 FORMAT(/,' CAUTION !!! SUM IN SWAP VERY SMALL, SUMMAX =',F10.5,
     1' JFILL=',I3)
      IFILL=JFILL
   90 CONTINUE
      IF(IFILL.LE.NOCC) RETURN
C
C    ITS EMPTY, SO SWAP IT WITH THE HIGHEST FILLED
C
      DO 100 I=1,N
         X=C(I,NOCC)
         C(I,NOCC)=C(I,IFILL)
         C(I,IFILL)=X
  100 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
