      SUBROUTINE SEARCH(XPARAM,ALPHA,SIG,NVAR,GMIN,OKC,OKF, FUNCT,*)    CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION XPARAM(*), SIG(*)
************************************************************************
*
* SEARCH PERFORMS A LINE SEARCH FOR POWSQ. IT MINIMIZES THE NORM OF
*        THE GRADIENT VECTOR IN THE DIRECTION SIG.
*
* ON INPUT  XPARAM = CURRENT POINT IN NVAR DIMENSIONAL SPACE.
*           ALPHA  = STEP SIZE (IN FACT ALPHA IS CALCULATED IN SEARCH).
*           SIG    = SEARCH DIRECTION VECTOR.
*           NVAR   = NUMBER OF PARAMETERS IN SIG (& XPARAM)
*
* ON OUTPUT XPARAM = PARAMETERS OF MINIMUM.
*           ALPHA  = DISTANCE TO MINIMUM.
*           GMIN   = GRADIENT NORM AT MINIMUM.
*           OKC    = EXITED BEFORE COUNTS WERE EXCEEDED (LOGICAL)
*           OKF    = FUNCTION WAS IMPROVED.
************************************************************************
      COMMON /SIGMA1/ GNEXT, AMIN, ANEXT
      COMMON /SIGMA2/  GNEXT1(MAXPAR), GMIN1(MAXPAR)
      COMMON/KEYWRD/ KEYWRD
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION GRAD(MAXPAR),XREF(MAXPAR), GREF(MAXPAR), XMIN1(MAXPAR)
      CHARACTER*160 KEYWRD
      LOGICAL DEBUG, OKC, OKF, NOPR
         SAVE                                                           GL0892
      DATA ICALCN /0/
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
C
C    TOLG   = CRITERION FOR EXIT BY RELATIVE CHANGE IN GRADIENT.
C
         DEBUG=(INDEX(KEYWRD,'LINMIN') .NE. 0)
         NOPR=( .NOT. DEBUG)
         LOOKS=0
         OKF=.TRUE.
         TINY=0.1D0
         TOLERG=0.02D0
         IF(NVAR .EQ. 1)TIMY=0.D0
         G=100.D0
         XMAXM=2.D0
         ALPHA=0.1D0
      ENDIF
      ANEXT1=0.D0
      DO 10 I=1,NVAR
         GREF(I)  =GMIN1(I)
         GNEXT1(I)=GMIN1(I)
         XMIN1(I) =XPARAM(I)
   10 XREF(I)  =XPARAM(I)
      IF(ABS(ALPHA) .GT. 0.2)ALPHA=SIGN(0.2D0,ALPHA)
      IF(DEBUG.AND.DOPRNT) THEN
         WRITE(6,'('' SEARCH DIRECTION VECTOR'')')
         WRITE(6,'(6F12.6)')(SIG(I),I=1,NVAR)
         WRITE(6,'('' INITIAL GRADIENT VECTOR'')')
         WRITE(6,'(6F12.6)')(GMIN1(I),I=1,NVAR)
      ENDIF
      gb = ddot(nvar,gmin1,1,gref,1)
      IF(DEBUG.AND.DOPRNT) WRITE(6,'(                                   CIO
     &         '' GRADIENT AT START OF SEARCH:'',F16.6)') SQRT(GB)      CIO
      GSTORE=GB
      AMIN=0.D0
      GMINN=1.D9
C
C
      TA=0.D0
      GA=GB
      GB=1.D9
      ITRYS=0
      GOTO 30
   20 SUM=GA/(GA-GB)
      ITRYS=ITRYS+1
      IF(ABS(SUM) .GT. 3.D0) SUM=SIGN(3.D0,SUM)
      ALPHA=(TB-TA)*SUM+TA
C
C         XPARAM IS THE GEOMETRY OF THE PREDICTED MINIMUM ALONG THE LINE
C
   30 CONTINUE
      DO 40 I=1,NVAR
   40 XPARAM(I)=XREF(I)+ALPHA*SIG(I)
C
C         CALCULATE GRADIENT NORM AND GRADIENTS AT THE PREDICTED MINIMUM
C
      IF(ITRYS.EQ.1)THEN
      DO 50 I=1,NVAR
   50 GRAD(I)=0.D0
      ENDIF
      CALL COMPFG (XPARAM, .TRUE., FUNCT, .TRUE., GRAD, .TRUE.,*9999)    CSTP(call)
      LOOKS=LOOKS+1
C
C          G IS THE PROJECTION OF THE GRADIENT ALONG SIG.
C
      g = ddot(nvar,gref,1,grad,1)
      gtot = sqrt(ddot(nvar,grad,1,grad,1))
      IF( .NOT. NOPR .AND.DOPRNT)                                       CIO
     1WRITE(6,'('' LOOKS'',I3,'' ALPHA ='',F12.6,'' GRADIENT'',F12.3,
     2'' G  ='',F16.6)')
     3looks,alpha,gtot,g
      IF(GTOT .LT. GMINN) THEN
         GMINN=GTOT
         IF(ABS(AMIN-ALPHA) .GT.1.D-2) THEN
*
* WE CAN MOVE ANEXT TO A POINT NEAR, BUT NOT TOO NEAR, AMIN, SO THAT THE
* SECOND DERIVATIVESWILLBEREALISTIC(D2E/DX2=(GNEXT1-GMIN1)/(ANEXT-AMIN))
*
            ANEXT=AMIN
            DO 60 I=1,NVAR
   60       GNEXT1(I)=GMIN1(I)
         ENDIF
         AMIN=ALPHA
         DO 70 I=1,NVAR
            IF(GMINN.LT.GMIN) XMIN1(I)=XPARAM(I)
   70    GMIN1(I)=GRAD(I)
         IF(GMIN.GT.GMINN)GMIN=GMINN
      ENDIF
      IF(ITRYS .GT. 8) GOTO 80
      IF (ABS(G/GSTORE).LT.TINY .OR. ABS(G) .LT. TOLERG) GO TO 80
      IF(ABS(G) .LT. MAX(ABS(GA),ABS(GB)) .OR.
     1     GA*GB .GT. 0.D0 .AND. G*GA .LT. 0.D0) THEN
C
C   G IS AN IMPROVEMENT ON GA OR GB.
C
         IF(ABS(GB) .LT. ABS(GA))THEN
            TA=ALPHA
            GA=G
            GO TO 20
         ELSE
            TB=ALPHA
            GB=G
            GO TO 20
         ENDIF
      ELSE
C#         WRITE(6,'(//10X,'' FAILED IN SEARCH, SEARCH CONTINUING'')')
         GOTO 80
      ENDIF
   80 LNSTOP=4
      gminn = sqrt(ddot(nvar,gmin1,1,gmin1,1))
      DO 90 I=1,NVAR
   90 XPARAM(I)=XMIN1(I)
      IF(DEBUG.AND.DOPRNT) THEN                                         CIO
         WRITE(6,'('' AT EXIT FROM SEARCH'')')
         WRITE(6,'('' XPARAM'',6F12.6)')(XPARAM(I),I=1,NVAR)
         WRITE(6,'('' GNEXT1'',6F12.6)')(GNEXT1(I),I=1,NVAR)
         WRITE(6,'('' GMIN1 '',6F12.6)')(GMIN1(I),I=1,NVAR)
         WRITE(6,'('' AMIN, ANEXT,GMIN'',4F12.6)')
     1    AMIN,ANEXT,GMIN
      ENDIF
      IF(GMINN.GT.GMIN)THEN
      DO 45 I=1,NVAR
  45  XPARAM(I)=XREF(I)
      ENDIF
      RETURN
C
 9999 RETURN 1                                                          CSTP
      ENTRY SEARCH_INIT                                                 CSAV
            ANEXT1 = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
                 G = 0.0D0                                              CSAV
                GA = 0.0D0                                              CSAV
                GB = 0.0D0                                              CSAV
             GMINN = 0.0D0                                              CSAV
              GRAD = 0.0D0                                              CSAV
              GREF = 0.0D0                                              CSAV
            GSTORE = 0.0D0                                              CSAV
              GTOT = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
             ITRYS = 0                                                  CSAV
            LNSTOP = 0                                                  CSAV
             LOOKS = 0                                                  CSAV
              NOPR = .FALSE.                                            CSAV
               SUM = 0.0D0                                              CSAV
                TA = 0.0D0                                              CSAV
                TB = 0.0D0                                              CSAV
              TIMY = 0.0D0                                              CSAV
              TINY = 0.0D0                                              CSAV
            TOLERG = 0.0D0                                              CSAV
             XMAXM = 0.0D0                                              CSAV
             XMIN1 = 0.0D0                                              CSAV
              XREF = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
