      SUBROUTINE LINMIN(XPARAM,STEP,PVECT,NVAR,FUNCT,OKF,OKC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      DIMENSION XPARAM(NVAR),PVECT(NVAR)
      COMMON /GRAVEC/ COSINE
      COMMON /NUMCAL/ NUMCAL
C*********************************************************************
C
C  LINMIN DOES A LINE MINIMISATION.
C
C  ON INPUT:  XPARAM = STARTING COORDINATE OF SEARCH.
C             STEP   = STEP SIZE FOR INITIATING SEARCH.
C             PVECT  = DIRECTION OF SEARCH.
C             NVAR   = NUMBER OF VARIABLES IN XPARAM.
C             FUNCT  = INITIAL VALUE OF THE FUNCTION TO BE MINIMIZED.
C             ISOK   = NOT IMPORTANT.
C             COSINE = COSINE OF ANGLE OF CURRENT AND PREVIOUS GRADIENT.
C
C  ON OUTPUT: XPARAM = COORDINATE OF MINIMUM OF FUNCTI0N.
C             STEP   = NEW STEP SIZE, USED IN NEXT CALL OF LINMIN.
C             PVECT  = UNCHANGED, OR NEGATED, DEPENDING ON STEP.
C             FUNCT  = FINAL, MINIMUM VALUE OF THE FUNCTION.
C             OKF    = TRUE IF LINMIN IMPROVED FUNCT, FALSE OTHERWISE.
C             OKC    = TRUE IF LINMIN FOUND THE MINIMUM, FALSE OTHERWISE
C
C**********************************************************************
      COMMON /KEYWRD/ KEYWRD
C
C  THE FOLLOWING COMMON IS USED TO FIND OUT IF A NON-VARIATIONALLY
C  OPTIMIZED WAVE-FUNCTION IS BEING USED.
C
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      CHARACTER*80 KEYWRD
      DIMENSION PHI(3), VT(3)
      DIMENSION XSTOR(MAXPAR)
      INTEGER LEFT,RIGHT,CENTER
      LOGICAL PRINT,OKF,OKC, FULSCF, ASKFUL, HALFE
      DATA ICALCN /0/
      IF (ICALCN.NE.NUMCAL) THEN
         HALFE=(INDEX(KEYWRD,'C.I.') .NE. 0 .OR. NCLOSE.NE.NOPEN)
         ASKFUL=(INDEX(KEYWRD,'FULSCF') .NE. 0)
         DROP=0.00002D0
         IF(INDEX(KEYWRD,'PREC') .NE. 0) DROP=DROP*0.01D0
         IF(INDEX(KEYWRD,'GNORM') .NE. 0)
     1DROP=DROP*MIN(READA(KEYWRD,INDEX(KEYWRD,'GNORM')),1.D0)
         XMAXM  = 0.4D0
         I      = 2
         STEP   = 1.D0
         MAXLIN = 15
         XCRIT  = 0.0001D0
         IF(INDEX(KEYWRD,'FORCE') .NE. 0) THEN
            I=3
            XCRIT=0.00001D0
         ENDIF
         ANGLE=0.8D0
         IF(HALFE) ANGLE=-2.D0
         COSINE=99.99D0
C
C  ANGLE IS USED TO DECIDE IF P IS TO BE UPDATED AS CALCULATION
C        PROCEEDS.
C
         YMAXST  = 0.4D0
         EPS=10**(-I)
         TEE=EPS
         PRINT=(INDEX(KEYWRD,'LINMIN') .NE. 0)
         ICALCN=NUMCAL
      ENDIF
      FULSCF=(ASKFUL.OR.COSINE.GT.ANGLE)
      IF(PRINT) WRITE(6,'(''  FULL SCF CALCULATIONS:'',3X,L1)')FULSCF
      XMAXM=0.D0
      DO 10 I=1,NVAR
         PABS=ABS(PVECT(I))
   10 XMAXM=MAX(XMAXM,PABS)
      XMINM=XMAXM
      XMAXM=YMAXST/XMAXM
      FIN=FUNCT
      SSQLST=FUNCT
      IQUIT=0
      PHI(1)=FUNCT
      VT(1)=0.0D00
      VT(2)=STEP/4.0D00
      IF (VT(2).GT.XMAXM) VT(2)=XMAXM
      FMAX=FUNCT
      FMIN=FUNCT
      STEP=VT(2)
      DO 20 I=1,NVAR
   20 XPARAM(I)=XPARAM(I)+STEP*PVECT(I)
      CALL COMPFG(XPARAM, .TRUE., PHI(2),FULSCF,GRAD,.FALSE.)
      IF(PHI(2).GT.FMAX) FMAX=PHI(2)
      IF(PHI(2).LT.FMIN) FMIN=PHI(2)
      CALL EXCHNG (PHI(2),SQSTOR,ENERGY,ESTOR,XPARAM,XSTOR,
     1STEP,ALFS,NVAR)
      IF (PHI(1).LE.PHI(2)) GO TO 30
      GO TO 40
   30 VT(3)=-VT(2)
      LEFT=3
      CENTER=1
      RIGHT=2
      GO TO 50
   40 VT(3)=2.0D00*VT(2)
      LEFT=1
      CENTER=2
      RIGHT=3
   50 STLAST=VT(3)
      STEP=STLAST-STEP
      DO 60 I=1,NVAR
   60 XPARAM(I)=XPARAM(I)+STEP*PVECT(I)
      CALL COMPFG (XPARAM, .TRUE., FUNCT,FULSCF,GRAD,.FALSE.)
      IF(FUNCT.GT.FMAX) FMAX=FUNCT
      IF(FUNCT.LT.FMIN) FMIN=FUNCT
      IF (FUNCT.LT.SQSTOR) CALL EXCHNG (FUNCT,SQSTOR,ENERGY,
     1ESTOR,XPARAM,XSTOR,STEP,ALFS,NVAR)
      IF (FUNCT.LT.FIN) IQUIT=1
      PHI(3)=FUNCT
      IF (PRINT)WRITE (6,230) VT(1),PHI(1),VT(2),PHI(2),VT(3),PHI(3)
      OKC=.TRUE.
      DO 180 ICTR=3,MAXLIN
         ALPHA=VT(2)-VT(3)
         BETA=VT(3)-VT(1)
         GAMMA=VT(1)-VT(2)
         IF(ABS(ALPHA*BETA*GAMMA) .GT. 1.D-20)THEN
            ALPHA=-(PHI(1)*ALPHA+PHI(2)*BETA+PHI(3)*GAMMA)/(ALPHA*BETA*G
     1AMM   A)
         ELSE
            GOTO 190
         ENDIF
         BETA=((PHI(1)-PHI(2))/GAMMA)-ALPHA*(VT(1)+VT(2))
         IF (ALPHA) 70,70,100
   70    IF (PHI(RIGHT).GT.PHI(LEFT)) GO TO 80
         STEP=3.0D00*VT(RIGHT)-2.0D00*VT(CENTER)
         GO TO 90
   80    STEP=3.0D00*VT(LEFT)-2.0D00*VT(CENTER)
   90    S=STEP-STLAST
         IF (ABS(S).GT.XMAXM) S=SIGN(XMAXM,S)*(1+0.01*(XMAXM/S))
         STEP=S+STLAST
         GO TO 110
  100    STEP=-BETA/(2.0D00*ALPHA)
         S=STEP-STLAST
         XXM=2.0D00*XMAXM
         IF (ABS(S).GT.XXM) S=SIGN(XXM,S)*(1+0.01*(XXM/S))
         STEP=S+STLAST
  110    CONTINUE
         IF (ICTR.LE.3) GO TO 120
         AABS=ABS(S*XMINM)
         IF (AABS.LT.XCRIT) GO TO 190
  120    CONTINUE
         DO 130 I=1,NVAR
  130    XPARAM(I)=XPARAM(I)+S*PVECT(I)
         FUNOLD=FUNCT
         CALL COMPFG (XPARAM, .TRUE., FUNCT,FULSCF,GRAD,.FALSE.)
         IF(FUNCT.GT.FMAX) FMAX=FUNCT
         IF(FUNCT.LT.FMIN) FMIN=FUNCT
         IF (FUNCT.LT.SQSTOR) CALL EXCHNG (FUNCT,SQSTOR,ENERGY,ESTOR,
     1   XPARAM,XSTOR,STEP,ALFS,NVAR)
         IF (FUNCT.LT.FIN) IQUIT=1
         IF (PRINT) WRITE (6,240) VT(LEFT),PHI(LEFT),
     1                            VT(CENTER),PHI(CENTER),
     2                            VT(RIGHT),PHI(RIGHT),STEP,FUNCT
C
C TEST TO EXIT FROM LINMIN IF NOT DROPPING IN VALUE OF FUNCTION FAST.
C
         TINY = MAX((SSQLST-FMIN)*0.2D0 , DROP)
         TINY = MIN(TINY,0.5D0)
         IF(PRINT) WRITE(6,'(''  TINY'',F14.9)')TINY
         IF(ABS(FUNOLD-FUNCT) .LT. TINY .AND. IQUIT .EQ. 1) GOTO 190
         IF ((ABS(STEP-STLAST).LE.EPS*ABS(STEP+STLAST)+TEE).
     1   AND.(IQUIT.EQ.1)) GO TO 190
         STLAST=STEP
         IF ((STEP.GT.VT(RIGHT)).OR.(STEP.GT.VT(CENTER)
     1        .AND.FUNCT.LT.PHI(CENTER)).OR.(STEP.GT.VT(LEFT)
     2        .AND.STEP.LT.VT(CENTER).AND.FUNCT.GT.PHI(CENTER)))
     3         GOTO 140
         VT(RIGHT)=STEP
         PHI(RIGHT)=FUNCT
         GO TO 150
  140    VT(LEFT)=STEP
         PHI(LEFT)=FUNCT
  150    IF (VT(CENTER).LT.VT(RIGHT)) GO TO 160
         I=CENTER
         CENTER=RIGHT
         RIGHT=I
  160    IF (VT(LEFT).LT.VT(CENTER)) GO TO 170
         I=LEFT
         LEFT=CENTER
         CENTER=I
  170    IF (VT(CENTER).LT.VT(RIGHT)) GO TO 180
         I=CENTER
         CENTER=RIGHT
         RIGHT=I
  180 CONTINUE
      OKC=.FALSE.
  190 CONTINUE
      CALL EXCHNG (SQSTOR,FUNCT,ESTOR,ENERGY,XSTOR,XPARAM,
     1             ALFS,STEP,NVAR)
      OKF = (FUNCT.LT.SSQLST)
      IF (FUNCT.GE.SSQLST) RETURN
      IF (STEP) 200,220,220
  200 STEP=-STEP
      DO 210 I=1,NVAR
  210 PVECT(I)=-PVECT(I)
  220 CONTINUE
      RETURN
C
  230 FORMAT ( 11H ---QLINMN ,/5X, 'LEFT   ...',F17.8,F17.11/5X,
     1 'CENTER ...',F17.8,F17.11,/5X, 'RIGHT  ...',F17.8,F17.11,/)
  240 FORMAT (5X,'LEFT    ...',F17.8,F17.11,/5X,'CENTER  ...',
     1F17.8,F17.11,/5X,'RIGHT   ...',F17.8,F17.11,/5X,
     2 'NEW     ...',F17.8,F17.11,/)
C
      END
