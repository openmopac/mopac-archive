      SUBROUTINE FLEPO (XPARAM,NVAR,FUNCT1,*)                           CSTP
***************************************************************
*                                                             *
* THIS SUBROUTINE USES THE BFGS UPDATE TO THE INVERSE HESSIAN *
* THE NAME FLEPO IS KEPT IN ORDER TO ALLOW COMPATABILITY WITH *
* OTHER PROGRAMS.   THE DFP FORMULA IS NOTED IN THE COMMENTS. *
*                                                             *
***************************************************************
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION XPARAM(*)
      COMMON /KEYWRD/ KEYWRD
      COMMON /GRAVEC/ COSINE
      COMMON /LAST  / LAST
C    changed common path for portability  (IR)
      COMMON /PATHI / LATOM,LPARAM
      COMMON /PATHR / REACT(200)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /MESAGE/ IFLEPO,ISCF
      COMMON /NUMSCF/ NSCF
      COMMON /TIMER / TIME0
      COMMON /FMATRX/ HESINV(MAXHES)
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      CHARACTER*160 KEYWRD
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
C
C     *
C     THIS SUBROUTINE ATTEMPTS TO MINIMIZE A REAL-VALUED FUNCTION OF
C     THE N-COMPONENT REAL VECTOR XPARAM ACCORDING TO THE
C     BFGS FORMULA. RELEVANT REFERENCES ARE
C
C     BROYDEN, C.G., JOURNAL OF THE INSTITUTE FOR MATHEMATICS AND
C                     APPLICATIONS, VOL. 6 PP 222-231, 1970.
C     FLETCHER, R., COMPUTER JOURNAL, VOL. 13, PP 317-322, 1970.
C
C     GOLDFARB, D. MATHEMATICS OF COMPUTATION, VOL. 24, PP 23-26, 1970.
C
C     SHANNO, D.F. MATHEMATICS OF COMPUTATION, VOL. 24, PP 647-656
C                    1970.
C
C   SEE ALSO SUMMARY IN
C
C    SHANNO, D.F., J. OF OPTIMIZATION THEORY AND APPLICATIONS
C          VOL.46, NO 1 PP 87-94 1985.
C     THE USER MUST SUPPLY THE SUBROUTINE
C     COMPFG(NVAR,XPARAM,FUNCT,GRAD,1)WHICH
C     COMPUTES FUNCTION VALUES  FUNCT  AND GRADIENTS  GRAD AT GIVEN
C     VALUES FOR THE VARIABLES XPARAM.  THE MINIMIZATION PROCEEDS BY ONE
C     OR MORE BINARY CHOPS WHICH FINDS AN IMPROVED VALUE OF THE
C     FUNCTION IN THE DIRECTION   XPARAM+ALPHA*PVECT,
C     WHERE XPARAM
C     IS THE VECTOR OF CURRENT VARIABLE VALUES,  ALPHA IS A SCALAR
C     VARIABLE, AND  PVECT  IS A SEARCH-DIRECTION VECTOR PROVIDED BY THE
C     BFGS ALGORITHM.  A SEQUENCE OF FUNCT VALUES CONVERGING TO SOME
C     LOCAL MINIMUM VALUE AND A SEQUENCE OF
C     XPARAM VECTORS CONVERGING TO THE CORRESPONDING MINIMUM POINT
C     ARE PRODUCED.
C                          CONVERGENCE TESTS.
C
C     HERBERTS TEST: THE ESTIMATED DISTANCE FROM THE CURRENT POINT
C                    POINT TO THE MINIMUM IS LESS THAN TOLERA.
C
C                    "HERBERTS TEST SATISFIED - GEOMETRY OPTIMIZED"
C
C     GRADIENT TEST: THE GRADIENT NORM HAS BECOME LESS THAN TOLERG
C                    TIMES THE SQUARE ROOT OF THE NUMBER OF VARIABLES.
C
C                    "TEST ON GRADIENT SATISFIED".
C
C     XPARAM TEST:  THE RELATIVE CHANGE IN XPARAM, MEASURED BY ITS NORM,
C                   OVER ANY TWO SUCCESSIVE ITERATION STEPS DROPS BELOW
C                   TOLERX.
C
C                    "TEST ON XPARAM SATISFIED".
C
C     FUNCTION TEST: THE CALCULATED VALUE OF THE HEAT OF FORMATION
C                    BETWEEN ANY TWO CYCLES IS WITHIN TOLERF OF
C                    EACH OTHER.
C
C                    "HEAT OF FORMATION TEST SATISFIED"
C
C     FOR THE GRADIENT, FUNCTION, AND XPARAM TESTS A FURTHER CONDITION,
C     THAT NO INDIVIDUAL COMPONENT OF THE GRADIENT IS GREATER
C     THAN TOLERG, MUST BE SATISFIED, IN WHICH CASE THE
C     CALCULATION EXITS WITH THE MESSAGE
C
C                     "PETERS TEST SATISFIED"
C
C     WILL BE PRINTED, AND FUNCT AND XPARAM WILL CONTAIN THE LAST
C     FUNCTION VALUE CUM VARIABLE VALUES REACHED.
C
C     SIMILAR UNSUCCESSFUL TERMINATIONS WILL TAKE PLACE IF THE COSINE OF
C     THE SEARCH DIRECTION TO GRADIENT VECTOR IS LESS THAN RST ON TWO
C     CONSECUTIVE ITERATIONS.
C
C     THE BROYDEN-FLETCHER-GOLDFARB-SHANNO ALGORITHM CHOOSES SEARCH
C     DIRECTIONS ON THE BASIS OF LOCAL PROPERTIES OF THE FUNCTION.
C     A MATRIX  H, WHICH IN FLEPO IS PRESET WITH THE IDENTITY, IS
C     MAINTAINED AND UPDATED AT EACH ITERATION STEP.
C     THE MATRIX DESCRIBES A LOCAL METRIC ON THE SURFACE OF FUNCTION
C     VALUES ABOVE THE POINT XPARAM.  THE SEARCH-DIRECTION VECTOR
C     PVECT  IS SIMPLY A TRANSFORMATION OF THE GRADIENT  GRAD
C     BY THE MATRIX H.  THE USER THROWS OUT H AND STARTS AGAIN
C     WHENEVER THE COSINE OF THE ANGLE BETWEEN  GRAD  AND PVECT BECOMES
C     LESS THAN RST.
C
      DIMENSION XD(MAXPAR), GD(MAXPAR), GLAST(MAXPAR), MDFP(9),XDFP(9),
     1          XLAST(MAXPAR), GG(MAXPAR), PVECT(MAXPAR)
      LOGICAL OKF, OKC, PRINT,  ISTIME, RESTRT, MINPRT, SADDLE, GEOOK
     1        ,RESET, RESFIL, DFP
      EQUIVALENCE (MDFP(1),JCYC  ),(MDFP(2),JNRST),(MDFP(3),NCOUNT),
     1            (MDFP(4),LNSTOP),(XDFP(1),ALPHA),(XDFP(2),COS   ),
     2            (XDFP(3),PNORM ),(XDFP(4),DROP ),(XDFP(5),DEL   ),
     3            (XDFP(6),FREPF ),(XDFP(7),CYCMX),(XDFP(8),TOTIME)
         SAVE                                                           GL0892
      DATA ICALCN /0/
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
      IF (ICALCN.NE.NUMCAL) THEN
C
C   THE FOLLOWING CONSTANTS SHOULD BE SET BY THE USER.
C
         RST   = 0.05D0
         TDEL  = 6.D0
         SFACT=1.5
         PMSTE = 0.1D0
         DELL  = 0.01D0
         EINC  = 0.3D0
         IGG1  = 300
         DEL=DELL
C
C    THESE CONSTANTS SHOULD BE SET BY THE PROGRAM.
C
         RESTRT = (INDEX(KEYWRD,'RESTART').NE.0)
         GEOOK  = (INDEX(KEYWRD,'GEO-OK').NE.0)
         ISTIME   = (INDEX(KEYWRD,'TIME').NE.0)
C
C   THE DAVIDON-FLETCHER-POWELL METHOD IS NOT RECOMMENDED
C   BUT CAN BE INVOKED BY USING THE KEY-WORD 'DFP'
C
         DFP   = (INDEX(KEYWRD,'DFP').NE.0)
         TLEFT=MAXTIM
         TOLERG=1.0D0
         CONST=1.D0
C        MINPRT=(INDEX(KEYWRD,'SADDLE')+INDEX(KEYWRD,'SADDLE') .EQ.0)
         MINPRT=(INDEX(KEYWRD,'SADDLE').EQ.0.AND.INDEX(KEYWRD,'SADDLE')
     1  .EQ.0) 
         SADDLE=(INDEX(KEYWRD,'SADDLE') .NE.0)
         IF( .NOT. MINPRT) MINPRT=(INDEX(KEYWRD,'DEBUG') .NE. 0)
         I=INDEX(KEYWRD,' T=')
         IF(I.NE.0) THEN
            TIM=READA(KEYWRD,I)
            DO 10 J=I+3,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') TIM=TIM*60
               IF( CH .EQ. 'H') TIM=TIM*3600
               IF( CH .EQ. 'D') TIM=TIM*86400
               GOTO 20
            ENDIF
   10       CONTINUE
C
C   LIMIT JOB TIME TO MAX. OF ONE YEAR, LARGE JOBTIMES STOP
C   DUMPS WORKING CORRECTLY AS TLEFT-(CYCLE TIME) = TLEFT!
C
            TIM=MIN(31557600.D0,TIM)
   20       TLEFT=TIM
         ENDIF
         TLAST=TLEFT
         TDUMP=MAXDMP
         I=INDEX(KEYWRD,' DUMP=')
         IF(I.NE.0) THEN
            TDUMP=READA(KEYWRD,I)
            DO 30 J=I+6,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') TDUMP=TDUMP*60
               IF( CH .EQ. 'H') TDUMP=TDUMP*3600
               IF( CH .EQ. 'D') TDUMP=TDUMP*86400
                  GOTO 40
               ENDIF
   30       CONTINUE
   40       CONTINUE
         ENDIF
         TX2=SECOND()
         TLEFT=TLEFT-TX2+TIME0
         IF(INDEX(KEYWRD,'GNORM=').NE.0) THEN
            ROOTV=1.D0
            CONST=1.D-20
         ELSE
            ROOTV=SQRT(NVAR+1.D-5)
         ENDIF
         PRINT  = (INDEX(KEYWRD,'FLEPO').NE.0)
         TOLERX = 0.0001D0*CONST
         DELHOF  = 0.0010D0*CONST
         TOLERF = 0.002D0*CONST
         TOLRG  = TOLERG
         IF (INDEX(KEYWRD,'FORCE') .NE. 0) THEN
            TOLERX = 0.00001D0
            TOLERF = 0.0002D0
            TOLERG = 0.1D0
            DELHOF  = 0.00010D0
         ENDIF
         IF(INDEX(KEYWRD,'PREC') .NE. 0) THEN
            TOLERX=TOLERX*0.01D0
            DELHOF=DELHOF*0.01D0
            TOLERF=TOLERF*0.01D0
            TOLERG=TOLERG*0.1D0
            EINC=EINC*0.01
         ENDIF
         IF(INDEX(KEYWRD,'GNORM=').NE.0) THEN
            TOLERG=READA(KEYWRD,INDEX(KEYWRD,'GNORM='))
            IF(.NOT.GEOOK.AND.INDEX(KEYWRD,'LET').EQ.0.AND.
     1TOLERG.LT.1.D-4)THEN
               IF (DOPRNT) WRITE(6,'(/,A)')                             CIO
     & '  GNORM HAS BEEN SET TOO LOW, RESET TO 0.0001'                  CIO
               TOLERG=1.D-4
               TOLRG=TOLERG
            ENDIF
         ELSE
            TOLERG=TOLERG/ROOTV
         ENDIF
      ENDIF
C
C   THE FOLLOWING CONSTANTS SHOULD BE SET TO SOME ARBITARY LARGE VALUE.
C
      DROP  = 1.D15
      FREPF = 1.D15
C
C     AND FINALLY, THE FOLLOWING CONSTANTS ARE CALCULATED.
C
      IHDIM=(NVAR*(NVAR+1))/2
      CNCADD=1.0D00/ROOTV
      IF (CNCADD.GT.0.15D00) CNCADD=0.15D00
C
C     FIRST, WE INITIALIZE THE VARIABLES.
C
      ABSMIN=1.D6
      ITRY1=0
      ITRY2=0
      OKC=.TRUE.
      OKF=.TRUE.
      JCYC=0
      LNSTOP=1
      IREPET=1
      ALPHA = 1.0D00
      PNORM=1.0D00
      JNRST=0
      CYCMX=0.D0
      COS=0.0D00
      TOTIME=0.D0
      NCOUNT=1
      RESFIL=.FALSE.
      IF( SADDLE) THEN
*
*   WE DON'T NEED HIGH PRECISION DURING A SADDLE-POINT CALCULATION.
*
         IF(NVAR.GT.0)GNORM=SQRT(ddot(nvar,grad,1,grad,1))-3.D0
         IF(GNORM.GT.10.D0)GNORM =10.D0
         IF(GNORM.GT.1.D0) TOLERG=TOLRG*GNORM
         IF (DOPRNT)                                                     CIO
     &      WRITE(6,'('' GRADIENT CRITERION IN FLEPO ='',F12.5)')TOLERG  CIO
      ENDIF
      IF (RESTRT .AND. ICALCN .NE. NUMCAL) THEN
         MDFP(9)=0
         CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP,*9999)      CSTP(call)
         I=nint(TOTIME/1000000.0)
         TOTIME=TOTIME-I*1000000
         IF (DOPRNT) WRITE(6,'(//10X,''TOTAL TIME USED SO FAR:'',       CIO
     1    F13.2,'' SECONDS'')')TOTIME                                   CIO
         NSCF=MDFP(5)
         IF(INDEX(KEYWRD,'1SCF') .NE. 0) THEN
            CALL COMPFG (XPARAM, .TRUE.,FUNCT1,.TRUE.,GRAD,.TRUE.,*9999) CSTP(call)
            ICALCN=NUMCAL
            IFLEPO=1
            TIME0=TIME0-TOTIME
            TOTIME=0.D0
            RETURN
         ENDIF
      ELSE
         TOTIME=0.D0
C
C CALCULATE THE VALUE OF THE FUNCTION -> FUNCT1, AND GRADIENTS -> GRAD.
C NORMAL SET-UP OF FUNCT1 AND GRAD, DONE ONCE ONLY.
C
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.,*9999)   CSTP(call)
         DO 50 I=1,NVAR
   50    GD(I)=GRAD(I)
      ENDIF
      ICALCN=NUMCAL
      IF(NVAR.NE.0)GNORM=SQRT(ddot(nvar,grad,1,grad,1))
      IFLEPO=1
      IF(INDEX(KEYWRD,'1SCF') .NE. 0) THEN
         IF(DOPRNT) WRITE(6,'(1X,/,30H   BFGS GEOMETRY OPTIMIZATION,/)') CIO
      END IF
      IF(INDEX(KEYWRD,'1SCF') .NE. 0) RETURN
      IFLEPO=2
      IF(GNORM.LT.TOLERG.OR.NVAR.EQ.0) THEN
         LAST=1
         IF(RESTRT) THEN
            CALL COMPFG (XPARAM, .TRUE.,FUNCT1,.TRUE.,GRAD,.TRUE.,*9999) CSTP(call)
         ELSE
            CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.FALSE.,     CSTP(call)
     & *9999)                                                           CSTP(call)
         ENDIF
         RETURN
      ENDIF
      TX1 =  SECOND()
      TLEFT=TLEFT-TX1+TX2
C     *
C     START OF EACH ITERATION CYCLE ...
C     *
C
      RESET=.FALSE.
      GOTO 80
   60 CONTINUE
      IF(COS .LT. RST) THEN
         DO 70 I=1,NVAR
   70    GD(I)=0.5D0
      ENDIF
   80 CONTINUE
      JCYC=JCYC+1
      JNRST=JNRST+1
      IF (LNSTOP.NE.1 .AND. COS.GT.RST) GOTO 160
C
C     *
C     RESTART SECTION
C     *
C
   90 CONTINUE
      RESET=.TRUE.
      DO 100 I=1,NVAR
         XD(I)=XPARAM(I)-SIGN(DEL,GRAD(I))
  100 CONTINUE
C
C THIS CALL OF COMPFG IS USED TO CALCULATE THE SECOND-ORDER MATRIX IN H
C IF THE NEW POINT HAPPENS TO IMPROVE THE RESULT, THEN IT IS KEPT.
C OTHERWISE IT IS SCRAPPED, BUT STILL THE SECOND-ORDER MATRIX IS O.K.
C
      CALL COMPFG (XD, .TRUE., FUNCT2,.TRUE.,GD,.TRUE.,*9999)            CSTP(call)
      IF(.NOT. GEOOK .AND. SQRT(ddot(nvar,gd,1,gd,1))/GNORM.GT.10.
     1 AND.GNORM/ROOTV.GT.20.AND.JCYC.GT.2)THEN
C
C  THE GEOMETRY IS BADLY SPECIFIED IN THAT MINOR CHANGES IN INTERNAL
C  COORDINATES LEAD TO LARGE CHANGES IN CARTESIAN COORDINATES, AND THESE
C  LARGE CHANGES ARE BETWEEN PAIRS OF ATOMS THAT ARE CHEMICALLY BONDED
C  TOGETHER.
      IF (DOPRNT) THEN                                                  CIO
         WRITE(6,'('' GRADIENTS OF OLD GEOMETRY, GNORM='',F13.6)')GNORM
         WRITE(6,'(6F12.6)')(GRAD(I),I=1,NVAR)
      ENDIF                                                             CIO
         GDNORM=SQRT(ddot(nvar,gd,1,gd,1))
      IF (DOPRNT) THEN                                                  CIO
         WRITE(6,'('' GRADIENTS OF NEW GEOMETRY, GNORM='',F13.6)')GDNORM
         WRITE(6,'(6F12.6)')(GD(I),I=1,NVAR)
         WRITE(6,'(///20X,''CALCULATION ABANDONED AT THIS POINT!'')')
         WRITE(6,'(//10X,'' SMALL CHANGES IN INTERNAL COORDINATES ARE'',
     1/10X,'' CAUSING A LARGE CHANGE IN THE DISTANCE BETWEEN'')')
         WRITE(6,'(10X,'' CHEMICALLY-BOUND ATOMS. THE OPTIMIZATION'',/
     110X,'' PROCEDURE WOULD LIKELY PRODUCE INCORRECT RESULTS'')')
      ENDIF                                                             CIO
         CALL GEOUT(*9999)                                               CSTP(call)
      RETURN 1                                                           CSTP (stop)
      ENDIF
      NCOUNT=NCOUNT+1
      DO 110 I=1,IHDIM
  110 HESINV(I)=0.0D00
      DO 120 I=1,NVAR
         II=(I*(I+1))/2
         DELTAG=GRAD(I)-GD(I)
         IF (ABS(DELTAG).LT.1.D-10) DELTAG = 1.D-10
         GGD=ABS(GRAD(I))
         IF (FUNCT2.LT.FUNCT1) GGD=ABS(GD(I))
         HESINV(II)=SIGN(DEL,GRAD(I))/DELTAG
         IF (HESINV(II).LT.0.0D00.AND.GGD.LT.1.D-12)HESINV(II)=0.01D00
         IF (HESINV(II).LT.0.D0) HESINV(II)=6.D0*DEL/GGD
         HESINV(II)=MIN(HESINV(II),ABS(PMSTE/MAX(1.D-12,GGD)))
  120 CONTINUE
      JNRST=0
      IF(JCYC.LT.2) COSINE=1.D0
      IF(FUNCT2 .GE. FUNCT1) THEN
         IF(DOPRNT.AND.PRINT)WRITE (6,130) FUNCT1,FUNCT2                CIO
  130    FORMAT (' FUNCTION VALUE=',F13.7,
     1 '  WILL NOT BE REPLACED BY VALUE=',F13.7,/10X,
     2 'CALCULATED BY RESTART PROCEDURE',/)
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GD,.FALSE.,*9999)    CSTP(call)
         COSINE=1.D0
      ELSE
         IF( DOPRNT.AND.PRINT ) WRITE (6,140) FUNCT1,FUNCT2             CIO
  140    FORMAT (' FUNCTION VALUE=',F13.7,
     1' IS BEING REPLACED BY VALUE=',F13.7,/10X,
     2' FOUND IN RESTART PROCEDURE',/,6X,
     3'THE CORRESPONDING X VALUES AND GRADIENTS ARE ALSO BEING REPLACED'
     4,/)
         FUNCT1=FUNCT2
         GNORM=0.0D00
         DO 150 I=1,NVAR
            XPARAM(I)=XD(I)
            GRAD(I)=GD(I)
  150    GNORM=GNORM+GRAD(I)**2
         GNORM=SQRT(GNORM)
      ENDIF
      GO TO 210
C
C     *
C     UPDATE VARIABLE-METRIC MATRIX
C     *
C
  160 PTY=0.0D00
      JNRST=JNRST+1
      YHY=0.0D00
      DO 180 I=1,NVAR
         S=0.0D00
         DO 170 J=1,NVAR
            IF (J.GT.I) THEN
               K=(J*(J-1))/2+I
            ELSE
               K=(I*(I-1))/2+J
            ENDIF
  170    S=S+HESINV(K)*(GRAD(J)-GLAST(J))
         GG(I)=S
         Y=GRAD(I)-GLAST(I)
         YHY=YHY+GG(I)*Y
  180 PTY=PTY+(XPARAM(I)-XLAST(I))*Y
      IF(DFP)THEN
         DO 190 I=1,NVAR
            PT=XPARAM(I)-XLAST(I)
            DO 190 J=I,NVAR
               K=(J*(J-1))/2+I
               P=XPARAM(J)-XLAST(J)
C     START OF DAVIDON FLETCHER POWELL FORMULA
               HESINV(K)=HESINV(K)+PT*P/PTY-GG(I)*GG(J)/YHY
C     END OF DAVIDON FLETCHER POWELL FORMULA
  190    CONTINUE
      ELSE
C    BFGS FORMULA ON NEXT LINE
         YHY=1.D0+YHY/PTY
C    BFGS FORMULA ON LAST LINE
         DO 200 I=1,NVAR
            PT=XPARAM(I)-XLAST(I)
            DO 200 J=I,NVAR
               P=XPARAM(J)-XLAST(J)
               K=(J*(J-1))/2+I
C    START OF BFGS FORMULA ON NEXT LINE
               HESINV(K)=HESINV(K)-(GG(J)*PT+P*GG(I))/PTY+YHY*P*PT/PTY
C    END OF BFGS FORMULA
  200    CONTINUE
      ENDIF
C
C     *
C     ESTABLISH NEW SEARCH DIRECTION
C     *
  210 PNLAST=PNORM
      PNORM=0.0D00
      DOTT=0.0D00
      DO 230 K=1,NVAR
         S=0.0D00
         DO 220 I=1,NVAR
            IJ=MAX(I,K)
            S=S-HESINV((IJ*(IJ-1))/2+I+K-IJ)*GRAD(I)
  220    CONTINUE
         PVECT(K)=S
         PNORM=PNORM+PVECT(K)**2
  230 DOTT=DOTT+PVECT(K)*GRAD(K)
      PNORM=SQRT(PNORM)
      COS=-DOTT/(PNORM*GNORM)
      IF (JNRST.EQ.0) GO TO 260
      IF (COS.LE.CNCADD.AND.DROP.GT.1.0D00) GO TO 240
      IF (COS.LE.RST) GO TO 240
      GO TO 260
  240 PNORM=PNLAST
      IF( DOPRNT.AND.PRINT )WRITE (6,250) COS                           CIO
  250 FORMAT (//,5X, 'SINCE COS=',F9.3,5X,'THE PROGRAM WILL GO TO RE',
     1'START SECTION',/)
      GO TO 90
  260 CONTINUE
      IF( PRINT )THEN
         IF (DOPRNT) WRITE (6,270)                                      CIO
  270    FORMAT ('  THE CURRENT POINT IS ...')
         NTO6=(NVAR-1)/6+1
         IINC1=-5
         DO 320 I=1,NTO6
            IF (DOPRNT) WRITE (6,'(/)')                                 CIO
            IINC1=IINC1+6
            IINC2=MIN(IINC1+5,NVAR)
      IF (DOPRNT) THEN                                                  CIO
            WRITE (6,280) (J,J=IINC1,IINC2)
            WRITE (6,290) (XPARAM(J),J=IINC1,IINC2)
            WRITE (6,300) (GRAD(J),J=IINC1,IINC2)
            WRITE (6,310) (ALPHA*PVECT(J),J=IINC1,IINC2)
  280       FORMAT (1H ,3X,  1HI,9X,I3,9(8X,I3))
  290       FORMAT (1H ,1X, 'XPARAM(I)',1X,F9.4,2X,9(F9.4,2X))
  300       FORMAT (1H ,1X, 'GRAD  (I)',F10.4,1X,9(F10.4,1X))
  310       FORMAT (1H ,1X, 'PVECT (I)',2X,F10.6,1X,9(F10.6,1X))
      ENDIF                                                             CIO
  320    CONTINUE
      ENDIF
      LNSTOP=0
      ALPHA=ALPHA*PNLAST/PNORM
      DO 330 I=1,NVAR
         GLAST(I)=GRAD(I)
  330 XLAST(I)=XPARAM(I)
      IF (JNRST.EQ.0) ALPHA=1.0D00
      DROP=ABS(ALPHA*DOTT)
      IF (JNRST.NE.0.AND.DROP.LT.DELHOF) THEN
         IF(DOPRNT.AND.MINPRT)WRITE (6,340)                             CIO
  340    FORMAT(//,10X,'HERBERTS TEST SATISFIED - GEOMETRY OPTIMIZED')
C
C   FLEPO IS ENDING PROPERLY. THIS IS IMMEDIATELY BEFORE THE RETURN.
C
         LAST=1
         CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.,*9999)   CSTP(call)
         IFLEPO=3
         TIME0=TIME0-TOTIME
         TOTIME=0.D0
         RETURN
      ENDIF
      SMVAL=FUNCT1
      IF(GNORM.GT.1.D0.AND.PNORM.GT.1.D-3)THEN
         CALL LINMIN(XPARAM,ALPHA,PVECT,NVAR,FUNCT1,OKF,OKC,*9999)       CSTP(call)
      ELSE
C
C   DO A BINARY CHOP TO LOCATE THE MINIMUM
C
         ALPHA=1.D0
C
C   SOMETIMES PNORM IS TOO LARGE,  THEREFORE TRIM ALPHA BACK
C
         IF(PNORM.GT.0.1D0)ALPHA=0.1D0/PNORM
         LOOP=0
  350    CONTINUE
         DO 360 I=1,NVAR
  360    XPARAM(I)=XLAST(I)+PVECT(I)*ALPHA
         CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.FALSE.,*9999)  CSTP(call)
C
C   1.D-5 IS TO PREVENT LOOPING
C
         IF(FUNCT1.GT.SMVAL+1.D-5.AND.PNORM*ALPHA.GT.1.D-6.AND.LOOP.LT.1
     10)THEN
            LOOP=LOOP+1
            ALPHA=ALPHA*0.5D0
            GOTO 350
         ENDIF
      ENDIF
      NCOUNT=NCOUNT+1
      IF ( .NOT. OKF) THEN
         LNSTOP = 1
         IF(DOPRNT.AND.MINPRT)WRITE (6,370)                             CIO
  370    FORMAT (1H ,///,20X, 'NO POINT LOWER IN ENERGY ',
     1    'THAN THE STARTING POINT',/,20X,'COULD BE FOUND ',
     2    'IN THE LINE MINIMIZATION')
         FUNCT1=SMVAL
         DO 380 I=1,NVAR
            GRAD(I)=GLAST(I)
            XPARAM(I)=XLAST(I)
  380    CONTINUE
         IF (JNRST.EQ.0)THEN
            IF (DOPRNT) WRITE (6,390)                                   CIO
  390       FORMAT (1H ,//,20X, 'SINCE COS WAS JUST RESET,THE SEARCH',
     1        ' IS BEING ENDED')
C
C          FLEPO IS ENDING BADLY. THIS IS IMMEDIATELY BEFORE THE RETURN.
C
            LAST=1
            CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.TRUE.,*9999) CSTP(call)
            IFLEPO=4
            TIME0=TIME0-TOTIME
            TOTIME=0.D0
            RETURN
         ENDIF
         IF(DOPRNT.AND.PRINT)WRITE (6,400)                              CIO
  400    FORMAT (1H ,20X, 'COS WILL BE RESET AND ANOTHER '
     1    ,'ATTEMPT MADE')
         COS=0.0D00
         GO TO 560
      ENDIF
C   WE WANT ACCURATE DERIVATIVES AT THIS POINT
C
C   LINMIN DOES NOT GENERATE ANY DERIVATIVES, THEREFORE COMPFG MUST BE
C   CALLED TO END THE SEARCH
C
      IF(RESET) THEN
         DO 410 J=1,NVAR
  410    GRAD(J)=0.D0
         RESET=.FALSE.
      ENDIF
      CALL COMPFG (XPARAM, .TRUE., FUNCT1,.TRUE.,GRAD,.TRUE.,*9999)      CSTP(call)
      GNORM=SQRT(ddot(nvar,grad,1,grad,1))
      IF (DOPRNT.AND..NOT. OKC .AND. MINPRT)WRITE (6,420) JCYC          CIO
  420 FORMAT ( 23H0LINMIN FAILED AT CYCLE,I5/,  1H0)
      XN=0.0D00
      DO 430 K=1,NVAR
  430 XN=XN+XPARAM(K)**2
      XN=SQRT(XN)
      TX=ABS(ALPHA*PNORM)
      IF (XN.NE.0.0D00) TX=TX/XN
      TF=SMVAL-FUNCT1
      IF (ALPHA.LT.0.01D0.OR.ABSMIN-SMVAL.LT.1.D-7)THEN
         ITRY2=ITRY2+1
         IF(COS.LT.RST)ITRY2=0
         IF(ITRY2.EQ.5)THEN
C
C   RE-CALCULATE ALL DERIVATIVES IF HALF-ELECTRON USED.
C
            COS=-1.D0
         ENDIF
      ELSE
         IF(ALPHA.GT.0.1D0)ITRY2=0
      ENDIF
      IF(ABSMIN-SMVAL.LT.1.D-7)THEN
         ITRY1=ITRY1+1
         IF(ITRY1.GT.10)THEN
            IF (DOPRNT) WRITE(6,'(//,                                   CIO
     &        '' HEAT OF FORMATION IS ESSENTIALLY STATIONARY'')')       CIO
            GOTO 550
         ENDIF
      ELSE
         ITRY1=0
         ABSMIN=SMVAL
      ENDIF
      IF (DOPRNT.AND.PRINT)                                             CIO
     &        WRITE (6,440) NCOUNT,COS,TX*XN,ALPHA,-DROP,-TF,GNORM      CIO
  440 FORMAT (/,'           NUMBER OF COUNTS =',I6,
     1'         COS    =',F11.4,/,
     2        '  ABSOLUTE  CHANGE IN X     =',F13.6,
     3'  ALPHA  =',F11.4,/,
     4        '  PREDICTED CHANGE IN F     =  ',G11.4,
     5'  ACTUAL =  ',G11.4,/,
     6        '  GRADIENT NORM             =  ',G11.4,//)
  450 IF (TX.LE.TOLERX) THEN
         IF(DOPRNT.AND.MINPRT) WRITE (6,460)                            CIO
  460    FORMAT ( 20H0TEST ON X SATISFIED)
         GO TO 490
      ENDIF
      IF (ABS(TF).LE.TOLERF) THEN
         IF(DOPRNT.AND.MINPRT) WRITE (6,470)                            CIO
  470    FORMAT (' HEAT OF FORMATION TEST SATISFIED')
         GO TO 490
      ENDIF
      IF (GNORM.LE.TOLERG*ROOTV) THEN
         IF(DOPRNT.AND.MINPRT) WRITE (6,480)                            CIO
  480    FORMAT ( 27H0TEST ON GRADIENT SATISFIED)
         GOTO 490
      ENDIF
      GOTO 560
  490 DO 530 I=1,NVAR
         IF (ABS(GRAD(I)).GT.TOLERG)THEN
            IREPET=IREPET+1
            IF (IREPET.GT.1) GO TO 500
            FREPF=FUNCT1
            COS=0.0D00
  500       CONTINUE
            IF(DOPRNT.AND.MINPRT) WRITE (6,510)TOLERG                   CIO
  510       FORMAT (20X,'HOWEVER, A COMPONENT OF GRADIENT IS ',
     1     'LARGER THAN',F6.2 ,/)
            IF (ABS(FUNCT1-FREPF).GT.EINC) IREPET=0
            IF (IREPET.GT.IGG1) THEN
               IF (DOPRNT) WRITE (6,520)IGG1,EINC                       CIO
  520          FORMAT ( 7X,' THERE HAVE BEEN',I2,' ATTEMPTS TO REDUCE TH
     1E ',' GRADIENT.',/7X,' DURING THESE ATTEMPTS THE ENERGY DROPPED',
     2' BY LESS THAN',F7.4,' KCAL/MOLE',/
     310X,' FURTHER CALCULATION IS NOT JUSTIFIED AT THIS POINT.')
               LAST=1
               CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.,   CSTP(call)
     & *9999)                                                           CSTP(call)
               IFLEPO=8
               TIME0=TIME0-TOTIME
               TOTIME=0.D0
               RETURN
            ELSE
               GOTO 560
            ENDIF
         ENDIF
  530 CONTINUE
      IF(DOPRNT.AND.MINPRT) WRITE (6,540)                               CIO
  540 FORMAT ( 23H PETERS TEST SATISFIED )
  550 LAST=1
      CALL COMPFG (XPARAM, .TRUE., FUNCT,.TRUE.,GRAD,.FALSE.,*9999)      CSTP(call)
      IFLEPO=6
      TIME0=TIME0-TOTIME
      TOTIME=0.D0
      RETURN
C
C   ALL TESTS HAVE FAILED, WE NEED TO DO ANOTHER CYCLE.
C
  560 CONTINUE
      BSMVF=ABS(SMVAL-FUNCT1)
      IF (BSMVF.GT.10.D00) COS = 0.0D00
      DEL=0.002D00
      IF (BSMVF.GT.1.0D00) DEL=DELL/2.0D00
      IF (BSMVF.GT.5.0D00) DEL=DELL
      TX2 = SECOND()
      TCYCLE=TX2-TX1
      TX1=TX2
C
C END OF ITERATION LOOP, EVERYTHING IS STILL O.K. SO GO TO
C NEXT ITERATION, IF THERE IS ENOUGH TIME LEFT.
C
      IF(TCYCLE.LT.100000.D0)CYCMX=MAX(CYCMX*0.8D0,TCYCLE)
      TLEFT=TLEFT-TCYCLE
      IF(TLEFT.LT.0)TLEFT=-0.1D0
      IF(TCYCLE.GT.1.D5)TCYCLE=0.D0
      IF(TLAST-TLEFT.GT.TDUMP)THEN
         TOTIM=TOTIME   +   SECOND()-TIME0
         TLAST=TLEFT
         MDFP(9)=2
         RESFIL=.TRUE.
         MDFP(5)=NSCF
         CALL DFPSAV(TOTIM,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP,*9999)       CSTP(call)
      ENDIF
      IF(RESFIL)THEN
         IF(DOPRNT.AND.MINPRT) WRITE(6,570)MIN(TLEFT,9999999.9D0),      CIO
     1MIN(GNORM,999999.999D0),FUNCT1
  570    FORMAT(' RESTART FILE WRITTEN,   TIME LEFT:',F9.1,
     1' GRAD.:',F10.3,' HEAT:',G13.7)
         RESFIL=.FALSE.
      ELSE
         IF(DOPRNT.AND.MINPRT) WRITE(6,580)JCYC,MIN(TCYCLE,9999.99D0),  CIO
     1MIN(TLEFT,9999999.9D0),MIN(GNORM,999999.999D0),FUNCT1             CIO
  580     FORMAT('CYCLE:',I4,'  TIME:',F7.1,'  TIME LEFT:',F8.1,
     1'  GRAD.:',F7.3,'  HEAT:',G13.7)
      ENDIF
      IF (TLEFT.GT.SFACT*CYCMX) GO TO 60
      IF (DOPRNT) WRITE(6,590)                                          CIO
  590 FORMAT (20X, 42HTHERE IS NOT ENOUGH TIME FOR ANOTHER CYCLE,/,30X,
     118HNOW GOING TO FINAL)
      TOTIME=TOTIME   +   SECOND()-TIME0
      MDFP(9)=1
      MDFP(5)=NSCF
      CALL DFPSAV(TOTIME,XPARAM,GD,XLAST,FUNCT1,MDFP,XDFP,*9999)         CSTP(call)
      RETURN 1                                                           CSTP (stop)
C
C
 9999 RETURN 1                                                          CSTP
      ENTRY FLEPO_INIT                                                  CSAV
C     Many variables already reinitialized every time starting at line 240. CSAV
            ABSMIN = 0.0D0                                              CSAV
             BSMVF = 0.0D0                                              CSAV
                CH = ''                                                 CSAV
             CHDOT = '.'                                                CSAV
            CNCADD = 0.0D0                                              CSAV
             CONST = 0.0D0                                              CSAV
                 D = 0.0D0                                              CSAV
                D0 = 0.0D0                                              CSAV
               D00 = 0.0D0                                              CSAV
               D15 = 0.0D0                                              CSAV
                D5 = 0.0D0                                              CSAV
                D6 = 0.0D0                                              CSAV
               DEL = 0.0D0                                              CSAV
            DELHOF = 0.0D0                                              CSAV
              DELL = 0.0D0                                              CSAV
            DELTAG = 0.0D0                                              CSAV
               DFP = .FALSE.                                            CSAV
              DOTT = 0.0D0                                              CSAV
              EINC = 0.0D0                                              CSAV
             FUNCT = 0.0D0                                              CSAV
            FUNCT2 = 0.0D0                                              CSAV
                GD = 0.0D0                                              CSAV
            GDNORM = 0.0D0                                              CSAV
             GEOOK = .FALSE.                                            CSAV
                GG = 0.0D0                                              CSAV
               GGD = 0.0D0                                              CSAV
             GLAST = 0.0D0                                              CSAV
                 I = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
              IGG1 = 0                                                  CSAV
             IHDIM = 0                                                  CSAV
                II = 0                                                  CSAV
             IINC1 = 0                                                  CSAV
             IINC2 = 0                                                  CSAV
                IJ = 0                                                  CSAV
            IREPET = 0                                                  CSAV
            ISTIME = .FALSE.                                            CSAV
             ITRY1 = 0                                                  CSAV
             ITRY2 = 0                                                  CSAV
                 J = 0                                                  CSAV
                 K = 0                                                  CSAV
              LOOP = 0                                                  CSAV
              MDFP = 0                                                  CSAV
            MINPRT = .FALSE.                                            CSAV
              NINE = '9'                                                CSAV
              NTO6 = 0                                                  CSAV
               OKC = .FALSE.                                            CSAV
               OKF = .FALSE.                                            CSAV
                 P = 0.0D0                                              CSAV
             PMSTE = 0.0D0                                              CSAV
            PNLAST = 0.0D0                                              CSAV
             PRINT = .FALSE.                                            CSAV
                PT = 0.0D0                                              CSAV
               PTY = 0.0D0                                              CSAV
             PVECT = 0.0D0                                              CSAV
             RESET = .FALSE.                                            CSAV
            RESFIL = .FALSE.                                            CSAV
            RESTRT = .FALSE.                                            CSAV
             ROOTV = 0.0D0                                              CSAV
               RST = 0.0D0                                              CSAV
                 S = 0.0D0                                              CSAV
            SADDLE = .FALSE.                                            CSAV
             SFACT = 0.0D0                                              CSAV
             SMVAL = 0.0D0                                              CSAV
             SPACE = ' '                                                CSAV
            TCYCLE = 0.0D0                                              CSAV
              TDEL = 0.0D0                                              CSAV
             TDUMP = 0.0D0                                              CSAV
                TF = 0.0D0                                              CSAV
               TIM = 0.0D0                                              CSAV
             TLAST = 0.0D0                                              CSAV
             TLEFT = 0.0D0                                              CSAV
            TOLERF = 0.0D0                                              CSAV
            TOLERG = 0.0D0                                              CSAV
            TOLERX = 0.0D0                                              CSAV
             TOLRG = 0.0D0                                              CSAV
             TOTIM = 0.0D0                                              CSAV
                TX = 0.0D0                                              CSAV
               TX1 = 0.0D0                                              CSAV
               TX2 = 0.0D0                                              CSAV
                XD = 0.0D0                                              CSAV
              XDFP = 0.0D0                                              CSAV
             XLAST = 0.0D0                                              CSAV
                XN = 0.0D0                                              CSAV
                 Y = 0.0D0                                              CSAV
               YHY = 0.0D0                                              CSAV
              ZERO = '0'                                                CSAV
      RETURN                                                            CSAV
      END
