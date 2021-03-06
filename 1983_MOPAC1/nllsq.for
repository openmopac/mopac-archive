
     SUBROUTINE NLLSQ(X,N)
     IMPLICIT DOUBLE PRECISION (A-H,O-Z)
     INCLUDE 'SIZES'
     COMMON /KEYWRD/ KEYWRD
     CHARACTER*80 KEYWRD
     DIMENSION X(*)
     COMMON /MESAGE/ IFLEPO,IITER
****************************************************************************

  NLLSQ IS A NON-DERIVATIVE, NONLINEAR LEAST-SQUARES MINIMIZER. IT USES
        BARTEL'S PROCEDURE TO MINIMISE A FUNCTION WHICH IS A SUM OF
        SQUARES.

    ON INPUT N    = NUMBER OF UNKNOWNS
             X    = PARAMETERS OF FUNCTION TO BE MINIMIZED.

    ON EXIT  X    = OPTIMISED PARAMETERS.

    THE FUNCTION TO BE MINIMIZED IS "COMPFG". COMPFG MUST HAVE THE CALLING
    SEQUENCE     "CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,EFS,.TRUE.)"
                  SSQ=DOT(EFS,EFS,N)
    WHERE   EFS  IS A VECTOR WHICH  COMPFG  FILLS WITH THE N INDIVIDUAL
                 COMPONENTS OF THE ERROR FUNCTION AT THE POINT X
            SSQ IS THE VALUE OF THE SUM OF THE  EFS  SQUARED.
    IN THIS FORMULATION OF NLLSQ M AND N ARE THE SAME. 
    THE PRECISE DEFINITIONS OF THESE TWO QUANTITIES IS:

     N = NUMBER OF PARAMETERS TO BE OPTIMIZED.
     M = NUMBER OF REFERENCE FUNCTIONS. M MUST BE GREATER THEN, OR EQUAL TO, N
****************************************************************************
     Q = ORTHOGONAL MATRIX   (M BY M)
     R = RIGHT-TRIANGULAR MATRIX   (M BY N)
     MXCNT(1) = MAX ALLOW OVERALL FUN EVALS
     MXCNT(2) = MAX ALLOW NO OF FNC EVALS PER LIN SEARCH
     TOLS1 = RELATIVE TOLERANCE ON X OVERALL
     TOLS2 = ABSOLUTE TOLERANCE ON X OVERALL
     TOLS3 = RELATIVE TOLERANCE ON SSQ OVERALL
     TOLS4 = ABSOLUTE TOLERANCE ON SSQ OVERALL
     TOLS5 = RELATIVE TOLERANCE ON X FOR LINEAR SEARCHES
     TOLS6 = ABSOLUTE TOLERANCE ON X FOR LINEAR SEARCHES
     IPRINT = PRINT SWITCH
     NRST = NUMBER OF CYCLES BETWEEN SIDESTEPS
     **********
     COMMON /TIME  / TIME0
     COMMON /NLLSQI/ NCOUNT
     DIMENSION EFSLST(MAXPAR), XLAST(MAXPAR), Q(MAXPAR,MAXPAR),
    + R(MAXPAR,MAXPAR),
    + Y(MAXPAR), EFS(MAXPAR), P(MAXPAR)
     COMMON /NLLCOM/IIIUM,DDDUM,EFSLST,Q,R,XLAST
     DIMENSION IIIUM(7),DDDUM(6)
     LOGICAL MIDDLE, PNLLSQ, CHOP, DEBUG
     EQUIVALENCE( IIIUM(1), JCYC),( IIIUM(2), ICYC),(IIIUM(3), IRST),
    +(IIIUM(4),JRST),
    1(DDDUM(2),ALF), (DDDUM(3),SSQ),(DDDUM(4), PN)
     MIDDLE=(INDEX(KEYWRD,'RESTART') .NE. 0)
     IFLEPO=10
*
     M=N
*
     MXCYCL=100
     I=INDEX(KEYWRD,'CYCLES=')
     IF (I.NE.0) THEN
         MXCYCL=READA(KEYWRD,I)
         WRITE(6,'(/10X,''NUMBER OF CYCLES TO BE RUN ='',I4)')MXCYCL
     ENDIF
     TOLS1=1.D-12
     TOLS2=1.D-10
     TOLS3=1.D-12
     TOLS4=1.D-13
     TOLS5=1.D-6
     TOLS6=1.D-3
     IPRINT=-1
     NRST=4
     YMAXST=1.D0
     TLEFT=28200
     I=INDEX(KEYWRD,' T=')
     IF(I .NE. 0)TLEFT=READA(KEYWRD,I)
     TLEFT=TLEFT-SECOND()+TIME0
     **********
     SET UP COUNTERS AND SWITCHES
     **********
     NTO=N/6
     IFRTL=0
     NREM=N-(NTO*6)
     IREPET=0
     NSST=0
     IF(IXSO.EQ.0) IXSO=N
     NP1 = N+1
     NP2 = N+2
     ICYC = 0
     IRST = 0
     JRST = 1
     EPS =TOLS5
     T = TOLS6
     **********
     GET STARTING-POINT FUNCTION VALUE
     SET UP ESTIMATE OF INITIAL LINE STEP
     **********
     IF(MIDDLE) THEN
         CALL PARSAV(0,N,M)
         CLOSE(13)
         NCOUNT=IIIUM(5)
         MXCYCL=MXCYCL+ICYC
         DO 976 I=1,N
 976     X(I)=XLAST(I)
         TIME1=SECOND()
         GOTO 3
     ENDIF
     CALL COMPFG(X,.TRUE.,ESCF,.TRUE.,EFSLST,.TRUE.)
     SSQ=DOT(EFSLST,EFSLST,N)
     NCOUNT = 1
 980 CONTINUE
     DO 20 I=1,M
     DO 19 J=1,N
     R(I,J) = 0.0D0
     IF (I .EQ. J)  R(I,J)=1.0D0
  19 CONTINUE
     DO 20 J=I,M
     Q(I,J) = 0.0D0
     Q(J,I) = 0.0D0
     IF (I .EQ. J)  Q(I,I)=1.0D0
  20 CONTINUE
     TEMP = 0.0D0
     DO 1 I=1,N
   1 TEMP = TEMP+X(I)**2
     ALF = 100.0D0*(EPS*SQRT(TEMP)+T)
     **********
     MAIN LOOP
     **********
     TIME1=SECOND()
   3 CONTINUE
     **********
     UPDATE COUNTERS AND TEST FOR PRINTING THIS CYCLE
     **********
     IFRTL=IFRTL+1
     ICYC = ICYC+1
     IRST = IRST+1
     **********
     SET  PRT,  THE LEVENBERG-MARQUARDT PARAMETER.
     **********
     PRT = SQRT(SSQ)
     **********
     IF A SIDESTEP IS TO BE TAKEN, GO TO 31
     **********
     IF (IRST .GE. NRST)  GO TO 31
     **********
     SOLVE THE SYSTEM    Q*R*P = -EFSLST    IN THE LEAST-SQUARES SENSE
     **********
     NSST=0
     DO 5 I=1,M
     TEMP = 0.0D0
     DO 4 J=1,M
   4 TEMP = TEMP-Q(J,I)*EFSLST(J)
   5 EFS(I) = TEMP
     DO 3000 J=1,N
     JJ = NP1-J
     DO 3000 I=1,J
     II = NP2-I
3000 R(II,JJ) = R(I,J)
     DO 3001 I=1,N
     I1 = I+1
     Y(I) = PRT
     EFSSS=0.0D0
     IF (I .GE. N)  GO TO 3003
     DO 3002 J=I1,N
3002 Y(J) = 0.0D0
3003 CONTINUE
     DO 3004 J=I,N
     II = NP2-J
     JJ = NP1-J
     IF (ABS(Y(J)) .LT. ABS(R(II,JJ)))  GO TO 3005
     TEMP = Y(J)*SQRT(1.0D0+(R(II,JJ)/Y(J))**2)
     GO TO 3006
3005 TEMP = R(II,JJ)*SQRT(1.0D0+(Y(J)/R(II,JJ))**2)
3006 CONTINUE
     SIN = R(II,JJ)/TEMP
     COS = Y(J)/TEMP
     R(II,JJ) = TEMP
     TEMP = EFS(J)
     EFS(J)=SIN*TEMP+COS*EFSSS
     EFSSS=SIN*EFSSS-COS*TEMP
     IF (J .GE. N)  GO TO 3001
     J1 = J+1
     DO 3007 K=J1,N
     JJ = NP1-K
     TEMP = R(II,JJ)
     R(II,JJ) = SIN*TEMP+COS*Y(K)
3007 Y(K) = SIN*Y(K)-COS*TEMP
3004 CONTINUE
3001 CONTINUE
     P(N) = EFS(N)/R(2,1)
     I = N
   6 I = I-1
     IF (I)  62,62,60
  60 TEMP = EFS(I)
     K = I+1
     II = NP2-I
     DO 61 J=K,N
     JJ = NP1-J
  61 TEMP = TEMP-R(II,JJ)*P(J)
     JJ = NP1-I
     P(I) = TEMP/R(II,JJ)
     GO TO 6
  62 CONTINUE
     GO TO 7
     **********
     SIDESTEP SECTION
     **********
  31 JRST = JRST+1
     NSST=NSST+1
     IF(NSST.GE.IXSO) GO TO 390
     IF (JRST .GT. N)  JRST=2
     IRST = 0
     **********
     PRODUCTION OF A VECTOR ORTHOGONAL TO THE LAST P-VECTOR
     **********
     WORK = PN*(ABS(P(1))+PN)
     TEMP = P(JRST)
     P(1) = TEMP*(P(1)+SIGN(PN,P(1)))
     DO 32 I=2,N
  32 P(I) = TEMP*P(I)
     P(JRST) = P(JRST)-WORK
     **********
     COMPUTE NORM AND NORM-SQUARE OF THE P-VECTOR
     **********
   7 PNLAST = PN
     PN=0.D0
     PN2 = 0.0D0
     DO 70 I=1,N
     PN=PN+ABS(P(I))
  70 PN2 = PN2+P(I)**2
     IF(PN.LT.1.D-20) THEN
     WRITE(6,'('' SYSTEM DOES NOT APPEAR TO BE OPTIMIZABLE.'',/
    +,'' THIS CAN HAPPEN IF (A) IT WAS OPTIMIZED TO BEGIN WITH'',/
    +,'' OR                 (B) IT IS NEITHER A GROUND NOR A'',
    +'' TRANSITION STATE'')')
     CALL GEOUT
     STOP
     ENDIF
     IF(PN2.LT.1.D-20)PN2=1.D-20
     PN = SQRT(PN2)
     IF(ALF.GT.1.D20)ALF=1.D20
     IF(ICYC .GT. 1) THEN
         ALF=ALF*1.D-20*PNLAST/PN
         IF(ALF.GT.1.D10)        ALF=1.D10
         ALF=ALF*1.D20
     ENDIF
     TTMP=ALF*PN
     IF(TTMP.LT.0.0001D0) ALF=0.001D0/PN
     **********
     PRINTING SECTION
     **********
     WRITE(6,501)ICYC,SSQ
     DO 777 I=1,N
     EFS(I)=X(I)
777  CONTINUE
     **********
     PERFORM LINE-MINIMIZATION FROM POINT X IN DIRECTION P OR -P
     **********
     SMVAL=ENERGY
     SSQLST = SSQ
     DO 1897 I=1,N
     EFS(I)=0.D0
1897 XLAST(I)=X(I)
     CALL LOCMIN(M,X,N,P,SSQ,ALF,EFS,IERR)
     IF(SSQLST .LT. SSQ ) THEN
         IF(IERR .EQ. 0)      SSQ=SSQLST
         DO 1898 I=1,N
1898         X(I)=XLAST(I)
         IRST=NRST
         PN=PNLAST
         GOTO 3
     ENDIF
     IREPET=0
     **********
     PRODUCE THE VECTOR   R*P
     **********
     DO 14 I=1,N
     TEMP = 0.0D0
     DO 13 J=I,N
  13 TEMP = TEMP+R(I,J)*P(J)
  14 Y(I) = TEMP
     **********
     PRODUCE THE VECTOR ...
                  Y  =    (EFS-EFSLST-ALF*Q*R*P)/(ALF*(NORMSQUARE(P))
     COMPUTE NORM OF THIS VECTOR AS WELL
     **********
     WORK = ALF*PN2
     YN = 0.0D0
     DO 141 I=1,M
     TEMP = 0.0D0
     DO 140 J=1,N
 140 TEMP = TEMP+Q(I,J)*Y(J)
     TEMP = (EFS(I)-EFSLST(I)-ALF*TEMP)
     EFSLST(I) = EFS(I)
     YN = YN+TEMP**2
 141 EFS(I) = TEMP/WORK
     YN = SQRT(YN)/WORK
     **********
     THE BROYDEN UPDATE   NEW MATRIX = OLD MATRIX + Y*(P-TRANS)
     HAS BEEN FORMED.  IT IS NOW NECESSARY TO UPDATE THE  QR DECOMP.
     FIRST LET    Y = (Q-TRANS)*Y.
     **********
     DO 143 I=1,M
     TEMP = 0.0D0
     DO 142 J=1,M
 142 TEMP = TEMP+Q(J,I)*EFS(J)
 143 Y(I) = TEMP
     **********
     REDUCE THE VECTOR Y TO A MULTIPLE OF THE FIRST UNIT VECTOR USING
     A HOUSEHOLDER TRANSFORMATION FOR COMPONENTS N+1 THROUGH M AND
     ELEMENTARY ROTATIONS FOR THE FIRST N+1 COMPONENTS.  APPLY ALL
     TRANSFORMATIONS TRANSPOSED ON THE RIGHT TO THE MATRIX Q, AND
     APPLY THE ROTATIONS ON THE LEFT TO THE MATRIX R.
     THIS GIVES    (Q*(V-TRANS))*((V*R) + (V*Y)*(P-TRANS)),    WHERE
     V IS THE COMPOSITE OF THE TRANSFORMATIONS.  THE MATRIX
     ((V*R) + (V*Y)*(P-TRANS))    IS UPPER HESSENBERG.
     **********
     IF (M .LE. NP1)  GO TO 1434

 THE NEXT THREE LINES WERE INSERTED TO TRY TO GET ROUND OVERFLOW BUGS.

     CONST=1.D-12
     DO 1429 I=NP1,M
1429 CONST=MAX(ABS(Y(NP1)),CONST)
     YTAIL = 0.0D0
     DO 1430 I=NP1,M
1430 YTAIL = YTAIL+(Y(I)/CONST)**2
     YTAIL = SQRT(YTAIL)*CONST
     BET = (1.0D25/YTAIL)/(YTAIL+ABS(Y(NP1)))
     Y(NP1) = SIGN (YTAIL+ABS(Y(NP1)),Y(NP1))
     DO 1433 I=1,M
     TMP = 0.0D0
     DO 1431 J=NP1,M
1431 TMP = TMP+Q(I,J)*Y(J)*1.D-25
     TMP = BET*TMP
     DO 1432 J=NP1,M
1432 Q(I,J) = Q(I,J)-TMP*Y(J)
1433 CONTINUE
     Y(NP1) = YTAIL
     I = NP1
     GO TO 1435
1434 CONTINUE
     I = M
1435 CONTINUE
 144 J = I
     I = I-1
     IF (I)  151,151,145
 145 IF (Y(J))  146,144,146
 146 IF (ABS(Y(I)) .LT. ABS(Y(J)))  GO TO 147
     TEMP = ABS(Y(I))*SQRT(1.0D0+(Y(J)/Y(I))**2)
     GO TO 148
 147 TEMP = ABS(Y(J))*SQRT(1.0D0+(Y(I)/Y(J))**2)
 148 COS = Y(I)/TEMP
     SIN = Y(J)/TEMP
     Y(I) = TEMP
     DO 149 K=1,M
     TEMP = COS*Q(K,I)+SIN*Q(K,J)
     WORK = -SIN*Q(K,I)+COS*Q(K,J)
     Q(K,I) = TEMP
 149 Q(K,J) = WORK
     IF (I .GT. N)  GO TO 144
     R(J,I) = -SIN*R(I,I)
     R(I,I) = COS*R(I,I)
     IF (J .GT. N)  GO TO 144
     DO 150 K=J,N
     TEMP = COS*R(I,K)+SIN*R(J,K)
     WORK = -SIN*R(I,K)+COS*R(J,K)
     R(I,K) = TEMP
 150 R(J,K) = WORK
     GO TO 144
 151 CONTINUE
     **********
     REDUCE THE UPPER-HESSENBERG MATRIX TO UPPER-TRIANGULAR FORM
     USING ELEMENTARY ROTATIONS.  APPLY THE SAME ROTATIONS, TRANSPOSED,
     ON THE RIGHT TO THE MATRIX  Q.
     **********
     DO 152 K=1,N
 152 R(1,K) = R(1,K)+YN*P(K)
     JEND = NP1
     IF (M .EQ. N)  JEND=N
     DO 159 J=2,JEND
     I = J-1
     IF (R(J,I))  153,159,153
 153 IF (ABS(R(I,I)) .LT. ABS(R(J,I)))  GO TO 154
     TEMP = ABS(R(I,I))*SQRT(1.0D0+(R(J,I)/R(I,I))**2)
     GO TO 155
 154 TEMP = ABS(R(J,I))*SQRT(1.0D0+(R(I,I)/R(J,I))**2)
 155 COS = R(I,I)/TEMP
     SIN = R(J,I)/TEMP
     R(I,I) = TEMP
     IF (J .GT. N)  GO TO 157
     DO 156 K=J,N
     TEMP = COS*R(I,K)+SIN*R(J,K)
     WORK = -SIN*R(I,K)+COS*R(J,K)
     R(I,K) = TEMP
 156 R(J,K) = WORK
 157 DO 158 K=1,M
     TEMP = COS*Q(K,I)+SIN*Q(K,J)
     WORK = -SIN*Q(K,I)+COS*Q(K,J)
     Q(K,I) = TEMP
 158 Q(K,J) = WORK
 159 CONTINUE
     **********
     CHECK THE STOPPING CRITERIA
     **********
     TEMP = 0.0D0
     DO 17 I=1,N
  17 TEMP = TEMP+X(I)**2
     TOLX = TOLS1*SQRT(TEMP)+TOLS2
     TOLF = TOLS3*SSQ+TOLS4
     IF (SQRT(ALF*PN2) .LE. TOLX)  GO TO 300
     IF(SSQ.GE.DFLOAT(N)) GO TO 790
     DO 791 I=1,N
*****
     The stopping criterion is that no individual gradient be 
         greater than 0.1
*****
     IF(ABS(EFSLST(I)).GE.0.1D0) GO TO 790
 791 CONTINUE
     WRITE(6,840) SSQ
     GO TO 301
 790 CONTINUE
     IF (ICYC .GE. MXCYCL)  THEN
         IFLEPO=12
         RETURN
     ENDIF
     TIME2=TIME1
     TIME1=SECOND()
     TCYCLE=TIME1-TIME2
     TLEFT=TLEFT-TCYCLE
     IF(TLEFT .GT. TCYCLE*2) GO TO 3
 302     IIIUM(5)=NCOUNT
         DO 68 I=1,N
 68          XLAST(I)=X(I)
         CALL PARSAV(1,N,M)
         STOP
 300 WRITE (6,506)  NCOUNT
     NSUCC=0
     RETURN
 301 WRITE (6,507)  NCOUNT
     NSUCC=0
     RETURN
 958 CONTINUE
     NSUCC=1
     RETURN
 390 CONTINUE
     NSUCC=9
     WRITE(6,391) IXSO
 391 FORMAT(1H ,5X,'ATTEMPT TO GO DOWNHILL IS UNSUCCESSFUL AFTER',I5,5X
    1,'ORTHOGONAL SEARCHES')
     RETURN
 840 FORMAT(1H ,'SSQ =',F15.7)
9000 FORMAT(1H ,3X,'ALF =',E12.4)
9001 FORMAT(1H ,3X,'NCOUNT =',I5)
 501 FORMAT(3X,'AT BEGINNING OF CYCLE',I5,3X,'GNORM SQUARED IS',F13.5)
 502 FORMAT(4(5X,'X(',I2,') = ',E15.8))
 503 FORMAT(4(5X,'P(',I2,') = ',E15.8))
 504 FORMAT(5X,'R-MATRIX DIAGONAL ENTRIES ...')
 505 FORMAT(6E13.3)
 506 FORMAT('0TEST ON X SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
 507 FORMAT('0TEST ON SSQ SATISFIED, NUMBER OF FUNCTION CALLS = ',I5)
 508 FORMAT(' ///// NEXT CYCLE IS A SIDE-STEP ALONG THE ',I2,
    .  '-TH NORMAL TO P')
 509 FORMAT('0ALLOWED NUMBER OF FUNCTION CALLS EXCEEDED.'/
    .  ' NUMBER OF FUNCTION CALLS WAS ',I5)
 510 FORMAT('  L.-M. PARAMETER = ',E15.7,
    .  '   SUMSQUARES CHANGE = ',E15.7)
 875 FORMAT(1H )
 851 FORMAT(1H )
 852 FORMAT(1H ,3X,'I',7X,I2,9(10X,I2))
 853 FORMAT(1H ,1X,'X(I)',1X,F10.5,2X,9(F10.5,2X))
 854 FORMAT(1H ,1X,'G(I)',1X,F10.5,2X,9(F10.5,2X))
 855 FORMAT(1H ,1X,'P(I)',1X,F10.5,2X,9(F10.5,2X))
     END