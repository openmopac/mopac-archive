      SUBROUTINE PATH1(X,E,G,N,EPS,LIMIT,IMETH,P) 
      IMPLICIT DOUBLE PRECISION(A-H,O-Z) 
      DOUBLE PRECISION CVMGT                                            GL0592
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C----------------------------------------------------------------------- 
C     REACTION PATH DESCRIPTION BY NUMERICAL INTEGRATION 
C       EPS   : CONVERGENCE THRESHOLD ON RMS GRADIENT G AT POINT X 
C       LIMIT : MAXIMUM NUMBER OF ITERATION 
C       IMETH : 8,9 ... FOR CURRENT EVALUATION OF G AT X 
C               20      NORMAL RETURN 
C               21      DIVERGENCE (INCREASING ENERGY) 
C     DATA ... 
C       POND(I)  PONDERATION FACTOR (EG MASSES OF NUCLEI) 
C       YE(I)    TRANSITION VECTOR  (IF PROVIDED ) 
C       HMIN     MINIMUM ALLOWED STEP LENGTH 
C       HMAX     MAXIMUM ------------------- 
C       HPREC    CONVERGENCE THRESHOLD ON VARIABLES AT EACH ITERATION 
C     REFERENCE FOR REACTION PATH DEFINITION  : 
C       KATO,FUKUI JACS VOL 98  P 6395  (1976) 
C     REFERENCE FOR HESSIAN UPDATE : 
C       M.J.D. POWELL MATH. PROG. VOL 1 P 26 (1971) 
C     REFERENCE AS A STARTING POINT FOR THIS INTEGRATOR : 
C       A. RALSTON FOR PREDICTOR CORRECTOR 2/4 
C       J. CERTAINE FOR LARGE TIME CONSTANT TREATMENT 
C                  BOTH IN 
C       MATHEMATICAL METHODS FOR DIGITAL COMPUTER 
C       (A. RALSTON ED) J.WILEY & SONS,NEW-YORK (1960) 
C     ELABORATED BY D.LIOTARD 
C     LABORATOIRE DE CHIMIE STRUCTURALE -PAU, FRANCE- DECEMBER 1984 
C 
      COMMON /PRECI/ SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR) 
      COMMON /OPTIMI / IMP,IMP0                                         3GL3092
      COMMON /OPTMCI / ICALL,ITERAT,IS,IPRIN,ITE,NBOUND,NBOULO,         DJG0995
     1                 IDUMMY(19 + 2*NCHAIN - 7)                        3GL3092
      COMMON /OPTMCL / KONV,HUPDAT,RESET,INTERP, LDUMMY(NCHAIN)         GCL0892
      COMMON /OPTMCR / HESSE(MAXHES),PDUMMY(MAXPAR,MAXPAR),             3GL3092
     1                 DY2(MAXPAR),POND(MAXPAR),D(MAXPAR),              3GL3092
     2                 XOLD(MAXPAR),TVOLD(MAXPAR),Y1(MAXPAR,2),         3GL3092
     3                 Y2(MAXPAR,2),Y3(MAXPAR,2),C(MAXPAR,4),           3GL3092
     4                 Y4(MAXPAR,2),Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR),  3GL3092
     5                 HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE,     3GL3092
     6                 Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE,                  3GL3092
     7                 RDUMY(MAXPAR * (2*MAXPAR + NCHAIN + 16 - 21) +   3GL3092
     8                       29 + NCHAIN - 14)                          3GL3092
C     COMMON /OPTIM/ IMP,IMP0,LEC,IPRT,HESSE(MAXHES) 
C    .              ,PDUMMY(MAXPAR,MAXPAR),DY2(MAXPAR),POND(MAXPAR) 
C    .              ,D(MAXPAR),XOLD(MAXPAR),TVOLD(MAXPAR) 
C    .              ,Y1(MAXPAR,2),Y2(MAXPAR,2),Y3(MAXPAR,2),C(MAXPAR,4) 
C    .              ,Y4(MAXPAR,2),Y(MAXPAR,2),YE(MAXPAR),DX(MAXPAR), 
C    .               HPREC,HMIN,H,HMAX,HTOT,DHTOT,RMS,EOLD,CRITE, 
C    .               Y1NORM,Y2NORM,Y3NORM,ETOL,TOLE, 
C    .               ICALL,ITERAT,IS,IPRINT,ITE,NBOUND,NBOULO, 
C    .               KONV,HUPDAT,RESET,INTERP 
      DIMENSION X(N),G(N),P(N,N) 
      LOGICAL KONV,HUPDAT,RESET,INTERP 
      LOGICAL LDUMMY                                                    3GL3092
      SAVE
C     ... 
C     DISPATCHING ACCORDING TO THE VALUE OF IMETH 
C       8   STARTING POINT 
C       9   GET AWAY FROM THE SADDLE 
C      10   ONE OF THE INTEGRATOR 
C    ... 
      IMETHO=IMETH-7 
      GO TO (9001,9002,9003),IMETHO 
      WRITE(JOUT,1220) IMETH 
      IMETH=21 
      RETURN 
 9001 CONTINUE 
C     ... 
C     OPTION CONTROL AND PRINTOUT 
C     ... 
      SS=DOT(YE,G,N) 
      WRITE(JOUT,1010) HMIN,HMAX,HPREC,LIMIT,IMP,EPS,(EG(I),I=1,3),E,SS 
      WRITE(JOUT,1040) (POND(I),I=1,N) 
      IF(KONV) GO TO 5 
      WRITE(JOUT,1060) 
      IMETH=21 
      RETURN 
    5 WRITE(JOUT,1050) (YE(I),I=1,N) 
C     INITIALIZE HESSIAN,COUNTERS AND PATH LENGTH HTOT. 
      NBOUND=N*(N+1)/2 
      DO 11 I=1,NBOUND 
   11 HESSE(I)=0.D0 
      NBOUND=N*N 
      DO 12 I=1,NBOUND 
   12 P(I,1)=0.D0 
      K=1 
      DO 13 I=1,N 
      D(I)=0.D0 
      P(K,1)=1.D0 
   13 K=K+N+1 
      IMETH=9 
      ICALL=1 
      ITERAT=0 
      NBOULO=N 
      NBOUND=N+1 
      HTOT=0.D0 
      DHTOT=HMIN*2.D0 
      IMP0=IMP 
      IMP=MIN(IMP,3)                                                    GCL0393
      RMS=SQRT(DOT(G,G,N)/DBLE(N))                                      GCL0393
C     ... 
C     AS LONG AS THE GRADIENT IS NOT YET PRECISE , 
C     GET AWAY FROM THE SADDLE IN TV DIRECTION,TOLERANCE ON ENERGY :TOLE 
C     ... 
   14 SS=DOT(G,YE,N) 
      DS=0.D0 
      DO 15 I=1,N 
   15 DS=DS+(YE(I)*C(I,1))**2 
      DS=SQRT(DS) 
      IF(SS.LT.-DS) GO TO 20 
      HTOT=HTOT+DHTOT 
      DO 16 I=1,N 
   16 X(I)=X(I)+DHTOT*YE(I) 
      RETURN 
 9002 CONTINUE 
      ICALL=ICALL+1 
      ITERAT=ITERAT+1 
      IF(IMP.LE.0) GO TO 2101 
      RMS=SQRT(DOT(G,G,N)/DBLE(N))                                      GCL0393
      IPRIN=1                                                           DJG0995
      GO TO 900 
 2101 IF(ITERAT.LT.MIN(LIMIT,20).AND.E.LT.TOLE) GO TO 14                GCL0393
      WRITE(JOUT,1120) ITERAT,SS,DS 
      IF(IMP.GE.2) GO TO 2102 
      IMP=2 
      IPRIN=2                                                           DJG0995
      GO TO 900 
 2102 IMP=IMP0 
      IMETH=21 
      RETURN 
C     ... 
C     INITIALIZATION BY EULER-CAUCHY METHOD WITH STEP HPREC<H<DHTOT 
C     ... 
   20 WRITE(JOUT,1030) ITERAT,E,HTOT 
      EOLD=E 
      HUPDAT=.FALSE. 
      IMETH=10 
      DO 21 I=1,N 
      Y1(I,1)=X(I) 
      Y1(I,2)=-G(I)/POND(I) 
      XOLD(I)=X(I) 
   21 TVOLD(I)=Y1(I,2) 
      Y1NORM=SQRT(DOT(Y1(1,2),Y1(1,2),N)) 
C     FIRST ORDER PREDICTOR 
      H=MAX(DHTOT,2.D0*HMIN)/Y1NORM 
   22 DO 23 I=1,N 
   23 Y3(I,1)=Y1(I,1)+H*Y1(I,2) 
      ITE=1 
   24 IS=1 
      GO TO 800 
C     SECOND ORDER CORRECTOR 
 3001 CRITE=0.D0 
      DO 25 I=1,N 
      Y(I,2)=Y1(I,1)+H*(Y1(I,2)+Y3(I,2))*0.5D0 
   25 CRITE=MAX(CRITE,ABS(Y(I,2)-Y3(I,1)))                              GCL0393
      IF(CRITE.LE.HPREC.AND.E.LE.EOLD+ETOL) GO TO 28 
      IF(CRITE.GT.HMAX) GO TO 27 
      DO 26 I=1,N 
   26 Y3(I,1)=Y(I,2) 
      ITE=ITE+1 
      IF(ITE.LE.3) GO TO 24 
C     REDUCED STEP SIZE 
   27 IF(H*Y1NORM.LT.HPREC) GO TO 100 
      H=MIN(2.1D0*HMIN/Y1NORM,H*0.5D0) 
      ITERAT=ITERAT+1 
      GO TO 22 
C     CONVERGENCE 
   28 EOLD=E 
      Y3NORM=SQRT(DOT(Y3(1,2),Y3(1,2),N)) 
      KONV=.TRUE. 
      RESET=.FALSE. 
      INTERP=.TRUE. 
      CALL PATH2 (Y1,Y3,N) 
      Y2NORM=SQRT(DOT(Y(1,2),Y(1,2),N)) 
      DHTOT=H*(Y1NORM+Y3NORM+4.D0*Y2NORM)/6.D0 
      HTOT=HTOT+DHTOT 
      ITERAT=ITERAT+1 
      WRITE(JOUT,1130) ITERAT,E,HTOT 
      IMP=IMP0 
C     ... 
C     START A STEP H OF P.C. IN EIGENVECTORS BASIS 
C     ... 
C     BACK ROTATE Y1 AND Y2 (IN Y3) ONTO C 
   30 Y2NORM=Y3NORM 
      IF(.NOT.HUPDAT) GO TO 36 
      DO 31 J=1,2 
      CALL MXM (P,N,Y1(1,J),N,C(1,J),1) 
      CALL MXM (P,N,Y3(1,J),N,C(1,J+2),1) 
   31 CALL MXM (P,N,Y4(1,J),N,Y2(1,J),1) 
C     ESTABLISH NEW ROTATION AND NBOUND 
      K=0 
      DO 32 I=1,N 
      DO 32 J=1,I 
      K=K+1 
   32 P(I,J)=HESSE(K) 
      CALL DIAGIV(P,N,N,D,EPS2) 
      EPS1=MAX(EPS2*DBLE(N),2.D0*HPREC/(H*DHTOT))                       GCL0393
      IF(RESET) EPS1=EPS2*DBLE(N)                                       GCL0393
      NBOUND=1 
   33 IF (D(NBOUND).GT.EPS1) GO TO 34 
      NBOUND=NBOUND+1 
      IF(NBOUND.LE.N) GO TO 33 
   34 NBOULO=NBOUND-1 
C     ROTATE Y1 AND Y2 FROM C TO THE NEW EIGENVECTORS BASIS 
      DO 35 J=1,2 
      IF (.NOT.INTERP) CALL MXM (Y2(1,J),1,P,N,Y4(1,J),N) 
      CALL MXM (C(1,J),1,P,N,Y1(1,J),N) 
   35 CALL MXM (C(1,J+2),1,P,N,Y2(1,J),N) 
      GO TO 40 
C     NO UPDATE OF THE HESSIAN 
   36 DO 37 I=1,N 
      Y2(I,1)=Y3(I,1) 
   37 Y2(I,2)=Y3(I,2) 
C     CONVERGENCE CRITERIA AND STANDARD EDITION 
   40 RMS=SQRT(DOT(G,G,N)/DBLE(N))                                      GCL0393
      IF(RMS.GT.EPS.OR.ITERAT.LT.10) GO TO 42 
      WRITE(JOUT,1140) 
      IMETH=20 
   41 IMP=MAX(IMP,3)                                                    GCL0393
      IPRIN=3                                                           DJG0995
      GO TO 900 
 2103 IMP=IMP0 
      RETURN 
   42 IF(ITERAT.LT.LIMIT) GO TO 44 
   43 WRITE(JOUT,1150) 
      IMETH=21 
      GO TO 41 
   44 ITERAT=ITERAT+1 
      IPRIN=4                                                           DJG0995
      IF (.NOT.RESET) GO TO 900 
C     ... 
C     START AN ITERATION OF CORRECTOR FORMULA WITH STEP H. 
C     ... 
 2104 CONTINUE 
      HUPDAT=.FALSE. 
C     PREDICTOR Y3 
   45 ITE=0 
      IF(NBOULO.EQ.0) GO TO 47 
      DO 46 I=1,NBOULO 
      Y3(I,1)=Y1(I,1)+2.D0*H*Y2(I,2) 
   46 Y(I,1)=ABS(Y3(I,1)-Y2(I,1))                                       GCL0393
      IF(NBOUND.GT.N) GO TO 49 
   47 DO 48 I=NBOUND,N 
      Y13=Y1(I,2)+D(I)*Y1(I,1) 
      Y23=Y2(I,2)+D(I)*Y2(I,1) 
      EXPD=EXP(-D(I)*H)                                                 GCL0393
      YE(I)=Y2(I,1)*EXPD 
      C(I,1)=(1.D0-EXPD)/D(I) 
      C(I,2)=(1.D0-C(I,1)/H)/D(I) 
      C(I,3)=(0.5D0-C(I,2)/H)/D(I) 
      Y3(I,1)=YE(I)+(C(I,1)+C(I,2))*Y23-C(I,2)*Y13 
      Y(I,1)=ABS(Y3(I,1)-Y2(I,1))                                       GCL0393
   48 YE(I)=YE(I)+(C(I,3)-0.5D0*C(I,2))*Y13+(C(I,1)-2.D0*C(I,3))*Y23 
   49 CRITE=0.D0 
      DO 50 I=1,N 
   50 CRITE=MAX(CRITE,Y(I,1)) 
      IF(CRITE.GT.HMAX) GO TO 90 
      ITE=1 
      IS=2 
      GO TO 800 
C     CORRECTOR Y3 AND CONVERGENCE CRITERIUM CRITE 
 3002 IF(NBOULO.EQ.0) GO TO 53 
      DO 52 I=1,NBOULO 
      Y(I,2)=Y3(I,1) 
      Y3(I,1)=Y1(I,1)+H*(Y3(I,2)+4.D0*Y2(I,2)+Y1(I,2))/3.D0 
   52 Y(I,1)=ABS(Y3(I,1)-Y(I,2))                                        GCL0393
      IF(NBOUND.GT.N) GO TO 55 
   53 DO 54 I=NBOUND,N 
      Y(I,2)=Y3(I,1) 
      Y3(I,1)=YE(I)+(C(I,3)+0.5D0*C(I,2))*(Y3(I,2)+D(I)*Y3(I,1)) 
   54 Y(I,1)=ABS(Y3(I,1)-Y(I,2))                                        GCL0393
   55 CRITE=0.D0 
      DO 56 I=1,N 
   56 CRITE=MAX(CRITE,Y(I,1)) 
      IF(CRITE.GT.HMAX) GO TO 90 
      CRITE=CRITE/HPREC 
      IS=3 
      GO TO 800 
 3003 IF(CRITE.GT.1.D0.AND.ITE.GT.6) GO TO 90 
      IF(CRITE.LE.1.D0) GO TO 62 
C     NEXT ITERATION OF THE IMPLICIT CORRECTOR FORMULA 
      ITE=ITE+1 
      GO TO 3002 
   62 IF(E.GT.EOLD+ETOL) GOTO 90 
C     CONVERGENCE ACHIEVED 
      RESET=.FALSE. 
      EOLD=E 
      Y3NORM=SQRT(DOT(Y3(1,2),Y3(1,2),N)) 
      CALL PATH2(Y2,Y3,N) 
      Y25NOR=SQRT(DOT(Y(1,2),Y(1,2),N)) 
      DHTOT=H*(Y2NORM+Y3NORM+4.D0*Y25NOR)/6.D0 
      HTOT=HTOT+DHTOT 
      IF(ITE.LE.2.AND.DHTOT.LE.0.5D0*HMAX.AND.KONV) GO TO 70 
      KONV=ITE.LE.2 
      IF((ITE.LE.4.OR.DHTOT.LT.HMIN*2.D0).AND.DHTOT.LT.HMAX) GO TO 64 
C     THE NEXT STEP WILL BE HALVED 
      H=H*0.5D0 
      DHTOT=DHTOT*0.5D0 
      DO 63 I=1,N 
      Y1(I,1)=Y(I,1) 
   63 Y1(I,2)=Y(I,2) 
      INTERP=.TRUE. 
      GO TO 30 
C     THE NEXT STEP WILL BE UNCHANGED 
   64 DO 65 I=1,N 
      Y1(I,1)=Y2(I,1) 
   65 Y1(I,2)=Y2(I,2) 
      INTERP=.TRUE. 
      GO TO 30 
C     THE NEXT STEP WILL BE DOUBLED WITH SAVING OF INTERMEDIATE IN Y4 
   70 H=H*2.D0 
      DHTOT=DHTOT*2.D0 
      DO 71 I=1,N 
      Y4(I,1)=Y2(I,1) 
   71 Y4(I,2)=Y2(I,2) 
      INTERP=.FALSE. 
      GO TO 30 
C     NOT YET CONVERGED : THE STEP MUST BE HALVED. 
   90 KONV=.FALSE. 
      IF (DHTOT.LT.HMIN) GO TO 96 
      IF (IMP.GE.4) WRITE(JOUT,1160) DHTOT,ITE 
      IF (.NOT.INTERP) GO TO 92 
      CALL PATH2 (Y1,Y2,N) 
      DO 91 I=1,N 
      Y1(I,1)=Y(I,1) 
   91 Y1(I,2)=Y(I,2) 
      GO TO 95 
   92 DO 93 I=1,N 
      Y1(I,1)=Y4(I,1) 
   93 Y1(I,2)=Y4(I,2) 
      INTERP=.TRUE. 
   95 H=H*0.5D0 
      DHTOT=DHTOT*0.5D0 
      GO TO 45 
C     LAST ATEMPT TO PURSUE ... 
   96 IF(.NOT.HUPDAT.OR.RESET) GO TO 98 
      WRITE(JOUT,1210) ITERAT 
      RESET=.TRUE. 
      DO 97 I=1,N 
      Y3(I,1)=Y2(I,1) 
   97 Y3(I,2)=Y2(I,2) 
      Y3NORM=Y2NORM 
      GO TO 30 
C     RESTART WITH EULER-CAUCHY IN EIGENVECTORS BASIS 
   98 WRITE(JOUT,1230) ITERAT 
      DO 99 I=1,N 
      Y1(I,1)=Y2(I,1) 
   99 Y1(I,2)=Y2(I,2) 
      Y1NORM=Y2NORM 
      NBOULO=N 
      NBOUND=N+1 
      H=2.D0*HMIN/Y1NORM 
      GO TO 22 
C     UNABLE TO CONVERGE WITH DHTOT<HMIN 
  100 WRITE(JOUT,1170) CRITE,DHTOT,HMIN,ITE 
      GO TO 43 
C     ... 
C     USUAL ENTRY POINT WITH BACK ROTATION AND HESSIAN'UPDATE 
C     ... 
C     BACK ROTATION OF Y3 ONTO X 
  800 CALL MXM (P,N,Y3,N,X,1) 
C     CALL ENERGY E AND GRADIENT G AT POINT X 
  810 RETURN 
 9003 CONTINUE 
      ICALL=ICALL+1 
C     UPDATE THE HESSIAN MATRIX 
      DO 820 I=1,N 
      Y(I,2)=-G(I)/POND(I) 
  820 DX(I)=X(I)-XOLD(I) 
      XNORM2=DOT(DX,DX,N) 
      IF(XNORM2.LE.HPREC**2) GO TO 860 
      XNORM2=1.0D0/XNORM2 
      CALL SUPDOT(Y,HESSE,DX,N,1) 
      DO 830 I=1,N 
      Y(I,1)=TVOLD(I)-Y(I,2)-Y(I,1) 
  830 Y(I,1)=CVMGT(0.D0,Y(I,1),ABS(Y(I,1)).LT.DY2(I)) 
      HUPDAT=.TRUE. 
      SCAL=DOT(Y,DX,N)*XNORM2 
      K=0 
      DO 840 I=1,N 
      DO 840 J=1,I 
      K=K+1 
  840 HESSE(K)=HESSE(K)+(Y(I,1)*DX(J)+Y(J,1)*DX(I)-DX(I)*DX(J)*SCAL) 
     . *XNORM2 
      DO 850 I=1,N 
      XOLD(I)=X(I) 
  850 TVOLD(I)=Y(I,2) 
C     ROTATE ONTO EIGENVECTORS BASIS THE WEIGHTED NEGATIVE GRADIENT Y3 
  860 CALL MXM (Y(1,2),1,P,N,Y3(1,2),N) 
      GO TO (3001,3002,3003),IS 
      WRITE(JOUT,1070) IS 
      IMETH=21 
      RETURN 
C     ... 
C     PRINTING SECTION.DISPATCH ACCORDING TO IPRIN                      DJG0995
C     ... 
  900 IF(IMP.LE.0) GO TO 920 
      WRITE(JOUT,1080) ITERAT,E,RMS,HTOT,ICALL 
      IF(IMP.GT.1) WRITE(JOUT,1090) (X(I),I=1,N) 
      IF(IMP.GT.2) WRITE(JOUT,1100) (G(I),I=1,N) 
      IF(.NOT.HUPDAT) GO TO 920 
      IF(IMP.GT.3) WRITE(JOUT,1180) NBOULO,(D(I),I=1,N) 
      IF(IMP.LT.5) GO TO 920 
      WRITE(JOUT,1190) 
      DO 910 I=1,N 
  910 WRITE(JOUT,1200)I,(P(J,I),J=1,N) 
  920 GO TO (2101,2102,2103,2104),IPRIN                                 DJG0995
      WRITE(JOUT,1110) IPRIN                                            DJG0995
      IMETH=21 
      RETURN 
C     ... 
 1000 FORMAT(2I5,3F10.0) 
 1010 FORMAT('1REACTION PATH...MIN/MAX STEPS :',2F8.5, 
     .' WITH REQUIRED ACCURACY :',F8.6/ 
     .17X,'MAX ITERATIONS=',I5,12X,'PRINTOUT LEVEL =',I3/ 
     .17X,'RMS GRADIENT CV THRESHOLD =   ',1PD9.1/ 
     .17X,'STANDARD DEVIATION OF GRADIENT',3D9.1/ 
     .17X,'STARTING POINT ENERGY=',D10.3,' <T.VCTOR!GRADIENT>=',D9.1) 
 1020 FORMAT(8F10.0) 
 1030 FORMAT(' START EULER-CAUCHY  PREDICTOR-CORRECTOR AT ITERATION',I3, 
     ./' WITH ENERGY=',1PD12.4,5X,'AND LENGTH=',0PF12.6) 
 1040 FORMAT(' WEIGHTS',9F8.4/(8X,9F8.4)) 
 1050 FORMAT(' T VCTOR',9F8.4/(8X,9F8.4)) 
 1060 FORMAT(' NO WEIGHT CAN BE NEGATIVE OR NUL ...BYE') 
 1070 FORMAT(I5,' :ILLEGAL VALUE OF''IS'' IN PATH1 ROUTINE...BYE') 
 1080 FORMAT(' ITE',I5,2X,'ENERGY=',1PD12.4,4X,'RMS GRADIENT=',D10.2/ 
     .11X,'INTEGRATED PATH LENGTH=',D12.4,4X,'NUMBER OF GRADIENT CALL' 
     .,I5) 
 1090 FORMAT(' COORD=',7F10.5/(7X,7F10.5)) 
 1100 FORMAT(' GRAD.=',7F10.2/(7X,7F10.2)) 
 1110 FORMAT(' ILLEGAL VALUE OF''IPRIN''=',I5,' IN PATH1 ROUTINE..BYE') DJG0995
 1120 FORMAT(' UNABLE TO GET AWAY FROM THE SADDLE AFTER',I4, 
     . 'ITERATIONS'/' SLOPE=',1PD10.2,' +-',D10.2) 
 1130 FORMAT(' START TIME CONSTANT PREDICTOR-CORRECTOR AT ITERATION',I3/ 
     .' WITH ENERGY=',1PD12.4,5X,'AND LENGTH=',0PF12.6) 
 1140 FORMAT(' WHAO ... CONVERGENCE ACHIEVED') 
 1150 FORMAT(' UNABLE TO CONVERGE ...BYE') 
 1160 FORMAT(' STEP (',F11.6,' ) HALVED AT ITERATION',I4, 
     . ' OF THE CORRECTOR FORMULA') 
 1170 FORMAT(' CRITERION=',F10.1,' > 1.0 OR STEP=',F11.6,' <',F11.6/ 
     .' OR ITER=',I2,'> 6 OR INCREASING ENERGY : DIVERGENCE'/ 
     .' TRY AGAIN FROM THIS POINT WITH A SMALLER VALUE OF HMIN ...') 
 1180 FORMAT(' EIGENVALUES OF HESSIAN  (WITH CUTOFF =',I3,')'/ 
     . (4X,1P,7D10.2)) 
 1190 FORMAT(' WITH EIGENVECTORS (ROWWISE)') 
 1200 FORMAT(1X,I3,1P,7D10.2/(4X,7D10.2)) 
 1210 FORMAT(' RESET H MATRIX AND TRY AGAIN AT ITERATION',I3) 
 1220 FORMAT(' ABNORMAL VALUE OF''IMETH''=',I5,' IN PATH1 ROUTINE..BYE') 
 1230 FORMAT(' RESTART EULER-CAUCHY WITH EIGENVECTORS AT ITERATION',I3) 
      END 
