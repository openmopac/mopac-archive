      SUBROUTINE INTERP(N,NP,NQ,MODE,E,FP,CP,VEC,FOCK,P,H,VECL,*)       CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION FP(MPACK), CP(N,N)
      DIMENSION VEC(N,N), FOCK(N,N),
     1          P(N,N), H(N*N), VECL(N*N)
**********************************************************************
*
* INTERP: AN INTERPOLATION PROCEDURE FOR FORCING SCF CONVERGENCE
*         ORIGINAL THEORY AND FORTRAN WRITTEN BY R.N. CAMP AND
*         H.F. KING, J. CHEM. PHYS. 75, 268 (1981)
**********************************************************************
*
* ON INPUT N     = NUMBER OF ORBITALS
*          NP    = NUMBER OF FILLED LEVELS
*          NQ    = NUMBER OF EMPTY LEVELS
*          MODE  = 1, DO NOT RESET.
*          E     = ENERGY
*          FP    = FOCK MATRIX, AS LOWER HALF TRIANGLE, PACKED
*          CP    = EIGENVECTORS OF FOCK MATRIX OF ITERATION -1
*                  AS PACKED ARRAY OF N*N COEFFICIENTS
*
* ON OUTPUT CP   = BEST GAUSSED SET OF EIGENVECTORS
*           MODE = 2 OR 3 - USED BY CALLING PROGRAM
**********************************************************************
      DIMENSION THETA(MAXORB), IA(MAXORB)
      COMMON /KEYWRD/ KEYWRD
C common FIT splitted for portability (Ivan Rossi 0394 &8) )
      COMMON /FITPTS/ NPNTS
     1       /FITDAT/ XLOW,XHIGH,XMIN,EMIN,DEMIN,X(12),F(12),DF(12)
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      LOGICAL DEBUG, DEBUG1
      CHARACTER*160 KEYWRD
         SAVE                                                           GL0892
      DATA ICALCN /0/
      DATA ZERO,FF,RADMAX/0.0,0.9,1.5708/
      XOLD=0.d0                                                         LF0610
      EOLD=0.d0                                                         LF0610
      DEBUG=.false.                                                     LF0610
      DEBUG1=.false.                                                    LF0610
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
         DEBUG=(INDEX(KEYWRD,'INTERP').NE.0)
         DEBUG1=(INDEX(KEYWRD,'DEBUG').NE.0.AND.DEBUG)
         DO 10 I=1,MAXORB
   10    IA(I)=(I*I-I)/2
      ENDIF
C
C     RADMAX=MAXIMUM ROTATION ANGLE (RADIANS).  1.5708 = 90 DEGREES.
C         FF=FACTOR FOR CONVERGENCE TEST FOR 1D SEARCH.
C
      MINPQ=MIN0(NP,NQ)
      MAXPQ=MAX0(NP,NQ)
      NP1=NP+1
      NP2=MAX0(1,NP/2)
      IF(MODE.EQ.2) GO TO 110
C
C     (MODE=1 OR 3 ENTRY)
C     TRANSFORM FOCK MATRIX TO CURRENT MO BASIS.
C     ONLY THE OFF DIAGONAL OCC-VIRT BLOCK IS COMPUTED.
C     STORE IN FOCK ARRAY
C
      II=0
      DO 50 I=1,N
         I1=I+1
         DO 40 J=1,NQ
            DUM=ZERO
            DO 20 K=1,I
   20       DUM=DUM+FP(II+K)*CP(K,J+NP)
            IF(I.EQ.N) GO TO 40
            IK=II+I+I
            DO 30 K=I1,N
               DUM=DUM+FP(IK)*CP(K,J+NP)
   30       IK=IK+K
   40    P(I,J)=DUM
   50 II=II+I
      DO 80 I=1,NP
         DO 70 J=1,NQ
            DUM=ZERO
            DO 60 K=1,N
   60       DUM=DUM+CP(K,I)*P(K,J)
            FOCK(I,J)=DUM
   70    CONTINUE
   80 CONTINUE
      IF(MODE.EQ.3) GO TO 100
C
C     CURRENT POINT BECOMES OLD POINT (MODE=1 ENTRY)
C
      DO 90 I=1,N
         DO 90 J=1,N
   90 VEC(I,J)=CP(I,J)
      EOLD=E
      XOLD=1.0
      MODE=2
      RETURN
C
C     (MODE=3 ENTRY)
C     FOCK CORRESPONDS TO CURRENT POINT IN CORRESPONDING REPRESENTATION.
C     VEC DOES NOT HOLD CURRENT VECTORS. VEC SET IN LAST MODE=2 ENTRY.
C
  100 NPNTS=NPNTS+1
      IF(DOPRNT.AND.DEBUG)                                              CIO
     &   WRITE(6,'(''   INTERPOLATED ENERGY:'',F13.6)')E*23.061D0       CIO
      IPOINT=NPNTS
      GO TO 500
C
C    (MODE=2 ENTRY) CALCULATE THETA, AND U, V, W MATRICES.
C                   U ROTATES CURRENT INTO OLD MO.
C                   V ROTATES CURRENT INTO CORRESPONDING CURRENT MO.
C                   W ROTATES OLD INTO CORRESPONDING OLD MO.
C
  110 J1=1
      DO 140 I=1,N
         IF(I.EQ.NP1) J1=NP1
         DO 130 J=J1,N
            P(I,J)=ZERO
            DO 120 K=1,N
  120       P(I,J)=P(I,J)+CP(K,I)*VEC(K,J)
  130    CONTINUE
  140 CONTINUE
C
C     U = CP(DAGGER)*VEC IS NOW IN P ARRAY.
C     VEC IS NOW AVAILABLE FOR TEMPORARY STORAGE.
C
      IJ=0
      DO 170 I=1,NP
         DO 160 J=1,I
            IJ=IJ+1
            H(IJ)=0.D0
            DO 150 K=NP1,N
  150       H(IJ)=H(IJ)+P(I,K)*P(J,K)
  160    CONTINUE
  170 CONTINUE
      CALL HQRII(H,NP,NP,THETA,VECL,*9999)                               CSTP(call)
      DO 180 I=NP,1,-1
         IL=I*NP-NP
         DO 180 J=NP,1,-1
  180 VEC(J,I)=VECL(J+IL)
      DO 200 I=1,NP2
         DUM=THETA(NP1-I)
         THETA(NP1-I)=THETA(I)
         THETA(I)=DUM
         DO 190 J=1,NP
            DUM=VEC(J,NP1-I)
            VEC(J,NP1-I)=VEC(J,I)
  190    VEC(J,I)=DUM
  200 CONTINUE
      DO 210 I=1,MINPQ
         THETA(I)=MAX(THETA(I),ZERO)
         THETA(I)=MIN(THETA(I),1.D0)
  210 THETA(I)=ASIN(SQRT(THETA(I)))
C
C     THETA MATRIX HAS NOW BEEN CALCULATED, ALSO UNITARY VP MATRIX
C     HAS BEEN CALCULATED AND STORED IN FIRST NP COLUMNS OF VEC MATRIX.
C     NOW COMPUTE WQ
C
      DO 240 I=1,NQ
         DO 230 J=1,MINPQ
            VEC(I,NP+J)=ZERO
            DO 220 K=1,NP
  220       VEC(I,NP+J)=VEC(I,NP+J)+P(K,NP+I)*VEC(K,J)
  230    CONTINUE
  240 CONTINUE
      CALL SCHMIT(VEC(1,NP1),NQ,N,*9999)                                 CSTP(call)
C
C     UNITARY WQ MATRIX NOW IN LAST NQ COLUMNS OF VEC MATRIX.
C     TRANSPOSE NP BY NP BLOCK OF U STORED IN P
C
      DO 260 I=1,NP
         DO 250 J=1,I
            DUM=P(I,J)
            P(I,J)=P(J,I)
  250    P(J,I)=DUM
  260 CONTINUE
C
C     CALCULATE WP MATRIX AND HOLD IN FIRST NP COLUMNS OF P
C
      DO 300 I=1,NP
         DO 270 K=1,NP
  270    H(K)=P(I,K)
         DO 290 J=1,NP
            P(I,J)=ZERO
            DO 280 K=1,NP
  280       P(I,J)=P(I,J)+H(K)*VEC(K,J)
  290    CONTINUE
  300 CONTINUE
      CALL SCHMIB(P,NP,N,*9999)                                          CSTP(call)
C
C     CALCULATE VQ MATRIX AND HOLD IN LAST NQ COLUMNS OF P MATRIX.
C
      DO 340 I=1,NQ
         DO 310 K=1,NQ
  310    H(K)=P(NP+I,NP+K)
         DO 330 J=NP1,N
            P(I,J)=ZERO
            DO 320 K=1,NQ
  320       P(I,J)=P(I,J)+H(K)*VEC(K,J)
  330    CONTINUE
  340 CONTINUE
      CALL SCHMIB(P(1,NP1),NQ,N,*9999)                                   CSTP(call)
C
C     CALCULATE (DE/DX) AT OLD POINT
C
      DEDX=ZERO
      DO 370 I=1,NP
         DO 360 J=1,NQ
            DUM=ZERO
            DO 350 K=1,MINPQ
  350       DUM=DUM+THETA(K)*P(I,K)*VEC(J,NP+K)
  360    DEDX=DEDX+DUM*FOCK(I,J)
  370 CONTINUE
C
C     STORE OLD POINT INFORMATION FOR SPLINE FIT
C
      DEOLD=-4.0*DEDX
      X(2)=XOLD
      F(2)=EOLD
      DF(2)=DEOLD
C
C     MOVE VP OUT OF VEC ARRAY INTO FIRST NP COLUMNS OF P MATRIX.
C
      DO 380 I=1,NP
         DO 380 J=1,NP
  380 P(I,J)=VEC(I,J)
      K1=0
      K2=NP
      DO 410 J=1,N
         IF(J.EQ.NP1) K1=NP
         IF(J.EQ.NP1) K2=NQ
         DO 400 I=1,N
            DUM=ZERO
            DO 390 K=1,K2
  390       DUM=DUM+CP(I,K1+K)*P(K,J)
  400    VEC(I,J)=DUM
  410 CONTINUE
C
C     CORRESPONDING CURRENT MO VECTORS NOW HELD IN VEC.
C     COMPUTE VEC(DAGGER)*FP*VEC
C     STORE OFF-DIAGONAL BLOCK IN FOCK ARRAY.
C
  420 II=0
      DO 460 I=1,N
         I1=I+1
         DO 450 J=1,NQ
            DUM=ZERO
            DO 430 K=1,I
  430       DUM=DUM+FP(II+K)*VEC(K,J+NP)
            IF(I.EQ.N) GO TO 450
            IK=II+I+I
            DO 440 K=I1,N
               DUM=DUM+FP(IK)*VEC(K,J+NP)
  440       IK=IK+K
  450    P(I,J)=DUM
  460 II=II+I
      DO 490 I=1,NP
         DO 480 J=1,NQ
            DUM=ZERO
            DO 470 K=1,N
  470       DUM=DUM+VEC(K,I)*P(K,J)
            FOCK(I,J)=DUM
  480    CONTINUE
  490 CONTINUE
C
C     SET LIMITS ON RANGE OF 1-D SEARCH
C
      NPNTS=2
      IPOINT=1
      XNOW=ZERO
      XHIGH=RADMAX/THETA(1)
      XLOW=-0.5*XHIGH
C
C     CALCULATE (DE/DX) AT CURRENT POINT AND
C     STORE INFORMATION FOR SPLINE FIT
C     ***** JUMP POINT FOR MODE=3 ENTRY *****
C
  500 DEDX=ZERO
      DO 510 K=1,MINPQ
  510 DEDX=DEDX+THETA(K)*FOCK(K,K)
      DENOW=-4.0*DEDX
      ENOW=E
      IF(IPOINT.LE.12) GO TO 530
  520 FORMAT(//34H EXCESSIVE DATA PNTS FOR SPLINE./
     1,9H IPOINT =,I3,15H MAXIMUM IS 12.)
C
C     PERFORM 1-D SEARCH AND DETERMINE EXIT MODE.
C
  530 X(IPOINT)=XNOW
      F(IPOINT)=ENOW
      DF(IPOINT)=DENOW
      CALL SPLINE(*9999)                                                 CSTP(call)
      IF((EOLD-ENOW).GT.FF*(EOLD-EMIN).OR.IPOINT.GT.10) GO TO 560
C
C     (MODE=3 EXIT) RECOMPUTE CP VECTORS AT PREDICTED MINIMUM.
C
      XNOW=XMIN
      DO 550 K=1,MINPQ
         CK=COS(XNOW*THETA(K))
         SK=SIN(XNOW*THETA(K))
         IF(DOPRNT.AND.DEBUG)                                           CIO
     &     WRITE(6,'('' ROTATION ANGLE:'',F12.4)')SK*57.29578           CIO
         DO 540 I=1,N
            CP(I,K)   =CK*VEC(I,K)-SK*VEC(I,NP+K)
  540    CP(I,NP+K)=SK*VEC(I,K)+CK*VEC(I,NP+K)
  550 CONTINUE
      MODE=3
      RETURN
C
C     (MODE=2 EXIT) CURRENT VECTORS GIVE SATISFACTORY ENERGY IMPROVEMENT
C     CURRENT POINT BECOMES OLD POINT FOR THE NEXT 1-D SEARCH.
C
  560 IF(MODE.EQ.2) GO TO 580
      DO 570 I=1,N
         DO 570 J=1,N
  570 VEC(I,J)=CP(I,J)
      MODE=2
  580 ROLD=XOLD*THETA(1)*57.29578
      RNOW=XNOW*THETA(1)*57.29578
      RMIN=XMIN*THETA(1)*57.29578
      IF(DOPRNT.AND.DEBUG) WRITE(6,600) XOLD,EOLD*23.061,DEOLD,ROLD     CIO
     1,             XNOW,ENOW*23.061,DENOW,RNOW
     2,             XMIN,EMIN*23.061,DEMIN,RMIN
      EOLD=ENOW
      IF(NPNTS.LE.200) RETURN
      IF (DOPRNT) WRITE(6,610)                                          CIO
      DO 590 K=1,NPNTS
  590 IF (DOPRNT) WRITE(6,620) K,X(K),F(K),DF(K)                        CIO
      IF (DOPRNT) WRITE(6,630)                                          CIO
  600 FORMAT(
     1/14X,3H X ,10X,6H F(X) ,9X,7H DF/DX ,21H   ROTATION (DEGREES),
     2/10H      OLD ,F10.5,3F15.10,
     3/10H  CURRENT ,F10.5,3F15.10,
     4/10H PREDICTED,F10.5,3F15.10/)
  610 FORMAT(3H  K,10H     X(K) ,15H       F(K)    ,10H     DF(K))
  620 FORMAT(I3,F10.5,2F15.10)
  630 FORMAT(10X)
      RETURN
 9999 RETURN 1                                                          CSTP
      ENTRY INTERP_INIT                                                 CSAV
                CK = 0.0D0                                              CSAV
             DEBUG = .FALSE.                                            CSAV
            DEBUG1 = .FALSE.                                            CSAV
              DEDX = 0.0D0                                              CSAV
             DENOW = 0.0D0                                              CSAV
             DEOLD = 0.0D0                                              CSAV
               DUM = 0.0D0                                              CSAV
              ENOW = 0.0D0                                              CSAV
              EOLD = 0.0D0                                              CSAV
                FF = 0.9D0                                              CSAV
                 I = 0                                                  CSAV
                I1 = 0                                                  CSAV
                IA = 0                                                  CSAV
            ICALCN = 0                                                  CSAV
                II = 0                                                  CSAV
                IJ = 0                                                  CSAV
                IK = 0                                                  CSAV
                IL = 0                                                  CSAV
            IPOINT = 0                                                  CSAV
                 J = 0                                                  CSAV
                J1 = 0                                                  CSAV
                 K = 0                                                  CSAV
                K1 = 0                                                  CSAV
                K2 = 0                                                  CSAV
             MAXPQ = 0                                                  CSAV
             MINPQ = 0                                                  CSAV
               NP1 = 0                                                  CSAV
               NP2 = 0                                                  CSAV
            RADMAX = 1.5708D0                                           CSAV
              RMIN = 0.0D0                                              CSAV
              RNOW = 0.0D0                                              CSAV
              ROLD = 0.0D0                                              CSAV
                SK = 0.0D0                                              CSAV
             THETA = 0.0D0                                              CSAV
              XNOW = 0.0D0                                              CSAV
              XOLD = 0.0D0                                              CSAV
              ZERO = 0.0D0                                              CSAV
      RETURN                                                            CSAV
      END
