      DOUBLE PRECISION FUNCTION AABBCD(IOCCA1, IOCCB1, IOCCA2,
     1                                 IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
       INCLUDE 'SIZES.i'
***********************************************************************
*
* AABBCD EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY TWO SETS OF M.O.S. ONE MICROSTATE HAS AN ALPHA ELECTRON
*       IN PSI(I) AND A BETA ELECTRON IN PSI(K) FOR WHICH THE OTHER
*       MICROSTATE HAS AN ALPHA ELECTRON IN PSI(J) AND A BETA ELECTRON
*       IN PSI(L)
*
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /SPQR/ ISPQR(MAXCI,NMECI),IS,ILOOP, JLOOP
       SAVE
      DO 10 I=1,NMOS
   10 IF(IOCCA1(I) .NE. IOCCA2(I)) GOTO 20
   20 DO 30 J=I+1,NMOS
   30 IF(IOCCA1(J) .NE. IOCCA2(J)) GOTO 40
   40 DO 50 K=1,NMOS
   50 IF(IOCCB1(K) .NE. IOCCB2(K)) GOTO 60
   60 DO 70 L=K+1,NMOS
   70 IF(IOCCB1(L) .NE. IOCCB2(L)) GOTO 80
   80 IF( I.EQ.K .AND. J.EQ.L .AND. IOCCA1(I).NE.IOCCB1(I)) THEN
         ISPQR(ILOOP,IS)=JLOOP
         IS=IS+1
      ENDIF
      IF(IOCCA1(I) .LT. IOCCA2(I)) THEN
         M=I
         I=J
         J=M
      ENDIF
      IF(IOCCB1(K) .LT. IOCCB2(K)) THEN
         M=K
         K=L
         L=M
      ENDIF
      XR=XY(I,J,K,L)
C
C   NOW UNTANGLE THE MICROSTATES
C
      IJ=1
      IF( I.GT.K .AND. J.GT.L .OR. I.LE.K .AND. J.LE.L)IJ=0
      M=J
      J=K
      K=M
      IF( I.GT.J ) IJ=IJ+IOCCA1(J)+IOCCB1(I)
      IF( K.GT.L ) IJ=IJ+IOCCA2(L)+IOCCB2(K)
      IF(I.NE.J)THEN
         IJMAX=MAX(I,J)
         IJMIN=MIN(I,J)
         DO 90 M=IJMIN,IJMAX
   90    IJ=IJ+IOCCB1(M)+IOCCA1(M)
      ENDIF
      IF(K.NE.L) THEN
         KLMIN=MIN(K,L)
         KLMAX=MAX(K,L)
         DO 100 M=KLMIN,KLMAX
  100    IJ=IJ+IOCCB2(M)+IOCCA2(M)
      ENDIF
C
C   IJ IN THE PERMUTATION NUMBER, .EQUIV. -1 IF IJ IS ODD.
C
      AABBCD=XR*DBLE(1-2*MOD(IJ,2))                                     GCL0393
      RETURN
      END