      SUBROUTINE DERI1 (C,CT,N,COORD,STEP,NUMBER,WORK,GRAD,W2,H2,H3
     .                 ,F,MINEAR,FD,NINEAR,PQKL,MPQKL,CIJRDY)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      PARAMETER (MFBWO=9*MAXPAR)                                        3GL3092
      EXTERNAL SDOT                                                     GL0492
C********************************************************************
C
C     DERI1 COMPUTE THE NON-RELAXED DERIVATIVE OF THE (1/2 ELECTRON OR
C     CI)-ENERGY WITH RESPECT TO ONE CARTESIAN COORDINATE AT A TIME
C                             AND
C     COMPUTE THE NON-RELAXED FOCK MATRIX DERIVATIVE IN M.O BASIS AS
C     REQUIRED IN THE RELAXATION SECTION (ROUTINE 'DERI2').
C
C   INPUT
C     C(N,N) : M.O. COEFFICIENTS.
C     CT     : IDEM, TRANSPOSED AND ORDERED AS CLOSED,OPEN,VIRTUAL.
C     COORD  : CARTESIAN COORDINATES ARRAY.
C     STEP   : STEP SIZE OF THE FINITE DIFFERENCE METHOD USED FOR THE
C              INTEGRAL DERIVATIVES.
C     NUMBER : LOCATION OF THE REQUIRED VARIABLE IN COORD.
C     WORK   : WORK ARRAY OF SIZE N*N.
C     W2     : WORK ARRAYS FOR d<PQ!RS> (2-CENTERS  A.O)
C     PQKL(MPQKL)           : WORK ARRAY  FOR d<IJ!KL> (C.I-ACTIVE M.O)
C   OUTPUT
C     C,CT,COORD,NUMBER : NOT MODIFIED.
C     STEP   : DESTROYED.
C     GRAD   : DERIVATIVE OF THE HEAT OF FORMATION WITH RESPECT TO
C              COORD(NUMBER), WITHOUT RELAXATION CORRECTION.
C     F(MINEAR) : NON-RELAXED FOCK MATRIX DERIVATIVE WITH RESPECT TO
C              COORD(NUMBER), EXPRESSED IN M.O BASIS, SCALED AND
C              PACKED, OFF-DIAGONAL BLOCKS ONLY.
C     FD(NINEAR): IDEM BUT UNSCALED, DIAGONAL BLOCKS, C.I-ACTIVE ONLY.
C
C***********************************************************************
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  3GL3092
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        3GL3092
     2                NCLOSE,NOPEN,NDUMY                                3GL3092
     3       /VECTOR/ CDUM(MORB2),EIGS(MAXORB),WDUM(MORB2),EIGB(MAXORB)
     4       /DENSTY/ P(MPACK), PA(MPACK), PB(MPACK)
     5       /WMATRX/ WDUMMY(N2ELEC*3), NUMBW, NWDUM(NUMATM)
     6       /HMATRX/ H(MPACK)
     7       /ELECT / ELECT
     8       /ENUCLR/ ENUCLR
     9       /PRECI / SCFCV,SCFTOL,EG(3),ESTIM(3),PMAX(3),KTYP(MAXPAR)
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
     1       /CIDATA/ VECTCI(MAXCI),XXCI,NCI1,NCI2,NCI3,                DL0397
     2                NCIDUM((1+2*NMECI)*MAXCI)                         DL0397
     3       /SCRAH1/ SCALAR(MPACK/2),DIAG(MPACK/2),FRACT2,FBWO(MFBWO), 3GL3092
     4                NBO(3), IDUM                                      3GL3092
     5       /SCRCHR/ DIJKLD(NMECI*(NMECI+1)*NUMATM*5),BUF(MORB2),      3GL3092
     6       DUM3(6*MAXPAR**2+1-MORB2-(NMECI*(NMECI+1)*NUMATM*5))       GCL0393
C
      DIMENSION COORD(*),C(N,N),WORK(N,N),F(*),FD(*),W2(*),H2(*),H3(*)
      DIMENSION CT(*)
      LOGICAL DEBUG,CIJRDY
       SAVE
C
      DEBUG=IDERI1.NE.0                                                 DJG0995
      LINEAR=NORBS*(NORBS+1)/2
      CALL PORCPU (TIME1)                                               GL0492
C
C     2 POINTS FINITE DIFFERENCE TO GET THE INTEGRAL DERIVATIVES
C     ----------------------------------------------------------
C     STORED IN H2 AND W2, WITHOUT DIVIDING BY THE STEP SIZE.
C
      NATI=(NUMBER-1)/3+1
      NATX=NUMBER-3*(NATI-1)
      CALL DHCORE (COORD,H2,W2,ENUCL2,NATI,NATX,STEP)
      STEP=0.5D0/STEP
C
C     NON-RELAXED FOCK MATRIX DERIVATIVE IN A.O BASIS.
C     ------------------------------------------------
C     STORED IN H3, DIVIDED BY STEP.
C
      CALL SCOPY (LINEAR,H2,1,H3,1)
      CALL DFOCK2 (H3,P,PA,W2,NUMAT,NFIRST,NMIDLE,NLAST,NATI)
C
C     DERIVATIVE OF THE SCF-ONLY ENERGY (I.E BEFORE C.I CORRECTION)
C     -------------------------------------------------------------
C
      GRAD=(HELECT(NORBS,P,H2,H3)+ENUCL2)*STEP
C     TAKE STEP INTO ACCOUNT IN H3
      DO 10 I=1,LINEAR
   10 H3(I)=H3(I)*STEP
C
C     RIGHT-HAND SIDE SUPER-VECTOR F = C' H3 C USED IN RELAXATION
C     -----------------------------------------------------------
C     STORED IN NON-STANDARD PACKED FORM IN F(MINEAR) AND FD(NINEAR).
C     THE SUPERVECTOR IS THE NON-RELAXED FOCK MATRIX DERIVATIVE IN
C     M.O BASIS: F(IJ)= ( (C' * FOCK * C)(I,J) )   WITH I.GT.J .
C     F IS SCALED AND PACKED IN SUPERVECTOR FORM WITH
C                THE CONSECUTIVE FOLLOWING OFF-DIAGONAL BLOCKS:
C             1) OPEN-CLOSED  I.E. F(IJ)=F(I,J) WITH I OPEN & J CLOSED
C                                  AND I RUNNING FASTER THAN J,
C             2) VIRTUAL-CLOSED SAME RULE OF ORDERING,
C             3) VIRTUAL-OPEN   SAME RULE OF ORDERING.
C     FD IS PACKED AS FOLLOWS
C        1) .... THE CONSECUTIVE DIAGONAL BLOCKS OVER C.I-ACTIVE M.O:
C             1) CLOSED-CLOSED   IN CANONICAL ORDER, WITHOUT THE
C                                DIAGONAL ELEMENTS,
C             2) OPEN-OPEN       SAME RULE OF ORDERING,
C             3) VIRTUAL-VIRTUAL SAME RULE OF ORDERING.
C        2) .... OFF-DIAGONAL VIRTUAL-VIRTUAL-C.I-ACTIVE (IF ANY)
C        3) .... DIAGONAL ELEMENTS OVER C.I-ACTIVE M.O
C
C     PART 1 : WORK(N,NEND) = H3(N,N) * C(N,NEND)
      NEND=MAX(NOPEN,NCI1+NCI2)
C     CRAY VERSION: UNPACK H3 IN BUF THEN CALL MXM.
      K=0
      DO 20 I=1,N
      IJ=I
CDIR$ IVDEP
      DO 20 JI=N*(I-1)+1,N*(I-1)+I
      K=K+1
      BUF(JI)=H3(K)
      BUF(IJ)=H3(K)
   20 IJ=IJ+N
      CALL MXM (BUF,N,C,N,WORK,NEND)                                    DL0397!
C
C     PART 2 : F(IJ) =  (C' * WORK)(I,J) ... OFF-DIAGONAL BLOCKS.
      L=1
      IF(NBO(2).NE.0 .AND. NBO(1).NE.0) THEN
C        OPEN-CLOSED
         CALL MXM (CT(NBO(1)*N+1),NBO(2),WORK,N,F(L),NBO(1))
         L=L+NBO(2)*NBO(1)
      ENDIF
      IF(NBO(3).NE.0 .AND. NBO(1).NE.0) THEN
C        VIRTUAL-CLOSED
         CALL MXM (CT(NOPEN*N+1),NBO(3),WORK,N,F(L),NBO(1))
         L=L+NBO(3)*NBO(1)
      ENDIF
      IF(NBO(3).NE.0 .AND. NBO(2).NE.0) THEN
C        VIRTUAL-OPEN
         CALL MXM (CT(NOPEN*N+1),NBO(3),WORK(1,NBO(1)+1),N,F(L),NBO(2))
      ENDIF
C     SCALE F ACCORDING TO THE DIAGONAL METRIC TENSOR 'SCALAR '.
      DO 30 I=1,MINEAR
   30 F(I)=F(I)*SCALAR(I)
C
C     PART 3 : SUPER-VECTOR FD, C.I-ACTIVE DIAGONAL BLOCKS, UNSCALED.
      L=1
      NEND=0
      DO 50 LOOP=1,3
      NINIT=NEND+1
      NEND =NEND+NBO(LOOP)
      N1=MAX(NINIT,NCI1+1   )
      N2=MIN(NEND ,NCI1+NCI2)
      IF(N2.LT.N1) GO TO 50
      DO 40 I=N1,N2
      IF(I.GT.NINIT) THEN
         CALL MXM (C(1,I),1,WORK(1,NINIT),N,FD(L),I-NINIT)
         L=L+I-NINIT
      ENDIF
   40 CONTINUE
   50 CONTINUE
      NCOL=N2-NINIT+1
      IF (NCOL.GT.0.AND.N2.LT.N) THEN
         CALL MTXM (C(1,N2+1),N-N2,WORK(1,NINIT),N,FD(L),NCOL)
         L=L+NCOL*(N-N2)
      ENDIF
C
C     NON-RELAXED C.I CORRECTION TO THE ENERGY DERIVATIVE.
C     ----------------------------------------------------
C
C     C.I-ACTIVE FOCK EIGENVALUES DERIVATIVES, STORED IN FD(CONTINUED).
      LCUT=L
      DO 60 I=NCI1+1,NCI1+NCI2
      FD(L)=SDOT(N,C(1,I),1,WORK(1,I),1)
   60 L=L+1
C     NOW WORK IS RELEASED AND FD HAS BEEN COMPLETED.
C
C     C.I-ACTIVE 2-ELECTRONS INTEGRALS DERIVATIVES. STORED IN XY.
      CALL DIJKL1 (C,N,NCI1+1,NCI2,W2,PQKL,MPQKL,XY,NMECI,NATI,CIJRDY)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CIJRDY=NATX.NE.3
      DO 70 I=1,NCI2
      DO 70 J=1,NCI2
      DO 70 K=1,NCI2
      DO 70 L=1,NCI2
   70 XY(I,J,K,L)=XY(I,J,K,L)*STEP
C
C     BUILD THE C.I MATRIX DERIVATIVE, STORED IN W2.
      CALL MECID (FD(LCUT-NCI1),GSE,EIGB,WORK)
      CALL MECIH (WORK,W2)
C
C     NON-RELAXED C.I CONTRIBUTION TO THE ENERGY DERIVATIVE.
      CALL SUPDOT (WORK,W2,VECTCI,NCI3,1)
      GRAD=(GRAD+SDOT(NCI3,VECTCI,1,WORK,1))*23.061D0
      IF(DEBUG.AND.IDEBUG.NE.0) THEN                                    DJG0995
         WRITE(JOUT,'('' * * * GRADIENT COMPONENT NUMBER'',I4)')NUMBER
         WRITE(JOUT,'('' NON-RELAXED C.I-ACTIVE FOCK EIGENVALUES '',
     .                ''DERIVATIVES (E.V.)'')')
         WRITE(JOUT,'(8F10.4)')(FD(LCUT-1+I),I=1,NCI2)
         WRITE(JOUT,'('' NON-RELAXED 2-ELECTRONS DERIVATIVES (E.V.)''/
     .''  I    J    K    L         d<I(1)J(1)!K(2)L(2)>'')')
         DO 80 I=1,NCI2
         DO 80 J=1,I
         DO 80 K=1,I
         LL=K
         IF(K.EQ.I) LL=J
         DO 80 L=1,LL
   80    WRITE(JOUT,'(4I5,F20.10)')
     .              NCI1+I,NCI1+J,NCI1+K,NCI1+L,XY(I,J,K,L)
      ENDIF
      IF (DEBUG) THEN
         WRITE(JOUT,'('' NON-RELAXED CART. GRADIENT COMPONENT'',
     .I3,'' : '',F10.4,'' KCAL/MOLE'')')NUMBER,GRAD
         CALL PORCPU (TFLY)                                             GL0492
         WRITE(JOUT,'('' ELAPSED TIME IN DERI1'',F10.4,'' SECOND'')')
     .              TFLY-TIME1
      ENDIF
      RETURN
      END
