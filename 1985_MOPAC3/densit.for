      SUBROUTINE DENSIT( C,MDIM, NORBS,NDUBL, NSINGL, FRACT, P)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION P(*), C(MDIM,*)
C***********************************************************************
C
C   DENSIT COMPUTES THE DENSITY MATRIX GIVEN THE EIGENVECTOR MATRIX, AND
C          INFORMATION ABOUT THE M.O. OCCUPANCY.
C
C  INPUT:  C     = SQUARE EIGENVECTOR MATRIX, C IS OF SIZE MDIM BY MDIM
C                  AND THE EIGENVECTORS ARE STORED IN THE TOP LEFT-HAND
C                  CORNER.
C          NORBS = NUMBER OF ORBITALS
C          NDUBL = NUMBER OF DOUBLY-OCCUPIED M.O.S ( =0 IF UHF)
C          NSINGL= NUMBER OF SINGLY OR FRACTIONALLY OCCUPIED M.O.S.
C
C   ON EXIT: P   = DENSITY MATRIX
C
C***********************************************************************
C
C SET UP LIMITS FOR SUMS
C  NL1 = BEGINING OF ONE ELECTRON SUM
C  NU1 = END OF SAME
C  NL2 = BEGINING OF TWO ELECTRON SUM
C  NU2 = END OF SAME
C
      NORBS2=NORBS/2
      NSINGL=MAX(NDUBL,NSINGL)
      IF(NSINGL .GT. NORBS2) THEN
C
C    TAKE POSITRON EQUIVALENT
C
         SIGN=-1.D0
         FRAC=2.D0-FRACT
         IF(NDUBL.EQ.0)THEN
            CONST=1.D0
            NL2=2
            NU2=0
            NL1=NSINGL+1
            NU1=NORBS
         ELSE
            CONST=2.D0
            NL2=NSINGL+1
            NU2=NORBS
            NL1=NDUBL+1
            NU1=NSINGL
         ENDIF
      ELSE
C
C    TAKE ELECTRON EQUIVALENT
C
         SIGN=1.D0
         FRAC=FRACT
         CONST=0.D0
         NL2=1
         NU2=NDUBL
         NL1=NDUBL+1
         NU1=NSINGL
      ENDIF
      L=0
      DO 40 I=1,NORBS
         DO 30 J=1,I
            L=L+1
            SUM2=0.D0
            SUM1=0.D0
            DO 10 K=NL2,NU2
   10       SUM2=SUM2+C(I,K)*C(J,K)
            SUM2=SUM2*2.D0
            DO 20 K=NL1,NU1
   20       SUM1=SUM1+C(I,K)*C(J,K)
   30    P(L)=(SUM2+SUM1*FRAC)*SIGN
   40 P(L)=CONST+P(L)
      RETURN
      END
