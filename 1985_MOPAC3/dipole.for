      FUNCTION DIPOLE (P,Q,COORD,DIPVEC)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM), NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     2                NCLOSE,NOPEN,NDUMY,FRACT
      COMMON /KEYWRD/ KEYWRD
      COMMON /MULTIP/ DD(107), QQ(107), AM(107), AD(107), AQ(107)
      DIMENSION P(*),Q(*),COORD(3,*),DIPVEC(3)
      CHARACTER*80 KEYWRD
C***********************************************************************
C     DIPOLE CALCULATES DIPOLE MOMENTS
C
C  ON INPUT P     = DENSITY MATRIX
C           Q     = TOTAL ATOMIC CHARGES, (NUCLEAR + ELECTRONIC)
C           NUMAT = NUMBER OF ATOMS IN MOLECULE
C           NAT   = ATOMIC NUMBERS OF ATOMS
C           NFIRST= START OF ATOM ORBITAL COUNTERS
C           COORD = COORDINATES OF ATOMS
C
C  OUTPUT  DIPOLE = DIPOLE MOMENT
C***********************************************************************
C
C     IN THE ZDO APPROXIMATION, ONLY TWO TERMS ARE RETAINED IN THE
C     CALCULATION OF DIPOLE MOMENTS.
C     1. THE POINT CHARGE TERM (INDEPENDENT OF PARAMETERIZATION).
C     2. THE ONE-CENTER HYBRIDIZATION TERM, WHICH ARISES FROM MATRIX
C     ELEMENTS OF THE FORM <NS/R/NP>. THIS TERM IS A FUNCTION OF
C     THE SLATER EXPONENTS (ZS,ZP) AND IS THUS DEPENDENT ON PARAMETER-
C     IZATION. THE HYBRIDIZATION FACTORS (HYF(I)) USED IN THIS SUB-
C     ROUTINE ARE CALCULATED FROM THE FOLLOWING FORMULAE.
C     FOR SECOND ROW ELEMENTS <2S/R/2P>
C     HYF(I)= 469.56193322*(SQRT(((ZS(I)**5)*(ZP(I)**5)))/
C           ((ZS(I) + ZP(I))**6))
C     FOR THIRD ROW ELEMENTS <3S/R/3P>
C     HYF(I)=2629.107682607*(SQRT(((ZS(I)**7)*(ZP(I)**7)))/
C           ((ZS(I) + ZP(I))**8))
C     FOR FOURTH ROW ELEMENTS AND UP :
C     HYF(I)=2*(2.10716)*DD(I)
C     WHERE DD(I) IS THE CHARGE SEPARATION IN ATOMIC UNITS
C
C
C     REFERENCES:
C     J.A.POPLE & D.L.BEVERIDGE: APPROXIMATE M.O. THEORY
C     S.P.MCGLYNN, ET AL: APPLIED QUANTUM CHEMISTRY
C
      DIMENSION DIP(4,3)
      DIMENSION HYF(107,2)
      LOGICAL FIRST, FORCE
      DATA HYF(1,1)     / 0.0D00           /
      DATA   HYF(1,2) /0.0D0     /
      DATA   HYF(5,2) /6.520587D0/
      DATA   HYF(6,2) /4.253676D0/
      DATA   HYF(7,2) /2.947501D0/
      DATA   HYF(8,2) /2.139793D0/
      DATA   HYF(9,2) /2.2210719D0/
      DATA   HYF(14,2)/6.663059D0/
      DATA   HYF(15,2)/5.657623D0/
      DATA   HYF(16,2)/6.345552D0/
      DATA   HYF(17,2)/2.522964D0/
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
         DO 10 I=4,107
            HYF(I,1)= 5.0832*DD(I)
   10    CONTINUE
         FIRST=.FALSE.
         FORCE=(INDEX(KEYWRD,'FORCE') .NE. 0)
         ITYPE=1
         IF(INDEX(KEYWRD,'MINDO') .NE. 0)ITYPE=2
      ENDIF
      DO 20 I=1,4
         DO 20 J=1,3
   20 DIP(I,J)=0.0D00
      DO 30 I=1,NUMAT
         NI=NAT(I)
         IA=NFIRST(I)
         DO 30 J=1,3
            K=((IA+J)*(IA+J-1))/2+IA
            DIP(J,2)=DIP(J,2)-HYF(NI,ITYPE)*P(K)
   30 DIP(J,1)=DIP(J,1)+4.803D00*Q(I)*COORD(J,I)
      DO 40 J=1,3
   40 DIP(J,3)=DIP(J,2)+DIP(J,1)
      DO 50 J=1,3
   50 DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2)
      IF( FORCE) THEN
         DIPVEC(1)=DIP(1,3)
         DIPVEC(2)=DIP(2,3)
         DIPVEC(3)=DIP(3,3)
      ELSE
         WRITE (6,60) ((DIP(I,J),I=1,4),J=1,3)
      ENDIF
      DIPOLE = DIP(4,3)
      RETURN
C
   60 FORMAT (' DIPOLE',11X,2HX ,8X,2HY ,8X,2HZ ,6X,'TOTAL',/,
     1' POINT-CHG.',4F10.3/,' HYBRID',4X,4F10.3/,' SUM',7X,4F10.3)
C
      END
