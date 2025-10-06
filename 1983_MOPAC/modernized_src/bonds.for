       SUBROUTINE BONDS(P)
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       INCLUDE 'SIZES'
       PARAMETER (NATMS2=(NUMATM*(NUMATM+1))/2)
       DIMENSION P(*)
       COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),
     1      NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     1                NCLOSE,NOPEN
       COMMON /SCRACH/ B(MAXORB,MAXORB), BONDAB(NATMS2)
C***********************************************************************
C
C    CALCULATES, AND PRINTS, THE BOND INDICES AND VALENCIES OF ATOMS
C
C    FOR REFERENCE, SEE "BOND INDICES AND VALENCY", J. C. S. DALTON,
C    ARMSTRONG, D.R., PERKINS, P.G., AND STEWART, J.J.P., 838 (1973)
C
C     ON INPUT
C              P = DENSITY MATRIX, LOWER HALF TRIANGLE, PACKED.
C              P   IS NOT ALTERED BY BONDS.
C
C***********************************************************************
       WRITE(6,11)
   11  FORMAT(//20X,'BOND ORDERS AND VALENCIES',//)
       K=0
       DO 1 I=1,NORBS
           DO 1 J=1,I
               K=K+1
               B(I,J)=P(K)
    1          B(J,I)=P(K)
       IJ = 0
       DO 600 I=1,NUMAT
           L=NFIRST(I)
           LL=NLAST(I)
           DO 602 J=1,I
               IJ = IJ + 1
               K=NFIRST(J)
               KK=NLAST(J)
               X=0.0
                   DO 601 IL=L,LL
                       DO 601 IH=K,KK
  601                 X=X+B(IL,IH)*B(IL,IH)
  602         BONDAB(IJ)=X
           X=-BONDAB(IJ)
           DO 603 J=L,LL
  603         X=X+2.D0*B(J,J)
           BONDAB(IJ)=X
  600     CONTINUE
       CALL VECPRT( BONDAB, NUMAT)
       RETURN
       END
