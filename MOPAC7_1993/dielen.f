      SUBROUTINE DIELEN (EDIE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON / SOLV / FEPSI,RDS,DISEX2,NSPA,NPS,NPS2,NDEN,
     1                COSURF(3,LENABC), SRAD(NUMATM),ABCMAT(LENAB2),
     2                TM(3,3,NUMATM),QDEN(MAXDEN),DIRTM(3,NPPA),
     3                BH(LENABC)
      CALL CQDEN()
      EDIE=0.D0
      I0=NPS2+NDEN*NPS
      DO 20 I=1,NDEN
         QI=QDEN(I)
         DO 10 J=1,I-1
            I0=I0+1
   10    EDIE=EDIE+2*QI*ABCMAT(I0)*QDEN(J)
         I0=I0+1
         EDIE=EDIE+QI*ABCMAT(I0)*QI
   20 CONTINUE
      RETURN
      END