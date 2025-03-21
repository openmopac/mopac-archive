      FUNCTION BABBBC(IOCCA1, IOCCB1, IOCCA2, IOCCB2, NMOS)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
      DIMENSION IOCCA1(NMOS), IOCCB1(NMOS), IOCCA2(NMOS), IOCCB2(NMOS)
***********************************************************************
*
* BABBBC EVALUATES THE C.I. MATRIX ELEMENT FOR TWO MICROSTATES DIFFERING
*       BY ONE BETA ELECTRON. THAT IS, ONE MICROSTATE HAS A BETA
*       ELECTRON IN PSI(I) AND THE OTHER MICROSTATE HAS AN ELECTRON IN
*       PSI(J).
***********************************************************************
      COMMON /XYIJKL/ XY(NMECI,NMECI,NMECI,NMECI)
      COMMON /BASEOC/ OCCA(NMECI),KDUM(NMECI+2)                         JZ0315
CSAV         SAVE                                                           GL0892
      DO 10 I=1,NMOS
   10 IF(IOCCB1(I).NE.IOCCB2(I)) GOTO 20
   20 IJ=0
      DO 30 J=I+1,NMOS
         IF(IOCCB1(J).NE.IOCCB2(J)) GOTO 40
   30 IJ=IJ+IOCCA1(J)+IOCCB1(J)
   40 IJ=IJ+IOCCA1(J)
C
C   THE UNPAIRED M.O.S ARE I AND J
      SUM=0.D0
      DO 50 K=1,NMOS
   50 SUM=SUM+ (XY(I,J,K,K)-XY(I,K,J,K))*(IOCCB1(K)-OCCA(K)) +
     1          XY(I,J,K,K)             *(IOCCA1(K)-OCCA(K))
      BABBBC=SUM*((-1)**(IJ-(IJ/2)*2))
      RETURN
      END
