      SUBROUTINE VECPRT (A,NUMM,*)                                      CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION  A(*)
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /ELEMTS/ ELEMNT(120)                                       LF0210
      COMMON /HPUSED/ HPUSED
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
C**********************************************************************
C
C  VECPRT PRINTS A LOWER-HALF TRIANGLE OF A SQUARE MATRIX, THE
C         LOWER-HALF TRIANGLE BEING STORED IN PACKED FORM IN THE
C         ARRAY "A"
C
C ON INPUT:
C      A      = ARRAY TO BE PRINTED
C      NUMM   = SIZE OF ARRAY TO BE PRINTED
C(REF) NUMAT  = NUMBER OF ATOMS IN THE MOLECULE (THIS IS NEEDED TO
C               DECIDE IF AN ATOMIC ARRAY OR ATOMIC ORBITAL ARRAY IS
C               TO BE PRINTED
C(REF) NAT    = LIST OF ATOMIC NUMBERS
C(REF) NFIRST = LIST OF ORBITAL COUNTERS
C(REF) NLAST  = LIST OF ORBITAL COUNTERS
C
C  NONE OF THE ARGUMENTS ARE ALTERED BY THE CALL OF VECPRT
C
C*********************************************************************
      DIMENSION NATOM(MAXORB)
      CHARACTER * 6 LINE(21)
      CHARACTER*2 ELEMNT,ATORBS(9), ITEXT(MAXORB), JTEXT(MAXORB)
      LOGICAL HPUSED                                                    LF0210
CSAV         SAVE                                                           GL0892
      DATA ATORBS/' S','PX','PY','PZ','X2','XZ','Z2','YZ','XY'/
      IF(NUMAT.NE.0.AND.NUMAT.EQ.NUMM) THEN
C
C    PRINT OVER ATOM COUNT
C
         DO 10 I=1,NUMAT
            ITEXT(I)='  '
            JTEXT(I)=ELEMNT(NAT(I))
            IF (HPUSED.AND.NAT(I).EQ.9) JTEXT(I)='Hp'                   LF0210
            NATOM(I)=I
   10    CONTINUE
      ELSE
         IF (NUMAT.NE.0.AND.NLAST(NUMAT) .EQ. NUMM) THEN
            DO 30 I=1,NUMAT
               JLO=NFIRST(I)
               JHI=NLAST(I)
               L=NAT(I)
               K=0
               DO 20 J=JLO,JHI
                  K=K+1
                  ITEXT(J)=ATORBS(K)
                  JTEXT(J)=ELEMNT(L)
                  IF (HPUSED.AND.NAT(I).EQ.9) JTEXT(I)='Hp'             LF0210
                  NATOM(J)=I
   20          CONTINUE
   30       CONTINUE
         ELSE
            NUMB=ABS(NUMM)
            DO 40 I=1,NUMB
               ITEXT(I) = '  '
               JTEXT(I) = '  '
   40       NATOM(I)=I
         ENDIF
      ENDIF
      NUMB=ABS(NUMM)
      DO 50 I=1,21
   50 LINE(I)='------'
      LIMIT=(NUMB*(NUMB+1))/2
      KK=8
      NA=1
   60 LL=0
      M=MIN0((NUMB+1-NA),6)
      MA=2*M+1
      M=NA+M-1
      IF (DOPRNT) WRITE(6,100)(ITEXT(I),JTEXT(I),NATOM(I),I=NA,M)       CIO
      IF (DOPRNT) WRITE (6,110) (LINE(K),K=1,MA)                        CIO
      DO 80 I=NA,NUMB
         LL=LL+1
         K=(I*(I-1))/2
         L=MIN0((K+M),(K+I))
         K=K+NA
         IF ((KK+LL).LE.50) GO TO 70
         IF (DOPRNT) WRITE (6,120)                                      CIO
         IF (DOPRNT) WRITE (6,100) (ITEXT(N),JTEXT(N),NATOM(N),N=NA,M)  CIO
         IF (DOPRNT) WRITE (6,110) (LINE(N),N=1,MA)                     CIO
         KK=4
         LL=0
   70    IF(DOPRNT)WRITE(6,130) ITEXT(I),JTEXT(I),NATOM(I),(A(N),N=K,L) CIO
   80 CONTINUE
      IF (L.GE.LIMIT) GO TO 90
      KK=KK+LL+4
      NA=M+1
      IF ((KK+NUMB+1-NA).LE.50) GO TO 60
      KK=4
      IF (DOPRNT) WRITE (6,120)                                         CIO
      GO TO 60
   90 RETURN
C
  100 FORMAT (1H /13X,10(1X,A2,1X,A2,I3,2X))
  110 FORMAT (1H ,21A6)
  120 FORMAT (1H1)
  130 FORMAT (1H ,A2,1X,A2,I5,10F11.6)
C
 9999 RETURN 1                                                          CSTP
      END
