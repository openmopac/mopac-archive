      SUBROUTINE FOCK1(F, PTOT, PA, PB,*)                               CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /NUMCAL/ NUMCAL
      DIMENSION F(*), PTOT(*), PA(*), PB(*)
C *********************************************************************
C
C *** COMPUTE THE REMAINING CONTRIBUTIONS TO THE ONE-CENTRE ELEMENTS.
C
C *********************************************************************
c Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
      COMMON /MOLKSR/ FRACT
      COMMON /GAUSSB/ FN1(120),FN2(120)
     1       /MOLORB/ USPD(MAXORB),DUMY(MAXORB)
      COMMON /TWOELE/ GSS(120),GSP(120),GPP(120),GP2(120),HSP(120)
     1                ,GSD(120),GPD(120),GDD(120)
      DIMENSION QTOT(NUMATM), QA(NUMATM), QB(NUMATM)
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*160 KEYWRD
CSAV         SAVE                                                           GL0892
      DATA ICALCN /0/
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
      ENDIF
      CALL CHRGE(PTOT,QTOT,*9999)                                        CSTP(call)
      CALL CHRGE(PA,QA,*9999)                                            CSTP(call)
      DO 10 I=1,NUMAT
   10 QB(I)=QTOT(I)-QA(I)
      DO 100 II=1,NUMAT
         IA=NFIRST(II)
         IB=NMIDLE(II)
         IC=NLAST(II)
         NI=NAT(II)
         DTPOP=0.D0
         DAPOP=0.D0
         PTPOP=0.D0
         PAPOP=0.D0
         GOTO (100,40,30,30,30,20,20,20,20,20)IC-IA+2
   20    DTPOP=PTOT((IC*(IC+1))/2)+PTOT(((IC-1)*(IC))/2)
     1        +PTOT(((IC-2)*(IC-1))/2)+PTOT(((IC-3)*(IC-2))/2)
     2        +PTOT(((IC-4)*(IC-3))/2)
         DAPOP=PA((IC*(IC+1))/2)+PA(((IC-1)*(IC))/2)
     1        +PA(((IC-2)*(IC-1))/2)+PA(((IC-3)*(IC-2))/2)
     2        +PA(((IC-4)*(IC-3))/2)
   30    PTPOP=PTOT((IB*(IB+1))/2)+PTOT(((IB-1)*(IB))/2)
     1        +PTOT(((IB-2)*(IB-1))/2)
         PAPOP=PA((IB*(IB+1))/2)+PA(((IB-1)*(IB))/2)
     1        +PA(((IB-2)*(IB-1))/2)
   40    DBPOP=DTPOP-DAPOP
         PBPOP=PTPOP-PAPOP
         IF(NI.EQ.1)THEN
            SUM=0.D0
         ELSE
            SUM2=0.D0
            SUM1=0.D0
            DO 60 I=IA,IB
               IM1=I-1
               DO 50 J=IA,IM1
   50          SUM1=SUM1+PTOT(J+(I*(I-1))/2)**2
   60       SUM2=SUM2+PTOT((I*(I+1))/2)**2
            SUM=SUM1*2.D0+SUM2
            SUM=SQRT(SUM)-QTOT(II)*0.5D0
         ENDIF
         SUM=SUM*FN1(NI)
C
C     F(S,S)
C
         KA=(IA*(IA+1))/2
         F(KA)=F(KA)+PB(KA)*GSS(NI)+PTPOP*GSP(NI)
     1         -PAPOP*HSP(NI) + DTPOP*GSD(NI)
         IF (NI.LT.3) GO TO 100
         IPLUS=IA+1
         L=KA
         DO 70 J=IPLUS,IB
            M=L+IA
            L=L+J
C
C     F(P,P)
C
            F(L)=F(L)+PTOT(KA)*GSP(NI)-PA(KA)*HSP(NI)+
     1      PB(L)*GPP(NI)+(PTPOP-PTOT(L))*GP2(NI)
     2      -0.5D0*(PAPOP-PA(L))*(GPP(NI)-GP2(NI))
     3      +DTPOP*GPD(NI)
C
C     F(S,P)
C
   70    F(M)=F(M)+2.D0*PTOT(M)*HSP(NI)-PA(M)*(HSP(NI)+GSP(NI))
C
C     F(P,P*)
C
         IMINUS=IB-1
         DO 80 J=IPLUS,IMINUS
            ICC=J+1
            DO 80 L=ICC,IB
               M=(L*(L-1))/2+J
   80    F(M)=F(M)+PTOT(M)*(GPP(NI)-GP2(NI))
     1      -0.5D0*PA  (M)*(GPP(NI)+GP2(NI))
         DO 90 J=IB+1,IC
            M=(J*(J+1))/2
   90    F(M)=F(M)+PTOT(KA)*GSD(NI)
     1         +PTPOP*GPD(NI)
     2         +(DTPOP-PA(M))*GDD(NI)
  100 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
