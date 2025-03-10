      SUBROUTINE REACT1(ESCF) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'KEYS.i'                                                 DJG0995
       INCLUDE 'FFILES.i'                                               GDH1095
      COMMON /GEOM  / GEO(3,NUMATM) 
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM), 
     1                NA(NUMATM), NB(NUMATM), NC(NUMATM) 
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK) 
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR) 
     1       /MOLORB/ USPD(MAXORB),PSPD(MAXORB) 
C     COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR) 
      COMMON /GEOVAR/ XPARAM(MAXPAR),NVAR,LOC(2,MAXPAR),IDUMY           3GL3092
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM 
      COMMON /REACTN/ STEP, GEOA, GEOVEC, CALCST 
      COMMON /MLKSTI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),NMIDLE(NUMATM),  03GCL93
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,        03GCL93
     2                NCLOSE,NOPEN,NDUMY                                03GCL93
      DIMENSION GEOA(3,NUMATM), GEOVEC(3,NUMATM), 
     1          P1STOR(MPACK), P2STOR(MPACK), 
     2          P3STOR(MPACK), XOLD(MAXPAR), GROLD(MAXPAR) 
      LOGICAL GRADNT, FINISH, XYZ, INT, FAIL                            03GCL93
************************************************************************ 
* 
*  REACT1 DETERMINES THE TRANSITION STATE OF A CHEMICAL REACTION. 
* 
*   REACT WORKS BY USING TWO SYSTEMS SIMULTANEOUSLY, THE HEATS OF 
*   FORMATION OF BOTH ARE CALCULATED, THEN THE MORE STABLE ONE 
*   IS MOVED IN THE DIRECTION OF THE OTHER. AFTER A STEP THE 
*   ENERGIES ARE COMPARED, AND THE NOW LOWER-ENERGY FORM IS MOVED 
*   IN THE DIRECTION OF THE HIGHER-ENERGY FORM. THIS IS REPEATED 
*   UNTIL THE SADDLE POINT IS REACHED. 
* 
*   IF ONE FORM IS MOVED 3 TIMES IN SUCCESSION, THEN THE HIGHER ENERGY 
*   FORM IS RE-OPTIMIZED WITHOUT SHORTENING THE DISTANCE BETWEEN THE TWO 
*   FORMS. THIS REDUCES THE CHANCE OF BEING CAUGHT ON THE SIDE OF A 
*   TRANSITION STATE. 
* 
************************************************************************ 
C     VARIABLE IROT CHANGED TO IROTA TO AVOID CONFLICT WITH KEYWORD FLAGDJG0995
      DIMENSION IDUM1(NUMATM), IDUM2(3,NUMATM), XSTORE(MAXPAR), 
     1DUMY(NUMATM), COORD(3,NUMATM), IROTA(2,3)                         DJG0995
      SAVE
      DATA IROTA/1,2,1,3,2,3/                                           DJG0995
      XYZ=(IXYZ.NE.0)                                                   DJG0995
      GRADNT=(IGRADI.NE.0)                                              DJG0995
      STEPMX=0.15D0 
      IF(IBAR.NE.0) STEPMX=FIBAR                                        DJG0995
      MAXSTP=30 
C 
C    READ IN THE SECOND GEOMETRY. 
C 
      IF(XYZ) THEN 
         CALL GETGEO(JDAT,LABELS,GEOA,LOC,NA,NB,NC,DUMY,NATOMS,INT) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ELSE 
         CALL GETGEO(JDAT,IDUM1,GEOA,IDUM2, 
     1         IDUM1,IDUM1,IDUM1,DUMY,NATOMS,INT) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      ENDIF 
      CLOSE (JDAT) 
      CALL PORCPU (TIME0)                                               GL0492
C 
C  SWAP FIRST AND SECOND GEOMETRIES AROUND 
C  SO THAT GEOUT CAN OUTPUT DATA ON SECOND GEOMETRY. 
C 
      NUMAT=0 
      DO 10 I=1,NATOMS 
         IF(LABELS(I).NE.99) NUMAT=NUMAT+1 
         CONST=1.D0 
         DO 10 J=1,3 
            X=GEOA(J,I)*CONST 
            CONST=0.0174532925D0 
            GEOA(J,I)=GEO(J,I) 
            GEO(J,I)=X 
   10 CONTINUE 
      WRITE(JOUT,'(//10X,'' GEOMETRY OF SECOND SYSTEM'',/)') 
      CALL GEOUT 
C 
C     CONVERT TO CARTESIAN, IF NECESSARY 
C 
      IF(   XYZ   )THEN 
         CALL GMETRY(GEO,COORD) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         SUMX=0.D0 
         SUMY=0.D0 
         SUMZ=0.D0 
         DO 20 J=1,NUMAT 
            SUMX=SUMX+COORD(1,J) 
            SUMY=SUMY+COORD(2,J) 
   20    SUMZ=SUMZ+COORD(3,J) 
         SUMX=SUMX/NUMAT 
         SUMY=SUMY/NUMAT 
         SUMZ=SUMZ/NUMAT 
         DO 30 J=1,NUMAT 
            GEO(1,J)=COORD(1,J)-SUMX 
            GEO(2,J)=COORD(2,J)-SUMY 
   30    GEO(3,J)=COORD(3,J)-SUMZ 
         WRITE(JOUT
     .    ,'(//,''  CARTESIAN GEOMETRY OF FIRST SYSTEM'',//)') 
         WRITE(JOUT,'(3F14.5)')((GEO(J,I),J=1,3),I=1,NUMAT) 
         SUM=0.D0 
         SUMX=0.D0 
         SUMY=0.D0 
         SUMZ=0.D0 
         DO 40 J=1,NUMAT 
            SUM=SUM+(GEO(1,J)-GEOA(1,J))**2 
     1           +(GEO(2,J)-GEOA(2,J))**2 
     2           +(GEO(3,J)-GEOA(3,J))**2 
            SUMX=SUMX+GEOA(1,J) 
            SUMY=SUMY+GEOA(2,J) 
   40    SUMZ=SUMZ+GEOA(3,J) 
         SUM=0.D0 
         SUMX=SUMX/NUMAT 
         SUMY=SUMY/NUMAT 
         SUMZ=SUMZ/NUMAT 
         DO 50 J=1,NUMAT 
            GEOA(1,J)=GEOA(1,J)-SUMX 
            GEOA(2,J)=GEOA(2,J)-SUMY 
            GEOA(3,J)=GEOA(3,J)-SUMZ 
            SUM=SUM+(GEO(1,J)-GEOA(1,J))**2 
     1           +(GEO(2,J)-GEOA(2,J))**2 
     2           +(GEO(3,J)-GEOA(3,J))**2 
   50    CONTINUE 
         DO 100 L=3,1,-1 
C 
C     DOCKING IS DONE IN STEPS OF 16, 4, AND 1 DEGREES AT A TIME. 
C 
            CA=COS(4.D0**(L-1)*0.01745329D0) 
            SA=SQRT(ABS(1.D0-CA**2)) 
            DO 90 J=1,3 
               IR=IROTA(1,J) 
               JR=IROTA(2,J) 
               DO 80 I=1,10 
                  SUMM=0.D0 
                  DO 60 K=1,NUMAT 
                     X         = CA*GEOA(IR,K)+SA*GEOA(JR,K) 
                     GEOA(JR,K)=-SA*GEOA(IR,K)+CA*GEOA(JR,K) 
                     GEOA(IR,K)=X 
                     SUMM=SUMM+(GEO(1,K)-GEOA(1,K))**2 
     1                         +(GEO(2,K)-GEOA(2,K))**2 
     2                         +(GEO(3,K)-GEOA(3,K))**2 
   60             CONTINUE 
                  IF(SUMM.GT.SUM) THEN 
                     IF(I.GT.1)THEN 
                        SA=-SA 
                        DO 70 K=1,NUMAT 
                           X         = CA*GEOA(IR,K)+SA*GEOA(JR,K) 
                           GEOA(JR,K)=-SA*GEOA(IR,K)+CA*GEOA(JR,K) 
                           GEOA(IR,K)=X 
   70                   CONTINUE 
                        GOTO 90 
                     ENDIF 
                     SA=-SA 
                  ENDIF 
   80          SUM=SUMM 
   90       CONTINUE 
  100    CONTINUE 
         WRITE(JOUT,
     .     '(//,''  CARTESIAN GEOMETRY OF SECOND SYSTEM'',//)') 
         WRITE(JOUT,'(3F14.5)')((GEOA(J,I),J=1,3),I=1,NUMAT) 
         WRITE(JOUT,'(//,''   "DISTANCE":'',F13.6)')SUM 
         WRITE(JOUT,'(//,''  REACTION COORDINATE VECTOR'',//)') 
         WRITE(JOUT,'(3F14.5)')((GEOA(J,I)-GEO(J,I),J=1,3),I=1,NUMAT) 
         NA(1)=99 
         J=0 
         NVAR=0 
         DO 120 I=1,NATOMS 
            IF(LABELS(I).NE.99)THEN 
               J=J+1 
               DO 110 K=1,3 
                  NVAR=NVAR+1 
                  LOC(2,NVAR)=K 
  110          LOC(1,NVAR)=J 
               LABELS(J)=LABELS(I) 
            ENDIF 
            NATOMS=NUMAT 
  120    CONTINUE 
      ENDIF 
C 
C   XPARAM HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEO 
C   XOLD   HOLDS THE VARIABLE PARAMETERS FOR GEOMETRY IN GEOA 
C 
      SUM=0.D0 
      DO 130 I=1,NVAR 
         GROLD(I)=1.D0 
         XPARAM(I)=GEO(LOC(2,I),LOC(1,I)) 
         XOLD(I)=GEOA(LOC(2,I),LOC(1,I)) 
  130 SUM=SUM+(XPARAM(I)-XOLD(I))**2 
      STEP0=SQRT(SUM) 
      GRNOLD=SQRT(DBLE(NVAR))                                           GCL0393
      ONE=1.D0 
      DELL=0.1D0 
      EOLD=-2000.D0 
      CALL PORCPU (TIME1)                                               GL0492
      SWAP=0 
      DO 140 I=1,NORBS 
         J=(I*(I+1))/2 
         P1STOR(J)=PSPD(I) 
         P2STOR(J)=PSPD(I)*0.5D0 
  140 P3STOR(J)=PSPD(I)*0.5D0 
      DO 230 ILOOP=1,MAXSTP 
         CALL PORCPU (TIME2)                                            GL0492
         WRITE(JOUT,'('' TIME='',F9.2)')TIME2-TIME1 
         TIME1=TIME2 
C 
C   THIS METHOD OF CALCULATING 'STEP' IS QUITE ARBITARY, AND NEEDS 
C   TO BE IMPROVED BY INTELLIGENT GUESSWORK] 
C 
         IF (GNORM.LT.1.D-3)GNORM=1.D-3 
         WRITE(JOUT,'('' CURRENT BAR, STEPMX, GNORM'',3F12.7)') 
     1STEP0,STEPMX,GNORM 
         STEP=MIN(SWAP,0.5D0, 6.D0/GNORM, DELL,STEPMX*STEP0+0.005D0) 
         SWAP=SWAP+1.D0 
         DELL=DELL+0.1 
         STEP0=STEP0-STEP 
         IF(STEP0.LT.0.01D0) GOTO 240 
         STEP=STEP0 
         DO 150 I=1,NVAR 
  150    XSTORE(I)=XPARAM(I) 
         CALL FLEPO(XPARAM, NVAR, ESCF) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
         DO 160 I=1,NVAR 
  160    XPARAM(I)=GEO(LOC(2,I),LOC(1,I)) 
         WRITE(JOUT,'(//10X,''FOR POINT'',I3)')ILOOP 
         WRITE(JOUT,'('' DISTANCE A - B  '',F12.6)')STEP 
C 
C   NOW TO CALCULATE THE "CORRECT" GRADIENTS, SWITCH OFF 'STEP'. 
C 
         STEP=0.D0 
         CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (FAIL) THEN                                                    GDH1095
      ISTOP=1                                                           GDH1095
      IWHERE=26                                                         GDH1095
      RETURN                                                            GDH1095
      ENDIF                                                             GDH1095
         GNORM=0.D0 
         COSINE=0.D0 
         DO 180 I=1,NVAR 
         GNORM=GNORM+GRAD(I)*GRAD(I) 
         COSINE=COSINE+GRAD(I)*GROLD(I) 
  180    GROLD(I)=GRAD(I) 
         GNORM=SQRT(GNORM) 
         COSINE=COSINE/(GNORM*GRNOLD) 
         GRNOLD=GNORM 
         IF (GRADNT) THEN 
            WRITE(JOUT,'(''  ACTUAL GRADIENTS OF THIS POINT'')') 
            WRITE(JOUT,'(8F10.4)')(GRAD(I),I=1,NVAR) 
         ENDIF 
         WRITE(JOUT,'('' HEAT            '',F12.6)')FUNCT1 
         GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
         WRITE(JOUT,'('' GRADIENT NORM   '',F12.6)')GNORM 
         COSINE=COSINE*ONE 
         WRITE(JOUT,'('' DIRECTION COSINE'',F12.6)')COSINE 
         CALL GEOUT 
         IF(SWAP.GT.2.9D0 .OR. ILOOP .GT. 3 .AND. COSINE .LT. 0.D0 
     1  .OR. ESCF .GT. EOLD) 
     2  THEN 
            IF(SWAP.GT.2.9D0)THEN 
               SWAP=0.D0 
            ELSE 
               SWAP=0.5D0 
            ENDIF 
C 
C   SWAP REACTANT AND PRODUCT AROUND 
C 
            FINISH=(ILOOP .GT. 3 .AND. COSINE .LT. 0.D0) 
            IF(FINISH) THEN 
           WRITE(JOUT,'(//10X,'' BOTH SYSTEMS ARE ON THE SAME SIDE OF T 
     1HE '',''TRANSITION STATE -'',/10X,'' GEOMETRIES OF THE SYSTEMS'', 
     2'' ON EACH SIDE OF THE T.S. ARE AS FOLLOWS'')') 
               DO 190 I=1,NVAR 
  190          XPARAM(I)=XSTORE(I) 
               CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
               WRITE(JOUT,
     .    '(//10X,'' GEOMETRY ON ONE SIDE OF THE TRANSITION 
     1'','' STATE'')') 
               CALL WRITES(TIME0,FUNCT1)                                GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
               IF(FAIL) THEN
                 ISTOP=1
                 IWHERE=28
                 RETURN
               ENDIF       
            ENDIF 
            WRITE(JOUT,'(''  REACTANTS AND PRODUCTS SWAPPED AROUND'')') 
            ONE=-1.D0 
            EOLD=ESCF 
            SUM=GOLD 
            GOLD=GNORM 
            GNORM=SUM 
            DO 200 I=1,NATOMS 
               DO 200 J=1,3 
                  X=GEO(J,I) 
                  GEO(J,I)=GEOA(J,I) 
  200       GEOA(J,I)=X 
            DO 210 I=1,NVAR 
               X=XOLD(I) 
               XOLD(I)=XPARAM(I) 
  210       XPARAM(I)=X 
C 
C    I'VE NOT HAD TIME TO WORK OUT THE CORRECT SIZE OFTHEDENSITYMATRICES 
C    SO 6000 IS AN ARBITARY LARGE NUMBER.  THIS SHOULD BE FIXED. 
C 
C    SWAP AROUND THE DENSITY MATRICES. 
C 
            DO 220 I=1,MPACK 
               X=P1STOR(I) 
               P1STOR(I)=P(I) 
               P(I)=X 
               X=P2STOR(I) 
               P2STOR(I)=PA(I) 
               PA(I)=X 
               X=P3STOR(I) 
               P3STOR(I)=PB(I) 
               PB(I)=X 
  220       CONTINUE 
            IF(FINISH) GOTO 240 
         ELSE 
            ONE=1.D0 
         ENDIF 
  230 CONTINUE 
  240 CONTINUE 
      WRITE(JOUT,'('' AT END OF REACTION'')') 
      GOLD=SQRT(DOT(GRAD,GRAD,NVAR)) 
      CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      GNORM=SQRT(DOT(GRAD,GRAD,NVAR)) 
      CALL WRITES(TIME0,FUNCT1)                                         GL0492
      IF (ISTOP.NE.0) RETURN                                            GDH1095
               IF(FAIL) THEN
                 ISTOP=1
                 IWHERE=29
                 RETURN
               ENDIF
* 
* THE GEOMETRIES HAVE (A) BEEN OPTIMISED CORRECTLY, OR 
*                     (B) BOTH ENDED UP ON THE SAME SIDE OF THE T.S. 
* 
*  TRANSITION STATE LIES BETWEEN THE TWO GEOMETRIES 
* 
      C1=GOLD/(GOLD+GNORM) 
      C2=1.D0-C1 
      WRITE(JOUT,
     .      '('' BEST ESTIMATE GEOMETRY OF THE TRANSITION STATE'')') 
      WRITE(JOUT,'(//10X,'' C1='',F8.3,''C2='',F8.3)')C1,C2 
      DO 250 I=1,NVAR 
  250 XPARAM(I)=C1*XPARAM(I)+C2*XOLD(I) 
      CALL COMPFG (XPARAM,FUNCT1,FAIL,GRAD,.TRUE.) 
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      CALL WRITES(TIME0,FUNCT1)                                         GL0492
      RETURN
      END 
