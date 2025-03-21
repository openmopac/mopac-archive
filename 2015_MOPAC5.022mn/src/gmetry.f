      SUBROUTINE GMETRY(GEO,COORD,*)                                    CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
     2       /EULER / TVEC(3,3), ID
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),COLCST
      COMMON /GEOOK/ IGEOOK
      COMMON /NUMCAL/ NUMCAL
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION GEO(3,*),COORD(3,*)
      CHARACTER *15 NDIMEN(4)
      LOGICAL GEOOK
CSAV         SAVE                                                           GL0892
      DATA NDIMEN/' MOLECULE     ',' POLYMER       ',
     1'LAYER STRUCTURE',' SOLID         '/
      DATA ICALCN /0/
C***********************************************************************
C
C    GMETRY  COMPUTES COORDINATES FROM BOND-ANGLES AND LENGTHS.
C *** IT IS ADAPTED FROM THE PROGRAM WRITTEN BY M.J.S. DEWAR.
C
C     THREE SEPARATE OPTIONS EXIST WITHIN GMETRY. THESE ARE:
C    (A) IF NA(1) IS EQUAL TO 99 (IMPOSSIBLE UNDER NORMAL CIRCUMSTANCES)
C        THEN GEO IS ASSUMED TO BE IN CARTESIAN RATHER THAN INTERNAL
C        COORDINATES, AND COORD IS THEN SET EQUAL TO GEO.
C    (B) IF STEP IS NON-ZERO (THIS IS THE CASE WHEN "SADDLE" IS USED)
C        THEN GEO IS FIRST MODIFIED BY SHIFTING THE INTERNAL COORDINATES
C        ALONG A RADIUS FROM GEOA TO PLACE GEO AT ADISTANCESTEPFROMGEOA.
C    (C) NORMAL CONVERSION FROM INTERNAL TO CARTESIAN COORDINATESISDONE.
C
C  ON INPUT:
C         GEO    = ARRAY OF INTERNAL COORDINATES.
C         NATOMS = NUMBER OF ATOMS, INCLUDING DUMMIES.
C         NA     = ARRAY OF ATOM LABELS FOR BOND LENGTHS.
C
C  ON OUTPUT:
C         COORD  = ARRAY OF CARTESIAN COORDINATES
C
C***********************************************************************
C                                     OPTION (B)
      GEOOK=(IGEOOK.EQ.99)
      IF(ABS(STEP) .GT. 1.D-4) THEN
         SUM=0.D0
         DO 10 I=1,NATOMS
            DO 10 J=1,3
               GEOVEC(J,I)=GEO(J,I)-GEOA(J,I)
   10    SUM=SUM+GEOVEC(J,I)**2
         SUM=SQRT(SUM)
         ERROR=(SUM-STEP)/SUM
      ELSE
         ERROR=0.D0
      ENDIF
      DO 20 I=1,NATOMS
         DO 20 J=1,3
   20 GEO(J,I)=GEO(J,I)-ERROR*GEOVEC(J,I)
C                                     OPTION (A)
      IF(NA(1).EQ.99) THEN
         DO 30 I=1,3
            DO 30 J=1,NATOMS
   30    COORD(I,J)=GEO(I,J)
         GOTO 110
      ENDIF
C                                     OPTION (C)
      COORD(1,1)=0.0D00
      COORD(2,1)=0.0D00
      COORD(3,1)=0.0D00
      COORD(1,2)=GEO(1,2)
      COORD(2,2)=0.0D00
      COORD(3,2)=0.0D00
      IF(NATOMS.EQ.2) GOTO 110
      CCOS=COS(GEO(2,3))
      IF(NA(3).EQ.1)THEN
         COORD(1,3)=COORD(1,1)+GEO(1,3)*CCOS
      ELSE
         COORD(1,3)=COORD(1,2)-GEO(1,3)*CCOS
      ENDIF
      COORD(2,3)=GEO(1,3)*SIN(GEO(2,3))
      COORD(3,3)=0.0D00
      DO 100 I=4,NATOMS
         COSA=COS(GEO(2,I))
         MB=NB(I)
         MC=NA(I)
         XB=COORD(1,MB)-COORD(1,MC)
         YB=COORD(2,MB)-COORD(2,MC)
         ZB=COORD(3,MB)-COORD(3,MC)
         RBC=1.0D00/SQRT(XB*XB+YB*YB+ZB*ZB)
         IF (ABS(COSA).LT.1.D0-1.D-12) GO TO 40
C
C     ATOMS MC, MB, AND (I) ARE COLLINEAR
C
         RBC=GEO(1,I)*RBC*COSA
         COORD(1,I)=COORD(1,MC)+XB*RBC
         COORD(2,I)=COORD(2,MC)+YB*RBC
         COORD(3,I)=COORD(3,MC)+ZB*RBC
         GO TO 100
C
C     THE ATOMS ARE NOT COLLINEAR
C
   40    MA=NC(I)
         XA=COORD(1,MA)-COORD(1,MC)
         YA=COORD(2,MA)-COORD(2,MC)
         ZA=COORD(3,MA)-COORD(3,MC)
C
C     ROTATE ABOUT THE Z-AXIS TO MAKE YB=0, AND XB POSITIVE.  IF XYB IS
C     TOO SMALL, FIRST ROTATE THE Y-AXIS BY 90 DEGREES.
C
         XYB=SQRT(XB*XB+YB*YB)
         K=-1
         IF (XYB.GT.0.1D00) GO TO 50
         XPA=ZA
         ZA=-XA
         XA=XPA
         XPB=ZB
         ZB=-XB
         XB=XPB
         XYB=SQRT(XB*XB+YB*YB)
         K=+1
C
C     ROTATE ABOUT THE Y-AXIS TO MAKE ZB VANISH
C
   50    COSTH=XB/XYB
         SINTH=YB/XYB
         XPA=XA*COSTH+YA*SINTH
         YPA=YA*COSTH-XA*SINTH
         SINPH=ZB*RBC
         COSPH=SQRT(ABS(1.D00-SINPH*SINPH))
         XQA=XPA*COSPH+ZA*SINPH
         ZQA=ZA*COSPH-XPA*SINPH
C
C     ROTATE ABOUT THE X-AXIS TO MAKE ZA=0, AND YA POSITIVE.
C
         YZA=SQRT(YPA**2+ZQA**2)
         IF(YZA.LT.1.D-4)GOTO 70
         IF(YZA.LT.2.D-2 .AND. .NOT.GEOOK)THEN
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//20X,'' CALCULATION ABANDONED AT THIS POINT'')')
            WRITE(6,'(//10X,'' THREE ATOMS BEING USED TO DEFINE THE'',/
     110X,'' COORDINATES OF A FOURTH ATOM, WHOSE BOND-ANGLE IS'')')
            WRITE(6,'(10X,'' NOT ZERO OR 180 DEGREEES, ARE '',
     1''IN AN ALMOST STRAIGHT'')')
            WRITE(6,'(10X,'' LINE.  THERE IS A HIGH PROBABILITY THAT THE
     1'',/10X,'' COORDINATES OF THE ATOM WILL BE INCORRECT.'')')
            WRITE(6,'(//20X,''THE FAULTY ATOM IS ATOM NUMBER'',I4)')I
            ENDIF                                                       CIO
            CALL GEOUT(*9999)                                           CSTP(call)
            IF (DOPRNT) THEN                                            CIO
            WRITE(6,'(//20X,''CARTESIAN COORDINATES UP TO FAULTY ATOM'')
     1')
            WRITE(6,'(//5X,''I'',12X,''X'',12X,''Y'',12X,''Z'')')
            ENDIF                                                       CIO
            IF (DOPRNT) THEN                                            CIO
            DO 60 J=1,I
   60       WRITE(6,'(I6,F16.5,2F13.5)')J,(COORD(K,J),K=1,3)
            WRITE(6,'(//6X,'' ATOMS'',I3,'','',I3,'', AND'',I3,
     1'' ARE WITHIN'',F7.4,'' ANGSTROMS OF A STRAIGHT LINE'')')
     2MC,MB,MA,YZA
            ENDIF                                                       CIO
      RETURN 1                                                          CSTP (stop)
         ENDIF
         COSKH=YPA/YZA
         SINKH=ZQA/YZA
         GOTO 80
   70    CONTINUE
C
C   ANGLE TOO SMALL TO BE IMPORTANT
C
         COSKH=1.D0
         SINKH=0.D0
   80    CONTINUE
C
C     COORDINATES :-   A=(XQA,YZA,0),   B=(RBC,0,0),  C=(0,0,0)
C     NONE ARE NEGATIVE.
C     THE COORDINATES OF I ARE EVALUATED IN THE NEW FRAME.
C
         SINA=SIN(GEO(2,I))
         SIND=-SIN(GEO(3,I))
         COSD=COS(GEO(3,I))
         XD=GEO(1,I)*COSA
         YD=GEO(1,I)*SINA*COSD
         ZD=GEO(1,I)*SINA*SIND
C
C     TRANSFORM THE COORDINATES BACK TO THE ORIGINAL SYSTEM.
C
         YPD=YD*COSKH-ZD*SINKH
         ZPD=ZD*COSKH+YD*SINKH
         XPD=XD*COSPH-ZPD*SINPH
         ZQD=ZPD*COSPH+XD*SINPH
         XQD=XPD*COSTH-YPD*SINTH
         YQD=YPD*COSTH+XPD*SINTH
         IF (K.LT.1) GO TO 90
         XRD=-ZQD
         ZQD=XQD
         XQD=XRD
   90    COORD(1,I)=XQD+COORD(1,MC)
         COORD(2,I)=YQD+COORD(2,MC)
         COORD(3,I)=ZQD+COORD(3,MC)
  100 CONTINUE
C
C *** NOW REMOVE THE TRANSLATION VECTORS, IF ANY, FROM THE ARRAY COOR
C
  110 CONTINUE
      K=NATOMS
  120 IF(LABELS(K).NE.107) GOTO 130
      K=K-1
      GOTO 120
  130 K=K+1
      IF(K.GT.NATOMS) GOTO 180
C
C   SYSTEM IS A SOLID, OF DIMENSION NATOMS+1-K
C
      L=0
      DO 140 I=K,NATOMS
         L=L+1
         MC=NA(I)
         DO 140 J=1,3
            TVEC(J,L)=COORD(J,I)-COORD(J,MC)
  140 CONTINUE
      ID=L
      IF (ICALCN .NE. NUMCAL) THEN
         ICALCN = NUMCAL
         IF (DOPRNT) WRITE(6,150)NDIMEN(ID+1)                           CIO
  150    FORMAT(/10X,'    THE SYSTEM IS A ',A15,/)
         IF(ID.EQ.0) GOTO 180
         IF (DOPRNT) WRITE(6,160)                                       CIO
         IF (DOPRNT) WRITE(6,170)(I,(TVEC(J,I),J=1,3),I=1,ID)           CIO
  160    FORMAT(/,'                UNIT CELL TRANSLATION VECTORS',/
     1/,'              X              Y              Z')
  170    FORMAT('    T',I1,' = ',F11.7,'    ',F11.7,'    ',F11.7)
      ENDIF
  180 CONTINUE
      J=0
      DO 200 I=1,NATOMS
         IF (LABELS(I).EQ.99.OR.LABELS(I).EQ.107) GO TO 200
         J=J+1
         DO 190 K=1,3
  190    COORD(K,J)=COORD(K,I)
  200 CONTINUE
      CUTOFF=200.D0
      RETURN
 9999 RETURN 1                                                          CSTP
      END
