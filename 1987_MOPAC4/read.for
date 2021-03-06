      SUBROUTINE READ
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)
      INCLUDE 'SIZES'
C
C MODULE TO READ IN GEOMETRY FILE, OUTPUT IT TO THE USER,
C AND CHECK THE DATA TO SEE IF IT IS REASONABLE.
C EXIT IF NECESSARY.
C
C
C
C  ON EXIT NATOMS    = NUMBER OF ATOMS PLUS DUMMY ATOMS (IF ANY).
C          KEYWRD    = KEYWORDS TO CONTROL CALCULATION
C          KOMENT    = COMMENT CARD
C          TITLE     = TITLE CARD
C          LABELS    = ARRAY OF ATOMIC LABELS INCLUDING DUMMY ATOMS.
C          GEO       = ARRAY OF INTERNAL COORDINATES.
C          LOPT      = FLAGS FOR OPTIMIZATION OF MOLECULE
C          NA        = ARRAY OF LABELS OF ATOMS, BOND LENGTHS.
C          NB        = ARRAY OF LABELS OF ATOMS, BOND ANGLES.
C          NC        = ARRAY OF LABELS OF ATOMS, DIHEDRAL ANGLES.
C          LATOM     = LABEL OF ATOM OF REACTION COORDINATE.
C          LPARAM    = RC: 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL
C          REACT(200)= REACTION COORDINATE PARAMETERS
C          LOC(1,I)  = LABEL OF ATOM TO BE OPTIMIZED.
C          LOC(2,I)  = 1 FOR LENGTH, 2 FOR ANGLE, AND 3 FOR DIHEDRAL.
C          NVAR      = NUMBER OF PARAMETERS TO BE OPTIMIZED.
C          XPARAM    = STARTING VALUE OF PARAMETERS TO BE OPTIMIZED.
C
************************************************************************
C *** INPUT THE TRIAL GEOMETRY  \IE.  KGEOM=0\
C   LABEL(I) = THE ATOMIC NUMBER OF ATOM\I\.
C            = 99, THEN THE I-TH ATOM IS A DUMMY ATOM USED ONLY TO
C              SIMPLIFY THE DEFINITION OF THE MOLECULAR GEOMETRY.
C   GEO(1,I) = THE INTERNUCLEAR SEPARATION \IN ANGSTROMS\ BETWEEN ATOMS
C              NA(I) AND (I).
C   GEO(2,I) = THE ANGLE NB(I):NA(I):(I) INPUT IN DEGREES; STORED IN
C              RADIANS.
C   GEO(3,I) = THE ANGLE BETWEEN THE VECTORS NC(I):NB(I) AND NA(I):(I)
C              INPUT IN DEGREES - STORED IN RADIANS.
C  LOPT(J,I) = -1 IF GEO(J,I) IS THE REACTION COORDINATE.
C            = +1 IF GEO(J,I) IS A PARAMETER TO BE OPTIMIZED
C            =  0 OTHERWISE.
C *** NOTE:    MUCH OF THIS DATA IS NOT INCLUDED FOR THE FIRST 3 ATOMS.
C     ATOM1  INPUT LABELS(1) ONLY.
C     ATOM2  INPUT LABELS(2) AND GEO(1,2) SEPARATION BETWEEN ATOMS 1+2
C     ATOM3  INPUT LABELS(3), GEO(1,3)    SEPARATION BETWEEN ATOMS 2+3
C              AND GEO(2,3)              ANGLE ATOM1 : ATOM2 : ATOM3
C
************************************************************************
C
      DIMENSION LOPT(3,NUMATM)
      CHARACTER*80 KEYWRD,KOMENT,TITLE,LINE
      CHARACTER KEYS(80)*1, SPACE*1, SPACE2*2, CH*1, CH2*2
      CHARACTER ELEMNT*2
      COMMON /KEYWRD/ KEYWRD
      COMMON /TITLES/ KOMENT,TITLE
      COMMON /GEOVAR/ NVAR, LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /PATH  / LATOM,LPARAM,REACT(200)
      COMMON /MESH  / LATOM1, LPARA1, LATOM2, LPARA2
      COMMON /ELEMTS/ ELEMNT(107)
      COMMON /ISTOPE/ AMS(107)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(MAXPAR), IDEPFN(MAXPAR),
     1                      LOCDEP(MAXPAR)
      LOGICAL INT
      DIMENSION COORD(3,NUMATM),VALUE(40)
      EQUIVALENCE (KEYS(1),KEYWRD)
      DATA SPACE, SPACE2/' ','  '/
C
      READ(5,'(A)')KEYWRD,KOMENT,TITLE
      ILOWA = ICHAR('a')
      ILOWZ = ICHAR('z')
      ICAPA = ICHAR('A')
************************************************************************
      DO 10 I=1,80
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            KEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   10 CONTINUE
************************************************************************
      IF(INDEX(KEYWRD,'ECHO').NE.0)THEN
         REWIND 5
         DO 30 I=1,1000
            READ(5,'(A)',END=40)KEYWRD
            DO 20 J=80,2,-1
   20       IF(KEYWRD(J:J).NE.' ')GOTO 30
            J=1
   30    WRITE(6,'(1X,A)')KEYWRD(1:J)
      ENDIF
   40 REWIND 5
      IF(INDEX(KEYWRD,'ECHO').NE.0)WRITE(6,'(''1'')')
      READ(5,'(A)')KEYWRD,KOMENT,TITLE
************************************************************************
      DO 50 I=1,80
         ILINE=ICHAR(KEYWRD(I:I))
         IF(ILINE.GE.ILOWA.AND.ILINE.LE.ILOWZ) THEN
            KEYWRD(I:I)=CHAR(ILINE+ICAPA-ILOWA)
         ENDIF
   50 CONTINUE
************************************************************************
      IF(KEYWRD(1:1) .NE. SPACE) THEN
         CH=KEYWRD(1:1)
         KEYWRD(1:1)=SPACE
         DO 60 I=2,78
            CH2=KEYWRD(I:I)
            KEYWRD(I:I)=CH
            CH=CH2
            IF(KEYWRD(I+1:I+2) .EQ. SPACE2) THEN
               KEYWRD(I+1:I+1)=CH
               GOTO 70
            ENDIF
   60    CONTINUE
         IF(KEYWRD(80:80) .NE. SPACE)
     1WRITE(6,'(//10X,''WARNING-THE LAST KEYWORD MAY BE CORRUPTED'')')
         KEYWRD(80:80)=CH
   70    CONTINUE
      ENDIF
      IF(KOMENT(1:1) .NE. SPACE) THEN
         CH=KOMENT(1:1)
         KOMENT(1:1)=SPACE
         DO 80 I=2,78
            CH2=KOMENT(I:I)
            KOMENT(I:I)=CH
            CH=CH2
            IF(KOMENT(I+1:I+2) .EQ. SPACE2) THEN
               KOMENT(I+1:I+1)=CH
               GOTO 90
            ENDIF
   80    CONTINUE
         IF(KOMENT(80:80) .NE. SPACE)
     1WRITE(6,'(//1X,A)')'WARNING-THE LAST WORD IN FIRST COMMENT LINE'//
     2' MAY BE CORRUPTED'
         KOMENT(80:80)=CH
   90    CONTINUE
      ENDIF
      IF(TITLE(1:1) .NE. SPACE) THEN
         CH=TITLE(1:1)
         TITLE(1:1)=SPACE
         DO 100 I=2,78
            CH2=TITLE(I:I)
            TITLE(I:I)=CH
            CH=CH2
            IF(TITLE(I+1:I+2) .EQ. SPACE2) THEN
               TITLE(I+1:I+1)=CH
               GOTO 110
            ENDIF
  100    CONTINUE
         IF(TITLE(80:80) .NE. SPACE)
     1WRITE(6,'(//1X,A)')'WARNING-THE LAST WORD IN SECOND COMMENT'//
     2' LINE MAY BE CORRUPTED'
         TITLE(80:80)=CH
  110    CONTINUE
      ENDIF
      CALL GETGEO(5,LABELS,GEO,LOPT,NA,NB,NC,AMS,NATOMS,INT)
C
C
C OUTPUT FILE TO UNIT 6
C
C    WRITE HEADER
      WRITE(6,'(1X,15(''*****''),''****'')')
      WRITE(6,'('' ** FRANK J. SEILER RES. LAB., U.S. '',
     1''AIR FORCE ACADEMY, COLO. SPGS., CO. 80840 **'')')
      IF(INDEX(KEYWRD,'MINDO') .NE. 0) THEN
         WRITE(6,'(1X,15(''*****''),''****'',//29X,
     1''MINDO/3 CALCULATION RESULTS'',28X,///1X,15(''*****'')
     2,''****'' )')
      ELSEIF(INDEX(KEYWRD,'AM1') .NE. 0) THEN
         WRITE(6,'(1X,15(''*****''),''****'',//29X,
     1''AM1 CALCULATION RESULTS'',28X,///1X,15(''*****''),''****'' )')
      ELSE
         WRITE(6,'(1X,15(''*****''),''****'',//29X,
     1''MNDO CALCULATION RESULTS'',28X,///1X,15(''*****''),''****'' )')
      ENDIF
      WRITE(6,'('' *'',20X,''VERSION '',F5.2)')VERSON
C
C CHECK DATA
C
      DO 120 I=1,NATOMS
         IF (LABELS(I) .LE. 0 ) THEN
            WRITE(6,'('' ATOMIC NUMBER OF '',I3,'' ?'')') LABELS(I)
            STOP
         ENDIF
         IF (  NA(I).GE.I.OR. NB(I).GE.I.OR. NC(I).GE.I
     1  .OR. (NA(I).EQ.NB(I))   .AND. I.GT.1
     2  .OR. (NA(I).EQ.NC(I).OR.NB(I).EQ.NC(I))  .AND. I.GT.2
     3  .OR.  NA(I)*NB(I)*NC(I).EQ.0  .AND. I.GT.3) THEN
            WRITE(6,'('' ATOM NUMBER '',I3,'' IS ILLDEFINED'')') I
            STOP
         ENDIF
  120 CONTINUE
C
C WRITE KEYWORDS BACK TO USER AS FEEDBACK
      CALL WRTKEY(KEYWRD)
      WRITE(6,'(1X,79(''*''))')
C
C CONVERT ANGLES TO RADIANS
      DO 130 I=1,NATOMS
         DO 130 J=2,3
            GEO(J,I) = GEO(J,I) * 2.D0*ASIN(1.D0)/180.D0
  130 CONTINUE
C
C FILL IN GEO MATRIX IF NEEDED
      NDEP=0
      IF( INDEX(KEYWRD,'SYM') .NE. 0) CALL GETSYM
      IF(NDEP.NE.0) CALL SYMTRY
C
C INITIALIZE FLAGS FOR OPTIMIZE AND PATH
      IFLAG = 0
      NVAR  = 0
      LATOM = 0
      NUMAT=0
      DO 160 I=1,NATOMS
         IF(LABELS(I).NE.99.AND.LABELS(I).NE.107)NUMAT=NUMAT+1
         DO 160 J=1,3
            IF (LOPT(J,I) ) 140, 160, 150
C    FLAG FOR PATH
  140       CONVRT=1.D0
            IF ( IFLAG .NE. 0 ) THEN
               IF(INDEX(KEYWRD,'STEP1').NE.0)THEN
                  LPARA1=LPARAM
                  LATOM1=LATOM
                  LPARA2=J
                  LATOM2=I
                  LATOM=0
                  IFLAG=0
                  GOTO 160
               ELSE
                  WRITE(6,'('' ONLY ONE REACTION COORDINATE PERMITTED'')
     1')
                  STOP
               ENDIF
            ENDIF
            LATOM  = I
            LPARAM = J
            IF(J.GT.1) CONVRT=0.01745329252D00
            REACT(1)  = GEO(J,I)
            IREACT=1
            IFLAG = 1
            GO TO 160
C    FLAG FOR OPTIMIZE
  150       NVAR = NVAR + 1
            LOC(1,NVAR) = I
            LOC(2,NVAR) = J
            XPARAM(NVAR)   = GEO(J,I)
  160 CONTINUE
C READ IN PATH VALUES
      IF(IFLAG.EQ.0) GO TO 200
      IF(INDEX(KEYWRD,'NLLSQ').NE.0)THEN
         WRITE(6,'(A)')' NLLSQ USED WITH REACTION PATH; '//
     1'THIS OPTION IS NOT ALLOWED'
         STOP
      ENDIF
      IF(INDEX(KEYWRD,'SIGMA').NE.0)THEN
         WRITE(6,'(A)')' SIGMA USED WITH REACTION PATH; '//
     1'THIS OPTION IS NOT ALLOWED'
         STOP
      ENDIF
  170 READ(5,'(A)',END=190) LINE
      CALL NUCHAR(LINE,VALUE,NREACT)
      DO 180 I=1,NREACT
         IJ=IREACT+I
         IF(IJ.GT.200)THEN
            WRITE(6,'(///,''    ONLY TWO HUNDRED POINTS ALLOWED IN REACT
     1ION'','' COORDINATE'')')
            STOP
         ENDIF
         REACT(IJ)=VALUE(I)*CONVRT
         IF(ABS(REACT(IJ)-REACT(IJ-1)).LT.1.D-5)THEN
            WRITE(6,'(///,'' TWO ADJACENT POINTS ARE IDENTICAL'',/,
     1'' THIS IS NOT ALLOWED IN A PATH CALCULATION'')')
            STOP
         ENDIF
  180 CONTINUE
      IREACT=IREACT+NREACT
      GO TO 170
  190 CONTINUE
      DEGREE=1.D0
      IF(LPARAM.GT.1)DEGREE=90.D0/ASIN(1.D0)
      IF(IREACT.LE.1) THEN
         WRITE(6,'(//10X,'' NO POINTS SUPPLIED FOR REACTION PATH'')')
         WRITE(6,'(//10X,'' GEOMETRY AS READ IN IS AS FOLLOWS'')')
         CALL GEOUT
         STOP
      ELSE
         WRITE(6,'(//10X,'' POINTS ON REACTION COORDINATE'')')
         WRITE(6,'(10X,8F8.2)')(REACT(I)*DEGREE,I=1,IREACT)
      ENDIF
      IEND=IREACT+1
      REACT(IEND)=-1.D12
C
C OUTPUT GEOMETRY AS FEEDBACK
C
  200 WRITE(6,'(A)')KEYWRD,KOMENT,TITLE
      CALL GEOUT
      CALL GMETRY(GEO,COORD)
      IF (INDEX(KEYWRD,'NOXYZ') .EQ. 0) THEN
         WRITE(6,'(//10X,''CARTESIAN COORDINATES '',/)')
         WRITE(6,'(4X,''NO.'',7X,''ATOM'',9X,''X'',
     1  9X,''Y'',9X,''Z'',/)')
         L=0
         DO 210 I=1,NATOMS
            IF(LABELS(I) .EQ. 99.OR.LABELS(I).EQ.107) GOTO 210
            L=L+1
            WRITE(6,'(I6,8X,A2,4X,3F10.4)')
     1  L,ELEMNT(LABELS(I)),(COORD(J,L),J=1,3)
  210    CONTINUE
      ENDIF
      IF(   INDEX(KEYWRD,' XYZ') .NE.0)THEN
         IF( INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN CARTESIAN ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' INTERNAL COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN CARTESIAN COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
            WRITE(6,'(//10X,'' THIS INVOLVES A LOGICALLLY ABSURD CHOICE'
     1',/10X,'' SO THE CALCULATION IS TERMINATED AT THIS POINT'')')
            STOP
         ENDIF
         SUMX=0.D0
         SUMY=0.D0
         SUMZ=0.D0
         DO 220 J=1,NUMAT
            SUMX=SUMX+COORD(1,J)
            SUMY=SUMY+COORD(2,J)
  220    SUMZ=SUMZ+COORD(3,J)
         SUMX=SUMX/NUMAT
         SUMY=SUMY/NUMAT
         SUMZ=SUMZ/NUMAT
         DO 230 J=1,NUMAT
            GEO(1,J)=COORD(1,J)-SUMX
            GEO(2,J)=COORD(2,J)-SUMY
  230    GEO(3,J)=COORD(3,J)-SUMZ
         NA(1)=99
         J=0
         MVAR=1
         NVAR=1
         DO 260 I=1,NATOMS
            IF(LABELS(I).NE.99)THEN
               J=J+1
  240          IF(LOC(1,MVAR) .EQ. I) THEN
                  XPARAM(NVAR)=GEO(LOC(2,NVAR),J)
                  LOC(1,NVAR)=J
                  MVAR=MVAR+1
                  NVAR=NVAR+1
                  GOTO 240
               ENDIF
               LABELS(J)=LABELS(I)
            ELSE
  250          IF(LOC(1,MVAR) .EQ. I)THEN
                  MVAR=MVAR+1
                  GOTO 250
               ENDIF
            ENDIF
  260    CONTINUE
         NVAR=NVAR-1
         NATOMS=NUMAT
      ELSE
         IF( .NOT. INT.AND.(NDEP .NE. 0 .OR.  NVAR.LT.3*NUMAT-6)) THEN
            IF(NDEP.NE.0)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND SYMMETRY''
     2,/10X,'' SPECIFIED, BUT CALCULATION TO BE RUN IN INTERNAL ''
     3,''COORDINATES'')')
            IF(NVAR.LT.3*NUMAT-6)
     1WRITE(6,'(//10X,'' CARTESIAN COORDINATES READ IN, AND'',
     2'' CALCULATION '',/10X,''TO BE RUN IN INTERNAL COORDINATES, '',
     3/10X,''BUT NOT ALL COORDINATES MARKED FOR OPTIMISATION'')')
            WRITE(6,'(//10X,''MOPAC, BY DEFAULT, USES INTERNAL COORDINAT
     1ES'',/10X,''TO SPECIFY CARTESIAN COORDINATES USE KEY-WORD :XYZ:'')
     2')
            WRITE(6,'(10X,''YOUR CURRENT CHOICE OF KEY-WORDS INVOLVES A
     1'',''LOGICALLLY '',
     2/10X,''ABSURD CHOICE SO THE CALCULATION IS TERMINATED AT THIS ''
     3,''POINT'')')
            STOP
         ENDIF
      ENDIF
      RETURN
      END
