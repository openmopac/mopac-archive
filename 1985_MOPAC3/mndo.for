      PROGRAM MAIN
C
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      COMMON /KEYWRD/ KEYWRD
      COMMON /GEOVAR/ NVAR,LOC(2,MAXPAR), IDUMY, XPARAM(MAXPAR)
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /NUMCAL/ NUMCAL
      COMMON /TIME  / TIME0
      COMMON /PATH  / LATOM,LPARAM,REACT(100)
      CHARACTER*80 KEYWRD
      OPEN(UNIT=5,FILE='FOR005',STATUS='OLD',BLANK='ZERO')
      REWIND 5
      OPEN(UNIT=6,FILE='FOR006',STATUS='NEW')
      REWIND 6
      NUMCAL=1
C
      TIME0=SECOND()
C
C READ AND CHECK INPUT FILE, EXIT IF NECESSARY.
C     WRITE INPUT FILE TO UNIT 6 AS FEEDBACK TO USER
C
   10 CALL READ
      IF(INDEX(KEYWRD,'AUTHOR') .NE. 0)
     1WRITE(6,'(30X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',/
     2         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'',/
     3         ,10X,''     BY JAMES J. P. STEWART AT THE'',/
     4         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     5         ,10X,''          AUSTIN, TEXAS, 78712'')')
C
C INITIALIZE CALCULATION AND WRITE CALCULATION INDEPENDENT INFO
C
      IF(INDEX(KEYWRD,'0SCF') .NE. 0) STOP
      IF(INDEX(KEYWRD,'EXTERNAL') .NE. 0) THEN
         CALL AM1
      ELSE
         CALL MOLDAT
      ENDIF
C
C CALCULATE
C
C
      IF(INDEX(KEYWRD,' DRC') .NE. 0) THEN
         CALL DRC
         STOP
      ENDIF
      IF(INDEX(KEYWRD,'SADDLE') .NE. 0) THEN
         CALL REACT1(ESCF)
         GOTO 40
      ENDIF
      CLOSE (5)
      IF(INDEX(KEYWRD,'STEP1') .NE. 0) THEN
         CALL GRID
         STOP
      ENDIF
      IF (LATOM .NE. 0) THEN
C
C       DO PATH
C
         CALL PATHS
         STOP
      END IF
      IF (INDEX(KEYWRD,'FORCE') .NE. 0 ) THEN
C
C FORCE CALCULATION IF DESIRED
C
         CALL FORCE
         STOP
      ENDIF
C
      IF(INDEX(KEYWRD,'NLLSQ') .NE. 0) THEN
         CALL NLLSQ(XPARAM, NVAR )
         CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,.TRUE.)
         GOTO 40
      ENDIF
C
      IF (INDEX(KEYWRD,'RESTART').EQ.0.AND.
     1        INDEX(KEYWRD,'1SCF') .NE. 0) THEN
         NVAR=0
         IF(INDEX(KEYWRD,'GRAD').NE.0) THEN
            NVAR=0
            DO 30 I=2,NATOMS
               IF(LABELS(I).EQ.99) GOTO 30
               IF(I.EQ.2)ILIM=1
               IF(I.EQ.3)ILIM=2
               IF(I.GT.3)ILIM=3
               DO 20 J=1,ILIM
                  NVAR=NVAR+1
                  LOC(1,NVAR)=I
                  LOC(2,NVAR)=J
   20          XPARAM(NVAR)=GEO(J,I)
   30       CONTINUE
         ENDIF
      ENDIF
      IF(INDEX(KEYWRD,'SIGMA') .NE. 0) THEN
         CALL POWSQ(XPARAM, NVAR, ESCF)
         GOTO 40
      ENDIF
C
C ORDINARY GEOMETRY OPTIMISATION
C
      CALL FLEPO(XPARAM, NVAR, ESCF)
   40 CALL WRITE(TIME0, ESCF)
      IF(INDEX(KEYWRD,'POLAR') .NE. 0) THEN
         CALL POLAR
      ENDIF
      END
