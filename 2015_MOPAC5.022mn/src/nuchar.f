      SUBROUTINE NUCHAR(LINE,VALUE,NVALUE,*)                            CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
************************************************************************
*
*   NUCHAR  DETERMINES AND RETURNS THE REAL VALUES OF ALL NUMBERS
*           FOUND IN 'LINE'. ALL CONNECTED SUBSTRINGS ARE ASSUMED
*           TO CONTAIN NUMBERS
*   ON ENTRY LINE    = CHARACTER STRING
*   ON EXIT  VALUE   = ARRAY OF NVALUE REAL VALUES
*
************************************************************************
      DIMENSION VALUE(40),ISTART(40)
      CHARACTER LINE*(*)                                                LF0311
      CHARACTER*1 TAB,COMMA,SPACE
      LOGICAL LEADSP
CSAV         SAVE                                                           GL0892
      DATA COMMA,SPACE/',',' '/
      TAB=CHAR(9)
*
* CLEAN OUT TABS AND COMMAS
*
      DO 10 I=1,LEN(LINE)                                               LF0311
   10 IF(LINE(I:I).EQ.TAB.OR.LINE(I:I).EQ.COMMA)LINE(I:I)=SPACE
*
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
*     BY A CHARACTER
*
      LEADSP=.TRUE.
      NVALUE=0
      DO 20 I=1,LEN(LINE)                                               LF0311
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN
            NVALUE=NVALUE+1
            ISTART(NVALUE)=I
         ENDIF
         LEADSP=(LINE(I:I).EQ.SPACE)
   20 CONTINUE
*
* FILL NUMBER ARRAY
*
      DO 30 I=1,NVALUE
         VALUE(I)=READA(LINE,ISTART(I))
   30 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
