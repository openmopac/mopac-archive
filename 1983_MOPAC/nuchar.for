      SUBROUTINE NUCHAR(LINE,VALUE,NVALUE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
* DETERMINE AND RETURN THE REAL VALUE OF ALL NUMBERS FOUND IN
* 'LINE'. ALL CONNECTED SUBSTRINGS ARE ASSUMED TO CONTAIN NUMBERS
      DIMENSION VALUE(40),ISTART(40)
      CHARACTER*80 LINE
      CHARACTER*1 TAB,COMMA,SPACE
      LOGICAL LEADSP
      DATA TAB,COMMA,SPACE/'	',',',' '/
* CLEAN OUT TABS AND COMMAS
      DO 10 I=1,80
10       IF(LINE(I:I).EQ.TAB.OR.LINE(I:I).EQ.COMMA)LINE(I:I)=SPACE
* FIND INITIAL DIGIT OF ALL NUMBERS, CHECK FOR LEADING SPACES FOLLOWED
*     BY A CHARACTER
      LEADSP=.TRUE.
      NVALUE=0
      DO 12 I=1,80
         IF (LEADSP.AND.LINE(I:I).NE.SPACE) THEN
           NVALUE=NVALUE+1
           ISTART(NVALUE)=I
         END IF
         LEADSP=(LINE(I:I).EQ.SPACE)
12    CONTINUE
* FILL NUMBER ARRAY
      DO 14 I=1,NVALUE
        VALUE(I)=READA(LINE,ISTART(I))
14    CONTINUE
      RETURN
      END