      SUBROUTINE DEPVAR (A,I,W,L)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(3,*)
C***********************************************************************
C
C  IN SUBROUTINE HADDON WHEN M, THE SYMMETRY OPERATION, IS 18 DEPVAR IS
C  CALLED. DEPVAR SHOULD THEN CONTAIN A USER-WRITTEN SYMMETRY OPERATION.
C  SEE HADDON TO GET THE IDEA ON HOW TO WRITE DEPVAR.
C
C ON INPUT:
C           A = ARRAY OF INTERNAL COORDINATES
C           I = ADDRESS OF REFERENCE ATOM
C ON OUTPUT:
C           L = 1 (IF A BOND-LENGTH IS THE DEPENDENT FUNCTION)
C             = 2 (IF AN ANGLE IS THE DEPENDENT FUNCTION)
C             = 3 (IF A DIHEDRAL ANGLE IS THE DEPENDENT FUNCTION)
C           W = VALUE OF THE FUNCTION
C
C  NOTE:  IT IS THE WRITER'S RESPONSIBILITY TO MAKE CERTAIN THAT THE
C         SUBROUTINE DOES NOT CONTAIN ANY ERRORS!
C***********************************************************************
      COMMON /KEYWRD/ KEYWRD
      CHARACTER*80 KEYWRD
      LOGICAL FIRST
      DATA FIRST/.TRUE./
      IF (FIRST) THEN
         FIRST=.FALSE.
         FACT=READA(KEYWRD,INDEX(KEYWRD,'DEPVAR'))
         WRITE(6,'(''  UNIT CELL LENGTH ='',F14.7,
     1'' TIMES BOND LENGTH'')')FACT
      ENDIF
      W=A(1,I)*FACT
      L=1
      RETURN
      END