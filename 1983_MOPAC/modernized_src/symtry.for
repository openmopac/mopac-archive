
      SUBROUTINE SYMTRY
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                     
      INCLUDE 'SIZES'
      COMMON /GMETRY/ GEO(3,NUMATM)
      COMMON /GEOSYM/ NDEP, LOCPAR(200), IDEPFN(200), LOCDEP(200)
C********************************************************************** 
C                                                                        
C   SYMTRY COMPUTES THE BOND LENGTHS AND ANGLES THAT ARE FUNCTIONS OF    
C          OTHER BOND LENGTHS AND ANGLES.                                
C                                                                        
C  ON INPUT GEO     = KNOWN INTERNAL COORDINATES                         
C           NDEP    = NUMBER OF DEPENDENCY FUNCTIONS.
C           IDEPFN  = ARRAY OF DEPENDENCY FUNCTIONS.
C           LOCDEP  = ARRAY OF LABELS OF DEPENDENT ATOMS.
C           LOCPAR  = ARRAY OF LABELS OF REFERENCE ATOMS.
C                                                                        
C   ON OUTPUT THE ARRAY "GEO" IS FILLED                                    
C***********************************************************************
C                                                                        
C      NOW COMPUTE THE DEPENDENT PARAMETERS.                             
                                                                        
      DO 30 I=1,NDEP                                                    
         CALL HADDON (VALUE,LOCN,IDEPFN(I),LOCPAR(I),GEO)
         J=LOCDEP(I)                                                    
   30 GEO(LOCN,J)=VALUE                                                   
      RETURN
      END
