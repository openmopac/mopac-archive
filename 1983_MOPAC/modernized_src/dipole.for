
      FUNCTION DIPOLE (P,Q,COORD,DIPVEC)                 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      INCLUDE 'SIZES'
      COMMON /MOLKST/ NUMAT,NAT(NUMATM),NFIRST(NUMATM), NMIDLE(NUMATM),
     1                NLAST(NUMATM), NORBS, NELECS,NALPHA,NBETA,
     +                NCLOSE,NOPEN
      COMMON /KEYWRD/ KEYWRD
      DIMENSION P(*),Q(*),COORD(3,*),DIPVEC(3)
      CHARACTER*80 KEYWRD
C***********************************************************************
C      DIPOLE CALCULATES DIPOLE MOMENTS                                  
C                                                                        
C   ON INPUT P     = DENSITY MATRIX                                      
C            Q     = TOTAL ATOMIC CHARGES, (NUCLEAR + ELECTRONIC)        
C            NUMAT = NUMBER OF ATOMS IN MOLECULE                         
C            NAT   = ATOMIC NUMBERS OF ATOMS                             
C            NFIRST= START OF ATOM ORBITAL COUNTERS                      
C            COORD = COORDINATES OF ATOMS                                
C                                                                        
C   OUTPUT  DIPOLE = DIPOLE MOMENT                                              
C***********************************************************************
                                                                        
C      IN THE ZDO APPROXIMATION, ONLY TWO TERMS ARE RETAINED IN THE      
C      CALCULATION OF DIPOLE MOMENTS.                                    
C      1. THE POINT CHARGE TERM (INDEPENDENT OF PARAMETERIZATION).       
C      2. THE ONE-CENTER HYBRIDIZATION TERM, WHICH ARISES FROM MATRIX    
C      ELEMENTS OF THE FORM <NS/R/NP>. THIS TERM IS A FUNCTION OF        
C      THE SLATER EXPONENTS (ZS,ZP) AND IS THUS DEPENDENT ON PARAMETER-  
C      IZATION. THE HYBRIDIZATION FACTORS (HYF(I)) USED IN THIS SUB-     
C      ROUTINE ARE CALCULATED FROM THE FOLLOWING FORMULAE.               
C      FOR SECOND ROW ELEMENTS <2S/R/2P>                                 
C      HYF(I)= 469.56193322*(SQRT(((ZS(I)**5)*(ZP(I)**5)))/              
C            ((ZS(I) + ZP(I))**6))                                       
C      FOR THIRD ROW ELEMENTS <3S/R/3P>                                  
C      HYF(I)=2629.54682607*(SQRT(((ZS(I)**7)*(ZP(I)**7)))/              
C            ((ZS(I) + ZP(I))**8))                                       
C      FOR FOURTH ROW ELEMENTS AND UP :
C      HYF(I)=2*(2.5416)*DD(I)
C      WHERE DD(I) IS THE CHARGE SEPARATION IN ATOMIC UNITS
C
C                                                                        
C      REFERENCES:                                                       
C      J.A.POPLE & D.L.BEVERIDGE: APPROXIMATE M.O. THEORY                
C      S.P.MCGLYNN, ET AL: APPLIED QUANTUM CHEMISTRY                     
C                                                                        
      DIMENSION DIP(4,3)                                                
      DIMENSION HYF(54,2)
      LOGICAL FIRST, FORCE
      DATA HYF(1,1)     / 0.0D00           /                              
      DATA HYF(4,1)/7.30614633D00/                                        
      DATA HYF(5,1)/4.86912910D00/                                        
      DATA HYF(6,1)/4.10447739D00/                                        
      DATA HYF(7,1)/3.25273083D00/                                        
      DATA HYF(8,1)/2.71746791D00/                                        
      DATA HYF(9,1)/2.57572001D00/                                        
      DATA HYF(13,1)/7.11254996D00/                                       
      DATA HYF(14,1)/7.15643126D00/                                       
      DATA HYF(15,1)/5.14908505D00/                                       
      DATA HYF(16,1)/4.18424965D00/                                       
      DATA HYF(17,1)/2.5349045D00/                                        
      DATA HYF(35,1)/3.0758819D00/
      DATA HYF(53,1)/7.2452040D00/

      DATA   HYF(1,2) /0.0D0     /
      DATA   HYF(5,2) /6.520587D0/
      DATA   HYF(6,2) /4.253676D0/
      DATA   HYF(7,2) /2.947501D0/
      DATA   HYF(8,2) /2.139793D0/
      DATA   HYF(9,2) /2.225419D0/
      DATA   HYF(14,2)/6.663059D0/
      DATA   HYF(15,2)/5.657623D0/
      DATA   HYF(16,2)/6.345552D0/
      DATA   HYF(17,2)/2.522964D0/
      DATA FIRST /.TRUE./
      IF (FIRST) THEN
          FIRST=.FALSE.
          FORCE=(INDEX(KEYWRD,'FORCE') .NE. 0)
         ITYPE=1
         IF(INDEX(KEYWRD,'MINDO3') .NE. 0)ITYPE=2
      ENDIF
      DO 10 I=1,4                                                       
      DO 10 J=1,3                                                     
   10 DIP(I,J)=0.0D00                                                   
      DO 20 I=1,NUMAT                                                   
         NI=NAT(I)                                                      
         IA=NFIRST(I)                                                   
      DO 20 J=1,3                                                       
         K=((IA+J)*(IA+J-1))/2+IA                                       
         DIP(J,2)=DIP(J,2)-HYF(NI,ITYPE)*P(K)
   20 DIP(J,1)=DIP(J,1)+4.803D00*Q(I)*COORD(J,I)                        
      DO 30 J=1,3                                                       
   30 DIP(J,3)=DIP(J,2)+DIP(J,1)                                        
      DO 40 J=1,3                                                       
   40 DIP(4,J)=SQRT(DIP(1,J)**2+DIP(2,J)**2+DIP(3,J)**2)                
      IF( FORCE) THEN
          DIPVEC(1)=DIP(1,3)
          DIPVEC(2)=DIP(2,3)
          DIPVEC(3)=DIP(3,3)
          ELSE
          WRITE (6,50) ((DIP(I,J),I=1,4),J=1,3)
      ENDIF
      DIPOLE = DIP(4,3)                                                       
      RETURN                                                            
                                                                        
   50 FORMAT (7H0DIPOLE,11X,2HX ,8X,2HY ,8X,2HZ ,6X,5HTOTAL/11H0POINT-CH
     1G.,4F10.3/7H0HYBRID,4X,4F10.3/4H0SUM,7X,4F10.3)                   
                                                                        
      END                                                               
