
      SUBROUTINE SWAP(C,N,MDIM,NOCC,IFILL)                              
      IMPLICIT  DOUBLE PRECISION (A-H,O-Z)
      DIMENSION C(MDIM,MDIM),PSI(200),STDPSI(200)                       
C******************************************************************     
C                                                                        
C         SWAP ENSURES THAT A NAMED MOLECULAR ORBITAL IFILL IS FILLED    
C  ON INPUT                                                              
C           C = EIGENVECTORS IN A MDIM*MDIM MATRIX                       
C           N = NUMBER OF ORBITALS                                       
C           NOCC = NUMBER OF OCCUPIED ORBITALS                           
C           IFILL = FILLED ORBITAL                                       
C******************************************************************     
      IF(IFILL.GT.0) GOTO 10                                            
C                                                                        
C      WE NOW DEFINE THE FILLED ORBITAL                                  
C                                                                        
      IFILL=-IFILL                                                      
      DO 1 I=1,N                                                        
      STDPSI(I)=C(I,IFILL)                                              
   1  PSI(I)=C(I,IFILL)                                                 
      RETURN                                                            
  10  CONTINUE                                                          
C                                                                        
C      FIRST FIND THE LOCATION OF IFILL                                  
C                                                                        
      SUM=0.D0                                                          
      DO 11 I=1,N                                                       
  11  SUM=SUM+PSI(I)*C(I,IFILL)                                         
      IF(ABS(SUM).GT.0.7071D0) GOTO 20                                  
C                                                                        
C      IFILL HAS MOVED!                                                  
C                                                                        
      SUMMAX=0.D0                                                       
      DO 16 IFILL=1,N                                                   
      SUM=0.D0                                                          
      DO 17 I=1,N                                                       
  17  SUM=SUM+STDPSI(I)*C(I,IFILL)                                      
      SUM=ABS(SUM)                                                      
      IF(SUM.GT.SUMMAX)JFILL=IFILL                                      
      IF(SUM.GT.SUMMAX)SUMMAX=SUM                                      
      IF(SUM.GT.0.7071D0) GOTO 20                                       
  16  CONTINUE                                                          
      DO 18 IFILL=1,N                                                   
      SUM=0.D0                                                          
      DO 19 I=1,N                                                       
  19  SUM=SUM+PSI(I)*C(I,IFILL)                                         
      SUM=ABS(SUM)                                                      
      IF(SUM.GT.SUMMAX)JFILL=IFILL                                      
      IF(SUM.GT.SUMMAX)SUMMAX=SUM                                       
      IF(SUM.GT.0.7071D0) GOTO 20                                       
  18  CONTINUE                                                          
  33  FORMAT(' SUM VERY SMALL, SUM =',F12.6,' JFILL=',I3)               
      IFILL=JFILL                                                       
  20  CONTINUE                                                          
C                                                                        
C     STORE THE NEW VECTOR IN PSI                                        
C                                                                        
       DO 22 I=1,N                                                      
   22  PSI(I)=C(I,IFILL)                                                
C                                                                        
C     NOW CHECK TO SEE IF IFILL IS FILLED                                
C                                                                        
      IF(IFILL.LE.NOCC) RETURN                                          
C                                                                        
C     ITS EMPTY, SO SWAP IT WITH THE HIGHEST FILLED                      
C                                                                        
      DO 21 I=1,N                                                       
      X=C(I,NOCC)                                                       
      C(I,NOCC)=C(I,IFILL)                                              
      C(I,IFILL)=X                                                      
  21  CONTINUE                                                          
      RETURN                                                            
      END                                                               
