      SUBROUTINE GMETRYx(GEO,COORD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      INCLUDE 'SIZES'
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     +NA(NUMATM),NB(NUMATM),NC(NUMATM)
      COMMON /REACTN/ STEP, GEOA(3,NUMATM), GEOVEC(3,NUMATM),COLCST
         DIMENSION GEO(3,*),COORD(3,*)
C***********************************************************************
C                                                                        
C     GMETRY  COMPUTES COORDINATES FROM BOND-ANGLES AND LENGTHS.         
C  *** IT IS ADAPTED FROM THE PROGRAM WRITTEN BY M.J.S. DEWAR.           
C
C      THREE SEPARATE OPTIONS EXIST WITHIN GMETRY. THESE ARE:
C     (A) IF NA(1) IS EQUAL TO 99 (IMPOSSIBLE UNDER NORMAL CIRCUMSTANCES)
C         THEN GEO IS ASSUMED TO BE IN CARTESIAN RATHER THAN INTERNAL
C         COORDINATES, AND COORD IS THEN SET EQUAL TO GEO.
C     (B) IF STEP IS NON-ZERO (THIS IS THE CASE WHEN "SADDLE" IS USED)
C         THEN GEO IS FIRST MODIFIED BY SHIFTING THE INTERNAL COORDINATES
C         ALONG A RADIUS FROM GEOA TO PLACE GEO AT A DISTANCE STEP FROM GEOA.
C     (C) NORMAL CONVERSION FROM INTERNAL TO CARTESIAN COORDINATES IS DONE.
C                                                                        
C   ON INPUT:                                                            
C          GEO    = ARRAY OF INTERNAL COORDINATES.                  
C          NATOMS = NUMBER OF ATOMS, INCLUDING DUMMIES.
C          NA     = ARRAY OF ATOM LABELS FOR BOND LENGTHS.
C
C   ON OUTPUT:                                                           
C          COORD  = ARRAY OF CARTESIAN COORDINATES                       
C
C***********************************************************************
C                                      OPTION (A)
      IF(NA(1).EQ.99) THEN
         DO 12 I=1,3
             DO 12 J=1,NATOMS
  12         COORD(I,J)=GEO(I,J)
         RETURN
         ENDIF
C                                      OPTION (B)
      IF(ABS(STEP) .GT. 1.D-4) THEN
      SUM=0.D0
      DO 11 I=1,NATOMS
      DO 11 J=1,3
      GEOVEC(J,I)=GEO(J,I)-GEOA(J,I)
  11  SUM=SUM+GEOVEC(J,I)**2
      SUM=SQRT(SUM)
      ERROR=(SUM-STEP)/SUM
      ELSE
      ERROR=0.D0
      ENDIF
      DO 14 I=1,NATOMS
      DO 14 J=1,3
  14  GEO(J,I)=GEO(J,I)-ERROR*GEOVEC(J,I)
C                                      OPTION (C)
      COORD(1,1)=0.0D00
      COORD(2,1)=0.0D00                                               
      COORD(3,1)=0.0D00                                               
      COORD(1,2)=GEO(1,2)
      COORD(2,2)=0.0D00                                               
      COORD(3,2)=0.0D00                
      IF(NATOMS.EQ.2) GOTO 80            
      CCOS=COS(GEO(2,3))                   
      IF(NA(3).EQ.1)THEN
          COORD(1,3)=COORD(1,1)+GEO(1,3)*CCOS
      ELSE
          COORD(1,3)=COORD(1,2)-GEO(1,3)*CCOS
      ENDIF
      COORD(2,3)=GEO(1,3)*SIN(GEO(2,3))
      COORD(3,3)=0.0D00                                               
      DO 70 I=4,NATOMS                                                 
         COSA=COS(GEO(2,I))                                               
         MB=NB(I)                                                       
         MC=NA(I)                                                       
         XB=COORD(1,MB)-COORD(1,MC)                                     
         YB=COORD(2,MB)-COORD(2,MC)                                     
         ZB=COORD(3,MB)-COORD(3,MC)                                     
         RBC=1.0D00/SQRT(XB*XB+YB*YB+ZB*ZB)                             
         IF (ABS(COSA).LT.0.99999999991D00) GO TO 40                    
C                                                                        
C      ATOMS MC, MB, AND (I) ARE COLLINEAR                               
C                                                                        
         RBC=GEO(1,I)*RBC*COSA
         COORD(1,I)=COORD(1,MC)+XB*RBC
         COORD(2,I)=COORD(2,MC)+YB*RBC
         COORD(3,I)=COORD(3,MC)+ZB*RBC                             
         GO TO 70                                                       
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
C      ROTATE ABOUT THE Y-AXIS TO MAKE ZB VANISH                         
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
C      ROTATE ABOUT THE X-AXIS TO MAKE ZA=0, AND YA POSITIVE.            
C                                                                        
         YZA=SQRT(YPA**2+ZQA**2)                                        
      IF(YZA.LT.1.D-1 )THEN
      IF(YZA.LT.1.D-10)GOTO 21                                          
      WRITE(6,'(//20X,'' CALCULATION ABANDONED AT THIS POINT'')')
      WRITE(6,'(//10X,'' THREE ATOMS BEING USED TO DEFINE THE'',/
     +10X,'' COORDINATES OF A FOURTH ATOM, WHOSE BOND-ANGLE IS'',/
     110X,'' NOT ZERO OR 180 DEGREEES, ARE IN AN ALMOST STRAIGHT'',/
     210X,'' LINE.  THERE IS A HIGH PROBABILITY THAT THE'',/
     310X,'' COORDINATES OF THE ATOM WILL BE INCORRECT.'')')
      WRITE(6,'(//20X,''THE FAULTY ATOM IS ATOM NUMBER'',I4)')I
      CALL GEOUT
      WRITE(6,'(//20X,''CARTESIAN COORDINATES UP TO FAULTY ATOM'')')
      WRITE(6,'(//5X,''I'',12X,''X'',12X,''Y'',12X,''Z'')')
      DO 164 J=1,I
  164 WRITE(6,'(I6,F16.5,2F13.5)')J,(COORD(K,J),K=1,3)
      WRITE(6,'(//6X,'' ATOMS'',I3,'','',I3,'', AND'',I3,
     +'' ARE WITHIN'',F7.4,'' ANGSTROMS OF A STRAIGHT LINE'')')
     +MC,MB,MA,YZA
      STOP
      ENDIF
         COSKH=YPA/YZA                                                  
         SINKH=ZQA/YZA                                                  
      GOTO 22                                                           
  21  CONTINUE                                                          
C                                                                        
C    ANGLE TOO SMALL TO BE IMPORTANT                                     
C                                                                        
      COSKH=1.D0                                                        
      SINKH=0.D0                                                        
  22  CONTINUE                                                          
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
C      TRANSFORM THE COORDINATES BACK TO THE ORIGIONAL SYSTEM.           
C                                                                        
         YPD=YD*COSKH-ZD*SINKH                                          
         ZPD=ZD*COSKH+YD*SINKH                                          
         XPD=XD*COSPH-ZPD*SINPH                                         
         ZQD=ZPD*COSPH+XD*SINPH                                         
         XQD=XPD*COSTH-YPD*SINTH                                        
         YQD=YPD*COSTH+XPD*SINTH                                        
         IF (K.LT.1) GO TO 60                                           
         XRD=-ZQD                                                       
         ZQD=XQD                                                        
         XQD=XRD                                                        
   60    COORD(1,I)=XQD+COORD(1,MC)                                     
         COORD(2,I)=YQD+COORD(2,MC)                                     
         COORD(3,I)=ZQD+COORD(3,MC)                                     
   70 CONTINUE                                                          
C                                                                        
C  *** NOW REMOVE THE DUMMY ATOM COORDINATES, IF ANY, FROM THE ARRAY COOR
C                                                                        
   80 NUMAT=NATOMS
      J=0                                                               
      DO 100 I=1,NATOMS                                                 
         IF (LABELS(I).EQ.99) GO TO 100                                     
         J=J+1                                                          
         DO 90 K=1,3                                                    
   90    COORD(K,J)=COORD(K,I)                                          
  100 CONTINUE                                                          
      NUMAT=J
      RETURN                                                            
                                                                        
      END                                                               
