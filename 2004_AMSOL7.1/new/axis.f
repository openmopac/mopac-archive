      SUBROUTINE AXIS(COORD,NUMAT,A,B,C,SUMW, MASS,EVEC) 
      IMPLICIT DOUBLE PRECISION (A-H,O-Z) 
       INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
************************************************************************ 
* 
*  AXIS CALCULATES THE THREE MOMENTS OF INERTIA AND THE MOLECULAR 
*       WEIGHT.  THE MOMENTS OF INERTIA ARE RETURNED IN A, B, AND C. 
*       THE MOLECULAR WEIGHT IN SUMW. 
*       THE UNITS OF INERTIA ARE 10**(-40)GRAM-CM**2, 
*       AND MOL.WEIGHT IN ATOMIC-MASS-UNITS. (AMU'S) 
************************************************************************ 
      DIMENSION COORD(3,NUMAT) 
      COMMON /ATMASS/ ATMASS(NUMATM) 
      COMMON /AREACM/ NOPTI,TAREA,NINTEG                                GDH0793
      COMMON /ONESCM/ ICONTR(100)                                       GDH0195
      DIMENSION T(6), X(NUMATM), Y(NUMATM), 
     1          Z(NUMATM), ROT(3), XYZMOM(3), EIG(3), EVEC(3,3) 
      LOGICAL TAREA,FIRST 
       SAVE
      DATA T /6*0.D0/
************************************************************************ 
*     CONST1 =  10**40/(N*A*A) 
*               N = AVERGADRO'S NUMBER 
*               A = CM IN AN ANGSTROM 
*               10**40 IS TO ALLOW UNITS TO BE 10**(-40)GRAM-CM**2 
* 
************************************************************************ 
      CONST1 = 1.66053D0 
************************************************************************ 
* 
*     CONST2 = CONVERSION FACTOR FROM ANGSTROM-AMU TO CM**(-1) 
*            = (PLANCK'S CONSTANT)/(4*PI*PI) 
* 
************************************************************************ 
      CONST2=16.85803902D0                                              GCL0393
C    FIRST WE CENTRE THE MOLECULE ABOUT THE CENTRE OF GRAVITY, 
C    THIS DEPENDS ON THE ISOTOPIC MASSES, AND THE CARTESIAN GEOMETRY. 
C 
      SUMW=0.D0 
      SUMWX=0.D0 
      SUMWY=0.D0 
      SUMWZ=0.D0 
      WEIGHT=1.D0 
      IF (ICONTR(20).EQ.1) THEN                                         GDH0195
         ICONTR(20)=2                                                   GDH0195
         FIRST=.TRUE.                                                   GDH0195
      ENDIF                                                             GDH0195
      DO 10 I=1,NUMAT 
         IF(MASS.GT.0)WEIGHT=ATMASS(I) 
         SUMW=SUMW+WEIGHT 
         SUMWX=SUMWX+WEIGHT*COORD(1,I) 
         SUMWY=SUMWY+WEIGHT*COORD(2,I) 
   10 SUMWZ=SUMWZ+WEIGHT*COORD(3,I) 
      IF((MASS.GT.0).AND.(FIRST)) THEN                                  GDH0195
         WRITE(JOUT,
     .           '(/,10X,''Molecular weight ='',F8.2,''amu'')')SUMW     GDH1093
      ENDIF      
      SUMWX=SUMWX/SUMW 
      SUMWY=SUMWY/SUMW 
      SUMWZ=SUMWZ/SUMW 
      DO 20 I=1,NUMAT 
         X(I)=COORD(1,I)-SUMWX 
         Y(I)=COORD(2,I)-SUMWY 
   20 Z(I)=COORD(3,I)-SUMWZ 
************************************************************************ 
* 
*    MATRIX FOR MOMENTS OF INERTIA IS OF FORM 
* 
*           !   Y**2+Z**2                         ! 
*           !    -Y*X       Z**2+X**2             ! -I =0 
*           !    -Z*X        -Z*Y       X**2+Y**2 ! 
* 
************************************************************************ 
      DO 30 I=1,6 
   30 T(I)=I*1.D-10 
      DO 40 I=1,NUMAT 
         IF(MASS.GT.0)WEIGHT=ATMASS(I) 
         T(1)=T(1)+WEIGHT*(Y(I)**2+Z(I)**2) 
         T(2)=T(2)-WEIGHT*X(I)*Y(I) 
         T(3)=T(3)+WEIGHT*(Z(I)**2+X(I)**2) 
         T(4)=T(4)-WEIGHT*Z(I)*X(I) 
         T(5)=T(5)-WEIGHT*Y(I)*Z(I) 
   40 T(6)=T(6)+WEIGHT*(X(I)**2+Y(I)**2) 
      CALL HQRII(T,3,3,EIG,EVEC)
      IF((MASS.GT.0).AND.(FIRST)) THEN                                  IR1294
         WRITE(JOUT,                                                    GDH1095
     .      '(/,10X,'' Principal moments of inertia in cm(-1)'')')  
         DO 50 I=1,3 
            IF(EIG(I).LT.3.D-4) THEN 
               EIG(I)=0.D0 
               ROT(I)=0.D0 
            ELSE 
               ROT(I)=CONST2/EIG(I) 
            ENDIF 
   50    XYZMOM(I)=EIG(I)*CONST1 
         WRITE(JOUT,'(10X,''A ='',F12.6,''   B ='',F12.6, 
     1''   C ='',F12.6)')(ROT(I),I=1,3)                                 GL0592
         WRITE(JOUT,'(/,10X,'' Principal moments of inertia in '',      GDH1095
     1''units of 10**(-40)*gram-cm**2'')')                              GL0592
         WRITE(JOUT,'(10X,''A ='',F12.6,''   B ='',F12.6, 
     1''   C ='',F12.6)')(XYZMOM(I),I=1,3)                              GL0592
         C=ROT(1) 
         B=ROT(2) 
         A=ROT(3) 
      ENDIF 
C 
C   NOW TO ORIENT THE MOLECULE SO THE CHIRALITY IS PRESERVED 
C 
      SUM=EVEC(1,1)*(EVEC(2,2)*EVEC(3,3)-EVEC(3,2)*EVEC(2,3)) + 
     1    EVEC(1,2)*(EVEC(2,3)*EVEC(3,1)-EVEC(2,1)*EVEC(3,3)) + 
     2    EVEC(1,3)*(EVEC(2,1)*EVEC(3,2)-EVEC(2,2)*EVEC(3,1)) 
      IF( SUM .LT. 0) THEN 
         DO 60 J=1,3 
   60    EVEC(J,1)=-EVEC(J,1) 
      ENDIF 
      DO 70 I=1,NUMAT 
         COORD(1,I)=X(I) 
         COORD(2,I)=Y(I) 
         COORD(3,I)=Z(I) 
   70 CONTINUE 
      IF(MASS.GT.0) FIRST=.FALSE.
      RETURN                                                            GDH0495
      END 
