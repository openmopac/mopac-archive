      SUBROUTINE DRC(STARTV, STARTK,*)                                  CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
      DIMENSION STARTV(*), STARTK(*)
************************************************************************
*                                                                      *
*    DRC IS DESIGNED TO FOLLOW A REACTION PATH FROM THE TRANSITION     *
*    STATE.  TWO MODES ARE SUPPORTED, FIRST: GAS PHASE:- AS THE SYSTEM *
*    MOVES FROM THE T/S THE MOMENTUM OF THE ATOMS IS STORED AND THE    *
*    POSITION OF THE ATOMS IS RELATED TO THE OLD POSITION BY (A) THE   *
*    CURRENT VELOCITY OF THE ATOM, AND (B) THE FORCES ACTING ON THAT   *
*    ATOM.  THE SECOND MODE IS CONDENSED PHASE, IN WHICH THE ATOMS MOVE*
*    IN RESPONSE TO THE FORCES ACTING ON THEM. I.E. INFINITELY DAMPED  *
*                                                                      *
************************************************************************
      COMMON /KEYWRD/ KEYWRD
      COMMON /DENSTY/ P(MPACK),PA(MPACK),PB(MPACK)
      COMMON /GRADNT/ GRAD(MAXPAR),GNORM
      COMMON /GEOSYM/ NDEP,LOCPAR(MAXPAR),IDEPFN(MAXPAR),LOCDEP(MAXPAR)
      COMMON /GEOM  / GEO(3,NUMATM)
      COMMON /ATMASS/ ATMASS(NUMATM)
      COMMON /GEOVAR/ XPARAM(MAXPAR), NVAR, LOC(2,MAXPAR)               IR0394
      COMMON /GEOKST/ NATOMS,LABELS(NUMATM),
     1                NA(NUMATM),NB(NUMATM),NC(NUMATM)
C Common MOLKST splitted in MOLKSI and MOLKSR    Ivan Rossi 0394   &8)
      COMMON /MOLKSI/ NUMAT,NAT(NUMATM),NFIRST(NUMATM),
     1                NMIDLE(NUMATM),NLAST(NUMATM), NORBS,
     2                NELECS,NALPHA,NBETA,NCLOSE,NOPEN
     3       /MOLKSR/ FRACT
      COMMON /DRCCOM/ MCOPRT(2,MAXPAR), NCOPRT, PRTMAX
      COMMON /IOCM/ IREAD
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      CHARACTER KEYWRD*160
      CHARACTER SPACE*1, CHDOT*1, ZERO*1, NINE*1, CH*1
      DIMENSION VELREF(MAXPAR), VELO0(MAXPAR), VELO1(MAXPAR),
     1VELO2(MAXPAR), VELO3(MAXPAR), GERROR(MAXPAR),
     2COORD(3,NUMATM), GROLD2(MAXPAR),
     3GROLD(MAXPAR), PAROLD(MAXPAR), GEOREF(3,NUMATM),
     4 SQRTMS(MAXPAR)
      LOGICAL INT, ADDK, LETOT, LET, VELRED,PRTMAX, IRCDRC
CSAV         SAVE                                                           GL0892
      DATA VELO0/MAXPAR*0.D0/, INT/.TRUE./
      DATA SPACE,CHDOT,ZERO,NINE /' ','.','0','9'/
      DATA ADDK/.TRUE./
      IF (DOPRNT) CLOSE (IREAD)                                         CIO
C7/26/GL91 TNOW=SECNDS()
           TNOW=SECOND()
C7/26/GL91 OLDTIM=SECNDS()
           OLDTIM=SECOND()
      DELOLD=10.D0
      GTOT=0.D0
      OPEN(UNIT=7,STATUS='SCRATCH')                                     No CIO
      IF(INDEX(KEYWRD,' PREC').NE.0)THEN
         ACCU=0.5D0
      ELSE
         ACCU=1.D0
      ENDIF
      LPOINT=0
      VELRED=(INDEX(KEYWRD,'VELOC').NE.0)
C
C     PRINT OUT INITIAL VELOCITIES
C
C#      WRITE(6,'(A)')' INITIAL VELOCITY IN DRC'
C#      WRITE(6,'(3F13.5)')(STARTV(I),I=1,NUMAT*3)
      LET=(INDEX(KEYWRD,' GEO-OK').NE.0)
      IF(INDEX(KEYWRD,' SYMME').NE.0)THEN
         IF (DOPRNT) WRITE(6,*)                                         CIO
     &                '  SYMMETRY SPECIFIED, BUT CANNOT BE USED IN DRC' CIO
         NDEP=0
      ENDIF
C
C      CONVERT TO CARTESIAN COORDINATES, IF NOT ALREADY DONE.
C
      IF(INDEX(KEYWRD,' XYZ').EQ.0)THEN
         NA(1)=0
         CALL GMETRY(GEO,COORD,*9999)                                    CSTP(call)
         L=0
         DO 20 I=1,NUMAT
            LABELS(I)=NAT(I)
            SUM=SQRT(ATMASS(NAT(I)))
            DO 10 J=1,3
               L=L+1
               SQRTMS(L)=SUM
               GEO(J,I)=COORD(J,I)
   10       COORD(J,I)=0.D0
   20    CONTINUE
         NA(1)=99
      ENDIF
C
C  TRANSFER COORDINATES TO XPARAM AND LOC
C
      IF(INDEX(KEYWRD,' DRC').NE.0)THEN
         PRTMAX=(LOC(1,1).EQ.1)
         IF(PRTMAX)THEN
            J=1
         ELSE
            J=0
         ENDIF
         NVAR=NVAR-J
         DO 30 I=1,NVAR
            MCOPRT(1,I)=LOC(1,I+J)
   30    MCOPRT(2,I)=LOC(2,I+J)
         IF(LOC(1,1).EQ.0)NVAR=0
         NCOPRT=NVAR
      ELSE
         NCOPRT=0
      ENDIF
      L=0
      DO 40 I=1,NUMAT
         DO 40 J=1,3
            L=L+1
            LOC(1,L)=I
            LOC(2,L)=J
            GEOREF(J,I)=GEO(J,I)
   40 XPARAM(L)=GEO(J,I)
      NVAR=NUMAT*3
C
C DETERMINE DAMPING FACTOR
C
      IRCDRC=(INDEX(KEYWRD,'IRC=').NE.0)
      IF(INDEX(KEYWRD,'DRC=').NE.0) THEN
         HALF=READA(KEYWRD,INDEX(KEYWRD,'DRC='))
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     &    '' DAMPING FACTOR FOR KINETIC ENERGY ='',F12.6)')HALF         CIO
      ELSEIF (INDEX(KEYWRD,'DRC').EQ.0) THEN
         HALF=0.D0
      ELSE
         HALF=1.D6
      ENDIF
      LETOT=(.NOT.IRCDRC.AND.HALF.LT.1.D0)
      HALF=SIGN(MAX(0.000001D0,ABS(HALF)),HALF)
C
C DETERMINE EXCESS KINETIC ENERGY
C
      IF(INDEX(KEYWRD,'KINE').NE.0) THEN
         ADDONK=READA(KEYWRD,INDEX(KEYWRD,'KINE'))
         IF (DOPRNT) WRITE(6,'(//10X,                                   CIO
     & '' EXCESS KINETIC ENERGY ENTERED INTO SYSTEM ='',F12.6)')ADDONK  CIO
      ELSE
         ADDONK=0.D0
      ENDIF
C
C   LOOP OVER TIME-INTERVALS OF DELTAT SECOND
C
      DELTAT=1.D-16
      QUADR=1.D0
      TOTIME=0.D0
      ONCE=0.D0
      ETOT=0.D0
      ESCF=0.D0
      CONST=1.D0
      I=INDEX(KEYWRD,' T=')
      IF(I.NE.0) THEN
         TIM=READA(KEYWRD,I)
         DO 50 J=I+3,160
            IF( KEYWRD(J+1:J+1).EQ.' ') THEN
               CH=KEYWRD(J:J)
               IF( CH .EQ. 'M') TIM=TIM*60
               IF( CH .EQ. 'H') TIM=TIM*3600
               IF( CH .EQ. 'D') TIM=TIM*86400
               GOTO 60
            ENDIF
   50    CONTINUE
C             4 SECONDS TO LOAD IN EXECUTABLE!
   60    TLEFT=TIM-4
      ELSE
         TLEFT=3596
      ENDIF
      IF( INDEX(KEYWRD,'REST').NE.0.AND.INDEX(KEYWRD,'IRC=').EQ.0)THEN
C
C  RESTART FROM A PREVIOUS RUN
C
         OPEN(UNIT=9,FILE='FOR009',STATUS='UNKNOWN',                    No CIO
     &                  FORM='FORMATTED')                               No CIO
         REWIND 9                                                       No CIO
         OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',                   No CIO
     &                  FORM='UNFORMATTED')                             No CIO
         REWIND 10                                                      No CIO
         READ(9,'(A80)')ALPHA                                           No CIO
         READ(9,'(3F19.13)')(XPARAM(I),I=1,NVAR)                        No CIO
         READ(9,'(A80)')ALPHA                                           No CIO
         READ(9,'(3F19.3)')(VELO0(I),I=1,NVAR)                          No CIO
         READ(9,'(A80)')ALPHA                                           No CIO
         READ(9,*)(GRAD(I),I=1,NVAR)                                    No CIO
         READ(9,*)(GROLD(I),I=1,NVAR)                                   No CIO
         READ(9,*)(GROLD2(I),I=1,NVAR)                                  No CIO
         READ(9,*)ETOT,ESCF,EKIN,DELOLD,DELTAT,DLOLD2,ILOOP,            No CIO
     1GNORM,LETOT,ELOST1,GTOT                                           No CIO
         WRITE(6,'(//10X,''CALCULATION RESTARTED, CURRENT'',            No CIO
     1'' KINETIC ENERGY='',F10.5,//)')EKIN                              No CIO
         GOTO 110 
      ELSE
         ILOOP=1
         IF(INDEX(KEYWRD,'IRC=').NE.0.OR.VELRED)THEN
            IF(INDEX(KEYWRD,'IRC=').NE.0)THEN
               K=READA(KEYWRD,INDEX(KEYWRD,'IRC='))
            ELSE
               K=1
            ENDIF
            IF(K.LT.0)THEN
               K=-K
               ONE=-1.D0
            ELSE
               ONE=1.D0
            ENDIF
            KL=(K-1)*NVAR
            SUMM=0.D0
            VELO1(1)=0
            VELO1(2)=0
            VELO1(3)=0
            SUMMAS=0.D0
            I=0
            DO 70 II=1,NUMAT
               AMS=ATMASS(II)
               SUMMAS=SUMMAS+AMS
               DO 70 I1=1,3
                  I=I+1
                  VELO0(I)=STARTV(KL+I)*ONE
                  VELREF(I)=VELO0(I)
                  VELO1(I1)=VELO1(I1)+VELO0(I)*AMS
   70       CONTINUE
            DO 80 I=1,3
   80       VELO1(I)=-VELO1(I)/SUMMAS
            I=0
            DO 90 II=1,NUMAT
               AMS=ATMASS(II)
               DO 90 I1=1,3
                  I=I+1
                  IF(ADDONK.GT.1.D-5.OR..NOT.VELRED)VELO0(I)=VELO0(I)+VE
     1LO1(I1)
   90       SUMM=SUMM+VELO0(I)**2*AMS
            IF(ADDONK.LT.1.D-5.AND.VELRED)ADDONK=0.5D0*SUMM/4.184D10
            IF(ADDONK.LT.1.D-5.AND..NOT.VELRED)THEN
               IF(ABS(HALF).GT.1.D-3.AND.STARTK(K).GT.105.D0)THEN
                  IF (DOPRNT) WRITE(6,'(A,F10.3,A,/,A)')                CIO
     &                      ' BY DEFAULT, ONE QUANTUM OF ENERGY,'       CIO
     &                      //' EQUIVALENT TO',STARTK(K),' CM(-1)',     CIO
     &                      ' WILL BE USED TO START THE DRC'            CIO
C
C    2.8585086D-3 CONVERTS CM(-1) INTO KCAL/MOLE
C
                  ADDONK=STARTK(K)*2.8585086D-3
                  IF (DOPRNT) WRITE(6,'(A,F7.2,A)')                     CIO
     &                              ' THIS REPRESENTS AN ENERGY OF',    CIO
     &                              ADDONK,' KCALS/MOLE'                CIO
               ELSEIF(ABS(HALF).GT.1.D-3)THEN
                  IF (DOPRNT) WRITE(6,'(A,F7.2,A)')                     CIO
     &                              ' THE VIBRATIONAL FREQUENCY (',     CIO
     &             STARTK(K),' IS TOO SMALL FOR ONE QUANTUM TO BE USED' CIO
                  IF (DOPRNT) WRITE(6,'(A)')                            CIO
     &            ' INSTEAD 0.3KCAL/MOLE WILL BE USED TO START THE IRC' CIO
                  ADDONK=0.3D0
               ELSE
                  ADDONK=0.3D0
               ENDIF
            ENDIF
C
C   AT THIS POINT ADDONK IS IN KCAL/MOLE
C   NORMALIZE SO THAT TOTAL K.E. = ONE QUANTUM (DEFAULT) (DRC ONLY)
C                              OR 0.3KCAL/MOLE (IRC ONLY)
C                              OR ADDONK IF KINETIC=NN SUPPLIED
C
            SUMM=SQRT(ADDONK/(0.5D0*SUMM/4.184D10))
            ADDK=.FALSE.
            IF(SUMM.GT.1.D-10)THEN
               DO  100 I=1,NVAR
  100          VELO0(I)=VELO0(I)*SUMM
            ENDIF
         ENDIF
      ENDIF
  110 CONTINUE
      IUPPER=ILOOP+4999
      ILP=ILOOP
      ONE=0.D0
      IF(INDEX(KEYWRD,'REST').NE.0.AND.INDEX(KEYWRD,'IRC=').EQ.0)
     1ONE=1.D0
      DO 190 ILOOP=ILP,IUPPER
C
C  MOVEMENT OF ATOMS WILL BE PROPORTIONAL TO THE AVERAGE VELOCITIES
C  OF THE ATOMS BEFORE AND AFTER TIME INTERVAL
C
C
C  RAPID CHANGE IN GRADIENT IMPLIES SMALL STEP SIZE FOR DELTAT
C
C   KINETIC ENERGY = 1/2 * M * V * V
C                  = 0.5 / (4.184D10) * M * V * V
C   NEW VELOCITY = OLD VELOCITY + GRADIENT * TIME / MASS
C                = KCAL/ANGSTROM*SECOND/(ATOMIC WEIGHT)
C                =4.184*10**10(ERGS)*10**8(PER CM)*DELTAT(SECONDS)
C   NEW POSITION = OLD POSITION - AVERAGE VELOCITY * TIME INTERVAL
C
C
C   ESTABLISH REFERENCE TOTAL ENERGY
C
         ERROR=(ETOT-(EKIN+ESCF))
         IF(ILOOP.GT.2)THEN
            QUADR = 1.D0+ERROR/(EKIN*CONST+0.001D0)*0.5D0
            QUADR = MIN(1.3D0,MAX(0.8D0,QUADR))
         ELSE
            QUADR=1.D0
         ENDIF
         IF((LET.OR.EKIN.GT.0.2).AND.ADDK)THEN
C
C   DUMP IN EXCESS KINETIC ENERGY
C
            ETOT=ETOT+ADDONK
            ADDK=.FALSE.
            ADDONK=0.D0
         ENDIF
         EKOLD=EKIN
C
C  CALCULATE THE DURATION OF THE NEXT STEP.
C  STEP SIZE IS THAT REQUIRED TO PRODUCE A CONSTANT CHANGE IN GEOMETRY
C
C
C  IF DAMPING IS USED, CALCULATE THE NEW TOTAL ENERGY AND
C  THE RATIO FOR REDUCING THE KINETIC ENERGY
C
         CONST=MAX(1.D-36,0.5D0**(DELTAT*1.D15/HALF))
         CONST=SQRT(CONST)
         VELVEC=0.D0
         EKIN=0.D0
         DELTA1=DELOLD+DLOLD2
         ELOST=0.D0
         DO 120 I=1,NVAR
C
C   CALCULATE COMPONENTS OF VELOCITY AS
C   V = V(0) + V'*T + V"*T*T
C   WE NEED ALL THREE TERMS, V(0), V' AND V"
C
            VELO1(I) = 1.D0/ATMASS(LOC(1,I))*GRAD(I)
            IF(ILOOP.GT.3) THEN
               VELO3(I) = 2.D0/ATMASS(LOC(1,I))*
     1(DELTA1*(GROLD(I)-GRAD(I))-DELOLD*(GROLD2(I)-GRAD(I)))/
     2(DELTA1*(DELOLD**2*1.D30)-DELOLD*(DELTA1**2*1.D30))
               VELO2(I)=1.D0/ATMASS(LOC(1,I))*
     1(GRAD(I)-GROLD(I)-0.5D0*VELO3(I)*(1.D30*DELOLD**2))/(DELOLD*1.D15)
            ELSE
               VELO2(I) = 1.D0/ATMASS(LOC(1,I))*
     1                 (GRAD(I)-GROLD(I))/(1.D15*DELOLD)
               VELO3(I)=0.D0
            ENDIF
C
C  MOVE ATOMS THROUGH DISTANCE EQUAL TO VELOCITY * DELTA-TIME, NOTE
C  VELOCITY CHANGES FROM START TO FINISH, THEREFORE AVERAGE.
C
            PAROLD(I)=XPARAM(I)
            XPARAM(I)=XPARAM(I)
     1             -1.D8*(DELTAT*VELO0(I)*ONE
     2             +0.5D0*DELTAT**2*VELO1(I)
     3             +0.16666D0*(DELTAT**2*1.D15)*DELTAT*VELO2(I)
     4             +0.0416666D0*DELTAT**2*(1.D30*DELTAT**2)*VELO3(I))
C
C   CORRECT ERRORS DUE TO CUBIC COMPONENTS IN ENERGY GRADIENT,
C   ALSO TO ADD ON EXCESS ENERGY, IF NECESSARY.
C
            VELVEC=VELVEC+VELO0(I)**2
C
C   MODIFY VELOCITY IN LIGHT OF CURRENT ENERGY GRADIENTS.
C
C   VELOCITY = OLD VELOCITY + (DELTA-T / ATOMIC MASS) * CURRENT GRADIENT
C                           + 1/2 *(DELTA-T * DELTA-T /ATOMIC MASS) *
C                             (SLOPE OF GRADIENT)
C              SLOPE OF GRADIENT = (GRAD(I)-GROLD(I))/DELOLD
C
C
C   THIS EXPRESSION IS ACCURATE TO SECOND ORDER IN TIME.
C
            VELO0(I) = VELO0(I) + DELTAT*VELO1(I) + 0.5D0*DELTAT**2*VELO
     12(I)*1.D15           + 0.166666D0*DELTAT*(1.D30*DELTAT**2)*VELO3(
     2I)
            IF(LET.OR.GNORM.GT.3.D0)THEN
               LET=.TRUE.
               ELOST=ELOST+VELO0(I)**2*ATMASS(LOC(1,I))*(1-CONST**2)
               VELO0(I)=VELO0(I)*CONST*QUADR
            ENDIF
C
C  CALCULATE KINETIC ENERGY (IN 2*ERGS AT THIS POINT)
C
            EKIN=EKIN+VELO0(I)**2*ATMASS(LOC(1,I))
  120    CONTINUE
         ONE=1.D0
         IF(LET.OR.GNORM.GT.3.D0)THEN
            IF(.NOT.LETOT)THEN
               ETOT=ESCF+ADDONK
               ADDONK=0.D0
               ELOST1=0.D0
               ELOST=0.D0
            ENDIF
            LETOT=.TRUE.
         ENDIF
C
C  CONVERT ENERGY INTO KCAL/MOLE
C
         EKIN=0.5*EKIN/4.184D10
         IF(LETOT.AND.ABS(HALF).GT.0.00001D0)
     1ETOT=ETOT-EKIN/CONST**2+EKIN
         ELOST1=ELOST1+0.5D0*ELOST/4.184D10
C
C STORE OLD GRADIENTS FOR DELTA - VELOCITY CALCULATION
C
         DO 130 I=1,NVAR
            GROLD2(I)=GROLD(I)
            GROLD(I)=GRAD(I)
  130    GRAD(I)=0.D0
C
C   CALCULATE ENERGY AND GRADIENTS
C
         SCFOLD=ESCF
         CALL COMPFG(XPARAM,.TRUE.,ESCF,.TRUE.,GRAD,.TRUE.,*9999)        CSTP(call)
         IF(ILOOP.GT.2)THEN
            GNORM=0.D0
            DO 150 I=1,NVAR,3
               SUM=SQRT(ddot(3,GRAD(I),1,GRAD(I),1)/
     1(ddot(3,VELO0(I),1,VELO0(I),1)+1.D-20))
               DO 140 J=I,I+2
  140          GERROR(J)=GERROR(J)+GRAD(J)+VELO0(J)*SUM
  150       CONTINUE
            GNORM=SQRT(ddot(nvar,GERROR,1,GERROR,1))
            GTOT=GNORM
         ENDIF
         GNORM=SQRT(ddot(nvar,grad,1,grad,1))
C
C   CONVERT GRADIENTS INTO ERGS/CM
C
         DO 160 I=1,NVAR
  160    GRAD(I)=GRAD(I)*4.184D18
C
C   SPECIAL TREATMENT FOR FIRST POINT - SET "OLD" GRADIENTS EQUAL TO
C   CURRENT GRADIENTS.
C
         IF(ILOOP.EQ.1) THEN
            DO 170 I=1,NVAR
  170       GROLD(I)=GRAD(I)
         ENDIF
         DLOLD2=DELOLD
         DELOLD=DELTAT
         SUM=0.D0
         DO 180 I=1,NVAR
  180    SUM=SUM + ((GRAD(I)-GROLD(I))/4.184D18)**2
         IF(ABS(HALF).LT.0.001D0)THEN
            DELTAT= DELTAT*
     1MIN(2.D0, (5.D-5*ACCU/(ABS(ESCF+ELOST1-ETOLD)+1.D-20)))**0.25D0
            ETOLD=ESCF+ELOST1
            IF(DOPRNT.AND.(ILOOP.GT.5.AND.SCFOLD-ESCF.LT.-1.D-3 .OR.     CIO
     1      ILOOP.GT.30.AND.SCFOLD-ESCF.LT.0.D0))  THEN                  CIO
               WRITE(6,'(//,'' IRC CALCULATION COMPLETE '')')
      RETURN 1                                                           CSTP (stop)
            ENDIF
         ELSE
            DELTAT= DELTAT*MIN(1.05D0, 10.D0*ACCU/(SUM+1.D-4))
************************************************************************
*
*         TESTING CODE - REMOVE BEFORE FINAL VERSION ASSEMBLED
C#          (ILOOP/400)*400.EQ.ILOOP)DELTAT=-DELTAT
*
************************************************************************
         ENDIF
         DELTAT=MAX(1.D-16,DELTAT)
         IF(ABS(HALF).LT.0.00001D0)THEN
            CALL PRTDRC(ESCF,DELTAT,XPARAM,GEOREF,
     1ELOST1,GTOT,ETOT,VELO0,NVAR,*9999)                                 CSTP(call)
         ELSE
            CALL PRTDRC(ESCF,DELTAT,XPARAM,GEOREF,
     1EKIN,ELOST,ETOT,VELO0,NVAR,*9999)                                  CSTP(call)
         ENDIF
         TNOW=SECOND()
         TCYCLE=TNOW-OLDTIM
         OLDTIM=TNOW
         TLEFT=TLEFT-TCYCLE
         IF (ILOOP.EQ.IUPPER.OR.TLEFT.LT.3*TCYCLE) THEN
           IF(DOPRNT) THEN
            IF(INDEX(KEYWRD,'REST').NE.0.OR.INDEX(KEYWRD,'ISOT').NE.0)  
     &      CLOSE(9)                                                    
            OPEN(UNIT=9,FILE='FOR009',STATUS='unknown',FORM='FORMATTED')
            REWIND 9
            OPEN(UNIT=10,FILE='FOR010',STATUS='UNKNOWN',FORM='UNFORMATTE
     1D')
            REWIND 10
            WRITE(9,'(A)')' CARTESIAN GEOMETRY PARAMETERS IN ANGSTROMS'
            WRITE(9,'(3F19.13)')(XPARAM(I),I=1,NVAR)
            WRITE(9,'(A)')' VELOCITY FOR EACH CARTESIAN COORDINATE, IN C
     1M/SEC'
            WRITE(9,'(3F19.3)')(VELO0(I),I=1,NVAR)
            WRITE(9,'(A)')' FIRST, SECOND, AND THIRD-ORDER GRADIENTS, ET
     1C'
            WRITE(9,*)(GRAD(I),I=1,NVAR)
            WRITE(9,*)(GROLD(I),I=1,NVAR)
            WRITE(9,*)(GROLD2(I),I=1,NVAR)
           ENDIF
            I=ILOOP+1
            IF(DOPRNT)WRITE(9,*)ETOT,ESCF,EKIN,DELOLD,DELTAT,DLOLD2,I,  CIO
     &                          GNORM,LETOT,ELOST1,GTOT                 CIO
            ESCF=-1.D9
            CALL PRTDRC(ESCF,DELTAT,XPARAM,GEOREF,
     1EKIN,ELOST,ETOT,VELO0,NVAR,*9999)                                  CSTP(call)
            LINEAR=(NORBS*(NORBS+1))/2
            IF (DOPRNT) WRITE(10)(PA(I),I=1,LINEAR)                     CIO
            IF(DOPRNT.AND.(NALPHA.NE.0))WRITE(10)(PB(I),I=1,LINEAR)     CIO
            IF(DOPRNT) WRITE(6,'(//10X,                                 CIO
     &            '' RUNNING OUT OF TIME, RESTART FILE WRITTEN '')')    CIO
      RETURN 1                                                           CSTP (stop)
         ENDIF
  190 CONTINUE
 9999 RETURN 1                                                          CSTP
      END
