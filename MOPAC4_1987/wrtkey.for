      SUBROUTINE WRTKEY(KEYWRD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      INCLUDE 'SIZES'
      CHARACTER*80 KEYWRD, ALLKEY
***********************************************************************
*
*  WRTKEY CHECKS ALL KEY-WORDS AND PRINTS THOSE IT RECOGNIZES.  IF IT
*  FINDS A WORD IT DOES NOT RECOGNIZE THE PROGRAM WILL BE STOPPED.
*
***********************************************************************
      LOGICAL UHF, TRIP, BIRAD, EXCI, CI, WORD
      LOGICAL AM1, MNDO, MINDO3
      CHARACTER SPACE*1, DOT*1, ZERO*1, NINE*1, CH*1
      DATA SPACE,DOT,ZERO,NINE /' ','.','0','9'/
      DATA AM1, MNDO, MINDO3 /3*.FALSE./
      ALLKEY=KEYWRD
      IF(WORD(ALLKEY,'AUTHOR')) THEN
         WRITE(6,'(10X,'' MOPAC - A GENERAL MOLECULAR ORBITAL PACKAGE'',
     1/         ,10X,''   ORIGINAL VERSION WRITTEN IN 1983'')')
         WRITE(6,'(10X,''     BY JAMES J. P. STEWART AT THE'',/
     1         ,10X,''     UNIVERSITY OF TEXAS AT AUSTIN'',/
     2         ,10X,''          AUSTIN, TEXAS, 78712'')')
      ENDIF
      IF (WORD(ALLKEY,'VECT') ) WRITE(6,70)
      IF (WORD(ALLKEY,' EXTE') ) THEN
         I=INDEX(KEYWRD,' EXTE')
         J=INDEX(KEYWRD(I:),'=')+I
         I=INDEX(KEYWRD(J:),' ')+J-1
         WRITE(6,80)KEYWRD(J:I)
      ENDIF
      MAXGEO=0
      IF (WORD(ALLKEY,' DENS') ) WRITE(6,90)
      IF (WORD(ALLKEY,'SPIN') ) WRITE(6,100)
      IF (WORD(ALLKEY,' DEPVAR') )
     1WRITE(6,580)READA(KEYWRD,INDEX(KEYWRD,'DEPVAR'))
      IF (WORD(ALLKEY,' DEP ') )WRITE(6,120)
      IF (WORD(ALLKEY,'TIMES') )WRITE(6,130)
      IF (WORD(ALLKEY,'PARASOK') ) WRITE(6,140)
      IF (WORD(ALLKEY,'FLEPO') ) WRITE(6,150)
      IF (WORD(ALLKEY,'BONDS') ) WRITE(6,160)
      IF (WORD(ALLKEY,'GEO-OK') ) WRITE(6,170)
      IF (WORD(ALLKEY,'FOCK') ) WRITE(6,180)
      IF (WORD(ALLKEY,'LARGE') ) WRITE(6,190)
      IF (WORD(ALLKEY,' S1978') ) WRITE(6,200)
      IF (WORD(ALLKEY,' SI1978') ) WRITE(6,210)
      IF (WORD(ALLKEY,' GRAP') ) WRITE(6,220)
      IF (WORD(ALLKEY,'1ELEC') ) WRITE(6,230)
      IF (WORD(ALLKEY,' ESR') ) WRITE(6,240)
      IF (WORD(ALLKEY,'DFP') ) WRITE(6,250)
      IF (WORD(ALLKEY,'ANALYT') ) WRITE(6,260)
      IF (WORD(ALLKEY,' MECI') ) WRITE(6,270)
      IF (WORD(ALLKEY,'LOCAL') ) WRITE(6,310)
      IF (WORD(ALLKEY,'MULLIK') ) WRITE(6,320)
      IF (WORD(ALLKEY,' XYZ') ) WRITE(6,330)
      IF (WORD(ALLKEY,' PI') ) WRITE(6,340)
      IF (WORD(ALLKEY,'ECHO') ) WRITE(6,350)
      IF (WORD(ALLKEY, 'SING') ) WRITE(6,510)
      IF (WORD(ALLKEY, 'DOUB') ) WRITE(6,520)
      IF (WORD(ALLKEY, 'QUAR') ) WRITE(6,540)
      IF (WORD(ALLKEY, 'QUIN') ) WRITE(6,550)
      IF (WORD(ALLKEY, 'SEXT') ) WRITE(6,560)
      IF (WORD(ALLKEY,'H-PRIO') ) WRITE(6,360)
      IF (WORD(ALLKEY,'X-PRIO') ) WRITE(6,370)
      IF (WORD(ALLKEY,'T-PRIO') ) WRITE(6,380)
      IF (WORD(ALLKEY,'POWSQ') ) WRITE(6,390)
      IF (WORD(ALLKEY,'POLAR') ) WRITE(6,400)
      IF (WORD(ALLKEY,'DEBUG ') ) WRITE(6,410)
      IF (WORD(ALLKEY,'RESTART') ) WRITE(6,420)
      IF (WORD(ALLKEY,'IRC=') ) THEN
         MAXGEO=1
         WRITE(6,430)NINT(READA(KEYWRD,INDEX(KEYWRD,'IRC=')))
      ELSEIF (WORD(ALLKEY,'IRC') ) THEN
         MAXGEO=1
         WRITE(6,440)
      ENDIF
      IF (WORD(ALLKEY,'CHARGE') )
     1 WRITE(6,450)NINT(READA(KEYWRD,INDEX(KEYWRD,'CHARGE')))
      IF (WORD(ALLKEY,'GRAD') ) WRITE(6,460)
      UHF=(WORD(ALLKEY,'UHF') )
      IF(UHF)WRITE(6,470)
      BIRAD=(WORD(ALLKEY,'BIRAD') )
      IF(BIRAD)WRITE(6,490)
      EXCI=(WORD(ALLKEY,'EXCITED') )
      IF(EXCI) WRITE(6,500)
      TRIP=(WORD(ALLKEY,'TRIPLET') )
      IF(TRIP)WRITE(6,530)
      IF (WORD(ALLKEY,'SYM') ) WRITE(6,570)
      IF(WORD(ALLKEY,'OPEN('))THEN
         I=INDEX(KEYWRD,'OPEN(')
         IELEC=READA(KEYWRD,I)
         ILEVEL=READA(KEYWRD,I+7)
         WRITE(6,590)IELEC,ILEVEL
      ENDIF
      IF(WORD(ALLKEY,'MICROS'))
     1WRITE(6,580)INT(READA(KEYWRD,INDEX(KEYWRD,'MICROS')))
      IF(WORD(ALLKEY,'DRC='))THEN
         MAXGEO=1
         WRITE(6,290)READA(KEYWRD,INDEX(KEYWRD,'DRC='))
      ELSEIF (WORD(ALLKEY,' DRC') ) THEN
         MAXGEO=1
         WRITE(6,280)
      ENDIF
      IF(WORD(ALLKEY,'KINE'))
     1WRITE(6,300)READA(KEYWRD,INDEX(KEYWRD,'KINE'))
      IF(WORD(ALLKEY,' T=')) THEN
         I=INDEX(KEYWRD,' T=')
         TIME=READA(KEYWRD,I)
         DO 10 J=I+3,80
            CH=KEYWRD(J:J)
            IF( CH .NE. 'D' .AND. CH .NE. 'E' .AND. CH .NE. DOT
     1 .AND. (CH .LT. ZERO .OR. CH .GT. NINE)) THEN
               IF( CH .EQ. 'M') TIME=TIME*60
               GOTO 20
            ENDIF
   10    CONTINUE
   20    CONTINUE
         IF(TIME.LT.99999.9D0)THEN
            WRITE(6,600)TIME
         ELSE
            WRITE(6,610)TIME
         ENDIF
      ELSE
         WRITE(6,600)MAXTIM*1.D0
      ENDIF
      IF(WORD(ALLKEY,' DUMP')) THEN
         TDUMP=READA(KEYWRD,INDEX(KEYWRD,' DUMP'))
         DO 30 J=I+5,80
            CH=KEYWRD(J:J)
            IF( CH .NE. 'D' .AND. CH .NE. 'E' .AND. CH .NE. DOT
     1 .AND. (CH .LT. ZERO .OR. CH .GT. NINE)) THEN
               IF( CH .EQ. 'M') TDUMP=TDUMP*60
               GOTO 40
            ENDIF
   30    CONTINUE
   40    CONTINUE
         IF(TDUMP.LT.99999.9D0)THEN
            WRITE(6,620)TDUMP
         ELSE
            WRITE(6,630)TDUMP
         ENDIF
      ELSE
         WRITE(6,620)MAXDMP*1.D0
      ENDIF
      IF (WORD(ALLKEY,'1SCF') ) THEN
         WRITE(6,640)
         MAXGEO=MAXGEO+1
      ENDIF
      IF(INDEX(ALLKEY,'C.I.').NE.0)
     1WRITE(6,650)INT(READA(KEYWRD,INDEX(KEYWRD,'C.I.')+5))
      CI=WORD(ALLKEY,'C.I.')
      IF (WORD(ALLKEY,'DFORCE') ) THEN
         WRITE(6,660)
         MAXGEO=MAXGEO+1
      ELSEIF (WORD(ALLKEY,'FORCE') ) THEN
         WRITE(6,670)
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'MINDO') ) THEN
         WRITE(6,680)
         MINDO3=.TRUE.
      ENDIF
      IF (WORD(ALLKEY,'AM1') ) THEN
         WRITE(6,690)
         AM1=.TRUE.
      ENDIF
      IF (WORD(ALLKEY,'MNDO') ) THEN
         MNDO=.TRUE.
      ENDIF
      IF (WORD(ALLKEY,'PREC') ) WRITE(6,700)
      IF (WORD(ALLKEY,'NOINTER') ) WRITE(6,710)
      IF (WORD(ALLKEY,'ISOTOPE') ) WRITE(6,720)
      IF (WORD(ALLKEY,'DENOUT') ) WRITE(6,730)
      IF (WORD(ALLKEY,'SHIFT') ) WRITE(6,740)
     1 READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
      IF (WORD(ALLKEY,'OLDENS') ) WRITE(6,750)
      IF (WORD(ALLKEY,'SCFCRT') ) WRITE(6,760)
     1 READA(KEYWRD,INDEX(KEYWRD,'SCFCRT'))
      IF (WORD(ALLKEY,'ENPART') ) WRITE(6,770)
      IF (WORD(ALLKEY,'NOXYZ') ) WRITE(6,780)
      IF (WORD(ALLKEY,'SIGMA') ) THEN
         WRITE(6,790)
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'NLLSQ') ) THEN
         WRITE(6,800)
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,'ROOT') ) WRITE(6,810)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ROOT')))
      IF (WORD(ALLKEY,'TRANS=') ) THEN
         WRITE(6,830)NINT(READA(KEYWRD,INDEX(KEYWRD,'TRANS=')))
      ELSEIF (WORD(ALLKEY,'TRANS') ) THEN
         WRITE(6,820)
      ENDIF
      IF (WORD(ALLKEY,'SADDLE') ) THEN
         WRITE(6,840)
         MAXGEO=MAXGEO+1
      ENDIF
      IF (WORD(ALLKEY,' LET') ) WRITE(6,850)
      IF (WORD(ALLKEY,'COMPFG') ) WRITE(6,860)
      IF (WORD(ALLKEY,'DERIV') ) WRITE(6,870)
      IF (WORD(ALLKEY,'FULSCF') ) WRITE(6,880)
      IF (WORD(ALLKEY,'DCART') ) WRITE(6,890)
      IF (WORD(ALLKEY,'GNORM') ) WRITE(6,900)
     1 READA(KEYWRD,INDEX(KEYWRD,'GNORM'))
      IF (WORD(ALLKEY,'FMAT') ) WRITE(6,910)
      IF (WORD(ALLKEY,'HCORE') ) WRITE(6,920)
      IF (WORD(ALLKEY,'ITER') ) WRITE(6,930)
      IF (WORD(ALLKEY,'PULAY') ) WRITE(6,940)
      IF (WORD(ALLKEY,'LINMIN') ) WRITE(6,950)
      IF (WORD(ALLKEY,'LOCMIN') ) WRITE(6,960)
      IF (WORD(ALLKEY,' STEP1')  )WRITE(6,970)
     1 READA(KEYWRD,INDEX(KEYWRD,'STEP1')+6)
      IF (WORD(ALLKEY,' STEP2')  )WRITE(6,980)
     1 READA(KEYWRD,INDEX(KEYWRD,'STEP2')+6)
      IF (WORD(ALLKEY,'BAR') ) WRITE(6,990)
     1 READA(KEYWRD,INDEX(KEYWRD,'BAR'))
      IF (WORD(ALLKEY,'DEBUGPULAY') ) WRITE(6,1000)
      IF (WORD(ALLKEY,'CAMP') ) WRITE(6,1010)
      IF (WORD(ALLKEY,'KING') ) WRITE(6,1010)
      IF (WORD(ALLKEY,'EIGS') ) WRITE(6,1020)
      IF (WORD(ALLKEY,'MOLDAT') ) WRITE(6,1030)
      IF (WORD(ALLKEY,'HYPERF') ) WRITE(6,1040)
      IF (WORD(ALLKEY,'OPCI') ) WRITE(6,1050)
      IF (WORD(ALLKEY,' PL') ) WRITE(6,1060)
      IF (WORD(ALLKEY,'SEARCH') ) WRITE(6,1070)
      IF (WORD(ALLKEY,'CYCLES') ) WRITE(6,1090)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'CYCLES')))
      IF (WORD(ALLKEY,'FILL') ) WRITE(6,1080)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'FILL')))
      IF (WORD(ALLKEY,'ITRY') ) WRITE(6,1120)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ITRY')))
      IF (WORD(ALLKEY,'0SCF') ) WRITE(6,1140)
      IF(UHF)THEN
         IF(BIRAD.OR.EXCI.OR.CI)THEN
            WRITE(6,'(//10X,
     1'' UHF USED WITH EITHER BIRAD, EXCITED OR C.I. '')')
            WRITE(6,1130)
            GOTO 60
         ENDIF
      ELSE
         IF(EXCI.AND. TRIP) THEN
            WRITE(6,'(//10X,'' EXCITED USED WITH TRIPLET'')')
            WRITE(6,1130)
            GOTO 60
         ENDIF
      ENDIF
      IF (INDEX(KEYWRD,'T-PRIO').NE.0.AND.
     1INDEX(KEYWRD,'DRC').EQ.0) THEN
         WRITE (6,'(//10X,''T-PRIO AND NO DRC'')')
         WRITE (6,1130)
         GOTO 60
      ENDIF
      IF ( (MINDO3 .AND. AM1) .OR. (MINDO3 .AND. MNDO) .OR.
     1     (MNDO .AND. AM1) )   THEN
         WRITE(6,'(//10X,'' ONLY ONE OF MINDO, MNDO AND AM1 ALLOWED'')')
         WRITE (6,1130)
         GOTO 60
      ENDIF
      IF (WORD(ALLKEY,'THERMO') )THEN
         WRITE(6,1100)
         IF(WORD(ALLKEY,' ROT')) THEN
            WRITE(6,1110)NINT(READA(KEYWRD,INDEX(KEYWRD,' ROT')))
         ELSE
            WRITE(6,'
     1    (//10X,'' YOU MUST SUPPLY THE SYMMETRY NUMBER "ROT"'')')
            STOP
         ENDIF
      ENDIF
      IF(MAXGEO.GT.1)THEN
         WRITE(6,'(//10X,''MORE THAN ONE GEOMETRY OPTION HAS BEEN '',
     1''SPECIFIED'',/10X,
     2''CONFLICT MUST BE RESOLVED BEFORE JOB WILL RUN'')')
         STOP
      ENDIF
      IF(ALLKEY.NE.' ')THEN
C
C  AN UNRECOGNIZED KEY-WORD HAS BEEN USED. FIND IT AND THEN ABANDON THE
C  RUN
         J=0
         DO 50 I=1,79
            IF(ALLKEY(I:I).NE.' '.OR.ALLKEY(I:I+1).NE.'  ')THEN
               J=J+1
               KEYWRD(J:J)=ALLKEY(I:I)
            ENDIF
   50    CONTINUE
         IF(ALLKEY(80:80).NE.' ')THEN
            J=J+1
            KEYWRD(J:J)=ALLKEY(80:80)
         ENDIF
         J=MAX(1,J)
         WRITE(6,'(///10X,''UNRECOGNIZED KEY-WORDS: ('',A,'')'')')
     1KEYWRD(:J)
         WRITE(6,'(///10X,''CALCULATION STOPPED TO AVOID WASTING TIME.''
     1)')
         STOP
      ENDIF
      RETURN
   60 WRITE(6,'(//10X,'' CALCULATION ABANDONED, SORRY!'')')
      STOP
   70 FORMAT(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED')
   80 FORMAT(' *  EXTERNAL - USE ATOMIC PARAMETERS FROM THE FOLLOWING '
     1,'FILE',/15X,A)
   90 FORMAT(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED')
  100 FORMAT(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED')
  110 FORMAT(' *  DEPVAR=N - TRANSLATION VECTOR IS',F7.4,
     1' TIMES BOND LENGTH')
  120 FORMAT(' *  DEP      - OUTPUT FORTRAN CODE FOR BLOCK-DATA')
  130 FORMAT(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED')
  140 FORMAT(' *  PARASOK  - USE SOME MNDO PARAMETERS IN AN AM1 CALCULA'
     1,'TION')
  150 FORMAT(' *  FLEPO    - PRINT DETAILS OF GEOMETRY OPTIMISATION')
  160 FORMAT(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED')
  170 FORMAT(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK')
  180 FORMAT(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED')
  190 FORMAT(' *  LARGE    - EXPANDED OUTPUT TO BE PRINTED')
  200 FORMAT(' *  S1978    - 1978 SULFUR PARAMETERS TO BE USED')
  210 FORMAT(' *  SI1978   - 1978 SILICON PARAMETERS TO BE USED')
  220 FORMAT(' *  GRAPH    - GENERATE FILE FOR GRAPHICS')
  230 FORMAT(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED')
  240 FORMAT(' *  ESR      - RHF SPIN DENSITY CALCULATION REQUESTED')
  250 FORMAT(' *  DFP      - USE DAVIDON FLETCHER POWELL OPTIMIZER')
  260 FORMAT(' *  ANALYT   - USE ANALYTIC DERIVATIVES ')
  270 FORMAT(' *  MECI     - M.E.C.I. WORKING TO BE PRINTED')
  280 FORMAT(' *  DRC      - DYNAMIC REACTION COORDINATE CALCULATION')
  290 FORMAT(' *  DRC=     - HALF-LIFE FOR KINETIC ENERGY LOSS =',F9.2,
     1' * 10**(-14) SECONDS')
  300 FORMAT(' *  KINETIC= - ',F7.3,' KCAL KINETIC ENERGY ADDED TO DRC')
  310 FORMAT(' *  LOCALIZE - LOCALIZED ORBITALS TO BE PRINTED')
  320 FORMAT(' *  MULLIK   - THE MULLIKEN ANALYSIS TO BE PERFORMED')
  330 FORMAT(' *   XYZ     - CARTESIAN COORDINATE SYSTEM TO BE USED')
  340 FORMAT(' *   PI      - BONDS MATRIX, SPLIT INTO SIGMA-PI-DELL',
     1' COMPONENTS, TO BE PRINTED')
  350 FORMAT(' *  ECHO     - ALL INPUT DATA TO BE ECHOED BEFORE RUN')
  360 FORMAT(' *  H-PRIOR  - HEAT OF FORMATION TAKES PRIORITY IN DRC')
  370 FORMAT(' *  X-PRIOR  - GEOMETRY CHANGES TAKE PRIORITY IN DRC')
  380 FORMAT(' *  T-PRIOR  - TIME TAKES PRIORITY IN DRC')
  390 FORMAT(' *  POWSQ    - PRINT DETAILS OF WORKING IN POWSQ')
  400 FORMAT(' *  POLAR    - CALCULATE THE POLARIZATION VOLUMES')
  410 FORMAT(' *  DEBUG    - DEBUG OPTION TURNED ON')
  420 FORMAT(' *  RESTART  - CALCULATION RESTARTED')
  430 FORMAT(' *  IRC=N    - INTRINSIC REACTION COORDINATE',I3,
     1' DEFINED')
  440 FORMAT(' *  IRC      - INTRINSIC REACTION COORDINATE CALCULATION')
  450 FORMAT(3(' *',/),' *',15X,'  CHARGE ON SYSTEM =',I3,3(/,' *'))
  460 FORMAT(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED')
  470 FORMAT(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION')
  480 FORMAT(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET')
  490 FORMAT(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS')
  500 FORMAT(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMIZED')
  510 FORMAT(' *  SINGLET  - SPIN STATE DEFINED AS A SINGLET')
  520 FORMAT(' *  DOUBLET  - SPIN STATE DEFINED AS A DOUBLET')
  530 FORMAT(' *  TRIPLET  - SPIN STATE DEFINED AS A TRIPLET')
  540 FORMAT(' *  QUARTET  - SPIN STATE DEFINED AS A QUARTET')
  550 FORMAT(' *  QUINTET  - SPIN STATE DEFINED AS A QUINTET')
  560 FORMAT(' *  SEXTET   - SPIN STATE DEFINED AS A SEXTET')
  570 FORMAT(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED')
  580 FORMAT(' *  MICROS=N -',I4,' MICROSTATES TO BE SUPPLIED FOR C.I.')
  590 FORMAT(' *  OPEN(N,N)- THERE ARE',I2,' ELECTRONS IN',I2,' LEVELS')
  600 FORMAT(' *   T=      - A TIME OF',F8.1,' SECONDS REQUESTED')
  610 FORMAT(' *   T=      - A TIME OF',G11.3,' SECONDS REQUESTED')
  620 FORMAT(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',F8.1,
     1' SECONDS')
  630 FORMAT(' *  DUMP=N   - RESTART FILE WRITTEN EVERY',G11.3,
     1' SECONDS')
  640 FORMAT(' *  1SCF     - DO 1 SCF AND THEN STOP ')
  650 FORMAT(' *  C.I.=N   -',I2,' M.O.S TO BE USED IN C.I.')
  660 FORMAT(' *  DFORCE   - PRINT HESSIAN MATRIX IN FORCE')
  670 FORMAT(' *  FORCE    - FORCE CALCULATION SPECIFIED')
  680 FORMAT(' *  MINDO/3  - THE MINDO/3 HAMILTONIAN TO BE USED')
  690 FORMAT(' *  AM1      - THE AM1 HAMILTONIAN TO BE USED')
  700 FORMAT(' *  PRECISE  - CRITERIA TO BE INCREASED BY 100 TIMES')
  710 FORMAT(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED')
  720 FORMAT(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )')
  730 FORMAT(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10')
  740 FORMAT(' *  SHIFT    - A DAMPING FACTOR OF',F8.2,' DEFINED')
  750 FORMAT(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK')
  760 FORMAT(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY',G12.3)
  770 FORMAT(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS')
  780 FORMAT(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED')
  790 FORMAT(' *  SIGMA    - GEOMETRY TO BE OPTIMIZED USING SIGMA.')
  800 FORMAT(' *  NLLSQ    - GRADIENTS TO BE MINIMIZED USING NLLSQ.')
  810 FORMAT(' *  ROOT     - IN A C.I. CALCULATION, ROOT',I2,
     1                       ' TO BE OPTIMIZED.')
  820 FORMAT(' *  TRANS    - THE REACTION VIBRATION TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  830 FORMAT(' *  TRANS=   - ',I4,' VIBRATIONS ARE TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  840 FORMAT(' *  SADDLE   - TRANSITION STATE TO BE OPTIMIZED')
  850 FORMAT(' *   LET     - DO NOT REDUCE GRADIENTS IN FORCE')
  860 FORMAT(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG')
  870 FORMAT(' *  DERIV    - PRINT PART OF WORKING IN SUB. DERIV')
  880 FORMAT(' *  FULSCF   - IN SEARCHES, FULL SCF CALCN''S TO BE DONE')
  890 FORMAT(' *  DCART    - PRINT DETAILS OF WORKING IN DCART')
  900 FORMAT(' *  GNORM=   - FLEPO EXIT WHEN GRADIENT NORM BELOW ',G8.3)
  910 FORMAT(' *  FMAT     - PRINT DETAILS OF WORKING IN FMAT')
  920 FORMAT(' *  HCORE    - PRINT DETAILS OF WORKING IN HCORE')
  930 FORMAT(' *  ITER     - PRINT DETAILS OF WORKING IN ITER')
  940 FORMAT(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF')
  950 FORMAT(' *  LINMIN   - PRINT DETAILS OF WORKING IN LINMIN')
  960 FORMAT(' *  LOCMIN   - PRINT DETAILS OF WORKING IN LOCMIN')
  970 FORMAT(' *  STEP1    - FIRST  STEP-SIZE IN GRID =',F7.2)
  980 FORMAT(' *  STEP2    - SECOND STEP-SIZE IN GRID =',F7.2)
  990 FORMAT(' *  BAR=     - REDUCE BAR LENGTH BY A MAX. OF',F7.2)
 1000 FORMAT(' *  DEBUGPULAY-PRINT DETAILS OF WORKING IN PULAY')
 1010 FORMAT(' *  CAMP,KING- THE CAMP-KING CONVERGER TO BE USED')
 1020 FORMAT(' *  EIGS     - PRINT ALL EIGENVALUES IN ITER')
 1030 FORMAT(' *  MOLDAT   - PRINT DETAILS OF WORKING IN MOLDAT')
 1040 FORMAT(' *  HYPERFINE- HYPERFINE COUPLING CONSTANTS TO BE'
     1,' PRINTED')
 1050 FORMAT(' *  OPCI     - PRINT DETAILS OF WORKING IN OPCI')
 1060 FORMAT(' *   PL      - MONITOR CONVERGANCE IN DENSITY MATRIX')
 1070 FORMAT(' *  SEARCH   - PRINT DETAILS OF WORKING IN SEARCH')
 1080 FORMAT(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',I3,' TO BE
     1 FILLED')
 1090 FORMAT(' *  CYCLES=  - DO A MAXIMUM OF ',I4,' CYCLES IN NLLSQ')
 1100 FORMAT(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED')
 1110 FORMAT(' *  ROT      - SYMMETRY NUMBER OF',I3,' SPECIFIED')
 1120 FORMAT(' *  ITRY=    - DO A MAXIMUM OF',I6,' ITERATIONS FOR SCF')
 1130 FORMAT( //10X,' IMPOSSIBLE OPTION REQUESTED,')
 1140 FORMAT(' *  0SCF     - AFTER READING AND PRINTING DATA, STOP')
      END
      LOGICAL FUNCTION WORD(KEYWRD,TESTWD)
      CHARACTER KEYWRD*80, TESTWD*(*)
      WORD=.FALSE.
   10 J=INDEX(KEYWRD,TESTWD)
      IF(J.NE.0)THEN
   20    IF(KEYWRD(J:J).NE.' ')GOTO 30
         J=J+1
         GOTO 20
   30    WORD=.TRUE.
         DO 60 K=J,80
            IF(KEYWRD(K:K).EQ.'='.OR.KEYWRD(K:K).EQ.' ') THEN
C
C     CHECK FOR ATTACHED '=' SIGN
C
               J=K
               IF(KEYWRD(J:J).EQ.'=')GOTO 50
C
C     CHECK FOR SEPARATED '=' SIGN
C
               DO 40 J=K+1,80
                  IF(KEYWRD(J:J).EQ.'=') GOTO 50
   40          IF(KEYWRD(J:J).NE.' ')GOTO 10
C
C    THERE IS NO '=' SIGN ASSOCIATED WITH THIS KEYWORD
C
               GOTO 10
   50          KEYWRD(J:J)=' '
C
C   THERE MUST BE A NUMBER AFTER THE '=' SIGN, SOMEWHERE
C
               GOTO 20
            ENDIF
   60    KEYWRD(K:K)=' '
      ENDIF
      RETURN
      END