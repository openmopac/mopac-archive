      SUBROUTINE WRTKEY(KEYWRD)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      CHARACTER*80 KEYWRD
      LOGICAL UHF, TRIP, BIRAD, EXCI, CI
      LOGICAL AM1, MNDO, MINDO3
      CHARACTER SPACE*1, DOT*1, ZERO*1, NINE*1, CH*1
      DATA SPACE,DOT,ZERO,NINE /' ','.','0','9'/
      IF (INDEX(KEYWRD,'VECT') .NE. 0 ) WRITE(6,40)
      IF (INDEX(KEYWRD,'DENSITY') .NE. 0 ) WRITE(6,50)
      IF (INDEX(KEYWRD,'SPIN') .NE. 0 ) WRITE(6,60)
      IF (INDEX(KEYWRD,'TIMES') .NE. 0 ) WRITE(6,70)
      IF (INDEX(KEYWRD,'FLEPO') .NE. 0 ) WRITE(6,80)
      IF (INDEX(KEYWRD,'BONDS') .NE. 0 ) WRITE(6,90)
      IF (INDEX(KEYWRD,'GEO-OK') .NE. 0 ) WRITE(6,100)
      IF (INDEX(KEYWRD,'FOCK') .NE. 0 ) WRITE(6,110)
      IF (INDEX(KEYWRD,'1ELEC') .NE. 0 ) WRITE(6,120)
      IF (INDEX(KEYWRD,' ESR') .NE. 0 ) WRITE(6,130)
      IF (INDEX(KEYWRD,' DRC') .NE. 0 ) WRITE(6,140)
      IF (INDEX(KEYWRD,'LOCAL') .NE. 0 ) WRITE(6,170)
      IF (INDEX(KEYWRD,'MULLIK') .NE. 0 ) WRITE(6,180)
      IF (INDEX(KEYWRD,' XYZ') .NE. 0 ) WRITE(6,190)
      IF (INDEX(KEYWRD,' PI') .NE. 0 ) WRITE(6,200)
      IF (INDEX(KEYWRD,'ECHO') .NE. 0 ) WRITE(6,210)
      IF (INDEX(KEYWRD, 'SING') .NE. 0 ) WRITE(6,310)
      IF (INDEX(KEYWRD, 'DOUB') .NE. 0 ) WRITE(6,320)
      IF (INDEX(KEYWRD, 'QUAR') .NE. 0 ) WRITE(6,340)
      IF (INDEX(KEYWRD, 'QUIN') .NE. 0 ) WRITE(6,350)
      IF (INDEX(KEYWRD, 'SEXT') .NE. 0 ) WRITE(6,360)
      IF (INDEX(KEYWRD,'POLAR') .NE. 0 ) WRITE(6,220)
      IF (INDEX(KEYWRD,'DEBUG') .NE. 0 ) WRITE(6,230)
      IF (INDEX(KEYWRD,'RESTART') .NE. 0 ) WRITE(6,240)
      IF (INDEX(KEYWRD,'CHARGE') .NE. 0 )
     1 WRITE(6,250)NINT(READA(KEYWRD,INDEX(KEYWRD,'CHARGE')))
      IF (INDEX(KEYWRD,'GRAD') .NE. 0 ) WRITE(6,260)
      UHF=(INDEX(KEYWRD,'UHF') .NE. 0 )
      IF(UHF)WRITE(6,270)
      BIRAD=(INDEX(KEYWRD,'BIRAD') .NE. 0 )
      IF(BIRAD)WRITE(6,290)
      EXCI=(INDEX(KEYWRD,'EXCITED') .NE. 0 )
      IF(EXCI) WRITE(6,300)
      TRIP=(INDEX(KEYWRD,'TRIPLET') .NE. 0 )
      IF(TRIP)WRITE(6,330)
      IF (INDEX(KEYWRD,'SYM') .NE. 0 ) WRITE(6,370)
      I=INDEX(KEYWRD,'OPEN(')
      IF(I.NE.0)THEN
         IELEC=READA(KEYWRD,I)
         ILEVEL=READA(KEYWRD,I+7)
         WRITE(6,390)IELEC,ILEVEL
      ENDIF
      I=INDEX(KEYWRD,'MICROS')
      IF(I.NE.0)THEN
         I=READA(KEYWRD,I)
         WRITE(6,380)I
      ENDIF
      I=INDEX(KEYWRD,'DRC=')
      IF(I.NE.0)THEN
         SUM=READA(KEYWRD,I)
         WRITE(6,150)SUM
      ENDIF
      I=INDEX(KEYWRD,'KINE')
      IF(I.NE.0)THEN
         SUM=READA(KEYWRD,I)
         WRITE(6,160)SUM
      ENDIF
      I=INDEX(KEYWRD,' T=')
      IF(I.NE.0) THEN
         TIME=READA(KEYWRD,I)
         DO 10 J=I+3,80
            CH=KEYWRD(J:J)
            IF( CH .NE. DOT .AND. (CH .LT. ZERO .OR. CH .GT. NINE)) THEN
               IF( CH .EQ. 'M') TIME=TIME*60
               GOTO 20
            ENDIF
   10    CONTINUE
   20    CONTINUE
         WRITE(6,400)TIME
      ENDIF
      IF (INDEX(KEYWRD,'1SCF') .NE. 0 ) WRITE(6,410)
      CI=(INDEX(KEYWRD,'C.I.').NE.0)
      I=INDEX(KEYWRD,'C.I.=')
      IF(I.NE.0)THEN
         I=READA(KEYWRD,I+5)
         WRITE(6,420)I
      ENDIF
      IF (INDEX(KEYWRD,'FORCE') .NE. 0 ) WRITE(6,430)
      IF (INDEX(KEYWRD,'MINDO') .NE. 0 ) THEN
         WRITE(6,440)
         MINDO3=.TRUE.
      ENDIF
      IF (INDEX(KEYWRD,'AM1') .NE. 0 ) THEN
         WRITE(6,450)
         AM1=.TRUE.
      ENDIF
      IF (INDEX(KEYWRD,'MNDO') .NE. 0 ) THEN
         MNDO=.TRUE.
      ENDIF
      IF (INDEX(KEYWRD,'PREC') .NE. 0 ) WRITE(6,460)
      IF (INDEX(KEYWRD,'NOINTER') .NE. 0 ) WRITE(6,470)
      IF (INDEX(KEYWRD,'ISOTOPE') .NE. 0 ) WRITE(6,480)
      IF (INDEX(KEYWRD,'DENOUT') .NE. 0 ) WRITE(6,490)
      IF (INDEX(KEYWRD,'SHIFT') .NE. 0 ) WRITE(6,500)
     1 READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
      IF (INDEX(KEYWRD,'OLDENS') .NE. 0 ) WRITE(6,510)
      IF (INDEX(KEYWRD,'SCFCRT') .NE. 0 ) WRITE(6,520)
     1 READA(KEYWRD,INDEX(KEYWRD,'SCFCRT'))
      IF (INDEX(KEYWRD,'ENPART') .NE. 0 ) WRITE(6,530)
      IF (INDEX(KEYWRD,'NOXYZ') .NE. 0 ) WRITE(6,540)
      IF (INDEX(KEYWRD,'SIGMA') .NE. 0 ) WRITE(6,550)
      IF (INDEX(KEYWRD,'NLLSQ') .NE. 0 ) WRITE(6,560)
      IF (INDEX(KEYWRD,'ROOT') .NE. 0 ) WRITE(6,570)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ROOT')))
      IF (INDEX(KEYWRD,'TRANS(') .NE. 0 ) THEN
         WRITE(6,600)
      ELSEIF (INDEX(KEYWRD,'TRANS=') .NE. 0 ) THEN
         WRITE(6,590)NINT(READA(KEYWRD,INDEX(KEYWRD,'TRANS=')))
      ELSEIF (INDEX(KEYWRD,'TRANS(') .NE. 0 ) THEN
         WRITE(6,580)
      ENDIF
      IF (INDEX(KEYWRD,'SADDLE') .NE. 0 ) WRITE(6,610)
      IF (INDEX(KEYWRD,' LET') .NE. 0 ) WRITE(6,620)
      IF (INDEX(KEYWRD,'COMPFG') .NE. 0 ) WRITE(6,630)
      IF (INDEX(KEYWRD,'DERIV') .NE. 0 ) WRITE(6,640)
      IF (INDEX(KEYWRD,'FULSCF') .NE. 0 ) WRITE(6,650)
      IF (INDEX(KEYWRD,'DCART') .NE. 0 ) WRITE(6,660)
      IF (INDEX(KEYWRD,'GNORM=') .NE. 0 ) WRITE(6,670)
     1 READA(KEYWRD,INDEX(KEYWRD,'GNORM='))
      IF (INDEX(KEYWRD,'FMAT') .NE. 0 ) WRITE(6,680)
      IF (INDEX(KEYWRD,'HCORE') .NE. 0 ) WRITE(6,690)
      IF (INDEX(KEYWRD,'ITER') .NE. 0 ) WRITE(6,700)
      IF (INDEX(KEYWRD,'PULAY') .NE. 0 ) WRITE(6,710)
      IF (INDEX(KEYWRD,'LINMIN') .NE. 0 ) WRITE(6,720)
      IF (INDEX(KEYWRD,'LOCMIN') .NE. 0 ) WRITE(6,730)
      IF (INDEX(KEYWRD,'BAR=') .NE. 0 ) WRITE(6,740)
     1 READA(KEYWRD,INDEX(KEYWRD,'BAR='))
      IF (INDEX(KEYWRD,'DEBUGPULAY') .NE. 0 ) WRITE(6,750)
      IF (INDEX(KEYWRD,'CAMP') .NE. 0 ) WRITE(6,760)
      IF (INDEX(KEYWRD,'KING') .NE. 0 ) WRITE(6,760)
      IF (INDEX(KEYWRD,'EIGS') .NE. 0 ) WRITE(6,770)
      IF (INDEX(KEYWRD,'MOLDAT') .NE. 0 ) WRITE(6,780)
      IF (INDEX(KEYWRD,'HYPERF') .NE. 0 ) WRITE(6,790)
      IF (INDEX(KEYWRD,'OPCI') .NE. 0 ) WRITE(6,800)
      IF (INDEX(KEYWRD,' PL') .NE. 0 ) WRITE(6,810)
      IF (INDEX(KEYWRD,'SEARCH') .NE. 0 ) WRITE(6,820)
      IF (INDEX(KEYWRD,'CYCLES=') .NE. 0 ) WRITE(6,840)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'CYCLES=')))
      IF (INDEX(KEYWRD,'FILL=') .NE. 0 ) WRITE(6,830)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'FILL')))
      IF (INDEX(KEYWRD,'ITRY=') .NE. 0 ) WRITE(6,870)
     1 NINT(READA(KEYWRD,INDEX(KEYWRD,'ITRY=')))
      IF (INDEX(KEYWRD,'0SCF') .NE. 0 ) WRITE(6,890)
      IF(UHF)THEN
         IF(BIRAD.OR.EXCI.OR.CI)THEN
            WRITE(6,880)
            GOTO 30
         ENDIF
      ELSE
         IF(EXCI.AND. TRIP) THEN
            WRITE(6,880)
            GOTO 30
         ENDIF
      ENDIF
      IF ( (MINDO3 .AND. AM1) .OR. (MINDO3 .AND. MNDO) .OR.
     1     (MNDO .AND. AM1) )   THEN
         WRITE (6,880)
         GOTO 30
      ENDIF
      IF (INDEX(KEYWRD,'THERMO') .NE. 0 )THEN
         WRITE(6,850)
         I=INDEX(KEYWRD,' ROT')
         IF(I.NE.0) THEN
            WRITE(6,860)NINT(READA(KEYWRD,I))
         ELSE
            WRITE(6,'
     1    (//10X,'' YOU MUST SUPPLY THE SYMMETRY NUMBER "ROT"'')')
            STOP
         ENDIF
      ENDIF
      RETURN
   30 WRITE(6,'(//10X,'' CALCULATION ABANDONED, SORRY!'')')
      STOP
   40 FORMAT(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED')
   50 FORMAT(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED')
   60 FORMAT(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED')
   70 FORMAT(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED')
   80 FORMAT(' *  FLEPO    - PRINT DETAILS OF GEOMETRY OPTIMISATION')
   90 FORMAT(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED')
  100 FORMAT(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK')
  110 FORMAT(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED')
  120 FORMAT(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED')
  130 FORMAT(' *  ESR      - RHF SPIN DENSITY CALCULATION REQUESTED')
  140 FORMAT(' *  DRC      - INTRINSIC REACTION COORDINATE CALCULATION')
  150 FORMAT(' *  DRC=     - HALF-LIFE FOR KINETIC ENERGY LOSS =',F9.2,
     1' * 10**(-14) SECONDS')
  160 FORMAT(' *  KINETIC= - ',F7.3,' KCAL KINETIC ENERGY ADDED TO DRC')
  170 FORMAT(' *  LOCALISE - LOCALISED ORBITALS TO BE PRINTED')
  180 FORMAT(' *  MULLIK   - THE MULLIKEN ANALYSIS TO BE PERFORMED')
  190 FORMAT(' *   XYZ     - CARTESIAN COORDINATE SYSTEM TO BE USED')
  200 FORMAT(' *   PI      - BONDS MATRIX, SPLIT INTO SIGMA-PI-DELL',
     1' COMPONENTS, TO BE PRINTED')
  210 FORMAT(' *  ECHO     - ALL INPUT DATA TO BE ECHOED BEFORE RUN')
  220 FORMAT(' *  POLAR    - CALCULATE THE POLARIZATION VOLUMES')
  230 FORMAT(' *  DEBUG    - DEBUG OPTION TURNED ON')
  240 FORMAT(' *  RESTART  - CALCULATION RESTARTED')
  250 FORMAT(3(' *',/),' *',15X,'  CHARGE ON SYSTEM =',I3,3(/,' *'))
  260 FORMAT(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED')
  270 FORMAT(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION')
  280 FORMAT(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET')
  290 FORMAT(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS')
  300 FORMAT(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMISED')
  310 FORMAT(' *  SINGLET  - SPIN STATE DEFINED AS A SINGLET')
  320 FORMAT(' *  DOUBLET  - SPIN STATE DEFINED AS A DOUBLET')
  330 FORMAT(' *  TRIPLET  - SPIN STATE DEFINED AS A TRIPLET')
  340 FORMAT(' *  QUARTET  - SPIN STATE DEFINED AS A QUARTET')
  350 FORMAT(' *  QUINTET  - SPIN STATE DEFINED AS A QUINTET')
  360 FORMAT(' *  SEXTET   - SPIN STATE DEFINED AS A SEXTET')
  370 FORMAT(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED')
  380 FORMAT(' *  MICROS=N -',I4,' MICROSTATES TO BE SUPPLIED FOR C.I.')
  390 FORMAT(' *  OPEN(N,N)- THERE ARE',I2,' ELECTRONS IN',I2,' LEVELS')
  400 FORMAT(' *   T=      - A TIME OF',F8.1,' SECONDS REQUESTED')
  410 FORMAT(' *  1SCF     - DO 1 SCF AND THEN STOP ')
  420 FORMAT(' *  C.I.=N   -',I2,' M.O.S TO BE USED IN C.I.')
  430 FORMAT(' *  FORCE    - FORCE CALCULATION SPECIFIED')
  440 FORMAT(' *  MINDO/3  - THE MINDO/3 HAMILTONIAN TO BE USED')
  450 FORMAT(' *  AM1      - THE AM1 HAMILTONIAN TO BE USED')
  460 FORMAT(' *  PRECISE  - CRITERIA TO BE INCREASED BY 100 TIMES')
  470 FORMAT(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED')
  480 FORMAT(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )')
  490 FORMAT(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10')
  500 FORMAT(' *  SHIFT    - A DAMPING FACTOR OF',F8.2,' DEFINED')
  510 FORMAT(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK')
  520 FORMAT(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY',F12.8)
  530 FORMAT(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS')
  540 FORMAT(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED')
  550 FORMAT(' *  SIGMA    - GEOMETRY TO BE OPTIMISED USING SIGMA.')
  560 FORMAT(' *  NLLSQ    - GRADIENTS TO BE MINIMISED USING NLLSQ.')
  570 FORMAT(' *  ROOT     - IN A C.I. CALCULATION, ROOT',I2,
     1                       ' TO BE OPTIMISED.')
  580 FORMAT(' *  TRANS    - THE REACTION VIBRATION TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  590 FORMAT(' *  TRANS=   - ',I4,' VIBRATIONS ARE TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  600 FORMAT(' *  TRANS(   - SPECIFIC VIBRATIONS TO BE DELETED FROM',
     1' THE THERMO CALCULATION')
  610 FORMAT(' *  SADDLE   - TRANSITION STATE TO BE OPTIMISED')
  620 FORMAT(' *   LET     - DO NOT REDUCE GRADIENTS IN FORCE')
  630 FORMAT(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG')
  640 FORMAT(' *  DERIV    - PRINT PART OF WORKING IN SUB. DERIV')
  650 FORMAT(' *  FULSCF   - IN SEARCHES, FULL SCF CALCN''S TO BE DONE')
  660 FORMAT(' *  DCART    - PRINT DETAILS OF WORKING IN DCART')
  670 FORMAT(' *  GNORM=   - FLEPO EXIT WHEN GRADIENT NORM BELOW',F9.3)
  680 FORMAT(' *  FMAT     - PRINT DETAILS OF WORKING IN FMAT')
  690 FORMAT(' *  HCORE    - PRINT DETAILS OF WORKING IN HCORE')
  700 FORMAT(' *  ITER     - PRINT DETAILS OF WORKING IN ITER')
  710 FORMAT(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF')
  720 FORMAT(' *  LINMIN   - PRINT DETAILS OF WORKING IN LINMIN')
  730 FORMAT(' *  LOCMIN   - PRINT DETAILS OF WORKING IN LOCMIN')
  740 FORMAT(' *  BAR=     - REDUCE BAR LENGTH BY A MAX. OF',F7.2)
  750 FORMAT(' *  DEBUGPULAY-PRINT DETAILS OF WORKING IN PULAY')
  760 FORMAT(' *  CAMP,KING- THE CAMP-KING CONVERGER TO BE USED')
  770 FORMAT(' *  EIGS     - PRINT ALL EIGENVALUES IN ITER')
  780 FORMAT(' *  MOLDAT   - PRINT DETAILS OF WORKING IN MOLDAT')
  790 FORMAT(' *  HYPERFINE- HYPERFINE COUPLING CONSTANTS TO BE'
     1,' PRINTED')
  800 FORMAT(' *  OPCI     - PRINT DETAILS OF WORKING IN OPCI')
  810 FORMAT(' *   PL      - MONITOR CONVERGANCE IN DENSITY MATRIX')
  820 FORMAT(' *  SEARCH   - PRINT DETAILS OF WORKING IN SEARCH')
  830 FORMAT(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',I3,' TO BE
     1 FILLED')
  840 FORMAT(' *  CYCLES=  - DO A MAXIMUM OF ',I4,' CYCLES IN NLLSQ')
  850 FORMAT(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED')
  860 FORMAT(' *  ROT      - SYMMETRY NUMBER OF',I3,' SPECIFIED')
  870 FORMAT(' *  ITRY=    - DO A MAXIMUM OF',I6,' ITERATIONS FOR SCF')
  880 FORMAT( //10X,' IMPOSSIBLE OPTION REQUESTED,')
  890 FORMAT(' *  0SCF     - AFTER READING AND PRINTING DATA, STOP')
      END