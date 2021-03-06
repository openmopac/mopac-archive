      SUBROUTINE WRTKEY(KEYWRD)
      CHARACTER*80 KEYWRD
      LOGICAL UHF, SING, TRIP, BIRAD, EXCI, CI, SPIN
      IF (INDEX(KEYWRD,'VECT') .NE. 0 ) WRITE(6,100)
      IF (INDEX(KEYWRD,'DENSITY') .NE. 0 ) WRITE(6,110)
      IF (INDEX(KEYWRD,'SPIN') .NE. 0 ) WRITE(6,120)
      IF (INDEX(KEYWRD,'TIMES') .NE. 0 ) WRITE(6,130)
      IF (INDEX(KEYWRD,'FLEPO') .NE. 0 ) WRITE(6,140)
      IF (INDEX(KEYWRD,'BONDS') .NE. 0 ) WRITE(6,150)
      IF (INDEX(KEYWRD,'GEO-OK') .NE. 0 ) WRITE(6,160)
      IF (INDEX(KEYWRD,'FOCK') .NE. 0 ) WRITE(6,170)
      IF (INDEX(KEYWRD,'1ELEC') .NE. 0 ) WRITE(6,180)
      IF (INDEX(KEYWRD,'LOCAL') .NE. 0 ) WRITE(6,190)
      IF (INDEX(KEYWRD,'DEBUG') .NE. 0 ) WRITE(6,200)
      IF (INDEX(KEYWRD,'RESTART') .NE. 0 ) WRITE(6,210)
      IF (INDEX(KEYWRD,'CHARGE') .NE. 0 )
     + WRITE(6,220)NINT(READA(KEYWRD,INDEX(KEYWRD,'CHARGE')))
      IF (INDEX(KEYWRD,'GRAD') .NE. 0 ) WRITE(6,230)
      UHF=(INDEX(KEYWRD,'UHF') .NE. 0 ) 
      IF(UHF)WRITE(6,240)
      BIRAD=(INDEX(KEYWRD,'BIRAD') .NE. 0 ) 
      IF(BIRAD)WRITE(6,247)
      EXCI=(INDEX(KEYWRD,'EXCITED') .NE. 0 ) 
      IF(EXCI) WRITE(6,250)
      TRIP=(INDEX(KEYWRD,'TRIPLET') .NE. 0 ) 
      IF(TRIP)WRITE(6,260)
      IF (INDEX(KEYWRD,'SYM') .NE. 0 ) WRITE(6,270)
      IF (INDEX(KEYWRD,' T=') .NE. 0 ) WRITE(6,280)
     + READA(KEYWRD,INDEX(KEYWRD,' T='))
      IF (INDEX(KEYWRD,'1SCF') .NE. 0 ) WRITE(6,290)
      CI=(INDEX(KEYWRD,'C.I.').NE.0) 
      IF(CI)WRITE(6,300)
      IF (INDEX(KEYWRD,'FORCE') .NE. 0 ) WRITE(6,320)
      IF (INDEX(KEYWRD,'MINDO3') .NE. 0 ) WRITE(6,330)
      IF (INDEX(KEYWRD,'PREC') .NE. 0 ) WRITE(6,340)
      IF (INDEX(KEYWRD,'NOINTER') .NE. 0 ) WRITE(6,350)
      IF (INDEX(KEYWRD,'ISOTOPE') .NE. 0 ) WRITE(6,352)
      IF (INDEX(KEYWRD,'DENOUT') .NE. 0 ) WRITE(6,355)
      IF (INDEX(KEYWRD,'SHIFT') .NE. 0 ) WRITE(6,360)
     + READA(KEYWRD,INDEX(KEYWRD,'SHIFT'))
      IF (INDEX(KEYWRD,'OLDENS') .NE. 0 ) WRITE(6,370)
      IF (INDEX(KEYWRD,'SCFCRT') .NE. 0 ) WRITE(6,380)
     + READA(KEYWRD,INDEX(KEYWRD,'SCFCRT'))
      IF (INDEX(KEYWRD,'ENPART') .NE. 0 ) WRITE(6,390)
      IF (INDEX(KEYWRD,'NOXYZ') .NE. 0 ) WRITE(6,400)
      IF (INDEX(KEYWRD,'SIGMA') .NE. 0 ) WRITE(6,410)
      IF (INDEX(KEYWRD,'NLLSQ') .NE. 0 ) WRITE(6,415)
      IF (INDEX(KEYWRD,'ROOT') .NE. 0 ) WRITE(6,420)
     + READA(KEYWRD,INDEX(KEYWRD,'ROOT'))
      IF (INDEX(KEYWRD,'TRANS') .NE. 0 ) WRITE(6,430)
      IF (INDEX(KEYWRD,'SADDLE') .NE. 0 ) WRITE(6,440)
      IF (INDEX(KEYWRD,' LET') .NE. 0 ) WRITE(6,450)
      IF (INDEX(KEYWRD,'COMPFG') .NE. 0 ) WRITE(6,460)
      IF (INDEX(KEYWRD,'DERIV') .NE. 0 ) WRITE(6,470)
      IF (INDEX(KEYWRD,'FULSCF') .NE. 0 ) WRITE(6,480)
      IF (INDEX(KEYWRD,'DCART') .NE. 0 ) WRITE(6,490)
      IF (INDEX(KEYWRD,'GNORM=') .NE. 0 ) WRITE(6,500)
     + READA(KEYWRD,INDEX(KEYWRD,'GNORM='))
      IF (INDEX(KEYWRD,'FMAT') .NE. 0 ) WRITE(6,510)
      IF (INDEX(KEYWRD,'HCORE') .NE. 0 ) WRITE(6,520)
      IF (INDEX(KEYWRD,'ITER') .NE. 0 ) WRITE(6,530)
      IF (INDEX(KEYWRD,'PULAY') .NE. 0 ) WRITE(6,540)
      IF (INDEX(KEYWRD,'LINMIN') .NE. 0 ) WRITE(6,550)
      IF (INDEX(KEYWRD,'LOCMIN') .NE. 0 ) WRITE(6,560)
      IF (INDEX(KEYWRD,'BAR=') .NE. 0 ) WRITE(6,570)
      IF (INDEX(KEYWRD,'FILL=') .NE. 0 ) WRITE(6,580)
     + INT(READA(KEYWRD,INDEX(KEYWRD,'FILL')))
      IF(UHF)THEN
          IF(BIRAD.OR.EXCI.OR.CI)THEN
              WRITE(6,'(//10X,'' IMPOSSIBLE OPTION REQUESTED,'')')
              GOTO 10
          ENDIF
      ELSE
          IF(EXCI.AND. TRIP) THEN
              WRITE(6,'(//10X,'' IMPOSSIBLE OPTION REQUESTED,'')')
              GOTO 10
          ENDIF
      ENDIF
      IF (INDEX(KEYWRD,'THERMO') .NE. 0 )THEN
      WRITE(6,590)
      I=INDEX(KEYWRD,' ROT')
      IF(I.NE.0) THEN
          WRITE(6,600)INT(READA(KEYWRD,I))
      ELSE
          WRITE(6,'
     +    (//10X,'' YOU MUST SUPPLY THE SYMMETRY NUMBER "ROT"'')')
          STOP
      ENDIF
      ENDIF
      RETURN
  10  WRITE(6,'(//10X,'' CALCULATION ABANDONED, SORRY!'')')
      STOP
  100 FORMAT(' *  VECTORS  - FINAL EIGENVECTORS TO BE PRINTED')
  110 FORMAT(' *  DENSITY  - FINAL DENSITY MATRIX TO BE PRINTED')
  120 FORMAT(' *  SPIN     - FINAL UHF SPIN MATRIX TO BE PRINTED')
  130 FORMAT(' *  TIMES    - TIMES OF VARIOUS STAGES TO BE PRINTED')
  140 FORMAT(' *  FLEPO    - PRINT DETAILS OF GEOMETRY OPTIMISATION')
  150 FORMAT(' *  BONDS    - FINAL BOND-ORDER MATRIX TO BE PRINTED')
  160 FORMAT(' *  GEO-OK   - OVERRIDE INTERATOMIC DISTANCE CHECK')
  170 FORMAT(' *  FOCK     - LAST FOCK MATRIX TO BE PRINTED')
  180 FORMAT(' *  1ELECTRON- FINAL ONE-ELECTRON MATRIX TO BE PRINTED')
  190 FORMAT(' *  LOCALISE - LOCALISED ORBITALS TO BE PRINTED')
  200 FORMAT(' *  DEBUG    - DEBUG OPTION TURNED ON')
  210 FORMAT(' *  RESTART  - CALCULATION RESTARTED')
  220 FORMAT(3(' *',/),' *',15X,'  CHARGE ON SYSTEM =',I3,3(/,' *'))     
  230 FORMAT(' *  GRADIENTS- ALL GRADIENTS TO BE PRINTED')
  240 FORMAT(' *  UHF      - UNRESTRICTED HARTREE-FOCK CALCULATION')    
  245 FORMAT(' *  SINGLET  - STATE REQUIRED MUST BE A SINGLET')
  247 FORMAT(' *  BIRADICAL- SYSTEM HAS TWO UNPAIRED ELECTRONS')                          
  250 FORMAT(' *  EXCITED  - FIRST EXCITED STATE IS TO BE OPTIMISED')                          
  260 FORMAT(' *  TRIPLET  - TRIPLET')                                  
  270 FORMAT(' *  SYMMETRY - SYMMETRY CONDITIONS TO BE IMPOSED')        
  280 FORMAT(' *   T=      - A TIME OF',F8.1,' SECONDS REQUESTED')        
  290 FORMAT(' *  1SCF     - DO 1 SCF AND THEN STOP ')
  300 FORMAT(' *  C.I.     - A SINGLET C.I. SPECIFIED')        
  320 FORMAT(' *  FORCE    - FORCE CALCULATION SPECIFIED')
  330 FORMAT(' *  MINDO3   - THE MINDO/3 HAMILTONIAN TO BE USED')
  340 FORMAT(' *  PRECISE  - CRITERIA TO BE INCREASED BY 100 TIMES')
  350 FORMAT(' *  NOINTER  - INTERATOMIC DISTANCES NOT TO BE PRINTED')
  352 FORMAT(' *  ISOTOPE  - FORCE MATRIX WRITTEN TO DISK (CHAN. 9 )')
  355 FORMAT(' *  DENOUT   - DENSITY MATRIX OUTPUT ON CHANNEL 10')
  360 FORMAT(' *  SHIFT    - A DAMPING FACTOR OF',F8.2,' DEFINED')
  370 FORMAT(' *  OLDENS   - INITIAL DENSITY MATRIX READ OF DISK')
  380 FORMAT(' *  SCFCRT   - DEFAULT SCF CRITERION REPLACED BY',F12.8)
  390 FORMAT(' *  ENPART   - ENERGY TO BE PARTITIONED INTO COMPONENTS')
  400 FORMAT(' *  NOXYZ    - CARTESIAN COORDINATES NOT TO BE PRINTED')
  410 FORMAT(' *  SIGMA    - GEOMETRY TO BE OPTIMISED USING SIGMA.')
  415 FORMAT(' *  NLLSQ    - GRADIENTS TO BE MINIMISED USING NLLSQ.')
  420 FORMAT(' *  ROOT     - IN A C.I. CALCULATION, ROOT',I2,
     +                       ' TO BE OPTIMISED.')
  430 FORMAT(' *  TRANS    - THE REACTION VIBRATION TO BE DELETED FROM',
     +' THE THERMO CALCULATION')
  440 FORMAT(' *  SADDLE   - TRANSITION STATE TO BE OPTIMISED')
  450 FORMAT(' *   LET     - DO NOT REDUCE GRADIENTS IN FORCE')
  460 FORMAT(' *  COMPFG   - PRINT HEAT OF FORMATION CALC''D IN COMPFG')
  470 FORMAT(' *  DERIV    - PRINT PART OF WORKING IN SUB. DERIV')
  480 FORMAT(' *  FULSCF   - IN SEARCHES, FULL SCF CALCN''S TO BE DONE')
  490 FORMAT(' *  DCART    - PRINT DETAILS OF WORKING IN DCART')
  500 FORMAT(' *  GNORM=   - FLEPO EXIT WHEN GRADIENT NORM BELOW',F9.3)
  510 FORMAT(' *  FMAT     - PRINT DETAILS OF WORKING IN FMAT')
  520 FORMAT(' *  HCORE    - PRINT DETAILS OF WORKING IN HCORE')
  530 FORMAT(' *  ITER     - PRINT DETAILS OF WORKING IN ITER')
  540 FORMAT(' *  PULAY    - PULAY''S METHOD TO BE USED IN SCF')
  550 FORMAT(' *  LINMIN   - PRINT DETAILS OF WORKING IN LINMIN')
  560 FORMAT(' *  LOCMIN   - PRINT DETAILS OF WORKING IN LOCMIN')
  570 FORMAT(' *  BAR=     - REDUCE BAR LENGTH BY A MAX. OF',F7.2)
  580 FORMAT(' *  FILL=    - IN RHF CLOSED SHELL, FORCE M.O.',I3,' TO BE
     + FILLED')
  590 FORMAT(' *  THERMO   - THERMODYNAMIC QUANTITIES TO BE CALCULATED')
  600 FORMAT(' *  ROT      - SYMMETRY NUMBER OF',I3,' SPECIFIED')
      END
