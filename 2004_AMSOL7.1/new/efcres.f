      SUBROUTINE EFCRES(TSTEP,FUNCT,GRAD,XPARAM,MSTEP,NSTEP,
     1                  IPOW,NHFLAG)
      IMPLICIT DOUBLE PRECISION(A-H,O-Z)
      INCLUDE 'KEYS.i'
      INCLUDE 'SIZES.i'
       INCLUDE 'FFILES.i'                                               GDH1095
C******************************************************************************
C
C     THIS SUBROUTINE WRITES 'RESTART' FILES WHEN DOING A TRUES/CALC RUN
C     SO THAT THE SOLVATION PORTION CAN BEGIN WITH THE GAS PHASE DENSITY
C     AND HESSIAN MATRICES.
C
C     SOME VARIABLES NEED TO BE RESET SINCE THESE ARE NOT AN ACTUAL
C     'RESTART', BUT WILL BE USED FOR THE SOLVATION PORTION.
C
C     NCALF=1 IF THE SOLVATION PORTION OF A TRUES/CALC CALCULATION
C             SHOULD READ RESTART FILES FROM THE GAS-PHASE PORTION.
C
C     CREATED BY DJG 1095
C                                                                             
C******************************************************************************
      COMMON /OPTIMI / IMP,IMP0         
      COMMON /NLLCOM/ HESS(MAXPAR,MAXPAR),BMAT(MAXPAR,MAXPAR),
     1                PMAT(MAXPAR,MAXPAR)
      COMMON /TRCACM/ NCALF
      DIMENSION GRAD(MAXPAR),XPARAM(*),IPOW(9)
      NCALF=1
      IPOW(9)=2
      NSTEP=0
      TSTEP=0.0D0
      NHFLAG=0
      WRITE(JOUT,100)
      CALL EFSAV(TSTEP,HESS,FUNCT,GRAD,XPARAM,PMAT,MSTEP,NSTEP,
     1           BMAT,IPOW,NHFLAG)
      IF (ISTOP.NE.0) RETURN                                            GDH1095
      IF (IRESTA.EQ.-1) IRESTA=0
      CALL RESINP(FUNCT)
100   FORMAT(/,13X,'WRITING RESTART FILES FOR USE IN SOLVATION ',
     1       'CALCULATION',/)
      RETURN
      END
