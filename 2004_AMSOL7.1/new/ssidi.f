      SUBROUTINE SSIDI(A,LDA,N,KPVT,DET,INERT,WORK,JOB)
      IMPLICIT DOUBLE PRECISION (A-H, O-Z)                              GCL0393
      INTEGER LDA,N,JOB
      DIMENSION A(LDA,1),WORK(1), DET(2)                                GCL0393
C     DOUBLE PRECISION A(LDA,1),WORK(1)
C     DOUBLE PRECISION DET(2)
      INTEGER KPVT(1),INERT(3)
C
C    LINPACK ROUTINE DSIDI CONVERTED TO SSIDI 03/25/92 BY GCL
C
C     DSIDI COMPUTES THE DETERMINANT, INERTIA AND INVERSE
C     OF A DOUBLE PRECISION SYMMETRIC MATRIX USING THE FACTORS FROM
C     DSIFA.
C
C     ON ENTRY
C
C        A       DOUBLE PRECISION(LDA,N)
C                THE OUTPUT FROM DSIFA.
C
C        LDA     INTEGER
C                THE LEADING DIMENSION OF THE ARRAY A.
C
C        N       INTEGER
C                THE ORDER OF THE MATRIX A.
C
C        KPVT    INTEGER(N)
C                THE PIVOT VECTOR FROM DSIFA.
C
C        WORK    DOUBLE PRECISION(N)
C                WORK VECTOR.  CONTENTS DESTROYED.
C
C        JOB     INTEGER
C                JOB HAS THE DECIMAL EXPANSION  ABC  WHERE
C                   IF  C .NE. 0, THE INVERSE IS COMPUTED,
C                   IF  B .NE. 0, THE DETERMINANT IS COMPUTED,
C                   IF  A .NE. 0, THE INERTIA IS COMPUTED.
C
C                FOR EXAMPLE, JOB = 111  GIVES ALL THREE.
C
C     ON RETURN
C
C        VARIABLES NOT REQUESTED BY JOB ARE NOT USED.
C
C        A      CONTAINS THE UPPER TRIANGLE OF THE INVERSE OF
C               THE ORIGINAL MATRIX.  THE STRICT LOWER TRIANGLE
C               IS NEVER REFERENCED.
C
C        DET    DOUBLE PRECISION(2)
C               DETERMINANT OF ORIGINAL MATRIX.
C               DETERMINANT = DET(1) * 10.0**DET(2)
C               WITH 1.0 .LE. DABS(DET(1)) .LT. 10.0
C               OR DET(1) = 0.0.
C
C        INERT  INTEGER(3)
C               THE INERTIA OF THE ORIGINAL MATRIX.
C               INERT(1)  =  NUMBER OF POSITIVE EIGENVALUES.
C               INERT(2)  =  NUMBER OF NEGATIVE EIGENVALUES.
C               INERT(3)  =  NUMBER OF ZERO EIGENVALUES.
C
C     ERROR CONDITION
C
C        A DIVISION BY ZERO MAY OCCUR IF THE INVERSE IS REQUESTED
C        AND  DSICO  HAS SET RCOND .EQ. 0.0
C        OR  DSIFA  HAS SET  INFO .NE. 0 .
C
C     LINPACK. THIS VERSION DATED 08/14/78 .
C     JAMES BUNCH, UNIV. CALIF. SAN DIEGO, ARGONNE NAT. LAB
C
C     SUBROUTINES AND FUNCTIONS
C
C     BLAS SAXPY,SCOPY,SDOT,DSWAP
C     FORTRAN DABS,IABS,MOD
C
C     INTERNAL VARIABLES.
C
C     DOUBLE PRECISION AKKP1,SDOT,TEMP
C     DOUBLE PRECISION TEN,D,T,AK,AKP1
      INTEGER J,JB,K,KM1,KS,KSTEP
      LOGICAL NOINV,NODET,NOERT
C
      NOINV = MOD(JOB,10) .EQ. 0
      NODET = MOD(JOB,100)/10 .EQ. 0
      NOERT = MOD(JOB,1000)/100 .EQ. 0
C
      IF (NODET .AND. NOERT) GO TO 140
         IF (NOERT) GO TO 10
            INERT(1) = 0
            INERT(2) = 0
            INERT(3) = 0
   10    CONTINUE
         IF (NODET) GO TO 20
            DET(1) = 1.0D0
            DET(2) = 0.0D0
            TEN = 10.0D0
   20    CONTINUE
         T = 0.0D0
         DO 130 K = 1, N
            D = A(K,K)
C
C           CHECK IF 1 BY 1
C
            IF (KPVT(K) .GT. 0) GO TO 50
C
C              2 BY 2 BLOCK
C              USE DET (D  S)  =  (D/T * C - T) * T  ,  T = DABS(S)
C                      (S  C)
C              TO AVOID UNDERFLOW/OVERFLOW TROUBLES.
C              TAKE TWO PASSES THROUGH SCALING.  USE  T  FOR FLAG.
C
               IF (T .NE. 0.0D0) GO TO 30
                  T = ABS(A(K,K+1))                                     GCL0393
                  D = (D/T)*A(K+1,K+1) - T
               GO TO 40
   30          CONTINUE
                  D = T
                  T = 0.0D0
   40          CONTINUE
   50       CONTINUE
C
            IF (NOERT) GO TO 60
               IF (D .GT. 0.0D0) INERT(1) = INERT(1) + 1
               IF (D .LT. 0.0D0) INERT(2) = INERT(2) + 1
               IF (D .EQ. 0.0D0) INERT(3) = INERT(3) + 1
   60       CONTINUE
C
            IF (NODET) GO TO 120
               DET(1) = D*DET(1)
               IF (DET(1) .EQ. 0.0D0) GO TO 110
   70             IF (ABS(DET(1)) .GE. 1.0D0) GO TO 80                  GCL0393
                     DET(1) = TEN*DET(1)
                     DET(2) = DET(2) - 1.0D0
                  GO TO 70
   80             CONTINUE
   90             IF (ABS(DET(1)) .LT. TEN) GO TO 100                   GCL0393
                     DET(1) = DET(1)/TEN
                     DET(2) = DET(2) + 1.0D0
                  GO TO 90
  100             CONTINUE
  110          CONTINUE
  120       CONTINUE
  130    CONTINUE
  140 CONTINUE
C
C     COMPUTE INVERSE(A)
C
      IF (NOINV) GO TO 270
         K = 1
  150    IF (K .GT. N) GO TO 260
            KM1 = K - 1
            IF (KPVT(K) .LT. 0) GO TO 180
C
C              1 BY 1
C
               A(K,K) = 1.0D0/A(K,K)
               IF (KM1 .LT. 1) GO TO 170
                  CALL SCOPY(KM1,A(1,K),1,WORK,1)
                  DO 160 J = 1, KM1
                     A(J,K) = SDOT(J,A(1,J),1,WORK,1)
                     CALL SAXPY(J-1,WORK(J),A(1,J),1,A(1,K),1)
  160             CONTINUE
                  A(K,K) = A(K,K) + SDOT(KM1,WORK,1,A(1,K),1)
  170          CONTINUE
               KSTEP = 1
            GO TO 220
  180       CONTINUE
C
C              2 BY 2
C
               T = ABS(A(K,K+1))                                        GCL0393
               AK = A(K,K)/T
               AKP1 = A(K+1,K+1)/T
               AKKP1 = A(K,K+1)/T
               D = T*(AK*AKP1 - 1.0D0)
               A(K,K) = AKP1/D
               A(K+1,K+1) = AK/D
               A(K,K+1) = -AKKP1/D
               IF (KM1 .LT. 1) GO TO 210
                  CALL SCOPY(KM1,A(1,K+1),1,WORK,1)
                  DO 190 J = 1, KM1
                     A(J,K+1) = SDOT(J,A(1,J),1,WORK,1)
                     CALL SAXPY(J-1,WORK(J),A(1,J),1,A(1,K+1),1)
  190             CONTINUE
                  A(K+1,K+1) = A(K+1,K+1) + SDOT(KM1,WORK,1,A(1,K+1),1)
                  A(K,K+1) = A(K,K+1) + SDOT(KM1,A(1,K),1,A(1,K+1),1)
                  CALL SCOPY(KM1,A(1,K),1,WORK,1)
                  DO 200 J = 1, KM1
                     A(J,K) = SDOT(J,A(1,J),1,WORK,1)
                     CALL SAXPY(J-1,WORK(J),A(1,J),1,A(1,K),1)
  200             CONTINUE
                  A(K,K) = A(K,K) + SDOT(KM1,WORK,1,A(1,K),1)
  210          CONTINUE
               KSTEP = 2
  220       CONTINUE
C
C           SWAP
C
            KS = ABS(KPVT(K))                                           GCL0393
            IF (KS .EQ. K) GO TO 250
               CALL DSWAP(KS,A(1,KS),1,A(1,K),1)
               DO 230 JB = KS, K
                  J = K + KS - JB
                  TEMP = A(J,K)
                  A(J,K) = A(KS,J)
                  A(KS,J) = TEMP
  230          CONTINUE
               IF (KSTEP .EQ. 1) GO TO 240
                  TEMP = A(KS,K+1)
                  A(KS,K+1) = A(K,K+1)
                  A(K,K+1) = TEMP
  240          CONTINUE
  250       CONTINUE
            K = K + KSTEP
         GO TO 150
  260    CONTINUE
  270 CONTINUE
      RETURN
      END
