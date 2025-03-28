      SUBROUTINE SCHMIB(U,N,NDIM,*)                                     CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
C
C
C     SAME AS SCHMIDT BUT WORKS FROM RIGHT TO LEFT.
C
      DIMENSION U(NDIM,NDIM)
CSAV         SAVE                                                           GL0892
      DATA ZERO,SMALL,ONE/0.0,0.01,1.0/
      N1=N+1
      II=0
      DO 110 K=1,N
         K1=K-1
C
C     NORMALIZE KTH COLUMN VECTOR
C
         DOT = ZERO
         DO 10 I=1,N
   10    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 20 I=1,N
   20    U(I,N1-K)=SCALE*U(I,N1-K)
   30    IF(K1.EQ.0) GO TO 110
         NPASS=0
C
C     PROJECT OUT K-1 PREVIOUS ORTHONORMAL VECTORS FROM KTH VECTOR
C
   40    NPASS=NPASS+1
         DO 70 J=1,K1
            DOT=ZERO
            DO 50 I=1,N
   50       DOT=DOT+U(I,N1-J)*U(I,N1-K)
            DO 60 I=1,N
   60       U(I,N1-K)=U(I,N1-K)-DOT*U(I,N1-J)
   70    CONTINUE
C
C     SECOND NORMALIZATION (AFTER PROJECTION)
C     IF KTH VECTOR IS SMALL BUT NOT ZERO THEN NORMALIZE
C     AND PROJECT AGAIN TO CONTROL ROUND-OFF ERRORS.
C
         DOT=ZERO
         DO 80 I=1,N
   80    DOT=DOT+U(I,N1-K)*U(I,N1-K)
         IF(DOT.EQ.ZERO) GO TO 100
         IF(DOT.LT.SMALL.AND.NPASS.GT.2) GO TO 100
         SCALE=ONE/SQRT(DOT)
         DO 90 I=1,N
   90    U(I,N1-K)=SCALE*U(I,N1-K)
         IF(DOT.LT.SMALL) GO TO 40
         GO TO 110
C
C     REPLACE LINEARLY DEPENDENT KTH VECTOR BY A UNIT VECTOR.
C
  100    II=II+1
C     IF(II.GT.N) RETURN 1                                               CSTP (stop)
         U(II,N1-K)=ONE
         GO TO 30
  110 CONTINUE
      RETURN
 9999 RETURN 1                                                          CSTP
      END
