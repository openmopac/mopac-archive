      SUBROUTINE SETUPG(*)                                              CSTP
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
C
      INCLUDE 'SIZES.i'
C
      include 'pmodsb.f'   ! Common block definition for /PMODSB/.      LF1010
C
      COMMON /EXPONT/ ZS(120),ZP(120),ZD(120)
      COMMON /NATYPE/ NZTYPE(120),MTYPE(30),LTYPE
      COMMON /TEMP/  CC(60,6),ZZ(60,6)
      COMMON /TEMP2/ CCSPEC(10,6), ZZSPEC(10,6)                         LF1010
      COMMON /HPUSED/ HPUSED                                            LF0410
      LOGICAL  HPUSED                                                   LF0410
      COMMON /DOPRNT/ DOPRNT                                            LF0510
      LOGICAL DOPRNT                                                    LF0510
      DIMENSION ALLC(6,5,2), ALLZ(6,5,2)
CSAV         SAVE                                                           GL0892
C     SET-UP THE STEWART'S STO-6G EXPANSIONS
C                                            1S
      ALLZ(1,1,1) =2.310303149D01
      ALLZ(2,1,1) =4.235915534D00
      ALLZ(3,1,1) =1.185056519D00
      ALLZ(4,1,1) =4.070988982D-01
      ALLZ(5,1,1) =1.580884151D-01
      ALLZ(6,1,1) =6.510953954D-02
C
      ALLC(1,1,1) =9.163596280D-03
      ALLC(2,1,1) =4.936149294D-02
      ALLC(3,1,1) =1.685383049D-01
      ALLC(4,1,1) =3.705627997D-01
      ALLC(5,1,1) =4.164915298D-01
      ALLC(6,1,1) =1.303340841D-01
C                                      2S
      ALLZ(1,2,1) =2.768496241D01
      ALLZ(2,2,1) =5.077140627D00
      ALLZ(3,2,1) =1.426786050D00
      ALLZ(4,2,1) =2.040335729D-01
      ALLZ(5,2,1) =9.260298399D-02
      ALLZ(6,2,1) =4.416183978D-02
C
      ALLC(1,2,1) =-4.151277819D-03
      ALLC(2,2,1) =-2.067024148D-02
      ALLC(3,2,1) =-5.150303337D-02
      ALLC(4,2,1) =3.346271174D-01
      ALLC(5,2,1) =5.621061301D-01
      ALLC(6,2,1) =1.712994697D-01
C                                     2P
      ALLZ(1,2,2) =5.868285913D00
      ALLZ(2,2,2) =1.530329631D00
      ALLZ(3,2,2) =5.475665231D-01
      ALLZ(4,2,2) =2.288932733D-01
      ALLZ(5,2,2) =1.046655969D-01
      ALLZ(6,2,2) =4.948220127D-02
C
      ALLC(1,2,2) =7.924233646D-03
      ALLC(2,2,2) =5.144104825D-02
      ALLC(3,2,2) =1.898400060D-01
      ALLC(4,2,2) =4.049863191D-01
      ALLC(5,2,2) =4.012362861D-01
      ALLC(6,2,2) =1.051855189D-01
C                                      3S
      ALLZ(1,3,1) =3.273031938D00
      ALLZ(2,3,1) =9.200611311D-01
      ALLZ(3,3,1) =3.593349765D-01
      ALLZ(4,3,1) =8.636686991D-02
      ALLZ(5,3,1) =4.797373812D-02
      ALLZ(6,3,1) =2.724741144D-02
      ALLC(1,3,1) =-6.775596947D-03
      ALLC(2,3,1) =-5.639325779D-02
      ALLC(3,3,1) =-1.587856086D-01
      ALLC(4,3,1) =5.534527651D-01
      ALLC(5,3,1) =5.015351020D-01
      ALLC(6,3,1) =7.223633674D-02
C                                     3P
      ALLZ(1,3,2) =5.077973607D00
      ALLZ(2,3,2) =1.340786940D00
      ALLZ(3,3,2) =2.248434849D-01
      ALLZ(4,3,2) =1.131741848D-01
      ALLZ(5,3,2) =6.076408893D-02
      ALLZ(6,3,2) =3.315424265D-02
      ALLC(1,3,2) =-3.329929840D-03
      ALLC(2,3,2) =-1.419488340D-02
      ALLC(3,3,2) =1.639395770D-01
      ALLC(4,3,2) =4.485358256D-01
      ALLC(5,3,2) =3.908813050D-01
      ALLC(6,3,2) =7.411456232D-02
C                                     4S
      ALLZ(1,4,1) = 1.365346 D+00
      ALLZ(2,4,1) = 4.393213 D-01
      ALLZ(3,4,1) = 1.877069 D-01
      ALLZ(4,4,1) = 9.360270 D-02
      ALLZ(5,4,1) = 5.052263 D-02
      ALLZ(6,4,1) = 2.809354 D-02
      ALLC(1,4,1) = 3.775056 D-03
      ALLC(2,4,1) =-5.585965 D-02
      ALLC(3,4,1) =-3.192946 D-01
      ALLC(4,4,1) =-2.764780 D-02
      ALLC(5,4,1) = 9.049199 D-01
      ALLC(6,4,1) = 3.406258 D-01
C                                   4P
      ALLC(1,4,2) =-7.052075 D-03
      ALLC(2,4,2) =-5.259505 D-02
      ALLC(3,4,2) =-3.773450 D-02
      ALLC(4,4,2) = 3.874773 D-01
      ALLC(5,4,2) = 5.791672 D-01
      ALLC(6,4,2) = 1.221817 D-01
      ALLZ(1,4,2) = 1.365346 D+00
      ALLZ(2,4,2) = 4.393213 D-01
      ALLZ(3,4,2) = 1.877069 D-01
      ALLZ(4,4,2) = 9.360270 D-02
      ALLZ(5,4,2) = 5.052263 D-02
      ALLZ(6,4,2) = 2.809354 D-02
C                                     5S
      ALLZ(1,5,1) = 7.701420258D-01
      ALLZ(2,5,1) = 2.756268915D-01
      ALLZ(3,5,1) = 1.301847480D-01
      ALLZ(4,5,1) = 6.953441940D-02
      ALLZ(5,5,1) = 4.002545502D-02
      ALLZ(6,5,1) = 2.348388309D-02
      ALLC(1,5,1) = 1.267447151D-02
      ALLC(2,5,1) = 3.266734789D-03
      ALLC(3,5,1) =-4.307553999D-01
      ALLC(4,5,1) =-3.231998963D-01
      ALLC(5,5,1) = 1.104322879D+00
      ALLC(6,5,1) = 4.368498703D-01
C                                      5P
      ALLZ(1,5,2) = 7.701420258D-01
      ALLZ(2,5,2) = 2.756268915D-01
      ALLZ(3,5,2) = 1.301847480D-01
      ALLZ(4,5,2) = 6.953441940D-02
      ALLZ(5,5,2) = 4.002545502D-02
      ALLZ(6,5,2) = 2.348388309D-02
      ALLC(1,5,2) =-1.105673292D-03
      ALLC(2,5,2) =-6.243132446D-02
      ALLC(3,5,2) =-1.628476766D-01
      ALLC(4,5,2) = 3.210328714D-01
      ALLC(5,5,2) = 6.964579592D-01
      ALLC(6,5,2) = 1.493146125D-01
      DO 30 I=1,10
         IF(MTYPE(I).EQ.0)GOTO 30
         NI=MTYPE(I)
         XI=ZS(NI)
         IA=I*4-3
         IB=IA+3
         IF(NI.LT.2) THEN
            NQN=1
         ELSEIF(NI.LT.10)THEN
            NQN=2
         ELSEIF(NI.LT.18)THEN
            NQN=3
         ELSEIF(NI.LT.36)THEN
            NQN=4
         ELSEIF(NI.LT.54)THEN
            NQN=5
         ELSE
            IF (DOPRNT) WRITE(6,*)' NO GAUSSIANS AVAILABLE'             CIO
            RETURN 1                                                    CSTP (stop)
         ENDIF
         DO 20 K=IA,IB
            L=1
            IF(K.GT.IA) L=2
            IF(K.GT.IA) XI=ZP(NI)
C     LF0410: For Hp atoms use a 1s orbital instead of a 2s orbital:    LF0410
            IF (HPUSED.AND.NI.EQ.9.AND.K.EQ.IA) THEN                    LF0410
            DO 5 J=1,6                                                  LF0410
               CC(K,J)=ALLC(J,1,1)                                      LF0410
    5       ZZ(K,J)=ALLZ(J,1,1)*XI**2                                   LF0410
            ELSE                                                        LF0410
            DO 10 J=1,6
               CC(K,J)=ALLC(J,NQN,L)
   10       ZZ(K,J)=ALLZ(J,NQN,L)*XI**2
            ENDIF                                                       LF0410
   20    CONTINUE
   30 CONTINUE
C     LF1010: If MOD1 is specified then use special CC and ZZ
C             values for these special Slater zeta values on Hp hydrogens and
C             oxygen atoms.  Note also that this file uses all atomic units.
C             This is okay as the zeta values that appear here as XI are already
C             in atomic units (since the BLOCK DATA file specifies all ZS, ZP, and
C             ZD in inverse bohrs already).
C             The first index in CCSPEC(?,?) and ZZSPEC(?,?) specifies which
C             special Slater exponent on an atom is being considered.  Here are the
C             specific meanings of this index value:
C                   1        =>  s Slater exponent for Hp atom in Hp-Hp pairs
C                                  when the MOD1 keyword is specified.
C                   2 thru 4 =>  p Slater exponent for Hp atom in Hp-Hp pairs
C                                  when the MOD1 keyword is specified.
C                   5        =>  s Slater exponent for O atom in O-O pairs
C                                  when the MOD1 keyword is specified.
C                   6 thru 8 =>  p Slater exponent for O atom in O-O pairs
C                                  when the MOD1 keyword is specified.
C
      IF (PMODS(1)) THEN
         DO 45 J=1,6
           CCSPEC(1,J)=ALLC(J,1,1)     ! (J,1,1) means J-th gaussian expansion coeff of 1s Slater orbital.
           CCSPEC(2,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           CCSPEC(3,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           CCSPEC(4,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           ZZSPEC(1,J)=ALLZ(J,1,1)*PMODVL(5)**2
           ZZSPEC(2,J)=ALLZ(J,2,2)*PMODVL(5)**2
           ZZSPEC(3,J)=ALLZ(J,2,2)*PMODVL(5)**2
           ZZSPEC(4,J)=ALLZ(J,2,2)*PMODVL(5)**2
           CCSPEC(5,J)=ALLC(J,2,1)     ! (J,2,1) means J-th gaussian expansion coeff of 2s Slater orbital.
           CCSPEC(6,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           CCSPEC(7,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           CCSPEC(8,J)=ALLC(J,2,2)     ! (J,2,2) means J-th gaussian expansion coeff of 2p Slater orbital.
           ZZSPEC(5,J)=ALLZ(J,2,1)*PMODVL(6)**2
           ZZSPEC(6,J)=ALLZ(J,2,2)*PMODVL(6)**2
           ZZSPEC(7,J)=ALLZ(J,2,2)*PMODVL(6)**2
           ZZSPEC(8,J)=ALLZ(J,2,2)*PMODVL(6)**2
   45    CONTINUE
      ENDIF
      RETURN
 9999 RETURN 1                                                          CSTP
      END
