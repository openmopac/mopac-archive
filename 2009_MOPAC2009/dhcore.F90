      subroutine dhcore(coord, h, ww, enuclr, nati, natx, step) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : norbs, numat, n2elec
      use common_arrays_C, only : nat, nfirst, nlast
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:08  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use h1elec_I 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: nati 
      integer , intent(in) :: natx 
      real(double) , intent(out) :: enuclr 
      real(double) , intent(in) :: step 
      real(double)  :: coord(3,numat) 
      real(double) , intent(inout) :: h(*) 
      real(double) , intent(out) :: ww(n2elec) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, kr, ia, ib, ni, j, ja, jb, nj, i2, i1, ij, &
        kro, ii, k
      real(double), dimension(45) :: e1b, de1b, e2a, de2a 
      real(double), dimension(9,9) :: di, ddi 
      real(double), dimension(2026) :: wjd, dwjd  
      real(double) :: csave, enuc, denuc 
!-----------------------------------------------
!
!  DHCORE GENERATES THE 1-ELECTRON  AND 2-ELECTRON INTEGRALS DERIVATIVES
!         WITH RESPECT TO THE CARTESIAN COORDINATE COORD (NATX,NATI).
!
!  INPUT
!      COORD     : CARTESIAN  COORDINATES OF THE MOLECULE.
!      NATI,NATX : INDICES OF THE MOVING COORDINATE.
!      STEP      : STEP SIZE OF THE 2-POINTS FINITE DIFFERENCE.
!  OUTPUT
!      H         : 1-ELECTRON INTEGRALS DERIVATIVES (PACKED CANONICAL).
!      W         : 2-ELECTRON INTEGRALS DERIVATIVES (ORDERED AS REQUIRED
!                             IN DFOCK2 AND DIJKL1).
!      ENUCLR    : NUCLEAR ENERGY DERIVATIVE.
!
!-----------------------------------------------
      h(:norbs*(norbs+1)/2) = 0 
      enuclr = 0.D0 
      kr = 1 
      i = nati 
      csave = coord(natx,nati) 
      ia = nfirst(nati) 
      ib = nlast(nati) 
      ni = nat(nati) 
      do j = 1, numat 
        if (j == nati) cycle  
        ja = nfirst(j) 
        jb = nlast(j) 
        nj = nat(j) 
        coord(natx,nati) = csave + step 
        call h1elec (ni, nj, coord(1,nati), coord(1,j), di) 
        coord(natx,nati) = csave - step 
        call h1elec (ni, nj, coord(1,nati), coord(1,j), ddi) 
!
!     FILL THE ATOM-OTHER ATOM ONE-ELECTRON MATRIX.
!
        i2 = 0 
        if (ia > ja) then 
          do i1 = ia, ib 
            ij = i1*(i1 - 1)/2 + ja - 1 
            i2 = i2 + 1 
            h(ij+1:jb-ja+1+ij) = h(ij+1:jb-ja+1+ij) + (di(i2,:jb-ja+1)-ddi(i2,:&
              jb-ja+1)) 
          end do 
        else 
          do i1 = ja, jb 
            ij = i1*(i1 - 1)/2 + ia - 1 
            i2 = i2 + 1 
            h(ij+1:ib-ia+1+ij) = h(ij+1:ib-ia+1+ij) + (di(:ib-ia+1,i2)-ddi(:ib-&
              ia+1,i2)) 
          end do 
        endif 
!
!     CALCULATE THE TWO-ELECTRON INTEGRALS, W; THE ELECTRON NUCLEAR TERM
!     E1B AND E2A; AND THE NUCLEAR-NUCLEAR TERM ENUC.
!
        kro = kr 
        coord(natx,nati) = csave + step  
!
!     Two-electron one and two center terms.
!
        call rotate (ni, nj, coord(1,nati), coord(1,j), wjd, kr, e1b, e2a, enuc) 
              kr = kro
        coord(natx,nati) = csave - step
        call rotate (ni, nj, coord(1,nati), coord(1,j), dwjd, kr, de1b, de2a, denuc) 
        k = kr - kro 
        if ( kr > 0) then
          wjd(:k + 1) = wjd(:k + 1) - dwjd(:k + 1)
          do i = 0, k - 1
            ww(i + kro) = wjd(i + 1)          
          end do
        end if 
      coord(natx,nati) = csave 
        enuclr = enuclr + enuc - denuc 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM I.
!
        i2 = 0 
        do i1 = ia, ib 
          ii = i1*(i1 - 1)/2 + ia - 1 
          if (i1 - ia + 1 > 0) then 
            h(ii+1:i1-ia+1+ii) = h(ii+1:i1-ia+1+ii) + e1b(i2+1:i1-ia+1+i2) - &
              de1b(i2+1:i1-ia+1+i2) 
            i2 = i1 - ia + 1 + i2 
          endif 
        end do 
!
!   ADD ON THE ELECTRON-NUCLEAR ATTRACTION TERM FOR ATOM J.
!
        i2 = 0 
        do i1 = ja, jb 
          ii = i1*(i1 - 1)/2 + ja - 1 
          if (i1 - ja + 1 > 0) then 
            h(ii+1:i1-ja+1+ii) = h(ii+1:i1-ja+1+ii) + e2a(i2+1:i1-ja+1+i2) - &
              de2a(i2+1:i1-ja+1+i2) 
            i2 = i1 - ja + 1 + i2 
          endif 
        end do 
      end do 
      return  
      end subroutine dhcore 
