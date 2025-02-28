  subroutine gdisp(xyz, r0ab, rs6, alp6, c6ab, s6, mxc, rcov, dxyz_temp)
!
! Calculates the derivative (gradient) of the D3 dispersion term, 
! based on material provided by Stefan Grimme, University of Muenster, Germany
! Downloaded from: http://toc.uni-muenster.de/DFTD3/
! Referred to in: http://sirius.chem.vt.edu/psi4manual/4.0b5/dftd3.html
! Download date for dftd3.tgz: 5/30/2015.  File dftd3.3.1.0 dated 6/30/2014
!
  use common_arrays_C, only: nat 
  use molkst_C, only : numat
  implicit none
  double precision, parameter :: k1 = 16.d0
  integer, parameter :: max_elem = 94, maxc = 5 ! maximum coordination number references per element    
  double precision :: &
  c6ab(max_elem, max_elem, maxc, maxc, 3), & ! C6 for all element pairs 
  dxyz_temp(3,numat),                      & ! Contribution to gradient
  xyz(3, numat),                           & ! coordinates in au
  rcov(max_elem),                          & ! covalent radii
  cn(numat),                               & ! coordination numbers of the atoms
  r0ab(max_elem, max_elem),                & ! cut  -  off radii for all element pairs
  rs6, alp6, s6
  integer :: mxc(max_elem)
!
!  Local variables
!
  integer :: i, j, linij
  double precision :: R0, r2, damp6, c6, tmp1, r, dc6_rest, rij(3), dc6iji, dc6ijj, r6, r7, t6, &
  rcovij, expterm, dcn,x1
  double precision, allocatable :: drij(:), dc6i(:)
  integer, external :: lin
  i = (numat*(numat+1))/2
  allocate(drij(i), dc6i(numat))
  call ncoord(numat,rcov,nat,xyz,cn)
  drij = 0.d0
  dc6i = 0.d0
  dc6_rest = 0.0d0
  do i = 2, numat
    do j = 1, i - 1
      rij = xyz(:,j) - xyz(:,i)
      r2 = sum(rij*rij)
       if (r2 > 10000.d0) cycle
      linij = lin(i,j)
      r0 = r0ab(nat(j),nat(i))
      call get_dC6_dCNij(maxc, max_elem, c6ab, mxc(nat(i)), &
            mxc(nat(j)), cn(i), cn(j), nat(i), nat(j), &
            c6, dc6iji, dc6ijj)
      r = sqrt(r2)
      r6 = r2*r2*r2
      r7 = r6*r
!
!  Calculate damping functions
!
      t6  =  (r/(rs6*R0))**( - alp6)
      damp6  = 1.d0/( 1.d0 + 6.d0*t6 )
      tmp1 = s6*6.d0*damp6*C6/r7
      drij(linij) = drij(linij) - tmp1  
      drij(linij) = drij(linij)  + tmp1*alp6*t6*damp6   
      dc6_rest = s6/r6*damp6 
!
!     saving all f_dmp/r6*dC6(ij)/dCN(i) for each atom for later
!
      dc6i(i) = dc6i(i) + dc6_rest*dc6iji
      dc6i(j) = dc6i(j) + dc6_rest*dc6ijj
    end do 
  end do 
!
! After calculating all derivatives dE/dr_ij w.r.t. distances,
! the grad w.r.t. the coordinates is calculated dE/dr_ij * dr_ij/dxyz_i
!
  do i = 2, numat
    do j = 1, i - 1
      linij = lin(i,j)
      rij = xyz(:,j) - xyz(:,i)
      r2 = sum(rij*rij)
      r = dsqrt(r2)
      if (r2 < 100.d0) then
        rcovij = rcov(nat(i)) + rcov(nat(j))
        expterm = exp( - k1*(rcovij/r - 1.d0))
        dcn =  - k1*rcovij*expterm/(r*r*(expterm + 1.d0)*(expterm + 1.d0))
      else
        dcn = 0.d0
      endif
      x1 = drij(linij) + dcn*(dc6i(i) + dc6i(j))
      dxyz_temp(:,i) = dxyz_temp(:,i) + x1*rij/r
      dxyz_temp(:,j) = dxyz_temp(:,j) - x1*rij/r
    end do 
  end do 
  end subroutine gdisp
  
  
  subroutine ncoord(natoms,rcov,nat,xyz,cn)
  implicit none
  integer nat(*),natoms, i, j
  double precision xyz(3,*), cn(*), rcov(94)
  double precision dx, dy, dz, r, damp, xn, rr, rco, r2
  do i = 1,natoms
    xn = 0.0d0
    do j = 1,natoms
        if(j /= i)then
          dx = xyz(1,j) - xyz(1,i)
          dy = xyz(2,j) - xyz(2,i)
          dz = xyz(3,j) - xyz(3,i)
          r2 = dx*dx + dy*dy + dz*dz
      !    if (r2 > 100.d0) cycle  WARNING
          r = sqrt(r2)
! covalent distance in Bohr
          rco = rcov(nat(i)) + rcov(nat(j))
          rr = rco/r
! counting function exponential has a better long - range behavior than MHGs inverse damping
          damp = 1.d0/(1.d0 + exp( -16.d0*(rr - 1.0d0)))
          xn = xn + damp
        endif
    end do
    cn(i) = xn
  end do
  end subroutine ncoord
  
  subroutine get_dC6_dCNij(maxc ,max_elem, c6ab, mxci, mxcj,  &
    cni, cnj, izi, izj, c6check, dc6i, dc6j)
    implicit none
    integer, intent (in) :: maxc ,max_elem, mxci, mxcj 
    double precision, intent (in) :: c6ab(max_elem,max_elem,maxc,maxc,3), cni, cnj    
    integer :: izi, izj, a, b
    double precision :: dc6i, dc6j, c6check, term
    double precision :: zaehler, nenner, dzaehler_i, dnenner_i, dzaehler_j, dnenner_j
    double precision :: expterm, cn_refi, cn_refj, c6ref, r
    double precision :: c6mem, r_save
    c6mem =  -1.d99
    r_save = 9999.d0
    zaehler = 0.0d0
    nenner = 0.0d0
    dzaehler_i = 0.d0
    dnenner_i = 0.d0
    dzaehler_j = 0.d0
    dnenner_j = 0.d0
    do a = 1,mxci
      do b = 1,mxcj
        c6ref = c6ab(izi, izj, a, b, 1)
        if (c6ref > 0.d0) then
          cn_refi = c6ab(izi, izj, a, b, 2)
          cn_refj = c6ab(izi, izj, a, b, 3)
          r = (cn_refi - cni)*(cn_refi - cni) + (cn_refj - cnj)*(cn_refj - cnj)
          if (r < r_save) then
            r_save = r
            c6mem = c6ref
          endif
          expterm = exp(-4.d0*r)
          zaehler = zaehler + c6ref*expterm
          nenner = nenner + expterm
          expterm = -4.d0*expterm*2.d0
          term = expterm*(cni - cn_refi)
          dzaehler_i = dzaehler_i + c6ref*term
          dnenner_i  = dnenner_i + term
          term = expterm*(cnj - cn_refj)
          dzaehler_j = dzaehler_j + c6ref*term
          dnenner_j  = dnenner_j +      term
        end if
      end do
    end do

    if (nenner > 1.0d-99) then
      c6check = zaehler/nenner
      dc6i = ((dzaehler_i*nenner) - (dnenner_i*zaehler))/(nenner*nenner)
      dc6j = ((dzaehler_j*nenner) - (dnenner_j*zaehler))/(nenner*nenner)
    else
      c6check = c6mem
      dc6i = 0.0d0
      dc6j = 0.0d0
    endif
end subroutine get_dC6_dCNij




