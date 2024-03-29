      subroutine mpcsyb(chr, kchrge, eionis, dip) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use molkst_C, only : numat, norbs, nclose, nalpha, nbeta, escf, &
      keywrd
      use common_arrays_C, only : coord, eigs
      use chanel_C, only : isyb, syb_fn
!...Translated by Pacific-Sierra Research 77to90  4.4G  09:17:07  02/26/07  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: kchrge 
      real(double) , intent(in) :: eionis, chr(numat)
      real(double) , intent(inout) :: dip 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, j, i1, i2, nfilled
!-----------------------------------------------
   open(unit = isyb, file = syb_fn)
!  Write out the charge flag and number of atoms
      write (16, '(2I4)', err=30) 1, numat 
!  Write out the coordinates and charges
      do i = 1, numat 
        write (16, '(4F12.6)', err=30) (coord(j,i),j=1,3), chr(i) 
      end do 
      nfilled = max(nclose, nalpha, nbeta)
      i1 = max(1,nfilled - 1) 
      i2 = min(norbs,nfilled + 2) 
!
!  Write out the 2 highest and 2 lowest orbital energies
!
      write (16, 20, err=30) (eigs(j),j=i1,i2), nfilled 
   20 format(4f12.6,2x,i4,2x,'HOMOs,LUMOs,# of occupied MOs') 
!
!  Write out the Heat of Formation and Ionisation Potential
!
      write (16, '(2F12.6,4X,''HF and IP'')', err=30) escf, eionis 
!
!  Write out the Dipole Moment
!
      if (kchrge /= 0) dip = 0.0 
      write (16, '(I4,F10.3,''  Charge,Dipole Moment'')', err=30) kchrge, dip 
      if (index(keywrd," MULL") /= 0) then
        call mpcpop(1)
      else
        call mpcpop(0)
      end if
      close(unit = isyb, status = "keep")
      return  
   30 continue 
      write (6, '(A)') 'Error writing SYBYL MOPAC output' 
      return  
      end subroutine mpcsyb 
!
! ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
      subroutine mpcpop(icok) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : numat
      use common_arrays_C, only : nfirst, nlast, nat, pb, chrg
      use parameters_C, only : tore
      use chanel_C, only : iw, isyb
      use elemts_C, only : elemnt
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:09:48  02/26/07  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: icok 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i, if, il, j, k 
      real(double), dimension(numat) :: pop
      real(double) :: sum 
!-----------------------------------------------
!
! This subroutine calculates the total Mulliken populations on the
!   atoms by summing the diagonal elements from the  Mulliken
!   population analysis.
!
      if (icok > -1) write (isyb, '(I4,5X,'' MULLIKEN POPULATION AND CHARGE'')', err=40) icok 
!
! ICOK = 1  ==> SYBYL present - do analysis, and write MOPAC and SYBYL output
! ICOK = 0  ==> KEYWORD SYBYL present, but MULLIK absent - don't do analysis
! ICOK = -1 ==> MULLIK present but SYBYL absent - do analysis, but don't write SYBYL output
!
      if (allocated(chrg)) deallocate(chrg)
      allocate(chrg(numat))
      if (icok /= 0) then 
        do i = 1, numat 
          if = nfirst(i) 
          il = nlast(i) 
          sum = 0.0 
          pop(i) = 0.0 
          chrg(i) = 0.0 
          do j = if, il 
!
!    Diagonal element of mulliken matrix
!
            sum = sum + pb((j*(j+1))/2) 
          end do 
          k = nat(i) 
!
!    Mulliken population for i'th atom
!
          pop(i) = sum 
          chrg(i) = tore(k) - pop(i) 
        end do 
        write (iw, '(3/8X,''MULLIKEN POPULATIONS AND CHARGES'',/)') 
        write (iw, '(6X,''NO.  ATOM   POPULATION      CHARGE'')') 
        write (iw, '(5X,I4,3X,A2,F13.6,F14.6)') &
         (j, elemnt(nat(j)), pop(j), chrg(j), j = 1, numat) 
        if (icok > -1) write (isyb, "(2f12.6)", err=40) pop(:numat), chrg(:numat) 
        call to_screen("To_file: Mulliken")
      endif 
      return  
   40 continue 
      write (iw, '(A)') 'Error writing SYBYL Mulliken population output' 
      return  
      end subroutine mpcpop 
!

