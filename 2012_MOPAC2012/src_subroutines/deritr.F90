      subroutine deritr() 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE molkst_C, ONLY: norbs, nelecs, numcal, ndep, nvar, &
      & enuclr, keywrd, natoms, escf, elect, method_pm7, mozyme, moperr
      use common_arrays_C, only : loc, geo, p, pa, coord, errfn
      USE funcon_C, only : fpc_9  
      USE chanel_C, only : iw 
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  17:52:28  03/12/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use symtry_I 
      use gmetry_I 
      use hcore_I 
      use iter_I 
      implicit none
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, k, l, j
      real(double) :: xderiv 
      real(double), dimension(3*natoms) :: xparam
      real(double) :: delta, const, aa, xstore, ee, escf_store, enuclr_store, &
      elect_store, sum
      logical :: debug, PM6_H, precise

      save icalcn, debug, xderiv, delta, const
!-----------------------------------------------
!***********************************************************************
!
!    DERITR CALCULATES THE DERIVATIVES OF THE ENERGY WITH RESPECT TO THE
!          INTERNAL COORDINATES. THIS IS DONE BY FINITE DIFFERENCES
!          USING FULL SCF CALCULATIONS.
!
!          THIS IS VERY TIME-CONSUMING, AND SHOULD ONLY BE USED WHEN
!          NO OTHER DERIVATIVE CALCULATION WILL DO.
!
!    THE MAIN ARRAYS IN DERIV ARE:
!        LOC    INTEGER ARRAY, LOC(1,I) CONTAINS THE ADDRESS OF THE ATOM
!               INTERNAL COORDINATE LOC(2,I) IS TO BE USED IN THE
!               DERIVATIVE CALCULATION.
!        GEO    ARRAY \GEO\ HOLDS THE INTERNAL COORDINATES.
!
!***********************************************************************
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        debug = (index(keywrd,'DERITR') /= 0) 
        precise = (index(keywrd,'PRECISE') /= 0) 
        PM6_H = (method_pm7 .or. index(keywrd,'PM6-D') + index(keywrd,'PM6-H') /= 0) 
!
!   DELTA IS A MACHINE-PRECISION DEPENDANT CONSTANT
!
        
        icalcn = numcal 
!
!   CONST = 23.06... eV to KCAL
!
        const = fpc_9 
!
      delta = 0.d0
      i = index(keywrd,' DELTA') 
      if (i /= 0) delta = reada(keywrd,i) 
        if (precise) then
          if (delta < 1.d-10) delta = 0.001D0
          xderiv = 0.5D0/delta           
        else
          if (delta < 1.d-10) delta = 0.0002D0
          xderiv = 1.d0/delta          
        end if
      endif       
      do i = 1, nvar 
        xparam(i) = geo(loc(2,i),loc(1,i)) 
      end do 
      escf_store = escf
      enuclr_store = enuclr
      elect_store = elect
      if ( .not. precise) then
!
!  ESTABLISH THE ENERGY AT THE CURRENT POINT
!
        if (ndep /= 0) call symtry 
        call gmetry (geo, coord) 
        if (norbs*nelecs > 0) then
          if (mozyme) then      
            call hcore_for_MOZYME () 
            if (moperr) return  
            aa = 0.d0
            call iter_for_MOZYME (aa) 
          else
            call hcore () 
            call iter (aa, .TRUE., .TRUE.) 
          end if 
        else
          aa = 0.d0
        end if
        if (PM6_H) then
          call h_bond_correction(sum, .false.)
          aa = aa + sum/const
        endif 
      end if
!
!  RESTORE THE DENSITY MATRIX (WHY?)
!
      if (allocated (pa)) p = pa*2.D0 
      aa = aa + enuclr 
      do i = 1, nvar 
        k = loc(1,i) 
        l = loc(2,i) 
        xstore = xparam(i) 
        do j = 1, nvar 
          geo(loc(2,j),loc(1,j)) = xparam(j) 
        end do 
        if (precise) then
          geo(l,k) = xstore + delta 
          if (ndep /= 0) call symtry 
          call gmetry (geo, coord) 
!
!   IF NEEDED, CALCULATE "EXACT" DERIVATIVES.
!
          if (norbs*nelecs > 0) then
            if (mozyme) then      
              call hcore_for_MOZYME () 
              if (moperr) return  
              call iter_for_MOZYME (aa) 
            else
              call hcore () 
              call iter (aa, .TRUE., .TRUE.) 
            end if 
            if (PM6_H) then
              call h_bond_correction(sum, .false.)
              aa = aa + sum/const
            end if
          else
            aa = 0.d0
          end if
          aa = aa + enuclr 
        end if
        geo(l,k) = xstore - delta
        if (ndep /= 0) call symtry 
        call gmetry (geo, coord) 
!
!   IF NEEDED, CALCULATE "EXACT" DERIVATIVES.
!
        if (norbs*nelecs > 0) then
          if (mozyme) then      
            call hcore_for_MOZYME () 
            if (moperr) return  
            call iter_for_MOZYME (ee) 
          else
            call hcore () 
            call iter (ee, .TRUE., .TRUE.) 
          end if 
          if (PM6_H) then
            call h_bond_correction(sum, .false.)
            ee = ee + sum/const
          end if
        else
          ee = 0.d0
        end if
        ee = ee + enuclr 
        errfn(i) = (aa - ee)*const*xderiv
      end do 
      if (debug) then 
        write (iw, '('' ERROR FUNCTION'')') 
        write (iw, '(10F8.3)') (errfn(i),i=1,nvar) 
      endif 
      escf = escf_store
      enuclr = enuclr_store
      elect = elect_store
      return  
      end subroutine deritr 
