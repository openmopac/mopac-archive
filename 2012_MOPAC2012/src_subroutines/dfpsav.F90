      subroutine dfpsav(totime, xparam, gd, xlast, funct1, mdfp, xdfp) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double  
      USE ef_C, ONLY: alparm, x0, x1, x2, iloop 
      USE maps_C, only : rxn_coord1, rxn_coord2, ijlp, ilp, jlp, ione, jlp1, surf, &
      rxn_coord, latom, kloop
      use common_arrays_C, only : na, geo, geoa, grad, hesinv, &
      errfn, profil 
      USE molkst_C, ONLY: natoms, numcal, nvar, keywrd, mozyme, &
      & koment, title, norbs, numat
      USE chanel_C, only : iw, ires, restart_fn
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:50:30  03/10/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use geout_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      real(double)  :: totime 
      real(double)  :: funct1 
      integer  :: mdfp(9) 
      real(double)  :: xparam(nvar) 
      real(double)  :: gd(nvar) 
      real(double)  :: xlast(nvar) 
      real(double)  :: xdfp(9)  
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: icalcn, i, j, linear, old_numat, old_norbs
      logical :: first, opend

      save first, icalcn 
!-----------------------------------------------
!*********************************************************************
!
! DFPSAV STORES AND RESTORES DATA USED IN THE D-F-P GEOMETRY
!        OPTIMISATION.
!
!  ON INPUT TOTIME = TOTAL CPU TIME ELAPSED DURING THE CALCULATION.
!           XPARAM = CURRENT VALUE OF PARAMETERS.
!           GD     = OLD GRADIENT.
!           XLAST  = OLD VALUE OF PARAMETERS.
!           FUNCT1 = CURRENT VALUE OF HEAT OF FORMATION.
!           MDFP   = INTEGER CONSTANTS USED IN D-F-P.
!           XDFP   = REAL CONSTANTS USED IN D-F-P.
!           MDFP(9)= 1 FOR DUMP, 0 FOR RESTORE.
!*********************************************************************
      data icalcn/ 0/  
      first = icalcn /= numcal 
      if (first) icalcn = numcal 
      inquire(unit=ires, opened=opend) 
      if (opend) close(unit=ires, status='KEEP') 
      open(unit=ires, file=restart_fn, status='UNKNOWN', form=&
        'UNFORMATTED', position='asis') 
      rewind ires 
      if (mdfp(9) /= 0) then 
        if (mdfp(9) == 1) then 
          write (iw, &
      '(2/10X,''- - - - - - - TIME UP - - - - - - -'',2/)') 
          if (index(keywrd,'SADDLE') /= 0) then 
            write (iw, &
      '(2/10X,'' NO RESTART EXISTS FOR SADDLE'',2/      10X, &
      & '' HERE IS A DATA-FILE FILES THAT MIGHT BE SUITABLE'',/ &
      & 10X,'' FOR RESTARTING THE CALCULATION'',3/)') 
            write (iw, '(A)') keywrd, koment, title 
            do iloop = 1, 2 
              call geout ((-iw)) 
              geo(:,:natoms) = geoa(:,:natoms) 
              na = 0
            end do 
            write (iw, '(3/10X,''CALCULATION TERMINATED HERE'')') 
            call mopend ('NO RESTART EXISTS FOR SADDLE. ') 
            return  
          endif 
          write (iw, &
      '(2/10X,'' - THE CALCULATION IS BEING DUMPED TO DISK'',/10X, &
      & ''   RESTART IT USING THE KEYWORD "RESTART"'')') 
          write (iw, &
            '(2/10X,''CURRENT VALUE OF HEAT OF FORMATION ='',F12.6)') funct1 
          if (index(keywrd," GRADI") /= 0 .and. mozyme) then 
            write (iw, '(3/7X,''CURRENT  POINT  AND  DERIVATIVES'',/)')        
            call prtgra ()
          end if
        endif 
        if (mdfp(9) == 1) call geout (iw) 
        write (ires) norbs, numat, (xparam(i),i=1,nvar), (gd(i),i=1,nvar) 
        write (ires) mdfp, xdfp, totime, funct1         
        write (ires) (xlast(i),i=1,nvar), (grad(i),i=1,nvar) 
        linear = (nvar*(nvar + 1))/2 
        write (ires) (hesinv(i),i=1,linear)  
        call den_in_out(1)
        if (latom /= 0) then 
          if (index(keywrd,'STEP') /= 0) then 
            write (ires) kloop 
            write (ires) rxn_coord 
            write (ires) (profil(i),i=1,kloop) 
          else 
            write (ires) ((alparm(j,i),j=1,3),i=1,nvar) 
            write (ires) iloop, x0, x1, x2 
          endif 
        endif 
        if (index(keywrd,'STEP1') /= 0) then 
          write (ires) ijlp, ilp, jlp, jlp1, ione 
          write (ires) rxn_coord1, rxn_coord2 
          write (ires) (surf(i),i=1,ijlp) 
        endif 
        write (ires) (errfn(i),i=1,nvar) 
        close(ires) 
      else 
        if (first) write (iw, '(2/10X,'' RESTORING DATA FROM DISK''/)') 
        read (ires, end=50, err=50)old_norbs, old_numat, (xparam(i),i=1,nvar), (gd(i),i=1,nvar) 
        if (norbs /= old_norbs .or. numat /= old_numat) then
              write(iw,*)" Restart file read in does not match current data set"
              call mopend (" Restart file read in does not match current data set")
              return
        end if
        read (ires, end=40, err=40) mdfp, xdfp, totime, funct1 
        if (first) write (iw, '(10X,''FUNCTION ='',F13.6,2/)') funct1        
        read (ires, end=50, err=50) (xlast(i),i=1,nvar), (grad(i),i=1,nvar) 
        linear = (nvar*(nvar + 1))/2 
        read (ires, end=50, err=50) (hesinv(i),i=1,linear)  
        call den_in_out(0)
        if (latom /= 0) then 
          if (index(keywrd,'STEP') /= 0) then 
            read (ires, end=50, err=50) kloop 
            read (ires, end=50, err=50) rxn_coord 
            read (ires, end=50, err=50) (profil(i),i=1,kloop) 
          else 
            read (ires, end=50, err=50) ((alparm(j,i),j=1,3),i=1,nvar) 
            read (ires, end=50, err=50) iloop, x0, x1, x2 
          endif 
        endif 
        if (index(keywrd,'STEP1') /= 0) then 
          read (ires, end=50, err=50) ijlp, ilp, jlp, jlp1, ione 
          read (ires, end=50, err=50) rxn_coord1, rxn_coord2 
          read (ires, end=50, err=50) (surf(i),i=1,ijlp) 
        endif 
        read (ires, end=50, err=50) (errfn(i),i=1,nvar) 
        first = .FALSE. 
        return  
   40   continue 
        write (iw, '(2/10X,''NO RESTART FILE EXISTS!'')') 
        call mopend ('NO RESTART FILE EXISTS!') 
        return  
   50   continue 
        write (iw, '(2/10X,''RESTART FILE EXISTS, BUT IS CORRUPT'')') 
        call mopend ('RESTART FILE EXISTS, BUT IS CORRUPT') 
        return  
      endif 
      return  
      end subroutine dfpsav 
