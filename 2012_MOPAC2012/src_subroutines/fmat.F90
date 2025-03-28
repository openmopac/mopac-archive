      subroutine fmat(fmatrx, nreal, tscf, tder, deldip, heat, evecs, ts) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use common_arrays_C, only : coord, na, labels, atmass, p, q, xparam
      use molkst_C, only : numat, nvar, keywrd, tleft, moperr, natoms, tdump, &
      emin, mozyme
      use parameters_C, only : tore
      use funcon_C, only : fpc_10
      use chanel_C, only : iw, ilog, log
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:47:14  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use reada_I 
      use second_I 
      use symr_I 
      use sympop_I 
      use compfg_I 
      use dot_I 
      use chrge_I 
      use dipole_I 
      use mopend_I 
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(out) :: nreal 
      logical, intent (in) :: ts
      real(double) , intent(in) :: tscf 
      real(double) , intent(in) :: tder 
      real(double)  :: heat 
      real(double)  :: fmatrx(((3*numat + 1)*(3*numat))/2) 
      real(double)  :: deldip(3,3*numat)  
      real(double)  :: evecs(9*numat*numat) 

!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, lin, maxcyc, istart, jstart, kountf, lu, iskip, j, &
        ii, ll, l, k, kk, iloop
      real(double), dimension(3*natoms) :: grad, grold
      real(double), dimension(3) :: del2 
      real(double), dimension(3*natoms) :: g2old, eigs, g2rad, fconst, dumy
      real(double) :: fact, tlast, totime, time2, estime, delta, &
        time3, tstep, estim, escf
      logical :: debug, restrt, prnt, resfil, precis, big 
      character :: line*120
      double precision, external :: dipole_for_MOZYME
      save fact 
!-----------------------------------------------
!**********************************************************************
!
!  VALUE CALCULATES THE SECOND-ORDER OF THE ENERGY WITH
!        RESPECT TO THE CARTESIAN COORDINATES I AND J AND PLACES IT
!        IN FMATRX
!
!  ON INPUT NATOMS  = NUMBER OF ATOMS IN THE SYSTEM.
!           XPARAM  = INTERNAL COORDINATES OF MOLECULE STORED LINEARLY
!
!
!  ON OUTPUT FMATRX = SECOND DERIVATIVE OF THE ENERGY WITH RESPECT TO
!                    CARTESIAN COORDINATES I AND J.
!**********************************************************************
      data del2/ 3*0.D0/  
!
!    FACT IS THE CONVERSION FACTOR FROM KCAL/MOLE TO ERGS
!
      fact = 4.184D0/fpc_10*1.D21 
    !  fact = 1.d0/627.51*0.529**2
!
! SET UP CONSTANTS AND FLAGS
      na = 0
!
!  SET UP THE VARIABLES IN XPARAM AND LOC,THESE ARE IN
!  CARTESIAN COORDINATES
!
      numat = 0 
      do i = 1, natoms 
        if (labels(i)==99 .or. labels(i)==107) cycle  
        numat = numat + 1 
        labels(numat) = labels(i) 
      end do 
      natoms = numat 
      grad = 0.d0
      lin = (nvar*(nvar + 1))/2 
      fmatrx(:lin) = 0.D0 
      maxcyc = 100000 
      if (index(keywrd,' CYCLES') /= 0) maxcyc = nint(reada(keywrd,index(keywrd,' CYCLES'))) 
      prnt = index(keywrd,'IRC=') == 0 
      precis = index(keywrd,'PREC') /= 0 
      restrt = index(keywrd,'RESTART') /= 0 
      if (index(keywrd,'NLLSQ') /= 0) restrt = .FALSE. 
      debug = index(keywrd,'FMAT') /= 0 
      big = index(keywrd,'LARGE')/=0 .and. debug 
      if (prnt) write (iw, &
      '(2/4X,''FIRST DERIVATIVES WILL BE USED IN THE CALCULATION OF SECOND DERIVATIVES'')') 
      tlast = tleft 
      resfil = .FALSE. 
      if (restrt) then 
        istart = 0 
        i = 0 
        totime = 0.d0
        jstart = 0
        fconst = 0.d0
        call forsav (totime, deldip, istart, fmatrx, coord, nvar, heat, evecs, jstart, fconst) 
        if (moperr) return  
        kountf = (istart*(istart + 1))/2 
        istart = istart + 1 
        jstart = jstart + 1 
        time2 = second(1) 
      else 
        kountf = 0 
        totime = 0.D0 
        if (tscf > 0.D0) tleft = tleft - tscf - tder 
        istart = 1 
      endif 
! CALCULATE FMATRX
      if (istart > 1) then 
        estime = (nvar - istart + 1)*totime/(istart - 1.D0) 
      else 
        estime = nvar*(tscf + tder)*2.D0 
        if (precis) estime = estime*2.D0 
      endif 
      if (tscf > 0) write (iw, &
      '(/10X,''ESTIMATED TIME TO COMPLETE CALCULATION ='',F12.2,'' SECONDS'')') estime 
      if (restrt) then 
        if (istart <= nvar) write (iw, &
          '(/10X,''STARTING AGAIN AT LINE'',18X,I4)') istart 
        write (iw, '(/10X,''TIME USED UP TO RESTART ='',F22.2)') totime 
      endif 
      lu = kountf 
     ! numat = nvar/3 
      eigs(:nvar) = 0.D0 
      if (.not. ts) call symr 
      iskip = 0 
      do i = istart, nvar 
        if (((i - 1)/3)*3 == i - 1) then 
!
!  START OF A NEW ATOM.  DOES A SYMMETRY OPERATION RELATE AN ALREADY
!  CALCULATED ATOM TO THIS ONE
!
          j = (i + 2)/3 
          call sympop (fmatrx, j, iskip, deldip) 
        endif 
        if (iskip > 0) then 
          write (line, '(" STEP:",I4,23X,"INTEGRAL =",F10.2," TIME LEFT:",F10.2)') i, totime, tleft 
          write(iw,"(a)")line(:len_trim(line))
          call to_screen(line)      
          endfile (iw) 
          backspace (iw) 
          iskip = iskip - 1 
          lu = lu + i 
          cycle  
        endif 
        time2 = second(1) 
        delta = 1.D0/120.D0 
        if (precis) then 
!
!   DETERMINE A GOOD STEP SIZE
!
          g2old = 100.D0 
          xparam(i) = xparam(i) + delta 
          call compfg (xparam, .TRUE., escf, .TRUE., g2old, .TRUE.) 
          if (moperr) return  
          xparam(i) = xparam(i) - delta 
          delta = delta*10.D0/sqrt(dot(g2old,g2old,nvar)) 
!
!   CONSTRAIN DELTA TO A 'REASONABLE' VALUE
!
          delta = min(0.05D0,max(0.005D0,delta)) 
          if (debug) write (iw, '(A,I3,A,F12.5)') ' STEP:', i, ' DELTA :', delta 
          g2old(1) = 100.D0 
          xparam(i) = xparam(i) + delta 
          emin = 0.d0
          call compfg (xparam, .TRUE., escf, .TRUE., g2old, .TRUE.) 
          if (moperr) return  
          if (debug) write (iw, '(A,F12.5)') ' GNORM +1.0*DELTA',  &
            sqrt(dot(g2old,g2old,nvar)) 
          xparam(i) = xparam(i) - delta*2.D0 
          g2rad = 100.D0 
          emin = 0.d0
          call compfg (xparam, .TRUE., escf, .TRUE., g2rad, .TRUE.) 
          if (moperr) return  
          xparam(i) = xparam(i) + delta 
          if (debug) write (iw, '(A,F12.5)') ' GNORM -1.0*DELTA', &
            sqrt(dot(g2rad,g2rad,nvar)) 
        else 
          if (debug) write (iw, '(A,I3,A,F12.5)') ' STEP:', i, ' DELTA :', delta 
        endif 
        xparam(i) = xparam(i) + 0.5D0*delta 
        grold = 100.D0 
        emin = 0.d0
        call compfg (xparam, .TRUE., escf, .TRUE., grold, .TRUE.) 
        if (moperr) return  
        if (debug) write (iw, '(A,F12.5)') ' GNORM +0.5*DELTA', sqrt(dot(grold,grold,nvar)) 
        call chrge (p, q) 
        q(:numat) = tore(labels(:numat)) - q(:numat) 
!
!   ESTIME IS USED HERE AS DIPOLE IS A FUNCTION
!
        if (mozyme) then
          estime = dipole_for_MOZYME (deldip(1,i),0)          
        else
          estime = dipole(p,xparam,deldip(1,i),0) 
        end if
       
        xparam(i) = xparam(i) - delta 
        grad(1) = escf ! dummy use of escf and grad 
        emin = 0.d0
        call compfg (xparam, .TRUE., escf, .TRUE., grad, .TRUE.) 
        if (moperr) return  
        if (debug) write (iw, '(A,F12.5)') ' GNORM -0.5*DELTA', sqrt(dot(grad,grad,nvar)) 
        call chrge (p, q) 
        q(:numat) = tore(labels(:numat)) - q(:numat) 
!
!   ESTIME IS USED HERE AS DIPOLE IS A FUNCTION
!
        if (mozyme) then
          estime = dipole_for_MOZYME (del2, 0)          
        else
          estime = dipole(p,xparam,del2, 0) 
        end if
        xparam(i) = xparam(i) + delta*0.5D0 
        deldip(1,i) = (deldip(1,i)-del2(1))*0.5D0/delta 
        deldip(2,i) = (deldip(2,i)-del2(2))*0.5D0/delta 
        deldip(3,i) = (deldip(3,i)-del2(3))*0.5D0/delta 
        ll = lu + 1 
        lu = ll + i - 1 
        l = 0 
        if (precis) then 
          if (lu - ll + 1 > 0) then 
!
!       G2OLD = X + 1.0*DELTA
!       GROLD = X + 0.5*DELTA
!       GRAD  = X - 0.5*DELTA
!       G2RAD = X - 1.0*DELTA
!
            dumy(:lu-ll+1) = (8.D0*(grold(:lu-ll+1)-grad(:lu-ll+1)) - &
              (g2old(:lu-ll+1)-g2rad(:lu-ll+1)))/delta*fact/12.D0 
            eigs(:lu-ll+1) = (2.D0*(grold(:lu-ll+1)-grad(:lu-ll+1)) - &
              (g2old(:lu-ll+1)-g2rad(:lu-ll+1)))/delta**3*fact/28.D0 
!
!  CORRECT FOR 4'TH ORDER CONTAMINATION
!
            fmatrx(ll:lu) = fmatrx(ll:lu) + dumy(:lu-ll+1) 
            l = lu - ll + 1 
          endif 
          l = l - 1 
          do k = i, nvar 
            l = l + 1 
            kk = (k*(k - 1))/2 + i 
            dumy(l) = (8.D0*(grold(l)-grad(l))-(g2old(l)-g2rad(l)))/delta*fact/12.D0 
            eigs(l) = (2.D0*(grold(l)-grad(l))-(g2old(l)-g2rad(l)))/delta**3*fact/28.D0 
!
!  CORRECT FOR 4'TH ORDER CONTAMINATION
!
            fmatrx(kk) = fmatrx(kk) + dumy(l) 
          end do 
        else 
          if (lu - ll + 1 > 0) then 
            dumy(l+1:lu-ll+1+l) = (grold(l+1:lu-ll+1+l)-grad(l+1:lu-ll+1+l))*0.5D0/delta*fact 
            fmatrx(ll:lu) = fmatrx(ll:lu) + dumy(l+1:lu-ll+1+l) 
            l = lu - ll + 1 + l 
          endif 
          l = l - 1 
          do k = i, nvar 
            l = l + 1 
            kk = (k*(k - 1))/2 + i 
            dumy(l) = (grold(l)-grad(l))*0.5D0/delta*fact 
            fmatrx(kk) = fmatrx(kk) + dumy(l) 
          end do 
        endif 
        if (big) then 
          write (iw, '(A)') ' CONTRIBUTIONS TO F-MATRIX' 
          write (iw, '(A)') &
      ' ELEMENT  +1.0*DELTA  +0.5*DELTA  -0.5*DELTA  -1.0*DELTA   2''ND ORDER 4TH ORDER' 
          write (iw, '(I7,6F12.6)') (l,g2old(l),grold(l),grad(l),g2rad(l),dumy(l),eigs(l),l=1,nvar) 
        endif 
        time3 = second(2) 
        tstep = time3 - time2 
        tleft = max(0.1D0,tleft - tstep) 
        if (tstep > 1.D7) tstep = tstep - 1.D7 
        totime = totime + tstep 
        if (resfil) then 
          write (iw, &
      '('' STEP:'',I4,'' RESTART FILE WRITTEN, INTEGRAL ='',F10.2,'' TIME LEFT:'',F10.2)') &
      i, min(totime,9999999.9D0), tleft 
          endfile (iw) 
          backspace (iw) 
          if (log) write (ilog, &
      '('' STEP:'',I4,'' RESTART FILE WRITTEN, INTEGRAL ='',F10.2,'' TIME LEFT:'',F10.2)') &
      i, min(totime,9999999.9D0), tleft 
          resfil = .FALSE. 
        else 
          write (iw, &
      '('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, INTEGRAL ='',F10.2,'' TIME LEFT:'',F10.2)') &
      i, tstep, totime, tleft 
          endfile (iw) 
          backspace (iw) 
          if (log) write (ilog, &
      '('' STEP:'',I4,'' TIME ='',F9.2,'' SECS, '',''INTEGRAL ='',F10.2,'' TIME LEFT:'',F10.2)') &
       i, tstep, totime, tleft 
        endif 
        write (line, '('' STEP:'',I4,''       '',16X,  ''INTEGRAL =''   ,F10.2,'' TIME LEFT:''&
      &,F10.2)') i, totime, tleft 
        call to_screen(line)
        estim = totime/i 
        if (tlast - tleft > tdump) then 
          tlast = tleft 
          resfil = .TRUE. 
          jstart = 1 
          ii = i 
          call forsav (totime, deldip, ii, fmatrx, coord, nvar, heat, evecs, &
            jstart, fconst) 
          if (moperr) return  
        endif 
        if (.not.(i /= nvar .and. tleft - 10.D0 < estim .or. i - istart >= maxcyc - 1)) &
          cycle  
        write (iw, &
      '(2/10X,''- - - - - - - TIME UP - - - - - - -'',2/)') 
        write (iw, '(/10X,'' POINT REACHED ='',I4)') i 
        write (iw, '(/10X,'' RESTART USING KEY-WORD "RESTART"'')') 
        write (iw, &
          '(10X,''ESTIMATED TIME FOR THE NEXT STEP ='',F9.2, '' SECONDS'')') estim 
        jstart = 1 
        ii = i 
        call forsav (totime, deldip, ii, fmatrx, coord, nvar, heat, evecs, &
          jstart, fconst) 
        if (moperr) return  
        write (iw, '(2/10X,''FORCE MATRIX WRITTEN TO DISK'')') 
        nreal = -1 
        return  
      end do 
      do i = 1, natoms 
        if (atmass(i)>=1.D-20 .or. labels(i)>=99) cycle  
        call forsav (totime, deldip, nvar, fmatrx, coord, nvar, heat, evecs, &
          iloop, fconst) 
        if (moperr) return  
        write (iw, '(A)') ' AT LEAST ONE ATOM HAS A ZERO MASS. A RESTART' 
        write (iw, '(A)') ' FILE HAS BEEN WRITTEN AND THE JOB STOPPED' 
        call mopend (&
       'AT LEAST ONE ATOM HAS A ZERO MASS.  A RESTART FILE HAS BEEN WRITTEN AND &
       &THE JOB STOPPED') 
        return  
      end do 
      if (istart<=nvar .and. index(keywrd,'ISOT')/=0) &
        call forsav (totime, deldip, nvar, fmatrx, coord, nvar, heat, evecs, iloop, fconst) 
      if (moperr) return  
      return  
      end subroutine fmat 
