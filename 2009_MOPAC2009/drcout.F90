      subroutine drcout(xyz3, geo3, vel3, nvar, time, escf3, ekin3, etot3, &
        xtot3, iloop, charge, fract, text1, text2, ii, jloop) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      use common_arrays_C, only : na, nb, nc, labels, loc, nat, c, eigs
      use molkst_C, only : natoms, numcal, keywrd, numat, title, koment, line
      use maps_C, only : rxn_coord, rc_escf, ekin
      use elemts_C, only : elemnt
      use chanel_C, only : iw, iw0
      use reada_I 
      use write_trajectory_I
      implicit none
      integer, intent(in) :: nvar, iloop, ii 
      integer, intent(inout) :: jloop
      real(double), intent(in) :: time, fract 
      character, intent(in) :: text1*3, text2*2 
      real(double), intent(in) :: xyz3(3,nvar), geo3(3,3*numat), vel3(3,nvar), &
        escf3(3), ekin3(3), etot3(3), xtot3(3), charge(natoms) 
!
      integer, dimension(3) :: iel1 
      integer :: prtkey, prtkom, prtitl, i, icalcn, iprint, l, j, ivar, k, iw00 
      real(double), dimension(3,numat) :: xyz, vel 
      real(double), dimension(3) :: gg 
      real(double) :: etot, errr, last_point = -1.d8
      logical :: drc, large, graph, run_local
      character :: alpha*2, frmat*1
      save prtkey, prtkom, prtitl, i, drc, icalcn, iprint, large, graph, &
        run_local
!************************************************************
!                                                           *
!    DRCOUT PRINTS THE GEOMETRY, ETC. FOR A DRC AT A        *
!    POSITION DETERMINED BY FRACT.                          *
!    ON INPUT XYZ3  = QUADRATIC EXPRESSION FOR THE GEOMETRY *
!             VEL3  = QUADRATIC EXPRESSION FOR THE VELOCITY *
!             ESCF3 = QUADRATIC EXPRESSION FOR THE P.E.     *
!             EKIN3 = QUADRATIC EXPRESSION FOR THE K.E.     *
!                                                           *
!************************************************************
      data icalcn/ 0/  
      if (icalcn /= numcal) then 
        icalcn = numcal 
        graph = (index(keywrd,' GRAPH') /= 0)
        run_local = (index(keywrd,' LOCAL') /= 0)
        if (index(keywrd,'RESTART')==0 .or. index(keywrd,'IRC=')/=0) jloop = 0 
        do i = 80, 1, -1 
          if (keywrd(i:i) /= ' ') go to 20 
        end do 
        i = 1 
   20   continue 
        prtkey = i 
        do i = 80, 1, -1 
          if (koment(i:i) /= ' ') go to 40 
        end do 
        i = 1 
   40   continue 
        prtkom = i 
        do i = 80, 1, -1 
          if (title(i:i) /= ' ') go to 60 
        end do 
        i = 1 
   60   continue 
        prtitl = i 
        drc = index(keywrd,'DRC') /= 0         
        i = index(keywrd,'LARGE') 
        iprint = 10000 
        large = .FALSE. 
        if (i /= 0) then 
          iprint = 1 
          i = i + 5 
          large = keywrd(i:i)==' ' .or. keywrd(i+1:i+1)=='-' 
          if (keywrd(i:i) == '=') iprint = nint(abs(reada(keywrd,i))) 
        endif 
      endif 
      if (jloop==0 .or. (jloop/iprint)*iprint==jloop) then 
        if (drc) then 
          write (line, &
      '('' FEMTOSECONDS  POINT  POTENTIAL +''         ,'' KINETIC  =   TOTAL &
      &    ERROR    REF%   MOVEMENT'')') 
        else 
          write (line, &
      '(''     POINT   POTENTIAL  +  ENERGY LOST   =   TOTAL      ERROR    REF%   MOVEMENT'')') 
        endif 
        write(iw,'(2/,a)')trim(line)
        iw00 = iw0
        iw0 = 0
        call to_screen(trim(line))
        iw0 = iw00
      endif 
      if (drc) then
!
! Sanity check - don't print two points that are the same.
!
        if (Abs(last_point - time) < 5.d-4) then
          return
        else
          last_point = time
        end if
      end if
      jloop = jloop + 1 
      rc_escf = escf3(1) + escf3(2)*fract + escf3(3)*fract**2 
      ekin = ekin3(1) + ekin3(2)*fract + ekin3(3)*fract**2 
      etot = etot3(1) + etot3(2)*fract + etot3(3)*fract**2 
      rxn_coord = xtot3(1) + xtot3(2)*fract + xtot3(3)*fract**2 
      errr = min(9999.99999D0,max(-999.99999D0,rc_escf + ekin - etot)) 
      if (rc_escf > 99999.d0 .or. rc_escf < -9999.d0) then
        frmat = "3"
      else if (rc_escf > 9999.d0 .or. rc_escf < -999.d0) then
        frmat = "4"
      else
        frmat = "5"
      end if
      if (ii /= 0) then 
        if (drc) then 
          write (line, &
      '(F10.3,I8,F12.'//frmat//',F11.5,F12.'//frmat//',                      F10.5,'' '',I5,3X,''%'&
      &',A,A,I3)') time, iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, text1&
            , text2, ii 
          write (line, '(9X,A,F9.4,A)') ' MOVEMENT FROM START =', rxn_coord,' ANGSTROMS' 
        else 
          write (line, &
      '(I8,F14.'//frmat//',F13.5,F17.5,F10.5,I6,''   %'',A,A,I3)') &
      iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, text1, &
            text2, ii 
          write (line, '(9X,A,F9.4,A)') ' MOVEMENT FROM START =', rxn_coord, ' ANGSTROMS' 
        endif 
      else 
        if (drc) then 
          if (text1==' ' .and. text2==' ') then 
            
            write (line, &
      '(F10.3,I8,F12.'//frmat//',F11.5,F12.'//frmat//',F10.5,'' '',I5,3X,''%'',F8.4)') &
      time, iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, rxn_coord 
          else 
            write (line, &
      '(F10.3,I8,F12.'//frmat//',F11.5,F12.'//frmat//',F10.5,'' '',I5,3X,''%'',A,A,I3)') &
      time, iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, text1, &
              text2 
          endif 
        else 
          if (text1==' ' .and. text2==' ') then 
            write (line, &
      '(I8,F14.'//frmat//',F13.5,F17.5,F10.5,'' '',I5,3X,''%'',F8.4)') &
      iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, rxn_coord 
          else 
            write (line, &
      '(I8,F14.'//frmat//',F13.5,F17.5,F10.5,'' '',I5,3X,''%'',A,A,I3)') &
      iloop - 2, rc_escf, ekin, rc_escf + ekin, errr, jloop, text1, text2 
          endif 
        endif 
      endif 
      write(iw,'(a)')trim(line)
      iw00 = iw0
      iw0 = 0
      call to_screen(trim(line))
      iw0 = iw00
      natoms = nvar/3 
      l = 0 
      do i = 1, natoms 
        vel(:,i) = vel3(1,l+1:3+l) + vel3(2,l+1:3+l)*fract + vel3(3,l+1:3+l)*fract**2 
        xyz(:,i) = xyz3(1,l+1:3+l) + xyz3(2,l+1:3+l)*fract + xyz3(3,l+1:3+l)*fract**2 
        l = 3 + l 
      end do 
      if (graph) then
        if (run_local)  call local (c, i, eigs, 0, "c ") 
        call mullik () 
      end if
      call to_screen("To_file: IRC-DRC")
      if (large .and. (jloop/iprint)*iprint==jloop) then 
        write (iw, '(A)') &
          '                CARTESIAN GEOMETRY           VELOCITY (IN CM/SEC)' 
        write (iw, '(A)') &
      '  ATOM        X          Y          Z                X          Y          Z' 
        do i = 1, numat 
          write (iw, '(I4,3X,A2,3F11.5,2X,3F11.1)') i, elemnt(nat(i)), &
          (xyz(j,i),j=1,3), (vel(j,i),j=1,3) 
        end do 
      endif 
!
!   Write out trajectory for graphics
!
      if (drc) then
        call write_trajectory(xyz, 1, charge, rc_escf, ekin, time, rxn_coord)
      else
        call write_trajectory(xyz, 1, charge, rc_escf, errr, 0.d0, rxn_coord)
      end if
      if ((jloop/iprint)*iprint == jloop) then 
        ivar = 1  
        l = 0 
        write (iw, '(2/10X,''FINAL GEOMETRY OBTAINED'',33X,''CHARGE'')') 
        write (iw, '(A)') keywrd(:prtkey), koment(:prtkom), title(:prtitl) 
        l = 0 
        do i = 1, numat 
          j = i/26 
          alpha(1:1) = char(ichar('A') + j) 
          j = i - j*26 
          alpha(2:2) = char(ichar('A') + j - 1) 
          iel1 = 0 
  110     continue 
          if (loc(1,ivar) == i) then 
            iel1(loc(2,ivar)) = 1 
            ivar = ivar + 1 
            go to 110 
          endif 
          if (i < 4) then 
            iel1(3) = 0 
            if (i < 3) then 
              iel1(2) = 0 
              if (i < 2) iel1(1) = 0 
            endif 
          endif 
          if (labels(i)<99 .or. labels(i)>102 .and. labels(i)<107) then 
            l = l + 1 
            gg(1) = geo3(1,i*3-2) + geo3(2,i*3-2)*fract + geo3(3,i*3-2)*fract**2 
            gg(2) = geo3(1,i*3-1) + geo3(2,i*3-1)*fract + geo3(3,i*3-1)*fract**2 
            gg(3) = geo3(1,i*3) + geo3(2,i*3)*fract + geo3(3,i*3)*fract**2 
            write (iw, '(2X,A2,3(F12.6,I3),I4,2I3,F10.4,I8,A)') elemnt(labels(i)), &
             (gg(k),iel1(k),k=1,3), na(i), nb(i), nc(i), charge(l), jloop, alpha//'*' 
          else 
            write (iw, '(2X,A2,3(F12.6,I3),I4,2I3,10X,I8,A)') elemnt(labels(i)), &
            (gg(k),iel1(k),k=1,3), na(i), nb(i), nc(i), jloop, alpha//'%' 
          endif 
        end do 
      endif 
      return  
      end subroutine drcout 
