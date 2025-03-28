      subroutine anavib(eigs, dipt, n3, vibs, rij, nv, hess, f) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double 
      USE molkst_C, only : numat
      use common_arrays_C, only : nat, coord
      USE elemts_C, only : elemnt 
      USE funcon_C, only : fpc_10, fpc_8, pi
      use to_screen_C, only : travel, redmas
      USE chanel_C, only : iw 
      USE symmetry_C, only :  jndex, namo
!...Translated by Pacific-Sierra Research 77to90  4.4G  11:19:44  03/16/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: n3 
      integer , intent(in) :: nv 
      real(double) , intent(in) :: eigs(n3) 
      real(double) , intent(in) :: dipt(n3) 
      real(double) , intent(in) :: vibs(n3,n3) 
      real(double) , intent(inout) :: rij((numat*(numat + 1))/2) 
      real(double) , intent(in) :: hess((3*numat*(3*numat + 1))/2) 
      real(double) , intent(inout) :: f((numat*(numat + 1))/2) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer , dimension(10) :: ijf 
      integer :: l, i, j, iline, k, j3, linear, j1, i1, jj, ii, ij, j2, i3&
        , i2 
      real(double), dimension(107) :: vanrad 
      real(double), dimension(10) :: fij 
      real(double) :: tot, vdw, eab, eb, ea, sum, xj, yj, zj, xi, yi, zi, x, y&
        , z, e, shift, radial, ans 
      logical :: vib1, vib2, vib3, vib4, vib5

      save vanrad 
!-----------------------------------------------
      data vanrad/ 0.32D0, 0.93D0, 1.23D0, 0.90D0, 0.82D0, 0.77D0, 0.75D0, &
        0.73D0, 0.72D0, 0.71D0, 1.54D0, 1.36D0, 1.18D0, 1.11D0, 1.06D0, 1.02D0&
        , 0.99D0, 0.98D0, 2.03D0, 1.74D0, 1.44D0, 1.32D0, 1.22D0, 1.18D0, &
        1.17D0, 1.17D0, 1.16D0, 1.15D0, 1.17D0, 1.25D0, 1.26D0, 1.22D0, 1.20D0&
        , 1.16D0, 1.14D0, 1.12D0, 2.16D0, 1.91D0, 1.62D0, 1.45D0, 1.34D0, &
        1.30D0, 1.27D0, 1.25D0, 1.25D0, 1.28D0, 1.34D0, 1.48D0, 1.44D0, 1.41D0&
        , 1.40D0, 1.36D0, 1.33D0, 1.31D0, 2.35D0, 1.98D0, 1.69D0, 1.65D0, &
        1.65D0, 1.64D0, 1.63D0, 1.62D0, 1.85D0, 1.61D0, 1.59D0, 1.59D0, 1.58D0&
        , 1.57D0, 1.56D0, 1.56D0, 1.56D0, 1.44D0, 1.34D0, 1.30D0, 1.28D0, &
        1.26D0, 1.27D0, 1.30D0, 1.34D0, 1.49D0, 1.48D0, 1.47D0, 1.46D0, 1.46D0&
        , 1.45D0, 1.45D0, 21*1.45D0/  
!
!    COMPUTE INTERATOMIC DISTANCES.
!
      l = 0 
      do i = 1, numat 
        do j = 1, i - 1 
          l = l + 1 
          rij(l) = sqrt((coord(1,j)-coord(1,i))**2+(coord(2,j)-coord(2,i))**2+(&
            coord(3,j)-coord(3,i))**2) + 1.D-10 
        end do 
      end do 
!
!     ANALYSE VIBRATIONS
!
      write (iw, '(2/10X,''DESCRIPTION OF VIBRATIONS'',/)') 
      iline = 0 
      do k = 1, nv 
        vib1 = .TRUE. 
        vib2 = .TRUE. 
        vib3 = .TRUE. 
        vib4 = .TRUE. 
        vib5 = .TRUE. 
        j3 = 0 
        l = 0 
        tot = 0.D0 
        linear = 0 
        j1 = -2 
        do j = 1, numat 
          j1 = j1 + 3 
          i1 = -2 
          do i = 1, j - 1 
            i1 = i1 + 3 
            vdw = (vanrad(nat(i))+vanrad(nat(j)))*1.5D0 
            l = l + 1 
            f(l) = 0.D0 
            if (rij(l) >= vdw) cycle  
!
! CALCULATE ENERGY TERM BETWEEN THE TWO ATOMS
!
            eab = 0.D0 
            do jj = j1, j1 + 2 
              do ii = i1, i1 + 2 
                eab = eab + vibs(jj,k)*hess((jj*(jj-1))/2+ii)*vibs(ii,k) 
              end do 
            end do 
            eb = 0.D0 
            do jj = j1, j1 + 2 
              do ii = j1, jj 
                eb = eb + vibs(jj,k)*hess((jj*(jj-1))/2+ii)*vibs(ii,k)*2.D0 
              end do 
              eb = eb - vibs(jj,k)*hess((jj*(jj+1))/2)*vibs(jj,k) 
            end do 
            ea = 0.D0 
            do jj = i1, i1 + 2 
              do ii = i1, jj 
                ea = ea + vibs(jj,k)*hess((jj*(jj-1))/2+ii)*vibs(ii,k)*2.D0 
              end do 
              ea = ea - vibs(jj,k)*hess((jj*(jj+1))/2)*vibs(jj,k) 
            end do 
            linear = linear + 1 
            f(l) = ea + eab*2.D0 + eb 
            tot = tot + f(l) 
          end do 
        end do 
        if (k == nv .or. jndex (k) .ne. jndex(k+1) .or. namo (k) .ne. namo(k+1)) then
        if (abs(tot) >= 1.D-5) then 
!
!  NOW TO SORT F INTO DECENDING ORDER
!
          do i = 1, 10 
            sum = -100.D0 
            do j = 1, l 
              if (abs(f(j)) <= sum) cycle  
              jj = j 
              sum = abs(f(j)) 
            end do 
            if (sum < 0.D0) go to 100 
            fij(i) = sum 
            f(jj) = -1.D-9 
            ijf(i) = jj 
          end do 
          i = 10 
  100     continue 
          linear = i 
          sum = 1.D0/tot 
          do ij = 1, linear 
            j = int(0.5D0*(0.99D0 + sqrt(1.D0 + 8.D0*ijf(ij)))) 
            i = ijf(ij) - (j*(j - 1))/2 
            j = j + 1 
            xj = coord(1,j) 
            yj = coord(2,j) 
            zj = coord(3,j) 
            j1 = 3*j - 2 
            j2 = j1 + 1 
            j3 = j2 + 1 
            i3 = 0 
            xi = coord(1,i) 
            yi = coord(2,i) 
            zi = coord(3,i) 
            i1 = 3*i - 2 
            i2 = i1 + 1 
            i3 = i2 + 1 
            x = vibs(j1,k) - vibs(i1,k) 
            y = vibs(j2,k) - vibs(i2,k) 
            z = vibs(j3,k) - vibs(i3,k) 
            e = fij(ij)*sum*100.D0 
            shift = x*x + y*y + z*z + 1.D-30 
            if (.not.(abs(e)>10.D0 .or. ij<5 .and. abs(e)>0.1D0)) cycle  
            shift = sqrt(shift) 
            radial = ((x*(xi - xj) + y*(yi - yj) + z*(zi - zj))/(shift*rij(ijf(ij))))**2*100.D0 
              ans = min(999.d0, max(-99.9d0, 100.D0*sqrt(fij(ij)*1.D5*fpc_10)/(fpc_8*pi*2.D0)/eigs(k)))
            if (vib1) then 
              write (iw, &
      '(/,'' VIBRATION'',I5,I5,A4,''    ATOM PAIR        ENERGY CONTRIBUTION    RADIAL'')') &
       k, jndex(k), namo(k) 
              write (iw, &
              & '('' FREQ.   '',F10.2,7X,1A2,I3,'' -- '',A2,I3, ''      '',sp, F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               eigs(k), elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
              vib1 = .FALSE. 
            else if (vib2) then 
              vib2 = .FALSE. 
              write (iw, &
              & '('' T-DIPOLE'',F10.4,7X,1A2,I3,'' -- '',A2,I3, ''      '', sp,F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               dipt(k), elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
            else if (vib3) then 
              vib3 = .FALSE. 
              write (iw, &
              & '('' TRAVEL  '',F10.4,7X,1A2,I3,'' -- '',A2,I3, ''      '', sp,F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               travel(k), elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
            else if (vib4) then 
              vib4 = .FALSE. 
              write (iw, &
              & '('' RED. MASS'',F9.4,7X,1A2,I3,'' -- '',A2,I3, ''      '',sp, F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               redmas(k, 1), elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
            else if (vib5) then
               vib5 = .false.
               write (iw, &
              & '('' EFF. MASS'',F9.4,7X,1A2,I3,'' -- '',A2,I3, ''      '',sp, F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               redmas(k, 2), elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
            else 
              iline = iline + 1 
              write (iw, & 
              & '(''                          '',1A2,I3,'' -- '',A2,I3, ''      '', sp,F9.1,ss,''% ('',F5.1,''%)'',F8.1,''%'')') &
               elemnt(nat(i)), i, elemnt(nat(j)), j, e, ans, radial 
            endif 
          end do 
        endif 
        if (vib1) then 
          write (iw, '(/,'' VIBRATION'',I4)') k 
          write (iw, '(  '' FREQ.    '',F8.2)') eigs(k) 
        endif 
        if (vib2) write (iw, '(  '' T-DIPOLE '',F9.4)') dipt(k) 
        if (vib3) write (iw, '(  '' TRAVEL   '',F9.4)') travel(k) 
        if (vib4) write (iw, '(  '' RED. MASS'',F9.4)') redmas(k, 1) 
        if (vib5) write (iw, '(  '' EFF. MASS'',F9.4)') min(9999.9999d0,max(-999.9999d0,redmas(k, 2))) 
        end if
      end do 
      return  
      end subroutine anavib 
