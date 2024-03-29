      subroutine sp_two_electron
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      use parameters_C, only : zsn, zpn, gss, gsp, gpp, gp2, hsp, &
      main_group
      use mndod_C, only : iii
      USE vast_kind_param, ONLY:  double 
!...Translated by Pacific-Sierra Research 77to90  4.4G  10:46:59  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
!-----------------------------------------------
!   I n t e r f a c e   B l o c k s
!-----------------------------------------------
      use rsc_I  
      implicit none
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: ni, ns 
      real(double) :: es, ep, r033, r233 
!
      do ni = 1, 80 
        ns = iii(ni) 
        es = zsn(ni) 
        ep = zpn(ni)
        if (es < 1.d-4 .or. ep < 1.d-4 .or. main_group(ni)) cycle  
        gss(ni) = rsc(0,ns,es,ns,es,ns,es,ns,es) 
        gsp(ni) = rsc(0,ns,es,ns,es,ns,ep,ns,ep) 
        hsp(ni) = rsc(1,ns,es,ns,ep,ns,es,ns,ep)/3.D0 
        r033 = rsc(0,ns,ep,ns,ep,ns,ep,ns,ep) 
        r233 = rsc(2,ns,ep,ns,ep,ns,ep,ns,ep) 
        gpp(ni) = r033 + 0.16D0*r233 
        gp2(ni) = r033 - 0.08D0*r233 
      end do 
      return  
      end subroutine sp_two_electron 

