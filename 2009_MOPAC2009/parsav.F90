      subroutine parsav(mode, n, m, q, r, efslst, xlast, iiium) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double    
      USE molkst_C, ONLY:  keywrd, numat, norbs
      USE chanel_C, only : ires, iw, restart_fn
      use common_arrays_C, only : aicorr, errfn
!***********************************************************************
!DECK MOPAC
!...Translated by Pacific-Sierra Research 77to90  4.4G  12:17:05  03/10/06  
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
      integer , intent(in) :: mode 
      integer  :: n, m, iiium(6)
      real(double)  :: q(n,n) 
      real(double), dimension(n) :: efslst, xlast
      real(double)  :: r(n + 2,n + 2) 
!-----------------------------------------------
!   L o c a l   P a r a m e t e r s
!-----------------------------------------------
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer ::  i, j, io_stat, old_numat, old_norbs
      logical :: opend 
!-----------------------------------------------
!*********************************************************************
!
!   PARSAV SAVES AND RESTORES DATA USED IN NLLSQ GRADIENT MINIMIZATION.
!
!    IF MODE IS 0 DATA ARE RESTORED, IF 1 THEN SAVED.
!
!*********************************************************************
      inquire(unit=ires, opened=opend) 
      if (opend) close(unit=ires, status='KEEP') 
      open(unit=ires, file=restart_fn, status='UNKNOWN', form=&
        'UNFORMATTED', position='asis', iostat = io_stat) 
      if (io_stat /= 0) then
          write(iw,*)" Restart file either does not exist or is not available for reading"
          call mopend ("Restart file either does not exist or is not available for reading")
          return
        end if 
      rewind ires 
      if (mode == 0) then 
!
!  MODE=0: RETRIEVE DATA FROM DISK.
!
        read (ires, iostat = io_stat) old_numat, old_norbs, (xlast(i),i=1,n), m, iiium, efslst, n
        if (norbs /= old_norbs .or. numat /= old_numat) then
              write(iw,*)" Restart file read in does not match current data set"
              call mopend (" Restart file read in does not match current data set")
              return
        end if
        read (ires, iostat = io_stat) ((q(j,i),j=1,m),i=1,m) 
        read (ires, iostat = io_stat) ((r(j,i),j=1,n),i=1,n) 
        if (index(keywrd,'AIDER') /= 0) then
          read (ires, iostat = io_stat) (aicorr(i),i=1,n)
          read (ires, iostat = io_stat) (errfn(i),i=1,n)
        end if
        if (io_stat /= 0) then
          write(iw,*)" Restart file is currupt"
          call mopend ("Restart file is currupt")
        end if
        return  
      endif 
      if (mode == 1) then 
        write (iw, &
      '(2/10X,                                              ''- - - - - - - TIM&
      &E UP - - - - - - -'',2/)') 
        write (iw, '(10X,A)') ' - THE CALCULATION IS BEING DUMPED TO DISK', &
          '   RESTART IT USING THE KEY-WORD "RESTART"' 
        write (iw, '(/10X,''CURRENT VALUE OF GEOMETRY'',/)') 
        call geout (iw) 
      endif 
      write (ires) numat, norbs, (xlast(i),i=1,n), m, iiium, efslst, n
      write (ires) ((q(j,i),j=1,m),i=1,m) 
      write (ires) ((r(j,i),j=1,n),i=1,n) 
      if (index(keywrd,'AIDER') /= 0) write (ires) (aicorr(i),i=1,n) 
      if (index(keywrd,'AIDER') /= 0) write (ires) (errfn(i),i=1,n) 
!*****
!     The density matrix is required by ITER upon restart .
!    
      call den_in_out(1)
!
!*****
      close(ires) 
      return  
      end subroutine parsav 
