      subroutine bangle(xyz, i, j, k, angle) 
!-----------------------------------------------
!   M o d u l e s 
!-----------------------------------------------
      USE vast_kind_param, ONLY:  double
      use common_arrays_C, only: tvec
      use molkst_C, only : natoms, id, l11, l21, l31
!...Translated by Pacific-Sierra Research 77to90  4.4G  08:33:12  03/09/06  
!...Switches: -rl INDDO=2 INDIF=2 
      implicit none
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer , intent(in) :: i 
      integer , intent(in) :: j 
      integer , intent(in) :: k 
      real(double) , intent(out) :: angle 
      real(double) , intent(in) :: xyz(3,natoms + 3) 
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      real(double) :: d2ij, d2jk, d2ik, xy, temp, Vab(3), Rab
      integer :: ii, jj, kk
!-----------------------------------------------
!********************************************************************
!
! BANGLE CALCULATES THE ANGLE BETWEEN ATOMS I,J, AND K. THE
!        CARTESIAN COORDINATES ARE IN XYZ.
!
!********************************************************************
      if (id == 0) then
        d2ij = (xyz(1,i)-xyz(1,j))**2 + (xyz(2,i)-xyz(2,j))**2 + (xyz(3,i)-xyz(3,j))**2 
        d2jk = (xyz(1,j)-xyz(1,k))**2 + (xyz(2,j)-xyz(2,k))**2 + (xyz(3,j)-xyz(3,k))**2 
        d2ik = (xyz(1,i)-xyz(1,k))**2 + (xyz(2,i)-xyz(2,k))**2 + (xyz(3,i)-xyz(3,k))**2 
      else
        d2ij = 1.d8
        d2jk = 1.d8
        d2ik = 1.d8
        do ii = -l11, l11 
          do jj = -l21, l21 
            do kk = -l31, l31 
              Vab = xyz(:,i) - xyz(:,j) + tvec(:,1)*ii + tvec(:,2)*jj + tvec(:,3)*kk 
              Rab = Vab(1)**2 + Vab(2)**2 + Vab(3)**2 
              if (Rab < d2ij) then
                d2ij = Rab
              end if  
              Vab = xyz(:,k) - xyz(:,j) + tvec(:,1)*ii + tvec(:,2)*jj + tvec(:,3)*kk 
              Rab = Vab(1)**2 + Vab(2)**2 + Vab(3)**2 
              if (Rab < d2jk) then
                d2jk = Rab
              end if   
              Vab = xyz(:,i) - xyz(:,k) + tvec(:,1)*ii + tvec(:,2)*jj + tvec(:,3)*kk 
              Rab = Vab(1)**2 + Vab(2)**2 + Vab(3)**2 
              if (Rab < d2ik) then
                d2ik = Rab
              end if                   
            end do 
          end do 
        end do 
      end if 
      xy = sqrt(d2ij*d2jk)
      if (xy < 1.d-20) then
        angle = 0.d0
        return
      end if
      temp = 0.5D0*(d2ij + d2jk - d2ik)/xy 
      temp = min(1.0D0,temp) 
      temp = dmax1(-1.0D0,temp) 
      angle = acos(temp) 
      return  
      end subroutine bangle 
