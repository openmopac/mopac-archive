subroutine filusp (nat, nfirst, nlast, uspd)
    use molkst_C, only :  norbs, numat
    use parameters_C, only : uss, upp, udd
  !
  !.. Implicit Declarations ..
    implicit none
  !
  !.. Formal Arguments ..
    integer, dimension (numat), intent (in) :: nat, nfirst, nlast
    double precision, dimension (norbs), intent (out) :: uspd
  !
  !.. Local Scalars ..
    integer :: i, ia, ib, j, ni_loc
  !
  ! ... Executable Statements ...
  !
    do i = 1, numat
      ni_loc = nat(i)
      ia = nfirst(i)
      ib = nlast(i)
      if (ia <= ib) then
      !
      !  Atom has an "s" orbital.
      !
        uspd(ia) = uss(ni_loc)
        if (ia /= ib) then
        !
        !  Atom has a "p" shell.
        !
          do j = ia + 1, ia + 3
            uspd(j) = upp(ni_loc)
          end do
        !
        !  Set the "d" shell, if present.
        !
          do j = ia + 4, ib
            uspd(j) = udd(ni_loc)
          end do
        end if
      end if
    end do
end subroutine filusp
