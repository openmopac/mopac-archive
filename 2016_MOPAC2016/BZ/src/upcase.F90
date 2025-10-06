subroutine upcase (keywrd)
  implicit none
  character(len=*), intent(inout) :: keywrd
!
  integer :: i, icapa, iline, ilowa, ilowz
  icapa = Ichar ("A")
  ilowa = Ichar ("a")
  ilowz = Ichar ("z")
  do i = 1, Len (keywrd)
    iline = Ichar (keywrd(i:i))
    if (iline>=ilowa .and. iline<=ilowz) then
      keywrd(i:i) = Char(iline+icapa-ilowa)
    end if
  end do
end subroutine upcase
