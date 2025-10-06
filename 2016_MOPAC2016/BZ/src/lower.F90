subroutine lower (a, n)
  implicit none
  character(len=*), intent(inout) :: a
  integer, intent(in) :: n
  integer :: i, j, lowa, lupa, lupz
  intrinsic Char, Ichar
  lowa = Ichar ("a")
  lupa = Ichar ("A")
  lupz = Ichar ("Z")
  do i = 1, n
    if (Ichar (a(i:i))>=lupa .and. Ichar (a(i:i))<=lupz) then
      j = Ichar (a(i:i)) + lowa - lupa
      a(i:i) = Char(j)
    end if
  end do
end subroutine lower
