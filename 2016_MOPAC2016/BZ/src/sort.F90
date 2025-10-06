subroutine sort (val, vec, n)
  implicit none
  integer, intent(in) :: n
  real, dimension(*), intent(inout) :: val
  complex, dimension(n, *), intent(inout) :: vec
  integer :: i, j, k
  real :: x
  complex :: sum
  ! 
  ! ... Executable Statements ...
  ! 
  do i = 1, n
    x = 1.e9
    do j = i, n
      if (val(j) < x) then
        k = j
        x = val(j)
      end if
    end do
    do j = 1, n
      sum = vec(j, k)
      vec(j, k) = vec(j, i)
      vec(j, i) = sum
    end do
    val(k) = val(i)
    val(i) = x
  end do
end subroutine sort
