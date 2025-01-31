! ================================================================================================================================ !
module utilities
  !! Contains general utility procedures, such as routines to determine if several integers are all equal or to swap the values of
  !! two variables.

  use types,     only: rp
  use constants, only: zero

  implicit none

  private

  public is_even
  public swapvars
  public same_integers
  public read_blank
  public reals_are_eq

  interface is_even
    module procedure is_even_integer
    ! module procedure is_even_real
    ! module procedure is_even_cplx
  end interface is_even

  interface swapvars
    module procedure swapvars_i
    module procedure swapvars_r
    module procedure swapvars_c
  end interface swapvars

  interface same_integers
    module procedure same_integers_2
    module procedure same_integers_3
    module procedure same_integers_4
  end interface same_integers


! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  !  IS_EVEN INTERFACE
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental function is_even_integer(i) result(answer)
    !! Test if the values of  the supplied argument is even
    implicit none
    integer, intent(in) :: i
    logical :: answer
    answer = .false.
    if(mod(i, 2) .ne. 0) return
    answer = .true.
  end function is_even_integer
  ! ! ------------------------------------------------------------------------------------------------------------------------------ !
  ! pure elemental function is_even_real(x) result(answer)
  !   implicit none
  !   real(rp), intent(in) :: x
  !   logical :: answer
  !   answer = .false.
  !   if(.not. is_integer(x)) return
  !   if(mod(nint(x), 2).ne.0) return
  !   answer = .true.
  ! end function is_even_real
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  ! pure elemental function is_even_cplx(z) result(answer)
  !   implicit none
  !   complex(rp), intent(in) :: z
  !   logical :: answer
  !   answer = .false.
  !   if(.not. is_integer(z)) return
  !   if(mod(nint(z%re),2).ne.0) return
  !   answer = .true.
  ! end function is_even_cplx

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  !  SWAPVARS INTERFACE
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental subroutine swapvars_i(a, b)
    !! Swap the values of the two supplied variables a and b
    integer, intent(inout) :: a
    integer, intent(inout) :: b
    integer :: c
    c = a
    a = b
    b = c
  end subroutine swapvars_i
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental subroutine swapvars_r(a, b)
    !! Swap the values of the two supplied variables a and b
    real(rp), intent(inout) :: a
    real(rp), intent(inout) :: b
    real(rp) :: c
    c = a
    a = b
    b = c
  end subroutine swapvars_r
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental subroutine swapvars_c(a, b)
    !! Swap the values of the two supplied variables a and b
    complex(rp), intent(inout) :: a
    complex(rp), intent(inout) :: b
    complex(rp) :: c
    c = a
    a = b
    b = c
  end subroutine swapvars_c

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  !  SAME_INTEGERS INTERFACE
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental function same_integers_2(a, b) result(answer)
    !! Test if all supplied integers are equal
    implicit none
    integer, intent(in)  :: a
    integer, intent(in)  :: b
    logical :: answer
    integer, allocatable :: args(:)
    args = [a, b]
    answer = .false.
    if(all(args .eq. args)) answer = .true.
  end function same_integers_2
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental function same_integers_3(a, b, c) result(answer)
    !! Test if all supplied integers are equal
    implicit none
    integer, intent(in)  :: a
    integer, intent(in)  :: b
    integer, intent(in)  :: c
    logical :: answer
    integer, allocatable :: args(:)
    args = [a, b, c]
    answer = .false.
    if(all(args .eq. args)) answer = .true.
  end function same_integers_3
  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure elemental function same_integers_4(a, b, c, d) result(answer)
    !! Test if all supplied integers are equal
    implicit none
    integer, intent(in)  :: a
    integer, intent(in)  :: b
    integer, intent(in)  :: c
    integer, intent(in)  :: d
    logical :: answer
    integer, allocatable :: args(:)
    args = [a, b, c, d]
    answer = .false.
    if(all(args .eq. args)) answer = .true.
  end function same_integers_4

  ! -------------------------------------------------------------------------------------------------------------------------------- !
  subroutine read_blank(read_unit, num_read)
    !! Reads num_read lines from unit read_unit, not storing any information. If num_read is not supplied, read one line.
    implicit none
    integer, intent(in)           :: read_unit
    integer, intent(in), optional :: num_read
    integer :: k, n
    n = 1 ; if(present(num_read)) n = num_read
    do k = 1, n ; read(read_unit,*) ; enddo
  end subroutine read_blank

  ! -------------------------------------------------------------------------------------------------------------------------------- !
  pure elemental function reals_are_eq(a, b, tol) result(ans)
    !! Test if two reals are within a tolerance. If `tol` is given, use that .
    !! Else, use 10 * machine epsilon

    use system, only: macheps

    implicit none

    real(rp), intent(in) :: a, b
    real(rp), intent(in), optional :: tol

    logical  :: ans
    real(rp) :: tol_local

    tol_local = 10 * macheps ; if(present(tol)) tol_local = tol

    ans = .false.

    if(abs(a - b) .le. tol_local) ans = .true.

  end function reals_are_eq

! ================================================================================================================================ !
end module utilities
! ================================================================================================================================ !
