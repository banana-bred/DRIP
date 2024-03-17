! ===================================================================================================!
module types
  !! Contains the type definitions used throughout the program and procedures for converting some types,
  !! like converting between an integer and a logical

  use iso_fortran_env, only: int8, int16, int32, int64, real32, real64, real128

  implicit none

  private

  ! -- procedures
  public :: log2int
  public :: int2log

  ! -- types
  public :: big_char
  public :: initial_int
  public :: ip
  public :: rp
  public :: rrp
  public :: complex_pair

  integer, parameter :: big_char    = 2000
    !! Just a large value for a character array for when we need to initialize a large array
    !! prior to writing to the character array

  integer, parameter :: initial_int = -409
    !! For determining if some values have not been overwritten past initialization

  integer, parameter :: ip = int32
    !! The precision for integer types. The max value of int32 types is
    !! 2147483647

  integer, parameter :: rp = real64
    !! The precision for real and complex types. The max value of real64
    !! types is 1.7976931348623157E+308

  integer, parameter :: rrp = rp * 2
    !! Twice the precision of rp. If rp is a double (real64), rrp is a quad

  type :: complex_pair
    !! Basically, two different complex numbers. Useful when we have to take the hermitian adjoint of a matrix but
    !! unitarity is not defined with respect to the inner product but something else and we need to define (and store)
    !! hermitian adjoint equivalents differently

    complex(rp) :: elem

    complex(rp) :: elem_d

  end type complex_pair


! =================================================================================================== !
contains
! =================================================================================================== !

! --------------------------------------------------------------------------------------------------- !
pure elemental function log2int(l) result(i)
  !!  Convert logical to integer
  !!    .false. -> 0
  !!    .true.  -> 1

  implicit none

  logical, intent(in)  :: l

  integer :: i

  i = 1

  if(l .eqv. .true.) return

  i = 0

end function log2int

! --------------------------------------------------------------------------------------------------- !
pure elemental function int2log(i) result(l)
  !!  Convert integer to logical
  !!    0 -> true
  !!    1 -> false

  implicit none

  integer, intent(in)  :: i

  logical :: l

  l = .true.

  if(i .eq. 1) return

  l = .false.

end function int2log

! ===================================================================================================!
end module types
! ===================================================================================================!
