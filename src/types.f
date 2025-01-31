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
  public :: targ_type
  public :: elec_chanl_type

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

  type targ_type
    !! The state of the target molecule
    integer :: n
      !! The index of the target state (ground = 1)
    integer :: ndegen
      !! The index of the target state accounting for degeneracies (ground = 1). In the case where there are the target
      !! states with n = 1, 2, 3, 4, 5 but the states 2/3 and 4/5 are mutually degenerate (e.g., a Π or Δ state), then
      !! ndegend will be 1, 2, 2, 3, 3.
    integer :: irrep
      !! The irrep of the target state
    integer :: M
      !! The projection of the target electronic state's angular momentum on the molecular axis (ℏ = 1)
  end type targ_type

  type elec_chanl_type
    !! The electronic channel of the system (target state + incident electron)
    integer :: idx
      !! The channel index. Useful for when electronic states (and therefore electronic channels) swap order
    type(targ_type) :: targ
    integer :: l
      !! The orbital angular momentum quantum number of the incident electron
    integer :: lambda
      !! The projection of l on the molecular axis
    integer :: q
      !! Determines the normalization used for the f and g coulomb functions.
      !!  q = 0 : alternative normalization [sqrt(B) from Seaton 2002, Comp Phys Comm 146 (2002) 225-249]
      !!  q = 4 : standard normalization. This is always the case for UKRmol.
  end type elec_chanl_type



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
