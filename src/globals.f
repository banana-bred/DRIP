! =================================================================================================== !
module globals
  !! Global variables that will be used throughout program execution.

use types, only: ip

implicit none

private

save

! -- variables
public :: natoms
public :: nelec
public :: nspins
public :: spins

! -- procedures
public :: read_globals
public :: spin_name

integer(ip) :: nspins
  !! The number of spin multiplicities of the neutral system
integer(ip) :: natoms
  !! number of atoms in the target molecule

integer(ip), allocatable :: spins(:)
    !! Array of spin multiplicities (2S+1) for the neutral system. The code is mosly spin-agnostic.
    !! The final calculated cross sections will be averaged over spin multiplicities.
integer(ip), allocatable :: nelec(:)
  !! Array holding the number of electronic states for a given spin multiplicity

namelist / globals_namelist /         &
  !! Global variables that affect most of the code
                              natoms, &
                              spins

! =================================================================================================== !
contains
! =================================================================================================== !

! ---------------------------------------------------------------------------------------------------!
subroutine read_globals
  !! Reads the globals namelist

  use types,      only: cs => big_char
  use system,     only: stdin, stdout, progname, die
  use arrays,     only: remove_value
  use constants,  only: initial_int
  use characters, only: int2char0

  implicit none

  character(:), allocatable :: error_message

  allocate(spins(100)) ; spins = initial_int

  read(stdin, globals_namelist)
  rewind(stdin)
  write(stdout, globals_namelist)
  write(stdout, *)

  ! -- resize the spins array to contain only the spins included in the namelist
  call remove_value(spins, initial_int)
  nspins = size(spins, 1)

  ! -- check that all given spins are > 0
  if(any(spins .lt. 1)) then

    allocate(character(cs) :: error_message)

    write(error_message, '(' // int2char0(nspins) // '(I0, X))') spins
    error_message = trim(error_message)

    call die("Non-positive spin multiplicities detected. Please correct.", "Spin multiplicities given : " // error_message)

  endif

  select case(natoms)
    case(initial_int) ; call die("Must specify the number of atoms")
    case(0:1, 4:)     ; call die(progname // " can only handle 2 or 3 atoms")
  end select

end subroutine read_globals

! ---------------------------------------------------------------------------------------------------!
pure function spin_name(spin_multiplicity) result(output)

  implicit none

  integer(ip), intent(in) :: spin_multiplicity
  character(:), allocatable :: output

  select case(spin_multiplicity)
  case(:0) ; output = "UNDEFINED_NONPOSITIVE"
  case(1)  ; output = "singlet"
  case(2)  ; output = "doublet"
  case(3)  ; output = "triplet"
  case(4)  ; output = "quartet"
  case(5)  ; output = "quintet"
  case(6:) ; output = "UNDEFINED_POSITIVE"
  end select

end function spin_name

! =================================================================================================== !
end module globals
! =================================================================================================== !
