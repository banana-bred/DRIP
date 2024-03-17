! =================================================================================================== !
module control
  !! Global variables that will be used throughout program execution.

  use types, only: ip

  implicit none

  private

  save

  ! -- variables
  public :: calculation_type
  public :: verbosity
  public :: print_K
  public :: print_S
  public :: molecule
  public :: energy_dependent
  public :: num_evaluation_energies

  ! -- procedures
  public :: read_control

  ! character(2) :: calculation_type
  character(2) :: calculation_type = "XX"
    !! The calculation type for this run. Possible values are
    !!   "DR" : dissociative recombination
    !!   "EX" : electron-impact excitation
    !!   "PI" : photoionization
  logical :: print_K = .false.
    !! Print the K-matrices as a function of internuclear distance ?
  logical :: print_S = .false.
    !! Print the S-matrices as a function of internuclear distance ?
  logical :: energy_dependent = .false.
    !! Will the code use an energy-dependent S-matrix approach ?

  integer(ip) :: verbosity = 0
    !! The verbosity of the program. Higher values determine how much info to print
    !!  0 : standard
    !!  1 : verbose
    !!  2 : very verbose

  integer(ip) :: num_evaluation_energies
    !! The number of evaluation energies for reading K-matrices. For each spin multiplicity in an energy independent calculation,
    !! DRIP will run this many times (once for each energy).

  character(:), allocatable :: molecule
    !! The name of the molecule

  character(:), allocatable :: input_type
    !! The expected input type. Can take on one of two values :
    !!  "ukrmol"
    !!  "david"

  character(11) :: frmt_xy = "(2e30.20e3)"
    !! default write format for outputting two real numbers  30 characters wide, 20 characters after the period (.), and 3 digits in the exponent

  namelist / control_namelist /                          &
    !! Controls the overall behavior and flow of the program.
                                calculation_type,        &
                                energy_dependent,        &
                                molecule,                &
                                print_K,                 &
                                print_S,                 &
                                input_type,              &
                                num_evaluation_energies, &
                                verbosity

! =================================================================================================== !
contains
! =================================================================================================== !

! --------------------------------------------------------------------------------------------------- !
  subroutine read_control
    !! Reads the control namelist

    use system,          only: stdin, stdout, iostat_ok, die
    use types,           only: big_char
    use characters,      only: to_upper
    use iso_fortran_env, only: iostat_end, iostat_eor

    implicit none

    integer(ip) :: io

    character(big_char), parameter :: temp = ""

    ! -- initialze allocatable characters for reading
    molecule   = temp
    input_type = temp

    read(stdin, control_namelist, iostat = io)

    if(io .eq. iostat_end) call die("The variable control_namelist was not found during stdin read")
    if(io .ne. iostat_ok)  call die("Problem reading namelist inputs from stdin")

    rewind(stdin)

    ! -- trim space off characters
    input_type = trim(input_type)
    molecule = trim(molecule)

    ! -- normalize the case
    call to_upper(calculation_type)
    call to_upper(input_type)

    write(stdout, control_namelist)
    write(stdout, *)

    ! -- only run the energy loop in the main program once if it's an energy-dependent run
    if(energy_dependent) num_evaluation_energies = 1

    ! -- make sure the input values make sense
    select case(calculation_type)
      case("DR", "EX", "PI") ; continue
      case("XX")             ; call die ("Must give calculation_type a value.")
      case default           ; call die ("The specified value of calculation_type (" // calculation_type // ") is not valid.")
    end select

    select case(input_type)
      case("UKRMOL", "DAVID") ; continue
      case(trim(temp))        ; call die("The variable input_type must be specified")
      case default            ; call die("The value of input_type (" // input_type // ") is unexpected.")
    end select

    select case(molecule)
      case("trim(temp)") ; call die("The name of the molecule must be specified")
      case default       ; continue
    end select

  end subroutine read_control

! =================================================================================================== !
end module control
! =================================================================================================== !
