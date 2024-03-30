! =================================================================================================== !
module control
  !! Global variables that will be used throughout program execution.

  use types,     only: ip, rp
  use constants, only: initial_int

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
  public :: evaluation_energy_indices
  public :: evaluation_energies

  ! -- procedures
  public :: read_control

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
  integer(ip) :: num_evaluation_energies = initial_int
    !! The number of evaluation energies for reading K-matrices. For each spin multiplicity in an energy independent calculation,
    !! DRIP will run this many times (once for each energy).

  integer(ip), allocatable :: evaluation_energy_indices(:)
    !! Array containing the evaluation energy indicies at which the K-matrices will be evaluated

  real(rp), allocatable :: evaluation_energies(:)
    !! Array containing the evaluation energies nearest which the K-matrices will be evaluated

  character(:), allocatable :: evaluation_energy_units
    !! The units of the input evaluation energies, if supplied. Options:
    !!  H(ARTREE), R(YDBERG), EV, INVCM, K(ELVIN)

  character(:), allocatable :: molecule
    !! The name of the molecule

  character(:), allocatable :: input_type
    !! The expected input type. Can take on one of two values :
    !!  "ukrmol"
    !!  "david"

  character(11) :: frmt_xy = "(2e30.20e3)"
    !! default write format for outputting two real numbers  30 characters wide, 20 characters after the period (.), and 3 digits in the exponent

  namelist / control_namelist /                            &
    !! Controls the overall behavior and flow of the program.
                                calculation_type,          &
                                energy_dependent,          &
                                molecule,                  &
                                print_K,                   &
                                print_S,                   &
                                input_type,                &
                                evaluation_energies,       &
                                evaluation_energy_indices, &
                                evaluation_energy_units,   &
                                verbosity

! =================================================================================================== !
contains
! =================================================================================================== !

! --------------------------------------------------------------------------------------------------- !
  subroutine read_control
    !! Reads the control namelist

    use types,           only: big_char
    use system,          only: stdin, stdout, iostat_ok, die
    use arrays,          only: remove_value, bubble_sort
    use constants,       only: zero, au2ev, au2ryd, au2k, au2invcm
    use characters,      only: to_upper
    use iso_fortran_env, only: iostat_end, iostat_eor

    implicit none

    integer(ip) :: io

    character(big_char), parameter :: temp = ""

    ! -- initialze allocatable variables for reading
    molecule                = temp
    input_type              = temp
    evaluation_energy_units = temp
    allocate(evaluation_energies(1000))       ; evaluation_energies       = zero
    allocate(evaluation_energy_indices(1000)) ; evaluation_energy_indices = 0

    read(stdin, control_namelist, iostat = io)

    if(io .eq. iostat_end) call die("The variable control_namelist was not found during stdin read")
    if(io .ne. iostat_ok)  call die("Problem reading namelist inputs from stdin")

    rewind(stdin)

    ! -- trim space off characters
    molecule                = trim(molecule)
    input_type              = trim(input_type)
    evaluation_energy_units = trim(evaluation_energy_units)

    ! -- normalize the case
    call to_upper(calculation_type)
    call to_upper(input_type)
    call to_upper(evaluation_energy_units)

    write(stdout, control_namelist)
    write(stdout, *)

    ! -- trim input arrays of default values (deallocate if no non-default values are supplied)
    call remove_value(evaluation_energies,       zero)
    call remove_value(evaluation_energy_indices, 0)

    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
    ! -- make sure the input values make sense -- !
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!

    select case(calculation_type)
      case("DR", "EX", "PI") ; continue
      case("XX")             ; call die ("Must give calculation_type a value.")
      case default           ; call die ("The specified value of calculation_type (" // calculation_type // ") is not valid.")
    end select

    select case(input_type)
      case("UKRMOL+")
        if(energy_dependent) call die("Cannot use the energy-dependent approach with the input_type '" // input_type // "'")
        continue
      case("DAVID") ; continue
      case(trim(temp))        ; call die("The variable input_type must be specified")
      case default            ; call die("The value of input_type (" // input_type // ") is unexpected.")
    end select

    select case(molecule)
      case(trim(temp)) ; call die("The name of the molecule must be specified")
      case default     ; continue
    end select

    if(allocated(evaluation_energies) .AND. allocated(evaluation_energy_indices)) then
      call die("Evaluation energies will not be determined by energy value AND index. Specify only one")
    endif

    if(allocated(evaluation_energies) .OR. allocated(evaluation_energy_indices)) then

      if(energy_dependent) call die("Cannot specify a number of evaluation energies AND an energy-dependent approach.")

      ! -- determine the number of evalauation energies to loop over
      if(allocated(evaluation_energies)) then

        ! -- need to know the input energy units
        select case(evaluation_energy_units)
          case("H", "HARTREE", "EV", "RYD", "RYDBERG", "INVCM", "K", "KELVIN") ; continue
          case(trim(temp))
            call die("The evaluation energy units must be specified. Choice of: H(ARTREE), EV, RYD(BERG), INVCM, K(ELVIN)")
          case default
            call die("Unkonw energy '" // evaluation_energy_units // &
              "' supplied. Please use one of H(ARTREE), EV, RYD(BERG), INVCM, K(ELVIN)")
        end select

        num_evaluation_energies = size(evaluation_energies, 1)

        ! -- convert supplied evaluation energies to atomic units in the code
        select case(evaluation_energy_units)
          case("H", "HARTREE")
            continue
          case("EV")
            evaluation_energies = evaluation_energies / au2ev
          case("RYD", "RYDBERG")
            evaluation_energies = evaluation_energies / au2ryd
          case("INVCM")
            evaluation_energies = evaluation_energies / au2invcm
          case("K", "KELVIN")
            evaluation_energies = evaluation_energies / au2k
        end select

        evaluation_energies = bubble_sort(evaluation_energies)

      endif

      if(allocated(evaluation_energy_indices)) then
        num_evaluation_energies = size(evaluation_energy_indices, 1)
        evaluation_energy_indices = bubble_sort(evaluation_energy_indices)
      endif

    endif

  end subroutine read_control

! =================================================================================================== !
end module control
! =================================================================================================== !
