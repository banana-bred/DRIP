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
  public :: target_projections
  public :: point_group

  ! -- procedures
  public :: read_globals

  integer(ip) :: nspins
    !! The number of spin multiplicities of the neutral system
  integer(ip) :: natoms
    !! number of atoms in the target molecule

  integer(ip), allocatable :: spins(:)
    !! Array of spin multiplicities (2S+1) for the neutral system. The code is mosly spin-agnostic.
    !! The final calculated cross sections will be averaged over spin multiplicities.
  integer(ip), allocatable :: nelec(:)
    !! Array holding the number of electronic states for a given spin multiplicity
  integer(ip), allocatable :: target_projections(:)
    !! Array containing the projections of the angular momentum of all electrons in the target (Λ) for  a given electronic state.
    !! The length of this array should be equal to the number of electronic ! states in the Abelian subgroup of the UKRmol+ calculations,
    !! including degeneracy (states with a ±|Λ| that isn't 0). For example, a molecule with the electronic structure
    !! (increasing energy towards the right) [[ X1Σ-, a1Δ, b1Σ+, A3Π ]] could have `target_projections = 0, -2, 2, 0, 1, -1`.
    !! Because the Δ state has Λ = ±2, the Π state Λ = ±1, and the sigma states only have one Λ = 0, by definition. For each degenerate
    !! state, the order shouldn't matter, i.e. `target_projections = 0, -2, 2, 0, 1, -1` should be the same as `target_projections = 0,  2,-2, 0, 1, -1`,
    !! but `target_projections = 0, -2, 2, 1, -1, 0` would give a different result.
    !! NOTE: Electronic states can cross. The value provided here should be the value for the FIRST SUPPLIED GEOMETRY.
    !! State crossings will be handled automatically

  character(:), allocatable :: point_group
  !! The point group in which the UKRmol+ calculations were run


  namelist / globals_namelist /                     &
    !! Global variables that relate to the target or total system
                                natoms,             &
                                target_projections, &
                                point_group,        &
                                spins

  ! =================================================================================================== !
  contains
  ! =================================================================================================== !

  ! ---------------------------------------------------------------------------------------------------!
  subroutine read_globals
    !! Reads the globals namelist

    use types,      only: big_char
    use system,     only: stdin, stdout, progname, die
    use arrays,     only: remove_value
    use symmetry,   only: available_point_groups
    use constants,  only: initial_int
    use characters, only: int2char0, to_upper

    implicit none

    character(big_char), parameter :: temp = ""
    character(:), allocatable :: error_message

    allocate(spins(100))              ; spins              = initial_int
    allocate(target_projections(100)) ; target_projections = initial_int
    point_group             = temp

    read(stdin, globals_namelist)
    rewind(stdin)

    point_group = trim(point_group)

    call to_upper(point_group)

    ! -- resize arrays to contain only the values included in the namelist
    call remove_value(spins, initial_int)
    call remove_value(target_projections, initial_int)

    nspins = size(spins, 1)
    target_projections = size(target_projections, 1)

    ! -- write namelist variables to stdout
    write(stdout, globals_namelist)
    write(stdout, *)

    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!
    ! -- make sure the input values make sense -- !
    !vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv!

    ! -- check that all given spins are > 0
    if(any(spins .lt. 1)) then

      allocate(character(big_char) :: error_message)

      write(error_message, '(' // int2char0(nspins) // '(I0, X))') spins
      error_message = trim(error_message)

      call die("Non-positive spin multiplicities detected. Please correct.", "Spin multiplicities given : " // error_message)

    endif

    select case(natoms)
      case(initial_int) ; call die("Must specify the number of atoms")
      case(0:1, 4:)     ; call die(progname // " can only handle 2 or 3 atoms")
    end select

    if(any(point_group .eq. available_point_groups)) then
      continue
    elseif(point_group .eq. trim(temp)) then
      call die("The variable point_group must be specified")
    else
      call die("The value of point_group (" // point_group // ") is unexpected.")
    endif


  end subroutine read_globals

! =================================================================================================== !
end module globals
! =================================================================================================== !
