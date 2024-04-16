! ================================================================================================================================ !
module globals
  !! Global variables that will be used throughout program execution.

  use types, only: ip, rp

  implicit none

  private

  save

  ! -- types
  public :: targ_type
  public :: electronic_channel_type

  ! -- variables
  public :: natoms
  public :: nelec
  public :: nspins
  public :: spins
  public :: targ_proj
  public :: targ_ndegen
  public :: point_group
  public :: reduced_mass
  public :: targ
  public :: electronic_channels
  public :: ntarg
  public :: geometries
  public :: ngeom
  public :: K_R

  ! -- procedures
  public :: read_globals
  public :: swap_electronic_channel_values

  integer(ip) :: nspins
    !! The number of spin multiplicities of the neutral system
  integer(ip) :: natoms
    !! number of atoms in the target molecule
  integer(ip) :: ntarg
    !! The number of target electronic states in the calculation
  integer(ip) :: ngeom
    !! The number of geometries that were read from the UKRmol directory

  integer(ip), allocatable :: spins(:)
    !! Array of spin multiplicities (2S+1) for the neutral system. The code is mosly spin-agnostic.
    !! The final calculated cross sections will be averaged over spin multiplicities.
  integer(ip), allocatable :: nelec(:)
    !! Array holding the number of electronic states for a given spin multiplicity
  integer(ip), allocatable :: targ_ndegen(:)
    !! Array of target states taking into account their degeneracy.
    !! The array 1, 2, 3, 4, 5 would be 5 non-degenerate states. If there
    !! are no degenerate target states in the calculation, this variable can be omitted.
    !! A calculation with doubly degenerate first and second excited state would
    !! have this variable contain the values 1, 2, 2, 3, 3. The ground state is 1,
    !! the first excited state is 2, 2 and the second excited state is 3, 3. The order
    !! given here must apply to the first geometry of the run.
  integer(ip), allocatable :: targ_proj(:)
    !! Array containing the projections of the angular momentum of all electrons in the target (Λ) for  a given electronic state.
    !! The length of this array should be equal to the number of electronic ! states in the Abelian subgroup of the UKRmol+ calculations,
    !! including degeneracy (states with a ±|Λ| that isn't 0). For example, a molecule with the electronic structure
    !! (increasing energy towards the right) [[ X1Σ-, a1Δ, b1Σ+, A3Π ]] could have `targ_proj = 0, -2, 2, 0, 1, -1`.
    !! Because the Δ state has Λ = ±2, the Π state Λ = ±1, and the sigma states only have one Λ = 0, by definition. For each degenerate
    !! state, the order shouldn't matter, i.e. `targ_proj = 0, -2, 2, 0, 1, -1` should be the same as `targ_proj = 0,  2,-2, 0, 1, -1`,
    !! but `targ_proj = 0, -2, 2, 1, -1, 0` would give a different result.
    !! NOTE: Electronic states can cross. The value provided here should be the value for the FIRST SUPPLIED GEOMETRY.
    !! State crossings will be handled automatically

  real(rp) :: reduced_mass
    !! The reduced mass of the molecule (used in solving the vibrational Hamiltonian)

  real(rp), allocatable :: geometries(:)
    !! Array of internuclear distances at which K-matrices were computed

  real(rp), allocatable :: K_R(:,:,:,:)
    !! The K-matrices as a function of internuclear distance. Indexed as (i, j, R, E)
    !! i, and j are the row and column, R is the internuclear distance, and E is the evaluation energy

  character(:), allocatable :: point_group
  !! The point group in which the UKRmol+ calculations were run

  type targ_type
    !! Represents the state of the target molecule
    integer(ip) :: n
      !! The index of the target state (ground = 1)
    integer(ip) :: ndegen
      !! The index of the target state accounting for degeneracies (ground = 1). In the case where there are the target
      !! states with n = 1, 2, 3, 4, 5 but the states 2/3 and 4/5 are mutually degenerate (e.g., a Π or Δ state), then
      !! ndegend will be 1, 2, 2, 3, 3.
    integer(ip) :: irrep
      !! The irrep of the target state
    integer(ip) :: M
      !! The projection of the target electronic state's angular momentum on the molecular axis (ℏ = 1)
  end type targ_type

  type electronic_channel_type
    !! Represents the state of the target molecule
    integer(ip) :: idx
      !! The channel index. Useful for when electronic states (and therefore electronic channels) swap order
    integer(ip) :: n
      !! The index of the target state (ground = 1)
    integer(ip) :: ndegen
      !! The index of the target state accounting for degeneracies (ground = 1). In the case where there are the target
      !! states with n = 1, 2, 3, 4, 5 but the states 2/3 and 4/5 are mutually degenerate (e.g., a Π or Δ state), then
      !! ndegend will be 1, 2, 2, 3, 3.
    integer(ip) :: irrep
      !! The irrep of the target state
    integer(ip) :: M
      !! The projection of the target electronic state's angular momentum on the molecular axis (ℏ = 1)
    integer(ip) :: l
      !! The orbital angular momentum quantum number of the incident electron
    integer(ip) :: lambda
      !! The projection of l on the molecular axis
    integer(ip) :: q
      !! Determines the normalization used for the f and g coulomb functions.
      !!  q = 0 : alternative normalization [sqrt(B) from Seaton 2002, Comp Phys Comm 146 (2002) 225-249]
      !!  q = 4 : standard normalization. This is always the case for UKRmol.
  end type electronic_channel_type

  type(targ_type),               allocatable :: targ(:)
  type(electronic_channel_type), allocatable :: electronic_channels(:)

  namelist / globals_namelist /              &
    !! Global variables that relate to the target or total system
                                natoms,      &
                                targ_proj,   &
                                point_group, &
                                targ_ndegen, &
                                spins

! ================================================================================================================================ !
  contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
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

    allocate(spins(100))       ; spins       = initial_int
    allocate(targ_proj(100))   ; targ_proj   = initial_int
    allocate(targ_ndegen(100)) ; targ_ndegen = initial_int
    point_group = temp

    read(stdin, globals_namelist)
    rewind(stdin)

    point_group = trim(point_group)

    call to_upper(point_group)

    ! -- resize arrays to contain only the values included in the namelist
    call remove_value(spins,       initial_int)
    call remove_value(targ_proj,   initial_int)
    call remove_value(targ_ndegen, initial_int)

    nspins = size(spins, 1)

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

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure subroutine swap_electronic_channel_values(channel1, channel2)
    !! Swap the values of two electronic channels. Everything BUT their index should change. After using this routine, the indices
    !! should be added/updated, anyway.
    implicit none
    type(electronic_channel_type), intent(inout) :: channel1
    type(electronic_channel_type), intent(inout) :: channel2
    type(electronic_channel_type) :: tmp
    tmp = electronic_channel_type( &
       idx    = 0,                 &
       n      = channel1 % n,      &
       ndegen = channel1 % ndegen, &
       irrep  = channel1 % irrep,  &
       M      = channel1 % M,      &
       l      = channel1 % l,      &
       lambda = channel1 % lambda, &
       q      = channel1 % q       &
    )
    channel1 = electronic_channel_type( &
       idx    = 0,                      &
       n      = channel2 % n,           &
       ndegen = channel2 % ndegen,      &
       irrep  = channel2 % irrep,       &
       M      = channel2 % M,           &
       l      = channel2 % l,           &
       lambda = channel2 % lambda,      &
       q      = channel1 % q            &
    )
    channel2 = electronic_channel_type( &
       idx    = 0,                      &
       n      = tmp % n,                &
       ndegen = tmp % ndegen,           &
       irrep  = tmp % irrep,            &
       M      = tmp % M,                &
       l      = tmp % l,                &
       lambda = tmp % lambda,           &
       q      = channel1 % q            &
    )
  end subroutine swap_electronic_channel_values

! ================================================================================================================================ !
end module globals
! ================================================================================================================================ !
