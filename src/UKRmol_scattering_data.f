! ================================================================================================================================ !
module UKRmol_scattering_data
  !! Contains procedures used to read electron scattering data from the UKRmol+ output, such as K-matrices, target electronic
  !! state energies, channel data (quantum numbers, indices, and energies), etc.

  use types,           only: ip, rp
  use system,          only: die, stdout, iostat_ok
  use iso_fortran_env, only: iostat_end

  implicit none

  private

  public get_K_matrix_and_electronic_channels

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine get_K_matrix_and_electronic_channels(spin, inrg)
    !! This subroutine reads the desired K-matrix and electronic channels from the UKRmol+ output nearest to the specified energy

    use control,     only: evaluation_energy_indices, evaluation_energies
    use characters,  only: char => int2char0
    use directories, only: ds => directory_separator, input_directory

    implicit none

    integer, intent(in) :: spin
      !! The spin multiplicity of the target + electron
    integer, intent(in) :: inrg
      !! The index of the energy to read. This is the index of EVALUATION_ENERGY_INDICES or EVALUATION_ENERGIES.
      !! For example, if EVALUATION_ENERGY_INDICES = [1, 3, 10], then EVALUATION_ENERGY_INDICES(inrg=3) is 10.
      !! This is to say the inrg is not the index inf EVALUATION_ENERGY_INDICES, but is used to index this array
      !! of indices.

    integer, parameter :: nqchem = 3
      !! The number of possible quantum chemistry codes to use with UKRmol+

    integer :: nqchem_detected
      !! The number of quantum chemistry output files detected

    logical :: qchem_exists(nqchem)
      !! Array of logicals keeping track of which quantum chemistry codes are present

    logical :: exists

    integer(ip) :: i

    character(6), parameter :: quantum_chemistry_names(nqchem) = [ "molcas", "molpro", "psi4  " ]
      !! The quantum chemistry codes that can be used with UKRmol+

    character(:), allocatable :: qchem_filename
      !! The name of the quantum chemistry output file
    character(:), allocatable :: qchem_name_detected
      !! The name of the detected quantum chemistry software
    character(:), allocatable :: filename

    ! -- check the first geometry to figure out the symmetry and channels

    ! -- read the output of the quantum chemistry software used to calculate the target orbitals
    do i = 1, size(quantum_chemistry_names, 1)
      qchem_name_detected = trim(quantum_chemistry_names(i))
      qchem_filename = input_directory // ds // "geom1" // ds // "outputs" // ds // "target." // qchem_name_detected // ".out"
      inquire(file = qchem_filename, exist = exists)
      qchem_exists(i) = exists
      if(.not. exists) cycle
      exit
    enddo

    ! -- check that exactly one quantum chemistry output is present
    nqchem_detected= count(qchem_exists .eqv. .true.)
    if( nqchem_detected .ne. 1 ) then
      call die("There were " // char(nqchem_detected) // " quantum chemistry output files detected, but there should be exactly 1.")
    endif

    ! -- determine the point group
    write(stdout, '("Found traces of quantum chemistry software : ", A)') qchem_name_detected
    call determine_point_group(qchem_filename, qchem_name_detected)

    ! -- get the total channels now from denprop.out and the electronic state projections from the same place i guess. Read only the
    filename = input_directory // ds // "geom1" // ds // "outputs" // ds // "target.denprop.out"
    call read_channels(filename)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! careful. Denprop is from UKRmol but david's code  changes this around but not denprop. Read the first denprop anywya and tehn
    ! have the code figure out if the actual first geometry channels are in th eright order and what to do if not
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    ! -- loop over the necessary irreps given the point group and read K-matrices

      ! -- determine point group and available indices
        ! get point group, get size of point group
        ! get electroic states for first geometry
          ! determine degeneracies
        ! loop over irreps
          ! get channels for first geometry
          ! ! We might have different channels for a different energy (so re-read channels for each energy IF energy independent)
            ! otherwise, only read channels once
          ! do the rest of the geometries
          !geom loop
            ! irrep loop

        ! read denprop file to determine electronic state order for first file?

      ! -- read channels from their respective channel files
      !    READ FIRST GEOMETRY ONLY (for now)
      !    MAKE SURE THAT WE CAN READ CHANNELS DESPITE CROSSING ELECTRONIC STATES.
      !    MAYBE assign them a "true" index.

      ! -- determine degenerate states


      ! OPTIONALLY SPECIFY A GEOM_START (if not specified, use 1) geom1, geom2, geom3, ..
      ! OPTIONALLY SPECIFY A GEOM_END (if not specified, use 1) ... geomN
      ! geom_loop: do igeom = 1, ngeom

      ! enddo geom_loop

      ! -- read K-matrices from their respective file


  end subroutine get_K_matrix_and_electronic_channels

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine determine_point_group(qchem_filename, qchem_name)
    !! Determine the point group of the calculations by reading the output of the quantum chemistry software (which contains
    !! the repeated input of the quantum chemistry software).
    use symmetry,   only: point_group, determine_molpro_point_group
    use characters, only: upper

    implicit none

    character(*), intent(in) :: qchem_name
    character(*), intent(in) :: qchem_filename

    logical :: exists

    integer :: funit
    integer :: io
    integer :: i

    character(20) :: line

    inquire(file = qchem_filename, exist = exists)
    if(.not. exists) call die("The quantum chemistry file " // qchem_filename // " does not exist.")

    open(newunit = funit, file = qchem_filename)

    lines: do

      read(funit, '(A)', iostat = io) line

      if(io .eq. iostat_end) exit lines
      if(io .ne. iostat_ok) call die("Problem reading data from file" // qchem_filename)

      select case(qchem_name)
      case("molcas")
        i = index(line, "group=")
        if(i .eq. 0) cycle lines
        point_group = upper(trim(line(i + len("group=") + 1: len(line))))
        return

      case("molpro")

        i = index(line, "SYMMETRY")
        if(i .eq. 0) cycle lines
        point_group = upper(determine_molpro_point_group(line))
        return

      case("psi4")
        i = index(line, "symmetry")
        if(i .eq. 0) cycle lines
        point_group = upper(trim(line(i + len("symmetry") + 1: len(line))))
        return

      end select

    enddo lines

    call die("Could not determine point group from quantum chemistry output")

  end subroutine determine_point_group

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine read_channels(denprop_filename)
    !! Read the first geometry's denprop.out to determine the number of electronic channels

    use globals,   only: reduced_mass, natoms, targ, targ_ndegen
    use constants, only: au2amu
    use utilities, only: read_blank

    implicit none

    character(*), intent(in) :: denprop_filename

    logical :: exists

    integer(ip) :: i
    integer(ip) :: n
    integer(ip) :: io
    integer(ip) :: irrep
    integer(ip) :: funit
    integer(ip) :: targ_spin
    integer(ip) :: natoms_read
    integer(ip) :: ntarg

    integer(ip) :: ijunk(10)

    real(rp) :: rjunk(10)

    character(1) :: cjunk(10)

    real(rp) :: m1, m2
      !! The masses of the target molecule (atomic mass units on read)

    character(33) :: line
    character(33), parameter :: target_character = " Output to unit 24 is as follows:"

    inquire(file = denprop_filename, exist = exists)
    if(.not. exists) call die("The DENPROP output file file " // denprop_filename // " does not exist.")

    open(newunit = funit, file = denprop_filename)

    lines: do

      read(funit, '(A)', iostat = io) line

      if(io .eq. iostat_end) exit lines
      if(io .ne. iostat_ok) call die("Problem reading data from file" // denprop_filename)

      if(line .eq. target_character) exit lines

    enddo lines

    call read_blank(funit)

    read(funit, *) ijunk(1:3), natoms_read, ntarg, ijunk(1:2), rjunk(1:3)

    if(natoms_read .ne. natoms) call die("The number of atoms in " // denprop_filename // " does not match the namelist input")

    allocate(targ(ntarg))

    ! -- determine the reduced mass of the molecule while we're here
    read(funit, *) ijunk(1:2), cjunk(1), ijunk(1), m1, rjunk(1:3)
    read(funit, *) ijunk(1:2), cjunk(1), ijunk(1), m2, rjunk(1:3)
    reduced_mass = m1 * m2 / (m1 + m2) / au2amu

    ! -- read info about the target states
    do i = 1, ntarg

      read(funit, *) ijunk(1), n, ijunk(1:2), irrep, targ_spin, ijunk(1:2), rjunk(1)

      targ(i) % n     = i
      targ(i) % irrep = irrep + 1

      if(allocated(targ_ndegen)) then
        targ(i) % ndegen = targ_ndegen(i)
      else
        targ(i) % ndegen = n
      endif

    enddo

    do i = 1, ntarg
      block
        use symmetry, only: irrep_name, point_group
        print*, n, irrep, targ_spin, irrep_name(targ(i) % irrep, point_group)
        call die("Reconcile the UKRmol order and my irrep order. Make a converter in symmetry module")
      end block
    enddo

    stop "test"

  end subroutine read_channels

! ================================================================================================================================ !
end module UKRmol_scattering_data
! ================================================================================================================================ !
