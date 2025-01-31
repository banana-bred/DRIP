! ================================================================================================================================ !
module UKRmol_scattering_data
  !! Contains procedures used to read electron scattering data from the UKRmol+ output, such as K-matrices, target electronic
  !! state energies, channel data (quantum numbers, indices, and energies), etc.

  implicit none

  private

  ! public read_Rmat
  public energy_dependent_Kmat_read

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  module subroutine energy_dependent_Kmat_read(spin, geoms, Kmat, elec_chanls, elec_chanls_energies)
    !! This subroutine reads the desired K-matrix and electronic channels from the UKRmol+ output nearest to the specified energy

    use types,           only: rp, elec_chanl_type
    use system,          only: stdout, die
    use symmetry,        only: point_group, irrep_name, spin_name, group_size
    use control,         only: geom_start, geom_end, skip_geom, input_type
    use constants,       only: initial_int
    use characters,      only: i2char => int2char0
    ! use directories,     only: ds => directory_separator, input_directory
    use iso_fortran_env, only: iostat_end

    implicit none

    integer, intent(in) :: spin
      !! The spin multiplicity of the target + electron
    real(rp), intent(out) :: geoms(:)
      !! The geometries to consider
    type(elec_chanl_type), intent(out) :: elec_chanls(:)
      !! Array containing information about the electronic channels
    real(rp), intent(out) :: elec_chanls_energies(:)
      !! Array containing the energies of the electronic channels
    real(rp), intent(out), allocatable :: Kmat(:,:,:,:)
      !! The K-matrices as a function of internuclear distance. Indexed as (i, j, E, R)
      !! i, and j are the row and column, E is the evaluation energy, and R is the internuclear distance

    character(:), allocatable :: qchem_code
      !! The quantum chemistry code that we used

    integer :: i
    integer :: igeom
    integer :: itarg
    integer :: irrep
    integer, allocatable :: indices(:)

    ! character(:), allocatable :: qchem_filename
    !   !! The name of the quantum chemistry output file
    character(:), allocatable :: qchem_filename

    call determine_qchem_code(qchem_code, qchem_filename)

    write(stdout, '("Found traces of quantum chemistry software : ", A)') qchem_code

    call determine_point_group(qchem_filename, qchem_code)

    stop "test the output of this procedure ^^^^^^^^^"

    !! -- get the target state info now from denprop.out
    !filename = input_directory // ds // "geom1" // ds // "outputs" // ds // "target.denprop.out"
    !call read_target(filename)

    !! -- loop over all irreps, build all channels based on first geom
    !do irrep = 1, group_size(point_group)

    !  filename = input_directory // ds // "collected_scattering_data" // ds // "channels" // ds // "channels.geom1." &
    !             // spin_name(spin) // "." // irrep_name(irrep, point_group)

    !  call add_channels_from_file(filename)

    !enddo

    !call sort_electronic_channels(electronic_channels)

    !! -- print some info to stdout
    !ntarg = size(targ, 1)
    !write(stdout, '("The detected target state indices are : ", ' // i2char(ntarg) // '(I0, X))') &
    !  [(targ(itarg) % n, itarg = 1, ntarg)]
    !write(stdout, '("With degeneracies :                     ", ' // i2char(ntarg) // '(I0, X))') &
    !  [(targ(itarg) % ndegen, itarg = 1, ntarg)]
    !write(stdout, '("The maximum detected value of l in this calculation is ", I0)') maxval(electronic_channels % l)

    !! -- check if degenerate states are equal in energy
    !do itarg = 1, ntarg - 1
    !  if(targ(itarg) % ndegen .ne. targ(itarg + 1) % ndegen) cycle
    !  if(targ(itarg) % nrg .ne. targ(itarg) % nrg) then
    !    call die("The degenerate target states " // i2char(itarg) // " and " // i2char(itarg + 1) // " have different energies.")
    !  endif
    !enddo

    !! -- read the available geometries
    !call read_geometries(geometries)
    !ngeom = size(geometries, 1)
    !if(geom_end .eq. initial_int) geom_end = ngeom

    !! -- filter the geometries based on initial, final, and skipped geometries.
    !do igeom = 1, ngeom
    !  if(igeom .lt. geom_start) cycle
    !  if(igeom .gt. geom_end) exit
    !  if(any(igeom .eq. skip_geom)) then
    !    write(stdout, '("Skipping geometry ", I0, " per user request")') igeom
    !    cycle
    !  endif
    !  if(igeom .ne. ngeom) then
    !    ! -- skip duplicate geometries
    !    if(geometries(igeom) .eq. geometries(igeom + 1)) then
    !      write(stdout, '("Skipping geometry ", I0, " because is the same as geometry ", I0)') igeom, igeom + 1
    !      cycle
    !    endif
    !  endif
    !  call append(indices, igeom)
    !enddo

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    !! -- read the other geometries' channels and K-matrices -- !
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    !! -- for each included geometry, read the K-matrices
    !do igeom = 1, ngeom
    !  select case(input_type)
    !    case("UKRMOL") ; call read_ukrmol_kmats(spin, igeom)
    !    case("DAVID")  ; call die("D*vid")
    !    case default   ; call die("Improper input type " // input_type)
    !  end select
    !enddo

  end subroutine energy_dependent_Kmat_read

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine determine_point_group(qchem_filename, qchem_code)
    !! Determine the point group of the calculations by reading the output of the quantum chemistry software (which contains
    !! the repeated input of the quantum chemistry software).

    use types,           only: ip
    use system,          only: iostat_ok, die
    use symmetry,        only: determine_molpro_point_group, point_group
    use characters,      only: upper
    use iso_fortran_env, only: iostat_end

    implicit none

    character(*), intent(in) :: qchem_code
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
      if(io .ne. iostat_ok) call die("Problem reading data from file " // qchem_filename)

      select case(qchem_code)
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
  ! subroutine read_target(denprop_filename)
  !   !! Read the first geometry's denprop.out to determine the number and properties of the target elecronic states

  !   use types,           only: ip, rp
  !   use system,          only: iostat_ok, die
  !   use globals,         only: reduced_mass, natoms, targ, targ_proj, targ_ndegen
  !   use symmetry,        only: convert_ukrmol_irrep, point_group
  !   use utilities,       only: read_blank
  !   use constants,       only: au2amu
  !   use iso_fortran_env, only: iostat_end

  !   implicit none

  !   character(*), intent(in) :: denprop_filename

  !   logical :: exists

  !   integer :: i
  !   integer :: n
  !   integer :: io
  !   integer :: irrep
  !   integer :: funit
  !   integer :: targ_spin
  !   integer :: natoms_read
  !   integer :: ntarg

  !   integer :: ijunk(10)

  !   real(rp) :: E
  !   real(rp) :: rjunk(10)

  !   character(1) :: cjunk(10)

  !   real(rp) :: m1, m2
  !     !! The masses of the target molecule (atomic mass units on read)

  !   character(33) :: line
  !   character(33), parameter :: target_character = " Output to unit 24 is as follows:"

  !   inquire(file = denprop_filename, exist = exists)
  !   if(.not. exists) call die("The DENPROP output file file " // denprop_filename // " does not exist.")

  !   open(newunit = funit, file = denprop_filename)

  !   lines: do

  !     read(funit, '(A)', iostat = io) line

  !     if(io .eq. iostat_end) exit lines
  !     if(io .ne. iostat_ok) call die("Problem reading data from file " // denprop_filename)

  !     if(line .eq. target_character) exit lines

  !   enddo lines

  !   call read_blank(funit)

  !   read(funit, *) ijunk(1:3), natoms_read, ntarg, ijunk(1:2), rjunk(1:3)

  !   if(natoms_read .ne. natoms) call die("The number of atoms in " // denprop_filename // " does not match the namelist input")

  !   if(size(targ_proj, 1) .lt. ntarg) call die("Target state electronic angular momentum projections not supplied for all states")

  !   allocate(targ(ntarg))

  !   ! -- determine the reduced mass of the molecule while we're here
  !   read(funit, *) ijunk(1:2), cjunk(1), ijunk(1), m1, rjunk(1:3)
  !   read(funit, *) ijunk(1:2), cjunk(1), ijunk(1), m2, rjunk(1:3)
  !   reduced_mass = m1 * m2 / (m1 + m2) / au2amu

  !   ! -- read info about the target states
  !   do i = 1, ntarg

  !     read(funit, *) ijunk(1), n, ijunk(1:2), irrep, targ_spin, ijunk(1:2), E

  !     call convert_ukrmol_irrep(irrep, point_group)

  !     targ(i) % n     = i
  !     targ(i) % irrep = irrep
  !     targ(i) % M     = targ_proj(i)
  !     targ(i) % nrg   = E

  !     if(allocated(targ_ndegen)) then
  !       targ(i) % ndegen = targ_ndegen(i)
  !     else
  !       targ(i) % ndegen = n
  !     endif

  !   enddo

  ! end subroutine read_target

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  ! subroutine add_channels_from_file(channelfile)
  !   !! Read the channels specified in the supplied channelfile

  !   use types,      only: ip, rp
  !   use system,     only: iostat_ok, die
  !   ! use globals,    only: electronic_channels, electronic_channel_type, targ
  !   use control,    only: E2au => ukrmol_channel_energy2au, input_type
  !   use utilities,  only: read_blank
  !   use characters, only: upper

  !   implicit none

  !   character(*), intent(in) :: channelfile

  !   logical :: exists

  !   integer :: io
  !   integer :: funit
  !   integer :: ntarg
  !   integer :: nchan
  !   integer :: ichan
  !   integer :: itarg
  !   integer :: l
  !   integer :: lambda
  !   integer :: ichan_read
  !   integer :: ijunk(1:4)
  !   integer :: q

  !   real(rp) :: E

  !   real(rp) :: rjunk

  !   character(:), allocatable :: filename

  !   filename = trim(channelfile)

  !   inquire(file = filename, exist = exists)

  !   if(.not. exists) call die("Channel file " // filename // "does not exist.")

  !   open(newunit = funit, file = filename)

  !   ! -- the default value for the channel normalization if not specified as something else in the channel files
  !   q = 4

  !   call read_blank(funit, 2)
  !   read(funit, *, iostat = io) ntarg, ijunk(1:2), nchan
  !   if(io .ne. iostat_ok) call die("Problem reading data from file " // filename)

  !   call read_blank(funit, ntarg + 1)

  !   do ichan = 1, nchan
  !     select case(upper(trim(input_type)))
  !     case("UKRMOL") ; read(funit, *, iostat = io) ichan_read, itarg, l, lambda, E
  !     case("DAVID")  ; read(funit, *, iostat = io) ichan_read, itarg, l, lambda, E, q
  !     case default
  !       call die("Could not determine the proper input type. Given input_type: " // trim(input_type))
  !     end select
  !     if(ichan .ne. ichan_read) call die("Channel indices don't match up in " // filename)
  !     if(io .ne. iostat_ok) call die("Problem reading data from file " // filename)
  !     E = E * E2au
  !     ! -- we have the info for this channel, now we just need to push it to an array that has this channel info.
  !     !    First, make an array that can hold this info (probably in global ?) and add one channel at a time to it.
  !     !    Have a way to search for duplicate channels ! Make sure to have a "true" channel index and at each geometry
  !     !    That's different than 1, we make sure that channels dont swap order.
  !     if(allocated(electronic_channels)) then
  !       electronic_channels = [            &
  !         electronic_channels,             &
  !         electronic_channel_type(         &
  !           idx    = 0,                    &
  !           n      = itarg,                &
  !           ndegen = targ(itarg) % ndegen, &
  !           irrep  = targ(itarg) % irrep,  &
  !           M      = targ(itarg) % M,      &
  !           l      = l,                    &
  !           lambda = lambda,               &
  !           q      = q                     &
  !         )                                &
  !       ]
  !     else
  !       electronic_channels = [            &
  !         electronic_channel_type(         &
  !           idx    = 0,                    &
  !           n      = itarg,                &
  !           ndegen = targ(itarg) % ndegen, &
  !           irrep  = targ(itarg) % irrep,  &
  !           M      = targ(itarg) % M,      &
  !           l      = l,                    &
  !           lambda = lambda,               &
  !           q      = q                     &
  !         )                                &
  !       ]
  !     endif
  !   enddo

  ! end subroutine add_channels_from_file

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure subroutine sort_electronic_channels(channels)
    !! Sorts the given array of electronic channels (of type electronic_channel_type)
    !! by increasing n (target electronic state), then
    !! by increasing l, then
    !! by λ from -l to l

    use types, only: ip, rp, elec_chanl_type

    implicit none

    type(elec_chanl_type), intent(inout) :: channels(:)

    integer :: ichan
    integer :: jchan
    integer :: nchan
    integer :: ni
    integer :: nj
    integer :: li
    integer :: lj
    integer :: lambdai
    integer :: lambdaj

    nchan = size(channels, 1)

    ! -- sort by target states
    do ichan = 1, nchan
      do jchan = ichan + 1, nchan
        ni = channels(ichan) % targ % n
        nj = channels(jchan) % targ % n
        if(nj .ge. ni) cycle
        call swap_elec_chanl_vals(channels(ichan), channels(jchan))
      enddo
    enddo

    ! -- sort by l
    do ichan = 1, nchan
      do jchan = ichan + 1, nchan
        ni = channels(ichan) % targ % n
        li = channels(ichan) % l
        nj = channels(jchan) % targ % n
        lj = channels(jchan) % l
        if(nj .ne. ni) cycle
        if(lj .ge. li) cycle
        call swap_elec_chanl_vals(channels(ichan), channels(jchan))
      enddo
    enddo

    ! -- sort by λ
    do ichan = 1, nchan
      do jchan = ichan + 1, nchan
        ni      = channels(ichan) % targ % n
        li      = channels(ichan) % l
        lambdai = channels(ichan) % lambda
        nj      = channels(jchan) % targ % n
        lj      = channels(jchan) % l
        lambdaj = channels(jchan) % lambda
        if(nj .ne. ni)           cycle
        if(lj .ne. li)           cycle
        if(lambdaj .ge. lambdai) cycle
        call swap_elec_chanl_vals(channels(ichan), channels(jchan))
      enddo
    enddo

    ! -- add channel indices now that they're sorted
    do ichan = 1, nchan
      channels(ichan) % idx = ichan
    enddo

  end subroutine sort_electronic_channels

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine read_geometries(geoms)
    !! Read the geometries from the file "geometries" in the UKRmol+ folder and
    !! filters which geometries to include based on the user's selection. Will try
    !! and ignore duplicate geometries

    use types,           only: rp
    use arrays,          only: append
    use system,          only: iostat_ok, die, stdout
    use control,         only: R2au => ukrmol_internuclear_distance2au, geom_start, geom_end, skip_geom
    use utilities,       only: read_blank, reals_are_eq
    use directories,     only: ds => directory_separator, input_directory
    use iso_fortran_env, only: iostat_end

    real(rp), allocatable, intent(out) :: geoms(:)
      !! Array of the input geometries (internuclear distances)

    integer :: igeom
    integer :: funit
    integer :: io
    integer :: ngeom
    real(rp) :: R

    integer, allocatable :: indices(:)

    character(:), allocatable :: filename

    filename = input_directory // ds // "geometries"

    open(newunit = funit, file = filename)
    call read_blank(funit)

    ! -- read file to determine available geometries
    do
      read(funit, *, iostat = io) igeom, R

      select case(io)
      case(iostat_ok)
        R = R * R2au
        call append(geoms, R)
      case(iostat_end)
        exit
      case default
        call die("Unknown problem reading " // filename)
      end select
    enddo

    close(funit)

    ngeom = size(geoms, 1)

    ! -- prune geometries
    do igeom = 1, ngeom

      if(igeom .lt. geom_start) cycle

      if(igeom .gt. geom_end) exit

      if(any(igeom .eq. skip_geom)) then
        write(stdout, '("Skipping geometry ", I0, " per user request")') igeom
        cycle
      endif

      if(igeom .ne. ngeom) then
        ! -- skip duplicate geometries
        if(reals_are_eq(geoms(igeom), geoms(igeom + 1))) then
          write(stdout, '("Skipping geometry ", I0, " because is the same as geometry ", I0)') igeom, igeom + 1
          cycle
        endif
      endif

      call append(indices, igeom)

    enddo

    geoms = geoms(indices)

  end subroutine read_geometries

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine read_ukrmol_kmats(spin, igeom)

    use types,       only: ip
    use system,      only: die
    use symmetry,    only: irrep_name, spin_name, point_group, group_size
    use utilities,   only: read_blank
    use characters,  only: i2char => int2char0
    use directories, only: ds => directory_separator, input_directory

    implicit none

    integer, intent(in) :: spin
      !! The spin multiplicity 2S + 1 of the target + electron system
    integer, intent(in) :: igeom
      !! The current geometry index

    logical :: exists
    integer :: funit
    integer :: io
    integer :: irrep
    integer :: ijunk(2)
    integer :: ntarg_read
    integer :: nchan_read
    character(:), allocatable :: filename

    ! -- loop over irreps,
    do irrep = 1, group_size(point_group)

      ! -- read this geometry's channels to determine if channels need to be swapped
      filename = input_directory // "collected_scattering_data" // ds // "channels" // ds // "channels.geom" // i2char(igeom) &
        // "." // spin_name(spin) // "." // irrep_name(irrep, point_group)
      inquire(           &
        file = filename, &
        exist = exists   &
      )

      if(.not. exists) call die("The file " // filename // " does not exist")

      open(newunit = funit, file = filename)

      call read_blank(funit, 2)

      read(funit, *, iostat = io) ntarg_read, ijunk(1:2), nchan_read

      call read_blank(funit)

      call die("We're at the point were we're reading the channels for each irrep and trybing to see if the degenerate states are" &
      // " different or if states cross. Mayb")

      close(funit)

      ! if state changed , figure out its symmetry. If symm changed, skipp appropriate number of states
      ! if skipping degen states, make sure their energies are the same andActualy degenerate.

      ! if degen states swap, make sure they have actaully swapped with energy check and then determine which irrep to swap to

      ! -- determine if the electronic states are in the correct order
      ! Will need to compare their irrep and their spin multiplicity
      ! Between geometries, a pair of degenerate states could swap, but actual electronic states could change order.
      ! Use the fact that target states of the same irrep  cannot cross,
      ! e.g., 1A1 (1B1 1B2) 2A1 can become
      ! (1B1 1B2) 1A1 2A2
      ! 1A1 2A1 (1B1 1B2)
      ! but not
      ! (1B1 1B2) 2A1 1A2
      ! 2A1 (1B1 1B2) 1A1 etc.
      ! Need to go one state at a time and itentiy the degenerate states first ?
      ! Given that we're reading in an irrep, the channels

    enddo

    ! -- we'd need to figure out what the inpur energy is froMthe channel files (atomic units I think) and then choose what units
    ! to print them out in (print to a file, of course). Will need to figure out how to structure the output at this point
    call die("Have the code print out the target state energies ?")

  end subroutine read_ukrmol_kmats

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  subroutine determine_qchem_code(qchem_code, qchem_filename)
    !! Determine which quantum chemistry code's output we want to use
    use control,     only: selected_qchem_code, default_qchem_code
    use system,      only: die
    use characters,  only: i2char => int2char0
    use directories, only: ds => directory_separator, input_directory

    implicit none

    character(:), allocatable, intent(out) :: qchem_code
      !! The name of the detected quantum chemistry code
    character(:), allocatable, intent(out) :: qchem_filename
      !! The corresponding quantum chemistry code's output file
    character(6), parameter :: qchem_codes(3) = [ "molcas", "molpro", "psi4  " ]
      !! The quantum chemistry codes that can be used with UKRmol+
    integer :: nqchem_detected
      !! The number of quantum chemistry codes that were detected
    logical :: qchem_exists(3)
      !! Array of logicals keeping track of which quantum chemistry codes are present

    qchem_code = trim(selected_qchem_code)

    if(qchem_code .eq. default_qchem_code) then

      ! -- No quantum chemistry code name provided. Check that exactly one quantum chemistry
      !    output is present and take that one, else die
      qchem_exists = does_qchem_output_exist(qchem_codes)
      nqchem_detected = count(qchem_exists .eqv. .true.)
      if( nqchem_detected .ne. 1 ) then
        call die("There were " // i2char(nqchem_detected) // &
          " quantum chemistry output files detected, but there should be exactly one.")
      endif

      qchem_code = trim(qchem_codes(findloc(qchem_exists, .true., 1)))

    else

      qchem_exists(1) = does_qchem_output_exist(qchem_code)

      if(.not. qchem_exists(1)) call die("The quantum chemistry output file for " // qchem_code // " cannot be detected !")

    endif

    qchem_filename = input_directory // "geom1" // ds // "outputs" // ds // "target." // trim(qchem_code) // ".out"

  contains

    impure elemental function does_qchem_output_exist(name) result(res)
      !! Check if there is output corresponding to the quantum chemistry software given as input.
      !! Only checks the first geometery.

      use system, only: iostat_ok
      use directories, only: ds => directory_separator, input_directory

      implicit none

      character(*), intent(in) :: name
      logical :: res
      integer :: funit, io

      character(:), allocatable :: filename

      filename = input_directory // "geom1" // ds // "outputs" // ds // "target." // trim(name) // ".out"

      inquire(file = filename, exist = res)

    end function does_qchem_output_exist

  end subroutine determine_qchem_code

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure subroutine swap_elec_chanl_vals(channel1, channel2)
    !! Swap the values describing two electronic channels, except for their channel indices.
    !! After using this routine, the indices should be added/updated, anyway.
    use types, only: elec_chanl_type

    implicit none

    type(elec_chanl_type), intent(inout) :: channel1
    type(elec_chanl_type), intent(inout) :: channel2

    integer :: idx1, idx2
    type(elec_chanl_type) :: tmp

    idx1 = channel1 % idx
    idx2 = channel2 % idx

    tmp      = channel1
    channel1 = channel2
    channel2 = channel1

    channel1 % idx = idx1
    channel2 % idx = idx2

  end subroutine swap_elec_chanl_vals

! ================================================================================================================================ !
end module UKRmol_scattering_data
! ================================================================================================================================ !
