! ===================================================================================================!
module directories
  !! Contains the directories needed for code execution and procedures to make them

  use types,      only: cs => big_char
  use system,     only: stdin, stdout, stderr, OS_is_windows
  use control,    only: molecule

  implicit none

  private

  save

  ! -- procedures
  public :: make_directories
  public :: read_directories

  ! -- variables
  public :: run_name
  public :: run_directory
  public :: spin_directory
  public :: collected_spins_directory
  public :: energy_directory
  public :: directory_separator
  public :: mkdir_command

  character(:), allocatable :: input_directory
    !! The input UKRmol+ directory that the code will read as input
  character(:), allocatable :: output_directory
    !! The main output directory of the code
  character(:), allocatable :: output_molecule_directory
    !! The molecule directory under the output directory of the code

  character(:), allocatable :: run_name
    !! The name associated with this run. Defines the names of output subdirectories
  character(:), allocatable :: run_directory
    !! The full path of the directory of this run (.../Molecule/run_name)
  character(:), allocatable :: energy_directory
    !! The full path of the energy directory of this run. Different for energy dependent and independent runs.
    !! This is a subdirectory of 'spin_directory' and 'collected_spin_directories' if there's more than one spin:
    !!   - energy   dependent: .../Molecule/run_name/spin/energy_dependent
    !!   - energy independent: .../Molecule/run_name/spin/energy_independent/E2
    !!      The subdirectory E2 here is an example. An energy independent run can go over several energies,
    !!      enumerated from 1 to N. Therefore, subdirectories E1, E2, ..., EN will be created
  character(:), allocatable :: spin_directory
    !! The full path of the spin directory of this run. It is a subdirectory of 'run_directory'.
    !! This will be given by the procedure 'spin_name()' from the module 'symmetry'.
  character(:), allocatable :: collected_spins_directory
    !! The full path of the directory containing data that has been averaged over spin multiplicities.
    !! Subdirectory of 'run_directory'. Will not be populated if there is only one spin.

  character(:), allocatable :: mkdir_command
    !! The command used to make directories. OS-dependent.

  character(1) :: directory_separator
    !! The OS-dependent directory separator

  namelist / directories_namelist /           &
    !! Directory names needed for program execution
                            input_directory,  &
                            output_directory, &
                            run_name

! ===================================================================================================!
contains
! ===================================================================================================!

! ---------------------------------------------------------------------------------------------------!
subroutine read_directories
  !! Read directories_namelist

  use types,      only: big_char
  use system,     only: die

  implicit none

  character(big_char), parameter :: temp = ""

  run_name         = temp
  input_directory  = temp
  output_directory = temp

  read(stdin, directories_namelist) ; rewind(stdin)

  ! -- trim space off arrays
  run_name         = trim(run_name)
  input_directory  = trim(input_directory)
  output_directory = trim(output_directory)

  write(stdout, directories_namelist)
  write(stdout, *)

  select case(run_name)
    case(trim(temp)) ; call die("Must define a run_name")
    case default     ; continue
  end select

  select case(input_directory)
    case(trim(temp), "/") ; call die("Must define input_directory")
    case default     ; continue
  end select

  select case(output_directory)
    case(trim(temp), "/") ; call die("Must define output_directory")
    case default     ; continue
  end select

end subroutine read_directories

! ---------------------------------------------------------------------------------------------------!
subroutine make_directories
  !! Make the directories needed for program execution

  use types,      only: ip
  use system,     only: die
  use control,    only: energy_dependent, num_evaluation_energies
  use globals,    only: spins, nspins, spin_name
  use characters, only: add_trailing, int2char0

  implicit none

  logical :: exists

  integer(ip) :: i
  integer(ip) :: ispin
  integer(ip) :: inrg
  integer(ip) :: num_dirs
  integer(ip) :: direc
  integer(ip) :: stat

  character(:),  allocatable :: filename

  character(cs), allocatable :: directories(:)

  if(OS_is_windows) then
    directory_separator = "\"
    mkdir_command = "md "
  else
    directory_separator = "/"
    mkdir_command = "mkdir -p "
  endif

  ! -- check that the input directory exists
  inquire(file = input_directory, exist = exists)
  if(.not. exists) call die("Input directory '" // input_directory // "' does not exist !")

  ! -- make sure directories are ready to have subdirectories concatenated to them
  call add_trailing(input_directory, directory_separator)
  call add_trailing(output_directory, directory_separator)

  ! -- Determine the run directory, which is the parent directory to all other directories created
  !    over the course of this run
  run_directory = output_directory // molecule // directory_separator // run_name
  call add_trailing(run_directory, directory_separator)

  ! -- Make the spin and energy subdirectories that we expect to be populated for this run
  if(nspins .gt. 1) collected_spins_directory = run_directory // "collected_"
  do ispin = 1, nspins

    spin_directory = run_directory // spin_name(spins(ispin))
    call add_trailing(spin_directory, directory_separator)

    ! -- the collected_spins_directory will contain the names of the spins represented
    if(nspins .gt. 1) collected_spins_directory = collected_spins_directory // spin_name(spins(ispin))
    if(ispin .lt. nspins) collected_spins_directory = collected_spins_directory // "_"

    do inrg = 1, num_evaluation_energies

      if(energy_dependent) then
        energy_directory = spin_directory // "energy_dependent"
      else
        energy_directory = spin_directory // "energy_independent" // directory_separator // "E" // int2char0(inrg)
      endif
      call add_trailing(energy_directory, directory_separator)

      call mkdir(energy_directory)

    enddo
  enddo

  if(nspins .gt. 1) then

    call add_trailing(collected_spins_directory, directory_separator)

    do inrg = 1, num_evaluation_energies

      if(energy_dependent) then
        energy_directory = collected_spins_directory // "energy_dependent"
      else
        energy_directory = collected_spins_directory // "energy_independent" // directory_separator // "E" // int2char0(inrg)
      endif
      call add_trailing(energy_directory, directory_separator)

      call mkdir(energy_directory)

    enddo


  endif

  ! E_dir           = trim(outdir) // 'Energies/'
  ! Prob_raw_dir    = trim(outdir) // 'Prob_out/raw/'
  ! XS_raw_dir      = trim(outdir) // 'XS/raw/'
  ! XS_conv_dir     = trim(outdir) // 'XS/convolved/'
  ! XS_therm_dir    = trim(outdir) // 'XS/thermal/'
  ! Rate_raw_dir    = trim(outdir) // 'Rates/raw/'
  ! Rate_conv_dir   = trim(outdir) // 'Rates/convolved/'
  ! Rate_therm_dir  = trim(outdir) // 'Rates/thermal/'
  ! K_elec_dir      = trim(outdir) // 'K_elec/'
  ! K_elec_dir_coup = trim(outdir) // 'K_elec/Couplings/'
  ! S_elec_dir      = trim(outdir) // 'S_elec/'
  ! S_ve_dir        = trim(outdir) // 'S_ve/'
  ! S_rve_dir       = trim(outdir) // 'S_rve/'
  ! P_mtrx_dir      = trim(outdir) // 'Phase_Matrix/'
  ! P_mtrx_dir_coup = trim(outdir) // 'Phase_Matrix/Couplings/'
  ! Probs_dir       = trim(outdir) // 'Probabilities/'
  ! Target_dir      = trim(outdir) // 'Target/'
  ! out_V_dir       = trim(outdir) // 'V/'
  ! dipole_out_dir  = trim(outdir) // 'Dipole/'
  ! in_V_dir        = trim(indir ) // 'V/'

  ! i = 1
  ! allocate( directories(i) )
  ! directories(i) = E_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = S_ve_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = in_V_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = S_rve_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = out_V_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Probs_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = P_mtrx_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Target_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = XS_raw_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = XS_therm_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = K_elec_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = S_elec_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = XS_conv_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Prob_raw_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Rate_therm_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Rate_raw_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = Rate_conv_dir
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = K_elec_dir_coup
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = P_mtrx_dir_coup
  ! i = i + 1 ; directories = [ directories , directories(1) ] ; directories(i) = dipole_out_dir

  ! num_dirs = size(directories,1)

  ! do direc=1,num_dirs

  !   call system('mkdir -p ' // trim(directories(direc)), stat)
  !   if(stat.ne.0) stop trim(directories(direc)) // ": error creating directory"

  !   if(directories(direc).eq.in_V_dir) cycle ! -- no run_name for this. Would just be empty directories
  !   call system('mkdir -p ' // trim(directories(direc)) // run_name_base, stat)

  !   if(stat.ne.0) stop trim(directories(direc)) // run_name_base // ": error creating directory"
  !   call system('mkdir -p ' // trim(directories(direc)) // run_name, stat)

  !   if(stat.ne.0) stop trim(directories(direc)) // run_name      // ": error creating directory"

  ! enddo

end subroutine make_directories

! ---------------------------------------------------------------------------------------------------!
subroutine mkdir(directory)
  !! Makes the directory "directory" and checks that it exists and is writeable

  use types,      only: ip
  use system,     only: die, shell_ok
  use characters, only: int2char0

  implicit none

  character(*) :: directory

  logical     :: exists
  integer(ip) :: stat

  call system(mkdir_command // directory, status = stat)

  inquire(file = directory, exist = exists)
  if(stat .ne. shell_ok) call die("Trying to make directory '" // directory // "' returned status code " // int2char0(stat) )

  inquire(file = directory, exist = exists)

  if(.not. exists) call die("Directory '" // directory // "' could not be made, even though '" // &
    mkdir_command // "' returned " // int2char0(stat))

end subroutine mkdir

! ===================================================================================================!
end module directories
! ===================================================================================================!
