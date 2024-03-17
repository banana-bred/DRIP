! ================================================================================================================================ !
program DRIP
  !! The main program.

  use types,                  only: ip, rp
  use system,                 only: stdout, determine_system_properties
  use globals,                only: spins, nspins, spin_name
  use control,                only: num_evaluation_energies, energy_dependent
  use characters,             only: s_hms, int2char0, add_trailing
  use directories,            only: make_directories, directory_separator, run_directory, spin_directory, energy_directory
  use UKRmol_scattering_data, only: get_K_matrix

  implicit none

  integer(ip) :: inrg
  integer(ip) :: ispin

  real(rp) :: start
  real(rp) :: finish

  call print_header

  ! -- when the program starts
  start = time()

  ! -- we can do an energy DEPENDENT method or an energy INDEPENDENT
  ! independent:
  !  energy selection by (i) energy value or (ii) energy number
    !  (i) is good for the UKRmol output where you can pick an energy and get close to what you want
    !   (do something like given an energy Ei, pick the closest energy E in the UKRmol file that satisfies this)
    !  (ii) is good for David's Kmats, but (ii) should also work. Both should work in both cases
  !
  ! dependent:
  !  only works with david's code because we need negative energies

  call read_namelists

  call make_directories

  ! -- The main loop over spin multiplicities. The program will run spin-agnostically within this loop
  !    Afterwards, the program average results over the input spin multiplicities.
  spin_loop: do ispin = 1 , nspins

    spin_directory = run_directory // spin_name(spins(ispin))
    call add_trailing(spin_directory, directory_separator)

    ! -- The loop over evaluation energies of the K-matrices. The energy-dependent implementation will
    !    iterate through this loop exactly once, i.e., there is essentially no loop in this case. The
    !    energy-independent calculations are all entirely separate from one another WITHIN A SPIN
    !    MULTIPLICITY. This means that the program will relay no information between energies E1 and E2,
    !    but energy E1 from spin 1 and E1 from spin 2 will be averaged together
    nrg_loop: do inrg = 1, num_evaluation_energies

      ! -- make sure we have the right energy_directory for this pass
      if(energy_dependent) then
        energy_directory = spin_directory // "energy_dependent"
      else
        energy_directory = spin_directory // "energy_independent" // directory_separator // "E" // int2char0(inrg)
      endif
      call add_trailing(energy_directory, directory_separator)

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! ready to read K-matrices ? This routine should only read the
      ! K-matrices and get the relevant information from the UKRmol+ codes
      ! like channel data and energies, and target state projections if
      ! possible. I have not figured out how to do the latter yet
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      call get_K_matrix

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! do stuff to k matrices
      !  interpolation, sine and cosine matrices, etc
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! get S matrices someway somehow
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! VFT
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! RFT
      !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!




    enddo nrg_loop



  enddo spin_loop

  ! -- Combine results from different spin multiplicities
  ! ...

  ! -- when the program ends
  finish = time()

  call print_footer(start, finish)

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! -------------------------------------------------------------------------------------------------------------------------------- !
  subroutine read_namelists
    !! Read program parameters from standard input

    use control,     only: read_control
    use system,      only: stdout, progname
    use globals,     only: read_globals
    use directories, only: read_directories

    implicit none

    write(stdout, '(A)') "=================================================="
    write(stdout, '(A)') "Printing namelist variables as interpreted by " // progname
    write(stdout, '(A)') "=================================================="
    write(stdout, *)

    call read_control

    call read_globals

    call read_directories

    write(stdout, '(A)') "=================================================="
    write(stdout, '(A)') "Done printing namelist variables"
    write(stdout, '(A)') "=================================================="
    write(stdout, *)

  end subroutine read_namelists

  ! -------------------------------------------------------------------------------------------------------------------------------- !
  subroutine print_header()
    use system,          only: stdout, determine_system_properties
    use iso_fortran_env, only: compiler_version, compiler_options
    implicit none
    write(stdout, *)
    write(stdout, '(A)') "_____________________________________________________________"
    write(stdout, *)
    write(stdout, '(A)') "       _/_/_/        _/_/_/_/     _/_/_/_/_/    _/_/_/_/_/"
    write(stdout, '(A)') "      _/     _/     _/     _/        _/        _/       _/"
    write(stdout, '(A)') "     _/      _/    _/     _/        _/        _/       _/"
    write(stdout, '(A)') "    _/      _/    _/   _/          _/        _/_/_/_/_/"
    write(stdout, '(A)') "   _/      _/    _/     _/        _/        _/"
    write(stdout, '(A)') "  _/_/_/_/      _/       _/  _/_/_/_/_/    _/"
    write(stdout, '(A)') "_____________________________________________________________"
    write(stdout, *)
    write(stdout, '(A)') "       Dissociative Recombination and PhotoIonization        "
    write(stdout, *)
    write(stdout, *)
    ! -- determine system / environment properies for system interaction later on
    call determine_system_properties
    write(stdout, "(2A)") "Fortran compiler and version :: ", compiler_version()
    write(stdout, *)
    write(stdout, "(2A)") "Fortran compiler options :: ", compiler_options()
    write(stdout, *)
  end subroutine print_header

  ! -------------------------------------------------------------------------------------------------------------------------------- !
  subroutine print_footer(time_start, time_end)
    real(rp) :: time_start
    real(rp) :: time_end
    write(stdout, *)
    write(stdout, '(A)') "=================================================="
    write(stdout,'(A)') "Program complete (^:"
    write(stdout, *)
    write(stdout,'("Elapsed time: ",A)') s_hms(time_end - time_start)
    write(stdout, '(A)') "=================================================="
  end subroutine print_footer

! ================================================================================================================================ !
end program DRIP
! ================================================================================================================================ !
