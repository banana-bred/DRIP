! ================================================================================================================================ !
module UKRmol_scattering_data
  !! Contains procedures used to read electron scattering data from the UKRmol+ output, such as K-matrices, target electronic
  !! state energies, channel data (quantum numbers, indices, and energies), etc.

  implicit none

  private

  public get_K_matrix_and_electronic_channels

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  subroutine get_K_matrix_and_electronic_channels(spin, inrg)
    !! This subroutine reads the desired K-matrix and electronic channels from the UKRmol+ output nearest to the specified energy

    use control, only: evaluation_energy_indices, evaluation_energies

    implicit none

    integer, intent(in) :: spin
      !! The spin multiplicity of the target + electron
    integer, intent(in) :: inrg
      !! The index of the energy to read. This is the index of EVALUATION_ENERGY_INDICES or EVALUATION_ENERGIES.
      !! For example, if EVALUATION_ENERGY_INDICES = [1, 3, 10], then EVALUATION_ENERGY_INDICES(inrg=3) is 10.
      !! This is to say the inrg is not the index inf EVALUATION_ENERGY_INDICES, but is used to index this array
      !! of indices.

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

! ================================================================================================================================ !
end module UKRmol_scattering_data
! ================================================================================================================================ !
