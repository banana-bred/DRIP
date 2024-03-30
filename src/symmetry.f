! ===================================================================================================!
module symmetry
  !! Contains procedures and variables related to various symmetries, including point groups,
  !! irreps, and spin multiplicities.


  implicit none

  private

  save

  public :: spin_name
  public :: available_point_groups

  character(3), parameter :: available_point_groups(1) = ["C2V"]

! ===================================================================================================!
contains
! ===================================================================================================!

  ! ---------------------------------------------------------------------------------------------------!
  pure function spin_name(spin_multiplicity) result(output)

    use types, only: ip

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

! ===================================================================================================!
end module symmetry
! ===================================================================================================!
