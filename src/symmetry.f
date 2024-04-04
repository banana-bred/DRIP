! ================================================================================================================================ !
module symmetry
  !! Contains procedures and variables related to various symmetries, including point groups,
  !! irreps, and spin multiplicities.

  use types,      only: ip
  use system,     only: die
  use characters, only: upper

  implicit none

  private

  save

  ! -- variables
  public :: available_point_groups
  public :: point_group
    !! The point group of the UKRmol+ calculation

  ! -- procedures
  public :: group_irreps
  public :: group_size
  public :: irrep_product
  public :: spin_name
  public :: determine_molpro_point_group

  character(3), parameter :: available_point_groups(1) = ["C2V"]
  character(33), parameter :: abelian_point_groups = "C1, Cs, C2, Ci, C2v, C2h, D2, D2h"
  character(:), allocatable :: point_group

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
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

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function group_irreps(point_group) result(irreps)
    !! Return an array containing the names of the irreps in the supplied point_group.
    !! Only Abelian point groups are considered. Irreps in the code will be referred to by their indicies, defined here, i.e.,
    !! the name of irrep 3 in point group C2v is "B1" because that is the value assigned to irreps(3).

    implicit none

    character(*), intent(in) :: point_group
    character(:), allocatable :: irreps(:)

    select case(upper(trim(point_group)))

      case("C1")  ; irreps = ["A"]
      case("CS")  ; irreps = ["AP ", "APP"]
      case("C2")  ; irreps = ["A", "B"]
      case("CI")  ; irreps = ["AG", "AU"]
      case("C2V") ; irreps = ["A1", "A2", "B1", "B2"]
      case("C2H") ; irreps = ["AG", "AU", "BG", "Bu"]
      case("D2")  ; irreps = ["A ", "B1", "B2", "B3"]
      case("D2H") ; irreps = ["AG ", "AU ", "B1G", "B1U", "B2G", "B2U", "B3G", "B3U"]

      case default
        call die("Unacceptable point group '" // point_group // "' given. Please choose one of " // abelian_point_groups // ".")

    end select

  end function group_irreps

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function irrep_product(irrep1, irrep2, point_group)
    !! Given two irrep indices irrep1 and irrep2, return the irrep index for their product.
    !! E.g., irrep_product(2, 3, "C2v") returns 4 because A2 x B1 = B2.
    !! Note: this can also used to "solve" the irrep equation AX = B.
    !!  AX = B
    !!  AAX = AB
    !!  X = AB (any of these irreps Γ multiplied by itself yields the totally symmetry irrep where all characters are +1)

    implicit none

    integer(ip), intent(in) :: irrep1
    integer(ip), intent(in) :: irrep2
    character(*), intent(in) :: point_group
    integer(ip) :: irrep_product

    integer(ip), allocatable :: irrep1_characters(:)
    integer(ip), allocatable :: irrep2_characters(:)
    integer(ip), allocatable :: irrep3_characters(:)
    character(:), allocatable :: pg

    pg = upper(trim(point_group))

    irrep1_characters = irrep_characters(irrep1, pg)
    irrep2_characters = irrep_characters(irrep2, pg)
    irrep3_characters = irrep1_characters * irrep2_characters

    irrep_product = which_irrep(irrep3_characters, pg)

  end function irrep_product

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function which_irrep(characters , point_group) result(irrep)
    !! Given an array of irrep characters in a point_group, return the corresponding irrep index.
    !! E.g., find_irrep([1, 1, -1, -1], "C2v") returns A2 because these are the characters of the associated symmetry operations for
    !! the B1 irrep.

    implicit none

    integer(ip),  intent(in)  :: characters(:)
    character(*), intent(in)  :: point_group
    integer(ip) :: irrep

    character(:), allocatable :: pg

    integer(ip) :: i
    integer(ip) :: n

    pg = upper(trim(point_group))

    n = group_size(pg)

    do irrep = 1, n

      if(all(characters .eq. irrep_characters(i, pg))) return

    enddo

    call die("Failed to determine the irrep in group " // pg // " with the associated characters.")

  end function which_irrep

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function group_size(point_group) result(n)
    !! Return the number of elements in point_group

    implicit none

    character(*), intent(in) :: point_group
    integer(ip) :: n

    character(:), allocatable :: pg

    pg = upper(trim(point_group))

    select case(pg)
      case("C1")
        n = 1

      case("CS", "C2", "CI")
        n = 2

      case("C2V", "C2H", "D2")
        n = 4

      case("D2H")
        n = 8

      case default
        call die("Bad point group (" // pg // ") supplied. Please choose one of " // abelian_point_groups)

    end select


  end function group_size

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function irrep_characters(irrep, point_group) result(characters)
    !! Given an irrep index in point_group, return the array of characters for this irrep wrt the group symmetry operations

    implicit none

    integer(ip), intent(in) :: irrep
    character(*), intent(in) :: point_group

    integer(ip), allocatable :: characters(:)

    character(:), allocatable :: pg

    pg = upper(trim(point_group))

    select case(pg)
    case('C1')
      characters = [ 1 ] ! A

    case('CS')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 ] ! A'  (Ap)
        case(2) ; characters = [ 1 ,-1 ] ! A'' (App)
      end select

    case('C2')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 ] ! A
        case(2) ; characters = [ 1 ,-1 ] ! B
      end select

    case('CI')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 ] ! Ag
        case(2) ; characters = [ 1 ,-1 ] ! Au
      end select

    case('C2V')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 , 1 , 1] ! A1
        case(2) ; characters = [ 1 , 1 ,-1 ,-1] ! A2
        case(3) ; characters = [ 1 ,-1 , 1 ,-1] ! B1
        case(4) ; characters = [ 1 ,-1 ,-1 , 1] ! B2
      end select

    case('C2H')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 , 1 , 1] ! Ag
        case(2) ; characters = [ 1 , 1 ,-1 ,-1] ! Au
        case(3) ; characters = [ 1 ,-1 , 1 ,-1] ! Bg
        case(4) ; characters = [ 1 ,-1 ,-1 , 1] ! Bu
      end select

    case('D2')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 , 1 , 1] ! A
        case(2) ; characters = [ 1 , 1 ,-1 ,-1] ! B1
        case(3) ; characters = [ 1 ,-1 , 1 ,-1] ! B2
        case(4) ; characters = [ 1 ,-1 ,-1 , 1] ! B3
      end select

    case('D2H')
      select case(irrep)
        case(1) ; characters = [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ] ! Ag
        case(2) ; characters = [ 1 , 1 , 1 , 1 ,-1 ,-1 ,-1 ,-1 ] ! Au
        case(3) ; characters = [ 1 , 1 ,-1 ,-1 , 1 , 1 ,-1 ,-1 ] ! B1g
        case(4) ; characters = [ 1 , 1 ,-1 ,-1 ,-1 ,-1 , 1 , 1 ] ! B1u
        case(5) ; characters = [ 1 ,-1 , 1 ,-1 , 1 ,-1 , 1 ,-1 ] ! B2g
        case(6) ; characters = [ 1 ,-1 , 1 ,-1 ,-1 , 1 ,-1 , 1 ] ! B2u
        case(7) ; characters = [ 1 ,-1 ,-1 , 1 , 1 ,-1 ,-1 , 1 ] ! B3g
        case(8) ; characters = [ 1 ,-1 ,-1 , 1 ,-1 , 1 , 1 ,-1 ] ! B3u
      end select

    case default
      call die("Unacceptable point group '" // point_group // "' given. Please choose one of " // abelian_point_groups // ".")

    end select

  end function irrep_characters

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function determine_molpro_point_group(symmetry_string) result(group)
    !! This function takes a line from a molpro input file generated by UKRmol+ and determines the point group for the calculation

    implicit none

    character(:), allocatable :: group
    character(*), intent(in) :: symmetry_string

    select case(trim(symmetry_string))
      case("SYMMETRY,X, Y, Z") ; group = "D2H"
      case("SYMMETRY,X, Y")    ; group = "C2V"
      case("SYMMETRY,XY, Z")   ; group = "C2H"
      case("SYMMETRY,XZ, YZ")  ; group = "D2"
      case("SYMMETRY,XY")      ; group = "C2"
      case("SYMMETRY,X")       ; group = "CS"
      case("SYMMETRY,XYZ")     ; group = "Ci"
      case("SYMMETRY,")        ; group = "C1"
      case default
        call die("Could not determine point group from the line '" // symmetry_string //"'")
    end select

  end function determine_molpro_point_group

! ================================================================================================================================ !
end module symmetry
! ================================================================================================================================ !
