! ================================================================================================================================ !
module symmetry
  !! Contains procedures and variables related to various symmetries, including point groups,
  !! irreps, and spin multiplicities.

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
  public :: irrep_name
  public :: convert_ukrmol_irrep

  character(3), parameter :: available_point_groups(1) = ["C2V"]
  character(33), parameter :: abelian_point_groups = "C1, Cs, C2, Ci, C2v, C2h, D2, D2h"
  character(:), allocatable :: point_group

  ! ---------------------------------------- !
  ! The integer labels of the various irreps !
  ! ---------------------------------------- !
  ! -- Cs
  integer, parameter :: Ap  = 1
  integer, parameter :: App = 2
  ! -- Ci, C2h
  integer, parameter :: Ag = 1
  integer, parameter :: Au = 2
  integer, parameter :: Bg = 3
  integer, parameter :: Bu = 4
  ! -- C1, C2, C2v, D2, D2h
  integer, parameter :: A  = 1
  integer, parameter :: A1 = 1
  integer, parameter :: A2 = 4
  integer, parameter :: B  = 2
  integer, parameter :: B1 = 2
  integer, parameter :: B2 = 3
  integer, parameter :: B3 = 4
  integer, parameter :: B1g = 3
  integer, parameter :: B1u = 4
  integer, parameter :: B2g = 5
  integer, parameter :: B2u = 6
  integer, parameter :: B3g = 7
  integer, parameter :: B3u = 8

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  pure function spin_name(spin_multiplicity) result(output)
    !! Return the name of the spin multiplicity given the spin multiplicity 2S + 1

    use types, only: ip

    implicit none

    integer, intent(in) :: spin_multiplicity
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
    !! Only Abelian point groups are considered. Irreps in the code will be referred to by their indicies

    use system,     only: die
    use characters, only: upper

    implicit none

    character(*), intent(in) :: point_group
    character(:), allocatable :: irreps(:)

    integer :: nirreps
    character(:), allocatable :: pg

    pg = upper(trim(point_group))
    nirreps = group_size(pg)


    select case(pg)

      case("C1")
        allocate(character(1) :: irreps(nirreps))
        irreps(A) = "A"

      case("CS")
        allocate(character(3) :: irreps(nirreps))
        irreps(Ap)  = "AP"
        irreps(App) = "APP"

      case("C2")
        allocate(character(1) :: irreps(nirreps))
        irreps(A) = "A"
        irreps(B) = "B"

      case("CI")
        allocate(character(2) :: irreps(nirreps))
        irreps(Ag) = "Ag"
        irreps(Au) = "Au"

      case("C2V")
        allocate(character(2) :: irreps(nirreps))
        irreps(A1) =  "A1"
        irreps(B1) =  "B1"
        irreps(B2) =  "B2"
        irreps(A2) =  "A2"

      case("C2H")
        allocate(character(2) :: irreps(nirreps))
        irreps(A)  = "Ag"
        irreps(B1) = "Au"
        irreps(B2) = "Bg"
        irreps(B3) = "Bu"

      case("D2")
        allocate(character(2) :: irreps(nirreps))
        irreps(A)  = "A"
        irreps(B1) = "B1"
        irreps(B2) = "B2"
        irreps(B3) = "B3"

      case("D2H")
        allocate(character(3) :: irreps(nirreps))
        irreps(Ag)  = "Ag"
        irreps(Au)  = "Au"
        irreps(B1g) = "B1g"
        irreps(B1u) = "B1u"
        irreps(B2g) = "B2g"
        irreps(B2u) = "B2u"
        irreps(B3g) = "B3g"
        irreps(B3u) = "B3u"

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

    use types,      only: ip
    use characters, only: upper

    implicit none

    integer, intent(in) :: irrep1
    integer, intent(in) :: irrep2
    character(*), intent(in) :: point_group
    integer :: irrep_product

    integer, allocatable :: irrep1_characters(:)
    integer, allocatable :: irrep2_characters(:)
    integer, allocatable :: irrep3_characters(:)
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

    use types,      only: ip
    use system,     only: die
    use characters, only: upper

    implicit none

    integer,  intent(in)  :: characters(:)
    character(*), intent(in)  :: point_group
    integer :: irrep

    character(:), allocatable :: pg

    integer :: i
    integer :: n

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

    use types,      only: ip
    use system,     only: die
    use characters, only: upper

    implicit none

    character(*), intent(in) :: point_group
    integer :: n

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

    use types,      only: ip
    use system,     only: die
    use characters, only: upper

    implicit none

    integer, intent(in) :: irrep
    character(*), intent(in) :: point_group

    integer, allocatable :: characters(:)

    character(:), allocatable :: pg

    pg = upper(trim(point_group))

    select case(pg)
    case('C1')
      characters = [ 1 ]

    case('CS')
      select case(irrep)
        case(Ap)  ; characters = [ 1 , 1 ]
        case(App) ; characters = [ 1 ,-1 ]
      end select

    case('C2')
      select case(irrep)
        case(A) ; characters = [ 1 , 1 ]
        case(B) ; characters = [ 1 ,-1 ]
      end select

    case('CI')
      select case(irrep)
        case(Ag) ; characters = [ 1 , 1 ]
        case(Au) ; characters = [ 1 ,-1 ]
      end select

    case('C2V')
      select case(irrep)
        case(A1) ; characters = [ 1 , 1 , 1 , 1]
        case(B1) ; characters = [ 1 ,-1 , 1 ,-1]
        case(B2) ; characters = [ 1 ,-1 ,-1 , 1]
        case(A2) ; characters = [ 1 , 1 ,-1 ,-1]
      end select

    case('C2H')
      select case(irrep)
        case(Ag) ; characters = [ 1 , 1 , 1 , 1]
        case(Au) ; characters = [ 1 , 1 ,-1 ,-1]
        case(Bg) ; characters = [ 1 ,-1 , 1 ,-1]
        case(Bu) ; characters = [ 1 ,-1 ,-1 , 1]
      end select

    case('D2')
      select case(irrep)
        case(A)  ; characters = [ 1 , 1 , 1 , 1]
        case(B1) ; characters = [ 1 , 1 ,-1 ,-1]
        case(B2) ; characters = [ 1 ,-1 , 1 ,-1]
        case(B3) ; characters = [ 1 ,-1 ,-1 , 1]
      end select

    case('D2H')
      select case(irrep)
        case(Ag)  ; characters = [ 1 , 1 , 1 , 1 , 1 , 1 , 1 , 1 ]
        case(Au)  ; characters = [ 1 , 1 , 1 , 1 ,-1 ,-1 ,-1 ,-1 ]
        case(B1g) ; characters = [ 1 , 1 ,-1 ,-1 , 1 , 1 ,-1 ,-1 ]
        case(B1u) ; characters = [ 1 , 1 ,-1 ,-1 ,-1 ,-1 , 1 , 1 ]
        case(B2g) ; characters = [ 1 ,-1 , 1 ,-1 , 1 ,-1 , 1 ,-1 ]
        case(B2u) ; characters = [ 1 ,-1 , 1 ,-1 ,-1 , 1 ,-1 , 1 ]
        case(B3g) ; characters = [ 1 ,-1 ,-1 , 1 , 1 ,-1 ,-1 , 1 ]
        case(B3u) ; characters = [ 1 ,-1 ,-1 , 1 ,-1 , 1 , 1 ,-1 ]
      end select

    case default
      call die("Unacceptable point group '" // point_group // "' given. Please choose one of " // abelian_point_groups // ".")

    end select

  end function irrep_characters

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function determine_molpro_point_group(symmetry_string) result(group)
    !! This function takes a line from a molpro input file generated by UKRmol+ and determines the point group for the calculation

    use system, only: die

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

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function irrep_name(irrep, point_group) result(output)
    !! Given an irrep index in point_group, return the name of the corresponding irrep

    use types,      only: ip
    use system,     only: die
    use characters, only: upper

    implicit none

    integer, intent(in) :: irrep
    character(*), intent(in) :: point_group
    character(:), allocatable :: output

    character(:), allocatable :: pg

    character(:), allocatable :: irreps(:)

    pg     = upper(trim(point_group))
    irreps = group_irreps(pg)
    output = trim(irreps(irrep))

  end function irrep_name

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  impure elemental subroutine convert_ukrmol_irrep(irrep, point_group)
    !! Convert the irrep index from ukrmol to the local irrep indices. Ukrmol irrep indices start at 0 and are defined in the
    !! ukrmollib.pm file supplied in the scripts.

    use types, only: ip
    use system, only: die
    use characters, only: upper, i2char => int2char0

    implicit none

    integer,  intent(inout) :: irrep
    character(*), intent(in) :: point_group

    character(:), allocatable :: pg

    pg = upper(trim(point_group))

    select case(pg)

    case("C1")
      if(irrep .ne. 0) call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      irrep = A

    case("CS")
      select case(irrep)
      case(0) ; irrep = Ap
      case(1) ; irrep = App
      case default
        call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("C2")
      select case(irrep)
      case(0) ; irrep = A
      case(1) ; irrep = B
      case default
        call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("CI")
      select case(irrep)
      case(0) ; irrep = Ag
      case(1) ; irrep = Au
      case default
        call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("C2V")
      select case(irrep)
        case(0) ; irrep = A1
        case(1) ; irrep = B1
        case(2) ; irrep = B2
        case(3) ; irrep = A2
        case default
          call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("C2H")
      select case(irrep)
        case(0) ; irrep = Ag
        case(1) ; irrep = Au
        case(2) ; irrep = Bu
        case(3) ; irrep = Bg
        case default
          call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("D2")
      select case(irrep)
        case(0) ; irrep = A
        case(1) ; irrep = B3
        case(2) ; irrep = B2
        case(3) ; irrep = B1
        case default
          call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case("D2H")
      select case(irrep)
        case(0) ; irrep = Ag
        case(1) ; irrep = B3u
        case(2) ; irrep = B2u
        case(3) ; irrep = B1g
        case(4) ; irrep = B1u
        case(5) ; irrep = B2g
        case(6) ; irrep = B3g
        case(7) ; irrep = Au
        case default
          call die("Unacceptable irrep label " // i2char(irrep) // " supplied for point group " // pg // ".")
      end select

    case default
      call die("Unacceptable point group '" // point_group // "' given. Please choose one of " // abelian_point_groups // ".")

    end select

  end subroutine convert_ukrmol_irrep

  ! ------------------------------------------------------------------------------------------------------------------------------ !
  function targ_sym(targ_irrep, targ_proj, point_group) result(symmetry)
    !! Return the symmetry of the target state based on its irrep and known angular momentum projection

    use types,      only: ip
    use system,     only: die
    use characters, only: upper, i2char => int2char0

    integer, intent(in) :: targ_irrep
    integer, intent(in) :: targ_proj
    character(*), intent(in) :: point_group
    character(:), allocatable :: symmetry

    integer :: i
    integer :: m
    character(:), allocatable :: pg

    i  = targ_irrep
    m  = targ_proj
    pg = upper(trim(point_group))

    select case(pg)

    case("C1", "CS", "C2", "CI", "C2H", "D2", "D2H")
      call die("targ_sym not programmed for point group " // pg)

    case("C2V")

      select case(abs(m))

      case(0)
        select case(i)
          case(A1) ; symmetry = "SP" ! Σ+
          case(A2) ; symmetry = "SM" ! Σ-
          case default
            call die("Undetermined symmetry in " // pg // " with projection " // i2char(m) // " and irrep " // i2char(i) )
        end select

      case(1)
        select case(i)
          case(B1, B2) ; symmetry = "P" ! Π
          case default
            call die("Undetermined symmetry in " // pg // " with projection " // i2char(m) // " and irrep " // i2char(i) )
        end select

      case(2)
        select case(i)
          case(A1, A2) ; symmetry = "D" ! Δ
          case default
            call die("Undetermined symmetry in " // pg // " with projection " // i2char(m) // " and irrep " // i2char(i) )
        end select

      case(3)
        select case(i)
          case(B1, B2) ; symmetry = "F" ! Φ
          case default
            call die("Undetermined symmetry in " // pg // " with projection " // i2char(m) // " and irrep " // i2char(i) )
        end select
      case(4)
        select case(i)
          case(A1, A2) ; symmetry = "G" ! Γ
          case default
            call die("Undetermined symmetry in " // pg // " with projection " // i2char(m) // " and irrep " // i2char(i) )
        end select

      case default
        ! -- The pattern seems to be
        ! 0 projection            : A1 XOR A2
        ! odd projection          : B1 & B2
        ! nonzero even projection : A1 & A2
        call die("Projection " // i2char(m) // " has not been programmed")

      end select

    end select

  end function targ_sym

! ================================================================================================================================ !
end module symmetry
! ================================================================================================================================ !
