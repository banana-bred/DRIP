! =================================================================================================================================!
module characters
  !! Contains procedures related to characters and character arrays, such as converting integers to characters

  use types, only: ip, rp, big_char

  implicit none

  private

  ! -- procedures
  public :: csgn
  public :: int2char0
  public :: s_hms
  public :: operator(+)
  public :: ndigits
  public :: numeric
  public :: to_lower
  public :: to_upper
  public :: add_trailing

  interface s_hms
    module procedure int_s_hms
    module procedure real_s_hms
  end interface

  interface operator(+)
    module procedure :: charcat
  end interface

  character(13), parameter :: numeric = "0123456789.+-"
    !! Character array containing the values considered 'numeric'

! =================================================================================================================================!
contains
! =================================================================================================================================!

! ---------------------------------------------------------------------------------------------------------------------------------!
pure elemental function csgn(i)
  !! Return '+' if the integer i is non-negative and '-' otherwise
  implicit none
  integer(ip), intent(in) :: i
  character(1) :: csgn
  csgn = '-' ; if(i.ge.0) csgn = '+'
end function csgn

! ---------------------------------------------------------------------------------------------------------------------------------!
recursive function int2char0(i, n) result(output)
  !! Convert the integer i to a character(n) with leading 0s. If n is not supplied, then this function determines
  !! the value to use such that the output character array 'output' is exactly the size needed to fit the integer i, e.g.,
  !!   int2char0(1) -> '1' ✔️
  !!   int2char0(1, 5) -> '00001' ✔️
  !!   int2char0(13, 1) -> error ❌

  use system, only: die

  implicit none

  integer(ip), intent(in) :: i
  integer(ip), intent(in), optional :: n
  character(:), allocatable :: output

  character(:), allocatable :: errmsg
  character(:), allocatable :: frmt

  integer(ip) :: nn
  integer(ip) :: nnn

  nn = ndigits(i)

  if(present(n)) then

    if(n < nn) then
      errmsg = "Not enough digits given (" // int2char0(n) // ") to accomodate for the supplied integer (" // int2char0(i) // ")"
      call die(errmsg)
    endif

    if(ndigits(n) .gt. ndigits(huge(i))) then
      errmsg = "The number of digits required for 'i' is larger than allowed for the type of 'i', somehow..."
      call die(errmsg)
    endif

    nn = n

  endif

  ! -- allocate characters so that they're wide enough to write to
  allocate(character(nn) :: output)
  allocate(character(digits(nn) + 5) :: frmt)

  write(frmt, '(A, I0, A)') "(I0.", nn, ")"
  write(output, frmt) i

end function int2char0

! ---------------------------------------------------------------------------------------------------------------------------------!
pure function charcat(a,b) result(ab)
  !! Concatenates two character arrays: a, b -> ab
  character(*), intent(in)  :: a, b
  character(:), allocatable :: ab
  ab = a // b
end function charcat

! ---------------------------------------------------------------------------------------------------------------------------------!
function int_s_hms(s) result(time)
  !! Given an integer 's' in seconds, convert to the format hh:mm:ss.

  implicit none

  integer(ip), intent(in) :: s
  character(:), allocatable :: time

  character(big_char) :: tmp
  integer(ip) :: hh,mm,ss

  select case(s)
    case(:-1)     ; time = 'negative time' ; return
    case(0:59)    ; hh = 0      ; mm = 0              ; ss = s                ; write(tmp,'(I2,"h ",I2,"m ",I2,"s")') hh,mm,ss
    case(60:3599) ; hh = 0      ; mm = s/60           ; ss = s - mm*60        ; write(tmp,'(I2,"h ",I2,"m ",I2,"s")') hh,mm,ss
    case(3600:)   ; hh = s/3600 ; mm = (s-hh*3600)/60 ; ss = s-hh*3600-mm*60  ; write(tmp,'(I0,"h ",I2,"m ",I2,"s")') hh,mm,ss
  end select

  time = trim(tmp)

end function int_s_hms

! ---------------------------------------------------------------------------------------------------------------------------------!
function real_s_hms(s_re) result(time)
  !! Given an integer in seconds, convert to the format hh:mm:ss. Input is a real, gets converted to int
  implicit none

  real(rp), intent(in) :: s_re
  character(:), allocatable :: time

  character(big_char) :: tmp
  integer(ip) :: hh,mm,ss
  integer(ip) :: s

  s = int(s_re)

  select case(s)
  case(:-1)     ; time = 'negative time' ; return
  case(0:59)    ; hh = 0      ; mm = 0              ; ss = s
  case(60:3599) ; hh = 0      ; mm = s/60           ; ss = s - mm*60
  case(3600:)   ; hh = s/3600 ; mm = (s-hh*3600)/60 ; ss = s-hh*3600-mm*60
  end select

  write(tmp,'(I0,"h ",I0,"m ",I0,"s")') hh,mm,ss

  time = trim(tmp)

end function real_s_hms

! ---------------------------------------------------------------------------------------------------------------------------------!
pure elemental function ndigits(n) result(num)
  !! Returns number of characters an integer will occupy

  use constants, only: one

  implicit none

  integer(ip), intent(in) :: n
  integer(ip) :: num

  num = 1

  if(n .eq. 0) return

  num = floor(log10(abs(n) * one)) + 1

  ! -- account for minus sign
  if(n.lt.1) num = num + 1

end function ndigits

! ---------------------------------------------------------------------------------------------------------------------------------!
pure elemental subroutine to_lower(chr)
  !! converts a character to lower case

  use constants, only: uppercase_a, uppercase_z

  implicit none

  character(*), intent(inout) :: chr

  integer(ip) :: i
  integer(ip) :: n
  integer(ip) :: ic

  n = len(chr)

  do i = 1, n

    ic = ichar(chr(i:i))

    if(ic .lt. uppercase_a) cycle

    if(ic .gt. uppercase_z) cycle

    chr(i:i) = char(ic+32)

  enddo

end subroutine to_lower

! ---------------------------------------------------------------------------------------------------------------------------------!
pure elemental subroutine to_upper(chr)
  !! converts a character to upper case

  use constants, only: lowercase_a, lowercase_z

  implicit none

  character(*), intent(inout) :: chr

  integer(ip) :: i
  integer(ip) :: n
  integer(ip) :: ic

  n = len(chr)

  do i = 1, n

    ic = ichar(chr(i:i))

    if(ic .lt. lowercase_a) cycle
    if(ic .gt. lowercase_z) cycle
    chr(i:i) = char(ic-32)

  enddo

end subroutine to_upper

! ---------------------------------------------------------------------------------------------------------------------------------!
pure subroutine add_trailing(chr, trail)

  implicit none

  character(:), allocatable,  intent(inout) :: chr
  character(*), intent(in) :: trail

  integer(ip) :: n
  integer(ip) :: m

  n = len(chr) - len(trail) + 1
  m = len(chr)

  if(n < 1) return

  if(chr(n:m) .eq. trail) return

  chr = chr // trail

end subroutine add_trailing

! =================================================================================================================================!
end module characters
! =================================================================================================================================!
