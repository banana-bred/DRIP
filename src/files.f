! =================================================================================================================================!
module files
  !! Contains procedures for special ways to read and write to files

  use characters, only: numeric

  implicit none

  private

  save

  ! -- variables
  public :: frmt_2reals

  ! -- procedures
  public :: read_commented_file

  character(11), parameter :: frmt_xy = "(2e30.20e3)"
    !! The standard format of printing two real numbers.

  interface read_commented_file
    !! Read a file's contents into arrays a and b, with the capability to skip lines that are well-commented.
    !! By "well-commented", I mean that the first comment character appears before or after all numeric values
    !! in a line, not only in between them, e.g.
    !!    ! 12.4    1.4    0.5e-320               ✔️
    !!       12.4   1.4    0.5e-320 ! <-- comment ✔️
    !!    ! 12.4    1.4    0.5e-320 ! <-- comment ✔️
    !!      12.4  ! 1.4    0.5e-320               ❌
    !!      12.4  ! 1.4    0.5e-320 ! <-- comment ❌
    !!
    !! The comment character can be specified, but defaults to "!"

    module procedure :: read_commented_file_real

  end interface read_commented_file


! =================================================================================================================================!
contains
! =================================================================================================================================!

! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine read_commented_file_real(fname, a, b, comment_char_in)
  !! Read a file's contents into arrays a and b, with the capability to skip lines that are properly commented.
  !! Refer to the inerface definition for an explanation
  !! The comment character defaults to "!", but can be set to anything not in the character 'numeric' from the 'characters' module and
  !! also not whitespace. This version reads real numbers.

  implicit none

  real(rp), intent(inout), allocatable :: a(:)
  real(rp), intent(inout), allocatable :: b(:)

  character(*), intent(in) :: fname

  character(1), intent(in), optional :: comment_char_in

  real(rp) :: aElement
  real(rp) :: bElement

  character(1) :: comment_char

  integer(ip) :: inunit
  integer(ip) :: io
  integer(ip) :: commentStart
  integer(ip) :: numericStart
  integer(ip) :: numericEnd

  character(big_char) :: line

  ! -- default comment character
  comment_char = "!"

  ! -- don't know a priori how much we will read, build arrays as we go. Make sure' they're deallocated
  if(allocated(a)) deallocate(a)
  if(allocated(b)) deallocate(b)

  ! -- set different comment character maybe
  if(present(comment_char_in)) comment_char = comment_char_in

  open(newunit = inunit, file = fname)

  do

    read(inunit, "(A)", iostat = io) line

    if(io .eq. iostat_end) exit

    if(io .ne. 0) call die("Problem reading data from file " // "'" // fname // "'")

    commentStart = scan(line, comment_char)    ! -- position of first comment character
    numericStart = scan(line, numeric)         ! -- position of first numeric character
    numericEnd   = scan(line, numeric, .true.) ! -- position of last  numeric character

    ! -- remove comments that appear after all numeric chars
    if(commentStart .gt. numericEnd) line = line(numericStart:numericEnd)

    ! -- cycle reading if the line appears commented out
    if(commentStart .gt. 0 .AND. commentStart .lt. numericStart) cycle

    read(line, *) aElement, bElement

    ! call append(a, aElement)
    ! call append(b, bElement)

  enddo

end subroutine read_commented_file_real

! =================================================================================================================================!
end module files
! =================================================================================================================================!
