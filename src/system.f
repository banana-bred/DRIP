! =================================================================================================================================!
module system
  !! Contains the definitions of stdout, stdin, stderr, and procedures to interact with the program/system
  !! such as producing warnings and stopping the execution of the code while producing error messages

  ! use arrays,          only: append, alloc, dealloc
  use types,           only: ip, rp, big_char
  use iso_fortran_env, only: input_unit, output_unit, error_unit, iostat_end

  implicit none

  private

  save

  ! -- procedures
  public :: die
  public :: warn
  public :: determine_system_properties

  ! -- variables
  public :: stdin
  public :: stdout
  public :: stderr
  public :: iostat_ok
  public :: shell_ok
  public :: progname
  public :: OS_is_windows

  logical :: OS_is_windows
    !! Is the current operating system Windows ?

  integer(ip), parameter :: stdin  = input_unit
    !! The file unit associated with standard input
  integer(ip), parameter :: stdout = output_unit
    !! The file unit associated with standard output
  integer(ip), parameter :: stderr = error_unit
    !! The file unit associated with standard error

  integer(ip), parameter :: iostat_ok = 0
    !! The expected iostat result from a successful call to read()
  integer(ip) :: shell_ok
    !! The expected return value for the current environment and shell. Used in system calls.

  character(4), parameter :: progname = "DRIP"

  interface die
    module procedure :: die_1
    module procedure :: die_2
  end interface die

  interface warn
    module procedure :: warn_1
    module procedure :: warn_2
  end interface warn

! ================================================================================================================================ !
contains
! ================================================================================================================================ !

! --------------------------------------------------------------------------------------------------------------------------------- !
subroutine die_1(message)
  !! Stop program execution with a message
  implicit none
  character(*), intent(in), optional :: message
  write(stderr,*)
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@                          ERROR                            @@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,*)
  if(.not.present(message)) error stop
  write(stderr,'("STOP",X,"::",X,A)') message
  write(stderr,*)
  error stop
end subroutine die_1
! --------------------------------------------------------------------------------------------------------------------------------- !
subroutine die_2(message1,message2)
  !! Stop program execution with two messages
  implicit none
  character(*), intent(in) :: message1
  character(*), intent(in) :: message2
  write(stderr,*)
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@                          ERROR                            @@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,*)
  write(stderr,'("STOP",X,"::",X,A,/,A)') message1, message2
  write(stderr,*)
  error stop
end subroutine die_2

! --------------------------------------------------------------------------------------------------------------------------------- !
subroutine warn_1(message)
  !! Print a warning message, but don't stop the program's execution
  implicit none
  character(*), intent(in) :: message
  write(stderr,*)
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@                         WARNING                           @@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,*)
  write(stderr,'("WARN",X,"::",X,A)') message
  write(stderr,*)
end subroutine warn_1
! --------------------------------------------------------------------------------------------------------------------------------- !
subroutine warn_2(message1,message2)
  !! Print two warning messages, but don't stop the program's execution
  implicit none
  character(*), intent(in) :: message1
  character(*), intent(in) :: message2
  write(stderr,*)
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@                         WARNING                           @@@@")')
  write(stderr,'("@@@@                                                           @@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,'("@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@")')
  write(stderr,*)
  write(stderr,'("WARN",X,"::",X,A,/,A)') message1, message2
  write(stderr,*)
end subroutine warn_2

! --------------------------------------------------------------------------------------------------------------------------------- !
subroutine determine_system_properties()
  !! Detects the type of the operating system. As far as system calls and directory structure go,
  !! this basically resolved to Windows or not Windows.

  use fpm_environment, only: get_os_type, &
                             OS_NAME,     &
                             OS_UNKNOWN,  &
                             OS_LINUX,    &
                             OS_MACOS,    &
                             OS_WINDOWS,  &
                             OS_CYGWIN,   &
                             OS_SOLARIS,  &
                             OS_FREEBSD,  &
                             OS_OPENBSD,  &
                             OS_WINDOWS

  implicit none

  integer(ip) :: OS
  integer(ip) :: OS_type_name

  OS = get_os_type()

  select case(OS)
  case(OS_LINUX, OS_MACOS, OS_CYGWIN, OS_SOLARIS, OS_FREEBSD, OS_OPENBSD)

    OS_is_windows = .false.

  case(OS_UNKNOWN)

    OS_is_windows = .false.
    call warn("Operating system unknown. Assuming it is of type unix.")

  case(OS_WINDOWS)

    OS_is_windows = .true.

  case default

    OS_is_windows = .false.
    call warn("Unable to detect the fact that the operating system is unknown. Assuming it is of type unix.")

  end select

  call system("", status = shell_ok)

  write(stdout, '(A)') "Detected operating system type :: " // OS_NAME(OS)
  write(stdout, *)

end subroutine determine_system_properties
! =================================================================================================================================!
end module system
! =================================================================================================================================!
