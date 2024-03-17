! ===================================================================================================!
module constants
  !! Contains defined constants, like pi and the number 1 at the current real precision (rp)

  use types, only: ip, rp, rrp

  implicit none

  private

  ! -- variables
  public :: Ryd
  public :: ahalf
  public :: au2Ghz
  public :: au2K
  public :: au2amu
  public :: au2ang
  public :: au2cm
  public :: au2ev
  public :: au2ps
  public :: au2s
  public :: c
  public :: ci
  public :: deg2rad
  public :: euler_mascheroni
  public :: initial_int
  public :: lowercase_a
  public :: lowercase_z
  public :: one
  public :: pi
  public :: rad2deg
  public :: two
  public :: uppercase_a
  public :: uppercase_z
  public :: zero

  ! -- numbers
  real(rp), parameter :: ahalf = 1._rp / 2._rp
  real(rp), parameter :: zero  = 0._rp
  real(rp), parameter :: one   = 1._rp
  real(rp), parameter :: two   = 2._rp
  real(rp), parameter :: pi    = atan(1._rp)*4._rp
    !! π
  complex(rp), parameter :: ci = (0._rp, 1._rp)
    !! the square root of -1

  ! -- ASCII constants
  integer(ip), parameter :: uppercase_a = 65
    !! the ASCII number representing the character "A"
  integer(ip), parameter :: uppercase_z = 90
    !! the ASCII number representing the character "Z"
  integer(ip), parameter :: lowercase_a = 97
    !! the ASCII number representing the character "a"
  integer(ip), parameter :: lowercase_z = 122
    !! the ASCII number representing the character "z"

  integer(ip), parameter :: initial_int = -409
    !! initializ positive integers to this number to figure out if they have taken a reasonable value

  ! -- conversion constants
  real(rp), parameter :: Ryd = 219474.6313710e0_rp
    !! multiplication factor to convert atomic units of energy (hartree) to wavenumbers (inverse centimeters)
  real(rp), parameter :: au2K = 3.1577465e5_rp
    !! multiplication factor to convert atomic units of energy (hartree) to Kelvin using Boltzmann's constant
  real(rp), parameter :: au2ev = 27.2113834e0_rp
    !! multiplication factor to convert atomic units of energy (hartree) to electron Volts
  real(rp), parameter :: au2ang = 0.5291772083_rp
    !! multiplication factor to convert atomic units of distance (bohr) to Ångstroms (1e-10 m)
  real(rp), parameter :: au2cm = au2ang * 1e-8_rp
    !! multiplication factor to convert atomic units of distance (bohr) to centimeters
  real(rp), parameter :: au2amu = 5.4858010860603975e-4_rp
    !! multiplication factor to convert atomic units of mass (electron mass = 1) to atomic mass units (Daltons)
  real(rp), parameter :: au2s = 2.4188843e-17_rp
    !! multiplication factor to convert atomic units of time (hbar / hartree) to seconds
  real(rp), parameter :: au2ps = 2.4189e-05_rp
    !! multiplication factor to convert atomic units of time (hbar / hartree) to picoseconds (1e-12 s)
  real(rp), parameter :: au2Ghz = Ryd*30._rp
    !! multiplication factor to convert atomic units of energy (hartree) to GHz via c = νλ
  real(rp), parameter :: xs2rate = 59309707.89761206_rp
    !! multiplication factor to convert atomic units of area (bohr^2) to units of a rate coefficient (bohr^3 / time)
  real(rp), parameter :: c = 137.035999084_rp
    !! speed of light in a vacuum in atomic units (bohr * hartree / hbar)
  ! real(rp), parameter :: alpha_au_to_SI  = au_cm**3._rp / au_to_sec
    !! multiplication factor to convert atomic units of rate coefficients (bohr^3 / time) to cm^3 / s
  real(rp), parameter :: rad2deg = 180._rp / pi
    !! multiplication factor to convert radians to degrees
  real(rp), parameter :: deg2rad = pi / 180._rp
    !! multiplication factor to convert degrees to radians
  real(rrp), parameter :: euler_mascheroni = 0.57721566490153286060651209008240243104215933593992_rrp
    !! the Euler Mascheroni constant. The difference between the harmonic series and the natural logarithm

! ===================================================================================================!
end module constants
! ===================================================================================================!
