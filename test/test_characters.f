! =================================================================================================================================!
module test_characters

  use types,      only: ip
  use testdrive,  only: new_unittest, unittest_type, error_type, check
  use characters, only: add_trailing, int2char0, csgn, operator(+), ndigits, to_lower, to_upper

  implicit none

  private

  public collect_characters

! =================================================================================================================================!
contains
! =================================================================================================================================!

! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine collect_characters(testsuite)
  !! Collect all exported unit tests

  type(unittest_type), allocatable, intent(out) :: testsuite(:)
    !! Collection of tests to run

  testsuite = [                                                            &
    new_unittest("csgn positive",            test_csgn_pos),               &
    new_unittest("csgn zero",                test_csgn_zero),              &
    new_unittest("csgn negative",            test_csgn_neg),               &
    new_unittest("ndigits 0",                test_ndigits0),               &
    new_unittest("ndigits 1",                test_ndigits1),               &
    new_unittest("ndigits 10",               test_ndigits10),              &
    new_unittest("ndigits -0",               test_ndigits0_minus),         &
    new_unittest("ndigits -1",               test_ndigits1_minus),         &
    new_unittest("ndigits -10",              test_ndigits10_minus),        &
    new_unittest("add_trailing",             test_add_trailing),           &
    new_unittest("add_trailing redundant",   test_add_trailing_redundant), &
    new_unittest("add_trailing (blank)",     test_add_trailing_blank),     &
    new_unittest("to_lower",                 test_to_lower),               &
    new_unittest("to_lower (blank)",         test_to_lower),               &
    new_unittest("to_upper",                 test_to_upper),               &
    new_unittest("to_upper (blank)",         test_to_upper),               &
    new_unittest("char plus",                test_char_plus),              &
    new_unittest("operato(+) (left blank)",  test_char_plus_blank_left),   &
    new_unittest("operato(+) (right blank)", test_char_plus_blank_right),  &
    new_unittest("operato(+) (both blank)",  test_char_plus_blank_double)  &
    ! new_unittest("invalid", test_invalid, should_fail=.true.) &
  ]

end subroutine collect_characters

! ---------------------------------------------------------------------------------------------------------------------------------!
! CSGN TESTS
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_csgn_pos(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, csgn(1_ip), "+")
  if(allocated(error)) return
end subroutine test_csgn_pos
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_csgn_zero(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, csgn(0_ip), "+")
  if(allocated(error)) return
end subroutine test_csgn_zero
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_csgn_neg(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, csgn(-1_ip), "-")
  if(allocated(error)) return
end subroutine test_csgn_neg

! ---------------------------------------------------------------------------------------------------------------------------------!
! OPERATOR + TEST
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_char_plus(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, "a" + "b", "ab")
  if(allocated(error)) return
end subroutine test_char_plus
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_char_plus_blank_right(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, "a" + "", "a")
  if(allocated(error)) return
end subroutine test_char_plus_blank_right
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_char_plus_blank_left(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, "" + "a", "a")
  if(allocated(error)) return
end subroutine test_char_plus_blank_left
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_char_plus_blank_double(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, "" + "a", "a")
  if(allocated(error)) return
end subroutine test_char_plus_blank_double

! ---------------------------------------------------------------------------------------------------------------------------------!
! NDIGITS TESTS
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits0(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(0_ip), 1_ip)
  if(allocated(error)) return
end subroutine test_ndigits0
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits0_minus(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(-0_ip), 1_ip)
  if(allocated(error)) return
end subroutine test_ndigits0_minus
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits1(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(1_ip), 1_ip)
  if(allocated(error)) return
end subroutine test_ndigits1
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits1_minus(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(-1_ip), 2_ip)
  if(allocated(error)) return
end subroutine test_ndigits1_minus
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits10(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(10_ip), 2_ip)
  if(allocated(error)) return
end subroutine test_ndigits10
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_ndigits10_minus(error)
  type(error_type), allocatable, intent(out) :: error
  call check(error, ndigits(-10_ip), 3_ip)
  if(allocated(error)) return
end subroutine test_ndigits10_minus

! ---------------------------------------------------------------------------------------------------------------------------------!
! ADD_TRAILING TESTS
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_add_trailing(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = "test"
  call add_trailing(test, "ing")
  call check(error, test, "testing")
  if(allocated(error)) return
end subroutine test_add_trailing
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_add_trailing_redundant(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = "test"
  call add_trailing(test, "t")
  call check(error, test, "test")
  if(allocated(error)) return
end subroutine test_add_trailing_redundant
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_add_trailing_blank(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = ""
  call add_trailing(test, "t")
  call check(error, test, "")
  if(allocated(error)) return
end subroutine test_add_trailing_blank

! ---------------------------------------------------------------------------------------------------------------------------------!
! TO_LOWER TESTS
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_to_lower(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = "tEsT18(\@%_dD]"
  call to_lower(test)
  call check(error, test, "test18(\@%_dd]")
  if(allocated(error)) return
end subroutine test_to_lower
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_to_lower_blank(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = ""
  call to_lower(test)
  call check(error, test, "")
  if(allocated(error)) return
end subroutine test_to_lower_blank

! ---------------------------------------------------------------------------------------------------------------------------------!
! TO_UPPER TESTS
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_to_upper(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = "tEsT18(\@%_dD]"
  call to_upper(test)
  call check(error, test, "TEST18(\@%_DD]")
  if(allocated(error)) return
end subroutine test_to_upper
! ---------------------------------------------------------------------------------------------------------------------------------!
subroutine test_to_upper_blank(error)
  type(error_type), allocatable, intent(out) :: error
  character(:), allocatable :: test
  test = ""
  call to_upper(test)
  call check(error, test, "")
  if(allocated(error)) return
end subroutine test_to_upper_blank


! =================================================================================================================================!
end module test_characters
! =================================================================================================================================!
