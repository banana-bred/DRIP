! ===================================================================================================!
module arrays
! ===================================================================================================!

use types, only: ip, rp

implicit none

private

public append
public append_uniq
public remove_value
public alloc
public dealloc
public binary_search
public interpolation_search
public swap

save

! ---------------------------------------------------------------------------------------------------!
interface append
  !! Append an element to an array. If not allocated, allocate the array to contain that element
  module procedure append_i
  module procedure append_r
  module procedure append_c
end interface append

! ---------------------------------------------------------------------------------------------------!
interface append_uniq
  !! Append a unique element to an array. If not allocated, allocate the array to contain that element.
  module procedure append_uniq_i
end interface append_uniq

! ---------------------------------------------------------------------------------------------------!
interface remove_value
  !! Remove all matching values from an array
  module procedure remove_value_i
  module procedure remove_value_iarr
end interface remove_value

! ---------------------------------------------------------------------------------------------------!
interface swap
  !! Swap two elements of an array
  module procedure swap_i
  module procedure swap_r
  module procedure swap_c
end interface swap

! ---------------------------------------------------------------------------------------------------!
interface alloc
  !! Allocate an array. If it's allocated, free the array first
  module procedure alloc_1D_int
  module procedure alloc_1D_real
  module procedure alloc_1D_cplx
  module procedure alloc_2D_int
  module procedure alloc_2D_real
  module procedure alloc_2D_cplx
  module procedure alloc_3D_int
  module procedure alloc_3D_real
  module procedure alloc_3D_cplx
end interface alloc

! ---------------------------------------------------------------------------------------------------!
interface dealloc
  !! Deallocate an array. If it isn't allocated, do nothing.
  module procedure dealloc_1D_int
  module procedure dealloc_1D_real
  module procedure dealloc_1D_cplx
  module procedure dealloc_2D_int
  module procedure dealloc_2D_real
  module procedure dealloc_2D_cplx
  module procedure dealloc_3D_int
  module procedure dealloc_3D_real
  module procedure dealloc_3D_cplx
end interface dealloc

! ---------------------------------------------------------------------------------------------------!
interface binary_search
  !! Binary search through a sorted array
  module procedure binary_search_r
end interface binary_search

! ---------------------------------------------------------------------------------------------------!
interface interpolation_search
  !! Interpolation (linear) search through a sorted array
  module procedure interpolation_search_r
end interface interpolation_search

! ===================================================================================================!
contains
! ===================================================================================================!

! ---------------------------------------------------------------------------------------------------!
! APPEND INTERFACE
! ---------------------------------------------------------------------------------------------------!
subroutine append_i(arr, new)
  !! Append element "new" to array "arr"

  implicit none

  integer(ip), intent(in)                 :: new
  integer(ip), intent(inout), allocatable :: arr(:)

  select case(allocated(arr))
  case(.true.)
    arr = [arr, new]
  case(.false.)
    arr = [new]
  end select

end subroutine append_i
! ---------------------------------------------------------------------------------------------------!
subroutine append_r(arr, new)
  !! Append element "new" to array "arr"

  implicit none

  real(rp), intent(in)                 :: new
  real(rp), intent(inout), allocatable :: arr(:)

  select case(allocated(arr))
  case(.true.)
    arr = [arr, new]
  case(.false.)
    arr = [new]
  end select

end subroutine append_r
! ---------------------------------------------------------------------------------------------------!
subroutine append_c(arr, new)
  !! Append element "new" to array "arr"

  implicit none

  complex(rp), intent(in)                 :: new
  complex(rp), intent(inout), allocatable :: arr(:)

  select case(allocated(arr))
  case(.true.)
    arr = [arr, new]
  case(.false.)
    arr = [new]
  end select

end subroutine append_c

! ---------------------------------------------------------------------------------------------------!
! APPEND_UNIQ INTERFACE
! ---------------------------------------------------------------------------------------------------!
subroutine append_uniq_i(arr, new)
  !! Append unique element "new" to array "arr"

  implicit none

  integer(ip), intent(in)                 :: new
  integer(ip), intent(inout), allocatable :: arr(:)

  select case(allocated(arr))
  case(.true.)
    if(any(arr .eq. new)) return
    arr = [arr, new]
  case(.false.)
    arr = [new]
  end select

end subroutine append_uniq_i

! ---------------------------------------------------------------------------------------------------!
! REMOVE_VALUE INTERFACE
! ---------------------------------------------------------------------------------------------------!
subroutine remove_value_i(arr, val)
  !! Remove all matches of "val" from an array "arr"

  implicit none

  integer(ip), intent(inout), allocatable :: arr(:)
  integer(ip), intent(in)  :: val
  integer(ip), allocatable :: tmp(:)

  integer(ip) :: k
  integer(ip) :: l
  integer(ip) :: u

  l = lbound(arr, 1)
  u = ubound(arr, 1)

  if(.not.allocated(arr)) return

  do k = l, u
    if(val .ne. arr(k)) call append(tmp, arr(k))
  enddo

  if(.not.allocated(tmp))  then
    deallocate(arr)
    return
  endif

  arr = tmp

end subroutine remove_value_i
! ---------------------------------------------------------------------------------------------------!
subroutine remove_value_iarr(arr, vals)
  !! Remove all matching values in array "vals" from array "arr"

  implicit none

  integer(ip), intent(inout), allocatable :: arr(:)
  integer(ip), intent(in)  :: vals(:)
  integer(ip), allocatable :: tmp(:)

  integer(ip) :: k
  integer(ip) :: l
  integer(ip) :: u

  l = lbound(arr, 1)
  u = ubound(arr, 1)

  if(.not.allocated(arr)) return

  do k = l, u
    if(.not.any(vals.eq.arr(k))) call append(tmp, arr(k))
  enddo

  if(.not.allocated(tmp))  then
    deallocate(arr)
    return
  endif

  arr = tmp

end subroutine remove_value_iarr

! ---------------------------------------------------------------------------------------------------!
! ALLOC INTERFACE
! ---------------------------------------------------------------------------------------------------!
!  1D
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_1D_int(arr, n)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(inout), allocatable :: arr(:)

  call dealloc(arr)
  allocate( arr(n) )

end subroutine alloc_1D_int
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_1D_real(arr,n)

  implicit none

  integer(ip),  intent(in) :: n
  real(rp), intent(inout), allocatable :: arr(:)

  call dealloc(arr)
  allocate( arr(n) )

end subroutine alloc_1D_real
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_1D_cplx(arr,n)

  implicit none

  integer(ip),     intent(in) :: n
  complex(rp), intent(inout), allocatable :: arr(:)

  call dealloc(arr)
  allocate( arr(n) )

end subroutine alloc_1D_cplx
! ---------------------------------------------------------------------------------------------------!
!  2D
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_2D_int(arr, n, m)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in) :: m
  integer(ip), intent(inout), allocatable :: arr(:,:)

  call dealloc(arr)
  allocate( arr(n, m) )

end subroutine alloc_2D_int
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_2D_real(arr, n, m)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in) :: m
  real(rp), intent(inout), allocatable :: arr(:,:)

  call dealloc(arr)
  allocate( arr(n, m) )

end subroutine alloc_2D_real
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_2D_cplx(arr, n, m)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in):: m
  complex(rp), intent(inout), allocatable :: arr(:,:)

  call dealloc(arr)
  allocate( arr(n, m) )

end subroutine alloc_2D_cplx
! ---------------------------------------------------------------------------------------------------!
!  3D
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_3D_int(arr, n, m, l)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in) :: m
  integer(ip), intent(in) :: l
  integer(ip), intent(inout), allocatable :: arr(:,:,:)

  call dealloc(arr)
  allocate( arr(n, m, l) )

end subroutine alloc_3D_int
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_3D_real(arr, n, m, l)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in) :: m
  integer(ip), intent(in) :: l

  real(rp), intent(inout), allocatable :: arr(:,:,:)
  call dealloc(arr)
  allocate( arr(n, m, l) )

end subroutine alloc_3D_real
! ---------------------------------------------------------------------------------------------------!
subroutine alloc_3D_cplx(arr, n, m, l)

  implicit none

  integer(ip), intent(in) :: n
  integer(ip), intent(in) :: m
  integer(ip), intent(in) :: l
  complex(rp), intent(inout), allocatable :: arr(:,:,:)

  call dealloc(arr)
  allocate( arr(n, m, l) )

end subroutine alloc_3D_cplx

! ---------------------------------------------------------------------------------------------------!
!  DEALLOC INTERFACE
! ---------------------------------------------------------------------------------------------------!
!  1D
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_1D_int(arr)

  implicit none

  integer(ip), intent(inout), allocatable :: arr(:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_1D_int
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_1D_real(arr)

  implicit none

  real(rp), intent(inout), allocatable :: arr(:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_1D_real
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_1D_cplx(arr)

  implicit none

  complex(rp), intent(inout), allocatable :: arr(:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_1D_cplx
! ---------------------------------------------------------------------------------------------------!
!  2D
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_2D_int(arr)

  implicit none

  integer(ip), intent(inout), allocatable :: arr(:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_2D_int
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_2D_real(arr)

  implicit none

  real(rp), intent(inout), allocatable :: arr(:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_2D_real
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_2D_cplx(arr)

  implicit none

  complex(rp), intent(inout), allocatable :: arr(:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_2D_cplx
! ---------------------------------------------------------------------------------------------------!
!  3D
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_3D_int(arr)

  implicit none

  integer(ip), intent(inout), allocatable :: arr(:,:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_3D_int
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_3D_real(arr)

  implicit none

  real(rp), intent(inout), allocatable :: arr(:,:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_3D_real
! ---------------------------------------------------------------------------------------------------!
subroutine dealloc_3D_cplx(arr)

  implicit none

  complex(rp), intent(inout), allocatable :: arr(:,:,:)

  if(allocated(arr)) deallocate(arr)

end subroutine dealloc_3D_cplx

! ---------------------------------------------------------------------------------------------------!
! BINARY_SEARCH INTERFACE
! ---------------------------------------------------------------------------------------------------!
pure function binary_search_r(arr,x) result(m)
  !! Binary search through a sorted array, return index i where arr(i) is closest to x
  implicit none
  real(rp), intent(in) :: arr(:)
  real(rp), intent(in) :: x
  integer(ip) :: m
  integer(ip) :: l,u
  integer(ip) :: ll,uu
  l = lbound(arr,1)
  u = ubound(arr,1)
  if(x.le.arr(l)) then
    m = l
    return
  elseif(x.ge.arr(u)) then
    m = u
    return
  endif
  ll = l
  uu = u
  do while(ll.le.uu)
    m = (ll+uu) / 2
    if(arr(m).lt.x) ll = m + 1
    if(arr(m).gt.x) uu = m - 1
  enddo
end function binary_search_r

! ---------------------------------------------------------------------------------------------------!
! INTERPOLATION_SEARCH INTERFACE
! ---------------------------------------------------------------------------------------------------!
pure function interpolation_search_r(arr,x) result(m)
  ! Interpolation (linear) search through a sorted array, return index i where arr(i) is closest to x
  ! Returns smallest bound if x is below the smallest element
  ! Returns largest  bound if x is above the largest  element

  implicit none

  real(rp), intent(in) :: arr(:)
  real(rp), intent(in) :: x

  integer(ip) :: m
  integer(ip) :: l
  integer(ip) :: u
  integer(ip) :: ll
  integer(ip) :: uu

  l = lbound(arr,1)
  u = ubound(arr,1)

  if(x .le. arr(l)) then

    m = l
    return

  elseif(x .ge. arr(u)) then

    m = u
    return

  endif

  ll = l
  uu = u

  do while ( arr(uu) .ne. arr(ll) .AND. x .ge. arr(ll) .AND. x .le. arr(uu) )

    m = ll + floor( (x - arr(ll)) * (uu - ll) / (arr(uu) - arr(ll)) )

    if(arr(m) .lt. x) then

      ll = m + 1

    elseif(arr(m) .gt. x) then

      uu = m - 1

    else

      return

    endif

  enddo

end function interpolation_search_r

! ---------------------------------------------------------------------------------------------------!
! SWAP INTERFACE
! ---------------------------------------------------------------------------------------------------!
pure function swap_i(v, i, j) result(w)
  !! Swap two elements of an array

  implicit none

  integer(ip), intent(in) :: i
  integer(ip), intent(in) :: j
  integer(ip), intent(in) :: v(:)

  integer(ip) :: w(size(v, 1))

  w = v

  if(i .eq. j) return

  w(i) = v(j)
  w(j) = v(i)

end function swap_i
! ---------------------------------------------------------------------------------------------------!
pure function swap_r(v, i, j) result(w)
  !! Swap two elements of an array

  implicit none

  integer(ip), intent(in) :: i
  integer(ip), intent(in) :: j

  real(rp), intent(in) :: v(:)
  real(rp) :: w(size(v, 1))

  w = v

  if(i .eq. j) return

  w(i) = v(j)
  w(j) = v(i)

end function swap_r
! ---------------------------------------------------------------------------------------------------!
pure function swap_c(v, i, j) result(w)
  !! Swap two elements of an array

  implicit none

  integer(ip), intent(in) :: i
  integer(ip), intent(in) :: j
  complex(rp), intent(in) :: v(:)
  complex(rp) :: w(size(v, 1))

  w = v

  if(i .eq. j) return

  w(i) = v(j)
  w(j) = v(i)

end function swap_c

! ===================================================================================================!
end module arrays
! ===================================================================================================!
