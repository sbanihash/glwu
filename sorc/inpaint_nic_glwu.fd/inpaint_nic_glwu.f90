PROGRAM readnic

IMPLICIT NONE

INTEGER :: NLON,NLAT
INTEGER :: ILON, ILAT
CHARACTER(LEN=20) :: FMT, FMT2
REAL,ALLOCATABLE :: MASK(:,:),CICE(:,:)

OPEN(12,FILE='T_OEBA88_C_KNWC.mask')
OPEN(13,FILE='T_OEBA88_C_KNWC.ice')
OPEN(52,FILE='T_OEBA88_C_KNWC.newice')

READ (13,*)NLON,NLAT

ALLOCATE(CICE(NLON,NLAT),MASK(NLON,NLAT))

DO ILAT=1,NLAT
DO ILON=1,NLON
  READ(12,*)MASK(ILON,ILAT)
  READ(13,*)CICE(ILON,ILAT)
  IF ((CICE(ILON,ILAT).EQ.9.999e+20).OR.(MASK(ILON,ILAT).EQ.-1)) THEN
     CICE(ILON,ILAT)=-1.
  ELSE 
     CICE(ILON,ILAT)=1E-1*CICE(ILON,ILAT)
  ENDIF
ENDDO
ENDDO

CALL INPAINT(NLON,NLAT,CICE)

DO ILAT=1,NLAT
DO ILON=1,NLON
  WRITE(FMT2,'("(",I0,"F6.2)")') 1
! Unstructured grid does not yet use transparencies for ico
! (eg, FLAGTR=4 in WW3). Setting here lower ice bound to 40%
! Anything > 25% is totally blocked.
  IF(CICE(ILON,ILAT).LT.0.70)THEN
    CICE(ILON,ILAT)=0.
  ELSE
    CICE(ILON,ILAT)=0.95
  ENDIF 
  WRITE(52,FMT2)MAX(0.,CICE(ILON,ILAT))
ENDDO
ENDDO

END

! Subroutine : inpaint
! Calling Functions : meanundef
! Author Stelios Flampouris
! v0.5.prttp
! 11.04.2015
! TODO : It needs a _main_
! 
! Variables
! xlen :: length of x-dim (e.g longtitude)
! ylen :: length of y-dim (e.g latitude)
! gap :: 2D-array(lenght(x),length(y)) integer, 0 : where must have a value, 1 : where must not have a value
! arr :: 2D-array(lenght(x),length(y)) with data which has the gaps. The gaps have NaN as values
!
subroutine inpaint(xlen, ylen, arr)
implicit none
!
integer :: xlen,ylen
real, dimension(xlen,ylen), intent(inout) :: arr
integer, parameter :: it_max = 10
! 
!local
real, dimension(xlen,ylen) :: arrNaN
real, dimension(2,2) :: subarr
logical, dimension(xlen,ylen) :: indNaN
integer :: ix, iy, indx_min, indx_max, indy_min, indy_max, ext_it
integer :: strt_ix, end_ix, strt_iy, end_iy,stp_ix, stp_iy
real :: UNDEF, max_val, Avg
!
! 1.Basic Input Manipulation
! CLMSK
max_val=maxval(arr)
!
arrNaN = arr
UNDEF=-1
indNaN = arrNaN.eq.UNDEF
!
do ix = 1,xlen,1
   do iy = 1,ylen,1
      if (indNaN(ix,iy)) then
         arrNaN(ix,iy) = UNDEF
         arr (ix,iy) = UNDEF
      endif
   enddo
enddo

! 2. The hoop 
do ext_it = 1,it_max,1
   do ix = 1,xlen,1
      do iy = 1,ylen,1
         indx_min = max(ix-1,1)
         indy_min = max(iy-1,1)
         indx_max = min(ix+1,xlen)
         indy_max = min(iy+1,ylen)

         if (arr(ix,iy).eq.UNDEF) then
             subarr=arrNaN(indx_min:indx_max,indy_min:indy_max)
           if ( any(subarr .ne. UNDEF) ) then
             arr(ix,iy) = meanundef(arrNaN(indx_min:indx_max,indy_min:indy_max), UNDEF)
           end if
         end if
      end do
   end do
   arrNaN = arr
end do

contains

function meanundef(array,UNDEF)
real :: meanundef
real, intent(in), dimension(:,:) :: array
real, intent(in) :: UNDEF
if ( any(array .ne. UNDEF) ) then
  meanundef = sum (array, array .ne. UNDEF) / max(1,count(array .ne. UNDEF ))
else
  meanundef = UNDEF
endif
end function meanundef
!
end subroutine inpaint

