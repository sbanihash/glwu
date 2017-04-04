!/ ------------------------------------------------------------------- /
	PROGRAM CONVERT_NDFD
!/
!$$$ MAIN PROGRAM DOCUMENTATION BLOCK
!
! MAIN PROGRAM: CONVERT_NDFD	input preprocessor for WAVEWATCH III preprocessor
! PRGMMR: ARUN CHAWLA	DATE:2007-12-07
!
! ABSTRACT: Aim of this code is to take the output from ndfd grib2 files that have been 
!           decoded by wgrib2 using the spread sheet function and get them into an 
!           acceptable format for WW3_PREP
!
! PROGRAM HISTORY LOG:
! 2007-12-07 : Origination
! 2012-06-20 : Modified to include air-sea temp diff placeholder (JH ALVES)
! 2013-09-17 : adjusted to 2km NDFD grids (JH ALVES)
! 2013-09-21 : Made grid adjustable to grib2 files using ALLOCATE (JH ALVES)
!
! USAGE:
!
! INPUT FILE: 
!   UNIT 11  - INPUT WIND SPEED DATA (wspd.txt)
!   UNIT 12 -  INPUT WIND DIRECTION DATA (wdir.txt)
!
! OUTPUT FILES:
!   UNIT 51 - LAT LON FILE (ll.dat)
!   UNIT 52 - DATA FILE (wdata.dat)
!   UNIT 53 - MASK FILE (wmask.dat)
!
! SUBPROGRAMS CALLED:
! UNIQUE:   - TICK21
!
!! ATTRIBUTES:
!   LANGUAGE: FORTRAN 90, single threaded
!   MACHINE:  Portable code
!
!$$$
! 1. Purpose
!    This program takes the decoded wgrib2 data (that has been decoded using the spread sheet option)
!    and writes out a lat-lon data file and a u,v data file in a form that can be used by ww3_prep to
!    generate a wind.ww3 file
!
! 2, Parameters
!    Local variables
!   ------------------------------------------------------------------
!	NXL	Int	Number of variables along longitudes
!	NYL	Int 	Number of values along latitude
!	WND1	R.A.	Wind Speed
!	WND2	R.A.	Wind direction
!	LON	R.A.	Longitude
!	LAT	R.A.	Latitude
!	TIME 	I.A.	Time in an integer array of size 2
!	DTIME	Real	Time increment in seconds
!   ------------------------------------------------------------------
!
! 3. Subroutines used
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!	TICK21	Subr.	W3TIMEMD	Advance time field
!     ----------------------------------------------------------------
!
! 4. Source code
!
	USE W3TIMEMD, ONLY: TICK21
	USE CONSTANTS
!
	IMPLICIT NONE
!
!/--------------------------------------------------------------------/
!	LOCAL PARAMETERS
!
	INTEGER		NXL,NYL,TIME(2),STIME(2)
!
!       PARAMETER	( NXL = 2145 )
!       PARAMETER	( NYL = 1377 )
!
	REAL, ALLOCATABLE :: LON(:,:), LAT(:,:), WND1(:,:), &
			     WND2(:,:), U(:,:), V(:,:), &
                             DTEMP(:,:)
        REAL            DTIME
	LOGICAL		WRITELL 
	INTEGER, ALLOCATABLE :: MASK(:,:)
	INTEGER		I,J, NCOUNT, ICOUNT
        CHARACTER         :: DIRUVC*3
!
!/--------------------------------------------------------------------/

!
	READ(*,*) NCOUNT, DIRUVC
                SELECT CASE(TRIM(DIRUVC(1:3)))
                  CASE('UVC')
	OPEN(UNIT=11,FILE='ugrd.txt',STATUS='OLD')
	OPEN(UNIT=12,FILE='vgrd.txt',STATUS='OLD')
                  CASE('DIR')
	OPEN(UNIT=11,FILE='wspd.txt',STATUS='OLD')
	OPEN(UNIT=12,FILE='wdir.txt',STATUS='OLD')
                END SELECT
	OPEN(UNIT=13,FILE='stamp.txt',STATUS='OLD')
	OPEN(UNIT=14,FILE='dimgrid.txt',STATUS='OLD')
	OPEN(UNIT=50,FILE='lat.dat',STATUS='NEW')
	OPEN(UNIT=51,FILE='lon.dat',STATUS='NEW')
	OPEN(UNIT=52,FILE='wmask.dat',STATUS='NEW')
	OPEN(UNIT=53,FILE='udata.dat',STATUS='NEW')
	OPEN(UNIT=54,FILE='vdata.dat',STATUS='NEW')
	OPEN(UNIT=55,FILE='tdata.dat',STATUS='NEW')
!
	WRITELL = .TRUE.

        DO ICOUNT = 1,NCOUNT
!	
	   READ(13,*,END=800)STIME(1),STIME(2)
	   TIME(1) = STIME(1)/100
	   TIME(2) = 0
           DTIME = (STIME(1)-TIME(1)*100+STIME(2))*3600
	   CALL TICK21(TIME,DTIME)
!
	   READ(14,*)NXL,NYL
           ALLOCATE ( LON(NXL,NYL), LAT(NXL,NYL), WND1(NXL,NYL), &
                             WND2(NXL,NYL), U(NXL,NYL), V(NXL,NYL), &
                             DTEMP(NXL,NYL), MASK(NXL,NYL) )

           READ(11,*)
           READ(12,*)
!
           DO J = 1,NYL
	      DO I = 1,NXL
		READ(11,*)LON(I,J),LAT(I,J),WND1(I,J)
		READ(12,*)LON(I,J),LAT(I,J),WND2(I,J)
              ENDDO
           ENDDO
           WRITE(*,*)'END READ'

           SELECT CASE(TRIM(DIRUVC(1:3)))     

           CASE('UVC')
             V(:,:) = WND2(:,:)
             U(:,:) = WND1(:,:)

           CASE('DIR')
             V(:,:) = -WND1*COS(DERA*WND2(:,:))
             U(:,:) = -WND1*SIN(DERA*WND2(:,:))

          END SELECT

           DTEMP(:,:) = 0.
           MASK(:,:) = 1

           WHERE (WND1 > 9000.)
             V = 9999.
             U = 9999.
             DTEMP = 9999.
             MASK = 0
           END WHERE                   

           WRITE(*,*)'END CONVERT'
!   
           WRITE(*,*)TIME
	   WRITE(53,*)TIME
	   
           DO J = 1,NYL
             IF (WRITELL) THEN
	       WRITE(50,*)(LAT(I,J),I=1,NXL)
	       WRITE(51,*)(LON(I,J)-360.,I=1,NXL)
	       WRITE(52,*)(MASK(I,J),I=1,NXL)
	     END IF
	     WRITE(53,*)(U(I,J),I=1,NXL)
	     WRITE(54,*)(V(I,J),I=1,NXL)
             WRITE(55,*)(DTEMP(I,J),I=1,NXL)
           END DO
           WRITELL = .FALSE.
        END DO
!
  800   CONTINUE
	CLOSE(11)
	CLOSE(12)
	CLOSE(13)
	CLOSE(50)
	CLOSE(51)
	CLOSE(52)
	CLOSE(53)
	CLOSE(54)
	CLOSE(55)
!
	END PROGRAM CONVERT_NDFD 
