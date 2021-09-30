#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      PROGRAM WAVESYSTRK
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |     A. J. van der Westhuysen      |
!/                  |            Jeff Hanson            |
!/                  |        Eve-Marie Devaliere        |
!/                  |                        FORTRAN 95 |
!/                  | Last update :         11-Feb-2014 |
!/                  +-----------------------------------+
!/
!/    03-Feb-2012 : Origination, based on Matlab code   ( version 4.05 )
!/                  by Jeff Hanson & Eve-Marie Devaliere
!/    04-Jan-2013 : Inclusion in trunk                  ( version 4.08 )
!/    29-Nov-2013 : Remove DOC control characters,
!/                  update MPI! to MPI/! (H.L. Tolman). ( version 4.15 )
!/    11-Feb-2014 : Add NetCDF output option. Both NetCDF-3 and
!/                  NetCDF-4 are available. (B. Li).    ( version 4.18 )
!/    26-Sep-2016 : Optimization updates (A. van der Westhuysen)
!/                                                      ( version 5.15 )
!/
!/    Copyright 2009-2013 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
      USE W3STRKMD
      IMPLICIT NONE
!
!  1. Purpose :
!
!     Perform spatial and temporal tracking of wave systems, based
!     on spectral partition (bulletin) output.
!
!  2. Method :
!
!     This is a controller program. It reads the input parameter file
!     wavesystrk.inp and calls subroutine waveTracking_NWS_V2 to
!     perform the actual tracking procedure. Write output (fields and
!     point output).
!
!  3. Parameters :
!
      LOGICAL      :: testout
      PARAMETER (testout = .FALSE.)
      CHARACTER    :: filename*80, paramFile*32
      REAL         :: dirKnob, perKnob, hsKnob, wetPts, seedLat, &
                      seedLon, dirTimeKnob, tpTimeKnob, tint
      REAL         :: lonout(100), latout(100)                            !Increase dimension?
      INTEGER      :: maxGroup, ntint, noutp
      TYPE(dat2d), POINTER :: wsdat(:)
      TYPE(timsys), POINTER :: sysA(:)
      INTEGER, POINTER :: maxSys(:)
!
!     Local parameters.
!     ----------------------------------------------------------------
!     intype         Int       input  Type of input (0 = from memory; 1 = from file)
!     tmax           Int       input  Value of maxTs to apply (1 or 2, used for model coupling)
!     tcur           Int       input  Index of current time step (1 or 2, used for model coupling)
!     ulimGroup      Int       input  Upper limit of number of wave systems to output
!
      LOGICAL           :: file_exists
      CHARACTER         :: inpstr*72,prtyear*4,prtmonth*2,prtday*2, &
                           prthr*2,prtmin*2,prtsec*2
      INTEGER           :: intype, tmax, tcur, maxI, maxJ
      INTEGER           :: it, igrp, sysmatch, ind, ip
      INTEGER           :: counter, i, j, maxTs, leng
      INTEGER           :: dumdate1, dumdate2, dumdate3, dumdate4
      INTEGER           :: dumdate5, dumdate6
      INTEGER           :: ulimGroup
      REAL, ALLOCATABLE :: dum(:,:)
      INTEGER NTIME_NC
      INTEGER           :: outputType
      LOGICAL           :: outputCheck1,outputCheck2
      DOUBLE PRECISION  :: date1, date2, tstart, tend
      REAL              :: dlon, dlat, lonprt, latprt
      REAL              :: dt
      REAL              :: minlon, maxlon, minlat, maxlat
      INTEGER           :: mxcwt, mycwt
 
!     For point output (bilinear interpolation)
      REAL :: hsprt(10),tpprt(10),dirprt(10)
      REAL :: BL_hsprt(10),BR_hsprt(10),TR_hsprt(10),TL_hsprt(10), &
              BL_tpprt(10),BR_tpprt(10),TR_tpprt(10),TL_tpprt(10), &
              BL_dirprt(10),BR_dirprt(10),TR_dirprt(10),TL_dirprt(10)
      REAL :: BL_dirx,BR_dirx,TR_dirx,TL_dirx, &
              BL_diry,BR_diry,TR_diry,TL_diry
      REAL :: BL_lonprt,BR_lonprt,TR_lonprt,TL_lonprt, &
              BL_latprt,BR_latprt,TR_latprt,TL_latprt
      REAL :: t, u, BL_W, BR_W, TR_W, TL_W
      REAL      :: PI
      PARAMETER  (PI = 3.1416)
!
!  4. Subroutines used :
!
!     waveTracking_NWS_V2
!
!  5. Called by :
!
!     None, stand-alone program.
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!     Calls subroutine waveTracking_NWS_V2 in trackmd.95 - see that
!     file for structure.
!
!  9. Switches :
!
!       !/SHRD  Switch for shared / distributed memory architecture.
!       !/MPI   Id.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!
!     Open log file
      OPEN(unit=20,file='sys_log.ww3',status='unknown')
 
!     Print code version
         WRITE(6,900)
      WRITE(20,900)
  900 FORMAT (/15X,'    *** WAVEWATCH III Wave system tracking ***  '/ &
               15X,'==============================================='/)
 
!     Since this program reads the raw partitioning input from file,
!     we set intype=1 or 2, and tmax and tcur to dummy values (not used).
      intype = 2
!      intype = 1
      IF (intype.EQ.1) WRITE(6,*) &
         '*** WARNING: partRes format input used!'
      tmax = 0
      tcur = 0
 
!     Read input parameter file
      INQUIRE(FILE='wavesystrk.inp', EXIST=file_exists)
      IF (.NOT.file_exists) THEN
         WRITE(20,2000)
         WRITE(6,2000)
         CALL ABORT
      END IF
      OPEN(unit=10,file='wavesystrk.inp',status='old')
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) filename
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,'(2X,F8.0,X,F6.0,F6.0,I5)') date1, date2, dt, ntint
      tstart = date1 + date2/1000000
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) outputType
 
      !Check for correct outputType option:
      IF (outputType.EQ.1) THEN
         !ASCII output
      ELSEIF (outputType.EQ.3) THEN
         !NetCDF 3 - requrires !/TRKNC switch
         outputCheck1 = .TRUE.
         IF(outputCheck1)  THEN
            WRITE(6,993)
            STOP
         END IF
      ELSEIF (outputType.EQ.4) THEN
         !NetCDF 4 - requrires !/TRKNC and !/NC4 switch
         outputCheck1 = .TRUE.
         outputCheck2 = .TRUE.
       outputCheck2 = .FALSE.
         IF(outputCheck1.OR.outputCheck2) THEN
            WRITE(6,994)
            STOP
         END IF
      ELSE
         !Not a valid outputType
         WRITE(6,995) outputType
         STOP
      ENDIF
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) minlon, maxlon, mxcwt
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) minlat, maxlat, mycwt
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) dirKnob, perKnob, hsKnob, wetPts, &
                dirTimeKnob, tpTimeKnob
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      READ(10,*) seedLat, seedLon
 
      READ(10,'(A72)') inpstr
      DO WHILE (inpstr(1:1).EQ.'$')
         READ(10,'(A72)') inpstr
      END DO
      BACKSPACE(10)
      noutp = 1
      lonout(:) = 9999.
      latout(:) = 9999.
      DO WHILE (.TRUE.)
         READ(10,*) lonout(noutp),latout(noutp)
         IF ((lonout(noutp).EQ.0.).AND.(latout(noutp).EQ.0.)) EXIT
         noutp = noutp + 1
      END DO
      noutp = noutp - 1
 2000 FORMAT (/' *** WAVEWATCH III ERROR IN WAVESYSTRK : '/    &
               '     ERROR IN OPENING INPUT FILE')
      CLOSE(10)
 
      WRITE(20,*) 'Raw partition file = ',filename
      WRITE(20,'(A,F15.6)') 'Start time = ',tstart
      WRITE(20,*) 'dt = ',dt
      WRITE(20,*) 'No. time levels = ',ntint
      WRITE(20,'(A,2F7.2)') 'Domain limits: Longitude =',minlon, maxlon
      WRITE(20,'(A,2F7.2)') '               Latitude  =',minlat, maxlat
      WRITE(20,*) 'No. increments: Long, Lat  =',mxcwt, mycwt
      WRITE(20,*) 'dirKnob, perKnob, hsKnob, wetPts, &
                dirTimeKnob, tpTimeKnob, seedLat, seedLon ='
      WRITE(20,'(8F6.2)') dirKnob, perKnob, hsKnob, wetPts, &
                dirTimeKnob, tpTimeKnob, seedLat, seedLon
      WRITE(20,*) 'No. output points =',noutp
      DO i = 1,noutp
         WRITE(20,*) lonout(i), latout(i)
      END DO
 
 
 
      CALL waveTracking_NWS_V2 (intype     ,tmax       , &
                                tcur       ,filename   , &
                                tstart     ,tend       , &
                                dt         ,ntint      , &
                                minlon     ,maxlon     , &
                                minlat     ,maxlat     , &
                                mxcwt      ,mycwt      , &
                                dirKnob    ,             &
                                perKnob    ,hsKnob     , &
                                wetPts     ,seedLat    , &
                                seedLon    ,dirTimeKnob, &
                                tpTimeKnob ,paramFile  , &
                                sysA       ,wsdat      , &
                                maxSys     ,maxGroup   )
 
 
!     Set upper limit for wave systems to output (limited by AWIPS display)
      ulimGroup = 9
 
!-----Output systems as plain text----------------------------------------
 
      maxI = SIZE(wsdat(1)%lon,1)
      maxJ = SIZE(wsdat(1)%lon,2)
      dlon = wsdat(1)%lon(2,2)-wsdat(1)%lon(1,1)
      dlat = wsdat(1)%lat(2,2)-wsdat(1)%lat(1,1)
      WRITE(20,*) 'dlon, dlat =',dlon,dlat
 
!-----Final SYSTEM output: Coordinates
      OPEN(unit=21,file='sys_coord.ww3', status='unknown')
 
      WRITE(21,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(21,'(I6,69X,A)') maxI,'Number of cols'
 
      WRITE(21,*) 'Longitude ='
      DO j = maxJ,1,-1
         DO i = 1,maxI
            WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lon(i,j)
         END DO
         WRITE(21,'(A)',ADVANCE='YES') ''
      END DO
 
      WRITE(21,*) 'Latitude = '
      DO j = maxJ,1,-1
         DO i = 1,maxI
            WRITE(21,'(F7.2)',ADVANCE='NO') wsdat(1)%lat(i,j)
         END DO
         WRITE(21,'(A)',ADVANCE='YES') ''
      END DO
 
      CLOSE(21)
 
!-----Final SYSTEM output: hs
      IF(outputType == 1) THEN
      OPEN(unit=22,file='sys_hs.ww3', status='unknown')
 
      WRITE(22,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(22,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF
 
      NTIME_NC=SIZE(sysA)
      ALLOCATE( dum(maxI,maxJ) )
 
      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup
         IF(outputType == 1) THEN
            WRITE(22,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(22,'(I6,69x,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%hs(ind)
               END DO
            ELSE
               leng = 0
            END IF
 
      IF(outputType == 1) THEN
            WRITE(22,'(I6,69x,A)') igrp,'System number'
            WRITE(22,'(I6,69x,A)') leng,'Number of points in system'
 
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(22,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(22,'(A)',ADVANCE='YES') ''
            END DO
      ELSE
      ENDIF
 
         END DO
      END DO
 
 
      IF(outputType.EQ.1) CLOSE(22)
 
!-----Final SYSTEM output: tp
      IF(outputType == 1) THEN
      OPEN(unit=23,file='sys_tp.ww3',status='unknown')
 
      WRITE(23,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(23,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF
 
      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup
         IF(outputType == 1) THEN
            WRITE(23,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(23,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%tp(ind)
               END DO
            ELSE
               leng = 0
            END IF
 
      IF(outputType == 1) THEN
            WRITE(23,'(I6,69X,A)') igrp,'System number'
            WRITE(23,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(23,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(23,'(A)',ADVANCE='YES') ''
            END DO
      ELSE
 
      ENDIF
 
         END DO
      END DO
 
 
      IF(outputType.EQ.1) CLOSE(23)
 
!-----Final SYSTEM output: dir
      IF(outputType == 1) THEN
      OPEN(unit=24,file='sys_dir.ww3',status='unknown')
 
      WRITE(24,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(24,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF
 
      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to
!        ulimGroup
         IF(outputType == 1) THEN
             WRITE(24,'(F15.6,60x,A)') wsdat(it)%date,'Time'
             WRITE(24,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%dir(ind)
               END DO
            ELSE
               leng = 0
            END IF
 
      IF(outputType == 1) THEN
            WRITE(24,'(I6,69X,A)') igrp,'System number'
            WRITE(24,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(24,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(24,'(A)',ADVANCE='YES') ''
            END DO
      ELSE
      END IF
 
         END DO
      END DO
 
      IF(outputType.EQ.1) CLOSE(24)
 
!-----Final SYSTEM output: dspr
      IF(outputType == 1) THEN
      OPEN(unit=25,file='sys_dspr.ww3',status='unknown')
 
      WRITE(25,'(I6,69X,A)') maxJ,'Number of rows'
      WRITE(25,'(I6,69X,A)') maxI,'Number of cols'
      ENDIF
 
      DO it = 1,SIZE(sysA)
!        Loop through identified groups, limiting the output in file to ulimGroup
         IF(outputType == 1) THEN
            WRITE(25,'(F15.6,60x,A)') wsdat(it)%date,'Time'
            WRITE(25,'(I6,69X,A)') MIN(ulimGroup,maxGroup), &
               'Tot number of systems'
         ENDIF
         DO igrp = 1,MIN(ulimGroup,maxGroup)
            dum(1:maxI,1:maxJ) = 9999.00
!           Find system with this group tag
            sysmatch = 1
            DO WHILE (sysmatch.LE.maxSys(it))
               IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
               sysmatch = sysmatch+1
            END DO
            IF (sysmatch.LE.maxSys(it)) THEN
!              Match found: fill the output matrix with this data
               leng = sysA(it)%sys(sysmatch)%nPoints
               DO ind = 1, leng
                  dum(sysA(it)%sys(sysmatch)%i(ind), &
                      sysA(it)%sys(sysmatch)%j(ind)) = &
                      sysA(it)%sys(sysmatch)%dspr(ind)
               END DO
            ELSE
               leng = 0
            END IF
 
      IF(outputType == 1) THEN
            WRITE(25,'(I6,69X,A)') igrp,'System number'
            WRITE(25,'(I6,69X,A)') leng,'Number of points in system'
            DO J = maxJ,1,-1
               DO i = 1,maxI
                  WRITE(25,'(F8.2)',ADVANCE='NO') dum(i,j)
               END DO
               WRITE(25,'(A)',ADVANCE='YES') ''
            END DO
       ELSE
       ENDIF
 
         END DO
      END DO
 
      IF(outputType.EQ.1) CLOSE(25)
 
      IF (ALLOCATED(DUM)) DEALLOCATE(dum)
 
 
!-----Final SYSTEM output: point output
      IF(outputType == 1) THEN
      OPEN(unit=26,file='sys_pnt.ww3',status='unknown')
      WRITE(26,'(A)') '%'
      WRITE(26,'(A)') '%'
      WRITE(26,'(A)') '% WW3 Wave tracking point output'
      WRITE(26,'(A)') '%'
      WRITE(26,'(10A)') '%       Xp            Yp            ', &
           'HsSY01        HsSY02        HsSY03        HsSY04        ', &
           'HsSY05        HsSY06        HsSY07        HsSY08        ', &
           'HsSY09        HsSY10        ', &
           'TpSY01        TpSY02        TpSY03        TpSY04        ', &
           'TpSY05        TpSY06        TpSY07        TpSY08        ', &
           'TpSY09        TpSY10        ', &
           'DrSY01        DrSY02        DrSY03        DrSY04        ', &
           'DrSY05        DrSY06        DrSY07        DrSY08        ', &
           'DrSY09        DrSY10'
      WRITE(26,'(10A)') '%       [degr]        [degr]        ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]'
      WRITE(26,'(A)') '%'
       ENDIF
 
      DO it = 1,SIZE(sysA)
      IF(outputType == 1) THEN
         WRITE(26,'(A,F15.6)') 'Time : ',wsdat(it)%date
       ENDIF
 
         DO ip = 1,noutp
            hsprt(1:10) = 999.9999
            tpprt(1:10) = 999.9999
            dirprt(1:10) = 999.9999
            lonprt = 999.9999
            latprt = 999.9999
            BL_hsprt(1:10) = 999.9999
            BL_tpprt(1:10) = 999.9999
            BL_dirprt(1:10) = 999.9999
            BR_hsprt(1:10) = 999.9999
            BR_tpprt(1:10) = 999.9999
            BR_dirprt(1:10) = 999.9999
            TL_hsprt(1:10) = 999.9999
            TL_tpprt(1:10) = 999.9999
            TL_dirprt(1:10) = 999.9999
            TR_hsprt(1:10) = 999.9999
            TR_tpprt(1:10) = 999.9999
            TR_dirprt(1:10) = 999.9999
            BL_lonprt = 999.9999
            BL_latprt = 999.9999
            BR_lonprt = 999.9999
            BR_latprt = 999.9999
            TL_lonprt = 999.9999
            TL_latprt = 999.9999
            TR_lonprt = 999.9999
            TR_latprt = 999.9999
            BL_W = 999
            BR_W = 999
            TR_W = 999
            TL_W = 999
 
            DO j = 1, (maxJ-1)
               DO i = 1, (maxI-1)
                  IF ( ( ((lonout(ip).GE. &
                           wsdat(1)%lon(i,j)).AND. &
                          (lonout(ip).LT. &
                           wsdat(1)%lon(i+1,j))).OR. &
                         ((lonout(ip).GT. &
                          wsdat(1)%lon(i,j)).AND. &
                         (lonout(ip).LE. &
                          wsdat(1)%lon(i+1,j))) ).AND. &
                       ( ((latout(ip).GE. &
                          wsdat(1)%lat(i,j)).AND. &
                          (latout(ip).LT. &
                          wsdat(1)%lat(i,j+1))).OR. &
                         ((latout(ip).GT. &
                          wsdat(1)%lat(i,j)).AND. &
                          (latout(ip).LE. &
                          wsdat(1)%lat(i,j+1))) ) ) &
                  THEN
                     BL_lonprt = wsdat(1)%lon(i,j)
                     BL_latprt = wsdat(1)%lat(i,j)
                     BR_lonprt = wsdat(1)%lon(i+1,j)
                     BR_latprt = wsdat(1)%lat(i+1,j)
                     TL_lonprt = wsdat(1)%lon(i,j+1)
                     TL_latprt = wsdat(1)%lat(i,j+1)
                     TR_lonprt = wsdat(1)%lon(i+1,j+1)
                     TR_latprt = wsdat(1)%lat(i+1,j+1)
!                    Compute weights for this point
                     t = (lonout(ip)-BL_lonprt)/(BR_lonprt-BL_lonprt)
                     u = (latout(ip)-BL_latprt)/(TL_latprt-BL_latprt)
                     BL_W = (1-t)*(1-u)
                     BR_W = t*(1-u)
                     TR_W = t*u
                     TL_W = (1-t)*u
!                    Compute output values using weights
                     lonprt = BL_W*BL_lonprt + BR_W*BR_lonprt + &
                              TL_W*TL_lonprt + TR_W*TR_lonprt
                     latprt = BL_W*BL_latprt + BR_W*BR_latprt + &
                              TL_W*TL_latprt + TR_W*TR_latprt
                  END IF
               END DO
            END DO
!           Loop through identified groups, limiting the output in file to 10
            DO igrp = 1,MIN(10,maxGroup)
!              Find system with this group tag
               sysmatch = 1
               DO WHILE (sysmatch.LE.maxSys(it))
                  IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
                  sysmatch = sysmatch+1
               END DO
               IF (sysmatch.LE.maxSys(it)) THEN
!                Match found: fill the output matrix with this data
                 leng = sysA(it)%sys(sysmatch)%nPoints
                 DO ind = 1, leng
!                  Write output point data with bilinear interpolation
                   IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                         BL_lonprt).AND.&
                        (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                         BL_latprt) ) THEN
                      BL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      BL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      BL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              BR_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              BR_latprt)) THEN
                      BR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      BR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      BR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              TL_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              TL_latprt)) THEN
                      TL_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      TL_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      TL_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   ELSE IF ( (sysA(it)%sys(sysmatch)%lon(ind).EQ.&
                              TR_lonprt).AND.&
                             (sysA(it)%sys(sysmatch)%lat(ind).EQ.&
                              TR_latprt)) THEN
                      TR_hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                      TR_tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                      TR_dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   END IF
                 END DO
!                  Compute output value using weights
!                  (only if output point is surrounded by valid points)
                   IF ( (BL_hsprt(igrp).NE.999.9999).AND. &
                        (BR_hsprt(igrp).NE.999.9999).AND. &
                        (TL_hsprt(igrp).NE.999.9999).AND. &
                        (TR_hsprt(igrp).NE.999.9999) ) THEN
                      hsprt(igrp) = BL_W * BL_hsprt(igrp) + &
                                    BR_W * BR_hsprt(igrp) + &
                                    TL_W * TL_hsprt(igrp) + &
                                    TR_W * TR_hsprt(igrp)
                      tpprt(igrp) = BL_W * BL_tpprt(igrp) + &
                                    BR_W * BR_tpprt(igrp) + &
                                    TL_W * TL_tpprt(igrp) + &
                                    TR_W * TR_tpprt(igrp)
                      BL_dirx = COS((270-BL_dirprt(igrp))*PI/180.)
                      BR_dirx = COS((270-BR_dirprt(igrp))*PI/180.)
                      TR_dirx = COS((270-TR_dirprt(igrp))*PI/180.)
                      TL_dirx = COS((270-TL_dirprt(igrp))*PI/180.)
                      BL_diry = SIN((270-BL_dirprt(igrp))*PI/180.)
                      BR_diry = SIN((270-BR_dirprt(igrp))*PI/180.)
                      TR_diry = SIN((270-TR_dirprt(igrp))*PI/180.)
                      TL_diry = SIN((270-TL_dirprt(igrp))*PI/180.)
                      dirprt(igrp)=270 - 180./PI* &
                         ATAN2(BL_W*BL_diry+BR_W*BR_diry+ &
                               TL_W*TL_diry+TR_W*TR_diry, &
                               BL_W*BL_dirx+BR_W*BR_dirx+ &
                               TL_W*TL_dirx+TR_W*TR_dirx)
                      IF (dirprt(igrp).GT.360.) THEN
                         dirprt(igrp) = dirprt(igrp) - 360.
                      END IF
                   ELSE
                      hsprt(igrp) = 999.9999
                      tpprt(igrp) = 999.9999
                      dirprt(igrp) = 999.9999
                   END IF
               END IF
            END DO
      IF(outputType == 1) THEN
            WRITE(26,'(32F14.4)') lonprt,latprt, &
                       hsprt(1:10),tpprt(1:10),dirprt(1:10)
        ENDIF
 
         END DO
      END DO
 
      IF(outputType.EQ.1) CLOSE(26)
 
!-----Final SYSTEM output: point output (Nearest neighbor, as a double check)
      IF (testout) THEN
      OPEN(unit=28,file='sys_pnt_nn.ww3',status='unknown')
      WRITE(28,'(A)') '%'
      WRITE(28,'(A)') '%'
      WRITE(28,'(A)') '% WW3 Wave tracking point output'
      WRITE(28,'(A)') '%'
      WRITE(28,'(10A)') '%       Xp            Yp            ', &
           'HsSY01        HsSY02        HsSY03        HsSY04        ', &
           'HsSY05        HsSY06        HsSY07        HsSY08        ', &
           'HsSY09        HsSY10        ', &
           'TpSY01        TpSY02        TpSY03        TpSY04        ', &
           'TpSY05        TpSY06        TpSY07        TpSY08        ', &
           'TpSY09        TpSY10        ', &
           'DrSY01        DrSY02        DrSY03        DrSY04        ', &
           'DrSY05        DrSY06        DrSY07        DrSY08        ', &
           'DrSY09        DrSY10'
      WRITE(28,'(10A)') '%       [degr]        [degr]        ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           [m]           [m]           ', &
           '[m]           [m]           ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         [sec]         [sec]         ', &
           '[sec]         [sec]         ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]        [degr]        [degr]        ', &
           '[degr]        [degr]'
      WRITE(28,'(A)') '%'
 
      DO it = 1,SIZE(sysA)
         WRITE(28,'(A,F15.6)') 'Time : ',wsdat(it)%date
 
         DO ip = 1,noutp
            hsprt(1:10) = 999.9999
            tpprt(1:10) = 999.9999
            dirprt(1:10) = 999.9999
            lonprt = 999.9999
            latprt = 999.9999
 
            DO j = 1, maxJ
               DO i = 1, maxI
!                 Write nearest nearbor output (no bilinear interpolation)
                  IF ( (lonout(ip).GE. &
                        (wsdat(1)%lon(i,j)-dlon/2)).AND. &
                        (lonout(ip).LT. &
                        (wsdat(1)%lon(i,j)+dlon/2)).AND. &
                        (latout(ip).GE. &
                        (wsdat(1)%lat(i,j)-dlat/2)).AND. &
                        (latout(ip).LT. &
                        (wsdat(1)%lat(i,j)+dlat/2)) ) &
                  THEN
                         lonprt = wsdat(1)%lon(i,j)
                         latprt = wsdat(1)%lat(i,j)
                  END IF
               END DO
            END DO
!           Loop through identified groups, limiting the output in file to 10
            DO igrp = 1,MIN(10,maxGroup)
!              Find system with this group tag
               sysmatch = 1
               DO WHILE (sysmatch.LE.maxSys(it))
                  IF (sysA(it)%sys(sysmatch)%grp.EQ.igrp) EXIT
                  sysmatch = sysmatch+1
               END DO
               IF (sysmatch.LE.maxSys(it)) THEN
!                Match found: fill the output matrix with this data
                 leng = sysA(it)%sys(sysmatch)%nPoints
                 DO ind = 1, leng
!                  Write nearest nearbor output (no bilinear interpolation)
                   IF ( (lonout(ip).GE. &
                        (sysA(it)%sys(sysmatch)%lon(ind)-dlon/2)).AND. &
                        (lonout(ip).LT. &
                        (sysA(it)%sys(sysmatch)%lon(ind)+dlon/2)).AND. &
                        (latout(ip).GE. &
                        (sysA(it)%sys(sysmatch)%lat(ind)-dlat/2)).AND. &
                        (latout(ip).LT. &
                        (sysA(it)%sys(sysmatch)%lat(ind)+dlat/2)) ) &
                      THEN
                         hsprt(igrp) = sysA(it)%sys(sysmatch)%hs(ind)
                         tpprt(igrp) = sysA(it)%sys(sysmatch)%tp(ind)
                         dirprt(igrp) = sysA(it)%sys(sysmatch)%dir(ind)
                   END IF
                 END DO
               END IF
            END DO
            WRITE(28,'(32F14.4)') lonprt,latprt, &
                       hsprt(1:10),tpprt(1:10),dirprt(1:10)
         END DO
      END DO
 
      CLOSE(28)
      END IF
 
!-------------------------------------------------------------------------
 
      WRITE(20,*) 'In wavesystrk: Deallocating wsdat, SysA, maxSys...'
      IF (ASSOCIATED(wsdat)) DEALLOCATE(wsdat)
      IF (ASSOCIATED(sysA)) DEALLOCATE(sysA)
      IF (ASSOCIATED(maxSys)) DEALLOCATE(maxSys)
      CLOSE(20)
 
      WRITE(6,*) '... wavesystrk completed successfully.'
      WRITE(6,999)
 
 
 
  993 FORMAT (/' *** WAVEWATCH III ERROR IN WAVESYSTRK : '/           &
               '     OutputType=3 needs TRKNC switch ')
  994 FORMAT (/' *** WAVEWATCH III ERROR IN WAVESYSTRK : '/           &
               '     OutputType=4 needs TRKNC and NC4 switch ')
  995 FORMAT (/' *** WAVEWATCH III ERROR IN WAVESYSTRK : '/           &
               '     OutputType,',I3,'not valid. Options: 1,3,4')
 
  999 FORMAT (/15X,'End of program '/ &
               15X,'==============================================='/ &
               15X,'     *** WAVEWATCH III Wave system tracking ***  ')
 
      END PROGRAM WAVESYSTRK
