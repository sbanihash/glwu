#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3TIMEMD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         23-Sep-2012 |
!/                  +-----------------------------------+
!/
!/    Copyright 2009 National Weather Service (NWS),
!/       National Oceanic and Atmospheric Administration.  All rights
!/       reserved.  WAVEWATCH III is a trademark of the NWS.
!/       No unauthorized use without permission.
!/
!  1. Purpose :
!
!     Routines for management of date and time.
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      PRFTB     I.A.  Private  Base time for profiling.
!      FLPROF    Log.  Private  Flag for profiling initialization.
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      TICK21    Subr. Public   Increment a date and time array with
!                               a given number of seconds.
!      IYMD21    I.F.  TICK21   Date increment function.
!      DSEC21    R.F.  Public   Calculate the difference in seconds
!                               between two data/time arrays.
!      TDIFF     R.F.  Public   Calculate the difference in seconds
!                               between two date/time arrays that
!                               were generated from DATE_AND_TIME
!      MYMD21    I.F.  DSEC21   Julian date function.
!      STME21    Subr. Public   Converts integer time to string.
!      JULDAY    I.F.  Public   Julian date function
!      CALDAT    Subr. Public   Transform Julian day to date
!      PRINIT    Subr. Public   Initialize profiling.
!      PRTIME    Subr. Public   Get profiling time.
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!  7. Source code :
!
!/ ------------------------------------------------------------------- /
!/
!
      PUBLIC
!
      INTEGER, PRIVATE        :: PRFTB(8)
      LOGICAL, PRIVATE        :: FLPROF = .FALSE.
!
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE TICK21 ( TIME, DTIME )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Nov-1999 |
!/                  +-----------------------------------+
!/                                Based on TICK of the GLA GCM.
!/
!/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Updates time information, DTIME=0 converts to "legal" time.
!     Goes into the 21st century.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
!                          (2) Current time in HHMMSS format.
!       DTIME   Real   I   Time step in seconds.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      IYMD21    Func. Internal Increment date in YYYYMMDD format.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any other routine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(INOUT)  :: TIME(2)
      REAL, INTENT(IN)        :: DTIME
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NYMD, NHMS, NSEC
!/
!/ ------------------------------------------------------------------- /
!/
!
! Zero increment: get "legal" date
!
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        END IF
!
! Convert and increment time :
!
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +        &
             MOD(NHMS,100) + NINT(DTIME)
!
! Check change of date :
!
  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        END IF
!
  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        END IF
!
      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
!
      TIME(1) = NYMD
      TIME(2) = NHMS
!
      RETURN
!/
!/ Internal function IYMD21 ------------------------------------------ /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION IYMD21 ( NYMD ,M )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Nov-1999 |
!/                  +-----------------------------------+
!/                                Based on INCYMD of the GLA GCM.
!/
!/    18-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Increment date in YYYYMMDD format by +/- 1 day.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Old date in YYMMDD format.
!       M       Int.   I   +/- 1 (Day adjustment)
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NYMD, M
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY, NM, ND
      INTEGER, SAVE           :: NDPM(12)
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      LOGICAL                 :: LEAP
!/
!/ ------------------------------------------------------------------- /
!/
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.                              &
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! M = -1, change month if necessary :
!
      IF (ND.EQ.0) THEN
          NM   = NM - 1
          IF (NM.EQ.0) THEN
              NM   = 12
              NY   = NY - 1
            ENDIF
          ND   = NDPM(NM)
          IF (NM.EQ.2 .AND. LEAP)  ND = 29
        END IF
!
! M = 1, leap year
!
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
!
!        next month
!
      IF (ND.GT.NDPM(NM)) THEN
          ND = 1
          NM = NM + 1
          IF (NM.GT.12) THEN
              NM = 1
              NY = NY + 1
          ENDIF
        END IF
!
   20 CONTINUE
      IYMD21 = NY*10000 + NM*100 + ND
!
      RETURN
!/
!/ End of IYMD21 ----------------------------------------------------- /
!/
      END FUNCTION IYMD21
!/
!/ End of TICK21 ----------------------------------------------------- /
!/
      END SUBROUTINE TICK21
!/ ------------------------------------------------------------------- /
      REAL FUNCTION DSEC21 ( TIME1, TIME2 )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         05-Jan-2001 |
!/                  +-----------------------------------+
!/
!/    23-Mar-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/    05-Jan-2001 : Y2K leap year error correction.     ( version 2.05 )
!/
!/
!  1. Purpose :
!
!     Calculate the time difference in seconds between two times in
!     YYMMD HHMMMSS formats.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
!                          TIMEn(2) is time in HHMMSS format.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      MYMD21    Func. Internal Calculate Julian date.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any routine.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: TIME1(2), TIME2(2)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY1, ND1, NY2, ND2, NS1, NS2, NS,   &
                                 ND, NST
!/
!/ ------------------------------------------------------------------- /
!/
!
! Convert dates and times :
!
      NY1    = TIME1(1) / 10000
      ND1    = MYMD21 ( TIME1(1) )
      NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 + &
               MOD(TIME1(2),100)
!
      NY2    = TIME2(1) / 10000
      ND2    = MYMD21 ( TIME2(1) )
      NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 + &
               MOD(TIME2(2),100)
!
! Number of days and seconds in difference :
!
      ND     = ND2 - ND1
!
      IF ( NY1 .NE. NY2 ) THEN
          NST    = SIGN ( 1 , NY2-NY1 )
  100     CONTINUE
          IF (NY1.EQ.NY2) GOTO 200
          IF (NST.GT.0) THEN
              NY2    = NY2 - 1
              ND     = ND  + MYMD21 ( NY2*10000 + 1231 )
            ELSE
              ND     = ND  - MYMD21 ( NY2*10000 + 1231 )
              NY2    = NY2 + 1
            ENDIF
          GOTO 100
  200     CONTINUE
        END IF
!
      NS     = NS2 - NS1
!
! Output of time difference :
!
      DSEC21 = REAL(NS) + 86400.*REAL(ND)
!
      RETURN
!/
!/ Internal function MYMD21 ------------------------------------------ /
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION MYMD21 ( NYMD )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         29-Nov-1999 |
!/                  +-----------------------------------+
!/                                Based on MODYMD of the GLA GCM.
!/
!/    19-Oct-1998 : Final FORTRAN 77                    ( version 1.18 )
!/    29-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Convert date in YYMMDD format to julian date.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       NYMD    Int.   I   Date in YYMMDD format.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any subroutine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: NYMD
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NY, NM, ND
      INTEGER, SAVE           :: NDPM(12)
      DATA    NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
      LOGICAL                 :: LEAP
!/
!/ ------------------------------------------------------------------- /
!/
!
! "Unpack" and increment date :
!
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      ND   = MOD(NYMD,100)
      LEAP = MOD(NY,400).EQ.0 .OR.                              &
              ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
!
! Loop over months :
!
      IF (NM.GT.2 .AND. LEAP)  ND = ND + 1
!
   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      GO TO 40
!
   60 CONTINUE
      MYMD21 = ND
!
      RETURN
!/
!/ End of MYMD21 ----------------------------------------------------- /
!/
      END FUNCTION MYMD21
!/
!/ End of DSEC21 ----------------------------------------------------- /
!/
      END FUNCTION DSEC21
!/ ------------------------------------------------------------------- /
      REAL FUNCTION TDIFF ( T1, T2 )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           Arun Chawla             |
!/                  |           Mark Szyszka            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         02-Feb-2014 |
!/                  +-----------------------------------+
!/
!/    02-Feb-2014 : Original code         ( version 4.18 )
!/
!/
!  1. Purpose :
!
!     Calculate the time difference in seconds between two time arrays
!     that have been generated from the F90 internal function
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       Tn      I.A.   I   This is an integer array returned from the
!                          internal subroutine DATE_AND_TIME. The type
!                          is integer(8). Individual values are
!                          Tn(1)    the year
!                          Tn(2)    the month
!                          Tn(3)    day of the month
!                          Tn(4)    time difference with UTC in minutes
!                          Tn(5)    hour of the day
!                          Tn(6)    minutes of the hour
!                          Tn(7)    seconds of the minute
!                          Tn(8)    milli seconds of the second
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any routine.
!
!  7. Remarks :
!
!     This code has been provided by Mark Szyszka of RPSGROUP
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: T1(8), T2(8)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: A1, B1, C1, D1, A2, B2, C2, D2
      REAL                    :: E1, E2
!/
!/ ------------------------------------------------------------------- /
!/
!
! Convert dates and times :
!
      A1 = (14-T1(2))/12
      B1 = T1(1) + 4800 - A1
      C1 = T1(2) + 12*A1 - 3
      D1 = T1(3) + (153*C1 + 2)/5 + 365*B1 + B1/4 -B1/100 + B1/400
      E1 = 3600.0*T1(5) + 60.0*(T1(6)-T1(4)) + T1(7) + T1(8)/1000.0
!
      A2 = (14-T2(2))/12
      B2 = T2(1) + 4800 - A2
      C2 = T2(2) + 12*A2 - 3
      D2 = T2(3) + (153*C2 + 2)/5 + 365*B2 + B2/4 -B2/100 + B2/400
      E2 = 3600.0*T2(5) + 60.0*(T2(6)-T2(4)) + T2(7) + T2(8)/1000.0
!
      TDIFF = 86400.0*(D2-D1) + E2-E1
!
      RETURN
!/
!/ End of TDIFF ------------------------------------------------------ /
!/
      END FUNCTION TDIFF
!/ ------------------------------------------------------------------- /
      SUBROUTINE STME21 ( TIME , DTME21 )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         23-Nov-1999 |
!/                  +-----------------------------------+
!/
!/    21-Jun-1993 : Final FORTRAN 77                    ( version 1.18 )
!/    23-Nov-1999 : Upgrade to FORTRAN 90               ( version 2.00 )
!/
!  1. Purpose :
!
!     Converts time to more readable string.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I   Time in YYYYMMDD HHMMSS format.
!                         TIME(1) < 0 indicates that time is not set.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!       None.
!
!  5. Called by :
!
!       Any subroutine/program.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(IN)     :: TIME(2)
      CHARACTER, INTENT(OUT)  :: DTME21*23
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: IY, IMO, ID, IH, IMI, IS
!/
!/ ------------------------------------------------------------------- /
!/
      IF ( TIME(1) .LT. 0 ) THEN
          DTME21 = ' date and time not set.'
        ELSE
          IY     = TIME(1) / 10000
          IMO    = MOD(TIME(1),10000) / 100
          ID     = MOD(TIME(1),100)
          IH     = TIME(2) / 10000
          IMI    = MOD(TIME(2),10000) / 100
          IS     = MOD(TIME(2),100)
          WRITE (DTME21,900) IY, IMO, ID, IH, IMI, IS
        ENDIF
!
      RETURN
!
! Formats
!
  900 FORMAT (I4.4,'/',I2.2,'/',I2.2,' ',I2.2,':',I2.2,':',I2.2,' UTC')
!/
!/ End of STME21 ----------------------------------------------------- /
!/
      END SUBROUTINE STME21
 
!/ ------------------------------------------------------------------- /
      INTEGER FUNCTION JULDAY(id,mm,iyyy)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           F. Ardhuin              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         23-Sep-2012 |
!/                  +-----------------------------------+
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
      INTEGER(KIND=4),    INTENT(in)  :: id,mm,iyyy
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
     INTEGER(KIND=4), PARAMETER :: IGREG=15+31*(10+12*1582)
     INTEGER(KIND=4) ja,jm,jy
     jy=iyyy
     IF (jy.EQ.0) WRITE(6,*) 'There is no zero year !!'
     IF (jy.LT.0) jy=jy+1
     IF (mm.GT.2) THEN
       jm=mm+1
     ELSE
       jy=jy-1
       jm=mm+13
       ENDIF
     julday=INT(365.25*jy)+int(30.6001*jm)+id+1720995
     IF (id+31*(mm+12*iyyy).GE.IGREG) THEN
       ja=INT(0.01*jy)
       julday=julday+2-ja+INT(0.25*ja)
       END IF
     RETURN
!/
!/ End of JULDAY ----------------------------------------------------- /
!/
     END FUNCTION JULDAY
 
!/ ------------------------------------------------------------------- /
      SUBROUTINE CALDAT(julian,id,mm,iyyy)
! See numerical recipes 2nd ed. The order of month and day have been swapped!
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
     INTEGER(KIND=4),    INTENT(in)  :: julian
     INTEGER(KIND=4),    INTENT(out) :: id,mm,iyyy
     INTEGER(KIND=4), PARAMETER :: IGREG=2299161
     INTEGER(KIND=4) ja,jalpha,jb,jc,jd,je
     if (julian.GE.IGREG) THEN
       jalpha=INT(((julian-1867216)-0.25)/36524.25)
       ja=julian+1+jalpha-INT(0.25*jalpha)
     ELSE
       ja=julian
       END IF
     jb=ja+1524
     jc=INT(6680.+((jb-2439870)-122.1)/365.25)
     jd=365*jc+INT(0.25*jc)
     je=INT((jb-jd)/30.6001)
     id=jb-jd-INT(30.6001*je)
     mm=je-1
     IF (mm.GT.12) mm=mm-12
     iyyy=jc-4715
     IF (mm.GT.2) iyyy=iyyy-1
     IF (iyyy.LE.0) iyyy=iyyy-1
     RETURN
!/
!/ End of CALDAT ----------------------------------------------------- /
!/
     END SUBROUTINE CALDAT
!/ ------------------------------------------------------------------- /
     REAL(KIND=8) FUNCTION  TIME2HOURS(TIME)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           F. Ardhuin              |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         26-Sep-2012 |
!/                  +-----------------------------------+
!
!     gives date as real number
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
!                          (2) Current time in HHMMSS format.
!       DTIME   Real   I   Time step in seconds.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!      IYMD21    Func. Internal Increment date in YYYYMMDD format.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any other routine.
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing using STRACE.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      INTEGER, INTENT(INOUT)  :: TIME(2)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: NYMD, NHMS, NSEC, IY,IMO,ID,IH,IMI,IS
      INTEGER(KIND=4)         :: JDAY
!/
!/ ------------------------------------------------------------------- /
!/
!
! Zero increment: get "legal" date
!
      IY     = TIME(1) / 10000
      IMO    = MOD(TIME(1),10000) / 100
      ID     = MOD(TIME(1),100)
      IH     = TIME(2) / 10000
      IMI    = MOD(TIME(2),10000) / 100
      IS     = MOD(TIME(2),100)
      JDAY    = julday(id,IMO,iy)
      TIME2HOURS = 24.d0*dfloat(JDAY)+dfloat(IH)+dfloat(IS+IMI*60)/3600.d0
      RETURN
!/
!/ End of TIME2HOURS-------------------------------------------------- /
!/
      END FUNCTION TIME2HOURS
!/ ------------------------------------------------------------------- /
      SUBROUTINE PRINIT
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-May-2005 !
!/                  +-----------------------------------+
!/
!/    06-May-2005 : Origination.                        ( version 3.07 )
!/
!  1. Purpose :
!
!     Initialize profiling routine PRTIME.
!
!  2. Method :
!
!     FORTRAN 90 SYSTEM_CLOCK intrinsic routine.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      SYSTEM_CLOCK
!                Sur.    n/a    Get system time ( !/F90 )
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!  6. Error messages :
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/F90  FORTRAN 90 specific calls.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      IMPLICIT NONE
!/
! -------------------------------------------------------------------- /
!
      CALL DATE_AND_TIME ( VALUES=PRFTB )
!
      FLPROF = .TRUE.
!
      RETURN
!/
!/ End of PRINIT ----------------------------------------------------- /
!/
      END SUBROUTINE PRINIT
!/ ------------------------------------------------------------------- /
      SUBROUTINE PRTIME ( PTIME )
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |           H. L. Tolman            |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         06-May-2005 !
!/                  +-----------------------------------+
!/
!/    06-May-2005 : Origination.                        ( version 3.07 )
!/
!  1. Purpose :
!
!     Get wallclock time for profiling purposes.
!
!  2. Method :
!
!     FORTRAN 90 SYSTEM_CLOCK intrinsic routine.
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!       PTIME   Real   O   Time retrieced from system.
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      SYSTEM_CLOCK
!                Sur.    n/a    Get system time ( !/F90 )
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!     Any, after PRINIT has been called.
!
!  6. Error messages :
!
!     - If no initialization, returned time equals -1.
!     - If no system clock, returned time equals -1.
!
!  7. Remarks :
!
!  8. Structure :
!
!  9. Switches :
!
!     !/F90  FORTRAN 90 specific calls.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
!/
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(OUT)       :: PTIME
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER                 :: PRFTA(8)
!
! -------------------------------------------------------------------- /
!
      PTIME  = -1.
!
      IF ( .NOT. FLPROF ) RETURN
!
      CALL DATE_AND_TIME ( VALUES=PRFTA )
      PTIME  = TDIFF ( PRFTB,PRFTA )
!
      RETURN
!/
!/ End of PRTIME ----------------------------------------------------- /
!/
      END SUBROUTINE PRTIME
!/
!/ End of module W3TIMEMD -------------------------------------------- /
!/
      END MODULE W3TIMEMD
