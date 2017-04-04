C/ ------------------------------------------------------------------- /
      SUBROUTINE TICK21 ( TIME, DTIME )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    TICK21      INCREMENT DATE AND TIME
C   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
C
C ABSTRACT: INCREMENT INTEGER TIME(2) IN YYYYMMDD HHMMSS FORMAT BY
C   A GIVEN NUMBER OF SECONDS.
C
C PROGRAM HISTORY LOG:
C   93-03-29  H.L. TOLMAN ORIGINATION
C   98-10-29  H.S. CHEN   ADDING DOCBLOCK
C             H.L. TOLMAN
C
C USAGE: SEE ORIGINAL DOCUMENTATION BELOW.
C
C REMARKS: SEE ORIGINAL DOCUMENTATION BELOW.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77-90
C   MACHINE:  CRAY C90 / IBM RS6000 SP
C
C$$$
C/                  +-----------------------------------+
C/                  | WAVEWATCH-III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         29-Mar-1993 |
C/                  +-----------------------------------+
C/                                Based on TICK of the GLA GCM.
C/
C  1. Purpose :
C
C     Updates time information, DTIME=0 converts to "legal" time.
C     Goes into the 21st century.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       TIME    I.A.  I/O  (1) Current date in YYYYMMDD format.
C                          (2) Current time in HHMMSS format.
C       DTIME   Real   I   Time step in seconds.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       IYMD21   Increment date in YYYYMMDD format.
C       STRACE   Service routine
C
C  5. Called by :
C
C     Any other routine.
C
C  8. Structure :
C
C     See source code.
C
C  9. Switches :
C
C     C/S  Enable subroutine tracing using STRACE.
C
C 10. Source code :
C/
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         TIME(2)
      REAL            DTIME
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NYMD, NHMS, NSEC
      INTEGER         IYMD21
      EXTERNAL        IYMD21
C/
C/ ------------------------------------------------------------------- /
C/
*
* Zero increment: get "legal" data
*
      NYMD   = TIME(1)
      NHMS   = TIME(2)
      IF (DTIME.EQ.0.) THEN
          NYMD = IYMD21 (NYMD,-1)
          NYMD = IYMD21 (NYMD, 1)
        ENDIF
*
* Convert and increment time :
*
      NSEC = NHMS/10000*3600 + MOD(NHMS,10000)/100* 60 +
     &       MOD(NHMS,100) + NINT(DTIME)
*
* Check change of date :
*
  100 CONTINUE
      IF (NSEC.GE.86400)  THEN
          NSEC = NSEC - 86400
          NYMD = IYMD21 (NYMD,1)
          GOTO 100
        ENDIF
*
  200 CONTINUE
      IF (NSEC.LT.00000)  THEN
          NSEC = 86400 + NSEC
          NYMD = IYMD21 (NYMD,-1)
          GOTO 200
        ENDIF
*
      NHMS = NSEC/3600*10000 + MOD(NSEC,3600)/60*100 + MOD(NSEC,60)
*
      TIME(1) = NYMD
      TIME(2) = NHMS
*
      RETURN
C/
C/ End of TICK21 ----------------------------------------------------- /
C/
      END
