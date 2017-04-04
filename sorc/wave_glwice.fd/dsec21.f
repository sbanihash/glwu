C/ ------------------------------------------------------------------- /
      REAL FUNCTION DSEC21 ( TIME1, TIME2 )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    DSEC21      TIME DIFFERENCE IN SECONDS
C   PRGMMR: HENDRIK          ORG: W/NP21     DATE: 2000-12-28
C
C ABSTRACT: CALCULATE TIME DIFFERENCE IN SECONDS BETWEEN TWO DATES
C   IN YYYYMMDD HHMMSS FORMAT.
C
C PROGRAM HISTORY LOG:
C   93-03-29  H.L. TOLMAN ORIGINATION
C   98-10-29  H.S. CHEN   ADDING DOCBLOCK
C             H.L. TOLMAN
C   00-12-28  H.L. TOLMAN LEAP YEAR EOY FIX
C
C USAGE: SEE ORIGINAL DOCUMENTATION BELOW.
C
C REMARKS: SEE ORIGINAL DOCUMENTATION BELOW.
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77-90
C   MACHINE:  CRAY C90-J90 / IBM RS6000 SP
C
C$$$
C/                  +-----------------------------------+
C/                  | WAVEWATCH-III           NOAA/NCEP |
C/                  |           H. L. Tolman            |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         29-Mar-1993 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Calculate the time difference in seconds between two times in
C     YYMMD HHMMMSS formats.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       TIMEn   I.A.   I   Times, TIMEn(1) is date in YYYYMMDD format,
C                          TIMEn(2) is time in HHMMSS format.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       MYMD21   Calculate Julian data.
C
C  5. Called by :
C
C     Any routine.
C
C  7. Remarks :
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
      INTEGER         TIME1(2), TIME2(2)
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NY1, ND1, NY2, ND2, NS1, NS2, NS, ND, NST
      INTEGER         MYMD21
      EXTERNAL        MYMD21
C/
C/ ------------------------------------------------------------------- /
C/
*
* Convert dates and times :
*
      NY1    = TIME1(1) / 10000
      ND1    = MYMD21 ( TIME1(1) )
      NS1    = TIME1(2)/10000*3600 + MOD(TIME1(2),10000)/100*60 +
     &         MOD(TIME1(2),100)
*
      NY2    = TIME2(1) / 10000
      ND2    = MYMD21 ( TIME2(1) )
      NS2    = TIME2(2)/10000*3600 + MOD(TIME2(2),10000)/100*60 +
     &         MOD(TIME2(2),100)
*
* Number of days and seconds in difference :
*
      ND     = ND2 - ND1
*
      IF (NY1.NE.NY2) THEN
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
        ENDIF
*
      NS     = NS2 - NS1
*
* Output of time difference :
*
      DSEC21 = REAL(NS) + 86400.*REAL(ND)
*
      RETURN
C/
C/ End of DSEC21 ----------------------------------------------------- /
C/
      END
