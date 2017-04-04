C/ ------------------------------------------------------------------- /
      INTEGER FUNCTION IYMD21 ( NYMD ,M )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    IYMD21      INCREMENT DATE BY +- 1
C   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
C
C ABSTRACT: INCREMENT DATE BY +/- 1
C
C PROGRAM HISTORY LOG:
C   98-10-18  H.L. TOLMAN ORIGINATION
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
C/                  | Last update :         18-Oct-1998 |
C/                  +-----------------------------------+
C/                                Based on INCYMD of the GLA GCM.
C/
C  1. Purpose :
C
C     Increment date in YYYYMMDD format by +/- 1 day.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       NYMD    Int.   I   Old date in YYMMDD format.
C       M       Int.   I   +/- 1 (Day adjustment)
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C       STRACE
C
C  5. Called by :
C
C     Any subroutine.
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
C
C/
C/ ------------------------------------------------------------------- /
C/ Parameter list
C/
      INTEGER         NYMD, M
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NDPM(12), NY, NM, ND
      LOGICAL         LEAP
C/
C/ ------------------------------------------------------------------- /
C/
      DATA     NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*
* "Unpack" and increment date :
*
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      NM   = MIN ( 12 , MAX(1,NM) )
      ND   = MOD(NYMD,100) + M
      LEAP = MOD(NY,400).EQ.0 .OR.
     &        ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
*
* M = -1, change month if necessary :
*
      IF (ND.EQ.0) THEN
          NM   = NM - 1
          IF (NM.EQ.0) THEN
              NM   = 12
              NY   = NY - 1
            ENDIF
          ND   = NDPM(NM)
          IF (NM.EQ.2 .AND. LEAP)  ND = 29
        ENDIF
*
* M = 1, leap year
*
      IF (ND.EQ.29 .AND. NM.EQ.2 .AND. LEAP)  GO TO 20
*
*        next month
*
      IF (ND.GT.NDPM(NM)) THEN
          ND = 1
          NM = NM + 1
          IF (NM.GT.12) THEN
              NM = 1
              NY = NY + 1
          ENDIF
        ENDIF
*
   20 CONTINUE
      IYMD21 = NY*10000 + NM*100 + ND
*
      RETURN
C/
C/ End of IYMD21 ----------------------------------------------------- /
C/
      END
