C/ ------------------------------------------------------------------- /
      INTEGER FUNCTION MYMD21 ( NYMD )
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .                                       .
C SUBPROGRAM:    MYMD21      CALCULATE JULIAN DATE
C   PRGMMR: H.S. CHEN        ORG: W/NP21     DATE: 1998-10-29
C
C ABSTRACT: CALCULATE JULIAN DATE FROM DATE IN YYYYMMDD FORMAT.
C
C PROGRAM HISTORY LOG:
C   98-10-19  H.L. TOLMAN ORIGINATION
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
C/                  | Last update :         19-Oct-1998 |
C/                  +-----------------------------------+
C/                                Based on MODYMD of the GLA GCM.
C/
C  1. Purpose :
C
C     Convert date in YYMMDD format to julian date.
C
C  3. Parameters :
C
C     Parameter list
C     ----------------------------------------------------------------
C       NYMD    Int.   I   Date in YYMMDD format.
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
C/
C/ ------------------------------------------------------------------- /
C/ Parameer list
C/
      INTEGER         NYMD
C/
C/ ------------------------------------------------------------------- /
C/ Local parameters
C/
      INTEGER         NDPM(12), NY, NM, ND
      LOGICAL         LEAP
C/
C/ ------------------------------------------------------------------- /
C/
      DATA    NDPM / 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 /
*
* "Unpack" and increment date :
*
      NY   = NYMD / 10000
      NM   = MOD(NYMD,10000) / 100
      ND   = MOD(NYMD,100)
      LEAP = MOD(NY,400).EQ.0 .OR.
     &        ( MOD(NY,4).EQ.0 .AND. MOD(NY,100).NE.0 )
*
* Loop over months :
*
      IF (NM.GT.2 .AND. LEAP)  ND = ND + 1
*
   40 CONTINUE
      IF (NM.LE.1)  GO TO 60
      NM = NM - 1
      ND = ND + NDPM(NM)
      GO TO 40
*
   60 CONTINUE
      MYMD21 = ND
*
      RETURN
C/
C/ End of MYMD21 ----------------------------------------------------- /
C/
      END
