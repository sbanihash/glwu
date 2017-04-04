C/ ------------------------------------------------------------------- /
      PROGRAM WAVE_GLWICE
C/
C$$$  MAIN PROGRAM DOCUMENTATION BLOCK
C ABSTRACT: CONVERT ICE CONCENTRATION DATA FOR GREAT LAKES WAVE MODEL
C   INPUT, EXPECTS INPUT DATA ON GRIB-LIKE SEQUENTIAL STRUCTURE
C
C WARNING: THIS VERSION EXPECTS WAVE AND ICE TO BE IN IDENTICAL GRIDS
C                .      .    .                                       .
C PROGRAM HISTORY LOG:
C   98-10-28  H.L. TOLMAN ORIGINATION, FOR PROCESSING AVN WINDS
C   04-07-01  J.H.ALVES   FULL REBUILD TO HANDLE ICE GRIB FILES
C   04-07-23  D. CAO      EXTRACT ICE CONCENTRATION  
C   14-10-13  JH ALVES    USE CLIMATOLOGY AND NIC ICE DATA
C
C USAGE:
C   INPUT FILES:
C     UNIT 11  - READ ICE model land-sea mask data (maski)   
C     UNIT 12  - READ ICE concentrations from eice file 
C     UNIT 13  - READ GRL model land-sea mask data (maskw) 
C
C   OUTPUT FILES: 
C     UNIT 51  - OUTPUT FILE WITH UNFORMATTED ICE CONCENTRATIONS
C     UNIT 51  - OUTPUT FILE WITH FORMATTED ICE CONCENTRATIONS
C
C   WORK   FILES: 
C     UNIT 84  - REPORT FILE FOR CHECKING ICE CONCENTRATIONS
C
C   SUBPROGRAMS CALLED: (LIST ALL CALLED FROM ANYWHERE IN CODES)
C     UNIQUE:    - TICK21 IYMD21 MYMD21 DSEC21 CONVRT
C     LIBRARY:
C       W3LIB    - W3TAGB W3TAGE
C       SPLIB    - SPTEZV SPTEZ
C
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          =   N - ABORT, SEE DIAGNOSTIC OUTPUT
C
C REMARKS: THIS PROGRAM MUST BE CONSISTENT WITH THE SHELL SCRIPT
C          EXWAVEGLW_PREP
C        - INTERVAL BETWEEN ICE FIELDS HADWIRED AT 1 OR 3 HOURS 
C          TRANSITION FROM 1 TO 3 HR DEFINED BY STDIN (TTIME13)
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 77
C/
C/                  +-----------------------------------+
C/                  | WAVEWATCH-III            NOAA/NMC |
C/                  |           H TOLMAN                |
C/                  |           D CAO                   |
C/                  |           J. H. Alves             |
C/                  |                        FORTRAN 77 |
C/                  | Last update :         13-Oct-2014 |
C/                  +-----------------------------------+
C/
C  1. Purpose :
C
C     Create ICE file using ASCII extracted data from ICE DATA files.
C
C  2. Method :
C
C     ICEC read from unit 12, fields written to
C     unit 51, time range obtained from standard input.
C
C
C  3. Parameters :
C
C     Variables set in parameter statements.
C     ----------------------------------------------------------------
C       IDIML   Int.  Dimension of regular lat. grid of ICE model.
C       JDIML   Int.  Dimension of regular long. grid of ICE model.
C     ----------------------------------------------------------------
C
C  4. Subroutines used :
C
C  5. Called by :
C
C       None (main program).
C
C  6. Error messages :
C
C       Check on time and array dimensions.
C
C  7. Remarks :
C
C  8. Structure :
C
C       See source code.
C
C  9. Switches :
C
C       None.
C
C 10. Source code :
C
C ----------------------------------------------------------------------
C
C
C Declarations
C
      INTEGER         IDIML, JDIML, IJDIML
C      ICE model demension size   *
      PARAMETER     ( IDIML  =   468, JDIML  =   688    )
      PARAMETER     ( IJDIML = IDIML*JDIML)
C      Wave model demension size
      PARAMETER     ( IWAVE  =   468, JWAVE  =   688    )
      PARAMETER     ( IJWAVE = IWAVE*JWAVE)
C     ICE model domain 
      PARAMETER   (ICELAT1=25.0625,ICELAT2=52.9375)
      PARAMETER   (ICELON1=235.0625,ICELON2=292.9375)
      PARAMETER   (DICE=0.125)
C     Wave model boundary limit and space step 
      PARAMETER   (GRLLAT1=41.1675,GRLLAT2=49.34)
      PARAMETER   (GRLLON1=267.28,GRLLON2=284.455)
      PARAMETER   (DLATGRL=0.0175,DLONGRL=0.025)
C     Define variables and arrays of ICE model 
      INTEGER    IDATE, IY, IM, ID, IS, IFT,
     &           NM, IMAX, JMAX, I, J, TTIME13,
     &           TIME(2), TEND(2), TTST(2), IJMAX
      REAL       FHOUR, FDSEC, FHOURICE
      INTEGER    ICE(IJDIML)
      real       FICE(IJDIML)
      REAL       UU(IJDIML), VV(IJDIML),T1(IJDIML),SST1(IJDIML)
      REAL       USIG, U10, UMIN, UMAX, TMIN, TMAX
      CHARACTER  NAMEI*15, NAMEO*15, TID*10

C     Define the variables to interploate the ice data 
      integer    maski(JDIML,IDIML),maskw(JWAVE,IWAVE)
      real       raw(JDIML,IDIML),new(JWAVE,IWAVE),fnew(IJWAVE)
      logical    flg(JWAVE,IWAVE)

C 0.0  Load the land-sea mask data  
C     Read ICE model land-sea mask
       open (11, file='mask.ice')
          do ii=1,IDIML
          do ij=1,JDIML
            read(11,*) maski(ij,ii)
         enddo
         enddo
      close(11)

C     Read GRL model land-sea mask
        open (13, file='mask.ww3')
          do ii=1,IWAVE
            read(13,*) (maskw(ij,ii),ij=1,JWAVE)
         enddo
      close(13)

C 1.  Initializations -------------------------------------------------
C 1.a DATA statements
C
      DATA         DTTST  / 21600.  /
C
C 1.b Parameters
C
      IMAX   = IDIML
      JMAX   = JDIML
      IJMAX  = IMAX * JMAX
C
C 1.c Time range
C
      WRITE (*,900)
C 
      READ (*,*) TIME, TEND, TTIME13

      WRITE (*,901) TIME(1), TIME(2)/10000, TEND(1), TEND(2)/10000
      WRITE (*,902) TTIME13
C
C 1.d File info
C
      NAMEO  = 'ice.new'
      OPEN (51,FILE=NAMEO,FORM='UNFORMATTED')
      REWIND (51)
C
C 2.  Loop over files ==================================================
C
  200 CONTINUE
C
C 2.a Open file
C
      NAMEI  = 'eice.yyyymmddhh'
      WRITE (TID,'(I8.8,I2.2)') TIME(1), TIME(2)/10000
      NAMEI(6:15) = TID
      OPEN (12,FILE=NAMEI)
      REWIND (12)
C
      WRITE (*,920) NAMEI, NAMEO
C
C
C 2.b Read header data
C
      READ(12,*,END=800,ERR=801,IOSTAT=IERR) IDATE, FHOUR
      READ(12,*) IDIMLN, JDIMLN
C
      IFT    = FHOUR
C
      IY     = IDATE/1000000
      IF ( IY .LT. 100 ) IY = IY + 1900
      IM     = IDATE / 10000 - IY * 100
      ID     = IDATE / 100 - ( IY * 10000 + IM * 100 ) 
      IH     = IDATE - ( IY * 1000000 + IM * 10000 + ID * 100 ) 
C
      TTST(1) = IDATE/100
      TTST(2) = ( IDATE - TTST(1)*100 ) * 10000
      FHOURICE =0.
      FDSEC   = FHOURICE * 3600.
      CALL TICK21 ( TTST , FDSEC )
C
      WRITE (*,922) IY, IM, ID, IH, IFT, TTST(1), TTST(2)/100
C
      IF ( TIME(1).NE.TTST(1) .OR. TIME(2).NE.TTST(2) ) GOTO 810
C
C 3. Read WIND, Air Temp and SFC Temp data
C
C
      READ  (12,*,ERR=750) (ICE(I),I=1,IJDIML)
C
      UMIN   = 100.
      UMAX   =   0.
      TMIN   = 546.
      TMAX   =   0.
C
C 7.  Make archive file -----------------------------------------------
C
      WRITE (51) TIME
      DO I=1,IJDIML
C Scale down to value between 0 and 1
       FICE(I)=ICE(I)*0.1
C Maximum ice concentration is 1, any larger number flags undefined
       IF (ICE(I).GT.1.) ICE(I)=0.
      ENDDO
C     Convert the ice data into gridded one    
      do ii=1,IDIML
        do ij=1,JDIML
          icelen=ij+(ii-1)*jdiml
          raw(ij,ii)=max(0.,fice(icelen))
          new(ij,ii)=raw(ij,ii)
        enddo
      enddo

         open(84,file='report1.txt')
         write(84,*)TIME
         do ii=1,IDIML 
         write(84,1444) (raw(ij,ii),ij=1,JDIML)
         enddo 
         close(84)
 1444    format(1x,464f7.3)

C   ice data modification 
C   Model domain and its resolution
         x0     =  GRLLON1 
         y0     =  GRLLAT1 
         dx     =  DLONGRL  
         dy     =  DLATGRL
CC ICE model domain and its resolution
         xi0    =  ICELON1 
         yi0    =  ICELAT1 
         dix    =  DICE 
         diy    =  DICE
C
         modx=JWAVE 
         mody=IWAVE
C
        do ix=4,modx-3
        do iy=4,mody-3
c new GRL mask: 1=ocean  0=land 
          if ( maskw(ix,iy) .eq. 1) then
            nearland=0
            do iix=ix-3,ix+3
            do iiy=iy-3,iy+3
              if (maskw(iix,iiy).eq.0) then
c This point is close to land
                nearland=1
              endif
             enddo
             enddo

             if (nearland .eq. 1) then
               vmaxice=-1.
               do iix=ix-3,ix+3
               do iiy=iy-3,iy+3
                     vmaxice=max(vmaxice,raw(iix,iiy))
               enddo
               enddo

              new(ix,iy)=vmaxice
 
             endif

         endif

        enddo
        enddo

C Second sweep to get rid of isolated near-land pools
        do iy=3,mody-2
        do ix=3,modx-2
c new GRL mask: 1=ocean  0=land 
          if ( maskw(ix,iy) .eq. 1) then
            nearland=0
            do iix=ix-2,ix+2
            do iiy=iy-2,iy+2
              if (maskw(iix,iiy).eq.0) then
c This point is close to land
                nearland=1
              endif
             enddo
             enddo

             if (nearland .eq. 1) then
               vmaxice=-1.
               do iix=ix-2,ix+2
               do iiy=iy-2,iy+2
                     vmaxice=max(vmaxice,new(iix,iiy))
               enddo
               enddo

              new(ix,iy)=vmaxice

             endif

         endif

        enddo
        enddo

C  Convert the gridded GRL ice data into sequence data
        do ii=1,IWAVE
        do ij=1,JWAVE
          icelen=ij+(ii-1)*jwave
          fnew(icelen)=new(ij,ii)
        enddo
        enddo
    
      WRITE (51) (fnew(I),i=1,ijwave)

      open(52,file='fice')
      do I=1,IJDIML
       write(52,*)FNEW(I)
      enddo

  750 CONTINUE

      IF (FHOUR.LT.0)THEN
        CALL TICK21 ( TIME, 10800. )
      ELSEIF (FHOUR.GE.TTIME13)THEN
        CALL TICK21 ( TIME, 10800. )
      ELSE
        CALL TICK21 ( TIME, 3600. )
      ENDIF 
      IF ( DSEC21(TIME,TEND) .GE. 0. ) GOTO 200
C
C ... End of loop over files ===========================================
C
      WRITE (*,999)
      GOTO 888
C
C ERROR eccape locations
C
  800 CONTINUE
      WRITE (*,1000) IERR
      CALL EXIT ( 1 )
C
  801 CONTINUE
      WRITE (*,1001) IERR
      CALL EXIT ( 2 )
C
  810 CONTINUE
      WRITE (*,1010) TIME, TTST
      CALL EXIT ( 10 )
C
  888 CONTINUE
C
C Formats
C
  900 FORMAT (/' EXTRACTING ICE FROM SPECTRAL FILES'/
     &         ' ========================================='/)
  901 FORMAT ( '     STARTING TIME : ',I8.8,I3.2,'Z'/
     &         '     ENDING TIME   : ',I8.8,I3.2,'Z'/)
  902 FORMAT ( '     FROM 1 TO 3 HR: ',I4,'H FCST'/)
C
  920 FORMAT ( ' ==> FILE ',A,' (',A,') STARTING')
  922 FORMAT ( '     TIME :',I5.4,3I3.2,'  FORECAST HOUR :',I3.2,
     &         '  (',I8.8,I5.4,')')
  928 FORMAT ( '     FIELDS TRANSFERRED TO LL GRID'/
     &         '     U RANGE : ',2F7.2,'     T RANGE : ',2F7.2)
  950 FORMAT ( '        *** SKIPPING TIME *** ')
C
  970 FORMAT (/' TEST OUTPUT : ',A/)
  971 FORMAT ( 1X,I5,1X,120I1)
  972 FORMAT ( 1X,5X,1X,120I1)
C
  999 FORMAT (/' END OF PROGRAM '/
     &         ' =============='/)
C
 1000 FORMAT (/' *** ERROR ***'/
     &         '     PREMATURE END OF INPUT FILE'/
     &         '     IOSTAT= ',I6/)
C
 1001 FORMAT (/' *** ERROR ***'/
     &         '     READ ERROR IN INPUT FILE'/
     &         '     IOSTAT= ',I6/)
C
 1010 FORMAT (/' *** ERROR ***'/
     &         '     UNEXPECTED TIME IN FILE '/
     &         '     FROM FILE : ',I8.8,I7.6/
     &         '     SHOULD BE : ',I8.8,I7.6/)
C
C End of WAVE_GLWICE  -------------------------------------------------------
C
      END
