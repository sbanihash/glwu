########################################################
PROGRAM  = multiwavefcst
########################################################

SHELL    = /bin/sh

CC       = 
CFLAGS   = 
CPPFLAGS = 
FC       = ftn
FFLAGS   = -O3  -convert big_endian -assume byterecl -prec-div -prec-sqrt -ip -I${NETCDF}/include -fp-model precise -fast-transcendentals -no-prec-div -no-prec-sqrt -align array64byte
LDFLAGS  = 
LIBS     = -L${NETCDF}/lib -lnetcdff -lnetcdf -L${HDF5_ROOT}/lib -lhdf5_hl -lhdf5 -lrt -lm -lz

INCS     = w3macros.h
SRCS     = multiwavefcst.F90 wmmdatmd.F90 w3gdatmd.F90 w3wdatmd.F90 w3adatmd.F90 w3idatmd.F90 w3odatmd.F90 wminitmd.F90 wmwavemd.F90 wmfinlmd.F90 wmgridmd.F90 wmupdtmd.F90 wminiomd.F90 w3fldsmd.F90 w3initmd.F90 w3wavemd.F90 w3wdasmd.F90 w3updtmd.F90 w3profsmd.F90 w3pro3md.F90 w3uqckmd.F90 w3triamd.F90 w3srcemd.F90 w3flx1md.F90 w3sln1md.F90 w3src4md.F90 w3snl3md.F90 w3sbt1md.F90 w3sdb1md.F90 w3iogrmd.F90 w3iogomd.F90 w3iopomd.F90 wmiopomd.F90 w3iotrmd.F90 w3iorsmd.F90 w3iobcmd.F90 w3iosfmd.F90 w3partmd.F90 constants.F90 w3servmd.F90 w3timemd.F90 w3arrymd.F90 w3dispmd.F90 w3cspcmd.F90 w3gsrumd.F90 wmunitmd.F90 w3namlmd.F90 scrip_constants.f scrip_grids.f scrip_iounitsmod.f90 scrip_remap_vars.f scrip_timers.f scrip_errormod.f90 scrip_interface.F90 scrip_kindsmod.f90 scrip_remap_conservative.f wmscrpmd.F90 scrip_netcdfmod.f90 scrip_remap_write.f scrip_remap_read.f
OBJS     = $(addsuffix .o, $(basename $(SRCS)))

SWITCHES = F90 NCO NOPA LRB4 NOGRB DIST MPI SCRIP SCRIPNC NC4 PR3 UQ ST4 STAB0 FLX0 LN1 NL3 BT1 DB1 IC0 IS0 REF0 MLIM TR0 BS0 XX0 WNX1 WNT1 CRX1 CRT1 O0 O1 O2 O4 O5 O6 O7 O14 O15

########################################################
# targets
########################################################
default : $(PROGRAM)

$(PROGRAM) : $(OBJS)
	$(FC) $(LDFLAGS) -o $(PROGRAM) $(OBJS) $(LIBS)

clean :
	\rm -f *.o *.mod

########################################################
# compile rules
########################################################
.SUFFIXES : .f .f90 .F .F90 .c

%.o : %.c
	$(CC) -c $(CFLAGS) $<

%.o : %.f90
	$(FC) -c $(FFLAGS) $<

%.o : %.F90
	$(FC) -c $(FFLAGS) $<

%.o : %.f
	$(FC) -c $(FFLAGS) $<

%.o : %.F
	$(FC) -c $(FFLAGS) $<

########################################################
# dependencies
########################################################

multiwavefcst.o : multiwavefcst.F90 w3macros.h wminitmd.o wmwavemd.o wmfinlmd.o wmmdatmd.o

wmmdatmd.o : wmmdatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3servmd.o

w3gdatmd.o : w3gdatmd.F90 w3macros.h constants.o w3servmd.o w3gsrumd.o

w3wdatmd.o : w3wdatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3servmd.o

w3adatmd.o : w3adatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3idatmd.o w3servmd.o

w3idatmd.o : w3idatmd.F90 w3macros.h w3gdatmd.o w3servmd.o

w3odatmd.o : w3odatmd.F90 w3macros.h w3gdatmd.o w3servmd.o

wminitmd.o : wminitmd.F90 w3macros.h w3initmd.o w3fldsmd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iogrmd.o w3iogomd.o constants.o w3servmd.o w3timemd.o w3gsrumd.o wmmdatmd.o wmgridmd.o wmunitmd.o wminiomd.o wmiopomd.o w3namlmd.o

wmwavemd.o : wmwavemd.F90 w3macros.h w3wavemd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iopomd.o constants.o w3servmd.o w3timemd.o wmmdatmd.o wmupdtmd.o wminiomd.o wmiopomd.o

wmfinlmd.o : wmfinlmd.F90 w3macros.h w3timemd.o wmmdatmd.o

wmgridmd.o : wmgridmd.F90 w3macros.h w3gdatmd.o w3adatmd.o w3odatmd.o constants.o w3servmd.o w3gsrumd.o w3triamd.o wmmdatmd.o wmscrpmd.o scrip_constants.o scrip_grids.o scrip_iounitsmod.o scrip_remap_vars.o scrip_timers.o scrip_errormod.o scrip_interface.o scrip_kindsmod.o scrip_remap_conservative.o scrip_netcdfmod.o scrip_remap_write.o scrip_remap_read.o

wmupdtmd.o : wmupdtmd.F90 w3macros.h w3fldsmd.o w3gdatmd.o w3wdatmd.o w3idatmd.o w3servmd.o w3timemd.o wmmdatmd.o

wminiomd.o : wminiomd.F90 w3macros.h w3updtmd.o w3cspcmd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3iobcmd.o w3servmd.o w3timemd.o wmmdatmd.o

w3fldsmd.o : w3fldsmd.F90 w3macros.h w3idatmd.o w3timemd.o w3gsrumd.o

w3initmd.o : w3initmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iogrmd.o w3iogomd.o w3iopomd.o w3iorsmd.o constants.o w3servmd.o w3timemd.o w3arrymd.o w3dispmd.o

w3wavemd.o : w3wavemd.F90 w3macros.h w3updtmd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iogrmd.o w3iogomd.o w3iopomd.o w3iotrmd.o w3iorsmd.o w3iobcmd.o w3iosfmd.o w3pro3md.o w3profsmd.o w3srcemd.o constants.o w3servmd.o w3timemd.o w3triamd.o

w3wdasmd.o : w3wdasmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o

w3updtmd.o : w3updtmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o constants.o w3servmd.o w3timemd.o w3dispmd.o w3triamd.o

w3profsmd.o : w3profsmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o constants.o w3timemd.o

w3pro3md.o : w3pro3md.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3uqckmd.o constants.o w3servmd.o w3timemd.o

w3uqckmd.o : w3uqckmd.F90 w3macros.h

w3triamd.o : w3triamd.F90 w3macros.h w3gdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iogrmd.o constants.o w3servmd.o w3arrymd.o w3dispmd.o

w3srcemd.o : w3srcemd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3odatmd.o w3idatmd.o w3sln1md.o w3src4md.o w3snl3md.o w3sbt1md.o w3sdb1md.o constants.o w3dispmd.o

w3flx1md.o : w3flx1md.F90 w3macros.h w3odatmd.o w3servmd.o

w3sln1md.o : w3sln1md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o

w3src4md.o : w3src4md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o w3dispmd.o

w3snl3md.o : w3snl3md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o w3dispmd.o

w3sbt1md.o : w3sbt1md.F90 w3macros.h w3gdatmd.o

w3sdb1md.o : w3sdb1md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o

w3iogrmd.o : w3iogrmd.F90 w3macros.h w3gdatmd.o w3adatmd.o w3odatmd.o w3src4md.o w3snl3md.o constants.o w3servmd.o w3dispmd.o

w3iogomd.o : w3iogomd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o constants.o w3servmd.o w3namlmd.o

w3iopomd.o : w3iopomd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o constants.o w3servmd.o w3gsrumd.o w3triamd.o

wmiopomd.o : wmiopomd.F90 w3macros.h w3initmd.o w3cspcmd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3iopomd.o constants.o w3servmd.o w3gsrumd.o w3triamd.o wmmdatmd.o

w3iotrmd.o : w3iotrmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o constants.o w3timemd.o w3gsrumd.o

w3iorsmd.o : w3iorsmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3servmd.o w3triamd.o

w3iobcmd.o : w3iobcmd.F90 w3macros.h w3cspcmd.o w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o constants.o w3servmd.o w3gsrumd.o w3triamd.o

w3iosfmd.o : w3iosfmd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3partmd.o constants.o w3servmd.o

w3partmd.o : w3partmd.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3dispmd.o

constants.o : constants.F90 w3macros.h

w3servmd.o : w3servmd.F90 w3macros.h constants.o

w3timemd.o : w3timemd.F90 w3macros.h

w3arrymd.o : w3arrymd.F90 w3macros.h

w3dispmd.o : w3dispmd.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o

w3cspcmd.o : w3cspcmd.F90 w3macros.h constants.o w3servmd.o

w3gsrumd.o : w3gsrumd.F90 w3macros.h w3servmd.o

wmunitmd.o : wmunitmd.F90 w3macros.h w3servmd.o

w3namlmd.o : w3namlmd.F90 w3macros.h wmmdatmd.o

scrip_constants.o : scrip_constants.f scrip_kindsmod.o

scrip_grids.o : scrip_grids.f scrip_kindsmod.o scrip_constants.o scrip_iounitsmod.o scrip_netcdfmod.o scrip_errormod.o

scrip_iounitsmod.o : scrip_iounitsmod.f90 scrip_kindsmod.o

scrip_remap_vars.o : scrip_remap_vars.f scrip_kindsmod.o scrip_constants.o scrip_grids.o scrip_errormod.o scrip_netcdfmod.o scrip_iounitsmod.o

scrip_timers.o : scrip_timers.f scrip_kindsmod.o

scrip_errormod.o : scrip_errormod.f90 scrip_kindsmod.o scrip_iounitsmod.o

scrip_interface.o : scrip_interface.F90 scrip_kindsmod.o scrip_constants.o scrip_timers.o scrip_remap_vars.o scrip_grids.o scrip_remap_conservative.o scrip_netcdfmod.o scrip_remap_write.o scrip_remap_read.o scrip_iounitsmod.o scrip_errormod.o

scrip_kindsmod.o : scrip_kindsmod.f90

scrip_remap_conservative.o : scrip_remap_conservative.f scrip_kindsmod.o scrip_constants.o scrip_timers.o scrip_remap_vars.o scrip_grids.o scrip_errormod.o scrip_netcdfmod.o scrip_iounitsmod.o

wmscrpmd.o : wmscrpmd.F90 w3macros.h w3gdatmd.o constants.o w3servmd.o scrip_constants.o scrip_grids.o scrip_iounitsmod.o scrip_remap_vars.o scrip_timers.o scrip_errormod.o scrip_interface.o scrip_kindsmod.o scrip_remap_conservative.o scrip_netcdfmod.o scrip_remap_write.o scrip_remap_read.o

scrip_netcdfmod.o : scrip_netcdfmod.f90 scrip_kindsmod.o scrip_errormod.o

scrip_remap_write.o : scrip_remap_write.f scrip_kindsmod.o scrip_constants.o scrip_netcdfmod.o scrip_remap_vars.o scrip_grids.o scrip_errormod.o scrip_iounitsmod.o

scrip_remap_read.o : scrip_remap_read.f scrip_kindsmod.o scrip_constants.o scrip_netcdfmod.o scrip_remap_vars.o scrip_grids.o scrip_errormod.o scrip_iounitsmod.o
