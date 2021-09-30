########################################################
PROGRAM  = wavesystrk
########################################################

SHELL    = /bin/sh

CC       = 
CFLAGS   = 
CPPFLAGS = 
FC       = ftn
FFLAGS   = -O3  -convert big_endian -assume byterecl -prec-div -prec-sqrt -ip -ftz
LDFLAGS  = 
LIBS     = -L${NETCDF}/lib -lnetcdff -lnetcdf -L${HDF5}/lib -lhdf5_hl -lhdf5 -lrt -lm -lz

INCS     = w3macros.h
SRCS     = wavesystrk.F90 w3strkmd.F90 constants.F90 w3servmd.F90 w3timemd.F90
OBJS     = $(addsuffix .o, $(basename $(SRCS)))

SWITCHES = F90 NCO NOPA LRB4 NCEP2 SHRD SCRIP SCRIPNC NC4 PR3 UQ ST4 STAB0 FLX0 LN1 NL3 BT1 DB1 IC0 IS0 REF0 MLIM TR0 BS0 XX0 WNX1 WNT1 CRX1 CRT1 O0 O1 O2 O4 O5 O6 O7 O14 O15

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

wavesystrk.o : wavesystrk.F90 w3macros.h w3strkmd.o

w3strkmd.o : w3strkmd.F90 w3macros.h

constants.o : constants.F90 w3macros.h

w3servmd.o : w3servmd.F90 w3macros.h constants.o

w3timemd.o : w3timemd.F90 w3macros.h
