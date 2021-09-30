########################################################
PROGRAM  = multiwavegrads_fields
########################################################

SHELL    = /bin/sh

CC       = 
CFLAGS   = 
CPPFLAGS = 
FC       = ftn
FFLAGS   = -O3  -convert big_endian -assume byterecl -prec-div -prec-sqrt -ip -ftz
LDFLAGS  = 
LIBS     = 

INCS     = w3macros.h
SRCS     = multiwavegrads_fields.F90 wmmdatmd.F90 w3gdatmd.F90 w3wdatmd.F90 w3adatmd.F90 w3idatmd.F90 w3odatmd.F90 w3triamd.F90 w3src4md.F90 w3snl3md.F90 w3sdb1md.F90 w3iogrmd.F90 w3iogomd.F90 constants.F90 w3servmd.F90 w3timemd.F90 w3arrymd.F90 w3dispmd.F90 w3gsrumd.F90 w3namlmd.F90
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

multiwavegrads_fields.o : multiwavegrads_fields.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o w3iogrmd.o w3iogomd.o constants.o w3servmd.o w3timemd.o

wmmdatmd.o : wmmdatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3servmd.o

w3gdatmd.o : w3gdatmd.F90 w3macros.h constants.o w3servmd.o w3gsrumd.o

w3wdatmd.o : w3wdatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3servmd.o

w3adatmd.o : w3adatmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3idatmd.o w3servmd.o

w3idatmd.o : w3idatmd.F90 w3macros.h w3gdatmd.o w3servmd.o

w3odatmd.o : w3odatmd.F90 w3macros.h w3gdatmd.o w3servmd.o

w3triamd.o : w3triamd.F90 w3macros.h w3gdatmd.o w3adatmd.o w3odatmd.o w3idatmd.o w3iogrmd.o constants.o w3servmd.o w3arrymd.o w3dispmd.o

w3src4md.o : w3src4md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o w3dispmd.o

w3snl3md.o : w3snl3md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o w3dispmd.o

w3sdb1md.o : w3sdb1md.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o

w3iogrmd.o : w3iogrmd.F90 w3macros.h w3gdatmd.o w3odatmd.o w3src4md.o w3snl3md.o constants.o w3servmd.o w3dispmd.o

w3iogomd.o : w3iogomd.F90 w3macros.h w3gdatmd.o w3wdatmd.o w3adatmd.o w3odatmd.o constants.o w3servmd.o w3namlmd.o

constants.o : constants.F90 w3macros.h

w3servmd.o : w3servmd.F90 w3macros.h constants.o

w3timemd.o : w3timemd.F90 w3macros.h

w3arrymd.o : w3arrymd.F90 w3macros.h

w3dispmd.o : w3dispmd.F90 w3macros.h w3gdatmd.o w3odatmd.o constants.o w3servmd.o

w3gsrumd.o : w3gsrumd.F90 w3macros.h w3servmd.o

w3namlmd.o : w3namlmd.F90 w3macros.h wmmdatmd.o
