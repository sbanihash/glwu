SHELL=/bin/sh
#
SRCS=  convert_ndfd_glwu.f90 w3timemd.f90 constants.f90
OBJS=  convert_ndfd_glwu.o w3timemd.o constants.o
MODS=  convert_ndfd_glwu.mod w3timemd.mod constants.mod
LSTS=  convert_ndfd_glwu.lst w3timemd.lst constants.lst xref.db
#
FC=	ftn
FFLAGS=		-O3 -list -auto -convert big_endian 
CMD=	convert_ndfd_glwu
#
all:	$(CMD)

convert_ndfd_glwu.o:		convert_ndfd_glwu.f90 w3timemd.o constants.o
	$(FC) -c $(FFLAGS) convert_ndfd_glwu.f90

w3timemd.o:		w3timemd.f90
	$(FC) -c $(FFLAGS) w3timemd.f90

constants.o:		constants.f90
	$(FC) -c $(FFLAGS) constants.f90
	
$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(CMD) $(OBJS)

clean:
	rm -f $(OBJS) $(MODS) $(LSTS)
