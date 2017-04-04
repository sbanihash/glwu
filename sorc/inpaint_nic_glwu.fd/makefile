SHELL=/bin/sh
#
SRCS=  inpaint_nic_glwu.f90
OBJS=  inpaint_nic_glwu.o
MODS=  
LSTS=  inpaint_nic_glwu.lst
#
FC=	ftn
FFLAGS=		-O3 -list -auto 
CMD=	inpaint_nic_glwu
#
all:	$(CMD)

inpaint_nic_glwu.o:		inpaint_nic_glwu.f90
	$(FC) -c $(FFLAGS) inpaint_nic_glwu.f90

$(CMD):	$(OBJS)
	$(FC) $(FFLAGS) -o $(CMD) $(OBJS)

clean:
	rm -f $(OBJS) $(MODS) $(LSTS)
