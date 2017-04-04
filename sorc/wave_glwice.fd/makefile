SHELL=/bin/sh
#
# This is the global wind extractor
#    compied for silver and winterhawk nodes
#
SRCS=	 wave_glwice.f tick21.f iymd21.f dsec21.f mymd21.f 
OBJS=	 wave_glwice.o tick21.o iymd21.o dsec21.o mymd21.o 
LSTS=    dsec21.lst  iymd21.lst  mymd21.lst  tick21.lst  wave_glwice.lst
XREF=    xref.db
#
FC =		ftn
FFLAGS =	-O -list -auto -convert big_endian -assume byterecl
LDFLAGS =	-O -list -auto
LIBS =
#LIBS =		-L/nwprod/lib -lsp_v2.0.0_4 -lw3nco_v2.0.3_4
#LIBS =		-L/nwprod/lib -lsp -lw3nco
CMD =		wave_glwice 
#

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(CMD) $(OBJS) $(LIBS)

clean:
	-rm -f $(OBJS) $(LSTS) $(XREF)
