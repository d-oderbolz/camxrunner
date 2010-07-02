F77  = pgf77

FLGS =  -byteswapio -Wl,-Bstatic

LIBS = 

TARGT = CAMxtrct

OBJCTS = \
bswap.cvt.o \
camxtrct.o \
crs2fin.o \
filbnd.o \
filfin.o \
fndchr.o \
grdtrp.o \
interp2d.o \
interpv.o \
juldate.o \
lftjst.o \
low2up.o \
operate.o \
rdhdcg.o \
rdhdfg.o \
rdhrcg.o \
rdhrfg.o \
rdhrpt.o \
rdstn.o \
rdtmpr.o \
readzp.o \
spcset.o \
strlen.o \
userin.o \
vertag.o \
wrtavg.o \
wrtdat.o \
wrtgrd.o \
wrthdr.o \
zeros.o 


CAMxtrct: $(OBJCTS)
	$(F77) -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS)
.f.o	:
	$(F77) -c -o $@ $(FLGS) $<

camxtrct.o 	: camxtrct.f	 camxtrct.inc
crs2fin.o 	: crs2fin.f	 camxtrct.inc
filbnd.o 	: filbnd.f	 camxtrct.inc
filfin.o 	: filfin.f	 camxtrct.inc
grdtrp.o 	: grdtrp.f	 camxtrct.inc
interp2d.o 	: interp2d.f	 camxtrct.inc
interpv.o 	: interpv.f	 camxtrct.inc
operate.o 	: operate.f	 camxtrct.inc
rdhdcg.o 	: rdhdcg.f	 camxtrct.inc
rdhdfg.o 	: rdhdfg.f	 camxtrct.inc
rdhrcg.o 	: rdhrcg.f	 camxtrct.inc
rdhrfg.o 	: rdhrfg.f	 camxtrct.inc
rdhrpt.o 	: rdhrpt.f	 camxtrct.inc
rdstn.o 	: rdstn.f	 camxtrct.inc
rdtmpr.o 	: rdtmpr.f	 camxtrct.inc
readzp.o 	: readzp.f	 camxtrct.inc
spcset.o 	: spcset.f	 camxtrct.inc
userin.o 	: userin.f	 camxtrct.inc
vertag.o 	: vertag.f	 camxtrct.inc
wrtavg.o 	: wrtavg.f	 camxtrct.inc
wrtdat.o 	: wrtdat.f	 camxtrct.inc
wrtgrd.o 	: wrtgrd.f	 camxtrct.inc
wrthdr.o 	: wrthdr.f	 camxtrct.inc
