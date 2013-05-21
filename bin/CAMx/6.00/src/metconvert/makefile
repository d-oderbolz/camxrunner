
FC = pgf90
FLGS = -O2 -Kieee -byteswapio -Mextend

.SUFFIXES: .o .f90

LIBS = 
INC  = 
FLAGG= 

TARGT = metconvert.linux

OBJCTS = \
metconvert.o

metconvert: $(OBJCTS) 
	$(FC) -o $(TARGT) $(FLGS) $(OBJCTS) $(LIBS) $(FLAGG) $(INC)
.f90.o:
	$(FC) -c -o $@ $(FLGS) $(INC) $<

clean:
	rm $(TARGT) $(OBJCTS)
