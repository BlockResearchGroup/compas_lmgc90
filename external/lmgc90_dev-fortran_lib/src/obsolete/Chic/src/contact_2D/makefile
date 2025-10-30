# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_CTCT=wrap_DKDKx.o wrap_DKKDx.o wrap_DKJCx.o wrap_DKDPx.o wrap_DKPDx.o \
            wrap_CLALp.o wrap_CLJCx.o wrap_PLPLx.o wrap_PLJCx.o wrap_PTPT2.o \
            wrap_P2P2L.o wrap_PLALp.o wrap_DKPLx.o wrap_DKALp.o wrap_DKDKL.o \
	    wrap_DPALp.o
 
OBJETS=$(OBJETS_CTCT)

wrap_contact_2D.a: $(OBJETS)
	rm -f wrap_contact_2D.a; ar crs wrap_contact_2D.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap contact_2D"
	@rm -f *.o *.a
