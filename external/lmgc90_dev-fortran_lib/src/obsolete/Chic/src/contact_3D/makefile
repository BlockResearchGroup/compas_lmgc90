# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_CTCT=wrap_SPSPx.o wrap_SPPLx.o wrap_SPCDx.o \
	    wrap_PTPT3.o wrap_PRPRx.o wrap_PRPLx.o \
	    wrap_SPDCx.o wrap_CSPRx.o wrap_CSASp.o \
            wrap_CDCDx.o wrap_CDPLx.o

OBJETS=$(OBJETS_CTCT)

wrap_contact_3D.a: $(OBJETS)
	rm -f wrap_contact_3D.a; ar crs wrap_contact_3D.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap contact_3D"
	@rm -f *.o *.a
