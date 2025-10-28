# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_BULK=wrap_RBDY3.o \
            wrap_SPHER.o \
	    wrap_PLANx.o \
	    wrap_POLYR.o \
	    wrap_PT3Dx.o \
	    wrap_DNLYC.o \
	    wrap_CYLND.o

OBJETS=$(OBJETS_BULK)

wrap_rigid_3D.a: $(OBJETS)
	rm -f wrap_rigid_3D.a; ar crs wrap_rigid_3D.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap rigid_3D"
	@rm -f *.o *.a
