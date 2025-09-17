# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_PREPRO=wrap_prepro.o

OBJETS_BULK=wrap_RBDY2.o \
            wrap_DISKx.o wrap_xKSID.o\
            wrap_JONCx.o \
            wrap_DISPx.o wrap_xPSID.o\
            wrap_PT2Dx.o wrap_POLYG.o


OBJETS=$(OBJETS_BULK) $(OBJETS_PREPRO)

wrap_rigid_2D.a: $(OBJETS)
	rm -f wrap_rigid_2D.a; ar crs wrap_rigid_2D.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap rigid_2D"
	@rm -f *.o *.a
