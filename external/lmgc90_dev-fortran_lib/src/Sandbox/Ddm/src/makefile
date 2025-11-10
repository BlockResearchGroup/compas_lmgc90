# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS=mod_SDMxx.o mod_AVSxx.o

ddm.a: $(OBJETS)
	rm -f ddm.a; ar crs ddm.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage contribs::MAILx"
	@rm -f *.o *.a
