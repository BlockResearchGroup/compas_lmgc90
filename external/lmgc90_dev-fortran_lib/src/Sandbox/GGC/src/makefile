# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS=mod_GGCxx.o 

GGC.a: $(OBJETS)
	rm -f GGC.a; ar crs GGC.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage contribs::GGCxx"
	@rm -f *.o *.a
