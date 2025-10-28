# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS = mod_chic.o wrap_overall.o wrap_bulk_behav.o wrap_models.o wrap_tact_behav.o wrap_timer.o


wrap_shared.a: $(OBJETS)
	rm -f wrap_shared.a; ar crs wrap_shared.a $(OBJETS)

mod_chic.o: mod_chic.f90
	$(FC) -c -o $@ $(CHICFLAGS) $< 

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap shared"
	@rm -f *.o *.a
