# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_EF_MAILx=wrap_MAILx.o

OBJETS_EF_MECA=wrap_mecaMAILx.o 

OBJETS_EF_THER=wrap_therMAILx.o 

OBJETS_CONTACT=wrap_CLxxx.o wrap_ALpxx.o wrap_PT2DL.o wrap_DISKL.o wrap_CSxxx.o wrap_ASpxx.o

OBJETS=$(OBJETS_EF_MAILx) $(OBJETS_EF_MECA) $(OBJETS_EF_THER) $(OBJETS_CONTACT) 

wrap_mailx.a: $(OBJETS)
	rm -f wrap_mailx.a; ar crs wrap_mailx.a $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap mailx"
	@rm -f *.o *.a