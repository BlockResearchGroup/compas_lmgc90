# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_2D = wrap_postpro.o wrap_post2D.o
OBJETS_3D = wrap_postpro_3D.o wrap_post3D.o wrap_display_3D.o

wrap_post_2D.a: $(OBJETS_2D)
	rm -f wrap_post_2D.a; ar crs wrap_post_2D.a $(OBJETS_2D)

wrap_post_3D.a: $(OBJETS_3D)
	rm -f wrap_post_3D.a; ar crs wrap_post_3D.a $(OBJETS_3D)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap post"
	@rm -f *.o *.a