# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

.SUFFIXES: .f90 .o

OBJETS_2D = wrap_nlgs.o wrap_cpg.o wrap_mp_solver.o
OBJETS_3D = wrap_nlgs_3D.o wrap_cpg_3D.o wrap_mp_solver_3D.o

wrap_kernel_2D.a: $(OBJETS_2D)
	rm -f wrap_kernel_2D.a; ar crs wrap_kernel_2D.a $(OBJETS_2D)

wrap_kernel_3D.a: $(OBJETS_3D)
	rm -f wrap_kernel_3D.a; ar crs wrap_kernel_3D.a $(OBJETS_3D)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "nettoyage de wrap kernel"
	@rm -f *.o *.a
