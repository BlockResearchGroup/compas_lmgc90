
include lmgc90.cfg

#
ADD_LIB_PATH = $(CONDA_PREFIX)/lib

FLIB_OPT = -I$(LMGC_INCLUDE) -L$(LMGC_LIBRARY_PATH) -llmgc90
CLIB_OPT = -I$(VIRTUAL_ENV)/include -L$(VIRTUAL_ENV)/lib -llmgc90

# must have the lmgc90.so in activated environment
C:
	$(CXX) command.cpp -o crun $(CLIB_OPT)

F:
	$(FC) command.f90 -o frun $(FLIB_OPT)

wrap:
	$(FC) wrap_lmgc90_compas.f90 -c -o wrap_lmgc90_compas.o $(FLIB_OPT)
	$(CC) comgc90.c wrap_lmgc90_compas.o -o comgc90 $(FLIB_OPT) -L$(ADD_LIB_PATH) -lgfortran

wrap_lib:
	$(FC) wrap_lmgc90_compas.f90 -shared -o _lmgc90compas.so $(FLIB_OPT) -L$(ADD_LIB_PATH) -lgfortran
clean:
	rm -f crun frun *.o *.mod *.so comgc90
