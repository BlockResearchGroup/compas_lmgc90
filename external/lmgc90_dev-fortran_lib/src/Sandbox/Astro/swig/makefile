# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 

CPP_LIBS_PATH=
CPP_LIBS=
PYTHON_INC=
NUMPY_INC=

SYSTYPE=$(shell uname -s)

# Linux
ifeq ($(SYSTYPE),Linux)
 LDFLAGS = -shared
 LIBEXT  = .so
endif

# Darwin
ifeq ($(SYSTYPE),Darwin)
 LDFLAGS = -dynamiclib -install_name $(TOP)/../lib/libastro.dylib -framework Python
 LIBEXT  = .dylib
endif

CORE=$(TOP)/../src

SRC = wrap_gravity.f90

# creation du wrapper, compilation avec support de NumPy et edition de lien
swig: $(SRC)
	swig -python -c++ astro.i
	g++ -c -fPIC -O3 $(PYTHON_INC) $(NUMPY_INC) astro_wrap.cxx
	$(FC) $(FFLAGS) $(LDFLAGS) -o ../lib/_astro.so astro_wrap.o \
	$(SRC) $(CORE)/gravity.o $(LIBS_PATH) $(CPP_LIBS_PATH) $(LIBS) $(CPP_LIBS)
	mv astro.py ../lib/
	mv astro_wrap.* ../lib/


#################

.SUFFIXES: .f90 .o

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean: clean_sources clean_swig

clean_sources:
	@rm -f *.o 

clean_swig:
	@rm -f $(TOP)/lib/_astro.so
	@rm -f $(TOP)/lib/astro_wrap.*
	@rm -f $(TOP)/lib/*.py*
