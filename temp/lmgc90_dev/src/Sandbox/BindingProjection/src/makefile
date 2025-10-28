# on définit le compilateur à utiliser
F90 = ifort
F77 = ifort -FI

# édition de liens avec la LibUtilf

# définition du répertoire
LIB_DD_DIR = ../lib
# définition de la lib
LIB_UTILF = $(LIB_DD_DIR)/LibUtilf.a
# définition de l'include de la lib
INCLUDE_UTILF = -I$(LIB_UTILF)

# édition de liens avec la LibPrincf90
LIB_PRINCF90 = $(LIB_DD_DIR)/LibPrincf90.a

# édition de liens avec les lib de LMGC90

# définition de l'include des interface des modules
LMGC90_MOD_DIR = ../../../modules
INCLUDE_LMGC90 = -I$(LMGC90_MOD_DIR)

# définition du réperoire contenant les lib
LIB_LMGC90_DIR = ../../../Core/lib
LIB_LMGC90 = $(LIB_LMGC90_DIR)/shared.a  $(LIB_LMGC90_DIR)/mailx.a

# édition de liens avec LAPACK et BLAS

# définition du répertoire contenant les lib
LIB_LAPACK_DIR = ../../../Bindings/LinearAlgebra/LAPACK/
LIB_LAPACK = -L$(LIB_LAPACK_DIR) -llapack_fast -lblas_fast

# définition du répertoire dans lequel placer les fichiers objets
LIB_PROJECTION_DIR = ../lib

all: dlapst.o mod_a_projection.o 
#test_ test_mod_ test_mod2_

# défintion des sources à compiler :

# la routine provenant de scalapack
dlapst.o: dlapst.f
	$(F77) -c -o dlapst.o dlapst.f
	cp dlapst.o $(LIB_PROJECTION_DIR)

# le module de projection
mod_a_projection.o: mod_a_projection.f90
	$(F90) $(INCLUDE_LMGC90) -c -o mod_a_projection.o mod_a_projection.f90
	cp *.mod $(LMGC90_MOD_DIR)
	cp mod_a_projection.o $(LIB_PROJECTION_DIR)

# compilation d'un test, utilisant directement les routines de DD
test_: dlapst.o test.f90
	$(F90) $(INCLUDE_UTILF) -c -o test.o test.f90
	$(F90) -o test test.o dlapst.o $(LIB_PRINCF90) $(LIB_UTILF) $(LIB_LAPACK)

# compilation d'un test, utilisant le module wrappant les routines de DD
test_mod_: dlapst.o mod_a_projection.o test_mod.f90
	$(F90) $(INCLUDE_UTILF) $(INCLUDE_LMGC90) -c -o test_mod.o test_mod.f90
	$(F90) -o test_mod test_mod.o dlapst.o mod_a_projection.o $(LIB_LMGC90) $(LIB_PRINCF90) $(LIB_UTILF) $(LIB_LAPACK)

# compilation d'un test, utilisant le module wrappant les routines de DD
test_mod2_: dlapst.o mod_a_projection.o test_mod2.f90
	$(F90) $(INCLUDE_UTILF) $(INCLUDE_LMGC90) -c -o test_mod2.o test_mod2.f90
	$(F90) -o test_mod2 test_mod2.o dlapst.o mod_a_projection.o $(LIB_LMGC90) $(LIB_PRINCF90) $(LIB_UTILF) $(LIB_LAPACK)

clean:
	rm *.o 
# test test_mod test_mod2
