# Simple Makefile for LMGC90 Compas Wrapper
# Uses ChiPy build (creates liblmgc90.so)

# Paths to LMGC90 build
LMGC_INCLUDE = ../../../build_fortran/modules
LMGC_LIB_PATH = ../../../build_fortran/lib
ADD_LIB_PATH = $(CONDA_PREFIX)/lib

# Simple unified library approach
FLIB_OPT = -I$(LMGC_INCLUDE) -L$(LMGC_LIB_PATH) -llmgc90

# Source directory
SRC_DIR = src

# Build targets
all: comgc90

comgc90: wrap_lmgc90_compas.o $(SRC_DIR)/comgc90.c
	/usr/bin/cc $(SRC_DIR)/comgc90.c wrap_lmgc90_compas.o -o comgc90 $(FLIB_OPT) -L$(ADD_LIB_PATH) -lgfortran

wrap_lmgc90_compas.o: $(SRC_DIR)/wrap_lmgc90_compas.f90
	/usr/bin/f95 $(SRC_DIR)/wrap_lmgc90_compas.f90 -c -o wrap_lmgc90_compas.o $(FLIB_OPT)

run: comgc90
	export LD_LIBRARY_PATH=$(LMGC_LIB_PATH):$$LD_LIBRARY_PATH && ./comgc90

clean:
	rm -f *.o *.mod comgc90

.PHONY: all run clean
