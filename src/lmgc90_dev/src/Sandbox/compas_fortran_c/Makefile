# Simple Makefile for LMGC90 Compas Wrapper
# Uses ChiPy build (creates liblmgc90.so)

# Paths to LMGC90 headers and libs (override via environment if needed)
LMGC_INCLUDE ?= ./modules
LMGC_LIB_PATH ?= ./lib
ADD_LIB_PATH ?= $(CONDA_PREFIX)/lib

# Unified library (if available)
LMGC_UNIFIED = -llmgc90

# Minimal static link set (no Python/SWIG/HDF5) for rigid 3D
LMGC_CORE_LIBS = -Wl,--start-group \
  -llmgc_core_shared -llmgc_core_rigid_3d -llmgc_core_contact_3d -llmgc_core_contactor_3d \
  -llmgc_core_kernel_3d -llmgc_core_post_3d -llmgc_core_utils -llmgc_core_mailx \
  -llmgc_core_shared_contribs -llmgc_core_other_contribs -llmgc_core_utils_contribs \
  -llmgc_ann -llmgc_clipper \
  -lpredicates -llibrtree -lminpack -ldemmefi -llmgc_exception \
  -llmgc_core_mbs3d -llmgc_core_mbs2d \
  -llmgc_bindings_FEM -llmgc_bindings_user -llmgc_bindings_sparse_la -llmgc_bindings_MBS \
  -lClipper2 -lClipper2Z -lClipper2utils -lClipper2Zutils \
  -lann_euclid -lann_manhattan \
  -Wl,--end-group

LMGC_SYS_LIBS = -lopenblas -lgfortran -lquadmath -lstdc++ -lm -ldl

# Choose linking mode: unified if liblmgc90.so exists, else static libs
ifeq ($(wildcard $(LMGC_LIB_PATH)/liblmgc90.so),$(LMGC_LIB_PATH)/liblmgc90.so)
  LMGC_LINK = $(LMGC_UNIFIED)
else
  LMGC_LINK = $(LMGC_CORE_LIBS)
endif

CFLAGS_COMMON = -I$(LMGC_INCLUDE) -L$(LMGC_LIB_PATH)

# Source directory
SRC_DIR = src

# Build targets
all: comgc90

comgc90: wrap_lmgc90_compas.o $(SRC_DIR)/comgc90.c
	/usr/bin/cc $(SRC_DIR)/comgc90.c wrap_lmgc90_compas.o -o comgc90 $(CFLAGS_COMMON) -L$(ADD_LIB_PATH) $(LMGC_LINK) $(LMGC_SYS_LIBS)

wrap_lmgc90_compas.o: $(SRC_DIR)/wrap_lmgc90_compas.f90
	/usr/bin/f95 $(SRC_DIR)/wrap_lmgc90_compas.f90 -c -o wrap_lmgc90_compas.o $(CFLAGS_COMMON)

run: comgc90
	mkdir -p OUTBOX POSTPRO && export LD_LIBRARY_PATH=$(LMGC_LIB_PATH):$$LD_LIBRARY_PATH && ./comgc90

clean:
	rm -f *.o *.mod comgc90

.PHONY: all run clean
