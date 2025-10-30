# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 
TOP=
FC=
FFLAGS=
LIBS=
LIBS_PATH=
F2PY=

SHARED=$(TOP)/src/shared
RIGID2D=$(TOP)/src/rigid_2D
MAILx=$(TOP)/src/mailx
CONTACT2D=$(TOP)/src/contact_2D
KERNEL=$(TOP)/src/kernel
POST=$(TOP)/src/post
RIGID3D=$(TOP)/src/rigid_3D

CORE=$(TOP)/../Core

SRC_SHARED = $(SHARED)/wrap_overall.f90 $(SHARED)/wrap_bulk_behav.f90 $(SHARED)/wrap_models.f90 $(SHARED)/wrap_ExternalModels.f90 $(SHARED)/wrap_tact_behav.f90 $(SHARED)/wrap_timer.f90 $(SHARED)/wrap_afterall.f90 $(SHARED)/wrap_utilities.f90

SRC_RIGID2D = $(RIGID2D)/wrap_DISKx.f90 $(RIGID2D)/wrap_DISPx.f90 $(RIGID2D)/wrap_JONCx.f90 $(RIGID2D)/wrap_POLYG.f90 $(RIGID2D)/wrap_PT2Dx.f90 $(RIGID2D)/wrap_xKSID.f90 $(RIGID2D)/wrap_xPSID.f90 $(RIGID2D)/wrap_RBDY2.f90 $(RIGID2D)/wrap_prepro.f90 

SRC_CONTACT2D= $(CONTACT2D)/wrap_DKDKx.f90 $(CONTACT2D)/wrap_CLALp.f90 $(CONTACT2D)/wrap_CLJCx.f90 $(CONTACT2D)/wrap_DKALp.f90 $(CONTACT2D)/wrap_DKDKL.f90 $(CONTACT2D)/wrap_DKDPx.f90 $(CONTACT2D)/wrap_DKJCx.f90 $(CONTACT2D)/wrap_DKKDx.f90  $(CONTACT2D)/wrap_DKPDx.f90 $(CONTACT2D)/wrap_DKPLx.f90  $(CONTACT2D)/wrap_P2P2L.f90  $(CONTACT2D)/wrap_PLALp.f90 $(CONTACT2D)/wrap_PLJCx.f90 $(CONTACT2D)/wrap_PLPLx.f90 $(CONTACT2D)/wrap_PTPT2.f90

SRC_MAILX= $(MAILx)/wrap_ALpxx.f90 $(MAILx)/wrap_CLxxx.f90 $(MAILx)/wrap_DISKL.f90 $(MAILx)/wrap_PT2DL.f90 $(MAILx)/wrap_MAILx.f90 $(MAILx)/wrap_mecaMAILx.f90 $(MAILx)/wrap_therMAILx.f90

SRC_KERNEL = $(KERNEL)/wrap_nlgs.f90 

SRC_POST = $(POST)/wrap_postpro.f90 $(POST)/wrap_post2D.f90 $(POST)/wrap_post3D.f90 $(POST)/wrap_postpro_3D.f90 $(POST)/wrap_display_3D.f90

SRC_RIGID3D = $(RIGID3D)/wrap_RBDY3.f90 

# le compilateur
ChiPy:  $(SRC_SHARED) $(SRC_RIGID2D) $(SRC_RIGID3D) $(SRC_MAILX) $(SRC_CONTACT2D) $(SRC_KERNEL) $(SRC_POST) 
	$(F2PY) -c --fcompiler="$(PFC)" --opt="$(FFLAGS)" --f90flags="$(FFLAGS)" -m ChiPy  \
	$(SRC_SHARED) $(SRC_RIGID2D) $(SRC_RIGID3D) $(SRC_MAILX) $(SRC_CONTACT2D) $(SRC_KERNEL) $(SRC_POST) \
	$(CORE)/lib/post_2D.a $(CORE)/lib/post_3D.a $(CORE)/lib/kernel_2D.a $(CORE)/lib/kernel_3D.a \
        $(CORE)/lib/contact_2D.a $(CORE)/lib/contact_3D.a $(CORE)/lib/rigid_2D.a $(CORE)/lib/rigid_3D.a $(CORE)/lib/mailx.a  \
        $(CORE)/lib/shared.a $(CORE)/lib/gmvwrite.a \
        $(CORE)/lib/bindings.a \
	$(LIBS_PATH) $(LIBS) #\
        #-lsvml # spécial pour archi SSE2 et ifort!

#################

.SUFFIXES: .f90 .o

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@echo "#########################"
	@echo "On nettoie les sources"
	@rm -f *.o *.so $(TOP)/modules/*
	@rm -f $(SHARED)/*.o 
	@rm -f $(RIGID2D)/*.o
	@rm -f $(MAILx)/*.o
	@rm -f $(CONTACT2D)/*.o
	@rm -f $(KERNEL)/*.o
	@rm -f $(POST)/*.o
