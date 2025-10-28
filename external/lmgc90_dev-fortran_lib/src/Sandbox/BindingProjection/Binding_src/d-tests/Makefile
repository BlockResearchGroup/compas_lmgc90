SHELL = /bin/sh

# choix du compilateur fortran:
#  * gfortran
#  * ifort
FC=ifort

# options pour gfortran
#OPTIF =  -g -I../d-LibUtilf
# options pour ifort
OPTIF = -O2 -FI -I../d-LibUtilf
#OPTIF = -C -FI -I../d-LibUtilf
OPTIC = 
OPTIL = $(OPTIF)
OPTIM = -I../d-LibUtilf

.SUFFIXES: .f .c .o .a

.SILENT:

#.IGNORE:

LIBUTILF = ../d-LibUtilf/LibUtilf.a
LIBPRINCF = ../d-LibPrincf/LibPrincf.a
LIBPRINCF90 = ../d-LibPrincf90/LibPrincf90.a


DEVEL0 = testdd0
SOURCEMAIN0 = testdd0.f
OBJMAIN0 = $(SOURCEMAIN0:.f=.o)
OBJDEV0 = 

DEVEL1 = testdd1
SOURCEMAIN1 = testdd1.f
OBJMAIN1 = $(SOURCEMAIN1:.f=.o)
OBJDEV1 = 

DEVEL1a = testdd1a
SOURCEMAIN1a = testdd1a.f dlapst.f
OBJMAIN1a = $(SOURCEMAIN1a:.f=.o)
OBJDEV1a = 

DEVEL2 = testdd2
OBJDEV2 = ChpoToChamnog.mexglx NodesInMeshg.mexglx ProjectChamnog.mexglx

DEVEL3 = testdd3
OBJDEV3 = testdd3.avs $(OBJDEV2)

DEVEL4 = testdd4
OBJDEV4 = testdd4.avs $(OBJDEV2)

DEVEL5 = testdd5
OBJDEV5 = testdd5.avs $(OBJDEV2)

DEVEL6 = testdd6
OBJDEV6 = $(OBJDEV2)

DEVEL6a = testdd6a
SOURCEMAIN6a = testdd6a.f
OBJMAIN6a = $(SOURCEMAIN6a:.f=.o)
OBJDEV6a = 

END = 

# Placer ici l'executable castem pour generer la mise en donnee
CAST3M = /usr/local/cast3m/bin/castem06

# LIBSTD = /usr/X11R6/lib/libX11.a

# utilisation de BLAS/LAPACK:
# si le chemin vers la lib est dans le PATH
#LIB1 = -llapack -lblas
# on utilise la version de BLAS/LAPACK qui vient avec LMGC90
#LIB1 = -L/home/alex/CODING/LMGC90v2_ifort/LMGC90v2_BindingLAPACK/LAPACK -llapack_fast -lblas_fast
LIB1 = -L/home/alex/CODING/LMGC90v2_ifort/LMGC90v2_BindingLAPACK/LAPACK -llapack_C -lblas_C

# ce qui suit est inutile: la routine a ete recuperee ailleurs!
# Librairie ScalaPack en statique pour eviter quelques problemes
#LIB2 = 
#dlapst.o
#/usr/lib/libscalapack.a

default : 
	echo 'gmake -r '$(DEVEL0)
	echo 'gmake -r '$(DEVEL1)
	echo 'gmake -r '$(DEVEL1a)
	echo 'gmake -r '$(DEVEL2)
	echo 'gmake -r '$(DEVEL3)
	echo 'gmake -r '$(DEVEL4)
	echo 'gmake -r '$(DEVEL5)
	echo 'gmake -r '$(DEVEL6)
	echo 'gmake -r '$(DEVEL6a)
	echo 'gmake -r all'

all : $(DEVEL0) $(DEVEL1) $(DEVEL1a) $(DEVEL2) $(DEVEL3) $(DEVEL4) $(DEVEL5) $(DEVEL6) $(DEVEL6a)

$(DEVEL2) : $(OBJDEV2)
	echo ' lancer matlab sur ' $@'.m' && echo ' sauvegarde de ' $@.m ; cp $@.m save/.

$(DEVEL3) : $(OBJDEV3)
	echo ' lancer matlab sur ' $@'.m' && echo ' sauvegarde de ' $@.m ; cp $@.m save/.

$(DEVEL4) : $(OBJDEV4)
	echo ' lancer matlab sur ' $@'.m' && echo ' sauvegarde de ' $@.m ; cp $@.m save/.

$(DEVEL5) : $(OBJDEV5)
	echo ' lancer matlab sur ' $@'.m' && echo ' sauvegarde de ' $@.m ; cp $@.m save/.

$(DEVEL6) : $(OBJDEV6)
	echo ' lancer matlab sur ' $@'.m' && echo ' sauvegarde de ' $@.m ; cp $@.m save/.

$(DEVEL0) : $(OBJMAIN0) $(OBJDEV0)
	echo ' edition de lien pour ' $@
	$(FC) $(OPTIL) -o $@ $(OBJMAIN0) $(OBJDEV0) $(LIBPRINCF90) $(LIBUTILF) $(LIB1)
	echo ' edition de lien terminee'

$(DEVEL1) : $(OBJMAIN1) $(OBJDEV1)
	echo ' edition de lien pour ' $@
	$(FC) $(OPTIL) -o $@ $(OBJMAIN1) $(OBJDEV1) $(LIBPRINCF) $(LIBUTILF) $(LIB1) $(LIB2) $(END)
	echo ' edition de lien terminee'

$(DEVEL1a) : $(OBJMAIN1a) $(OBJDEV1a) $(LIBPRINCF90) $(LIBUTILF)
	echo ' edition de lien pour ' $@
	$(FC) $(OPTIL) -o $@ $(OBJMAIN1a) $(OBJDEV1a) $(LIBPRINCF90) $(LIBUTILF) $(LIB1) $(END)
	echo ' edition de lien terminee'

$(DEVEL6a) : $(OBJMAIN6a) $(OBJDEV6a)
	echo ' edition de lien pour ' $@
	$(FC) $(OPTIL) -o $@ $(OBJMAIN6a) $(OBJDEV6a) $(LIBPRINCF90) $(LIBUTILF) $(LIB1) $(LIB2)
	echo ' edition de lien terminee'

.f.o :;
	echo ' compilation de ' $*.f
	$(FC) $(OPTIF) -c $*.f -o $*.o && echo ' sauvegarde de ' $*.f ; cp $*.f save/$*.f

.c.o :;
	echo ' compilation de ' $*.c
	gcc $(OPTIC) -c $*.c -o $*.o && echo ' sauvegarde de ' $*.c ; cp $*.c save/$*.c

traces.o : traces.f traces.h
	echo ' compilation de ' $*.f
	$(FC) $(OPTIF) -c $*.f -o $*.o && echo ' sauvegarde de ' $*.f ; cp $*.f save/$*.f ; echo ' sauvegarde de ' $*.h ; cp $*.h save/$*.h

ChpoToChamnog.mexglx : ChpoToChamnog.F ddreal8toint.f ddinttoreal8.f
	echo ' compilation et creation du mex file de ' $@
	mex -fortran $(OPTIM) $(LIB1) ChpoToChamnog.F ddreal8toint.f ddinttoreal8.f $(LIBPRINCF90) $(LIBUTILF) && echo ' sauvegarde de ' $+ ; cp $+ save/.

NodesInMeshg.mexglx : NodesInMeshg.F ddreal8toint.f ddinttoreal8.f
	echo ' compilation et creation du mex file de ' $@
	mex -fortran $(OPTIM) $(LIB1) NodesInMeshg.F ddreal8toint.f ddinttoreal8.f $(LIBPRINCF90) $(LIBUTILF) $(LIB2) && echo ' sauvegarde de ' $+ ; cp $+ save/.

ProjectChamnog.mexglx : ProjectChamnog.F ddreal8toint.f ddinttoreal8.f
	echo ' compilation et creation du mex file de ' $@
	mex -fortran $(OPTIM) $(LIB1) ProjectChamnog.F ddreal8toint.f ddinttoreal8.f $(LIBPRINCF90) $(LIBUTILF) && echo ' sauvegarde de ' $+ ; cp $+ save/.

testdd3.avs : testdd3.dgibi
	echo ' generation du fichier de donnees ' $@
	$(CAST3M) testdd3.dgibi && echo ' sauvegarde de ' $+ ; cp $+ save/.

testdd4.avs : testdd4.dgibi
	echo ' generation du fichier de donnees ' $@
	$(CAST3M) testdd4.dgibi && echo ' sauvegarde de ' $+ ; cp $+ save/.

testdd5.avs : testdd5.dgibi
	echo ' generation du fichier de donnees ' $@
	$(CAST3M) testdd5.dgibi && echo ' sauvegarde de ' $+ ; cp $+ save/.

# nouveaux tests:

dlapst.o :
#	$(FC) -C -FI -I../d-LibUtilf -c dlapst.f -o dlapst.o
	$(FC) -O2 -FI -I../d-LibUtilf -c dlapst.f -o dlapst.o

testam1a.o :
#	$(FC) -C -I../d-LibUtilf -c testam1a.f90 -o testam1a.o  
	$(FC) -O2 -I../d-LibUtilf -c testam1a.f90 -o testam1a.o

testam1a: testam1a.o dlapst.o
	echo ' edition de lien pour ' $@
	ifort $(OPTIL) -o testam1a testam1a.o dlapst.o $(LIBPRINCF90) $(LIBUTILF) $(LIB1) $(END)
	echo ' edition de lien terminee'

clean_testam1a:
	rm testam1a.o testam1a

ChamPtToChamNo.o:
#	$(FC) -C -c ChamPtToChamNo.o ChamPtToChamNo.f90
	$(FC) -O2 -c ChamPtToChamNo.o ChamPtToChamNo.f90

testam2.o :
#	$(FC) -C -I../d-LibUtilf -c testam2.f90 -o testam2.o  
	$(FC) -O2 -I../d-LibUtilf -c testam2.f90 -o testam2.o  

testam2: testam2.o dlapst.o ChamPtToChamNo.o
	echo ' edition de lien pour ' $@
	ifort $(OPTIL) -I. -o testam2 testam2.o dlapst.o ChamPtToChamNo.o $(LIBPRINCF90) $(LIBUTILF) $(LIB1) $(END)
	echo ' edition de lien terminee'

clean_testam2:
	rm testam2.o testam2
