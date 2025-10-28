# [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
#     Mises a jour
# [][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][][]
#     DUREISSEIX David  L.M.T. STRUCTURES et SYSTEMES  le 27 / 12 / 2007
#
#     faire 'make' pour avoir la doc
# ======================================================================

# Shell a utiliser
SHELL = /bin/sh

# Debug
.SILENT:

# Pour le batch... on continue
#.IGNORE:

# Extensions non standard
.SUFFIXES: .f90 .f .c .o .a

# Pour une compilation en parallele
NPROC = 1

# Options
# options pour gfortran
#OPTIF90 = -O2 -ffixed-form
# options pour ifort
OPTIF90 = -FI -O2
#OPTIF90 = -C
# options pour g77
#OPTIF = -O2
# options pour ifort
OPTIF = -FI -O2
#OPTIF = -FI -C
# options pour le compilateur C
OPTIC = -O
OPTIL = $(OPTIF)

# For savings
SAUVE = cp
#SAUVE = echo
DSAUV = save

CC  = cc
# compilation du fortran 77 avec g77
#F77 = g77
# compilation du fortran 77 avec ifort
F77 = ifort
# compilation du fortran 90 avec gfortran
#F90 = gfortran
# compilation du fortran 90 avec ifort
F90 = ifort
# edition de liens pour une compilation g77/gfortran
#LNK = g77
# edition de liens pour une compilation avec ifort
F90 = ifort

LIB = LibPrincf90.a
SOURCE = ChpoToChamno.f90 NodesInElement.f90 NodesInMesh.f90
OBJLIB = $(SOURCE:.f90=.o)

default : $(LIB) \
#	echo 'gmake -r '$(LIB)
#	echo 'gmake -j '$(NPROC)' -r '$(LIB)
# on copie la lib au bon endroit
	cp $(LIB) ../../lib

.f90.o :;
	echo ' compilation de ' $*.f90
	$(F90) $(OPTIF90) -c $*.f90 -o $*.o && echo ' sauvegarde de ' $*.f90 ; $(SAUVE) $*.f90 $(DSAUV)/$*.f90

.f.o :;
	echo ' compilation de ' $*.f
	$(F77) $(OPTIF) -c $*.f -o $*.o && echo ' sauvegarde de ' $*.f ; $(SAUVE) $*.f $(DSAUV)/$*.f

.c.o :;
	echo ' compilation de ' $*.c
	$(CC) $(OPTIC) -c $*.c -o $*.o && echo ' sauvegarde de ' $*.c ; $(SAUVE) $*.c $(DSAUV)/$*.c

# Pour creer une librairie, on passe par liste_a pour les dependances
(%.o) : %.o
	echo $*.o >> liste_a

fichier_a::
	rm -f liste_a
	touch liste_a

liste_a: $(LIB)($(OBJLIB))

$(LIB): fichier_a liste_a
	if [ -s liste_a ]; then \
	echo ' creation de l archive ' $@ ; \
	ar crv $@ `cat liste_a` && rm `cat liste_a` ; rm -f liste_a ; \
	fi
	echo ' creation de l archive terminee '
	$(SAUVE) Makefile $(DSAUV)/Makefile

#	split -l 400 liste_a liste_a ;\
#	(for i in `ls liste_a??`; do ar rv $@ `cat $$i`; done) && (for i in `ls liste_a??`; do rm -f `cat $$i`; done) ; rm -f liste_a ;\
#	rm -f liste_a?? ;\

clean:
	rm *.a
