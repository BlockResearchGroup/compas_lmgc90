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
# pour gfortran:
#OPTIF90 = -O2 -ffixed-form
# pour ifort:
OPTIF90 = -O2
#OPTIF90 = -C
# pour g77:
#OPTIF = -O2
# pour ifort:
OPTIF = -FI -O2
#OPTIF = -FI -C
# debug, pour g77
#OPTIF = -g
# optimisation pour le compilateur c
OPTIC = -O
OPTIL = $(OPTIF)

# For savings
SAUVE = cp
#SAUVE = echo
DSAUV = save

CC  = cc
#F77 = g77
F77 = ifort
#F90 = gfortran
F90 = ifort 
# edition lien pour une compilation g77/gfortran
#LNK = g77
# edition de liens pour une compilation avec ifort
LNK = ifort
LIB = LibUtilf.a
SOURCE = DistToBox.f DistToSimplex.f ianul.f NodesInSimplex.f NodesInSimplex2.f ProjectChamno.f traces.f zdanul.f place2.f NodesRefCoor.f ElementToSimplexes.f ElementToSimplexesNB.f ShapeFunctionValues.f
OBJLIB = $(SOURCE:.f=.o)

default : $(LIB) \
#	echo 'gmake -r '$(LIB)
#	echo 'gmake -j '$(NPROC)' -r '$(LIB)
# on copie la lib
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
