# This is part of LMGC90-v0.0 by F.Dubois & M.Jean (c) 2001 CNRS-UMII 
#
# LIBS n est declare que pour le move ...


### vient du dessus ####
FC=
FFLAGS=
CC=
CFLAGS=
FIRST=
LAPACK95=
TOP=
####

all: info lib_cell move

# Repertoires de travail

.PHONY: info lib_cell move

info:
	@echo "option de compilation :" $(FFLAGS) 


# repertoire de contribs

lib_cell:
	cd $(TOP)/src; \
	make -e -f makefile "FC=$(FC)" "FFLAGS=$(FFLAGS)" cell.a

move:
	@echo "#########################"
	@echo "on deplace les librairies"
	@rm -f $(TOP)/lib/*.a
	@cp $(TOP)/src/cell.a $(TOP)/lib
	@ranlib $(TOP)/lib/cell.a
	@echo "#########################"

# menage

clean:
	@echo "#########################"
	@echo "On nettoie les sources"
	@rm -rf $(TOP)/lib/*.a 
	@cd $(TOP)/src; \
	make -f makefile clean
	@echo "#########################"

