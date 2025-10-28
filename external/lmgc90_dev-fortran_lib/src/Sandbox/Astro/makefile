
all: gravity wrap_gravity

gravity:
	cd $(TOP)/src; \
	make -f makefile -e "FC=$(F)" "FFLAGS=$(FFLAGS) $(MODOPT)$(TOP)/modules" "TOP=$(TOP)/src" 

wrap_gravity:$(FIRST)
	cd $(TOP)/swig; \
	make -f makefile -e "FC=$(F)" "FFLAGS=$(FFLAGS) -I$(TOP)/modules $(MODOPT)$(TOP)/modules" "LIBS=$(LIBS)" "LIBS_PATH=$(LIBS_PATH)" "TOP=$(TOP)/swig" "CPP_LIBS_PATH=$(CPP_LIBS_PATH)" "CPP_LIBS=$(CPP_LIBS)" "PYTHON_INC=$(PYTHON_INC)" "NUMPY_INC=$(NUMPY_INC)" swig

############# nettoyage #######################"

clean: clean_gravity clean_wrap_gravity
	rm -f modules/*
	rm -f lib/*

clean_gravity:
	cd $(TOP)/src; \
	make -f makefile -e "TOP=$(TOP)/src" clean

clean_wrap_gravity:
	cd $(TOP)/swig; \
	make -f makefile -e "TOP=$(TOP)/swig" clean;

