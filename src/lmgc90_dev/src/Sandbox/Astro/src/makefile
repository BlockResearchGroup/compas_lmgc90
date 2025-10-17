
.SUFFIXES: .f90 .o

OBJETS=gravity.o

all: $(OBJETS)

%.o: %.f90
	$(FC) -c -o $@ $(FFLAGS) $<

.f90.o:
	$(FC) -c -o $@ $(FFLAGS) $<

clean:
	@rm -f *.o *.a
