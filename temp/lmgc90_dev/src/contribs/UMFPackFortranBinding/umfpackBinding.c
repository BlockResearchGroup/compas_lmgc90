
#include "stdio.h"

#include "umfpackBinding.h"


int c_symbolic(int nrow, int ncol, int * ap, int * ai, double * ax, void ** symbolic, double ** control, double * info)
{
  int i;
  umfpack_di_defaults(*control);
  return umfpack_di_symbolic (nrow, ncol, ap, ai, ax, symbolic, *control, info) ;
}

int c_numeric(int * ap, int * ai, double * ax, void ** symbolic, void ** numeric, double ** control, double * info)
{
  return umfpack_di_numeric(ap, ai, ax, *symbolic, numeric, *control, info) ;
}

int c_solve(int * ap, int * ai, double * ax, double ** x, double ** b, void ** numeric, double ** control, double * info)
{
  return umfpack_di_solve(UMFPACK_Aat, ap, ai, ax, *x, *b, *numeric, *control, info) ;
}

void c_free_symbolic(void ** symbolic)
{
  return umfpack_di_free_symbolic(symbolic);
}

void c_free_numeric(void ** numeric)
{
  return umfpack_di_free_numeric(numeric);
}

