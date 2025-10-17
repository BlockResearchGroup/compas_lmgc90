
#ifndef umfpackBinding
#define umfpackBinding

#include "umfpack.h"

int c_symbolic(int nrow, int ncol, int * ap, int * ai, double * ax, void ** symbolic, double ** control, double * info);

int c_numeric(int * ap, int * ai, double * ax, void ** symbolic, void ** numeric, double ** control, double * info);

int c_solve(int * ap, int * ai, double * ax, double ** x, double ** b, void ** numeric, double ** control, double * info);

void c_free_symbolic(void ** symbolic);

void c_free_numeric(void ** numeric);

#endif // umfpackBinding

