
#ifndef _PREDICATES_H_
#define _PREDICATES_H_

double exactinit();
double incircle(double *pa, double *pb, double *pc, double *pd);
double insphere(double *pa, double *pb, double *pc, double *pd, double *pe);
double orient2d(double *pa, double *pb, double *pc);
double orient3d(double *pa, double *pb, double *pc, double *pd);

#endif
