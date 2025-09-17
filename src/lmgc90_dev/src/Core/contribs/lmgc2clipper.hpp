
#ifndef lmgc2clipper_h
#define lmgc2clipper_h

extern "C" void clipper_intersection(double *p1, int *n1, int size_n1, double *p2, int *n2, int size_n2, double shrink1, double shrink2, double delta, double **p3, int **n3, int & size_n3, double ** area);

extern "C" void clipper_simplification(double *pin, int nin, double delta, double **pout, int & nout)

extern "C" void clipper_pointinpolygon(double px, double py, double *pin, int *nin, int size_nin, int &result)

#endif // lmgc2clipper_h
