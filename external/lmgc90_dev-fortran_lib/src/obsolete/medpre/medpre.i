%module medpre

%{
#define SWIG_FILE_WITH_INIT
#include "medpre.h"
%}

%include numpy.i

%init %{
import_array();
%}

%apply(double** ARGOUTVIEWM_ARRAY1,int *DIM1) {(double **_coords, int *_size)};
%apply(int   ** ARGOUTVIEWM_ARRAY1,int *DIM1) {(int    **_conn,   int *_size)};
%apply(int   ** ARGOUTVIEWM_ARRAY1,int *DIM1) {(int    **_ids,    int *_size)};

%include "medpre.h"
