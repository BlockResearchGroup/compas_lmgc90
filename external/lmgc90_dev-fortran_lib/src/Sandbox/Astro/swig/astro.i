/* declarations : en-tete d'un fichier SWIG + ajout des headers necessaires */
%{
#define SWIG_FILE_WITH_INIT

#include "wrap_gravity.h"
%}

/* directives supplementaires pour assurer le transfert d'argumlents/resultats
 * entre SWIG et numpy */
/*%include "src//home/alex/epd_py25-4.1.30101-rh3-amd64/lib/python2.5/site-packages/numpy-1.1.1.0001-py2.5-linux-x86_64.egg/numpy/doc/swig/numpy.i"*/
%include "numpy.i"
%init %{
import_array();
%}

/* directive supplementaire pour effectuer des passages d'arguments par adresse
 * ou transformer des procedures en fonctions */
%include "typemaps.i"

/* declaration du nom du module */
%module astro

/* definitions des regles a appliquer */

/* turn an input numpy array in a pointer and a size usable in C/Fortran
 */
%apply (double* IN_ARRAY1, int DIM1) {(double* masses, int mass_size)};
%apply (double* IN_FARRAY2, int DIM1, int DIM2) {(double* pos, int dim, int nb_part)};

/* length is an input, the size of the vector to get back as an output in python
 */
%apply (double* ARGOUT_ARRAY1, int DIM1) {(double* vector_out, int length)};

/* on inclut directement les fonctions definies dans le fichier d'en-tete, pour
 * quelles soient wrappees dans le module python */
%include "wrap_gravity.h"
