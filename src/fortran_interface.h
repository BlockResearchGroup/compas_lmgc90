#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// Declaration of Fortran functions
void fortran_multiply(double input_value, double factor, double* result);
void fortran_translate_points(double* points, int num_points, double* translation);

#ifdef __cplusplus
}
#endif
