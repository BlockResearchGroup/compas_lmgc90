#include "fortran_interface.h"
#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <iostream>

namespace nb = nanobind;

double multiply(double input_value, double factor) {
    double result;
    fortran_multiply(input_value, factor, &result);
    return result;
}

void translate_points(nb::ndarray<double> points, const nb::ndarray<double> translation) {
    double* points_data = points.data();
    double* translation_data = translation.data();
    int num_points = static_cast<int>(points.shape(0));
    fortran_translate_points(points_data, num_points, translation_data);
}

NB_MODULE(_fortran_bindings, m) {
    m.doc() = "Fortran bindings for compas_lmgc90";
    
    m.def("multiply", 
        &multiply, 
        "Multiply a value by a factor using Fortran",
        nb::arg("input_value"), 
        nb::arg("factor"));
    
    m.def("translate_points", 
        &translate_points, 
        "Translate an array of 3D points",
        nb::arg("points"), 
        nb::arg("translation"));
}
