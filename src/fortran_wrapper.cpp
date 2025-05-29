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
    // Validate input arrays
    if (points.ndim() != 2 || points.shape(1) != 3) {
        throw std::runtime_error("Points array must have shape (n_points, 3)");
    }
    
    if (translation.ndim() != 1 || translation.shape(0) != 3) {
        throw std::runtime_error("Translation vector must have shape (3,)");
    }
    
    // Get array data
    double* points_data = points.data();
    double* translation_data = translation.data();
    int num_points = static_cast<int>(points.shape(0));
    
    // Reshape for column-major (Fortran) layout
    // This is tricky because we need to account for row-major vs column-major differences
    
    // Option 1: Transpose the data in memory (inefficient for large arrays)
    double* fortran_points = new double[num_points * 3];
    
    // Transpose from row-major (points[i][0,1,2]) to column-major (fortran_points[0,1,2][i])
    for (int i = 0; i < num_points; i++) {
        for (int j = 0; j < 3; j++) {
            fortran_points[j * num_points + i] = points_data[i * 3 + j];
        }
    }
    
    // Call Fortran function
    fortran_translate_points(fortran_points, num_points, translation_data);
    
    // Copy data back with transposition
    for (int i = 0; i < num_points; i++) {
        for (int j = 0; j < 3; j++) {
            points_data[i * 3 + j] = fortran_points[j * num_points + i];
        }
    }
    
    delete[] fortran_points;
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
