#include "compas.h"
#include <nanobind/stl/bind_vector.h>


using DoubleVector = std::vector<double>;
void subtract_inplace(DoubleVector& a, const DoubleVector& b) {
    for (size_t i = 0; i < a.size(); i++) {
        a[i] = a[i] - b[i];
    }
}


NB_MODULE(_vectors_reference, m) {
    m.doc() = "Vectors by reference example.";
    nb::bind_vector<DoubleVector>(m, "DoubleVector");
    m.def("subtract_inplace", &subtract_inplace, "a"_a, "b"_a, "Subtract two vectors in place");
}
