#include "compas.h"
#include <nanobind/stl/vector.h>


std::vector<double> add(std::vector<double> a, std::vector<double> b) {
    std::vector<double> result;
    for (size_t i = 0; i < a.size(); i++) {
        result.push_back(a[i] + b[i]);
    }
    return result;
}


void subtract(std::vector<double>& a, std::vector<double>& b) {
    for (size_t i = 0; i < a.size(); i++) {
        a[i] -= b[i];
    }
}

NB_MODULE(_vectors_copy, m) {
    m.doc() = "Vectors example.";
    m.def("add", &add, "a"_a, "b"_a, "Add two vectors");
    m.def("subtract", &subtract, "a"_a, "b"_a, "Subtract two vectors");
}
