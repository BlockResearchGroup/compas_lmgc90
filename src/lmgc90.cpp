#include "compas.h"
#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/eigen/dense.h>
#include <nanobind/stl/bind_vector.h>
#include <nanobind/stl/vector.h>
#include <vector>
#include <iostream>
#include "lmgc90_solver_interface.h"  // Include the Fortran interface

typedef std::vector<nb::ndarray<double>> VectorNumpyArrayDouble;
typedef std::vector<nb::ndarray<int>> VectorNumpyArrayInt;

NB_MAKE_OPAQUE(VectorNumpyArrayDouble);
NB_MAKE_OPAQUE(VectorNumpyArrayInt);

void solve(
    double gravity,
    const Eigen::Ref<const Eigen::Matrix<bool, Eigen::Dynamic, 1>>& is_support,
    const Eigen::Ref<const Eigen::VectorXi>& nodes,
    const Eigen::Ref<const Eigen::MatrixXi>& edges,
    VectorNumpyArrayDouble& vertices_arrays,
    VectorNumpyArrayInt& faces_arrays
) {
    std::cout << "LMGC90 solve" << std::endl;
    
    if (vertices_arrays.empty()) {
        std::cerr << "Error: No vertex arrays provided" << std::endl;
        return;
    }
    
    nb::ndarray<double> vertices = vertices_arrays[0];
    if (vertices.ndim() != 2 || vertices.shape(1) != 3) {
        std::cerr << "Error: Vertices array must be of shape (n_vertices, 3)" << std::endl;
        return;
    }
    
    // Prepare data for Fortran
    const bool* is_support_data = is_support.data();
    const int* nodes_data = nodes.data();
    const int* edges_data = edges.data();
    int is_support_size = static_cast<int>(is_support.size());
    int nodes_size = static_cast<int>(nodes.size());
    int edges_rows = static_cast<int>(edges.rows());
    int edges_cols = static_cast<int>(edges.cols());
    
    // Prepare vertex arrays data
    std::vector<void*> vertices_ptrs(vertices_arrays.size());
    std::vector<int> vertices_rows(vertices_arrays.size());
    std::vector<int> vertices_cols(vertices_arrays.size());
    
    for (size_t i = 0; i < vertices_arrays.size(); i++) {
        nb::ndarray<double> vertices = vertices_arrays[i];
        if (vertices.ndim() == 2 && vertices.shape(1) == 3) {
            vertices_ptrs[i] = vertices.data();
            vertices_rows[i] = static_cast<int>(vertices.shape(0));
            vertices_cols[i] = static_cast<int>(vertices.shape(1));
        } else {
            vertices_ptrs[i] = nullptr;
            vertices_rows[i] = 0;
            vertices_cols[i] = 0;
        }
    }
    
    // Call Fortran solver
    fortran_solve(
        gravity,
        (void*)is_support_data, is_support_size,
        (void*)nodes_data, nodes_size,
        (void*)edges_data, edges_rows, edges_cols,
        vertices_ptrs.data(), static_cast<int>(vertices_ptrs.size()),
        vertices_rows.data(), vertices_cols.data()
    );
}

NB_MODULE(_lmgc90, m) {
    m.doc() = "LMGC90 binding.";
    
    nb::bind_vector<VectorNumpyArrayDouble>(m, "VectorNumpyArrayDouble");
    nb::bind_vector<VectorNumpyArrayInt>(m, "VectorNumpyArrayInt");

    m.def("solve", 
        &solve, 
        "gravity"_a, 
        "is_support"_a, 
        "nodes"_a, 
        "edges"_a, 
        "vertices_arrays"_a, 
        "faces_arrays"_a,
        "Solve using the LMGC90 solver with NumPy arrays"
    );
}
