#include "compas.h"
#include <nanobind/nanobind.h>
#include <nanobind/ndarray.h>
#include <nanobind/eigen/dense.h>
#include <nanobind/stl/bind_vector.h>
#include <nanobind/stl/vector.h>
#include <vector>

typedef std::vector<nb::ndarray<double>> VectorNumpyArrayDouble;
typedef std::vector<nb::ndarray<int>> VectorNumpyArrayInt;

NB_MAKE_OPAQUE(VectorNumpyArrayDouble);
NB_MAKE_OPAQUE(VectorNumpyArrayInt);

void lmgc90_solve_numpy(
    double gravity, // Transfer a number 
    const Eigen::Ref<const Eigen::Matrix<bool, Eigen::Dynamic, 1>>& is_support, // A vector of booleans
    const Eigen::Ref<const Eigen::VectorXi>& nodes, // A vector of integers
    const Eigen::Ref<const Eigen::MatrixXi>& edges, // A matrix of integers
    VectorNumpyArrayDouble& vertices_arrays,  // A vector of arrays, each array has shape (n_vertices, 3)
    VectorNumpyArrayInt& faces_arrays        // A vector of arrays, each array has shape (n_faces, n_vertices_per_face)
) {
    std::cout << "LMGC90 solve (numpy version)" << std::endl;
    std::cout << "gravity: " << gravity << std::endl;

    for (size_t i = 0; i < vertices_arrays.size(); i++) {
        nb::ndarray<double> vertices = vertices_arrays[i]; 
        double* vertex_data = vertices.data();
        size_t num_vertices = vertices.shape(0); // Get the dimensions of this array
        
        // Modify all vertices
        for (size_t v = 0; v < num_vertices; v++) {
            size_t offset = v * 3;
            vertex_data[offset + 0] -= 0.0;  // x
            vertex_data[offset + 1] -= 0.0;  // y
            vertex_data[offset + 2] -= 0.5;  // z
        }

        break;
    }
}

NB_MODULE(_lmgc90, m) {
    m.doc() = "LMGC90 binding.";
    
    nb::bind_vector<VectorNumpyArrayDouble>(m, "VectorNumpyArrayDouble");
    nb::bind_vector<VectorNumpyArrayInt>(m, "VectorNumpyArrayInt");

    m.def("lmgc90_solve_numpy", 
        &lmgc90_solve_numpy, 
        "gravity"_a, 
        "is_support"_a, 
        "nodes"_a, 
        "edges"_a, 
        "vertices_arrays"_a, 
        "faces_arrays"_a,
        "Solve using the LMGC90 solver with NumPy arrays"
    );
}
