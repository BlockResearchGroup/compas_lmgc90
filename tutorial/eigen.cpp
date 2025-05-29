// https://github.com/wjakob/nanobind/blob/master/tests/test_eigen.cpp
// https://github.com/wjakob/nanobind/blob/master/tests/test_eigen.py
#include "compas.h"
#include <nanobind/eigen/dense.h>

/**
 * Type aliases for cleaner code and better readability
 * These types simplify working with Eigen matrices/vectors with specific memory layouts
 */
// Matrix types
using ColMatrixXf = Eigen::MatrixXf; // Default is column-major
using RowMatrixXf = Eigen::Matrix<float, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>;

// Vector types
using VectorXf = Eigen::VectorXf;       // Column vector (Nx1)
using RowVectorXf = Eigen::RowVectorXf; // Row vector (1xN)

/**
 * Create a column-major matrix (Eigen default)
 * @param rows Number of rows in the matrix
 * @param cols Number of columns in the matrix
 * @return Column-major matrix filled with sequential values
 */
ColMatrixXf matrix_colmajor(int rows, int cols) {
    ColMatrixXf mat(rows, cols);
    int count = 0;
    for (int c = 0; c < cols; c++) {
        for (int r = 0; r < rows; r++) {
            mat(r, c) = ++count;
        }
    }
    return mat;
}

/**
 * Create a row-major matrix
 * @param rows Number of rows in the matrix
 * @param cols Number of columns in the matrix
 * @return Row-major matrix filled with sequential values
 */
RowMatrixXf matrix_rowmajor(int rows, int cols) {
    RowMatrixXf mat(rows, cols);
    int count = 0;
    for (int r = 0; r < rows; r++) {
        for (int c = 0; c < cols; c++) {
            mat(r, c) = ++count;
        }
    }
    return mat;
}

/**
 * Accept only column-major matrices
 * @param x Column-major matrix reference, will reject row-major matrices
 */
void matrix_colmajor_only(const Eigen::Ref<const ColMatrixXf>& x) {
    printf("Column-major matrix %ldx%ld\n", x.rows(), x.cols());
}

/**
 * Accept only row-major matrices
 * @param x Row-major matrix reference, will reject column-major matrices
 */
void matrix_rowmajor_only(const Eigen::Ref<const RowMatrixXf>& x) {
    printf("Row-major matrix %ldx%ld\n", x.rows(), x.cols());
}

/**
 * Zero-copy for any layout using DRef
 * @param x Matrix reference that accepts any memory layout without copying
 */
void matrix_dref(const nb::DRef<Eigen::MatrixXf>& x) {
    printf("Matrix %ldx%ld\n", x.rows(), x.cols());
}

/**
 * Modify a matrix element in-place (zero-copy)
 * @param x Non-const matrix reference that can be modified
 */
void matrix_modify(Eigen::Ref<ColMatrixXf> x) {
    // Correct syntax for modifying a matrix element
    x(0, 0) = 99.0f;
    printf("Modified matrix[0,0] = %.1f\n", x(0, 0));
}

/**
 * Safe sum with explicit return type for proper evaluation
 * @param a First matrix to sum
 * @param b Second matrix to sum
 * @return Result of matrix addition, properly evaluated for zero-copy return
 */
ColMatrixXf matrix_sum(const ColMatrixXf& a, const ColMatrixXf& b) {
    return a + b;
}

//====================== VECTOR OPERATIONS ======================//

/**
 * Create a vector and return it efficiently (zero-copy)
 * @param size Size of the vector to create
 * @return Column vector filled with sequential values, transferred to Python without copying
 */
VectorXf vector_create(int size) {
    VectorXf vec(size);
    for (int i = 0; i < size; i++) {
        vec(i) = i + 1.0f;
    }
    // Zero-copy when returned by value
    return vec;
}

/**
 * Zero-copy modify vector in-place (uses non-const reference)
 * @param v Vector reference that can be modified, changes reflect in Python
 */
void vector_modify(Eigen::Ref<VectorXf> v) {
    // Directly modifies the input vector
    if (v.size() > 0) {
        v(0) = 99.0f;
        printf("Modified vector[0] = %.1f\n", v(0));
    }
}

//====================== MAP OPERATIONS ======================//

/**
 * Map a 1D array to an Eigen vector and modify it in-place
 * @param array NumPy array to map to an Eigen vector
 * @throws std::runtime_error if the array is not 1D
 */
void map_vector(nb::ndarray<float> array) {
    if (array.ndim() != 1) {
        throw std::runtime_error("Expected a 1D array");
    }
    
    // Map NumPy array to Eigen vector (zero-copy)
    Eigen::Map<VectorXf> vec(array.data(), array.shape(0));
    
    // Modify the vector (changes reflect in NumPy array)
    vec *= 2.0f;
}

/**
 * Map a 2D array to an Eigen matrix and modify it in-place
 * @param array NumPy array to map to an Eigen matrix
 * @throws std::runtime_error if the array is not 2D
 */
void map_matrix(nb::ndarray<float> array) {
    if (array.ndim() != 2) {
        throw std::runtime_error("Expected a 2D array");
    }
    
    // Map NumPy array to Eigen matrix (zero-copy)
    Eigen::Map<Eigen::MatrixXf> mat(array.data(), array.shape(0), array.shape(1));
    
    // Modify the matrix (changes reflect in NumPy array)
    mat *= 2.0f;
}

NB_MODULE(_eigen, m) {
    m.doc() = "Eigen matrix and vector examples with efficient zero-copy operations";
    
    // Matrix functions
    m.def("colmajor", &matrix_colmajor, "Create a column-major matrix",
          nb::arg("rows") = 3, nb::arg("cols") = 4);
    m.def("rowmajor", &matrix_rowmajor, "Create a row-major matrix",
          nb::arg("rows") = 3, nb::arg("cols") = 4);
    m.def("colmajor_only", &matrix_colmajor_only, "Accept only column-major matrices");
    m.def("rowmajor_only", &matrix_rowmajor_only, "Accept only row-major matrices");
    m.def("dref", &matrix_dref, "Zero-copy for any layout using DRef");
    m.def("modify", &matrix_modify, "Modify a matrix element (non-const reference)");
    m.def("sum", &matrix_sum, "Sum two matrices");
    
    // Vector functions - minimal zero-copy examples
    m.def("vector", &vector_create, "Create a vector (zero-copy return)",
          nb::arg("size") = 5);
    m.def("vector_modify", &vector_modify, "Modify a vector in-place (zero-copy)");
    
    // Map functions - direct memory access examples
    m.def("map_vector", &map_vector, "Map 1D array to Eigen vector and modify in-place");
    m.def("map_matrix", &map_matrix, "Map 2D array to Eigen matrix and modify in-place");
}
