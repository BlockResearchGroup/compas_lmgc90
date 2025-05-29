from compas_lmgc90 import _eigen
import numpy as np


# Matrix examples
def create_colmajor_matrix():
    return _eigen.colmajor(3, 4)  # Column-major (Eigen default)


def create_rowmajor_matrix():
    return _eigen.rowmajor(3, 4)  # Row-major


def test_layout_specific(col_mat, row_mat):
    # Original matrices created in C++
    _eigen.colmajor_only(col_mat)  # Only accepts column-major
    _eigen.rowmajor_only(row_mat)  # Only accepts row-major


def test_layout_specific_numpy():
    # Create matrices directly with NumPy using different memory layouts
    np_col = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.float32, order="F")  # Column-major layout
    np_row = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.float32, order="C")  # Row-major layout (default)

    # Pass to layout-specific functions
    _eigen.colmajor_only(np_col)  # Works with column-major NumPy array
    _eigen.rowmajor_only(np_row)  # Works with row-major NumPy array

    return np_col, np_row


def modify_matrix():
    matrix = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.float32, order="F")  # Column-major
    _eigen.modify(matrix)  # Modifies element [0,0] to 99
    return matrix


# Vector examples
def create_vector(size=5):
    return _eigen.vector(size)


def modify_vector():
    vec = np.array([1, 2, 3, 4, 5], dtype=np.float32)
    _eigen.vector_modify(vec)  # Modifies element [0] to 99
    return vec


# Eigen::Map examples
def map_vector_example():
    vector = np.array([1, 2, 3, 4, 5], dtype=np.float32)
    _eigen.map_vector(vector)  # Doubles all values
    return vector


def map_matrix_example():
    matrix = np.array([[1, 2, 3], [4, 5, 6]], dtype=np.float32)
    _eigen.map_matrix(matrix)  # Doubles all values
    return matrix


# Matrix examples using matrices created in C++
col_mat = create_colmajor_matrix()
row_mat = create_rowmajor_matrix()
test_layout_specific(col_mat, row_mat)

# Matrix examples using NumPy arrays directly
np_col, np_row = test_layout_specific_numpy()
modified_matrix = modify_matrix()

# Vector examples
vector = create_vector()
modified_vector = modify_vector()

# Map examples
mapped_vector = map_vector_example()
mapped_matrix = map_matrix_example()

# Print results
print(f"Modified matrix[0,0]: {modified_matrix[0, 0]}")  # Should be 99
print(f"Modified vector[0]: {modified_vector[0]}")  # Should be 99
print(f"Mapped vector[0]: {mapped_vector[0]}")  # Should be 2 (doubled)
print(f"Mapped matrix[0,0]: {mapped_matrix[0, 0]}")  # Should be 2 (doubled)
