import numpy as np
from compas_lmgc90._fortran_bindings import multiply, translate_points

value = 10.0
factor = 2.5
result = multiply(value, factor)

points = np.array([
    [1.0, 2.0, 3.0],
    [4.0, 5.0, 6.0],
    [7.0, 8.0, 9.0],
], dtype=np.float64)

print(f"Original points:\n{points}")

translation = np.array([0.5, -1.0, 2.0], dtype=np.float64)
translate_points(points, translation)

print(f"Translated points:\n{points}")
