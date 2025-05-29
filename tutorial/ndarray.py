from compas_lmgc90 import _ndarray
import numpy as np
import time


def example_basic_inspection():
    """Example 1: Basic ndarray inspection"""
    a = np.array([[1, 2, 3], [3, 4, 5]], dtype=np.float32)
    _ndarray.inspect(a)


def example_process_rgb_image():
    """Example 2: Process an RGB image (doubles brightness)"""
    rgb_image = np.ones((2, 3, 3), dtype=np.uint8) * 50
    print("rgb_image", rgb_image)
    _ndarray.process(rgb_image)
    print("rgb_image", rgb_image)


def example_matrix4f_view():
    """Example 3: Matrix4f with ndarray view"""
    numpy_array = _ndarray.Matrix4f().view()
    print(type(numpy_array))
    numpy_array[0, 0] = 1.0
    print(numpy_array[0, 0])


def example_dynamic_2d_array():
    """Example 4: Dynamic 2D array with ownership"""
    dynamic_array = _ndarray.create_2d(3, 4)  # Create a 3x4 array
    print("\nDynamic 2D array:")
    print(dynamic_array)
    print("Shape:", dynamic_array.shape)
    print("Sum:", np.sum(dynamic_array))  # Should sum values from 0 to 11
    dynamic_array[1, 2] = 99.5
    print("\nModified array:")
    print(dynamic_array)


def example_multiple_arrays():
    """Example 5: Multiple arrays with shared ownership"""
    array1, array2 = _ndarray.return_multiple()
    print("Array 1 (increasing values):")
    print(array1, type(array1))
    print("Array 2 (decreasing values):")
    print(array2, type(array2))

    # Modify one of the arrays
    array1[2] = 99.9
    print("\nAfter modification:")
    print("Array 1:", array1)
    print("Array 2:", array2)


def example_vector3f():
    """Example 6: Return a 3D vector"""
    vec3 = _ndarray.return_vec3()
    vec3[2] = 99.9
    print("\nModified vector:" + str(vec3))


def example_array_view_benchmark():
    """Example 7: Fast array view optimization benchmark"""
    rows, cols = 1000, 1000
    array_regular = np.zeros((rows, cols), dtype=np.float32)
    array_optimized = np.zeros((rows, cols), dtype=np.float32)

    # Benchmark C++ regular method
    start_time = time.time()
    _ndarray.fill_array_regular(array_regular)
    regular_time = time.time() - start_time
    print(f"C++ regular: {regular_time:.6f} sec")

    # Benchmark C++ with view() optimization
    start_time = time.time()
    _ndarray.fill_array_optimized(array_optimized)
    optimized_time = time.time() - start_time
    print(f"C++ optimized: {optimized_time:.6f} sec")

    # Benchmark NumPy native method
    start_time = time.time()
    ii, jj = np.meshgrid(np.arange(rows, dtype=np.float32), np.arange(cols, dtype=np.float32), indexing="ij")
    array_numpy = ii * jj
    numpy_time = time.time() - start_time
    print(f"NumPy native: {numpy_time:.6f} sec")

    # Simple comparison
    print(f"\nSpeed comparison:")
    print(f"C++ regular vs optimized: {regular_time / optimized_time:.2f}x")
    print(f"C++ regular vs NumPy: {regular_time / numpy_time:.2f}x")


def example_specialized_views():
    """Example 8: Runtime specialized views"""
    float_array = np.zeros((5, 5), dtype=np.float32)
    int_array = np.zeros((5, 5), dtype=np.int32)
    float_3d_array = np.zeros((3, 3, 3), dtype=np.float32)

    print(f"Float array: {_ndarray.fill_array_specialized(float_array)}")
    print(f"Int array: {_ndarray.fill_array_specialized(int_array)}")
    print(f"3D array: {_ndarray.fill_array_specialized(float_3d_array)}")

    print("Float array result:")
    print(float_array)
    print("Int array result:")
    print(int_array)
    print("3D array (unchanged):")
    print(float_3d_array)


example_basic_inspection()
example_process_rgb_image()
example_matrix4f_view()
example_dynamic_2d_array()
example_multiple_arrays()
example_vector3f()
example_array_view_benchmark()
example_specialized_views()
