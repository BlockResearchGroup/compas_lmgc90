// https://nanobind.readthedocs.io/en/latest/ndarray.html
// https://github.com/wjakob/nanobind/blob/master/tests/test_ndarray.cpp
// https://github.com/wjakob/nanobind/blob/master/tests/test_ndarray.py
#include "compas.h"
#include <nanobind/ndarray.h>
#include <algorithm> // For std::min
#include <cmath>    // For sin, cos, sqrt

// Define RGBImage type for MxNx3 RGB image arrays on CPU
using RGBImage = nb::ndarray<uint8_t, nb::shape<-1, -1, 3>, nb::device::cpu>;

/**
 * Inspect and print details about a numpy ndarray
 * @param a The ndarray to inspect
 */
void inspect_ndarray(const nb::ndarray<> &a) {
    printf("Array data pointer: %p\n", a.data());
    printf("Array dimension : %zu\n", a.ndim());
    printf("Array shape : ");
    for (size_t i = 0; i < a.ndim(); i++) {
        printf("Array dimension [%zu] : %zu\n", i, a.shape(i));
        printf("Array strides [%zu] : %lld\n", i, a.stride(i));
    }

    printf("Device ID = %u\n", a.device_id());
    
    // Check if these device types exist in your version of nanobind
    printf("Device ID = %u (cpu=%i, cuda=%i)\n", a.device_id(),
    int(a.device_type() == nb::device::cpu::value),
    int(a.device_type() == nb::device::cuda::value));
    
    printf("Array dtype: int16=%i, uint32=%i, float32=%i\n",
        a.dtype() == nb::dtype<int16_t>(),
        a.dtype() == nb::dtype<uint32_t>(),
        a.dtype() == nb::dtype<float>());
}

/**
 * Process an RGB image by doubling its brightness
 * @param data Input/output RGB image with shape MxNx3
 */
void process_rgb_image(RGBImage data) {
    // Double brightness of the MxNx3 RGB image
    for (size_t y = 0; y < data.shape(0); ++y)
        for (size_t x = 0; x < data.shape(1); ++x)
            for (size_t ch = 0; ch < 3; ++ch)
                data(y, x, ch) = (uint8_t) std::min(255, data(y, x, ch) * 2);
}

// Define a simple 4x4 matrix structure
struct Matrix4f { 
    float m[4][4] = {}; // Initialize with zeros
};

/**
 * Create a view of the Matrix4f as a NumPy array
 * @param matrix Reference to the Matrix4f instance
 * @return NumPy array view of the matrix
 */
nb::ndarray<float, nb::numpy, nb::shape<4, 4>> matrix4f_view(Matrix4f &matrix) {
    // Create a numpy array view of the 4x4 matrix
    // We need to use the correct constructor signature as per the error message
    // The constructor takes the data pointer directly
    return nb::ndarray<float, nb::numpy, nb::shape<4, 4>>(matrix.m);
}

/**
 * Create a dynamic 2D array with sequential values
 * @param rows Number of rows
 * @param cols Number of columns
 * @return NumPy array with values from 0 to rows*cols-1
 */
nb::ndarray<nb::numpy, float, nb::ndim<2>> create_2d_array(size_t rows, size_t cols) {
    // Allocate a memory region and initialize it
    float *data = new float[rows * cols];
    for (size_t i = 0; i < rows * cols; ++i)
        data[i] = (float) i;

    // It creates a Python object that "owns" a C++ pointer and handles its lifetime.
    nb::capsule owner(data, [](void *p) noexcept {
        delete[] (float *) p;
    });

    // The constructor requires an initializer_list, not a std::vector
    // Pass the shape directly as an initializer list
    return nb::ndarray<nb::numpy, float, nb::ndim<2>>(
        data,                  // Data pointer
        {rows, cols},          // Shape as initializer_list
        owner                  // Owner capsule
    );
}

// Define a struct to hold multiple data arrays that will be shared
struct SharedArrays {
    std::vector<float> vec_1;
    std::vector<float> vec_2;
};


/**
 * Create and return multiple arrays that share ownership of a single data structure
 * @return A tuple of NumPy arrays with shared ownership
 */
nb::tuple return_multiple_arrays() {
    // Create vectors with example data
    std::vector<float> increasing(5);
    for (size_t i = 0; i < increasing.size(); ++i) {
        increasing[i] = static_cast<float>(i);
    }

    std::vector<float> decreasing(10);
    for (size_t i = 0; i < decreasing.size(); ++i) {
        decreasing[i] = static_cast<float>(decreasing.size() - i - 1);
    }

    // Create a shared data structure and move the vectors into it
    SharedArrays *shared = new SharedArrays();
    shared->vec_1 = std::move(increasing);
    shared->vec_2 = std::move(decreasing);

    // Create a capsule that will delete the shared data when all arrays are gone
    nb::capsule deleter(shared, [](void *p) noexcept {
        delete static_cast<SharedArrays *>(p);
    });

    // Get sizes for the arrays
    size_t size_1 = shared->vec_1.size();
    size_t size_2 = shared->vec_2.size();

    // Return a Python tuple containing both arrays directly
    return nb::make_tuple(
        nb::ndarray<nb::numpy, float>(shared->vec_1.data(), { size_1 }, deleter),
        nb::ndarray<nb::numpy, float>(shared->vec_2.data(), { size_2 }, deleter)
    );
}

using Vector3f = nb::ndarray<float, nb::numpy, nb::shape<3>>;

Vector3f return_vec3() {
    float data[] { 1, 2, 3 };
    // Perfect.
    return Vector3f(data);
}

/**
 * Demonstrates the fast array view optimization for efficient array access
 * Fills an array with the product of its indices (i*j)
 * @param arr A 2D array to fill with data
 */
void fill_array_optimized(nb::ndarray<float, nb::ndim<2>, nb::c_contig, nb::device::cpu> arr) {
    // Create a view - this is a small data structure that can be held in CPU registers
    auto view = arr.view();
    
    // Access the array through the view for better performance
    for (size_t i = 0; i < view.shape(0); ++i) {
        for (size_t j = 0; j < view.shape(1); ++j) {
            // Fill with product of indices
            view(i, j) = static_cast<float>(i * j);
        }
    }
}

/**
 * Same functionality as fill_array_optimized but without using the view optimization
 * @param arr A 2D array to fill with data
 */
void fill_array_regular(nb::ndarray<float, nb::ndim<2>, nb::c_contig, nb::device::cpu> arr) {
    // Access the array directly which may be slower due to indirection
    for (size_t i = 0; i < arr.shape(0); ++i) {
        for (size_t j = 0; j < arr.shape(1); ++j) {
            // Fill with product of indices
            arr(i, j) = static_cast<float>(i * j);
        }
    }
}

/**
 * Demonstrates runtime specialization of array views based on type checking
 * This function accepts arrays of any type and dimensionality, but creates optimized views
 * for 2D float arrays and 2D int32 arrays with different filling patterns
 * 
 * @param arr Generic input array (can be any type or dimension)
 * @return Information about what operation was performed
 */
std::string fill_array_specialized(nb::ndarray<nb::c_contig, nb::device::cpu> arr) {
    // Check if the array is a 2D float array at runtime
    if (arr.dtype() == nb::dtype<float>() && arr.ndim() == 2) {
        // Create a specialized view only when we know it's safe to do so
        auto view = arr.view<float, nb::ndim<2>>();
        
        // Fill with a custom pattern (i*j + 0.5)
        for (size_t i = 0; i < view.shape(0); ++i) {
            for (size_t j = 0; j < view.shape(1); ++j) {
                view(i, j) = static_cast<float>(i * j) + 0.5f;
            }
        }
        return "Used specialized 2D float view";
    } 
    else if (arr.dtype() == nb::dtype<int32_t>() && arr.ndim() == 2) {
        // Create a specialized view for int32 2D arrays
        auto view = arr.view<int32_t, nb::ndim<2>>();
        
        // Fill with a different pattern for integers (i+j)
        for (size_t i = 0; i < view.shape(0); ++i) {
            for (size_t j = 0; j < view.shape(1); ++j) {
                view(i, j) = static_cast<int32_t>(i + j);
            }
        }
        return "Used specialized 2D int32 view";
    }
    else {
        // Fallback for unsupported types/dimensions
        return "Unsupported array type or dimension";
    }
}



NB_MODULE(_ndarray, m) {
    m.doc() = "NDArray example.";

    // Bind the inspect function
    m.def("inspect", &inspect_ndarray, "Inspect and print details about a numpy ndarray");
    
    // Bind the process function for RGB images
    m.def("process", &process_rgb_image, "Process an RGB image by doubling its brightness");
    
    // Bind the Matrix4f class
    nb::class_<Matrix4f>(m, "Matrix4f")
        .def(nb::init<>())
        .def("view", &matrix4f_view, nb::rv_policy::reference_internal);
    
    // Create a dynamic 2D array with custom memory management
    m.def("create_2d", &create_2d_array, 
          "Create a 2D array with sequential values from 0 to rows*cols-1");
          
    // Return multiple arrays with shared ownership
    m.def("return_multiple", &return_multiple_arrays,
          "Return multiple arrays with shared ownership of a single data structure");
          
    // Return a Vector3f with preserved type signature using cast()
    m.def("return_vec3", []{
        // Call our standalone function
        Vector3f result = return_vec3();
        // Use .cast() to preserve the type signature in Python
        return result.cast();
    }, "Return a NumPy array with shape (3,) and float32 dtype");
    
    // Add the optimized array filling functions
    m.def("fill_array_optimized", &fill_array_optimized,
          "Fill a 2D array with products of indices using optimized view access");
    m.def("fill_array_regular", &fill_array_regular,
          "Fill a 2D array with products of indices using regular access");
          
    // Add runtime-specialized view function
    m.def("fill_array_specialized", &fill_array_specialized,
          "Fill arrays with specialized patterns based on runtime type checking");
}
