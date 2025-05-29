# COMPAS_PPC Tutorial

This README provides instructions on how to use C++ and Python files together with nanobind in this project.

## Setup Instructions

### 1. Adding nanobind Extensions to CMakeLists.txt

Add the following lines at the end of your CMakeLists.txt to enable the nanobind extensions:

```cmake
add_nanobind_extension(_vectors_copy src/vectors_copy.cpp)
add_nanobind_extension(_vectors_reference src/vectors_reference.cpp)
add_nanobind_extension(_class_primitives src/class_primitives.cpp)
add_nanobind_extension(_class_unique_pointer src/class_unique_pointer.cpp)
add_nanobind_extension(_class_shared_pointer src/class_shared_pointer.cpp)
add_nanobind_extension(_eigen src/eigen.cpp)
add_nanobind_extension(_ndarray src/ndarray.cpp)
```

### 2. C++ Implementation Files

Place your C++ implementation files in the `src` folder. These files should implement the functionality that will be exposed to Python. For example:

- `src/vectors_copy.cpp`: Demonstrates passing vectors by copy
- `src/vectors_reference.cpp`: Demonstrates passing vectors by reference
- `src/class_primitives.cpp`: Demonstrates binding classes with primitive types
- `src/class_unique_pointer.cpp`: Shows how to handle unique pointers
- `src/class_shared_pointer.cpp`: Shows how to handle shared pointers
- `src/eigen.cpp`: Demonstrates integration with Eigen library
- `src/ndarray.cpp`: Shows how to work with n-dimensional arrays

### 3. Python Usage Files

Run python examples files from `tutorial` folder. For example:

- `src/vectors_copy.py`
- `src/vectors_reference.py`
- `src/class_primitives.py`
- `src/class_unique_pointer.py`
- `src/class_shared_pointer.py`
- `src/eigen.py`
- `src/ndarray.py`

## Building and Running

   ```
   pip install --no-build-isolation -ve .
   ```

## Example Workflow

1. Write a C++ extension in `src/my_extension.cpp`
2. Add it to CMakeLists.txt: `add_nanobind_extension(_my_extension src/my_extension.cpp)`
3. Build the project
4. Create a Python script in `scripts/use_my_extension.py` that imports and uses `_my_extension`
5. Run your Python script
