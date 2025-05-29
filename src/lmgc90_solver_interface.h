#pragma once

#ifdef __cplusplus
extern "C" {
#endif

// Declaration of the Fortran solver function
void fortran_solve(
    double gravity,
    void* is_support_ptr, int is_support_size,
    void* nodes_ptr, int nodes_size,
    void* edges_ptr, int edges_rows, int edges_cols,
    void* vertices_arrays_ptr, int num_arrays,
    int* vertices_rows_ptr, int* vertices_cols_ptr
);

#ifdef __cplusplus
}
#endif
