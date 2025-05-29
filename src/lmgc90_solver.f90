module lmgc90_solver_module
    use iso_c_binding
    implicit none

contains
    subroutine fortran_solve(gravity, is_support_ptr, is_support_size, &
                           nodes_ptr, nodes_size, &
                           edges_ptr, edges_rows, edges_cols, &
                           vertices_arrays_ptr, num_arrays, &
                           vertices_rows_ptr, vertices_cols_ptr) bind(c, name="fortran_solve")
        use iso_c_binding
        implicit none
        
        real(c_double), intent(in), value :: gravity
        type(c_ptr), intent(in), value :: is_support_ptr
        integer(c_int), intent(in), value :: is_support_size
        type(c_ptr), intent(in), value :: nodes_ptr
        integer(c_int), intent(in), value :: nodes_size
        type(c_ptr), intent(in), value :: edges_ptr
        integer(c_int), intent(in), value :: edges_rows, edges_cols
        type(c_ptr), intent(in), value :: vertices_arrays_ptr
        integer(c_int), intent(in), value :: num_arrays
        type(c_ptr), intent(in), value :: vertices_rows_ptr
        type(c_ptr), intent(in), value :: vertices_cols_ptr
        
        logical(c_bool), pointer :: is_support(:)
        integer(c_int), pointer :: nodes(:)
        integer(c_int), pointer :: edges(:,:)
        type(c_ptr), pointer :: ptr_array(:)
        integer(c_int), pointer :: rows(:), cols(:)
        real(c_double), pointer :: vertices(:,:)
        integer :: i, j
        
        print *, "Fortran says hello to COMPAS."
        print *, "Fortran: g", gravity
        
        call c_f_pointer(is_support_ptr, is_support, [is_support_size])
        call c_f_pointer(nodes_ptr, nodes, [nodes_size])
        call c_f_pointer(edges_ptr, edges, [edges_rows, edges_cols])
        call c_f_pointer(vertices_arrays_ptr, ptr_array, [num_arrays])
        call c_f_pointer(vertices_rows_ptr, rows, [num_arrays])
        call c_f_pointer(vertices_cols_ptr, cols, [num_arrays])
        
        ! Print nodes values
        print *, "Fortran: nodes", nodes
        print *, "Fortran: edges shape =", edges_rows, "x", edges_cols
        do i = 1, edges_rows ! Print all edges
            print *, "Edge", i, "=", edges(i, 1), edges(i, 2)
        end do
                
        ! Modify all vertex arrays with a fixed delta_z value of -0.5
        do i = 1, num_arrays
            call c_f_pointer(ptr_array(i), vertices, [cols(i), rows(i)])
            vertices(3, :) = vertices(3, :) + 0.5 ! Modify z-coordinate of all vertices
        end do
        
        print *, "Fortran: done"
        
    end subroutine fortran_solve
    
end module lmgc90_solver_module
