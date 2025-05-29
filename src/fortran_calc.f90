subroutine fortran_multiply(input_value, factor, result) bind(c, name="fortran_multiply")
    use iso_c_binding
    implicit none
    
    real(c_double), intent(in), value :: input_value
    real(c_double), intent(in), value :: factor
    real(c_double), intent(out) :: result
    
    result = input_value * factor
    
    print *, "Fortran: calculated ", result, " from ", input_value, " * ", factor
    
end subroutine fortran_multiply


subroutine fortran_translate_points(points, num_points, translation) bind(c, name="fortran_translate_points")
    use iso_c_binding
    implicit none
    
    integer(c_int), intent(in), value :: num_points
    real(c_double), intent(inout) :: points(3, num_points)  ! 3D points (x,y,z)
    real(c_double), intent(in) :: translation(3)  ! translation vector (x,y,z)
    
    integer :: i
    
    do i = 1, num_points
        points(1, i) = points(1, i) + translation(1)  ! x
        points(2, i) = points(2, i) + translation(2)  ! y
        points(3, i) = points(3, i) + translation(3)  ! z
    end do
    
    print *, "Fortran: translated", num_points, "points by", translation
    
end subroutine fortran_translate_points
