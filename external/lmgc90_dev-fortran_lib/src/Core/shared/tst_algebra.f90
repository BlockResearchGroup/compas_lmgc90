program test

  use parameters
  use algebra

  implicit none

  call test_elementary_matrix

contains     

  function test_sym_mat_elem()
    implicit none
    integer(kind=4) :: test_sym_mat_elem
    !
    type(G_elementary_matrix) :: mat, mat2
    integer(kind=4) :: order = 4
    real(kind=8), dimension(4)  :: vec1, vec2, ref
    real(kind=8), dimension(10) :: init

    integer(kind=4) :: i, j, i_store, i_shape
    real(kind=8) :: val

    test_sym_mat_elem = 0

    i_store = get_matrix_storage_id_from_name('full____')
    i_shape = get_matrix_shape_id_from_name('sym_____')
    call new_G_elementary_matrix(mat,  i_store, i_shape, order)
    call new_G_elementary_matrix(mat2, i_store, i_shape, order)

    vec1 = (/ 1., 2., 3., 4. /)
    init = (/1., 2., 3., 4., 5., 6., 7., 8., 9., 10./)
    call set_G_elementary_matrix_from_vec(mat, init)
    do i = 1, 2
      do j = i, 4
        val = 11. - (5-i)*(6-i)/2 + j - i 
        call set_term_of_G_elementary_matrix(mat2, i, j, val)
      end do
    end do
    do i = 3, 4
      do j = 1, i
        val = 11. - (5-j)*(6-j)/2 + i - j 
        call set_term_of_G_elementary_matrix(mat2, i, j, val)
      end do
    end do

    !call add_to_G_elementary_matrix(mat, reshape(init, shape=(/order,order/)), 2.d0)
    call product_G_elementary_matrix_vector(mat, vec1, vec2)

    call delete_G_elementary_matrix(mat)
    call delete_G_elementary_matrix(mat2)

    !ref = (/ 90.d0, 174.d0, 225.d0, 255.d0 /)
    ref = (/ 30.d0, 58.d0, 75.d0, 85.d0 /)
    if( any(ref/=vec2) ) then
      print *, ref
      print *, vec2
      write(*,*) "TEST ERROR[test_sym_mat_elem] : computed result is different from stored one"
      test_sym_mat_elem = 1
    end if
  end function test_sym_mat_elem

  function test_dia_mat_elem()
    implicit none
    integer(kind=4) :: test_dia_mat_elem
    !
    type(G_elementary_matrix) :: mat, mat2
    integer(kind=4) :: order = 4
    real(kind=8), dimension(4)  :: vec1, vec2, ref
    real(kind=8), dimension(10) :: init

    integer(kind=4) :: i, j, i_store, i_shape

    test_dia_mat_elem = 0

    i_store = get_matrix_storage_id_from_name('diagonal')
    i_shape = get_matrix_shape_id_from_name('std_____')
    call new_G_elementary_matrix(mat,  i_store, i_shape, order)
    call new_G_elementary_matrix(mat2, i_store, i_shape, order)

    vec1 = (/ 1., 2., 3., 4. /)
    call set_G_elementary_matrix_from_vec(mat, vec1)
    do i = 1, 4
      call set_term_of_G_elementary_matrix(mat2, i, i, vec1(i)) 
    end do

    call add_to_G_elementary_matrix(mat, reshape(vec1,(/order,1/)), 2.d0)
    call product_G_elementary_matrix_vector(mat, vec1, vec2)

    call delete_G_elementary_matrix(mat)
    call delete_G_elementary_matrix(mat2)

    ref = (/ 3.d0, 12.d0, 27.d0, 48.d0 /)
    if( any(ref/=vec2) ) then
      write(*,*) "TEST ERROR[test_dia_mat_elem] : computed result is different from stored one"
      test_dia_mat_elem = 1
    end if
  end function test_dia_mat_elem

  function test_ful_mat_elem()
    implicit none
    integer(kind=4) :: test_ful_mat_elem
    !
    type(G_elementary_matrix) :: mat, mat2
    integer(kind=4) :: order = 4
    real(kind=8), dimension(4)  :: vec1, vec2, ref
    real(kind=8), dimension(16) :: init

    integer(kind=4) :: i, j, i_store, i_shape

    test_ful_mat_elem = 0

    i_store = get_matrix_storage_id_from_name('full____')
    i_shape = get_matrix_shape_id_from_name('std_____')
    call new_G_elementary_matrix(mat,  i_store, i_shape, order)
    call new_G_elementary_matrix(mat2, i_store, i_shape, order)

    vec1 = (/ 1., 2., 3., 4. /)
    init = (/1., 2., 3., 4., 5., 6., 7., 8., 9., 10., 11., 12., 13., 14., 15., 16./)
    call set_G_elementary_matrix_from_vec(mat, init)
    call set_G_elementary_matrix_from_mat(mat2, reshape(init, shape=(/order,order/)))

    call add_to_G_elementary_matrix(mat, reshape(init, shape=(/order,order/)), 2.d0)
    call product_G_elementary_matrix_vector(mat, vec1, vec2)

    call delete_G_elementary_matrix(mat)
    call delete_G_elementary_matrix(mat2)

    ref = (/ 270.d0, 300.d0, 330.d0, 360.d0 /)
    if( any(ref/=vec2) ) then
      write(*,*) "TEST ERROR[test_ful_mat_elem] : computed result is different from stored one"
      test_ful_mat_elem = 1
    end if
  end function test_ful_mat_elem

  subroutine test_elementary_matrix
    implicit none
    integer(kind=4) :: nb_err

    nb_err = test_sym_mat_elem()
    nb_err = nb_err + test_dia_mat_elem()
    nb_err = nb_err + test_ful_mat_elem()

    if( nb_err > 0 ) STOP 1

  end subroutine test_elementary_matrix

end program test
