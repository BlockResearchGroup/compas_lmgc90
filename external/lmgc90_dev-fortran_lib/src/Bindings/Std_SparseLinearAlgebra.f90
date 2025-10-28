
module SparseLinearAlgebra
  implicit none
  private

  type, public :: G_sparse_matrix
     private
     logical :: empty = .true.
     !type(dmumps_struc) :: mumps_par    

  end type G_sparse_matrix 

  logical, parameter, public :: sparse_storage_available = .false.

  public sparse_declare, &
         sparse_build,   &
         sparse_solve,   &
         sparse_erase

  contains

  subroutine sparse_declare(matrix,nb_dofs, nb_non_zero, i_indices, j_indices, is_sym, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    integer(kind=4)                        :: nb_dofs, nb_non_zero, info
    integer(kind=4), dimension(:), pointer :: i_indices
    integer(kind=4), dimension(:), pointer :: j_indices
    logical                                :: is_sym
    !
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'SparseLinearAlgebra::sparse_declare'

    print*,IAM,' this function is a dummy function... Use a real SparseLinearAlgebra module'
    info = 1

  end subroutine

  subroutine sparse_build(matrix, val, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    real(kind=8)   , dimension(:), pointer :: val
    integer(kind=4) :: info

    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_build'

    print*, IAM,' this function is a dummy function... Use a real SparseLinearAlgebra module'
    info = 1

  end subroutine

  subroutine sparse_solve(matrix, rhs, info)
    implicit none

    type(G_sparse_matrix) :: matrix

    real(kind=8)   , dimension(:), pointer :: rhs
    integer(kind=4) :: info

    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_solve'

    print *, IAM,' this function is a dummy function... Use a real SparseLinearAlgebra module'
    info = 1


  end subroutine

  subroutine sparse_erase(matrix)
    implicit none
    type(G_sparse_matrix) :: matrix

    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_erase'

    print *, IAM,' this function is a dummy function... Use a real SparseLinearAlgebra module'

  end subroutine

end module
