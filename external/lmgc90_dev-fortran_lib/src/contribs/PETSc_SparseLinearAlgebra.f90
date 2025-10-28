!> \todo: this is just a draft of a previous attempt to
!>        use PETSc instead of MUMPS. Some serious thinking
!>        must be done on how to use these libraries especially
!>        in parallilazation cases before taking that up again.

module SparseLinearAlgebra

  implicit none

  private

#include <finclude/petsc.h90>

  type, public :: G_sparse_matrix
     private

     !> solution 
     Vec :: x
     !> righ hand side
     Vec :: b
     !> linear system matrix
     Mat :: A
     ! linear solver context
     KSP :: ksp
     ! preconditioner context
     PC  :: pc
 
     logical :: sym = .false.

     integer(kind=4), dimension(:),  pointer :: i_indices => null()
     integer(kind=4), dimension(:),  pointer :: j_indices => null()

  end type G_sparse_matrix 

  public sparse_init   , &
         sparse_declare, &
         sparse_build  , &
         sparse_solve  , &
         sparse_erase  , &
         sparse_finalize

  contains

  subroutine sparse_init()
    implicit none
    !
    integer(kind=4)   :: ierr
    character(len=32) :: IAM
    !      12345678901234567890123456789012
    IAM = 'SparseLinearAlgebra::sparse_init'

    call PetscInitialize(PETSC_NULL_CHARACTER,ierr)

    if( ierr > 0 ) then
      print *, '[', IAM, '] PetscInitalize failed with value : ', ierr
      stop
    end if

  end subroutine

  subroutine sparse_declare(matrix,nb_dofs, nb_non_zero, i_indices, j_indices, is_sym, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    integer(kind=4)                        :: nb_dofs, nb_non_zero, info
    integer(kind=4), dimension(:), pointer :: i_indices
    integer(kind=4), dimension(:), pointer :: j_indices
    logical                                :: is_sym
    !
    PetscBool :: is_spd
    real(kind=8) :: tol
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'SparseLinearAlgebra::sparse_declare'

    is_spd = PETSC_FALSE
    tol = 1.e-5

    ! create right hand side
    call VecCreate(PETSC_COMM_WORLD,matrix%b,info)
    call VecSetSizes(matrix%b,PETSC_DECIDE,nb_dofs,info)
    call VecSetFromOptions(matrix%b,info)

    ! create solution
    call VecDuplicate(matrix%b,matrix%x,info)

    ! create left hand side
    call MatCreate(PETSC_COMM_WORLD,matrix%A,info)
    call MatSetSizes(matrix%A,PETSC_DECIDE,PETSC_DECIDE,nb_dofs,nb_dofs,info)
    call MatSetFromOptions(matrix%A,info)
    !if( is_sym ) then
    !  is_spd = PETSC_TRUE
    !  matrix%sym = .true.
    !end if
    !call MatSetOption(matrix%A,MAT_SPD,is_spd)
    call MatSetUp(matrix%A,info)

    ! create linear solver
    call KSPCreate(PETSC_COMM_WORLD,matrix%ksp,info)

    call KSPGetPC(matrix%ksp,matrix%pc,info)
    call PCSetType(matrix%pc,PCJACOBI,info)
    !call KSPSetTolerances(matrix%ksp,tol,PETSC_DEFAULT_REAL,PETSC_DEFAULT_REAL,PETSC_DEFAULT_REAL,info)

    matrix%i_indices => i_indices
    matrix%j_indices => j_indices

  end subroutine

  subroutine sparse_build(matrix, val, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    real(kind=8)   , dimension(:), pointer :: val
    integer(kind=4) :: info
    !
    integer(kind=4)   :: i
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_build'

    do i = 1, size(val)
      call MatSetValues(matrix%A,1,matrix%i_indices(i)-1,1,matrix%j_indices(i)-1, &
                        val(i),INSERT_VALUES, info)
    end do
    if( .not. matrix%sym ) then
      do i = 1, size(val)
        call MatSetValues(matrix%A,1,matrix%j_indices(i)-1,1,matrix%i_indices(i)-1, &
                          val(i),INSERT_VALUES, info)
      end do
    end if

    call MatAssemblyBegin(matrix%A,MAT_FINAL_ASSEMBLY,info)
    call MatAssemblyEnd(matrix%A,MAT_FINAL_ASSEMBLY,info)

    call KSPSetOperators(matrix%ksp,matrix%A,matrix%A,info)
    call KSPSetFromOptions(matrix%ksp,info)

  end subroutine

  subroutine sparse_solve(matrix, rhs, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    real(kind=8)   , dimension(:), pointer :: rhs
    integer(kind=4) :: info
    !
    integer(kind=4) :: i
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_solve'

    do i = 1, size(rhs)
      call VecSetValues(matrix%b,1,i-1,rhs(i),INSERT_VALUES,info)
    end do
    call VecAssemblyBegin(matrix%b,info)
    call VecAssemblyEnd(matrix%b,info)

    call KSPSolve(matrix%ksp,matrix%b,matrix%x,info)
    !activate only if verbose...
    !call KSPView(matrix%ksp,PETSC_VIEWER_STDOUT_WORLD,info)

    do i = 1, size(rhs)
      call VecGetValues(matrix%x,1,i-1,rhs(i),info)
    end do

  end subroutine

  subroutine sparse_erase(matrix)
    implicit none
    type(G_sparse_matrix) :: matrix
    !
    integer(kind=4)   :: info
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'SparseLinearAlgebra::sparse_erase'

    call VecDestroy(matrix%x,info)
    call VecDestroy(matrix%b,info)
    call MatDestroy(matrix%A,info)
    call KSPDestroy(matrix%ksp,info)

  end subroutine

  subroutine sparse_finalize()
    implicit none
    !
    integer(kind=4)   :: ierr
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'SparseLinearAlgebra::sparse_finalize'

    call PetsCFinalize(ierr)

    if( ierr > 0 ) then
      print *, '[', IAM, '] PetscFinalize failed with value : ', ierr
      stop
    end if

  end subroutine

end module
