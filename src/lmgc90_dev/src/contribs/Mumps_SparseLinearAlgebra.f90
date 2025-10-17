
module SparseLinearAlgebra

  use timer, only: get_new_etimer_ID, &
                   start_etimer     , &
                   stop_etimer

  implicit none

  private

  include 'dmumps_struc.h'

  type, public :: G_sparse_matrix
     private
     type(dmumps_struc) :: mumps_par    

  end type G_sparse_matrix 

  logical, parameter, public  :: sparse_storage_available = .true.
  logical, parameter, private :: write_matrix = .false.

  public sparse_declare, &
         sparse_build,   &
         sparse_solve,   &
         sparse_erase

  private save_matrix_

  contains

  subroutine sparse_declare(matrix, nb_dofs, nb_non_zero, i_indices, j_indices, is_sym, info)
    implicit none
    !
    type(G_sparse_matrix) :: matrix
    integer                        :: nb_dofs, nb_non_zero, info
    integer, dimension(:), pointer :: i_indices, j_indices
    logical                        :: is_sym
    !
    integer, save :: timer_id = 0
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[MUMPS] declare     ')
    call start_etimer(timer_id)

    ! initializing a mumps instance
    matrix%mumps_par%job = -1

    matrix%mumps_par%comm = 0

    ! is input symetric ?
    if (is_sym) then
      matrix%mumps_par%sym  = 1
    else
      matrix%mumps_par%sym  = 0
    endif
    ! is parallelism used ?
    matrix%mumps_par%par  = 1

    ! let's do it

    call DMUMPS( matrix%mumps_par )

    ! Analysis
    matrix%mumps_par%job = 1

    ! setting verbosity to minimum
    matrix%mumps_par%ICNTL(1)=0
    matrix%mumps_par%ICNTL(2)=0
    matrix%mumps_par%ICNTL(3)=0
    matrix%mumps_par%ICNTL(4)=0


    ! assembled format input matrix
    matrix%mumps_par%ICNTL(5 ) = 0
    ! triplet coordinates
    matrix%mumps_par%ICNTL(18) = 0
    matrix%mumps_par%N   =  nb_dofs
    matrix%mumps_par%NZ  =  nb_non_zero
    matrix%mumps_par%IRN => i_indices
    matrix%mumps_par%JCN => j_indices

    ! scaling method
    !matrix%mumps_par%ICNTL(8) = 8

    ! let's do it
    call DMUMPS( matrix%mumps_par )

    info = matrix%mumps_par%INFOG(1)

    call stop_etimer(timer_id)

  end subroutine


  subroutine sparse_build(matrix, val, info)
    implicit none
    !
    type(G_sparse_matrix) :: matrix
    real(kind=8), dimension(:), pointer :: val
    integer :: info
    !
    integer, save :: timer_id = 0
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[MUMPS] build       ')
    call start_etimer(timer_id)

    ! Factorize
    matrix%mumps_par%job = 2

    matrix%mumps_par%A   => val

    ! let's do it
    call DMUMPS( matrix%mumps_par )

    info = matrix%mumps_par%INFOG(1)

    call stop_etimer(timer_id)

  end subroutine

  subroutine sparse_solve(matrix, rhs, info)
    implicit none
    !
    type(G_sparse_matrix)     , intent(inout) :: matrix
    real(kind=8), dimension(:), pointer       :: rhs
    integer                   , intent(out)   :: info
    !
    integer, save :: timer_id = 0
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[MUMPS] solve       ')
    call start_etimer(timer_id)

    ! solve
    matrix%mumps_par%job = 3

    ! rhs
    matrix%mumps_par%RHS => rhs

    ! save problem
    if( write_matrix ) then
      call save_matrix_(matrix)
    end if

    ! let's do it
    call DMUMPS( matrix%mumps_par )
    info = -matrix%mumps_par%INFOG(1)

    call stop_etimer(timer_id)

  end subroutine

  subroutine sparse_erase(matrix)
    implicit none
    type(G_sparse_matrix) :: matrix

    matrix%mumps_par%job = -2

    ! let's do it
    call DMUMPS( matrix%mumps_par )

  end subroutine

  subroutine save_matrix_(matrix)
    implicit none
    type(G_sparse_matrix) :: matrix

    matrix%mumps_par%write_problem = './mumps.mat'
    matrix%mumps_par%job = 1
    call DMUMPS( matrix%mumps_par )

    matrix%mumps_par%job = 2
    call DMUMPS( matrix%mumps_par )

    matrix%mumps_par%job = 3
  end subroutine save_matrix_

end module
