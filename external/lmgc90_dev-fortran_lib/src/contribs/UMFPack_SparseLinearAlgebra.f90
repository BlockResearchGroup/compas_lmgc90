
module SParseLinearAlgebra

  use iso_c_binding

  implicit none 

  private

  interface
    function umfpack_symbolic(nrow,ncol,ap,ai,ax,symbolic,control,info) bind(c, name="c_symbolic")
      import c_int, c_double, c_ptr

      integer(c_int), intent(in), value :: nrow
      integer(c_int), intent(in), value :: ncol

      type(c_ptr), value :: ap, ai, ax
      type(c_ptr) :: symbolic, control

      integer(c_int) :: umfpack_symbolic

      real(c_double), dimension(90) :: info

    end function
  end interface

  interface
    function umfpack_numeric(ap,ai,ax,symbolic,numeric,control,info) bind(c, name="c_numeric")
      import c_int, c_double, c_ptr

      type(c_ptr), value :: ap, ai, ax
      type(c_ptr) :: symbolic, numeric, control

      integer(c_int) :: umfpack_numeric

      real(c_double), dimension(90) :: info

    end function
  end interface

  interface
    function umfpack_solve(ap,ai,ax,x,b,numeric,control,info) bind(c, name="c_solve")
      import c_int, c_double, c_ptr

      type(c_ptr), value :: ap, ai, ax
      type(c_ptr) :: x, b, numeric, control

      integer(c_int) :: umfpack_solve

      real(c_double), dimension(90) :: info

    end function
  end interface

  interface
    subroutine umfpack_free_symbolic(symbolic) bind(c, name="c_free_symbolic")
      import c_ptr
      type(c_ptr) :: symbolic
    end subroutine
  end interface

  interface
    subroutine umfpack_free_numeric(numeric) bind(c, name="c_free_numeric")
      import c_ptr
      type(c_ptr) :: numeric
    end subroutine
  end interface


  type, public :: G_sparse_matrix
    private

    integer(c_int) :: n

    type(c_ptr) :: symbolic = c_null_ptr
    type(c_ptr) :: numeric  = c_null_ptr

    type(c_ptr) :: Ap = c_null_ptr
    type(c_ptr) :: Ai = c_null_ptr
    type(c_ptr) :: Ax = c_null_ptr

    type(c_ptr) :: control = c_null_ptr
    real(kind=c_double), dimension(90) :: info

    real(kind=c_double), dimension(:), pointer :: sol => null()

  end type G_sparse_matrix


  public sparse_declare, &
         sparse_build,   &
         sparse_solve,   &
         sparse_erase


  contains

  subroutine sparse_declare(matrix, nb_dofs, nb_non_zero, i_indices, j_indices, is_sym, info)
    implicit none

    type(G_sparse_matrix) :: matrix

    integer(kind=4)                        :: nb_dofs, nb_non_zero, info
    integer(kind=4), dimension(:), pointer :: i_indices
    integer(kind=4), dimension(:), pointer :: j_indices

    logical                                :: is_sym

    ! Ap ~ system%cc_adj_dof
    ! Ai ~ system%adj_dof

    integer(kind=c_int), dimension(:), pointer :: Ap, Ai
    real(kind=c_double), dimension(:), pointer :: control

    integer(kind=4) :: idx, i

    if( is_sym ) then
      print *, 'ERROR : umfpack available only for unsymmetric systems'
      print *, '        please use a function of type xxxxMAILx_UnspecifiedShape'
    end if

    allocate( Ap(nb_dofs+1  ) )
    allocate( Ai(nb_non_zero) )

    Ap(:) = 0
    Ai(:) = j_indices(:)-1

    ! assume i_indices is ordered in an increasing order

    ! counting non zero on a line
    idx = 0

    do i = 2, nb_non_zero
      if( idx /= i_indices(i) ) idx = i_indices(i)
      Ap(idx+1) = Ap(idx+1) + 1
    end do

    ! cumulating count
    Ap(1) = 1
    do i = 1, nb_dofs
      Ap(i+1) = Ap(i) + Ap(i+1)
    end do
    Ap(1) = 0

    matrix%Ap = c_loc( Ap(1) )
    matrix%Ai = c_loc( Ai(1) )

    matrix%n  = nb_dofs

    allocate( control(20) )
    control(:) = 0.d0
    matrix%control  = c_loc( control(1) )


    info = umfpack_symbolic(nb_dofs,nb_dofs,matrix%Ap,matrix%Ai,matrix%Ax,matrix%symbolic,matrix%control,matrix%info)

    allocate( matrix%sol(nb_dofs) )
    matrix%sol = 0.d0

  end subroutine


  subroutine sparse_build(matrix, val, info)
    implicit none
    type(G_sparse_matrix) :: matrix
    real(kind=8)   , dimension(:), pointer :: val
    integer(kind=4) :: info

    call umfpack_free_numeric(  matrix%numeric  )

    matrix%Ax = c_loc( val(1) )

    ! Factorize
    info = umfpack_numeric(matrix%Ap,matrix%Ai,matrix%Ax,matrix%symbolic,matrix%numeric,matrix%control,matrix%info)

  end subroutine

  subroutine sparse_solve(matrix, rhs, info)
    implicit none

    type(G_sparse_matrix) :: matrix

    real(kind=8)   , dimension(:), pointer :: rhs
    integer(kind=4) :: info

    ! Solve
    info = umfpack_solve( matrix%Ap, matrix%Ai, matrix%Ax, c_loc(matrix%sol(1)), c_loc(rhs(1)), &
                          matrix%numeric, matrix%control, matrix%info )

    rhs = matrix%sol

  end subroutine

  subroutine sparse_erase(matrix)
    implicit none
    type(G_sparse_matrix) :: matrix

    integer(kind=c_int), dimension(:), pointer :: Ap, Ai
    real(kind=c_double), dimension(:), pointer :: Ax, sol, control

    call umfpack_free_symbolic( matrix%symbolic )
    call umfpack_free_numeric(  matrix%numeric  )

    matrix%symbolic = c_null_ptr
    matrix%numeric  = c_null_ptr

    call c_f_pointer(matrix%Ap, ap, (/matrix%n/))

    call c_f_pointer(matrix%Ai, ai, (/ap(matrix%n+1)/))

    if(associated(ap)) deallocate(ap)
    if(associated(ai)) deallocate(ai)

    matrix%Ap = c_null_ptr
    matrix%Ai = c_null_ptr
    matrix%Ax = c_null_ptr

    call c_f_pointer(matrix%control, control, (/20/))
    deallocate(control)
    matrix%control = c_null_ptr

    if( associated(matrix%sol) ) deallocate(matrix%sol)
    matrix%sol => null()

  end subroutine

end module
