program test_a_MATRIX

   use a_matrix
   
   implicit none
  
   integer(kind=4) :: nb_err

   type(G_matrix) :: A
   integer :: i, n, info
   integer :: bw
   real(kind=8), dimension(:), allocatable :: b
   !                         1234567890123
   character(len=13) :: IAM='test_a_MATRIX'

   !!n = 6

   !!!
   !!! test of the diagonal storage
   !!!
 
   !!! set matrix size
   !!n=3

   !!! allocate memory for the right hand side
   !!allocate(b(n))
 
   !!! declare the matrix
   !!call G_declare(A, 'diagonal', n, .false., (/ 1 /), (/ 1 /)) 

   !!! set the band width
   !!bw=1
   !!call G_settle(A, (/ 1, bw /))

   !!! allocate memory for the matrix (ant set all its terms to 0)
   !!call G_build(A)

   !!! fill the matrix
   !!! A = 2*Id, where Id is the 3x3 identity matrix
   !!do i=1, n
   !!   call G_add(A, 2.d0, i, i)
   !!end do

   !!! build some auxilliary matrix
   !!call G_store(A)

   !!print *, 'inverse of A (diagonal storage):'

   !!! compute the columns of the inverse of the matrix
   !!do i=1, n
   !!   ! compute the right hand side
   !!   b=0.d0
   !!   b(i) = 1.d0

   !!   ! solve the system
   !!   call G_solve_linear_system(A, b, info)

   !!   print*, b

   !!   ! some paranoid test...
   !!   if (info /= 0) then
   !!      call FATERR(IAM, 'No solution')
   !!   end if
   !!end do

   !!! deallocate the memory used by the system
   !!call G_free(A)
   !!deallocate(b)

   nb_err = test_sym_full()
   nb_err = nb_err + test_sym_band()
   nb_err = nb_err + test_std_band()

   if( nb_err > 0 ) then
     stop 1
   end if

contains

  function test_sym_full()
    integer(kind=4) :: test_sym_full
    !
    integer(kind=4), parameter :: n  = 6
    integer(kind=4), parameter :: bw = 4
    integer(kind=4) :: i, info
    type(G_matrix) :: A
    real(kind=8), dimension(n) :: b, sol

    test_sym_full = 0

    b   = (/1, 2, 3, 4, 5, 6/)
    sol = (/2, 3, 4, 3, 4, 5/)

    call G_declare(A, 'sym_full', n, .false., (/ 1 /), (/ 1 /)) 
    call G_settle(A, (/ 1, bw /))
    call G_build(A)

    call G_zero(A)
    ! fill the matrix
    ! A = | 2*Id   -Id |
    !     |  -Id, 2*Id |, where Id is the 3x3 identity matrix
    do i = 1, n
      call G_add(A, 2.d0, i, i)
    end do
    do i = bw, n
      call G_add(A, -1.d0, i - bw + 1, i)
      !call G_add(A, -1.d0, i, i - bw + 1)
    end do
    call G_store(A)

    call G_solve_linear_system(A, b, info)

    call G_free(A)

    if( info /= 0 .or. any( abs(b-sol) > 1.e-15 ) ) then
      print *, 'Error testing sym full' 
      test_sym_full = 1
    end if

  end function

  function test_sym_band()
    integer(kind=4) :: test_sym_band
    !
    integer(kind=4), parameter :: n  = 6
    integer(kind=4), parameter :: bw = 4
    integer(kind=4) :: i, info
    type(G_matrix) :: A
    real(kind=8), dimension(n) :: b, sol

    test_sym_band = 0

    b   = (/1, 2, 3, 4, 5, 6/)
    sol = (/2, 3, 4, 3, 4, 5/)

    call G_declare(A, 'sym_band', n, .false., (/ 1 /), (/ 1 /)) 
    call G_settle(A, (/ 1, bw /))
    call G_build(A)

    call G_zero(A)
    ! fill the matrix
    ! A = | 2*Id   -Id |
    !     |  -Id, 2*Id |, where Id is the 3x3 identity matrix
    do i = 1, n
      call G_add(A, 2.d0, i, i)
    end do
    do i = bw, n
      call G_add(A, -1.d0, i - bw + 1, i)
    end do
    call G_store(A)

    call G_solve_linear_system(A, b, info)

    call G_free(A)

    if( info /= 0 .or. any( abs(b-sol) > 1.e-15 ) ) then
      print *, 'Error testing sym band' 
      test_sym_band = 1
    end if

  end function

  function test_std_band()
    integer(kind=4) :: test_std_band
    !
    integer(kind=4), parameter :: n  = 9
    integer(kind=4), parameter :: bw = 4
    integer(kind=4) :: i, info
    type(G_matrix) :: A
    real(kind=8), dimension(n) :: b, sol

    test_std_band = 0

    b   = (/1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0, 7.d0, 8.d0, 9.d0/)
    sol = (/5.d0, 7.d0, 9.d0, 7.d0, 8.d0, 9.d0, 7.d0, 8.d0, 9.d0/) / 3.d0

    call G_declare(A, 'std_band', n, .false., (/ 1 /), (/ 1 /)) 
    call G_settle(A, (/ 1, bw /))
    call G_build(A)

    call G_zero(A)
    ! fill the matrix
    ! A = | 2*Id   -Id |
    !     |  +Id, 2*Id |, where Id is the 3x3 identity matrix
    do i = 1, n
      call G_add(A, 2.d0, i, i)
    end do
    do i = bw, n
      call G_add(A, -1.d0, i - bw + 1, i)
      call G_add(A,  1.d0, i, i - bw + 1)
    end do
    call G_store(A)

    call G_solve_linear_system(A, b, info)

    call G_free(A)

    if( info /= 0 .or. any(abs(b-sol)>1.e-15) ) then
      print *, 'Error testing std band' 
      test_std_band = 1
    end if
  end function

  function test_sym_sky()
    integer(kind=4) :: test_sym_sky

    test_sym_sky = 0

  end function

  function test_std_diag()
    integer(kind=4) :: test_std_diag

    test_std_diag = 0

  end function

end program test_a_MATRIX
