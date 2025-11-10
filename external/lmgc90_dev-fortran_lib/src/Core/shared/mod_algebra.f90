!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program 
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use, 
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info". 
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability. 
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or 
! data to be ensured and,  more generally, to use and operate it in the 
! same conditions as regards security. 
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================

!> algebra gives basic fonctionnality on vectors (3), matrices (2*2|3*3) 
!>@author f dubois
MODULE Algebra

  use paranoid_checks
  use utilities
  use parameters

  IMPLICIT NONE

  private

  real(kind=8), dimension(9)  , parameter, public :: Id3 = (/ 1.d0, 0.d0, 0.d0, &
                                                              0.d0, 1.d0, 0.d0, &
                                                              0.d0, 0.d0, 1.d0  /)

  real(kind=8), dimension(3,3), parameter, public :: Id33 = reshape( &
                                                            (/ 1.d0, 0.d0, 0.d0, &
                                                               0.d0, 1.d0, 0.d0, &
                                                               0.d0, 0.d0, 1.d0 /), &
                                                            (/3, 3/) )



  real(kind=8), dimension(2,2), parameter, public :: Id22 = reshape( &
                                                            (/ 1.d0, 0.d0, &
                                                               0.d0, 1.d0  /), &
                                                            (/2, 2/) )

  !> a symetric elementary matrix type
  !> this mainly copied from mod_a_MATRIX
  type :: T_mat_sym_elem
    integer(kind=4) :: n !< number of terms in the matrix
    real(kind=8), dimension(:), allocatable :: V !< values
  end type T_mat_sym_elem

  !> a diagonal elementary matrix type
  !> this mainly copied from mod_a_MATRIX
  type :: T_mat_dia_elem
    integer(kind=4) :: n !< number of diagonal terms
    real(kind=8), dimension(:), allocatable :: V !< diagonal coefficients
  end type T_mat_dia_elem

  !> a full elementary matrix type
  !> this mainly copied from mod_a_MATRIX
  type :: T_mat_ful_elem
    real(kind=8), dimension(:,:), allocatable :: V !< values
  end type T_mat_ful_elem

  integer(kind=4),parameter :: i_sym_full=1 , &
                               i_std_full= i_sym_full + 1 , &
                               i_diag    = i_std_full + 1

  !> general elementary matrix type allowing to
  !> get an abstraction on a symetric or a diagonal one
  type, public :: G_elementary_matrix
    private
    integer(kind=4) :: storage !< storage of elementary matrix (see parameters module)
    integer(kind=4) :: shape   !< shape   of elementary matrix (see parameters module)
    integer(kind=4) :: order   !< order of the matrix
    integer(kind=4) :: id      !< internal storage/shape

    type(T_mat_sym_elem), pointer :: std_sym => null() !< symetric elementary matrix (id == 1)
    type(T_mat_dia_elem), pointer :: std_dia => null() !< diagonal elementary matrix (id == 5)
    type(T_mat_ful_elem), pointer :: std_ful => null() !< full elementary matrix     (id == 6)
  end type G_elementary_matrix

  public &
       cross_product, &
       length3, &
       length2, &
       orient2, &
       inverse33, &
       inverse22, &
       determinant33, &
       diagonalise33, &
       transpose33, &
       GRAMM_SCHMIDT, &
       determinant, &
       new_G_elementary_matrix, &
       delete_G_elementary_matrix, &
       zero_G_elementary_matrix, &
       set_G_elementary_matrix_from_vec  , &
       set_G_elementary_matrix_from_mat  , &
       set_term_of_G_elementary_matrix   , &
       get_term_of_G_elementary_matrix   , &
       get_G_elementary_matrix_order     , &
       add_to_G_elementary_matrix        , &
       product_G_elementary_matrix_vector, &
       display_G_elementary_matrix
  
  private new_mat_sym_elem, &
          new_mat_dia_elem, &
          new_mat_ful_elem, &
          delete_mat_sym_elem, &
          delete_mat_dia_elem, &
          delete_mat_ful_elem, &
          zero_mat_sym_elem, &
          zero_mat_dia_elem, &
          set_mat_sym_elem_from_mat, &
          set_mat_dia_elem_from_mat, &
          set_term_of_mat_sym_elem, &
          set_term_of_mat_dia_elem, &
          get_mat_sym_elem, &
          get_mat_dia_elem, &
          get_term_of_mat_sym_elem, &
          get_term_of_mat_dia_elem, &
          product_mat_sym_elem_vector, &
          product_mat_dia_elem_vector

contains

  FUNCTION cross_product(a,b)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: a,b,cross_product

    cross_product(1)=(a(2)*b(3)) - (a(3)*b(2))
    cross_product(2)=(a(3)*b(1)) - (a(1)*b(3))
    cross_product(3)=(a(1)*b(2)) - (a(2)*b(1))

  END FUNCTION cross_product

!!!------------------------------------------------------------------------

  REAL(kind=8) FUNCTION length3(x)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: x 

    length3 = dsqrt(DOT_PRODUCT(x,x))

  END FUNCTION length3

!!!------------------------------------------------------------------------

  REAL(kind=8) FUNCTION length2(x)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(2) :: x 

    length2 = dsqrt(DOT_PRODUCT(x,x))

  END FUNCTION length2

!!!------------------------------------------------------------------------

  REAL(kind=8) FUNCTION orient2(x,y,z)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(2) :: x,y,z,a,b 
    a=y-x
    b=z-x
    orient2 = (a(1)*b(2)) - (a(2)*b(1))

  END FUNCTION orient2

!!!------------------------------------------------------------------------

  REAL(kind=8) FUNCTION determinant33(A)

    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3,3) :: A

    determinant33 = determinant(A(:,1),A(:,2),A(:,3))
    
  END FUNCTION determinant33

!!!------------------------------------------------------------------------
  REAL(kind=8) FUNCTION determinant(A,B,C)

    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: A,B,C

    determinant = A(1)*(B(2)*C(3) - B(3)*C(2)) &
                - A(2)*(B(1)*C(3) - B(3)*C(1)) &
                + A(3)*(B(1)*C(2) - B(2)*C(1))
    
  END FUNCTION determinant

!!!------------------------------------------------------------------------

  !> transpose of a 3*3 matrix 
  FUNCTION transpose33(A)

    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3,3) :: A,transpose33
    ! ***
    INTEGER                     :: i,j

    DO i=1,3
       DO j=1,3
          transpose33(i,j) = A(j,i)
       END DO
    END DO

  END FUNCTION transpose33

!!!--------------------------------------------------------------------

  !> inverse of a 2*2 matrix
  SUBROUTINE inverse22(A,err)
    
    IMPLICIT NONE
    
    INTEGER                     :: err
    REAL(kind=8),DIMENSION(2,2) :: A,B
    REAL(kind=8)                :: delta,inv_delta

    err = 0
    
    delta = A(1, 1)*A(2, 2) - A(2, 1)*A(1, 2)
    
    IF(ABS(delta) <= 1.D-24)THEN
      err = 1
      return 
    ENDIF

    inv_delta=1.D0/delta
    B(1,1)= A(2, 2)*inv_delta
    B(2,1)=-A(2, 1)*inv_delta
    B(1,2)=-A(1, 2)*inv_delta
    B(2,2)= A(1, 1)*inv_delta
    
    A=B

  END SUBROUTINE inverse22

!!!--------------------------------------------------------------------

  !> inverse of a 3*3 matrix
  subroutine inverse33(A,err)
    
    IMPLICIT NONE
    
    REAL(kind=8),DIMENSION(3,3),intent(inout) :: A
    integer :: err

    ! ***
    REAL(kind=8),DIMENSION(3,3) :: B
    REAL(kind=8)                :: delta,inv_delta
    REAL(kind=8),DIMENSION(3)   :: det

    det(1)=A(2,2)*A(3,3)-A(2,3)*A(3,2)
    det(2)=A(2,1)*A(3,3)-A(3,1)*A(2,3)
    det(3)=A(2,1)*A(3,2)-A(3,1)*A(2,2)
    
    delta = A(1,1)*det(1)-A(1,2)*det(2)+A(1,3)*det(3)
    
    IF(ABS(delta) <= 1.D-24) then
      err = 1  ! pas inversible
      return
    ENDIF

    inv_delta=1.D0/delta
    B(1,1)= det(1)*inv_delta
    B(2,1)=-det(2)*inv_delta
    B(3,1)= det(3)*inv_delta
    B(1,2)=-(A(1,2)*A(3,3)-A(3,2)*A(1,3))*inv_delta
    B(2,2)= (A(1,1)*A(3,3)-A(3,1)*A(1,3))*inv_delta
    B(3,2)=-(A(1,1)*A(3,2)-A(3,1)*A(1,2))*inv_delta
    B(1,3)= (A(1,2)*A(2,3)-A(2,2)*A(1,3))*inv_delta
    B(2,3)=-(A(1,1)*A(2,3)-A(2,1)*A(1,3))*inv_delta
    B(3,3)= (A(1,1)*A(2,2)-A(2,1)*A(1,2))*inv_delta
    
    A=B

    err = 0

  END subroutine inverse33

!!!--------------------------------------------------------------------

  !> re-orthogonalisation et re-normalisation d'une base 3*3  
  SUBROUTINE GRAMM_SCHMIDT(v1,v2,v3)

    IMPLICIT NONE

    REAL(kind=8),DIMENSION(3) :: v1,v2,v3
    REAL(kind=8),DIMENSION(3) :: u1,u2,u3
    REAL(kind=8),DIMENSION(3) :: w2,w3,norm

    norm = SQRT(v1(1)*v1(1)+v1(2)*v1(2)+v1(3)*v1(3))
    u1 = v1/norm

    w2 = v2 -(v2(1)*u1(1)+v2(2)*u1(2)+v2(3)*u1(3))*u1
    norm = SQRT(w2(1)*w2(1)+w2(2)*w2(2)+w2(3)*w2(3))
    u2 = w2/norm

    w3 = v3 -(v3(1)*u1(1)+v3(2)*u1(2)+v3(3)*u1(3))*u1-(v3(1)*u2(1)+v3(2)*u2(2)+v3(3)*u2(3))*u2
    norm = SQRT(w3(1)*w3(1)+w3(2)*w3(2)+w3(3)*w3(3))
    u3 = w3/norm

    v1 = u1
    v2 = u2
    v3 = u3

  END SUBROUTINE GRAMM_SCHMIDT

!!!------------------------------------------------------------------------

  subroutine diagonalise33(I,Ip,Fp)
    implicit none

    real(kind=8),dimension(3) :: Ip
    real(kind=8),dimension(3,3) :: I,Fp
    
    integer :: nb,j,k,ierror
    real(kind=8) :: norm
    real(kind=8) :: tmp(3)

    ! lapack related
    integer(kind=4), parameter :: lwork  = 1+6*3+2*3*3 !1+6N+2N^2)
    integer(kind=4), parameter :: liwork = 3+5*3 !3+5N
    real(kind=8)   , dimension(lwork)  :: work
    integer(kind=4), dimension(liwork) :: iwork

                             !1234567890123456789012
    character(len=22) :: IAM='algebra::diagonalise33'
    
    ! Diagonalisation de la matrice d'inertie: 
    ! vecteur p -> Z et valeur propre -> wr
    !
    !fd si la matrice est quasi diagonale on diagonalise.
    !fd question les termes diagonaux sont ils predominants ?
    !fd quelle precision esperer pour les tests !?
    !fd sur un pc il semble qu'on n'ait que 15 chiffre significatifs
    nb = 0 

    DO j=1,3
       if (abs(I(j,j)) > 1.d-10) then
         DO k=1,3
           IF ( ABS(I(j,k)) < 1.D-14*ABS(I(j,j))) THEN
             I(j,k)=0.D0
           END IF
           IF ((j/=k).AND.(I(j,k)==0.D0)) THEN
              nb = nb + 1
           END IF
         END DO
       else
!fd je laisse ce test mais je pense qu'il est idiot
!fd le bon critere c'est le ratio terme non-diag terme diag
         DO k=1,3
            IF (ABS(I(j,k))<1.D-17) THEN
              I(j,k)=0.D0
           END IF
           IF ((j/=k).AND.(I(j,k)==0.D0)) THEN
              nb = nb + 1
           END IF
         END DO
       endif
    END DO

!    print*,'================'
    
    IF (nb==6) THEN

!       print*,'direct'

       Ip(1)  = I(1,1)
       Ip(2)  = I(2,2)
       Ip(3)  = I(3,3)
       Fp=0.D0
       Fp(1,1) = 1.D0
       Fp(2,2) = 1.D0
       Fp(3,3) = 1.D0

    ELSE


     ! print*,'Field'
     ! print '(3(1x,D12.5))',I

       
      Fp = I

      ! 'V' to compute eigen value
      ! 'L' means lower triangle matrix
      ! les vecteurs propres sont stockes en colonne
      ! les valeurs propres sont en ordre croissant 
      call dsyevd('V','L',3,Fp,3,Ip,work,lwork,iwork,liwork,ierror)

      if (ierror /= 0) call faterr(IAM,'diagonalisation not possible')  
      
     ! print*,'diago'
     ! print*,'Ip'
     ! print '(3(1x,D12.5))',Ip 
     ! print*,'frame'
     ! ! les vecteurs sont stockes en colonne
     ! do j=1,3
     !   print '(3(1x,D12.5))',Fp(:,j)
     ! enddo   

! fd ca ne marche pas comme on veut le rep n'est pas tjs directe
!      do  j=1,3
!        norm = dot_product(Fp(1:3,j),Fp(1:3,j))
!        Fp(1:3,j) = Fp(1:3,j)/sqrt(norm)
!        Ip(j) = Ip(j)*norm
!      enddo


      do  j=1,2
        norm = dot_product(Fp(1:3,j),Fp(1:3,j))
        Fp(1:3,j) = Fp(1:3,j)/sqrt(norm)
        Ip(j) = Ip(j)*norm
      enddo

      norm = dot_product(Fp(1:3,3),Fp(1:3,3))
      Ip(3) = Ip(3)*norm

      tmp = cross_product(Fp(1:3,1),Fp(1:3,2))
      Fp(1:3,3) = tmp(1:3)

    ENDIF


  end subroutine

  !> create a new general elemetary matrix
  subroutine new_G_elementary_matrix(G, storage, shape, order)
    implicit none
    type(G_elementary_matrix)   :: G       !< [in,out] General elementary matrix
    integer(kind=4), intent(in) :: storage !< [in] storage type (full, diag)
    integer(kind=4), intent(in) :: shape !< [in] storage type (sym, std)
    integer(kind=4), intent(in) :: order   !< [in] order of the matrix

    G%storage = storage
    G%shape   = shape
    G%order   = order

    G%id = get_id(storage, shape) 

    select case( G%id )
    case( i_sym_full )
      if( associated(G%std_sym) ) then
        deallocate(G%std_sym)
        nullify(G%std_sym)
      end if
      allocate( G%std_sym )
      call new_mat_sym_elem(G%std_sym, order)
    case( i_diag )
      if( associated(G%std_dia) ) then
        deallocate(G%std_dia)
        nullify(G%std_dia)
      end if
      allocate( G%std_dia )
      call new_mat_dia_elem(G%std_dia, order)
    case( i_std_full )
      if( associated(G%std_ful) ) then
        deallocate(G%std_ful)
        nullify(G%std_ful)
      end if
      allocate( G%std_ful )
      call new_mat_ful_elem(G%std_ful, order)
    case default
      write (*,*) 'impossible to create an elementary matrix with storage :', & 
                   get_id(storage,shape)
      call faterr('algebra::new_G_elementary_matrix','unknown storage type')
    end select
  end subroutine

  !> \brief Delete an elementary matrix
  subroutine delete_G_elementary_matrix(G)
    implicit none
    type(G_elementary_matrix) :: G !< [in,out] the matrix to delete

    select case( G%id )
    case( i_sym_full )
      call delete_mat_sym_elem(G%std_sym)
      deallocate( G%std_sym )
      nullify(G%std_sym)
    case( i_diag )
      call delete_mat_dia_elem(G%std_dia)
      deallocate( G%std_dia )
      nullify(G%std_dia)
    case( i_std_full )
      call delete_mat_ful_elem(G%std_ful)
      deallocate( G%std_ful )
      nullify(G%std_ful)
    case default
      call faterr('algebra::delete_G_elementary_matrix','unknown storage type')
    end select
    G%id = 0
  end subroutine
 
  !> \brief Set all terms of an elementary matrix to zero
  subroutine zero_G_elementary_matrix(G)
    implicit none
    type(G_elementary_matrix) :: G !< [in,out] The elementary  matrix to reset

    !print *, 'id      : ', G%id
    !print *, 'order   : ', G%order
    !print *, 'std_sym : ', associated(G%std_sym)
    !print *, 'std_dia : ', associated(G%std_dia)
    select case( G%id )
    case( i_sym_full )
      call zero_mat_sym_elem(G%std_sym)
    case( i_diag )
      call zero_mat_dia_elem(G%std_dia)
    case( i_std_full )
      G%std_ful%V(1:G%order,1:G%order) = 0.d0
    case default
      call faterr('algebra::zero_G_elementary_matrix','unknown storage type')
    end select
  end subroutine

  !> \brief Set the values of an elementary from a vector
  subroutine set_G_elementary_matrix_from_vec(G, vec)
    implicit none
    type(G_elementary_matrix)              :: G   !< [in,out] The elementary  matrix to set
    real(kind=8), dimension(:), intent(in) :: vec !< [in] vector of new values
    !
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'algebra::set_G_elementary_matrix_from_vec'

    select case( G%id )
    case( i_sym_full )
      call paranoid_check_r8_size(IAM,vec,G%std_sym%n)
      G%std_sym%V(1:G%std_sym%n) = vec(1:G%std_sym%n)
    case( i_diag )
      call paranoid_check_r8_size(IAM,vec,G%std_dia%n)
      G%std_dia%V(1:G%std_dia%n) = vec(1:G%std_dia%n)
    case( i_std_full )
      call paranoid_check_r8_size(IAM,vec,G%order*G%order)
      G%std_ful%V(1:G%order,1:G%order) = reshape(vec, (/G%order,G%order/))
    case default
      call faterr('algebra::set_G_elementary_matrix_from_vec','unknown storage type')
    end select
  end subroutine

  !> \brief Set the values of an elementary from a matrix
  subroutine set_G_elementary_matrix_from_mat(G, mat)
    implicit none
    type(G_elementary_matrix)                :: G   !< [in,out] The elementary  matrix to set
    real(kind=8), dimension(:,:), intent(in) :: mat !< [in] matrix of new values

    select case( G%id )
    case( i_sym_full )
      call set_mat_sym_elem_from_mat(G%std_sym, mat, G%order)
    case( i_diag )
      call set_mat_dia_elem_from_mat(G%std_dia, mat)
    case( i_std_full )
      G%std_ful%V(1:G%order,1:G%order) = mat(1:G%order,1:G%order)
    case default
      call faterr('algebra::set_G_elementary_matrix_from_mat','unknown storage type')
    end select
  end subroutine

  !> \brief Set a term of an elementary matrix
  subroutine set_term_of_G_elementary_matrix(G, i, j, val)
    implicit none
    type(G_elementary_matrix)   :: G   !< [in,out] The elementary matrix to modify
    integer(kind=4), intent(in) :: i   !< [in] line number
    integer(kind=4), intent(in) :: j   !< [in] column number
    real(kind=8),    intent(in) :: val !< [in] new value
    !
    character(len=103) :: cout
    character(len=40)  :: IAM
    !    1234567890123456789012345678901234567890
    IAM='algebra::set_term_of_G_elementary_matrix'

    if( i<1 .or. j<1 .or. i>G%order .or. j>G%order ) then
      call FATERR(IAM,'index out of range')
    end if

    select case( G%id )
    case( i_sym_full )
      call set_term_of_mat_sym_elem(G%std_sym, i, j, val, G%order)
    case( i_diag )
      call set_term_of_mat_dia_elem(G%std_dia, i, j, val)
    case( i_std_full )
      G%std_ful%V(i,j) = val
    case default
      call faterr(IAM,'unknown storage type')
    end select
  end subroutine

  !> \brief Get a term of an elementary matrix
  function get_term_of_G_elementary_matrix(G, i, j)
    implicit none
    real(kind=8) :: get_term_of_G_elementary_matrix !< [return] value of the desired term
    type(G_elementary_matrix)   :: G !< [in] The elementary matrix to get term of
    integer(kind=4), intent(in) :: i !< [in] line number
    integer(kind=4), intent(in) :: j !< [in] column number
    !
    character(len=103) :: cout
    character(len=40)  :: IAM
    !    1234567890123456789012345678901234567890
    IAM='algebra::get_term_of_G_elementary_matrix'

    if( i<1 .or. j<1 .or. i>G%order .or. j>G%order ) then
      call FATERR(IAM,'index out of range')
    end if

    select case( G%id )
    case( i_sym_full )
      get_term_of_G_elementary_matrix = get_term_of_mat_sym_elem(G%std_sym, i, j, G%order)
    case( i_diag )
      get_term_of_G_elementary_matrix = get_term_of_mat_dia_elem(G%std_dia, i, j)
    case( i_std_full )
      get_term_of_G_elementary_matrix = G%std_ful%V(i,j)
    case default
      call faterr(IAM,'unknown storage type')
    end select
  end function

  !> \brief Get order of an elementary matrix
  function get_G_elementary_matrix_order(G)
    implicit none
    type(G_elementary_matrix), intent(in) :: G !< [in] The elementary matrix
    integer(kind=4) :: get_G_elementary_matrix_order !< [return] order of the elementary matrix

    get_G_elementary_matrix_order = G%order

  end function

  !> \brief Performs G1 = G1 + lambda * G2
  !> provided that G1's type is 'larger' than G2's
  subroutine add_to_G_elementary_matrix(G1, G2, lambda)
    implicit none
    type(G_elementary_matrix)                :: G1     !< matrix in which to add
    real(kind=8), dimension(:,:), intent(in) :: G2     !< matrix to add
    real(kind=8), optional,       intent(in) :: lambda !< factor
    !
    real(kind=8) :: coeff
    integer(kind=4)    :: n, i, j
    character(len=103) :: cout
    character(len=35)  :: IAM
    !    12345678901234567890123456789012345
    IAM='algebra::add_to_G_elementary_matrix'
    

    ! checks
    if( G1%order /= size(G2,1) ) then
      call FATERR(IAM,'G1 and G2 are of different order')
    end if

    if( .not. present(lambda) ) then
      coeff = 1.0
    else
      coeff = lambda
    end if

    select case( G1%id )
    case( i_diag )
      if( size(G2,2) == 1 ) then
        n = G1%std_dia%n
        G1%std_dia%V(1:n) = G1%std_dia%V(1:n) + coeff * G2(1:n,1)
      else
        write(cout,'(A,1x,I0,1x,I0)') 'storage of G1 : ',G1%id
        call logmes(cout)
        write(cout,'(A,1x,I0,1x,I0)') 'and shape of G2 : ', shape(G2)
        call logmes(cout)
        call faterr(IAM,'operation not implemented for these storage types')
      end if
    case( i_std_full )
      if( size(G2,2) > 1 ) then
        n = G1%order
        G1%std_ful%V(1:n,1:n) = G1%std_ful%V(1:n,1:n) + coeff * G2(1:n,1:n)
      !case( i_sym_full )
      !  do i = 1, G1%order
      !    do j = i, G1%order
      !      n = G2%std_sym%n - (G1%order-i+1) * (G1%order-i+2) / 2 + j - i + 1
      !      G1%std_ful%V(i,j) = G1%std_ful%V(i,j) + coeff * G2%std_sym%V(n)
      !    end do
      !  end do
      else if( size(G2,2) == 1 ) then
        do i = 1, G1%order
          G1%std_ful%V(i,i) = G1%std_ful%V(i,i) + coeff * G2(i,1)
        end do
      else
        write(cout,'(A,1x,I0,1x,I0)') 'storage of G1 : ',G1%id
        call logmes(cout)
        write(cout,'(A,1x,I0,1x,I0)') 'and shape of G2 : ', shape(G2)
        call logmes(cout)
        call faterr(IAM,'operation not implemented for these storage types')
      end if
    case default
      call faterr(IAM,'unknown storage type')
    end select

  end subroutine
 
  !> \brief Performs vec2 = G * vec1 elementary matrix vector product
  subroutine product_G_elementary_matrix_vector(G, vec1, vec2)
    implicit none
    type(G_elementary_matrix),  intent(in)  :: G    !< [in] elementary matrix
    real(kind=8), dimension(:), intent(in)  :: vec1 !< [in] input elementary vector
    real(kind=8), dimension(:), intent(out) :: vec2 !< [out] output elementary vector
    !
    character(len=103) :: cout
    character(len=43)  :: IAM
    !    1234567890123456789012345678901234567890123
    IAM='algebra::product_G_elementary_matrix_vector'

    if( G%order /= size(vec1) ) then
      call FATERR(IAM,'G and input vector are not of compatible size')
    end if

    if( G%order /= size(vec2) ) then
      call FATERR(IAM,'G and outuput vector are not of compatible size')
    end if

    select case(G%id)
    case( i_sym_full )
      call product_mat_sym_elem_vector(G%std_sym, vec1, vec2, G%order)
    case( i_diag )
      call product_mat_dia_elem_vector(G%std_dia, vec1, vec2, G%order)
    case( i_std_full )
      vec2 = matmul(G%std_ful%V, vec1)
    case default
      call faterr(IAM,'unknown storage type')
    end select
    
  end subroutine

  !> \brief Display an elementary matrix
  subroutine display_G_elementary_matrix(G, i_unit)
    implicit none
    type(G_elementary_matrix)             :: G      !< [in,out] elementary matrix
    integer(kind=4), optional, intent(in) :: i_unit !< [in] optional unit number in which to display
    !
    real(kind=8), dimension(:,:), allocatable :: mat
    integer(kind=4)   :: order
    character(len=40) :: IAM
    !      1234567890123456789012345678901234567890
    IAM = 'algebra::display_G_elementary_matrix'

    order = G%order
    allocate(mat(order,order))

    select case(G%id)
    case( i_sym_full )
      call get_mat_sym_elem(G%std_sym, mat, order)
    case( i_diag )
      call get_mat_dia_elem(G%std_dia, mat)
    case( i_std_full )
      mat = G%std_ful%V
    case default
      call faterr(IAM,'unknown storage type')
    end select
    
    if( present(i_unit) ) then
      write(i_unit,*) mat
    else
      write(*,*) mat
    end if

    deallocate(mat)
  end subroutine

!!--------------------------------------------------------------------------------
!! new elementary matrix: memory allocation
!!--------------------------------------------------------------------------------
  !> \brief Allocate a new symetric elementary matrix
  subroutine new_mat_sym_elem(mat,n)
    implicit none
    integer(kind=4)      :: n   !< order of the elementary matrix
    type(T_mat_sym_elem) :: mat !< symetric elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=25)  :: IAM
    !    1234567890123456789012345
    IAM='algebra::new_mat_sym_elem'

    mat%n = n * (n+1) / 2

    allocate(mat%V(mat%n),stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error allocating mat_sym_elem')
    end if
    
    mat%V(1:mat%n) = 0.d0

  end subroutine new_mat_sym_elem

  !> \brief Allocate a new diagonal elementary matrix
  subroutine new_mat_dia_elem(mat,n)
    implicit none
    integer(kind=4)      :: n   !< order of the elementary matrix
    type(T_mat_dia_elem) :: mat !< diagonal elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=25)  :: IAM
    !    1234567890123456789012345
    IAM='algebra::new_mat_sym_elem'

    allocate(mat%V(n),stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error allocating mat_dia_elem')
    end if
    
    mat%n = n
    mat%V(1:n) = 0.d0

  end subroutine new_mat_dia_elem

  subroutine new_mat_ful_elem(mat,n)
    implicit none
    integer(kind=4)      :: n   !< order of the elementary matrix
    type(T_mat_ful_elem) :: mat !< full elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=25)  :: IAM
    !    1234567890123456789012345
    IAM='algebra::new_mat_ful_elem'

    allocate(mat%V(n,n),stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error allocating mat_ful_elem')
    end if
    
    mat%V(1:n,1:n) = 0.d0

  end subroutine new_mat_ful_elem

!!--------------------------------------------------------------------------------
!! delete elementary matrix: memory deallocation
!!--------------------------------------------------------------------------------
  !> \brief Delete a symetric elementary matrix
  subroutine delete_mat_sym_elem(mat)
    implicit none
    type(T_mat_sym_elem) :: mat !< symetric elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=28)  :: IAM
    !    1234567890123456789012345678
    IAM='algebra::delete_mat_sym_elem'

    deallocate(mat%V,stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error deallocating mat_sym_elem')
    end if
  end subroutine delete_mat_sym_elem

  !> \brief Delete a diagonal elementary matrix
  subroutine delete_mat_dia_elem(mat)
    implicit none
    type(T_mat_dia_elem) :: mat !< diagonal elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=28)  :: IAM
    !    1234567890123456789012345678
    IAM='algebra::delete_mat_dia_elem'

    deallocate(mat%V,stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error deallocating mat_dia_elem')
    end if
  end subroutine delete_mat_dia_elem

  !> \brief Delete a full elementary matrix
  subroutine delete_mat_ful_elem(mat)
    implicit none
    type(T_mat_ful_elem) :: mat !< full elementary matrix
    !
    integer(kind=4)    :: errare
    character(len=103) :: cout
    character(len=28)  :: IAM
    !    1234567890123456789012345678
    IAM='algebra::delete_mat_ful_elem'

    deallocate(mat%V,stat=errare)
    if( errare /= 0 ) then
       call FATERR(IAM,'error deallocating mat_ful_elem')
    end if
  end subroutine delete_mat_ful_elem

!!--------------------------------------------------------------------------------
!! reset elementary matrix : the whole vector of values is set to zero
!!--------------------------------------------------------------------------------
  !> \brief Set a symetric elementary matrix to zero
  subroutine zero_mat_sym_elem(mat)
    implicit none
    type(T_mat_sym_elem), intent(inout) :: mat !< [in,out] the symetric elementary matrix to reset
    !
    character(len=26)  :: IAM
    !    12345678901234567890123456
    IAM='algebra::zero_mat_sym_elem'

    mat%V(1:mat%n) = 0.d0

  end subroutine zero_mat_sym_elem

  !> \brief Set a diagonal elementary matrix to zero
  subroutine zero_mat_dia_elem(mat)
    implicit none
    type(T_mat_dia_elem), intent(inout) :: mat !< [in,out] the symetric elementary matrix to reset
    !
    character(len=26)  :: IAM
    !    12345678901234567890123456
    IAM='algebra::zero_mat_dia_elem'

    mat%V(1:mat%n) = 0.d0

  end subroutine zero_mat_dia_elem

!!--------------------------------------------------------------------------------
!! set elementary matrix : the whole vector of values or only on term
!!--------------------------------------------------------------------------------
  !> \brief Set a symetric elementary matrix from a vector
  subroutine set_mat_sym_elem_from_vec(mat, vec)
    implicit none
    type(T_mat_sym_elem),       intent(inout) :: mat !< [in,out] the symetric elementary matrix to set
    real(kind=8), dimension(:), intent(in)    :: vec !< [in] the vector of new values
    !
    character(len=34)  :: IAM
    !    1234567890123456789012345678901234
    IAM='algebra::set_mat_sym_elem_from_vec'

    call paranoid_check_r8_size(IAM,vec,mat%n)

    mat%V(1:mat%n) = vec(1:mat%n)

  end subroutine set_mat_sym_elem_from_vec

  !> \brief Set a diagonal elementary matrix from a vector
  subroutine set_mat_dia_elem_from_vec(mat, vec)
    implicit none
    type(T_mat_dia_elem),       intent(inout) :: mat !< [in,out] the symetric elementary matrix to set
    real(kind=8), dimension(:), intent(in)    :: vec !< [in] the vector of new values
    !
    character(len=34)  :: IAM
    !    1234567890123456789012345678901234
    IAM='algebra::set_mat_dia_elem_from_vec'

    call paranoid_check_r8_size(IAM,vec,mat%n)
    
    mat%V(1:mat%n) = vec(1:mat%n)
  end subroutine set_mat_dia_elem_from_vec

  !> \brief Set a symetric elementary matrix from an array
  !> There is not test to insure that new_mat input parameter
  !> really is symetric. The upper part of the matrix is stored
  !> and that's it
  subroutine set_mat_sym_elem_from_mat(mat, new_mat, order)
    implicit none
    type(T_mat_sym_elem),         intent(inout) :: mat     !< [in,out] the symetric elementary matrix to set
    real(kind=8), dimension(:,:), intent(in)    :: new_mat !< [in] the array of new values
    integer(kind=4),              intent(in)    :: order   !< [in] the order of the symetric elementary matrix
    !
    integer(kind=4)   :: i, j, index
    character(len=34) :: IAM
    !    1234567890123456789012345678901234
    IAM='algebra::set_mat_sym_elem_from_mat'

    call paranoid_check_r8_size(IAM,new_mat(:,1),order)
    call paranoid_check_r8_size(IAM,new_mat(1,:),order)

    do i = 1, order
      do j = i, order
        index = mat%n - (order-i+1) * (order-i+2) / 2 + j - i + 1
        mat%V(index) = new_mat(i,j)
      end do
    end do

  end subroutine set_mat_sym_elem_from_mat

  !> \brief Set a diagonal elementary matrix from an array
  !> There is not test to insure that new_mat input parameter
  !> really is diagonal. The diagonal part of the matrix is stored
  !> and that's it
  subroutine set_mat_dia_elem_from_mat(mat, new_mat)
    implicit none
    type(T_mat_dia_elem),         intent(inout) :: mat     !< [in,out] the diagonal elementary matrix to set
    real(kind=8), dimension(:,:), intent(in)    :: new_mat !< [in] the array of new values
    !
    integer(kind=4)   :: i
    character(len=34) :: IAM
    !    1234567890123456789012345678901234
    IAM='algebra::set_mat_dia_elem_from_mat'

    call paranoid_check_r8_size(IAM,mat%V,size(new_mat,1))
    call paranoid_check_r8_size(IAM,mat%V,size(new_mat,2))

    do i = 1, mat%n
      mat%V(i) = new_mat(i,i)
    end do
  end subroutine set_mat_dia_elem_from_mat

  !> \brief Set a term of a symetric elementary matrix
  subroutine set_term_of_mat_sym_elem(mat, i, j, val, order)
    implicit none
    type(T_mat_sym_elem), intent(inout) :: mat   !< [in,out] the symetric elementary matrix to modify
    integer(kind=4),      intent(in)    :: i     !< [in] row number
    integer(kind=4),      intent(in)    :: j     !< [in] column number
    integer(kind=4),      intent(in)    :: order !< [in] oreder of the symetric elementary matrix
    real(kind=8),         intent(in)    :: val   !< [in] the new value of the term
    !
    integer(kind=4) :: index

    if( i <= j ) then
      index = mat%n - (order-i+1) * (order-i+2) / 2 + j - i
    else
      index = mat%n - (order-j+1) * (order-j+2) / 2 + i - j
    end if

    index = index + 1
    mat%V(index) = val
  end subroutine set_term_of_mat_sym_elem

  !> \brief Set a term of a diagonal elementary matrix
  subroutine set_term_of_mat_dia_elem(mat, i, j, val)
    implicit none
    type(T_mat_dia_elem), intent(inout) :: mat   !< [in,out] the diagonal elementary matrix to modify
    integer(kind=4),      intent(in)    :: i     !< [in] row number
    integer(kind=4),      intent(in)    :: j     !< [in] column number
    real(kind=8),         intent(in)    :: val   !< [in] the new value of the term

    if( i == j ) then
      mat%V(i) = val
    end if
  end subroutine set_term_of_mat_dia_elem

!!--------------------------------------------------------------------------------
!! get elementary matrix : as a matrix or only on term
!!--------------------------------------------------------------------------------
  !> \brief Get a symetric elementary matrix
  subroutine get_mat_sym_elem(mat, copy, order)
    implicit none
    type(T_mat_sym_elem),         intent(in)  :: mat   !< [in] symetric elementary matrix to get
    real(kind=8), dimension(:,:), intent(out) :: copy  !< [out] array copying values of mat
    integer(kind=4),              intent(in)  :: order !< [in] the order of the elementary matrix
    !
    integer(kind=4)   :: i, j, index
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'algebra::get_mat_sym_elem'

    call paranoid_check_r8_size(IAM,copy(:,1),order)
    call paranoid_check_r8_size(IAM,copy(1,:),order)

    do i = 1, order
      do j = 1, order
        if( i <= j ) then
          index = mat%n - (order-i+1) * (order-i+2) / 2 + j - i
        else
          index = mat%n - (order-j+1) * (order-j+2) / 2 + i - j
        end if
        index = index + 1
        copy(i,j) = mat%V(index)
      end do
    end do
  end subroutine get_mat_sym_elem

  !> \brief Get a diagonal elementary matrix
  subroutine get_mat_dia_elem(mat, copy)
    implicit none
    type(T_mat_dia_elem),         intent(in)  :: mat   !< [in] diagonal elementary matrix to get
    real(kind=8), dimension(:,:), intent(out) :: copy  !< [out] array copying values of mat
    !
    integer(kind=4)   :: i
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'algebra::get_mat_dia_elem'

    call paranoid_check_r8_size(IAM,mat%V, size(copy,1))
    call paranoid_check_r8_size(IAM,mat%V, size(copy,2))

    copy(1:mat%n,1:mat%n) = 0.d0

    do i = 1, mat%n
      copy(i,i) = mat%V(i)
    end do
  end subroutine get_mat_dia_elem

  !> \brief Get a term of a symetric elementary matrix
  function get_term_of_mat_sym_elem(mat, i, j, order)
    implicit none
    real(kind=8) :: get_term_of_mat_sym_elem  !< [return] the value of the desired term
    type(T_mat_sym_elem), intent(in) :: mat   !< [in] elementary matrix to get term of
    integer(kind=4),      intent(in) :: i     !< [in] row number
    integer(kind=4),      intent(in) :: j     !< [in] column number
    integer(kind=4),      intent(in) :: order !< [in] the order of the elementary matrix
    !
    integer(kind=4) :: index

    if( i <= j ) then
      index = mat%n - (order-i+1) * (order-i+2) / 2 + j - i
    else
      index = mat%n - (order-j+1) * (order-j+2) / 2 + i - j
    end if

    index = index + 1
    get_term_of_mat_sym_elem = mat%V(index)
  end function get_term_of_mat_sym_elem

  !> \brief Get a term of a diagonal elementary matrix
  function get_term_of_mat_dia_elem(mat, i, j)
    implicit none
    real(kind=8) :: get_term_of_mat_dia_elem  !< [return] the value of the desired term
    type(T_mat_dia_elem), intent(in) :: mat   !< [in] elementary matrix to get term of
    integer(kind=4),      intent(in) :: i     !< [in] row number
    integer(kind=4),      intent(in) :: j     !< [in] column number

    if( i == j ) then
      get_term_of_mat_dia_elem = mat%V(i)
    else
      get_term_of_mat_dia_elem = 0.D0
    end if
  end function get_term_of_mat_dia_elem

!!--------------------------------------------------------------------------------
!! product elementary matrix vector : performs vec2 = mat * vec1
!!--------------------------------------------------------------------------------

  !> \brief Performs vec2 = mat  * vec1 for a symetric elementary matrix
  subroutine product_mat_sym_elem_vector(mat,vec1,vec2,order)
    implicit none
    type(T_mat_sym_elem),       intent(in)  :: mat   !< [in] symetrice elementary matrix
    real(kind=8), dimension(:), intent(in)  :: vec1  !< [in] input vector
    real(kind=8), dimension(:), intent(out) :: vec2  !< [out] ouput vector
    integer(kind=4),            intent(in)  :: order !< [in] order of the elementary matrix/vectors
    !
    integer(kind=4) :: debut, i

    vec2(1:order) = 0.d0

    do i = 1, order
      debut = mat%n - (order-i+1) * (order-i+2) / 2 + 1
      vec2(i) = vec2(i) + dot_product(mat%V(debut:debut+order-i),vec1(i:order))
      vec2(i+1:order) = vec2(i+1:order) + vec1(i) * mat%V(debut+1:debut+order-i)
    end do

  end subroutine

  !> \brief Performs vec2 = mat  * vec1 for a diagonal elementary matrix
  subroutine product_mat_dia_elem_vector(mat,vec1,vec2,order)
    implicit none
    type(T_mat_dia_elem),       intent(in)  :: mat   !< [in] symetrice elementary matrix
    real(kind=8), dimension(:), intent(in)  :: vec1  !< [in] input vector
    real(kind=8), dimension(:), intent(out) :: vec2  !< [in] input vector
    integer(kind=4),            intent(in)  :: order !< [in] order of the elementary matrix/vectors

    vec2(1:order) = vec1(1:order)*mat%V(1:order)
  end subroutine

  function get_id(i_storage, i_shape)
    implicit none
    integer(kind=4)  :: i_storage, i_shape
    integer(kind=4)  :: get_id 

    if      (i_storage == i_full .and. i_shape == i_std) then

       get_id = i_std_full

    else if (i_storage == i_full .and. i_shape == i_sym) then

       get_id = i_sym_full

    else if (i_storage == i_diagonal) then

       get_id = i_diag

    else

       get_id = -99

    endif

  end function

END MODULE ALGEBRA
