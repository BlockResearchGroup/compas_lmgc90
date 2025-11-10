
program test_genericEF_iso

  use parameters, only : i_2D_stress
  use a_genericEF_iso

  implicit none

  type(T_genEF_iso) :: genEF
  integer(kind=4)   :: i
  real(kind=8), dimension(:,:), pointer :: Ke

  integer(kind=4), parameter :: i_displacement = 1
  integer(kind=4), parameter :: i_temperature  = 2

  integer(kind=4), parameter :: i_stiffness     = 1
  integer(kind=4), parameter :: i_conductivity = 2
  integer(kind=4), parameter :: i_coupling     = 3

  call abracadabra(genEF)

  call display_genericEF(genEF)
  !call display_module()

  allocate(Ke(genEF%N_DOF,genEF%N_DOF))
  Ke = 0.D0
  call compute_elementary_matrix(genEF, Ke)

  call clean_genericEF(genEF)
  call clean_module()
  deallocate(Ke)

contains

!fd routine a mettre dans un module chapeau comme a_poro_mailx_iso
subroutine abracadabra(genEF)
  implicit none 
  type(T_genEF_iso) :: genEF
  ! ***
  integer(kind=4) :: i, tab(8)
  ! 2D
  ! Q84xx
  genEF%name           ='Q63xx'
  genEF%N_NODE         = 6

  ! nb nodal fields = 2
  ! nb elementary fields = 3
  call new_genericEF(genEF, 2, 3)

  do i =1, 8
    tab(i) = i
  end do
  ! adding displacement node_field
  !call add_node_field_genericEF(genEF, i_displacement, 2, i_q_p2, 8, (/ (i, i=1,8) /))
  call add_node_field_genericEF(genEF, i_displacement, 2, 4, i_t_p2, 6, tab(1:6))
  ! adding thermal node_field
  !call add_node_field_genericEF(genEF, i_temperature, 1, i_q_p1, 4, (/ (i, i=1,4) /))
  call add_node_field_genericEF(genEF, i_temperature, 1, 2, i_t_p1, 3, tab(1:3))

  ! adding stiffness operator
  call add_operator_genericEF(genEF, i_stiffness, p_Bt_B, i_tr06, 1, 1)
  ! adding conductivity operator
  call add_operator_genericEF(genEF, i_conductivity, p_Bt_B, i_tr03, 2, 2)
  ! adding coupling operator
  call add_operator_genericEF(genEF, i_coupling, p_VNpt_Nu, i_tr06, 2, 1)

  call init_genericEF(genEF)

end subroutine

subroutine compute_elementary_matrix(genEF, Ke)
  implicit none
  type(T_genEF_iso) :: genEF
  real(kind=8), dimension(:,:), pointer :: Ke
  !
  integer(kind=4) :: i_eo, i_nf_p, i_nf_d
  real(kind=8)    :: dt
  real(kind=8), dimension(2,6) :: X
  real(kind=8), dimension(3,3) :: D

  real(kind=8),    dimension(:,:), pointer :: K
  real(kind=8),    dimension(:),   pointer :: Fint
  integer(kind=4), dimension(:),   pointer :: primal_node_list, dual_node_list

  nbDIME   = 2
  DIME_mod = i_2D_strain

  dt = 1.0

  ! coordinates of real element
  X(:,1) = (/ 0. , 0. /); X(:,2) = (/ 1., 0. /)     ; X(:,3) = (/ 0., 1. /)
  X(:,4) = (/ 0.5, 0. /); X(:,5) = (/ 0.707, 0.707/); X(:,6) = (/ 0., 0.5/)

  ! this is bad, all operator are computed at once
  ! in fact depending on M, K or C
  do i_eo = 1, genEF%nb_ele_operator
    i_nf_p = genEF%ele_operator(i_eo)%nf_primal
    i_nf_d = genEF%ele_operator(i_eo)%nf_dual
    allocate(K(genEF%node_field(i_nf_d)%nf_dim*genEF%node_field(i_nf_d)%nb_support,&
               genEF%node_field(i_nf_p)%nf_dim*genEF%node_field(i_nf_p)%nb_support ))
    allocate(Fint(genEF%node_field(i_nf_d)%nf_dim*genEF%node_field(i_nf_d)%nb_support))

    primal_node_list => genEF%node_field(i_nf_p)%support
    dual_node_list   => genEF%node_field(i_nf_d)%support

    call compute_matrix(genEF%ele_operator(i_eo),primal_node_list,dual_node_list, dt, X, D, K, Fint)
    print *,' compute matrix of ele_operator : ', i_eo
    print *, K
    call put_in_augmented_matrix(genEF, i_eo, Ke)
    print *,' Put this K matrix in the augmented Ke : '
    print *, Ke

    deallocate(K,Fint)

  end do

end subroutine

subroutine compute_matrix(eo, p_nl, d_nl, dt, X, D, K, Fint)
  implicit none
  ! elementary operator to use to compute elementary matrix
  type(T_ele_operator), intent(in) :: eo
  ! primal node list
  integer(kind=4), dimension(:), intent(in) :: p_nl
  ! dual node list
  integer(kind=4), dimension(:), intent(in) :: d_nl
  ! time step
  real(kind=8), intent(in) :: dt
  ! nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  ! D
  real(kind=8), dimension(:,:), intent(in)  :: D
  ! elementary matrix
  real(kind=8), dimension(:,:), intent(out) :: K
  ! internal forces
  real(kind=8), dimension(:)  , intent(out) :: Fint
  !!
  integer(kind=4) :: i, j

  Fint = 0.d0
  ! this should call get the fields and call the right function
  ! to compute the matrix for this test the matrix depending on the operator
  !select case( eo%eo_id )
  !case( i_mass )
  !case( i_stiffness )
  !  call compute_stiffness_hpp(eo,dt,X,D,K,Fint)
  !case( i_capacity )
  !case( i_conductivity )
  !case( i_thermo_meca )
  !  call compute_coupling_temperature_displacement(eo,p_nl,d_nl,dt,X,D,K)
  !case default
  !end select

  do i = 1, size(K,2)
    do j = 1, size(K,1)
      K(j,i) = 10*(eo%quadrature) + (i-1)*size(K,1) + j
    end do
  end do

end subroutine

end program
