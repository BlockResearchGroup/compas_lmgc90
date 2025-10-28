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

!>\todo: Les operateurs travaillants sur les degrés de libertés de pression
!>       ne regardent pas s'il les pressions sont des physiques différentes
!>       (par exemples Pc et Pn). Du coup ils utilisent toujours les fonctions
!>       de formes primales sans chercher à comprendre alors que des fois il
!>       faudrait moralement calculer les fonctions de forme du champ dual aussi.

!> this module helps to build specific finite element
module a_genericEF_iso

use parameters, only: i_2D_stress, i_2D_strain, i_2D_axisym, i_3D, &
                      get_dime_mode_name_from_id

use utilities, only: faterr
use overall  , only: nbDIME, DIME_mod

use a_EF

use models, only: d_solid_iso    , &
                  comp_stress    , &
                  get_ppset_value, &
                  get_nb_external_variables

implicit none

public

!> operator type ID
integer(kind=4), parameter :: p_Nut_Nu   = 1             ! 
integer(kind=4), parameter :: p_Npt_Np   = p_Nut_Nu  + 1 !
integer(kind=4), parameter :: p_VNpt_Nu  = p_Npt_Np  + 1 !
integer(kind=4), parameter :: p_VNpt_VNp = p_VNpt_Nu + 1 ! 
integer(kind=4), parameter :: p_Bt_Np    = p_VNpt_VNp+ 1 ! 
integer(kind=4), parameter :: p_Npt_B    = p_Bt_Np   + 1 ! 
integer(kind=4), parameter :: p_Bt_B     = p_Npt_B   + 1 ! 

!> nodal field of an finite element
type T_node_field
   !> dimension of the node field
   integer(kind=4)                        :: nf_dim = 0
   !> dimension of the gradient of node field
   integer(kind=4)                        :: grad_dim = 0

   !> kind of interpolation
   integer(kind=4)                        :: interpolation = 0
   !> number of nodes used to interpolate
   integer(kind=4)                        :: nb_support = 0
   !> list of nodes used to interpolate
   integer(kind=4), dimension(:), pointer :: support => null()

   !> map between (local) nodal field dof to (global) element dof 
   integer(kind=4), dimension(:), pointer :: l2g => null()

   !> dof working array (primal/dual_ele)
   real(kind=8), dimension(:), pointer :: dof_work => null()
   !> scalar working array (primal/dual_ele)
   real(kind=8), dimension(:), pointer :: sca_work => null()

end type T_node_field

!> elementary operator of finite element
type T_ele_operator
   !> operator type
   integer(kind=4) :: type_id = 0
   !> kind of quadrature
   integer(kind=4) :: quadrature = 0
   !> index of primal nodal field
   integer(kind=4) :: nf_primal = 0
   !> index of dual nodal field
   integer(kind=4) :: nf_dual = 0

   !> number of quadrature points for the operator
   integer(kind=4) :: nb_GP = 0
   !> data at quadrature points for primal node field
   type(T_pt_gauss), dimension(:), pointer :: primal_GP => null()
   !> data at quadrature points for dual node field
   type(T_pt_gauss), dimension(:), pointer :: dual_GP => null()

   !> working array for computed matrix operator
   real(kind=8), dimension(:,:), pointer :: mat_work => null()
   !> working array for computed vector operator
   real(kind=8), dimension(:)  , pointer :: vec_work => null()
   !> working array for scalar field
   real(kind=8), dimension(:)  , pointer :: field_work => null()
   !> working array for grad field
   real(kind=8), dimension(:,:), pointer :: grad_work => null()
   !> working array for flux field
   real(kind=8), dimension(:,:), pointer :: flux_work => null()

   !> mapping matrice: from Gauss points to nodes
   real(kind=8), dimension(:,:), pointer :: gp2node

end type T_ele_operator

!> generic isoparametric finite element
type T_genEF_iso
   !> name of the element
   character(len=5)          :: NAME = '-----'
   !> number of geometric nodes
   integer(kind=4)           :: N_NODE = 0
   !> total number of dofs in the element
   integer(kind=4)           :: N_DOF = 0
   !
   !> map between a dof and the field it belongs to (and its number in it)
   integer(kind=4), dimension(:), pointer :: dof2field_map => null()
   !
   !> number of nodal fields in the element
   integer(kind=4) :: nb_nodal_field = 0
   !> list of nodal fields in the element
   type(T_node_field), dimension(:), pointer :: node_field => null()
   ! 
   !> number of elementary operators in the element
   integer(kind=4) :: nb_ele_operator = 0
   !> list of elementary operators in the element
   type(T_ele_operator), dimension(:), pointer :: ele_operator => null()

   !> coordinates working array
   real(kind=8), dimension(:,:), pointer :: coor_work => null()
   !> degrees of freedom working array
   real(kind=8), dimension(:)  , pointer :: dofs_work => null()

   !> edge to vertices map
   !> for a vertex of n-th order element, neighbour vertices of (n-1)-th order element
   integer(kind=4), dimension(:,:), pointer :: edge2vertices => null()

end type T_genEF_iso 

!> a linked list of all needed gauss_points structures
type(T_link_pt_gauss), pointer :: all_gauss_points => null()

! to manage dynamic format
character(len=40) :: fout

contains
! -----------------------------------------------------------------------------!

!============ Include EF utilities ============!
! only compute_gp2node subroutine at this time !
include 'EF_utilities.f90'
!==============================================!

! -----------------------------------------------------------------------------!
!> \brief Initialize a generic finite element
subroutine new_genericEF(EF, nb_nf, nb_eo)
  implicit none 
  !> the new finite element
  type(T_genEF_iso), intent(inout) :: EF
  !> number of nodal fields in EF
  integer(kind=4), intent(in) :: nb_nf
  !> number of elementary operator in EF
  integer(kind=4), intent(in) :: nb_eo
  ! ***
  character(len=20) :: IAM
  !      12345678901234567890
  IAM = 'a_genericEF_iso::new'

  EF%nb_nodal_field  = nb_nf
  EF%nb_ele_operator = nb_eo
  allocate(EF%node_field(nb_nf))
  allocate(EF%ele_operator(nb_eo))

end subroutine new_genericEF
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief Add a new nodal field to a finite element
subroutine add_node_field_genericEF(EF, rank, nf_dim, grad_dim, interp, nb_support, support)
  implicit none
  !> the finite element to which add the new nodal field
  type(T_genEF_iso), intent(inout) :: EF
  !> rank of the nodal field to add
  integer(kind=4), intent(in) :: rank
  !> dimension of the new nodal field
  integer(kind=4), intent(in) :: nf_dim
  !> dimension of the gradient of the new nodal field
  integer(kind=4), intent(in) :: grad_dim
  !> type of interpolation to compute the form functions
  integer(kind=4), intent(in) :: interp
  !> number of supporting nodes of the nodal field 
  integer(kind=4), intent(in) :: nb_support
  !> list of supporting nodes (among the nodes of the element) of the nodal field 
  integer(kind=4),dimension(nb_support), intent(in) :: support
  ! ***

  EF%node_field(rank)%nf_dim        = nf_dim
  EF%node_field(rank)%grad_dim      = grad_dim
  EF%node_field(rank)%interpolation = interp

  EF%node_field(rank)%nb_support = nb_support

  allocate(EF%node_field(rank)%support(nb_support))
  EF%node_field(rank)%support = support

  allocate(EF%node_field(rank)%l2g(nf_dim*nb_support))

  allocate(EF%node_field(rank)%dof_work(nf_dim*nb_support))
  allocate(EF%node_field(rank)%sca_work(nb_support))

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief Add a new elementary operator to a finite element
subroutine add_operator_genericEF(EF, rank, i_type, quadrature_id, nf_primal, nf_dual)
  implicit none
  !> the finite element to which add the new elementary operator
  type(T_genEF_iso), intent(inout) :: EF
  !> rank of the elementary operator to add
  integer(kind=4), intent(in) :: rank
  !> type id of the operator
  integer(kind=4), intent(in) :: i_type
  !> type of quadrature rule to use
  integer(kind=4), intent(in) :: quadrature_id
  !> index of primal node_field
  integer(kind=4), intent(in) :: nf_primal
  !> index of dual node_field
  integer(kind=4), intent(in) :: nf_dual
  ! ***
  !
  integer(kind=4) :: interpolation_id, grad_dim, flux_dim, nb_gp, nb_nodes

  ! ids
  EF%ele_operator(rank)%type_id    = i_type
  EF%ele_operator(rank)%quadrature = quadrature_id

  ! input and output node fields
  EF%ele_operator(rank)%nf_primal  = nf_primal
  EF%ele_operator(rank)%nf_dual    = nf_dual

  ! get or create gauss points data for the couple interpolation id of primal node field and the quadrature
  interpolation_id = EF%node_field(nf_primal)%interpolation
  EF%ele_operator(rank)%primal_GP => get_gauss_points_from_list(interpolation_id, quadrature_id)

  ! if primal and dual node fields are different
  ! get or create gauss points data for the couple interpolation id of dual node field and the quadrature
  if( interpolation_id == EF%node_field(nf_dual)%interpolation ) then
    EF%ele_operator(rank)%dual_GP => EF%ele_operator(rank)%primal_GP
  else
    interpolation_id = EF%node_field(nf_dual)%interpolation
    EF%ele_operator(rank)%dual_GP => get_gauss_points_from_list(interpolation_id, quadrature_id)
  end if

  nb_gp = size(EF%ele_operator(rank)%dual_GP)
  EF%ele_operator(rank)%nb_GP = nb_gp

  ! allocating working array
  allocate(EF%ele_operator(rank)%vec_work( EF%node_field(nf_dual)%nf_dim*EF%node_field(nf_dual)%nb_support ) )
  allocate(EF%ele_operator(rank)%mat_work( EF%node_field(nf_dual)%nf_dim*EF%node_field(nf_dual)%nb_support, &
                                           EF%node_field(nf_primal)%nf_dim*EF%node_field(nf_primal)%nb_support))
  allocate(EF%ele_operator(rank)%field_work(nb_gp))

  select case( i_type )
  case( p_Nut_Nu, p_Npt_Np )
    grad_dim = 0
    flux_dim = 0
  case( p_Npt_B )
    grad_dim = EF%node_field(nf_primal)%grad_dim
    flux_dim = 0
    allocate(EF%ele_operator(rank)%grad_work(grad_dim,nb_gp))
    EF%ele_operator(rank)%grad_work = 0.d0
  case( p_VNpt_Nu, p_Bt_Np )
    grad_dim = 0
    flux_dim = EF%node_field(nf_dual)%grad_dim
    allocate(EF%ele_operator(rank)%flux_work(flux_dim,nb_gp))
    EF%ele_operator(rank)%flux_work = 0.d0
  case( p_VNpt_VNp, p_Bt_B )
    grad_dim = EF%node_field(nf_primal)%grad_dim
    flux_dim = EF%node_field(nf_dual  )%grad_dim
    allocate(EF%ele_operator(rank)%grad_work(grad_dim,nb_gp))
    allocate(EF%ele_operator(rank)%flux_work(flux_dim,nb_gp))
    EF%ele_operator(rank)%grad_work = 0.d0
    EF%ele_operator(rank)%flux_work = 0.d0
  case default
    call faterr('a_genericEF_iso::add_operator','unknown operator type: '//get_operator_type_from_id(i_type))
  end select
  EF%ele_operator(rank)%vec_work   = 0.d0
  EF%ele_operator(rank)%mat_work   = 0.d0
  EF%ele_operator(rank)%field_work = 0.d0

  ! gp2node allocation and computation
  !
  ! Compute_gp2node works only for linear elements.
  ! Since the numbering of node is: first linear nodes, then quadratic nodes
  ! 1/ the map is allocated for the real order of the element
  ! 2/ compute_gp2node fills the linear part
  ! 3/ quadratic part is computed thanks to edge2vertices
  nb_nodes = EF%node_field(nf_dual)%nb_support
  allocate(EF%ele_operator(rank)%gp2node(nb_nodes,nb_gp))
  call compute_gp2node(interpolation_id,nb_nodes,quadrature_id,nb_gp,EF%ele_operator(rank)%gp2node)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief Initialize an finite element
!> Compute maps and number of degrees of freedom.
!> Can be done only once new add_node_field and add_operator have been called.
subroutine init_genericEF(EF)
  implicit none 
  !> the finite element to initialize
  type(T_genEF_iso), intent(inout) :: EF
  ! ***
  integer(kind=4)   :: idx, i_nf, i_dof, i_node, total_size, i_interp

  total_size = 0
  i_interp = 0
  do i_nf = 1, EF%nb_nodal_field
    total_size = total_size + EF%node_field(i_nf)%nb_support * EF%node_field(i_nf)%nf_dim
    i_interp = max(i_interp, EF%node_field(i_nf)%interpolation)
  end do
  EF%N_DOF = total_size

  ! building maps between dofs
  allocate(EF%dof2field_map(total_size))

  EF%dof2field_map = 0
  idx = 0
  do i_node = 1, EF%N_NODE
    do i_nf = 1, EF%nb_nodal_field
      if (any(EF%node_field(i_nf)%support == i_node)) then
        EF%dof2field_map(idx+1:idx+EF%node_field(i_nf)%nf_dim) = i_nf
        idx = idx + EF%node_field(i_nf)%nf_dim
      end if
    end do
  end do

  ! map for nodal fields
  do i_nf = 1, EF%nb_nodal_field
    idx = 0
    do i_dof = 1, total_size
      if( EF%dof2field_map(i_dof) == i_nf ) then
        idx = idx + 1
        EF%node_field(i_nf)%l2g(idx) = i_dof
      end if
    end do
  end do

  ! working array
  allocate(EF%coor_work(nbDIME,EF%N_NODE))
  allocate(EF%dofs_work(EF%N_DOF))

  EF%edge2vertices => get_ptr_edge2vertices(i_interp)

end subroutine init_genericEF

!> \brief compute the bandwith of the elementary matrix associated to a nodal field
integer function get_bw_node_field(nf,nodes)
  implicit none
  !> nodal field invovlved
  type(T_node_field), intent(in) :: nf
  !> list of nodes
  integer(kind=4), dimension(:), intent(in) :: nodes
  !
  integer(kind=4) :: i,j,tempo

  tempo=0
  get_bw_node_field = 0

  do i = 1, nf%nb_support - 1
    do j = i+1, nf%nb_support
      tempo = abs(nodes(nf%support(i)) - nodes(nf%support(j)))
      if( tempo .gt. get_bw_node_field) get_bw_node_field = tempo
    end do
  end do

  get_bw_node_field = (get_bw_node_field+1)*nf%nf_dim

end function get_bw_node_field
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief get the coordinates of the Gauss point for an elementary operator
subroutine get_coor_gp(EF, i_eo, X, coor_gp)
  implicit none
  !> generic EF
  type(T_genEF_iso), intent(in) :: EF
  !> elementary operator
  integer(kind=4), intent(in) :: i_eo
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in) :: X
  !> Gauss points coordinates
  real(kind=8), dimension(:,:), intent(out) :: coor_gp
  !
  integer(kind=4) :: i_gp, i_dime

  do i_gp = 1, EF%ele_operator(i_eo)%nb_GP
    do i_dime = 1, EF%node_field(EF%ele_operator(i_eo)%nf_primal)%nf_dim
      coor_gp(i_dime,i_gp) = dot_product(EF%ele_operator(i_eo)%primal_GP(i_gp)%N(:),X(i_dime,:))
    end do
  end do

end subroutine get_coor_gp

!> \brief get the values of field at Gauss point from values at nodes
!> Result is stored in field_work array of the operator
subroutine interpolate_node2gp(eo, valnoe)
  implicit none
  !> elementary operator
  type(T_ele_operator), intent(inout) :: eo
  !> values at nodes
  real(kind=8), dimension(:), intent(in) :: valnoe
  !
  integer(kind=4) :: i_gp

  do i_gp = 1, eo%nb_GP
    eo%field_work(i_gp) = dot_product(eo%dual_GP(i_gp)%N,valnoe)
  end do

end subroutine interpolate_node2gp

!> \brief get the values of field at nodes from values at Gauss points
!> Field used is the one stored in field_work array of the operator
subroutine gpv2node(eo, valnoe)
  implicit none
  !> elementary operator
  type(T_ele_operator), intent(in) :: eo
  !> values at nodes
  real(kind=8), dimension(:), intent(out) :: valnoe
  !
  integer(kind=4) :: i_gp

  valnoe = matmul(eo%gp2node,eo%field_work)

end subroutine gpv2node
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief compute mass
subroutine compute_Nut_Nu_operator(eo, nl, X, is_lump)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> is mass to be lumped
  logical, intent(in) :: is_lump
  !
  integer(kind=4) :: i_gp, ie, je, ke, in
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer     :: DNX
  real(kind=8), dimension(:,:), allocatable :: xn

  real(kind=8) :: mass,sum,alpha

  nullify(DNX)

  allocate(xn(nbDIME,nbDIME*size(nl)))

  eo%mat_work = 0.d0

  mass = 0.d0

  do i_gp = 1, eo%nb_GP
    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)

    mass  = mass  + COEFINT*eo%field_work(i_gp)

    xn = 0.d0
    select case(nbDIME)
    case(2)
      ie=0 ; je=0
      do in = 1, size(nl)
        ie=je+1; je=ie+1
        xn(1,ie) = eo%primal_GP(i_gp)%N(in)
        xn(2,je) = eo%primal_GP(i_gp)%N(in)
      end do
    case(3)
      ie=0; je=0; ke=0
      do in = 1, size(nl)
        ie=ke+1; je=ie+1; ke=je+1
        xn(1,ie) = eo%primal_GP(i_gp)%N(in)
        xn(2,je) = eo%primal_GP(i_gp)%N(in)
        xn(3,ke) = eo%primal_GP(i_gp)%N(in)
      end do
    end select

    eo%mat_work = eo%mat_work + eo%field_work(i_gp) * matmul(transpose(xn),xn) * coeFint

  end do

  if ( is_lump ) then

    if ((nbDIME == 2 .and. (size(nl) == 3 .or. size(nl) == 4                   )) .or. &
        (nbDIME == 3 .and. (size(nl) == 4 .or. size(nl) == 6 .or. size(nl) == 8))) then

      ! ele lineaire

      do ie = 1, size(nl)*nbDIME
        val = 0.d0
        do je = 1, size(nl)*nbDIME
          val = val + eo%mat_work(ie,je)
          if( ie /= je ) eo%mat_work(ie,je) = 0.d0
        end do
        eo%mat_work(ie,ie) = val
      end do

    else

      ! ele quadratique

      sum = 0.d0
	  
      DO ie=1,nbDIME*size(nl)
        sum = sum + eo%mat_work(ie, ie)
      END DO
      alpha = mass/sum

      DO ie=1,nbDIME*size(nl) 
        DO je=1,nbDIME*size(nl)
          IF (ie /= je)  eo%mat_work(ie, je)=0.d0
        END DO 
        eo%mat_work(ie, ie) = alpha*eo%mat_work(ie, ie) 
      END DO

    endif

  end if

  if( associated(DNX) ) deallocate(DNX)
  deallocate(xn)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine compute_NpT_Np_operator(eo,nl,X,is_lump)
  implicit none
  !>elementary operator to use to compute elementary matrix
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> is mass to be lumped
  logical, intent(in) :: is_lump
  !
  integer(kind=4) :: i_gp, ie, je, ke, in
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer :: DNX

  nullify(DNX)

  eo%mat_work = 0.d0

  do i_gp = 1, eo%nb_GP
    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)

    do ie = 1,size(nl) !Loop for outer product

      eo%mat_work(:,ie) = eo%mat_work(:,ie) + eo%field_work(i_gp)*eo%primal_GP(i_gp)%N*eo%primal_GP(i_gp)%N(ie)*coeFint

    end do

  end do

  if ( is_lump ) then
    do ie = 1, size(nl)
      val = 0.d0
      do je = 1, size(nl)
        val = val + eo%mat_work(ie,je)
        if( ie /= je ) eo%mat_work(ie,je) = 0.d0
      end do
      eo%mat_work(ie,ie) = val
    end do
  end if

  !fout = ' ' 
  !write(fout,'(A,I0,A)') "(",nbDIME*size(nl),"(1x,D12.5))" 
  !do ie = 1, size(nl) 
  !  write(*,trim(fout)) eo%mat_work(ie,:) 
  !enddo

  if( associated(DNX) ) deallocate(DNX)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine compute_VNpT_Nu_operator(eo, p_nl, d_nl, X)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: p_nl
  !> list of nodes of dual node field
  integer(kind=4), dimension(:), intent(in) :: d_nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !
  integer(kind=4) :: i_gp, ie, je, ke, in
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer :: DNX, xn

  nullify(DNX)
  allocate(xn(nbDIME,nbDIME*size(X,2)))

  eo%mat_work = 0.d0

  do i_gp = 1, eo%nb_GP
    call gradient_iso(eo%dual_GP(i_gp)%N,eo%dual_GP(i_gp)%DN, eo%dual_GP(i_gp)%POIDS, &
                      X(:,d_nl), DNX, coefInt, R)

    xn = 0.d0
    select case(nbDIME)
    case(2)
      ie=0 ; je=0
      do in = 1, size(p_nl)
        ie=je+1; je=ie+1
        xn(1,ie) = eo%primal_GP(i_gp)%N(in)
        xn(2,je) = eo%primal_GP(i_gp)%N(in)
      end do
    case(3)
      ie=0; je=0; ke=0
      do in = 1, size(p_nl)
        ie=ke+1; je=ie+1; ke=je+1
        xn(1,ie) = eo%primal_GP(i_gp)%N(in)
        xn(2,je) = eo%primal_GP(i_gp)%N(in)
        xn(3,ke) = eo%primal_GP(i_gp)%N(in)
      end do
    end select

    eo%mat_work = eo%mat_work + eo%field_work(i_gp) * matmul(transpose(DNX),xn) * coeFint

  end do

  !fout = ' ' 
  !write(fout,'(A,I0,A)') "(",nbDIME*size(X,2),"(1x,D12.5))" 
  !print*,'field gp 1= ',eo%field_work(1)
  !do ie = 1, size(d_nl) 
  !  write(*,trim(fout)) eo%mat_work(ie,:) 
  !enddo

  if( associated(DNX) ) deallocate(DNX)
  deallocate(xn)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine compute_VNpT_VNp_operator(eo, nl, X)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !
  integer(kind=4) :: i_gp, ie, je, ke, in
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer :: DNX

  nullify(DNX)

  eo%mat_work = 0.d0

  do i_gp = 1, eo%nb_GP
    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)

    eo%mat_work = eo%mat_work + eo%field_work(i_gp) * matmul(transpose(DNX),DNX) * coeFint

  end do

  if( associated(DNX) ) deallocate(DNX)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine compute_BT_Np_operator(eo, p_nl, d_nl, X)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node list
  integer(kind=4), dimension(:), intent(in) :: p_nl
  !> list of nodes of dual node list
  integer(kind=4), dimension(:), intent(in) :: d_nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !
  integer(kind=4) :: i_gp, ie, je, ke, in, istrg, i
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer     :: DNX !, Bl
  real(kind=8), dimension(:)  , pointer     :: Bl

  nullify(DNX, Bl)

  eo%mat_work = 0.d0
  istrg = 1
  
  do i_gp = 1, eo%nb_GP 
    call gradient_iso(eo%dual_GP(i_gp)%N,eo%dual_GP(i_gp)%DN, eo%dual_GP(i_gp)%POIDS, X(:,d_nl), DNX, coefInt, R)
    call Bl_iso_poro(eo%dual_GP(i_gp)%N, DNX, R, Bl)

    do ie = 1,size(p_nl) !Loop to do outer product
      eo%mat_work(:,ie) = eo%mat_work(:,ie) + eo%field_work(i_gp)*Bl*eo%primal_GP(i_gp)%N(ie)*coeFint
    end do
    
  end do

   ! print *, 'Bl : ', shape(Bl)

  if( associated(DNX) ) deallocate(DNX)
  if( associated(Bl)  ) deallocate(Bl)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
subroutine compute_Npt_B_operator(eo, p_nl, d_nl, X)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node list
  integer(kind=4), dimension(:), intent(in) :: p_nl
  !> list of nodes of dual node list
  integer(kind=4), dimension(:), intent(in) :: d_nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !
  integer(kind=4) :: i_gp, ie, je, ke, in, istrg, i
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer     :: DNX !, Bl
  real(kind=8), dimension(:)  , pointer     :: Bl

  nullify(DNX, Bl)

  eo%mat_work = 0.d0
  istrg = 1
  
  do i_gp = 1, eo%nb_GP 
    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X(:,p_nl), DNX, coefInt, R)
    call Bl_iso_poro(eo%primal_GP(i_gp)%N, DNX, R, Bl)

    do ie = 1,size(p_nl) !Loop to do outer product
      eo%mat_work(:,ie) = eo%mat_work(:,ie) + eo%field_work(i_gp)*Bl(ie)*eo%dual_GP(i_gp)%N(:)*coeFint
    end do
    
  end do

   ! print *, 'Bl : ', shape(Bl)

  if( associated(DNX) ) deallocate(DNX)
  if( associated(Bl)  ) deallocate(Bl)

end subroutine
! -----------------------------------------------------------------------------!
! -----------------------------------------------------------------------------!
!> Called compute B^T ___ B even if compute Stiffness
subroutine compute_BT_B_operator(eo, ppsnb, nl, X, U)
  implicit none
  !> elementary operator to use to compute elementary stiffness
  type(T_ele_operator), intent(inout) :: eo
  !> property set
  integer(kind=4), intent(in) :: ppsnb
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:)  , intent(in)  :: U
  !
  integer(kind=4) :: i_gp, istrg!, mdlnb, inull, nb_external
  real(kind=8)    :: coefInt, R, val
  real(kind=8), dimension(:,:), pointer   :: DNX, Bl, D!, xn

  nullify(DNX, Bl, D)

  eo%vec_work = 0.d0
  eo%mat_work = 0.d0
  istrg   = 1

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X, DNX, coefInt, R)
    call Bl_iso_meca(eo%primal_GP(i_gp)%N, DNX, R, istrg, Bl)    ! formation de Bl        
    eo%grad_work(:,i_gp) = matmul(Bl, U)
    !--------------------------------------------------------------------!
    ! to be done in models module                                        !
    call D_SOLID_ISO(ppsnb,D)                                            !
    call comp_stress(ppsnb,eo%grad_work(:,i_gp),eo%flux_work(:,i_gp))!
    !--------------------------------------------------------------------!
    eo%mat_work = eo%mat_work + matmul( transpose(Bl),matmul(D,Bl))*coeFint !  ke= Blt.D.Bl.coef
    eo%vec_work = eo%vec_work + matmul( transpose(Bl),eo%flux_work(1:size(Bl,dim=1),i_gp) )*coeFint

  end do

  if( associated(DNX) ) deallocate(DNX)
  if( associated(Bl)  ) deallocate(Bl)
  if( associated(D)   ) deallocate(D)

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief Compute new values of grad and flux fields for a BT_B type operator
subroutine compute_Bt_B_flux(eo, ppsnb, nl, X, U)
  implicit none
  !> elementary operator to use to compute flux
  type(T_ele_operator), intent(inout) :: eo
  !> property set
  integer(kind=4), intent(in) :: ppsnb
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:)  , intent(in)  :: U
  !
  integer(kind=4) :: i_gp, istrg
  real(kind=8)    :: coefInt, R
  real(kind=8), dimension(:,:), pointer   :: DNX, Bl, D

  nullify(DNX, Bl)

  istrg   = 1

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X, DNX, coefInt, R)
    call Bl_ISO_MECA(eo%primal_GP(i_gp)%N, DNX, R, istrg, Bl)    ! formation de Bl        
    eo%grad_work(:,i_gp) = matmul(Bl, U)
    !---------------------------------------------------------------------!
    ! to be done in models module                                         !
    call comp_stress(ppsnb,eo%grad_work(:,i_gp),eo%flux_work(:,i_gp)) !
    !---------------------------------------------------------------------!

  end do

  if( associated(DNX) ) deallocate(DNX)
  if( associated(Bl)  ) deallocate(Bl)
  if( associated(D)   ) deallocate(D)

end subroutine

!> \brief Compute new values of grad and flux fields for a VNpt_VNp type operator
subroutine compute_VNpt_VNp_flux(eo, ppsnb, nl, X, U)
  implicit none
  !> elementary operator to use to compute flux
  type(T_ele_operator), intent(inout) :: eo
  !> property set
  integer(kind=4), intent(in) :: ppsnb
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:)  , intent(in)  :: U
  !
  integer(kind=4) :: i_gp
  real(kind=8)    :: coefInt, R
  real(kind=8), dimension(:,:), pointer   :: DNX
  !
  !rm: there should be a function to compute the behaviour
  !    of material for this operator; currently the value
  !    of the field and the identity matrix i used

  nullify(DNX)

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)
    eo%grad_work(:,i_gp) = matmul(DNX, U)

    !fd & wsc correction signe le 20-12-2013
    eo%flux_work(:,i_gp) = -eo%field_work(i_gp) * eo%grad_work(:,i_gp)

  end do

  if( associated(DNX) ) deallocate(DNX)

end subroutine

!> \brief Compute new values of internal forces for a Bt_B type operator
subroutine compute_Bt_B_internal_f(eo, nl, X, U)
  implicit none
  !> elementary operator to use to compute flux
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:)  , intent(in)  :: U
  !!> new values of grad field
  !real(kind=8), dimension(:,:), intent(out) :: grad
  !!> new values of flux field
  !real(kind=8), dimension(:,:), intent(out) :: flux
  !
  integer(kind=4) :: i_gp, istrg
  real(kind=8)    :: coefInt, R
  real(kind=8), dimension(:,:), pointer   :: DNX, Bl

  nullify(DNX, Bl)

  istrg   = 1

  eo%vec_work = 0.d0

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X, DNX, coefInt, R)
    call Bl_ISO_MECA(eo%primal_GP(i_gp)%N, DNX, R, istrg, Bl)    ! formation de Bl        
    eo%vec_work = eo%vec_work + matmul( transpose(Bl),eo%flux_work(1:size(Bl,dim=1),i_gp) )*coeFint

  end do

  if( associated(DNX) ) deallocate(DNX)
  if( associated(Bl)  ) deallocate(Bl)

end subroutine

!> \brief Compute new values of external forces for a Bt_B type operator due to pressure divergence
subroutine compute_Bt_B_external_f_from_divp(eo, nl, X, P)
  use algebra
  implicit none
  !> elementary operator to use to compute flux
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:)  , intent(in)  :: P

  integer(kind=4)                              :: i_gp, inod, n_ne, idim, i, ic
  real(kind=8)                                 :: coefInt, R, div
  real(kind=8)   , dimension(:,:), pointer     :: gauss_coordinates
  real(kind=8)   , dimension(:)  , pointer     :: N, weights
!!$  real(kind=8)   , dimension(:,:), pointer     :: DNX
  real(kind=8)   , dimension(:,:), allocatable :: tmp, gp_field
  real(kind=8)   , dimension(:)  , allocatable :: pg
  integer(kind=4), dimension(:,:), allocatable :: idx
  real(kind=8) :: t(2),normal(2)

!!$  print*,'---X-coordinates'
!!$  write(*,'(2(1x,D12.5))') X
!!$  print*,'---P'
!!$  write(*,'(2(1x,D12.5))') P

  eo%vec_work = 0.d0

  n_ne = size(X)/nbdime

!!$  nullify(DNX)
  nullify(N, gauss_coordinates, weights)


!fd pas bon ... cette quantite est nulle

!!$  do i_gp = 1, eo%nb_GP
!!$
!!$    nullify(DNX)
!!$    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X, DNX, coefInt, R)
!!$
!!$    ! on somme sur une direction 
!!$    do idim=1,nbdime
!!$
!!$        div = 0.d0
!!$        ! pour chaque noeud de l'element
!!$        do iNod=1, n_ne
!!$         
!!$         ! on ajoute la contribution du noeud courant au calcul de la divergence
!!$          div = div + (P((iNod-1)*nbdime+idim) * DNX(idim, iNod))
!!$  
!!$        end do
!!$      
!!$        ! test: affichage de la divergence
!!$        ! print*, 'iG=', i_Gp, ' div=', div
!!$ 
!!$        ! pour chaque noeud de l'element
!!$        do iNod=1, n_ne
!!$       
!!$          ! on ajoute la contribution du point de Gauss courant
!!$          ! a la composante associee au noeud courant du vecteur
!!$          ! elementaire:
!!$          !   div(field)(x_iG)*N_iNod(x_iG)*w(iG)*det(J)(x_iG)
!!$
!!$          eo%vec_work(((iNod-1)*nbdime)+idim) = eo%vec_work(((iNod-1)*nbdime)+idim) + div*eo%primal_GP(i_gp)%N(iNod)*COEFINT
!!$
!!$          !print*,((iNod-1)*nbdime)+idim,eo%vec_work(((iNod-1)*nbdime)+idim)
!!$   
!!$        end do
!!$
!!$      enddo
!!$      ! on libere l'espce memoire occupe par DNX
!!$      deallocate(DNX)
!!$
!!$  end do

! fd il faut calculer la resultante du vecteur pression au travers des bords  

  if (nbdime == 2 .and. n_ne == 8) then

    !print*,'yess ... on y va'

    ! support Q8
    ! les coor d un bord & les index
    allocate(tmp(nbdime,3),idx(3,4))

    tmp = 0.d0

    idx(:,1) = (/ 1,2,5 /)
    idx(:,2) = (/ 2,3,6 /)
    idx(:,3) = (/ 3,4,7 /)
    idx(:,4) = (/ 4,1,8 /)

    call pos_gauss(i_lig2, gauss_coordinates, weights)
    allocate(gp_field(size(gauss_coordinates,dim=2),3),pg(size(gauss_coordinates,dim=2)))

    do ic=1,4
      tmp(:, 1) = X(:, idx(1,ic))
      tmp(:, 2) = X(:, idx(2,ic))
      tmp(:, 3) = X(:, idx(3,ic))

      t = tmp(:, 2) - tmp(:, 1)
      t = t / length2(t)

      normal(1) =   t(2)
      normal(2) = - t(1)

      !write(*,'(2(1x,D12.5))') normal

      ! on construit la pression aux gp
      pg = 0.d0
      do i_gp=1,size(gauss_coordinates,dim=2)

        CALL fonct_forme(i_L_P2,gauss_coordinates(:,i_gp),N)

        pg(i_gp) = pg(i_gp) + &
                   N(1)*P((idx(1,ic)-1)*nbdime+1) + &
                   N(2)*P((idx(2,ic)-1)*nbdime+1) + & 
                   N(3)*P((idx(3,ic)-1)*nbdime+1)

!!$                   N(1)*10000. + &
!!$                   N(2)*10000. + & 
!!$                   N(3)*10000.


      enddo

      !print*,'pression au pg'
      !write(*,'(2(1x,D12.5))') pg

      ! on construit la contribution des gp au noeuds (sans la normale qui est supposee constante sur le bord)
      gp_field=0.d0
      do i_gp=1,size(gauss_coordinates,dim=2)

        CALL fonct_forme(i_L_P2,gauss_coordinates(:,i_gp),N)

        gp_field(i_gp,1:3) = gp_field(i_gp,1:3) + (pg(i_gp) * N(1:3))

      enddo

      !print*,'ponderation pressions au pg par fonc forme'
      !write(*,'(2(1x,D12.5))') gp_field


      ! on calcule la contribution sur chaque noeud du bord
      ! boucle sur les noeuds
      do i=1,3

        call INTEGRATE_field(gp_field(:,i), 3, i_L_P2, i_lig2, 2, .FALSE., tmp, div)

        !print*,'contribution au noeud ',i,' = ',div

       ! 
       inod = idx(i,ic)
       eo%vec_work(((iNod-1)*nbdime)+1:iNod*nbdime) = eo%vec_work(((iNod-1)*nbdime)+1:iNod*nbdime) + div*normal(1:nbdime) 

      enddo      

    enddo  

    !write(*,'(2(1x,D12.5))') eo%vec_work


    deallocate(gp_field,pg,N)
    deallocate(tmp,idx,gauss_coordinates,weights)

  endif

end subroutine

!> \brief Compute energy of mass operator
function compute_Nut_Nu_energy(eo, nl, X, U)!, U2)
  implicit none
  !> elementary operator to use to compute energy
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> nodes degrees of freedom of primal node field
  real(kind=8), dimension(:,:)  , intent(in)  :: U
  !> something else... gravity ?
  !real(kind=8), dimension(:), intent(in), optional :: U2
  !> energy
  real(kind=8) :: compute_Nut_Nu_energy
  !
  integer(kind=4) :: i_gp, i
  real(kind=8)    :: coefInt, R
  real(kind=8), dimension(nbDIME) :: xn
  real(kind=8), dimension(:,:), pointer :: DNX

  nullify(DNX)

  compute_Nut_Nu_energy = 0.d0

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)

    xn = 0.d0
    do i = 1, nbDIME
      xn(i) = dot_product(eo%primal_GP(i_gp)%N(:),U(i,:))
    end do

    !if( present(U2) ) then
    !compute_Nut_Nu_energy = compute_Nut_Nu_energy  + &
    !                        eo%field_work(i_gp) * dot_product( U2, xn ) * coeFint
    !else
      compute_Nut_Nu_energy = compute_Nut_Nu_energy  + &
                              eo%field_work(i_gp) * 0.5 * dot_product( xn, xn ) * coeFint
    !end if

  end do

  if( associated(DNX) ) deallocate(DNX)

end function

!> \brief Compute energy of stiffness operator
function compute_Bt_B_energy(eo, nl, X)
  implicit none
  !> elementary operator to use to compute energy
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> energy
  real(kind=8) :: compute_Bt_B_energy
  !
  integer(kind=4) :: i_gp
  real(kind=8)    :: coefInt, R
  real(kind=8), dimension(:,:), pointer   :: DNX

  nullify(DNX)

  compute_Bt_B_energy = 0.d0

  do i_gp = 1, eo%nb_GP

    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, X(:,nl), DNX, coefInt, R)

    compute_Bt_B_energy = compute_Bt_B_energy  + 0.5 * dot_product( eo%grad_work(:,i_gp), eo%flux_work(:,i_gp) ) * coefint

  end do

  if( associated(DNX) ) deallocate(DNX)

end function

!> \brief compute volume
function compute_volume(eo, nl, X)
  implicit none
  !> elementary operator to use to compute elementary volume
  type(T_ele_operator), intent(inout) :: eo
  !> list of nodes of primal node field
  integer(kind=4), dimension(:), intent(in) :: nl
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> computed volume of the element
  real(kind=8) :: compute_volume
  !
  integer(kind=4) :: i_gp
  real(kind=8)    :: coefint, R
  real(kind=8), dimension(:,:), pointer :: DNX

  nullify(DNX)

  compute_volume = 0.d0

  do i_gp = 1, eo%nb_GP
    call gradient_iso(eo%primal_GP(i_gp)%N,eo%primal_GP(i_gp)%DN, eo%primal_GP(i_gp)%POIDS, &
                      X(:,nl), DNX, coefInt, R)

    compute_volume = compute_volume + coefint

  end do

  if( associated(DNX) ) deallocate(DNX)

end function

!> \brief compute center
subroutine genEF_compute_center(nf, X, center)
  implicit none
  !> node field  to use to compute elementary center
  type(T_node_field), intent(inout) :: nf
  !> nodes coordinates
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> computed center of the element
  real(kind=8), dimension(:), intent(out) :: center
  !

  call compute_center(nf%interpolation, X(:,nf%support), center)

end subroutine

! -----------------------------------------------------------------------------!
!> \brief Put the working matrix of an elementary operator in an augmented matrix
subroutine put_in_augmented_matrix(EF, i_eo, augmented_mat)
  implicit none
  !> Generic finite element
  type(T_genEF_iso) :: EF
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
  real(kind=8), dimension(:,:), intent(out) :: augmented_mat
  !
  ! local variables
  integer(kind=4) :: i_nf_p, i_nf_d, i, j

  i_nf_p = EF%ele_operator(i_eo)%nf_primal
  i_nf_d = EF%ele_operator(i_eo)%nf_dual

  do i = 1, size(EF%node_field(i_nf_p)%l2g)
    do j = 1, size(EF%node_field(i_nf_d)%l2g)
      augmented_mat( EF%node_field(i_nf_d)%l2g(j), EF%node_field(i_nf_p)%l2g(i) ) = EF%ele_operator(i_eo)%mat_work(j,i)
    end do
  end do

end subroutine
! -----------------------------------------------------------------------------!

!!$! -----------------------------------------------------------------------------!
!!$!> \brief Put the opposite transposed of the working matrix of an elementary operator in the symetric position of an augmented matrix
!!$subroutine put_ot_in_augmented_matrix(EF, i_eo, augmented_mat)
!!$  implicit none
!!$  !> Generic finite element
!!$  type(T_genEF_iso) :: EF
!!$  !> index of elementary operator to use
!!$  integer(kind=4), intent(in) :: i_eo
!!$  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
!!$  real(kind=8), dimension(:,:) :: augmented_mat
!!$  !
!!$  ! local variables
!!$  integer(kind=4) :: i_nf_p, i_nf_d, i, j
!!$
!!$  i_nf_p = EF%ele_operator(i_eo)%nf_primal
!!$  i_nf_d = EF%ele_operator(i_eo)%nf_dual
!!$
!!$  do j = 1, size(EF%node_field(i_nf_p)%l2g)
!!$    do i = 1, size(EF%node_field(i_nf_d)%l2g)
!!$      augmented_mat( EF%node_field(i_nf_p)%l2g(j), EF%node_field(i_nf_d)%l2g(i) ) = -EF%ele_operator(i_eo)%mat_work(i,j)
!!$    end do
!!$  end do
!!$
!!$end subroutine
!!$! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief Put the working vector computed by an elementary operator in an augmented vector
subroutine put_in_augmented_vector(EF, i_eo, augmented_vec)
  implicit none
  !> Generic finite element
  type(T_genEF_iso) :: EF
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented vector in which to store the vector computed by elementary operator i_eo
  real(kind=8), dimension(:)  :: augmented_vec
  !
  ! local variables
  integer(kind=4) :: i_nf_d, i

  i_nf_d = EF%ele_operator(i_eo)%nf_dual

  do i = 1, size(EF%node_field(i_nf_d)%l2g)
    augmented_vec( EF%node_field(i_nf_d)%l2g(i) ) = EF%ele_operator(i_eo)%vec_work(i)
  end do

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief deallocate memory within a genericEF
subroutine clean_genericEF(EF)
  implicit none
  type(T_genEF_iso) :: EF
  ! ***
  integer(kind=4) :: j

  if ( associated(EF%node_field) ) then
    do j = 1, size(EF%node_field)
        if( associated(EF%node_field(j)%support) ) deallocate(EF%node_field(j)%support)
        if( associated(EF%node_field(j)%l2g)     ) deallocate(EF%node_field(j)%l2g)
        if( associated(EF%node_field(j)%dof_work)) deallocate(EF%node_field(j)%dof_work)
        if( associated(EF%node_field(j)%sca_work)) deallocate(EF%node_field(j)%sca_work)
    end do
    deallocate(EF%node_field)
    nullify(EF%node_field)
  end if
  if ( associated(EF%ele_operator) ) then
    do j = 1, size(EF%ele_operator)
      ! no deallocation here since the memory is stored in all_gauss_points linked list
      nullify(EF%ele_operator(j)%primal_GP)
      nullify(EF%ele_operator(j)%dual_GP)
      if( associated(EF%ele_operator(j)%vec_work)  ) deallocate(EF%ele_operator(j)%vec_work)
      if( associated(EF%ele_operator(j)%mat_work)  ) deallocate(EF%ele_operator(j)%mat_work)
      if( associated(EF%ele_operator(j)%field_work)) deallocate(EF%ele_operator(j)%field_work)
      if( associated(EF%ele_operator(j)%grad_work) ) deallocate(EF%ele_operator(j)%grad_work)
      if( associated(EF%ele_operator(j)%flux_work) ) deallocate(EF%ele_operator(j)%flux_work)
      if( associated(EF%ele_operator(j)%gp2node)   ) deallocate(EF%ele_operator(j)%gp2node)
    end do
    deallocate(EF%ele_operator)
    nullify(EF%ele_operator)
  end if
  if( associated(EF%dof2field_map) ) deallocate(EF%dof2field_map)
  nullify(EF%dof2field_map)
  if( associated(EF%coor_work) ) deallocate(EF%coor_work)
  if( associated(EF%dofs_work) ) deallocate(EF%dofs_work)

  EF%NAME='-----'; EF%N_NODE=0; EF%N_DOF=0;
  EF%nb_nodal_field=0; EF%nb_ele_operator=0;

end subroutine

!> \brief deallocate memory within the module
!> Must NOT be called before all genericEF has been erased with clean_genericEF
subroutine clean_module()
  implicit none
  !
  type(T_link_pt_gauss), pointer :: current, tmp
  integer(kind=4) :: i

  if( associated(all_gauss_points) ) then
    current => all_gauss_points
  end if

  do while( associated(current) )
    if( associated(current%GP) ) then
      do i = 1, size(current%GP)
        if( associated(current%GP(i)%N ) ) deallocate(current%GP(i)%N)
        if( associated(current%GP(i)%DN) ) deallocate(current%GP(i)%DN)
      end do
      deallocate(current%GP)
    end if
    tmp => current%next
    deallocate(current)
    current => tmp
  end do

  all_gauss_points => null()

end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief display content of a generic finite element
!> For debug purpose only...
subroutine display_genericEF(EF)
  implicit none
  type(T_genEF_iso) :: EF
  ! ***
  integer(kind=4) :: i

  print *,'EF%name   : ', EF%name
  print *,'EF%n_node : ', EF%n_node
 
  print *,'EF%map    : fields attached to nodes '
  print *, EF%dof2field_map

 
  do i = 1, EF%nb_nodal_field
    print '(A,1x,I0,A,I0)', 'nodal field :', i,'/',EF%nb_nodal_field
    print *, '      dim   : ', EF%node_field(i)%nf_dim
    print *, '      inter : ', get_interpolation_from_id(EF%node_field(i)%interpolation)
    print *, '      nb_su : ', EF%node_field(i)%nb_support
    print *, '      supp  : ', EF%node_field(i)%support
    print *, '      lmap  : ', EF%node_field(i)%l2g
  end do

  do i = 1, EF%nb_ele_operator
    print '(A,1x,I0,A,I0)', 'ele operator:', i,'/',EF%nb_ele_operator
    print *, '      type  : ', get_operator_type_from_id(EF%ele_operator(i)%type_id)
    print *, '      quad  : ', get_quadrature_from_id(EF%ele_operator(i)%quadrature)
  end do
  
end subroutine
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief display data stored within the module
!> For debug purpose only...
subroutine display_module()
  implicit none
  !
  type(T_link_pt_gauss), pointer :: current
  integer(kind=4) :: i

  if( associated(all_gauss_points) ) then
    current => all_gauss_points
  end if

  do while( associated(current) )
    print *, 'Element of link list for : '
    print *, ' - interpolation  : ', get_interpolation_from_id(current%interpolation_id)
    print *, ' - quadrature     : ', get_quadrature_from_id(current%quadrature_id)
    print *, ' - nb Gauss point : ', size(current%GP)
    do i = 1, size(current%GP)
      print *, '    * point : ', i
      print *, '       + weight : ', current%GP(i)%POIDS
      print *, '       + forme  : ', current%GP(i)%N
      print *, '       + Dforme : ', current%GP(i)%DN
    end do
    current => current%next
  end do

end subroutine
! -----------------------------------------------------------------------------!
! =======================
! EF utilities
! =======================
! -----------------------------------------------------------------------------!
!> \brief Compute the gradient of form functions for real element
subroutine gradient_iso(N,DN,POIDS,X,DNX,COEFINT,R)
  implicit none
  !> form functions of reference element
  real(kind=8), dimension(:),   intent(in)  :: N
  !> derivative of form functions of reference element
  real(kind=8), dimension(:,:), intent(in)  :: DN
  !> weight of the Gauss point in reference element
  real(kind=8),                 intent(in)  :: POIDS
  !> node positions
  real(kind=8), dimension(:,:), intent(in)  :: X
  !> evalution of derivative of form functions of real element
  real(kind=8), dimension(:,:), pointer     :: DNX
  !> weight of the Gauss point in real element
  real(kind=8), intent(out) :: coefint
  !> radius in case of axisymetric
  real(kind=8), intent(out) :: R
  ! 
  ! Variables locales
  integer(kind=4) :: i1, i2, n_ne
  real(KIND=8)    :: detJ
  real(KIND=8), dimension(:,:), allocatable :: J, invJ
  character(len=28) :: IAM
  !      1234567890123456789012345678
  IAM = 'a_genericEF_iso::gradient_iso'

  ! number of nodes of the element
  n_ne = size(DN,2)

  ! Initialisation a vide des nouveaux pointeurs
  if( associated(DNX) ) then
    if( any(shape(DNX)/=(/nbDIME,n_ne/)) ) then
      deallocate(DNX)
      nullify(DNX)
    end if
  end if
  
  allocate(J(nbDIME,nbDIME),INVJ(nbDIME,nbDIME))
  if( .not. associated(DNX) ) allocate(DNX(nbDIME,n_ne))

  DNX = 0.d0
  J   = 0.d0

  ! Compute Jacobian matrix, its determinant, and its invert
  do i1 = 1, nbDIME
    do i2 = 1, nbDIME
      J(i2,i1) = dot_product(DN(i2,:),X(i1,:))
    end do
  end do

  select case(nbDIME)
  case(2)
    detJ = J(1,1)*J(2,2) - J(1,2)*J(2,1)
    invJ(1,:) = (/  J(2,2)/detJ ,-J(1,2)/detJ /)
    invJ(2,:) = (/ -J(2,1)/detJ , J(1,1)/detJ /)
  case(3)
    detJ = J(1,1) * ( J(2,2)*J(3,3) - J(2,3)*J(3,2) ) &
          -J(1,2) * ( J(2,1)*J(3,3) - J(3,1)*J(2,3) ) &
          +J(1,3) * ( J(2,1)*J(3,2) - J(3,1)*J(2,2) )     
    invJ(1,1) = ( J(2,2)*J(3,3) - J(3,2)*J(2,3) ) / detJ
    invJ(2,1) =-( J(2,1)*J(3,3) - J(3,1)*J(2,3) ) / detJ
    invJ(3,1) = ( J(2,1)*J(3,2) - J(3,1)*J(2,2) ) / detJ
    invJ(1,2) =-( J(1,2)*J(3,3) - J(3,2)*J(1,3) ) / detJ
    invJ(2,2) = ( J(1,1)*J(3,3) - J(3,1)*J(1,3) ) / detJ
    invJ(3,2) =-( J(1,1)*J(3,2) - J(3,1)*J(1,2) ) / detJ
    invJ(1,3) = ( J(1,2)*J(2,3) - J(2,2)*J(1,3) ) / detJ
    invJ(2,3) =-( J(1,1)*J(2,3) - J(2,1)*J(1,3) ) / detJ
    invJ(3,3) = ( J(1,1)*J(2,2) - J(2,1)*J(1,2) ) / detJ
  end select
  
  if( abs(detJ) < 1.E-16) then
    call faterr(IAM,'Matrice Jacobienne non inv. ')
  end if
  
  coefint = detJ * POIDS

  ! Compute gradient matrix at points (x,y)
  do i1 = 1, n_ne
    DNX(:,i1) = matmul(invJ,DN(:,i1))
  end do
  
  ! Calcul du rayon dans le cas axisymetrique
  if( DIME_mod == i_2D_axisym ) then
     ! Calcul du rayon
     R       = dot_product(N,X(1,:))
     coeFint = coeFint * (6.2831853_LONG) * R
  end if
  
  deallocate(J,invJ)
  
end subroutine gradient_iso
! -----------------------------------------------------------------------------!
!> \brief computes the Bl matrix (symmetric deformation) at a gauss point
!> See a_mecaEF_iso module for details on Bl storage
subroutine  Bl_iso_meca(N,DNX,R,istrg,Bl)
  implicit none
  !> interpolation polynome values at Gauss point
  real(kind=8), dimension(:)   :: N
  !> derivative of interpolation polynome values at Gauss point
  real(kind=8), dimension(:,:) :: DNX
  !> radius of the gauss point (axi) 
  real(kind=8)    :: R
  !> deformation storage (voigt=1, stainier=2)
  integer(kind=4) :: istrg
  !> symetric deformation
  real(kind=8), dimension(:,:), pointer :: Bl
  !
  ! Variable locale
  integer(kind=4) :: i, n_ne
  character(len=28) :: IAM
  !      1234567890123456789012345678
  IAM = 'a_genericEF_iso::Bl_iso_meca'

  ! number of nodes
  n_ne = size(N)

  ! Initialisation des nouveaux pointeurs
  if( associated(Bl) ) then
    deallocate(Bl)
    nullify(Bl)
  end if

  select case(DIME_mod)
  
  case(i_2D_stress,i_2D_strain)
  
    select case (istrg)
    case(1)
      ALLOCATE(Bl(3,2*N_NE))
      DO I=1,N_NE
         Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   0.d0      /)
         Bl(2,2*(I-1)+1:2*I) = (/   0.d0     ,  DNX(2,I)   /)
         Bl(3,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
      ENDDO
    case(2)
      ALLOCATE(Bl(4,2*N_NE))
      DO I=1,N_NE
         Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   0.d0      /)
         Bl(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
         Bl(3,2*(I-1)+1:2*I) = (/   0.d0     ,  DNX(2,I)   /)
         Bl(4,2*(I-1)+1:2*I) = (/   0.d0     ,   0.d0      /)
      ENDDO
    case default
      call faterr(IAM,'error allocating Bl')
    end select 
  
  case(i_2D_axisym)
    select case (istrg)
    case(1)
      ALLOCATE(Bl(4,2*N_NE))
      DO I=1,N_NE
         Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   0.d0      /)
         Bl(2,2*(I-1)+1:2*I) = (/   0.d0     ,  DNX(2,I)   /)
         Bl(3,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
         Bl(4,2*(I-1)+1:2*I) = (/  (N(I)/R)  ,   0.d0      /)
      ENDDO
    case(2)
      ALLOCATE(Bl(4,2*N_NE))
      DO I=1,N_NE
         Bl(1,2*(I-1)+1:2*I) = (/  DNX(1,I)  ,   0.d0      /)
         Bl(2,2*(I-1)+1:2*I) = (/  DNX(2,I)  ,  DNX(1,I)   /)
         Bl(3,2*(I-1)+1:2*I) = (/   0.d0     ,  DNX(2,I)   /)
         Bl(4,2*(I-1)+1:2*I) = (/  (N(I)/R)  ,   0.d0      /)
      ENDDO
    case default
      call faterr(IAM,'error allocating Bl')
    end select 
    
  case(i_3D)
    select case (istrg)
    case(1)
      ALLOCATE(Bl(6,3*N_NE))
      DO I=1,N_NE
         Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   0.d0      ,   0.d0      /)
         Bl(2,3*(I-1)+1:3*I) = (/   0.d0     ,  DNX(2,I)   ,   0.d0      /)
         Bl(3,3*(I-1)+1:3*I) = (/   0.d0     ,   0.d0      ,  DNX(3,I)   /)
         Bl(4,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   0.d0      /)
         Bl(5,3*(I-1)+1:3*I) = (/   0.d0     ,  DNX(3,I)   ,  DNX(2,I)   /)
         Bl(6,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   0.d0      ,  DNX(1,I)   /)      
      ENDDO
    case(2)
      ALLOCATE(Bl(6,3*N_NE))
      DO I=1,N_NE
         Bl(1,3*(I-1)+1:3*I) = (/  DNX(1,I)  ,   0.d0      ,   0.d0      /)
         Bl(2,3*(I-1)+1:3*I) = (/  DNX(2,I)  ,  DNX(1,I)   ,   0.d0      /)
         Bl(3,3*(I-1)+1:3*I) = (/   0.d0     ,  DNX(2,I)   ,   0.d0      /)
         Bl(4,3*(I-1)+1:3*I) = (/  DNX(3,I)  ,   0.d0      ,  DNX(1,I)   /)      
         Bl(5,3*(I-1)+1:3*I) = (/   0.d0     ,  DNX(3,I)   ,  DNX(2,I)   /)
         Bl(6,3*(I-1)+1:3*I) = (/   0.d0     ,   0.d0      ,  DNX(3,I)   /)
      ENDDO
    case default
      call faterr(IAM,'error allocating Bl')
    end select 
  
  case default
    call faterr(IAM,'Bl_iso not implemented')
  end select
 
end subroutine Bl_iso_meca
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief computes the Bl matrix (symmetric deformation) at a gauss point
!> See a_therEF_iso module for details on Bl storage
subroutine  Bl_iso_ther(N,DNX,Bl)
  implicit none
  !> interpolation polynome values at Gauss point
  real(kind=8), dimension(:)   :: N
  !> derivative of interpolation polynome values at Gauss point
  real(kind=8), dimension(:,:) :: DNX
  !> symetric deformation
  real(kind=8), dimension(:,:), pointer :: Bl
  !
  ! Variable locale
  integer(kind=4) :: i, n_ne
  character(len=28) :: IAM
  !      1234567890123456789012345678
  IAM = 'a_genericEF_iso::Bl_iso_ther'

  ! number of nodes
  n_ne = size(N)

  ! Initialisation des nouveaux pointeurs
  if( associated(Bl) ) then
    deallocate(Bl)
    nullify(Bl)
  end if

  select case(nbDIME)
  case(2)
    allocate(Bl(3,N_NE))
    do I=1,N_NE
       Bl(1,I) = DNX(1,I)
       Bl(2,I) = DNX(2,I)
       Bl(3,I) = ZERO
    end do
  
  case(3)
    allocate(Bl(3,N_NE))
    do I=1,N_NE
       Bl(1,I) = DNX(1,I)
       Bl(2,I) = DNX(2,I)
       Bl(3,I) = DNX(3,I)
    end do
  case default
    call faterr(IAM,'Bl not implemented')
  end select
 
end subroutine Bl_iso_ther
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief computes the Bl matrix (symmetric deformation) at a gauss point
!> See a_poroEF_iso module for details on Bl storage
subroutine  Bl_iso_poro(N,DNX,R,Bl)
  implicit none
  !> interpolation polynome values at Gauss point
  real(kind=8), dimension(:)   :: N
  !> derivative of interpolation polynome values at Gauss point
  real(kind=8), dimension(:,:) :: DNX
  !> radius of the gauss point (axi) 
  real(kind=8) :: R
  !> symetric deformation
  real(kind=8), dimension(:), pointer :: Bl
  !
  ! Variable locale
  integer(kind=4) :: i, n_ne
  character(len=28) :: IAM
  !      1234567890123456789012345678
  IAM = 'a_genericEF_iso::Bl_iso_poro'

  ! number of nodes
  n_ne = size(N)

  ! Initialisation des nouveaux pointeurs
  if( associated(Bl) ) then
    deallocate(Bl)
    nullify(Bl)
  end if

  allocate(Bl(nbDIME*n_ne))

  do i = 0, n_ne-1
    Bl(i*nbDIME+1) = DNX(1,i+1)
    Bl(i*nbDIME+2) = DNX(2,i+1)
  end do

  if( nbDIME == 3 ) then
    do i = 0, n_ne-1
      Bl(i*nbDIME+3) = DNX(3,i+1)
    end do
  end if

  if( DIME_mod == i_2D_axisym ) then
    do i = 0, n_ne-1
      Bl(i*nbDIME+1) = Bl(i*nbDIME+1) + N(i+1)/R
    end do
  end if

end subroutine Bl_iso_poro
!------------------------------------------------------------------------------!

!------------------------------------------------------------------------------!
!> Computes the N matrix (fonct form of pressure) at a gauss point
subroutine Np_iso(N_NE,N,B)
!------------------------------------------------------------------------------!
!                                                                              !
!   B  est de la forme                                                         !
!                                                                              !
!  EN 2D : BI  = [     N1        N2  ...                                       !
!  EN AXI              N1        N2  ...  ]                                    !
!                                                                              !
!  EN 3D : BlI = [     N1        N2  ...                                       !
!                      N1        N2  ...                                       !
!                      N1        N2  ...  ]                                    !
!------------------------------------------------------------------------------!

  implicit none
  integer        , intent(in)    :: N_NE
  real(kind=long)                :: N(:)
  real(kind=long), pointer       :: B(:,:)

  ! Variable locale
  integer                        :: i

  ! Initialisation des nouveaux pointeurs
  if(associated(B)) then ; deallocate(B) ; nullify(B) ; endif
  
  select case(dime_mod)
  
   case(i_2D_strain)
     allocate(b(2,n_ne))
     do i=1,n_ne
        B(1,i) = N(i)
        B(2,i) = N(i)
     enddo
  
   case(i_2D_axisym)
     allocate(b(2,N_NE))
     do i=1,n_ne
        B(1,i) = N(i)
        B(2,i) = N(i)
     enddo
     
   case(i_3D)
     allocate(b(3,N_NE))
     do i=1,n_ne
        B(1,i) = N(i)
        B(2,i) = N(i)
        B(3,i) = N(i)
     enddo
  
   case default
     call faterr('a_genericEF_iso::Np_iso','DIME NOT SUPPORTED '//get_dime_mode_name_from_id(dime_mod))
  
  end select
 
end subroutine Np_iso
! -----------------------------------------------------------------------------!
! linked list utilities
! -----------------------------------------------------------------------------!
!> \brief Get gauss points for an interpolation/quadrature couple if it exists
function get_gauss_points_from_list(interpolation_id, quadrature_id)
  implicit none
  !> pointer on the gauss points array if it exists for the interpolation/quadrature, null otherwise
  type(T_pt_gauss), dimension(:), pointer :: get_gauss_points_from_list
  !> interpolation id
  integer(kind=4), intent(in) :: interpolation_id
  !> quadrature id
  integer(kind=4), intent(in) :: quadrature_id
  !
  type(T_link_pt_gauss), pointer :: current, last

  get_gauss_points_from_list => null()

  if( .not. associated(all_gauss_points) ) then
    all_gauss_points => new_gauss_points(interpolation_id, quadrature_id)
    get_gauss_points_from_list => all_gauss_points%GP
    return
  end if

  current => all_gauss_points
  do while( associated(current) )
    if( current%quadrature_id==quadrature_id .and. current%interpolation_id==interpolation_id ) then
      get_gauss_points_from_list => current%GP
      return
    end if
    last    => current
    current => current%next
  end do

  current   => new_gauss_points(interpolation_id, quadrature_id)
  last%next => current

  get_gauss_points_from_list => current%GP

end function
! -----------------------------------------------------------------------------!


! -----------------------------------------------------------------------------!
!> \brief Create a new Gauss points array for an interpolation/quadrature couple put it in a link of a list and return it
function  new_gauss_points(interpolation_id, quadrature_id)
  implicit none
  !> pointer on the new list element with the needed Gauss points array
  type(T_link_pt_gauss), pointer :: new_gauss_points
  !> interpolation id
  integer(kind=4), intent(in) :: interpolation_id
  !> quadrature id
  integer(kind=4), intent(in) :: quadrature_id
  !
  integer(kind=4) :: i_gp, nb_GP
  real(kind=8), dimension(:),   pointer :: weights
  real(kind=8), dimension(:,:), pointer :: gauss_coordinates

  ! these will be allocated withing pos_gauss subroutine
  weights           => null()
  gauss_coordinates => null()

  ! create new link
  allocate(new_gauss_points)
  new_gauss_points%interpolation_id = interpolation_id
  new_gauss_points%quadrature_id    = quadrature_id

  ! allocate and initialize gauss_points structure
  nb_GP = get_nb_points_from_quadrature(quadrature_id)
  allocate(new_gauss_points%GP(nb_GP))

  call pos_gauss(quadrature_id, gauss_coordinates, weights)
  do i_gp = 1, nb_GP
    new_gauss_points%GP(i_gp)%POIDS = weights(i_gp)
    call fonct_forme(interpolation_id, gauss_coordinates(:,i_gp), new_gauss_points%GP(i_gp)%N)
    call derive_forme(interpolation_id, gauss_coordinates(:,i_gp), new_gauss_points%GP(i_gp)%DN)
  end do

  deallocate(weights, gauss_coordinates)
  nullify(weights, gauss_coordinates)

end function
! -----------------------------------------------------------------------------!

! -----------------------------------------------------------------------------!
!> \brief get type of operator from id
function get_operator_type_from_id(id)
  implicit none
  !> id of operator type
  integer(kind=4), intent(in) :: id
  !> name of a quadrature
  character(len=8) :: get_operator_type_from_id

  select case( id )
  case( p_Nut_Nu  )
    get_operator_type_from_id = 'Nut_Nu'
  case( p_Npt_Np  )
    get_operator_type_from_id = 'Npt_Np'
  case( p_VNpt_Nu )
    get_operator_type_from_id = 'VNpt_Nu'
  case( p_VNpt_VNp)
    get_operator_type_from_id = 'VNpt_VNp'
  case( p_Bt_Np  )
    get_operator_type_from_id = 'Bt_Np'
  case( p_Npt_B  )
    get_operator_type_from_id = 'Npt_B'
  case( p_Bt_B  )
    get_operator_type_from_id = 'Bt_B'
  case default
    get_operator_type_from_id = '--------'
  end select
end function
! -----------------------------------------------------------------------------!
 
end module a_genericEF_iso
