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
module a_multiEF_iso
! class Finite Element for multiphysics problems
! Basic computations on Finite Elements
! this class defines data type and methods and type

use utilities, only : faterr, &
                      logmes

use models, only : get_eleop_value

use a_genericEF_iso

implicit none

!> The array of generic EF for isoparametric multi models
type(T_genEF_iso), dimension(8), private :: multiEF

!> the number of node fields used by multi
integer(kind=4), parameter :: nb_nf = 3
!> the number of elementary operators used by multi
integer(kind=4), parameter :: nb_eo = 15

!> nodal fields ID
!> \todo : p_disp -> p_vel
integer(kind=4), parameter :: p_disp = 1          !displacement
integer(kind=4), parameter :: p_pc   = p_disp + 1 !capillary pressure (pc = pn - pw)
integer(kind=4), parameter :: p_pn   = p_pc   + 1 !non wetting fluid pressure

!> elementary operator ID
integer(kind=4), parameter :: p_mass_s      = 1                 ! solid mass
integer(kind=4), parameter :: p_mass_wf     = p_mass_s      + 1 ! wetting fluid mass
integer(kind=4), parameter :: p_mass_nwf    = p_mass_wf     + 1 ! non wetting fluid mass
integer(kind=4), parameter :: p_stiffness   = p_mass_nwf    + 1 ! solid stiffness
integer(kind=4), parameter :: p_wf_permy    = p_stiffness   + 1 ! wetting fluid permeability
integer(kind=4), parameter :: p_wf_permy_cpl= p_wf_permy    + 1 ! wetting fluid permeability second term 
integer(kind=4), parameter :: p_nwf_permy   = p_wf_permy_cpl+ 1 ! non wetting fluid permeability
integer(kind=4), parameter :: p_wf_compy    = p_nwf_permy   + 1 ! wetting fluid compressibility
integer(kind=4), parameter :: p_nwf_compy   = p_wf_compy    + 1 ! non wetting fluid compressibility
integer(kind=4), parameter :: p_pc2disp_cpl = p_nwf_compy   + 1 ! coupling from pc to disp
integer(kind=4), parameter :: p_pn2disp_cpl = p_pc2disp_cpl + 1 ! coupling from pn to disp
integer(kind=4), parameter :: p_disp2pc_cpl = p_pn2disp_cpl + 1 ! coupling from disp to pc
integer(kind=4), parameter :: p_disp2pn_cpl = p_disp2pc_cpl + 1 ! coupling from disp to pn
integer(kind=4), parameter :: p_pc2pn_cpl   = p_disp2pn_cpl + 1 ! coupling from pc to pn
integer(kind=4), parameter :: p_pn2pc_cpl   = p_pc2pn_cpl   + 1 ! coupling from pn to pc

contains

!============ some low level function ==================
!> \brief Get number of isoparametric multi finite elements
integer function get_nb_ele_iso(ghost)
  implicit none
  !> ?
  integer(kind=4), intent(in), optional :: ghost

  get_nb_ele_iso = size(multiEF)

end function get_nb_ele_iso

!> \brief Initialize finite elements
subroutine init_multiEF_iso
  implicit none
  logical :: is_initialize = .false.
  integer(kind=4)   :: i, tab(20)
  character(len=31) :: IAM
  !      1234567890123456789012345678901
  IAM = 'a_multiEF_iso::init_multiEF_iso'
  
  if( is_initialize ) return

  do i =1, 20
    tab(i) = i
  end do

  !TODO enlever les node_fields et operator inutiles!
  ! Declaration des elements finis
  ! EF 2D
  !
  ! Q8
  multiEF(1)%NAME          ='Q84xx'
  multiEF(1)%N_NODE        = 8

  !> \todo: B->Bu et VNp->Bp
  call new_genericEF(multiEF(1),nb_nf,nb_eo)
  call add_node_field_genericEF(multiEF(1), p_disp, 2, 4, i_q_p2, 8, tab(1:8))
  call add_node_field_genericEF(multiEF(1), p_pc  , 1, 2, i_q_p1, 4, tab(1:4))
  call add_node_field_genericEF(multiEF(1), p_pn  , 1, 2, i_q_p1, 4, tab(1:4))

  call add_operator_genericEF(multiEF(1), p_mass_s      , p_Nut_Nu  , i_q3x3, p_disp, p_disp)
  call add_operator_genericEF(multiEF(1), p_stiffness   , p_Bt_B    , i_q3x3, p_disp, p_disp)
  call add_operator_genericEF(multiEF(1), p_mass_wf     , p_VNpt_Nu , i_q2x2, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(1), p_mass_nwf    , p_VNpt_Nu , i_q2x2, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(1), p_wf_permy    , p_VNpt_VNp, i_q2x2, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(1), p_wf_permy_cpl, p_VNpt_VNp, i_q2x2, p_pn  , p_pc  )
  call add_operator_genericEF(multiEF(1), p_nwf_permy   , p_VNpt_VNp, i_q2x2, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(1), p_wf_compy    , p_Npt_Np  , i_q2x2, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(1), p_nwf_compy   , p_Npt_Np  , i_q2x2, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(1), p_pc2disp_cpl , p_Bt_Np   , i_q3x3, p_pc  , p_disp)
  call add_operator_genericEF(multiEF(1), p_pn2disp_cpl , p_Bt_Np   , i_q3x3, p_pn  , p_disp)
  call add_operator_genericEF(multiEF(1), p_disp2pc_cpl , p_Npt_B   , i_q2x2, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(1), p_disp2pn_cpl , p_Npt_B   , i_q2x2, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(1), p_pc2pn_cpl   , p_Npt_Np  , i_q2x2, p_pc  , p_pn  )
  call add_operator_genericEF(multiEF(1), p_pn2pc_cpl   , p_Npt_Np  , i_q2x2, p_pn  , p_pc  )

  ! T6
  multiEF(2)%NAME          ='T63xx'
  multiEF(2)%N_NODE        = 6
  call new_genericEF(multiEF(2),nb_nf,nb_eo)
  call add_node_field_genericEF(multiEF(2), p_disp, 2, 4, i_t_p2, 6, tab(1:6))
  call add_node_field_genericEF(multiEF(2), p_pc  , 1, 2, i_t_p1, 3, tab(1:3))
  call add_node_field_genericEF(multiEF(2), p_pn  , 1, 2, i_t_p1, 3, tab(1:3))

  call add_operator_genericEF(multiEF(2), p_mass_s      , p_Nut_Nu  , i_tr06, p_disp, p_disp)
  call add_operator_genericEF(multiEF(2), p_stiffness   , p_Bt_B    , i_tr06, p_disp, p_disp)
  call add_operator_genericEF(multiEF(2), p_mass_wf     , p_VNpt_Nu , i_tr03, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(2), p_mass_nwf    , p_VNpt_Nu , i_tr03, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(2), p_wf_permy    , p_VNpt_VNp, i_tr03, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(2), p_wf_permy_cpl, p_VNpt_VNp, i_tr03, p_pn  , p_pc  )
  call add_operator_genericEF(multiEF(2), p_nwf_permy   , p_VNpt_VNp, i_tr03, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(2), p_wf_compy    , p_Npt_Np  , i_tr03, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(2), p_nwf_compy   , p_Npt_Np  , i_tr03, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(2), p_pc2disp_cpl , p_Bt_Np   , i_tr06, p_pc  , p_disp)
  call add_operator_genericEF(multiEF(2), p_pn2disp_cpl , p_Bt_Np   , i_tr06, p_pn  , p_disp)
  call add_operator_genericEF(multiEF(2), p_disp2pc_cpl , p_Npt_B   , i_tr03, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(2), p_disp2pn_cpl , p_Npt_B   , i_tr03, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(2), p_pc2pn_cpl   , p_Npt_Np  , i_tr03, p_pc  , p_pn  )
  call add_operator_genericEF(multiEF(2), p_pn2pc_cpl   , p_Npt_Np  , i_tr03, p_pn  , p_pc  )

  ! TE10
  multiEF(3)%NAME          ='TE104'
  multiEF(3)%N_NODE        = 10
  call new_genericEF(multiEF(3),nb_nf,nb_eo)
  call add_node_field_genericEF(multiEF(3), p_disp, 3, 6, i_tep2,10, tab(1:10))
  call add_node_field_genericEF(multiEF(3), p_pc  , 1, 3, i_tep1, 4, tab(1:4))
  call add_node_field_genericEF(multiEF(3), p_pn  , 1, 3, i_tep1, 4, tab(1:4))

  call add_operator_genericEF(multiEF(3), p_mass_s      , p_Nut_Nu  , i_te04, p_disp, p_disp)
  call add_operator_genericEF(multiEF(3), p_stiffness   , p_Bt_B    , i_te01, p_disp, p_disp)
  call add_operator_genericEF(multiEF(3), p_mass_wf     , p_VNpt_Nu , i_te01, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(3), p_mass_nwf    , p_VNpt_Nu , i_te01, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(3), p_wf_permy    , p_VNpt_VNp, i_te01, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(3), p_wf_permy_cpl, p_VNpt_VNp, i_te01, p_pn  , p_pc  )
  call add_operator_genericEF(multiEF(3), p_nwf_permy   , p_VNpt_VNp, i_te01, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(3), p_wf_compy    , p_Npt_Np  , i_te04, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(3), p_nwf_compy   , p_Npt_Np  , i_te04, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(3), p_pc2disp_cpl , p_Bt_Np   , i_te04, p_pc  , p_disp)
  call add_operator_genericEF(multiEF(3), p_pn2disp_cpl , p_Bt_Np   , i_te04, p_pn  , p_disp)
  call add_operator_genericEF(multiEF(3), p_disp2pc_cpl , p_Npt_B   , i_te01, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(3), p_disp2pn_cpl , p_Npt_B   , i_te01, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(3), p_pc2pn_cpl   , p_Npt_Np  , i_te04, p_pc  , p_pn  )
  call add_operator_genericEF(multiEF(3), p_pn2pc_cpl   , p_Npt_Np  , i_te04, p_pn  , p_pc  )

  ! H8
  multiEF(4)%NAME          ='H8xxx'
  multiEF(4)%N_NODE        = 8
  call new_genericEF(multiEF(4),nb_nf,nb_eo)
  call add_node_field_genericEF(multiEF(4), p_disp, 3, 6, i_h_p1, 8, tab(1:8))
  call add_node_field_genericEF(multiEF(4), p_pc  , 1, 3, i_h_p1, 8, tab(1:8))
  call add_node_field_genericEF(multiEF(4), p_pn  , 1, 3, i_h_p1, 8, tab(1:8))

  call add_operator_genericEF(multiEF(4), p_mass_s      , p_Nut_Nu  , i_h222, p_disp, p_disp)
  call add_operator_genericEF(multiEF(4), p_stiffness   , p_Bt_B    , i_h222, p_disp, p_disp)
  call add_operator_genericEF(multiEF(4), p_mass_wf     , p_VNpt_Nu , i_h222, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(4), p_mass_nwf    , p_VNpt_Nu , i_h222, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(4), p_wf_permy    , p_VNpt_VNp, i_h222, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(4), p_wf_permy_cpl, p_VNpt_VNp, i_h222, p_pn  , p_pc  )
  call add_operator_genericEF(multiEF(4), p_nwf_permy   , p_VNpt_VNp, i_h222, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(4), p_wf_compy    , p_Npt_Np  , i_h222, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(4), p_nwf_compy   , p_Npt_Np  , i_h222, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(4), p_pc2disp_cpl , p_Bt_Np   , i_h222, p_pc  , p_disp)
  call add_operator_genericEF(multiEF(4), p_pn2disp_cpl , p_Bt_Np   , i_h222, p_pn  , p_disp)
  call add_operator_genericEF(multiEF(4), p_disp2pc_cpl , p_Npt_B   , i_h222, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(4), p_disp2pn_cpl , p_Npt_B   , i_h222, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(4), p_pc2pn_cpl   , p_Npt_Np  , i_h222, p_pc  , p_pn  )
  call add_operator_genericEF(multiEF(4), p_pn2pc_cpl   , p_Npt_Np  , i_h222, p_pn  , p_pc  )

  ! H20
  multiEF(5)%NAME          ='H208x'
  multiEF(5)%N_NODE        = 20
  call new_genericEF(multiEF(5),nb_nf,nb_eo)
  call add_node_field_genericEF(multiEF(5), p_disp, 3, 6, i_h_p2,20, tab(1:20))
  call add_node_field_genericEF(multiEF(5), p_pc  , 1, 3, i_h_p1, 8, tab(1:8))
  call add_node_field_genericEF(multiEF(5), p_pn  , 1, 3, i_h_p1, 8, tab(1:8))

  call add_operator_genericEF(multiEF(5), p_mass_s      , p_Nut_Nu  , i_h333, p_disp, p_disp)
  call add_operator_genericEF(multiEF(5), p_stiffness   , p_Bt_B    , i_h222, p_disp, p_disp)
  call add_operator_genericEF(multiEF(5), p_mass_wf     , p_VNpt_Nu , i_h222, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(5), p_mass_nwf    , p_VNpt_Nu , i_h222, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(5), p_wf_permy    , p_VNpt_VNp, i_h222, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(5), p_wf_permy_cpl, p_VNpt_VNp, i_h222, p_pn  , p_pc  )
  call add_operator_genericEF(multiEF(5), p_nwf_permy   , p_VNpt_VNp, i_h222, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(5), p_wf_compy    , p_Npt_Np  , i_h222, p_pc  , p_pc  )
  call add_operator_genericEF(multiEF(5), p_nwf_compy   , p_Npt_Np  , i_h222, p_pn  , p_pn  )
  call add_operator_genericEF(multiEF(5), p_pc2disp_cpl , p_Bt_Np   , i_h333, p_pc  , p_disp)
  call add_operator_genericEF(multiEF(5), p_pn2disp_cpl , p_Bt_Np   , i_h333, p_pn  , p_disp)
  call add_operator_genericEF(multiEF(5), p_disp2pc_cpl , p_Npt_B   , i_h222, p_disp, p_pc  )
  call add_operator_genericEF(multiEF(5), p_disp2pn_cpl , p_Npt_B   , i_h222, p_disp, p_pn  )
  call add_operator_genericEF(multiEF(5), p_pc2pn_cpl   , p_Npt_Np  , i_h222, p_pc  , p_pn  )
  call add_operator_genericEF(multiEF(5), p_pn2pc_cpl   , p_Npt_Np  , i_h222, p_pn  , p_pc  )

  do i = 1, 5
    call init_genericEF(multiEF(i))
  end do

  is_initialize = .true.

end subroutine init_multiEF_iso

!> \brief Get name of a multiEF from its ID
character(len=5) function get_NAME_multiEF_iso(nb)
  implicit none
  !> integer id of the finite element
  integer(kind=4) :: nb
  
  get_NAME_multiEF_iso = multiEF(nb)%NAME

end function get_NAME_multiEF_iso

!> \brief return the number of degrees of freedom of a node of an finite element
integer(kind=4) function get_N_DOF_of_NODE_multiEF_iso(nb, i_node)
  implicit none
  !> element number
  integer(kind=4), intent(in) :: nb
  !> node index
  integer(kind=4), intent(in) :: i_node
  !
  integer(kind=4)  :: i_nf

  get_N_DOF_of_NODE_multiEF_iso = 0
  do i_nf = 1, multiEF(nb)%nb_nodal_field
    if( any(i_node == multiEF(nb)%node_field(i_nf)%support) ) then
      get_N_DOF_of_NODE_multiEF_iso = get_N_DOF_of_NODE_multiEF_iso + multiEF(nb)%node_field(i_nf)%nf_dim
    end if
  end do

end function get_N_DOF_of_NODE_multiEF_iso

!> \brief return the number of degrees of freedom of a finite element
integer(kind=4) function get_N_DOF_multiEF_iso(nb)
  implicit none
  !> element number
  integer(kind=4), intent(in) :: nb
  !
  integer(kind=4)  :: i_nf

  get_N_DOF_multiEF_iso = multiEF(nb)%N_DOF

end function get_N_DOF_multiEF_iso

!> \brief return the number of Gauss points used by an operator
integer(kind=4) function get_N_GP_multiEF_iso(nb, i_eo)
  implicit none
  !> EF number
  integer(kind=4), intent(in) :: nb
  !> elementary operator id
  integer(kind=4), intent(in) :: i_eo

  get_N_GP_multiEF_iso = multiEF(nb)%ele_operator(i_eo)%nb_GP

end function get_N_GP_multiEF_iso

!> \brief return the size of grad/flux field associated to a node field
integer(kind=4) function get_grad_size_multiEF_iso(nb, i_nf)
  implicit none
  !> EF number
  integer(kind=4), intent(in) :: nb
  !> nodal field id
  integer(kind=4), intent(in) :: i_nf

  get_grad_size_multiEF_iso = multiEF(nb)%node_field(i_nf)%grad_dim

end function get_grad_size_multiEF_iso

!> \brief Put the working vector of an elementary operator in an augmented vector
subroutine put_in_augmented_vector_multiEF_iso(i,i_eo,augmented_vec)
  !> id of multiEF_iso
  integer(kind=4), intent(in) :: i
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented vector in which to store the vector computed by elementary operator i_eo
  real(kind=8), dimension(:), intent(out) :: augmented_vec

  call put_in_augmented_vector(multiEF(i),i_eo,augmented_vec)

end subroutine

!> \brief Put the working matrix of an elementary operator in an augmented matrix
subroutine put_in_augmented_matrix_multiEF_iso(i,i_eo,augmented_mat)
  !> id of multiEF_iso
  integer(kind=4), intent(in) :: i
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
  real(kind=8), dimension(:,:), intent(out) :: augmented_mat

  call put_in_augmented_matrix(multiEF(i),i_eo,augmented_mat)

end subroutine

!!$!> \brief Put the opposite transpose of the working matrix of an elementary operator in an augmented matrix
!!$subroutine put_ot_in_augmented_matrix_multiEF_iso(i,i_eo,augmented_mat)
!!$  !> id of multiEF_iso
!!$  integer(kind=4), intent(in) :: i
!!$  !> index of elementary operator to use
!!$  integer(kind=4), intent(in) :: i_eo
!!$  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
!!$  real(kind=8), dimension(:,:), intent(out) :: augmented_mat
!!$
!!$  call put_ot_in_augmented_matrix(multiEF(i),i_eo,augmented_mat)
!!$
!!$end subroutine

! ------------------------------------------------------------------------------
!
!> \brief compute elementary operator matrix
!> Switch between operator type
!> Uses internal work arrays for inputs and outputs
subroutine compute_elementary_operator_iso(i,i_eo,ppsnb,eviz)
  implicit none
  !> id of the multiEF in local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  logical           :: is_lump
  integer(kind=4)   :: i_p_nf, i_d_nf, mdlnb, lawnb, i_dof
  integer(kind=4), dimension(:)  , pointer :: p_nlist, d_nlist
  real(kind=8),    dimension(:)  , pointer :: p_dofs
  real(kind=8),    dimension(:,:), pointer :: X
  real(kind=8),    dimension(:)  , pointer :: defo

  ! fd & se merde
  real(kind=8) :: xmin,xmax,ymin,ymax,l0x,l0y

  character(len=46) :: IAM
  !      1234567890123456789012345678901234567890123456
  IAM = 'a_multiEF_iso::compute_elementary_operator_iso'




  ! on recupere les coordonnees des sommets: dans multiMAILx on utilise cooref  
  X => multiEF(i)%coor_work

  ! element erode ... on retire la contribution de rigidite de la structure
  if ( eviz == 0 ) then
    if ( i_eo == p_stiffness   .or. &
         i_eo == p_pc2disp_cpl .or. &     
         i_eo == p_pn2disp_cpl      ) then

      ! if element not visible, just zero and skip

      multiEF(i)%ele_operator(i_eo)%mat_work = 0.d0
      multiEF(i)%ele_operator(i_eo)%vec_work = 0.d0

!la masse reste attachee aux noeuds
!!$      ! is there stored value(s) better than this size ?
!!$      do i_dof = 1, size(multiEF(i)%ele_operator(i_eo)%mat_work,dim=1)
!!$        multiEF(i)%ele_operator(i_eo)%mat_work(i_dof,i_dof) = 1.d-10
!!$      end do

      return

    else if ( i_eo == p_wf_compy) then


      ! que faire ici !?
      
      !print*,'-'
      defo => compute_elementary_deformation_iso(i, p_stiffness, ppsnb)
      !print*,'+'

      !print*,defo(1:3)

      multiEF(i)%ele_operator(i_eo)%field_work=0.5d-9

      deallocate(defo)

    else if ( i_eo == p_wf_permy) then


      ! on essaie de calculer la permeabilite avec poiseuille 

      ! on essaie de calculer les longuers de reference l0x/l0y des elements 
      xmin = minval(X(1,:)) ; xmax = maxval(X(1,:))
      ymin = minval(X(2,:)) ; ymax = maxval(X(2,:))
      l0x= xmax - xmin
      l0y= ymax - ymin

      print*,'l0x= ',l0x,' l0y= ',l0y 

      print*,'--'
      defo => compute_elementary_deformation_iso(i, p_stiffness, ppsnb)
      print*,defo(1:3)

      print*,'dx= ',l0x*defo(1),' dy= ',l0y*defo(3) 
      
      print*,'++'

      multiEF(i)%ele_operator(i_eo)%field_work=5.5d-5

      deallocate(defo)

    end if
  end if

  is_lump = .false.
  call get_ppset_value(ppsnb,mdlnb,lawnb)

  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal
  i_d_nf = multiEF(i)%ele_operator(i_eo)%nf_dual
  p_nlist => multiEF(i)%node_field(i_p_nf)%support
  d_nlist => multiEF(i)%node_field(i_d_nf)%support
  p_dofs  => multiEF(i)%node_field(i_p_nf)%dof_work

  select case( multiEF(i)%ele_operator(i_eo)%type_id )
  case( p_Nut_Nu  )
    if( get_eleop_value(mdlnb,'mstrg') == 'lump_' ) is_lump = .true.
    call compute_Nut_Nu_operator(multiEF(i)%ele_operator(i_eo), p_nlist, X, is_lump)
  case( p_Npt_Np  )
    if( get_eleop_value(mdlnb,'fcstr') == 'lump_' ) is_lump = .true.
    call compute_Npt_Np_operator(multiEF(i)%ele_operator(i_eo), p_nlist, X, is_lump)
  case( p_VNpt_Nu )
    call compute_VNpt_Nu_operator(multiEF(i)%ele_operator(i_eo), p_nlist, d_nlist, X)
  case( p_VNpt_VNp)
    call compute_VNpt_VNp_operator(multiEF(i)%ele_operator(i_eo), p_nlist, X)
  case( p_Bt_Np  )
    call compute_Bt_Np_operator(multiEF(i)%ele_operator(i_eo), p_nlist, d_nlist, X)
  case( p_Npt_B  )
    call compute_Npt_B_operator(multiEF(i)%ele_operator(i_eo), p_nlist, d_nlist, X)
  case( p_Bt_B  )
    call compute_Bt_B_operator(multiEF(i)%ele_operator(i_eo), ppsnb, p_nlist, X, p_dofs)
  case default
    call faterr(IAM, 'Unknown operator type : '//&
                get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id))
  end select
  
  !print *, 'op : ', get_ele_operator_from_id(i_eo), get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id)
  !print *, multiEF(i)%ele_operator(i_eo)%mat_work
  
end subroutine compute_elementary_operator_iso

!> \brief compute elementary gp flux (a.k.a stress in mechanics)
!> Use an operator to compute new values
subroutine compute_elementary_flux_iso(i,i_eo,ppsnb,eviz)
  implicit none
  !> id of the multiEF in local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  integer(kind=4)   :: i_p_nf, i_d_nf, mdlnb, lawnb
  integer(kind=4), dimension(:)  , pointer :: p_nlist, d_nlist
  real(kind=8),    dimension(:)  , pointer :: p_dofs
  real(kind=8),    dimension(:,:), pointer :: X
  character(len=46) :: IAM
  !      123456789012345678901234567890123456789012
  IAM = 'a_multiEF_iso::compute_elementary_flux_iso'

  ! if element not visible, just zero and skip
  if( eviz == 0 ) then
    if ( i_eo == p_stiffness   .or. &
         i_eo == p_pc2disp_cpl .or. &     
         i_eo == p_pn2disp_cpl      ) then

      multiEF(i)%ele_operator(i_eo)%grad_work = 0.d0
      multiEF(i)%ele_operator(i_eo)%flux_work = 0.d0
      return
    endif
  end if

  call get_ppset_value(ppsnb,mdlnb,lawnb)

  X => multiEF(i)%coor_work

  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal
  p_nlist => multiEF(i)%node_field(i_p_nf)%support
  p_dofs  => multiEF(i)%node_field(i_p_nf)%dof_work

  select case( multiEF(i)%ele_operator(i_eo)%type_id )
  case( p_Bt_B  )
    call compute_Bt_B_flux(multiEF(i)%ele_operator(i_eo), ppsnb, p_nlist, X, p_dofs)
  case( p_VNpt_VNp )
    call compute_VNpt_VNp_flux(multiEF(i)%ele_operator(i_eo), ppsnb, p_nlist, X, p_dofs)
  case default
    call faterr(IAM, 'Unknown operator type : '//&
                get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id))
  end select
  
end subroutine compute_elementary_flux_iso

!> \brief compute elementary nodal internal forces/flux
!> Use an operator to compute new values
subroutine compute_elementary_internal_f_iso(i,i_eo,eviz)
  implicit none
  !> id of the multiEF in local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  integer(kind=4)   :: i_p_nf, i_d_nf, mdlnb, lawnb
  integer(kind=4), dimension(:)  , pointer :: p_nlist, d_nlist
  real(kind=8),    dimension(:)  , pointer :: p_dofs
  real(kind=8),    dimension(:,:), pointer :: X
  character(len=48) :: IAM
  !      123456789012345678901234567890123456789012345678
  IAM = 'a_multiEF_iso::compute_elementary_internal_f_iso'

  if( eviz == 0 ) then
    if ( i_eo == p_stiffness   .or. &
         i_eo == p_pc2disp_cpl .or. &     
         i_eo == p_pn2disp_cpl      ) then

      multiEF(i)%ele_operator(i_eo)%vec_work = 0.d0
      return
    endif
  endif

  X => multiEF(i)%coor_work

  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal
  p_nlist => multiEF(i)%node_field(i_p_nf)%support
  p_dofs  => multiEF(i)%node_field(i_p_nf)%dof_work

  select case( multiEF(i)%ele_operator(i_eo)%type_id )
  case( p_Bt_B  )
    call compute_Bt_B_internal_f(multiEF(i)%ele_operator(i_eo), p_nlist, X, p_dofs)
  case default
    call faterr(IAM, 'Unknown operator type : '//&
                get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id))
  end select
  
end subroutine compute_elementary_internal_f_iso

!> \brief compute elementary nodal body force due to an hydrostatic flux (a.k.a pressure)
!> take place only in eroded elements
!> Use an operator to compute new values
subroutine compute_elementary_external_body_f_from_divp_iso(i,i_eo)
  implicit none
  !> id of the multiEF in local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !
  integer(kind=4)   :: i_p_nf, i_d_nf, mdlnb, lawnb
  integer(kind=4), dimension(:)  , pointer :: p_nlist, d_nlist
  real(kind=8),    dimension(:)  , pointer :: p_dofs
  real(kind=8),    dimension(:,:), pointer :: X

  character(len=63) :: IAM
  !      123456789012345678901234567890123456789012345678901234567890123
  IAM = 'a_multiEF_iso::compute_elementary_external_body_f_from_divp_iso'


  X => multiEF(i)%coor_work

  ! on s'en fout c'est dans dofs_work
  ! nodal field rank (mecha)
  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal

  ! support nodes (mecha)
  p_nlist => multiEF(i)%node_field(i_p_nf)%support

  ! nodal field values at support nodes non !!!
  !p_dofs  => multiEF(i)%node_field(i_p_nf)%dof_work
  p_dofs  => multiEF(i)%dofs_work

  select case( multiEF(i)%ele_operator(i_eo)%type_id )
  ! working on mechanics
  case( p_Bt_B  )
    call compute_Bt_B_external_f_from_divp(multiEF(i)%ele_operator(i_eo), p_nlist, X, p_dofs)
  case default
    call faterr(IAM, 'Unknown operator type : '//&
                get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id))
  end select
  
end subroutine compute_elementary_external_body_f_from_divp_iso


!> \brief compute elementary volume of an element
function compute_elementary_volume_iso(i)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: i
  !> volume of the element
  real(kind=8) :: compute_elementary_volume_iso
  !
  real(kind=8)   , dimension(:,:), pointer :: X
  integer(kind=4), dimension(:)  , pointer :: p_nlist
  integer(kind=4)   :: nb, i_eo, i_p_nf
  character(len=44) :: IAM
  character(len=90) :: cout
  !      12345678901234567890123456789012345678901234
  IAM = 'a_multiEF_iso::compute_elementary_volume_iso'

  ! on utilise la quadrature de l'operateur de mass 
  ! pour calculer le  volume
  i_eo = p_mass_s 
  
  X => multiEF(i)%coor_work

  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal
  p_nlist => multiEF(i)%node_field(i_p_nf)%support
  
  compute_elementary_volume_iso = compute_volume(multiEF(i)%ele_operator(i_eo), p_nlist, X)

end function 


!> \brief compute elementary center of an element
subroutine compute_elementary_center_iso(i,center)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: i
  !> center of the element
  real(kind=8), dimension(:), intent(out) :: center
  !
  real(kind=8)   , dimension(:,:), pointer :: X
  integer(kind=4)   :: nb, i_nf
  character(len=44) :: IAM
  character(len=90) :: cout
  !      12345678901234567890123456789012345678901234
  IAM = 'a_multiEF_iso::compute_elementary_center_iso'

  ! on utilise l'interpolation du deplacement 
  ! pour calculer le centre
  i_nf = p_disp 
  
  X => multiEF(i)%coor_work

  call genEF_compute_center(multiEF(i)%node_field(i_nf), X, center)

end subroutine 

!> \brief Compute elementary energy associated to an operator
function compute_elementary_energy_iso(i, i_eo)!,g)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> gravity ?
  !real(kind=8), dimension(:), intent(in), optional :: g
  !> computed energy
  real(kind=8) :: compute_elementary_energy_iso
  !
  integer(kind=4) :: i_p_nf
  integer(kind=4), dimension(:)  , pointer :: p_nlist
  real(kind=8),    dimension(:)  , pointer :: p_dofs
  real(kind=8),    dimension(:,:), pointer :: X
  real(kind=8),    dimension(:,:), allocatable :: tmp
  character(len=34) :: IAM
  character(len=90) :: cout
  !      12345678901234567890123456789012345678901234
  IAM = 'a_multiEF_iso::compute_elementary_energy_iso'

  X => multiEF(i)%coor_work

  i_p_nf = multiEF(i)%ele_operator(i_eo)%nf_primal
  p_nlist => multiEF(i)%node_field(i_p_nf)%support
  
  select case( multiEF(i)%ele_operator(i_eo)%type_id )
  case( p_Nut_Nu )
    !> \todo: vomito
    p_dofs  => multiEF(i)%node_field(i_p_nf)%dof_work
    allocate(tmp(size(p_dofs)/size(p_nlist),size(p_nlist)))
    tmp = reshape(p_dofs,(/size(p_dofs)/size(p_nlist),size(p_nlist)/))
    !if( present(g) ) then
    !  compute_elementary_energy_iso = compute_Nut_Nu_energy(multiEF(i)%ele_operator(i_eo), p_nlist, X, tmp, g)
    !else
      compute_elementary_energy_iso = compute_Nut_Nu_energy(multiEF(i)%ele_operator(i_eo), p_nlist, X, tmp)
    !end if
    deallocate(tmp)
  case( p_Bt_B )
    compute_elementary_energy_iso = compute_Bt_B_energy(multiEF(i)%ele_operator(i_eo), p_nlist, X)
  case default
    call faterr(IAM, 'Unknown operator type for energy computation: '//&
                get_operator_type_from_id(multiEF(i)%ele_operator(i_eo)%type_id))
  end select

end function
  
!> \brief Compute elementary jacobian associated to the gradient field of an operator
function compute_elementary_jacobian_iso(i, i_eo, ppsnb)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> computed jacobian
  real(kind=8) :: compute_elementary_jacobian_iso
  !
  logical :: is_small
  integer(kind=4) :: i_gp
  real(kind=8), dimension(:,:), pointer :: grad
  

  is_small = .true.
  !call get_ppset_value(ppsnb(1),mdlnb,lawnb)
  !if( get_eleop_value(mdlnb,'kine_') == 'large' ) is_small = .false.

  grad => multiEF(i)%ele_operator(i_eo)%grad_work

  compute_elementary_jacobian_iso = 0.d0
  do i_gp = 1, multiEF(i)%ele_operator(i_eo)%nb_GP
    if( is_small ) then
      compute_elementary_jacobian_iso = compute_elementary_jacobian_iso + &
                                        (1.0d0 + grad(1,i_gp) + grad(2,i_gp))
     ! fd pour matlib
     ! compute_elementary_jacobian_iso = compute_elementary_jacobian_iso + &
     !                                   (1.0d0 + grad(1,i_gp) + grad(3,i_gp) + grad(4,i_gp))
    else
      compute_elementary_jacobian_iso = compute_elementary_jacobian_iso + &
                                        ((grad(1,i_gp)*grad(4,i_gp)) - (grad(2,i_gp)*grad(3,i_gp)))
    end if
  end do

  compute_elementary_jacobian_iso = compute_elementary_jacobian_iso / multiEF(i)%ele_operator(i_eo)%nb_GP
  
end function


!> \brief Compute elementary jacobian associated to the gradient field of an operator
function compute_elementary_deformation_iso(i, i_eo, ppsnb)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: i
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> computed deformation
  real(kind=8),dimension(:),pointer :: compute_elementary_deformation_iso
  !
  logical :: is_small
  integer(kind=4) :: i_gp
  real(kind=8), dimension(:,:), pointer :: grad
  
  is_small = .true.

  grad => multiEF(i)%ele_operator(i_eo)%grad_work

  !print*,size(grad,dim=1)

  allocate(compute_elementary_deformation_iso(size(grad,dim=1)))

  compute_elementary_deformation_iso = 0.d0
  do i_gp = 1, multiEF(i)%ele_operator(i_eo)%nb_GP
    if( is_small ) then
      compute_elementary_deformation_iso(:) = compute_elementary_deformation_iso(:) + grad(:,i_gp)
    else
      compute_elementary_deformation_iso = 0.d0

    end if
  end do

  compute_elementary_deformation_iso = compute_elementary_deformation_iso / multiEF(i)%ele_operator(i_eo)%nb_GP
  
  !print*,compute_elementary_deformation_iso

end function


!> \brief Compute elementary deforamtion energy associated to an operator
function compute_elementary_deformation_energy_iso(nb, i_eo, mat)
  implicit none
  !> id of the multiEF in the local list
  integer(kind=4), intent(in) :: nb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> augmented matrix to extract matrix to compute energy
  real(kind=8), dimension(:,:), intent(in) :: mat
  !> computed deformation_energy
  real(kind=8) :: compute_elementary_deformation_energy_iso
  !
  integer(kind=4) :: i, j, li, lj, i_p_nf, i_d_nf
  real(kind=8), dimension(:)  , pointer :: prim_ele_vec, dual_ele_vec
  real(kind=8), dimension(:,:), pointer :: ele_mat

  i_p_nf = multiEF(nb)%ele_operator(i_eo)%nf_primal
  i_d_nf = multiEF(nb)%ele_operator(i_eo)%nf_dual

  prim_ele_vec => multiEF(nb)%node_field(i_p_nf)%dof_work
  dual_ele_vec => multiEF(nb)%node_field(i_d_nf)%dof_work

  ele_mat => multiEF(nb)%ele_operator(i_eo)%mat_work

  do j = 1, size(ele_mat,2)
    lj = multiEF(nb)%node_field(i_p_nf)%l2g(j)
    do i = 1, size(ele_mat,1)
      li = multiEF(nb)%node_field(i_d_nf)%l2g(i)
      ele_mat(i,j) = mat(li,lj)
    end do
  end do

  compute_elementary_deformation_energy_iso = 0.5 * dot_product(dual_ele_vec, matmul(ele_mat,prim_ele_vec))
  
end function

!*************************************************************************

!*************************************************************************
!> \brief Compute values at Gauss points from values at nodes
!> Use field working array of operator for output
subroutine interpolate_node2gp_multiEF_iso(i,i_eo,valnoe)
  implicit none
  !> finite element number
  integer(kind=4), intent(in) :: i
  !> elementary operator id
  integer(kind=4), intent(in) :: i_eo
  !> values at nodes of the element
  real(kind=8), dimension(:), intent(in) :: valnoe
  !
  integer(kind=4) :: i_d_nf
  integer(kind=4), dimension(:), pointer :: d_nlist
  
  i_d_nf = multiEF(i)%ele_operator(i_eo)%nf_dual
  d_nlist => multiEF(i)%node_field(i_d_nf)%support

  call interpolate_node2gp(multiEF(i)%ele_operator(i_eo),valnoe(d_nlist))

end subroutine interpolate_node2gp_multiEF_iso

!*************************************************************************
!------------------------------------------------------------------------
!> \brief Compute values at nodes from values at Gauss points of the field_work array
!> Use field working array of operator for output
subroutine gpv2node_multiEF_iso(i,i_eo)
  implicit none
  !> finite element number
  integer(kind=4), intent(in) :: i
  !> elementary operator id
  integer(kind=4), intent(in) :: i_eo
  !
  integer(kind=4) :: i_d_nf
  integer(kind=4), dimension(:), pointer :: d_nlist
  real(kind=8)   , dimension(:), pointer :: valnoe
  
  i_d_nf  =  multiEF(i)%ele_operator(i_eo)%nf_dual
  valnoe  => multiEF(i)%node_field(i_d_nf)%sca_work

  call gpv2node(multiEF(i)%ele_operator(i_eo),valnoe)

end subroutine gpv2node_multiEF_iso

!------------------------------------------------------------------------

integer function get_bw_multiEF_iso(nb,nodes)
  implicit none
  integer(kind=4), intent(in) :: nb
  integer(kind=4), dimension(:), intent(in) :: nodes
  !
  integer(kind=4) :: i_nf

  get_bw_multiEF_iso = 0
  do i_nf = 1, multiEF(nb)%nb_nodal_field
    get_bw_multiEF_iso = get_bw_multiEF_iso + get_bw_node_field(multiEF(nb)%node_field(i_nf),nodes)
  end do

end function get_bw_multiEF_iso

!> \brief Get pointer on working array of coordinates of a finite element
function get_ptr_coor_ele_multiEF_iso(nb)
  implicit none
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> returned pointer on working array of coordinates
  real(kind=8), dimension(:,:), pointer :: get_ptr_coor_ele_multiEF_iso

  get_ptr_coor_ele_multiEF_iso => multiEF(nb)%coor_work

end function

!> \brief Get pointer on working array of dofs of a finite element
function get_ptr_dofs_ele_multiEF_iso(nb)
  implicit none
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> returned pointer on working array of dofs
  real(kind=8), dimension(:), pointer :: get_ptr_dofs_ele_multiEF_iso

  get_ptr_dofs_ele_multiEF_iso => multiEF(nb)%dofs_work

end function

!> \brief Get pointer on field working array of an operator
function get_ptr_field_ele_multiEF_iso(nb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on input field working array
  real(kind=8), dimension(:), pointer :: get_ptr_field_ele_multiEF_iso

  get_ptr_field_ele_multiEF_iso => multiEF(nb)%ele_operator(i_eo)%field_work

end function

!> \brief Get pointer on grad field working array of an operator
function get_ptr_grad_field_ele_multiEF_iso(nb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on grad field working array
  real(kind=8), dimension(:,:), pointer :: get_ptr_grad_field_ele_multiEF_iso

  get_ptr_grad_field_ele_multiEF_iso => multiEF(nb)%ele_operator(i_eo)%grad_work

end function

!> \brief Get pointer on flux field working array of an operator
function get_ptr_flux_field_ele_multiEF_iso(nb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on flux field working array
  real(kind=8), dimension(:,:), pointer :: get_ptr_flux_field_ele_multiEF_iso

  get_ptr_flux_field_ele_multiEF_iso => multiEF(nb)%ele_operator(i_eo)%flux_work

end function

!> \brief Get pointer on working array of a node field
function get_ptr_node_field_multiEF_iso(nb,i_nf)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> id of the node field to get working arrays of
  integer(kind=4), intent(in) :: i_nf
  !> returned pointer on working array
  real(kind=8), dimension(:), pointer :: get_ptr_node_field_multiEF_iso

  get_ptr_node_field_multiEF_iso => multiEF(nb)%node_field(i_nf)%dof_work

end function

!> \brief Get pointer on scalar working array of a node field
function get_ptr_node_scalar_field_multiEF_iso(nb,i_nf)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> id of the node field to get working arrays of
  integer(kind=4), intent(in) :: i_nf
  !> returned pointer on scalare working array
  real(kind=8), dimension(:), pointer :: get_ptr_node_scalar_field_multiEF_iso

  get_ptr_node_scalar_field_multiEF_iso => multiEF(nb)%node_field(i_nf)%sca_work

end function

!> \brief Get pointer on edge2vertices map of an element
function get_ptr_edge2vertices_multiEF_iso(nb)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: nb
  !> returned pointer on edge2vertices map
  integer(kind=4), dimension(:,:), pointer :: get_ptr_edge2vertices_multiEF_iso

  get_ptr_edge2vertices_multiEF_iso => multiEF(nb)%edge2vertices

end function

! -----------------------
! parameters utilities

!> \brief get name of nodal field from id
function get_nodal_field_from_id(id)
  implicit none
  !> id of a nodal field
  integer(kind=4), intent(in) :: id
  !> name of a nodal field
  character(len=15) :: get_nodal_field_from_id

  select case( id )
  case( p_disp )
    get_nodal_field_from_id = 'displacement'
  case( p_pc )
    get_nodal_field_from_id = 'wet fluid P '
  case( p_pn )
    get_nodal_field_from_id = 'non wet fluid P'
  case default
    get_nodal_field_from_id = '---------------'
  end select

end function

!> \brief get name of an elementary operator from id
function get_ele_operator_from_id(id)
  implicit none
  !> id of an elementary operator
  integer(kind=4), intent(in) :: id
  !> name of an elementary operator
  character(len=19) :: get_ele_operator_from_id

  select case( id )
  case( p_mass_s      )
    get_ele_operator_from_id = 'solid mass'
  case( p_stiffness   )
    get_ele_operator_from_id = 'stiffness'
  case( p_mass_wf     )
    get_ele_operator_from_id = 'wf mass'
  case( p_mass_nwf    )
    get_ele_operator_from_id = 'nwf mass'
  case( p_wf_permy    )
    get_ele_operator_from_id = 'wf permeability'
  case( p_nwf_permy   )
    get_ele_operator_from_id = 'nwf permeability'
  case( p_wf_compy    )
    get_ele_operator_from_id = 'wf compressiblity'
  case( p_nwf_compy   )
    get_ele_operator_from_id = 'nwf compressiblity'
  case( p_pc2disp_cpl )
    get_ele_operator_from_id = 'pc 2 disp coupling'
  case( p_pn2disp_cpl )
    get_ele_operator_from_id = 'pn 2 disp coupling'
  case( p_pc2pn_cpl   )
    get_ele_operator_from_id = 'pc 2 pn coupling'
  case( p_pn2pc_cpl   )
    get_ele_operator_from_id = 'pn 2 pc coupling'
  case default                 !1234567890123456789
    get_ele_operator_from_id = '------------------'
  end select

end function

end module a_multiEF_iso


