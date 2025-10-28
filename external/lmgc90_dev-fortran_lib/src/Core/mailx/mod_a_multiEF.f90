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


!> generic API to multi-phasic finite element 
module a_multiEF

use utilities

use a_EF

use a_multiEF_iso

implicit none

!> switch between different type of model of multiEF
type T_multiEF
  !> type id of model (iso, bar, shell)
  integer(kind=4)  :: id
  !> rank of the finite element
  integer(kind=4)  :: nb
  !> nickname
  character(len=5) :: name
end type T_multiEF

!> number of multiEF
integer(kind=4) :: nb_multiEF = 0

!> list of elements
type(T_multiEF), dimension(:), allocatable :: multiEF

interface get_N_DOF_of_NODE_multiEF
  module procedure get_N_DOF_of_NODE_multiEF_by_name, &
                   get_N_DOF_of_NODE_multiEF_by_id
end interface get_N_DOF_of_NODE_multiEF
interface get_N_GP_multiEF
  module procedure get_N_GP_multiEF_by_name, &
                   get_N_GP_multiEF_by_id
end interface get_N_GP_multiEF

contains

!> \brief Initialize module
subroutine init_multiEF
  implicit none
  !
  integer(kind=4) :: i,j
  !                         12345678901234567890123
  character(len=23) :: IAM='a_multiEF::init_multiEF'

  !rm : for reload...
  if( nb_multiEF > 0 ) return

  ! type initialization
  call init_multiEF_iso
  
  ! hash table build
  nb_multiEF = nb_multiEF + get_nb_ele_iso()

  allocate(multiEF(nb_multiEF))
  
  j=0
  do i = 1, get_nb_ele_iso()
    multiEF(i+j)%ID = i_iso
    multiEF(i+j)%nb = i
    multiEF(i+j)%NAME = get_NAME_multiEF_iso(i)
  end do
  j = j + get_nb_ele_iso()
  
end subroutine init_multiEF

!=======================================================================
!> \brief give back index in array of the finite element named NAME
integer(kind=4) function get_nb_in_multiEF(NAME)
  implicit none
  !> nickname of the multiEF
  character(len=5), intent(in) :: NAME
  !
  integer(kind=4)  :: i
  character(len=28) :: IAM
  character(len=30) :: cout
  !      1234567890123456789012345678
  IAM = 'a_multiEF::get_nb_in_multiEF'

  do i = 1, size(multiEF)
   if (multiEF(i)%NAME == NAME) then
     get_nb_in_multiEF = i
     return
   end if
  end do
                         
  write(cout,'(A,A5,A)') 'multiEF ',NAME,' unknown'
  call FATERR(IAM,cout)

end function get_nb_in_multiEF

!=======================================================================
!> \brief return index of finite element in multiEFmodel array
integer(kind=4) function get_nb_in_multiEFmodel(i)
  implicit none
  !> index of the finite element in multiEF
  integer(kind=4), intent(in) :: i

  get_nb_in_multiEFmodel = multiEF(i)%nb
  return
end function get_nb_in_multiEFmodel

!=======================================================================
!> \brief return the ID (iso/bar/shell) of the element
integer(kind=4) function ele_is(nb)
  implicit none
  !> index of the finite element
  integer(kind=4), intent(in) :: nb

  ele_is=multiEF(nb)%ID

end function ele_is

!=======================================================================
!*iNTEGER FUNCTION get_bw_multiEF(i,nodes)
!*!
!*! fonction qui retourne la largueur de bande de l'element
!*! d'indice i dans multiEF
!*!
!*  IMPLICIT NONE
!*  INTEGER :: nb,i
!*  INTEGER,DIMENSION(:) :: nodes
!*!
!*  nb=get_nb_in_multiEFmodel(i)
!*
!*  SELECT CASE (ele_is(i))
!*    CASE('iso  ')
!*      get_bw_multiEF=get_bw_multiEF_iso(nb,nodes)
!*  END SELECT
!*
!*!!  print*,'element de type: ',ele_is(i)
!*!!  print*,'numero dans le type:',nb
!*!!  print*,'connectivite   : ',nodes
!*!!  print*,'largeur de bande:',get_bw_multiEF
!*
!*eND FUNCTION get_bw_multiEF
!*

!> \brief Put the working vector of an elementary operator in an augmented vector
subroutine put_in_augmented_vector_multiEF(blmnb,i_eo,augmented_vec)
  !> id of multiEF
  integer(kind=4), intent(in) :: blmnb
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented vector in which to store the vector computed by elementary operator i_eo
  real(kind=8), dimension(:), intent(out) :: augmented_vec
  !
  integer(kind=4) :: nb

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call put_in_augmented_vector_multiEF_iso(nb,i_eo,augmented_vec)
  case default
    call faterr('a_multiEF::put_in_augmented_vector','unknown type of element')
  end select
end subroutine

!> \brief Put the working matrix of an elementary operator in an augmented matrix
subroutine put_in_augmented_matrix_multiEF(blmnb,i_eo,augmented_mat)
  !> id of multiEF
  integer(kind=4), intent(in) :: blmnb
  !> index of elementary operator to use
  integer(kind=4), intent(in) :: i_eo
  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
  real(kind=8), dimension(:,:), intent(out) :: augmented_mat
  !
  integer(kind=4) :: nb

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call put_in_augmented_matrix_multiEF_iso(nb,i_eo,augmented_mat)
  case default
    call faterr('a_multiEF::put_in_augmented_matrix','unknown type of element')
  end select
end subroutine

!!$!> \brief Put the opposite transpose of the working matrix of an elementary operator in an augmented matrix
!!$subroutine put_ot_in_augmented_matrix_multiEF(blmnb,i_eo,augmented_mat)
!!$  !> id of multiEF
!!$  integer(kind=4), intent(in) :: blmnb
!!$  !> index of elementary operator to use
!!$  integer(kind=4), intent(in) :: i_eo
!!$  !> the augmented matrix in which to store the matrix computed by elementary operator i_eo
!!$  real(kind=8), dimension(:,:), intent(out) :: augmented_mat
!!$  !
!!$  integer(kind=4) :: nb
!!$
!!$  nb = get_nb_in_multiEFmodel(blmnb)
!!$
!!$  select case (ele_is(blmnb))
!!$  case(i_iso)
!!$    call put_ot_in_augmented_matrix_multiEF_iso(nb,i_eo,augmented_mat)
!!$  case default
!!$    call faterr('a_multiEF::put_ot_in_augmented_matrix','unknown type of element')
!!$  end select
!!$end subroutine

!=================================================================
!============= Calcul des matrices elementaires ==================
!=================================================================

!============ compute elementary operator matrices ===
!
!> \brief compute elementary operator of an element
!> Uses internal work array for inputs and outputs
subroutine compute_elementary_operator(blmnb,i_eo,ppsnb,eviz)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  integer(kind=4)   :: nb
  character(len=38) :: IAM
  character(len=80) :: cout
  !      12345678901234567890123456789012345678
  IAM = 'a_multiEF::compute_elementary_operator'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call compute_elementary_operator_iso(nb,i_eo,ppsnb,eviz)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end subroutine compute_elementary_operator

!============ compute elementary flux ===
!
!> \brief compute elementary flux of an element
subroutine compute_elementary_flux(blmnb,i_eo,ppsnb,eviz)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  integer(kind=4)   :: nb
  character(len=34) :: IAM
  character(len=80) :: cout
  !      1234567890123456789012345678901234
  IAM = 'a_multiEF::compute_elementary_flux'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call compute_elementary_flux_iso(nb,i_eo,ppsnb,eviz)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end subroutine compute_elementary_flux

!> \brief compute elementary internal flux/forces of an operator
subroutine compute_elementary_internal_f(blmnb,i_eo,eviz)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> is the element visible or not
  integer(kind=4), intent(in) :: eviz
  !
  integer(kind=4)   :: nb
  character(len=34) :: IAM
  character(len=90) :: cout
  !      1234567890123456789012345678901234567890
  IAM = 'a_multiEF::compute_elementary_internal_f'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call compute_elementary_internal_f_iso(nb,i_eo,eviz)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end subroutine compute_elementary_internal_f

!> \brief compute elementary body force due to pressure divergence of an operator
subroutine compute_elementary_external_body_f_from_divp(blmnb,i_eo)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !
  integer(kind=4)   :: nb
  character(len=45) :: IAM
  character(len=90) :: cout
  !      1234567890123456789012345678901234567890123456789012345
  IAM = 'a_multiEF::compute_elementary_external_body_f_from_divp'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call compute_elementary_external_body_f_from_divp_iso(nb,i_eo)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end subroutine compute_elementary_external_body_f_from_divp


!> \brief compute elementary volume of an element
function compute_elementary_volume(blmnb)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> volume of the element
  real(kind=8) :: compute_elementary_volume
  !
  integer(kind=4)   :: nb
  character(len=36) :: IAM
  character(len=90) :: cout
  !      123456789012345678901234567890123456
  IAM = 'a_multiEF::compute_elementary_volume'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    compute_elementary_volume = compute_elementary_volume_iso(nb)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end function compute_elementary_volume

!> \brief compute geometric center of an element
subroutine compute_elementary_center(blmnb, center)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> center of element
  real(kind=8), dimension(:), intent(out) :: center
  !
  integer(kind=4)   :: nb
  character(len=36) :: IAM
  character(len=90) :: cout
  !      123456789012345678901234567890123456
  IAM = 'a_multiEF::compute_elementary_center'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call compute_elementary_center_iso(nb,center)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end subroutine compute_elementary_center

!> \brief Compute elementary energy associated to an operator
function compute_elementary_energy(blmnb, i_eo)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> computed energy
  real(kind=8) :: compute_elementary_energy
  !
  integer(kind=4)   :: nb
  character(len=26) :: IAM
  character(len=90) :: cout
  !      123456789012345678901234567890123456
  IAM = 'a_multiEF::compute_elementary_energy'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    compute_elementary_energy = compute_elementary_energy_iso(nb,i_eo)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end function

!> \brief Compute elementary jacobian associated to the grad field of an operator
function compute_elementary_jacobian(blmnb, i_eo, ppsnb)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> computed jacobian
  real(kind=8) :: compute_elementary_jacobian
  !
  integer(kind=4)   :: nb
  character(len=28) :: IAM
  character(len=90) :: cout
  !      12345678901234567890123456789012345678
  IAM = 'a_multiEF::compute_elementary_jacobian'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    compute_elementary_jacobian = compute_elementary_jacobian_iso(nb,i_eo,ppsnb)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end function

!> \brief Compute elementary deformation
function compute_elementary_deformation(blmnb, i_eo, ppsnb)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> property set rank of the element
  integer(kind=4), intent(in) :: ppsnb
  !> computed jacobian
  real(kind=8),dimension(:),pointer :: compute_elementary_deformation
  !
  integer(kind=4)   :: nb
  character(len=31) :: IAM
  character(len=90) :: cout
  !      12345678901234567890123456789012345678901
  IAM = 'a_multiEF::compute_elementary_deformation'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    compute_elementary_deformation = compute_elementary_deformation_iso(nb,i_eo,ppsnb)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end function

!> \brief Compute elementary deformation energy associated to the an operator
function compute_elementary_deformation_energy(blmnb, i_eo, mat)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the elementary_operator
  integer(kind=4), intent(in) :: i_eo
  !> augmented matrix to extract from
  real(kind=8), dimension(:,:), intent(in) :: mat
  !> computed deformation energy
  real(kind=8) :: compute_elementary_deformation_energy
  !
  integer(kind=4)   :: nb
  character(len=48) :: IAM
  character(len=90) :: cout
  !      123456789012345678901234567890123456789012345678
  IAM = 'a_multiEF::compute_elementary_deformation_energy'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    compute_elementary_deformation_energy = compute_elementary_deformation_energy_iso(nb,i_eo,mat)
  case default
    call faterr(IAM,'unknown type of element')
  end select

end function

!*!===================================================
!*!============= low level routines ==================
!*!===================================================
!=======================================================================
!> \brief return the number of degrees of freedom of a node of a finite element
integer(kind=4) function get_N_DOF_of_NODE_multiEF_by_name(NAME, i_node)
  implicit none
  !> finite element name
  character(len=5), intent(in) :: NAME
  !> index of the node in the element numbering
  integer(kind=4), intent(in) :: i_node
  !
  integer(kind=4) :: i,nb

  i  = get_nb_in_multiEF(NAME)  
  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_N_DOF_of_NODE_multiEF_by_name = get_N_DOF_of_NODE_multiEF_iso(nb, i_node)
  case default
    call faterr('a_multiEF::get_N_DOF_of_NODE_multiEF_by_name','unknown type of element')
  end select

end function get_N_DOF_of_NODE_multiEF_by_name

!> \brief return the number of degrees of freedom of a node of a finite element
integer(kind=4) function get_N_DOF_of_NODE_multiEF_by_id(i, i_node)
  implicit none
  !> finite element nunber
  integer(kind=4), intent(in) :: i
  !> index of the node in the element numbering
  integer(kind=4), intent(in) :: i_node
  !
  integer(kind=4) :: nb

  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_N_DOF_of_NODE_multiEF_by_id = get_N_DOF_of_NODE_multiEF_iso(nb, i_node)
  case default
    call faterr('a_multiEF::get_N_DOF_of_NODE_multiEF_by_id','unknown type of element')
  end select

end function get_N_DOF_of_NODE_multiEF_by_id

!> \brief return the number of degrees of freedom of a finite element
integer(kind=4) function get_N_DOF_multiEF(NAME)
  implicit none
  !> finite element name
  character(len=5), intent(in) :: NAME
  !
  integer(kind=4) :: i,nb

  i  = get_nb_in_multiEF(NAME)  
  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_N_DOF_multiEF = get_N_DOF_multiEF_iso(nb)
  case default
    call faterr('a_multiEF::get_N_DOF_multiEF','unknown type of element')
  end select

end function get_N_DOF_multiEF

!=======================================================================
!> \brief return the number of Gauss points used by an elmentary operator
integer(kind=4) function get_N_GP_multiEF_by_name(NAME, i_eo)
  implicit none
  !> model name
  character(len=5), intent(in) :: NAME
  !> id of an elementary operator
  integer(kind=4), intent(in) :: i_eo
  !
  integer          :: i, nb
  
  i  = get_nb_in_multiEF(NAME)  
  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_N_GP_multiEF_by_name = get_N_GP_multiEF_iso(nb, i_eo)
  case default
    call faterr('a_multiEF::get_n_GP_multiEF_by_name','unknown type of element')
  end select

end function get_N_GP_multiEF_by_name

!> \brief return the number of Gauss points used by an elmentary operator
integer(kind=4) function get_N_GP_multiEF_by_id(i, i_eo)
  implicit none
  !> model id
  integer(kind=4), intent(in) :: i
  !> id of an elementary operator
  integer(kind=4), intent(in) :: i_eo
  !
  integer(kind=4) :: nb
  
  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_N_GP_multiEF_by_id = get_N_GP_multiEF_iso(nb, i_eo)
  case default
    call faterr('a_multiEF::get_n_GP_multiEF_by_id','unknown type of element')
  end select

end function get_N_GP_multiEF_by_id

!> \brief get the dimensio of the grad/flux array of a physic
integer(kind=4) function get_grad_size_multiEF(i,i_nf)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: i
  !> id of an node field
  integer(kind=4), intent(in) :: i_nf
  !
  integer(kind=4) :: nb

  nb = get_nb_in_multiEFmodel(i)

  select case (ele_is(i))
  case(i_iso)
    get_grad_size_multiEF = get_grad_size_multiEF_iso(nb, i_nf)
  case default
    call faterr('a_multiEF::get_grad_size','unknown type of element')
  end select

end function get_grad_size_multiEF

!> \brief get the values of field at Gauss point from values at nodes
!> Use field working array of operator for output
subroutine interpolate_node2gp_multiEF(blmnb,i_eo,valnoe)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the operator to use
  integer(kind=4), intent(in) :: i_eo
  !> values at nodes of the element
  real(kind=8), dimension(:), intent(in) :: valnoe
  !
  integer(kind=4) :: nb
  
  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call interpolate_node2gp_multiEF_iso(nb,i_eo,valnoe)
  case default
    call faterr('a_multiEF::interoplate_node2gp_multiEF','unknown model')
  end select

end subroutine interpolate_node2gp_multiEF

!> \brief get the values of field at nodes from values at Gauss points
!> Use field working array of operator as input and scalar node field working array as output
subroutine gpv2node_multiEF(blmnb,i_eo)
  implicit none
  !> id of the multiEF
  integer(kind=4), intent(in) :: blmnb
  !> id of the operator to use
  integer(kind=4), intent(in) :: i_eo
  !
  integer(kind=4) :: nb
  
  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    call gpv2node_multiEF_iso(nb,i_eo)
  case default
    call faterr('a_multiEF::gpv2node_multiEF','unknown model')
  end select

end subroutine gpv2node_multiEF

!*!=======================================================================
!*INTEGER FUNCTION get_local_connectivity_edge_multiEF(i,nodes,num)
!*!
!*! fonction qui retourne le noeud de la edge
!*!
!*  IMPLICIT NONE
!*  INTEGER :: num,i,nodes,nb
!*!
!*  nb=get_nb_in_multiEFmodel(i)
!*
!*  SELECT CASE (ele_is(i))
!*    CASE('iso  ')
!*      get_local_connectivity_edge_multiEF = get_local_connectivity_edge_multiEF_iso(nb,nodes,num)
!*  END SELECT
!*
!*END FUNCTION get_local_connectivity_edge_multiEF

!> \brief Get pointer on working array of coordinates of a finite element
function get_ptr_coor_ele_multiEF(blmnb)
  implicit none
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> returned pointer on working array of coordinates
  real(kind=8), dimension(:,:), pointer :: get_ptr_coor_ele_multiEF
  !
  integer(kind=4)   :: nb
  character(len=35) :: IAM
  !      12345678901234567890123456789012345
  IAM = 'a_multiEF::get_ptr_coor_ele_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_coor_ele_multiEF => get_ptr_coor_ele_multiEF_iso(nb)
  case default
    call FATERR(IAM,'unknown model')
  end select
end function

!> \brief Get pointer on working array of dofs of a finite element
function get_ptr_dofs_ele_multiEF(blmnb)
  implicit none
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> returned pointer on working array of dofs
  real(kind=8), dimension(:), pointer :: get_ptr_dofs_ele_multiEF
  !
  integer(kind=4)   :: nb
  character(len=35) :: IAM
  !      12345678901234567890123456789012345
  IAM = 'a_multiEF::get_ptr_dofs_ele_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_dofs_ele_multiEF => get_ptr_dofs_ele_multiEF_iso(nb)
  case default
    call FATERR(IAM,'unknown model')
  end select
end function

!> \brief Get pointer on field working array of an operator of a finite element
function get_ptr_field_ele_multiEF(blmnb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on field working array
  real(kind=8), dimension(:), pointer :: get_ptr_field_ele_multiEF
  !
  integer :: nb
  character(len=36) :: IAM
  !      123456789012345678901234567890123456
  IAM = 'a_multiEF::get_ptr_field_ele_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_field_ele_multiEF => get_ptr_field_ele_multiEF_iso(nb,i_eo)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

!> \brief Get pointer on grad field working array of an operator of a finite element
function get_ptr_grad_field_ele_multiEF(blmnb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on grad field working array
  real(kind=8), dimension(:,:), pointer :: get_ptr_grad_field_ele_multiEF
  !
  integer :: nb
  character(len=33) :: IAM
  !      123456789012345678901234567890123
  IAM = 'a_multiEF::get_ptr_grad_field_ele'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_grad_field_ele_multiEF => get_ptr_grad_field_ele_multiEF_iso(nb,i_eo)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

!> \brief Get pointer on flux field working array of an operator of a finite element
function get_ptr_flux_field_ele_multiEF(blmnb,i_eo)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> id of the operator to get working arrays of
  integer(kind=4), intent(in) :: i_eo
  !> returned pointer on flux field working array
  real(kind=8), dimension(:,:), pointer :: get_ptr_flux_field_ele_multiEF
  !
  integer :: nb
  character(len=33) :: IAM
  !      123456789012345678901234567890123
  IAM = 'a_multiEF::get_ptr_flux_field_ele'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_flux_field_ele_multiEF => get_ptr_flux_field_ele_multiEF_iso(nb,i_eo)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

!> \brief Get pointer on working array of a node field of a finite element
function get_ptr_node_field_multiEF(blmnb,i_nf)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> id of the node field to get working arrays of
  integer(kind=4), intent(in) :: i_nf
  !> returned pointer on node field working array
  real(kind=8), dimension(:), pointer :: get_ptr_node_field_multiEF
  !
  integer :: nb
  character(len=37) :: IAM
  !      1234567890123456789012345678901234567
  IAM = 'a_multiEF::get_ptr_node_field_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_node_field_multiEF => get_ptr_node_field_multiEF_iso(nb,i_nf)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

!> \brief Get pointer on scalar working array of a node field of a finite element
function get_ptr_node_scalar_field_multiEF(blmnb,i_nf)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> id of the node field to get working arrays of
  integer(kind=4), intent(in) :: i_nf
  !> returned pointer on scalar node field working array
  real(kind=8), dimension(:), pointer :: get_ptr_node_scalar_field_multiEF
  !
  integer :: nb
  character(len=45) :: IAM
  !      123456789012345678901234567890123456789012345
  IAM = 'a_multiEF::get_ptr_node_scalare_field_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_node_scalar_field_multiEF => get_ptr_node_scalar_field_multiEF_iso(nb,i_nf)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

!> \brief Get pointer on edge2vertices map of an element
function get_ptr_edge2vertices_multiEF(blmnb)
  implicit none 
  !> id of the finite element in list
  integer(kind=4), intent(in) :: blmnb
  !> returned pointer on edge2vertices map
  integer(kind=4), dimension(:,:), pointer :: get_ptr_edge2vertices_multiEF
  !
  integer :: nb
  character(len=39) :: IAM
  !      1234567890123456789012345678901234567890
  IAM = 'a_multiEF::get_ptr_edge2vertices_multiEF'

  nb = get_nb_in_multiEFmodel(blmnb)

  select case (ele_is(blmnb))
  case(i_iso)
    get_ptr_edge2vertices_multiEF => get_ptr_edge2vertices_multiEF_iso(nb)
  case default
    call FATERR(IAM,'unknown model')
  end select

end function

end module a_multiEF
