!===========================================================================
!
! Copyright 2000-2022 CNRS-UM.
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

!> manage the existing avatar in the system
module modelization

  use utilities
  use overall
  use algebra

  use mdl_hdl_parameters
  use parameters
  use paranoid_checks

  use models,         only : modelz, &
                             get_model_nb, &
                             get_ppset_nb, &
                             get_external_field_nb, &
                             get_external_field_name
  use bulk_behaviour, only : get_bulk_behav_nb  , &
                             get_bulk_field_nb  , &
                             get_bulk_field_name, &
                             grav1, grav2, grav3

  ! \todo : faire des includes plutot que USE...
  !         uniquement des fonctions dans les includes
  !         et potentiellement des USE dans ces fonctions
  use RBDY2,    only : set_bulk_of_RBDY2       , &
                       comp_mass_one_body_RBDY2, &
                       comp_Fext_one_body_RBDY2
  use RBDY3,    only : set_bulk_of_RBDY3       , &
                       comp_mass_one_body_RBDY3, &
                       comp_Fext_one_body_RBDY3

  !use a_therEF, only : compute_elementary_capacity2, &
  !                     compute_elementary_conductivity2

  implicit none

  private

  !> a general bulk type
  type T_bulk

    !> type of geometric bulk element
    character(len=5) :: blmID
    !> nickname of bulk model
    character(len=5) :: model
    !> nickname of material
    character(len=5) :: behav

    !> integer corresponding to blmID
    integer(kind=4) :: i_blmID
    !> integer corresponding to model
    integer(kind=4) :: i_model
    !> integer corresponding to behav
    integer(kind=4) :: i_behav

    !> size of connectivity array
    integer(kind=4)                            :: connec_size
    !> connectivity of the element
    integer(kind=4), dimension(:), allocatable :: connectivity

    !> serial number of the property set in model
    integer(kind=4), dimension(:), allocatable :: ppsnb
    !> data storage for rigids
    !> avr_radius and gyr_radius for 2D rigids
    !> avr_radius, I1, I2, I3, volume and local frame for 3D rigids
    real(kind=8),    dimension(:), allocatable :: rigid_data

    !> number of dofs in the element
    integer(kind=4)                             :: ndofs_by_element
    !> elementary matrices
    real(kind=8), dimension(:,:,:), allocatable :: M_elem

  end type T_bulk


  !> a general modelization type
  type T_modelisation

    !> number of bulk elements in the modelization
    integer(kind=4)                         :: nb_elements
    !> list of bulks elements
    type(T_bulk), dimension(:), allocatable :: elements

  end type T_modelisation

  !> number of modelizations
  integer(kind=4)                                 :: nb_modelizations
  !> all the modelizations of the simulation
  type(T_modelisation), dimension(:), allocatable :: all_modelizations


  public set_nb_modelizations          , &
         add_modelization              , &
         add_element_to_modelization   , &
         set_node2elements             , &
         set_ppsets                    , &
         set_cc_meca_nodal_field       , &
         set_cc_ther_nodal_field       , &
         set_cc_x_nodal_field          , &
         set_cc_el_rhs_contrib         , &
         set_cc_elementary_fields      , &
         set_edof2gdof                 , &
         set_local_frame               , &
         initialize_elementary_matrices, &
         compute_masses                , &
         compute_capacities            , &
         compute_conductivities        , &
         compute_lhs                   , &
         compute_inertia_forces        , &
         compute_external_forces       , &
         compute_bulks                 , &
         compute_fields                , &
         get_elementary_fields_on_nodes, &
         get_connectivities            , &
         clean_module

  private paranoid_check_all_modelizations, &
          paranoid_check_elements

  contains

  !> \brief Allocate all_modelizations array
  !> because of init_meca/therEF calls, must be called
  !> after reading the models
  subroutine set_nb_modelizations(nb)
    use a_mecaEF, only : init_mecaEF
    use a_therEF, only : init_therEF
    implicit none
    !> [in] number of modelizations
    integer(kind=4), intent(in) :: nb
    !
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=34) :: IAM
    !      1234567890123456789012345678901234
    IAM = 'modelization::set_nb_modelizations'

    if( allocated(all_modelizations) ) then
      call faterr(IAM,'all_modelizations already allocated')
    end if

    allocate(all_modelizations(nb), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating all_modelizations'
      call faterr(IAM,cout)
    end if

    call init_mecaEF()
    call init_therEF()

    nb_modelizations = nb
  end subroutine

  !> \brief Add a modelization in all_modelizations
  !> Allocate elements
  subroutine add_modelization(id, nb_elements)
    implicit none
    !> [in] index of the new modelization
    integer(kind=4), intent(in) :: id
    !> [in] number of elements in the new modelization
    integer(kind=4), intent(in) :: nb_elements
    !
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = 'modelization::add_modelization'

    call paranoid_check_all_modelizations(id, 'modelization::add_modelization')

    all_modelizations(id)%nb_elements = nb_elements
    if( allocated(all_modelizations(id)%elements) ) then
      write(cout,'(A,1x,I0,1x,A)') 'elements of modelization', id, 'already allocated'
      call faterr(IAM,cout)
    end if

    allocate(all_modelizations(id)%elements(nb_elements), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, 'while allocating elements of modelization', id
      call faterr(IAM,cout)
    end if
  end subroutine

  !> \brief Add an element to a modelization and return the number of dofs of the element
  !> bulk_behav and models must have been read
  function add_element_to_modelization(id, i_element, i_type, c5, i4, r8)
    implicit none
    !> [return] the number of dofs of the element
    integer(kind=4) :: add_element_to_modelization
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] index of the element
    integer(kind=4), intent(in) :: i_element
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] anonymous string data of the element
    character(len=5), dimension(:), pointer :: c5
    !> [in] anonymous integer data of the element
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] anonymous real data of the element
    real(kind=8),     dimension(:), pointer :: r8
    !
    integer(kind=4)   :: i_model, connec_size, errare
    character(len=5)  :: model, behav, blmID
    character(len=80) :: cout
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'modelization::add_element_to_modelization'

    call paranoid_check_all_modelizations(id, IAM)   ! check modelization index
    call paranoid_check_elements(id, i_element, IAM) ! check element index

    call paranoid_check_i4_ptr(IAM,i4,1) ! at least one element
    call paranoid_check_c5_ptr(IAM,c5,3) ! at least blmId, model and behav

    if( allocated(all_modelizations(id)%elements(i_element)%connectivity) ) then
      write (cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'connectivity of element', i_element, &
                                               'of modelization', id, 'already allocated'
      call faterr(IAM,cout)
    end if

    connec_size = size(i4)
    all_modelizations(id)%elements(i_element)%connec_size = connec_size
    allocate(all_modelizations(id)%elements(i_element)%connectivity(connec_size), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, 'while  allocating connectivity of element', &
                                           i_element, 'of modelization', id
      call faterr(IAM,cout)
    end if

    all_modelizations(id)%elements(i_element)%connectivity(1:connec_size) = i4(1:connec_size)

    blmID = c5(1)
    model = c5(2)
    behav = c5(3)

    all_modelizations(id)%elements(i_element)%blmID = blmID
    all_modelizations(id)%elements(i_element)%model = model
    all_modelizations(id)%elements(i_element)%behav = behav 

    all_modelizations(id)%elements(i_element)%i_blmID = -1 ! default for rigids
    all_modelizations(id)%elements(i_element)%i_model = -1 ! default for rigids
    all_modelizations(id)%elements(i_element)%i_behav = get_bulk_behav_nb(behav)

    select case( i_type )
    case( p_rigid2D )

      call paranoid_check_r8_ptr(IAM,r8,2)

      ! avr_radius and gyr_radius
      call set_bulk_of_RBDY2(all_modelizations(id)%elements(i_element)%ndofs_by_element, &
                             all_modelizations(id)%elements(i_element)%rigid_data, r8)

    case( p_rigid3D )

      call paranoid_check_r8_ptr(IAM,r8,4)

      ! avr_radius, I1, I2, I3 and volume
      call set_bulk_of_RBDY3(all_modelizations(id)%elements(i_element)%ndofs_by_element, &
                             all_modelizations(id)%elements(i_element)%rigid_data, r8)

    case( p_mecaMAILx )

      call set_bulk_of_mecaFE(model, blmID, &
                              all_modelizations(id)%elements(i_element)%i_model, &
                              all_modelizations(id)%elements(i_element)%i_blmID, &
                              all_modelizations(id)%elements(i_element)%ndofs_by_element)

    case( p_therMAILx )

      call set_bulk_of_therFE(model, blmID, &
                              all_modelizations(id)%elements(i_element)%i_model, &
                              all_modelizations(id)%elements(i_element)%i_blmID, &
                              all_modelizations(id)%elements(i_element)%ndofs_by_element)

    end select

    add_element_to_modelization = all_modelizations(id)%elements(i_element)%ndofs_by_element
  end function

  !> \brief Set the array holding for each node the list of elements it belongs to
  subroutine set_node2elements(id, nb_nodes, node2elements)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] number of nodes in the modelization
    integer(kind=4), intent(in) :: nb_nodes
    !> [in,out] for each node, the list of elements it belongs to
    type(G_i_list), dimension(:), intent(inout) :: node2elements
    !
    integer(kind=4) :: i_node, i_element, nb_elements
    integer(kind=4) :: t_i, errare
    integer(kind=4), dimension(:), allocatable :: t_i_list
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'modelization::set_node2elements'

    ! paranoid check ?

    nb_elements = all_modelizations(id)%nb_elements

    ! count how many times a node appears in the elements
    allocate( t_i_list(nb_nodes) )
    t_i_list(1:nb_nodes) = 0
    do i_element = 1, nb_elements
      do i_node = 1, all_modelizations(id)%elements(i_element)%connec_size
        t_i = all_modelizations(id)%elements(i_element)%connectivity(i_node)
        t_i_list(t_i) = t_i_list(t_i) + 1
      end do
    end do

    ! for each node, an array is allocated of the size the number of times it appears
    do i_node = 1, nb_nodes
       t_i = t_i_list(i_node)

       if( associated(node2elements(i_node)%G_i) ) then
         write (cout,'(A,I0,A,1x,I0)') 'Component G_i already associated for node2elements(', &
                                       i_node,') of modelization:', id
         call faterr(IAM,cout)
       end if

       allocate(node2elements(i_node)%G_i(t_i),stat=errare)
       if( errare /= 0 ) then
         write (cout,'(A,1x,I0,1x,A,I0,A,1x,I0)') 'Error', errare, 'while allocating G_i of node2elements(', &
                                                  i_node,') for modelization:', id
         call faterr(IAM,cout)
       end if
    end do

    ! the list is needed for something else
    t_i_list(1:nb_nodes) = 0 ! to keep track of the index of node2blmty%G in which to write

    do i_element = 1, nb_elements
       do i_node = 1, all_modelizations(id)%elements(i_element)%connec_size
          ! the element index is added in node2element
          t_i = all_modelizations(id)%elements(i_element)%connectivity(i_node)
          t_i_list(t_i) = t_i_list(t_i) + 1
          node2elements(t_i)%G_i(t_i_list(t_i)) = i_element
       end do
    end do

    deallocate( t_i_list )

  end subroutine

  !> \brief Set the property set of a modelization
  subroutine set_ppsets(id, i_type, nb_nodes)
    use a_mecaEF, only : get_N_GP_mecaEF
    use a_therEF, only : get_N_GP_therEF
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4), intent(in) :: nb_nodes
    !
    integer(kind=4) :: i_node, i_element, nb_elements
    integer(kind=4) :: i_model, i_behav, t_i, errare
    integer(kind=4), dimension(:), allocatable :: t_i_list
    logical :: use_existing_ppset
    integer(kind=4) :: nb_gp, i_gp
    character(len=80) :: cout
    character(len=24) :: IAM
    !    123456789012345678901234
    IAM='modelization::set_ppsets'
  
    ! paranoid check ?

    !> \todo : know what to do with this
    use_existing_ppset = .true.

    nb_elements = all_modelizations(id)%nb_elements

    do i_element = 1, nb_elements

      i_model = all_modelizations(id)%elements(i_element)%i_model
      i_behav = all_modelizations(id)%elements(i_element)%i_behav

      select case( i_type )
      case( p_mecaMAILx )
        nb_gp = get_N_GP_mecaEF(modelz(i_model)%ID)
      case( p_therMAILx )
        nb_gp = get_N_GP_therEF(modelz(i_model)%ID)
      case default
        nb_gp = 0
      end select

      if( allocated(all_modelizations(id)%elements(i_element)%ppsnb) ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'property set of element', i_element, &
                                                'of modelization number', id, 'already allocated'
        call faterr(IAM,cout)
      end if

      allocate( all_modelizations(id)%elements(i_element)%ppsnb(nb_gp), stat=errare )
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error ', errare, ' while allocating property set of element', &
                                                      i_element, 'of modelization number', id
        call faterr(IAM,cout)
      end if

      do i_gp = 1, nb_gp
        all_modelizations(id)%elements(i_element)%ppsnb(i_gp) = get_ppset_nb(use_existing_ppset, i_model, i_behav)
      end do
    end do
  end subroutine

!! TO REMOVE
  !> \brief Set the map between a node and a nodal field index of a modelization of type X
  subroutine set_cc_x_nodal_field(id,i_type,nb_nodes,cc_nodal_field)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4), intent(in) :: nb_nodes
    !> [in,out] map of indices between nodes and nodal field
    integer(kind=4), dimension(:), intent(inout) :: cc_nodal_field
    !
    integer(kind=4)   :: i_node
    character(len=34) :: IAM
    !    1234567890123456789012345678901234
    IAM='modelization::set_cc_x_nodal_field'
  
    call paranoid_check_i4_size(IAM, cc_nodal_field, nb_nodes+1)

    select case( i_type )
    case( p_rigid2D )
      cc_nodal_field(1) = 0
      cc_nodal_field(2) = 2 + 4 ! X + frame
    case( p_rigid3D )
      cc_nodal_field(1) = 0
      cc_nodal_field(2) = 3 + 9 ! X + frame
    case( p_mecaMAILx )
      cc_nodal_field(1) = 0
      do i_node = 1, nb_nodes
        cc_nodal_field(i_node+1) = cc_nodal_field(i_node) + nbDIME ! X
      end do
    end select

  end subroutine

  !> \brief Set the map between a node and a nodal field index of a mechanical modelization
  subroutine set_cc_meca_nodal_field(id,i_type,nb_nodes,cc_nodal_field)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4), intent(in) :: nb_nodes
    !> [in,out] map of indices between nodes and nodal field
    integer(kind=4), dimension(:), intent(inout) :: cc_nodal_field
    !
    integer(kind=4)   :: i_node, i_element, i_model, i_connec, n_dof
    character(len=37) :: IAM
    !    1234567890123456789012345678901234567
    IAM='modelization::set_cc_meca_nodal_field'
  
    call paranoid_check_i4_size(IAM, cc_nodal_field, nb_nodes+1)

    cc_nodal_field(1:nb_nodes+1) = 0

    select case( i_type )
    case( p_rigid2D, p_rigid3D )

      cc_nodal_field(2) = all_modelizations(id)%elements(1)%ndofs_by_element

    case( p_mecaMAILx )

      call set_cc_nodal_field_mecaFE(id, nb_nodes, cc_nodal_field)

    end select

  end subroutine

  !> \brief Set the map between a node and a nodal field index of a thermal modelization
  subroutine set_cc_ther_nodal_field(id,i_type,nb_nodes,cc_nodal_field)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4), intent(in) :: nb_nodes
    !> [in,out] map of indices between nodes and nodal field
    integer(kind=4), dimension(:), intent(inout) :: cc_nodal_field
    !
    character(len=37) :: IAM
    !    1234567890123456789012345678901234567
    IAM='modelization::set_cc_ther_nodal_field'
  
    call paranoid_check_i4_size(IAM, cc_nodal_field, nb_nodes+1)

    cc_nodal_field(1:nb_nodes+1) = 0

    select case( i_type )
    case( p_therMAILx )

      call set_cc_nodal_field_therFE(id, nb_nodes, cc_nodal_field)

    end select

  end subroutine

  !> \brief Set the map between an element and an elementary rhs contribution index
  subroutine set_cc_el_rhs_contrib(id, cc_el_rhs_contrib)
    implicit none
    !> [in] index of the modelization
    integer(kind=4),               intent(in)    :: id
    !> [in,out] map of indices between elements and elementary rhs contributions
    integer(kind=4), dimension(:), intent(inout) :: cc_el_rhs_contrib
    !
    integer(kind=4)   :: i_element, nb_elements
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'modelization::set_cc_el_rhs_contrib'

    nb_elements = all_modelizations(id)%nb_elements

    call paranoid_check_i4_size(IAM, cc_el_rhs_contrib, nb_elements+1)

    cc_el_rhs_contrib(1:nb_elements+1) = 0

    do i_element = 1, nb_elements
      cc_el_rhs_contrib(i_element+1) = cc_el_rhs_contrib(i_element) + all_modelizations(id)%elements(i_element)%ndofs_by_element
    end do

  end subroutine

  !> \brief Set the map between the dof index of an element and the index in an assembled vector of dofs
  subroutine set_edof2gdof(id,i_element,ccdof,edof2gdof)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] index of the element of the modelization
    integer(kind=4), intent(in) :: i_element
    !> [in] map of indices between nodes and dofs
    integer(kind=4), dimension(:), intent(in) :: ccdof
    !> [in,out] map of element dof to global dofs
    integer(kind=4), dimension(:), allocatable, intent(inout) :: edof2gdof
    !
    integer(kind=4)   :: nb_dof, nb_connec, errare
    integer(kind=4)   :: i_ccdof, i_connec, i_node, i_dof
    character(len=80) :: cout
    character(len=27) :: IAM
    !    123456789012345678901234567
    IAM='modelization::set_edof2gdof'

    ! paranoid check ?

    nb_dof    = all_modelizations(id)%elements(i_element)%ndofs_by_element
    nb_connec = all_modelizations(id)%elements(i_element)%connec_size

    allocate(edof2gdof(nb_dof), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error', errare, 'while allocating edof2gdof of element', &
                                                    i_element, 'of modelization', id
      call faterr(IAM,cout)
    end if

    i_ccdof = 0
    do i_connec = 1, nb_connec
      i_node = all_modelizations(id)%elements(i_element)%connectivity(i_connec)
      ! \todo: grouille 3eme !!!
      do i_dof = 1, nb_dof / nb_connec
        i_ccdof = i_ccdof + 1
        edof2gdof(i_ccdof) = ccdof(i_node) + i_dof
      end do
    end do

    ! paranoid
    if( i_ccdof /= nb_dof ) call faterr(IAM,'get lost')

  end subroutine

  !> \brief Set the ef_names and ef_map of a modelization
  subroutine set_cc_elementary_fields(id,i_type,ef_names,ef_map,ef_size)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in,out] map between quadrature point and field index to fields
    type(T_i4_matrix),    dimension(:), intent(inout) :: ef_map
    !> [in,out] list of name of fields
    type(T_string_array), dimension(:), intent(inout) :: ef_names
    !> [out] total size of the elementary field array
    integer(kind=4), intent(out) :: ef_size
    !
    integer(kind=4)   :: nb_elements, i_element, i_model, i_behav
    integer(kind=4)   :: nb_dof, nb_ef, nb_bf, nbf, nb_gp, i_gp, i_f
    integer(kind=4)   :: total_nbf, nb_external, nb_internal, errare
    character(len=80) :: cout
    character(len=38) :: IAM
    !    12345678901234567890123456789012345678
    IAM='modelization::set_cc_elementary_fields'
  
    ef_size = 0

    select case( i_type )
    case( p_rigid2D, p_rigid3D )

      return

    case( p_mecaMAILx )

      call set_cc_elementary_fields_mecaFE(id, ef_names, ef_map, ef_size)

    case( p_therMAILx )

      call set_cc_elementary_fields_therFE(id, ef_names, ef_map, ef_size)

    end select

  end subroutine

  !> \brief Get initial local frame of a modelization
  !> \todo : pas clair... faut-il juste changer le nom de la fonction
  !>         ou carrement la virer pout que l'équivalent d'un compute mass
  !>         ou compute rigid properties fournisse/appel ce service...
  subroutine set_local_frame(id, i_type, frame)
    implicit none
    !> [in] index of the modelization
    integer(kind=4),            intent(in)  :: id
    !> [in] type of the modelization
    integer(kind=4),            intent(in)  :: i_type
    !> [out] local frame of the modelization
    real(kind=8), dimension(:), intent(out) :: frame
    !
    character(len=29) :: IAM
    !      12345678901234567890123456789
    IAM = 'modelization::set_local_frame'

    select case( i_type )
    case( p_rigid2D )
      frame(1:4) = all_modelizations(id)%elements(1)%rigid_data(3:6)
    case( p_rigid3D )
      frame(1:9) = all_modelizations(id)%elements(1)%rigid_data(6:14)
    case default
      call faterr(IAM,'not implemented yet')
    end select

  end subroutine

  !> \brief Initialize elementary matrices of a modelization
  subroutine initialize_elementary_matrices(id, i_type, nb_matrices)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4), intent(in) :: i_type
    !> [in] number of elementary matrices to store
    integer(kind=4), intent(in) :: nb_matrices
    !
    integer(kind=4)   :: i_element, n_dof, errare
    character(len=80) :: cout
    character(len=44) :: IAM
    !      12345678901234567890123456789012345678901234
    IAM = 'modelization::initialize_elementary_matrices'

    call paranoid_check_all_modelizations(id, IAM)

    ! for each element create all elementary matrices needed
    do i_element = 1, all_modelizations(id)%nb_elements

      if( allocated(all_modelizations(id)%elements(i_element)%M_elem) ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'M_elem of element', i_element, &
                                                'of modelization number', id, 'already allocated'
        call faterr(IAM,cout)
      end if

      n_dof = all_modelizations(id)%elements(i_element)%ndofs_by_element

      select case( i_type )
      case( p_rigid2D, p_rigid3D )
        allocate(all_modelizations(id)%elements(i_element)%M_elem(n_dof,1,nb_matrices), stat=errare)
      case( p_mecaMAILx, p_therMAILx )
        allocate(all_modelizations(id)%elements(i_element)%M_elem(n_dof,n_dof,nb_matrices), stat=errare)
        all_modelizations(id)%elements(i_element)%M_elem(1:n_dof,1:n_dof,1:nb_matrices) = 0.d0
      case default
        write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), &
                                            'for modelization number:', id
        call faterr(IAM,cout)
      end select

      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error ', errare, ' while allocating M_elem of element', &
                                                      i_element, 'of modelization number', id
        call faterr(IAM,cout)
      end if
 
    end do

  end subroutine

  !> \brief Computation of elementary mass matrices
  subroutine compute_masses(id, i_type, i_mat, nodes)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),              intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4),              intent(in) :: i_type
    !> [in] index of mass matrix in M_elem
    integer(kind=4),              intent(in) :: i_mat
    !> [in] the position of the nodes
    real(kind=8), dimension(:,:), intent(in) :: nodes
    !
    integer(kind=4)   :: n_dofs
    character(len=80) :: cout
    character(len=28) :: IAM
    !      1234567890123456789012345678
    IAM = 'modelization::compute_masses'

    select case( i_type )
    case( p_rigid2D )
      n_dofs = all_modelizations(id)%elements(1)%ndofs_by_element
      call comp_mass_one_body_RBDY2(all_modelizations(id)%elements(1)%M_elem(1:n_dofs,1,i_mat), &
                                    all_modelizations(id)%elements(1)%i_behav                 , &
                                    all_modelizations(id)%elements(1)%rigid_data )
    case( p_rigid3D )
      n_dofs = all_modelizations(id)%elements(1)%ndofs_by_element
      call comp_mass_one_body_RBDY3(all_modelizations(id)%elements(1)%M_elem(1:n_dofs,1,i_mat), &
                                    all_modelizations(id)%elements(1)%i_behav                 , &
                                    all_modelizations(id)%elements(1)%rigid_data )
    case( p_mecaMAILx )

      call comp_mass_meca_FE(id, i_mat, nodes)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), &
                                          'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Computation of elementary capacity matrices
  subroutine compute_capacities(id, i_type, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),                      intent(in)  :: id
    !> [in] type of the modelization
    integer(kind=4),                      intent(in)  :: i_type
    !> [in] index of conductivity matrix in M_elem
    integer(kind=4),                      intent(in)  :: i_mat
    !> [in] time differential
    real(kind=8),                         intent(in)  :: dt
    !> [in] the position of the nodes
    real(kind=8),         dimension(:,:), intent(in)  :: nodes
    !> [in] temperature increment on the nodes
    real(kind=8),         dimension(:)  , intent(in)  :: temp
    !> [in] map to acces temp
    integer(kind=4),      dimension(:)  , intent(in)  :: temp_map
    !> [in] fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(in)  :: fields
    !> [in] map to acces quadrature point fields
    type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
    !> [in] map to get quadrature point fields' name
    type(T_string_array), dimension(:)  , intent(in)  :: ef_names
    !> [out] vector in which to store internal fluxes
    real(kind=8),         dimension(:)  , intent(out) :: fint
    !> [in] map to acces internal fluxes vector by element
    integer(kind=4),      dimension(:)  , intent(in)  :: fint_map
    !
    integer(kind=4)   :: i_element, elem_dofs
    character(len=80) :: cout
    character(len=32) :: IAM
    !      12345678901234567890123456789012
    IAM = 'modelization::compute_capacities'

    select case( i_type )
    case( p_therMAILx )

      call comp_capacity_ther_FE(id, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), &
                                          'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Computation of elementary conductivity matrices
  subroutine compute_conductivities(id, i_type, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),                      intent(in)  :: id
    !> [in] type of the modelization
    integer(kind=4),                      intent(in)  :: i_type
    !> [in] index of conductivity matrix in M_elem
    integer(kind=4),                      intent(in)  :: i_mat
    !> [in] time differential
    real(kind=8),                         intent(in)  :: dt
    !> [in] the position of the nodes
    real(kind=8),         dimension(:,:), intent(in)  :: nodes
    !> [in] temperature of the nodes
    real(kind=8),         dimension(:)  , intent(in)  :: temp
    !> [in] map to acces temp
    integer(kind=4),      dimension(:)  , intent(in)  :: temp_map
    !> [in] fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(in)  :: fields
    !> [in] map to acces quadrature point fields
    type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
    !> [in] map to get quadrature point fields' name
    type(T_string_array), dimension(:)  , intent(in)  :: ef_names
    !> [out] vector in which to store internal fluxes
    real(kind=8),         dimension(:)  , intent(out) :: fint
    !> [in] map to acces internal fluxes vector by element
    integer(kind=4),      dimension(:)  , intent(in)  :: fint_map
    !
    integer(kind=4)   :: i_element, elem_dofs
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'modelization::compute_conductivities'

    select case( i_type )
    case( p_therMAILx )

      call comp_conductivity_ther_FE(id, i_mat, dt, nodes, temp, temp_map, fields, ef_map, ef_names, fint, fint_map)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), &
                                          'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Combine elementary matrices to build an elementary left hand side
  subroutine compute_lhs(id, i_type, i_element, coeffs, matrix)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),              intent(in)    :: id
    !> [in] type of the modelization
    integer(kind=4),              intent(in)    :: i_type
    !> [in] index of the element
    integer(kind=4),              intent(in)    :: i_element
    !> [in] coefficients to use to compute elementary_lhs
    real(kind=8), dimension(:),   intent(in)    :: coeffs
    !> [in,out] Left hand side matrix
    real(kind=8), dimension(:,:), intent(inout) :: matrix
    !
    integer(kind=4)   :: i_mat, elem_dofs, i_mat_dim
    character(len=80) :: cout
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'modelization::compute_lhs'

    elem_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element

    ! \todo : tout pourri, on fait le select/case pour chaque element de chaque corps !!!
    select case( i_type )
    case( p_rigid2D, p_rigid3D )
      i_mat_dim = 1
    case( p_mecaMAILx, p_therMAILx )
      i_mat_dim = elem_dofs
    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), 'for modelization number:', id
      call faterr(IAM,cout)
    end select

    matrix(1:elem_dofs,1:i_mat_dim) = 0.d0
    do i_mat = 1, size(coeffs)
      
      matrix(1:elem_dofs,1:i_mat_dim) = matrix(1:elem_dofs,1:i_mat_dim) + &
                                        all_modelizations(id)%elements(i_element)%M_elem(1:elem_dofs,1:i_mat_dim,i_mat) &
                                        * coeffs(i_mat)

    end do

  end subroutine

  !> \brief Compute inertia forces due to an elementary matrice to an elementary vector
  subroutine compute_inertia_forces(id, i_type, i_element, i_mat, rhs, dofs)
    implicit none
    !> [in] id of the modelizatin
    integer(kind=4),            intent(in)    :: id
    !> [in] type of the modelization
    integer(kind=4),            intent(in)    :: i_type
    !> [in] index of the element on which compute inertia forces
    integer(kind=4),            intent(in)    :: i_element
    !> [in] index of the elementary matrice to use for the computation
    integer(kind=4),            intent(in)    :: i_mat
    !> [in,out] vector in which to add inertia forces contribution
    real(kind=8), dimension(:), intent(inout) :: rhs
    !> [in] vector to use to compute inertia forces
    real(kind=8), dimension(:), intent(inout) :: dofs
    !
    integer(kind=4)   :: i_dofs
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'modelization::compute_inertia_forces'

    i_dofs = all_modelizations(id)%elements(i_element)%ndofs_by_element

    call paranoid_check_r8_size(IAM, rhs, i_dofs) 
    call paranoid_check_r8_size(IAM, dofs, i_dofs)

    ! \todo : tout pourri, on fait le select/case pour chaque element de chaque corps !!!
    select case( i_type )
    case( p_rigid2D, p_rigid3D )
      ! rm : on a le cas rigide ici, mais cette fonction n'est utilisée que dans les integrateurs déformables
      !      de plus ça fait double emploi avec compute_fext.
      rhs(1:i_dofs) = all_modelizations(id)%elements(i_element)%M_elem(1:i_dofs,1,i_mat) * dofs(1:i_dofs)
    case( p_mecaMAILx, p_therMAILx )
      rhs(1:i_dofs) = matmul( all_modelizations(id)%elements(i_element)%M_elem(1:i_dofs,1:i_dofs,i_mat), dofs(1:i_dofs) )
    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), 'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Compute elementary external forces
  subroutine compute_external_forces(id, i_type, i_mat, fext, fext_map)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),               intent(in)  :: id
    !> [in] type of the modelization
    integer(kind=4),               intent(in)  :: i_type
    !> [in] index of the elementary matrice to use for the computation
    integer(kind=4),               intent(in)  :: i_mat
    !> [out] vector in which to store external forces
    real(kind=8),    dimension(:), intent(out) :: fext
    !> [in] map to access external forces vector
    integer(kind=4), dimension(:), intent(in)  :: fext_map
    !
    real(kind=8), dimension(nbDIME) :: gravy_vect
    real(kind=8), dimension(:), allocatable :: tmp
    integer(kind=4)   :: i_element, i_dof, i1, i2
    integer(kind=4)   :: nb_elements 
    character(len=80) :: cout
    character(len=37) :: IAM
    !      1234567890123456789012345678901234567
    IAM = 'modelization::compute_external_forces'

    call paranoid_check_all_modelizations(id, IAM)

    nb_elements = all_modelizations(id)%nb_elements

    i1 = fext_map(1) + 1
    i2 = fext_map(nb_elements+1)

    fext(i1:i2) = 0.d0

    select case( i_type )
    case( p_rigid2D )

      do i_element = 1, nb_elements

        i_dof = all_modelizations(id)%elements(i_element)%ndofs_by_element
        i1 = fext_map(i_element) + 1
        i2 = fext_map(i_element+1)

        call comp_Fext_one_body_RBDY2(all_modelizations(id)%elements(i_element)%M_elem(1:i_dof,1,i_mat), fext(i1:i2))
      end do

    case( p_rigid3D )

      do i_element = 1, nb_elements

        i_dof = all_modelizations(id)%elements(i_element)%ndofs_by_element
        i1 = fext_map(i_element) + 1
        i2 = fext_map(i_element+1)

        call comp_Fext_one_body_RBDY3(all_modelizations(id)%elements(i_element)%M_elem(1:i_dof,1,i_mat), fext(i1:i2))
      end do

    case( p_mecaMAILx )

       call comp_Fext_mecaFE(id, i_mat, fext, fext_map)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), 'for modelization number:', id
      call faterr(IAM,cout)
    end select

    !print *,'------------------------------'
    !print *,'id : ', id
    !print *,'fe : ', fext(fext_map(1)+1:fext_map(nb_elements+1))
    !print *,'------------------------------'

  end subroutine

  !> \brief Compute elementary stiffness and internal forces
  subroutine compute_bulks(id, i_type, i_mat, dt, coor, disp, disp_map, old_fields, new_fields, ef_map, ef_names, fint, fint_map)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),                      intent(in)    :: id
    !> [in] type of the modelization
    integer(kind=4),                      intent(in)    :: i_type
    !> [in] index of the elementary matrice to store stiffness 
    integer(kind=4),                      intent(in)    :: i_mat
    !> [in] time differential
    real(kind=8),                         intent(in)    :: dt
    !> [in] reference coordinates of the nodes
    real(kind=8),         dimension(:,:), intent(in)    :: coor
    !> [in] displacement of the nodes
    real(kind=8),         dimension(:)  , intent(in)    :: disp
    !> [in] map to acces disp
    integer(kind=4),      dimension(:)  , intent(in)    :: disp_map
    !> [in] fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(inout) :: old_fields
    !> [in] fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(inout) :: new_fields
    !> [in] map to acces quadrature point fields
    type(T_i4_matrix),    dimension(:)  , intent(in)    :: ef_map
    !> [in] map to get quadrature point fields' name
    type(T_string_array), dimension(:)  , intent(in)    :: ef_names
    !> [out] vector in which to store internal forces
    real(kind=8),         dimension(:)  , intent(out)   :: fint
    !> [in] map to acces internal forces vector by element
    integer(kind=4),      dimension(:)  , intent(in)    :: fint_map
    !
    real(kind=8), dimension(:,:), allocatable :: X, K
    real(kind=8), dimension(:)  , allocatable :: U
    integer(kind=4)   :: i, i_element, i_node, idof, elem_dofs, nb_nodes, i1, i2
    character(len=80) :: cout
    character(len=27) :: IAM
    !      123456789012345678901234567
    IAM = 'modelization::compute_bulks'

    ! do we really need this selection ?
    select case( i_type )

    case( p_rigid2D, p_rigid3D )

      i1 = fint_map(i_element) + 1
      i2 = fint_map(i_element+1)

      fint(i1:i2) = 0.d0

    case( p_mecaMAILx )

      call comp_bulks_mecaFE(id, i_mat, dt, coor, disp, disp_map, old_fields, ef_map, ef_names, fint, fint_map)

    case( p_therMAILx )

      call comp_bulks_therFE(id, dt, coor, old_fields, new_fields, ef_map, ef_names, fint, fint_map)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), 'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Compute elementary fields
  subroutine compute_fields(id, i_type, dt, coor, disp, disp_map, fields_old, fields_new, ef_map, ef_names)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),                      intent(in)  :: id
    !> [in] type of the modelization
    integer(kind=4),                      intent(in)  :: i_type
    !> [in] time differential
    real(kind=8),                         intent(in)  :: dt
    !> [in] reference coordinates of the nodes
    real(kind=8),         dimension(:,:), intent(in)  :: coor
    !> [in] displacement of the nodes
    real(kind=8),         dimension(:)  , intent(in)  :: disp
    !> [in] map to acces disp
    integer(kind=4),      dimension(:)  , intent(in)  :: disp_map
    !> [in] old fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(in)  :: fields_old
    !> [in] new fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(out) :: fields_new
    !> [in] map to acces quadrature point fields
    type(T_i4_matrix),    dimension(:)  , intent(in)  :: ef_map
    !> [in] map to get quadrature point fields' name
    type(T_string_array), dimension(:)  , intent(in)  :: ef_names
    !
    real(kind=8), dimension(:,:), allocatable :: X, K
    real(kind=8), dimension(:)  , allocatable :: U
    integer(kind=4)   :: i, i_element, i_node, idof, elem_dofs, nb_nodes
    character(len=80) :: cout
    character(len=28) :: IAM
    !      1234567890123456789012345678
    IAM = 'modelization::compute_fields'

    ! do we really need this selection ?
    select case( i_type )

    case( p_mecaMAILx )

      call comp_fields_mecaFE(id, dt, coor, disp, disp_map, fields_old, fields_new, ef_map, ef_names)

    case default
      write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', get_model_type_from_id(i_type), 'for modelization number:', id
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Get interpolation of gauss points fields on nodes
  subroutine get_elementary_fields_on_nodes(id, i_type, nb_nodes, ef_name, fields, ef_map, ef_names, node2elements, ef_out)
    implicit none
    !> [in] id of the modelization
    integer(kind=4),                      intent(in) :: id
    !> [in] type of the modelization
    integer(kind=4),                      intent(in) :: i_type
    !> [in] number of nodes in the modelization
    integer(kind=4),                      intent(in) :: nb_nodes
    !> [in] name of the elementary field to get
    character(len=30),                    intent(in) :: ef_name
    !> [in] fields values at quadrature points
    real(kind=8),         dimension(:)  , intent(in) :: fields
    !> [in] map to acces quadrature point fields
    type(T_i4_matrix),    dimension(:)  , intent(in) :: ef_map
    !> [in] map to get quadrature point fields' name
    type(T_string_array), dimension(:)  , intent(in) :: ef_names
    !> [in] the list of elements a node belongs to
    type(G_i_list),       dimension(:)  , intent(in) :: node2elements
    !> [in,out] fields values interpolated on nodes
    real(kind=8),         dimension(:,:), pointer    :: ef_out
    !
    logical :: name_found
    integer(kind=4) :: i, i_element, i_ef, i_node, i_connec, field_size, connec_size
    real(kind=8), dimension(:,:), allocatable :: tmp

    select case(i_type)
    case (p_mecaMAILx)
      call get_elementary_fields_on_nodes_mecaFE(id, nb_nodes, ef_name, fields, ef_map, ef_names, node2elements, ef_out)
    end select
  end subroutine

  !> \brief Get the list of connectivities of a modelization
  !> Memory allocation is done within the function, thus use:
  !> connec => get_connectivities
  function get_connectivities(id)
    implicit none
    !> [in] id of the modelization
    integer(kind=4), intent(in)            :: id
    !> [return] list of connectivities
    integer(kind=4), dimension(:), pointer :: get_connectivities
    !
    integer(kind=4) :: i, i_elem, nb_elements, c_size

    get_connectivities => null()

    nb_elements = all_modelizations(id)%nb_elements
    if( nb_elements < 1 ) return

    ! sizing :
    c_size = 1
    do i_elem = 1, nb_elements
      c_size = c_size + 1 + all_modelizations(id)%elements(i_elem)%connec_size
    end do

    ! allocating :
    allocate(get_connectivities(c_size) )

    ! filling :
    get_connectivities(1) = nb_elements
    i = 2
    do i_elem = 1, nb_elements
      c_size = all_modelizations(id)%elements(i_elem)%connec_size
      get_connectivities(i) = c_size
      get_connectivities(i+1:i+c_size) = all_modelizations(id)%elements(i_elem)%connectivity(1:c_size)
      i = i + c_size + 1
    end do 

  end function

  !> \brief Check that desired id exists in all_modelizations array
  subroutine paranoid_check_all_modelizations(id, where)
    implicit none
    !> [in] id of the desired modelization
    integer(kind=4),  intent(in) :: id
    !> [in] name of the function calling the check
    character(len=*), intent(in) :: where
    !
    character(len=80) :: cout

    if( .not. allocated(all_modelizations) ) then
      call faterr(where,'all_modelizations is not allocated')
    end if

    if( id < 1 .or. id > size(all_modelizations) ) then
      write (cout,'(A,I0,1x,A,1x,I0)') 'index of modelization out of range: id=', id, &
                                       'should be between 1 and', size(all_modelizations)
      call faterr(where,cout)
    end if
  end subroutine

  !> \brief Check that desired id exists in elements array of an existing modelization
  subroutine paranoid_check_elements(id, i_element, where)
    implicit none
    !> [in] id of the desired modelization
    integer(kind=4),  intent(in) :: id
    !> [in] id of the desired element
    integer(kind=4),  intent(in) :: i_element
    !> [in] name of the function calling the check
    character(len=*), intent(in) :: where
    !
    character(len=130) :: cout

    if( .not. allocated(all_modelizations(id)%elements) ) then
      write (cout,'(A,1x,I0,1x,A)') 'elements array of modelization number:', id, 'not allocated'
      call faterr(where,cout)
    end if

    if( i_element < 1 .or. i_element > size(all_modelizations(id)%elements) ) then
      write (cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'index of element of modelization number', id, &
                                                     'out of range: i_element=', id, &
                                                     'should be between 1 and', size(all_modelizations(id)%elements)
      call faterr(where,cout)
    end if
  end subroutine

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none
    integer(kind=4) :: i, j, k

    nb_modelizations = 0

    if( allocated(all_modelizations) ) then
      do i = 1, size(all_modelizations)
        if( allocated(all_modelizations(i)%elements) ) then
          do j = 1, size(all_modelizations(i)%elements)
            if( allocated(all_modelizations(i)%elements(j)%connectivity) ) deallocate(all_modelizations(i)%elements(j)%connectivity)
            if( allocated(all_modelizations(i)%elements(j)%ppsnb)        ) deallocate(all_modelizations(i)%elements(j)%ppsnb)
            if( allocated(all_modelizations(i)%elements(j)%rigid_data)   ) deallocate(all_modelizations(i)%elements(j)%rigid_data)
            if( allocated(all_modelizations(i)%elements(j)%M_elem)       ) deallocate(all_modelizations(i)%elements(j)%M_elem)
          end do
          deallocate(all_modelizations(i)%elements)
        end if
      end do

      deallocate(all_modelizations)
    end if

  end subroutine

  include 'inc_model_mecaFE.f90'
  include 'inc_model_therFE.f90'

end module modelization
