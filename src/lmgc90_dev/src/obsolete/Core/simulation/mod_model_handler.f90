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
!> module managing all mechanical and thermal model

module model_handler

  use paranoid_checks

  use parameters
  use mdl_hdl_parameters
  use integrator_parameters

  use utilities
  use a_DOF
  use algebra
  use a_matrix

  use integrator, only: T_integrator
  use integrator_list, integrator_clean_module => clean_module

  use modelization, only : set_nb_modelizations          , &
                           add_modelization              , &
                           add_element_to_modelization   , &
                           set_node2elements             , &
                           set_ppsets                    , &
                           set_cc_el_rhs_contrib         , &
                           set_cc_elementary_fields      , &
                           set_edof2gdof                 , &
                           set_local_frame               , &
                           initialize_elementary_matrices, &
                           compute_masses                , &
                           compute_capacities            , &
                           compute_conductivities        , &
                           compute_external_forces       , &
                           compute_bulks                 , &
                           compute_fields                , &
                           compute_inertia_forces        , &
                           compute_lhs                   , &
                           get_elementary_fields_on_nodes, &
                           modelization_clean_module       => clean_module , &
                           modelization_get_connectivities => get_connectivities

  use state, state_initialize => initialize, &
             state_set_node   => set_node  , &
             state_update     => update

  use soe, only : soe_initialize => initialize, &
                  initialize_soe              , &
                  change_system_shape         , &
                  solve                       , &
                  set_sol_value               , &
                  set_elementary_lhs          , &
                  set_system_drvdofs          , &
                  erase_system_drvdofs        , &
                  reset_rhs                   , &
                  assemble_rhs                , &
                  set_system_rhs              , &
                  apply_driven_dof_rhs        , &
                  set_system_drvvalues        , &
                  add_sol_contributions       , &
                  add_rhs_contributions

  implicit none

  private

  !> An element type
  !> \todo : mettre ça dans soe
  type T_element
    !> number of dofs of the element
    integer(kind=4) :: idof
    !> elementary KT matrice
    real(kind=8)   , dimension(:,:), allocatable :: elementary_lhs
    !> elementary right hand side without reaction
    real(kind=8)   , dimension(:)  , allocatable :: elementary_rhs
    !> map from elementary dof to global dof
    integer(kind=4), dimension(:), allocatable :: edof2gdof
  end type T_element


  !> A type to handle a modelization with respects to its state
  type T_model_handle
    !> type of the model (see 'parameters')
    integer(kind=4) :: type
    !> id of the modelization associated to the model
    integer(kind=4) :: id_modelization
    !
    !> state of the model
    type(T_State), pointer :: state
    !
    !> integrator to use with this modelization
    type(T_integrator), pointer :: integrator
    !
    !> elementary matrix and right hand side of the model
    type(T_element),    dimension(:), allocatable :: elements
    !> primal driven dofs
    type(T_driven_dof), dimension(:), allocatable :: prim_drvdofs
    !> dual driven dofs
    type(T_driven_dof), dimension(:), allocatable :: dual_drvdofs
  end type T_model_handle

  !> number of model_handles
  integer(kind=4) :: nb_model_handles = 0
  !> all model_handles
  type(T_model_handle), dimension(:), allocatable :: all_model_handles


  public set_nb_models                    , &
         get_nb_models                    , &
         add_model                        , &
         add_node_to_model                , &
         add_bulk_to_model                , &
         add_primal_drvdof_to_model       , &
         add_dual_drvdof_to_model         , &
         set_init_val_to_model            , &
         initialize                       , &
         change_system_storage            , &
         compute_elementary_masses        , &
         compute_elementary_capacities    , &
         compute_elementary_conductivities, &
         compute_elementary_bulks         , &
         compute_elementary_fext          , &
         compute_elementary_lhs           , &
         compute_elementary_rhs           , &
         get_nb_dofs                      , &
         get_storage                      , &
         get_if_renum                     , &
         get_type                         , &
         get_nb_nodes                     , &
         assemble_elementary_lhs_4all     , &
         assemble_elementary_rhs_4all     , &
         compute_free_state               , &
         compute_dofs                     , &
         compute_residue_norm             , &
         ! set_nb_rhs, &
         ! set_nb_lhs
         update                           , &
         get_coor                         , &
         get_detection_configuration      , &
         get_nb_nodal_field               , &
         get_nodal_field                  , &
         get_nodal_field_ptr              , &
         set_nodal_field                  , &
         get_nb_elementary_rhs_contrib    , &
         get_rhs_contribution             , &
         get_elementary_field             , &
         get_connectivities               , &
         clean_module
 

  private paranoid_check

  contains

  !> \brief Size array holding the modelization
  subroutine set_nb_models(nb_models)
    use overall, only: add_nb_ENTITY
    implicit none
    !> [in] number of modelization
    integer(kind=4), intent(in) :: nb_models
    !
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=28) :: IAM
    !      1234567890123456789012345678
    IAM = 'model_handler::set_nb_models'

    nb_model_handles = nb_models

    ! rm: for SDL contact solver (something in detect_and_compute_contact present too)
    !     maybe something different could be made ? Or everything done in interaction_handler module
    call add_nb_ENTITY(nb_models)
    call init_EntityList()

    if( nb_models == 0 ) then
      write(cout,'(A,1x,A)') IAM, 'No model handle...'
      call logmes(cout)
      return
    end if

    if( allocated(all_model_handles) ) then
      write(cout,'(A)') 'all_model_handles already allocated'
      call faterr(IAM,cout)
    end if

    allocate(all_model_handles(nb_models), stat=errare)
    if( errare /= 0 ) then
      write(cout,'(A,1x,I0,1x,A)') 'Error', errare, 'while allocating all_model_handles'
      call faterr(IAM,cout)
    end if

    call set_nb_modelizations(nb_models)

  end subroutine

  !> \brief Get the number of models
  function get_nb_models()
    implicit none
    !> [return] the number of models in all_model_handles array
    integer(kind=4) :: get_nb_models

    get_nb_models = nb_model_handles
  end function

  !> \brief Add a modelization
  subroutine add_model(id, modelType, type, nb_nodes, nb_bulks, nb_prim, nb_dual)
    implicit none
    !> [in] index of the modelization
    integer(kind=4),  intent(in) :: id
    !> [in] type of the model : MECAx or THERx
    character(len=5), intent(in) :: modelType
    !> [in] type of the modelization : RBDY2, RBDY3 or MAILx
    character(len=5), intent(in) :: type
    !> [in] number of nodes of the modelization
    integer(kind=4),  intent(in) :: nb_nodes
    !> [in] number of elements of the modelization
    integer(kind=4),  intent(in) :: nb_bulks
    !> [in] number of primal driven dof in the modelization
    integer(kind=4),  intent(in) :: nb_prim
    !> [in] number of dual driven dof in the modelization
    integer(kind=4),  intent(in) :: nb_dual
    !
    integer(kind=4)   :: errare
    character(len=80) :: cout
    character(len=24) :: IAM
    !      123456789012345678901234
    IAM = 'model_handler::add_model'

    call paranoid_check(id, IAM)

    all_model_handles(id)%id_modelization = id

    all_model_handles(id)%type = get_model_id_from_type(type)

    if( all_model_handles(id)%type == p_MAILx ) then
      select case(modelType)
      case( 'MECAx' )
        all_model_handles(id)%type = p_mecaMAILx
      case( 'THERx' ) 
        all_model_handles(id)%type = p_therMAILx
      case default
        write (cout,'(A,1x,A,1x,A,1x,I0)') 'Unknown modelType:', modelType, 'for model_handle number', id
        call faterr(IAM,cout)
      end select
    end if
 
    ! allocating elements
    if( allocated(all_model_handles(id)%elements) ) then
      write (cout,'(A,1x,I0,1x,A)') 'elements field of model_handle', id, 'already allocated'
      call faterr(IAM,cout)
    end if
    allocate(all_model_handles(id)%elements(nb_bulks), stat=errare)
    if( errare /= 0 ) then
      write (cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                          'while allocating elements field of model_handle', id
      call faterr(IAM,cout)
    end if

    ! adding corresponding modelization
    call add_modelization(id, nb_bulks)

    ! creatint corresponding state
    all_model_handles(id)%state => get_new_state(id, nb_nodes, nb_bulks)

    ! allocating driven dofs
    if( allocated(all_model_handles(id)%prim_drvdofs) ) then
      write (cout,'(A,1x,I0,1x,A)') 'prim_drvdofs  field of model_handle', id, 'already allocated'
      call faterr(IAM,cout)
    end if
    if( nb_prim > 0 ) then
      allocate(all_model_handles(id)%prim_drvdofs(nb_prim), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                            'while allocating prim_drvdofs field of model_handle', id
        call faterr(IAM,cout)
      end if
    end if
    if( allocated(all_model_handles(id)%dual_drvdofs) ) then
      write (cout,'(A,1x,I0,1x,A)') 'dual_drvdofs  field of model_handle', id, 'already allocated'
      call faterr(IAM,cout)
    end if
    if( nb_dual > 0 ) then
      allocate(all_model_handles(id)%dual_drvdofs(nb_dual), stat=errare)
      if( errare /= 0 ) then
        write (cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                            'while allocating dual_drvdofs field of model_handle', id
        call faterr(IAM,cout)
      end if
    end if

  end subroutine

  !> \brief Add a node to the state of a model_handle
  subroutine add_node_to_model(i_model, i_node, coor)
    implicit none
    !> [in] index of the modelHandle
    integer(kind=4), intent(in) :: i_model
    !> [in] index of the node
    integer(kind=4), intent(in) :: i_node
    !> [in] coordinates of the node
    real(kind=8), dimension(:), intent(in) :: coor

    call paranoid_check(i_model, 'model_handler::add_node_to_model')

    call state_set_node(all_model_handles(i_model)%state, i_node, coor)

  end subroutine

  !> \brief Add an element to a model_handle
  subroutine add_bulk_to_model(i_model, i_bulk, c5, i4, r8)
    implicit none
    !> [in] index of the modelization
    integer(kind=4), intent(in) :: i_model
    !> [in] index of the bulk of the modelization
    integer(kind=4), intent(in) :: i_bulk
    !> [in] anonymous strings data of the element
    character(len=5), dimension(:), pointer :: c5
    !> [in] anonymous integer data of the element
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] anonymous real data of the element
    real(kind=8),     dimension(:), pointer :: r8
    !
    integer(kind=4) :: idof
    
    call paranoid_check(i_model, 'model_handler::add_bulk_to_model')

    idof = add_element_to_modelization(i_model, i_bulk, all_model_handles(i_model)%type, c5, i4, r8)
    all_model_handles(i_model)%elements(i_bulk)%idof = idof

  end subroutine

  !> \brief Add a primal driven dof to a model_handle
  subroutine add_primal_drvdof_to_model(i_model, i_drvdof, c5, i4, r8, cx)
    implicit none
    !> [in] index of the model_handle
    integer(kind=4), intent(in) :: i_model
    !> [in] index of the driven dof of the model_handle
    integer(kind=4), intent(in) :: i_drvdof
    !> [in] anonymous character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] anonymous integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] anonymous real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] anonymous strings data of the driven dof
    character(len=128), dimension(:), pointer :: cx
    !
    integer(kind=4)   :: idof
    character(len=80) :: cout
    character(len=41) :: IAM
    !      12345678901234567890123456789012345678901
    IAM = 'model_handler::add_primal_drvdof_to_model'
    
    call paranoid_check(i_model,IAM)

    ! this is a paranoid check too
    if( i_drvdof > size(all_model_handles(i_model)%prim_drvdofs) .or. i_drvdof < 1 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0)') 'index out of range, is', i_drvdof, &
                                         'shoudl be between 1 and', size(all_model_handles(i_model)%prim_drvdofs)
      call faterr(IAM, cout) 
    end if

    call set_driven_dof(all_model_handles(i_model)%prim_drvdofs(i_drvdof), c5(1), i4, r8, cx)

  end subroutine

  !> \brief Add a dual driven dof to a model_handle
  subroutine add_dual_drvdof_to_model(i_model, i_drvdof, c5, i4, r8, cx)
    implicit none
    !> [in] index of the model_handle
    integer(kind=4), intent(in) :: i_model
    !> [in] index of the driven dof of the model_handle
    integer(kind=4), intent(in) :: i_drvdof
    !> [in] anonymous character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] anonymous integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] anonymous real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] anonymous strings data of the driven dof
    character(len=128), dimension(:), pointer :: cx
    !
    integer(kind=4)   :: idof
    character(len=80) :: cout
    character(len=39) :: IAM
    !      123456789012345678901234567890123456789
    IAM = 'model_handler::add_dual_drvdof_to_model'
    
    call paranoid_check(i_model,IAM)

    ! this is a paranoid check too
    if( i_drvdof > size(all_model_handles(i_model)%dual_drvdofs) .or. i_drvdof < 1 ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0)') 'index out of range, is', i_drvdof, &
                                         'shoudl be between 1 and', size(all_model_handles(i_model)%dual_drvdofs)
      call faterr(IAM, cout) 
    end if

    call set_driven_dof(all_model_handles(i_model)%dual_drvdofs(i_drvdof), c5(1), i4, r8, cx)

  end subroutine

  !> \brief Set dofs initial values to a model_handle
  !> This function cannot be called if initialize function has not been called yet
  subroutine set_init_val_to_model(i_model, i_initval, i4, r8)
    implicit none
    !> [in] index of the model_handle
    integer(kind=4), intent(in) :: i_model
    !> [in] index of the initial values of the model_handle
    integer(kind=4), intent(in) :: i_initval
    !> [in] anonymous integer data of the initial dof
    integer(kind=4), dimension(:), pointer :: i4
    !> [in] anonymous real data of the initial dof
    real(kind=8),    dimension(:), pointer :: r8
    !
    integer(kind=4)   :: i, j, i_nf, i_no, i_td
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'model_handler::set_init_val_to_model'
    
    call paranoid_check(i_model,IAM)

    call paranoid_check_i4_ptr(IAM, i4, 1)
    call paranoid_check_r8_ptr(IAM, r8)

    i_nf  = all_model_handles(i_model)%integrator%i_solve_nodal_field
    i_no  = i4(1)

    do i_td = 1, all_model_handles(i_model)%integrator%nf_depths(i_nf)
      call get_node_nodal_field_indices_boundary(i,j, all_model_handles(i_model)%state, i_nf, i_no, i_td)
      all_model_handles(i_model)%state%nodal_fields(i:j) = r8(1:size(r8))
    end do

  end subroutine

  !> \brief For each model, allocate and fill mapping arrays ccdof, ccgauss, etc
  !> This assumes integrator are initialized, and their parameters set
  subroutine initialize()
    implicit none
    type(T_integrator), pointer :: integr
    integer(kind=4)   :: nb_nodes, nb_elements, i_mdl_hdl, i_element, errare
    integer(kind=4)   :: i, idof, nb_matrices, ef_size, nb_dofs_for_soe
    integer(kind=4)   :: j, idof2, i_node, i_nf, i_no, i_td, i_storage
    logical           :: with_renum
    integer(kind=4), dimension(:), pointer   :: connectivities
    character(len=80) :: cout
    character(len=25) :: IAM
    !      1234567890123456789012345
    IAM = 'model_handler::initialize'

    connectivities => null()

    call paranoid_check(nb_model_handles,IAM)

    call initialize_integrators()

    do i_mdl_hdl = 1, nb_model_handles

      ! choose the right integrator
      integr => null()
      select case(all_model_handles(i_mdl_hdl)%type)
      case( p_rigid2D, p_rigid3D )

        integr => get_integrator(p_meca_rigid)
        all_model_handles(i_mdl_hdl)%integrator => integr

      case( p_mecaMAILx )

        integr => get_integrator(p_meca_defor)
        all_model_handles(i_mdl_hdl)%integrator => integr

      case( p_therMAILx )

        integr => get_integrator(p_ther_defor)
        all_model_handles(i_mdl_hdl)%integrator => integr

      case default
        write(cout,'(A,1x,A,1x,A,1x,I0)') 'Unknown modelization type', &
                                          get_model_type_from_id(all_model_handles(i_mdl_hdl)%type), &
                                          'for model_handles', i_mdl_hdl
        call faterr(IAM,cout)
      end select

      nb_nodes    = all_model_handles(i_mdl_hdl)%state%nb_nodes
      nb_elements = all_model_handles(i_mdl_hdl)%state%nb_elements

      ! -- node2elements --
      if( allocated(all_model_handles(i_mdl_hdl)%state%node2elements) ) then
        write(cout,'(A,1x,I0,1x,A)') 'node2element of state in model_handle', i_mdl_hdl, &
                                     'already allocated'
        call faterr(IAM,cout)
      end if

      allocate(all_model_handles(i_mdl_hdl)%state%node2elements(nb_nodes), stat=errare)
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                           'while allocating node2element of state in model_handle', i_mdl_hdl
        call faterr(IAM,cout)
      end if

      call set_node2elements(all_model_handles(i_mdl_hdl)%id_modelization, &
                             nb_nodes, &
                             all_model_handles(i_mdl_hdl)%state%node2elements)
      ! -- end node2elements --

      ! -- ppset --
      call set_ppsets(all_model_handles(i_mdl_hdl)%id_modelization, &
                      all_model_handles(i_mdl_hdl)%type, &
                      nb_nodes)
      ! -- end ppset --

      ! -- nodal_fields map --
      if( allocated(all_model_handles(i_mdl_hdl)%state%nf_map) ) then
        write(cout,'(A,1x,I0,1x,A)') 'nf_map of state in model_handle', i_mdl_hdl, &
                                     'already allocated'
        call faterr(IAM,cout)
      end if

      allocate(all_model_handles(i_mdl_hdl)%state%nf_map(nb_nodes+1,integr%nb_nf), stat=errare)
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                           'while allocating nf_map of state in model_handle', i_mdl_hdl
        call faterr(IAM,cout)
      end if

      call all_model_handles(i_mdl_hdl)%integrator%set_cc_nodal_fields(all_model_handles(i_mdl_hdl)%id_modelization, &
                                                                       all_model_handles(i_mdl_hdl)%type, nb_nodes , &
                                                                       all_model_handles(i_mdl_hdl)%state%nf_map   , &
                                                                       nb_dofs_for_soe )

      ! depending on soe or whatever, decide what to put here
      all_model_handles(i_mdl_hdl)%state%nb_dofs = nb_dofs_for_soe

      ! -- end cc_nodal_fields --

      ! -- cc_el_rhs_contrib --
      if( allocated(all_model_handles(i_mdl_hdl)%state%erc_map) ) then
        write(cout,'(A,1x,I0,1x,A)') 'erc_map of state in model_handle', i_mdl_hdl, &
                                     'already allocated'
        call faterr(IAM,cout)
      end if

      allocate(all_model_handles(i_mdl_hdl)%state%erc_map(nb_elements+1), stat=errare)
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                           'while allocating erc_map of state in model_handle', i_mdl_hdl
        call faterr(IAM,cout)
      end if

      call set_cc_el_rhs_contrib(all_model_handles(i_mdl_hdl)%id_modelization, &
                                 all_model_handles(i_mdl_hdl)%state%erc_map)
      ! -- end cc_el_rhs_contrib --

      ! -- edof2gdof --
      do i_element = 1, nb_elements
        if( allocated(all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof) ) then
          write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'edof2gdof of element', i_element, 'in model_handle', &
                                                  i_mdl_hdl, 'already allocated'
          call faterr(IAM,cout)
        end if

        ! \todo : l'intégrateur doit rendre l'index de cc_nodal_fields a utiliser au lieu de 1...
        call set_edof2gdof(all_model_handles(i_mdl_hdl)%id_modelization             , &
                           i_element                                                , &
                           all_model_handles(i_mdl_hdl)%state%nf_map(1:nb_nodes+1,1), &
                           all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof)
      end do
      ! -- end edof2gdof --

      ! -- ef_map, ef_names --
      if( allocated(all_model_handles(i_mdl_hdl)%state%ef_names) ) then
        write(cout,'(A,1x,I0,1x,A)') 'ef_names of state in model_handle', &
                                     i_mdl_hdl, 'already allocated'
        call faterr(IAM,cout)
      end if

      allocate(all_model_handles(i_mdl_hdl)%state%ef_names(nb_elements), stat=errare)
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                           'while allocating ef_names of state in model_handle', i_mdl_hdl
        call faterr(IAM,cout)
      end if

      if( allocated(all_model_handles(i_mdl_hdl)%state%ef_map) ) then
        write(cout,'(A,1x,I0,1x,A)') 'ef_map of state in model_handle', &
                                     i_mdl_hdl, 'already allocated'
        call faterr(IAM,cout)
      end if

      allocate(all_model_handles(i_mdl_hdl)%state%ef_map(nb_elements), stat=errare)
      if( errare /= 0 ) then
        write(cout,'(A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                           'while allocating ef_map of state in model_handle', i_mdl_hdl
        call faterr(IAM,cout)
      end if

      call set_cc_elementary_fields(all_model_handles(i_mdl_hdl)%id_modelization, &
                                    all_model_handles(i_mdl_hdl)%type, &
                                    all_model_handles(i_mdl_hdl)%state%ef_names, &
                                    all_model_handles(i_mdl_hdl)%state%ef_map  , &
                                    ef_size)

      ! -- end ef_map, ef_names --

      ! compute lhs_coeffs and set functions to compute rhs

      nb_matrices = integr%nb_mats

      ! allocate fields in state (dofs, gauss and nodal)
      ! \todo : rm a ajouté de la merde, j'ai écrit explicitement que l'on avait qu'un seul
      !         elementary field, mais l'intégrateur peut moralement en avoir plusieur !
      call state_initialize(all_model_handles(i_mdl_hdl)%state                , &
                            all_model_handles(i_mdl_hdl)%integrator%nf_depths , &
                            all_model_handles(i_mdl_hdl)%integrator%nb_nf     , &
                            all_model_handles(i_mdl_hdl)%integrator%erc_depths, &
                            all_model_handles(i_mdl_hdl)%integrator%nb_erc    , &
                            all_model_handles(i_mdl_hdl)%integrator%ef_depths , &
                            all_model_handles(i_mdl_hdl)%integrator%nb_ef     , &
                            ef_size)

      ! if rigid_3D or 2D: initialize local frame part of nodal_field vector with frame stored in modelization
      ! \todo : maybe an other way to test this (like is rigid_data allocated)
      if( all_model_handles(i_mdl_hdl)%type == p_rigid3D .or. all_model_handles(i_mdl_hdl)%type == p_rigid2D ) then
        call get_nodal_field_indices_boundary(i, j, all_model_handles(i_mdl_hdl)%state, p_meca_td_X, 2)
        call set_local_frame(all_model_handles(i_mdl_hdl)%id_modelization, &
                             all_model_handles(i_mdl_hdl)%type           , &
                             all_model_handles(i_mdl_hdl)%state%nodal_fields(i+nbDIME:j) )
      end if

      ! allocate elementary matrices in modelization
      call initialize_elementary_matrices(all_model_handles(i_mdl_hdl)%id_modelization, &
                                          all_model_handles(i_mdl_hdl)%type, nb_matrices)

      ! allocate elementary matrix and vector of model_handle elements
      do i_element = 1, all_model_handles(i_mdl_hdl)%state%nb_elements

        idof = all_model_handles(i_mdl_hdl)%elements(i_element)%idof
        select case( all_model_handles(i_mdl_hdl)%type )
        case( p_rigid2D, p_rigid3D )
          idof2 = 1
        case( p_mecaMAILx, p_therMAILx )
          idof2 = idof
        case default
          write (cout,'(A,1x,A,1x,A,1x,I0)') 'unknown modelization type', &
                                             get_model_type_from_id(all_model_handles(i_mdl_hdl)%type), &
                                             'for modelization number:', i_mdl_hdl
          call faterr(IAM,cout)
        end select

        if( allocated(all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_lhs) ) then
          write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'elementary_lhs of element', i_element, &
                                                  'in model_handle', i_mdl_hdl, 'already allocated'
          call faterr(IAM,cout)
        end if
        allocate(all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_lhs(1:idof,1:idof2))

        if( allocated(all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_rhs) ) then
          write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'elementary_rhs of element', i_element, &
                                                  'in model_handle', i_mdl_hdl, 'already allocated'
          call faterr(IAM,cout)
        end if
        allocate(all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_rhs(idof), stat=errare)
        if( errare /= 0 ) then
          write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'Error', errare, &
                                                        'while allocating elementary_rhs of element', &
                                                        i_element, 'in model_handle', i_mdl_hdl
          call faterr(IAM,cout)
        end if
        all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_rhs(1:idof) = 0.d0

      end do

    end do

    call soe_initialize(nb_model_handles)
    do i_mdl_hdl = 1, nb_model_handles

      select case(all_model_handles(i_mdl_hdl)%type)
      case( p_rigid2D, p_rigid3D )
        i_storage  = i_std_diag
        with_renum = .false.
      case( p_mecaMAILx, p_therMAILx )
        i_storage  = i_sym_band
        with_renum = .true.
      case default
        write(cout,'(A,1x,A,1x,A,1x,I0)') 'Unknown modelization type', &
                                          get_model_type_from_id(all_model_handles(i_mdl_hdl)%type), &
                                          'for model_handles', i_mdl_hdl
        call faterr(IAM,cout)
      end select

      connectivities => get_connectivities(i_mdl_hdl)
      i_nf  = all_model_handles(i_mdl_hdl)%integrator%i_solve_nodal_field
      call initialize_soe(i_mdl_hdl, all_model_handles(i_mdl_hdl)%state%nf_map(:,i_nf), &
                             connectivities, i_storage, with_renum )

      if( associated(connectivities) ) then
        deallocate(connectivities)
        nullify(connectivities)
      end if

    end do

  end subroutine

  !> \brief Change the type of storage of the system of a model handle
  subroutine change_system_storage(id, i_storage)
    implicit none
    !> [in] index of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] storage type id
    integer(kind=4), intent(in) :: i_storage
    !
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'model_handler::set_system_storage'

    call paranoid_check(id, IAM)

    call change_system_shape(id, i_storage)
    
  end subroutine

  !> \brief Compute the elementary masses of a model_handle
  subroutine compute_elementary_masses(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    character(len=40) :: IAM
    !      1234567890123456789012345678901234567890
    IAM = 'model_handler::compute_elementary_masses'

    call paranoid_check(id, IAM)

    ! which index of nodes ? 1, 2 ?
    call compute_masses(all_model_handles(id)%id_modelization, &
                        all_model_handles(id)%type           , &
                        p_mass_mat_index                     , &
                        all_model_handles(id)%state%nodes(:,:,1))
      
  end subroutine

  !> \brief Compute the elementary capacities of a model_handle
  subroutine compute_elementary_capacities(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    real(kind=8), dimension(:), allocatable :: temp
    integer(kind=4)   :: nb_nodes, nb_dofs, i1, i2, i3, i4
    real(kind=8)      :: dt
    character(len=44) :: IAM
    !      12345678901234567890123456789012345678901234
    IAM = 'model_handler::compute_elementary_capacities'

    call paranoid_check(id, IAM)

    ! could be theta*dt ?
    dt = all_model_handles(id)%integrator%params(p_time_step)

    nb_nodes = all_model_handles(id)%state%nb_nodes + 1
    nb_dofs  = all_model_handles(id)%state%nf_map(nb_nodes,p_ther_td_T)
    allocate(temp(nb_dofs))
    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, p_ther_td_T, 2)
    call get_nodal_field_indices_boundary(i3, i4, all_model_handles(id)%state, p_ther_td_T, 1)
    temp(1:nb_dofs) = all_model_handles(id)%state%nodal_fields(i3:i4) - all_model_handles(id)%state%nodal_fields(i1:i2)

    call get_el_rhs_contrib_indices_boundary(i1,i2,all_model_handles(id)%state, p_ther_td_Fint, 1)

    ! which index of nodes ? 1, 2 ?
    call compute_capacities(all_model_handles(id)%id_modelization, &
                            all_model_handles(id)%type                        , &
                            p_capa_mat_index                                  , &
                            dt                                                , &
                            all_model_handles(id)%state%nodes(:,:,1)          , &
                            temp                                              , &
                            all_model_handles(id)%state%nf_map(:,p_ther_td_T) , &
                            all_model_handles(id)%state%elementary_fields(:,2), &
                            all_model_handles(id)%state%ef_map                , &
                            all_model_handles(id)%state%ef_names              , &
                            all_model_handles(id)%state%el_rhs_contribs(i1:i2), &
                            all_model_handles(id)%state%erc_map(:)              )

    deallocate(temp)

  end subroutine

  !> \brief Compute the elementary conductivities of a model_handle
  subroutine compute_elementary_conductivities(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    real(kind=8), dimension(:), allocatable :: temp
    integer(kind=4)   :: nb_nodes, nb_dofs, i1, i2
    real(kind=8)      :: dt
    character(len=48) :: IAM
    !      123456789012345678901234567890123456789012345678
    IAM = 'model_handler::compute_elementary_conductivities'

    call paranoid_check(id, IAM)


    ! could be theta*dt ?
    dt = all_model_handles(id)%integrator%params(p_time_step)

    nb_nodes = all_model_handles(id)%state%nb_nodes + 1
    nb_dofs  = all_model_handles(id)%state%nf_map(nb_nodes,p_ther_td_T)
    allocate(temp(nb_dofs))
    call all_model_handles(id)%integrator%get_config_for_bulk_comp(all_model_handles(id)%state, temp, &
                                                                   all_model_handles(id)%integrator%params)

    call get_el_rhs_contrib_indices_boundary(i1,i2,all_model_handles(id)%state, p_ther_td_Fint, 1)

    ! which index of nodes ? 1, 2 ?
    call compute_conductivities(all_model_handles(id)%id_modelization             , &
                                all_model_handles(id)%type                        , &
                                p_cond_mat_index                                  , &
                                dt                                                , &
                                all_model_handles(id)%state%nodes(:,:,1)          , &
                                temp                                              , &
                                all_model_handles(id)%state%nf_map(:,p_ther_td_T) , &
                                all_model_handles(id)%state%elementary_fields(:,2), &
                                all_model_handles(id)%state%ef_map                , &
                                all_model_handles(id)%state%ef_names              , &
                                all_model_handles(id)%state%el_rhs_contribs(i1:i2), &
                                all_model_handles(id)%state%erc_map(:)              )

    deallocate(temp)

  end subroutine

  !> \brief Compute the elementary external forces of a model_handle
  subroutine compute_elementary_fext(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    integer(kind=4)   :: i1, i2
    character(len=80) :: cout
    character(len=38) :: IAM
    !      12345678901234567890123456789012345678
    IAM = 'model_handler::compute_elementary_fext'

    select case( all_model_handles(id)%type )

    case ( p_rigid2D, p_rigid3D, p_mecaMAILx )

      call get_el_rhs_contrib_indices_boundary(i1,i2,all_model_handles(id)%state, p_meca_td_Fext, 1)
      call compute_external_forces(all_model_handles(id)%id_modelization             , &
                                   all_model_handles(id)%type, p_mass_mat_index      , &
                                   all_model_handles(id)%state%el_rhs_contribs(i1:i2), &
                                   all_model_handles(id)%state%erc_map )
    case ( p_therMAILx )
      ! do something for thermal models ?
    case default
      ! if not all model type are tested in the 'select case' block, remove faterr call
      write(cout,'(A,1x,I0,1x,A,1x,A)') 'model_handle :', id, 'has an unknwon model type:', &
                                         get_model_type_from_id(all_model_handles(id)%type)
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Compute the elementary bulks (internal forces and grad_q of internal forces) of a model_handle
  !> This function works currently only with mechanical modelization... but does nothing for newton euler type
  subroutine compute_elementary_bulks(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    real(kind=8), dimension(:), allocatable :: du
    real(kind=8)      :: dt
    integer(kind=4)   :: i1, i2, nb_x_dofs, nb_nodes, nb_elements
    character(len=80) :: cout
    character(len=39) :: IAM
    !      123456789012345678901234567890123456789
    IAM = 'model_handler::compute_elementary_bulks'

    call paranoid_check(id, IAM)

    select case( all_model_handles(id)%type )

    ! mouarf : tested in compute bulks too !
    case ( p_rigid2D, p_rigid3D )

    case ( p_mecaMAILx )

      nb_nodes    = all_model_handles(id)%state%nb_nodes + 1
      nb_elements = all_model_handles(id)%state%nb_elements + 1
      nb_x_dofs   = all_model_handles(id)%state%nf_map(nb_nodes,p_meca_td_X)

      allocate(du(nb_x_dofs))

      ! could be theta*dt ?
      dt = all_model_handles(id)%integrator%params(p_time_step) * all_model_handles(id)%integrator%params(p_theta)

      ! ask integrator to give a displacement
      call all_model_handles(id)%integrator%get_config_for_bulk_comp(all_model_handles(id)%state, du, &
                                                                     all_model_handles(id)%integrator%params)

      call get_el_rhs_contrib_indices_boundary(i1,i2,all_model_handles(id)%state, p_meca_td_Fint, 1)
      ! which index of nodes ? 1 for cooref, 2 for those a beginning of time step?
      call compute_bulks(all_model_handles(id)%id_modelization             , &
                         all_model_handles(id)%type                        , &
                         p_stif_mat_index                                  , &
                         dt                                                , &
                         all_model_handles(id)%state%nodes(:,:,1)          , &
                         du                                                , &
                         all_model_handles(id)%state%nf_map(:,p_meca_td_X) , &
                         all_model_handles(id)%state%elementary_fields(:,2), &
                         all_model_handles(id)%state%elementary_fields(:,1), &
                         all_model_handles(id)%state%ef_map                , &
                         all_model_handles(id)%state%ef_names              , &
                         all_model_handles(id)%state%el_rhs_contribs(i1:i2), &
                         all_model_handles(id)%state%erc_map(:)           )
    case ( p_therMAILx )
      ! do something for thermal models ?
    case default
      ! if not all model type are tested in the 'select case' block, remove faterr call
      write(cout,'(A,1x,I0,1x,A,1x,A)') 'model_handle :', id, 'has an unknown model type:', &
                                         get_model_type_from_id(all_model_handles(id)%type)
      call faterr(IAM,cout)
    end select
      
  end subroutine

  !> \brief Compute elementary LHS of a model_handle
  subroutine compute_elementary_lhs(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    integer(kind=4)   :: i_element
    character(len=37) :: IAM
    !      1234567890123456789012345678901234567
    IAM = 'model_handler::compute_elementary_lhs'

    !write(*, '(A,1x,I0)') 'Body:', id
    do i_element = 1, all_model_handles(id)%state%nb_elements
      call compute_lhs(all_model_handles(id)%id_modelization, &
                       all_model_handles(id)%type, i_element, &
                       all_model_handles(id)%integrator%lhs_coeff, &
                       all_model_handles(id)%elements(i_element)%elementary_lhs)
      !write(*, '(A,1x,I0)') 'Element:', i_element
      !write(*, '(A)') 'Elementary LHS'
      !write(*, '(4(1x,E20.13))') all_model_handles(id)%elements(i_element)%elementary_lhs
    end do
      
  end subroutine

  !> \brief Compute elementary RHS of a model_handle
  subroutine compute_elementary_rhs(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    integer(kind=4)   :: i_element
    character(len=37) :: IAM
    !      1234567890123456789012345678901234567
    IAM = 'model_handler::compute_elementary_rhs'

    !write(*, '(A,1x,I0)') 'Body:', id
    do i_element = 1, all_model_handles(id)%state%nb_elements

      call all_model_handles(id)%integrator%compute_rhs(all_model_handles(id)%id_modelization , &
                                                        all_model_handles(id)%type            , &
                                                        all_model_handles(id)%state, i_element, &
                                                        all_model_handles(id)%elements(i_element)%edof2gdof, & 
                                                        all_model_handles(id)%elements(i_element)%idof     , & 
                                                        all_model_handles(id)%elements(i_element)%elementary_rhs, &
                                                        all_model_handles(id)%integrator%params )
      !write(*, '(A,1x,I0)') 'Element:', i_element
      !write(*, '(A)') 'Elementary RHS'
      !write(*,*) all_model_handles(id)%elements(i_element)%elementary_rhs
    end do
      
  end subroutine

  !> \brief Get the number of degrees of freedom of the corresponding soe
  function get_nb_dofs(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] number of degrees of freedom
    integer(kind=4)             :: get_nb_dofs

    call paranoid_check(id, 'model_handler::get_nb_dofs')

    get_nb_dofs = all_model_handles(id)%state%nb_dofs

  end function

  !> \brief Get the matrix storage type of a model_handle
  function get_storage(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] id of the storage type
    integer(kind=4)             :: get_storage

    call paranoid_check(id, 'model_handler::get_storage')

    get_storage = 0!i_std_diag all_model_handles(id)%i_storage
    call faterr('model_handler::get_storage','not implemented yet')

  end function

  !> \brief Get if the nodes of the model_handle must be renumbered
  function get_if_renum(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] true if nodes must be renumbered
    logical :: get_if_renum

    call faterr('model_handler::get_if_renum','not implemented yet')
    call paranoid_check(id, 'model_handler::get_if_renum')

    get_if_renum =0! all_model_handles(id)%with_renum

  end function

  !> \brief Get the integer type of a model_handle
  function get_type(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] the type of the model_handle
    integer(kind=4)             :: get_type

    call paranoid_check(id, 'model_handler::get_type')

    get_type = all_model_handles(id)%type

  end function

  !> \brief Get the number of node(s) of a model_handle
  function get_nb_nodes(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] the number of node(s) in a model_handle
    integer(kind=4)             :: get_nb_nodes

    get_nb_nodes = 0

    call paranoid_check(id, 'model_handler::get_nb_nodes')

    if( associated(all_model_handles(id)%state) ) then
      get_nb_nodes = all_model_handles(id)%state%nb_nodes
    end if

  end function

  !> \brief Assemble elementary matrices of a model_handle in its system
  subroutine assemble_elementary_lhs_4all()
    implicit none
    integer(kind=4) :: i_mdl_hdl, i_field, i_element, i_node, i_dof, i_drvdof, nb_drvdofs
    integer(kind=4), dimension(:), allocatable :: drivdof

    do i_mdl_hdl = 1, nb_model_handles

      call compute_elementary_lhs(i_mdl_hdl)

      do i_element = 1, all_model_handles(i_mdl_hdl)%state%nb_elements
        call set_elementary_lhs(i_mdl_hdl, i_element, &
                                all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_lhs)
      end do

      nb_drvdofs = 0
      if( allocated(all_model_handles(i_mdl_hdl)%prim_drvdofs) ) then

        i_field =  all_model_handles(i_mdl_hdl)%integrator%i_solve_nodal_field

        nb_drvdofs = size(all_model_handles(i_mdl_hdl)%prim_drvdofs)
        allocate(drivdof(nb_drvdofs))
        do i_drvdof = 1, nb_drvdofs
          i_node  =  all_model_handles(i_mdl_hdl)%prim_drvdofs(i_drvdof)%nodnb
          drivdof(i_drvdof) =  all_model_handles(i_mdl_hdl)%state%nf_map(i_node,i_field) &
                             + all_model_handles(i_mdl_hdl)%prim_drvdofs(i_drvdof)%dofnb

        end do

        call set_system_drvdofs(i_mdl_hdl, drivdof)
        deallocate(drivdof)
      else
        call erase_system_drvdofs(i_mdl_hdl)
      end if

    end do

  end subroutine

  !> \brief Compute the dofs free of interactions for a model handle
  subroutine compute_free_state(id, i_sol, i_rhs)
    use overall, only : TPSbegin
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] index of soe solution in which to solve
    integer(kind=4), intent(in) :: i_sol
    !> [in] index of RHS to use
    integer(kind=4), intent(in) :: i_rhs
    !
    real(kind=8)   , dimension(:), allocatable :: driv
    integer(kind=4), dimension(:), allocatable :: drivdof
    real(kind=8)      :: val, time
    integer(kind=4)   :: i, j, i_field, i_drvdof, nb_drvdofs, i_node, i_nf, i_dof, nb_dofs
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'model_handler::compute_free_state'

    nb_dofs = all_model_handles(id)%state%nb_dofs

    i_field =  all_model_handles(id)%integrator%i_solve_nodal_field
    ! reset i_sol value
    call get_nodal_field_indices_boundary(i,j, all_model_handles(id)%state, i_field, 1)
    call set_sol_value(id, i_sol, all_model_handles(id)%state%nodal_fields(i:j))

    if( allocated(all_model_handles(id)%prim_drvdofs) ) then

      !> \todo : get time from integrator
      time    = TPSbegin

      nb_drvdofs = size(all_model_handles(id)%prim_drvdofs)
      allocate(driv(nb_drvdofs), drivdof(nb_drvdofs))

      do i_drvdof = 1, nb_drvdofs
        ! time depths : 2 -> use of value at beginning of time step
        i_node =  all_model_handles(id)%prim_drvdofs(i_drvdof)%nodnb
        i_dof  =  all_model_handles(id)%state%nf_map(i_node,i_field)         &
                + all_model_handles(id)%prim_drvdofs(i_drvdof)%dofnb
        i_nf  = i_dof + all_model_handles(id)%state%nf_map_shift(i_field)%G_i(2)
 
        call comp_a_driven_dof_at_t(all_model_handles(id)%prim_drvdofs(i_drvdof), time, val)
        driv(i_drvdof)    = val
        drivdof(i_drvdof) = i_dof
      end do

      call apply_driven_dof_rhs(id, i_sol, i_rhs, driv, drivdof)
      deallocate(driv, drivdof)

    else

      call set_system_rhs(id, i_rhs)

    end if

    call solve(id, i_sol, i_rhs)

    ! storing V = V + Vfree 
    call get_nodal_field_indices_boundary(i, j, all_model_handles(id)%state, i_field, 1)
    call add_sol_contributions(id, all_model_handles(id)%state%nodal_fields(i:j), (/i_sol/))

    ! put in soe too for contact solver use
    call set_sol_value(id, i_sol, all_model_handles(id)%state%nodal_fields(i:j))

  end subroutine

  !> \brief Assemble elementary vectors of a model_handle in an assembled vector
  subroutine assemble_elementary_rhs_4all(i_rhs)
    use overall, only : TPSbegin
    implicit none
    !> [in] index of rhs in which to assemble
    integer(kind=4), intent(in)  :: i_rhs
    !
    real(kind=8)    :: time
    integer(kind=4) :: i_mdl_hdl, i_element, nb_drvdofs, i_node, i_dof, i_drvdof, i_field
    real(kind=8)   , dimension(:), allocatable :: driv
    integer(kind=4), dimension(:), allocatable :: drivdof


    do i_mdl_hdl = 1, nb_model_handles

      ! \todo: get time from integrator :
      time = TPSbegin

      call compute_elementary_rhs(i_mdl_hdl)

      call reset_rhs(i_mdl_hdl, i_rhs)
      do i_element = 1, all_model_handles(i_mdl_hdl)%state%nb_elements
        call assemble_rhs(i_mdl_hdl, i_rhs , &
                          all_model_handles(i_mdl_hdl)%elements(i_element)%elementary_rhs, &
                          all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof           )
      end do

      if( allocated(all_model_handles(i_mdl_hdl)%dual_drvdofs) ) then
        nb_drvdofs = size(all_model_handles(i_mdl_hdl)%dual_drvdofs)
        allocate( driv(nb_drvdofs), drivdof(nb_drvdofs) )
        call all_model_handles(i_mdl_hdl)%integrator%compute_dual_drvdofs(time                                     , &
                                                                          all_model_handles(i_mdl_hdl)%dual_drvdofs, &
                                                                          all_model_handles(i_mdl_hdl)%state       , &
                                                                          driv, nb_drvdofs                         , &
                                                                          all_model_handles(i_mdl_hdl)%integrator%params)
        i_field =  all_model_handles(i_mdl_hdl)%integrator%i_solve_nodal_field
        do i_drvdof = 1, nb_drvdofs
          i_node  =  all_model_handles(i_mdl_hdl)%dual_drvdofs(i_drvdof)%nodnb
          drivdof(i_drvdof) =  all_model_handles(i_mdl_hdl)%state%nf_map(i_node,i_field) &
                             + all_model_handles(i_mdl_hdl)%dual_drvdofs(i_drvdof)%dofnb
        end do
        call assemble_rhs(i_mdl_hdl, i_rhs , driv, drivdof)
        deallocate( driv, drivdof)
      end if

    end do

  end subroutine

  !> \brief Compute the degrees of freedom which were not computed by the solver
  subroutine compute_dofs(id, i_sol, i_rhs, i_list)
    use overall, only : TPSbegin
    implicit none
    !> [in] id of the model_handle
    integer(kind=4),               intent(in) :: id
    !> [in] index of solution of soe
    integer(kind=4),               intent(in) :: i_sol
    !> [in] index of rhs of soe
    integer(kind=4),               intent(in) :: i_rhs
    !> [in] list of solution vectors to get from soe
    integer(kind=4), dimension(:), intent(in) :: i_list
    !
    integer(kind=4) :: i_nf, i1, i2, i_node, i_field, i_drvdof, nb_drvdofs
    real(kind=8)    :: time, val
    real(kind=8), dimension(:), allocatable :: driv

    ! todo: it would be a good time to do something about it
    time = TPSbegin

    call set_system_rhs(id, i_rhs)
    if( allocated(all_model_handles(id)%prim_drvdofs) ) then
      nb_drvdofs = size(all_model_handles(id)%prim_drvdofs)
      allocate(driv(nb_drvdofs))
      driv = 0D0
      call set_system_drvvalues(id, driv)
      deallocate(driv)
    end if

    ! solve the linear system of reaction term
    call solve(id,i_sol,i_rhs) ! 2 : for reac RHS/LHS

    !print *,'------------------------'
    !print *,'id : ', id
    i_nf = all_model_handles(id)%integrator%i_solve_nodal_field

    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, i_nf, 1)

    ! new value is last value plus contributions given by the solver
    call add_sol_contributions(id, all_model_handles(id)%state%nodal_fields(i1:i2), i_list)

    ! reseting V/T to V/Tdriv on driven lines
    if( allocated(all_model_handles(id)%prim_drvdofs) ) then
      i_field =  all_model_handles(id)%integrator%i_solve_nodal_field
      nb_drvdofs = size(all_model_handles(id)%prim_drvdofs)
      do i_drvdof = 1, nb_drvdofs
        ! time depths : 1 -> set current value
        i_node =  all_model_handles(id)%prim_drvdofs(i_drvdof)%nodnb
        i_nf   =  all_model_handles(id)%state%nf_map_shift(i_field)%G_i(1) &
                + all_model_handles(id)%state%nf_map(i_node,i_field)       &
                + all_model_handles(id)%prim_drvdofs(i_drvdof)%dofnb
        call comp_a_driven_dof_at_t(all_model_handles(id)%prim_drvdofs(i_drvdof), time, val)
        all_model_handles(id)%state%nodal_fields(i_nf) = val
      end do
    end if

    ! compute degrees of freedom not computed by the solver
    if( associated(all_model_handles(id)%integrator%compute_dofs) ) then
      call all_model_handles(id)%integrator%compute_dofs( all_model_handles(id)%state, all_model_handles(id)%integrator%params )
    !else
    !  print *,' temperature : '
    !  print *, all_model_handles(id)%state%nodal_fields(i1:i2)
    end if
    !print *,'------------------------'

  end subroutine

  !> \brief Compute convergence norms of a model_handle
  function compute_residue_norm(i_sol, i_list)
    implicit none
    !> [in] index of solution to compute new norm
    integer(kind=4),               intent(in)  :: i_sol
    !> [in] list of rhs vector to use to compute norm of the residue
    integer(kind=4), dimension(:), intent(in)  :: i_list
    !> [return] compute_residue_norm
    real(kind=8) :: compute_residue_norm
    !
    integer(kind=4) :: i, g_i, i_nf, i1, i2, nb_dofs, i_erc, i_element, i_dof, i_mat, i_node
    integer(kind=4) :: i_mdl_hdl, i_mdl_hdl_res, i_mdl_hdl_sol, nb_drvdofs, i_drvdof
    real(kind=8)    :: max_ddof, max_dof, max_res, max_f, max_erc
    real(kind=8)    :: norm_res, new_norm_res, norm_sol, new_norm_sol
    real(kind=8), dimension(:), allocatable :: dof, rhs, nodal_vector, dv, inert
    character(len=80) :: cout
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'model_handler::compute_residue_norm'

    norm_res = 0.d0
    norm_sol = 0.d0

    i_mdl_hdl_res = 0
    i_mdl_hdl_sol = 0

    do i_mdl_hdl = 1, nb_model_handles

      nb_dofs = all_model_handles(i_mdl_hdl)%state%nb_dofs

      allocate( dof(nb_dofs), rhs(nb_dofs) )
      dof(1:nb_dofs) = 0.d0; rhs(1:nb_dofs) = 0.d0
      call add_rhs_contributions(i_mdl_hdl, rhs(1:nb_dofs), i_list)
      call add_sol_contributions(i_mdl_hdl, dof(1:nb_dofs), (/i_sol/))

      if( allocated(all_model_handles(i_mdl_hdl)%prim_drvdofs) ) then
        nb_drvdofs = size(all_model_handles(i_mdl_hdl)%prim_drvdofs)
        i_nf       = all_model_handles(i_mdl_hdl)%integrator%i_solve_nodal_field

        ! reseting rhs to 0 on driven lines
        do i_drvdof = 1, nb_drvdofs
          i_node =  all_model_handles(i_mdl_hdl)%prim_drvdofs(i_drvdof)%nodnb
          i_dof  =  all_model_handles(i_mdl_hdl)%state%nf_map(i_node,i_nf) &
                  + all_model_handles(i_mdl_hdl)%prim_drvdofs(i_drvdof)%dofnb
          rhs(i_dof) =  0.d0
        end do
      end if

      if( is_model_meca(all_model_handles(i_mdl_hdl)%type) ) i_mat = p_mass_mat_index
      if( is_model_ther(all_model_handles(i_mdl_hdl)%type) ) i_mat = p_cond_mat_index

      ! convergence norm of the dofs :
      max_ddof = max( maxval(dof), abs(minval(dof)) )

      i_nf = all_model_handles(i_mdl_hdl)%integrator%i_solve_nodal_field
      call get_nodal_field_indices_boundary(i1, i2, all_model_handles(i_mdl_hdl)%state, i_nf, 1)
      max_dof  = max(     maxval(all_model_handles(i_mdl_hdl)%state%nodal_fields(i1:i2)), &
                      abs(minval(all_model_handles(i_mdl_hdl)%state%nodal_fields(i1:i2))) )

      if( max_dof < 1d-10 ) max_dof = 1.d0

      !call mdl_hdl_compute_residue_norm(i_system, all_systems(i_system)%X(:,1), tmp, new_norm_X, new_norm_Res)
      new_norm_sol = max_ddof / max_dof

      ! convergence norm of the residue :
      max_res = max( maxval(rhs), abs(minval(rhs)) )

      allocate( nodal_vector(nb_dofs) )
      ! for each elementary rhs contribution
      max_f = 0.d0
      do i_erc = 1, all_model_handles(i_mdl_hdl)%integrator%nb_erc
        nodal_vector(1:nb_dofs) = 0.d0
        do i_element = 1, all_model_handles(i_mdl_hdl)%state%nb_elements
          call get_element_el_rhs_contrib_indices_boundary(i1, i2, all_model_handles(i_mdl_hdl)%state, i_erc, i_element, 1)
          call assemb_vector(nodal_vector,all_model_handles(i_mdl_hdl)%state%el_rhs_contribs(i1:i2), &
                             all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof              )
        end do
        max_erc  = max( maxval(nodal_vector(1:nb_dofs)), abs(minval(nodal_vector(1:nb_dofs))) )
        max_f = max(max_f, max_erc)
      end do
      ! check inertia term too
      nodal_vector(1:nb_dofs) = 0.d0
      call get_nodal_field_index(i1, all_model_handles(i_mdl_hdl)%state, i_nf, 1)
      call get_nodal_field_index(i2, all_model_handles(i_mdl_hdl)%state, i_nf, 2)
      do i_element = 1, all_model_handles(i_mdl_hdl)%state%nb_elements
        i_dof = all_model_handles(i_mdl_hdl)%elements(i_element)%idof
        allocate(dv(i_dof))
        allocate(inert(i_dof))
        do i = 1, i_dof
          g_i   =   all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof(i)
          dv(i) =   all_model_handles(i_mdl_hdl)%state%nodal_fields(i2+g_i) &
                  - all_model_handles(i_mdl_hdl)%state%nodal_fields(i1+g_i)
        end do
        call compute_inertia_forces(all_model_handles(i_mdl_hdl)%id_modelization, &
                                    all_model_handles(i_mdl_hdl)%type, i_element, i_mat, inert, dv)
        call assemb_vector(nodal_vector,inert, all_model_handles(i_mdl_hdl)%elements(i_element)%edof2gdof)
        deallocate(dv)
        deallocate(inert)
      end do
      max_erc  = max( maxval(nodal_vector(1:nb_dofs)), abs(minval(nodal_vector(1:nb_dofs))) )
      max_f = max(max_f, max_erc)
      ! multiplication with dt, since this term should be integrated on the time step
      max_f = max_f * all_model_handles(i_mdl_hdl)%integrator%params(p_time_step)

      if( max_f <= 1d-01 ) max_f = 1.d0
      new_norm_res = max_res / max_f

      deallocate( nodal_vector )

      deallocate(dof, rhs)

      if( norm_sol < new_norm_sol ) then
        norm_sol      = new_norm_sol
        i_mdl_hdl_sol = i_mdl_hdl
      end if

      if( norm_res < new_norm_res ) then
        norm_res      = new_norm_res
        i_mdl_hdl_res = i_mdl_hdl
      end if

    end do

    compute_residue_norm = norm_Res

    call logmes('')
    write(cout,'(1X,A,1X,A,D12.5,1X,A,I0)') ' @ ','MaxRes/MaxRhsContrib = ',norm_res,'body : ', i_mdl_hdl_res
    call logmes(cout)
    write(cout,'(1X,A,1X,A,D12.5,1X,A,I0)') ' @ ','MaxDX /MaxX          = ',norm_sol,'body : ', i_mdl_hdl_sol
    call logmes(cout)
    call logmes('')

  end function

  !> \brief Update the state of a model_handle
  subroutine update(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !
    integer(kind=4)   :: i1, i2, i3, i4, nb_x_dofs, nb_nodes, nb_elements, i_nf
    character(len=80) :: cout
    character(len=21) :: IAM
    !      123456789012345678901
    IAM = 'model_handler::update'

    call paranoid_check(id, IAM)

    ! update of elementary fields
    select case( all_model_handles(id)%type )

    case ( p_rigid2D, p_rigid3D )

    case ( p_mecaMAILx )

      ! next block is like 'update_bulk' function in mod_mecaMAILx
      ! it looks very much like compute_bulk

      nb_nodes    = all_model_handles(id)%state%nb_nodes + 1
      nb_elements = all_model_handles(id)%state%nb_elements + 1
      nb_x_dofs   = all_model_handles(id)%state%nf_map(nb_nodes,p_meca_td_X)


      call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, p_meca_td_X, 1)

      ! which index of nodes ? 1 for cooref, 2 for those a beginning of time step?
      call compute_fields(all_model_handles(id)%id_modelization                     , &
                          all_model_handles(id)%type                                , &
                          all_model_handles(id)%integrator%params(p_time_step)      , &
                          all_model_handles(id)%state%nodes(:,:,1)                  , &
                          all_model_handles(id)%state%nodal_fields(i1:i2)           , &
                          all_model_handles(id)%state%nf_map(:,p_meca_td_X)         , &
                          all_model_handles(id)%state%elementary_fields(:,2)        , &
                          all_model_handles(id)%state%elementary_fields(:,1)        , &
                          all_model_handles(id)%state%ef_map                        , &
                          all_model_handles(id)%state%ef_names )
    case ( p_therMAILx )
      ! do something for thermal models ?
    case default
      ! if not all model type are tested in the 'select case' block, remove faterr call
      write(cout,'(A,1x,I0,1x,A,1x,A)') 'model_handle :', id, 'has an unknwon model type:', &
                                         get_model_type_from_id(all_model_handles(id)%type)
      call faterr(IAM,cout)
    end select
      
    ! update of the state
    call state_update(all_model_handles(id)%state                , &
                      all_model_handles(id)%integrator%nf_depths , &
                      all_model_handles(id)%integrator%nb_nf     , &
                      all_model_handles(id)%integrator%erc_depths, &
                      all_model_handles(id)%integrator%nb_erc    , &
                      all_model_handles(id)%integrator%ef_depths , &
                      all_model_handles(id)%integrator%nb_ef )

    ! initialize current value with previous for the solved nodal field
    ! -> otherwise the inertia term in compute_rhs of the integrator may be wrong
    ! -> and allows to add the increment of dof solved by soe to last computed value (nl only)
    i_nf = all_model_handles(id)%integrator%i_solve_nodal_field
    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, i_nf, 1)
    call get_nodal_field_indices_boundary(i3, i4, all_model_handles(id)%state, i_nf, 2)

    all_model_handles(id)%state%nodal_fields(i1:i2) = all_model_handles(id)%state%nodal_fields(i3:i4)

  end subroutine

  !> \brief Get the coordinates of a body
  subroutine get_coor(id, i_what, coor)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] what coordinates to get (reference, detection...)
    integer(kind=4), intent(in) :: i_what
    !> [out] coordinates
    real(kind=8), dimension(:,:), pointer :: coor
    !
    integer(kind=4)   :: nb_nodes, nnb_nodes, i1, i2
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'model_handler::get_coor'

    ! some checks
    call paranoid_check(id, IAM)

    nb_nodes = all_model_handles(id)%state%nb_nodes

    select case(all_model_handles(id)%type)
    case(p_rigid2D)
      nnb_nodes = 3
    case(p_rigid3D)
      nnb_nodes = 4
    case default
      nnb_nodes = nb_nodes
    end select

    if( .not. associated(coor) ) then
      allocate(coor(nbDIME,nnb_nodes))
    else
      print *, shape(coor), nbDIME, nb_nodes, nnb_nodes
      if( any(shape(coor) < (/nbDIME,nnb_nodes/)) ) then
        call faterr(IAM,'wrong size... get lost!')
      end if
    end if

    coor = 0.d0

    ! in case of coor_begin or coor_ add the displacement to the reference coordinates
    !> \todo : the reshape is badly done, in case of RBDY3 the shape is not of the same size of the input array
    !>         check if the norm states if the behaviour obtained is standard.
    select case( i_what )
    case( p_coor_begin )
      call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, p_meca_td_X, 1)
      coor(1:nbDIME,1:nb_nodes)  = all_model_handles(id)%state%nodes(1:nbDIME,1:nb_nodes,1)
      coor(1:nbDIME,1:nnb_nodes) = coor(1:nbDIME,1:nnb_nodes) &
                                  + reshape(all_model_handles(id)%state%nodal_fields(i1:i2), shape=(/nbDIME,nnb_nodes/))
    case( p_coor_ )
      call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, p_meca_td_X, 2)
      coor(1:nbDIME,1:nb_nodes)  = all_model_handles(id)%state%nodes(1:nbDIME,1:nb_nodes,1)
      coor(1:nbDIME,1:nnb_nodes) = coor(1:nbDIME,1:nnb_nodes) & 
                                  + reshape(all_model_handles(id)%state%nodal_fields(i1:i2), shape=(/nbDIME,nnb_nodes/))
    case default
      ! otherwise if the index is available just get the corresponding coordinates
      if( i_what > size(all_model_handles(id)%state%nodes,3) ) then
        call faterr(IAM,'do not know what to do with coordinates type : ')
      else
        coor(1:nbDIME,1:nb_nodes) = all_model_handles(id)%state%nodes(1:nbDIME,1:nb_nodes,i_what)
      end if
    end select

  end subroutine

  !> \brief Get the detection configuration
  subroutine get_detection_configuration(id, node_list, rigid_detec_config, defor_detec_config)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4),               intent(in) :: id
    !> [in] list of nodes to compute
    integer(kind=4), dimension(:), intent(in) :: node_list
    !> [in,out] detection configuration of the rigid part
    real(kind=8),    dimension(:,:) , allocatable :: rigid_detec_config
    !> [in,out] detection configuration of the deformable part
    real(kind=8),    dimension(:,:) , allocatable :: defor_detec_config
    !
    integer(kind=4)   :: nb_nodes, nb_nodes_list, i1, i2
    character(len=23) :: IAM
    !      12345678901234567890123
    IAM = 'model_handler::get_detection_configuration'

    ! some checks
    call paranoid_check(id, IAM)

    nb_nodes      = all_model_handles(id)%state%nb_nodes
    nb_nodes_list = size(node_list)

    if( any( node_list > nb_nodes ) ) then
      call faterr(IAM,'wrong node index asked... get lost!')
    end if

    if( associated(all_model_handles(id)%integrator%compute_detec_config) ) then
      call all_model_handles(id)%integrator%compute_detec_config(all_model_handles(id)%state            , &
                                                                 all_model_handles(id)%integrator%params, &
                                                                 node_list,rigid_detec_config,defor_detec_config)
    end if

  end subroutine

  !> \brief Get the number of nodal field stored by a model_handle
  function get_nb_nodal_field(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] the number of nodal field in the model_handle
    integer(kind=4)             :: get_nb_nodal_field

    get_nb_nodal_field = 0

    if( associated(all_model_handles(id)%integrator) ) then
      get_nb_nodal_field = all_model_handles(id)%integrator%nb_nf
    end if

  end function

  !> \brief Get a nodal field of a body
  subroutine get_nodal_field(id, i_nf, i_when, nodal_field)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] id of the nodal field to get
    integer(kind=4), intent(in) :: i_nf
    !> [in] time depths index (1: current, 2: previous...)
    integer(kind=4), intent(in) :: i_when
    !> [out] nodal field
    real(kind=8), dimension(:,:), pointer :: nodal_field
    !
    integer(kind=4)   :: i1, i2, i_size, nb_nodes, dime, nf_shape(2)
    character(len=80) :: cout
    character(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = 'model_handler::get_nodal_field'

    ! paranoid check :
    call paranoid_check(id, IAM)

    if( i_when > all_model_handles(id)%integrator%nf_depths(i_nf) ) then
      write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id, &
                                                 ', time depth index out of range, cannot be greater than', &
                                                 all_model_handles(id)%integrator%nf_depths(i_nf), 'and is', i_when
      call faterr(IAM,cout)
    end if

    nb_nodes = all_model_handles(id)%state%nb_nodes
    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, i_nf, i_when)
    i_size   = i2 - i1 + 1
    dime     = i_size / nb_nodes
    nf_shape = (/ dime, nb_nodes /)

    if( .not. associated(nodal_field) ) then
      allocate(nodal_field(dime,nb_nodes))
    else
      if( any(shape(nodal_field) /= nf_shape) ) then
        write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id, &
                                                   ', output vector already allocated, but of wrong shape, is:', &
                                                   shape(nodal_field), 'and should be:', nf_shape
        call faterr(IAM,cout)
      end if
    end if

    nodal_field(1:dime,1:nb_nodes) = reshape(all_model_handles(id)%state%nodal_fields(i1:i2), shape=nf_shape )

  end subroutine

  !> \brief Get a pointer on a nodal field of a body
  function get_nodal_field_ptr(id, i_nf, i_when, nb_nodes)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in)  :: id
    !> [in] id of the nodal field to get
    integer(kind=4), intent(in)  :: i_nf
    !> [in] time depths index (1: current, 2: previous...)
    integer(kind=4), intent(in)  :: i_when
    !> [in] number of nodes in the nodal field
    integer(kind=4), intent(out) :: nb_nodes
    !> [out] nodal field
    real(kind=8), dimension(:), pointer :: get_nodal_field_ptr
    !
    integer(kind=4)   :: i1, i2, i_size
    character(len=80) :: cout
    character(len=34) :: IAM
    !      1234567890123456789012345678901234
    IAM = 'model_handler::get_nodal_field_ptr'

    ! paranoid check :
    call paranoid_check(id, IAM)

    if( i_when > all_model_handles(id)%integrator%nf_depths(i_nf) ) then
      write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id, &
                                                 ', time depth index out of range, cannot be greater than', &
                                                 all_model_handles(id)%integrator%nf_depths(i_nf), 'and is', i_when
      call faterr(IAM,cout)
    end if

    nb_nodes = all_model_handles(id)%state%nb_nodes
    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, i_nf, i_when)
    i_size   = i2 - i1 + 1

    if( associated(get_nodal_field_ptr) ) then
      deallocate(get_nodal_field_ptr)
      nullify(get_nodal_field_ptr)
    end if

    get_nodal_field_ptr => all_model_handles(id)%state%nodal_fields(i1:i2)

  end function

  !> \brief Set a nodal field of a body
  subroutine set_nodal_field(id, i_nf, i_when, nodal_field)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] id of the nodal field to set
    integer(kind=4), intent(in) :: i_nf
    !> [in] time depths index (1: current, 2: previous...)
    integer(kind=4), intent(in) :: i_when
    !> [in] new nodal field values
    real(kind=8), dimension(:)  :: nodal_field
    !
    integer(kind=4)   :: i1, i2, i_size, nf_size
    character(len=80) :: cout
    character(len=30) :: IAM
    !      123456789012345678901234567890
    IAM = 'model_handler::get_nodal_field'

    ! paranoid check :
    call paranoid_check(id, IAM)

    if( i_when > all_model_handles(id)%integrator%nf_depths(i_nf) ) then
      write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id, &
                                                 ', time depth index out of range, cannot be greater than', &
                                                 all_model_handles(id)%integrator%nf_depths(i_nf), 'and is', i_when
      call faterr(IAM,cout)
    end if

    call get_nodal_field_indices_boundary(i1, i2, all_model_handles(id)%state, i_nf, i_when)
    i_size  = i2 - i1 + 1
    nf_size = size(nodal_field)

    if( nf_size /= i_size ) then
      write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id,', input vector of wrong size, is:', &
                                                 nf_size, 'and should be:', i_size
      call faterr(IAM,cout)
    end if

    all_model_handles(id)%state%nodal_fields(i1:i2) = nodal_field(1:nf_size)

  end subroutine

  !> \brief Get the number of elementary rhs contribution stored by a model_handle
  function get_nb_elementary_rhs_contrib(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [return] the number of elementary rhs contribution in the model_handle
    integer(kind=4)             :: get_nb_elementary_rhs_contrib

    get_nb_elementary_rhs_contrib = 0

    if( associated(all_model_handles(id)%integrator) ) then
      get_nb_elementary_rhs_contrib = all_model_handles(id)%integrator%nb_erc
    end if

  end function

  !> \brief Get a rhs contribution on nodes of a body
  subroutine get_rhs_contribution(id, i_erc, i_when, rhs_contrib)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in) :: id
    !> [in] id of the elementary rhs contribution to get
    integer(kind=4), intent(in) :: i_erc
    !> [in] time depths index (1: current, 2: previous...)
    integer(kind=4), intent(in) :: i_when
    !> [out] rhs contribution
    real(kind=8), dimension(:,:), pointer :: rhs_contrib
    !
    integer(kind=4)   :: i1, i2, i_size, nb_nodes, dime, erc_shape(2), i_element, i, j, k
    character(len=80) :: cout
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'model_handler::get_rhs_contribution'

    ! paranoid check :
    call paranoid_check(id, IAM)

    if( i_when > all_model_handles(id)%integrator%erc_depths(i_erc) ) then
      write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id, &
                      ', time depth index out of range, cannot be greater than', &
            all_model_handles(id)%integrator%erc_depths(i_erc), 'and is', i_when
      call faterr(IAM,cout)
    end if

    ! rm * : there is a difference between the number of nodes of the much and the sum of number
    !        of nodes for each elements. But it is needed to correctly allocate the size of the
    !        output vector since the size of the field to get (that is the number of values on each
    !        nodes) is not known beforehand
    nb_nodes  = all_model_handles(id)%state%nb_nodes
    i = 0
    do j = 1, nb_nodes
      i = i + size(all_model_handles(id)%state%node2elements(j)%G_i)
    end do

    call get_el_rhs_contrib_indices_boundary(i1, i2, all_model_handles(id)%state, i_erc, i_when)
    i_size    = i2 - i1 + 1
    dime      = i_size / i
    erc_shape = (/ dime, nb_nodes /)

    if( .not. associated(rhs_contrib) ) then
      allocate(rhs_contrib(dime,nb_nodes))
    else
      if( any(shape(rhs_contrib) /= erc_shape) ) then
        write(cout,'(A,1x,I0,A,1x,I0,1x,A,1x,I0)') 'for model_handler number', id,&
                     ', output vector already allocated, but of wrong shape, is:', &
                                   shape(rhs_contrib), 'and should be:', erc_shape
        call faterr(IAM,cout)
      end if
    end if

    rhs_contrib(1:dime,1:nb_nodes) = 0.d0

    ! rm's shit to avoid to make an allocation, assemble in the vector and only then reshape
    do i_element = 1, all_model_handles(id)%state%nb_elements
      call get_element_el_rhs_contrib_indices_boundary(i1, i2, all_model_handles(id)%state, i_erc, i_element, i_when)
      do i = 1, size(all_model_handles(id)%elements(i_element)%edof2gdof)
        j = mod(all_model_handles(id)%elements(i_element)%edof2gdof(i)-1,dime) + 1
        k = (all_model_handles(id)%elements(i_element)%edof2gdof(i)-1) / dime + 1

        rhs_contrib(j,k) = rhs_contrib(j,k) + all_model_handles(id)%state%el_rhs_contribs(i1+i-1)
      end do
    end do

  end subroutine

  !> \brief Get elementary field of a body interpolated on (mesh) nodes
  subroutine get_elementary_field(id, ef_name, i_when, elementary_field)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4)  , intent(in) :: id
    !> [in] elementary field name to get
    character(len=30), intent(in) :: ef_name
    !> [in] time depths index (1: current, 2: previous...)
    integer(kind=4)  , intent(in) :: i_when
    !> [out] elementary field
    real(kind=8), dimension(:,:), pointer :: elementary_field
    !
    integer(kind=4)   :: i_ef
    character(len=80) :: cout
    character(len=35) :: IAM
    !      12345678901234567890123456789012345
    IAM = 'model_handler::get_elementary_field'

    ! paranoid check :
    call paranoid_check(id, IAM)

    ! check i_when value
    ! check allocation of elementary_field parameter
    call get_elementary_fields_on_nodes(id, all_model_handles(id)%type                         , &
                                        all_model_handles(id)%state%nb_nodes, ef_name          , &
                                        all_model_handles(id)%state%elementary_fields(:,i_when), &
                                        all_model_handles(id)%state%ef_map                     , &
                                        all_model_handles(id)%state%ef_names                   , &
                                        all_model_handles(id)%state%node2elements              , &
                                        elementary_field )
  end subroutine

  !> \brief Get the list of connectivies of a model_handle
  !> Memory allocation is done within the function thus, use:
  !> connec => get_connectivities
  function get_connectivities(id)
    implicit none
    !> [in] id of the model_handle
    integer(kind=4), intent(in)            :: id
    !> [out] list of connectivities
    integer(kind=4), dimension(:), pointer :: get_connectivities
    !
    character(len=80) :: cout
    character(len=33) :: IAM
    !      123456789012345678901234567890123
    IAM = 'model_handler::get_connectivities'

    call paranoid_check(id, IAM)

    get_connectivities => modelization_get_connectivities(id)

  end function

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none
    integer(kind=4) :: i, j

    call modelization_clean_module()

    nb_model_handles = 0

    if( allocated(all_model_handles) ) then
      do i = 1, size(all_model_handles)
        if( associated(all_model_handles(i)%state) )     call delete_state(all_model_handles(i)%state)
        if( associated(all_model_handles(i)%integrator) ) nullify(all_model_handles(i)%integrator)

        if( allocated(all_model_handles(i)%elements) ) then
          do j = 1, size(all_model_handles(i)%elements)
            if( allocated(all_model_handles(i)%elements(j)%elementary_lhs) )then
               deallocate(all_model_handles(i)%elements(j)%elementary_lhs)
            end if
            if( allocated(all_model_handles(i)%elements(j)%elementary_rhs) )then
               deallocate(all_model_handles(i)%elements(j)%elementary_rhs)
            end if
            if( allocated(all_model_handles(i)%elements(j)%edof2gdof) ) then
               deallocate(all_model_handles(i)%elements(j)%edof2gdof)
            end if
          end do
          deallocate(all_model_handles(i)%elements)
        end if

        if( allocated(all_model_handles(i)%prim_drvdofs) ) then
          do j = 1, size(all_model_handles(i)%prim_drvdofs)
            if( associated(all_model_handles(i)%prim_drvdofs(j)%time_evolution%x)  ) then
                deallocate(all_model_handles(i)%prim_drvdofs(j)%time_evolution%x)
            end if
            if( associated(all_model_handles(i)%prim_drvdofs(j)%time_evolution%fx) ) then
                deallocate(all_model_handles(i)%prim_drvdofs(j)%time_evolution%fx)
            end if
          end do
          deallocate(all_model_handles(i)%prim_drvdofs)
        end if

        if( allocated(all_model_handles(i)%dual_drvdofs) ) then
          do j = 1, size(all_model_handles(i)%dual_drvdofs)
            if( associated(all_model_handles(i)%dual_drvdofs(j)%time_evolution%x)  ) then
                deallocate(all_model_handles(i)%dual_drvdofs(j)%time_evolution%x)
            end if
            if( associated(all_model_handles(i)%dual_drvdofs(j)%time_evolution%fx) ) then
                deallocate(all_model_handles(i)%dual_drvdofs(j)%time_evolution%fx)
            end if
          end do
          deallocate(all_model_handles(i)%dual_drvdofs)
        end if
      end do

      deallocate(all_model_handles)
    end if

  end subroutine

  !> \brief Check that desired id exists in all_model_handles array
  subroutine paranoid_check(id, where)
    implicit none
    !> [in] id of the desired model_handle
    integer(kind=4),  intent(in) :: id
    !> [in] name of the function calling the check
    character(len=*), intent(in) :: where
    !
    character(len=80) :: cout

    if( .not. allocated(all_model_handles) ) then
      call faterr(where,'all_model_handles is not allocated')
    end if

    if( id < 1 .or. id > size(all_model_handles) ) then
      write (cout,'(A,1x,I0,1x,A,1x,I0)') 'Index of model_handle out of range, is', id, &
                                          'should be between 1 and', size(all_model_handles)
      call faterr(where,cout)
    end if
  end subroutine

end module model_handler
