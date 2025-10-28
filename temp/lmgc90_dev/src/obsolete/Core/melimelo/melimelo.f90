!fd todo
! **
! faire remove object
! **
! la gestion du rank est correcte si on ajoute a la fin
! si on retire un objet du conteneur ca devient n'importe quoi ...
! a voir si il faut garder
! **
! eviter l'allocation/desallocation de entities si possible
! **
! faire un modify au niveau d'un objet d'une structure closed
! **

! Warning: some of the following comments respect doxygen formats

!> LMGC90 simulation database management
!> \author F. Dubois
!> \date   January 2009
!>
!> A simulation concerns the evolution of entities in "interaction".
!> One needs to describe the entities and then to give some rules 
!> concerning interaction detection.
!> LMGC90 "entity" (i.e. object) data structure is as follows
!>  - An entity may have various representation: Rigid, deformable with FEM, deformable with NEM, etc
!>  - each representation contains a description of its:
!>      -# bulk behaviour: mathematical and physical model, material parameter
!>      -# discretization nodes 
!>      -# contactors: some 
!>      . 
!>    In a given representation two models have the same discretization (mesh for example)
!>  - entity possess method to switch between two representations


module melI_melO

  use utilities, only : faterr, &
                        logmes

  use model_handler, only : set_nb_models
  use contactor    , only : set_nb_contactors

  use entity_container, only : T_entity_container => PTR_CONTAINER, &
                               erase_ptr_container, &
                               close_ptr_container, &
                               open_ptr_container , &
                               get_status         , &
                               add_entity_to_container , &
                               add_avatar_to_container , &
                               add_bulk_to_container   , &
                               add_node_to_container   , &
                               add_tact_to_container   , &
                               add_drvdof_to_container , &
                               add_drvdof_to_desired_container      , &
                               add_initval_to_container             , &
                               add_initval_to_desired_container     , &
                               add_coor_to_node_of_entity_container , &
                               set_frame_of_bulk_of_entity_container, &
                               set_model_type_of_avatar_in_container, &
                               display_container => display_entity_container, &
                               get_nb_avatars_in_container       , &
                               push_container_in_mdl_hdl         , &
                               push_container_initvals_in_mdl_hdl, &
                               get_nb_tacts_in_container

  implicit none

  private

  type(T_entity_container), save :: entities !< the container of entities
  logical,                  save :: is_created = .false.

  public create_entity_container, &
         erase_entity_container , &
         add_entity , &
         add_avatar , &
         add_bulk   , &
         add_node   , &
         add_tact   , &
         add_drvdof , &
         add_drvdof_to_desired   , &
         add_initval             , &
         add_initval_to_desired  , &
         add_coor_to_node        , &
         set_frame_of_bulk       , &
         set_model_type_of_avatar, &
         open_entity_container   , &
         close_entity_container  , &
         display_entity_container, &
         push_entity_container_in_mdl_hdl, &
         push_initial_values_in_mdl_hdl

  contains

  !> \brief create a new empty container
  subroutine create_entity_container
    implicit none

    if (is_created) then
      call faterr('[melimelo::create_entity_container]','entity container already created')
    endif

    is_created = .true.

    !print *,'Lets go'

  end subroutine

  !> \brief erase every entities
  !> with memory deallocation
  subroutine erase_entity_container
    implicit none

    call erase_ptr_container(entities)
    is_created = .false.

  end subroutine

  !> \brief add a new entity to the entities container
  subroutine add_entity(e_rank)
    implicit none
    !> [in] rank of the entity to add
    integer(kind=4), intent(in) :: e_rank

    call add_entity_to_container(entities, e_rank)

    !print*,'ok add_entity_to_entity_container'

  end subroutine


  !> \brief add an avatar to the entities container
  subroutine add_avatar(a_rank, type, modelType)
    implicit none
    !> [in] rank of the avatar to add
    integer(kind=4),  intent(in) :: a_rank
    !> [in] type of the new avatar (RBDY2, RBDY3, MAILx)
    character(len=5), intent(in) :: type
    !> [in] type of model the new avatar (MECAx, THERx)
    character(len=5), intent(in) :: modelType

    call add_avatar_to_container(entities, a_rank, type, modelType)

  end subroutine

  !> \brief add a bulk to the entities container
  !> Anonymous input, have a look into bulk module to get
  !> more information on what data is expected
  subroutine add_bulk(b_rank, c5_vect, i4_vect, r8_vect)
    implicit none
    !> [in] rank of the bulk to add
    integer(kind=4),  intent(in) :: b_rank
    !> type, model and behav (in that order)
    character(len=5), dimension(:), pointer :: c5_vect
    !> integer data (for a meshed type bulk)
    integer(kind=4),  dimension(:), pointer :: i4_vect
    !> real data (for a rigid type bulk)
    real(kind=8),     dimension(:), pointer :: r8_vect

    call add_bulk_to_container(entities, b_rank, c5_vect, i4_vect, r8_vect)

  end subroutine

  !> \brief add a node to the entities container
  !> Anonymous input, have a look into node module to get
  !> more information on what data is expected
  subroutine add_node(n_rank, c5_vect, r8_vect)
    implicit none
    !> [in] rank of the node to add
    integer(kind=4),  intent(in) :: n_rank
    !> type of node
    character(len=5), dimension(:), pointer :: c5_vect
    !> coordinates
    real(kind=8),     dimension(:), pointer :: r8_vect

    call add_node_to_container(entities, n_rank, c5_vect, r8_vect)

  end subroutine

  !> \brief add a tact to the entities container
  !> Anonymous input, have a look into tact module to get
  !> more information on what data is expected
  subroutine add_tact(t_rank, c5_vect, i4_vect, r8_vect)
    implicit none
    !> [in] rank of the tact to add
    integer(kind=4),  intent(in) :: t_rank
    !> type and color (in that order)
    character(len=5), dimension(:), pointer :: c5_vect
    !> integer data allowing to define the contactor
    integer(kind=4),  dimension(:), pointer :: i4_vect
    !> real data allowing to define the contactor
    real(kind=8),     dimension(:), pointer :: r8_vect

    call add_tact_to_container(entities, t_rank, c5_vect, i4_vect, r8_vect)

  end subroutine

  !> \brief add a driven dof to the entities container
  !> Anonymous input, have a look into drvdof module to get
  !> more information on what data is expected
  subroutine add_drvdof(d_rank, c5_vect, i4_vect, r8_vect, cx_vect)
    implicit none
    !> [in] rank of the driven dof to add
    integer(kind=4),    intent(in) :: d_rank
    !> 5 character strings defining the driven dof
    character(len=5),   dimension(:), pointer :: c5_vect
    !> integer data defining the driven dof
    integer(kind=4),    dimension(:), pointer :: i4_vect
    !> real data defining the driven dof
    real(kind=8),       dimension(:), pointer :: r8_vect
    !> 128 character strings defining the driven dof
    character(len=128), dimension(:), pointer :: cx_vect

    call add_drvdof_to_container(entities, d_rank, c5_vect, i4_vect, r8_vect, cx_vect)

  end subroutine

  !> \brief add a driven dof to a desired avatar of entities container
  !> Anonymous input, have a look into drvdof module to get
  !> more information on what data is expected
  subroutine add_drvdof_to_desired(e_rank, a_rank, d_rank, c5_vect, i4_vect, r8_vect, cx_vect)
    implicit none
    !> [in] entity rank on which to add the driven dof
    integer(kind=4),    intent(in) :: e_rank
    !> [in] avatar rank on which to add the driven dof
    integer(kind=4),    intent(in) :: a_rank
    !> [in] rank of the driven dof to add
    integer(kind=4),    intent(in) :: d_rank
    !> 5 character strings defining the driven dof
    character(len=5),   dimension(:), pointer :: c5_vect
    !> integer data defining the driven dof
    integer(kind=4),    dimension(:), pointer :: i4_vect
    !> real data defining the driven dof
    real(kind=8),       dimension(:), pointer :: r8_vect
    !> 128 character strings defining the driven dof
    character(len=128), dimension(:), pointer :: cx_vect

    call add_drvdof_to_desired_container(entities, e_rank, a_rank, d_rank, c5_vect, i4_vect, r8_vect, cx_vect)

  end subroutine

  !> \brief add a initial value to dofs to the entities container
  !> Anonymous input, have a look into initval module to get
  !> more information on what data is expected
  subroutine add_initval(i_rank, i4_vect, r8_vect)
    implicit none
    !> [in] rank of the dof initial values to add
    integer(kind=4), intent(in)             :: i_rank
    !> integer data defining the dof initial values
    integer(kind=4), dimension(:), pointer  :: i4_vect
    !> real data defining the dof initial values
    real(kind=8)   , dimension(:), pointer  :: r8_vect

    call add_initval_to_container(entities, i_rank, i4_vect, r8_vect)

  end subroutine

  !> \brief add a initial value to a desired avatar of the entities container
  !> Anonymous input, have a look into initval module to get
  !> more information on what data is expected
  subroutine add_initval_to_desired(e_rank, a_rank, i_rank, i4_vect, r8_vect)
    implicit none
    !> [in] entity rank on which to add the initial value
    integer(kind=4), intent(in)             :: e_rank
    !> [in] avatar rank on which to add the initial value
    integer(kind=4), intent(in)             :: a_rank
    !> [in] rank of the dof initial values to add
    integer(kind=4), intent(in)             :: i_rank
    !> integer data defining the dof initial values
    integer(kind=4), dimension(:), pointer  :: i4_vect
    !> real data defining the dof initial values
    real(kind=8)   , dimension(:), pointer  :: r8_vect

    call add_initval_to_desired_container(entities, e_rank, a_rank, i_rank, i4_vect, r8_vect)

  end subroutine

  !> \brief add coor to current r8 values of a node
  subroutine add_coor_to_node(e_rank, a_rank, n_rank, coor)
    implicit none
    !> [in] rank of the entity
    integer(kind=4),  intent(in) :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),  intent(in) :: a_rank
    !> [in] rank of the node
    integer(kind=4),  intent(in) :: n_rank
    !> [in] coordinates to add
    real(kind=8),     dimension(:), pointer :: coor

    call add_coor_to_node_of_entity_container(entities, e_rank, a_rank, n_rank, coor)
    
  end subroutine

  !> \brief set frame of a bulk
  !> For rigid bulks only
  subroutine set_frame_of_bulk(e_rank, a_rank, b_rank, frame)
    implicit none
    !> [in] rank of the entity
    integer(kind=4),  intent(in) :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),  intent(in) :: a_rank
    !> [in] rank of the bulk
    integer(kind=4),  intent(in) :: b_rank
    !> [in] new frame
    real(kind=8),     dimension(:,:), pointer :: frame

    call set_frame_of_bulk_of_entity_container(entities, e_rank, a_rank, b_rank, frame)
    
  end subroutine

  !> \brief set the type of model of an avatar
  subroutine set_model_type_of_avatar(e_rank, a_rank, model)
    implicit none
    !> [in] rank of the entity
    integer(kind=4),  intent(in) :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),  intent(in) :: a_rank
    !> [in] new model type
    character(len=5), intent(in) :: model

    call set_model_type_of_avatar_in_container(entities, e_rank, a_rank, model)
    
  end subroutine

  !> \brief close entities container
  subroutine close_entity_container
    implicit none

    call close_ptr_container(entities)

  end subroutine

  !> \brief open entities container
  subroutine open_entity_container
    implicit none

    call open_ptr_container(entities)

  end subroutine

  !> \brief display the content of entities container
  !> display on standard input
  subroutine display_entity_container
    implicit none

    call display_container(entities)

  end subroutine

  ! -- for new arch -- !

  !> \brief push entities container in model_handler
  subroutine push_entity_container_in_mdl_hdl
    implicit none
    integer(kind=4) :: nb_models, nb_tacts

    if( get_status(entities) ) then
      call logmes('WARNING[melimelo::push_entity_container_in_mdl_hdl] : closing container')
      call close_entity_container
    end if

    nb_models = get_nb_avatars_in_container(entities)
    nb_tacts  = get_nb_tacts_in_container(entities)

    call set_nb_models(nb_models)
    call set_nb_contactors(nb_tacts)

    call push_container_in_mdl_hdl(entities)

  end subroutine

  !> \brief push initial values of entities in model_handler
  subroutine push_initial_values_in_mdl_hdl
    implicit none
    
    if( get_status(entities) ) then
      call logmes('WARNING[melimelo::push_initial_values_in_mdl_hdl] : closing container')
      call close_entity_container
    end if

    call push_container_initvals_in_mdl_hdl(entities)

  end subroutine

end module
