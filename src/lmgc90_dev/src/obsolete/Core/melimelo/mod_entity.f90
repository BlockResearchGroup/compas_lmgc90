
!> entity
module entity

  use avatar_container, only : T_avatar_container => PTR_CONTAINER, &
                               erase_ptr_container, &
                               close_ptr_container, &
                               open_ptr_container , &
                               display_avatar_container , &
                               add_avatar_to_container  , &
                               add_bulk_to_container    , &
                               add_node_to_container    , &
                               add_tact_to_container    , &
                               add_drvdof_to_container  , &
                               add_drvdof_to_desired_container      , &
                               add_initval_to_container             , &
                               add_initval_to_desired_container     , &
                               add_coor_to_node_of_avatar_container , &
                               set_frame_of_bulk_of_avatar_container, &
                               set_model_type_of_avatar_in_container, &
                               push_container_in_mdl_hdl            , &
                               get_nb_data                          , &
                               push_container_initvals_in_mdl_hdl   , &
                               get_nb_tacts_in_container

  implicit none

  private

  !> Smart entity type. \n
  !> Embeds all derived types.
  type, public :: T_entity
     private
     !**
     integer(kind=4) :: rank 
     type(T_avatar_container) :: avatars
     !**
     !
  END TYPE T_entity

  public new_entity    , &
         erase_entity  , &
         close_entity  , &
         open_entity   , &
         set_rank      , &
         get_rank      , &
         add_avatar    , &
         add_bulk      , &
         add_node      , &
         add_tact      , &
         add_drvdof    , &
         add_drvdof_to_desired      , &
         add_initval                , &
         add_initval_to_desired     , &
         add_coor_to_node_of_entity , &
         set_frame_of_bulk_of_entity, &
         set_model_type_of_avatar   , &
         get_nb_avatars             , &
         get_nb_tacts               , &
         display_entity             , &
         push_entity_in_mdl_hdl     , &
         push_entity_initvals_in_mdl_hdl

  contains

  !> \brief entity constructor \n 
  !> default or copy 
  function new_entity(other_entity)
    implicit none
    !> [in] (optional) entity to copy from
    type(T_entity), optional, intent(in) :: other_entity
    !> [return] new entity
    type(T_entity), pointer              :: new_entity
    
    allocate(new_entity)
    if (present(other_entity)) then
      ! what does it do on pointers ?
      ! an association
      new_entity = other_entity
      ! should do something like
      ! bulks = new_bulks(other_entity%bulks)
    else
      new_entity%rank = 0
    endif
  end function

  !> \brief erase an object
  subroutine erase_entity(entity)
    implicit none
    !> [in,out] erase content of an entity
    type(T_entity), intent(inout) :: entity

    entity%rank = 0

    call erase_ptr_container(entity%avatars)

  end subroutine
  
  !> \brief close an entity
  subroutine close_entity(entity)
    implicit none
    !> [in,out] entity open in input, close on output
    type(T_entity), intent(inout) :: entity

    call close_ptr_container(entity%avatars)

  end subroutine

  !> \brief open an entity
  subroutine open_entity(entity)
    implicit none
    !> [in,out] entity close in input, open on output
    type(T_entity), intent(inout) :: entity

    call open_ptr_container(entity%avatars)

  end subroutine

  ! ***

  !> \brief set rank of an entity
  subroutine set_rank(entity,rank)
    implicit none
    !> [in, out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] new rank
    integer(kind=4), intent(in)    :: rank

    entity%rank = rank

  end subroutine

  !> \brief get rank of an entity
  function get_rank(entity)
    implicit none
    !> [in] entity
    type(T_entity), intent(in) :: entity
    !> [return] rank of entity
    integer(kind=4)            :: get_rank

    get_rank = entity%rank

  end function

  !> \brief add a new avatar to an entity
  subroutine add_avatar(entity, a_rank, type, modelType)
    implicit none
    !> [in,out] entity
    type(T_entity),   intent(inout) :: entity
    !> [in] rank of the new avatar
    integer(kind=4),  intent(in)    :: a_rank
    !> [in] type of the new avatar
    character(len=5), intent(in)    :: type
    !> [in] type of model
    character(len=5), intent(in)    :: modelType
 
    call add_avatar_to_container(entity%avatars, a_rank, type, modelType)

  end subroutine

  !> \brief add a new bulk to last added avatar of an entity
  subroutine add_bulk(entity, b_rank, c5, i4, r8)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] rank of the new bulk
    integer(kind=4), intent(in)    :: b_rank
    !> [in] string data of the bulk
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the bulk
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the bulk
    real(kind=8),     dimension(:), pointer :: r8

    call add_bulk_to_container(entity%avatars, b_rank, c5, i4, r8)

  end subroutine

  !> \brief add a new node to last added avatar of an entity
  subroutine add_node(entity, n_rank, c5, r8)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] rank of the new node
    integer(kind=4), intent(in)    :: n_rank
    !> [in] string data of the node
    character(len=5), dimension(:), pointer :: c5
    !> [in] real data of the node
    real(kind=8),     dimension(:), pointer :: r8

    call add_node_to_container(entity%avatars, n_rank, c5, r8)

  end subroutine

  !> \brief add a new tact to last added avatar of an entity
  subroutine add_tact(entity, t_rank, c5, i4, r8)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] rank of the new tact
    integer(kind=4), intent(in)    :: t_rank
    !> [in] string data of the tact
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the tact
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the tact
    real(kind=8),     dimension(:), pointer :: r8

    call add_tact_to_container(entity%avatars, t_rank, c5, i4, r8)

  end subroutine

  !> \brief add a new driven dof to last added avatar of entity
  subroutine add_drvdof(entity, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] rank of the new driven dof
    integer(kind=4), intent(in)    :: d_rank
    !> [in] character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] string data of the driven dof
    character(len=128), dimension(:), pointer :: cx

    call add_drvdof_to_container(entity%avatars, d_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add a new driven dof to desired avatar of entity
  subroutine add_drvdof_to_desired(entity, a_rank, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] avatar rank
    integer(kind=4), intent(in)    :: a_rank
    !> [in] rank of the new driven dof
    integer(kind=4), intent(in)    :: d_rank
    !> [in] character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] string data of the driven dof
    character(len=128), dimension(:), pointer :: cx

    call add_drvdof_to_desired_container(entity%avatars, a_rank, d_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add dof initial values to last added avatar of entity
  subroutine add_initval(entity, i_rank, i4, r8)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] rank of the dof initial values
    integer(kind=4), intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8

    call add_initval_to_container(entity%avatars, i_rank, i4, r8)

  end subroutine

  !> \brief add dof initial values to desired avatar of entity
  subroutine add_initval_to_desired(entity, a_rank, i_rank, i4, r8)
    implicit none
    !> [in,out] entity
    type(T_entity),  intent(inout) :: entity
    !> [in] avatar rank
    integer(kind=4), intent(in)    :: a_rank
    !> [in] rank of the dof initial values
    integer(kind=4), intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8

    call add_initval_to_desired_container(entity%avatars, a_rank, i_rank, i4, r8)

  end subroutine

  !> \brief add coor vector to current values of a node
  subroutine add_coor_to_node_of_entity(entity, a_rank, n_rank, coor)
    implicit none
    !> [in,out] entity
    type(T_entity) , intent(inout)      :: entity
    !> [in] rank of the avatar
    integer(kind=4), intent(in)         :: a_rank
    !> [in] rank of the node
    integer(kind=4), intent(in)         :: n_rank
    !> [in] coordinates to add to current
    real(kind=8), dimension(:), pointer :: coor
 
    call add_coor_to_node_of_avatar_container(entity%avatars, a_rank, n_rank, coor)
    
  end subroutine

  !> \brief set frame of a bulk
  subroutine set_frame_of_bulk_of_entity(entity, a_rank, b_rank, frame)
    implicit none
    !> [in,out] entity
    type(T_entity) , intent(inout)        :: entity
    !> [in] rank of the avatar
    integer(kind=4), intent(in)           :: a_rank
    !> [in] rank of the bulk
    integer(kind=4), intent(in)           :: b_rank
    !> [in] new frame
    real(kind=8), dimension(:,:), pointer :: frame
 
    call set_frame_of_bulk_of_avatar_container(entity%avatars, a_rank, b_rank, frame)
    
  end subroutine

  !> \brief set the type of model of an avatar
  subroutine set_model_type_of_avatar(entity, a_rank, model)
    implicit none
    !> [in,out] entity
    type(T_entity)  , intent(inout) :: entity
    !> [in] rank of the avatar
    integer(kind=4) , intent(in)    :: a_rank
    !> [in] new model type
    character(len=5), intent(in)    :: model
 
    call set_model_type_of_avatar_in_container(entity%avatars, a_rank, model)
    
  end subroutine

  !> \brief get the number of avatars in an entity
  function get_nb_avatars(entity)
    implicit none
    !> [in] entity
    type(T_entity), intent(in) :: entity
    !> [return] number of avatars
    integer(kind=4)            :: get_nb_avatars

    get_nb_avatars = get_nb_data(entity%avatars)

  end function

  !> \brief get the number of tacts in all avatars of an entity
  function get_nb_tacts(entity)
    implicit none
    !> [in] entity
    type(T_entity), intent(in) :: entity
    !> [return] number of tacts 
    integer(kind=4)            :: get_nb_tacts

    get_nb_tacts = get_nb_tacts_in_container(entity%avatars)

  end function

  ! ***

  !> \brief display entity
  subroutine display_entity(entity, ifich)
    implicit none
    !> [in] entity to display
    type(T_entity), intent(in) :: entity
    !> [in] (optional) unit number in which to write
    integer(kind=4), optional  :: ifich
    !
    integer(kind=4) :: i, i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '    entity of rank : ', entity%rank
    call display_avatar_container(entity%avatars, ifich)

  end subroutine

  !> \brief push all avatars of an entity to model_handler
  subroutine push_entity_in_mdl_hdl(entity, i_model, i_tact)
    implicit none
    !> [in] entity to push in model_hanler
    type(T_entity),  intent(in)    :: entity
    !> [in,out] index of last added model
    integer(kind=4), intent(inout) :: i_model
    !> [in,out] index of last added contactor
    integer(kind=4), intent(inout) :: i_tact

    call push_container_in_mdl_hdl(entity%avatars, i_model, i_tact)

  end subroutine

  !> \brief push initial values of all avatars of an entity to model_handler
  subroutine push_entity_initvals_in_mdl_hdl(entity, i_model)
    implicit none
    !> [in] entity of which to push initial values
    type(T_entity), intent(in)     :: entity
    !> [in,out] index of last used model
    integer(kind=4), intent(inout) :: i_model

    call push_container_initvals_in_mdl_hdl(entity%avatars, i_model)

  end subroutine

end module
