
!> entity database : management
!> \author R. Mozul
!> \date   March 2011
!>

module entity_container

  use utilities, only : faterr

  use entity, only : list_data    => T_entity      , &
                     new_data     => new_entity    , &
                     erase_data   => erase_entity  , &
                     close_data   => close_entity  , &
                     open_data    => open_entity   , &
                     display_data => display_entity, &
                     new_entity    , &
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
                     push_entity_in_mdl_hdl     , &
                     push_entity_initvals_in_mdl_hdl

  implicit none

  private

  include 'pointer_container_type.f90'

  public add_entity_to_container , &
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
         get_nb_avatars_in_container          , &
         push_container_in_mdl_hdl            , &
         push_container_initvals_in_mdl_hdl   , &
         get_nb_tacts_in_container            , &
         display_entity_container

  contains

  include 'pointer_container_methods.f90'

  !> \brief add an new entity to entity container
  subroutine add_entity_to_container(entities, rank)
    implicit none
    !> [in,out] entity container
    type(ptr_container),  intent(inout) :: entities
    !> [in] rank of the new entity
    integer(kind=4),      intent(in)    :: rank
    !
    type(list_data), pointer :: entity
   
    entity => null()

    if( entities%open ) then
      entity => new_entity()
      call set_rank(entity, rank)

      call add_to_ptr_container(entities, entity)
    else
      call faterr('[entity_container::add_entity_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add an new avatar to last added entity of container
  subroutine add_avatar_to_container(entities, a_rank, type, modelType)
    implicit none
    !> [in,out] entity container
    type(ptr_container),  intent(inout) :: entities
    !> [in] rank of the new avatar
    integer(kind=4),      intent(in)    :: a_rank
    !> [in] type of avatar (RBDY2, RBDY3, MAILx...)
    character(len=5),     intent(in)    :: type
    !> [in] type of model (MECAx, THERx...)
    character(len=5),     intent(in)    :: modelType

    if( entities%open ) then
      if( get_nb_data(entities) == 0 ) then
        call faterr('[entity_container::add_avatar_to_container]','no entity in the container')
      end if
      call add_avatar(entities%root%data, a_rank, type, modelType)
    else
      call faterr('[entity_container::add_avatar_to_container]','entity container close')
      stop 1
    end if

  end subroutine

  !> \brief add a new bulk to last added avatar of container
  subroutine add_bulk_to_container(entities, b_rank, c5, i4, r8)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the new bulk
    integer(kind=4),     intent(in)    :: b_rank
    !> [in] string data of the bulk
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the bulk
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the bulk
    real(kind=8),     dimension(:), pointer :: r8

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_bulk_to_container]','no entity in the container')
      end if
      call add_bulk(entities%root%data, b_rank, c5, i4, r8)
    else
      call faterr('[entity_container::add_bulk_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add a new node to last added avatar of container
  subroutine add_node_to_container(entities, n_rank, c5, r8)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the new node
    integer(kind=4),     intent(in)    :: n_rank
    !> [in] string data of the node
    character(len=5), dimension(:), pointer :: c5
    !> [in] real data of the node
    real(kind=8),     dimension(:), pointer :: r8

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_node_to_container]','no entity in the container')
      end if
      call add_node(entities%root%data, n_rank, c5, r8)
    else
      call faterr('[entity_container::add_node_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add a new tact to last added avatar of container
  subroutine add_tact_to_container(entities, t_rank, c5, i4, r8)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the new tact
    integer(kind=4),     intent(in)    :: t_rank
    !> [in] string data of the tact
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the tact
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the tact
    real(kind=8),     dimension(:), pointer :: r8

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_tact_to_container]','no entity in the container')
      end if
      call add_tact(entities%root%data, t_rank, c5, i4, r8)
    else
      call faterr('[entity_container::add_tact_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add a new driven dof to last added avatar of container
  subroutine add_drvdof_to_container(entities, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the new driven dof
    integer(kind=4),     intent(in)    :: d_rank
    !> [in] character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] string data of the driven dof
    character(len=128), dimension(:), pointer :: cx

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_drvdof_to_container]','no entity in the container')
      end if
      call add_drvdof(entities%root%data, d_rank, c5, i4, r8, cx)
    else
      call faterr('[entity_container::add_drvdof_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add a new driven dof to desired avatar of container
  subroutine add_drvdof_to_desired_container(entities, e_rank, a_rank, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] entity rank
    integer(kind=4),     intent(in)    :: e_rank
    !> [in] avatar rank
    integer(kind=4),     intent(in)    :: a_rank
    !> [in] rank of the new driven dof
    integer(kind=4),     intent(in)    :: d_rank
    !> [in] character[5] data of the driven dof
    character(len=5),   dimension(:), pointer :: c5
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !> [in] string data of the driven dof
    character(len=128), dimension(:), pointer :: cx
    !
    type(list_data), pointer :: entity

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_drvdof_to_desired_container]','no entity in the container')
      end if
      entity => get_data(entities, e_rank)
      call add_drvdof_to_desired(entity, a_rank, d_rank, c5, i4, r8, cx)
    else
      call faterr('[entity_container::add_drvdof_to_desired_container]','entity container close')
    end if

  end subroutine

  !> \brief add new dof initial values to last added avatar of container
  subroutine add_initval_to_container(entities, i_rank, i4, r8)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the dof initial values
    integer(kind=4),     intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_initval_to_container]','no entity in the container')
      end if
      call add_initval(entities%root%data, i_rank, i4, r8)
    else
      call faterr('[entity_container::add_initval_to_container]','entity container close')
    end if

  end subroutine

  !> \brief add new dof initial values to desired avatar of container
  subroutine add_initval_to_desired_container(entities, e_rank, a_rank, i_rank, i4, r8)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] entity rank
    integer(kind=4),     intent(in)    :: e_rank
    !> [in] avatar rank
    integer(kind=4),     intent(in)    :: a_rank
    !> [in] rank of the dof initial values
    integer(kind=4),     intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !
    type(list_data), pointer :: entity

    if( entities%open ) then
      if( entities%nb_data == 0 ) then
        call faterr('[entity_container::add_initval_to_desired_container]','no entity in the container')
      end if
      entity => get_data(entities, e_rank)
      call add_initval_to_desired(entity, a_rank, i_rank, i4, r8)
    else
      call faterr('[entity_container::add_initval_to_desired_container]','entity container close')
    end if

  end subroutine

  !> \brief add coor vector to current values of a node
  subroutine add_coor_to_node_of_entity_container(entities, e_rank, a_rank, n_rank, coor)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout)  :: entities
    !> [in] rank of the entity
    integer(kind=4),     intent(in)     :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),     intent(in)     :: a_rank
    !> [in] rank of the node
    integer(kind=4),     intent(in)     :: n_rank
    !> [in] coordinates to add to current
    real(kind=8), dimension(:), pointer :: coor
    !
    type(list_data), pointer :: entity
 
    entity => get_data(entities, e_rank)
    call add_coor_to_node_of_entity(entity, a_rank, n_rank, coor)
    
  end subroutine

  !> \brief set frame of a bulk
  subroutine set_frame_of_bulk_of_entity_container(entities, e_rank, a_rank, b_rank, frame)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout)    :: entities
    !> [in] rank of the entity
    integer(kind=4),     intent(in)       :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),     intent(in)       :: a_rank
    !> [in] rank of the bulk
    integer(kind=4),     intent(in)       :: b_rank
    !> [in] new frame
    real(kind=8), dimension(:,:), pointer :: frame
    !
    type(list_data), pointer :: entity
 
    entity => get_data(entities, e_rank)
    call set_frame_of_bulk_of_entity(entity, a_rank, b_rank, frame)
    
  end subroutine

  !> \brief set the type of model of an avatar
  subroutine set_model_type_of_avatar_in_container(entities, e_rank, a_rank, model)
    implicit none
    !> [in,out] entity container
    type(ptr_container), intent(inout) :: entities
    !> [in] rank of the entity
    integer(kind=4),     intent(in)    :: e_rank
    !> [in] rank of the avatar
    integer(kind=4),     intent(in)    :: a_rank
    !> [in] new model type
    character(len=5),    intent(in)    :: model
    !
    type(list_data), pointer :: entity
 
    entity => get_data(entities, e_rank)
    call set_model_type_of_avatar(entity, a_rank, model)
    
  end subroutine

  !> \brief display an entity container
  subroutine display_entity_container(entities, ifich)
    implicit none
    !> [in] entity container
    type(ptr_container), intent(in) :: entities
    !> [in] (optional) unit number in which to write
    integer(kind=4),     optional   :: ifich
    !
    integer(kind=4) :: i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '    number of entities: ', get_nb_data(entities)
    call display_ptr_container(entities, ifich)

  end subroutine

  !> \brief get the number of avatars in an entity container
  !> looks much like get_nb_data of linkedlist.f90
  function get_nb_avatars_in_container(entities)
    implicit none
    !> [in] entity container
    type(ptr_container), intent(in) :: entities
    !> [return] total number of avatars
    integer(kind=4)                 :: get_nb_avatars_in_container
    !
    type(linked_list), pointer :: current
    integer(kind=4)            :: i_entity

    current => null()

    get_nb_avatars_in_container = 0

    if( get_nb_data(entities) == 0 ) return

    if( entities%open ) then
      current => entities%root
      do while( associated(current) )
        get_nb_avatars_in_container = get_nb_avatars_in_container + get_nb_avatars(current%data)
        current => current%next
      end do
    else
      do i_entity = 1, get_nb_data(entities)
        get_nb_avatars_in_container = get_nb_avatars_in_container + get_nb_avatars(entities%data_array(i_entity)%data)
      end do
    end if

  end function

  !> \brief push every avatars of every entities of a container in model_handler
  !> entity container must be close, no exception caught
  subroutine push_container_in_mdl_hdl(entities)
    implicit none
    !> [in] close entity container to push
    type(ptr_container), intent(in) :: entities
    !
    integer(kind=4) :: i_entity, i_model, i_tact

    i_model = 0
    i_tact  = 0

    do i_entity = 1, get_nb_data(entities)
      call push_entity_in_mdl_hdl(entities%data_array(i_entity)%data, i_model, i_tact)
    end do

  end subroutine

  !> \brief push initial values of every avatars of every entities of a container in model_handler
  !> entity container must be close, no exception caught
  subroutine push_container_initvals_in_mdl_hdl(entities)
    implicit none
    !> [in[ close entity container to push
    type(ptr_container), intent(in) :: entities
    !
    integer(kind=4) :: i_entity, i_model

    i_model = 0

    do i_entity = 1, get_nb_data(entities)
      call push_entity_initvals_in_mdl_hdl(entities%data_array(i_entity)%data, i_model)
    end do

  end subroutine

  !> \brief get the number of tacts in an entity container
  !> looks much like get_nb_data of linkedlist.f90
  function get_nb_tacts_in_container(entities)
    implicit none
    !> [in] entity container
    type(ptr_container), intent(in) :: entities
    !> [return] total number of tacts
    integer(kind=4)                 :: get_nb_tacts_in_container
    !
    type(linked_list), pointer :: current
    integer(kind=4)            :: i_entity

    current => null()

    get_nb_tacts_in_container = 0

    if( get_nb_data(entities) == 0 ) return

    if( entities%open ) then
      current => entities%root
      do while( associated(current) )
        get_nb_tacts_in_container = get_nb_tacts_in_container + get_nb_tacts(current%data)
        current => current%next
      end do
    else
      do i_entity = 1, get_nb_data(entities)
        get_nb_tacts_in_container = get_nb_tacts_in_container + get_nb_tacts(entities%data_array(i_entity)%data)
      end do
    end if

  end function

end module

