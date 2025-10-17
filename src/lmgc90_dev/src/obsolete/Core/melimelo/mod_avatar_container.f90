
!> avatar database : management
!> \author R. Mozul
!> \date   March 2011
!>

module avatar_container

  use utilities, only : faterr

  use avatar, only : list_data    => T_avatar    , &
                     new_data     => new_avatar  , &
                     erase_data   => erase_avatar, &
                     close_data   => close_avatar, &
                     open_data    => open_avatar , &
                     display_data => display_avatar, &
                     new_avatar   , &
                     set_rank     , &
                     get_rank     , &
                     set_active   , &
                     set_type     , &
                     set_modelType, &
                     add_bulk     , &
                     add_node     , &
                     add_tact     , &
                     add_drvdof   , &
                     add_initval  , &
                     add_coor_to_node_of_avatar , &
                     get_nb_tacts               , &
                     set_frame_of_bulk_of_avatar, &
                     push_avatar_in_mdl_hdl     , &
                     push_avatar_initvals_in_mdl_hdl

  implicit none

  private

  include 'pointer_container_type.f90'

  public add_avatar_to_container  , &
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
         display_avatar_container             , &
         push_container_in_mdl_hdl            , &
         push_container_initvals_in_mdl_hdl   , &
         get_nb_tacts_in_container

  contains

  include 'pointer_container_methods.f90'

  !> \brief add an new avatar to avatar container
  subroutine add_avatar_to_container(avatars, rank, type, modelType)
    implicit none
    !> [in,out] avatar container
    type(ptr_container),  intent(inout) :: avatars
    !> [in] rank of the new avatar
    integer(kind=4),      intent(in)    :: rank
    !> [in] type of avatar (RBDY2, RBDY3, MAILx...)
    character(len=5),     intent(in)    :: type
    !> [in] type of model (MECAx, THERx...)
    character(len=5),     intent(in)    :: modelType
    !
    type(list_data), pointer :: avatar
   
    avatar => null()

    if( avatars%open ) then
      avatar => new_avatar()
      call set_rank(avatar, rank)
      call set_type(avatar, type)
      call set_modelType(avatar, modelType)

      ! last avatar added becomes active
      if( get_nb_data(avatars) > 0 ) call set_active(avatars%root%data, .false.)
      call set_active(avatar, .true.)

      call add_to_ptr_container(avatars, avatar)
    else
      call faterr('[avatar_container::add_avatar_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a new bulk to last added avatar of container
  subroutine add_bulk_to_container(avatars, b_rank, c5, i4, r8)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] rank of the new bulk
    integer(kind=4),     intent(in)    :: b_rank
    !> [in] string data of the bulk
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the bulk
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the bulk
    real(kind=8),     dimension(:), pointer :: r8

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_bulk_to_container]','no avatar in the container')
      end if
      call add_bulk(avatars%root%data, b_rank, c5, i4, r8)
    else
      call faterr('[avatar_container::add_bulk_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a new node to last added avatar of container
  subroutine add_node_to_container(avatars, n_rank, c5, r8)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] rank of the new node
    integer(kind=4),     intent(in)    :: n_rank
    !> [in] string data of the node
    character(len=5), dimension(:), pointer :: c5
    !> [in] real data of the node
    real(kind=8),     dimension(:), pointer :: r8

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_node_to_container]','no avatar in the container')
      end if
      call add_node(avatars%root%data, n_rank, c5, r8)
    else
      call faterr('[avatar_container::add_node_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a new tact to last added avatar of container
  subroutine add_tact_to_container(avatars, t_rank, c5, i4, r8)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] rank of the new tact
    integer(kind=4),     intent(in)    :: t_rank
    !> [in] string data of the tact
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the tact
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the tact
    real(kind=8),     dimension(:), pointer :: r8

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_tact_to_container]','no avatar in the container')
      end if
      call add_tact(avatars%root%data, t_rank, c5, i4, r8)
    else
      call faterr('[avatar_container::add_tact_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a new driven dof to last added avatar of container
  subroutine add_drvdof_to_container(avatars, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
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

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_drvdof_to_container]','no avatar in the container')
      end if
      call add_drvdof(avatars%root%data, d_rank, c5, i4, r8, cx)
    else
      call faterr('[avatar_container::add_drvdof_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a new driven dof to last added avatar of container
  subroutine add_drvdof_to_desired_container(avatars, a_rank, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
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
    type(list_data), pointer :: avatar

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_drvdof_to_desired_container]','no avatar in the container')
      end if
      avatar => get_data(avatars, a_rank)
      call add_drvdof(avatar, d_rank, c5, i4, r8, cx)
    else
      call faterr('[avatar_container::add_drvdof_to_desired_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a dof initial values to last added avatar of container
  subroutine add_initval_to_container(avatars, i_rank, i4, r8)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] rank of the new driven dof
    integer(kind=4),     intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_initval_to_container]','no avatar in the container')
      end if
      call add_initval(avatars%root%data, i_rank, i4, r8)
    else
      call faterr('[avatar_container::add_initval_to_container]','avatar container close')
    end if

  end subroutine

  !> \brief add a dof initial values to desired avatar of container
  subroutine add_initval_to_desired_container(avatars, a_rank, i_rank, i4, r8)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] avatar rank
    integer(kind=4),     intent(in)    :: a_rank
    !> [in] rank of the new driven dof
    integer(kind=4),     intent(in)    :: i_rank
    !> [in] integer data of the driven dof
    integer(kind=4),    dimension(:), pointer :: i4
    !> [in] real data of the driven dof
    real(kind=8),       dimension(:), pointer :: r8
    !
    type(list_data), pointer :: avatar

    if( avatars%open ) then
      if( get_nb_data(avatars) == 0 ) then
        call faterr('[avatar_container::add_initval_to_desired_container]','no avatar in the container')
      end if
      avatar => get_data(avatars, a_rank)
      call add_initval(avatar, i_rank, i4, r8)
    else
      call faterr('[avatar_container::add_initval_to_desired_container]','avatar container close')
    end if

  end subroutine

  !> \brief add coor vector to current values of a node
  subroutine add_coor_to_node_of_avatar_container(avatars, a_rank, n_rank, coor)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout)  :: avatars
    !> [in] rank of the avatar
    integer(kind=4),     intent(in)     :: a_rank
    !> [in] rank of the node
    integer(kind=4),     intent(in)     :: n_rank
    !> [in] coordinates to add to current
    real(kind=8), dimension(:), pointer :: coor
    !
    type(list_data), pointer :: avatar
 
    avatar => get_data(avatars, a_rank)
    call add_coor_to_node_of_avatar(avatar, n_rank, coor)
    
  end subroutine

  !> \brief set frame of bulk
  subroutine set_frame_of_bulk_of_avatar_container(avatars, a_rank, b_rank, frame)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout)    :: avatars
    !> [in] rank of the avata
    integer(kind=4),     intent(in)       :: a_rank
    !> [in] rank of the bulk
    integer(kind=4),     intent(in)       :: b_rank
    !> [in] new frame
    real(kind=8), dimension(:,:), pointer :: frame
    !
    type(list_data), pointer :: avatar
 
    avatar => get_data(avatars, a_rank)
    call set_frame_of_bulk_of_avatar(avatar, b_rank, frame)
    
  end subroutine

  !> \brief set the type of model of an avatar
  subroutine set_model_type_of_avatar_in_container(avatars, a_rank, model)
    implicit none
    !> [in,out] avatar container
    type(ptr_container), intent(inout) :: avatars
    !> [in] rank of the avatar
    integer(kind=4),     intent(in)    :: a_rank
    !> [in] new model type
    character(len=5),    intent(in)    :: model
    !
    type(list_data), pointer :: avatar
 
    avatar => get_data(avatars, a_rank)
    call set_modelType(avatar, model)
    
  end subroutine

  !> \brief display an avatar container
  subroutine display_avatar_container(avatars, ifich)
    implicit none
    !> [in] avatar container
    type(ptr_container), intent(in) :: avatars
    !> [in] (optional) unit number in which to write
    integer(kind=4), optional        :: ifich
    !
    integer(kind=4) :: i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '    number of avatars: ', get_nb_data(avatars)
    call display_ptr_container(avatars, ifich)

  end subroutine

  !> \brief push every avatars  of a container in model_handler
  !> entity container must be close, no exception caught
  subroutine push_container_in_mdl_hdl(avatars, i_model, i_tact)
    implicit none
    !> [in] close avatar container to push
    type(ptr_container), intent(in)    :: avatars
    !> [in,out] index of last added model
    integer(kind=4),     intent(inout) :: i_model
    !> [in,out] index of last added tact
    integer(kind=4),     intent(inout) :: i_tact
    !
    integer(kind=4) :: i_avatar

    do i_avatar = 1, get_nb_data(avatars)
      i_model = i_model + 1
      call push_avatar_in_mdl_hdl(avatars%data_array(i_avatar)%data, i_model, i_tact)
    end do

  end subroutine

  !> \brief push initial values of every avatars  of a container in model_handler
  !> entity container must be close, no exception caught
  subroutine push_container_initvals_in_mdl_hdl(avatars, i_model)
    implicit none
    !> [in] close avatar container to push
     type(ptr_container), intent(in)    :: avatars
    !> [in,out] index of last used model
    integer(kind=4),     intent(inout) :: i_model
    !
    integer(kind=4) :: i_avatar

    do i_avatar = 1, get_nb_data(avatars)
      i_model = i_model + 1
      call push_avatar_initvals_in_mdl_hdl(avatars%data_array(i_avatar)%data, i_model)
    end do

  end subroutine

  !> \brief get the number of tacts in an avatar container
  !> looks much like get_nb_data of linkedlist.f90
  function get_nb_tacts_in_container(avatars)
    implicit none
    !> [in] avatar container
    type(ptr_container), intent(in) :: avatars
    !> [return] total number of tacts
    integer(kind=4)                 :: get_nb_tacts_in_container
    !
    type(linked_list), pointer :: current
    integer(kind=4)            :: i_avatar

    current => null()

    get_nb_tacts_in_container = 0

    if( get_nb_data(avatars) == 0 ) return

    if( avatars%open ) then
      current => avatars%root
      do while( associated(current) )
        get_nb_tacts_in_container = get_nb_tacts_in_container + get_nb_tacts(current%data)
        current => current%next
      end do
    else
      do i_avatar = 1, get_nb_data(avatars)
        get_nb_tacts_in_container = get_nb_tacts_in_container + get_nb_tacts(avatars%data_array(i_avatar)%data)
      end do
    end if

  end function

end module

