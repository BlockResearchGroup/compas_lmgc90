
!> gives one represention of an entity
module avatar

  use anonymous_ptr_container, only : T_object_container => PTR_CONTAINER  , &
                                      erase_ptr_container, &
                                      close_ptr_container, &
                                      open_ptr_container , &
                                      display_object_ptr_container, &
                                      add_object_to_ptr_container, &
                                      get_nb_object => get_nb_data

  use bulk,    only : set_frame_of_bulk, &
                      push_bulk_container_in_mdl_hdl

  use node,    only : add_coor_to_node, &
                      push_node_container_in_mdl_hdl

  use drvdof,  only : get_nb_primal_in_drvdof_container, &
                      get_nb_dual_in_drvdof_container  , &
                      push_drvdof_container_in_mdl_hdl

  use initval, only : push_initval_container_in_mdl_hdl

  use tact,    only : push_tact_container_in_contactor


  use model_handler, only : add_model

  implicit none

  private

  !> Smart avatar type.
  !> Embeds all derived types.
  TYPE, public :: T_avatar
     private
     !**
     !> rank
     integer(kind=4) :: rank
     !> is active
     logical         :: active
     !**
     !> RBDY2, RBDY3, MAILx, CNEMx
     character(len=5) :: type
     !> MECAx, THERx
     character(len=5) :: modelType
     !**
     type(T_object_container) :: bulks
     type(T_object_container) :: nodes
     type(T_object_container) :: tacts
     type(T_object_container) :: drvdofs
     type(T_object_container) :: initvals
  end type T_avatar

  ! routines publiques
  public new_avatar    , &
         erase_avatar  , &
         close_avatar  , &
         open_avatar   , &
         set_rank      , &
         get_rank      , &
         set_active    , &
         set_type      , &
         set_modelType , &
         add_bulk      , &
         add_node      , &
         add_tact      , &
         add_drvdof    , &
         add_initval   , &
         add_coor_to_node_of_avatar , &
         get_nb_tacts               , &
         set_frame_of_bulk_of_avatar, &
         display_avatar             , &
         push_avatar_in_mdl_hdl     , &
         push_avatar_initvals_in_mdl_hdl

  contains

  !> \brief avatar constructor \n 
  !> default or copy 
  function new_avatar(other_avatar)
    implicit none
    !> [in] (optional) avatar to copy from
    type(T_avatar), optional, intent(in) :: other_avatar
    !> [return] new avatar
    type(T_avatar), pointer              :: new_avatar
    
    allocate(new_avatar)
    if (present(other_avatar)) then
      ! what does it do on pointers ?
      ! an association
      new_avatar = other_avatar
      ! should do something like
      ! bulks = new_bulks(other_avatar%bulks)
    else
      new_avatar%rank = 0
    endif
  end function

  !> \brief erase an object
  subroutine erase_avatar(avatar)
    implicit none
    !> [in,out] erase content of an avatar
    type(T_avatar), intent(inout) :: avatar

    avatar%rank      = 0
    avatar%active    = .false.
    avatar%type      = 'none_'
    avatar%modelType = 'none_'

    call erase_ptr_container(avatar%bulks)
    call erase_ptr_container(avatar%nodes)
    call erase_ptr_container(avatar%tacts)
    call erase_ptr_container(avatar%drvdofs)
    call erase_ptr_container(avatar%initvals)

  end subroutine
  
  !> \brief close an avatar
  subroutine close_avatar(avatar)
    implicit none
    !> [in,out] avatar open in input, close on output
    type(T_avatar), intent(inout) :: avatar

    call close_ptr_container(avatar%bulks)
    call close_ptr_container(avatar%nodes)
    call close_ptr_container(avatar%tacts)
    call close_ptr_container(avatar%drvdofs)
    call close_ptr_container(avatar%initvals)

  end subroutine

  !> \brief open an avatar
  subroutine open_avatar(avatar)
    implicit none
    !> [in,out] avatar close in input, open on output
    type(T_avatar), intent(inout) :: avatar

    call open_ptr_container(avatar%bulks)
    call open_ptr_container(avatar%nodes)
    call open_ptr_container(avatar%tacts)
    call open_ptr_container(avatar%drvdofs)
    call open_ptr_container(avatar%initvals)

  end subroutine

  ! ***

  !> \brief set rank of an avatar
  subroutine set_rank(avatar,rank)
    implicit none
    !> [in, out] avatar
    type(T_avatar),  intent(inout) :: avatar
    !> [in] new rank
    integer(kind=4), intent(in)    :: rank

    avatar%rank = rank

  end subroutine

  !> \brief get rank of an avatar
  function get_rank(avatar)
    implicit none
    !> [in] avatar
    type(T_avatar), intent(in) :: avatar
    !> [return] rank of avatar
    integer(kind=4)            :: get_rank

    get_rank = avatar%rank

  end function

  !> \brief set active field of an avatar
  subroutine set_active(avatar, active)
    implicit none
    !> [in,out] avatar
    type(T_avatar), intent(inout) :: avatar
    !> [in] is active
    logical,        intent(in)    :: active

    avatar%active = active

  end subroutine

  !> \brief set type of an avatar (RBDY2, RBDY3, MAILx...)
  subroutine set_type(avatar, type)
    implicit none
    !> [in,out] avatar
    type(T_avatar),   intent(inout) :: avatar
    !> [in] new type
    character(len=5), intent(in)    :: type

    avatar%type = type

  end subroutine

  !> \brief set modelType of an avatar (MECAx, THERX...)
  subroutine set_modelType(avatar, modelType)
    implicit none
    !> [in,out] avatar
    type(T_avatar),   intent(inout) :: avatar
    !> [in] new type of model
    character(len=5), intent(in)    :: modelType

    avatar%modelType = modelType

  end subroutine

  ! ***

  !> \brief add a new bulk to an avatar
  subroutine add_bulk(avatar, b_rank, c5, i4, r8)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout) :: avatar
    !> [in] rank of the new bulk
    integer(kind=4), intent(in)    :: b_rank
    !> [in] string data of the bulk
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the bulk
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the bulk
    real(kind=8),     dimension(:), pointer :: r8
    !
    character(len=128), dimension(:), pointer :: cx

    cx => null()

    call add_object_to_ptr_container(avatar%bulks, b_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add a new node to an avatar
  subroutine add_node(avatar, n_rank, c5, r8)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout) :: avatar
    !> [in] rank of the new node
    integer(kind=4), intent(in)    :: n_rank
    !> [in] strng data of the node
    character(len=5), dimension(:), pointer :: c5
    !> [in] real data of the node
    real(kind=8),     dimension(:), pointer :: r8
    !
    integer(kind=4), dimension(:), pointer :: i4
    character(len=128), dimension(:), pointer :: cx

    i4 => null()
    cx => null()

    call add_object_to_ptr_container(avatar%nodes, n_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add a new tact to an avatar
  subroutine add_tact(avatar, t_rank, c5, i4, r8)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout) :: avatar
    !> [in] rank of the new tact
    integer(kind=4), intent(in)    :: t_rank
    !> [in] string data of the tact
    character(len=5), dimension(:), pointer :: c5
    !> [in] integer data of the tact
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the tact
    real(kind=8),     dimension(:), pointer :: r8
    !
    character(len=128), dimension(:), pointer :: cx

    cx => null()

    call add_object_to_ptr_container(avatar%tacts, t_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add a new driven dof to an avatar
  subroutine add_drvdof(avatar, d_rank, c5, i4, r8, cx)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout) :: avatar
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

    call add_object_to_ptr_container(avatar%drvdofs, d_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add initial values to an avatar
  subroutine add_initval(avatar, i_rank, i4, r8)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout) :: avatar
    !> [in] rank of the new tact
    integer(kind=4), intent(in)    :: i_rank
    !> [in] integer data of the tact
    integer(kind=4),  dimension(:), pointer :: i4
    !> [in] real data of the tact
    real(kind=8),     dimension(:), pointer :: r8
    !
    character(len=5)  , dimension(:), pointer :: c5
    character(len=128), dimension(:), pointer :: cx

    c5 => null()
    cx => null()

    call add_object_to_ptr_container(avatar%initvals, i_rank, c5, i4, r8, cx)

  end subroutine

  !> \brief add coor vector to current values of a node
  subroutine add_coor_to_node_of_avatar(avatar, n_rank, coor)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout)      :: avatar
    !> [in] rank of the node
    integer(kind=4), intent(in)         :: n_rank
    !> [in] coordinates to add to current
    real(kind=8), dimension(:), pointer :: coor
 
    call add_coor_to_node(avatar%nodes, n_rank, coor)
    
  end subroutine

  !> \brief get the number of tact in an avatar
  function get_nb_tacts(avatar)
    implicit none
    !> [in] avatar
    type(T_avatar), intent(in) :: avatar
    !> [return] number of tacts in the avatar
    integer(kind=4)            :: get_nb_tacts
 
    get_nb_tacts = get_nb_object(avatar%tacts)
    
  end function

  !> \brief set frame of a bulk
  subroutine set_frame_of_bulk_of_avatar(avatar, b_rank, frame)
    implicit none
    !> [in,out] avatar
    type(T_avatar),  intent(inout)        :: avatar
    !> [in] rank of the bulk
    integer(kind=4), intent(in)           :: b_rank
    !> [in] new frame
    real(kind=8), dimension(:,:), pointer :: frame
 
    call set_frame_of_bulk(avatar%bulks, b_rank, frame)
    
  end subroutine

  ! ***

  !> \brief display avatar
  subroutine display_avatar(avatar, ifich)
    implicit none
    !> [in] avatar to display
    type(T_avatar), intent(in) :: avatar
    !> [in] (optional) unit number in which to write
    integer(kind=4), optional  :: ifich
    !
    integer(kind=4) :: i, i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) '        avatar of rank  : ', avatar%rank
    write(i_unit,*) '            is active   : ', avatar%active
    write(i_unit,*) '            of type     : ', avatar%type
    write(i_unit,*) '            of modelType: ', avatar%modelType
    write(i_unit,*) '            displaying bulks : '
    call display_object_ptr_container(avatar%bulks)
    write(i_unit,*) '            displaying nodes : '
    call display_object_ptr_container(avatar%nodes)
    write(i_unit,*) '            displaying tacts : '
    call display_object_ptr_container(avatar%tacts)
    write(i_unit,*) '            displaying drvdofs : '
    call display_object_ptr_container(avatar%drvdofs)
    write(i_unit,*) '            displaying initvals: '
    call display_object_ptr_container(avatar%initvals)

  end subroutine

  !> \brief push an avatar in model_handler
  subroutine push_avatar_in_mdl_hdl(avatar, i_model, i_tact)
    implicit none
    !> [in] avatar to push in model_handler
    type(T_avatar),  intent(in) :: avatar
    !> [in] index of array in which to add the avatar
    integer(kind=4), intent(in) :: i_model
    !> [in,out] index of last added tact
    integer(kind=4), intent(inout) :: i_tact
    !
    integer(kind=4) :: nb_nodes, nb_bulks, nb_prims, nb_duals

    nb_bulks = get_nb_object(avatar%bulks)
    nb_nodes = get_nb_object(avatar%nodes)
    nb_prims = get_nb_primal_in_drvdof_container(avatar%drvdofs)
    nb_duals = get_nb_dual_in_drvdof_container(avatar%drvdofs)

    call add_model(i_model, avatar%modelType, avatar%type, nb_nodes, nb_bulks, nb_prims, nb_duals)

    ! add the bulks
    call push_bulk_container_in_mdl_hdl(avatar%bulks, i_model)
    ! add the nodes
    call push_node_container_in_mdl_hdl(avatar%nodes, i_model)
    ! add the dvdofs
    call push_drvdof_container_in_mdl_hdl(avatar%drvdofs, i_model)

    ! add the tacts 
    call push_tact_container_in_contactor(avatar%tacts, i_model, i_tact)

  end subroutine

  !> \brief push initial values of an avatar in model_handler
  subroutine push_avatar_initvals_in_mdl_hdl(avatar, i_model)
    implicit none
    !> [in] avatar to push in model_handler
    type(T_avatar),  intent(in) :: avatar
    !> [in] index of array in which to add the avatar
    integer(kind=4), intent(in) :: i_model

    call push_initval_container_in_mdl_hdl(avatar%initvals, i_model)

  end subroutine

end module
