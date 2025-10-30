! Warning: comments must respect doxygen formats

!> LMGC90 node object managing
!> \author F. Dubois
!> \date   January 2009
!> type of node: NO1xx ... NO6xx


module node

  use utilities, only : logmes, faterr

  use anonymous, only : T_object     , &
                        get_rank     , &
                        get_c5_vector, &
                        get_i4_vector, &
                        get_r8_vector

  use anonymous_ptr_container, only : T_object_container => PTR_CONTAINER, &
                                      get_nb_objects => get_nb_data, &
                                      get_object     => get_data   , &
                                      get_status

  use visitor, only : start_node_visitor, &
                      stop_node_visitor

  use model_handler, only : add_node_to_model

  implicit none

  private

  public visit_node_container, &
         add_coor_to_node    , &
         push_node_container_in_mdl_hdl

contains

  !> \brief subroutine to visit a node container
  !>  Only the current object if the container is open.
  !>  The array if the container is closed.
  subroutine visit_node_container(nodes)
    implicit none
    !> [in] node container to visit
    type(T_object_container), intent(in) :: nodes
    ! 
    type(T_object)  :: node
    integer(kind=4) :: rank, i
    real(kind=8), dimension(:), pointer :: r8_vector

    if( get_nb_objects(nodes) == 0 ) then
      call logmes('[node::visit_node_container] : No nodes yet')
      return
    endif  

    if( get_status(nodes) ) then

      node = get_object(nodes)

      rank = get_rank(node)
      r8_vector => get_r8_vector(node)

      call start_node_visitor(0,rank,r8_vector)
      call stop_node_visitor(0)

    else

      do i = 1, get_nb_objects(nodes)

        node = get_object(nodes,i)

        rank = get_rank(node)
        r8_vector => get_r8_vector(node)

        call start_node_visitor(1,rank,r8_vector)
        call stop_node_visitor(1)

      enddo

    endif

  end subroutine 

  !> \brief add coordinates to current values of r8 vector of a node
  subroutine add_coor_to_node(nodes, n_rank, coor)
    implicit none
    !> [in,out] node container
    type(T_object_container)  , intent(inout) :: nodes
    !> [in] rank of the node
    integer(kind=4)           , intent(in)    :: n_rank
    !> [in] real data to add to current
    real(kind=8), dimension(:), pointer       :: coor
    !
    type(T_object), pointer :: node
    real(kind=8), dimension(:), pointer :: r8
 
    node => get_object(nodes, n_rank)
    r8   => get_r8_vector(node)

    !\todo : paranoid check
    if( size(r8) /= size(coor) ) then
      call faterr('[node::add_coor_to_node]','Non concordant size of coor with r8 vector of node')
    end if
    r8(:) = r8(:) + coor(:)
    
  end subroutine

  !> \brief push a node container in model_handler
  !> container must be close, no exception caught if not
  subroutine push_node_container_in_mdl_hdl(nodes, i_model)
    implicit none
    !> [in] node container to push in model_handler
    type(T_object_container), intent(in) :: nodes
    !> [in] index of array in which to add the node
    integer(kind=4),          intent(in) :: i_model
    !
    type(T_object)  :: object
    integer(kind=4) :: i_node, node_rank
    real(kind=8), dimension(:), pointer :: r8_vector

    r8_vector => null()

    do i_node = 1, get_nb_objects(nodes)

      object = get_object(nodes, i_node)

      r8_vector => get_r8_vector(object)
      node_rank =  get_rank(object)

      call add_node_to_model(i_model, node_rank, r8_vector)

    end do

  end subroutine

end module
