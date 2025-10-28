! Warning: comments must respect doxygen formats

!> LMGC90 bulk object managing
!> \author F. Dubois
!> \date   January 2009

module bulk

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

  use visitor, only : start_bulk_visitor, &
                      stop_bulk_visitor

  use model_handler, only : add_bulk_to_model

  implicit none

  private

  !> Smart bulk type \n
  !> RBDY2 : \n
  !> i4_vector=NULL(), r8_vector= (/ avrd,gyrd,a1(1:2),a2(1:2) /)  \n
  !> RBDY3 : \n
  !> i4_vector=NULL(), r8_vector= (/ avrd,I1,I2,I3,a1(1:3),a2(1:3),a3(1:3) /) \n 
  !> S2xxx,T3xxx,Q4xxx,Q8xxx,H8xxx,H20xx,TE4xx,TE10x : \n
  !> i4_vector=connec(), r8_vector= NULL() \n

  public visit_bulk_container, &
         set_frame_of_bulk   , &
         push_bulk_container_in_mdl_hdl

contains

  !> \brief subroutine to visit a bulk container
  !>  Only the current object if the container is open.
  !>  The array if the container is closed.
  subroutine visit_bulk_container(bulks)
    implicit none
    !> [in] bulk container to visit
    type(T_object_container), intent(in) :: bulks
    ! 
    type(T_object)   :: bulk
    integer(kind=4)  :: rank, i, j, k
    integer(kind=4)  :: list1(1),list2(2),list3(3),list4(4)
    character(len=5) :: type, behav, model
    character(len=5), dimension(:), pointer :: c5_vector
    real(kind=8)    , dimension(:), pointer :: r8_vector
    integer(kind=4) , dimension(:), pointer :: i4_vector

    !print*,'bulk'

    if( get_nb_objects(bulks) == 0 ) then
      call logmes('[bulk:visit_bulk_container] : No bulk yet')
      return
    endif

    if( get_status(bulks) ) then

      bulk = get_object(bulks)

      rank = get_rank(bulk)
      c5_vector => get_c5_vector(bulk)
      r8_vector => get_r8_vector(bulk)
      i4_vector => get_i4_vector(bulk)

      type=c5_vector(1); model=c5_vector(2); behav=c5_vector(3)

      call start_bulk_visitor(0,rank,type,model,behav, &  
                                   i4_vector=i4_vector, &
                                   r8_vector=r8_vector)

      call stop_bulk_visitor(0)

    else

      do i = 1, get_nb_objects(bulks)

        bulk = get_object(bulks,i)

        rank = get_rank(bulk)
        c5_vector => get_c5_vector(bulk)
        r8_vector => get_r8_vector(bulk)
        i4_vector => get_i4_vector(bulk)

        type=c5_vector(1); model=c5_vector(2);  behav=c5_vector(3)
  
        call start_bulk_visitor(1,rank,type,model,behav, &  
                                   i4_vector=i4_vector, &
                                   r8_vector=r8_vector)

        call stop_bulk_visitor(1)

      enddo

    endif

  end subroutine

  !> \brief set the frame of bulk if it is rigid
  subroutine set_frame_of_bulk(bulks, b_rank, frame)
    implicit none
    !> [in,out] bulk container
    type(T_object_container)    , intent(inout) :: bulks
    !> [in] rank of the bulk
    integer(kind=4)             , intent(in)    :: b_rank
    !> [in] new frame
    real(kind=8), dimension(:,:), pointer       :: frame
    !
    type(T_object), pointer :: bulk
    real(kind=8), dimension(:), pointer :: r8
 
    bulk => get_object(bulks, b_rank)
    r8   => get_r8_vector(bulk)

    if( .not. associated(r8) ) then
      call faterr('[bulk::set_frame_of_bulk]','Cannot set frame of the bulk (it is probably not rigid)')
    end if

    if( size(frame) == 4 ) then
      r8(3:6)  = reshape(frame, shape=(/4/))
    else if( size(frame) == 9 ) then
      r8(5:13) = reshape(frame, shape=(/9/))
    else
      call faterr('[bulk::set_frame_of_bulk]','Wrong size of input frame')
    end if
    
  end subroutine

  !> \brief push a bulk container in model_handler
  !> container must be close, no exeception caught if not
  subroutine push_bulk_container_in_mdl_hdl(bulks, i_model)
    implicit none
    !> [in] bulk container to push in model_handler
    type(T_object_container), intent(in) :: bulks
    !> [in] index of array in which to add the node
    integer(kind=4),          intent(in) :: i_model
    !
    type(T_object)  :: object
    integer(kind=4) :: i_bulk
    character(len=5), dimension(:), pointer :: c5_vector
    integer(kind=4) , dimension(:), pointer :: i4_vector
    real(kind=8)    , dimension(:), pointer :: r8_vector

    c5_vector => null()
    i4_vector => null()
    r8_vector => null()

    do i_bulk = 1, get_nb_objects(bulks)

      object = get_object(bulks, i_bulk)

      c5_vector => get_c5_vector(object)
      i4_vector => get_i4_vector(object)
      r8_vector => get_r8_vector(object)

      call add_bulk_to_model(i_model, i_bulk, c5_vector, i4_vector, r8_vector)

    end do

  end subroutine

end module
