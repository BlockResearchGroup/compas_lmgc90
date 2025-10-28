! Warning: comments must respect doxygen formats

!> LMGC90 tact object managing
!> \author F. Dubois
!> \date   January 2009

module tact

  use utilities, only : logmes

  use anonymous, only : T_object     , &
                        get_rank     , &
                        get_c5_vector, &
                        get_i4_vector, &
                        get_r8_vector

  use anonymous_ptr_container, only : T_object_container => PTR_CONTAINER, &
                                      get_nb_objects => get_nb_data, &
                                      get_object     => get_data   , &
                                      get_status

  use visitor, only : start_tact_visitor, &
                      stop_tact_visitor

  use contactor, only : add_contactor

  implicit none

  private

  !> anonymous tact type stored in object \n
  !> \n
  !> DISKx,xKSID,DISPx,xPSID :  \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ radius /) \n
  !> DISKb : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ radius, coor(1:2) /) \n
  !> JONCx : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ ax1, ax2 /) \n
  !> POLYG : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ nb_vertex /), r8_vector=(/ coor(1:2)*nb_vertex /) \n 
  !> PT2Dx :  \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ coor(1:2) /) \n
  !> CLxxx : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ id_a,id_b  /), r8_vector=(/ apab /) \n
  !> ALpxx : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ nb_al,(id_a,id_b)*nb_al  /), r8_vector=NULL() \n
  !> ASpx3 : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ nb_al,(id_a,id_b,id_c)*nb_al  /), r8_vector=NULL() \n
  !> ASpx4 : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ nb_al,(id_a,id_b,id_c,id_d)*nb_al  /), r8_vector=NULL() \n
  !> CSxx3 : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ id_a,id_b,id_c /), r8_vector=(/ w1,w2,w3 /) \n
  !> CSxx4 : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ id_a,id_b,id_c,id_d /), r8_vector=(/ w1,w2,w3,w4 /) \n
  !> DISKL : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ id_a,id_b /), r8_vector=(/ apab,bdyr,brpm /) \n
  !> PT2DL : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ id_a,id_b /), r8_vector=(/ apab /) \n
  !> CYLND,DNLYC : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ High,byrd /) \n
  !> PLANx : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ axe1,axe2,ax3,localFrame(1:9),shift(1:3) /) \n
  !> POLYR : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=(/ nb_vertex,nb_faces,connec(3)*nb_faces  /), r8_vector=(/ coor(1:3)*nb_vertex /) \n
  !> PT3Dx : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ coor(1:3) /) \n
  !> SPHER : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ radius /) \n
  !> SPHEb : \n
  !>   c5_vector=(/ type, color /) \n
  !>   i4_vector=NULL(), r8_vector=(/ radius,shift(1:3) /) \n
  !>

  public push_tact_container_in_contactor, &
         visit_tact_container

contains

  !> \brief subroutine to visit a tact container
  !>  Only the current object if the container is open.
  !>  The array if the container is closed.
  subroutine visit_tact_container(tacts)
    implicit none
    !> [in] tact container to visit
    type(T_object_container), intent(in) :: tacts
    ! 
    type(T_object)   :: tact
    integer(kind=4)  :: rank, i
    character(len=5) :: type,color
    character(len=5), dimension(:), pointer :: c5_vector
    integer(kind=4) , dimension(:), pointer :: i4_vector
    real(kind=8),     dimension(:), pointer :: r8_vector
    !

    !print *,'open container'

    if( get_nb_objects(tacts) == 0 ) then
      call logmes('[tact::visit_tact_container] : No tacts yet')
      return
    endif  

    if( get_status(tacts) ) then

!!         r8_vector    = pack(tacts%Current%val%polyg%coor,mask=.true.)

      tact = get_object(tacts)

      rank = get_rank(tact)
      c5_vector => get_c5_vector(tact)
      r8_vector => get_r8_vector(tact)
      i4_vector => get_i4_vector(tact)

      type=c5_vector(1); color=c5_vector(2)

      call start_tact_visitor(0,rank, &
                                type, &
                                color, &
                                r8_vector=r8_vector, &
                                i4_vector=i4_vector)
      call stop_tact_visitor(0)

    else

      do i = 1, get_nb_objects(tacts)

        tact = get_object(tacts,i)

        rank = get_rank(tact)
        c5_vector => get_c5_vector(tact)
        r8_vector => get_r8_vector(tact)
        i4_vector => get_i4_vector(tact)

        type=c5_vector(1); color=c5_vector(2)

        call start_tact_visitor(1,rank, &
                                  type, &
                                  color, &
                                  r8_vector=r8_vector, &
                                  i4_vector=i4_vector)

        call stop_tact_visitor(1)

      enddo

    endif

  end subroutine

  !> \brief push a tact container in contactor
  !> container must be close, no exeception caught if not
  subroutine push_tact_container_in_contactor(tacts, i_model, i_tact)
    implicit none
    !> [in] tact container to push in model_handler
    type(T_object_container), intent(in)    :: tacts
    !> [in] index of the model owning the contactors
    integer(kind=4),          intent(in)    :: i_model
    !> [in] beginning index of array in which to add the tact
    integer(kind=4),          intent(inout) :: i_tact
    !
    type(T_object)  :: object
    integer(kind=4) :: i_tacty
    character(len=5), dimension(:), pointer :: c5_vector
    integer(kind=4) , dimension(:), pointer :: i4_vector
    real(kind=8)    , dimension(:), pointer :: r8_vector

    c5_vector => null()
    i4_vector => null()
    r8_vector => null()

    do i_tacty = 1, get_nb_objects(tacts)

      i_tact = i_tact + 1

      object = get_object(tacts, i_tacty)

      c5_vector => get_c5_vector(object)
      i4_vector => get_i4_vector(object)
      r8_vector => get_r8_vector(object)

      call add_contactor(i_tact, i_model, c5_vector, i4_vector, r8_vector)

    end do

  end subroutine

end module
