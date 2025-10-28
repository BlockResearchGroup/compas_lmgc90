! Warning: comments must respect doxygen formats

!> LMGC90 driven dof object managing
!> \author R. Mozul (copied from mod_nodty.f90)
!> \date   January 2010


module initval

  use utilities, only : logmes, &
                        faterr

  use anonymous, only : T_object     , &
                        get_rank     , &
                        get_c5_vector, &
                        get_i4_vector, &
                        get_r8_vector, &
                        get_cx_vector

  use anonymous_ptr_container, only : T_object_container => PTR_CONTAINER, &
                                      get_nb_objects => get_nb_data, &
                                      get_object     => get_data   , &
                                      get_status

  !use visitor, only : start_tact_visitor, &
  !                    stop_tact_visitor

  use model_handler, only : set_init_val_to_model

  implicit none

  public visit_initval_container, &
         push_initval_container_in_mdl_hdl

  !> \todo : the visitor...
contains

  !> \brief subroutine to visit a initval container
  !>  Only the current object if the container is open.
  !>  The array if the container is closed.
  subroutine visit_initval_container(initvals)
    implicit none
    !> [in] initial values container to visit
    type(T_object_container), intent(in) :: initvals
    ! 
    type(T_object)  :: initval
    integer(kind=4) :: rank, i
    character(len=5),   dimension(:), pointer :: c5_vector
    integer(kind=4),    dimension(:), pointer :: i4_vector
    real(kind=8),       dimension(:), pointer :: r8_vector
    character(len=128), dimension(:), pointer :: cx_vector

    if( get_nb_objects(initvals) == 0 ) then
      call logmes('[initval::visit_initvals_container] : No driven dofs yet')
      return
    endif  

    if( get_status(initvals) ) then

      initval = get_object(initvals)

      rank = get_rank(initval)
      c5_vector => get_c5_vector(initval)
      i4_vector => get_i4_vector(initval)
      r8_vector => get_r8_vector(initval)
      cx_vector => get_cx_vector(initval)

      !call start_drvdof_visitor(0,rank,c5_vector,i4_vector,r8_vector,cx_vector)
      !call stop_drvdof_visitor(0)

    else

      do i = 1, get_nb_objects(initvals)

        initval = get_object(initvals,i)

        rank = get_rank(initval)
        c5_vector => get_c5_vector(initval)
        i4_vector => get_i4_vector(initval)
        r8_vector => get_r8_vector(initval)
        cx_vector => get_cx_vector(initval)

        !call start_drvdof_visitor(1,rank,c5_vector,i4_vector,r8_vector,cx_vector)
        !call stop_drvdof_visitor(1)

      enddo

    endif

  end subroutine 

  !> \brief push a initval container in model_handler
  !> container must be close, no exception caught if not
  subroutine push_initval_container_in_mdl_hdl(initvals, i_model)
    implicit none
    !> [in] initval container to push in model_handler
    type(T_object_container), intent(in) :: initvals
    !> [in] index of array in which to add the node
    integer(kind=4),          intent(in) :: i_model
    !
    type(T_object)  :: object
    integer(kind=4) :: i_initval
    character(len=5),   dimension(:), pointer :: c5
    integer(kind=4),    dimension(:), pointer :: i4
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    do i_initval = 1, get_nb_objects(initvals)

      object = get_object(initvals, i_initval)

      c5 => get_c5_vector(object)
      i4 => get_i4_vector(object)
      r8 => get_r8_vector(object)
      cx => get_cx_vector(object)

      call set_init_val_to_model(i_model, i_initval, i4, r8)

    end do

  end subroutine

end module
