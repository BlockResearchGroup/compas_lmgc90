! Warning: comments must respect doxygen formats

!> LMGC90 driven dof object managing
!> \author R. Mozul (copied from mod_nodty.f90)
!> \date   January 2010


module drvdof

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

  use model_handler, only : add_primal_drvdof_to_model, &
                            add_dual_drvdof_to_model

  implicit none

  private is_primal_drvdof, &
          is_dual_drvdof

  public get_nb_primal_in_drvdof_container, &
         get_nb_dual_in_drvdof_container  , &
         visit_drvdof_container           , &
         push_drvdof_container_in_mdl_hdl

  !> \todo : the visitor...
contains

  !> \brief count the number of primal driven dofs in a container
  function get_nb_primal_in_drvdof_container(drvdofs)
    implicit none
    !> [in] driven dof container
    type(T_object_container), intent(in) :: drvdofs
    !> [return] the number of primal driven dof in the container
    integer(kind=4) :: get_nb_primal_in_drvdof_container
    !
    integer(kind=4) :: i_drvdof
    type(T_object)  :: drvdof
    character(len=5), dimension(:), pointer :: c5_vector

    get_nb_primal_in_drvdof_container = 0

    do i_drvdof = 1, get_nb_objects(drvdofs)

      drvdof = get_object(drvdofs, i_drvdof)

      if( is_primal_drvdof(drvdof) ) then
          get_nb_primal_in_drvdof_container = get_nb_primal_in_drvdof_container + 1
      end if

    end do

  end function

  !> \brief count the number of dual driven dofs in a container
  function get_nb_dual_in_drvdof_container(drvdofs)
    implicit none
    !> [in] driven dof container
    type(T_object_container), intent(in) :: drvdofs
    !> [return] the number of dual driven dof in the container
    integer(kind=4) :: get_nb_dual_in_drvdof_container
    !
    integer(kind=4) :: i_drvdof
    type(T_object)  :: drvdof
    character(len=5), dimension(:), pointer :: c5_vector

    get_nb_dual_in_drvdof_container = 0

    do i_drvdof = 1, get_nb_objects(drvdofs)

      drvdof = get_object(drvdofs, i_drvdof)

      if( is_dual_drvdof(drvdof) ) then
          get_nb_dual_in_drvdof_container = get_nb_dual_in_drvdof_container + 1
      end if

    end do

  end function

  !> \brief subroutine to visit a drvdof container
  !>  Only the current object if the container is open.
  !>  The array if the container is closed.
  subroutine visit_drvdof_container(drvdofs)
    implicit none
    !> [in] drvdofs container to visit
    type(T_object_container), intent(in) :: drvdofs
    ! 
    type(T_object)  :: drvdof
    integer(kind=4) :: rank, i
    character(len=5),   dimension(:), pointer :: c5_vector
    integer(kind=4),    dimension(:), pointer :: i4_vector
    real(kind=8),       dimension(:), pointer :: r8_vector
    character(len=128), dimension(:), pointer :: cx_vector

    if( get_nb_objects(drvdofs) == 0 ) then
      call logmes('[drvdof::visit_drvdofs_container] : No driven dofs yet')
      return
    endif  

    if( get_status(drvdofs) ) then

      drvdof = get_object(drvdofs)

      rank = get_rank(drvdof)
      c5_vector => get_c5_vector(drvdof)
      i4_vector => get_i4_vector(drvdof)
      r8_vector => get_r8_vector(drvdof)
      cx_vector => get_cx_vector(drvdof)

      !call start_drvdof_visitor(0,rank,c5_vector,i4_vector,r8_vector,cx_vector)
      !call stop_drvdof_visitor(0)

    else

      do i = 1, get_nb_objects(drvdofs)

        drvdof = get_object(drvdofs,i)

        rank = get_rank(drvdof)
        c5_vector => get_c5_vector(drvdof)
        i4_vector => get_i4_vector(drvdof)
        r8_vector => get_r8_vector(drvdof)
        cx_vector => get_cx_vector(drvdof)

        !call start_drvdof_visitor(1,rank,c5_vector,i4_vector,r8_vector,cx_vector)
        !call stop_drvdof_visitor(1)

      enddo

    endif

  end subroutine 

  !> \brief push a drvdof container in model_handler
  !> container must be close, no exception caught if not
  subroutine push_drvdof_container_in_mdl_hdl(drvdofs, i_model)
    implicit none
    !> [in] drvdof container to push in model_handler
    type(T_object_container), intent(in) :: drvdofs
    !> [in] index of array in which to add the node
    integer(kind=4),          intent(in) :: i_model
    !
    type(T_object)  :: object
    integer(kind=4) :: i_drvdof, i_prim, i_dual
    character(len=5),   dimension(:), pointer :: c5
    integer(kind=4),    dimension(:), pointer :: i4
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    i_prim = 0
    i_dual = 0

    do i_drvdof = 1, get_nb_objects(drvdofs)

      object = get_object(drvdofs, i_drvdof)

      c5 => get_c5_vector(object)
      i4 => get_i4_vector(object)
      r8 => get_r8_vector(object)
      cx => get_cx_vector(object)

      if( is_primal_drvdof(object) ) then
        i_prim = i_prim + 1
        call add_primal_drvdof_to_model(i_model, i_prim, c5, i4, r8, cx)
      else if( is_dual_drvdof(object) ) then
        i_dual = i_dual + 1
        call add_dual_drvdof_to_model(i_model, i_dual, c5, i4, r8, cx)
      else
        call faterr('drvdof::push_drvdof_container_in_mdl_hdl', &
                    'gratz... you found a boundary condition of the third type!')
      end if

    end do

  end subroutine

  !> \brief return if a driven dof is primal
  function is_primal_drvdof(drvdof)
    implicit none
    !> [in] driven dof to test
    type(T_object) :: drvdof
    !> [return] is the driven dof primal
    logical :: is_primal_drvdof
    !
    character(len=5), dimension(:), pointer :: c5_vector

      c5_vector => get_c5_vector(drvdof)

      is_primal_drvdof = (c5_vector(2) == 'vlocy') .or. (c5_vector(2) == 'temp ')

  end function

  !> \brief return if a driven dof is dual
  function is_dual_drvdof(drvdof)
    implicit none
    !> [in] driven dof to test
    type(T_object) :: drvdof
    !> [return] is the driven dof dual
    logical :: is_dual_drvdof
    !
    character(len=5), dimension(:), pointer :: c5_vector

      c5_vector => get_c5_vector(drvdof)

      is_dual_drvdof = (c5_vector(2) == 'force') .or. (c5_vector(2) == 'flux ')

  end function

end module
