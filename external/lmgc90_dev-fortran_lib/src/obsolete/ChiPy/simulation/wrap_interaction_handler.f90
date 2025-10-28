!> LMGC90 python wrap of interaction_handler
!> \author F. Dubois
!> \date   January 2010
!>

module wrap_interaction_handler

  use ISO_C_BINDING

  use interaction_handler, only : open_this             , &
                                  close_this            , &
                                  delete_this           , &
                                  initialize            , &
                                  coor_prediction       , &
                                  run_rough_detection   , &
                                  run_fine_detections   , &
                                  stock_rloc, recup_rloc, &
                                  write_xxx_Vloc_Rloc   , &
                                  get_nb_point_outlines , &
                                  init_outlines         , &
                                  update_postdata       , &
                                  clean_module

  use contactor, only : get_nb_contactors

  implicit none

  public selectProxTactors , &
         stockRloc         , &
         recupRloc         , &
         writeLastVlocRloc , &
         initOutlines      , &
         updatePostdata    , &
         getNbPointOutlines, &
         cleanMemory

  contains

  subroutine selectProxTactors() bind(c, name='interaction_handler_selectProxTactors')
    implicit none
    logical :: is_initialized_itr_hdl = .false.

    ! cleaning/opening this
    ! todo : think of a clean and efficient way
    !        to delete the container but with consistency
    !        with stock/recup rloc functions
    call delete_this()
    !call open_this()
    
    if( get_nb_contactors() < 1 ) return

    if( .not. is_initialized_itr_hdl ) then
      call initialize
      is_initialized_itr_hdl = .true.
    endif

    call coor_prediction

    call run_rough_detection()
    call run_fine_detections()

    ! todo : other detections...

    ! close this
    call close_this()

  end subroutine
  !!!--------------------------------------------------

  subroutine stockRloc() bind(c, name='interaction_handler_stockRloc')
    implicit none

    ! must create verlet from this in interaction_handler
    call stock_rloc()

  end subroutine
  !!!--------------------------------------------------
  subroutine recupRloc() bind(c, name='interaction_handler_recupRloc')
    implicit none

    ! must initialize this from verlet in interaction_handler
    call recup_rloc()

  end subroutine
  !!!--------------------------------------------------

  subroutine writeLastVlocRloc() bind(c, name='interaction_handler_writeLastVlocRloc')
    implicit none

     ! rm: an other test ?
     !if( .not. check_DKDKx() ) return

     call write_xxx_Vloc_Rloc(2)

  end subroutine

  subroutine initOutlines(ptr, dim1, dim2) bind(c, name='contactor_initOutlines')
    implicit none
    integer(kind=c_int) :: dim1, dim2
    type(c_ptr) :: ptr
    !
    real(kind=8), dimension(:,:), pointer :: outlines

    outlines => init_outlines()

    if( associated(outlines) ) then
      ptr    = c_loc(outlines(1,1))
      dim1 = size(outlines,1)
      dim2 = size(outlines,2)
    else
      ptr = c_null_ptr
      dim1 = 0
      dim2 = 0
    end if

  end subroutine

  subroutine updatePostdata() bind(c, name='contactor_updatePostdata')
    implicit none

    call update_postdata

  end subroutine

  subroutine getNbPointOutlines(ptr, length) bind(c, name='contactor_getNbPointOutlines')
    implicit none
    type(c_ptr)    :: ptr
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: nb_point_outlines

    nb_point_outlines => get_nb_point_outlines()

    if( associated(nb_point_outlines) ) then
      ptr    = c_loc(nb_point_outlines(1))
      length = size(nb_point_outlines)
    else
      ptr = c_null_ptr
      length = 0
    end if

  end subroutine getNbPointOutlines

  subroutine cleanMemory() bind(c, name='interaction_handler_cleanMemory')
    implicit none

    call clean_module()

  end subroutine

end module
