!------------------------------------------------------------------------
  integer(kind=4) function get_ENT(itact)
    implicit none
    integer(kind=4) :: itact
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_ent = get_entity_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      get_ent = get_entity_MBS2D(tactype2bdyty(1,itact))

    endif

  end function
!------------------------------------------------------------------------
  function get_color(itact)
    implicit none
    integer(kind=4)      :: itact
    character(len=5) :: get_color

    if( tactype2bdyty(3,itact) == i_rbdy2 ) then
      get_color = get_color_RBDY2(tactype2bdyty(1,itact),tactype2bdyty(2,itact))
    else if( tactype2bdyty(3,itact) == i_mbs2 ) then
      get_color = get_color_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact))
    end if

  end function get_color
!------------------------------------------------------------------------
  function get_Xbegin(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_Xbegin
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_Xbegin = get_Xbegin_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_Xbegin = 0.d0

    endif
 
  end function
!------------------------------------------------------------------------
  function get_X(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_X
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_X = get_X_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_X = 0.d0

    endif
 
  end function
!------------------------------------------------------------------------
  function get_Vbegin(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_Vbegin
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_Vbegin = get_Vbegin_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_Vbegin = 0.d0

    endif

  end function
!------------------------------------------------------------------------
  function get_V(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_V
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_V = get_V_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_V = 0.d0

    endif
 
  end function
!------------------------------------------------------------------------
  function get_reac(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_reac
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_reac = get_reac_RBDY2(tactype2bdyty(1,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_reac = 0.d0
   
    endif
 
  end function
!------------------------------------------------------------------------
  function get_cooref(itact)
    implicit none
    integer(kind=4), intent(in) :: itact
    real(kind=8), dimension(3)  :: get_cooref
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_cooref = get_cooref_RBDY2(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      call faterr('get_cooref','not available for MBS2D') 
       
      !get_cooref = get_cooref_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact))
 
    endif
 
  end function
!------------------------------------------------------------------------
  function get_coor(itact)
    implicit none
    integer(kind=4), intent(in) :: itact
    real(kind=8), dimension(3)  :: get_coor
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_coor = get_coor_RBDY2(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      get_coor = get_coor_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact))
 
    endif
 
  end function
!------------------------------------------------------------------------
  function get_visible(itact)
    implicit none
    integer(kind=4) :: itact 
    logical :: get_visible

    if( tactype2bdyty(3,itact) == i_rbdy2 ) then
      get_visible = get_visible_RBDY2(tactype2bdyty(1,itact))
    else
      get_visible = .true.
    end if

  end function get_visible
!------------------------------------------------------------------------
  subroutine add_reac(itact,xxccdof,xxreac,storage)
    implicit none
    integer(kind=4) :: itact, storage
    integer(kind=4), dimension(3)  :: xxccdof
    real(kind=8)   , dimension(3)  :: xxreac
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then
 
      call add_reac_RBDY2(tactype2bdyty(1,itact),xxccdof,xxreac,storage)

    else if (tactype2bdyty(3,itact) == i_mbs2) then
 
      call add_reac_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact),xxccdof,xxreac,storage)

    endif
 
  end subroutine
!!!------------------------------------------------------------------------
  function get_coorTT(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(3) :: get_coorTT

    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_coorTT = get_coorTT_RBDY2(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      get_coorTT = get_coorTT_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    endif

  end function
!!!------------------------------------------------------------------------
  subroutine comp_vlocy(itact,storage)
    implicit none
    integer(kind=4) :: itact, storage

    if (tactype2bdyty(3,itact) == i_rbdy2) then

      call comp_vlocy_RBDY2(tactype2bdyty(1,itact),storage)

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      call comp_vlocy_MBS2D(tactype2bdyty(1,itact),storage)

    endif

  end subroutine
!!!------------------------------------------------------------------------
  subroutine nullify_vlocy(itact,storage)
    implicit none
    integer(kind=4) :: itact, storage

    if (tactype2bdyty(3,itact) == i_rbdy2) then

      call nullify_vlocy_RBDY2(tactype2bdyty(1,itact),storage)

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      call nullify_vlocy_MBS2D(tactype2bdyty(1,itact),storage)

    endif

  end subroutine
!!!------------------------------------------------------------------------
  subroutine nullify_reac(itact,storage)
    implicit none
    integer(kind=4) :: itact, storage

    if (tactype2bdyty(3,itact) == i_rbdy2) then

      call nullify_reac_RBDY2(tactype2bdyty(1,itact),storage)

   else if (tactype2bdyty(3,itact) == i_mbs2) then

      call nullify_reac_MBS2D(tactype2bdyty(1,itact),storage)

   endif

  end subroutine
!!!------------------------------------------------------------------------
  subroutine get_vlocy(itact,istate,vlocy)
    implicit none
    integer(kind=4) :: itact, istate
    real(kind=8), dimension(3) :: vlocy
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      call get_vlocy_RBDY2(tactype2bdyty(1,itact),istate, vlocy)

   else if (tactype2bdyty(3,itact) == i_mbs2) then

      vlocy(:) = get_vlocy_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact),istate)

   endif

  end subroutine
!!!------------------------------------------------------------------------
  function get_shiftTT(itact)
    implicit none
    integer(kind=4) :: itact
    real(kind=8), dimension(2) :: get_shiftTT
 
    if (tactype2bdyty(3,itact) == i_rbdy2) then

      get_shiftTT(:) = get_shiftTT_RBDY2(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    else if (tactype2bdyty(3,itact) == i_mbs2) then

      get_shiftTT(:) = get_shiftTT_MBS2D(tactype2bdyty(1,itact),tactype2bdyty(2,itact))

    endif

  end function

  function get_tact_id(ibdyty, itacty, bdyty)
    implicit none
    integer, intent(in) :: bdyty, ibdyty, itacty
    integer :: get_tact_id
    !
    integer :: i_tact

    get_tact_id = 0

    do i_tact = 1, size(tactype2bdyty) / 3
        if( tactype2bdyty(1,i_tact) == ibdyty .and. &
            tactype2bdyty(2,i_tact) == itacty .and. &
            tactype2bdyty(3,i_tact) == bdyty  ) then
            get_tact_id = i_tact
            exit
        end if
    end do

  end function

