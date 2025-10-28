
  !> Solve some algebraic systems somewhere
  subroutine vitrad(id_inter, icdan, storage, need_full_V)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> where to get the right hand side and write the solution
    integer, intent(in) :: storage
    !> if need vfree+v or only v
    logical, intent(in), optional :: need_full_V

    call select_this_(id_inter)
    call vitrad_inter(icdan, storage, need_full_V)

  end subroutine vitrad

  !> Nullify the reaction somewhere
  subroutine nullify_reac(id_inter, icdan, storage)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> where to nullify the reaction
    integer, intent(in) :: storage

    call select_this_(id_inter)
    call nullify_reac_inter(icdan, storage)

  end subroutine nullify_reac

  !> Nullify the velocity somewhere
  subroutine nullify_vlocy(id_inter, icdan, storage)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> where to nullify the velocity
    integer, intent(in) :: storage

    call select_this_(id_inter)
    call nullify_vlocy_inter(icdan, storage)

  end subroutine nullify_vlocy

  ! access to this data structure

  !> Get a copy of the internal variables of an interaction
  subroutine get_internal(id_inter, icdan, internals)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> values of internal variables
    real(kind=8), dimension(max_internal_tact), intent(out) :: internals

    call select_this_(id_inter)

    internals(1:max_internal_tact) = this_inter(icdan)%internal(1:max_internal_tact)

  end subroutine get_internal

  !> Get gapTT and gapTTBegin
  subroutine get_gaps(id_inter, icdan, gapTT, gapTTBegin)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> gap
    real(kind=8), intent(out) :: gapTT
    !> gap begin
    real(kind=8), intent(out) :: gapTTBegin

    call select_this_(id_inter)

    gapTT      = this_inter(icdan)%gapTT
    gapTTBegin = this_inter(icdan)%gapTTBegin

  end subroutine get_gaps

  !> Set a values of the internal variables of an interaction
  subroutine set_internal(id_inter, icdan, internals)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> new values of internal variables
    real(kind=8), dimension(max_internal_tact), intent(in) :: internals

    call select_this_(id_inter)

    this_inter(icdan)%internal(1:max_internal_tact) = internals(1:max_internal_tact)

  end subroutine set_internal

  !> Set the value of a single internal variable of an interaction
  !> 'bv' stands for 'by value'
  subroutine set_internal_bv(id_inter, icdan, idx, val)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> internal index
    integer     , intent(in) :: idx
    !> new value of internal variable
    real(kind=8), intent(in) :: val

    call select_this_(id_inter)

    this_inter(icdan)%internal(idx) = val

  end subroutine set_internal_bv

  !> Get entity id of the candidate and the antagonist
  subroutine inter2ENT(id_inter, icdan, icdent, ianent )
    implicit none
    !> interaction type id
    integer, intent(in)  :: id_inter
    !> interaction index
    integer, intent(in)  :: icdan
    !> entity of candidate
    integer, intent(out) :: icdent
    !> entity of antagonist
    integer, intent(out) :: ianent

    call select_this_(id_inter)
    icdent = this_inter(icdan)%icdent
    ianent = this_inter(icdan)%ianent

  end subroutine inter2ENT

  !> Get the contact law index of an interaction
  function get_tact_lawnb(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> contact law id
    integer             :: get_tact_lawnb

    call select_this_(id_inter)
    get_tact_lawnb = this_inter(icdan)%lawnb

  end function get_tact_lawnb

  !> Get the body candidate number
  function get_icdbdy(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> candidate body index
    integer             :: get_icdbdy

    call select_this_(id_inter)
    get_icdbdy = this_inter(icdan)%icdbdy

  end function get_icdbdy

  !> Get the body antagonist number
  function get_ianbdy(id_inter, icdan)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> interaction index
    integer, intent(in) :: icdan
    !> antagonist body index
    integer             :: get_ianbdy

    call select_this_(id_inter)
    get_ianbdy = this_inter(icdan)%ianbdy

  end function get_ianbdy

  !> Get the integer data of an interaction
  subroutine get_idata(id_inter, icdan, idata)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> values of integer data
    integer, dimension(13), intent(out) :: idata

    call select_this_(id_inter)

    idata( 1) = this_inter(icdan)%icdbtyp
    idata( 2) = this_inter(icdan)%ianbtyp
    idata( 3) = this_inter(icdan)%icdbdy
    idata( 4) = this_inter(icdan)%ianbdy
    idata( 5) = this_inter(icdan)%icdctyp
    idata( 6) = this_inter(icdan)%ianctyp
    idata( 7) = this_inter(icdan)%icdbtac
    idata( 8) = this_inter(icdan)%ianbtac
    idata( 9) = this_inter(icdan)%icdsci
    idata(10) = this_inter(icdan)%iansci
    idata(11) = this_inter(icdan)%lawnb
    idata(12) = this_inter(icdan)%status
    idata(13) = this_inter(icdan)%nb_internal

  end subroutine get_idata

  !> Get effective mass and radius (only for specific contactors/laws)
  !> \TODO: check if still needed
  subroutine get_eff(id_inter, icdan, meff, reff)
    implicit none
    !> interaction type id
    integer     , intent(in)  :: id_inter
    !> interaction index
    integer     , intent(in)  :: icdan
    !> effective mass
    real(kind=8), intent(out) :: meff
    !> effective radius
    real(kind=8), intent(out) :: reff

     call select_this_(id_inter)

     reff = this_inter(icdan)%reff
     meff = this_inter(icdan)%meff

  end subroutine get_eff

  function get_ptr_one(id_inter, icdan)
    implicit none
    integer(kind=4), intent(in) :: id_inter, icdan
    type(T_interaction), pointer :: get_ptr_one

    call select_this_(id_inter)

    get_ptr_one => this_inter(icdan)

  end function get_ptr_one

  !> Get the indices in verlet array of an interaction in this
  subroutine this2verlet(id_inter, icdan, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in)  :: id_inter
    !> interaction index
    integer, intent(in)  :: icdan
    !> contactor candidate index
    integer, intent(out) :: icdtac
    !> adjacent index
    integer, intent(out) :: iadj

    call select_this_(id_inter)

    icdtac = this_inter(icdan)%icdtac
    iadj   = this_inter(icdan)%iadj

  end subroutine this2verlet
  
  ! access to verlet data structure

  !> Get number of interactions in verlet array
  integer function get_nb_verlets(id_inter)
    implicit none
    !> interaction type id
    integer, intent(in)  :: id_inter
    !
    integer :: icdtac, nb_cd, iadj

    call select_this_( id_inter )

    if( associated(verlet_inter) ) then
      nb_cd = size(verlet_inter)
    else
      nb_cd = 0
    end if

    get_nb_verlets = 0

    if (nb_cd == 0) return 
    
    do icdtac = 1, nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle

      do iadj = 1, verlet_inter(icdtac)%adjsz
        get_nb_verlets = get_nb_verlets + 1
      end do

    end do

  end function get_nb_verlets
  
  !> Get the size of verlet array
  integer function get_verlet_size(id_inter)
    implicit none
    !> interaction type id
    integer, intent(in)  :: id_inter

    call select_this_(id_inter)

    if( associated(verlet_inter) ) then
        get_verlet_size = size( verlet_inter )
    else
        get_verlet_size = 0
    end if

  end function get_verlet_size

  !> Get the number of adjacents of a candidate contactor
  integer function get_verlet_adjsz(id_inter, icdtac)
    implicit none
    !> interaction type id
    integer, intent(in)  :: id_inter
    !> candidate contactor index
    integer, intent(in)  :: icdtac

    call select_this_(id_inter)

    get_verlet_adjsz = verlet_inter(icdtac)%adjsz

  end function get_verlet_adjsz


  !> Get the candidate contactor index in avatar
  integer function get_verlet_iantac(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_(id_inter)
    get_verlet_iantac = verlet_inter(icdtac)%antac(iadj)

  end function get_verlet_iantac

  !> Get the candidate contactor index in contactor list
  integer function get_verlet_lantac(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !
    integer :: ianmdl, ianbdy, iantac
    integer, dimension(:,:), pointer :: cdtac2bdyty

    call select_this_(id_inter)

    ianmdl = verlet_inter(icdtac)%anmodel(iadj)
    ianbdy = verlet_inter(icdtac)%anbdy(iadj)
    iantac = verlet_inter(icdtac)%antac(iadj)

    get_verlet_lantac = get_an_tacty_inter(ianmdl, ianbdy, iantac)

  end function get_verlet_lantac

  !> Get the candidate body index
  integer function get_verlet_icdbdy(id_inter, icdtac)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac

    call select_this_(id_inter)
    get_verlet_icdbdy = verlet_inter(icdtac)%cdbdy

  end function get_verlet_icdbdy

  !> Get the antagonist body index
  integer function get_verlet_ianbdy(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_(id_inter)
    get_verlet_ianbdy = verlet_inter(icdtac)%anbdy(iadj)

  end function get_verlet_ianbdy

  !> Get the contactor index of the candidate body
  integer function get_verlet_icdbtac(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_(id_inter)

    call faterr('inter_handler::get_verlet_icdbtac','not implemented yet')
    !beurk beurk beurk beurk...
    get_verlet_icdbtac = this_inter( verlet_inter(icdtac)%icdan(iadj) )%icdbtac

  end function get_verlet_icdbtac

  !> Get the contactor index of the antagonist body
  integer function get_verlet_ianbtac(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> contactor candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_(id_inter)

    call faterr('inter_handler::get_verlet_ianbtac','not implemented yet')
    !beurk beurk beurk beurk...
    get_verlet_ianbtac = this_inter( verlet_inter(icdtac)%icdan(iadj) )%ianbtac

  end function get_verlet_ianbtac

  !> Get the type gap of an interaction
  real(kind=8) function get_verlet_gapTT(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_(id_inter)

    get_verlet_gapTT = verlet_inter(icdtac)%gapTT(iadj)

  end function get_verlet_gapTT

  !> Get the tact law id of a verlet interaction
  integer function get_verlet_tact_lawnb(id_inter, icdtac, iadj)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj

    call select_this_( id_inter )

    get_verlet_tact_lawnb = get_verlet_tact_lawnb_inter(icdtac, iadj)

  end function get_verlet_tact_lawnb

  !> Get the internal array of a verlet interaction
  subroutine get_verlet_internal(id_inter, icdtac, iadj, internal)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> internal array
    real(kind=8), dimension(:), intent(out) :: internal

    call select_this_(id_inter)

    internal(:) = verlet_inter(icdtac)%internal(:,iadj)

  end subroutine get_verlet_internal

  !> Set the internal array of a verlet interaction
  subroutine set_verlet_internal(id_inter, icdtac, iadj, internal)
    implicit none
    !> interaction type id
    integer, intent(in) :: id_inter
    !> candidate index
    integer, intent(in) :: icdtac
    !> adjacent index
    integer, intent(in) :: iadj
    !> internal array
    real(kind=8), dimension(:), intent(in) :: internal

    call select_this_(id_inter)

    verlet_inter(icdtac)%internal(:,iadj) = internal

  end subroutine set_verlet_internal

  !> \brief Get the internal array of all the verlet interactions of a submodule
  !> Allocate memory and copy
  function get_all_internal( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> internal array of all interactions of selected type
    real(kind=8), dimension(:,:), pointer :: get_all_internal
    !
    integer :: nb_cd, nb_v, icdtac, iadj, icdan

    get_all_internal => null()
 
    call select_this_(inter_id)

    if( associated(verlet_inter) ) then
      nb_cd = size(verlet_inter)
    else
      nb_cd = 0
    end if

    if( nb_cd <= 0 ) return

    nb_v = get_nb_verlets( inter_id )
    if( nb_v <= 0 ) return

    allocate( get_all_internal(max_internal_tact, nb_v) )

    do icdtac = 1, nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle

      do iadj = 1, verlet_inter(icdtac)%adjsz
        icdan = verlet_inter(icdtac)%icdan(iadj)
        get_all_internal(1:max_internal_tact, icdan) = verlet_inter(icdtac)%internal(1:max_internal_tact, iadj)
      end do

    end do

  end function get_all_internal


  !> \brief Get the integer data array of all the verlet interactions of a submodule
  !> Allocate memory and copy
  function get_all_idata( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> integer data array of all interactions of selected type
     integer, dimension(:,:), pointer :: get_all_idata
    !
    integer :: nb_cd, nb_v, icdtac, iadj, icdan, nb_data, lawnb

    ! store in that order :
    ! cdmodel, anmodel, icdbdy, ianbdy, cdtac, antac, icdtac, iantac, icdsci, iansci, lawnb, status, nb_internal
    nb_data = 13

    get_all_idata => null()

    call select_this_(inter_id)

    if( associated(verlet_inter) ) then
      nb_cd = size(verlet_inter)
    else
      nb_cd = 0
    end if

    if( nb_cd <= 0 ) return
    
    nb_v = get_nb_verlets( inter_id )
    if( nb_v <= 0 ) return

    allocate( get_all_idata(nb_data, nb_v) )

    do icdtac = 1, nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle

      do iadj = 1, verlet_inter(icdtac)%adjsz

        icdan = verlet_inter(icdtac)%icdan(iadj)

        lawnb = get_verlet_tact_lawnb_inter(icdtac, iadj)

        get_all_idata( 1,icdan) = verlet_inter(icdtac)%cdmodel
        get_all_idata( 2,icdan) = verlet_inter(icdtac)%anmodel(iadj)
        get_all_idata( 3,icdan) = verlet_inter(icdtac)%cdbdy
        get_all_idata( 4,icdan) = verlet_inter(icdtac)%anbdy(iadj)
        get_all_idata( 5,icdan) = con_inter%id_cdtac
        get_all_idata( 6,icdan) = con_inter%id_antac
        get_all_idata( 7,icdan) = verlet_inter(icdtac)%cdtac
        get_all_idata( 8,icdan) = verlet_inter(icdtac)%antac(iadj)
        get_all_idata( 9,icdan) = verlet_inter(icdtac)%cdsci(iadj)
        get_all_idata(10,icdan) = verlet_inter(icdtac)%ansci(iadj)
        get_all_idata(11,icdan) = lawnb
        get_all_idata(12,icdan) = verlet_inter(icdtac)%status(iadj)
        get_all_idata(13,icdan) = get_nb_internal(lawnb)

      end do

    end do

  end function get_all_idata

  !> \brief Get the internal array of all the interactions of a submodule
  !> Allocate memory and copy
  function get_all_tactlawnb( inter_id )
    implicit none
    !> type id of the interactions to get
    integer, intent(in) :: inter_id
    !> tact law number of all interactions of selected type
    integer(kind=4), dimension(:), pointer :: get_all_tactlawnb
    !
    integer :: nb_cd, nb_v, icdtac, iadj, icdan


    get_all_tactlawnb => null()
 
    call select_this_(inter_id)

    if( associated(verlet_inter) ) then
      nb_cd = size(verlet_inter)
    else
      nb_cd = 0
    end if
    if( nb_cd <= 0 ) return

    nb_v = get_nb_verlets( inter_id )
    if( nb_v <= 0 ) return

    allocate( get_all_tactlawnb(nb_v) )
 
    do icdtac = 1, nb_cd

      if (verlet_inter(icdtac)%adjsz == 0) cycle

      do iadj = 1, verlet_inter(icdtac)%adjsz
         
        icdan = verlet_inter(icdtac)%icdan(iadj)
         
        get_all_tactlawnb(icdan) = get_verlet_tact_lawnb_inter(icdtac, iadj)

      end do

    end do
 
  end function get_all_tactlawnb

  ! !> Get effective mass and radius (only for specific contactors/laws)
  ! !> \TODO: check if still needed
  ! subroutine get_eff(id_inter, icdan, meff, reff)
  
  ! violation data structure

  !> Set the violation value of a contact
  subroutine set_violation(id_inter, icdan, vlton)
    implicit none
    !> interaction type id
    integer     , intent(in) :: id_inter
    !> interaction index
    integer     , intent(in) :: icdan
    !> violation value
    real(kind=8), intent(in) :: vlton

    call select_this_(id_inter)
    violation_inter(icdan) = vlton

  end subroutine set_violation
