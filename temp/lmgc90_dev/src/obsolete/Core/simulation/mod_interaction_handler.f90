
!> interaction database : management
!> \author R. Mozul (alas)
!> \date   October 2011
!>

module interaction_handler

  use utilities, only : logmes     , &
                        faterr     , &
                        get_io_unit, &
                        T_r8_matrix, &
                        G_i_list

  use overall, only : location, nbDIME  , &
                      H, Hrate          , &
                      out_Vloc_Rloc     , &
                      last_Vloc_Rloc

  use tact_behaviour, only : see, &
                             get_isee_specific2

  use anonymous, only: T_object                   , &
                       set_r8_ptr                 , &
                       get_r8_vector              , &
                       get_i4_vector              , &
                       set_object_rank => set_rank, &
                       get_object_rank => get_rank, &
                       erase_object
                       

  use anonymous_ptr_container, only: object_container   => ptr_container              , &
                                     get_nb_object      => get_nb_data                , &
                                     get_object         => get_data                   , &
                                     reset_ptr_search   => reset_search               , &
                                     next_ptr_search    => next_search                , &
                                     current_ptr_search => get_current_search_data    , &
                                     del_cur_ptr_search => delete_current_list_element, &
                                     open_container     => open_ptr_container         , &
                                     close_container    => close_ptr_container        , &
                                     erase_container    => erase_ptr_container

  use interaction, only : list_data    => T_interaction      , &
                          new_data     => new_interaction    , &
                          erase_data   => erase_interaction  , &
                          close_data   => close_interaction  , &
                          open_data    => open_interaction   , &
                          display_data => display_interaction, &
                          set_rank                           , &
                          get_rank

  use contactor, only : T_contactor         , &
                        get_nb_contactors   , &
                        get_contactor       , &
                        get_rough_boundaries, &
                        get_nb_point_outline, &
                        get_outline         , &
                        compute_support_points

  use model_handler, only : get_nb_models  , &
                            get_coor       , &
                            get_nodal_field, &
                            get_detection_configuration

  use DKDKx, only : get_nb_rough_DKDKx              , &
                    get_rough_DKDKx                 , &
                    reset_violation_DKDKx           , &
                    detect_and_compute_contact_DKDKx

  use rough_detections, only : boxes_method

  implicit none

  private

  include 'pointer_container_type.f90'

  !> array of interactions, at current and previous steps
  type(ptr_container), dimension(2) :: these
  !> number of adjactent pairs of a candidate contactor
  integer(kind=4),     dimension(:), allocatable :: nb_adj
  !> map the cumulated number of adjacent pairs to a candidate contactor
  integer(kind=4),     dimension(:), allocatable :: vrlt_map
  !> index of these(2) in these(1)
  integer(kind=4),     dimension(:), allocatable :: verlet

  !> for all model_handles, the list of nodes needed by contact detection
  type(G_i_list), dimension(:), allocatable :: all_mdlhdl_nodes
  !> the contact detection configuration of the model_handles (coorTT and frameTT)
  type(T_r8_matrix), dimension(:), allocatable :: rigid_mdlhdl_config
  !> the contact detection configuration for each nodes of the model_handles (dispTT)
  type(T_r8_matrix), dimension(:), allocatable :: defor_mdlhdl_config

  !> rough boundaries of each contactor
  type(T_object) , dimension(:)  , allocatable :: rough_boundaries
  !> for each rough boundary its boundary (radius only yet)
  real(kind=8)   , dimension(:)  , allocatable :: rough_boundary
  !> for each rough boundary its position
  real(kind=8)   , dimension(:,:), allocatable :: rough_position
  !> for each rough boundary its contactor number
  integer(kind=4), dimension(:)  , allocatable :: rough_index

  !> a rough list of candidat and antagonist indexed by rough boundary indices
  type(object_container) :: rough_list

  !> number of outline points for each contactor for visu (should be in contactor ?)
  integer(kind=4),dimension(:), pointer :: nb_point_outlines => null() 
  !> outline of contactors for visu (should be in contactor ?)
  real(kind=8), dimension(:,:), pointer :: outlines => null()

  public get_nb_interactions , &
         get_interaction     , &
         add_interaction     , &
         initialize_search   , &
         get_next_interaction, &
         delete_interaction  , &
         delete_this         , &
         close_this          , &
         open_this           , &
         display_interactions, &
         write_xxx_Vloc_Rloc , &
         write_out_Vloc_Rloc

  public initialize             , &
         coor_prediction        , &
         add_rough_DKDKx_to_this, &
         run_rough_detection    , &
         run_fine_detections    , &
         stock_rloc, recup_rloc , &
         init_outlines          , &
         update_postdata        , &
         get_nb_point_outlines  , &
         clean_module

  contains

  include 'pointer_container_methods.f90'

  !----------------------------------------------------------
  ! Functions to work on a container of interactions
  !----------------------------------------------------------

  !> \brief add an interaction to 'this' interaction container
  subroutine add_interaction(icdtac, iantac, isee, periodic)
    implicit none
    !> [in] index of candidat contactor
    integer(kind=4), intent(in) :: icdtac
    !> [in] index of antagonist contactor
    integer(kind=4), intent(in) :: iantac
    !> [in] id of the contact law
    integer(kind=4), intent(in) :: isee
    !> [in] is the contact periodic (1:yes, 0:no)
    integer(kind=4), intent(in) :: periodic
    !
    type(list_data), pointer :: interaction
    character(len=80) :: cout
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'interaction_handler::add_interaction'
   
    interaction => null()

    if( these(1)%open ) then
      interaction => new_data()
      ! trick to auto rank current object
      ! by incrementing rank of last object added
      if( these(1)%nb_data > 0 ) then
        interaction%rank = these(1)%root%data%rank + 1
      else
        interaction%rank = 1
      end if
      interaction%icdtac   = icdtac
      interaction%iantac   = iantac
      interaction%isee     = isee
      interaction%periodic = periodic
      call add_to_ptr_container(these(1), interaction)
    else
      write(cout,'(A)') 'interaction container close'
      call faterr(IAM,cout)
    end if

  end subroutine

  !> \brief get first interaction of search
  function  initialize_search()
    implicit none
    !> [return] first interaction of search
    type(list_data), pointer :: initialize_search

    call reset_search(these(1))
    initialize_search => get_current_search_data(these(1))

  end function

  !> \brief get next interaction of searched process
  function  get_next_interaction()
    implicit none
    !> [return] an interaction
    type(list_data), pointer :: get_next_interaction

    call next_search(these(1))
    get_next_interaction => get_current_search_data(these(1))

  end function

  !> \brief remove current interaction of searched process, and return next one
  function  delete_interaction()
    implicit none
    !> [return] next interaction
    type(list_data), pointer :: delete_interaction

    call delete_current_list_element(these(1))
    delete_interaction => get_current_search_data(these(1))

  end function

  !> \brief open this container
  subroutine open_this()
    implicit none
    call open_ptr_container(these(1))
  end subroutine

  !> \brief close this container
  subroutine close_this()
    implicit none
    call close_ptr_container(these(1))
  end subroutine

  !> \brief delete this container
  subroutine delete_this()
    implicit none
    call erase_ptr_container(these(1))
  end subroutine

  !> \brief display 'this' interaction container
  subroutine display_interactions(ifich)
    implicit none
    !> [in] (optional) unit number in which to write
    integer(kind=4), optional :: ifich
    !
    integer(kind=4) :: i_unit

    if( present(ifich) ) then
      i_unit = ifich
    else
      i_unit = 6
    end if

    write(i_unit,*) 'Number of interactions: ', get_nb_data(these(1))
    call display_ptr_container(these(1), ifich)

  end subroutine

  !> \brief get the number of interaction in 'this' container
  function get_nb_interactions()
    implicit none
    !> [return] number of interactions
    integer(kind=4) :: get_nb_interactions

    get_nb_interactions = these(1)%nb_data
  end function

  !> \brief return an interaction of 'this' close container
  function get_interaction(rank)
    implicit none
    !> [in] index of the interaction
    integer(kind=4), intent(in) :: rank
    !> [return] interaction
    type(list_data), pointer    :: get_interaction
    !
    character(len=36) :: IAM
    !      123456789012345678901234567890123456
    IAM = 'interaction_handler::get_interaction'

    get_interaction => null()
    if( these(1)%open ) then
      call faterr(IAM,'this container is open')
    end if
 
    if( rank < 1 .or. rank > these(1)%nb_data ) then
      call faterr(IAM,'desired rank out of range of this container')
    end if

    get_interaction => these(1)%data_array(rank)%data
  end function

  !> \brief write a VlocRloc file
  subroutine write_xxx_Vloc_Rloc(which)
    implicit none
    !> [in] in what file to write
    integer(kind=4), intent(in) :: which
    !
    integer(kind=4) :: nfich,lc
    
    nfich = get_io_unit()
    
    select case(which)
    case(1)
       lc = len_trim(out_Vloc_Rloc)
       open(unit=nfich,status='OLD',position='APPEND',file=TRIM(location(out_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(2)
       lc = len_trim(last_Vloc_Rloc)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=TRIM(location(last_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(6)
       call write_out_Vloc_Rloc(6)
    end select
    
  end subroutine write_xxx_Vloc_Rloc

  subroutine write_out_Vloc_Rloc(nfich)
    implicit none
    !> [in] unit file in which to write
    integer(kind=4), intent(in) :: nfich
    !
    integer(kind=4) :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
    integer(kind=4) :: icdtact
    real(kind=8), dimension(2) :: coor
    type(list_data), pointer :: inter
    
    !> \todo: because of stock_rloc it is these(2) to write
    do icdan = 1, these(2)%nb_data

       inter  => these(2)%data_array(icdan)%data 

       icdtac = inter%icdtac
       iantac = inter%iantac
       
       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKDKx',icdan     
       !1234567890123456789012345678901234567890123456789012345678901234567890123456  
       WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'     
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
            'RBDY2',inter%icdbdy,'DISKx',inter%icdtac,  see(inter%isee)%behav,  &
            'RBDY2',inter%ianbdy,'DISKx',inter%iantac,  inter%status,iantac
!       if (smooth_method) then
!         WRITE(nfich,104)'rlt  ',inter%rl(1)  ,'rln  ',inter%rl(2)  ,'rls  ',0.D0
!       else
       WRITE(nfich,104)'rlt/H',inter%rl(1)/H,'rln/H',inter%rl(2)/H,'rls/H',0.D0
!       endif
       WRITE(nfich,104)'vlt =',inter%vl(1)   ,'vln =',inter%vl(2)   ,'vls =',0.D0
       WRITE(nfich,103)'gapTT',inter%gapTT 
       WRITE(nfich,104)'n(1)=',inter%uc(1,2),'n(2)=',inter%uc(2,2),'n(3)=',0.D0
       WRITE(nfich,104)'coo1=',inter%coor(1)           ,'coo2=',inter%coor(2)           ,'coo3=',0.D0
       
       IF (inter%nb_internal /= 0) THEN
          WRITE(nfich,'(6(1x,D14.7))')  inter%internal(1:inter%nb_internal)
       ENDIF
       
       WRITE(nfich,'(A1)')' '               
    END DO

103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

  end subroutine write_out_Vloc_Rloc


  !----------------------------------------------------------
  ! Functions to perform the detections
  !----------------------------------------------------------

  !> \brief Initialize interacion handler : size vrlt_map and adjac arrays
  subroutine initialize()
    implicit none
    integer(kind=4)   :: nb_contactors, errare
    integer(kind=4)   :: nb_used_nodes, nb_nodes_list, i_mdl_hdl, i_tact, i_node, i, to_add
    integer(kind=4)   , dimension(:), pointer :: new_node_list
    type(T_contactor), pointer :: contactor
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'interaction_handler::initialize'

    nb_contactors = get_nb_contactors()

    if( allocated(nb_adj) .and. size(nb_adj) /= nb_contactors ) then
      deallocate(nb_adj)
    end if

    if( .not. allocated(nb_adj) ) then
      allocate(nb_adj(nb_contactors), stat=errare)
      if( errare /=0 ) then
        write(cout, '(A,1x,I0,A)') 'error in allocating nb_adj (error', errare,')'
        call faterr(IAM,cout)
      end if
    end if

    if( allocated(vrlt_map) .and. size(vrlt_map) /= nb_contactors+1 ) then
      deallocate(vrlt_map)
    end if

    if( .not. allocated(vrlt_map) ) then
      allocate(vrlt_map(nb_contactors+1), stat=errare)
      if( errare /=0 ) then
        write(cout, '(A,1x,I0,A)') 'error in allocating vrlt_map (error', errare,')'
        call faterr(IAM,cout)
      end if
    end if

    vrlt_map(1:nb_contactors) = 0

    !if( .not. allocated(adjac) ) then

    !  allocate( adjac(nb_contactors), stat=errare )
    !  if( errare /=0 ) then
    !    write(cout,'(A)') 'error in allocating adjac'
    !    call faterr(IAM,cout)
    !  end if

    !  do i_tact = 1, nb_contactors
    !    nullify(adjac(itact)%icdan)
    !  end do

    !else
    !  do i_tact = 1, nb_contactors
    !    if( associated(adjac(i_tact)%icdan) )  deallocate(adjac(i_tact)%icdan)
    !    nullify(adjac(i_tact)%icdan)
    !  end do
    !end if

    ! generate the list of nodes of model_handles used by the contactor
    if( nb_contactors == 0 ) return

    i = get_nb_models()
    allocate(all_mdlhdl_nodes(i))
    allocate(rigid_mdlhdl_config(i))
    allocate(defor_mdlhdl_config(i))

    new_node_list => null()

    do i_tact = 1, nb_contactors

      contactor => get_contactor(i_tact)
      i_mdl_hdl = contactor%model_ID
      nb_used_nodes = size(contactor%mdlhdl_node_list)

      ! checking if model_handle has already been seen
      if( associated(all_mdlhdl_nodes(i_mdl_hdl)%G_i) ) then

        ! counting nodes not already in the list
        to_add = 0
        do i_node = 1, nb_used_nodes
          if( .not. any( all_mdlhdl_nodes(i_mdl_hdl)%G_i==contactor%mdlhdl_node_list(i_node) ) ) to_add = to_add + 1
        end do

        ! adding nodes not already in the list
        nb_nodes_list = size(all_mdlhdl_nodes(i_mdl_hdl)%G_i)
        allocate(new_node_list(nb_nodes_list+to_add))
        new_node_list(1:nb_nodes_list) = all_mdlhdl_nodes(i_mdl_hdl)%G_i(1:nb_nodes_list)
        i = nb_nodes_list + 1
        do i_node = 1, to_add
          if( any( all_mdlhdl_nodes(i_mdl_hdl)%G_i==contactor%mdlhdl_node_list(i_node) ) ) cycle

          new_node_list(i) = contactor%mdlhdl_node_list(i_node)
          i = i + 1
        end do

        deallocate(all_mdlhdl_nodes(i_mdl_hdl)%G_i)
        all_mdlhdl_nodes(i_mdl_hdl)%G_i => new_node_list
        new_node_list => null()

      else

        ! removing duplicates
        allocate(new_node_list(nb_used_nodes))
        new_node_list = contactor%mdlhdl_node_list

        to_add = 0
        new_node_list = 0
        do i_node = 1, nb_used_nodes
          if( any( new_node_list==contactor%mdlhdl_node_list(i_node) ) ) cycle
          to_add = to_add + 1
          new_node_list(to_add) = contactor%mdlhdl_node_list(i_node)
        end do
 
        allocate( all_mdlhdl_nodes(i_mdl_hdl)%G_i(to_add) )
        all_mdlhdl_nodes(i_mdl_hdl)%G_i(1:to_add) = new_node_list(1:to_add)

        deallocate(new_node_list)

      end if

    end do

    !> \todo : It may not be stupid to sort the G_i list of all_mdlhdl_nodes

  end subroutine

  !> \brief update coordinates of contactor according to model_handler prediction
  subroutine coor_prediction()
    implicit none
    integer(kind=4) :: i, i_mdlhdl
    type(T_contactor), pointer :: contactor

    do i = 1, get_nb_models()
      if( .not. associated(all_mdlhdl_nodes(i)%G_i) ) cycle

      ! no not yet...
      !! really ? realloc at each detection within get_detection_configuration ?
      !if( allocated(rigid_mdlhdl_config(i)%rdata) ) then
      !  deallocate(rigid_mdlhdl_config(i)%rdata)
      !end if
      !if( allocated(defor_mdlhdl_config(i)%rdata) ) then
      !  deallocate(defor_mdlhdl_config(i)%rdata)
      !end if

      ! passing only _config and not %rdata ?
      call get_detection_configuration(i, all_mdlhdl_nodes(i)%G_i, rigid_mdlhdl_config(i)%rdata, defor_mdlhdl_config(i)%rdata)

    end do

    do i = 1, get_nb_contactors()
      contactor => get_contactor(i)
      i_mdlhdl  = contactor%model_ID
      call compute_support_points(contactor, rigid_mdlhdl_config(i_mdlhdl)%rdata, defor_mdlhdl_config(i_mdlhdl)%rdata)
    end do

  end subroutine

  !> \brief get the rough interactions of DKDKx module to initialize this container
  subroutine add_rough_DKDKx_to_this()
    implicit none
    integer(kind=4) :: icdan, icdtac, iantac, isee, periodic

    do icdan = 1, get_nb_rough_DKDKx()
      call get_rough_DKDKx(icdan, icdtac, iantac, isee, periodic) 
      call add_interaction(icdtac, iantac, isee, periodic)
    end do

  end subroutine

  !> \brief run rough detection
  subroutine run_rough_detection()
    implicit none
    integer(kind=4) :: i_tact, nb_contactors, nb_trb, i_rb, isee!, icdan
    real(kind=8)    :: alert
    type(T_contactor), pointer :: contactor, cd, an
    type(T_object)   , pointer :: current_rough
    integer(kind=4), dimension(:)  , pointer :: i4
    real(kind=8)   , dimension(:)  , pointer :: trb
    character(len=80) :: cout
    character(len=40) :: IAM
    !      1234567890123456789012345678901234567890
    IAM = 'interaction_handler::run_rough_detection'

    trb => null()

    nb_contactors = get_nb_contactors()

    ! generate the list of rough_boundary objects
    ! currently a rought object is a center and a radius
    ! but other possibilities should be handable (AABB, OBB...)

    if( .not. allocated(rough_boundaries) ) then
      allocate(rough_boundaries(nb_contactors))
    end if

    if( size(rough_boundaries) /= nb_contactors ) then
      write(cout,'(A)') 'rough_boundaries array already allocated but of the wrong size'
      call faterr(IAM,cout)
    end if

    i_rb = 0
    do i_tact = 1, nb_contactors
      contactor => get_contactor(i_tact)
      call get_rough_boundaries(contactor, nb_trb, trb)

      call set_object_rank(rough_boundaries(i_tact), nb_trb)
      call set_r8_ptr(rough_boundaries(i_tact), trb)
      trb => null()

      i_rb = i_rb + nb_trb
    end do

    if( .not. allocated(rough_index) ) then
      allocate( rough_index(i_rb) )
      allocate( rough_position(1:nbDIME,i_rb) )
      allocate( rough_boundary(i_rb) )
    end if

    if( size(rough_index) /= i_rb) then
      write(cout,'(A)') 'rough_index array already allocated but of the wrong size'
      call faterr(IAM,cout)
    end if

    if( size(rough_index) /= size(rough_position,2) .or. size(rough_index) /= size(rough_boundary) ) then
      write(cout,'(A)') 'rough_index and rough_position or rough_boundary have not the same dimension... go to hell!!!'
      call faterr(IAM,cout)
    end if

    i_rb = 0
    do i_tact = 1, nb_contactors
      trb => get_r8_vector(rough_boundaries(i_tact))
      do nb_trb = 0, get_object_rank(rough_boundaries(i_tact)) - 1
        i_rb = i_rb + 1
        rough_index(i_rb) = i_tact
        rough_position(1:nbDIME,i_rb) = trb(nb_trb*(nbDIME+1)+1:nb_trb*(nbDIME+1)+nbDIME)
        rough_boundary(i_rb)          = trb((nb_trb+1)*(nbDIME+1))
      end do
    end do
    
    ! cleaning rough_* ?
    call erase_container(rough_list)
    ! should be max( alert of sees )
    alert = 0.1
    call boxes_method(rough_position, rough_boundary, alert, rough_list, .false., 0.d0)

    call open_container(rough_list)
    call reset_ptr_search(rough_list)
    current_rough => current_ptr_search(rough_list)
    do while( associated(current_rough) )

      i4    => get_i4_vector(current_rough)

      cd    => get_contactor( rough_index(i4(1)) )
      an    => get_contactor( rough_index(i4(2)) )

      isee  = get_isee_specific2( cd%geometric_ID, cd%color, an%geometric_ID, an%color)
      if( cd%model_ID == an%model_ID .or. isee == 0 ) then   
        call del_cur_ptr_search(rough_list)
        current_rough => current_ptr_search(rough_list)
      else
        call next_ptr_search(rough_list)
        current_rough => current_ptr_search(rough_list)
      end if

    end do
    call close_container(rough_list)

    write(cout,'(A,1x,I0)') 'nb_rough found:', get_nb_object(rough_list)
    call logmes(cout)

  end subroutine

  !> \brief run fine detections of submodules
  subroutine run_fine_detections()
    use mdl_hdl_parameters !because get_nodal_field use p_meca_td_V... to remove !!!
    implicit none
    type(T_contactor), pointer :: cd, an
    type(T_object)   , pointer :: rough
    type(list_data)  , pointer :: interaction
    real(kind=8)   , dimension(:,:), pointer :: vcd, van
    real(kind=8)   , dimension(nbDIME)       :: shift_cd, shift_an
    integer(kind=4), dimension(:)  , pointer :: i4
    integer(kind=4) :: icdan, icdtac, iantac, isee, i_rough
    character(len=80) :: cout

    vcd => null()
    van => null()

    interaction => null()

    ! reminder: this is an array
    nb_adj = 0

    icdan = 0        

    do i_rough = 1, get_nb_object(rough_list)

      rough => get_object(rough_list,i_rough)
      i4    => get_i4_vector(rough)

      icdtac = rough_index(i4(1))
      iantac = rough_index(i4(2))

      cd    => get_contactor( icdtac )
      an    => get_contactor( iantac )

      isee  = get_isee_specific2( cd%geometric_ID, cd%color, an%geometric_ID, an%color)
      if( isee == 0 ) cycle

      shift_cd = matmul(rigid_mdlhdl_config(cd%model_ID)%rdata(1:nbDIME,2:nbDIME+1), cd%support_nodes(:,1))
      shift_an = matmul(rigid_mdlhdl_config(an%model_ID)%rdata(1:nbDIME,2:nbDIME+1), an%support_nodes(:,1))

      ! \todo from here to interaction => detect should be hidden behing a switch function
      ! \todo probably the involved contactors should be given in input instead of all the data
      ! \todo interaction should compute only Gcd and Gan... an initialization should somewhere else should compute vlBEGIN
      call get_nodal_field(cd%model_ID, p_meca_td_V, 2, vcd)
      call get_nodal_field(an%model_ID, p_meca_td_V, 2, van)
      interaction => detect_and_compute_contact_DKDKx(rough_position(:,i4(1)), rough_position(:,i4(2)), &
                                                      rough_boundary(i4(1))  , rough_boundary(i4(2))  , &
                                                      shift_cd               , shift_an               , &
                                                      vcd, van, cd%model_ID, an%model_ID, isee, 0, 0.d0 )

      if( .not. associated(interaction) ) cycle

      icdan = icdan + 1

      interaction%icdan    = icdan
      interaction%icdtac   = icdtac
      interaction%iantac   = iantac

      interaction%icdbdy   = cd%model_ID
      interaction%ianbdy   = an%model_ID

      interaction%isee     = isee
      interaction%periodic = 0

      nb_adj(icdtac)     = nb_adj(icdtac)+1
      interaction%iadj   = nb_adj(icdtac)

      interaction%rank = icdan
      call add_to_ptr_container(these(1), interaction)
      interaction => null()

    end do

    if(associated(vcd)) deallocate(vcd)
    if(associated(van)) deallocate(van)

    write(cout,'(1X,I0,1x,A)') icdan ,'contacts found'
    call logmes(cout)
    
  !     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)

  !     IF (nb_adj(ibdy) /= 0) THEN
  !        ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
  !        IF (errare /=0 ) THEN
  !           CALL FATERR(IAM,'error in allocating adjac(icdbdy)%.....')
  !        END IF
  !     ELSE 
  !        NULLIFY(adjac(ibdy)%icdan)
  !     END IF

  !  END DO
  !  
  !  !rm: must go through the list... again :s
  !  !adjac(icdtac)%icdan(current_interaction%iadj) = icdan      

  !  call reset_violation_DKDKx(icdan)

  end subroutine

  !> \brief Stock local reaction from this to verlet
  subroutine stock_rloc()
    implicit none
    type(list_data), pointer :: inter
    integer(kind=4)   :: i_tact, i_inter, nb_interactions, errare
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'interaction_handler::stock_rloc'

    nb_interactions = get_nb_interactions()

    if( allocated(verlet) .and. size(verlet) /=  nb_interactions ) deallocate(verlet)
    if( .not. allocated(verlet) ) then
      allocate( verlet(nb_interactions), stat=errare )
      if( errare /= 0 ) then
        write(cout, '(A,1x,I0,A)') 'error in allocating verlet (error', errare,')'
        call faterr(IAM,cout)
      end if
    end if

    do i_tact = 1, get_nb_contactors()
      vrlt_map(i_tact+1) = vrlt_map(i_tact) + nb_adj(i_tact)
    end do

    verlet(1:nb_interactions) = 0
    do i_inter = 1, nb_interactions
      inter => get_interaction(i_inter)
      verlet( vrlt_map(inter%icdtac) + inter%iadj ) = i_inter
    end do

    ! \warning: now these(1) which is considered 'this' is 'this' of previous detection...
    !           to prevent confuction a call to delete_this may be done
    these(1:2) = cshift( these(1:2), shift=-1 )

  end subroutine

  !> \brief Recup local reaction from verlet to this
  subroutine recup_rloc()
    implicit none
    type(list_data), pointer :: inter, v_inter
    integer(kind=4)   :: i_inter, i_adj, nb_recup
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'interaction_handler::recup_rloc'

    if( these(2)%nb_data == 0 ) return

    nb_recup = 0
    do i_inter = 1, these(1)%nb_data
      inter => get_interaction(i_inter)
      do i_adj = vrlt_map(inter%icdtac)+1, vrlt_map(inter%icdtac+1)
        v_inter => these(2)%data_array(verlet(i_adj))%data

        if( v_inter%cdan  == inter%cdan    .and. &
            v_inter%icdbdy == inter%icdbdy .and. &
            v_inter%icdtac == inter%icdtac .and. &
            v_inter%ianbdy == inter%ianbdy .and. &
            v_inter%iantac == inter%iantac       ) then

          inter%rl(:) = v_inter%rl(:) * Hrate
          inter%statusBEGIN = v_inter%status
          inter%internal(:) = v_inter%internal(:)

          nb_recup = nb_recup + 1
          exit

        end if
      end do
    end do

    write(cout,'(1X,I0,1x,A)') nb_recup ,'contacts recup'
    call logmes(cout)

  end subroutine

  !> \brief Initialize outlines pointer and return it
  function init_outlines()
    implicit none
    !> [return] pointer on outline points for all contactor
    real(kind=8), dimension(:,:),pointer :: init_outlines
    integer(kind=4) :: nb_contactors, i_tact, sz 

    nb_contactors = get_nb_contactors()

    if( nb_contactors == 0 ) then
       init_outlines => null()
       return
    end if
    
    if( associated(nb_point_outlines)) deallocate(nb_point_outlines)
    allocate(nb_point_outlines(nb_contactors+1)) 

    nb_point_outlines(1) = 0
    do i_tact = 1, nb_contactors
      nb_point_outlines(i_tact+1) = nb_point_outlines(i_tact) + get_nb_point_outline(i_tact)
    end do

    sz =  nb_point_outlines(nb_contactors+1)

    if (associated(outlines)) deallocate(outlines)
    allocate(outlines(nbDIME,sz))

    outlines = 0.D0

    init_outlines => outlines

  end function init_outlines

  !> \brief Update outline and contactor fields values
  subroutine update_postdata()
    implicit none
    integer(kind=4) :: i, j, i_mdlhdl, i_contactor
    type(T_contactor), pointer :: contactor
    real(kind=8), dimension(:,:), pointer :: coor

    coor => null()

    do i_contactor = 1, get_nb_contactors()
      contactor => get_contactor(i_contactor)
      i_mdlhdl  = contactor%model_ID

      ! not good with cluster or meshed bodies :s
      call get_coor(i_mdlhdl, 4, coor)

      i = nb_point_outlines(i_contactor)
      j = nb_point_outlines(i_contactor+1)     

      call get_outline(contactor, coor, outlines(:,i+1:j), j-i)
      if( associated(coor) ) deallocate(coor)
      coor => null()

    end do

  end subroutine

  !> \brief Get the nb_point_outlines array
  function get_nb_point_outlines()
    implicit none
    integer(kind=4), dimension(:), pointer :: get_nb_point_outlines

    get_nb_point_outlines => nb_point_outlines

  end function get_nb_point_outlines

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none
    integer(kind=4) :: i

    call erase_ptr_container(these(1))
    call erase_ptr_container(these(2))

    if( allocated(nb_adj)   ) deallocate(nb_adj)
    if( allocated(vrlt_map) ) deallocate(vrlt_map)
    if( allocated(verlet)   ) deallocate(verlet)

    if( allocated(all_mdlhdl_nodes) ) then
      do i = 1, size(all_mdlhdl_nodes)
        if( associated(all_mdlhdl_nodes(i)%G_i) ) deallocate(all_mdlhdl_nodes(i)%G_i)
      end do
      deallocate(all_mdlhdl_nodes)
    end if
    if( allocated(rigid_mdlhdl_config) ) then
      do i = 1, size(rigid_mdlhdl_config)
        if( allocated(rigid_mdlhdl_config(i)%rdata) ) deallocate(rigid_mdlhdl_config(i)%rdata)
      end do
      deallocate(rigid_mdlhdl_config)
    end if
    if( allocated(defor_mdlhdl_config) ) then
      do i = 1, size(defor_mdlhdl_config)
        if( allocated(defor_mdlhdl_config(i)%rdata) ) deallocate(defor_mdlhdl_config(i)%rdata)
      end do
      deallocate(defor_mdlhdl_config)
    end if

    if( allocated(rough_boundaries) ) then
      do i = 1, size(rough_boundaries)
        call erase_object(rough_boundaries(i))
      end do
      deallocate(rough_boundaries)
    end if

    if( allocated(rough_boundary) ) deallocate(rough_boundary)
    if( allocated(rough_position) ) deallocate(rough_position)
    if( allocated(rough_index)    ) deallocate(rough_index)

    call erase_container(rough_list)

    if( associated(nb_point_outlines) ) deallocate(nb_point_outlines)
    if( associated(outlines)          ) deallocate(outlines)

  end subroutine

end module

