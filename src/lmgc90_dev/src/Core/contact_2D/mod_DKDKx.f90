!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use,
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info".
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability.
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or
! data to be ensured and,  more generally, to use and operate it in the
! same conditions as regards security.
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================
module DKDKx

  !!  This module deals with geoemetric and kinematic operations between contactors DISKx.
  !!  In this module candidate contactors are DISKx and antagonist contactors are DISKx

  use overall
  use tact_behaviour

  use DISKx, only : get_nb_cdtac        => get_nb_diskx , &
                    get_nb_antac        => get_nb_diskx , &
                    cdtac2bdyty         => diskx2bdyty  , &
                    antac2bdyty         => diskx2bdyty  , &
                    get_ent_cdtac       => get_ent      , &
                    get_ent_antac       => get_ent      , &                    
                    get_color_cdtac     => get_color    , &
                    get_color_cdtac     => get_color    , &                    
                    get_visible_cdtac   => get_visible  , &
                    get_visible_antac   => get_visible  , &                    
                    get_coor_cdtac      => get_coor     , &
                    get_coor_antac      => get_coor     , &                    
                    get_coorTT_cdtac    => get_coorTT   , &
                    get_coorTT_antac    => get_coorTT   , &                    
                    get_shiftTT_cdtac   => get_shiftTT  , &
                    get_shiftTT_antac   => get_shiftTT  , &                    
                    get_V_cdtac         => get_V        , &
                    get_V_antac         => get_V        , &                    
                    get_Vbegin_cdtac    => get_Vbegin   , &
                    get_Vbegin_antac    => get_Vbegin   , &
                    add_reac_cdtac      => add_reac     , &
                    add_reac_antac      => add_reac     , &
                    get_vlocy_cdtac     => get_vlocy    , &
                    get_vlocy_antac     => get_vlocy    , &
                    comp_vlocy_cdtac    => comp_vlocy   , &
                    comp_vlocy_antac    => comp_vlocy   , &
                    nullify_reac_cdtac  => nullify_reac , &
                    nullify_reac_antac  => nullify_reac , &
                    nullify_vlocy_cdtac => nullify_vlocy, &
                    nullify_vlocy_antac => nullify_vlocy, &
                    ! is_diskx_same_bdyty                 , &
                    ! get_mean_radius_DISKx               , &
                    ! get_max_radius_DISKx                , &
                    ! get_min_radius_DISKx                , &
                    get_mass_cdtac     => get_mass_DISKx, &
                    get_mass_antac     => get_mass_DISKx, &
                    !
                    !old fashion -- RIP
                    !
                    get_nb_diskx, diskx2bdyty           , &
                    print_info_diskx, get_radius_DISKx  , &
                    get_ent_diskx       => get_ent      , &
                    get_color_diskx     => get_color    , &
                    get_visible_diskx   => get_visible  , &
                    get_coor_diskx      => get_coor     , &
                    get_coorTT_diskx    => get_coorTT   , &
                    get_shiftTT_diskx   => get_shiftTT  , &
                    get_V_DISKx         => get_V        , &
                    get_Vbegin_DISKx    => get_Vbegin   , &
                    add_reac_diskx      => add_reac     , &
                    get_vlocy_diskx     => get_vlocy    , &
                    comp_vlocy_diskx    => comp_vlocy   , &
                    nullify_reac_diskx  => nullify_reac , &
                    nullify_vlocy_diskx => nullify_vlocy, &
                    is_diskx_same_bdyty                 , &
                    get_mean_radius_DISKx               , &
                    get_max_radius_DISKx                , &
                    get_min_radius_DISKx                , &
                    get_mass_DISKx                      , &
                    get_ws_diskx                        , &
                    add_betai_to_diskx                  , &
                    get_Vd_diskx                        , &
                    add_stress_diskx                    , &
                    update_status_sector_diskx          , &
                    get_tact_id, all_dof_driven_DISKx

  use anonymous_ptr_container, only : get_object               => get_data                   , &
                                      get_nb_objects           => get_nb_data                , &
                                      close_container          => close_ptr_container        , &
                                      add_object_to_container  => add_object_to_ptr_container, &
                                      display_object_container => display_ptr_container      , &
                                      ptr_container
  use anonymous

  use parameters, only : i_dkdkx, i_diskx, &
                         i_undefined, i_free, i_sheared, i_stationary, &
                         i_IQS_MAC_CZM,i_MAC_CZM,i_IQS_WET_CZM

!  use interaction_2D, T_interaction    , &
!                      T_tact2tact      , &
!                      set_nb_face2faces, &
!                      initialize_interactions

  !vv pour appel dans get_g2g et get_sym_g2g
  use RBDY2, only : get_ptr_mass, get_nb_RBDY2, &
                    is_vlocy_drvdof_RBDY2     , &
                    initialize_status_sectors , &
                    get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color
  use MAILx, only : get_color_MAILx

  use inter_meca_2D

  implicit none

  private

  ! contact element type DKDKx --------------------------------------------

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  integer(kind=4) :: nb_DKDKx  ! nb_DKDKx = number of selected candidates DISKx against DISKx
                               ! <= size(this).

  integer(kind=4) :: nb_vDKDKx

  
  !------------------------------------------------------------------------ 

 type T_visavis

    integer(kind=4) :: cd,an
    integer(kind=4) :: isee,nb_ctc
    logical :: FREE

    real(kind=8) :: gapREF

    real(kind=8),dimension(2):: Icoorcd,coorcd
    real(kind=8),dimension(2):: Icooran,cooran


 end type T_visavis

 type(T_visavis),dimension(:),allocatable:: visavis
 type(T_visavis) :: VAVNULL

 integer(kind=4) :: nb_visavis = 0 , nb_MAX_VAV = 0 , NBN_VAV = 0
 logical :: WITH_VAV = .false.
 logical :: FIRST_TIME_VAV = .true.

!------------------------------------------------------------------------ 


!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer(kind=4), dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs DISKx-DISKx
                                                                 !                  to candidate contactor DISKx icdtac.
!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------
!------------------------------------------------------------------------ 

 type T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_DKDKx.
      
   integer(kind=4)                          :: popul  ! box(ibox1,ibox2)%popul: number of DISKx in box ibox1,ibox2;
   
   integer(kind=4), dimension(:), pointer   :: which  ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of DISKx labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 end type T_box 

 type(T_box), dimension(:,:),allocatable    :: box    ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

 type T_rough_DKDKx  
                                                      ! définit le type de la liste des plus proches voisins
    integer(kind=4)          :: cd                    ! le candidat, l'antagoniste et isee pour la loi de contact
    integer(kind=4)          :: an
    integer(kind=4)          :: isee

                                                      ! indique des informations complémentaires sur le contact
    integer(kind=4)          :: group                 ! actuellement : INTRF et NOINT pour INTERFACE et NOT INTERFACE

    !!! > md > !!!
    real(kind=8)             :: meff,reff             ! effective mass and radius for md method 
    !!! < md < !!!

    integer(kind=4)          :: periodic              ! periodic contact flag

 end type T_rough_DKDKx
 
 type(T_rough_DKDKx),dimension(:),allocatable   :: rough_DKDKx     ! table  de visibilité

 type T_link_rough_DKDKx                                           ! liste chainée pour determiner les listes de cand_ant car
                                                                   ! on ne connait pas a priori le nb de cand-ant 
    type(T_link_rough_DKDKx), pointer :: p                         ! pointeur sur le precedent
    type(T_rough_DKDKx)               :: val                       ! les valeurs
    type(T_link_rough_DKDKx), pointer :: n                         ! pointeur sur le suivant

 end type T_link_rough_DKDKx
 
 type(T_link_rough_DKDKx),pointer                    :: Root,Current,Previous

!--------------------------------------------------------------------------

 integer(kind=4)           :: nb_WSsect = 1
 integer(kind=4),parameter :: i_max_friction=0,i_min_friction=1,i_average_friction=2
 integer(kind=4)           :: i_friction_model = 2
 
 real(kind=8),dimension(:,:),allocatable :: betaiDISKx 
 logical :: first_time_betai=.true.

 !--------------------------------------------------------------------------
 integer(kind=4)                            :: Nstep_rough_seek_DKDKx=1
 logical                                    :: write_creation_tab_visu

!--------------------------------------------------------------------------

 logical                                    :: PERIODIC=.false.
 real(KIND=8)                               :: PERIODE = 0.d0
 integer(kind=4)                            :: nb_PERIODIC_DKDKx
 integer(kind=4),dimension(:),allocatable   :: periodic_DKDKx

!------------------------------------------------------------------------
! variables attached to surrounding boxes

 real (kind=8)  :: maxray, minray, maxalert, meanradius
 real (kind=8)  :: Lbox,LBox_1,norm
 integer(kind=4) :: nb_rough_DKDKx
 integer(kind=4) :: nb_recup_DKDKx
 integer(kind=4) :: maxpopul

!------------------------------------------------------------------------ 
! managing big diskx

 real(kind=8)                             :: big_ratio=5.d0
 integer(kind=4)                          :: nb_big_diskx
 integer(kind=4),dimension(:),allocatable :: big_diskx_list
 integer(kind=4),dimension(:),allocatable :: iamABigDISKx

!------------------------------------------------------------------------
!
 real(kind=8)                                      :: Reac_DKDKx_MAX=0.D0
 real(kind=8), dimension(:)  , allocatable, target :: violation
 real(kind=8), dimension(:,:), allocatable, target :: DKcoor  !  coordinates of bodies owning DISKx to be used in selecting prox tactors
!------------------------------------------------------------------------
 
 type T_ENERGY
    real(kind=8) :: failure
    real(kind=8) :: damage
    real(kind=8) :: stored
    real(kind=8) :: cohesion
 end type T_ENERGY

 type(T_ENERGY),dimension(:),allocatable :: energyDKDKx

!------------------------------------------------------------------------

 logical :: RUN=.false.

 logical :: module_checked_ = .FALSE.
 logical :: check_DKDKx_    = .FALSE.


 logical :: skip_creation_tab_visu = .false.
!------------------------------------------------------------------------
! public functions 
!
 public :: &
      stock_rloc_DKDKx, &
      recup_rloc_DKDKx, &
      smooth_computation_DKDKx, &
      compute_box_DKDKx, &
      read_ini_Vloc_Rloc_DKDKx, &
      read_ini_contact_snapshot_sample_DKDKx, &
      write_xxx_Vloc_Rloc_DKDKx, &
      !write_out_one_Vloc_Rloc_DKDKx, &
      set_periodic_data_DKDKx, &
      set_friction_model_DKDKx, &
      coor_prediction_DKDKx, &
      creation_tab_visu_DKDKx, &
      compute_contact_DKDKx, &
      display_prox_tactors_DKDKx, &
      RUN_DKDKx, &
      CHECK_DKDKx, &
      get_write_Vloc_Rloc_DKDKx, &
      set_anonymous_to_rough,    &
      set_interactions_to_rough,    &
      interface_creation_tab_visu_DKDKx

 public :: nullify_reac_DKDKx,                                     &
           nullify_vlocy_DKDKx,                                    &
           injj_DKDKx, prjj_DKDKx, vitrad_DKDKx,                   & 
           get_nb_DKDKx,                                           &
           DKDKx2DISKx,                                            &
           get_periode_DKDKx,                                      &
!!         get_Wik_DKDKx,compute_Wikik_DKDKx,compute_Wikjl_DKDKx,  &
           get_length_DKDKx, &
           print_info_DKDKx,   &
           get_g2l_DKDKx,                                          &
!!! > md > !!!
           get_eff_DKDKx,update_cohe_DKDKx, update_fric_DKDKx, &
!!! < md < !!!
           detect_and_compute_contact_DKDKx, &
           get_rough_DKDKx                 , &
           reset_violation_DKDKx           , &   
           reset_nb_adj_DKDKx              , &
           add_adj_DKDKx                   , &
           compute_one_contact_DKDKx       , &
           !compute_contacts_in_t2t_DKDKx   , &
           get_icdtac_DKDKx, &
           get_iantac_DKDKx, &
           get_nb_INTRF_DKDKx, &
           get_list_INTRF_DKDKx, &
           get_old_index_DKDKx,set_vav_DKDKx, &
           get_g2g_DKDKx,get_size_g2g_DKDKx,  &
           get_sym_g2g_DKDKx,get_size_sym_g2g_DKDKx,  &
           set_surface_sectors_DKDKx, &
           update_WS_sector_DKDKx, &
           compute_stress_DKDKx, &
           compute_betai_DKDKx, &
           ! vv
           clean_memory_DKDKx, &
           compute_czm_energy_DKDKx, &
           get_CZM_energy_DKDKx

 !rm for handler
 public get_this    , &
        set_nb_DKDKx, &
        redo_nb_adj_DKDKx, &
        get_an_tacty     , &
        get_verlet_tact_lawnb

contains

  include 'interaction_common.f90'
  ! defines the following subroutines
  !subroutine get_behaviour_( icdan, see, tact_behav )
  !subroutine get_this(this_inter, verlet_inter, violation_inter)
  !function get_an_tacty(i_mdl, i_bdy, i_tac)
  !subroutine redo_nb_adj_( nb_cd )
  !subroutine new_verlet_(icdtac, size, errare)
  !subroutine free_verlet_(icdtac)
  !subroutine nullify_verlet_(icdtac)
  !subroutine clean_memory_inter_meca_()
  include 'interaction_common_2D.f90'
  ! defines the following subroutines
  !function get_verlet_tact_lawnb( icdtac, iadj )

!------------------------------------------------------------------------

!------------------------------------------------------------------------
!------------------------------------------------------------------------

  !> \brief Recup a tempory solution to perform contact detection
  subroutine coor_prediction_DKDKx
    implicit none
    integer(kind=4) :: itact,errare
    integer(kind=4) :: nb_DISKx

    nb_DISKx = get_nb_DISKx()

    if (smooth_method) then
       do itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coor_DISKx(itact)
       end do
    else 
       do itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coorTT_DISKx(itact)
          
          if (PERIODIC) then
             if (DKcoor(1,itact)  > periode) then
                !print*,'on corrige le DISKx ',itact,' qui sort par x+'
                DKcoor(1,itact) = DKcoor(1,itact) - periode
             else if (DKcoor(1,itact) < 0.D0) then
                !print*,'on corrige le DISKx ',itact,' qui sort par x-'
                DKcoor(1,itact) = DKcoor(1,itact) + periode
             end if
          end if
       end do
    end if

  end subroutine coor_prediction_DKDKx

  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_DKDKx(step)
    implicit none
    integer(kind=4), intent(in) :: step
    
    G_nfich=get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(:))))
    else
      open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
    end if

    if ( get_with_experimental_dev() ) then
      call experimental_read_ini_Vloc_Rloc
    else
      call read_ini_Vloc_Rloc
    endif

    close(G_nfich)
    
  end subroutine read_ini_Vloc_Rloc_DKDKx

  !> \brief Read Vloc_Rloc.INI file
  subroutine read_ini_contact_snapshot_sample_DKDKx
    implicit none

    G_nfich=get_io_unit()
    open(unit=G_nfich,file=trim(location(post_contact_sample(:))))
    call read_ini_contact_snapshot_sample
    close(G_nfich)
    
  end subroutine read_ini_contact_snapshot_sample_DKDKx

  !> \brief Write Vloc_Rloc.OUT file
  subroutine write_xxx_Vloc_Rloc_DKDKx(which)
    implicit none
    integer(kind=4),intent(in) :: which
    integer(kind=4)            :: nfich,lc
    
    nfich = get_io_unit()
    
    select case(which)
    case(1)
       lc = len_trim(out_Vloc_Rloc)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(2)
       lc = len_trim(last_Vloc_Rloc)
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Vloc_Rloc(1:lc))))
       call write_out_Vloc_Rloc(nfich)
       close(nfich)
    case(6)
       call write_out_Vloc_Rloc(6)
    end select
    
  end subroutine write_xxx_Vloc_Rloc_DKDKx
  
  !> \brief Set period value for periodic conditions in the x direction
  subroutine set_periodic_data_DKDKx(per,FLAG)
    implicit none
    real(kind=8) :: per
    logical      :: FLAG
    
    periode  = per
    PERIODIC = FLAG
    
  end subroutine set_periodic_data_DKDKx

   !> \brief
  subroutine set_friction_model_DKDKx(FLAG)
    implicit none
    character(len=3)  :: FLAG
    character(len=29) :: IAM
    IAM = 'mod_DKDKx::set_friction_model'
    
    select case(FLAG)
    case('min')
       i_friction_model = i_min_friction
    case('max')
       i_friction_model = i_max_friction
    case('ave')
       i_friction_model = i_average_friction
    case default
       call FATERR(IAM,'the model is not defined')
    end select
    
  end subroutine set_friction_model_DKDKx
  
  !> \brief Size and initialize boxes used in contact detection
  subroutine compute_box_DKDKx
    implicit none
    integer(kind=4)    :: isee,errare,ibdy,jbdy,itact,ifound
    integer(kind=4)    :: nb_DISKx
    real(kind=8)       :: tmp_radius
    character(len=22)  :: IAM
    character(len=103) :: cout

    IAM = 'mod_DKDKx::compute_box'
    ! on fait ici les choses qui ne doivent que lorsque nb_DISKx change

    nb_DISKx=get_nb_DISKx()    

    minray     = get_min_radius_DISKx()
    maxray     = get_max_radius_DISKx()
    meanradius = get_mean_radius_DISKx()
    
    ! TODO: defined a function for the test of tmp_radius
    !
    if (allocated(iamABigDISKx)) deallocate(iamABigDISKx)
    allocate(iamABigDISKx(nb_DISKx),stat=errare)
    iamABigDISKx = 0
    !
    ! big diskx case
    ! 1. Size big diskx container
    ! 

    nb_big_DISKx = 0
    tmp_radius=big_ratio*meanradius
    do ibdy=1,nb_DISKx
       if ( get_radius_DISKx(ibdy) > tmp_radius ) then
          nb_big_DISKx = nb_big_DISKx + 1
          iamABigDISKx(ibdy) = 1
       end if
    end do

    !print*,'nb_big_diskx=',nb_big_diskx

    !
    ! 2. Filled big diskx container
    !

    if ( nb_big_diskx .ne. 0 )then
       if ( allocated(big_diskx_list) ) deallocate(big_diskx_list)
       allocate( big_diskx_list(nb_big_DISKx) )

       nb_big_DISKx = 0
       do ibdy=1,nb_DISKx
          if ( iamABigDISKx(ibdy) .eq. 1 ) then
             nb_big_diskx = nb_big_diskx + 1
             big_diskx_list(nb_big_DISKx) = ibdy
          end if
       end do

    !
    ! 3. Modification of minray, maxray and mean_radius
    !    without big diskx
    !
       minray     =  1.d20
       maxray     = -1.d20
       meanradius =  0.d0
       
       do ibdy=1,nb_DISKx

          if( iamABigDISKx(ibdy).eq.1 ) cycle

          tmp_radius=get_radius_DISKx(ibdy)
          minray     = min(tmp_radius,minray)
          maxray     = max(tmp_radius,maxray)
          meanradius = meanradius + tmp_radius

       end do
       
       meanradius = meanradius/(nb_DISKx-nb_big_DISKx)

    end if
    !
    ! end of big diskx case
    !
    if (minray > maxray ) then
       write(cout,'(A42)') 'messing error computing minray and maxray'
       call FATERR(IAM,cout)
    end if
    
    ! computing largest alert distance between disks 
    maxalert=0.D0  
    do isee=1,size(see)
       if (see(isee)%cdtac .eq. 'DISKx' .and. see(isee)%antac .eq. 'DISKx') then
          maxalert=max(maxalert,see(isee)%alert)
       end if
    end do
    
    Lbox   = 1.01D0*(2.D0*maxray + maxalert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/minray
    
    maxpopul = (1+int(norm))*(1+int(norm))
    !
    ! for each box maxpopul is less than the total number of DISKx 
    !
    maxpopul=min(maxpopul,nb_DISKx)

    if (.not. allocated(adjac))then
       allocate(adjac(nb_DISKx),stat=errare)
       if ( errare.ne.0 ) then
          write(cout,'(A25)') 'error in allocating adjac'
          call FATERR(IAM,cout)
       end if
       do ibdy=1,nb_DISKx
          nullify(adjac(ibdy)%icdan)
       end do
    else
       do ibdy=1,nb_DISKx
          if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
          nullify(adjac(ibdy)%icdan)
       end do
    end if
    
    if (allocated(nb_adj)) deallocate(nb_adj)
    allocate(nb_adj(nb_DISKx),stat=errare)
    if ( errare.ne.0 ) then
       write(cout,'(A25)') 'error in allocating nb_adj'
       call FATERR(IAM,cout)
    end if
    
    nb_adj = 0
    
    ! DKcoor are coordinates of bodies owning DISKx to be used in selecting prox tactors
    if (allocated(DKcoor)) deallocate(DKcoor)
    allocate(DKcoor(3,nb_DISKx),stat=errare)

  end subroutine compute_box_DKDKx

  !> \brief Create the visibility table for contact detection
  subroutine creation_tab_visu_DKDKx
    implicit none

    if( skip_creation_tab_visu ) return

    if (get_with_experimental_dev()) then
      call experimental_creation_tab_visu
    else
      call creation_tab_visu
    endif
  
  end subroutine creation_tab_visu_DKDKx

  !> \brief 
  subroutine set_anonymous_to_rough(anonymous_rough,nb_anonymous_rough)
    implicit none
    type(PTR_CONTAINER), intent(in)   :: anonymous_rough
    integer(kind=4)    , intent(in)   :: nb_anonymous_rough
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Arguments de sortie de la subroutine
   ! argument implicite car global au module :
   ! type(T_rough_DKDKx), dimension(:), allocatable :: rough_DKDKx 
   ! integer(kind=4)                                :: nb_rough_DKDKx 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Arguments internes à la subroutine
   integer                                 :: icdan,icdtac,iantac,isee,group
   integer                                 :: nb_rough_tmp
   real(kind=8)                            :: masscd,massan,raycd,rayan
   character(len=5)                        :: ancol,cdcol
   integer(kind=4),  dimension(:), pointer :: cdan              ! joue le role du "i4" de la structure container
   type(T_object)                          :: anonymous_contact ! pour acceder au contenu de anonymous_rough
   character(len=103)                      :: cout


   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue une zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   nullify(Root)
   nullify(Current)
   nullify(Previous)

   ! On va élimininer les contacts entre contacteurs d'un même corps et/ou ne se voyant pas.
   ! D'où nb_rough_DKDKx < ou = nb_anonymous_rough
   nb_rough_DKDKx=0
   do icdan=1,nb_anonymous_rough
      
      ! On récupère l'objet anonymous_contact d'indice icdan
      anonymous_contact = get_object(anonymous_rough,icdan)    
      ! On récupère la paire num_candidat/num_antagoniste dans l'objet "anonymous_contact"                
      cdan => get_i4_vector(anonymous_contact)
      icdtac=cdan(1)
      iantac=cdan(2)

      ! On récupère la visibilité des contacteurs
      cdcol = get_color_DISKx(icdtac)
      ancol = get_color_DISKx(iantac)
      if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
        isee  = get_isee_specific('DISKx',cdcol,ancol)
      else
        isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                        get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
      end if

      ! Si les contacteurs se voient et n'appartiennent pas au même corps, on continu!
      if (isee==0 .or. is_DISKx_same_BDYTY(icdtac,iantac) ) cycle

      !Incrémentation du nombre de contacts grossiers
      nb_rough_DKDKx=nb_rough_DKDKx+1

      ! On récupère le goupe du candidat et de l'antagoniste dans l'objet "anonymous_contact"                
      group=cdan(3)
      
      if ( nb_rough_DKDKx == 1) then
         allocate(Root)
         Current => Root
         nullify(Root%p)
      else
         allocate(Current)
         Previous%n => Current
      endif
   
      ! On rempli la liste chaînée avec les données de anonymous_rough + isee
      Current%val%cd         = icdtac
      Current%val%an         = iantac
      Current%val%isee       = isee
      Current%val%group = group
      ! Je n'ai pas traité le cas périodique
      Current%val%periodic = 0   
      Current%p => Previous
      nullify(Current%n)
      Previous => Current
     
   end do

   write(cout,'(4X,I10,A20)') nb_rough_DKDKx,' DKDKx roughly found'
   call logmes(cout)

   if (allocated(periodic_DKDKx)) deallocate(periodic_DKDKx)
   allocate(periodic_DKDKx(nb_rough_DKDKx))

   if (allocated(rough_DKDKx)) deallocate(rough_DKDKx)
   allocate(rough_DKDKx(nb_rough_DKDKx))     ! the visibility array used in compute_contact is allocated

   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_DKDKx))            ! the oversized array this is temporaly allocated

   ! Ayant le bon nombre de contacts grossiers et rempli la liste chaînée, on peut la dérouler
   ! pour obtenir la structure rough_DKDKx attedue pour la détection fine.  
   do icdan=nb_rough_DKDKx,1,-1
      
      Previous => Current%p
      rough_DKDKx(icdan)%cd         = Current%val%cd
      rough_DKDKx(icdan)%an         = Current%val%an
      rough_DKDKx(icdan)%isee       = Current%val%isee
      rough_DKDKx(icdan)%group      = Current%val%group
      rough_DKDKx(icdan)%periodic   = Current%val%periodic
      
!!! > md > !!!
!!! a modifier pour des corps non-convexes
     
      raycd = get_radius_DISKx(Current%val%cd)
      rayan = get_radius_DISKx(Current%val%an)
      
      rough_DKDKx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))
      massan=get_mass_DISKx(diskx2bdyty(1,Current%val%an))
      
      rough_DKDKx(icdan)%meff = masscd*massan/(masscd+massan)
      
      deallocate(Current)
      Current => Previous
   end do
   
   nullify(Root)
  end subroutine set_anonymous_to_rough
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  subroutine set_interactions_to_rough(interactions,nb_interactions)
   implicit none

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Arguments d'entree de la subroutine 
   integer(kind=4)                              , intent(in)   :: nb_interactions
   integer(kind=4), dimension(4*nb_interactions), intent(in)   :: interactions
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Arguments de sortie de la subroutine
   ! argument implicite car global au module :
   ! type(T_rough_DKDKx), dimension(:), allocatable :: rough_DKDKx 
   ! integer(kind=4)                                :: nb_rough_DKDKx 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! Arguments internes à la subroutine
   integer                                 :: icdan,icdtac,iantac,isee,group
   character(len=5)                        :: cdcol, ancol
   real(kind=8)                            :: masscd,massan,raycd,rayan
   character(len=103)                      :: cout

   skip_creation_tab_visu = .true.

   nb_rough_DKDKx=nb_interactions

   write(cout,'(4X,I10,A20)') nb_rough_DKDKx,' DKDKx roughly found'
   call logmes(cout)

   if (allocated(periodic_DKDKx)) deallocate(periodic_DKDKx)
   allocate(periodic_DKDKx(nb_rough_DKDKx))

   if (allocated(rough_DKDKx)) deallocate(rough_DKDKx)
   allocate(rough_DKDKx(nb_rough_DKDKx))     ! the visibility array used in compute_contact is allocated

   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_DKDKx))            ! the oversized array this is temporaly allocated

   ! Ayant le bon nombre de contacts grossiers et rempli la liste chaînée, on peut la dérouler
   ! pour obtenir la structure rough_DKDKx attedue pour la détection fine.  
   do icdan=1,nb_rough_DKDKx

      icdtac=interactions(4*(icdan-1)+1)
      iantac=interactions(4*(icdan-1)+2)
      cdcol = get_color_DISKx(icdtac)
      ancol = get_color_DISKx(iantac)
      if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
        isee  = get_isee_specific('DISKx',cdcol,ancol)
      else
        isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                        get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
      end if

      rough_DKDKx(icdan)%cd         = icdtac
      rough_DKDKx(icdan)%an         = iantac
      rough_DKDKx(icdan)%isee       = isee
      rough_DKDKx(icdan)%group      = interactions(4*(icdan-1)+3)
      rough_DKDKx(icdan)%periodic   = interactions(4*(icdan-1)+4)
      
!!! > md > !!!
!!! a modifier pour des corps non-convexes
     
      raycd = get_radius_DISKx(icdtac)
      rayan = get_radius_DISKx(iantac)
      
      rough_DKDKx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_DISKx(diskx2bdyty(1,icdtac))
      massan=get_mass_DISKx(diskx2bdyty(1,iantac))
      
      rough_DKDKx(icdan)%meff = masscd*massan/(masscd+massan)
      
   end do
   
  end subroutine set_interactions_to_rough
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
  subroutine experimental_creation_tab_visu
   
    implicit none 
   
    integer                     :: errare 
    
    integer                     :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
    integer                     :: icdan,iadj,ibdy,icdbdy,ianbdy,itac,icdtac,iantac,isee,itacty   
    real(kind=8)                :: Bleft,Bright,Bup,Bdown
    character(len=5)            :: cdtac,cdcol,antac,ancol
    real(kind=8),dimension(3)   :: coord,coordcd,coordan 
    real(kind=8)                :: raycd,rayan,adist,dist,nonuc,gapT
    real(kind=8)                :: masscd,massan
    logical                     :: is_allocated_yet
    integer                     :: minibox1=0,maxibox1=0,minibox2=0,maxibox2=0
    character(len=103)          :: cout
    character(len=28)           :: IAM = 'mod_DKDKx::creation_tab_visu'
   

    integer :: nb_DISKx

    nb_DISKx = get_nb_DISKx()

    is_allocated_yet=.false.

    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of disks and largest box containing disks.
    !
    ! The computation of maximal radius of disks allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficacious when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of disks is also used, in order to estimate the maximal 
    ! number of disks per box.   
    ! This quick sorting method may be applied to bodies other than disks, such as 
    ! ellipsoidal or polygonal bodies with a reasonable aspect ratio, less than 
    ! 3 or even 5. Such bodies are enclosed in disks with radius max_radius, and 
    ! enclosing a disk with radius min_radius. The sorting algorithm may then be 
    ! straightforwardly applied.
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Bleft    =  1.D24
    Bright   = -1.D24
    Bup      = -1.D24
    Bdown    =  1.D24
    
    do ibdy=1,nb_DISKx
       if (.not.get_visible_DISKx(ibdy)) cycle
       
       coord = DKcoor(1:3,ibdy)
       Bleft = min(coord(1),Bleft )
       Bright= max(coord(1),Bright)
       Bup   = max(coord(2),Bup   )
       Bdown = min(coord(2),Bdown )
    end do
 

   !fd A VOIR le 03/01/08 les commentaires qui suivent son faux !!
   !
   ! Box size is defined to be largest (1%) than maxray+maxalert so as to
   ! ensure that all contacts may be detected.
   !
   ! Lbox1=0.101D+01*(maxray+0.5D0*maxalert)
   ! Lbox2=0.101D+01*0.5D0*dsqrt(2.D0)*(maxray+minray+maxalert)
   ! Lbox=max(Lbox1,Lbox2)
   !
   ! A box is located by pairs of integer numbers (ibox1,ibox2), where 
   ! ibox1 is the column number of the box, ibox2 the layer number of the box.
   ! 
   !
   !      ____ ____ ____ ____ ____ 
   !     |    |    |    |    |    |
   !  3  |    |    |    |    |    |   
   !     |____|____|____|____|____| 
   !     |    |    |    |    |    |
   !  2  |    |    |    |    |    |    ibox2
   !     |____|____|____|____|____| 
   !     |    |    |    |    |    |     
   !  1  |    |    |    |    |    |    
   !     |____|____|____|____|____| 
   !     
   !       1    2    3    4    5
   !
   !             ibox1
   !
   ! Coordinates of lower left and upper right corners of a (ibox1,ibox2) box are:
   !
   ! lowerleft  = ( Bleft + (ibox1-1)*Lbox , Bleft + (ibox2-1)*Lbox )
   ! upperright = ( Bdown +  ibox2   *Lbox , Bdown +  ibox2   *Lbox
   !
   ! An oversize covering of the big box (Bright,Bdown,Bleft,Bup) containing all disks 
   ! is the collection of elementary boxes such that, 
   !
   ! -1 .le. ibox1 .le. 1 + AINT((Bleft-Bright)/Lbox) ,
   ! -1 .le. ibox2 .le. 1 + AINT((Bup  -Bdown )/Lbox) . 
   !  

    if ( PERIODIC ) then
       !     print*,'Bright',Bright,'Bleft',Bleft,' Lbox ',Lbox
       !     print*,INT(periode*Lbox_1)*Lbox,(1+(INT(periode)*Lbox_1))*Lbox
       
       if (Bright > periode) then
          print*,'Bright ',Bright,' Periode ',periode
          call FATERR(IAM,'the max right coordinate is greater than the periode')
          !     else if (Bright < periode - LBox) then
          !       print*,'Bright ',Bright,' Periode ',periode,' Lbox ',Lbox
          !       call FATERR(IAM,'the max right column will be empty !')
       endif
       
       if (Bleft < 0.d0) then
          print*,'Bleft ',Bleft
          call FATERR(IAM,'the min left coordinate is less than zero')
          !     else if (Bleft > LBox) then
          !       print*,'Bleft ',Bleft,' Lbox ',Lbox
          !       call FATERR(IAM,'the min left column will be empty !')
       endif
       
       !fd pas sur que ca soit si malin ...
       !     if (INT(periode*Lbox_1)*Lbox > Bright) then
       !       Bright = periode - LBox !+ maxalert
       !     else
       Bright = periode
       !     endif
       Bleft  = 0.d0    !- maxalert
    end if
    
    minibox1 = 1
    maxibox1 = 1 + int((Bright-Bleft)*Lbox_1)
    minibox2 = 1
    maxibox2 = 1 + int((Bup - Bdown )*Lbox_1)
    !   

    ! we try to allocate only if necessary

    errare = 0
    if (allocated(box) .and. &
       (maxibox1 > size(box,dim=1) .or. maxibox2 > size(box,dim=2))) then
       do ibox1=minibox1,size(box,dim=1)
          do ibox2=minibox2,size(box,dim=2)
             if ( associated(box(ibox1,ibox2)%which) ) deallocate( box(ibox1,ibox2)%which )
          end do
       end do
       deallocate(box)

       allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)

       is_allocated_yet =.true.

    else if (.not. allocated(box)) then
       allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)
       is_allocated_yet =.true.
    endif    

    if (errare /=0 ) then
       write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       call LOGMES(cout)
       write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
       call LOGMES(cout)
       call FATERR(IAM,'error allocating box')
    end if
    
    do ibox1=minibox1,maxibox1
       do ibox2=minibox2,maxibox2
          box(ibox1,ibox2)%popul=0
          if (is_allocated_yet) then
            allocate(box(ibox1,ibox2)%which(maxpopul),stat=errare)
            if (errare /=0 ) then
               call FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%which')
            end if
          endif
          box(ibox1,ibox2)%which=0
       end do
    end do
    
   ! filling boxes with disks
   ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%which(ipopul) is the rank of body DISKx labelled ipopul in the box
  
   ! filling boxes   

   do ibdy=1,nb_DISKx
      coord=DKcoor(1:3,ibdy)
      
      if (.not.get_visible_DISKx(ibdy)) cycle
      ibox1=1+int((coord(1)-Bleft )*Lbox_1)
      ibox2=1+int((coord(2)-Bdown )*Lbox_1)
      if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
         call LOGMES(cout)
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2
         call LOGMES(cout)
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
         call LOGMES(cout)
         write(cout,'(A13,I10,A13)') '  body DISKx ',ibdy,' out of boxes'
         call FATERR(IAM,cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5)
      end if
      box(ibox1,ibox2)%popul = box(ibox1,ibox2)%popul+1
      box(ibox1,ibox2)%which(box(ibox1,ibox2)%popul) = ibdy
   end do

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
   
   nb_rough_DKDKx = 0
   
   ! creation de la liste de paire a examiner
  
   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue une zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   nullify(Root)
   nullify(Current)
   nullify(Previous)
  

!fd A VOIR le 03/01/08 on pourrait diminuer le nombre de tests en gerant 
!fd le test if iantac <= icdtac cycle.
!fd c'est ahurissant !!

   do ibox1cd = minibox1,maxibox1  
      do ibox2cd = minibox2,maxibox2
         do icdpop = 1,box(ibox1cd,ibox2cd)%popul
            icdtac = box(ibox1cd,ibox2cd)%which(icdpop)
            cdcol = get_color_DISKx(icdtac)
            ! box loop investigating antagonist diskx
            do ibox1an = max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                        
               do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
                  do ianpop = 1,box(ibox1an,ibox2an)%popul
                     iantac = box(ibox1an,ibox2an)%which(ianpop)
                     if (iantac .le. icdtac .or. is_DISKx_same_BDYTY(icdtac,iantac)) cycle
                     ancol = get_color_DISKx(iantac)
                     if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                        isee  = get_isee_specific('DISKx',cdcol,ancol)
                     else
                        isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                        get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                     end if
                     
                     if ( isee /= 0 ) then
                        adist = see(isee)%alert 
                        ! checking ROUGHLY distance against alert distance           
                        coordcd = DKcoor(1:3,icdtac)
                        coordan = DKcoor(1:3,iantac)
                        raycd   = get_radius_DISKx(icdtac)
                        rayan   = get_radius_DISKx(iantac)

                        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                        ! results might be different up to some non significant figures, but when comparing to
                        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

                        adist = 0.1005D+01*adist+raycd+rayan

                        if (       dabs(coordcd(1)-coordan(1)) <= adist &
                             .and. dabs(coordcd(2)-coordan(2)) <= adist) then

                           nb_rough_DKDKx=nb_rough_DKDKx+1

                           if ( nb_rough_DKDKx == 1) then
                              allocate(Root)
                              Current => Root
                              nullify(Root%p)
                           else
                              allocate(Current)
                              Previous%n => Current
                           endif
                           Current%val%cd       = icdtac
                           Current%val%an       = iantac
                           Current%val%isee     = isee
                           Current%val%periodic = 0

                           Current%p => Previous
                           nullify(Current%n)
                           Previous => Current
                        end if
                     end if
                  end do
               end do
            end do
         end do
      end do
   end do
   
   nb_PERIODIC_DKDKx = 0
   
   if ( PERIODIC ) then

!      print*,'on teste le periodic entre les colonnes',maxibox1,' et ',minibox1
      
      !fd 03/01/08 le dernier-1 est necessaire car le decoupage en boites est approximatif donc
      !fd il peut y des cas ou la derniere colonne (qui contient la limite de periodicite)
      !fd est vide alors que la dernier-1 colonne contient des objets dont la frontiere passe de l'autre cote

      do ibox1cd = maxibox1-1,maxibox1
      do ibox2cd = minibox2,maxibox2

!         print*,'boite candidate ',ibox2cd,' population ',box(ibox1cd,ibox2cd)%popul

         do icdpop = 1,box(ibox1cd,ibox2cd)%popul
            
            icdtac = box(ibox1cd,ibox2cd)%which(icdpop)
            cdcol  = get_color_DISKx(icdtac)
            ! box loop investigating antagonist diskx
            !fd 03/01/08 A VOIR je ne comprends pas le premier+1 !?
            do ibox1an = minibox1,minibox1+1
            do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
!               print*,'boite antagoniste ',ibox2an,' population ',box(ibox1an,ibox2an)%popul

               do ianpop = 1,box(ibox1an,ibox2an)%popul
                  
                  iantac = box(ibox1an,ibox2an)%which(ianpop)
                  
!fd 03/01/08 A VOIR manque le test ?
!fd IF (is_DISKx_same_RBDY2(icdtac,iantac)) CYCLE

                  ancol = get_color_DISKx(iantac)
                  if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                    isee  = get_isee_specific('DISKx',cdcol,ancol)
                  else
                    isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                    get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                  end if
                  
                  if (isee /= 0 ) then
                     adist   = see(isee)%alert 
                     ! checking ROUGHLY distance against alert distance           
                     coordcd = DKcoor(1:3,icdtac)
                     coordan = DKcoor(1:3,iantac)
                     raycd   = get_radius_DISKx(icdtac)
                     rayan   = get_radius_DISKx(iantac)

                     coordan(1) = coordan(1) + periode
                     
                     ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                     ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                     ! results might be different up to some non significant figures, but when comparing to
                     ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                     adist = 0.1005D+01*adist+raycd+rayan
                     if (       dabs(coordcd(1)-coordan(1)) <= adist &
                          .and. dabs(coordcd(2)-coordan(2)) <= adist ) then
                        
                        nb_rough_DKDKx    = nb_rough_DKDKx+1
                        nb_PERIODIC_DKDKx = nb_PERIODIC_DKDKx + 1
                        
                        if ( nb_rough_DKDKx == 1) then
                           allocate(Root)
                           Current => Root
                           nullify(Root%p)
                        else
                           allocate(Current)
                           Previous%n => Current
                        end if
                        
                        Current%val%cd       = icdtac
                        Current%val%an       = iantac
                        Current%val%isee     = isee
                        Current%val%periodic = 1 
                        Current%p => Previous
                        nullify(Current%n)
                        Previous => Current
                     end if
                  end if
               end do
            end do
            end do
         end do
      end do
      end do
   end if

   write(cout,'(4X,I10,A20)') nb_rough_DKDKx,' DKDKx roughly found'
   call logmes(cout)

   if (nb_rough_DKDKx > size(this)) then

     if (allocated(periodic_DKDKx)) deallocate(periodic_DKDKx)
     allocate(periodic_DKDKx(nb_rough_DKDKx)) 
   
     if (allocated(rough_DKDKx)) deallocate(rough_DKDKx)
     allocate(rough_DKDKx(nb_rough_DKDKx))     ! the visibility array used in compute_contact is allocated
   
     if (allocated(this)) deallocate(this)
     allocate(this(nb_rough_DKDKx))            ! the oversized array this is temporaly allocated

   endif
   
   do icdan=nb_rough_DKDKx,1,-1
      
      Previous => Current%p
      rough_DKDKx(icdan)%cd       = Current%val%cd
      rough_DKDKx(icdan)%an       = Current%val%an
      rough_DKDKx(icdan)%isee     = Current%val%isee
      rough_DKDKx(icdan)%periodic = Current%val%periodic
      rough_DKDKx(icdan)%group    = NOINT
      
!!! > md > !!!
!!! a modifier pour des corps non-convexes
     
      raycd = get_radius_DISKx(Current%val%cd)
      rayan = get_radius_DISKx(Current%val%an)
      
      rough_DKDKx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))
      massan=get_mass_DISKx(diskx2bdyty(1,Current%val%an))
      
      rough_DKDKx(icdan)%meff = masscd*massan/(masscd+massan)
      
      deallocate(Current)
      Current => Previous
   end do
   
   nullify(Root)

 end subroutine experimental_creation_tab_visu
!--------------------------------------------------------------------------------------------------
 !> \brief 
 subroutine creation_tab_visu
   implicit none 
   integer(kind=4)           :: errare,ifound,nb_DISKx 
   integer(kind=4)           :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
   integer(kind=4)           :: icdan,iadj,ibdy,jbdy,icdbdy,ianbdy,itac,icdtac,iantac,isee,itacty   
   integer(kind=4)           :: minibox1=0,maxibox1=0,minibox2=0,maxibox2=0
   real(kind=8)              :: Bleft,Bright,Bup,Bdown
   real(kind=8)              :: raycd,rayan,adist,dist,nonuc,gapT
   real(kind=8)              :: masscd,massan
   real(kind=8),dimension(3) :: coord,coordcd,coordan 
   character(len=5)          :: cdtac,cdcol,antac,ancol
   character(len=28)         :: IAM
   character(len=103)        :: cout
    
   IAM = 'mod_DKDKx::creation_tab_visu'
    
   nb_DISKx = get_nb_DISKx()

   ! Since the list of proximate contactors may not be updated at every time step,
   ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
   ! A warning condition prevents undue deallocation. 
   
    if (allocated(box)) then
       do ibox1=minibox1,maxibox1
          do ibox2=minibox2,maxibox2
             if ( associated(box(ibox1,ibox2)%which) ) deallocate( box(ibox1,ibox2)%which )
          end do
       end do
       deallocate(box)
    end if
    
    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of disks and largest box containing disks.
    !
    ! The computation of maximal radius of disks allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficacious when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of disks is also used, in order to estimate the maximal 
    ! number of disks per box.   
    ! This quick sorting method may be applied to bodies other than disks, such as 
    ! ellipsoidal or polygonal bodies with a reasonable aspect ratio, less than 
    ! 3 or even 5. Such bodies are enclosed in disks with radius max_radius, and 
    ! enclosing a disk with radius min_radius. The sorting algorithm may then be 
    ! straightforwardly applied.
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Bleft    =  1.D24
    Bright   = -1.D24
    Bup      = -1.D24
    Bdown    =  1.D24
    
    do ibdy=1,nb_DISKx
       

       if (.not.get_visible_DISKx(ibdy)) cycle

       if ( iamABigDISKx(ibdy).eq.1 ) cycle

       coord = DKcoor(1:3,ibdy)
       Bleft = min(coord(1),Bleft )
       Bright= max(coord(1),Bright)
       Bup   = max(coord(2),Bup   )
       Bdown = min(coord(2),Bdown )

    end do
 
    !fd A VOIR le 03/01/08 les commentaires qui suivent son faux !!
   !
    ! Box size is defined to be largest (1%) than maxray+maxalert so as to
    ! ensure that all contacts may be detected.
    !
    ! Lbox1=0.101D+01*(maxray+0.5D0*maxalert)
    ! Lbox2=0.101D+01*0.5D0*dsqrt(2.D0)*(maxray+minray+maxalert)
    ! Lbox=max(Lbox1,Lbox2)
    !
    ! A box is located by pairs of integer numbers (ibox1,ibox2), where 
    ! ibox1 is the column number of the box, ibox2 the layer number of the box.
    ! 
    !
    !      ____ ____ ____ ____ ____ 
    !     |    |    |    |    |    |
    !  3  |    |    |    |    |    |   
    !     |____|____|____|____|____| 
    !     |    |    |    |    |    |
    !  2  |    |    |    |    |    |    ibox2
    !     |____|____|____|____|____| 
    !     |    |    |    |    |    |     
    !  1  |    |    |    |    |    |    
    !     |____|____|____|____|____| 
    !     
    !       1    2    3    4    5
    !
    !             ibox1
    !
    ! Coordinates of lower left and upper right corners of a (ibox1,ibox2) box are:
    !
    ! lowerleft  = ( Bleft + (ibox1-1)*Lbox , Bleft + (ibox2-1)*Lbox )
    ! upperright = ( Bdown +  ibox2   *Lbox , Bdown +  ibox2   *Lbox
    !
    ! An oversize covering of the big box (Bright,Bdown,Bleft,Bup) containing all disks 
    ! is the collection of elementary boxes such that, 
    !
    ! -1 .le. ibox1 .le. 1 + AINT((Bleft-Bright)/Lbox) ,
    ! -1 .le. ibox2 .le. 1 + AINT((Bup  -Bdown )/Lbox) . 
    !  
    
    if ( PERIODIC ) then
       if (Bright > periode) then
          print*,'Bright ',Bright,' Periode ',periode
          call FATERR(IAM,'the max right coordinate is greater than the periode')
       endif
       
       if (Bleft < 0.d0) then
          print*,'Bleft ',Bleft
          call FATERR(IAM,'the min left coordinate is less than zero')
       endif
       
       !fd pas sur que ca soit si malin ...
       !     if (INT(periode*Lbox_1)*Lbox > Bright) then
       !       Bright = periode - LBox !+ maxalert
       !     else
       Bright = periode
       !     endif
       Bleft  = 0.d0    !- maxalert
    end if
    
    minibox1 = 1
    maxibox1 = 1 + int((Bright-Bleft)*Lbox_1)
    minibox2 = 1
    maxibox2 = 1 + int((Bup - Bdown )*Lbox_1)
    !   
    allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)
    
    if (errare /=0 ) then
       write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       call LOGMES(cout)
       write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
       call LOGMES(cout)
       call FATERR(IAM,'error allocating box')
    end if
    
    do ibox1=minibox1,maxibox1
       do ibox2=minibox2,maxibox2
          box(ibox1,ibox2)%popul=0
          allocate(box(ibox1,ibox2)%which(maxpopul),stat=errare)
          if (errare /=0 ) then
             call FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%which')
          end if
          box(ibox1,ibox2)%which=0
       end do
    end do
    
   ! filling boxes with disks
   ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%which(ipopul) is the rank of body DISKx labelled ipopul in the box
  
   ! filling boxes   

   do ibdy=1,nb_DISKx

      if (.not.get_visible_DISKx(ibdy)) cycle

      if(iamABigDISKx(ibdy).eq.1) cycle

      coord=DKcoor(1:3,ibdy)
      ibox1=1+int((coord(1)-Bleft )*Lbox_1)
      ibox2=1+int((coord(2)-Bdown )*Lbox_1)
      if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
         call LOGMES(cout)
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2
         call LOGMES(cout)
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
         call LOGMES(cout)
         write(cout,'(A13,I10,A13)') '  body DISKx ',ibdy,' out of boxes'
         call FATERR(IAM,cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5)
      end if
      box(ibox1,ibox2)%popul = box(ibox1,ibox2)%popul+1
      if( box(ibox1,ibox2)%popul > size(box(ibox1,ibox2)%which) ) then
          call faterr(IAM, "Estimated max popul limit reached.")
      end if
      box(ibox1,ibox2)%which(box(ibox1,ibox2)%popul) = ibdy
   end do

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
   
   nb_rough_DKDKx = 0
   
   ! creation de la liste de paire a examiner
  
   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue une zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   nullify(Root)
   nullify(Current)
   nullify(Previous)
  

   !fd A VOIR le 03/01/08 on pourrait diminuer le nombre de tests en gerant 
   !fd le test if iantac <= icdtac cycle.
   !fd c'est ahurissant !!

   do ibox1cd = minibox1,maxibox1  
      do ibox2cd = minibox2,maxibox2
         do icdpop = 1,box(ibox1cd,ibox2cd)%popul
            icdtac = box(ibox1cd,ibox2cd)%which(icdpop)
            cdcol = get_color_DISKx(icdtac)
            ! box loop investigating antagonist diskx
            do ibox1an = max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                        
               do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
                  do ianpop = 1,box(ibox1an,ibox2an)%popul
                     iantac = box(ibox1an,ibox2an)%which(ianpop)
                     if (iantac .le. icdtac .or. is_DISKx_same_BDYTY(icdtac,iantac)) cycle
                     ancol = get_color_DISKx(iantac)
                     if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                        isee  = get_isee_specific('DISKx',cdcol,ancol)
                     else
                        isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                        get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                     end if
                     
                     if ( isee /= 0 ) then
                        adist = see(isee)%alert 
                        ! checking ROUGHLY distance against alert distance           
                        coordcd = DKcoor(1:3,icdtac)
                        coordan = DKcoor(1:3,iantac)
                        raycd   = get_radius_DISKx(icdtac)
                        rayan   = get_radius_DISKx(iantac)

                        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                        ! results might be different up to some non significant figures, but when comparing to
                        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

                        adist = 0.1005D+01*adist+raycd+rayan

                        if (       dabs(coordcd(1)-coordan(1)) <= adist &
                             .and. dabs(coordcd(2)-coordan(2)) <= adist) then

                           nb_rough_DKDKx=nb_rough_DKDKx+1

                           if ( nb_rough_DKDKx == 1) then
                              allocate(Root)
                              Current => Root
                              nullify(Root%p)
                           else
                              allocate(Current)
                              Previous%n => Current
                           endif
                           Current%val%cd       = icdtac
                           Current%val%an       = iantac
                           Current%val%isee     = isee
                           Current%val%periodic = 0

                           Current%p => Previous
                           nullify(Current%n)
                           Previous => Current
                        end if
                     end if
                  end do
               end do
            end do
         end do
      end do
   end do
   
   do ibdy=1,nb_big_DISKx

      icdtac=big_DISKx_list(ibdy)
      if (.not.get_visible_DISKx(icdtac)) cycle
      cdcol  = get_color_DISKx(icdtac)
      coordcd = DKcoor(1:3,icdtac)
      raycd  = get_radius_DISKx(icdtac)

      do iantac=1,nb_DISKx

         if(icdtac.eq.iantac) cycle
         if (.not.get_visible_DISKx(iantac)) cycle
         if (is_DISKx_same_BDYTY(icdtac,iantac)) cycle

         ancol = get_color_DISKx(iantac)
         if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
            isee  = get_isee_specific('DISKx',cdcol,ancol)
         else
            isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                            get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
         end if
         
         if (isee.ne.0)then
            adist=see(isee)%alert 
            coordan = DKcoor(1:3,iantac)
            rayan = get_radius_DISKx(iantac)
            adist=0.1005D+01*adist+raycd+rayan
            dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
                 (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))
            if (dist<adist*adist) then
               nb_rough_DKDKx=nb_rough_DKDKx+1
               if (nb_rough_DKDKx == 1) then
                  allocate(Root)
                  Current => Root
                  nullify(Root%p)
               else
                  allocate(Current)
                  Previous%n => Current
               endif
               Current%val%cd       =icdtac
               Current%val%an       =iantac
               Current%val%isee     =isee
               Current%val%periodic =0
                  
               Current%p => Previous
               nullify(Current%n)
               Previous => Current
            end if
         end if
      end do

   end do

   nb_PERIODIC_DKDKx = 0
  
   if ( PERIODIC ) then

!      print*,'on teste le periodic entre les colonnes',maxibox1,' et ',minibox1
      
      !fd 03/01/08 le dernier-1 est necessaire car le decoupage en boites est approximatif donc
      !fd il peut y des cas ou la derniere colonne (qui contient la limite de periodicite)
      !fd est vide alors que la dernier-1 colonne contient des objets dont la frontiere passe de l'autre cote

      do ibox1cd = maxibox1-1,maxibox1
      do ibox2cd = minibox2,maxibox2

!         print*,'boite candidate ',ibox2cd,' population ',box(ibox1cd,ibox2cd)%popul

         do icdpop = 1,box(ibox1cd,ibox2cd)%popul
            
            icdtac = box(ibox1cd,ibox2cd)%which(icdpop)
            cdcol  = get_color_DISKx(icdtac)
            ! box loop investigating antagonist diskx
            !fd 03/01/08 A VOIR je ne comprends pas le premier+1 !?
            do ibox1an = minibox1,minibox1+1
            do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
!               print*,'boite antagoniste ',ibox2an,' population ',box(ibox1an,ibox2an)%popul

               do ianpop = 1,box(ibox1an,ibox2an)%popul
                  
                  iantac = box(ibox1an,ibox2an)%which(ianpop)
                  
                  IF (is_DISKx_same_BDYTY(icdtac,iantac)) CYCLE

                  ancol = get_color_DISKx(iantac)
                  if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                    isee  = get_isee_specific('DISKx',cdcol,ancol)
                  else
                    isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                    get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                  end if
                  
                  if (isee /= 0 ) then
                     adist   = see(isee)%alert 
                     ! checking ROUGHLY distance against alert distance           
                     coordcd = DKcoor(1:3,icdtac)
                     coordan = DKcoor(1:3,iantac)
                     raycd   = get_radius_DISKx(icdtac)
                     rayan   = get_radius_DISKx(iantac)

                     coordan(1) = coordan(1) + periode
                     
                     ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                     ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                     ! results might be different up to some non significant figures, but when comparing to
                     ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                     adist = 0.1005D+01*adist+raycd+rayan
                     if (       dabs(coordcd(1)-coordan(1)) <= adist &
                          .and. dabs(coordcd(2)-coordan(2)) <= adist ) then
                        
                        nb_rough_DKDKx    = nb_rough_DKDKx+1
                        nb_PERIODIC_DKDKx = nb_PERIODIC_DKDKx + 1
                        
                        if ( nb_rough_DKDKx == 1) then
                           allocate(Root)
                           Current => Root
                           nullify(Root%p)
                        else
                           allocate(Current)
                           Previous%n => Current
                        end if
                        
                        !mr : for periodic case to preserve cd < an ordering 
                        if(icdtac.gt.iantac)then
                           Current%val%cd       = iantac
                           Current%val%an       = icdtac
                           Current%val%periodic =-1                  
                        else
                           Current%val%cd       = icdtac
                           Current%val%an       = iantac
                           Current%val%periodic = 1                  
                        end if
                        Current%val%isee     = isee
                        Current%p => Previous
                        nullify(Current%n)
                        Previous => Current
                     end if
                  end if
               end do
            end do
            end do
         end do
      end do
      end do

      !mr: case of big DISKx for periodic conditions
      do ibdy=1,nb_big_DISKx

         icdtac=big_DISKx_list(ibdy)

         !print*,'big ',icdtac, periode

         if (.not.get_visible_DISKx(icdtac)) cycle
         cdcol  = get_color_DISKx(icdtac)
         coordcd = DKcoor(1:3,icdtac)
         raycd  = get_radius_DISKx(icdtac)
         
         do iantac=1,nb_DISKx
            
            if( icdtac.eq.iantac ) cycle
            if ( .not.get_visible_DISKx(iantac) ) cycle
            if ( is_DISKx_same_BDYTY(icdtac,iantac) ) cycle
            
            ancol = get_color_DISKx(iantac)
            if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                isee  = get_isee_specific('DISKx',cdcol,ancol)
            else
                isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
            end if
            
            if (isee.ne.0) then
               adist=see(isee)%alert 
               rayan = get_radius_DISKx(iantac)
               adist=0.1005D+01*adist+raycd+rayan

               ! sens +

               coordan = DKcoor(1:3,iantac)
               coordan(1) = coordan(1) + periode
               dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
                    (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))

               if (dist<adist*adist) then

                  !print*,'big sens +'  

                  nb_rough_DKDKx=nb_rough_DKDKx+1
                  nb_PERIODIC_DKDKx = nb_PERIODIC_DKDKx + 1

                  if (nb_rough_DKDKx == 1) then
                     allocate(Root)
                     Current => Root
                     nullify(Root%p)
                  else
                     allocate(Current)
                     Previous%n => Current
                  endif

                  !mr : for periodic case to preserve cd < an ordering 
                  if ( icdtac .gt. iantac ) then
                     Current%val%cd       = iantac
                     Current%val%an       = icdtac
                     Current%val%periodic =-1                  
                  else
                     Current%val%cd       = icdtac
                     Current%val%an       = iantac
                     Current%val%periodic = 1                  
                  end if

                  !print*,Current%val%cd,Current%val%an,Current%val%periodic

                  Current%val%isee     = isee
                  Current%p => Previous
                  nullify(Current%n)
                  Previous => Current
               end if

               !fd stupide on refait la meme chose qu'avant !?

               ! sens -

               coordan = DKcoor(1:3,iantac)
               coordan(1) = coordan(1) - periode
               dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
                    (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))

               if ( dist < adist*adist ) then

                  !print*,'big sens -'  

                  nb_rough_DKDKx=nb_rough_DKDKx+1
                  nb_PERIODIC_DKDKx = nb_PERIODIC_DKDKx + 1

                  if (nb_rough_DKDKx == 1) then
                     allocate(Root)
                     Current => Root
                     nullify(Root%p)
                  else
                     allocate(Current)
                     Previous%n => Current
                  endif

                  !mr : for periodic case to preserve cd < an ordering 
                  if ( icdtac.gt.iantac ) then
                     Current%val%cd       = iantac
                     Current%val%an       = icdtac
                     Current%val%periodic = 1                  
                  else
                     Current%val%cd       = icdtac
                     Current%val%an       = iantac
                     Current%val%periodic =-1                  
                  end if

                  !print*,Current%val%cd,Current%val%an,Current%val%periodic

                  Current%val%isee     = isee
                  Current%p => Previous
                  nullify(Current%n)
                  Previous => Current

               end if
            end if
         end do
         
      end do

   end if
   
   write(cout,'(4X,I10,A20)') nb_rough_DKDKx,' DKDKx roughly found'
   call logmes(cout)

   if (allocated(periodic_DKDKx)) deallocate(periodic_DKDKx)
   allocate(periodic_DKDKx(nb_rough_DKDKx)) 
   
   if (allocated(rough_DKDKx)) deallocate(rough_DKDKx)
   allocate(rough_DKDKx(nb_rough_DKDKx))     ! the visibility array used in compute_contact is allocated
   
   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_DKDKx))            ! the oversized array this is temporaly allocated
   
   do icdan=nb_rough_DKDKx,1,-1
      
      Previous => Current%p
      rough_DKDKx(icdan)%cd       = Current%val%cd
      rough_DKDKx(icdan)%an       = Current%val%an
      rough_DKDKx(icdan)%isee     = Current%val%isee
      rough_DKDKx(icdan)%periodic = Current%val%periodic
      rough_DKDKx(icdan)%group    = NOINT
      
!!! > md > !!!
!!! a modifier pour des corps non-convexes
     
      raycd = get_radius_DISKx(Current%val%cd)
      rayan = get_radius_DISKx(Current%val%an)
      
      rough_DKDKx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))
      massan=get_mass_DISKx(diskx2bdyty(1,Current%val%an))
      
      rough_DKDKx(icdan)%meff = masscd*massan/(masscd+massan)
      
      deallocate(Current)
      Current => Previous
   end do
   
   nullify(Root)

 end subroutine creation_tab_visu
 
 !--------------------------------------------------------------------------------------------------
 subroutine compute_contact_DKDKx
   implicit none  
   integer(kind=4)    :: i, errare, i4_input(6), i4_output(5)
   integer(kind=4)    :: icdan, ibdy!,iadj,icdbdy,ianbdy,itac
   !integer(kind=4)    :: icdtac,iantac,isee,itacty,iprd  
   real(kind=8)       :: gapTT, r8_vec_out(2,6)
   logical            :: to_keep, all_dof_cd, all_dof_an
   character(len=32)  :: IAM
   character(len=103) :: cout

   integer(kind=4) :: nb_DISKx
   integer(kind=4) :: nb_potential_contact,i_visavis

   IAM= 'mod_DKDKx::compute_contact_DKDKx'

   icdan    = 0        
   nb_DKDKx = 0
   nb_adj   = 0
 
   if (nb_rough_DKDKx .eq. 0 ) return

   nb_potential_contact = nb_rough_DKDKx

   if (WITH_VAV) then
      if (FIRST_TIME_VAV) then
         nb_visavis = 0
         i_visavis  = 0
      else
         nb_potential_contact = nb_visavis  
      end if
   end if
   !
   ! preparing detection
   !
   
   do i = 1,nb_potential_contact

      all_dof_cd = all_dof_driven_DISKx(rough_DKDKx(i)%cd)
      all_dof_an = all_dof_driven_DISKx(rough_DKDKx(i)%an)
      if( all_dof_cd .and. all_dof_an ) cycle

      i4_input(1) = rough_DKDKx(i)%cd
      i4_input(2) = rough_DKDKx(i)%an
      i4_input(3) = rough_DKDKx(i)%isee
      i4_input(4) = rough_DKDKx(i)%periodic

      if (WITH_VAV.and..not.FIRST_TIME_VAV) then
        i4_input(5) = visavis(i)%cd
        i4_input(6) = visavis(i)%an 
        i4_input(3) = visavis(i)%isee
      end if

      call compute_one_contact_DKDKx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)

      if( to_keep ) then
        icdan = icdan+1

        this(icdan)%icdbtac = diskx2bdyty(2, i4_input(1))
        this(icdan)%ianbtac = diskx2bdyty(2, i4_input(2))

        this(icdan)%icdbtyp = diskx2bdyty(3, i4_input(1))
        this(icdan)%ianbtyp = diskx2bdyty(3, i4_input(2))

        this(icdan)%icdctyp = i_diskx
        this(icdan)%ianctyp = i_diskx

        this(icdan)%iadj   = i4_output(1)
        this(icdan)%icdbdy = i4_output(2)
        this(icdan)%icdtac = i4_output(3)
        this(icdan)%icdsci = 0

        this(icdan)%ianbdy = i4_output(4)
        this(icdan)%iantac = i4_output(5)
        this(icdan)%iansci = 0

        this(icdan)%icdent = get_ent_DISKx(i4_output(3))
        this(icdan)%ianent = get_ent_DISKx(i4_output(5))

        this(icdan)%isee  = i4_input(3)
        this(icdan)%group = rough_DKDKx(i)%group

        this(icdan)%tuc = r8_vec_out(:,1)
        this(icdan)%nuc = r8_vec_out(:,2)

        this(icdan)%gapTTbegin =  gapTT
        periodic_DKDKx(icdan)  =  rough_DKDKx(i)%periodic

        this(icdan)%Gcdt3 = r8_vec_out(1,3)
        this(icdan)%Gcdn3 = r8_vec_out(2,3)
        this(icdan)%Gant3 = r8_vec_out(1,4)
        this(icdan)%Gann3 = r8_vec_out(2,4)

        this(icdan)%vltBEGIN = r8_vec_out(1,5)
        this(icdan)%vlnBEGIN = r8_vec_out(2,5)

        this(icdan)%rlt       = 0.D0
        this(icdan)%rln       = 0.D0
        this(icdan)%vlt       = this(icdan)%vltBEGIN
        this(icdan)%vln       = this(icdan)%vlnBEGIN
        this(icdan)%gapTT     = this(icdan)%gapTTbegin
        this(icdan)%status    = i_nknow
    
        this(icdan)%reff = rough_DKDKx(i)%reff
        this(icdan)%meff = rough_DKDKx(i)%meff
         
        this(icdan)%coor(1:2) = r8_vec_out(:,6)


        call get_behaviour_( icdan, see, tact_behav )

      end if
         
   end do
      
   nb_DKDKx = icdan

   write(cout,'(1X,I10,A12)') nb_DKDKx,' DKDKx found'
   call logmes(cout)

   nb_DISKx = get_nb_DISKx()
   do ibdy=1,nb_DISKx

      if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)

      if (nb_adj(ibdy) /= 0) then
         allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating adjac(icdbdy)%.....')
         end if
      else 
         nullify(adjac(ibdy)%icdan)
      end if

   end do
   
   do icdan=1,nb_DKDKx
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   end do
   
   if (allocated(violation)) deallocate(violation)
   allocate(violation(nb_DKDKx),stat=errare)

 end subroutine compute_contact_DKDKx
 

 !> compute one contact
 subroutine compute_one_contact_DKDKx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)
   implicit none
   !> integer input parameters of the contact computation
   integer(kind=4), dimension(6)  , intent(in)  :: i4_input
   !> integer output parameters of the contact computation
   integer(kind=4), dimension(5)  , intent(out) :: i4_output
   !> real output data of the contact computation
   real(kind=8)   , dimension(2,6), intent(out) :: r8_vec_out
   !> is rough contact close enough
   logical, intent(out) :: to_keep
   !> computed gap of the contact
   real(kind=8), intent(out) :: gapTT
   !
   integer(kind=4) :: icdtac, iantac, isee, iprd, icdbdy, ianbdy, cd_ent, an_ent
   real(kind=8)    :: adist, raycd, rayan, nonuc
   real(kind=8), dimension(3) :: coordcd, coordan, cd_Vbegin, an_Vbegin
   real(kind=8),dimension(2)  :: n,t,cdlev,anlev,cd_shift,an_shift
   character(len=30)          :: IAM
   character(len=103)         :: cout

   !     123456789012345678901234567890
   IAM= 'mod_DKDKx::compute_one_contact'

   to_keep = .false.

   icdtac = i4_input(1)
   iantac = i4_input(2)
   isee   = i4_input(3)
   iprd   = i4_input(4)

   if (WITH_VAV.and..not.FIRST_TIME_VAV) then
     icdbdy = i4_input(5)
     ianbdy = i4_input(6)
   end if
        
   adist   = see(isee)%alert 
   coordcd = DKcoor(1:3,icdtac)
   coordan = DKcoor(1:3,iantac)
   coordan(1) = coordan(1) + (real(iprd,8)*periode)
   
   raycd   = get_radius_DISKx(icdtac)
   rayan   = get_radius_DISKx(iantac)
   
   nonuc = sqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
   
   if (nonuc < 1.D-18) then
      write(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of disk',icdtac, &
           'within 1.e-18 from center of disk',iantac,'in comp_local_frame_DKDKx' 
      call FATERR(IAM,cout)
   end if
        
   gapTT = nonuc-(raycd+rayan)
   
   ! checking distance against alert distance           
   if (gapTT .gt. adist) return
     
   to_keep = .true.

   nb_adj(icdtac) = nb_adj(icdtac)+1
        
   if (smooth_method) then
     cd_Vbegin = get_V_DISKx(icdtac)
     an_Vbegin = get_V_DISKx(iantac)
   else
     cd_Vbegin = get_Vbegin_DISKx(icdtac)
     an_Vbegin = get_Vbegin_DISKx(iantac)
   endif
        
   i4_output(1) = nb_adj(icdtac)
   i4_output(2) = diskx2bdyty(1,icdtac)
   i4_output(3) = icdtac
   i4_output(4) = diskx2bdyty(1,iantac)
   i4_output(5) = iantac
   !i4_output(6) = isee                 

   n(1:2) = (coordcd(1:2)-coordan(1:2))/nonuc
   t(1) = n(2) ; t(2) = -n(1)
        
   r8_vec_out(:,1) = t
   r8_vec_out(:,2) = n
   
   cd_ent = get_ent_DISKx(icdtac)
   an_ent = get_ent_DISKx(iantac)
   
   if (cd_ent /= an_ent) then
     entity(cd_ent)%nb = entity(cd_ent)%nb+1
     entity(an_ent)%nb = entity(an_ent)%nb+1
   else
     entity(cd_ent)%nb = entity(cd_ent)%nb+1
   end if
        
   cd_shift = get_shiftTT_DISKx(icdtac)
   an_shift = get_shiftTT_DISKx(iantac)
   
   cdlev = (-raycd*n) + cd_shift
   anlev = ( rayan*n) + an_shift
        
   !Gcdt/n3
   r8_vec_out(1,3) = -cdlev(2)*t(1)+cdlev(1)*t(2)
   r8_vec_out(2,3) = -cdlev(2)*n(1)+cdlev(1)*n(2)
        
   !Gant/n3
   r8_vec_out(1,4) = -anlev(2)*t(1)+anlev(1)*t(2)
   r8_vec_out(2,4) = -anlev(2)*n(1)+anlev(1)*n(2)
        
   !vlt/nBegin
   r8_vec_out(1,5)  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                    + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                    +  cd_Vbegin(3)*r8_vec_out(1,3) &
                    -  an_Vbegin(3)*r8_vec_out(1,4)
        
   r8_vec_out(2,5) = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                   + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                   +  cd_Vbegin(3)*r8_vec_out(2,3) &
                   -  an_Vbegin(3)*r8_vec_out(2,4)
           
   r8_vec_out(1:2,6) = DKcoor(1:2,icdtac) - (raycd +0.5D0*gapTT)*n(1:2)
        
 end subroutine compute_one_contact_DKDKx

 !--------------------------------------------------------------------------------------------------
 !mr DDM??
 subroutine get_nb_INTRF_DKDKx(nb_INTRF)
   implicit none
   integer, intent(out) :: nb_INTRF ! Nombre de "contacts d'interface",
                                    ! i.e. de contacts dont le %cd et/ou
                                    ! le %an sont taggés "INTRF".
   integer :: i

   nb_INTRF=0
   do i = 1, nb_DKDKx
      if ( this(i)%group == INTRF ) nb_INTRF=nb_INTRF+1
   end do

 end subroutine get_nb_INTRF_DKDKx

!--------------------------------------------------------------------------------------------------
 subroutine get_list_INTRF_DKDKx(nb_INTRF,liste_INTRF)

   implicit none

   integer, intent(in) :: nb_INTRF ! Nombre de "contacts d'interface",
                                   ! i.e. de contacts dont le %cd et/ou
                                   ! le %an sont taggés "INTRF".

   integer, dimension(nb_INTRF), intent(out) :: liste_INTRF

   integer           :: i, compteur
                             !12345678901234567890123456
   character(len=26) :: IAM= 'mod_DKDKx::get_list_INTRF'

   compteur=0
   do i = 1, nb_DKDKx
      if ( this(i)%group == INTRF ) then
         compteur = compteur + 1
         if (compteur>nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")
         liste_INTRF(compteur) = i
      end if
   end do

   if (compteur/=nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")

 end subroutine get_list_INTRF_DKDKx

 !--------------------------------------------------------------------------------------------------
 subroutine compute_vav_contact_DKDKx
 
   implicit none  

   integer                   :: errare
   integer                   :: icdan,iadj,ibdy,icdbdy,ianbdy,itac
   integer                   :: icdtac,iantac,isee,itacty,iprd  
   real(kind=8),dimension(3) :: coord,coordcd,coordan,cd_Vbegin,an_Vbegin
   real(kind=8)              :: raycd,rayan,adist,dist,nonuc,gapTT
   integer                   :: i,id,j
   real(kind=8)              :: norm                                ! scalaire contenant la norme de sep
   real(kind=8),dimension(2) :: n,t,cdlev,anlev,cd_shift,an_shift
   integer                   :: cd_ent,an_ent
   
   character(len=103)        :: cout
   character(len=32)         :: IAM= 'mod_DKDKx::compute_contact_DKDKx'

   integer :: nb_DISKx,ivav
   integer :: nb_potential_contact,i_visavis
   logical :: FLAGik

   type(T_visavis) :: VAVik

   icdan    = 0        
   nb_DKDKx = 0
   nb_adj   = 0
   
   if (nb_rough_DKDKx .eq. 0 ) return

   icdtac = 1  ! pour l'instant, c'est ok...
   iantac = 1
      
   if (FIRST_TIME_VAV) then
      do i = 1,nb_rough_DKDKx
         
         icdtac = rough_DKDKx(i)%cd
         iantac = rough_DKDKx(i)%an
         isee   = rough_DKDKx(i)%isee 
         iprd   = rough_DKDKx(i)%periodic

         adist   = see(isee)%alert 
         coordcd = DKcoor(1:3,icdtac)
         coordan = DKcoor(1:3,iantac)
         coordan(1) = coordan(1) + (real(iprd,8)*periode)
      
         raycd   = get_radius_DISKx(icdtac)
         rayan   = get_radius_DISKx(iantac)
      
         nonuc = sqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
      
         if (nonuc < 1.D-18) then
            write(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of disk',icdtac, &
                 'within 1.e-18 from center of disk',iantac,'in comp_local_frame_DKDKx' 
            call FATERR(IAM,cout)
         end if
         
         gapTT = nonuc-(raycd+rayan)
      
         ! checking distance against alert distance           
      
         if (gapTT .le. adist) then    
            
            icdan          = icdan+1
            nb_adj(icdtac) = nb_adj(icdtac)+1
            iadj           = nb_adj(icdtac)

!!! ------- vis-a-vis structure
            visavis(icdan)%cd   = icdtac
            visavis(icdan)%an   = iantac
            visavis(icdan)%isee = isee
            visavis(icdan)%FREE = .false.
!!! ---------------------------
         
            if (smooth_method) then
               cd_Vbegin = get_V_DISKx(icdtac)
               an_Vbegin = get_V_DISKx(iantac)
            else
               cd_Vbegin = get_Vbegin_DISKx(icdtac)
               an_Vbegin = get_Vbegin_DISKx(iantac)
            end if
         
            this(icdan)%iadj     =  iadj
            this(icdan)%icdbdy   =  diskx2bdyty(1,icdtac)
            this(icdan)%icdtac   =  icdtac
            this(icdan)%ianbdy   =  diskx2bdyty(1,iantac)
            this(icdan)%iantac   =  iantac
            this(icdan)%isee     =  isee                 
            this(icdan)%nuc(1:2) =  (coordcd(1:2)-coordan(1:2))/nonuc
            this(icdan)%tuc(1)   =  this(icdan)%nuc(2)
            this(icdan)%tuc(2)   = -this(icdan)%nuc(1)   
         
            cd_ent = get_ent_DISKx(this(icdan)%icdtac)
            an_ent = get_ent_DISKx(this(icdan)%iantac) 
            
            entity(cd_ent)%nb = entity(cd_ent)%nb+1
            entity(an_ent)%nb = entity(an_ent)%nb+1
            
            this(icdan)%gapTTbegin =  gapTT
            periodic_DKDKx(icdan)  =  iprd
            
!!! ------- vis-a-vis structure
            visavis(icdan)%gapREF   = this(icdan)%gapTTbegin

            visavis(icdan)%Icoorcd(1:2) = coordcd(1:2) - raycd*this(icdan)%nuc(1:2)
            visavis(icdan)%Icooran(1:2) = coordan(1:2) + rayan*this(icdan)%nuc(1:2)
            visavis(icdan)%coorcd(1:2)  = coordcd(1:2)
            visavis(icdan)%cooran(1:2)  = coordan(1:2)
!!! ---------------------------

            cd_shift = get_shiftTT_DISKx(icdtac)
            an_shift = get_shiftTT_DISKx(iantac)
            
            cdlev= (-raycd*this(icdan)%nuc) + cd_shift
            anlev= ( rayan*this(icdan)%nuc) + an_shift
            
            n(1) = this(icdan)%nuc(1)
            n(2) = this(icdan)%nuc(2)               
            t(1) = n(2) ; t(2) = -n(1)
            
            this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
            this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
            
            this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
            this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)
         
            this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                                  + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                                  +  cd_Vbegin(3)*this(icdan)%Gcdt3 &
                                  -  an_Vbegin(3)*this(icdan)%Gant3
         
            this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                                  + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                                  +  cd_Vbegin(3)*this(icdan)%Gcdn3 &
                                  -  an_Vbegin(3)*this(icdan)%Gann3
            
            this(icdan)%rlt       = 0.D0
            this(icdan)%rln       = 0.D0
            this(icdan)%vlt       = this(icdan)%vltBEGIN
            this(icdan)%vln       = this(icdan)%vlnBEGIN
            this(icdan)%gapTT     = this(icdan)%gapTTbegin
            this(icdan)%status    = i_nknow
            
            this(icdan)%reff    = rough_DKDKx(i)%reff
            this(icdan)%meff    = rough_DKDKx(i)%meff
         
         
            call get_behaviour_( icdan, see, tact_behav )

         end if
      
      end do

   else
!!! NOT FIRST VIS-A-VIS STEP
      do i = 1,nb_rough_DKDKx
         
         icdtac = rough_DKDKx(i)%cd
         iantac = rough_DKDKx(i)%an

         isee   = rough_DKDKx(i)%isee 
         iprd   = rough_DKDKx(i)%periodic

         adist   = see(isee)%alert 
         coordcd = DKcoor(1:3,icdtac)
         coordan = DKcoor(1:3,iantac)
         coordan(1) = coordan(1) + (real(iprd,8)*periode)
      
         raycd   = get_radius_DISKx(icdtac)
         rayan   = get_radius_DISKx(iantac)
      
         nonuc = sqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
      
         if (nonuc < 1.D-18) then
            write(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of disk',icdtac, &
                 'within 1.e-18 from center of disk',iantac,'in comp_local_frame_DKDKx' 
            call FATERR(IAM,cout)
         end if
         
         gapTT = nonuc-(raycd+rayan)
      
         ! checking distance against alert distance           
      
         if (gapTT .le. adist) then    

            icdan          = icdan+1
            nb_adj(icdtac) = nb_adj(icdtac)+1
            iadj           = nb_adj(icdtac)
         
            if (smooth_method) then
               cd_Vbegin = get_V_DISKx(icdtac)
               an_Vbegin = get_V_DISKx(iantac)
            else
               cd_Vbegin = get_Vbegin_DISKx(icdtac)
               an_Vbegin = get_Vbegin_DISKx(iantac)
            end if
         
            this(icdan)%iadj     =  iadj
            this(icdan)%icdbdy   =  diskx2bdyty(1,icdtac)
            this(icdan)%icdtac   =  icdtac
            this(icdan)%ianbdy   =  diskx2bdyty(1,iantac)
            this(icdan)%iantac   =  iantac
            this(icdan)%isee     =  isee                 
            this(icdan)%nuc(1:2) =  (coordcd(1:2)-coordan(1:2))/nonuc
            this(icdan)%tuc(1)   =  this(icdan)%nuc(2)
            this(icdan)%tuc(2)   = -this(icdan)%nuc(1)   
         
            cd_ent = get_ent_DISKx(this(icdan)%icdtac)
            an_ent = get_ent_DISKx(this(icdan)%iantac) 
            
            entity(cd_ent)%nb = entity(cd_ent)%nb+1
            entity(an_ent)%nb = entity(an_ent)%nb+1
            
            call AM_I_OLD_VAV(icdtac,iantac,ivav,FLAGik)

            if (FLAGik) then
               
            else
               call FILL_VAV_DKDKx(VAVik,icdan)
            end if

            this(icdan)%gapTTbegin =  gapTT
            periodic_DKDKx(icdan)  =  iprd
            
            cd_shift = get_shiftTT_DISKx(icdtac)
            an_shift = get_shiftTT_DISKx(iantac)
            
            cdlev= (-raycd*this(icdan)%nuc) + cd_shift
            anlev= ( rayan*this(icdan)%nuc) + an_shift
            
            n(1) = this(icdan)%nuc(1)
            n(2) = this(icdan)%nuc(2)               
            t(1) = n(2) ; t(2) = -n(1)
            
            this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
            this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
            
            this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
            this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)
         
            this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                                  + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                                  +  cd_Vbegin(3)*this(icdan)%Gcdt3 &
                                  -  an_Vbegin(3)*this(icdan)%Gant3
         
            this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                                  + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                                  +  cd_Vbegin(3)*this(icdan)%Gcdn3 &
                                  -  an_Vbegin(3)*this(icdan)%Gann3
            
            this(icdan)%rlt       = 0.D0
            this(icdan)%rln       = 0.D0
            this(icdan)%vlt       = this(icdan)%vltBEGIN
            this(icdan)%vln       = this(icdan)%vlnBEGIN
            this(icdan)%gapTT     = this(icdan)%gapTTbegin
            this(icdan)%status    = i_nknow
            
            this(icdan)%reff    = rough_DKDKx(i)%reff
            this(icdan)%meff    = rough_DKDKx(i)%meff
         
         
            call get_behaviour_( icdan, see, tact_behav )

         end if
      
      end do
   end if

   nb_DKDKx = icdan
      
   write(cout,'(1X,I10,A12)') nb_DKDKx,' DKDKx found'
   call logmes(cout)

   nb_DISKx = get_nb_DISKx()

   do ibdy=1,nb_DISKx

      if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
      
      if (nb_adj(ibdy) /= 0) then
         allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating adjac(icdbdy)%.....')
         end if
      else 
         nullify(adjac(ibdy)%icdan)
      end if
      
   end do
   
   do icdan=1,nb_DKDKx
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   end do
   
   if (allocated(violation)) deallocate(violation)
   allocate(violation(nb_DKDKx),stat=errare)
   
 end subroutine compute_vav_contact_DKDKx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
 subroutine smooth_computation_DKDKx

    implicit none
    integer          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    do icdan=1,nb_DKDKx
       
!       print *,'-----------------'
!       print*,'icdan ',icdan       

       call compute_2D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            this(icdan)%reff,this(icdan)%meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

  !     print *,'-----------------'

    end do
    !$OMP END DO
    !$OMP END PARALLEL
    
!fd cette ligne a des effets de bord car on peut ecraser ce qui a ete pose par dkjc, etc.
!fd    DO icdan=1,nb_DKDKx  
!fd       CALL nullify_reac_DKDKx(icdan,iIreac)
!fd    END DO

!fd on fait confiance a l'initialisation faite par increment
    
    do icdan=1,nb_DKDKx
       call injj_DKDKx(icdan,this(icdan)%rlt,this(icdan)%rln,iIreac)
    end do
    
  end subroutine smooth_computation_DKDKx
!!!------------------------------------------------------------------------ 
 subroutine display_prox_tactors_DKDKx

   implicit none

   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   integer          :: nb_DISKx
   character(len=5) :: cdmodel, anmodel

   nb_DISKx = get_nb_DISKx()

   if (xxl_check) then
      do icdtact=1,nb_DISKx    
         do iadj=1,nb_adj(icdtact)         
            icdan  = adjac(icdtact)%icdan(iadj)
            icdtac = this(icdan)%icdtac
            iantac = this(icdan)%iantac
            cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
            anmodel = get_body_model_name_from_id( diskx2bdyty(3,iantac) )
            write(*,'(A1)')' '
            write(*,'(A6,2X,I10)')'$icdan',icdan
                            !1234567890123456789012345678901234567890123456789012345678901234567890123456789012
            write(*,'(A82)')' cdbdy       numbr  cdtac  numbr  behav  anbdy       numbr  antac  numbr          '
            write(*,'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5)')   &
                 cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
                 anmodel,diskx2bdyty(1,iantac),'DISKx',diskx2bdyty(2,iantac)
            write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
            write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
            write(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
            ! write(*,104)'rlt =',this(icdan)%rlt,'rln =',this(icdan)%rln,'rls =',0.D0
            write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
            write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTbegin
            write(*,'(A1)')' '                     
         end do
      end do
   else
      do icdtact=1,nb_DISKx    
         do iadj=1,nb_adj(icdtact)         
            icdan  = adjac(icdtact)%icdan(iadj)
            icdtac = this(icdan)%icdtac
            iantac = this(icdan)%iantac
            cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
            anmodel = get_body_model_name_from_id( diskx2bdyty(3,iantac) )
            write(*,'(A1)')' '
            write(*,'(A6,2X,I5)')'$icdan',icdan
            !123456789012345678901234567890123456789012345678901234567890123456789012
            write(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
            write(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
                 cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
                 anmodel,diskx2bdyty(1,iantac),'DISKx',diskx2bdyty(2,iantac)
            write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
            write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
            write(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
            ! write(*,104)'rlt =',this(icdan)%rlt,'rln =',this(icdan)%rln,'rls =',0.D0
            write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
            write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTbegin
            write(*,'(A1)')' '                     
         end do
      end do
   end if

104 format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_DKDKx
!------------------------------------------------------------------------  
! SUBROUTINE stock_rloc
! get data from this and put into verlt
!------------------------------------------------------------------------ 
 subroutine stock_rloc_DKDKx
   implicit none

   if (get_with_experimental_dev()) then
     call experimental_stock_rloc
   else
     call stock_rloc
   endif

 end subroutine stock_rloc_DKDKx

 subroutine stock_rloc
 
   implicit none

   integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   integer            :: errare
   character(len=103) :: cout
   character(len=20)  :: IAM
   integer :: nb_DISKx
   
   IAM = 'mod_DKDKx::stoc_rloc'

   nb_DISKx=get_nb_DISKx()

   ! sizing verlt:

   if (.not. allocated(verlt)) then

      allocate(verlt(nb_DISKx),stat=errare)
      if (errare /=0 ) then
         call FATERR(IAM,' error allocating verlt')
      end if
      do icdtac = 1,nb_DISKx
         verlt(icdtac)%adjsz = 0
         iadj = nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating verlt(icdtac)%.....')
         end if
      end do
   else 

      do icdtac=1,nb_DISKx
         verlt(icdtac)%adjsz=0
         call free_verlet_(icdtac)
         iadj = nb_adj(icdtac)

         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
      end do
   end if

   ! filling data:
   do icdan=1,nb_DKDKx
      icdtac = this(icdan)%icdtac                  ! serial number of candidate contactor for contact icdan
      iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
      iadj   = this(icdan)%iadj                    ! serial adjacent number of pair contactor 
                                                   ! adjacent to candidate contactor for contact icdan 
      verlt(icdtac)%icdan(iadj)   = icdan
      verlt(icdtac)%cdbdy         = diskx2bdyty(1,icdtac)
      verlt(icdtac)%cdtac         = diskx2bdyty(2,icdtac)
      verlt(icdtac)%cdmodel       = diskx2bdyty(3,icdtac)
      verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
      verlt(icdtac)%anbdy(iadj)   = diskx2bdyty(1,iantac)
      verlt(icdtac)%antac(iadj)   = diskx2bdyty(2,iantac)
      verlt(icdtac)%anmodel(iadj) = diskx2bdyty(3,iantac)
      verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
      !PRINT*,'icdan',diskx2bdyty(1,icdtac),diskx2bdyty(2,icdtac),diskx2bdyty(1,iantac),diskx2bdyty(2,iantac)

      verlt(icdtac)%status(iadj)  = this(icdan)%status
      verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
      verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
      verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
      verlt(icdtac)%vln(iadj)     = this(icdan)%vln
      verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT
      verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
      verlt(icdtac)%tuc(1,iadj)   = this(icdan)%nuc(2)
      verlt(icdtac)%tuc(2,iadj)   =-this(icdan)%nuc(1)
      verlt(icdtac)%coor(1:2,iadj)= this(icdan)%coor

      verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   end do

   nb_vDKDKx = nb_DKDKx

   WRITE(cout,'(1X,I10,A12)') nb_vDKDKx,' stock DKDKx'
   call logmes(cout)

 end subroutine stock_rloc
!
 subroutine experimental_stock_rloc
 
   implicit none

   integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   integer            :: errare
   character(len=103) :: cout
   character(len=21)  :: IAM
   integer :: nb_DISKx
   
         !123456789012345678901
   IAM = 'mod_DKDKx::stock_rloc'

   nb_DISKx=get_nb_DISKx()

   ! sizing verlt:
   if (.not. allocated(verlt)) then
      allocate(verlt(nb_DISKx),stat=errare)
      if (errare /=0 ) then
         call FATERR(IAM,' error allocating verlt')
      end if
      do icdtac = 1,nb_DISKx
        iadj = nb_adj(icdtac)
        verlt(icdtac)%adjsz=iadj

        iadj=max(iadj,6)

        call new_verlet_(icdtac, iadj, errare)

        if (errare /=0 ) then
          call FATERR(IAM,'error in allocating verlt(icdtac)%.....')
        end if
      end do
   else 
      do icdtac=1,nb_DISKx
        iadj = nb_adj(icdtac)
        verlt(icdtac)%adjsz=iadj

        if (iadj > size(verlt(icdtac)%icdan)) then

          call free_verlet_(icdtac)
          call new_verlet_(icdtac, iadj, errare)

        end if
      end do
   end if
   
   ! filling data:
   do icdan=1,nb_DKDKx
      icdtac = this(icdan)%icdtac                  ! serial number of candidate contactor for contact icdan
      iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
      iadj   = this(icdan)%iadj                    ! serial adjacent number of pair contactor 
                                                   ! adjacent to candidate contactor for contact icdan 
      verlt(icdtac)%icdan(iadj)   = icdan
      verlt(icdtac)%cdbdy         = diskx2bdyty(1,icdtac)
      verlt(icdtac)%cdtac         = diskx2bdyty(2,icdtac)
      verlt(icdtac)%cdmodel        = diskx2bdyty(3,icdtac)
      verlt(icdtac)%anbdy(iadj)   = diskx2bdyty(1,iantac)
      verlt(icdtac)%antac(iadj)   = diskx2bdyty(2,iantac)
      verlt(icdtac)%anmodel(iadj)  = diskx2bdyty(3,iantac)
      
      verlt(icdtac)%status(iadj)  = this(icdan)%status
      verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
      verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
      verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
      verlt(icdtac)%vln(iadj)     = this(icdan)%vln
      verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT
      verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
      verlt(icdtac)%tuc(1:2,iadj) = this(icdan)%tuc(1:2)

!mj    verlt(icdtac)%coor(1,iadj)  = DKcoor(1,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(1))
!mj    verlt(icdtac)%coor(2,iadj)  = DKcoor(2,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(2))
!mj    verlt(icdtac)%coor(1:2,iadj)= verlt(icdtac)%coor(1:2,iadj)
!fd    + get_shiftTT_DISKx(diskx2bdyty(1,icdtac),diskx2bdyty(2,icdtac))
!mj    coordinates of the mid gap point, for graphical purposes

      verlt(icdtac)%coor(1,iadj) = DKcoor(1,icdtac) &
                                 - (get_radius_DISKx(icdtac)+0.5D0*verlt(icdtac)%gapTT(iadj))*this(icdan)%nuc(1)
      verlt(icdtac)%coor(2,iadj) = DKcoor(2,icdtac) &
                                 - (get_radius_DISKx(icdtac)+0.5D0*verlt(icdtac)%gapTT(iadj))*this(icdan)%nuc(2) 
      
      verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   end do

   nb_vDKDKx = nb_DKDKx

   WRITE(cout,'(1X,I10,A12)') nb_vDKDKx,' stock DKDKx'
   call logmes(cout)

 end subroutine experimental_stock_rloc
!------------------------------------------------------------------------ 
! SUBROUTINE recup_rloc
! get data from Verlet list verlt and put into this
!------------------------------------------------------------------------ 
 subroutine recup_rloc_DKDKx

   implicit none

   integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   character(len=103) :: cout
   character(len=21)  :: IAM = 'mod_DKDKx::recup_rloc'

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if
   
   nb_recup_DKDKx = 0

   do icdan=1,nb_DKDKx

      this(icdan)%rlt         = 0.D0
      this(icdan)%rln         = 0.D0
      this(icdan)%statusBEGIN = i_nknow

      icdtac = this(icdan)%icdtac                ! serial number of candidate contactor for contact icdan
      iantac = this(icdan)%iantac                ! serial number of antagonist contactor for contact icdan 
       
      if (verlt(icdtac)%adjsz /= 0) then
         do iadj=1,verlt(icdtac)%adjsz

!fd A VOIR 03/01/08 ce test pourrait etre foireux en periodique car il n'y a plus
!fd unicité de l'ordre cd & an ... 

            if ( ( verlt(icdtac)%cdbdy        == diskx2bdyty(1,icdtac) .and. &
                   verlt(icdtac)%cdtac        == diskx2bdyty(2,icdtac) .and. &
                   verlt(icdtac)%cdmodel      == diskx2bdyty(3,icdtac) .and. &
                   verlt(icdtac)%anbdy(iadj)  == diskx2bdyty(1,iantac) .and. &
                   verlt(icdtac)%antac(iadj)  == diskx2bdyty(2,iantac) .and. &
                   verlt(icdtac)%anmodel(iadj)== diskx2bdyty(3,iantac)) .or. &
                 ( verlt(icdtac)%cdbdy        == diskx2bdyty(1,iantac) .and. &
                   verlt(icdtac)%cdtac        == diskx2bdyty(2,iantac) .and. &
                   verlt(icdtac)%cdmodel      == diskx2bdyty(3,iantac) .and. &
                   verlt(icdtac)%anbdy(iadj)  == diskx2bdyty(1,icdtac) .and. &
                   verlt(icdtac)%antac(iadj)  == diskx2bdyty(2,icdtac) .and. &
                   verlt(icdtac)%anmodel(iadj)== diskx2bdyty(3,icdtac))      &
               ) then
               this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H
               this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H 
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
               
               this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)

               nb_recup_DKDKx = nb_recup_DKDKx+1
               exit
            end if
         end do
      end if
   end do

   write(cout,'(1X,I10,A12)') nb_recup_DKDKx,' recup DKDKx'
   call logmes(cout)

 end subroutine recup_rloc_DKDKx
!------------------------------------------------------------------------
! SUBROUTINE read_ini_Vloc_Rloc
!
! get data from file Vloc_Rloc.INI and put into a Verlet list
!------------------------------------------------------------------------ 
!mj a !fd Voila le genre de truc qu'il faut eviter, changer le format des fichiers de donnees. Les benchmarks 
!mj ne marchent plus parcequ'ils ne savent plus lire les donnees. Comme, entre tous les utilisateurs, il 
!mj y a bien une centaine de benchs, c'est la joie. Il y a deux solutions. Soit faire un read_old, soit 
!mj tenter une acrobatie pour lire des vieux fichiers. De toutes facons, j'ai "rationalise" l'ordre
!mj des donnees, ce qui veut dire que je me suis arrange pour que le nouveau fichier, soit construit par
!mj AJOUT de donnees sur l'ancien fichier, sans DEPLACER les anciennes donnees. Au moins, je peux lire
!mj MES anciens fichiers. Egoiste va !

!mj A par ca, ces nouvelles donnees, gap, nuc, etc. sont inutiles, car elles sont recalculees dans PROX TACTORS.
!mj J'espere qu'elles sont utiles par ailleurs dans le Verlet pour le traitement graphique par exemple.
!mj Du coup, j'ai ajoute vlt,vln et gapTT. Ainsi, les fichiers Vloc_Rloc.INI et Vloc_Rloc.LAST ont
!mj le meme format avec les memes data. 

!mj J'ai des doutes sur la signification de la variable iadj affichee et stockee. Il ya une redondance
!mj a la stocker dans verlt.

!mj Il a fallu faire les modifs dans tous les fichiers contacteurs. Un type ou une subroutine genre
!mj read_Vloc_Rloc_INI('DKDKx') serait bienvenu. Idem pour stock_rloc, recup_rloc. Une solution 
!mj pour ces dernieres subroutines consisterait a utiliser un include, qui est de mauvais gout parait-il
!mj en fortran90.
 
!mj coo1,coo2, pourraient etre les coordonnes du point milieu de l'interstice. C'est ce que fait JJ
!mj et qui peut etre utile pour le graphique. Pour le moment, je n'ai rien change.

!mj J'ai repere UNE GROSSE FAUTE dans le scanner read_ini:
!mj if (G_clin(9:13)/= 'DKDKx') qui ne trouve pas le caractere DKDKx
!mj de sorte que read_ini ne fait rien. Donc les restarts sont fantaisistes, en ce sens 
!mj que les reactions sont initialisees a O. et le statut a "unknown". Je ne sais si cette erreur, qui doit trainer
!mj depuis un moment, vient de Montpellier ou de Marseille. Peut etre que vos nouveaux fichiers portent
!mj le caractere DKDKx, mais pas les miens. Voir mon scanner qui doit marcher pour tous.
!---------------------------------------------------------------------------------------------------
 subroutine read_ini_Vloc_Rloc

   implicit none
   
   integer                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   integer                          :: cdmodel, anmodel, errare
   integer                          :: ibehav,nb_internal,i_internal
   real(kind=8)                     :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)        :: nuc,coor
   character(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus

   character(len=103) :: cout
   character(len=29)  :: IAM = 'mod_DKDKx::read_ini_Vloc_Rloc'
   integer :: nb_DISKx

!fd new 24/10/08
   real(kind=8),dimension(2) :: tuc    
   real(kind=8),dimension(3) :: cdreac,anreac    
   integer     ,dimension(3) :: cdccdof,anccdof

   nb_DISKx = get_nb_DISKx()

   ! first reading: sizing verlt
   ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
   ! For this purpose nb_adj is introduced.

   if (.not. allocated(nb_adj)) then
     allocate(nb_adj(nb_DISKx),stat=errare)
     if (errare /=0 ) call FATERR(IAM,' error allocating nb_adj')
   end if

   nb_adj=0

   do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle
      ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKDKx') cycle     
      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit
      if(xxl_check)then
         read(G_clin(1:79),'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      else
         read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      end if
      if (cdtac /= 'DISKx' .or. antac /= 'DISKx') cycle
      cdmodel = get_body_model_id_from_name( cdbdy )
      do icdtact = 1, nb_DISKx
         if (diskx2bdyty(1,icdtact) == icdbdy .and. &
             diskx2bdyty(2,icdtact) == icdtac .and. &
             diskx2bdyty(3,icdtact) == cdmodel ) then
            nb_adj(icdtact) = nb_adj(icdtact) + 1
            exit
         end if
      end do
      cycle
   end do

   if (.not. allocated(verlt)) then
      allocate(verlt(nb_DISKx),stat=errare)
      if (errare /=0 ) then
         call FATERR(IAM,' error allocating verlt')
      end if

      do icdtac=1,nb_DISKx
         verlt(icdtac)%adjsz=0
         iadj=nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating verlt(icdtac)%.....')
         end if
      end do
   else 
      do icdtac=1,nb_DISKx
         call free_verlet_(icdtac)

         verlt(icdtac)%adjsz=0
         iadj=nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
      end do
   end if

   ! second reading: filling data
   rewind(G_nfich)

   nb_adj=0
   icdan = 0
   
   do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKDKx') cycle     
      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit
      if(xxl_check)then
         read(G_clin(1:79),'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      else
         read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      endif
      if (cdtac /= 'DISKx' .or. antac /= 'DISKx') cycle
      cdmodel = get_body_model_id_from_name( cdbdy )
      anmodel = get_body_model_id_from_name( anbdy )
      do icdtact=1,nb_DISKx
         if ( diskx2bdyty(1,icdtact) == icdbdy .and. &
              diskx2bdyty(2,icdtact) == icdtac .and. &
              diskx2bdyty(3,icdtact) == cdmodel ) then

            icdan = icdan + 1

            nb_adj(icdtact) = nb_adj(icdtact) + 1
            verlt(icdtact)%icdan(nb_adj(icdtact))  = icdan
            verlt(icdtact)%cdbdy                   = icdbdy
            verlt(icdtact)%cdtac                   = icdtac
            verlt(icdtact)%cdmodel                 = cdmodel
            verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
            verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
            verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
            verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
            if( .not. read_G_clin()) exit
            read(G_clin(1:90),'(27X,2(7X,D14.7))') rlt,rln
            verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
            verlt(icdtact)%rln(nb_adj(icdtact)) = rln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') vlt,vln
            verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
            verlt(icdtact)%vln(nb_adj(icdtact)) = vln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
            verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'n(1)=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
               verlt(icdtact)%nuc(1,nb_adj(icdtact)) = nuc(1)
               verlt(icdtact)%nuc(2,nb_adj(icdtact)) = nuc(2)
               verlt(icdtact)%tuc(1,nb_adj(icdtact)) = nuc(2)
               verlt(icdtact)%tuc(2,nb_adj(icdtact)) =-nuc(2)
            else 
               backspace(G_nfich)
            end if
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'coo1=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
               verlt(icdtact)%coor(1,nb_adj(icdtact)) = coor(1)
               verlt(icdtact)%coor(2,nb_adj(icdtact)) = coor(2)
            else 
               backspace(G_nfich)
            end if
            
            verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact)) = 0.d0
            ibehav      = get_ibehav(behav)
            nb_internal = get_nb_internal(ibehav)
            
            if (nb_internal /= 0 ) then  
               if( .not. read_G_clin()) exit
               do i_internal=1, nb_internal
                  read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
                       verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
               end do
            end if
            exit
         end if
      end do
      cycle
   end do

   nb_vDKDKx=0
   
   do icdtact=1,nb_DISKx
      nb_vDKDKx = nb_vDKDKx + nb_adj(icdtact)
      
      if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
         write(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
              'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         call FATERR(IAM,cout)
      end if
   end do

!fd 24/10/08 
! one try to rebuild the reac field

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         iantac = verlt(icdtact)%antac(iadj)

         call nullify_reac_DISKx(icdtact,iIreac)
         call nullify_reac_DISKx(iantac,iIreac)
      enddo
   enddo

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         icdbdy = verlt(icdtact)%cdbdy
         ianbdy = verlt(icdtact)%anbdy(iadj)
         iantac = verlt(icdtact)%antac(iadj)
         RlN = verlt(icdtact)%rln(iadj)*H
         RlT = verlt(icdtact)%rlt(iadj)*H
         nuc(1:2) = verlt(icdtact)%nuc(1:2,iadj)
         tuc(1) = nuc(2)
         tuc(2) =-nuc(1)

         cdccdof(1) = 1
         anccdof(1) = 1
         cdreac(1)  = RlT*tuc(1)+RlN*nuc(1)
         anreac(1)  =-cdreac(1)
         cdccdof(2) = 2
         anccdof(2) = 2
         cdreac(2)  = RlT*tuc(2)+RlN*nuc(2)      
         anreac(2)  =-cdreac(2)
         cdccdof(3) = 3
         anccdof(3) = 3
         cdreac(3) = 0.d0
         anreac(3) = 0.d0

         call add_reac_DISKx(icdtact,cdccdof,cdreac,iIreac)
         call add_reac_DISKx(iantac,anccdof,anreac,iIreac)
      enddo
   enddo
 end subroutine read_ini_Vloc_Rloc

 subroutine experimental_read_ini_Vloc_Rloc

   implicit none
   
   integer                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   integer                          :: cdmodel, anmodel, errare
   integer                          :: ibehav,nb_internal,i_internal
   real(kind=8)                     :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)        :: nuc,coor
   character(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus

!fd new 24/10/08
   real(kind=8),dimension(2) :: tuc    
   real(kind=8),dimension(3) :: cdreac,anreac    
   integer     ,dimension(3) :: cdccdof,anccdof

   character(len=103) :: cout
   character(len=29)  :: IAM = 'mod_DKDKx::read_ini_Vloc_Rloc'
   integer :: nb_DISKx

   nb_DISKx = get_nb_DISKx()

   !<fd le 16/10/08
   ! on veut reduire le nombre d'alloc/dealloc 
   !
   !df>  

   ! first reading: sizing verlt
   ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
   ! For this purpose nb_adj is introduced.

   if (.not. allocated(nb_adj)) allocate(nb_adj(nb_DISKx),stat=errare)
   if (errare /=0 ) then
      call FATERR(IAM,' error allocating nb_adj')
   end if

   nb_adj=0

   do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle
      ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKDKx') cycle     
      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit
      if(xxl_check)then
         read(G_clin(1:79),'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      else
         read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      end if
      if (cdtac /= 'DISKx' .or. antac /= 'DISKx') cycle
      cdmodel = get_body_model_id_from_name( cdbdy )
      do icdtact = 1, nb_DISKx
         if (diskx2bdyty(1,icdtact) == icdbdy .and. &
             diskx2bdyty(2,icdtact) == icdtac .and. &
             diskx2bdyty(3,icdtact) == cdmodel ) then
            nb_adj(icdtact) = nb_adj(icdtact) + 1
            exit
         end if
      end do
      cycle
   end do

   if (.not. allocated(verlt)) then
      allocate(verlt(nb_DISKx),stat=errare)
      if (errare /=0 ) then
         call FATERR(IAM,' error allocating verlt')
      end if

      do icdtac=1,nb_DISKx
         iadj=nb_adj(icdtac)
         verlt(icdtac)%adjsz=iadj

!fd choix arbitraire

         iadj=max(iadj,6)

         call new_verlet_(icdtac, iadj, errare)

         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating verlt(icdtac)%.....')
         end if
      end do
   else 
      do icdtac=1,nb_DISKx
         iadj=nb_adj(icdtac)
         verlt(icdtac)%adjsz=iadj
         if (iadj > size(verlt(icdtac)%icdan)) then

           call free_verlet_(icdtac)
           call new_verlet_(icdtac, iadj, errare)

         end if
      end do
   end if
    
   ! second reading: filling data
   rewind(G_nfich)

   nb_adj=0
   
   do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKDKx') cycle     
      if ( .not. read_G_clin()) exit
      if ( .not. read_G_clin()) exit
      if(xxl_check)then
         read(G_clin(1:79),'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      else
         read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdbdy,icdbdy,cdtac,icdtac,                                          &
              behav,                                                              &
              anbdy,ianbdy,antac,iantac,                                          &
              sttus
      endif
      if (cdtac /= 'DISKx' .or. antac /= 'DISKx') cycle
      cdmodel = get_body_model_id_from_name( cdbdy )
      anmodel = get_body_model_id_from_name( anbdy )
      do icdtact = 1, nb_DISKx
         if ( diskx2bdyty(1,icdtact) == icdbdy .and. &
              diskx2bdyty(2,icdtact) == icdtac .and. &
              diskx2bdyty(3,icdtact) == cdmodel ) then
            nb_adj(icdtact) = nb_adj(icdtact) + 1
            !verlt(icdtact)%icdan(nb_adj(icdtact))=icdan
            verlt(icdtact)%cdbdy                   = icdbdy
            verlt(icdtact)%cdtac                   = icdtac
            verlt(icdtact)%cdmodel                  = cdmodel
            verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
            verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
            verlt(icdtact)%anmodel(nb_adj(icdtact)) = anmodel
            verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
            if( .not. read_G_clin()) exit
            read(G_clin(1:90),'(27X,2(7X,D14.7))') rlt,rln
            verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
            verlt(icdtact)%rln(nb_adj(icdtact)) = rln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') vlt,vln
            verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
            verlt(icdtact)%vln(nb_adj(icdtact)) = vln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
            verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'n(1)=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
               verlt(icdtact)%nuc(1,nb_adj(icdtact)) = nuc(1)
               verlt(icdtact)%nuc(2,nb_adj(icdtact)) = nuc(2)
               verlt(icdtact)%tuc(1,nb_adj(icdtact)) = nuc(2)
               verlt(icdtact)%tuc(2,nb_adj(icdtact)) =-nuc(1)
            else 
               backspace(G_nfich)
            end if
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'coo1=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
               verlt(icdtact)%coor(1,nb_adj(icdtact)) = coor(1)
               verlt(icdtact)%coor(2,nb_adj(icdtact)) = coor(2)
            else 
               backspace(G_nfich)
            end if
            
            verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact)) = 0.d0
            ibehav      = get_ibehav(behav)
            nb_internal = get_nb_internal(ibehav)
            
            if (nb_internal /= 0 ) then  
               if( .not. read_G_clin()) exit
               do i_internal=1, nb_internal
                  read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
                       verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
               end do
            end if
            exit
         end if
      end do
      cycle
   end do
   
   nb_vDKDKx=0
   
   do icdtact=1,nb_DISKx
      nb_vDKDKx = nb_vDKDKx + nb_adj(icdtact)
      
      if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
         write(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
              'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         call FATERR(IAM,cout)
      end if
   end do

!fd 24/10/08 
! one try to rebuild the reac field

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         iantac = verlt(icdtact)%antac(iadj)

         call nullify_reac_DISKx(icdtact,iIreac)
         call nullify_reac_DISKx(iantac,iIreac)
      enddo
   enddo

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         icdbdy = verlt(icdtact)%cdbdy
         ianbdy = verlt(icdtact)%anbdy(iadj)
         iantac = verlt(icdtact)%antac(iadj)
         RlN = verlt(icdtact)%rln(iadj)*H
         RlT = verlt(icdtact)%rlt(iadj)*H
         nuc(1:2) = verlt(icdtact)%nuc(1:2,iadj)

         tuc(1) = nuc(2)
         tuc(2) =-nuc(1)

         cdccdof(1) = 1
         anccdof(1) = 1
         cdreac(1)  = RlT*tuc(1)+RlN*nuc(1)
         anreac(1)  =-cdreac(1)
         cdccdof(2) = 2
         anccdof(2) = 2
         cdreac(2)  = RlT*tuc(2)+RlN*nuc(2)      
         anreac(2)  =-cdreac(2)
         cdccdof(3) = 3
         anccdof(3) = 3
         cdreac(3) = 0.d0
         anreac(3) = 0.d0

         call add_reac_DISKx(icdtact,cdccdof,cdreac,iIreac)
         call add_reac_DISKx(iantac,anccdof,anreac,iIreac)
      enddo
   enddo
 end subroutine experimental_read_ini_Vloc_Rloc
!------------------------------------------------------------------------   
! SUBROUTINE write_out_Vloc_Rloc:
!
! write into file out_Vloc_Rloc data from this, in verlt style
!------------------------------------------------------------------------ 
 subroutine write_out_Vloc_Rloc(nfich)
   
   implicit none

   integer                   :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
   integer                   :: nfich,icdtact
   real(kind=8),dimension(2) :: coor
   integer :: nb_DISKx

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   !fd pourquoi n'ecrit on pas le verlet !
   !mj pour des raisons de lisibilite

   nb_DISKx=get_nb_DISKx()
  
   if( nb_vDKDKx > 99999 ) then
       xxl_check = .true.
   end if

   if(xxl_check)then
      do icdtact=1,nb_DISKx    
         do iadj=1,nb_adj(icdtact)         
            icdan  = adjac(icdtact)%icdan(iadj)
            icdtac = this(icdan)%icdtac
            iantac = this(icdan)%iantac
            
            ! (coordinates of the contact point if contact is active)
            coor(1) = DKcoor(1,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(1))
            coor(2) = DKcoor(2,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(2))
            
            cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
            anmodel = get_body_model_name_from_id( diskx2bdyty(3,iantac) )
            write(nfich,'(A6,2X,A5,2X,I10)')'$icdan','DKDKx',icdan     
            !12345678901234567890123456789012345678901234567890123456789012345678901234567890123456  
            write(nfich,'(A86)')' cdbdy       numbr  cdtac  numbr  behav  anbdy       numbr  antac  numbr  sttus   iadj'
            write(nfich,'(1X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,A5,2X,I10,2X,A5,2X,I5,2X,A5,2X,I5)')   &
                 cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),  &
                 see(this(icdan)%isee)%behav,  &
                 anmodel,diskx2bdyty(1,iantac),'DISKx',diskx2bdyty(2,iantac),  &
                 get_contact_status_name_from_id(this(icdan)%status),iantac
            if (smooth_method) then
              write(nfich,104)'rlt  ',this(icdan)%rlt   ,'rln  ',this(icdan)%rln   ,'rls  ',0.D0
            else
              write(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',0.D0
            endif
            write(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',0.D0
            write(nfich,103)'gapTT',this(icdan)%gapTT 
            write(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
            write(nfich,104)'coo1=',coor(1)           ,'coo2=',coor(2)           ,'coo3=',0.D0
            
            if (this(icdan)%nb_internal /= 0) then
               call write_internal_comment(nfich,this(icdan)%lawnb)
               write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
               write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
            endif
            
            write(nfich,'(A1)')' '               
         end do
      end do
   else
      do icdtact=1,nb_DISKx    
         do iadj=1,nb_adj(icdtact)         
            icdan  = adjac(icdtact)%icdan(iadj)
            icdtac = this(icdan)%icdtac
            iantac = this(icdan)%iantac
            
            ! (coordinates of the contact point if contact is active)
            coor(1) = DKcoor(1,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(1))
            coor(2) = DKcoor(2,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(2))
            
            cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
            anmodel = get_body_model_name_from_id( diskx2bdyty(3,iantac) )
            write(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKDKx',icdan     
            !1234567890123456789012345678901234567890123456789012345678901234567890123456  
            write(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'     
            write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
                 cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),  &
                 see(this(icdan)%isee)%behav,  &
                 anmodel,diskx2bdyty(1,iantac),'DISKx',diskx2bdyty(2,iantac),  &
                 get_contact_status_name_from_id(this(icdan)%status),iantac
            if (smooth_method) then
              write(nfich,104)'rlt  ',this(icdan)%rlt   ,'rln  ',this(icdan)%rln   ,'rls  ',0.D0
            else
              write(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',0.D0
            endif
            write(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',0.D0
            write(nfich,103)'gapTT',this(icdan)%gapTT 
            write(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
            write(nfich,104)'coo1=',coor(1)           ,'coo2=',coor(2)           ,'coo3=',0.D0
            
            if (this(icdan)%nb_internal /= 0) then
               write(nfich,'(6(1x,D14.7))')  this(icdan)%internal(1:this(icdan)%nb_internal)
            endif
            
            write(nfich,'(A1)')' '               
         end do
      end do
   endif

103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

 end subroutine write_out_Vloc_Rloc
!------------------------------------------------------------------------ 
!! !> \brief Write the local velocity and reaction of an interaction to a file
!! subroutine write_out_one_Vloc_Rloc_dkdkx(nfich, inter)
!!   implicit none
!!   !> the unit number in which to write
!!   integer(kind=4), intent(in) :: nfich
!!   !> the interaction to write
!!   type(T_interaction) :: inter
!!
!!   write(nfich,'(A6,2X,A5,2X,I7)')'$icdan', 'DKDKx' , inter%icdan
!!   !1234567890123456789012345678901234567890123456789012345678901234567890123456  
!!   write(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'     
!!   write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')        &
!!        'RBDY2', inter%icdbdy, 'DISKx', diskx2bdyty(2, inter%icdtac), see(inter%isee)%behav, &
!!        'RBDY2', inter%ianbdy, 'DISKx', diskx2bdyty(2, inter%iantac), inter%status,inter%iantac
!!   write(nfich,104)'rlt/H',inter%rl(1)/H,'rln/H',inter%rl(2)/H,'rls/H',0.D0
!!   write(nfich,104)'vlt =',inter%vl(1)  ,'vln =',inter%vl(2)  ,'vls =',0.D0
!!   write(nfich,103)'gapTT',inter%gapTT 
!!   write(nfich,104)'n(1)=',inter%uc(1,2),'n(2)=',inter%uc(2,2),'n(3)=',0.d0
!!   write(nfich,104)'coo1=',inter%coor(1),'coo2=',inter%coor(2),'coo3=',0.d0
!!   
!!   if (inter%nb_internal /= 0) then
!!     write(nfich,'(6(1x,D14.7))')  inter%internal(1:inter%nb_internal)
!!   end if
!!   write(nfich,'(A1)')' '               
!!
!!103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
!!104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
!!
!! end subroutine

!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine nullify_reac_DKDKx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,storage
    
   icdtac = this(icdan)%icdtac
   call nullify_reac_DISKx(icdtac,storage)
   
   iantac = this(icdan)%iantac
   call nullify_reac_DISKx(iantac,storage)
   
 end subroutine nullify_reac_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine nullify_vlocy_DKDKx(icdan,storage)

   implicit none

   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,storage
    
   icdtac = this(icdan)%icdtac
   call nullify_vlocy_DISKx(icdtac,storage)
   
   iantac = this(icdan)%iantac
   call nullify_vlocy_DISKx(iantac,storage)
    
 end subroutine nullify_vlocy_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine vitrad_DKDKx( icdan, storage, need_full_V )

   implicit none

   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,storage
   logical, optional  :: need_full_V
   
   icdtac = this(icdan)%icdtac
   call comp_vlocy_DISKx(icdtac,storage)
   
   iantac = this(icdan)%iantac
   call comp_vlocy_DISKx(iantac,storage)
   
 end subroutine vitrad_DKDKx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 subroutine injj_DKDKx(icdan,RTIK,RNIK,storage)
 
   implicit none

   integer,intent(in)        :: icdan
   integer                   :: storage
   integer,dimension(3)      :: cdccdof,anccdof
   real(kind=8),intent(in)   :: RTIK,RNIK
   real(kind=8),dimension(3) :: cdreac, anreac

   cdccdof(1) = 1
   anccdof(1) = 1
   cdreac(1)  = RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)  =-cdreac(1)
   cdccdof(2) = 2
   anccdof(2) = 2
   cdreac(2)  = RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)  =-cdreac(2)
   cdccdof(3) = 3
   anccdof(3) = 3

!fd modifs pour l'excentrement
!fd   cdreac(3)= this(icdan)%Gcdt3*RTIK
!fd   anreac(3)=-this(icdan)%Gant3*RTIK

   cdreac(3) = this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK
   anreac(3) =-this(icdan)%Gant3*RTIK-this(icdan)%Gann3*RNIK

   call add_reac_DISKx(this(icdan)%icdtac,cdccdof,cdreac,storage)
   call add_reac_DISKx(this(icdan)%iantac,anccdof,anreac,storage)
   
 end subroutine injj_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 subroutine prjj_DKDKx(icdan,VTIK,VNIK,storage)
 
   implicit none

   integer     ,intent(in)   :: icdan
   integer     ,intent(in)   :: storage
   real(kind=8),intent(out)  :: VTIK,VNIK
   real(kind=8),dimension(3) :: Vcd,Van
   real(kind=8)              :: Vdcd,Vdan,Vd
   
   if (storage == iVfree) then
     ! fd dila
     call get_Vd_DISKx(this(icdan)%icdtac,Vdcd)
     call get_Vd_DISKx(this(icdan)%iantac,Vdan)

     Vd=Vdcd+Vdan
   
     !print*,icdtac,Vthcd
     !print*,iantac,Vthan

   else 
     Vd=0.d0
   endif


   call get_vlocy_DISKx(this(icdan)%icdtac,storage,Vcd)
   call get_vlocy_DISKx(this(icdan)%iantac,storage,Van)     

!fd modifs pour l'excentrement
!fd   VTIK=Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3  &
!fd       -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3
!fd   VNIK=Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)                           &
!fd       -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2) 

   VTIK = Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3  &
        - Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3

   VNIK = Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        - Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3 &
        - Vd

 end subroutine prjj_DKDKx 
!!$!------------------------------------------------------------------------ 
!!$ subroutine compute_Wikik_DKDKx(icdan,WTT,WTN,WNT,WNN)
!!$
!!$   implicit none
!!$   
!!$   integer                   :: icdan,icdbdy,ianbdy
!!$   real(kind=8)              :: WTT,WTN,WNT,WNN
!!$   real(kind=8),dimension(3) :: icdmass,ianmass
!!$   
!!$   icdbdy = this(icdan)%icdbdy
!!$   ianbdy = this(icdan)%ianbdy
!!$   
!!$   icdmass = get_inv_mass_DISKx(icdbdy)
!!$   ianmass = get_inv_mass_DISKx(ianbdy)
!!$
!!$   WTT =  icdmass(1)+icdmass(3)*this(icdan)%Gcdt3*this(icdan)%Gcdt3 &
!!$        + ianmass(1)+ianmass(3)*this(icdan)%Gant3*this(icdan)%Gant3
!!$   WNN =  icdmass(1)+icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdn3 &
!!$        + ianmass(1)+ianmass(3)*this(icdan)%Gann3*this(icdan)%Gann3
!!$   WTN =  icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdt3 &
!!$        + ianmass(3)*this(icdan)%Gann3*this(icdan)%Gant3
!!$   WNT = WTN
!!$
!!$ end subroutine compute_Wikik_DKDKx
!!$ !------------------------------------------------------------------------ 
!!$ subroutine get_Wik_DKDKx(icdan,ikcd,ikan,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
!!$
!!$   implicit none
!!$
!!$   integer                   :: icdan,ikcd,ikan
!!$   real(kind=8)              :: ikGcdt,ikGcdn,ikGant,ikGann
!!$   real(kind=8),dimension(3) :: ikcdmass,ikanmass
!!$   real(kind=8),dimension(2) :: tik,nik
!!$   
!!$   ikcd     = this(icdan)%icdbdy
!!$   ikan     = this(icdan)%ianbdy
!!$   ikGcdt   = this(icdan)%Gcdt3
!!$   ikGcdn   = this(icdan)%Gcdn3
!!$   ikGant   = this(icdan)%Gant3
!!$   ikGann   = this(icdan)%Gann3
!!$   ikcdmass = get_inv_mass_DISKx(ikcd)
!!$   ikanmass = get_inv_mass_DISKx(ikan)
!!$   tik      = this(icdan)%tuc
!!$   nik      = this(icdan)%nuc
!!$   
!!$ end subroutine get_Wik_DKDKx
!!$!------------------------------------------------------------------------ 
!!$ subroutine compute_Wikjl_DKDKx(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$   implicit none
!!$   integer                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy,janbdy
!!$   real(kind=8)              :: WTT,WTN,WNT,WNN
!!$   real(kind=8),dimension(3) :: icdmass,ianmass,jcdmass,janmass
!!$   
!!$   icdbdy = this(icdan)%icdbdy
!!$   ianbdy = this(icdan)%ianbdy
!!$   jcdbdy = this(jcdan)%icdbdy
!!$   janbdy = this(jcdan)%ianbdy
!!$   
!!$   icdmass = get_inv_mass_DISKx(icdbdy)
!!$   ianmass = get_inv_mass_DISKx(ianbdy)
!!$   jcdmass = get_inv_mass_DISKx(jcdbdy)
!!$   janmass = get_inv_mass_DISKx(janbdy)
!!$   
!!$   !cas ik-il
!!$   if (icdbdy == jcdbdy) then
!!$      
!!$      WTT =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$           +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdt3
!!$      
!!$      WTN =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$           +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdn3
!!$      
!!$      WNT =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$           +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdt3
!!$      
!!$      WNN =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$           +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdn3
!!$      !cas ik-jk
!!$   else if (ianbdy == janbdy) then
!!$      
!!$      WTT =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$           +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gant3
!!$      
!!$      WTN =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$           +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gann3
!!$      
!!$      WNT =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$           +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gant3
!!$      
!!$      WNN =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$           +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gann3
!!$      !cas ik-kl
!!$   else if (ianbdy == jcdbdy) then
!!$      
!!$      WTT = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$           -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdt3
!!$      
!!$      WTN = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$           -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdn3
!!$      
!!$      WNT = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$           -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdt3
!!$      
!!$      WNN = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$           -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdn3
!!$   else
!!$      !cas ik-ji
!!$      
!!$      WTT = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$           -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gant3
!!$      
!!$      WTN = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$           -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gann3
!!$      
!!$      WNT = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$           -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gant3
!!$      
!!$      WNN = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$           -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gann3
!!$   end if
!!$   
!!$ end subroutine compute_Wikjl_DKDKx
!------------------------------------------------------------------------ 
 subroutine compute_stress_DKDKx

   implicit none
   integer(kind=4)             :: icdan,ID_RBDY2
   integer(kind=4)             :: icdbdy,ianbdy,icdtac,iantac
   real(kind=8),dimension(2)   :: Fik,Lcd,Lan,coor
   real(kind=8),dimension(3)   :: tmp
   real(kind=8),dimension(2,2) :: SIGMA

   do icdan=1,nb_DKDKx

      Fik = this(icdan)%rln*this(icdan)%nuc + this(icdan)%rlt*this(icdan)%tuc

      icdtac = this(icdan)%icdtac
      
      ID_RBDY2 = diskx2bdyty(1,icdtac)

      tmp  = get_coorTT_DISKx(icdtac)
      coor = tmp(1:2) - get_shiftTT_DISKx(icdtac)

      Lcd = coor - this(icdan)%coor

      SIGMA(1,1:2) = Lcd(1)*Fik(1:2)
      SIGMA(2,1:2) = Lcd(2)*Fik(1:2)

      call add_stress_DISKx(ID_RBDY2,SIGMA)

      iantac = this(icdan)%iantac

      ID_RBDY2 = diskx2bdyty(1,iantac)

      tmp  = get_coorTT_DISKx(iantac)
      coor = tmp(1:2) - get_shiftTT_DISKx(iantac)

      Lan = coor - this(icdan)%coor

      SIGMA(1,1:2) =-Lan(1)*Fik(1:2)
      SIGMA(2,1:2) =-Lan(2)*Fik(1:2)

      call add_stress_DISKx(ID_RBDY2,SIGMA)

   end do

 end subroutine compute_stress_DKDKx
!------------------------------------------------------------------------ 
 integer function get_nb_DKDKx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_DKDKx = nb_DKDKx
   case(i_verlet_tactor)
      get_nb_DKDKx = nb_vDKDKx
   case(i_rough_tactor)
      get_nb_DKDKx = nb_rough_DKDKx
   case(i_recup_tactor)
      get_nb_DKDKx = nb_recup_DKDKx
   end select

 end function get_nb_DKDKx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine DKDKx2DISKx(icdan,icdtac,iantac)

   implicit none

   integer :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

 end subroutine DKDKx2DISKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_periode_DKDKx(icdtac)

   implicit none

   integer :: icdtac

   if(.not.allocated(periodic_DKDKx)) then
        get_periode_DKDKx = 0
        return
   endif

   get_periode_DKDKx = periodic_DKDKx(icdtac)

 end function get_periode_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 subroutine print_info_DKDKx(icdan)

   implicit none

   integer           :: icdan,icdtac,iantac,icdbdy,ianbdy

   character(len=80) :: cout

   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

   write(cout,1) icdtac,iantac
   call LOGMES(cout,.TRUE.)
   
1  format(1X,'DISKx:',1x,I5,1x,'DISKx:',1x,I5)

   icdbdy = this(icdan)%icdbdy
   ianbdy = this(icdan)%ianbdy

   call print_info_DISKx(icdbdy)
   call print_info_DISKx(ianbdy)

 end subroutine print_info_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 real(kind=8) function get_length_DKDKx(icdan)

   implicit none

   integer,intent(in) :: icdan 
   integer            :: icdtac
   real(kind=8)       :: raycd,rayan

   raycd   = get_radius_DISKx(this(icdan)%icdtac)
   rayan   = get_radius_DISKx(this(icdan)%iantac)

   get_length_DKDKx = (raycd*rayan)/(rayan+raycd)
   
 end function get_length_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
!!! > md > !!!
!------------------------------------------------------------------------
 subroutine get_eff_DKDKx(icdan,meff,reff)

   implicit none

   integer      :: icdan
   real(kind=8) :: meff,reff
   
   reff = this(icdan)%reff
   
   meff = this(icdan)%meff

 end subroutine get_eff_DKDKx
!------------------------------------------------------------------------
logical function RUN_DKDKx(fantome)

  implicit none
  integer,optional :: fantome

  RUN_DKDKx = RUN_TACTOR

end function RUN_DKDKx
!------------------------------------------------------------------------
  logical function CHECK_DKDKx()
    implicit none
    !   
    integer :: isee, nb_DISKx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_DKDKx = check_DKDKx_
      return
    end if

    con_pedigree%module_name = 'DKDKx'

    con_pedigree%id_cdan  = i_dkdkx
    con_pedigree%id_cdtac = i_diskx
    con_pedigree%id_antac = i_diskx

    cdtact2bdyty => diskx2bdyty
    antact2bdyty => diskx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_DISKx = get_nb_DISKx()
    if( nb_DISKx < 2 ) then
      CHECK_DKDKx = check_DKDKx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'DISKx') then
        check_DKDKx_ = .true.
        exit
      end if
    end do
  
    CHECK_DKDKx = check_DKDKx_
    return
  
  end function CHECK_DKDKx
  !------------------------------------------------------------------------ 
  logical function get_write_Vloc_Rloc_DKDKx(fantome)
    implicit none
    integer,optional :: fantome

    get_write_Vloc_Rloc_DKDKx = write_Vloc_Rloc

  end function get_write_Vloc_Rloc_DKDKx
!------------------------------------------------------------------------ 
  subroutine update_fric_DKDKx(icdan,fric)
    implicit none
    integer(kind=4) :: icdan,icdtact,iantact,isect
    real(kind=8)    :: WScd,WSan,fric
    
    if (nb_WSsect .eq. 1)then
       isect = 1
       icdtact     = this(icdan)%icdtac
       iantact     = this(icdan)%iantac
    
       WScd = get_WS_DISKx(diskx2bdyty(1,icdtact),diskx2bdyty(2,icdtact),isect)
       WSan = get_WS_DISKx(diskx2bdyty(1,iantact),diskx2bdyty(2,iantact),isect)
       !mr experimental model
       select case(i_friction_model)
       case(i_min_friction)
          fric = min(WScd,WSan)
       case(i_max_friction)
          fric = max(WScd,WSan)
       case(i_average_friction)
          fric = 0.5*(WScd+WSan)
       end select
    end if
    
  end subroutine update_fric_DKDKx
!------------------------------------------------------------------------ 
!MR&VHN  
  !> brief Give the number of sector of contactor surface
  subroutine set_surface_sectors_DKDKx(nb)
    implicit none
    integer(kind=4),intent(in) :: nb

    nb_WSsect = 2*nb

  end subroutine set_surface_sectors_DKDKx
!------------------------------------------------------------------------ 
!MR&VHN
  !> brief
  subroutine update_cohe_DKDKx(icdan,cohe)
    implicit none
    integer(kind=4)            :: icdan,itact,isect,i
    real(kind=8)               :: c,s,cohe
    real(kind=8),dimension(2)  :: IOcd,IOan,IO
    real(kind=8)               :: WScd,WSan,da,cdradius,anradius,cdli,anlj
    
    itact     = this(icdan)%icdtac
    IOcd(1:2) = this(icdan)%coor(1:2) - DKcoor(1:2,itact)

    c=cos(DKcoor(3,itact))
    s=sin(DKcoor(3,itact))

    IO(1) = c*IOcd(1) + s*IOcd(2)
    IO(2) =-s*IOcd(1) + c*IOcd(2)
    norm = SQRT(IO(1)*IO(1)+IO(2)*IO(2))
    IO = IO/norm

    da = PI_g*2.d0/REAL(nb_WSsect,8)

    do i=1,nb_WSsect/2
       if(IO(1).gt.cos(i*da))then 
          if(IO(2).gt.0)then
             isect = i
             exit !MR&VHN break when found sector
          else
             isect = nb_WSsect - i + 1
             exit !MR&VHN break when found sector
          end if
       end if
    end do

    WScd = get_WS_DISKx(diskx2bdyty(1,itact),diskx2bdyty(2,itact),isect)
    cdradius = get_radius_DISKx(itact)
    itact = this(icdan)%iantac
    IOan(1:2) = this(icdan)%coor(1:2) - DKcoor(1:2,itact)

    c=cos(DKcoor(3,itact))
    s=sin(DKcoor(3,itact))

    IO(1) = c*IOan(1) + s*IOan(2)
    IO(2) =-s*IOan(1) + c*IOan(2)
    norm = SQRT(IO(1)*IO(1)+IO(2)*IO(2))
    IO = IO/norm

    do i=1,nb_WSsect/2
       if(IO(1).gt.cos(i*da))then 
          if(IO(2).gt.0)then
             isect = i
             exit !MR&VHN break when found sector
          else
             isect = nb_WSsect - i + 1
             exit !MR&VHN break when found sector
          end if
       end if
    end do

    WSan = get_WS_DISKx(diskx2bdyty(1,itact),diskx2bdyty(2,itact),isect)
    anradius = get_radius_DISKx(itact)

    cdli = (cdradius*2.d0*PI_g)/(nb_WSsect)
    anlj = (anradius*2.d0*PI_g)/(nb_WSsect)
    if (abs(cdli*WScd+anlj*WSan).lt.1.D-16) then
       cohe = 0.D0
    else
       !mr&vhn experimental
       cohe = (WSan*WScd)/(cdli*WScd+anlj*WSan)
    end if

  end subroutine update_cohe_DKDKx

!------------------------------------------------------------------------ 
!MR&VHN 
 !> brief
  subroutine update_WS_sector_DKDKx()
    implicit none
    integer(kind=4)            :: icdan
    integer(kind=4)            :: wsstatus,itact,isect,i
    real(kind=8)               :: c,s
    real(kind=8),dimension(2)  :: IOcd,IOan,IO
    real(kind=8)               :: WScd,WSan,da

    call initialize_status_sectors   !vhn

    do icdan=1,nb_DKDKx

       select case(this(icdan)%status)
 !      case('noctc')
       case(i_Wnctc)                    !vhnhu
          !wsstatus = i_free
          wsstatus = i_stationary
       case(i_stick,i_Wstck)
          wsstatus = i_stationary
       case(i_slifw,i_Wslfw)
          wsstatus = i_sheared
       case(i_slibw,i_Wslbw)
          wsstatus = i_sheared
       case default
          wsstatus = i_free !noctc
       end select
       
       itact     = this(icdan)%icdtac
       IOcd(1:2) = this(icdan)%coor(1:2) - DKcoor(1:2,itact)
       
       c=cos(DKcoor(3,itact))
       s=sin(DKcoor(3,itact))
       
       IO(1) = c*IOcd(1) + s*IOcd(2)
       IO(2) =-s*IOcd(1) + c*IOcd(2)
       norm = SQRT(IO(1)*IO(1)+IO(2)*IO(2))
       IO = IO/norm
       
       da = PI_g*2.d0/REAL(nb_WSsect,8)
       
       do i=1,nb_WSsect/2
          if(IO(1).gt.cos(i*da))then 
             if(IO(2).gt.0)then
                isect = i
                exit !MR&VHN break when found sector
             else
                isect = nb_WSsect - i + 1
                exit !MR&VHN break when found sector
             end if
          end if
       end do
       
       call update_status_sector_DISKx(diskx2bdyty(1,itact),diskx2bdyty(2,itact),isect,wsstatus)

       itact = this(icdan)%iantac
       IOan(1:2) = this(icdan)%coor(1:2) - DKcoor(1:2,itact)
       
       c=cos(DKcoor(3,itact))
       s=sin(DKcoor(3,itact))
       
       IO(1) = c*IOan(1) + s*IOan(2)
       IO(2) =-s*IOan(1) + c*IOan(2)
       norm = SQRT(IO(1)*IO(1)+IO(2)*IO(2))
       IO = IO/norm
       
       do i=1,nb_WSsect/2
          if(IO(1).gt.cos(i*da))then 
             if(IO(2).gt.0)then
                isect = i
                exit !MR&VHN break when found sector
             else
                isect = nb_WSsect - i + 1
                exit !MR&VHN break when found sector
             end if
          end if
       end do
       
       call update_status_sector_DISKx(diskx2bdyty(1,itact),diskx2bdyty(2,itact),isect,wsstatus)
    end do
  end subroutine update_WS_sector_DKDKx
  !------------------------------------------------------------------------ 
!!!--------------------------------------------------------------------------------------------------
  subroutine mr_compute_contact_DKDKx
 
   implicit none  

   integer                   :: errare
   integer                   :: icdan,iadj,ibdy,icdbdy,ianbdy,itac
   integer                   :: icdtac,iantac,isee,itacty,iprd  
   real(kind=8),dimension(3) :: coord,coordcd,coordan,cd_Vbegin,an_Vbegin
   real(kind=8)              :: raycd,rayan,adist,dist,nonuc,gapTT
   integer                   :: i,id,j
   real(kind=8)              :: norm                                ! scalaire contenant la norme de sep
   real(kind=8),dimension(2) :: n,t,cdlev,anlev,cd_shift,an_shift
   integer                   :: cd_ent,an_ent
   
   character(len=103)        :: cout
   character(len=32)         :: IAM= 'mod_DKDKx::compute_contact_DKDKx'

   integer :: nb_DISKx
   
   icdan    = 0        
   nb_DKDKx = 0
   nb_adj   = 0
   
   if (nb_rough_DKDKx /= 0 ) then
      !
      ! preparing detection
      !
      do i = 1,nb_rough_DKDKx
         
         icdtac = rough_DKDKx(i)%cd
         iantac = rough_DKDKx(i)%an
         isee   = rough_DKDKx(i)%isee 
         iprd   = rough_DKDKx(i)%periodic

         adist   = see(isee)%alert 
         coordcd = DKcoor(1:3,icdtac)
         coordan = DKcoor(1:3,iantac)
         raycd   = get_radius_DISKx(icdtac)
         rayan   = get_radius_DISKx(iantac)
         
         coordan(1) = coordan(1) + (real(iprd,8)*periode)
         
         nonuc = sqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
         
         if (nonuc < 1.D-18) then
            write(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of disk',icdtac, &
                 'within 1.e-18 from center of disk',iantac,'in comp_local_frame_DKDKx' 
            call FATERR(IAM,cout)
         end if
         
         gapTT = nonuc-(raycd+rayan)
         ! checking distance against alert distance           
         if (gapTT .le. adist) then    

!         if (iprd == 1) print *,'actif'

            icdan          = icdan+1
            nb_adj(icdtac) = nb_adj(icdtac)+1
            iadj           = nb_adj(icdtac)
            
            if (smooth_method) then
               cd_Vbegin = get_V_DISKx(icdtac)
               an_Vbegin = get_V_DISKx(iantac)
            else
               cd_Vbegin = get_Vbegin_DISKx(icdtac)
               an_Vbegin = get_Vbegin_DISKx(iantac)
            endif
            
            this(icdan)%iadj     =  iadj
            this(icdan)%icdbdy   =  diskx2bdyty(1,icdtac)
            this(icdan)%icdtac   =  icdtac
            this(icdan)%ianbdy   =  diskx2bdyty(1,iantac)
            this(icdan)%iantac   =  iantac
            this(icdan)%isee     =  isee                 
            this(icdan)%nuc(1:2) =  (coordcd(1:2)-coordan(1:2))/nonuc
            this(icdan)%tuc(1)   =  this(icdan)%nuc(2)
            this(icdan)%tuc(2)   = -this(icdan)%nuc(1)   
            
            cd_ent = get_ent_DISKx(this(icdan)%icdtac)
            an_ent = get_ent_DISKx(this(icdan)%iantac) 
            
            entity(cd_ent)%nb = entity(cd_ent)%nb+1
            entity(an_ent)%nb = entity(an_ent)%nb+1

            this(icdan)%gapTTbegin =  gapTT
            periodic_DKDKx(icdan)  =  iprd
            
!fd < modifs pour tenir compte de l'excentrement
        
!fd        this(icdan)%Gcdt3     =  raycd
!fd        this(icdan)%Gcdn3     =  0.D0
!fd
!fd        this(icdan)%Gant3     = -rayan
!fd        this(icdan)%Gann3     =  0.D0
!fd
!fd
!fd        this(icdan)%vltBEGIN  =    (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) &
!fd                                 + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2) &
!fd                                 + cd_Vbegin(3)*this(icdan)%Gcdt3                 &
!fd                                 - an_Vbegin(3)*this(icdan)%Gant3
!fd            
!fd        this(icdan)%vlnBEGIN  =    (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) &
!fd                                 + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2)
!fd                              !o + cd_Vbegin(3)*this(icdan)%Gcdn3                 &
!fd                              !o - an_Vbegin(3)*this(icdan)%Gann3
!fd

            cd_shift = get_shiftTT_DISKx(icdtac)
            an_shift = get_shiftTT_DISKx(iantac)
            
            cdlev= (-raycd*this(icdan)%nuc) + cd_shift
            anlev= ( rayan*this(icdan)%nuc) + an_shift
            
            n(1) = this(icdan)%nuc(1)
            n(2) = this(icdan)%nuc(2)               
            t(1) = n(2) ; t(2) = -n(1)
            
            this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
            this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
            
            this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
            this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)
            
            this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                 + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                 +  cd_Vbegin(3)*this(icdan)%Gcdt3 &
                 -  an_Vbegin(3)*this(icdan)%Gant3
            
            this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                 + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                 +  cd_Vbegin(3)*this(icdan)%Gcdn3 &
                 -  an_Vbegin(3)*this(icdan)%Gann3
            
            !fd modifs pour tenir compte de l'excentrement >
            
            
            this(icdan)%rlt       = 0.D0
            this(icdan)%rln       = 0.D0
            this(icdan)%vlt       = this(icdan)%vltBEGIN
            this(icdan)%vln       = this(icdan)%vlnBEGIN
            this(icdan)%gapTT     = this(icdan)%gapTTbegin
            this(icdan)%status    = i_nknow
            
            this(icdan)%reff    = rough_DKDKx(i)%reff
            this(icdan)%meff    = rough_DKDKx(i)%meff


            call get_behaviour_( icdan, see, tact_behav )

         end if

      end do
      
      nb_DKDKx = icdan
      

   end if

   nb_DISKx=get_nb_DISKx()

   do ibdy=1,nb_DISKx
      if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
      if (nb_adj(ibdy) /= 0) then
         allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating adjac(icdbdy)%.....')
         end if
      else 
         nullify(adjac(ibdy)%icdan)
      end if
   end do
   
   do icdan=1,nb_DKDKx
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   end do
   
   if (allocated(violation)) deallocate(violation)
   allocate(violation(nb_DKDKx),stat=errare)

 end subroutine mr_compute_contact_DKDKx
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine set_vav_DKDKx(NBN)

   implicit none
   integer :: nb_DISKx,NBN

   WITH_VAV = .true.
   FIRST_TIME_VAV  = .true.

   nb_MAX_VAV = NBN*get_nb_DISKx()
   NBN_VAV    = NBN

   nb_visavis = nb_MAX_VAV

   allocate(visavis(nb_MAX_VAV))

   call LOGMES(' @ Vis-a-vis allocated in DKDKx modulus')
   print*,' @ VAV SIZE: ',nb_MAX_VAV

   VAVNULL%cd   = 0
   VAVNULL%an   = 0
   VAVNULL%isee = 0
   VAVNULL%Icoorcd = 0.D0
   VAVNULL%Icooran = 0.D0
   VAVNULL%coorcd  = 0.D0
   VAVNULL%cooran  = 0.D0


 end subroutine set_vav_DKDKx
!------------------------------------------------------------------------ 
 subroutine AM_I_OLD_VAV(cd,an,icdan,FLAG)

   implicit none
   integer :: iv,cd,an,icdan
   logical :: FLAG

   FLAG = .false.

   do iv = 1,nb_MAX_VAV

      if (visavis(iv)%FREE) cycle
      if (visavis(iv)%cd.ne.cd) cycle
      if (visavis(iv)%an.ne.an) cycle
            
      FLAG = .true.
      icdan = iv
      exit

   end do

 end subroutine AM_I_OLD_VAV
!------------------------------------------------------------------------ 
 subroutine FILL_VAV_DKDKx(VAVik,icdan)

   implicit none
   type(T_visavis) :: VAVik
   type(T_visavis),dimension(:),allocatable :: VAVtmp

   integer :: NTMP,iv,icdan

   icdan = 0

   do iv = 1,nb_MAX_VAV

      if (.not.visavis(iv)%FREE) cycle

      visavis(iv) = VAVik
      icdan = iv
      exit

   end do

   if (icdan.ne.0) return

   ! CASE visavis is FULL and a new contact should be added

   NTMP = nb_MAX_VAV + NBN_VAV


   allocate(VAVtmp(NTMP))
   
   do iv = 1,nb_MAX_VAV
      VAVtmp(iv) = visavis(iv)
   end do

   deallocate(visavis)
   allocate(visavis(NTMP))

   do iv = 1,nb_MAX_VAV
      visavis(iv) = VAVtmp(iv)
   end do

   visavis(nb_MAX_VAV+1) = VAVik
   icdan = nb_MAX_VAV+1

   do iv = nb_MAX_VAV + 2,NTMP
      visavis(iv) = VAVNULL
   end do

   nb_MAX_VAV = NTMP

   deallocate(VAVtmp)

 end subroutine FILL_VAV_DKDKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 subroutine get_g2l_DKDKx(icdan,g2l)

   implicit none
   integer      :: icdan
   real(kind=8), dimension(2,6) :: g2l

   !fd 
   ! construction de la matrice qui permet de calculer 
   ! la vitesse relative t,n (l comme locale)
   ! a partir de 
   ! la vitesse des objets candidat et antagoniste
   ! x_c,y_c,w_c,x_a,y_a,w_a (g comme globale) 

   g2l = 0.d0

   !vlt =  (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
   !     + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
   !     + cd_Vbegin(3)*this(icdan)%Gcdt3   &
   !     - an_Vbegin(3)*this(icdan)%Gant3

   g2l(1,1) = this(icdan)%tuc(1)
   g2l(1,2) = this(icdan)%tuc(2) 
   g2l(1,3) = this(icdan)%Gcdt3 
   g2l(1,4) =-this(icdan)%tuc(1)
   g2l(1,5) =-this(icdan)%tuc(2) 
   g2l(1,6) =-this(icdan)%Gant3 
   
            
   !vln =  (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
   !     + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
   !     + cd_Vbegin(3)*this(icdan)%Gcdn3   &
   !     -  an_Vbegin(3)*this(icdan)%Gann3

   g2l(2,1) = this(icdan)%nuc(1)
   g2l(2,2) = this(icdan)%nuc(2) 
   g2l(2,3) = this(icdan)%Gcdn3 
   g2l(2,4) =-this(icdan)%nuc(1)
   g2l(2,5) =-this(icdan)%nuc(2) 
   g2l(2,6) =-this(icdan)%Gann3 
   
 end subroutine get_g2l_DKDKx
!------------------------------------------------------------------------ 
 subroutine get_size_g2g_dkdkx(size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2)

   !di & fd on travaille avec un seul type de primitive des diskx

   implicit none

   ! on donne toutes les tailles
   integer :: size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2

   ! ****************
   integer,dimension(:),allocatable,target,save  :: nb_adj_rbdy2

   ! nb de rbdy2 adjacent a ce RBDY2 (tableau temporaire surdimensionne)
   integer,dimension(:,:),allocatable :: tmp_adj_rbdy2

   integer :: nb_rbdy2,icdan,icdbdy,ianbdy,ibdy

   integer :: max_sz_adj

                                     !1234567890123456789012345
   character(len=25)         :: iam= 'dkdkx::get_size_g2g_dkdkx'
   character(len=10000)        :: cout

   nb_rbdy2 = get_nb_RBDY2()  !! <--- ca c est de la grosse merde

   max_sz_adj = 20*maxval(nb_adj)  ! fred est un porc

!   write(cout,*) iam,"max_sz_adj=", max_sz_adj
!   call logmes(cout)

   allocate(tmp_adj_rbdy2(max_sz_adj,nb_RBDY2),nb_adj_rbdy2(nb_rbdy2))
   tmp_adj_rbdy2=0

!   write(cout,*) IAM," nb_RBDY2=",nb_rbdy2
!   call logmes(cout)
!   write(cout,*) IAM," size_nb_adj_rbdy2=",size(nb_adj_rbdy2)
!   call logmes(cout)

   ! D'office chaque corps est adjacent a lui-meme
   nb_adj_rbdy2 = 1

!   write(cout,*) iam," 1-sum(nb_adj_rbdy2)=", sum(nb_adj_rbdy2)
!   call logmes(cout)
!   write(cout,*) iam," 1-nb_DKDKx=", nb_DKDKx
!   call logmes(cout)

   do ibdy=1,nb_rbdy2
      tmp_adj_rbdy2(1,ibdy)=ibdy 
   end do

   ! Pour chaque contact
   do icdan=1,nb_DKDKx

      ! Recuperation des indices du candidat et de l'antagoniste
      icdbdy = this(icdan)%icdbdy
      ianbdy = this(icdan)%ianbdy

      if (icdbdy == ianbdy) call faterr(IAM, "ON NE TRAITE PAS &
                            & LES AUTO CONTACTS")

      !--------------------------------------------------------
      ! Remplissage de la liste des corps adjacents au cd et an
      ! PS: on peut avoir plus d'un contact etre deux corps

      if (count(tmp_adj_rbdy2(:,icdbdy) == ianbdy) == 0) then
         nb_adj_rbdy2(icdbdy) = nb_adj_rbdy2(icdbdy)  + 1
         if (nb_adj_rbdy2(icdbdy) >  max_sz_adj) then
            call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
         end if
         tmp_adj_rbdy2(nb_adj_rbdy2(icdbdy),icdbdy) = ianbdy
      end if

      if (count(tmp_adj_rbdy2(:,ianbdy) == icdbdy) == 0) then
         nb_adj_rbdy2(ianbdy) = nb_adj_rbdy2(ianbdy)  + 1
         if (nb_adj_rbdy2(ianbdy) >  max_sz_adj) then
           call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
         end if
         tmp_adj_rbdy2(nb_adj_rbdy2(ianbdy),ianbdy) = icdbdy
      end if
      !--------------------------------------------------------
   end do

   max_sz_adj = sum(nb_adj_rbdy2)

!   write(cout,*) iam," 2-sum(nb_adj_rbdy2)=", max_sz_adj
!   call logmes(cout)
!   write(cout,*) iam," 2-nb_adj_rbdy2=", nb_adj_rbdy2
!   call logmes(cout)

   size_g2g = 9*max_sz_adj
   size_idx_rbdy2 = nb_rbdy2
   size_adj_rbdy2 = max_sz_adj
   size_nb_adj_rbdy2 = nb_rbdy2

   deallocate(tmp_adj_rbdy2,nb_adj_rbdy2)

 end subroutine get_size_g2g_dkdkx
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  !> Routine de construction de la matrice g2g = M + (1/eta)*HH^T
 subroutine get_g2g_DKDKx(    g2g, IRN, JRN, &
                         size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2 )

   !di & fd on travaille avec un seul type de primitive des diskx

   implicit none

   ! on donne toutes les tailles
   integer, intent(in) :: size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2

   ! ****************
   !stockage de la matrice
   real(kind=8),dimension(size_g2g),     intent(out) :: g2g
   !indices des termes non nuls suivant I
   integer(kind=4),dimension(size_g2g),  intent(out) :: IRN
   !indices des termes non nuls suivant J
   integer(kind=4),dimension(size_g2g),  intent(out) :: JRN
   ! index dans le tableau de stockage
   integer,dimension(size_idx_rbdy2)    :: idx_rbdy2
   ! liste des corps adjacents
   integer,dimension(size_adj_rbdy2)    :: adj_rbdy2 
   ! nb de corps adjacents 
   integer,dimension(size_nb_adj_rbdy2) :: nb_adj_rbdy2

   ! nb de rbdy2 adjacent a ce diskx (tableau temporaire surdimensionne)
   integer,dimension(:,:),allocatable :: tmp_adj_rbdy2

   ! par soucis de simplicite on genere une liste de contact adjacent
   ! au corps 
   type(G_i_list),dimension(:),allocatable :: my_adjac

   integer :: nb_rbdy2,icdan,icdbdy,ianbdy,ibdy,itac,iadj,ii,jj,idx_cd,idx_an,idx_g
   integer :: i,idx,tmp_1(1),i_ligne,i_colonne
   integer :: max_sz_adj

   logical                     :: is_vlocy_drvdofi, is_vlocy_drvdofj
   real(kind=8),dimension(3)   :: mass_ibdy
   real(kind=8),dimension(2,6) :: g2l
   real(kind=8),dimension(2,3) :: tmp_2x3,Hi_T,Hj_T
   real(kind=8),dimension(3,2) :: Hi
   real(kind=8),dimension(3,3) :: tmp_3x3

                                     !12345678901234567890
   character(len=20)         :: IAM= 'DKDKx::get_g2g_DKDKx'
   character(len=10000)     :: cout

   nb_rbdy2 = get_nb_RBDY2()  !! <--- ca c est de la grosse merde

   max_sz_adj = 20*maxval(nb_adj)  ! fred est un porc


!   write(cout,*) IAM," max_sz_adj=",max_sz_adj
!   call logmes(cout)
!
!   write(cout,*) IAM," nb_RBDY2=",nb_rbdy2
!   call logmes(cout)
!   write(cout,*) IAM," size_nb_adj_rbdy2=",size_nb_adj_rbdy2
!   call logmes(cout)


   allocate(tmp_adj_rbdy2(max_sz_adj,nb_RBDY2), &
            my_adjac(nb_rbdy2))

   tmp_adj_rbdy2 = 0

   ! Depart a 1 voir ci dessous tmp_adj_rbdy2(1,ibdy)=ibdy
   nb_adj_rbdy2 = 1

!   write(cout,*) iam," 1-sum(nb_adj_rbdy2)=", sum(nb_adj_rbdy2)
!   call logmes(cout)
!   write(cout,*) iam," 1-nb_DKDKx=", nb_DKDKx
!   call logmes(cout)

   do ibdy=1,nb_rbdy2
      allocate(my_adjac(ibdy)%G_i(max_sz_adj))
      my_adjac(ibdy)%G_i=0
      ! D'office chaque corps est adjacent a lui-meme
      tmp_adj_rbdy2(1,ibdy)=ibdy 
   end do


   ! Pour chaque contact
   do icdan=1,nb_DKDKx

      ! Recuperation des indices du RBDY2 candidat et de l'antagoniste
      icdbdy = this(icdan)%icdbdy
      ianbdy = this(icdan)%ianbdy

      if (icdbdy == ianbdy) call faterr(IAM, "ON NE TRAITE PAS &
                            & LES AUTO CONTACTS")

      !--------------------------------------------------------
      ! Remplissage de la liste des contacts supportes par le cd et an

      ! Si my_adjac(icdbdy)%G_i est entierement remplit,
      ! c'est qu'il y a bugg
      if (minval(my_adjac(icdbdy)%G_i) > 0) then
         call FATERR(IAM,'max_sz_adj is reached for my_adjac%g_i')
      end if

      ! On recupere l'indice du premier zero -> idx
      tmp_1 = minloc(my_adjac(icdbdy)%G_i)
      idx = tmp_1(1)
      ! et on affecte à cet indice le numero du contact
      my_adjac(icdbdy)%G_i(idx) = icdan

      ! Si my_adjac(icdbdy)%G_i est entierement remplit,
      ! c'est qu'il y a bugg
      if (minval(my_adjac(ianbdy)%G_i) > 0) then
         call FATERR(IAM,'max_sz_adj is reached for my_adjac%g_i')
      end if
      ! On recupere l'indice du premier zero -> idx
      tmp_1 = minloc(my_adjac(ianbdy)%G_i)
      idx = tmp_1(1)
      ! et on affecte à cet indice le numero du contact
      my_adjac(ianbdy)%G_i(idx) = icdan
      !--------------------------------------------------------

      !--------------------------------------------------------
      ! Remplissage de la liste des corps adjacents au cd et an
      ! PS: on peut avoir plus d'un contact etre deux corps

      if (count(tmp_adj_rbdy2(:,icdbdy) == ianbdy) == 0) then
         nb_adj_rbdy2(icdbdy) = nb_adj_rbdy2(icdbdy)  + 1
         if (nb_adj_rbdy2(icdbdy) >  max_sz_adj) then
            call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
         end if
         tmp_adj_rbdy2(nb_adj_rbdy2(icdbdy),icdbdy) = ianbdy
      end if

      if (count(tmp_adj_rbdy2(:,ianbdy) == icdbdy) == 0) then
         nb_adj_rbdy2(ianbdy) = nb_adj_rbdy2(ianbdy)  + 1
         if (nb_adj_rbdy2(ianbdy) >  max_sz_adj) then
           call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
         end if
         tmp_adj_rbdy2(nb_adj_rbdy2(ianbdy),ianbdy) = icdbdy
      end if
      !--------------------------------------------------------

   end do

   ! Nombre total de corps adjacents
   max_sz_adj = sum(nb_adj_rbdy2)
   
!   write(cout,*) iam," 2-sum(nb_adj_rbdy2)=", max_sz_adj
!   call logmes(cout)
!   write(cout,*) iam," 2-nb_adj_rbdy2_abcd=", nb_adj_rbdy2
!   call logmes(cout)

   if (max_sz_adj /= size_adj_rbdy2) then
      call FATERR(IAM,'Error unmatching size')
   endif

   idx_rbdy2 = 0
   g2g=0.d0

   idx=0

   ! Pour chaque corps (RBDY2) on stocke la liste des corps (RBDY2) adjacents
   do ibdy=1,nb_rbdy2
      if (nb_adj_rbdy2(ibdy) /= 0) then
         ! Debut de la tranche, relative a ibdy, a idx (zero pour ibdy==1!!)
         idx_rbdy2(ibdy) = idx
         ! Longueur de la tranche = nb_adj_rbdy2(ibdy)
         ! Corps adjacents : tmp_adj_rbdy2(1:nb_adj_rbdy2(ibdy),ibdy)
         adj_rbdy2(idx+1:idx+nb_adj_rbdy2(ibdy)) = tmp_adj_rbdy2(1:nb_adj_rbdy2(ibdy),ibdy)
         idx = idx + nb_adj_rbdy2(ibdy)
      end if
   end do

!   write(cout,*) IAM,' idx_rbdy2=',idx_rbdy2
!   call logmes(cout)
!   write(cout,*) IAM,' nb_adj_rbdy2=',nb_adj_rbdy2
!   call logmes(cout)
!   !write(cout,*) IAM,' adj_rbdy2=',adj_rbdy2
!   !call logmes(cout)
   
   deallocate(tmp_adj_rbdy2)

   ! Pour tous les corps
   do ibdy=1,nb_rbdy2
      ! On va construire les blocs de HH^T pour lignes correspondantes
      ! au ibdy (lignes 3*(ibdy-1)+1 a 3*ibdy).
      !                 |                                |
      !                 :                                :
      ! 3*(ibdy-1)+1 -> |                                |
      ! 3*(ibdy-1)+2 -> |                                |
      ! 3*(ibdy-1)+3 -> |                                |
      !                 :                                :
      !                 |                                |

      ! On boucle sur les contacts suportes par ibdy
      do iadj=1,size(my_adjac(ibdy)%G_i)  
 
         ! Numero du contact
         icdan=my_adjac(ibdy)%G_i(iadj)

         ! Si l'on arrive a 0, c'est qu'il n'y a plus 
         ! de contact a traiter pour le corps ibdy
         if (icdan == 0) exit

         ! Recuperation des numeros des corps (RBDY2)
         ! associes au contact icdan
         icdbdy= this(icdan)%icdbdy
         ianbdy= this(icdan)%ianbdy

         ! Chaque contact contribut deux fois aux lignes associees a ibdy
         ! 1) bloc 3x3 sur la diagonale de g2g
         !                 
         !                  1 ... 3*(ibdy-1)+1 3*(ibdy-1)+2 3*(ibdy-1)+3 ... 3*nb_RBDY2
         !                             |            |            |
         !                             v            v            v
         !                 |                                                           |
         !                 :                                                           :
         ! 3*(ibdy-1)+1 -> |      [                                    ]               |
         ! 3*(ibdy-1)+2 -> |      [               HiHi^T               ]               |
         ! 3*(ibdy-1)+3 -> |      [                                    ]               |
         !                 :                                                           :
         !                 |                                                           |
         ! 2) bloc 3x3 pour les colonnes associees au corps adjacent à ibdy :
         !    colonnes 3*(ibdy_adj-1)+1 a 3*(ibdy_adj-1)+3
         !                 
         !                  1 ... 3*(ibdy_adj-1)+1 3*(ibdy_adj-1)+2 3*(ibdy_adj-1)+3 ... 3*nb_RBDY2
         !                                 |                |                 |
         !                                 v                v                 v
         !                 |                                                                       |
         !                 :                                                                       :
         ! 3*(ibdy-1)+1 -> |      [                                                ]               |
         ! 3*(ibdy-1)+2 -> |      [                    += HiHj^T                   ]               |
         ! 3*(ibdy-1)+3 -> |      [                                                ]               |
         !                 :                                                                       :
         !                 |                                                                       |

         ! Recuperation de la matrice HT
         ! associee au contact icdan 
         call get_g2l_DKDKx(icdan,g2l) 

         ! Si ibdy est le candidat au contact
         if (ibdy == icdbdy) then
            i_ligne   = icdbdy
            i_colonne = ianbdy
            ii = 0
            jj = 1          

         ! Si ibdy est l'antagoniste
         else if (ibdy == ianbdy) then
            i_ligne   = ianbdy
            i_colonne = icdbdy
            ii = 1
            jj = 0
         else

            call FATERR(IAM,'Unable to find ibdy in my_adjac(ibdy)%G_i(iadj)')
         endif

         is_vlocy_drvdofi = .false.
         call is_vlocy_drvdof_RBDY2(i_ligne, is_vlocy_drvdofi)
         is_vlocy_drvdofj = .false.
         call is_vlocy_drvdof_RBDY2(i_colonne, is_vlocy_drvdofj)

         ! Recuperation du bloc 2x3 associe a i_ligne
         tmp_2x3 = g2l(1:2,(ii*3)+1:(ii*3)+3)

         ! Pour ne recuperer que la partie normale
         !tmp_2x3 = 0.d0
         !tmp_2x3(2,:) = g2l(2,(ii*3)+1:(ii*3)+3)

         Hi = transpose(tmp_2x3)
         Hi_T = tmp_2x3

         ! Recuperation du bloc 2x3 associe a i_colonne
         Hj_T = g2l(1:2,(jj*3)+1:(jj*3)+3)  
  

         ! Pour ne recuperer que la partie normale
         !Hj_T = 0.d0
         !Hj_T(2,:) = g2l(2,(jj*3)+1:(jj*3)+3)
  
         idx_g = idx_rbdy2(i_ligne)
  
         ! terme ii
         tmp_3x3 = matmul(Hi,Hi_T)
         !tmp_3x3(1,1) = tmp_3x3(1,1) 
         !tmp_3x3(2,2) = tmp_3x3(2,2) 
         !tmp_3x3(3,3) = tmp_3x3(3,3) 

         idx_cd = -1
         do i=1,nb_adj_rbdy2(i_ligne)
            if (adj_rbdy2(idx_g+i) == i_ligne) then
               idx_cd = i-1
               exit
            end if
         end do
         if (idx_cd == -1) then
            call FATERR(IAM,'Unable to find i_ligne in adj_rbdy2 list')
         end if

         ! Avec cette technique, on ne gere que les corps non pilotes en vitesse 
         ! OU pilotes entierement (touss de dofs) en vitesse.
         if ((is_vlocy_drvdofi .eqv. .false.) .and. (is_vlocy_drvdofj .eqv. .false.)) then
            g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9) + &
                                                          pack(tmp_3x3,mask=.true.)       
         else 
            g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)= (/1.d0, 0.d0, 0.d0, &
                                                          0.d0, 1.d0, 0.d0, &
                                                          0.d0, 0.d0, 1.d0 /)
         end if 

         IRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne /)

         JRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+1, 3*(i_ligne-1)+1, &
                                                       3*(i_ligne-1)+2, 3*(i_ligne-1)+2, 3*(i_ligne-1)+2, &
                                                       3* i_ligne,       3*i_ligne,       3*i_ligne /)
  
         ! terme ij
         tmp_3x3 = matmul(Hi,Hj_T)        
  
         idx_an = -1
         do i=1,nb_adj_rbdy2(i_ligne)
            if (adj_rbdy2(idx_g+i) == i_colonne) then
               idx_an = i-1 
               exit
            end if
         end do
         if (idx_an == -1) then
            call FATERR(IAM,'Unable to find i_colonne in adj_rbdy2 list')
         endif
  
         ! Avec cette technique, on ne gere que les corps non pilotes en vitesse 
         ! OU pilotes entierement (touss de dofs) en vitesse.
         if ((is_vlocy_drvdofi .eqv. .false.) .and. (is_vlocy_drvdofj .eqv. .false.)) then
            g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9) + &
                                                        pack(tmp_3x3,mask=.true.)              
         else
            ! g2g etant initialisee a 0.d0, pas la peine de faire cette affectation ici.
            !g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)= 0.d0       
         end if 

         IRN(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne /)

         JRN(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=(/ 3*(i_colonne-1)+1, 3*(i_colonne-1)+1, 3*(i_colonne-1)+1, &
                                                       3*(i_colonne-1)+2, 3*(i_colonne-1)+2, 3*(i_colonne-1)+2, &
                                                       3* i_colonne,      3* i_colonne,      3* i_colonne /) 
      end do
   end do

   g2g = d1_eta * g2g
   ! On vient de construire (en sparse) la matrice (1/eta) * HHT

   ! Il reste à y ajouter la matrice de masse pour obtenir g2g = M + (1/eta)*HHT
   do ibdy = 1, nb_rbdy2
      idx_g = idx_rbdy2(ibdy)
      idx_cd = -1
      do i=1,nb_adj_rbdy2(ibdy)
         if (adj_rbdy2(idx_g+i) == ibdy) then
            idx_cd = i-1 
            exit
         end if
      end do
      if (idx_cd == -1) then
         call FATERR(IAM,'Unable to find ibdy in adj_rbdy2 list')
      endif

      mass_ibdy(1:3) = get_ptr_mass(ibdy)
      g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9) + &
                                                     (/ mass_ibdy(1),      0.d0,        0.d0, &
                                                           0.d0,      mass_ibdy(2),     0.d0, &
                                                           0.d0,           0.d0,    mass_ibdy(3) /)

      IRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy, &
                                                    3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy, &
                                                    3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy /)

      JRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(ibdy-1)+1, 3*(ibdy-1)+1, 3*(ibdy-1)+1, &
                                                    3*(ibdy-1)+2, 3*(ibdy-1)+2, 3*(ibdy-1)+2, &
                                                    3* ibdy,       3*ibdy,       3*ibdy /)
   end do
   do ibdy=1,nb_rbdy2
      deallocate(my_adjac(ibdy)%G_i)
   end do
   deallocate(my_adjac)

 end subroutine get_g2g_DKDKx
!------------------------------------------------------------------------ 
 subroutine get_size_sym_g2g_dkdkx(size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2)

   !di & fd on travaille avec un seul type de primitive des diskx

   implicit none

   ! on donne toutes les tailles
   integer :: size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2

   ! ****************
   integer,dimension(:),allocatable,target,save  :: nb_adj_rbdy2

   ! nb de rbdy2 adjacent a ce RBDY2 (tableau temporaire surdimensionne)
   integer,dimension(:,:),allocatable :: tmp_adj_rbdy2

   integer :: nb_rbdy2,icdan,icdbdy,ianbdy,ibdy

   integer :: max_sz_adj

                                     !12345678901234567890123456789
   character(len=29)         :: iam= 'dkdkx::get_size_sym_g2g_dkdkx'
   character(len=10000)        :: cout

   nb_rbdy2 = get_nb_RBDY2()  !! <--- ca c est de la grosse merde

   max_sz_adj = 20*maxval(nb_adj)  ! fred est un porc

   allocate(tmp_adj_rbdy2(max_sz_adj,nb_RBDY2),nb_adj_rbdy2(nb_rbdy2))
   tmp_adj_rbdy2=0

   ! D'office chaque corps est adjacent a lui-meme
   nb_adj_rbdy2 = 1

   do ibdy=1,nb_rbdy2
      tmp_adj_rbdy2(1,ibdy)=ibdy 
   end do

   ! Pour chaque contact
   do icdan=1,nb_DKDKx

      ! si le contact est inactif, on ne le considere pas
      !print *, "this(",icdan,")%status =",this(icdan)%status
      if (this(icdan)%status == i_noctc) cycle

      ! Recuperation des indices du candidat et de l'antagoniste
      icdbdy = this(icdan)%icdbdy
      ianbdy = this(icdan)%ianbdy

      if (icdbdy == ianbdy) call faterr(IAM, "ON NE TRAITE PAS &
                            & LES AUTO CONTACTS")

      !--------------------------------------------------------
      ! Remplissage de la liste des corps adjacents au cd et an
      ! PS: on peut avoir plus d'un contact etre deux corps

      if (ianbdy > icdbdy) then
         if (count(tmp_adj_rbdy2(:,icdbdy) == ianbdy) == 0) then
            nb_adj_rbdy2(icdbdy) = nb_adj_rbdy2(icdbdy)  + 1
            if (nb_adj_rbdy2(icdbdy) >  max_sz_adj) then
               call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
            end if
            tmp_adj_rbdy2(nb_adj_rbdy2(icdbdy),icdbdy) = ianbdy
         end if
      end if

      if (icdbdy > ianbdy) then
         if (count(tmp_adj_rbdy2(:,ianbdy) == icdbdy) == 0) then
            nb_adj_rbdy2(ianbdy) = nb_adj_rbdy2(ianbdy)  + 1
            if (nb_adj_rbdy2(ianbdy) >  max_sz_adj) then
              call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
            end if
            tmp_adj_rbdy2(nb_adj_rbdy2(ianbdy),ianbdy) = icdbdy
         end if
      end if
      !--------------------------------------------------------
   end do

   max_sz_adj = sum(nb_adj_rbdy2)

   size_g2g = 9*max_sz_adj
   size_idx_rbdy2 = nb_rbdy2
   size_adj_rbdy2 = max_sz_adj
   size_nb_adj_rbdy2 = nb_rbdy2

   deallocate(tmp_adj_rbdy2,nb_adj_rbdy2)

 end subroutine get_size_sym_g2g_dkdkx
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  !> Routine de construction de la matrice g2g = M + (1/eta)*HH^T
 subroutine get_sym_g2g_DKDKx(    g2g, IRN, JRN, &
                         size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2 )

   !di & fd on travaille avec un seul type de primitive des diskx

   implicit none

   ! on donne toutes les tailles
   integer, intent(in) :: size_g2g,size_idx_rbdy2,size_adj_rbdy2,size_nb_adj_rbdy2

   ! ****************
   !stockage de la matrice
   real(kind=8),dimension(size_g2g),     intent(out) :: g2g
   !indices des termes non nuls suivant I
   integer(kind=4),dimension(size_g2g),  intent(out) :: IRN
   !indices des termes non nuls suivant J
   integer(kind=4),dimension(size_g2g),  intent(out) :: JRN
   ! index dans le tableau de stockage
   integer,dimension(size_idx_rbdy2)    :: idx_rbdy2
   ! liste des corps adjacents
   integer,dimension(size_adj_rbdy2)    :: adj_rbdy2 
   ! nb de corps adjacents 
   integer,dimension(size_nb_adj_rbdy2) :: nb_adj_rbdy2

   ! nb de rbdy2 adjacent a ce diskx (tableau temporaire surdimensionne)
   integer,dimension(:,:),allocatable :: tmp_adj_rbdy2

   ! par soucis de simplicite on genere une liste de contact adjacent
   ! au corps 
   type(G_i_list),dimension(:),allocatable :: my_adjac

   integer :: nb_rbdy2,icdan,icdbdy,ianbdy,ibdy,itac,iadj,ii,jj,idx_cd,idx_an,idx_g
   integer :: i,idx,tmp_1(1),i_ligne,i_colonne
   integer :: max_sz_adj

   logical                     :: is_vlocy_drvdofi, is_vlocy_drvdofj
   real(kind=8),dimension(3)   :: mass_ibdy
   real(kind=8),dimension(2,6) :: g2l
   real(kind=8),dimension(2,3) :: tmp_2x3,Hi_T,Hj_T
   real(kind=8),dimension(3,2) :: Hi
   real(kind=8),dimension(3,3) :: tmp_3x3

                                     !123456789012345678901234
   character(len=24)         :: IAM= 'DKDKx::get_sym_g2g_DKDKx'
   character(len=10000)     :: cout

   nb_rbdy2 = get_nb_RBDY2()  !! <--- ca c est de la grosse merde

   max_sz_adj = 20*maxval(nb_adj)  ! fred est un porc


   allocate(tmp_adj_rbdy2(max_sz_adj,nb_RBDY2), &
            my_adjac(nb_rbdy2))

   tmp_adj_rbdy2 = 0

   ! Depart a 1 voir ci dessous tmp_adj_rbdy2(1,ibdy)=ibdy
   nb_adj_rbdy2 = 1

   do ibdy=1,nb_rbdy2
      allocate(my_adjac(ibdy)%G_i(max_sz_adj))
      my_adjac(ibdy)%G_i=0
      ! D'office chaque corps est adjacent a lui-meme
      tmp_adj_rbdy2(1,ibdy)=ibdy 
   end do


   ! Pour chaque contact
   do icdan=1,nb_DKDKx

      ! si le contact est inactif, on ne le considere pas
      if (this(icdan)%status == i_noctc) cycle

      ! Recuperation des indices du RBDY2 candidat et de l'antagoniste
      icdbdy = this(icdan)%icdbdy
      ianbdy = this(icdan)%ianbdy

      if (icdbdy == ianbdy) call faterr(IAM, "ON NE TRAITE PAS &
                            & LES AUTO CONTACTS")

      !--------------------------------------------------------
      ! Remplissage de la liste des contacts supportes par le cd et an
      ! Si my_adjac(icdbdy)%G_i est entierement remplit,
      ! c'est qu'il y a bugg
      if (minval(my_adjac(icdbdy)%G_i) > 0) then
         call FATERR(IAM,'max_sz_adj is reached for my_adjac%g_i')
      end if

      ! On recupere l'indice du premier zero -> idx
      tmp_1 = minloc(my_adjac(icdbdy)%G_i)
      idx = tmp_1(1)
      ! et on affecte à cet indice le numero du contact
      my_adjac(icdbdy)%G_i(idx) = icdan

      ! Si my_adjac(icdbdy)%G_i est entierement remplit,
      ! c'est qu'il y a bugg
      if (minval(my_adjac(ianbdy)%G_i) > 0) then
         call FATERR(IAM,'max_sz_adj is reached for my_adjac%g_i')
      end if

      ! On recupere l'indice du premier zero -> idx
      tmp_1 = minloc(my_adjac(ianbdy)%G_i)
      idx = tmp_1(1)
      ! et on affecte à cet indice le numero du contact
      my_adjac(ianbdy)%G_i(idx) = icdan
      !--------------------------------------------------------

      !--------------------------------------------------------
      ! Remplissage de la liste des corps adjacents au cd et an
      ! PS: on peut avoir plus d'un contact etre deux corps
      if (ianbdy > icdbdy) then
         if (count(tmp_adj_rbdy2(:,icdbdy) == ianbdy) == 0) then
            nb_adj_rbdy2(icdbdy) = nb_adj_rbdy2(icdbdy)  + 1
            if (nb_adj_rbdy2(icdbdy) >  max_sz_adj) then
               call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
            end if
            tmp_adj_rbdy2(nb_adj_rbdy2(icdbdy),icdbdy) = ianbdy
         end if
      end if

      if (icdbdy > ianbdy) then
         if (count(tmp_adj_rbdy2(:,ianbdy) == icdbdy) == 0) then
            nb_adj_rbdy2(ianbdy) = nb_adj_rbdy2(ianbdy)  + 1
            if (nb_adj_rbdy2(ianbdy) >  max_sz_adj) then
              call FATERR(IAM,'max_sz_adj is reached for nb_adj_rbdy2')
            end if
            tmp_adj_rbdy2(nb_adj_rbdy2(ianbdy),ianbdy) = icdbdy
         end if
      end if
      !--------------------------------------------------------

   end do

   ! Nombre total de corps adjacents
   max_sz_adj = sum(nb_adj_rbdy2)
   
   if (max_sz_adj /= size_adj_rbdy2) then
      call FATERR(IAM,'Error unmatching size')
   endif

   idx_rbdy2 = 0
   g2g=0.d0

   idx=0

   ! Pour chaque corps (RBDY2) on stocke la liste des corps (RBDY2) adjacents
   do ibdy=1,nb_rbdy2
      if (nb_adj_rbdy2(ibdy) /= 0) then
         ! Debut de la tranche, relative a ibdy, a idx (zero pour ibdy==1!!)
         idx_rbdy2(ibdy) = idx
         ! Longueur de la tranche = nb_adj_rbdy2(ibdy)
         ! Corps adjacents : tmp_adj_rbdy2(1:nb_adj_rbdy2(ibdy),ibdy)
         adj_rbdy2(idx+1:idx+nb_adj_rbdy2(ibdy)) = tmp_adj_rbdy2(1:nb_adj_rbdy2(ibdy),ibdy)
         idx = idx + nb_adj_rbdy2(ibdy)
      end if
   end do

   deallocate(tmp_adj_rbdy2)

   ! Pour tous les corps
   do ibdy=1,nb_rbdy2
      ! On va construire les blocs de HH^T pour lignes correspondantes
      ! au ibdy (lignes 3*(ibdy-1)+1 a 3*ibdy).
      !                 |                                |
      !                 :                                :
      ! 3*(ibdy-1)+1 -> |                                |
      ! 3*(ibdy-1)+2 -> |                                |
      ! 3*(ibdy-1)+3 -> |                                |
      !                 :                                :
      !                 |                                |

      ! On boucle sur les contacts suportes par ibdy
      do iadj=1,size(my_adjac(ibdy)%G_i)  
 
         ! Numero du contact
         icdan=my_adjac(ibdy)%G_i(iadj)

         ! Si l'on arrive a 0, c'est qu'il n'y a plus 
         ! de contact a traiter pour le corps ibdy
         if (icdan == 0) exit

         ! Recuperation des numeros des corps (RBDY2)
         ! associes au contact icdan
         icdbdy= this(icdan)%icdbdy
         ianbdy= this(icdan)%ianbdy

         ! Chaque contact contribut deux fois aux lignes associees a ibdy
         ! 1) bloc 3x3 sur la diagonale de g2g
         !                 
         !                  1 ... 3*(ibdy-1)+1 3*(ibdy-1)+2 3*(ibdy-1)+3 ... 3*nb_RBDY2
         !                             |            |            |
         !                             v            v            v
         !                 |                                                           |
         !                 :                                                           :
         ! 3*(ibdy-1)+1 -> |      [                                    ]               |
         ! 3*(ibdy-1)+2 -> |      [               HiHi^T               ]               |
         ! 3*(ibdy-1)+3 -> |      [                                    ]               |
         !                 :                                                           :
         !                 |                                                           |
         ! 2) bloc 3x3 pour les colonnes associees au corps adjacent à ibdy :
         !    colonnes 3*(ibdy_adj-1)+1 a 3*(ibdy_adj-1)+3
         !                 
         !                  1 ... 3*(ibdy_adj-1)+1 3*(ibdy_adj-1)+2 3*(ibdy_adj-1)+3 ... 3*nb_RBDY2
         !                                 |                |                 |
         !                                 v                v                 v
         !                 |                                                                       |
         !                 :                                                                       :
         ! 3*(ibdy-1)+1 -> |      [                                                ]               |
         ! 3*(ibdy-1)+2 -> |      [                    += HiHj^T                   ]               |
         ! 3*(ibdy-1)+3 -> |      [                                                ]               |
         !                 :                                                                       :
         !                 |                                                                       |

         ! Recuperation de la matrice HT
         ! associee au contact icdan 
         call get_g2l_DKDKx(icdan,g2l) 

         ! Si ibdy est le candidat au contact
         if (ibdy == icdbdy) then
            i_ligne   = icdbdy
            i_colonne = ianbdy
            ii = 0
            jj = 1          

         ! Si ibdy est l'antagoniste
         else if (ibdy == ianbdy) then
            i_ligne   = ianbdy
            i_colonne = icdbdy
            ii = 1
            jj = 0
         else

            call FATERR(IAM,'Unable to find ibdy in my_adjac(ibdy)%G_i(iadj)')
         endif

         is_vlocy_drvdofi = .false.
         call is_vlocy_drvdof_RBDY2(i_ligne, is_vlocy_drvdofi)
         is_vlocy_drvdofj = .false.
         call is_vlocy_drvdof_RBDY2(i_colonne, is_vlocy_drvdofj)

         !! Recuperation du bloc 2x3 associe a i_ligne
         !tmp_2x3 = g2l(1:2,(ii*3)+1:(ii*3)+3)

         ! Pour ne recuperer que la partie normale
         tmp_2x3 = 0.d0
         tmp_2x3(2,:) = g2l(2,(ii*3)+1:(ii*3)+3)

         Hi = transpose(tmp_2x3)
         Hi_T = tmp_2x3

         !! Recuperation du bloc 2x3 associe a i_colonne
         !Hj_T = g2l(1:2,(jj*3)+1:(jj*3)+3)  

         ! Pour ne recuperer que la partie normale
         Hj_T = 0.d0
         Hj_T(2,:) = g2l(2,(jj*3)+1:(jj*3)+3)
  
         idx_g = idx_rbdy2(i_ligne)
  
         ! terme ii
         tmp_3x3 = matmul(Hi,Hi_T)

         ! application MUMPS symetrique
         tmp_3x3(2,1) = 0.d0
         tmp_3x3(3,1) = 0.d0
         tmp_3x3(3,2) = 0.d0

         idx_cd = -1
         do i=1,nb_adj_rbdy2(i_ligne)
            if (adj_rbdy2(idx_g+i) == i_ligne) then
               idx_cd = i-1
               exit
            end if
         end do
         if (idx_cd == -1) then
            call FATERR(IAM,'Unable to find i_ligne in adj_rbdy2 list')
         end if

         ! Avec cette technique, on ne gere que les corps non pilotes en vitesse 
         ! OU pilotes entierement (touss de dofs) en vitesse.
         if ((is_vlocy_drvdofi .eqv. .false.) .and. (is_vlocy_drvdofj .eqv. .false.)) then
           g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9) + &
                                                          pack(tmp_3x3,mask=.true.)       
         else 
           g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)= (/1.d0, 0.d0, 0.d0, &
                                                         0.d0, 1.d0, 0.d0, &
                                                         0.d0, 0.d0, 1.d0 /)
         end if 

         IRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3* i_ligne /)

         JRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+1, 3*(i_ligne-1)+1, &
                                                       3*(i_ligne-1)+2, 3*(i_ligne-1)+2, 3*(i_ligne-1)+2, &
                                                       3* i_ligne,       3*i_ligne,       3*i_ligne /)
 
         if (i_colonne < i_ligne) cycle
 
         ! terme ij
         tmp_3x3 = matmul(Hi,Hj_T)
  
         idx_an = -1
         do i=1,nb_adj_rbdy2(i_ligne)
            if (adj_rbdy2(idx_g+i) == i_colonne) then
               idx_an = i-1 
               exit
            end if
         end do
         if (idx_an == -1) then
            call FATERR(IAM,'Unable to find i_colonne in adj_rbdy2 list')
         endif
  
         ! Avec cette technique, on ne gere que les corps non pilotes en vitesse 
         ! OU pilotes entierement (touss de dofs) en vitesse.
         if ((is_vlocy_drvdofi .eqv. .false.) .and. (is_vlocy_drvdofj .eqv. .false.)) then
            g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9) + &
                                                        pack(tmp_3x3,mask=.true.)              
         else
            ! g2g etant initialisee a 0.d0, il n'est peut-etre pas utile de faire cette affectation.
            g2g(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)= 0.d0       
         end if 

         IRN(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=(/ 3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne, &
                                                       3*(i_ligne-1)+1, 3*(i_ligne-1)+2, 3*i_ligne /)

         JRN(9*(idx_g+idx_an)+1:9*(idx_g+idx_an)+9)=(/ 3*(i_colonne-1)+1, 3*(i_colonne-1)+1, 3*(i_colonne-1)+1, &
                                                       3*(i_colonne-1)+2, 3*(i_colonne-1)+2, 3*(i_colonne-1)+2, &
                                                       3* i_colonne,      3* i_colonne,      3* i_colonne /) 
      end do
   end do

   g2g = d1_eta * g2g
   ! On vient de construire (en sparse) la matrice (1/eta) * HHT

   ! Il reste à y ajouter la matrice de masse pour obtenir g2g = M + (1/eta)*HHT
   do ibdy = 1, nb_rbdy2
      idx_g = idx_rbdy2(ibdy)
      idx_cd = -1
      do i=1,nb_adj_rbdy2(ibdy)
         if (adj_rbdy2(idx_g+i) == ibdy) then
            idx_cd = i-1 
            exit
         end if
      end do
      if (idx_an == -1) then
         call FATERR(IAM,'Unable to find ibdy in adj_rbdy2 list')
      endif

      mass_ibdy(1:3) = get_ptr_mass(ibdy)
      g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=g2g(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9) + &
                                                     (/ mass_ibdy(1),      0.d0,        0.d0, &
                                                           0.d0,      mass_ibdy(2),     0.d0, &
                                                           0.d0,           0.d0,    mass_ibdy(3) /)

      IRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy, &
                                                    3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy, &
                                                    3*(ibdy-1)+1, 3*(ibdy-1)+2, 3* ibdy /)

      JRN(9*(idx_g+idx_cd)+1:9*(idx_g+idx_cd)+9)=(/ 3*(ibdy-1)+1, 3*(ibdy-1)+1, 3*(ibdy-1)+1, &
                                                    3*(ibdy-1)+2, 3*(ibdy-1)+2, 3*(ibdy-1)+2, &
                                                    3* ibdy,       3*ibdy,       3*ibdy /)
   end do

   do ibdy=1,nb_rbdy2
      deallocate(my_adjac(ibdy)%G_i)
   end do
   deallocate(my_adjac)

 end subroutine get_sym_g2g_DKDKx
!------------------------------------------------------------------------ 
!vv: for get_size_(sym_)g2g, get_(sym_)g2g 
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  !> contact detection for  
  !> x_min,x_max,y_min,y_max sub box for detection 
  !> diskx2sdm : for a given diskx gives the sdm number
  !> sdm1,sdm2 : the two sdm we are interested in
  !> diskx2inter : 0: not concerned, 1: concerned

  subroutine interface_creation_tab_visu_DKDKx(x_min,x_max,y_min,y_max,diskx2sdm,sdm1,sdm2,diskx2inter)

    implicit none 
   
    integer                     :: errare 
    
    integer                     :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
    integer                     :: icdan,iadj,ibdy,icdbdy,ianbdy,itac,icdtac,iantac,isee,itacty   
    real(kind=8)                :: Bleft,Bright,Bup,Bdown
    character(len=5)            :: cdtac,cdcol,antac,ancol
    real(kind=8),dimension(3)   :: coord,coordcd,coordan 
    real(kind=8)                :: raycd,rayan,adist,dist,nonuc,gapT
    real(kind=8)                :: masscd,massan
    integer                     :: minibox1=0, maxibox1=0,minibox2=0,maxibox2=0    
    character(len=103)          :: cout
    character(len=28)           :: IAM = 'mod_DKDKx::creation_tab_visu'
   
    integer :: nb_DISKx

    real(kind=8) :: x_min,x_max,y_min,y_max
    integer :: diskx2sdm(:),sdm1,sdm2,diskx2inter(:)     

    nb_DISKx = get_nb_DISKx()

    diskx2inter = 0

! Since the list of proximate contactors may not be updated at every time step,
! boxes data would be lost if deallocated. When starting the program, boxes are not created.
! A warning condition prevents undue deallocation. 

    if (allocated(box)) then
       do ibox1=minibox1,maxibox1
          do ibox2=minibox2,maxibox2
             if ( associated(box(ibox1,ibox2)%which) ) deallocate( box(ibox1,ibox2)%which )
          end do
       end do
       deallocate(box)
    end if
    
    ! Building boxes for quick sorting 
    
    ! Computing maximal boundary radius of disks and largest box containing disks.
    !
    ! The computation of maximal radius of disks allows to size a grid of boxes,
    ! the purpose of which is quick sorting of candidates to contact. The following
    ! sorting method is reasonnably fast. It is not really efficacious when the 
    ! ratio between maximal and minimal radius is too large (>10), since the box 
    ! size becomes too large. Ratio less than 3 or even 5 are fair.
    ! The minimum radius of disks is also used, in order to estimate the maximal 
    ! number of disks per box.   
    ! This quick sorting method may be applied to bodies other than disks, such as 
    ! ellipsoidal or polygonal bodies with a reasonable aspect ratio, less than 
    ! 3 or even 5. Such bodies are enclosed in disks with radius max_radius, and 
    ! enclosing a disk with radius min_radius. The sorting algorithm may then be 
    ! straightforwardly applied.
    ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
    ! Since the data file allows, for further generalization purposes, other 
    ! contactors than BDARY (the true boundary), extracting min_radius and 
    ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.
    
    Bleft    =  x_min
    Bright   =  x_max
    Bup      =  y_max
    Bdown    =  y_min
    
    minibox1 = 1
    maxibox1 = 1 + int((Bright-Bleft)*Lbox_1)
    minibox2 = 1
    maxibox2 = 1 + int((Bup - Bdown )*Lbox_1)
    !   
    allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)
    
    if (errare /=0 ) then
       write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       call LOGMES(cout)
       write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
       call LOGMES(cout)
       call FATERR(IAM,'error allocating box')
    end if
    
    do ibox1=minibox1,maxibox1
       do ibox2=minibox2,maxibox2
          box(ibox1,ibox2)%popul=0
          allocate(box(ibox1,ibox2)%which(maxpopul),stat=errare)
          if (errare /=0 ) then
             call FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%which')
          end if
          box(ibox1,ibox2)%which=0
       end do
    end do
    
   ! filling boxes with disks
   ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%which(ipopul) is the rank of body DISKx labelled ipopul in the box
  
   ! filling boxes   

   do ibdy=1,nb_DISKx
      coord=DKcoor(1:3,ibdy)
      if (.not.get_visible_DISKx(ibdy)) cycle
      ibox1=1+int((coord(1)-Bleft )*Lbox_1)
      ibox2=1+int((coord(2)-Bdown )*Lbox_1)
      if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
         write(cout,'(A13,I10,A13)') '  body DISKx ',ibdy,' out of boxes'
         call LOGMES(cout)
         cycle
      end if
      box(ibox1,ibox2)%popul = box(ibox1,ibox2)%popul+1
      box(ibox1,ibox2)%which(box(ibox1,ibox2)%popul) = ibdy
   end do

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
   
   nb_rough_DKDKx = 0
   
   ! creation de la liste de paire a examiner
  
   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue une zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   nullify(Root)
   nullify(Current)
   nullify(Previous)
  

!fd A VOIR le 03/01/08 on pourrait diminuer le nombre de tests en gerant 
!fd le test if iantac <= icdtac cycle.
!fd c'est ahurissant !!

   do ibox1cd = minibox1,maxibox1  
      do ibox2cd = minibox2,maxibox2
         do icdpop = 1,box(ibox1cd,ibox2cd)%popul
            icdtac = box(ibox1cd,ibox2cd)%which(icdpop)
            cdcol = get_color_DISKx(icdtac)
            ! box loop investigating antagonist diskx
            do ibox1an = max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                        
               do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
                  do ianpop = 1,box(ibox1an,ibox2an)%popul
                     iantac = box(ibox1an,ibox2an)%which(ianpop)
                     if (iantac .le. icdtac .or. is_DISKx_same_BDYTY(icdtac,iantac)) cycle

                     ! on cherche les cas un objets de chaque cote

                     if (diskx2sdm(icdtac) == sdm1) then 
                       if (diskx2sdm(iantac) /= sdm2) cycle
                     else if (diskx2sdm(icdtac) == sdm2) then
                       if (diskx2sdm(iantac) /= sdm1) cycle                       
                     else
                       cycle
                     endif

                     ancol = get_color_DISKx(iantac)
                     if( diskx2bdyty(3,iantac) == diskx2bdyty(3,icdtac) ) then
                        isee  = get_isee_specific('DISKx',cdcol,ancol)
                     else
                        isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                        get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                     end if
                     
                     if ( isee /= 0 ) then
                        adist = see(isee)%alert 
                        ! checking ROUGHLY distance against alert distance           
                        coordcd = DKcoor(1:3,icdtac)
                        coordan = DKcoor(1:3,iantac)
                        raycd   = get_radius_DISKx(icdtac)
                        rayan   = get_radius_DISKx(iantac)

                        ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                        ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                        ! results might be different up to some non significant figures, but when comparing to
                        ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

                        adist = 0.1005D+01*adist+raycd+rayan

                        if (       dabs(coordcd(1)-coordan(1)) <= adist &
                             .and. dabs(coordcd(2)-coordan(2)) <= adist) then

                           nb_rough_DKDKx=nb_rough_DKDKx+1

                           if ( nb_rough_DKDKx == 1) then
                              allocate(Root)
                              Current => Root
                              nullify(Root%p)
                           else
                              allocate(Current)
                              Previous%n => Current
                           endif
                           Current%val%cd       = icdtac
                           Current%val%an       = iantac
                           Current%val%isee     = isee
                           Current%val%periodic = 0

                           Current%p => Previous
                           nullify(Current%n)
                           Previous => Current
                        end if
                     end if
                  end do
               end do
            end do
         end do
      end do
   end do
   
   if (allocated(rough_DKDKx)) deallocate(rough_DKDKx)
   allocate(rough_DKDKx(nb_rough_DKDKx))     ! the visibility array used in compute_contact is allocated
   
   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_DKDKx))            ! the oversized array this is temporaly allocated
   
   do icdan=nb_rough_DKDKx,1,-1
      
      Previous => Current%p
      rough_DKDKx(icdan)%cd       = Current%val%cd
      rough_DKDKx(icdan)%an       = Current%val%an
      rough_DKDKx(icdan)%isee     = Current%val%isee
      rough_DKDKx(icdan)%periodic = Current%val%periodic
      
      diskx2inter(Current%val%cd) = 1
      diskx2inter(Current%val%an) = 1

!!! > md > !!!
!!! a modifier pour des corps non-convexes
     
      raycd = get_radius_DISKx(Current%val%cd)
      rayan = get_radius_DISKx(Current%val%an)
      
      rough_DKDKx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))
      massan=get_mass_DISKx(diskx2bdyty(1,Current%val%an))
      
      rough_DKDKx(icdan)%meff = masscd*massan/(masscd+massan)
      
      deallocate(Current)
      Current => Previous
   end do
   
   nullify(Root)

103      format(1X,A10,1X,I5,1X,A10,1X,I5)

 end subroutine 
!--------------------------------------------------------------------------------------------------

 ! rm : allocating violation array to test itrHdl
 subroutine reset_violation_DKDKx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(violation) ) deallocate(violation)
   allocate( violation(nb) )
 end subroutine

 subroutine reset_nb_adj_DKDKx()
   implicit none

   if (.not. allocated(nb_adj)) allocate(nb_adj(get_nb_DISKx()))

   nb_adj = 0

 end subroutine

 !> add an adjacent
 subroutine add_adj_DKDKx(icdbdy, icdtac)
   implicit none
   !> body number of candidat to add adjacent to
   integer(kind=4), intent(in) :: icdbdy
   !> contactor number of candidat to add adjacent to
   integer(kind=4), intent(in) :: icdtac
   !
   integer(kind=4) :: i_tact

   do i_tact =1, get_nb_DISKx()
     if (diskx2bdyty(1,i_tact) == icdbdy .and. &
         diskx2bdyty(2,i_tact) == icdtac       ) then
       nb_adj(i_tact) = nb_adj(i_tact) + 1
       exit
     end if
   end do

 end subroutine

 ! rm : getter on a rough dkdkx for testing itrHdl
 subroutine get_rough_DKDKx(icdan, icdtac, iantac, isee, periodic)
   implicit none
   integer(kind=4), intent(in) :: icdan
   integer(kind=4), intent(out) :: icdtac, iantac, isee, periodic

   icdtac   = rough_DKDKx(icdan)%cd
   iantac   = rough_DKDKx(icdan)%an
   isee     = rough_DKDKx(icdan)%isee
   periodic = rough_DKDKx(icdan)%periodic

 end subroutine

 function get_icdtac_DKDKx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_DKDKx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_DISKx
   logical :: found

   found = .false.

   nb_DISKx = get_nb_DISKx()

   icc = 0
   do icdtac = 1, nb_DISKx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_DKDKx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKDKx::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_DKDKx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_DKDKx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_DISKx
   logical :: found

   found = .false.

   nb_DISKx = get_nb_DISKx()

   icc = 0
   do icdtac = 1, nb_DISKx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_DKDKx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKDKx::get_icdtac','unknown contact index')
   

   get_iantac_DKDKx = this(icdan)%iantac

 end function

 ! rm : functions for siconos wrapper

 function get_old_index_DKDKx(icdan)
   implicit none
   integer :: icdan
   integer :: get_old_index_DKDKx
   !
   integer :: icdtac,iantac,iadj

   get_old_index_DKDKx = 0

   if (.not. allocated(verlt)) then
      return
   endif
   
   ! serial number of candidate contactor for contact icdan
   icdtac = this(icdan)%icdtac
   ! serial number of antagonist contactor for contact icdan
   iantac = this(icdan)%iantac

   if (verlt(icdtac)%adjsz /= 0) then
      if ( verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac)       &
         ) then
         do iadj=1,verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == diskx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == diskx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== diskx2bdyty(3,iantac)       &
                ) then
              get_old_index_DKDKx = verlt(icdtac)%icdan(iadj)
              exit
           end if
         end do
      end if
   end if

 end function get_old_index_DKDKx

 !> \brief Fine detection between diskx contactors
 !> \todo use contactors instead of all data in arguments
 function detect_and_compute_contact_DKDKx(coordcd, coordan, raycd, rayan, cd_shift, an_shift, &
                                           cd_Vbegin, an_Vbegin, cd_ent, an_ent, isee, iprd, periode)! result(interaction)
   implicit none  
   !> [in] coordinates of candidate contactor
   real(kind=8), dimension(2), intent(in) :: coordcd
   !> [in] coordinates of antagonist contactor
   real(kind=8), dimension(2), intent(in) :: coordan
   !> [in] radius of candidate contactor
   real(kind=8)              , intent(in) :: raycd
   !> [in] radius of antagonist contactor
   real(kind=8)              , intent(in) :: rayan
   !> [in] velocity of candidate contactor's model_handle
   real(kind=8), dimension(3), intent(in) :: cd_Vbegin
   !> [in] velocity of antagonist contactor's model_handle
   real(kind=8), dimension(3), intent(in) :: an_Vbegin
   !> [in] model_handle id of candidate contactor's model_handle
   integer(kind=4)           , intent(in) :: cd_ent
   !> [in] model_handle id of antagonist contactor's model_handle
   integer(kind=4)           , intent(in) :: an_ent
   !> [in] shift of candidate contactor
   real(kind=8), dimension(2), intent(in) :: cd_shift
   !> [in] shift of antagonist contactor
   real(kind=8), dimension(2), intent(in) :: an_shift
   !> [in] index of see type
   integer(kind=4)           , intent(in) :: isee
   !> [in] periodic
   integer(kind=4)           , intent(in) :: iprd
   !> [in] periode... used only if iprd is 1
   real(kind=8)              , intent(in) :: periode
   !> [return] a pointer on the detected/computed interaction (may be null)
   !type(T_interaction), pointer :: interaction
   real(kind=8) :: detect_and_compute_contact_DKDKx
   !
   real(kind=8), dimension(2) :: cdlev, anlev
   integer(kind=4)    :: itest, ibehav
   real(kind=8)       :: adist, nonuc, gapTT
   character(len=103) :: cout
   character(len=42)  :: IAM
   !     123456789012345678901234567890123456789012
   IAM ='DKDKx::detect_and_compute_contact_DKDKx'

 !!  interaction => null()

 !!  adist = see(isee)%alert 
 !!  
 !!  ! better use periodic has an optional ?
 !!  if( iprd /= 0 ) then
 !!    !coordan(1) = coordan(1) + (real(iprd,8)*periode)
 !!  end if
 !!  
 !!  nonuc = sqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
 !!  
 !!  if( nonuc < 1.D-18 ) then
 !!    write(cout, *) 'two diskx centers are within 1.e-18 from each other in'
 !!    call faterr(IAM,cout)
 !!  end if
 !!     
 !!  gapTT = nonuc-(raycd+rayan)
 !!     
 !!  ! checking distance against alert distance           
 !!  if( gapTT <= adist ) then !there is contact

 !!    interaction => new_interaction()
 !!    interaction%gapTT = gapTT

 !!    !icdan          = icdan+1
 !!    !group       = rough_DKDKx(i)%group
 !!    !iprd        = rough_DKDKx(i)%periodic
 !!    interaction%cdan   = i_dkdkx
 !!    interaction%icdtyp = i_diskx
 !!    interaction%iantyp = i_diskx

 !!    !IF (WITH_VAV.AND..NOT.FIRST_TIME_VAV) THEN
 !!    !   icdbdy    = visavis(i)%cd
 !!    !   ianbdy    = visavis(i)%an 
 !!    !   isee      = visavis(i)%isee
 !!    !END IF
 !!    
 !!    !interaction%group  =  group 

 !!    entity(cd_ent)%nb = entity(cd_ent)%nb+1
 !!    entity(an_ent)%nb = entity(an_ent)%nb+1
 !!    
 !!    interaction%icdent = cd_ent
 !!    interaction%ianent = an_ent


 !!    ! uc = (t, n)
 !!    interaction%uc(1:nbDIME,2) = (coordcd(1:nbDIME)-coordan(1:nbDIME))/nonuc
 !!    interaction%uc(1,1)        = interaction%uc(2,2)
 !!    interaction%uc(2,1)        =-interaction%uc(1,2)
 !!           
 !!    cdlev = -raycd*interaction%uc(1:2,2) + cd_shift(:)
 !!    anlev =  rayan*interaction%uc(1:2,2) + an_shift(:)
 !!    
 !!    interaction%Gcd(1:nbDIME,1) = -cdlev(2)*interaction%uc(1,1:nbDIME)+cdlev(1)*interaction%uc(2,1:nbDIME)
 !!    interaction%Gan(1:nbDIME,1) = -anlev(2)*interaction%uc(1,1:nbDIME)+anlev(1)*interaction%uc(2,1:nbDIME)
 !!    
 !!    interaction%vlBEGIN(1:nbDIME) = matmul( transpose(interaction%uc), cd_Vbegin(1:nbDIME)-an_Vbegin(1:nbDIME) )
 !!    interaction%vlBEGIN(1)        = interaction%vlBEGIN(1) + cd_Vbegin(3)*interaction%Gcd(1,1) - an_Vbegin(3)*interaction%Gan(1,1)
 !!    interaction%vlBEGIN(2)        = interaction%vlBEGIN(2) + cd_Vbegin(3)*interaction%Gcd(2,1) - an_Vbegin(3)*interaction%Gan(2,1)

 !!    interaction%rl(1:nbDIME) = 0.D0
 !!    interaction%vl(1:nbDIME) = interaction%vlBEGIN(1:nbDIME)
 !!    
 !!    !this(icdan)%reff    = rough_DKDKx(i)%reff
 !!    !this(icdan)%meff    = rough_DKDKx(i)%meff
 !!    
 !!    interaction%coor(1:nbDIME) = coordcd(1:nbDIME) - (raycd +0.5D0*interaction%gapTT)*interaction%uc(1:nbDIME,2)
 !!    interaction%gapTTbegin     = interaction%gapTT
 !!    
 !!    itest = 0
 !!    do ibehav = 1, size(tact_behav)
 !!      if( see(isee)%behav == tact_behav(ibehav)%behav ) then
 !!        interaction%lawnb       = ibehav
 !!        interaction%nb_internal = get_nb_internal(ibehav)
 !!        interaction%internal    = init_internal(ibehav)
 !!        itest = 1
 !!        exit
 !!      end if
 !!    end do
 !!    if( itest == 0 ) then
 !!       write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A)') 'nickname',see(isee)%behav,'ctact rank',interaction%rank,&
 !!                                               'unknown in lawty'
 !!       call logmes('check TACT-BEHAV.DAT in DATBOX')
 !!       call faterr(IAM,cout)
 !!    end if

 !!  end if
 
 end function detect_and_compute_contact_DKDKx

!! !> \brief Create interaction of DKDK in a generic t2t
!! subroutine compute_contacts_in_t2t_DKDKx(t2t, icdan)
!!   implicit none  
!!   !> t2t in which to compute interactions
!!   type(T_tact2tact) :: t2t
!!   !> current index of dkdkx
!!   integer(kind=4), intent(in) :: icdan
!!   !
!!   integer(kind=4)    :: i, itest, ibehav, errare, i4_input(6), i4_output(5)
!!   real(kind=8)       :: gapTT, raycd, rayan, r8_vec_out(2,6)
!!   logical            :: to_keep
!!   character(len=40)  :: IAM
!!   character(len=103) :: cout
!!   !     1234567890123456789012345678901234567890
!!   IAM= 'mod_DKDKx::compute_contacts_in_t2t_DKDKx'
!!
!!   i4_input(1) = t2t%icdtac
!!   i4_input(2) = t2t%iantac
!!   i4_input(3) = t2t%isee
!!   i4_input(4) = t2t%xperiodic
!!
!!   call compute_one_contact_DKDKx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)
!!
!!   if( to_keep ) then
!!
!!     call set_nb_face2faces(t2t, 1)
!!
!!     t2t%f2f(1)%nb_ctc = 1
!!     call initialize_interactions(t2t%f2f(1), 1)
!!
!!     t2t%f2f(1)%ctc(1)%inter%cdan   = i_dkdkx
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdent = get_ent_DISKx(i4_output(2))
!!     t2t%f2f(1)%ctc(1)%inter%ianent = get_ent_DISKx(i4_output(4))
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdtyp = i_diskx
!!     t2t%f2f(1)%ctc(1)%inter%iantyp = i_diskx
!!
!!     t2t%f2f(1)%ctc(1)%inter%iadj   = i4_output(1)
!!     t2t%f2f(1)%ctc(1)%inter%icdbdy = i4_output(2)
!!     t2t%f2f(1)%ctc(1)%inter%icdtac = i4_output(3)
!!     t2t%f2f(1)%ctc(1)%inter%ianbdy = i4_output(4)
!!     t2t%f2f(1)%ctc(1)%inter%iantac = i4_output(5)
!!
!!     t2t%f2f(1)%ctc(1)%inter%isee  = i4_input(3)
!!     !t2t%ctc(1)%inter%group = t2t%group
!!
!!     t2t%f2f(1)%ctc(1)%inter%uc(1:nbDIME,1:nbDIME) = r8_vec_out(:,1:2)
!!
!!     t2t%f2f(1)%ctc(1)%inter%gapTTbegin =  gapTT
!!     periodic_DKDKx(icdan+1) =  t2t%xperiodic
!!
!!     t2t%f2f(1)%ctc(1)%inter%Gcd(1:nbDIME,1) = r8_vec_out(:,3)
!!     t2t%f2f(1)%ctc(1)%inter%Gan(1:nbDIME,1) = r8_vec_out(:,4)
!!
!!     t2t%f2f(1)%ctc(1)%inter%vlBEGIN(1:nbDIME) = r8_vec_out(:,5)
!!
!!     t2t%f2f(1)%ctc(1)%inter%rl     = 0.d0
!!     t2t%f2f(1)%ctc(1)%inter%vl     = t2t%f2f(1)%ctc(1)%inter%vlBEGIN
!!     t2t%f2f(1)%ctc(1)%inter%gapTT  = t2t%f2f(1)%ctc(1)%inter%gapTTbegin
!!     t2t%f2f(1)%ctc(1)%inter%status = 'nknow'
!!    
!!     !t2t%ctc(1)%inter%reff = t2t%reff
!!     !t2t%ctc(1)%inter%meff = t2t%meff
!!      
!!     t2t%f2f(1)%ctc(1)%inter%coor(1:2) = r8_vec_out(:,6)
!!
!!     itest = 0
!!     do ibehav = 1, size(tact_behav)
!!       if( see(t2t%isee)%behav == tact_behav(ibehav)%behav ) then
!!         t2t%f2f(1)%ctc(1)%inter%lawnb       = ibehav
!!         t2t%f2f(1)%ctc(1)%inter%i_law       = tact_behav(ibehav)%ilaw
!!         t2t%f2f(1)%ctc(1)%inter%nb_internal = get_nb_internal(ibehav)
!!         t2t%f2f(1)%ctc(1)%inter%internal    = init_internal(ibehav)
!!         itest = 1
!!         exit
!!       end if
!!     end do
!!     if( itest == 0 ) then
!!        write(cout,'(A,1x,I0,1x,A)') 'nickname',see(t2t%isee)%behav,'unknown in lawty'
!!        call logmes('check TACT-BEHAV.DAT in DATBOX')
!!        call faterr(IAM,cout)
!!     end if
!!
!!     ! needed by some interaction laws
!!     raycd = get_radius_DISKx(t2t%icdtac)
!!     rayan = get_radius_DISKx(t2t%iantac)
!!     t2t%f2f(1)%ctc(1)%inter%area = (raycd*rayan) / (rayan+raycd)
!!
!!   end if
!!         
!!   !write(cout,'(1X,I10,A12)') nb_DKDKx,' DKDKx found'
!!   !call logmes(cout)
!!
!! end subroutine compute_contacts_in_t2t_DKDKx
 
 !>brief
 subroutine read_ini_contact_snapshot_sample
   implicit none
   integer                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   integer                          :: cdmodel, anmodel, nb_DISKx
   integer                          :: errare 
   integer                          :: ibehav,nb_internal,i_internal
   real(kind=8)                     :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)        :: nuc,coor
   character(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus
   real(kind=8),dimension(max_internal_tact) :: internal
   character(len=103) :: cout
   character(len=29)  :: IAM
   real(kind=8),dimension(2) :: tuc    
   real(kind=8),dimension(3) :: cdreac,anreac    
   integer     ,dimension(3) :: cdccdof,anccdof

   IAM = 'mod_DKDKx::read_ini_contact_snapshot_sample'
   nb_DISKx = get_nb_DISKx()

   errare = 0.d0
   
   if (.not. allocated(nb_adj)) allocate(nb_adj(nb_DISKx),stat=errare)
   if (errare.ne.0) then
      call FATERR(IAM,' error allocating nb_adj')
   end if

   nb_adj=0

   do    
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6).ne.'DKDKx') cycle
      read(G_clin(1:180),'(6X,3(1X,I7),10(1X,D14.7))') icdan,icdbdy,ianbdy,rln,rlt,vln,vlt,internal(1:6)
      do icdtact=1,nb_DISKx
         if (diskx2bdyty(1,icdtact) .eq. icdbdy) then
            nb_adj(icdtact)=nb_adj(icdtact)+1       
            exit
         end if
      end do
      cycle
   end do

   if (.not. allocated(verlt)) then
      allocate(verlt(nb_DISKx),stat=errare)
      if (errare /=0 ) then
         call FATERR(IAM,' error allocating verlt')
      end if

      do icdtac=1,nb_DISKx
         verlt(icdtac)%adjsz=0
         iadj=nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating verlt(icdtac)%.....')
         end if
      end do
   else 
      do icdtac=1,nb_DISKx
         call free_verlet_(icdtac)
         verlt(icdtac)%adjsz=0
         iadj=nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
      end do
   end if

   ! second reading: filling data
   rewind(G_nfich)

   nb_adj=0
   
   do 
      if ( .not. read_G_clin()) exit
      if (G_clin(2:6).ne.'DKDKx') cycle
      read(G_clin(1:180),'(6X,3(1X,I7),10(1X,D14.7))') icdan,icdbdy,ianbdy,rln,rlt,vln,vlt,internal(1:6)
      cdmodel = get_body_model_id_from_name( cdbdy )
      anmodel = get_body_model_id_from_name( anbdy )
      do icdtact=1,nb_DISKx
         if ( diskx2bdyty(1,icdtact) == icdbdy) then
            nb_adj(icdtact)=nb_adj(icdtact)+1 
            verlt(icdtact)%cdbdy                   = icdbdy
            verlt(icdtact)%cdtac                   = icdtac
            verlt(icdtact)%cdmodel                 = cdmodel
            verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
            verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
            verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
            verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
            if( .not. read_G_clin()) exit
            read(G_clin(1:90),'(27X,2(7X,D14.7))') rlt,rln
            verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
            verlt(icdtact)%rln(nb_adj(icdtact)) = rln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') vlt,vln
            verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
            verlt(icdtact)%vln(nb_adj(icdtact)) = vln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
            verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'n(1)=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
               verlt(icdtact)%nuc(1,nb_adj(icdtact)) = nuc(1)
               verlt(icdtact)%nuc(2,nb_adj(icdtact)) = nuc(2)
            else 
               backspace(G_nfich)
            end if
            if( .not. read_G_clin()) exit
            if (G_clin(30:34)== 'coo1=') then
               read(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
               verlt(icdtact)%coor(1,nb_adj(icdtact)) = coor(1)
               verlt(icdtact)%coor(2,nb_adj(icdtact)) = coor(2)
            else 
               backspace(G_nfich)
            end if
            
            verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact)) = 0.d0
            ibehav      = get_ibehav(behav)
            nb_internal = get_nb_internal(ibehav)
            
            if (nb_internal /= 0 ) then  
               if( .not. read_G_clin()) exit
               do i_internal=1, nb_internal
                  read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
                       verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
               end do
            end if
            exit
         end if
      end do
      cycle
   end do

   nb_vDKDKx=0
   
   do icdtact=1,nb_DISKx
      nb_vDKDKx = nb_vDKDKx + nb_adj(icdtact)
      
      if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
         write(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
              'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         call FATERR(IAM,cout)
      end if
   end do

!fd 24/10/08 
! one try to rebuild the reac field

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         iantac = verlt(icdtact)%antac(iadj)

         call nullify_reac_DISKx(icdtact,iIreac)
         call nullify_reac_DISKx(iantac,iIreac)
      enddo
   enddo

   do icdtact=1,nb_DISKx
      do iadj=1, verlt(icdtact)%adjsz
         icdbdy = verlt(icdtact)%cdbdy
         ianbdy = verlt(icdtact)%anbdy(iadj)
         iantac = verlt(icdtact)%antac(iadj)
         RlN = verlt(icdtact)%rln(iadj)*H
         RlT = verlt(icdtact)%rlt(iadj)*H
         nuc(1:2) = verlt(icdtact)%nuc(1:2,iadj)
         tuc(1) = nuc(2)
         tuc(2) =-nuc(1)

         cdccdof(1) = 1
         anccdof(1) = 1
         cdreac(1)  = RlT*tuc(1)+RlN*nuc(1)
         anreac(1)  =-cdreac(1)
         cdccdof(2) = 2
         anccdof(2) = 2
         cdreac(2)  = RlT*tuc(2)+RlN*nuc(2)      
         anreac(2)  =-cdreac(2)
         cdccdof(3) = 3
         anccdof(3) = 3
         cdreac(3) = 0.d0
         anreac(3) = 0.d0

         call add_reac_DISKx(icdtact,cdccdof,cdreac,iIreac)
         call add_reac_DISKx(iantac,anccdof,anreac,iIreac)
      enddo
   enddo
 end subroutine read_ini_contact_snapshot_sample

 subroutine compute_betai_DKDKx
   implicit none
   integer :: icdtac,iantac,iadj
   integer(kind=4) :: nb_DISKx,nb_adj
   real(kind=8),dimension(max_internal_tact) :: internal

   nb_DISKx = get_nb_DISKx()

   if(first_time_betai)then
      first_time_betai =.false.
      if(allocated(betaiDISKx)) deallocate(betaiDISKx)
      allocate(betaiDISKx(nb_DISKx,2))
      
      betaiDISKx = 0.d0

      if(nb_DKDKx.eq.0)then
         first_time_betai =.true.
         deallocate(betaiDISKx)
         return
      end if

      do icdtac = 1,nb_DISKx
         
         nb_adj = verlt(icdtac)%adjsz
         do iadj = 1,nb_adj
            internal = 0.D0
            internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
            iantac = get_tact_id( verlt(icdtac)%anbdy(iadj), &
                                  verlt(icdtac)%antac(iadj), &
                                  verlt(icdtac)%anmodel(iadj)&
                                )
            
            betaiDISKx(icdtac,1) = betaiDISKx(icdtac,1) + internal(4)
            betaiDISKx(icdtac,2) = betaiDISKx(icdtac,2) + 1.d0

            betaiDISKx(iantac,1) = betaiDISKx(iantac,1) + internal(4)
            betaiDISKx(iantac,2) = betaiDISKx(iantac,2) + 1.d0
         end do
      end do

      do icdtac = 1,nb_DISKx
         betaiDISKx(icdtac,2) = max(1.d0,betaiDISKx(icdtac,2))
         betaiDISKx(icdtac,1) = betaiDISKx(icdtac,1)/betaiDISKx(icdtac,2)
      end do

   else

      betaiDISKx(:,1) = 0.d0

      do icdtac = 1,nb_DISKx
         
         nb_adj = verlt(icdtac)%adjsz
         do iadj = 1,nb_adj
            internal = 0.D0
            internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
            iantac = get_tact_id( verlt(icdtac)%anbdy(iadj), &
                                  verlt(icdtac)%antac(iadj), &
                                  verlt(icdtac)%anmodel(iadj)&
                                )
            
            betaiDISKx(icdtac,1) = betaiDISKx(icdtac,1) + internal(4)
            betaiDISKx(iantac,1) = betaiDISKx(iantac,1) + internal(4)
         end do
      end do

      do icdtac = 1,nb_DISKx
         betaiDISKx(icdtac,1) = betaiDISKx(icdtac,1)/betaiDISKx(icdtac,2)
      end do
   end if

   do icdtac = 1,nb_DISKx
      call add_betai_to_DISKx(diskx2bdyty(1,icdtac),diskx2bdyty(2,icdtac),betaiDISKx(icdtac,1))
   end do

 end subroutine compute_betai_DKDKx

 subroutine clean_memory_DKDKx
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_DKDKx  = 0
   nb_vDKDKx = 0

   nb_visavis = 0
   nb_max_vav = 0
   nbn_vav    = 0
   first_time_vav = .true.
   if( allocated(visavis) ) deallocate(visavis)

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%which) ) deallocate(box(i,j)%which)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_DKDKx) ) deallocate(rough_DKDKx)

   nb_rough_DKDKx = 0
   nstep_rough_seek_DKDKx = 1
   nb_recup_DKDKx = 0

   RUN = .false.

   if( allocated(DKcoor) ) deallocate(DKcoor)

   Reac_DKDKx_MAX = 0.D0

   first_time_betai = .true.
   if( allocated(betaiDISKx) ) deallocate(betaiDISKx)

   !PERIODIC = .false.
   !PERIODE  = 0.d0
   nb_PERIODIC_DKDKx = 0
   if( allocated(periodic_DKDKx) ) deallocate(periodic_DKDKx)

   !maxray, minray, maxalert, meanradius
   !Lbox,LBox_1,norm
   !maxpopul
   nb_big_diskx = 0
   if( allocated(big_diskx_list) ) deallocate(big_diskx_list)
   if( allocated(iamABigDISKx  ) ) deallocate(iamABigDISKx)

   !skip_creation_tab_visu = .false.
   module_checked_ = .FALSE.
   check_DKDKx_    = .FALSE.

 end subroutine
!----------------------------------------------------
 subroutine compute_czm_energy_DKDKx
   implicit none
   integer(kind=4) :: icdan,ibehav
   real(kind=8)    :: stored_nuc,damage_nuc,stored_tuc,damage_tuc
   real(kind=8)    :: gapTT
   real(kind=8)    :: cn,ct,smax,w,d,p,b,cohn,coht,dw,dg
   
   real(kind=8),dimension(max_internal_tact) :: internal
   
   if (allocated(energyDKDKx)) deallocate(energyDKDKx)
   allocate(energyDKDKx(nb_DKDKx))
   
   do icdan = 1,nb_DKDKx

      energyDKDKx(icdan)%failure  = 0.d0
      energyDKDKx(icdan)%stored   = 0.d0
      energyDKDKx(icdan)%damage   = 0.d0
      energyDKDKx(icdan)%cohesion = 0.d0

      internal = this(icdan)%internal
      ibehav   = this(icdan)%lawnb
      
      select case(tact_behav(ibehav)%ilaw)
      case(i_IQS_MAC_CZM,i_MAC_CZM)
         call get_czm(ibehav,cn,ct,smax,w,b)
         if(internal(4).eq.0.0) then    
            energyDKDKx(icdan)%failure = w*internal(1) 
         else
            if(internal(3).ne.0.D0) then
               stored_nuc = 0.5*internal(4)*(cn*internal(3)*internal(3))*internal(1)
               damage_nuc = (w*internal(1))-stored_nuc
            end if
            if(internal(2).ne.0.D0) then 
               stored_tuc = 0.5*internal(4)*(ct*internal(2)*internal(2))*internal(1) 
               damage_tuc = (w*internal(1))-stored_tuc
            end if
            energyDKDKx(icdan)%stored = stored_nuc + stored_tuc
            energyDKDKx(icdan)%damage = damage_nuc + damage_tuc
         end if
      case(i_IQS_WET_CZM)
         call get_czm(ibehav,cn,ct,smax,w,b)
         call get_coh(ibehav,cohn,coht,dw)
         gapTT = this(icdan)%gapTT
         if(internal(4).eq.0.0) then    
            energyDKDKx(icdan)%failure = w*internal(1) 
            if((gapTT.ge.0.D0).and.(gapTT.le.dw)) energyDKDKx(icdan)%cohesion = cohn*gapTT !+ coht*dg 
         else
            if(internal(3).ne.0.D0) then
               stored_nuc = 0.5*internal(4)*(cn*internal(3)*internal(3))*internal(1)
               damage_nuc = (w*internal(1))-stored_nuc
            end if
            if(internal(2).ne.0.D0) then 
               stored_tuc = 0.5*internal(4)*(ct*internal(2)*internal(2))*internal(1) 
               damage_tuc = (w*internal(1))-stored_tuc
            end if
            energyDKDKx(icdan)%stored = stored_nuc + stored_tuc
            energyDKDKx(icdan)%damage = damage_nuc + damage_tuc
         end if
      case default
         ! nothing to do ... yet
      end select
   end do
   
 end subroutine compute_czm_energy_DKDKx

 subroutine get_CZM_energy_DKDKx(icdan,stored,damage,failure,cohesion)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: stored,damage,failure,cohesion

   stored   = energyDKDKx(icdan)%stored
   damage   = energyDKDKx(icdan)%damage
   failure  = energyDKDKx(icdan)%failure
   cohesion = energyDKDKx(icdan)%cohesion

 end subroutine get_CZM_energy_DKDKx

 subroutine set_nb_DKDKx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_DKDKx = nb

 end subroutine

 subroutine redo_nb_adj_DKDKx()
   implicit none

   call redo_nb_adj_( get_nb_DISKx() )

   ! because DKcoor is needed in this case
   ! to write vloc_rloc
   if (allocated(DKcoor)) deallocate(DKcoor)
   allocate( DKcoor( 3, get_nb_DISKx() ) )
   call coor_prediction_DKDKx()

 end subroutine

end module DKDKx
