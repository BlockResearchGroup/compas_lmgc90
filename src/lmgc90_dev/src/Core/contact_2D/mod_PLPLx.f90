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
module PLPLx
   
!> This modulus deals with geoemetric and kinematic operations between contactors POLYG.
!> In this modulus candidate contactors are POLYG and antagonist contactors are POLYG.

  use overall
  use tact_behaviour
  use POLYG, only : T_POLYG, get_nb_polyg, polyg2bdyty  , &
                    get_l_polyg, get_radius_polyg       , &
                    get_radii_polyg                     , &       
                    get_min_radius_polyg                , &
                    get_max_radius_polyg                , &
                    get_mean_radius_polyg               , &
                    get_ws_polyg                        , &
                    is_polyg_same_BDYTY                 , &
                    add_betai_to_polyg                  , &
                    get_Vd_polyg                        , &
                    add_stress_polyg                    , &
                    move_bdary_polyg, print_info_polyg  , &
                    get_inv_mass_polyg, get_mass_polyg  , &
                    get_ent_polyg       => get_ent      , &
                    get_color_polyg     => get_color    , &
                    get_visible_polyg   => get_visible  , &
                    get_shiftTT_polyg   => get_shiftTT  , &
                    get_coorTT_polyg    => get_coorTT   , &
                    get_Vbegin_polyg    => get_Vbegin   , &
                    add_reac_polyg      => add_reac     , &
                    get_vlocy_polyg     => get_vlocy    , &
                    comp_vlocy_polyg    => comp_vlocy   , &
                    nullify_reac_polyg  => nullify_reac , &
                    nullify_vlocy_polyg => nullify_vlocy, &
                    get_tact_id
  
  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use parameters, only : i_plplx, i_polyg, i_mailx, i_rbdy2, i_mbs2, &
                         i_IQS_MAC_CZM,i_MAC_CZM,i_IQS_WET_CZM

  use inter_meca_2D

  implicit none
  
  private

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

 ! nb_PLPLx = number of selected candidate POLYG against POLYG
 ! due to the fact that their might be 2 node_segment for each
 ! entry in this it should be higher than size(this)  
 integer         :: nb_PLPLx                          

 integer :: nb_vPLPLx

 type(T_this_adjac), dimension(:), allocatable, target :: adjac

 ! nb_adj(icdbdy): number of adjacent pairs body-contactor to candidate body icdbdy.
 integer           , dimension(:), allocatable, target :: nb_adj

!------------------------------------------------------------------------  

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------

 type T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_PLPLx.
                              
   integer                               :: popul     ! box(ibox1,ibox2)%popul: number of disks in box ibox1,ibox2;
   
   integer, dimension(:), pointer        :: which     ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 end type T_box 

 type(T_box), dimension(:,:),allocatable  :: box      ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

!------------------------------------------------------------------------ 

 type T_rough_PLPLx  
                                                      ! définit le type de la liste des plus proches voisins
    integer :: cd                                     ! le candidat, l'antagoniste et isee pour la loi de contact
    integer :: an
    integer :: isee

!!! > md > !!!
    real(kind=8) :: meff,reff                         ! effective mass and radius for md method 
!!! < md < !!!

    integer      :: periodic                          ! periodic contact flag

 end type T_rough_PLPLx
 
 type(T_rough_PLPLx),dimension(:),allocatable   :: rough_PLPLx     ! table  de visibilité

!------------------------------------------------------------------------ 

 type T_link_rough_PLPLx                                           ! liste chainée pour determiner les listes de cand_ant car
                                                                   ! on ne connait pas a priori le nb de cand-ant 
    type(T_link_rough_PLPLx), pointer :: p                         ! pointeur sur le precedent
    type(T_rough_PLPLx)               :: val                       ! les valeurs
    type(T_link_rough_PLPLx), pointer :: n                         ! pointeur sur le suivant

 end type T_link_rough_PLPLx

 type(T_link_rough_PLPLx),pointer                  :: Root,Current,Previous

!--------------------------------------------------------------------------

 integer                                       :: Nstep_rough_seek_PLPLx=1
 logical                                       :: write_creation_tab_visu
 
!------------------------------------------------------------------------
 logical      :: PERIODIC=.false.
 real(KIND=8) :: PERIODE = 0.d0
 integer      :: nb_PERIODIC_PLPLx
 real(kind=8) :: shrink_ = 0.d0

 integer,dimension(:),allocatable   :: periodic_PLPLx

!------------------------------------------------------------------------
! variables attached to surrounding boxes

 real (kind=8)                    :: maxray, minray, maxalert, meanradius
 real (kind=8)                    :: Lbox,LBox_1,norm
 integer                          :: nb_rough_PLPLx
 integer                          :: nb_recup_PLPLx
 integer                          :: minibox1,maxibox1,minibox2,maxibox2,maxpopul
 integer                          :: nb_big_POLYG
 integer,dimension(:),allocatable :: big_polyg_list
 real(kind=8)                     :: big_polyg_tolerance = 2.0
!------------------------------------------------------------------------

 real(kind=8) :: Reac_PLPLx_MAX=0.D0
 real(kind=8), dimension(:)  , allocatable, target :: violation
 !  coordinates of bodies owning POLYG to be used in selecting prox tactors
 real(kind=8), dimension(:,:), allocatable         :: PLcoor

 !------------------------------------------------------------------------
 
 integer(kind=4)           :: nb_WSsect = 1
 integer(kind=4),parameter :: i_max_friction=0,i_min_friction=1,i_average_friction=2
 integer(kind=4)           :: i_friction_model = 2

 !------------------------------------------------------------------------
! a remonter dans python
 
 type T_ENERGY
    real(kind=8) :: failure
    real(kind=8) :: damage
    real(kind=8) :: stored
    real(kind=8) :: cohesion
 end type T_ENERGY

 type(T_ENERGY),dimension(:),allocatable :: energyPLPLx


 !fd a virer 
 real(kind=8),dimension(:,:),allocatable :: betaiPOLYG
 logical :: first_time_betai=.true.

!------------------------------------------------------------------------

 logical :: RUN=.false.

 logical :: module_checked_ = .FALSE.
 logical :: check_PLPLx_    = .FALSE.

!fd 
!fd variables de post-traitement
!fd
 integer,public            :: nb_ctc_simple            ! nombre de contact avec un seul point
 integer,public            :: nb_ctc_double            ! nombre de contact avec deux points

!------------------------------------------------------------------------
! liste des fonctions publiques
!
 public &
      stock_rloc_PLPLx, &
      recup_rloc_PLPLx, &
      recup_rloc_byposition_PLPLx, &
      compute_box_PLPLx, &
      read_ini_Vloc_Rloc_PLPLx, &
      write_xxx_Vloc_Rloc_PLPLx, &
      set_periodic_data_PLPLx, &
      set_friction_model_PLPLx, &
      coor_prediction_PLPLx, &
      creation_tab_visu_PLPLx, &
      compute_contact_PLPLx, &
      compute_contact_nc_PLPLx, &
      display_prox_tactors_PLPLx, &
      RUN_PLPLx, &
      CHECK_PLPLx, &
      get_write_Vloc_Rloc_PLPLx

 public &
      nullify_reac_PLPLx,&
      nullify_vlocy_PLPLx,&
      injj_PLPLx, prjj_PLPLx, vitrad_PLPLx, &
      get_nb_PLPLx, &
      PLPLx2POLYG, &
!!$   compute_Wikik_PLPLx,compute_Wikjl_PLPLx,get_Wik_PLPLx, &
      print_info_PLPLx,get_type_PLPLx, &
      get_beta_PLPLx,get_length_PLPLx, &
      get_periode_PLPLx, &
      get_eff_plplx, &
      get_g2l_plplx, &
      get_old_index_PLPLx, &
      compute_stress_PLPLx, &
      compute_betai_PLPLx, &
      get_icdtac_PLPLx, &
      get_iantac_PLPLx, &
      clean_memory_PLPLx, &
      set_big_polyg_tolerance_PLPLx, &
      compute_czm_energy_PLPLx, &
      get_CZM_energy_PLPLx, &
      set_shrink_polyg_faces_PLPLx, &
!fd      get_h5_Vloc_Rloc_arg_PLPLx, &
!fd      get_nb_interaction_PLPLx, &
      update_fric_PLPLx
      
 !rm for handler
 public get_this    , &
        set_nb_PLPLx, &
        redo_nb_adj_PLPLx, &
        get_an_tacty     , &
        get_verlet_tact_lawnb

contains

  include 'interaction_common.f90'
  ! defines the following subroutines
  !subroutine get_behaviour_( icdan, see, tact_behav )
  !subroutine get_this_(this_inter, verlet_inter, violation_inter)
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

  subroutine coor_prediction_PLPLx

    implicit none
    
    integer :: itact,errare
    integer :: nb_polyg

    nb_POLYG=get_nb_POLYG()

    do itact=1,nb_POLYG
       PLcoor(1:3,itact) = get_coorTT_POLYG(itact)

       if ( PERIODIC ) then
         if ( PLcoor(1,itact)  > periode ) then
           !print*,'on corrige le POLYG ',itact,' qui sort par x+'
           PLcoor(1,itact) = PLcoor(1,itact) - periode
         else if ( PLcoor(1,itact) < 0.D0 ) then
           !print*,'on corrige le POLYG ',itact,' qui sort par x-'
           PLcoor(1,itact) = PLcoor(1,itact) + periode
         end if
       end if

       !fd
       !fd on replace vertex et normale dans la coorTT
       !fd
       call move_BDARY_POLYG(itact,PLcoor(1:3,itact))
    end do

  end subroutine coor_prediction_PLPLx
  
!------------------------------------------------------------------------
  
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PLPLx(step)
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

    call read_ini_Vloc_Rloc
    close(G_nfich)
    
  end subroutine read_ini_Vloc_Rloc_PLPLx
  
!------------------------------------------------------------------------

  subroutine write_xxx_Vloc_Rloc_PLPLx(which)
    implicit none
    integer(kind=4) :: which,nfich,lc
    
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
    
  end subroutine write_xxx_Vloc_Rloc_PLPLx
  
!------------------------------------------------------------------------

  subroutine set_periodic_data_PLPLx(per,FLAG)
    implicit none
    real(kind=8) :: per
    logical      :: FLAG
    
    periode  = per
    PERIODIC = FLAG
    
  end subroutine set_periodic_data_PLPLx

  !> \brief Set friction model for simulation using evolutive friction
  subroutine set_friction_model_PLPLx(FLAG)
    implicit none
    character(len=3) :: FLAG
    character(len=29) :: IAM
    IAM = 'mod_DKPLx::set_friction_model'
 
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
    
  end subroutine set_friction_model_PLPLx
!------------------------------------------------------------------------

  subroutine set_big_polyg_tolerance_PLPLx(bpt)
             
    implicit none
    real(kind=8) :: bpt

    big_polyg_tolerance = bpt

  end subroutine set_big_polyg_tolerance_PLPLx
  
!------------------------------------------------------------------------

  subroutine compute_box_PLPLx
    implicit none
    integer(kind=4)   :: isee,errare,ibdy,k,found,i_norm
    real(kind=8)      :: tmp_radius,min_radius,max_radius
    integer(kind=4)   :: nb_polyg
    character(len=80) :: cout
                               !1234567890123456789012
    character(len=22) :: IAM = 'mod_PLPLx::compute_box'

    ! on fait ici les choses qui ne doivent etre faites que lorsque nb_POLYG change
 
    nb_POLYG   = get_nb_POLYG()
    minray     = get_min_radius_POLYG()
    maxray     = get_max_radius_POLYG()
    meanradius = get_mean_radius_POLYG()
  
    ! Creation of a list of too big polygon
    ! on dimensionne et on alloue...

    nb_big_POLYG=0
    do ibdy=1,nb_POLYG
      tmp_radius=get_radius_POLYG(ibdy)
      if (tmp_radius > big_polyg_tolerance*meanradius) nb_big_POLYG=nb_big_POLYG+1
    enddo
    write(cout,'(4X,A,I0)') IAM//' : NB BIG POLYG ', nb_big_POLYG
    call logmes(cout,.true.)

    if (nb_big_POLYG.ne.0) then
      if (allocated(big_polyg_list)) deallocate(big_polyg_list)
      allocate(big_polyg_list(nb_big_POLYG))

      ! ... on remplit

      nb_big_POLYG=0
      do ibdy=1,nb_POLYG      
         tmp_radius=get_radius_POLYG(ibdy)
         if (tmp_radius>2.D0*meanradius) then
            nb_big_POLYG=nb_big_POLYG+1
            big_polyg_list(nb_big_POLYG)=ibdy
         endif
      end do

      ! on modifie la taille moyenne de minray,maxray et meanradius
      ! pour ne plus prendre en compte les big polyg

      minray = 1.D20
      maxray = -1.D20
      meanradius=0.D0
   
      do ibdy=1,nb_POLYG
         found=0 
         do k=1,nb_big_POLYG
            if (ibdy==big_polyg_list(k)) then
               found=1
               exit
            endif
         enddo
         if (found==1) cycle
         call get_radii_POLYG(ibdy,min_radius,max_radius)         
         minray  = min(min_radius,minray)
         maxray  = max(max_radius,maxray)
         meanradius=meanradius+max_radius
      enddo

      meanradius=meanradius/(nb_POLYG-nb_big_POLYG)

    end if
  

    if (minray > maxray ) then
      write(cout,'(A,D14.7,1x,D14.7)') 'Messing error computing minray and maxray ', minray,maxray
      call LOGMES(' messing error computing minray and maxray, in compute_box in mod_POLYG')
      call faterr(IAM,cout)
    end if

    ! computing largest alert distance between POLYG

    maxalert=0.D0  
    do isee=1,size(see)
      if (see(isee)%cdtac == 'POLYG' .and. see(isee)%antac == 'POLYG') then
        maxalert=max(maxalert,see(isee)%alert)
      end if
    end do

    Lbox   = 1.01D0*(2.D0*maxray + maxalert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/minray

    i_norm=int(norm)
    maxpopul=(1+i_norm)*(1+i_norm)
    maxpopul=min(maxpopul,nb_POLYG)

    if (.not. allocated(adjac)) then
      allocate(adjac(nb_POLYG),stat=errare)
      if (errare /=0 ) then
        call faterr(IAM,'Error allocating adjac')
      end if
      do ibdy=1,nb_POLYG
        nullify(adjac(ibdy)%icdan)
      end do
    else
      do ibdy=1,nb_POLYG
        if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
        nullify(adjac(ibdy)%icdan)
      enddo
    endif 

    if (allocated(nb_adj)) deallocate(nb_adj)
    allocate(nb_adj(nb_POLYG),stat=errare)
    if (errare /=0 ) then
      call faterr(IAM,'Error allocating nb_adj')
    end if

    nb_adj=0

    ! PLcoor are coordinates of bodies owning POLYG to be used in selecting prox tactors

    if (allocated(PLcoor)) deallocate(PLcoor)
    allocate(PLcoor(3,nb_POLYG),stat=errare)

  end subroutine compute_box_PLPLx
 
!------------------------------------------------------------------------

  subroutine creation_tab_visu_PLPLx(convex)
    implicit none
    logical                               :: convex
    integer                               :: errare,k,found,i
    integer                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
    integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                             icdtac,iantac,isee,itacty   
    real(kind=8)                          :: Bleft,Bright,Bup,Bdown,masscd,massan
    character(len=5)                      :: cdtac,cdcol,antac,ancol
    real(kind=8),dimension(3)             :: coord,coordcd,coordan 
    real(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gapTT
    integer                               :: nb_polyg

    character(len=28) :: IAM = 'mod_PLPLx::creation_tab_visu'
    character(len=80) :: cout

    nb_POLYG=get_nb_POLYG()

! Since the list of proximate contactors may not be updated at every time step,
! boxes data would be lost if deallocated. When starting the program, boxes are not created.
! A warning condition prevents undue deallocation. 

    if (allocated(box)) then
      do ibox1=minibox1,maxibox1
         do ibox2=minibox2,maxibox2
            if (associated(box(ibox1,ibox2)%which)) deallocate(box(ibox1,ibox2)%which)
         enddo
      enddo
      deallocate(box)
    endif
    
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

    Bleft   =  1.D24
    Bright  = -1.D24
    Bup     = -1.D24
    Bdown   =  1.D24
  
    do ibdy=1,nb_POLYG
      if (.not.get_visible_POLYG(ibdy)) cycle
      found=0
      do k=1,nb_big_POLYG
         if (ibdy==big_polyg_list(k)) then
            found=1
            exit
         endif
      enddo
      if (found==1) cycle
      coord = PLcoor(1:3,ibdy)
      Bleft = min(coord(1),Bleft )
      Bright= max(coord(1),Bright)
      Bup   = max(coord(2),Bup   )
      Bdown = min(coord(2),Bdown )
    end do
  
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
    ! for each box maxpopul is less than the total number of POLYG 
    !

    !   
    allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)
   
    if (errare /=0 ) then
      call faterr(IAM,'Error allocating box')
   end if    
   do ibox1=minibox1,maxibox1
   do ibox2=minibox2,maxibox2
     box(ibox1,ibox2)%popul=0
     allocate(box(ibox1,ibox2)%which(maxpopul),stat=errare)
     if (errare /=0 ) then
       write(cout,'(A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%which'
       call faterr(IAM,cout)
     end if
   end do
   end do
   
   ! filling boxes with polygons
   ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%which(ipopul) is the rank of body POLYG labelled ipopul in the box
   
   ! filling boxes   
   
   do ibdy=1,nb_POLYG

     if (.not.get_visible_POLYG(ibdy)) cycle

     found=0
     do k=1,nb_big_POLYG
        if (ibdy==big_polyg_list(k)) then
           found=1
           exit
        endif
     enddo
     if (found==1) cycle

     coord=PLcoor(1:3,ibdy)
     ibox1=1+int((coord(1)-Bleft )*Lbox_1)
     ibox2=1+int((coord(2)-Bdown )*Lbox_1)
     if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
       write(cout,'(A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       write(cout,'(A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2
       write(cout,'(A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2
       write(cout,'(A13,I5,A13)')'  body POLYG ',ibdy,' out of boxes'
       call faterr(IAM,cout)
     end if
   
     box(ibox1,ibox2)%popul=box(ibox1,ibox2)%popul+1
     if( box(ibox1,ibox2)%popul > size(box(ibox1,ibox2)%which) ) then
         call faterr(IAM, "Estimated max popul limit reached.")
     end if
     box(ibox1,ibox2)%which(box(ibox1,ibox2)%popul)=ibdy
   end do  

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
     
   nb_rough_PLPLx=0

   ! création de la liste de paire de polygones à examiner

   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

   nullify(Root) 
   nullify(Current)
   nullify(Previous)
   
   do ibox2cd=minibox2,maxibox2
   do ibox1cd=minibox1,maxibox1  
     do icdpop=1,box(ibox1cd,ibox2cd)%popul
       
       icdtac=box(ibox1cd,ibox2cd)%which(icdpop)
       
       cdcol=get_color_POLYG(icdtac)
       coordcd = PLcoor(1:3,icdtac)
       raycd = get_radius_POLYG(icdtac)
       ! box loop investigating antagonist polyg
       do ibox2an=max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                        
       do ibox1an=max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                   
         do ianpop=1,box(ibox1an,ibox2an)%popul

           iantac=box(ibox1an,ibox2an)%which(ianpop)
           if (iantac .le. icdtac) cycle
           if (is_POLYG_same_BDYTY(icdtac,iantac)) cycle
           ancol=get_color_POLYG(iantac)

           !Gs trop lent pour même tacty isee=get_isee('RBDY2','POLYG',cdcol,'RBDY2','POLYG',ancol)

           if( polyg2bdyty(3,iantac) == polyg2bdyty(3,icdtac) ) then
             isee = get_isee_specific('POLYG',cdcol,ancol)
           else
             isee = get_isee(get_body_model_name_from_id(polyg2bdyty(3,icdtac)),'POLYG',cdcol, &
                             get_body_model_name_from_id(polyg2bdyty(3,iantac)),'POLYG',ancol)
           end if
           if (isee /= 0) then
             adist=see(isee)%alert 
             ! checking ROUGHLY distance against alert distance           
             coordan = PLcoor(1:3,iantac)
             rayan = get_radius_POLYG(iantac)
             ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
             ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
             ! results might be different up to some non significant figures, but when comparing to
             ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

             adist=0.1005D+01*adist+raycd+rayan
            
             dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
                  (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))

             if (dist<adist*adist) then

               nb_rough_PLPLx=nb_rough_PLPLx+1
               if ( nb_rough_PLPLx == 1) then
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
       end do
     end do
   enddo
   enddo

   !fd 27/08/08 big polyg ne gere pas perio  
   do k=1,nb_big_POLYG
     icdtac=big_polyg_list(k)

     if (.not.get_visible_POLYG(icdtac)) cycle
     cdcol   = get_color_POLYG(icdtac)
     coordcd = PLcoor(1:3,icdtac)
     raycd   = get_radius_POLYG(icdtac)

     do iantac=1,nb_POLYG

       if (icdtac==iantac) cycle
       if (.not. get_visible_POLYG(iantac)) cycle
       if (is_POLYG_same_BDYTY(icdtac,iantac)) cycle

       ! pour eviter de compter 2 fois le contact entre 2 big_POLYG
       found=0
       do i=1,nb_big_POLYG
          if (iantac == big_polyg_list(i)) then
             found=1
             exit
          endif
       enddo
       if (found==1 .and. iantac <= icdtac) cycle
       
       ancol=get_color_POLYG(iantac)

       if( polyg2bdyty(3,iantac) == polyg2bdyty(3,icdtac) ) then
         isee = get_isee_specific('POLYG',cdcol,ancol)
       else
         isee = get_isee(get_body_model_name_from_id(polyg2bdyty(3,icdtac)),'POLYG',cdcol, &
                         get_body_model_name_from_id(polyg2bdyty(3,iantac)),'POLYG',ancol)
       end if

       if (isee /= 0) then
         adist=see(isee)%alert 
         coordan = PLcoor(1:3,iantac)
         rayan = get_radius_POLYG(iantac)
         adist=0.1005D+01*adist+raycd+rayan
         dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
              (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))
         if (dist<adist*adist) then
           nb_rough_PLPLx=nb_rough_PLPLx+1
           if (nb_rough_PLPLx == 1) then
             allocate(Root)
             Current => Root
             nullify(Root%p)
           else
             allocate(Current)
             Previous%n => Current
           endif
           ! if (icdtac>iantac) then
           !   Current%val%cd       =iantac
           !   Current%val%an       =icdtac
           !   Current%val%isee     =isee
           !   Current%val%periodic =0
           ! else  
             Current%val%cd       =icdtac
             Current%val%an       =iantac
             Current%val%isee     =isee
             Current%val%periodic =0
           ! endif       
           Current%p => Previous
           nullify(Current%n)
           Previous => Current
         endif
       endif  
     enddo   
   enddo   

   
   nb_PERIODIC_PLPLx = 0
   
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
            cdcol  = get_color_POLYG(icdtac)
            ! box loop investigating antagonist polyg
            !fd 03/01/08 A VOIR je ne comprends pas le premier+1 !?
            do ibox1an = minibox1,minibox1+1
            do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
!               print*,'boite antagoniste ',ibox2an,' population ',box(ibox1an,ibox2an)%popul

               do ianpop = 1,box(ibox1an,ibox2an)%popul
                  
                  iantac = box(ibox1an,ibox2an)%which(ianpop)
                  
                  IF (is_POLYG_same_BDYTY(icdtac,iantac)) CYCLE

                  ancol = get_color_POLYG(iantac)

                  if( polyg2bdyty(3,iantac) == polyg2bdyty(3,icdtac) ) then
                    isee = get_isee_specific('POLYG',cdcol,ancol)
                  else
                    isee = get_isee(get_body_model_name_from_id(polyg2bdyty(3,icdtac)),'POLYG',cdcol, &
                                    get_body_model_name_from_id(polyg2bdyty(3,iantac)),'POLYG',ancol)
                  end if
                  
                  if (isee /= 0 ) then
                     adist   = see(isee)%alert 
                     ! checking ROUGHLY distance against alert distance           
                     coordcd = PLcoor(1:3,icdtac)
                     coordan = PLcoor(1:3,iantac)

                     coordan(1) = coordan(1) + periode
                     
                     raycd   = get_radius_POLYG(icdtac)
                     rayan   = get_radius_POLYG(iantac)
                     
                     ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                     ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                     ! results might be different up to some non significant figures, but when comparing to
                     ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                     adist = 0.1005D+01*adist+raycd+rayan
                     if (       dabs(coordcd(1)-coordan(1)) <= adist &
                          .and. dabs(coordcd(2)-coordan(2)) <= adist ) then
                        
                        nb_rough_PLPLx    = nb_rough_PLPLx+1
                        nb_PERIODIC_PLPLx = nb_PERIODIC_PLPLx + 1
                        
                        if ( nb_rough_PLPLx == 1) then
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

     do k=1,nb_big_POLYG
       icdtac=big_polyg_list(k)

       if (.not.get_visible_POLYG(icdtac)) cycle
       cdcol   = get_color_POLYG(icdtac)
       coordcd = PLcoor(1:3,icdtac)
       raycd   = get_radius_POLYG(icdtac)

       do iantac=1,nb_POLYG

         if (icdtac==iantac) cycle
         if (.not. get_visible_POLYG(iantac)) cycle
         if (is_POLYG_same_BDYTY(icdtac,iantac)) cycle

         ! pour eviter de compter 2 fois le contact entre 2 big_POLYG
         found=0
         do i=1,nb_big_POLYG
            if (iantac == big_polyg_list(i)) then
               found=1
               exit
            endif
         enddo
         if (found==1 .and. icdtac > iantac) cycle
       
         ancol=get_color_POLYG(iantac)

         if( polyg2bdyty(3,iantac) == polyg2bdyty(3,icdtac) ) then
           isee = get_isee_specific('POLYG',cdcol,ancol)
         else
           isee = get_isee(get_body_model_name_from_id(polyg2bdyty(3,icdtac)),'POLYG',cdcol, &
                           get_body_model_name_from_id(polyg2bdyty(3,iantac)),'POLYG',ancol)
         end if

         if (isee /= 0) then
           adist=see(isee)%alert 
           coordan = PLcoor(1:3,iantac)
           coordan(1) = coordan(1) + periode
           rayan = get_radius_POLYG(iantac)
           adist=0.1005D+01*adist+raycd+rayan
           dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
                (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))
           if (dist<adist*adist) then
              
             nb_rough_PLPLx=nb_rough_PLPLx+1
             nb_PERIODIC_PLPLx = nb_PERIODIC_PLPLx +1
             
             if (nb_rough_PLPLx == 1) then
               allocate(Root)
               Current => Root
               nullify(Root%p)
             else
               allocate(Current)
               Previous%n => Current
             endif
             if (icdtac > iantac) then
               Current%val%cd       =iantac
               Current%val%an       =icdtac
               Current%val%isee     =isee
               Current%val%periodic =-1
             else
               Current%val%cd       =icdtac
               Current%val%an       =iantac
               Current%val%isee     =isee
               Current%val%periodic =1
             endif  
             Current%p => Previous
             nullify(Current%n)
             Previous => Current
           endif
         endif  
       enddo   
     enddo   
      
     write(cout,'(4X,I0,A)') nb_PERIODIC_PLPLx,' periodic PLPLx roughly found'
     call logmes(cout)

   end if

  write(cout,'(4X,I10,A20)') nb_rough_PLPLx,' PLPLx roughly found'
  call logmes(cout)


  if (allocated(rough_PLPLx)) deallocate(rough_PLPLx)
  allocate(rough_PLPLx(nb_rough_PLPLx))     ! the visibility array used in compute_contact is allocated
  
  if (allocated(this)) deallocate(this)
  if (allocated(periodic_PLPLx)) deallocate(periodic_PLPLx)
  if (convex) then
    ! si convexe, il y a au maximum 2 points de contact entre 2 POLYG
    allocate(this(2*nb_rough_PLPLx))
    allocate(periodic_PLPLx(2*nb_rough_PLPLx)) 
  else
    ! si non convexe, il peut y en avoir bcp plus !
    allocate(this(10*nb_rough_PLPLx))
    allocate(periodic_PLPLx(10*nb_rough_PLPLx)) 
  endif
  
  do icdan=nb_rough_PLPLx,1,-1
     
    Previous => Current%p
    rough_PLPLx(icdan)%cd     = Current%val%cd
    rough_PLPLx(icdan)%an     = Current%val%an
    rough_PLPLx(icdan)%isee   = Current%val%isee
    rough_PLPLx(icdan)%periodic   = Current%val%periodic

    raycd = get_radius_POLYG(Current%val%cd)
    rayan = get_radius_POLYG(Current%val%an)
      
    rough_PLPLx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
    masscd=get_mass_POLYG(polyg2bdyty(1,Current%val%cd))
    massan=get_mass_POLYG(polyg2bdyty(1,Current%val%an))
      
    rough_PLPLx(icdan)%meff = masscd*massan/(masscd+massan)

    deallocate(Current)
    Current => Previous
  end do 
   
  nullify(Root)

  end subroutine creation_tab_visu_PLPLx

!--------------------------------------------------------------------------------------

  subroutine compute_contact_PLPLx

  use algebra
  use predicates

  implicit none  
  integer                               :: errare
  type(T_POLYG)                         :: PGicdtac,PGiantac,r_PGicdtac,r_PGiantac
  integer                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
  integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                           icdtac,iantac,isee,itacty    
  character(len=5)                      :: cdtac,cdcol,antac,ancol
  real(kind=8),dimension(3)             :: coordcd,coordan,cd_Vbegin,an_Vbegin 
  real(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gapTT,dgap
  integer                               :: r_icdtac,r_iantac ! real ...
  integer,dimension(2)                  :: icdver                              ! vertex candidats 
  integer                               :: ianseg                              ! segment antagoniste
  real(kind=8),dimension(2)             :: cd_shift,an_shift                               ! local barycenter shift vector 
  integer                               :: i,id,j,iadj_to
  real(kind=8),dimension(2,2)           :: xco                            ! points de contact
  real(kind=8),dimension(2)             :: ovlap                          ! les gaps en sortie detect()
  real(kind=8),dimension(2)             :: t,n                            ! la normale en sortie detect()
  real(kind=8),dimension(2)             :: cdlev,anlev                    ! les vecteurs centre -> point de contact
  real(kind=8),dimension(2)             :: sep                            ! vecteur reliant les centres des polygones
  real(kind=8)                          :: norm                           ! scalaire contenant la norme de sep
  integer                               :: cd_ent,an_ent
  integer                               :: cd_na,cd_nb,an_na,an_nb        ! numero des vertex
  real(kind=8)                          :: xi                             ! coordonnee reduite sur la ligne
  integer :: nb_polyg,iprd
  real(kind=8),dimension(2)             :: perio_shift, shift

  character(len=80) :: cout

  real(kind=8),dimension(2)             :: tcd,ncd,nan

  icdan=0        
  nb_PLPLx=0

  nb_adj=0

  nb_ctc_simple=0
  nb_ctc_double=0
 
  if (nb_rough_PLPLx /= 0 ) then
!
! preparing detection
!
    icdtac=1  ! pour l'instant, c'est ok...
    iantac=1

    do i=1,nb_rough_plplx
      icdtac=rough_PLPLx(i)%cd
      iantac=rough_PLPLx(i)%an

      isee=rough_PLPLx(i)%isee

      iprd   = rough_PLPLx(i)%periodic
         
      adist=see(isee)%alert 

      coordcd = PLcoor(1:3,icdtac)
      coordan = PLcoor(1:3,iantac)

      coordan(1) = coordan(1) + (real(iprd,8)*periode)

      raycd= get_radius_POLYG(icdtac)
      rayan= get_radius_POLYG(iantac)
 
      dist= raycd+rayan+adist

      ! predetection par les disques d'encombrement
      sep=coordcd(1:2)-coordan(1:2)                   
      norm=sep(1)*sep(1)+sep(2)*sep(2)
     
      if (norm<dist**2) then

        !fd 28/08/08 on decale juste pour le traitement du contact
        if (iprd .ne. 0) call move_BDARY_POLYG(iantac,coordan)   
  
        sep=sep/dsqrt(norm)                                        

        PGicdtac= get_l_POLYG(icdtac)
        PGiantac= get_l_POLYG(iantac)
        
        call detect(PGicdtac,PGiantac,icdtac,iantac, &
                    r_icdtac,r_iantac,icdver,ianseg,xco,n,ovlap,-sep,adist)

        ! shrink des points de contact vers le centre du joint (que si il y a 2 ptc)
        ! shrink = 0 -> ptc au bord
        ! shrink = 1 -> pts au centre
        if (( shrink_ /= 0 ) .and. (icdver(1) /= 0) .and. (icdver(2) /= 0)) then
           shift(1:2) = shrink_ * 0.5 * ( xco( 1:2, 2 ) - xco( 1:2, 1 ) )
           xco(1:2,1) = xco(1:2,1) + shift(1:2)
           xco(1:2,2) = xco(1:2,2) - shift(1:2)
           
           dgap       = shrink_ * 0.5 * ( ovlap(2) - ovlap(1) )
           ovlap(1)   = ovlap(1) + dgap
           ovlap(2)   = ovlap(2) - dgap
        endif
       
       !fd 28/08/08 on ramene une fois termine
        if (iprd .ne. 0) call move_BDARY_POLYG(iantac,PLcoor(1:3,iantac))

        ! checking distance against alert distance           
        if (r_icdtac /= 0) then

          nb_ctc_simple=nb_ctc_simple+1

          !switch cd/an 
          perio_shift=0.D0
          if (icdtac /= r_icdtac) then
            r_PGicdtac=get_l_POLYG(r_icdtac)
            r_PGiantac=get_l_POLYG(r_iantac)
            perio_shift(1) = (real(iprd,8)*periode)
          else
            !flip normal vector            
            r_PGicdtac=PGicdtac
            r_PGiantac=PGiantac
            n=-n     
            perio_shift(2) = (real(iprd,8)*periode)
          endif

          icdan=icdan+1

          nb_adj(r_icdtac)    = nb_adj(r_icdtac) + 1

          iadj=nb_adj(r_icdtac)
                     
          this(icdan)%icdbtac = polyg2bdyty(2, icdtac)
          this(icdan)%ianbtac = polyg2bdyty(2, iantac)

          this(icdan)%icdbtyp = polyg2bdyty(3, icdtac)
          this(icdan)%ianbtyp = polyg2bdyty(3, iantac)

          this(icdan)%icdctyp = i_polyg
          this(icdan)%ianctyp = i_polyg

          this(icdan)%iadj    = iadj
          this(icdan)%icdbdy  = polyg2bdyty(1, r_icdtac)
          this(icdan)%icdtac  = r_icdtac
          this(icdan)%ianbdy  = polyg2bdyty(1, r_iantac)
          this(icdan)%iantac  = r_iantac
          this(icdan)%icocdan = 0
          this(icdan)%dct     = 0

          this(icdan)%isee    = isee

          cd_ent = get_ent_POLYG(this(icdan)%icdtac)
          an_ent = get_ent_POLYG(this(icdan)%iantac) 

          this(icdan)%icdent = cd_ent
          this(icdan)%ianent = an_ent

          if (cd_ent /= an_ent) then
            entity(cd_ent)%nb = entity(cd_ent)%nb+1
            entity(an_ent)%nb = entity(an_ent)%nb+1
          else
            entity(cd_ent)%nb = entity(cd_ent)%nb+1
          end if
     
          this(icdan)%icdsci   = icdver(1)
          this(icdan)%iansci   = ianseg
          this(icdan)%nuc(1:2) =  n

          t(1)=n(2);t(2)=-n(1)
          this(icdan)%tuc(1:2) =  t 

          cd_shift = get_shiftTT_POLYG(r_icdtac)
          an_shift = get_shiftTT_POLYG(r_iantac)

          this(icdan)%coor(1:2) = xco(1:2, 1)
          
          cdlev= xco(1:2,1) + cd_shift(1:2) - PLcoor(1:2, r_icdtac)
          anlev= xco(1:2,1) + an_shift(1:2) - PLcoor(1:2, r_iantac)

          ! on decale si periodique mais comme on a potentiellement
          ! permute les objets on fait gaffe

          cdlev(1) = cdlev(1) - perio_shift(1)
          anlev(1) = anlev(1) - perio_shift(2)

          this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
          this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
          this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
          this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)
                    
          cd_Vbegin = get_Vbegin_POLYG(r_icdtac)
          an_Vbegin = get_Vbegin_POLYG(r_iantac)

          this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                                 + cd_Vbegin(3)*this(icdan)%Gcdt3 &
                                 - an_Vbegin(3)*this(icdan)%Gant3

          this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                                 + cd_Vbegin(3)*this(icdan)%Gcdn3 &
                                 - an_Vbegin(3)*this(icdan)%Gann3
  
          this(icdan)%gapTTBEGIN  = -ovlap(1)
          periodic_PLPLx(icdan)  =  iprd

          this(icdan)%rlt       =0.D0
          this(icdan)%rln       =0.D0
          this(icdan)%vlt       =this(icdan)%vltBEGIN
          this(icdan)%vln       =this(icdan)%vlnBEGIN
          this(icdan)%gapTT     =this(icdan)%gapTTBEGIN
          this(icdan)%status    =i_nknow

          this(icdan)%reff    = rough_PLPLx(i)%reff
          this(icdan)%meff    = rough_PLPLx(i)%meff

          !fd 10/02/05
          ! calcul de la coordonnee du point de contact par rapport aux polygones de reference 
          
          cd_na = icdver(1)
          cd_nb = icdver(1)+1
          if (cd_nb > r_PGicdtac%nb_vertex) cd_nb=1

          this(icdan)%icdcoor(1) = r_PGicdtac%vertex_ref(1,icdver(1))
          this(icdan)%icdcoor(2) = r_PGicdtac%vertex_ref(2,icdver(1))

          an_na = ianseg
          an_nb = ianseg+1
          if (an_nb > r_PGiantac%nb_vertex) an_nb=1          

          xi =( ( this(icdan)%coor(1)-r_PGiantac%vertex(1, an_na) ) * t(1) +   &
                ( this(icdan)%coor(2)-r_PGiantac%vertex(2, an_na) ) * t(2) ) / &
              ( ( r_PGiantac%vertex(1, an_nb)-r_PGiantac%vertex(1, an_na) ) * t(1) + &
                ( r_PGiantac%vertex(2, an_nb)-r_PGiantac%vertex(2, an_na) ) * t(2)   )
         
          this(icdan)%iancoor(1) = (1.d0-xi) * r_PGiantac%vertex_ref(1, an_na) + &
                                         xi  * r_PGiantac%vertex_ref(1, an_nb)
          this(icdan)%iancoor(2) = (1.d0-xi) * r_PGiantac%vertex_ref(2, an_na) + &
                                         xi  * r_PGiantac%vertex_ref(2, an_nb)

          if (icdver(2) /= 0) then 

            nb_ctc_double = nb_ctc_double + 1
            nb_ctc_simple = nb_ctc_simple - 1

            icdan = icdan+1

            nb_adj(r_icdtac) = nb_adj(r_icdtac) + 1
            iadj             = nb_adj(r_icdtac)

            this(icdan)%iadj = iadj

            !fd infos sur le double contact

            this(icdan)%dct = 1 ; this(icdan-1)%dct = 1
            this(icdan)%icocdan   = icdan-1
            this(icdan-1)%icocdan = icdan

            this(icdan)%icdbtac = polyg2bdyty(2, r_icdtac)
            this(icdan)%ianbtac = polyg2bdyty(2, r_iantac)

            this(icdan)%icdbtyp = polyg2bdyty(3, r_icdtac)
            this(icdan)%ianbtyp = polyg2bdyty(3, r_iantac)

            this(icdan)%icdctyp = i_polyg
            this(icdan)%ianctyp = i_polyg

            this(icdan)%icdbdy  = polyg2bdyty(1, r_icdtac)
            this(icdan)%icdtac  = r_icdtac
            this(icdan)%ianbdy  = polyg2bdyty(1, r_iantac)
            this(icdan)%iantac  = r_iantac
            this(icdan)%isee    = isee

            cd_shift = get_shiftTT_POLYG(r_icdtac)
            an_shift = get_shiftTT_POLYG(r_iantac)

            this(icdan)%coor(1:2) = xco(1:2, 2)

            cdlev= xco(1:2,2) + cd_shift(1:2) - PLcoor(1:2, r_icdtac)
            anlev= xco(1:2,2) + an_shift(1:2) - PLcoor(1:2, r_iantac)

            cdlev(1) = cdlev(1) - perio_shift(1)
            anlev(1) = anlev(1) - perio_shift(2)

            cd_ent = get_ent_POLYG(this(icdan)%icdtac)
            an_ent = get_ent_POLYG(this(icdan)%iantac) 

            this(icdan)%icdent = cd_ent
            this(icdan)%ianent = an_ent
            if (cd_ent /= an_ent) then
              entity(cd_ent)%nb = entity(cd_ent)%nb+1
              entity(an_ent)%nb = entity(an_ent)%nb+1
            else
              entity(cd_ent)%nb = entity(cd_ent)%nb+1
            end if

            this(icdan)%icdsci = icdver(2)
            this(icdan)%iansci = ianseg
            this(icdan)%nuc(:) =  n

            t(1)=n(2);t(2)=-n(1)
            this(icdan)%tuc(:) =  t 

            this(icdan)%Gcdt3  = -cdlev(2)*t(1) + cdlev(1)*t(2)
            this(icdan)%Gcdn3  = -cdlev(2)*n(1) + cdlev(1)*n(2)
            this(icdan)%Gant3  = -anlev(2)*t(1) + anlev(1)*t(2)
            this(icdan)%Gann3  = -anlev(2)*n(1) + anlev(1)*n(2)

            cd_Vbegin = get_Vbegin_POLYG(r_icdtac)
            an_Vbegin = get_Vbegin_POLYG(r_iantac)

            this(icdan)%vltBEGIN   =  ( cd_Vbegin(1)-an_Vbegin(1) ) * t(1) &
                                    + ( cd_Vbegin(2)-an_Vbegin(2) ) * t(2) &
                                    + cd_Vbegin(3)*this(icdan)%Gcdt3       &
                                    - an_Vbegin(3)*this(icdan)%Gant3

            this(icdan)%vlnBEGIN   =  ( cd_Vbegin(1)-an_Vbegin(1) ) * n(1) &
                                    + ( cd_Vbegin(2)-an_Vbegin(2) ) * n(2) &
                                    + cd_Vbegin(3)*this(icdan)%Gcdn3       &
                                    - an_Vbegin(3)*this(icdan)%Gann3

            this(icdan)%gapTTBEGIN = -ovlap(2)
            periodic_PLPLx(icdan)  =  iprd

            this(icdan)%rlt        = 0.d0
            this(icdan)%rln        = 0.D0
            this(icdan)%vlt        = this(icdan)%vltBEGIN
            this(icdan)%vln        = this(icdan)%vlnBEGIN
            this(icdan)%gapTT      = this(icdan)%gapTTBEGIN
            this(icdan)%status     = i_nknow

            !fd 10/02/05
            ! calcul de la coordonnee du point de contact par rapport aux polygones de reference 
            
            if (icdver(1) < icdver(2) .or. (icdver(1) == r_PGicdtac%nb_vertex .and. icdver(2)==1 )) then
              cd_na = icdver(1)
              cd_nb = icdver(2)
            else
              cd_na = icdver(2)
              cd_nb = icdver(1)
            endif

            xi = ( ( this(icdan)%coor(1)-r_PGicdtac%vertex(1, cd_na) )        * t(1) +   &
                   ( this(icdan)%coor(2)-r_PGicdtac%vertex(2, cd_na) )        * t(2) ) / &
                 ( ( r_PGicdtac%vertex(1, cd_nb)-r_PGicdtac%vertex(1, cd_na)) * t(1) +   &
                   ( r_PGicdtac%vertex(2, cd_nb)-r_PGicdtac%vertex(2, cd_na)) * t(2) )


            this(icdan)%icdcoor(1) = (1.d0-xi) * r_PGicdtac%vertex_ref(1, cd_na) + &
                                           xi  * r_PGicdtac%vertex_ref(1, cd_nb)
            this(icdan)%icdcoor(2) = (1.d0-xi) * r_PGicdtac%vertex_ref(2, cd_na) + &
                                           xi  * r_PGicdtac%vertex_ref(2, cd_nb)

            an_na = ianseg
            an_nb = ianseg+1
            if (an_nb > r_PGiantac%nb_vertex) an_nb=1          

            xi = ( ( this(icdan)%coor(1)-r_PGiantac%vertex(1, an_na) )        * t(1) +   &
                   ( this(icdan)%coor(2)-r_PGiantac%vertex(2, an_na) )        * t(2) ) / &
                 ( ( r_PGiantac%vertex(1, an_nb)-r_PGiantac%vertex(1, an_na)) * t(1) +   &
                   ( r_PGiantac%vertex(2, an_nb)-r_PGiantac%vertex(2, an_na)) * t(2) )

            this(icdan)%iancoor(1) = (1.d0-xi) * r_PGiantac%vertex_ref(1, an_na) + &
                                           xi  * r_PGiantac%vertex_ref(1, an_nb)
            this(icdan)%iancoor(2) = (1.d0-xi) * r_PGiantac%vertex_ref(2, an_na) + &
                                           xi  * r_PGiantac%vertex_ref(2, an_nb)
          end if 
        end if 
      endif 
    enddo
    nb_PLPLx=icdan
  endif

  write(cout,'(1X,I10,A12)') nb_PLPLx,' PLPLx found'
  call logmes(cout)

  nb_POLYG=get_nb_POLYG()   

  do ibdy=1,nb_POLYG
    if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)

    if (nb_adj(ibdy) /= 0) then
      allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      if (errare /=0 ) then
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_PLPLx::compute_contact',cout)
      end if
    endif

  enddo 
   
  do icdan=1,nb_PLPLx
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
  end do 

  do icdan = 1, nb_PLPLx
     call get_behaviour_( icdan, see, tact_behav )
  end do

  if (allocated(violation)) deallocate(violation)
  allocate(violation(nb_PLPLx),stat=errare)
 
  end subroutine compute_contact_PLPLx

!--------------------------------------------------------------------------------------

  subroutine compute_contact_nc_PLPLx

  implicit none
  integer                               :: errare
  integer                               :: nbv,iv
  integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                           icdtac,iantac,isee,nb_POLYG
  real(kind=8),dimension(3)             :: coordcd,coordan,cd_Vbegin,an_Vbegin 
  real(kind=8)                          :: adist,gdist,dist,gapTT,rayan,raycd
  ! local barycenter shift vector 
  real(kind=8),dimension(2)             :: cd_shift,an_shift,sep                           
  integer                               :: i,j,k,ivan,ivap,iprd
  ! statut du point de contact
  integer,dimension(:),pointer          :: status=>null(),ianseg=>null(),icdver=>null()           
  integer,dimension(:),pointer          :: r_icdtac=>null(),r_iantac=>null()
  ! les gaps en sortie de detect()  
  real(kind=8),dimension(:),pointer     :: ovlap=>null()                  
  ! la normale en sortie de detect()
  real(kind=8),dimension(:,:),pointer   :: nn=>null()
  ! weight sur vertices  
  real(kind=8),dimension(:),pointer     :: weight=>null()                         
  real(kind=8),dimension(2)             :: t,n                            ! 
  real(kind=8),dimension(2)             :: cdlev,anlev                    ! les vecteurs centre -> point de contact
  real(kind=8)                          :: norm                           ! scalaire contenant la norme de sep
  integer                               :: cd_ent,an_ent
  logical                               :: contact
  type(T_POLYG)                         :: PGcdtac,PGantac

  character(len=80) :: cout


  icdan    = 0        
  nb_PLPLx = 0

  nb_adj   = 0

  if (nb_rough_PLPLx /= 0 ) then
!
! preparing detection
!
    do i=1,nb_rough_plplx
      icdtac = rough_PLPLx(i)%cd
      iantac = rough_PLPLx(i)%an
      isee   = rough_PLPLx(i)%isee
      iprd   = rough_PLPLx(i)%periodic
      ! print*,"---------------------------------------------------------"
      ! print*,"rough_PLPLx=",i,"  :  ",icdtac," -> ",iantac

      adist  = see(isee)%alert
      gdist  = see(isee)%global_alert 

      coordcd    = PLcoor(:,icdtac)
      coordan    = PLcoor(:,iantac)
      coordan(1) = coordan(1) + (real(iprd,8)*periode)
      sep        = coordcd(1:2) - coordan(1:2)                      ! predetection par les disques d'encombrement
      norm       = dot_product(sep,sep)

      raycd = get_radius_POLYG(icdtac)
      rayan = get_radius_POLYG(iantac)
      dist  = raycd+rayan+adist

      if (norm > dist**2) CYCLE

      !fd 28/08/08 on decale juste pour le traitement du contact
      if (iprd .ne. 0) call move_BDARY_POLYG(iantac,coordan)

      ! detection non convexe
      if (associated(status))   deallocate(status)
      if (associated(r_icdtac)) deallocate(r_icdtac)
      if (associated(r_iantac)) deallocate(r_iantac)
      if (associated(icdver))   deallocate(icdver)
      if (associated(ianseg))   deallocate(ianseg)
      if (associated(weight))   deallocate(weight)
      if (associated(nn))       deallocate(nn)
      if (associated(ovlap))    deallocate(ovlap)

      nullify(status,r_icdtac,r_iantac,icdver,ianseg,weight,nn,ovlap)

      call detect_non_convex(icdtac,iantac,adist, gdist, &
                             contact,nbv,status,r_icdtac,r_iantac,icdver,ianseg,weight,nn,ovlap)

      !fd 28/08/08 on ramene une fois termine
      if (iprd .ne. 0) call move_BDARY_POLYG(iantac,PLcoor(1:3,iantac))

      IF (.not.(contact)) CYCLE
        
    ! boucle sur tous les contacts trouves dans la detection
      do j=1,nbv

        if (status(j) == 0) cycle

        icdan     = icdan + 1
        icdtac    = r_icdtac(j)
        iantac    = r_iantac(j)
        PGcdtac   = get_l_POLYG(icdtac)
        PGantac   = get_l_POLYG(iantac)
        
        cd_Vbegin = get_Vbegin_POLYG(icdtac)
        an_Vbegin = get_Vbegin_POLYG(iantac)

        cd_shift  = get_shiftTT_POLYG(icdtac)
        an_shift  = get_shiftTT_POLYG(iantac)
        
        nb_adj(icdtac) = nb_adj(icdtac) + 1

        iadj = nb_adj(icdtac)
                     
        this(icdan)%icdbtyp = polyg2bdyty(3, icdtac)
        this(icdan)%ianbtyp = polyg2bdyty(3, iantac)
        this(icdan)%icdctyp = i_polyg
        this(icdan)%ianctyp = i_polyg

        this(icdan)%iadj    = iadj
        this(icdan)%icdbdy  = polyg2bdyty(1, icdtac)
        this(icdan)%icdtac  = icdtac
        this(icdan)%ianbdy  = polyg2bdyty(1, iantac)
        this(icdan)%iantac  = iantac
        this(icdan)%icocdan = 0
        this(icdan)%dct     = 0

        this(icdan)%isee    = isee

        cd_ent = get_ent_POLYG(this(icdan)%icdtac)
        an_ent = get_ent_POLYG(this(icdan)%iantac) 

        this(icdan)%icdent = cd_ent
        this(icdan)%ianent = an_ent

        if (cd_ent /= an_ent) then
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
          entity(an_ent)%nb = entity(an_ent)%nb+1
        else
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
        end if

        this(icdan)%icdsci = icdver(j)
        this(icdan)%iansci = ianseg(j)
                    
        ivan = ianseg(j)
        ivap = ianseg(j)+1
        if (ivap > PGantac%nb_vertex) ivap = 1  
        this(icdan)%coor(:) = weight(j)*PGantac%vertex(:,ivan) + (1.d0-weight(j))*PGantac%vertex(:,ivap)

        n(:) = nn(:,j)
        t(1) =  n(2)
        t(2) = -n(1)
        this(icdan)%nuc(:) = n(:)
        this(icdan)%tuc(:) = t(:)

        cdlev = this(icdan)%coor(:) + cd_shift(:) - PLcoor(1:2, icdtac)
        anlev = this(icdan)%coor(:) + an_shift(:) - PLcoor(1:2, iantac)
        ! on decale si periodique mais comme on a potentiellement
        ! permute icdtac et iantac, on fait gaffe
        if (icdtac == rough_PLPLx(i)%cd) then
           cdlev(1) = cdlev(1) - real(iprd,8)*periode
        else
           anlev(1) = anlev(1) - real(iprd,8)*periode
        endif

        this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
        this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
        this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
        this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)
                    
        this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdt3 &
                               - an_Vbegin(3)*this(icdan)%Gant3

        this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdn3 &
                               - an_Vbegin(3)*this(icdan)%Gann3
  
        this(icdan)%gapTTBEGIN  = ovlap(j)
        
        this(icdan)%rlt       = 0.D0
        this(icdan)%rln       = 0.D0
        this(icdan)%vlt       = this(icdan)%vltBEGIN
        this(icdan)%vln       = this(icdan)%vlnBEGIN
        this(icdan)%gapTT     = this(icdan)%gapTTBEGIN
        this(icdan)%status    = i_nknow

        this(icdan)%reff      = rough_PLPLx(i)%reff
        this(icdan)%meff      = rough_PLPLx(i)%meff
        
        periodic_PLPLx(icdan)   =  iprd

      enddo
    enddo
    nb_PLPLx = icdan
  endif

  write(cout,'(1X,I10,A12)') nb_PLPLx,' PLPLx found'
  call logmes(cout)

  nb_POLYG = get_nb_POLYG()   

  do ibdy=1,nb_POLYG
    if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)

    if (nb_adj(ibdy) /= 0) then
      allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      if (errare /= 0 ) then
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_PLPLx::compute_contact',cout)
      end if
    endif

  enddo 
   
  do icdan=1,nb_PLPLx
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan

    call get_behaviour_( icdan, see, tact_behav )
  end do 

  if (allocated(violation)) deallocate(violation)
  allocate(violation(nb_PLPLx),stat=errare)
 
  end subroutine compute_contact_nc_PLPLx

!------------------------------------------------------------------------

  subroutine display_prox_tactors_PLPLx

   implicit none
   integer :: iadj,itact,icdan,icdbdy,icdtac,icdver,ianbdy,iantac,ianseg,isee
   character(len=5) :: cdmodel, anmodel

   if (nb_PLPLx == 0) return
   
!fd   do itact=1,nb_POLYG    
!fd     do iadj=1,nb_adj(itact)         
!fd       icdan  = adjac(itact)%icdan(iadj)

   do icdan=1,nb_PLPLx

       icdtac = this(icdan)%icdtac
       icdver = this(icdan)%icdsci
       iantac = this(icdan)%iantac
       ianseg = this(icdan)%iansci

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyg2bdyty(3,iantac) )

       write(*,'(A1)')' '
       write(*,'(A6,2X,I5)')'$icdan',icdan
                       !123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
       write(*,'(A90)')' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr'
       write(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       anmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',icdver, &
       see(this(icdan)%isee)%behav,  &
       cdmodel,polyg2bdyty(1,iantac),'POLYG',polyg2bdyty(2,iantac),'ANSEG',ianseg
       write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       write(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
       write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBEGIN
       write(*,'(A1)')' '               

enddo

!fd     end do                           
!fd   end do

104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_PLPLx

!------------------------------------------------------------------------ 

 subroutine stock_rloc_PLPLx
 
   !  
   ! get data from this and put into verlt
   !           
 
   implicit none

   integer                               :: errare 

   integer :: icdan,icdtac,ianbdy,iantac,iadj

   integer :: nb_polyg

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PLPLx::stock_rloc'

   nb_POLYG=get_nb_POLYG()
   ! sizing verlt:
   if (.not. allocated(verlt)) then
     allocate(verlt(nb_POLYG),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'Error allocating verlt')
     end if
     do icdtac=1,nb_POLYG
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
         if (errare /=0 ) then
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         end if
       else 
         call nullify_verlet_(icdtac)
        end if
     end do
   else 
     do icdtac=1,nb_POLYG
       verlt(icdtac)%adjsz=0
       call free_verlet_(icdtac)
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
         if (errare /=0 ) then
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         end if
       else 
         call nullify_verlet_(icdtac)
       end if
     end do
   end if

  ! filling data:
   do icdan=1,nb_PLPLx

     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                       ! serial adjacent number of pair body-contactor 
                                                      ! adjacent to candidate body for contact icdan 

     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdmodel         = polyg2bdyty(3, icdtac)
     verlt(icdtac)%cdbdy           = polyg2bdyty(1, icdtac)
     verlt(icdtac)%cdtac           = polyg2bdyty(2, icdtac)
     verlt(icdtac)%cdsci(iadj)     = this(icdan)%icdsci
     verlt(icdtac)%anmodel(iadj)   = polyg2bdyty(3, iantac)
     verlt(icdtac)%anbdy(iadj)     = polyg2bdyty(1, iantac)
     verlt(icdtac)%antac(iadj)     = polyg2bdyty(2, iantac)
     verlt(icdtac)%ansci(iadj)     = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)       = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)       = this(icdan)%rln/H
     verlt(icdtac)%status(iadj)    = this(icdan)%status
     verlt(icdtac)%vlt(iadj)       = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)       = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)     = this(icdan)%gapTT
     verlt(icdtac)%tuc(1:2, iadj)  = this(icdan)%tuc(1:2)
     verlt(icdtac)%nuc(1:2, iadj)  = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2, iadj) = this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   end do

   nb_vPLPLx = nb_PLPLx

   WRITE(cout,'(1X,I10,A12)') nb_vPLPLx,' stock PLPLx'
   call logmes(cout)

 end subroutine stock_rloc_PLPLx

!------------------------------------------------------------------------ 

 subroutine recup_rloc_PLPLx

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   implicit none
   integer :: icdan,icdtac,icdver,iantac,ianseg,iadj
   logical :: found

   character(len=21) :: IAM = 'mod_PLPLx::recup_rloc'
   character(len=80) :: cout
   
   if (nb_PLPLx == 0) return  

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_PLPLx=0

   do icdan=1,nb_PLPLx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     icdver = this(icdan)%icdsci 
     iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        
     ianseg = this(icdan)%iansci

     if (verlt(icdtac)%adjsz /= 0) then
       do iadj=1,verlt(icdtac)%adjsz
         if (                                                           &
             (verlt(icdtac)%cdmodel      == polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,icdtac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,iantac)).or.   &
             (verlt(icdtac)%cdmodel      == polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,iantac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,icdtac))       &
         ) then
           if (verlt(icdtac)%cdsci(iadj) == icdver              .and.  &
               verlt(icdtac)%ansci(iadj) == ianseg                     &
          ) then
             this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
             this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
             this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

             this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
             nb_recup_PLPLx=nb_recup_PLPLx+1
             exit

           endif
         end if
       end do
     endif

   end do

   write(cout,'(1X,I10,A12)') nb_recup_PLPLx,' recup PLPLx'
   call logmes(cout)

 end subroutine recup_rloc_PLPLx
 
!------------------------------------------------------------------------ 

 subroutine recup_rloc_ByPosition_PLPLx(tol)

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   implicit none
   integer :: icdan,icdtac,icdver,iantac,ianseg,iadj,iprd
   logical :: found

   real(kind=8) :: v(2),norm1,norm2,tol
   character(len=80) :: cout

   if (nb_PLPLx == 0) return  
 
   if (.not. allocated(verlt)) then
     call faterr('mod_PLPLx::recup_rloc_ByPosition','verlt not allocated, illegal to recup')
   endif

   nb_recup_PLPLx=0
   do icdan=1,nb_PLPLx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                 ! serial number of candidate contactor for contact icdan
     icdver = this(icdan)%icdsci 
     iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        
     ianseg = this(icdan)%iansci

     found=.false.

     if (verlt(icdtac)%adjsz /= 0) then
!       print*,'on cherche parmi les verlet icd'
!       print*,polyg2bdyty(1,icdtac),polyg2bdyty(2,icdtac),polyg2bdyty(1,iantac),polyg2bdyty(2,iantac)

       do iadj=1,verlt(icdtac)%adjsz
!         print*,verlt(icdtac)%cdbdy(iadj),verlt(icdtac)%cdtac(iadj),verlt(icdtac)%anbdy(iadj), verlt(icdtac)%antac(iadj)
         if ((verlt(icdtac)%cdmodel      == polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,icdtac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,iantac))       &
              .or.                                                      &
             (verlt(icdtac)%cdmodel      == polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,iantac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,icdtac))       &
            ) then
           iprd   = periodic_PLPLx(icdan)
           v(1:2) = verlt(icdtac)%coor(1:2,iadj) - (this(icdan)%coor(1:2)) 
           norm   = dsqrt((v(1)*v(1)) + (v(2)*v(2)))

           if (( norm < tol ).OR.(periodic .and. abs((periode-norm))<tol)) then
             this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
             this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
             this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

             this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
             nb_recup_PLPLx=nb_recup_PLPLx+1
             found = .true.
!             print*,'found'
             exit
           else
!             print*,'too far',norm
           endif
         end if
       end do
     end if
     if (verlt(iantac)%adjsz /= 0) then
!       print*,'on cherche parmi les verlet ian'
       do iadj=1,verlt(iantac)%adjsz
         if ((verlt(iantac)%cdmodel      == polyg2bdyty(3,icdtac) .and.  &
              verlt(iantac)%cdbdy        == polyg2bdyty(1,icdtac) .and.  &
              verlt(iantac)%cdtac        == polyg2bdyty(2,icdtac) .and.  &
              verlt(iantac)%anmodel(iadj)== polyg2bdyty(3,iantac) .and.  &
              verlt(iantac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and.  &
              verlt(iantac)%antac(iadj)  == polyg2bdyty(2,iantac))       &
              .or.                                                      &
             (verlt(iantac)%cdmodel      == polyg2bdyty(3,iantac) .and.  &
              verlt(iantac)%cdbdy        == polyg2bdyty(1,iantac) .and.  &
              verlt(iantac)%cdtac        == polyg2bdyty(2,iantac) .and.  &
              verlt(iantac)%anmodel(iadj)== polyg2bdyty(3,icdtac) .and.  &
              verlt(iantac)%anbdy(iadj)  == polyg2bdyty(1,icdtac) .and.  &
              verlt(iantac)%antac(iadj)  == polyg2bdyty(2,icdtac))       &
            ) then
           iprd = periodic_PLPLx(icdan)
           v(1:2) = verlt(iantac)%coor(1:2,iadj) - (this(icdan)%coor(1:2)) 
           norm = dsqrt((v(1)*v(1)) + (v(2)*v(2)))
           if (( norm < tol ).OR.(periodic .and. abs((periode-norm))<tol)) then
             this(icdan)%rlt    = verlt(iantac)%rlt(iadj)*H
             this(icdan)%rln    = verlt(iantac)%rln(iadj)*H
             this(icdan)%statusBEGIN = verlt(iantac)%status(iadj)

             this(icdan)%internal(1:max_internal_tact)=verlt(iantac)%internal(1:max_internal_tact,iadj)
             nb_recup_PLPLx=nb_recup_PLPLx+1
             found = .true.
             exit
           else
!             print*,'too far',norm
           endif
         end if
       end do
     endif

!     if (.not. found) then
!       print*,icdan,'orphelin'
!       print*,this(icdan)%icdtac,this(icdan)%iantac 
!       print*,this(icdan)%coor(1:2)
!     endif
   end do

   write(cout,'(1X,I10,A12)') nb_recup_PLPLx,' recup PLPLx'
   call logmes(cout)

 end subroutine recup_rloc_ByPosition_PLPLx

!------------------------------------------------------------------------ 

 subroutine read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   implicit none

   integer(kind=4)                   :: icdan,icdbdy,icdtac,icdver,ianbdy,iantac,ianseg
   integer(kind=4)                   :: iadj,icdtact,cdmodel,anmodel
   real(kind=8)                      :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)         :: nuc,coor
   character(len=5)                  :: cdbdy,cdtac,cdver,anbdy,antac,anseg,behav,sttus

   integer                           :: errare 
   
   integer :: ibehav,nb_internal,i_internal
   integer :: nb_polyg

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_PLPLx::read_ini_Vloc_Rloc'


   nb_POLYG=get_nb_POLYG()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contacts have to be selected.  
  ! For this purpose nb_adj is introduced.
   if(allocated(nb_adj)) deallocate(nb_adj)
   if (.not. allocated(nb_adj)) then
     allocate(nb_adj(nb_POLYG),stat=errare)
     if (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   end if
    
   nb_adj=0

   do
     if ( .not. read_G_clin()) exit
     if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
     if (G_clin(9:13)/= 'PLPLx') cycle     
     if ( .not. read_G_clin()) exit
     if ( .not. read_G_clin()) exit
     read(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,anseg,ianseg,  &
                      sttus
     if (cdtac /= 'POLYG' .or. antac /= 'POLYG') cycle

     cdmodel = get_body_model_id_from_name( cdbdy )

     do icdtact=1,nb_POLYG
       if (polyg2bdyty(3,icdtact) == cdmodel .and. &
           polyg2bdyty(1,icdtact) == icdbdy  .and. &
           polyg2bdyty(2,icdtact) == icdtac  ) then
         nb_adj(icdtact)=nb_adj(icdtact)+1       
         exit
       end if
     end do
     cycle
   end do   
   if(allocated(verlt)) deallocate(verlt)
   if (.not. allocated(verlt)) then
     allocate(verlt(nb_POLYG),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'Error allocating verlt')
     end if
     do icdtac=1,nb_POLYG
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       if (iadj > 0) then
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
         if (errare /=0 ) then
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         end if
       else 
         call nullify_verlet_(icdtac)
       end if
     end do
   else 
     do icdtac=1,nb_POLYG
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
     if (G_clin(9:13)/= 'PLPLx') cycle     
     if ( .not. read_G_clin()) exit
     if ( .not. read_G_clin()) exit

     read(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,anseg,ianseg,  &
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )

     if (cdtac /= 'POLYG' .and. antac /= 'POLYG') cycle
     do icdtact=1,nb_POLYG
       if (polyg2bdyty(3,icdtact) == cdmodel .and. &
           polyg2bdyty(1,icdtact) == icdbdy  .and. &
           polyg2bdyty(2,icdtact) == icdtac  ) then

         icdan = icdan + 1

         nb_adj(icdtact)=nb_adj(icdtact)+1 

         verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%cdsci(nb_adj(icdtact))  = icdver
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
         verlt(icdtact)%ansci(nb_adj(icdtact))  = ianseg
         verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)

         if( .not. read_G_clin()) exit
         read(G_clin(1:90),'(27X,2(7X,D14.7))')rlt,rln
         verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
         verlt(icdtact)%rln(nb_adj(icdtact))=rln
         if( .not. read_G_clin()) exit 
         read(G_clin(1:90),'(27X,2(7X,D14.7))')vlt,vln
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         if( .not. read_G_clin()) exit 
         read(G_clin(1:90),'(27X,2(7X,D14.7))')gapTT
         verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
         if( .not. read_G_clin()) exit
         if (G_clin(30:34)== 'n(1)=') then
           read(G_clin(1:90),'(27X,2(7X,D14.7))')nuc(1),nuc(2)
           verlt(icdtact)%nuc(1,nb_adj(icdtact))=nuc(1)
           verlt(icdtact)%nuc(2,nb_adj(icdtact))=nuc(2)
         else 
           backspace(G_nfich)
         end if
         if( .not. read_G_clin()) exit
         if (G_clin(30:34)== 'coo1=') then
           read(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
           verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
           verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)
         else 
           backspace(G_nfich)
         end if

         verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
         ibehav = get_ibehav(behav)
         nb_internal = get_nb_internal(ibehav)

         if (nb_internal /= 0 ) then  
           if( .not. read_G_clin()) exit
           do i_internal=1, nb_internal
             read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
           enddo
         endif

         exit 
       end if
     enddo
     cycle
   end do   

   nb_vPLPLx=0

   do icdtact=1,nb_POLYG
     nb_vPLPLx = nb_vPLPLx + nb_adj(icdtact)

     if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
       write(cout,'(A,1x,I0)')   'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')   'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     endif

   end do

end subroutine read_ini_Vloc_Rloc
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 subroutine write_out_Vloc_Rloc(nfich)

  !
  ! write into file out_Vloc_Rloc data from this, in verlt style
  !

   implicit none
   integer :: iadj,icdan,icdbdy,icdtac,icdver,ianbdy,iantac,ianseg,isee,nfich,icdtact
   integer :: lc
   real(kind=8),dimension(2) :: coor
   integer :: nb_polyg
   character(len=5) :: cdmodel, anmodel

   character(len=20) :: fmt
   
   if (nb_PLPLx==0) return

   nb_POLYG=get_nb_POLYG()

!fd   do icdtact=1,nb_POLYG   
!fd     do iadj=1,nb_adj(icdtact)         
!fd       icdan  = adjac(icdtact)%icdan(iadj)
 
  do icdan=1,nb_PLPLx

       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyg2bdyty(3,iantac) )

       write(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PLPLx',icdan
                           !1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
       write(nfich,'(A90)')' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr  sttus  iadj'
       write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,1X,I5)')   &
       cdmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',this(icdan)%icdsci, &
       see(this(icdan)%isee)%behav,  &
       anmodel,polyg2bdyty(1,iantac),'POLYG',polyg2bdyty(2,iantac),'ANSEG',this(icdan)%iansci, &
       get_contact_status_name_from_id(this(icdan)%status),iantac
       write(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',0.D0
       write(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',0.D0
       write(nfich,103)'gapTT',this(icdan)%gapTT 
       write(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
       write(nfich,104)'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',0.D0

       if (this(icdan)%nb_internal /= 0) then
         call write_internal_comment(nfich,this(icdan)%lawnb)
         write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
         write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
       endif

       write(nfich,'(A1)')' '               

  enddo

!fd     end do                           
!fd   end do

103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine write_out_Vloc_Rloc
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 subroutine nullify_reac_PLPLx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
    
   icdtac=this(icdan)%icdtac
   call nullify_reac_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_reac_POLYG(iantac,storage)
    
 end subroutine nullify_reac_PLPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine nullify_vlocy_PLPLx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
    
   icdtac=this(icdan)%icdtac
   call nullify_vlocy_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_vlocy_POLYG(iantac,storage)
    
 end subroutine nullify_vlocy_PLPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine vitrad_PLPLx( icdan, storage, need_full_V )

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
   logical, optional  :: need_full_V
    
  icdtac=this(icdan)%icdtac
  call comp_vlocy_POLYG(icdtac,storage)
    
  iantac=this(icdan)%iantac
  call comp_vlocy_POLYG(iantac,storage)
    
 end subroutine vitrad_PLPLx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 subroutine injj_PLPLx(icdan,RTIK,RNIK,storage)
 
   implicit none
   integer     ,intent(in)    :: icdan
   real(kind=8),intent(in)    :: RTIK,RNIK
   integer,     dimension(3)  :: cdccdof,anccdof
   real(kind=8),dimension(3)  :: cdreac, anreac
   integer                    :: storage
   
   cdccdof(1)=1
   anccdof(1)=1
   cdreac(1)= RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)
   cdccdof(2)=2
   anccdof(2)=2
   cdreac(2)= RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)=-cdreac(2)
   cdccdof(3)=3
   anccdof(3)=3
   cdreac(3)= this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK
   anreac(3)=-this(icdan)%Gant3*RTIK-this(icdan)%Gann3*RNIK

   call add_reac_POLYG(this(icdan)%icdtac,cdccdof,cdreac,storage)
   call add_reac_POLYG(this(icdan)%iantac,anccdof,anreac,storage)

 end subroutine injj_PLPLx 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
subroutine prjj_PLPLx(icdan,VTIK,VNIK,storage)
 
   implicit none
   integer     ,intent(in)   :: icdan
   real(kind=8),intent(out)  :: VTIK,VNIK
   real(kind=8),dimension(3) :: Vcd,Van

   integer(kind=4)             :: icdbdy,ianbdy
   integer(kind=4), intent(in) :: storage
   
   real(kind=8),dimension(2) :: Vdcd,Vdan,Vd

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   call get_vlocy_POLYG(this(icdan)%icdtac,storage,Vcd)
   call get_vlocy_POLYG(this(icdan)%iantac,storage,Van)   

   if (storage == iVfree) then
     call get_Vd_POLYG(icdbdy,this(icdan)%icdcoor,Vdcd)
     call get_Vd_POLYG(ianbdy,this(icdan)%iancoor,Vdan)
     
     Vd=Vdcd-Vdan
   else
     Vd=0.d0
   endif

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3 &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3 &
        +Vd(1)*this(icdan)%tuc(1)+Vd(2)*this(icdan)%tuc(2)

   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3 &
        +Vd(1)*this(icdan)%nuc(1)+Vd(2)*this(icdan)%nuc(2)

end subroutine prjj_PLPLx 
!!$!------------------------------------------------------------------------ 
!!$subroutine compute_Wikik_PLPLx(icdan,WTT,WTN,WNT,WNN)
!!$
!!$  implicit none
!!$  integer                   :: icdan,icdbdy,ianbdy
!!$  real(kind=8)              :: WTT,WTN,WNT,WNN
!!$  real(kind=8),dimension(3) :: icdmass,ianmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$
!!$  icdmass = get_inv_mass_POLYG(icdbdy)
!!$  ianmass = get_inv_mass_POLYG(ianbdy)
!!$
!!$  WTT =  icdmass(1)+icdmass(3)*this(icdan)%Gcdt3*this(icdan)%Gcdt3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gant3*this(icdan)%Gant3
!!$  WNN =  icdmass(1)+icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdn3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gann3*this(icdan)%Gann3
!!$  WTN =  icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdt3 &
!!$       + ianmass(3)*this(icdan)%Gann3*this(icdan)%Gant3
!!$
!!$  WNT = WTN
!!$
!!$ end subroutine compute_Wikik_PLPLx
!!$!------------------------------------------------------------------------ 
!!$ subroutine get_Wik_PLPLx(icdan,ikcd,ikan,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
!!$
!!$  implicit none
!!$  integer                   :: icdan,ikcd,ikan
!!$  real(kind=8)              :: ikGcdt,ikGcdn,ikGant,ikGann
!!$  real(kind=8),dimension(3) :: ikcdmass,ikanmass
!!$  real(kind=8),dimension(2) :: tik,nik
!!$
!!$  ikcd    = this(icdan)%icdbdy
!!$  ikan    = this(icdan)%ianbdy
!!$  ikGcdt  = this(icdan)%Gcdt3
!!$  ikGcdn  = this(icdan)%Gcdn3
!!$  ikGant  = this(icdan)%Gant3
!!$  ikGann  = this(icdan)%Gann3
!!$  ikcdmass= get_inv_mass_POLYG(ikcd)
!!$  ikanmass= get_inv_mass_POLYG(ikan)
!!$  tik     = this(icdan)%tuc
!!$  nik     = this(icdan)%nuc
!!$
!!$ end subroutine get_Wik_PLPLx
!!$!------------------------------------------------------------------------ 
!!$ subroutine compute_Wikjl_PLPLx(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$  implicit none
!!$  integer                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy,janbdy
!!$  real(kind=8)              :: WTT,WTN,WNT,WNN
!!$  real(kind=8),dimension(3) :: icdmass,ianmass,jcdmass,janmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$  jcdbdy=this(jcdan)%icdbdy
!!$  janbdy=this(jcdan)%ianbdy
!!$
!!$  icdmass = get_inv_mass_POLYG(icdbdy)
!!$  ianmass = get_inv_mass_POLYG(ianbdy)
!!$  jcdmass = get_inv_mass_POLYG(jcdbdy)
!!$  janmass = get_inv_mass_POLYG(janbdy)
!!$
!!$!cas ij-ik
!!$  if (icdbdy == jcdbdy) then
!!$
!!$    WTT =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdt3
!!$
!!$    WTN =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdn3
!!$
!!$    WNT =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdt3
!!$
!!$    WNN =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdn3
!!$!cas ij-kj
!!$  elseif (ianbdy == janbdy) then
!!$
!!$    WTT =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gant3
!!$
!!$    WTN =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gann3
!!$
!!$    WNT =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gant3
!!$
!!$    WNN =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gann3
!!$!cas ij-jk
!!$  elseif (ianbdy == jcdbdy) then
!!$
!!$    WTT = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdt3
!!$
!!$    WTN = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdn3
!!$
!!$    WNT = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdt3
!!$
!!$    WNN = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdn3
!!$  else
!!$!cas ij-ki
!!$    WTT = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gant3
!!$
!!$    WTN = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gann3
!!$
!!$    WNT = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gant3
!!$
!!$    WNN = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gann3
!!$  endif
!!$
!!$ end subroutine compute_Wikjl_PLPLx
!------------------------------------------------------------------------ 
 subroutine compute_stress_PLPLx
   implicit none
   integer(kind=4)             :: icdan,ID_RBDY2,ID_TACTY
   integer(kind=4)             :: icdbdy,ianbdy,icdtac,iantac
   real(kind=8),dimension(2)   :: Fik,Lcd,Lan,coor
   real(kind=8),dimension(2,2) :: SIGMA
   real(kind=8),dimension(3)   :: coorTT

   do icdan=1,nb_PLPLx

      Fik = this(icdan)%rln*this(icdan)%nuc + this(icdan)%rlt*this(icdan)%tuc
      
      icdtac = this(icdan)%icdtac
      
      ID_RBDY2 = polyg2bdyty(1,icdtac)
      ID_TACTY = polyg2bdyty(2,icdtac)

      coorTT = get_coorTT_POLYG(icdtac)
      coor   = coorTT(1:2) - get_shiftTT_POLYG(icdtac)

      Lcd = coor - this(icdan)%coor

      SIGMA(1,1:2) = Lcd(1)*Fik(1:2)
      SIGMA(2,1:2) = Lcd(2)*Fik(1:2)

      call add_stress_POLYG(ID_RBDY2,SIGMA)

      iantac = this(icdan)%iantac

      ID_RBDY2 = polyg2bdyty(1,iantac)
      ID_TACTY = polyg2bdyty(2,iantac)

      coorTT = get_coorTT_POLYG(iantac)
      coor   = coorTT(1:2) - get_shiftTT_POLYG(iantac)

      Lan = coor - this(icdan)%coor

      SIGMA(1,1:2) =-Lan(1)*Fik(1:2)
      SIGMA(2,1:2) =-Lan(2)*Fik(1:2)

      call add_stress_POLYG(ID_RBDY2,SIGMA)

   end do

 end subroutine compute_stress_PLPLx
!------------------------------------------------------------------------ 
 integer function get_nb_PLPLx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_PLPLx = nb_PLPLx
   case(i_verlet_tactor)
      get_nb_PLPLx = nb_vPLPLx
   case(i_rough_tactor)
      get_nb_PLPLx = nb_rough_PLPLx
   case(i_recup_tactor)
      get_nb_PLPLx = nb_recup_PLPLx
   end select

 end function get_nb_PLPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
subroutine PLPLx2POLYG(icdan,icdtac,iantac)

   implicit none
   integer :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

end subroutine PLPLx2POLYG
!------------------------------------------------------------------------ 
!--------------------------------------------------------------------------------------
!  SUBROUTINE DETECT:
!  Evaluation du contact éventuel entre deux polygones et dans un cas de contact
!  renvoie un ou deux points de contact, le gap, le numéro du polygone
!  candidat et le numéro de l'antagoniste.
!
!  routine de detection par la methode du shadow overlap de JJ. MOREAU
!
!  on cherche le contact entre 2 polygones facetises. 
!
!  en entree: PGa,PGb       : polygones a,b
!              Xa,Xb        : deplacements de a,b
!            numa,numb      : numeros des contacteurs concernes
!            
!  en sortie: icdtac,iantac : numero du contacteur candidat,antagoniste
!             icdver,ianver : numero du/des vertex acteurs, concerne
!             xco           : les coordonnees des points d'application des contacts
!             ovlap         : les gap (distance signée entre le vertex candidat 
!                             et le segment antagoniste)
!             n             : la normale sortante
!
! principe grossier de l'algo:
!
!  0/ les sommets des vertex du polygone 
!     sont dans leur position reelle. Les normales
!     sont par rapport a la configuration actuelle.
!
!     Les vertex sont ranges dans le sens trigo de 1 a nb_vertex,
!     et donc les arretes vont de 1 a nb_vertex.
!
!  1/ on determine les vertex qui se recouvre par shadow overlap.
!     on place les 2 polygones par rapport a leur ligne des centres.
! 
!  2/ partant des infos obtenues au 1/ on va chercher a trouver le 
!     meilleur couple "vertex-candidat" "segment-antagoniste". 
!     c'est un processus iteratif, on cherche qui des 2 corps 
!     contient le noeud qui traverse un segment de l'autre.
!
!     0- on initialise en prenant comme segment celui dont la
!        normale est la plus alignée avec la ligne des centres. 
!
!     i- on calcule le gap du vertex par rapport a ce segment.
!        si il est positif le vertex est bien du bon cote de la facette
!        si le vertex peut se projeter sur le segment suivant on recommence
!        le test sur ce nouveau segment
!
!  3/ si plusieurs noeuds on en fait traverse on cherche un second meilleur
!
!  4/ on remonte les infos ...
!
!--------------------------------------------------------------------------------------
 subroutine detect(PGa,PGb,numa,numb, &
                  icdtac,iantac,icdver,ianver, &
                  xco,n,ovlap,sep,adist)

   implicit none

   type(T_POLYG)               :: PGa,PGb             ! corps a et b notation JJM
   real(kind=8),dimension(3)   :: coorda,coordb       ! Coordonnées des centres des corps a et b
   integer                     :: numa,numb,icdtac,iantac,ianver
   integer,dimension(2)        :: icdver
   real(kind=8),dimension(2)   :: ovlap               ! overlap pour le point 1 et éventuellement 2
   real(kind=8),dimension(2,2) :: xco                 ! coordonnées des points de contacts
   integer                     :: nb_vertex_PGa,nb_vertex_PGb 
  
   real(kind=8)                :: scal,base1,base2,dist1,dist2,abs1,abs2,scal1
   real(kind=8)                :: proda,prodb,prodi   ! valeurs des projections sur la ligne des centres sep
   real(kind=8)                :: norm,over0          ! valeur de la norme de sep et valeur de l'overlap
   integer                     :: sens,rang           ! sens pour le parcours des sommets des polygones et 
                                                      ! rang=0: 1 pt de contact
                                                      ! rang=1 deux points de contact
   integer                     :: preceda,crita,suiva
   integer                     :: precedb,critb,suivb,num_vertex  ! variables pour parcourir les sommets
   integer                     :: obj                 ! obj=-1: le corps B est actif | obj=1: le corps A est actif
   integer                     :: iverta,ivertb,i,crita0,critb0
   real(kind=8),dimension(2)   :: tgn,del,sep0
   real(kind=8),dimension(2)   :: xco1,xco2           ! coordonnées points de contact
   real(kind=8),dimension(2)   :: sep                 ! vecteur separateur   
   real(kind=8),dimension(2)   :: n                   ! normale du contact
   real(kind=8),dimension(2)   :: centre              ! coordonnées du milieu des centres actifs

   real(kind=8)   :: adist

   icdtac = 0
   iantac = 0
   icdver = 0
   ianver = 0
   ovlap  = 0.d0
   xco    = 0.d0

   nb_vertex_PGa=PGa%nb_vertex
   nb_vertex_PGb=PGb%nb_vertex
   rang=0
   xco1=0.d0;xco2=0.d0;
   abs1=0.d0;abs2=0.d0;base1=0.d0;base2=0.d0

   proda=-1.d+20
   prodb= 1.d+20

!------------------------------------------------------------------------------------------
! Recherche des sommets critiques par projection sur la ligne des centres
! en fait le vecteur sep et determination de l'overlap, traduisent
! le recoupement eventuel des projections
!------------------------------------------------------------------------------------------

   do iverta=1,nb_vertex_PGa
     prodi=  sep(1)*PGa%vertex(1,iverta) &
           + sep(2)*PGa%vertex(2,iverta) 

     if ( prodi > proda ) then
       crita=iverta
       proda=prodi
     endif
   enddo
   !    
   do ivertb=1,nb_vertex_PGb
     prodi= sep(1)*PGb%vertex(1,ivertb) &
          + sep(2)*PGb%vertex(2,ivertb)

     if ( prodi < prodb ) then
       critb=ivertb
       prodb=prodi
     endif
   enddo

   over0=proda-prodb
   
   ovlap(1)=over0   

!------------------------------------------------------------------------------------------
! Si l'overlap est negatif les projections des sommets des deux polygones ne se recoupent
! pas dc on est sur qu'il n'y a pas contact. Dans le cas contraire il faut faire une
! analyse plus fine.
!------------------------------------------------------------------------------------------

   !fd ici on teste -over0 > adist
   if (over0 + adist < 0.d0) return

!------------------------------------------------------------------------------------------
! Determination des rotations du vecteur reliant les centres possibles pour diminuer
! l'overlap et eventuellement le rendre negatif. Si aucune position trouvee
! alors il y a contact
!------------------------------------------------------------------------------------------

   crita0=crita; critb0=critb; sep0=sep

   del=PGa%vertex(1:2,crita)-PGb%vertex(1:2,critb)

   sens=-1
   if (sep(1)*del(2)-sep(2)*del(1) > 0.d0) sens=1

   if (sens == 1) then

     do
       !on retient la moins ample des deux rotations negatives possible
       if (crita==1) then
         preceda=nb_vertex_PGa
       else
         preceda=crita-1
       endif      
       if (critb==1) then
         precedb=nb_vertex_PGb
       else
         precedb=critb-1
       endif
       scal= sep(1)*PGa%normale(1,preceda)+sep(2)*PGa%normale(2,preceda) 
       scal1=sep(1)*PGb%normale(1,precedb)+sep(2)*PGb%normale(2,precedb)
       if (scal > -scal1) then
         sep=PGa%normale(:,preceda)
         crita=preceda
         obj=1
       else
         sep=-PGb%normale(:,precedb)
         critb=precedb
         obj=-1
       endif

       ! on actualise le segment actif
       del=PGa%vertex(1:2,crita)-PGb%vertex(1:2,critb)

       over0=sep(1)*del(1)+sep(2)*del(2)

       if (sep(1)*del(2)-sep(2)*del(1) < 0.d0 ) exit ! on sort de la boucle

     enddo
     
   else

     ! determination de la plus petite des deux rotations possibles
     do
       scal= sep(1)*PGa%normale(1,crita)+sep(2)*PGa%normale(2,crita)
       scal1=sep(1)*PGb%normale(1,critb)+sep(2)*PGb%normale(2,critb)
       if (scal>-scal1) then
         sep=PGa%normale(:,crita)
         if (crita==nb_vertex_PGa) then
           crita=1
         else
           crita=crita+1
         endif
         
         obj=1
       else
         sep=-PGb%normale(:,critb)
         if (critb==nb_vertex_PGb) then
           critb=1
         else
           critb=critb+1
         endif        
         obj=-1
       endif      
       ! on actualise le segment actif
       del=PGa%vertex(1:2,crita)-PGb%vertex(1:2,critb) 

       over0=sep(1)*del(1)+sep(2)*del(2)

       if (sep(1)*del(2)-sep(2)*del(1) > 0.d0 ) exit ! on sort de la boucle

      enddo                    

   endif

   ovlap(1)=over0

   if (over0 + adist <= 0.d0) return ! on sort de la routine

!--------------------------------------------------------------------------------------------
! si en faisant tourner le vecteur des centres sep, on a trouve une direction pour
! laquelle l'overlap est négatif alors il n' y a pas contact. Dans le cas contraire
! il y a contact est on va determiner le ou les points de contact
!--------------------------------------------------------------------------------------------

   centre=0.5*(PGa%vertex(1:2,crita)+PGb%vertex(1:2,critb))
   n=sep;

   if (obj==-1) then        ! B est actif
     ! determination du point principal
     ! il est le remonté d'un demi over0
     xco1=PGa%vertex(1:2,crita)-0.5*over0*sep
   
     icdtac=numa
     iantac=numb

     if (sens == 1) then
       ianver=critb
     else 
       if (critb==1) then
         ianver=nb_vertex_PGb
       else
         ianver=critb-1
       endif

     endif

     icdver(1)=crita   
     icdver(2)=0
     
     !voir si d'autres sommets de A (cand) sont en contact

     if (crita==1) then
       preceda=nb_vertex_PGa
     else
       preceda=crita-1
     endif
     dist1=(PGa%vertex(1,preceda)-centre(1))*sep(1) &
          +(PGa%vertex(2,preceda)-centre(2))*sep(2)
     if (dist1+adist>0.d0) then
       num_vertex=preceda 
       rang=1
     endif
     
     if (crita==nb_vertex_PGa) then
       suiva=1
     else
       suiva=crita+1
     endif
     dist2=(PGa%vertex(1,suiva)-centre(1))*sep(1) &
          +(PGa%vertex(2,suiva)-centre(2))*sep(2)
     if ((dist2+adist>0.d0).and.(dist2>dist1)) then

       dist1=dist2
       num_vertex=suiva
       rang=1
      
     endif

!---------------------------------------------------------------------------------------------------------------
! calcul du second point xco2: on prend la tangente a la normale n, et on evalue la position de xco2
! par rapport au cote en contact. Si celui-ci est en dehors de ce segment alors il ft reajuster
! sa position au sommet du corps en contact
!             __________
!     ________|___1_____|xco2
!    |____________| on réajuste xco2 sur 1 par exemple
!---------------------------------------------------------------------------------------------------------------

     if (rang==1) then
       xco2=PGa%vertex(1:2,num_vertex)-dist1*sep
       icdver(2)=num_vertex
       ovlap(2)=(PGa%vertex(1,num_vertex)-PGb%vertex(1,critb))*sep(1) &
               +(PGa%vertex(2,num_vertex)-PGb%vertex(2,critb))*sep(2)
       tgn(1)=-sep(2)
       tgn(2)=sep(1)
       abs2=(xco2(1)-centre(1))*tgn(1)+(xco2(2)-centre(2))*tgn(2)
       if (sens == 1) then
         if (critb==nb_vertex_PGb) then
           suivb=1
         else
           suivb=critb+1
         endif
         base1= (PGb%vertex(1,suivb)-centre(1))*tgn(1) &
               +(PGb%vertex(2,suivb)-centre(2))*tgn(2)
         base2= (PGb%vertex(1,critb)-centre(1))*tgn(1) &
               +(PGb%vertex(2,critb)-centre(2))*tgn(2)
       else  
         base1= (PGb%vertex(1,critb)-centre(1))*tgn(1) &
               +(PGb%vertex(2,critb)-centre(2))*tgn(2)
         if (critb==1) then
           precedb=nb_vertex_PGb
         else
           precedb=critb-1
         endif
         base2= (PGb%vertex(1,precedb)-centre(1))*tgn(1) &
               +(PGb%vertex(2,precedb)-centre(2))*tgn(2)
       endif
       if (abs2 > base2) abs2=base2
       if (abs2 < base1) abs2=base1
       xco2=centre+abs2*tgn
     endif

   endif                  

   if (obj==1) then        ! A est actif
                           ! determination du point principal
     xco1=PGb%vertex(1:2,critb)+0.5*over0*sep
     !voir si des sommets de B (anta) sont en contact

     icdtac=numb
     iantac=numa

     if (sens == 1) then
       ianver=crita
     else 
       if (crita==1) then
         ianver=nb_vertex_PGa
       else
         ianver=crita-1
       endif
     endif

     icdver(1)=critb   
     icdver(2)=0

     if (critb==1) then
       precedb=nb_vertex_PGb
     else
       precedb=critb-1
     endif


     dist1= (PGb%vertex(1,precedb)-centre(1))*sep(1) &
           +(PGb%vertex(2,precedb)-centre(2))*sep(2)
     if (dist1-adist<0.d0) then
       num_vertex=precedb 
       rang=1
     endif
        if (critb==nb_vertex_PGb) then
          suivb=1
         else
          suivb=critb+1
        endif
     dist2= (PGb%vertex(1,suivb)-centre(1))*sep(1) &
           +(PGb%vertex(2,suivb)-centre(2))*sep(2)
     if ((dist2-adist<0.d0).and.(dist2<dist1)) then
       num_vertex=suivb 
       dist1=dist2
       rang=1
     endif

!---------------------------------------------------------------------------------------------------------------
! calcul du second point xco2: on prend la tangente a la normale n, et on evalue la position de xco2
! par rapport au cote en contact. Si celui-ci est en dehors de ce segment alors il ft reajuster
! sa position au sommet du corps en contact
!             __________
!     ________|___1_____|xco2
!    |____________| on reajuste xco2 sur 1 par exemple
!---------------------------------------------------------------------------------------------------------------

     if (rang==1) then
       xco2=PGb%vertex(1:2,num_vertex)-dist1*sep
       ovlap(2)=-(PGb%vertex(1,num_vertex)-PGa%vertex(1,crita))*sep(1) &
               - (PGb%vertex(2,num_vertex)-PGa%vertex(2,crita))*sep(2)
       icdver(2)=num_vertex
       tgn(1)=-sep(2)
       tgn(2)=sep(1)
       abs2=(xco2(1)-centre(1))*tgn(1) +(xco2(2)-centre(2))*tgn(2)
       if (sens == 1) then
         base1=(PGa%vertex(1,crita)-centre(1))*tgn(1) &
              +(PGa%vertex(2,crita)-centre(2))*tgn(2)
         if (crita==nb_vertex_PGa) then
           suiva=1
         else
           suiva=crita+1
         endif
         base2=(PGa%vertex(1,suiva)-centre(1))*tgn(1) &
              +(PGa%vertex(2,suiva)-centre(2))*tgn(2)
       else  
         if (crita==1) then
           preceda=nb_vertex_PGa
         else
           preceda=crita-1
         endif
         base1=(PGa%vertex(1,preceda)-centre(1))*tgn(1) &
              +(PGa%vertex(2,preceda)-centre(2))*tgn(2)
         base2=(PGa%vertex(1,crita)-centre(1))*tgn(1) &
              +(PGa%vertex(2,crita)-centre(2))*tgn(2)
       endif
       if (abs2 > base2) abs2=base2
       if (abs2 < base1) abs2=base1

       xco2=centre+abs2*tgn

    endif

   endif 
   
   xco(:,1)=xco1
   xco(:,2)=xco2
 
 end subroutine detect
 
 
 
!--------------------------------------------------------------------------------------
!  SUBROUTINE detection_non_convex:
!  Evaluation du contact éventuel entre deux polygones et renvoie une liste 
!  de points de contact, de gap, de normales et d'info pour positionner ces points de contact.
!
!  on cherche le contact entre 2 polygones facetises. 
!
!  en entree: icdtac,iantac       : les indices des polygones candidat et antagoniste
!             adist               : la distance d'alerte
!            
!  en sortie: contatc           : true ou false si il y a au moins un contact
!             nbv               : la taille des listes ci-dessous
!             status            : O=pas de contact, 1=contact vertex/vertex, 2=contact vertex/segment
!             r_icdtac,r_iantac : les indices des tact candidat et antagoniste
!             icdver,ianseg     : les indices des candidat_vertex et antagoniste_segement
!             weight            : unn coeff pour reperer la position du contact sur le segment ianseg
!             n                 : les normales au contact
!             ovlap             : les gap au contact
!--------------------------------------------------------------------------------------
 subroutine detect_non_convex(icdtac,iantac,adist,gdist,contact,nbv,status,r_icdtac,r_iantac,icdver,ianseg,weight,n,ovlap)

   implicit none

   integer                             :: icdtac,iantac
   real(kind=8)                        :: adist,gdist
   real(kind=8),dimension(:),pointer   :: ovlap
   real(kind=8),dimension(:),pointer   :: weight
   REAL(kind=8),DIMENSION(:,:),pointer :: n
   integer,DIMENSION(:),pointer        :: status,icdver,ianseg
   integer,DIMENSION(:),pointer        :: r_icdtac,r_iantac
  
   type(T_POLYG)                       :: PGa,PGb             ! corps a et b notation JJM
   integer                     :: nbv,nbva,nbvb,iv,iva,ivb,check,iseg
   REAL(kind=8)                :: gap,ww,cd_coor(2),nn(2),cd_norm(2)
   logical                     :: contact

   ! nombre de points de contact
   contact = .false.
   ! les 2 POLYG impliques
   PGa = get_l_POLYG(icdtac)
   PGb = get_l_POLYG(iantac)

   nbva = PGa%nb_vertex
   nbvb = PGb%nb_vertex
   nbv  = nbva + nbvb
   allocate(status(nbv),r_icdtac(nbv),r_iantac(nbv),ovlap(nbv),icdver(nbv),ianseg(nbv),weight(nbv),n(2,nbv))
   status   = 0
   r_icdtac = 0
   r_iantac = 0
   ovlap    = 0.d0
   icdver   = 0
   ianseg   = 0
   weight   = 0.d0
   n        = 0.d0

   ! parcourt les vertex du PGa
   ! et cherche les contacts avec PGb
   do iv=1,nbva
   
     cd_coor(:) = PGa%vertex(:,iv)

     !print*,'< '
     !print*,'noeud ',iv,' du corps ',icdtac

     ! normale moyenne au vertex candidat
     cd_norm(:) = 0.5*( PGa%normale(:,modulo(iv-2,PGa%nb_vertex)+1) + PGa%normale(:,iv) )

     
     check = node_POLYG_proximity(PGb,cd_coor,cd_norm,gdist,gap,iseg,ww,nn)
     
     !if (check > 0) then
     !  print*,check, gap, adist, abs(gap) - adist
     !else
     !  print*,check 
     !endif
     !print*,'> '     

     !if (check > 0 .and. abs(gap) < adist) then
     if (check > 0 .and. gap < adist) then        
        contact      = .true.
        status(iv)   = check
        r_icdtac(iv) = icdtac
        r_iantac(iv) = iantac
        ! info on the location of contact point
        icdver(iv)   = iv
        ianseg(iv)   = iseg
        weight(iv)   = ww
        ovlap(iv)    = gap
        n(:,iv)      = nn(:)
     endif
     
   enddo
 
   ! parcourt les vertex du PGb
   ! et cherche les contacts avec PGa
   do iv=1,nbvb

     cd_coor(:) = PGb%vertex(:,iv)
     
     !print*,'< '
     !print*,'noeud ',iv,' du corps ',iantac

     ! normale moyenne au vertex candidat
     cd_norm(:) = 0.5*( PGb%normale(:,modulo(iv-2,PGb%nb_vertex)+1) + PGb%normale(:,iv) )

     check = node_POLYG_proximity(PGa,cd_coor,cd_norm,gdist,gap,iseg,ww,nn)

     !if (check > 0) then
     !  print*,check, gap, adist, abs(gap) - adist
     !else
     !  print*,check 
     !endif
     !print*,'> '     

     
     !if (check > 0 .and. abs(gap) < adist) then
     if (check > 0 .and. gap < adist) then        
        contact           = .true.
        status(nbva+iv)   = check
        ! in this case, indexes of cdtac and antac are inverted
        r_icdtac(nbva+iv) = iantac
        r_iantac(nbva+iv) = icdtac
        ! info on the location of contact point
        icdver(nbva+iv)   = iv
        ianseg(nbva+iv)   = iseg
        weight(nbva+iv)   = ww
        ovlap(nbva+iv)    = gap
        n(:,nbva+iv)      = nn(:)
     endif

   
     

     
   enddo

   !fd cette partie fout le brun 
   if (.false.) then
   
     ! 1er nettoyage pour ne garder que les contacts les plus proches
     do iva=1,nbva
        ! si ce vertex de PGa est implique dans un contact avec un vertex de PGb
        ! on regarde si ce vertex de PGb ne serait pas deja implique dans un contact avec PGa
        if (status(iva) == 1) then
           ivb = ianseg(iva)
           ! si c'est un contact avec vertex different, alors il est forcement plus proche, on vire          
           if (status(nbva+ivb) == 1 .and. ianseg(nbva+ivb)/=iva) status(iva) = 0  
           ! si c'est contact avec un segment, alors il est forcement plus proche, on vire           
           if (status(nbva+ivb) == 2) status(iva) = 0
        endif
     enddo
     do ivb=1,nbvb
        ! on repete en inversant PGa et PGb
        if (status(nbva+ivb) == 1) then
           iva = ianseg(nbva+ivb)
           if (status(iva) == 1 .and. ianseg(iva)/=ivb) status(nbva+ivb) = 0
           if (status(iva) == 2) status(nbva+ivb) = 0
        endif
     enddo
   endif
  
   ! 2nd nettoyage pour eviter les doubles contact entre les memes vertex
   do iva=1,nbva
     if (status(iva) == 1) then
       ivb = ianseg(iva)
       ! si c'est un autre contact entre les 2 memes points, on vire
       if (status(nbva+ivb) == 1 .and. ianseg(nbva+ivb)==iva) status(iva) = 0
     endif
   enddo

 end subroutine detect_non_convex

 !> one looks for the proximal point in a POLYG for a given point
!> INPUT
!> POLYG    the antagonist POLYG
!> cd_coor  the given candidat
!> OUTPUT
!> gap      distance
!> point    the closest point
!> weight   the closest point expressed by the weights of the vertices
!> n        contact normal
!> node_POLYG_proximity = 0: no contact, 1:vertex , 2:edge
integer function node_POLYG_proximity(POLYG,cd_coor,cd_norm,gdist,gap,iseg,weight,n)
  use predicates
  use algebra
  implicit none
  type(T_POLYG)  :: POLYG
  integer        :: iseg
  real(kind=8)   :: cd_coor(2),cd_norm(2),n(2),gap,weight,gdist
  ! ***
  integer        :: min_vert,i,vii,vim,vip
  real(kind=8)   :: min_dist,dist,proj,ww,vec2,vec(2),nm(2),np(2),t(2),seg(2)
  real(kind=8)   :: vecp(2),vecm(2)
  real(kind=8)   :: gdist2, xx, cpnmvm, cpnmv, cpnpv, cpnpvp 
  logical        :: compm, compp, is_found
  
                          !123456789012345678901234567890123456789012345
  character(len=45):: IAM='mod_PLPLx::node_POLYG_proximity'

  ! default case: no contact  
  node_POLYG_proximity = 0

  gdist2 = gdist*gdist

  ! on cherche le vertex du POLYG le plus proche du cd_coor
  min_vert = 0
  min_dist = 1.d20

  !fd faire du manhattan ?!
  
  do i=1,POLYG%nb_vertex

    vec(:) = cd_coor(:) - POLYG%vertex(:,i)
    dist   = dot_product(vec,vec)

    if ( dist < gdist2 .and. dist < min_dist) then
      min_dist = dist
      min_vert = i
    endif
    
  enddo

  ! pas de contact car trop loin
  if ( min_vert == 0 ) then
    !print*,'no contact'
    return 
  endif

  vii    = min_vert
  vec2   = min_dist
  
  ! previous (m) and next (p) vertex
  vim = modulo(vii-2,POLYG%nb_vertex)+1
  vip = modulo(vii,  POLYG%nb_vertex)+1

  ! usefull vectors
  vec(:) = cd_coor(:) - POLYG%vertex(:,vii)
  vecm(:) = cd_coor(:) - POLYG%vertex(:,vim)
  vecp(:) = cd_coor(:) - POLYG%vertex(:,vip)  

  ! previous/next normals
  nm(:) = POLYG%normale(:,vim)
  np(:) = POLYG%normale(:,vii)

  ! pointe (<0) ou creux (>0) ?
  ! f_orient2d(pa, pb, pc) positive value if pc is left of axis, 0 if on axis, negative value otherwise
  xx = f_orient2d(POLYG%vertex(:,vim), POLYG%vertex(:,vip),  POLYG%vertex(:,vii))

  !
  ! nm X vecm
  cpnmvm = (nm(1)*vecm(2)) - (nm(2)*vecm(1)) 
  ! nm X vec
  cpnmv = (nm(1)*vec(2)) - (nm(2)*vec(1))
  ! np X vec
  cpnpv = (np(1)*vec(2)) - (np(2)*vec(1))
  ! np X vecp
  cpnpvp = (np(1)*vecp(2)) - (np(2)*vecp(1))

  !print*,xx,cpnmvm,cpnmv,cpnmv,cpnpvp
  
  ! pas de contact car pas en face
  if (cpnmvm < 0.d0 .or. cpnpvp > 0.d0) then
    !print*,'no contact' 
    return 
  endif

  is_found=.false.
  
  ! on determine dans quelle zone on tombe
  compm=.false.
  compp=.false.
  if (xx < 0.d0 ) then
    !cas angle sortant 
    if (cpnmv < 0.d0 .and. cpnpv > 0.d0) then
      ! dedans
      ! on doit tout tester
      compm=.true.
      compp=.true. 
    else if (cpnmv < 0.d0 .and. cpnpv <= 0.d0) then
      ! on est dehors en face du segment psm
      compm=.true.
    else if (cpnmv >= 0.d0 .and. cpnpv > 0.d0) then
      ! on est dehors en face du segment psp
      compp=.true.
    else if ( cpnmv >= 0.d0 .and. cpnpv <= 0.d0) then
      ! on est dehors et en face du coin
      node_POLYG_proximity = 1
      iseg=vii 
      weight = 1.d0
      !if (min_dist > 1e-6*gdist*gdist) then
      !  ! si on est loin on prend l'inter-axe
      !  gap = dsqrt(min_dist) 
      !  n(:) = vec(:)/dsqrt(vec2)
      !else
        ! si on est pres on prend la moyenne des normales  
        n = (np+nm) / length2(np+nm)
        gap = dot_product(vec,n)
      !endif
      !print*,'---'
      !print*,node_POLYG_proximity,iseg,weight
      !print*,gap
      !print*,n
      return
    endif   
  else
    !cas angle entrant
    if (cpnmv < 0.d0 .and. cpnpv > 0.d0) then
      ! on est dehors et en face du coin
      ! on doit tout tester 
      compm=.true.
      compp=.true.
    else if (cpnmv < 0.d0 .and. cpnpv <= 0.d0) then
      ! on est dehors en face du segment psm
      compm=.true.
    else if (cpnmv >= 0.d0 .and. cpnpv > 0.d0) then
      ! on est dehors en face du segment psp
      compp=.true.
    else if ( cpnmv >= 0.d0 .and. cpnpv <= 0.d0) then
      ! dedans
      node_POLYG_proximity = 1
      weight = 1.d0
      iseg=vii
      !if (min_dist > 1e-6*gdist*gdist) then
      !  ! si on est loin on prend l'inter-axe
      !  gap = -dsqrt(min_dist) 
      !  n(:) = -vec(:)/dsqrt(vec2)
      !else
        ! si on est pres on prend la moyenne des normales  
        n = (np+nm) / length2(np+nm)
        gap = dot_product(vec,n)
      !endif   
      !print*,'---'
      !print*,node_POLYG_proximity,iseg,weight
      !print*,gap
      !print*,n
      return
    endif   

  endif   

  ! ne peut pas arriver
  if (.not. compm .and. .not. compp) then
    call faterr(IAM,'Stange situation in plpl nc detection') 
  endif
  
  if (compm) then
    !si les normales candidat/antagoniste sont plus ou moins opposees, alors ok     
    if (dot_product(cd_norm,nm) < 0.d0) then
      ! on regarde le segment avant le vertex le plus proche
      ! t va de vii a vim 
      t(1)     =  nm(2)
      t(2)     = -nm(1)
      proj     = dot_product(vec,t)
      ! si le cd_coor se projette dans le segment d'avant
      if (proj >= 0.d0) then
        dist = vec2 - proj**2
        ! si en plus la distance est plus petite
        if (dist <= min_dist) then
          node_POLYG_proximity = 2
          min_dist    = dist
          iseg        = vim
          seg         = POLYG%vertex(:,vim) - POLYG%vertex(:,vii)
          weight      = min(1.d0,proj/dsqrt(dot_product(seg,seg)))
          n(:)        = nm(:)
          ! c 'est luxueux on a dist
          gap         = dot_product(vec,n)
          is_found=.true.
        endif
      else
        !print*,proj  
        call faterr(IAM,'wrong forward projection')  
      endif  
    endif
  endif
  
  if (compp) then
    !si les normales candidat/antagoniste sont plus ou moins opposees, alors ok     
    if (dot_product(cd_norm,np) < 0.d0) then
      ! on regarde le segment apres le vertex le plus proche
      ! t va de vii a vip
      t(1)     =  -np(2)  
      t(2)     =   np(1)
      proj     = dot_product(vec,t)
      ! si le cd_coor se projette dans le segment d'apres
      if (proj >= 0.d0) then
        dist = vec2 - proj**2
        ! si en plus la distance est plus petite 
        if (dist < min_dist) then
          node_POLYG_proximity = 2
          min_dist    = dist
          iseg        = vii
          seg         = POLYG%vertex(:,vip) - POLYG%vertex(:,vii)
          weight      = max(0.d0,1.d0 - proj/dsqrt(dot_product(seg,seg)))
          n(:)        = np(:)
          gap         = dot_product(vec,n)
          is_found=.true.
        endif
      else
        !print*,proj
        call faterr(IAM,'wrong backward projection')  
      endif
    endif
  endif

  if (compm .and. compp) then
    if (dot_product(cd_norm,(nm+np)) < 0.d0) then 
      if (node_POLYG_proximity == 0) then
        ! c'etait bien vii le plus proche
        node_POLYG_proximity = 1         
        iseg=vii 
        weight = 1.d0
        ! fd au final pas necessaire
        !if (min_dist > 1e-6*gdist*gdist) then
        !  ! si on est loin on prend l'inter-axe
        !  if (xx < 0.d0) then
        !    !dedans
        !    gap = -dsqrt(min_dist) 
        !    n(:) = -vec(:)/dsqrt(vec2)
        !  else
        !    !dehors 
        !    gap = dsqrt(min_dist) 
        !    n(:) = vec(:)/dsqrt(vec2)
        !  endif
        !else
         ! si on est pres on prend la moyenne des normales  
        n = (np+nm) / length2(np+nm)
        gap = dot_product(vec,n)
        !endif
        is_found=.true.
      endif
    endif
  endif

  !print*,'---'
  !print*,is_found
  !print*,node_POLYG_proximity,iseg,weight
  !print*,gap
  !print*,n

end function node_POLYG_proximity
 
!------------------------------------------------------------------------
 integer(kind=4) function get_type_PLPLx(icdan)
   implicit none
   integer :: icdan
   get_type_PLPLx = this(icdan)%dct
   
 end function get_type_PLPLx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
subroutine print_info_PLPLx(icdan)
   implicit none
   integer          :: icdan,icdtac,iantac,icdbdy,ianbdy

   character(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   write(cout,1) icdtac,iantac
   call LOGMES(cout)

1  format(1X,'POLYG:',1x,I5,1x,'POLYG:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   call print_info_POLYG(icdbdy)
   call print_info_POLYG(ianbdy)

end subroutine print_info_PLPLx
!------------------------------------------------------------------------
logical function RUN_PLPLx(fantome)

  implicit none
  integer,optional :: fantome

  RUN_PLPLx = RUN_TACTOR

end function RUN_PLPLx
!------------------------------------------------------------------------
  logical function CHECK_PLPLx()
    implicit none
    !   
    integer :: isee, nb_POLYG
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PLPLx = check_PLPLx_
      return
    end if

    con_pedigree%module_name = 'PLPLx'

    con_pedigree%id_cdan  = i_plplx
    con_pedigree%id_cdtac = i_polyg
    con_pedigree%id_antac = i_polyg

    cdtact2bdyty => polyg2bdyty
    antact2bdyty => polyg2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_POLYG = get_nb_POLYG()
    if( nb_POLYG < 2 ) then
      CHECK_PLPLx = check_PLPLx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'POLYG' .and. see(isee)%antac == 'POLYG') then
        check_PLPLx_ = .true.
        exit
      end if
    end do
  
    CHECK_PLPLx = check_PLPLx_
    return
  
  end function CHECK_PLPLx
!------------------------------------------------------------------------
  logical function get_write_Vloc_Rloc_PLPLx(fantome)

    implicit none
    integer,optional :: fantome

    get_write_Vloc_Rloc_PLPLx = write_Vloc_Rloc

  end function get_write_Vloc_Rloc_PLPLx

!!!------------------------------------------------------------------------  
 subroutine get_beta_PLPLx(icdtac,iadj,beta) 

   implicit none
   integer     :: icdtac,iadj
   real(kind=8):: beta

!fd burk: il faudrait verifier qu'on n'a pas n'importe quoi dans cette valeur !!

   beta=verlt(icdtac)%internal(4,iadj)

 end subroutine get_beta_PLPLx
!!!------------------------------------------------------------------------  
  subroutine update_fric_PLPLx(icdan,fric)
    implicit none
    integer(kind=4) :: icdan,icdtact,iantact,isect
    real(kind=8)    :: WScd,WSan,fric
    
    if (nb_WSsect .eq. 1)then
       isect = 1
       icdtact     = this(icdan)%icdtac
       iantact     = this(icdan)%iantac
    
       WScd = get_WS_POLYG(polyg2bdyty(1,icdtact),polyg2bdyty(2,icdtact),isect)
       WSan = get_WS_POLYG(polyg2bdyty(1,iantact),polyg2bdyty(2,iantact),isect)
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
    
  end subroutine update_fric_PLPLx
!!!------------------------------------------------------------------------
 real(kind=8) function get_length_PLPLx(icdan)

! fd le 04/11/07
! calcul de la longueur d'un contact PLPL
! soit 2 points -> distance entre les 2 points /2
! soit 1 point  -> rayon effectif (idem spheres) * 2*Pi/36 == la longueur de 10deg

   implicit none

   integer,intent(in)        :: icdan
   real(kind=8)              :: raycd,rayan
   real(kind=8),dimension(2) :: bord

   if (this(icdan)%dct /= 0) then

     bord = this(icdan)%coor - this(this(icdan)%icocdan)%coor

     get_length_PLPLx = 0.5*( dsqrt( dot_product( bord, bord ) ) )

   else
     raycd   = get_radius_POLYG(this(icdan)%icdtac)
     rayan   = get_radius_POLYG(this(icdan)%iantac)
     get_length_PLPLx = ((raycd*rayan)/(rayan+raycd)) * PI_g/18.d0
   endif

 end function get_length_PLPLx

!------------------------------------------------------------------------ 
 integer function get_periode_PLPLx(icdtac)

   implicit none

   integer :: icdtac

   if(.not.allocated(periodic_PLPLx)) then
        get_periode_PLPLx = 0
        return
   endif

   get_periode_PLPLx = periodic_PLPLx(icdtac)

 end function get_periode_PLPLx
!------------------------------------------------------------------------ 
 subroutine get_eff_PLPLx(icdan,meff,reff)

   implicit none

   integer      :: icdan
   real(kind=8) :: meff,reff
   
   reff = this(icdan)%reff
   
   meff = this(icdan)%meff

 end subroutine get_eff_PLPLx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine get_g2l_PLPLx(icdan,g2l)

   implicit none
   integer      :: icdan
   real(kind=8) :: g2l(2,6)

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
   
 end subroutine get_g2l_PLPLx

 ! rm : functions for siconos wrapper

 function get_old_index_PLPLx(icdan)
   implicit none
   integer(kind=4), intent(in) :: icdan
   integer(kind=4) :: get_old_index_PLPLx
   !
   integer :: icdtac,iantac,iadj

   get_old_index_PLPLx = 0

   if (.not. allocated(verlt)) then
      return
   endif
   
   icdtac = this(icdan)%icdtac                ! serial number of candidate contactor for contact icdan
   iantac = this(icdan)%iantac                ! serial number of antagonist contactor for contact icdan 

   if (verlt(icdtac)%adjsz /= 0) then
      do iadj=1,verlt(icdtac)%adjsz

         if (                                                           &
             (verlt(icdtac)%cdmodel      == polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,icdtac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,iantac)).or.   &
             (verlt(icdtac)%cdmodel      == polyg2bdyty(3,iantac) .and.  &
              verlt(icdtac)%cdbdy        == polyg2bdyty(1,iantac) .and.  &
              verlt(icdtac)%cdtac        == polyg2bdyty(2,iantac) .and.  &
              verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,icdtac) .and.  &
              verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,icdtac) .and.  &
              verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,icdtac))       &
         ) then
           if (verlt(icdtac)%cdsci(iadj) == this(icdan)%icdsci .and.  &
               verlt(icdtac)%ansci(iadj) == this(icdan)%iansci        &
           ) then
             get_old_index_PLPLx = verlt(icdtac)%icdan(iadj)
             exit
           end if
         end if
      end do
   end if

 end function get_old_index_PLPLx

!----------------------------------------------------
 
 function get_icdtac_PLPLx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_PLPLx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_POLYG
   logical :: found

   found = .false.

   nb_POLYG = get_nb_POLYG()

   icc = 0
   do icdtac = 1, nb_POLYG
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_PLPLx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLPLx::get_icdtac','unknown contact index')
   
 end function

!----------------------------------------------------
 
 function get_iantac_PLPLx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_PLPLx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_POLYG
   logical :: found

   found = .false.

   nb_POLYG = get_nb_POLYG()

   icc = 0
   do icdtac = 1, nb_POLYG
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_PLPLx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLPLx::get_icdtac','unknown contact index')
   

   get_iantac_PLPLx = this(icdan)%iantac

 end function

!----------------------------------------------------
 
 subroutine clean_memory_PLPLx
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_PLPLx  = 0
   nb_vPLPLx = 0

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%which) ) deallocate(box(i,j)%which)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_PLPLx) ) deallocate(rough_PLPLx)

   nb_rough_PLPLx = 0
   nstep_rough_seek_PLPLx = 1
   nb_recup_PLPLx = 0

   RUN = .false.

   if( allocated(PLcoor) ) deallocate(PLcoor)

   Reac_PLPLx_MAX = 0.D0

   first_time_betai = .true.
   if( allocated(betaiPOLYG) ) deallocate(betaiPOLYG)

   !PERIODIC = .false.
   !PERIODE  = 0.d0
   nb_PERIODIC_PLPLx = 0
   if( allocated(periodic_PLPLx) ) deallocate(periodic_PLPLx)

   !maxray, minray, maxalert, meanradius
   !Lbox,LBox_1,norm
   !maxpopul
   nb_big_polyg = 0
   if( allocated(big_polyg_list) ) deallocate(big_polyg_list)

   nb_ctc_simple = 0
   nb_ctc_double = 0

   module_checked_ = .FALSE.
   check_PLPLx_    = .FALSE.

 end subroutine

!----------------------------------------------------

 SUBROUTINE set_shrink_polyg_faces_PLPLx(shr)

    IMPLICIT NONE
    REAL(kind=8) :: shr
                             !12345678901234567890123456789
    character(len=29) :: IAM='PLPLx::set_shrink_polyg_faces'

    shrink_ = shr

    if ( shrink_ < 0.d0 .or. shrink_ > 1.d0) then
      call logmes('shrink should be between 0. and 1.')
      call logmes('0. nothing happen')
      call FATERR(IAM,'incompatible value of shrink coefficient')
    endif

END SUBROUTINE set_shrink_polyg_faces_PLPLx

!-------------------------------------------------------------------------------------
! routines a remonter dans python

 subroutine compute_czm_energy_PLPLx
   implicit none
   integer(kind=4) :: icdan,ibehav
   real(kind=8)    :: stored_nuc,damage_nuc,stored_tuc,damage_tuc
   real(kind=8)    :: gapTT
   real(kind=8)    :: cn,ct,smax,w,d,p,b,cohn,coht,dw,dg
   
   real(kind=8),dimension(max_internal_tact) :: internal
   
   if (allocated(energyPLPLx)) deallocate(energyPLPLx)
   allocate(energyPLPLx(nb_PLPLx))
   
   do icdan = 1,nb_PLPLx

      energyPLPLx(icdan)%failure  = 0.d0
      energyPLPLx(icdan)%stored   = 0.d0
      energyPLPLx(icdan)%damage   = 0.d0
      energyPLPLx(icdan)%cohesion = 0.d0

      internal = this(icdan)%internal
      ibehav   = this(icdan)%lawnb
      
      select case(tact_behav(ibehav)%ilaw)
      case(i_IQS_MAC_CZM,i_MAC_CZM)
         call get_czm(ibehav,cn,ct,smax,w,d)
         if(internal(4).eq.0.0) then    
            energyPLPLx(icdan)%failure = w*internal(1) 
         else
            if(internal(3).ne.0.D0) then
               stored_nuc = 0.5*internal(4)*(cn*internal(3)*internal(3))*internal(1)
               damage_nuc = (w*internal(1))-stored_nuc
            end if
            if(internal(2).ne.0.D0) then 
               stored_tuc = 0.5*internal(4)*(ct*internal(2)*internal(2))*internal(1) 
               damage_tuc = (w*internal(1))-stored_tuc
            end if
            energyPLPLx(icdan)%stored = stored_nuc + stored_tuc
            energyPLPLx(icdan)%damage = damage_nuc + damage_tuc
         end if
      case(i_IQS_WET_CZM)
         call get_czm(ibehav,cn,ct,smax,w,d)
         call get_coh(ibehav,cohn,coht,dw)
         gapTT = this(icdan)%gapTT
         if(internal(4).eq.0.0) then    
            energyPLPLx(icdan)%failure = w*internal(1) 
            if((gapTT.ge.0.D0).and.(gapTT.le.dw)) energyPLPLx(icdan)%cohesion = cohn*gapTT !+ coht*dg 
         else
            if(internal(3).ne.0.D0) then
               stored_nuc = 0.5*internal(4)*(cn*internal(3)*internal(3))*internal(1)
               damage_nuc = (w*internal(1))-stored_nuc
            end if
            if(internal(2).ne.0.D0) then 
               stored_tuc = 0.5*internal(4)*(ct*internal(2)*internal(2))*internal(1) 
               damage_tuc = (w*internal(1))-stored_tuc
            end if
            energyPLPLx(icdan)%stored = stored_nuc + stored_tuc
            energyPLPLx(icdan)%damage = damage_nuc + damage_tuc
         end if
      case default
         ! nothing to do ... yet
      end select
   end do
   
 end subroutine compute_czm_energy_PLPLx
!----------------------------------------------------
 subroutine get_CZM_energy_PLPLx(icdan,stored,damage,failure,cohesion)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: stored,damage,failure,cohesion

   stored   = energyPLPLx(icdan)%stored
   damage   = energyPLPLx(icdan)%damage
   failure  = energyPLPLx(icdan)%failure
   cohesion = energyPLPLx(icdan)%cohesion

 end subroutine get_CZM_energy_PLPLx
!----------------------------------------------------



 subroutine compute_betai_PLPLx
   implicit none
   integer         :: icdtac,iantac,iadj
   integer(kind=4) :: nb_POLYG,nb_adj
   real(kind=8),dimension(max_internal_tact) :: internal

   nb_POLYG = get_nb_POLYG()

   if(first_time_betai)then
      first_time_betai =.false.
      if(allocated(betaiPOLYG)) deallocate(betaiPOLYG)
      allocate(betaiPOLYG(nb_POLYG,2))
      
      betaiPOLYG = 0.d0

      if(nb_PLPLx.eq.0)then
         first_time_betai =.true.
         deallocate(betaiPOLYG)
         return
      end if

      do icdtac = 1,nb_POLYG
         
         nb_adj = verlt(icdtac)%adjsz
         do iadj = 1,nb_adj
            internal = 0.D0
            internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
            iantac = get_tact_id( verlt(icdtac)%anbdy(iadj), &
                                  verlt(icdtac)%antac(iadj), &
                                  verlt(icdtac)%anmodel(iadj)&
                                )

            
            betaiPOLYG(icdtac,1) = betaiPOLYG(icdtac,1) + internal(4)
            betaiPOLYG(icdtac,2) = betaiPOLYG(icdtac,2) + 1.d0

            betaiPOLYG(iantac,1) = betaiPOLYG(iantac,1) + internal(4)
            betaiPOLYG(iantac,2) = betaiPOLYG(iantac,2) + 1.d0
         end do
      end do

      do icdtac = 1,nb_POLYG
         betaiPOLYG(icdtac,2) = max(1.d0,betaiPOLYG(icdtac,2))
         betaiPOLYG(icdtac,1) = betaiPOLYG(icdtac,1)/betaiPOLYG(icdtac,2)
      end do

   else

      betaiPOLYG(:,1) = 0.d0

      do icdtac = 1,nb_POLYG
         
         nb_adj = verlt(icdtac)%adjsz
         do iadj = 1,nb_adj
            internal = 0.D0
            internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
            iantac = get_tact_id( verlt(icdtac)%anbdy(iadj), &
                                  verlt(icdtac)%antac(iadj), &
                                  verlt(icdtac)%anmodel(iadj)&
                                )
            
            betaiPOLYG(icdtac,1) = betaiPOLYG(icdtac,1) + internal(4)
            betaiPOLYG(iantac,1) = betaiPOLYG(iantac,1) + internal(4)
         end do
      end do

      do icdtac = 1,nb_POLYG
         !print*,betaiPOLYG(icdtac,1),betaiPOLYG(icdtac,2)
         betaiPOLYG(icdtac,1) = betaiPOLYG(icdtac,1)/betaiPOLYG(icdtac,2)
      end do
   end if

   do icdtac = 1,nb_POLYG
      call add_betai_to_POLYG(polyg2bdyty(1,icdtac),polyg2bdyty(2,icdtac),betaiPOLYG(icdtac,1))
   end do

 end subroutine compute_betai_PLPLx

 subroutine set_nb_PLPLx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PLPLx = nb

 end subroutine

 subroutine redo_nb_adj_PLPLx()
   implicit none

   call redo_nb_adj_( get_nb_POLYG() )

 end subroutine

end module PLPLx
