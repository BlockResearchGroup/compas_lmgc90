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
module DKKDx                                          

  !!****h* LMGC90.CORE/DKKDx
  !! NAME
  !!  module DKKDx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors DISKx and xKSID.
  !!  In this modulus candidate contactors are DISKx and antagonist 
  !!  contactors are xKSID.
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/DISKx
  !!  LMGC90.CORE/xKSID
  !!****

  use overall
  use tact_behaviour
  use DISKx, only : get_nb_diskx, diskx2bdyty           , &
                    print_info_diskx, get_radius_DISKx  , &
                    get_ent_diskx       => get_ent      , &
                    get_color_diskx     => get_color    , &
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
                    get_mean_radius_DISKx               , &
                    get_max_radius_DISKx                , &
                    get_min_radius_DISKx                , &
                    get_mass_DISKx                      , &
                    get_ws_diskx                        , &
                    add_stress_diskx                    , &
                    update_status_sector_diskx          , &
                    get_vd_diskx                        

  use xKSID, only : get_nb_xksid, xksid2bdyty           , &
                    print_info_xksid, get_radius_Xksid  , &
                    get_ent_xksid       => get_ent      , &
                    get_color_xksid     => get_color    , &
                    get_coor_xksid      => get_coor     , &
                    get_coorTT_xksid    => get_coorTT   , &
                    get_V_xksid         => get_V        , &
                    get_Vbegin_xksid    => get_Vbegin   , &
                    add_reac_xksid      => add_reac     , &
                    get_vlocy_xksid     => get_vlocy    , &
                    comp_vlocy_xksid    => comp_vlocy   , &
                    nullify_reac_xksid  => nullify_reac , &
                    nullify_vlocy_xksid => nullify_vlocy, &
                    get_ws_xksid                        , &
                    get_max_radius_xksid                , &
                    get_min_radius_xksid                , &
                    get_vd_xksid



  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use parameters, only : i_dkkdx, i_diskx, i_xksid, i_mailx, i_rbdy2, i_mbs2, &
                         i_undefined, i_free, i_sheared, i_stationary

  use inter_meca_2D

  implicit none
  
  private

  integer          :: nb_DISKx
  integer          :: nb_xKSID
 
  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!!!------------------------------------------------------------------------ 

  integer :: nb_DKKDx                  ! nb_DKKDx = number of selected candidates DISKx against xKSID
                                       ! <= size(this).
  integer :: nb_vDKKDx

!!!------------------------------------------------------------------------ 


  type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!!!------------------------------------------------------------------------  

  integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs DISKx-xKSID
                                                          ! to candidate contactor DISKx icdtac.

!!!------------------------------------------------------------------------ 

  type(T_verlet), dimension(:), allocatable, target :: verlt

!!!------------------------------------------------------------------------

  type T_box
     ! For quick sorting, disks are owned by boxes, sorting being 
     ! performed within a box and immediate surrounding boxes, see
     ! subroutine enumerate_DKKDx.
                              
     integer                               :: DKpopul   ! box(ibox1,ibox2)%popul: number of DISKx in box ibox1,ibox2;
   
     integer, dimension(:), pointer        :: DKwhich   ! box(ibox1,ibox2)%which(ipopul):
                                                        ! rank in the list of contactors of DISKx labelled ipopul
                                                        ! in box ibox1,ibox2;
 
     integer                               :: KDpopul   ! box(ibox1,ibox2)%popul: number of xKSID in box ibox1,ibox2;
   
     integer, dimension(:), pointer        :: KDwhich   ! box(ibox1,ibox2)%which(ipopul): 
                                                        ! rank in the list of contactors of xKSID labelled ipopul
                                                        ! in box ibox1,ibox2;
  end type T_box

  type(T_box), dimension(:,:),allocatable  :: box       ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

  type T_rough_DKKDx
                                                        ! définit le type de la liste des plus proches voisins
     integer :: cd                                      ! le candidat, l'antagoniste et isee pour la loi de contact
     integer :: an
     integer :: isee

!!! > md > !!!
     real(kind=8)               :: meff,reff            ! effective mass and radius for md method 
!!! < md < !!!

  end type T_rough_DKKDx
 
  type(T_rough_DKKDx),dimension(:),allocatable   :: rough_DKKDx            ! table  de visibilité

  type T_link_rough_DKKDx                                                  ! liste chainée pour déterminer les listes de cand anta car
                                                                           ! on ne connait pas le nb de cand -ant à priori
     type(T_link_rough_DKKDx), pointer :: p                                ! pointeur sur le precedent
     type(T_rough_DKKDx)               :: val                              ! les valeurs
     type(T_link_rough_DKKDx), pointer :: n                                ! pointeur sur le suivant

  end type T_link_rough_DKKDx

  type(T_link_rough_DKKDx),pointer               :: Root,Current,Previous

  integer(kind=4) :: nb_WSsect = 1

  integer                                        :: Nstep_rough_seek_DKKDx=1
  logical                                        :: write_creation_tab_visu

!!!------------------------------------------------------------------------
!!! variables attached to surrounding boxes

  real (kind=8)  :: maxray, minray, maxalert, meanradius
  real (kind=8)  :: Lbox,LBox_1,norm
  integer        :: nb_rough_DKKDx
  integer        :: nb_recup_DKKDx
  integer        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul

!!!------------------------------------------------------------------------

  real(kind=8)   :: Reac_DKKDx_MAX=0.D0
  real(kind=8), dimension(:)  , allocatable, target :: violation
  real(kind=8), dimension(:,:), allocatable, target :: DKcoor   !  coordinates of body owning DISKx to be used in selecting prox tactors
  real(kind=8), dimension(:,:), allocatable, target :: KDcoor   !  coordinates of body owning xKSID to be used in selecting prox tactors

!!!------------------------------------------------------------------------

  logical :: RUN=.false.

  logical :: module_checked_ = .FALSE.
  logical :: check_DKKDx_    = .FALSE.

!!!------------------------------------------------------------------------
!!! public functions 
  public &
      stock_rloc_DKKDx, &
      recup_rloc_DKKDx, &
      smooth_computation_DKKDx, &
      compute_box_DKKDx, &
      read_ini_Vloc_Rloc_DKKDx, &
      write_xxx_Vloc_Rloc_DKKDx, &
      display_prox_tactors_DKKDx, &
      coor_prediction_DKKDx, &
      creation_tab_visu_DKKDx, &
      compute_contact_DKKDx, &
      RUN_DKKDx, &
      CHECK_DKKDx, &
      get_write_Vloc_Rloc_DKKDx

  public &
       nullify_reac_DKKDx, &
       nullify_vlocy_DKKDx, &
       injj_DKKDx, prjj_DKKDx, vitrad_DKKDx, & 
       get_nb_DKKDx, &
       DKKDx2DISKx, &
       DKKDx2xKSID, &
!!$    get_Wik_DKKDx,compute_Wikik_DKKDx,compute_Wikjl_DKKDx, &
       print_info_DKKDx, &
       get_length_DKKDx, &
       get_eff_DKKDx,update_cohe_DKKDx, update_fric_DKKDx, &
       get_g2l_DKKDx, &
       get_icdtac_DKKDx, &
       get_iantac_DKKDx, &
       set_surface_sectors_DKKDx, &
       clean_memory_DKKDx

  !rm for handler
  public get_this    , &
         set_nb_DKKDx, &
         redo_nb_adj_DKKDx, &
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

!!!------------------------------------------------------------------------
  subroutine coor_prediction_DKKDx

    implicit none
    
    integer :: ibdy,itac,itact,errare

    nb_DISKx = get_nb_DISKx()
    nb_xKSID = get_nb_xKSID()

    if (smooth_method) then
       do itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coor_DISKx(itact)
       end do
       do itact=1,nb_xKSID
          KDcoor(1:3,itact) = get_coor_xKSID(itact)
       end do
    else
       
       do itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coorTT_DISKx(itact)
       end do
       do itact=1,nb_xKSID
          KDcoor(1:3,itact) = get_coorTT_xKSID(itact)
       end do
    end if

  end subroutine coor_prediction_DKKDx
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_DKKDx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_DKKDx
!!!------------------------------------------------------------------------
  subroutine write_xxx_Vloc_Rloc_DKKDx(which)
    
    implicit none
    
    integer :: which,nfich,lc
    
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
    
  end subroutine write_xxx_Vloc_Rloc_DKKDx
!!!------------------------------------------------------------------------
  subroutine compute_box_DKKDx

    implicit none

    integer             :: isee,errare,ibdy
    character(len=103)  :: cout
    character(len=22)   :: IAM = 'mod_DKKDx::compute_box'

    nb_DISKx = get_nb_DISKx()
    nb_xKSID = get_nb_xKSID()

    minray     = get_min_radius_DISKx()
    maxray     = get_max_radius_DISKx()
    meanradius = get_mean_radius_DISKx()
    minray     = min(minray,get_min_radius_xKSID())
    maxray     = max(maxray,get_max_radius_xKSID())

    if (minray > maxray ) then
       call faterr(IAM,'Messing error computing minray and maxray')
    end if

    ! computing largest alert distance between disks 
    maxalert=0.D0  
    do isee=1,size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'xKSID') then
        maxalert=max(maxalert,see(isee)%alert)
      end if
    end do
    
    Lbox   = 1.01D0*(2.D0*maxray + maxalert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/minray

    if (.not. allocated(adjac))then
       allocate(adjac(nb_DISKx),stat=errare)
       if (errare /=0 ) then
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
    if (errare /=0 ) then
       write(cout,'(A25)') 'error in allocating nb_adj'
       call FATERR(IAM,cout)
    end if

    nb_adj = 0
    
    ! DKcoor are coordinates of bodies owning DISKx to be used in selecting prox tactors
    if (allocated(DKcoor)) deallocate(DKcoor)
    allocate(DKcoor(3,nb_DISKx),stat=errare)

    ! KDcoor are coordinates of bodies owning xKSID to be used in selecting prox tactors
    if (allocated(KDcoor)) deallocate(KDcoor)
    allocate(KDcoor(3,nb_xKSID),stat=errare)

 end subroutine compute_box_DKKDx

!!!------------------------------------------------------------------------ 

 subroutine creation_tab_visu_DKKDx
 
   implicit none 
   
   integer                               :: errare 

   integer                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
   integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty   
   real(kind=8)                          :: Bleft,Bright,Bup,Bdown
   character(len=5)                      :: cdtac,cdcol,antac,ancol
   real(kind=8),dimension(3)             :: coord,coordcd,coordan 
   real(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gapTT,masscd

   character(len=103)          :: cout
   character(len=28)           :: IAM

   IAM = 'mod_DKKDx::creation_tab_visu'

!!! Since the list of proximate contactors may not be updated at every time step,
!!! boxes data would be lost if deallocated. When starting the program, boxes are not created.
!!! A warning condition prevents undue deallocation. 

   if (allocated(box)) then
      do ibox1=minibox1,maxibox1
         do ibox2=minibox2,maxibox2
            if (associated(box(ibox1,ibox2)%DKwhich)) deallocate(box(ibox1,ibox2)%DKwhich)
            if (associated(box(ibox1,ibox2)%KDwhich)) deallocate(box(ibox1,ibox2)%KDwhich)
         end do
      end do
      deallocate(box)
   end if
    
!!! Building boxes for quick sorting 
  
!!! Computing maximal boundary radius of disks and largest box containing disks.
!!!
!!! The computation of maximal radius of disks allows to size a grid of boxes,
!!! the purpose of which is quick sorting of candidates to contact. The following
!!! sorting method is reasonnably fast. It is not really efficacious when the 
!!! ratio between maximal and minimal radius is too large (>10), since the box 
!!! size becomes too large. Ratio less than 3 or even 5 are fair.
!!! The minimum radius of disks is also used, in order to estimate the maximal 
!!! number of disks per box.   
!!! This quick sorting method may be applied to bodies other than disks, such as 
!!! ellipsoidal or polygonal bodies with a reasonable aspect ratio, less than 
!!! 3 or even 5. Such bodies are enclosed in disks with radius max_radius, and 
!!! enclosing a disk with radius min_radius. The sorting algorithm may then be 
!!! straightforwardly applied.
!!! In the case of disks, max_radius and min_radius should be merely bry_radius. 
!!! Since the data file allows, for further generalization purposes, other 
!!! contactors than BDARY (the true boundary), extracting min_radius and 
!!! max_radius as bry_radius might seem to be somewhat tortuous, though simple.

   Bleft    =  1.D24
   Bright   = -1.D24
   Bup      = -1.D24
   Bdown    =  1.D24

   do ibdy=1,nb_DISKx
      coord = DKcoor(1:3,ibdy)
      Bleft = min(coord(1),Bleft )
      Bright= max(coord(1),Bright)
      Bup   = max(coord(2),Bup   )
      Bdown = min(coord(2),Bdown )
   end do

   do ibdy=1,nb_xKSID
      coord = KDcoor(1:3,ibdy)
      Bleft = min(coord(1),Bleft )
      Bright= max(coord(1),Bright)
      Bup   = max(coord(2),Bup   )
      Bdown = min(coord(2),Bdown )
   end do
 
   !if (minray < 0.1D-02*(Bright-Bleft) .or. minray < 0.1D-02*(Bup-Bdown)) then
   !   print*,' minray is quite small, in select_prox_tactors in mod_DKKDx'
   !  write(*,'(A9,D14.7)')'  minray=',minray
   !  !stop
   !end if   
   !if (maxray > (Bright-Bleft) .or. maxray > (Bup-Bdown)) then
   !  print*,' maxray is quite large, in select_prox_tactors in mod_DKKDx'
   !  write(*,'(A9,D14.7)')'  maxray=',maxray
   !end if 

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

   minibox1 = 1
   maxibox1 = 1 + int((Bright-Bleft)*Lbox_1)
   minibox2 = 1
   maxibox2 = 1 + int((Bup - Bdown )*Lbox_1)
   maxpopul = (1+int(norm))*(1+int(norm))
   !
   ! for each box maxpopul is less than the total number of DISKx 
   !
   maxpopul=min(maxpopul,nb_DISKx+nb_xKSID)
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
         box(ibox1,ibox2)%DKpopul=0
         box(ibox1,ibox2)%KDpopul=0
         allocate(box(ibox1,ibox2)%DKwhich(maxpopul),stat=errare)
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%DKwhich')
         end if
         allocate(box(ibox1,ibox2)%KDwhich(maxpopul),stat=errare)
         if (errare /=0 ) then
            call FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%KDwhich')
         end if
      end do
   end do
  
   ! filling boxes with disks
   ! box(ibox1,ibox2)%DKpopul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%KDwhich(ipopul) is the rank of body DISKx labelled ipopul in the box
  
   ! filling boxes   

   do ibdy=1,nb_DISKx
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
      end if

      box(ibox1,ibox2)%DKpopul = box(ibox1,ibox2)%DKpopul + 1
      if( box(ibox1,ibox2)%DKpopul > size(box(ibox1,ibox2)%DKwhich) ) then
          call faterr(IAM, "Estimated max popul limit reached for DK.")
      end if

      box(ibox1,ibox2)%DKwhich(box(ibox1,ibox2)%DKpopul) = ibdy

   end do

   do ibdy=1,nb_xKSID
      coord=KDcoor(1:3,ibdy)
      ibox1=1+int((coord(1)-Bleft )*Lbox_1)
      ibox2=1+int((coord(2)-Bdown )*Lbox_1)
      if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
         write(cout,103) ' maxibox1=',maxibox1,'maxibox2=',maxibox2
         call LOGMES(cout)
         write(cout,103) '    ibox1=',ibox1,   '   ibox2=',ibox2
         call LOGMES(cout)
         write(cout,103) ' minibox1=',minibox1,'minibox2=',minibox2 
         call LOGMES(cout)
         write(cout,'(A13,I10,A13)') '  body xKSID ',ibdy,' out of boxes'
         call FATERR(IAM,cout)
103      format(1X,A10,1X,I5,1X,A10,1X,I5)
      end if
      
      box(ibox1,ibox2)%KDpopul = box(ibox1,ibox2)%KDpopul+1
      if( box(ibox1,ibox2)%KDpopul > size(box(ibox1,ibox2)%KDwhich) ) then
          call faterr(IAM, "Estimated max popul limit reached for KD.")
      end if
      box(ibox1,ibox2)%KDwhich(box(ibox1,ibox2)%KDpopul) = ibdy
   end do
   
   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
   
   nb_rough_DKKDx = 0
   
   ! création de la liste de paire à examiner
   
   ! on esalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue un zone memoire au fur et à mesure que l'on determine un candidat - antagoniste
   
   nullify(Root)
   nullify(Current)
   nullify(Previous)
   
   do ibox1cd=minibox1,maxibox1  
      do ibox2cd=minibox2,maxibox2
         do icdpop=1,box(ibox1cd,ibox2cd)%DKpopul

            icdtac=box(ibox1cd,ibox2cd)%DKwhich(icdpop)
            cdcol=get_color_DISKx(icdtac)
            ! box loop investigating antagonist xKSID

            do ibox1an=max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                        
               do ibox2an=max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
                  do ianpop=1,box(ibox1an,ibox2an)%KDpopul

                     iantac=box(ibox1an,ibox2an)%KDwhich(ianpop)
                     ancol=get_color_xKSID(iantac)
                     isee=get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol,'RBDY2','xKSID',ancol)

                     if (isee /= 0) then
                        adist=see(isee)%alert 
                        ! checking ROUGHLY distance against alert distance           
                        coordcd = DKcoor(1:3,icdtac)
                        coordan = KDcoor(1:3,iantac)
                        raycd = get_radius_DISKx(icdtac)
                        rayan = get_radius_xKSID(iantac)

                       ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                       ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                       ! results might be different up to some non significant figures, but when comparing to
                       ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

                        adist=0.1005D+01*adist
                        nonuc=dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)                 
                        gapTT=rayan-nonuc-raycd
                        ! checking distance against alert distance 
                        if (gapTT .le. adist) then
                           nb_rough_DKKDx = nb_rough_DKKDx+1
                           if ( nb_rough_DKKDx == 1) then
                              allocate(Root)
                              Current => Root
                              nullify(Root%p)
                           else
                              allocate(Current)
                              Previous%n => Current
                           end if
                           Current%val%cd       =icdtac
                           Current%val%an       =iantac
                           Current%val%isee     =isee
                           
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

   write(cout,'(4X,I10,A20)') nb_rough_DKKDx,' DKKDx roughly found'
   call logmes(cout)

   if (allocated(rough_DKKDx)) deallocate(rough_DKKDx)
   allocate(rough_DKKDx(nb_rough_DKKDx))      ! the visibility array used in compute_contact is allocated
   
   if (allocated(this)) deallocate(this)
   allocate(this(nb_rough_DKKDx))             ! the oversized array this is temporaly allocated 

   do icdan=nb_rough_DKKDx,1,-1
     
      Previous => Current%p
      rough_DKKDx(icdan)%cd     = Current%val%cd
      rough_DKKDx(icdan)%an     = Current%val%an
      rough_DKKDx(icdan)%isee   = Current%val%isee
      
      raycd = get_radius_DISKx(Current%val%cd)
      
      rough_DKKDx(icdan)%reff = raycd
      
      masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))
      
      rough_DKKDx(icdan)%meff = masscd
      
      deallocate(Current)
      Current => Previous
   end do
   
   nullify(Root)
   
 end subroutine creation_tab_visu_DKKDx

!!!--------------------------------------------------------------------------------------------------

 subroutine compute_contact_DKKDx
 
   implicit none  
   
   integer                               :: errare 

   integer                               :: icdan,iadj,ibdy,icdtac,iantac,isee,itacty    
   character(len=5)                      :: cdtac,cdcol,antac,ancol
   real(kind=8),dimension(3)             :: coordcd,coordan,cd_Vbegin,an_Vbegin
   !fd 2020-06-18
   real(kind=8),dimension(2)             :: cdlev  
   real(kind=8)                          :: raycd,rayan,adist,dist,gapTT,ut1,ut2,un1,un2,Gant3,Gann3
   integer                               :: i,id,j
   real(kind=8)                          :: norm,nonuc                                ! scalaire contenant la norme de sep

   integer                               :: cd_ent,an_ent

   character(len=103)        :: cout
   character(len=32)         :: IAM
   
   IAM = 'mod_DKKDx::compute_contact_DKKDx'
   icdan = 0        
   nb_DKKDx = 0

   nb_adj = 0

   if (nb_rough_DKKDx /= 0 ) then
      !
      ! preparing detection 
      !
      icdtac=1  !fd pour l'instant, c'est ok...
      iantac=1
      
      do i=1,nb_rough_DKKDx
         icdtac=rough_DKKDx(i)%cd
         iantac=rough_DKKDx(i)%an
         isee=rough_DKKDx(i)%isee  
         adist=see(isee)%alert 
         coordcd = DKcoor(1:3,icdtac)
         coordan = KDcoor(1:3,iantac)
         raycd= get_radius_DISKx(icdtac)
         rayan= get_radius_xKSID(iantac)

         nonuc=dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)                 
         gapTT=rayan-nonuc-raycd
         ! checking distance against alert distance           
         if (gapTT .le. adist) then    
            icdan=icdan+1
            nb_adj(icdtac)=nb_adj(icdtac)+1                     
            
            iadj = nb_adj(icdtac)
            
            if (smooth_method) then
               cd_Vbegin = get_V_DISKx(icdtac)
               an_Vbegin = get_V_xKSID(iantac)
            else
               cd_Vbegin = get_Vbegin_DISKx(icdtac)
               an_Vbegin = get_Vbegin_xKSID(iantac)
            endif
            
            this(icdan)%iadj    = iadj
            this(icdan)%icdbdy  = diskx2bdyty(1, icdtac)
            this(icdan)%icdtac  = icdtac
            this(icdan)%icdsci  = 0
            this(icdan)%ianbdy  = xksid2bdyty(1, iantac)
            this(icdan)%iantac  = iantac
            this(icdan)%iansci  = 0
            this(icdan)%isee    = isee
            
            this(icdan)%icdbtac = diskx2bdyty(2, icdtac)
            this(icdan)%ianbtac = xksid2bdyty(2, iantac)

            this(icdan)%icdbtyp = diskx2bdyty(3, icdtac)
            this(icdan)%ianbtyp = xksid2bdyty(3, iantac)

            this(icdan)%icdctyp = i_diskx
            this(icdan)%ianctyp = i_xksid

            cd_ent = get_ent_DISKx(this(icdan)%icdtac)
            an_ent = get_ent_xKSID(this(icdan)%iantac) 
            
            this(icdan)%icdent = cd_ent
            this(icdan)%ianent = an_ent

            if (cd_ent /= an_ent) then
              entity(cd_ent)%nb = entity(cd_ent)%nb+1
              entity(an_ent)%nb = entity(an_ent)%nb+1
            else
              entity(cd_ent)%nb = entity(cd_ent)%nb+1
            end if
            
            this(icdan)%nuc(:)    = -(coordcd(1:2)-coordan(1:2))/nonuc
            this(icdan)%tuc(1)    =  this(icdan)%nuc(2)
            this(icdan)%tuc(2)    = -this(icdan)%nuc(1)

            
            cdlev=(-raycd*this(icdan)%nuc) + get_shiftTT_DISKx(icdtac)

            this(icdan)%Gcdt3      =  -cdlev(2)*this(icdan)%tuc(1)+cdlev(1)*this(icdan)%tuc(2)
            this(icdan)%Gcdn3      =  -cdlev(2)*this(icdan)%nuc(1)+cdlev(1)*this(icdan)%nuc(2)
            this(icdan)%Gant3      =  rayan
            this(icdan)%Gann3      =  0.D0
            this(icdan)%gapTTbegin =  gapTT
            
            this(icdan)%vltBEGIN   = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1)    &
                                   + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2)    &
                                   + cd_Vbegin(3)*this(icdan)%Gcdt3   &
                                   - an_Vbegin(3)*this(icdan)%Gant3

            this(icdan)%vlnBEGIN   = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1)   &
                                   + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2)    &
                                   + cd_Vbegin(3)*this(icdan)%Gcdn3   &
                                   - an_Vbegin(3)*this(icdan)%Gann3

            this(icdan)%rlt       = 0.D0
            this(icdan)%rln       = 0.D0
            this(icdan)%vlt       = this(icdan)%vltBEGIN
            this(icdan)%vln       = this(icdan)%vlnBEGIN
            this(icdan)%gapTT     = this(icdan)%gapTTbegin
            this(icdan)%status    = i_nknow
            
            this(icdan)%reff    = rough_DKKDx(i)%reff
            this(icdan)%meff    = rough_DKKDx(i)%meff

            this(icdan)%coor(1:2) = DKcoor(1:2, icdtac) - raycd * this(icdan)%nuc(1:2)

         end if
      end do

      nb_DKKDx=icdan
      
   end if
  
   write(cout,'(1X,I10,A12)') nb_DKKDx,' DKKDx found'
   call logmes(cout)

   do ibdy=1,nb_DISKx
      if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
      if (nb_adj(ibdy) /= 0) then
         allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
         if (errare /=0 ) then
            write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
            call faterr(IAM,cout)
         end if
      end if
   end do
  
   do icdan=1,nb_DKKDx
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   end do
   
   
   do icdan = 1, nb_DKKDx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   if (allocated(violation)) deallocate(violation)
   allocate(violation(nb_DKKDx),stat=errare)
   

 end subroutine compute_contact_DKKDx
!!!------------------------------------------------------------------------
  subroutine smooth_computation_DKKDx

    implicit none
    integer          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    do icdan=1,nb_DKKDx
       
       call compute_2D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            this(icdan)%reff,this(icdan)%meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

    end do
    !$OMP END DO
    !$OMP END PARALLEL
    
    do icdan=1,nb_DKKDx  
       call nullify_reac_DKKDx(icdan,iIreac)
    end do
    
    do icdan=1,nb_DKKDx
       call injj_DKKDx(icdan,this(icdan)%rlt,this(icdan)%rln,iIreac)
    end do
    
  end subroutine smooth_computation_DKKDx
!!!------------------------------------------------------------------------
 subroutine display_prox_tactors_DKKDx

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   character(len=5) :: cdmodel, anmodel
   
   nb_DISKx = get_nb_DISKx()

   do icdtact=1,nb_DISKx    
      do iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( xksid2bdyty(3,iantac) )
         write(*,'(A1)')' '
         write(*,'(A6,2X,I5)')'$icdan',icdan     
                          !123456789012345678901234567890123456789012345678901234567890123456789012
         write(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '       
         write(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
              cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
              anmodel,xksid2bdyty(1,iantac),'xKSID',xksid2bdyty(2,iantac)
         write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
         write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
         write(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
         ! write(*,104)'rlt =',this(icdan)%rlt,'rln =',this(icdan)%rln,'rls =',0.D0
         write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
         write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTbegin
         write(*,'(A1)')' '               
      end do
   end do
   
104 format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_DKKDx

!!!------------------------------------------------------------------------  

 subroutine stock_rloc_DKKDx
   ! get data from this and put into verlt
   !           
   implicit none

   integer :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   integer :: errare

   character(len=103) :: cout
   character(len=20)  :: IAM = 'mod_DKKDx::stoc_rloc'

   nb_DISKx = get_nb_DISKx()
   
   ! sizing verlt:
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
         verlt(icdtac)%adjsz=0
         call free_verlet_(icdtac)
         iadj=nb_adj(icdtac)
         if (iadj > 0) then
            verlt(icdtac)%adjsz=iadj
            call new_verlet_(icdtac, iadj, errare)
         else
            call nullify_verlet_(icdtac)
         end if
      end do
   end if
   
   ! filling data:
   do icdan=1,nb_DKKDx
      icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
      iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
      iadj   = this(icdan)%iadj                        ! serial adjacent number of pair contactor 
      ! adjacent to candidate contactor for contact icdan 
      verlt(icdtac)%icdan(iadj)   = icdan
      verlt(icdtac)%cdbdy         = diskx2bdyty(1,icdtac)
      verlt(icdtac)%cdtac         = diskx2bdyty(2,icdtac)
      verlt(icdtac)%cdmodel       = diskx2bdyty(3,icdtac)
      verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
      verlt(icdtac)%anbdy(iadj)   = xksid2bdyty(1,iantac)
      verlt(icdtac)%antac(iadj)   = xksid2bdyty(2,iantac)
      verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
      verlt(icdtac)%anmodel(iadj) = xksid2bdyty(3,iantac)
      verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
      verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
      verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
      verlt(icdtac)%vln(iadj)     = this(icdan)%vln
      verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT


      verlt(icdtac)%status(iadj)  = this(icdan)%status
      verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
      verlt(icdtac)%coor(1:2,iadj)= this(icdan)%coor(1:2)

      verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
      
   end do

   nb_vDKKDx = nb_DKKDx
   
   WRITE(cout,'(1X,I10,A12)') nb_vDKKDx,' stock DKKDx'
   call logmes(cout)

 end subroutine stock_rloc_DKKDx

!!!------------------------------------------------------------------------ 

 subroutine recup_rloc_DKKDx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   implicit none

   integer :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   character(len=103) :: cout
   character(len=21)  :: IAM = 'mod_DKKDx::recup_rloc'

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_DKKDx=0

   do icdan=1,nb_DKKDx

      this(icdan)%rlt=0.D0
      this(icdan)%rln=0.D0
      this(icdan)%statusBEGIN=i_nknow
      icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
      iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        
      
      if (verlt(icdtac)%adjsz /= 0) then
         if ( verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
              verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac) .and. &
              verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac)       &
            ) then
            do iadj = 1, verlt(icdtac)%adjsz
               if ( verlt(icdtac)%anbdy(iadj)  == xksid2bdyty(1,iantac) .and. &
                    verlt(icdtac)%antac(iadj)  == xksid2bdyty(2,iantac) .and. &
                    verlt(icdtac)%anmodel(iadj)== xksid2bdyty(3,iantac)       &
                    ) then
                  this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
                  this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
                  this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
                  
                  this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
                  nb_recup_DKKDx = nb_recup_DKKDx + 1
                  exit
               end if
            end do
         end if
      end if
   end do

   write(cout,'(1X,I10,A12)') nb_recup_DKKDx,' recup DKKDx'
   call logmes(cout)

 end subroutine recup_rloc_DKKDx

!!!------------------------------------------------------------------------  
 
 subroutine read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   implicit none

   character(len=103) :: clin

   integer                           :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   integer                           :: errare, cdmodel, anmodel
   integer                           :: ibehav,nb_internal,i_internal

   real(kind=8)                      :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)         :: coor,nuc,tuc

   character(len=5)                  :: cdbdy,cdtac,anbdy,antac,behav,sttus
   character(len=103)                :: cout
   character(len=28)                 :: IAM = 'mod_DKKDx::read_ini_Vloc_Rloc'

   nb_DISKx=get_nb_DISKx()
   nb_xKSID=get_nb_xKSID()
   
   ! first reading: sizing verlt
   ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
   ! For this purpose nb_adj is introduced.
   if (.not. allocated(nb_adj)) then 
     allocate(nb_adj(nb_DISKx),stat=errare)
     if (errare /=0 ) call FATERR(IAM,' error allocating nb_adj')
   end if

   do icdtac=1,nb_DISKx
     nb_adj(icdtac)=0
   end do

   do    
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKKDx') cycle     
      if( .not. read_G_clin()) exit
      if( .not. read_G_clin()) exit
      
      read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                         cdbdy,icdbdy,cdtac,icdtac,behav,anbdy,ianbdy,antac,iantac,sttus

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
   do icdtac=1,nb_DISKx
      nb_adj(icdtac)=0
   end do
   icdan = 0
   do    
      if( .not. read_G_clin()) exit
      if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
      if (G_clin(9:13)/= 'DKKDx') cycle
      if( .not. read_G_clin()) exit
      if( .not. read_G_clin()) exit
      read(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                         cdbdy,icdbdy,cdtac,icdtac,behav,anbdy,ianbdy,antac,iantac,sttus
      cdmodel = get_body_model_id_from_name( cdbdy )
      anmodel = get_body_model_id_from_name( anbdy )
      do icdtact=1,nb_DISKx
         if (diskx2bdyty(1,icdtact) == icdbdy .and. &
             diskx2bdyty(2,icdtact) == icdtac .and. &
             diskx2bdyty(3,icdtact) == cdmodel ) then
            icdan = icdan + 1

            nb_adj(icdtact)=nb_adj(icdtact)+1 
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
            verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
            verlt(icdtact)%rln(nb_adj(icdtact))=rln
            if( .not. read_G_clin()) exit
            read(G_clin(1:90),'(27X,2(7X,D14.7))')vlt,vln
            verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
            verlt(icdtact)%vln(nb_adj(icdtact))=vln
            if( .not. read_G_clin()) exit 
            read(G_clin(1:90),'(27X,1(7X,D14.7))')gapTT
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
      end do
      cycle
   end do
   
   nb_vDKKDx=0
   
   do icdtact=1,nb_DISKx
      nb_vDKKDx = nb_vDKKDx + nb_adj(icdtact)
      if ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) then 
         write(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
              'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         call FATERR(IAM,cout)
      end if
   end do

 end subroutine read_ini_Vloc_Rloc

!!!------------------------------------------------------------------------   

 subroutine write_out_Vloc_Rloc(nfich)
   !
   ! write into file out_Vloc_Rloc data from this, in verlt style
   !
   implicit none

   integer                   :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nfich,icdtact
   integer                   :: lc
   real(kind=8),dimension(2) :: coor

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_xKSID = get_nb_xKSID()
   nb_DISKx = get_nb_DISKx()
   
   do icdtact=1,nb_DISKx    
      do iadj=1,nb_adj(icdtact)    
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         
         !mj Should rather be the coordinates of the mid gap point 
         ! (coordinates of the contact point if contact is active)
         coor(1) = DKcoor(1,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(1))
         coor(2) = DKcoor(2,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(2))

         cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( xksid2bdyty(3,iantac) )
         write(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKKDx',icdan 
         !1234567890123456789012345678901234567890123456789012345678901234567890123456
         write(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'       
         write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
              cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),  &
              see(this(icdan)%isee)%behav,  &
              anmodel,xksid2bdyty(1,iantac),'xKSID',xksid2bdyty(2,iantac),  &
              get_contact_status_name_from_id(this(icdan)%status),iantac
         write(nfich,104)'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls =',0.D0
         write(nfich,104)'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',0.D0
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
 
103 format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104 format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine write_out_Vloc_Rloc

!!!------------------------------------------------------------------------   


!!!------------------------------------------------------------------------   

 subroutine nullify_reac_DKKDx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer :: icdtac,iantac,storage   
    
   icdtac=this(icdan)%icdtac
   call nullify_reac_DISKx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_reac_xKSID(iantac,storage)
    
 end subroutine nullify_reac_DKKDx

!!!------------------------------------------------------------------------ 

 subroutine nullify_vlocy_DKKDx(icdan,storage)

   implicit none

   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,storage   
    
   icdtac=this(icdan)%icdtac
   call nullify_vlocy_DISKx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_vlocy_xKSID(iantac,storage)
    
 end subroutine nullify_vlocy_DKKDx

!!!------------------------------------------------------------------------ 

 subroutine vitrad_DKKDx( icdan, storage, need_full_V )

   implicit none

   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac,storage   
   logical, optional  :: need_full_V
    
   icdtac=this(icdan)%icdtac
   call comp_vlocy_DISKx(icdtac,storage)
    
   iantac=this(icdan)%iantac
   call comp_vlocy_xKSID(iantac,storage)
    
 end subroutine vitrad_DKKDx

!!!------------------------------------------------------------------------  

 subroutine injj_DKKDx(icdan,RTIK,RNIK,storage)
 
   implicit none

   integer     ,intent(in)    :: icdan
   real(kind=8),intent(in)    :: RTIK,RNIK
   integer,     dimension(3)  :: cdccdof,anccdof
   real(kind=8),dimension(3)  :: cdreac,anreac
   integer                    :: storage   
   
   cdccdof(1)=1
   anccdof(1)=1
   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)
   cdccdof(2)=2
   anccdof(2)=2
   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)=-cdreac(2)
   cdccdof(3)=3
   anccdof(3)=3
   cdreac(3)= this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK
   anreac(3)=-this(icdan)%Gant3*RTIK-this(icdan)%Gann3*RNIK

   call add_reac_DISKx(this(icdan)%icdtac,cdccdof,cdreac,storage)
   call add_reac_xKSID(this(icdan)%iantac,anccdof,anreac,storage)

 end subroutine injj_DKKDx 

!!!------------------------------------------------------------------------ 

 subroutine prjj_DKKDx(icdan,VTIK,VNIK,storage)
 
   implicit none

   integer     ,intent(in)   :: icdan
   real(kind=8),intent(out)  :: VTIK,VNIK
   real(kind=8),dimension(3) :: Vcd,Van
   integer     ,intent(in)   :: storage
   integer                   :: icdtac,ianbdy
   real(kind=8)              :: Vdcd,Vdan,Vd
   
   if (storage == iVfree) then
     ! fd dila
     call get_Vd_DISKx(this(icdan)%icdtac,Vdcd)
     call get_Vd_xKSID(this(icdan)%iantac,Vdan)
     !fd le signe est important
     Vd=Vdcd-Vdan
   else 
     Vd=0.d0
   endif

   call get_vlocy_DISKx(this(icdan)%icdtac,storage,Vcd)
   call get_vlocy_xKSID(this(icdan)%iantac,storage,Van) 

   VTIK=Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3  &
       -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3 
   VNIK=Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3  &
       -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3  &
       -Vd 

 end subroutine prjj_DKKDx 

!!$!!!------------------------------------------------------------------------ 
!!$
!!$ subroutine compute_Wikik_DKKDx(icdan,WTT,WTN,WNT,WNN)
!!$
!!$  implicit none
!!$
!!$  integer                   :: icdan,icdbdy,ianbdy
!!$  real(kind=8)              :: WTT,WTN,WNT,WNN
!!$  real(kind=8),dimension(3) :: icdmass,ianmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$
!!$  icdmass = get_inv_mass_DISKx(icdbdy)
!!$  ianmass = get_inv_mass_xKSID(ianbdy)
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
!!$ end subroutine compute_Wikik_DKKDx
!!$
!!$!!!------------------------------------------------------------------------ 
!!$
!!$ subroutine get_Wik_DKKDx(icdan,ikcd,tik,nik,ikcdmass,ikGcdt,ikGcdn)
!!$
!!$  implicit none
!!$  integer                   :: icdan,ikcd
!!$  real(kind=8)              :: ikGcdt,ikGcdn
!!$  real(kind=8),dimension(3) :: ikcdmass
!!$  real(kind=8),dimension(2) :: tik,nik
!!$
!!$  ikcd    = this(icdan)%icdbdy
!!$  ikGcdt  = this(icdan)%Gcdt3
!!$  ikGcdn  = this(icdan)%Gcdn3
!!$  ikcdmass= get_inv_mass_DISKx(ikcd)
!!$  tik     = this(icdan)%tuc
!!$  nik     = this(icdan)%nuc
!!$
!!$ end subroutine get_Wik_DKKDx
!!$
!!$!!!------------------------------------------------------------------------ 
!!$
!!$ subroutine compute_Wikjl_DKKDx(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$  implicit none
!!$
!!$  integer                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy
!!$  real(kind=8)              :: WTT,WTN,WNT,WNN
!!$  real(kind=8),dimension(3) :: icdmass,ianmass,jcdmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$  jcdbdy=this(jcdan)%icdbdy
!!$
!!$  icdmass = get_inv_mass_DISKx(icdbdy)
!!$  ianmass = get_inv_mass_xKSID(ianbdy)
!!$  jcdmass = get_inv_mass_DISKx(jcdbdy)
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
!!$  else
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
!!$  endif
!!$
!!$ end subroutine compute_Wikjl_DKKDx

!!!------------------------------------------------------------------------ 
 subroutine compute_stress_DKKDx

   implicit none
   integer(kind=4)             :: icdan,ID_RBDY2
   integer(kind=4)             :: icdbdy,icdtac
   real(kind=8),dimension(2)   :: Fik,Lcd,coor
   real(kind=8),dimension(3)   :: tmp
   real(kind=8),dimension(2,2) :: SIGMA

   do icdan=1,nb_DKKDx

      Fik = this(icdan)%rln*this(icdan)%nuc + this(icdan)%rlt*this(icdan)%tuc

      icdtac = this(icdan)%icdtac
      
      ID_RBDY2 = diskx2bdyty(1,icdtac)

      tmp  = get_coorTT_DISKx(icdtac)
      coor = tmp(1:2) - get_shiftTT_DISKx(icdtac)

      Lcd = coor - this(icdan)%coor

      SIGMA(1,1:2) = Lcd(1)*Fik(1:2)
      SIGMA(2,1:2) = Lcd(2)*Fik(1:2)

      call add_stress_DISKx(ID_RBDY2,SIGMA)

   end do

 end subroutine compute_stress_DKKDx
!!!------------------------------------------------------------------------ 
 integer function get_nb_DKKDx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_DKKDx = nb_DKKDx
   case(i_verlet_tactor)
      get_nb_DKKDx = nb_vDKKDx
   case(i_rough_tactor)
      get_nb_DKKDx = nb_rough_DKKDx
   case(i_recup_tactor)
      get_nb_DKKDx = nb_recup_DKKDx
   end select

 end function get_nb_DKKDx
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
 subroutine DKKDx2DISKx(icdan,icdtac)

   implicit none

   integer :: icdan,icdtac

   icdtac = this(icdan)%icdtac

 end subroutine DKKDx2DISKx

!!!------------------------------------------------------------------------ 

 subroutine DKKDx2xKSID(icdan,iantac)

   implicit none

   integer :: icdan,iantac

   iantac = this(icdan)%iantac

 end subroutine DKKDx2xKSID

!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
 subroutine get_eff_DKKDx(icdan,meff,reff)
  implicit none

   integer      :: icdan
   real(kind=8) :: meff,reff

   reff = this(icdan)%reff

   meff = this(icdan)%meff

 end subroutine get_eff_DKKDx
!!!------------------------------------------------------------------------ 

 subroutine print_info_DKKDx(icdan)

   implicit none
   integer          :: icdan,icdtac,iantac,icdbdy,ianbdy
   
   character(len=80) :: cout
   
   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac
   
   write(cout,1) icdtac,iantac
   call LOGMES(cout)
   
1  format(1X,'DISKx:',1x,I5,1x,'xKSID:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy
   
   call print_info_DISKx(icdbdy)
   call print_info_xKSID(ianbdy)
   
 end subroutine print_info_DKKDx
!------------------------------------------------------------------------
 real(kind=8) function get_length_DKKDx(icdan)
   implicit none
   !
   integer(kind=4), intent(in) :: icdan 
   !
   integer(kind=4) :: icdtac
   real(kind=8)    :: raycd

   raycd = get_radius_DISKx(this(icdan)%icdtac)

   get_length_DKKDx = raycd
   
 end function get_length_DKKDx
!------------------------------------------------------------------------ 
logical function RUN_DKKDx(fantome)

  implicit none
  integer,optional :: fantome

  RUN_DKKDx = RUN_TACTOR

end function RUN_DKKDx
!------------------------------------------------------------------------
  logical function CHECK_DKKDx()
    implicit none
    !   
    integer :: isee
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_DKKDx = check_DKKDx_
      return
    end if

    con_pedigree%module_name = 'DKKDx'

    con_pedigree%id_cdan  = i_dkkdx
    con_pedigree%id_cdtac = i_diskx
    con_pedigree%id_antac = i_xksid

    cdtact2bdyty => diskx2bdyty
    antact2bdyty => xksid2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_xKSID = get_nb_xKSID()
    nb_DISKx = get_nb_DISKx()
    if( nb_xKSID == 0 .or. nb_DISKx == 0 ) then
      CHECK_DKKDx = check_DKKDx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'xKSID') then
        check_DKKDx_ = .true.
        exit
      end if
    end do
  
    CHECK_DKKDx = check_DKKDx_
    return
  
  end function CHECK_DKKDx
!!!------------------------------------------------------------------------ 
  logical function get_write_Vloc_Rloc_DKKDx(fantome)

    implicit none
    integer,optional :: fantome

    get_write_Vloc_Rloc_DKKDx = write_Vloc_Rloc

  end function get_write_Vloc_Rloc_DKKDx
!!!------------------------------------------------------------------------ 
!!! Some user contributions
!------------------------------------------------------------------------ 
  subroutine update_fric_DKKDx(icdan,fric)
    implicit none
    integer(kind=4) :: icdan,icdtact,iantact,isect
    real(kind=8)    :: WScd,WSan,fric
    
    if (nb_WSsect .eq. 1)then
       isect = 1
       icdtact     = this(icdan)%icdtac
       iantact     = this(icdan)%iantac
    
       WScd = get_WS_DISKx(diskx2bdyty(1,icdtact),diskx2bdyty(2,icdtact),isect)
       WSan = get_WS_xKSID(xksid2bdyty(1,iantact),xksid2bdyty(2,iantact),isect)
       !mr experimental model
       fric = min(WScd,WSan)
    end if
    
  end subroutine update_fric_DKKDx

!!!------------------------------------------------------------------------ 
!> brief Give the number of sector of contactor surface
  subroutine set_surface_sectors_DKKDx(nb)
    implicit none
    integer(kind=4),intent(in) :: nb

    nb_WSsect = 2*nb

  end subroutine set_surface_sectors_DKKDx

  subroutine update_cohe_DKKDx(icdan,cohe)
   implicit none
    integer(kind=4)            :: icdan,itact,isect,i
    real(kind=8)               :: c,s,cohe
    real(kind=8),dimension(2)  :: IOcd,IOan,IO
    real(kind=8)               :: WScd,WSan,da

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
             exit
          else
             isect = nb_WSsect - i + 1
             exit
          end if
       end if
    end do

    WScd = get_WS_DISKx(diskx2bdyty(1,itact),diskx2bdyty(2,itact),isect)

    itact = this(icdan)%iantac
    IOan(1:2) = this(icdan)%coor(1:2) - KDcoor(1:2,itact)

    c=cos(KDcoor(3,itact))
    s=sin(KDcoor(3,itact))

    IO(1) = c*IOan(1) + s*IOan(2)
    IO(2) =-s*IOan(1) + c*IOan(2)
    norm = SQRT(IO(1)*IO(1)+IO(2)*IO(2))
    IO = IO/norm

    do i=1,nb_WSsect/2
       if(IO(1).gt.cos(i*da))then 
          if(IO(2).gt.0)then
             isect = i
             exit
          else
             isect = nb_WSsect - i + 1
             exit
          end if
       end if
    end do

    WSan = get_WS_xKSID(xksid2bdyty(1,itact),xksid2bdyty(2,itact),isect)

   if (abs(WScd+WSan).lt.1.D-16) then
      cohe = 0.D0
   else
      cohe = (WSan*WScd)/(WScd+WSan)
   end if

 end subroutine update_cohe_DKKDx
 !------------------------------------------------------------------------ 
!MR&VHN 
 !> brief
  subroutine update_WS_sector_DKKDx()
    implicit none
    integer(kind=4)            :: icdan
    integer(kind=4)            :: wsstatus,itact,isect,i
    real(kind=8)               :: c,s
    real(kind=8),dimension(2)  :: IOcd,IOan,IO
    real(kind=8)               :: WScd,WSan,da


    do icdan=1,nb_DKKDx

       select case(this(icdan)%status)
       case(i_Wnctc)                    !vhnhu
          wsstatus = i_free
       case(i_stick,i_Wstck)
          wsstatus = i_stationary
       case(i_slifw,i_Wslfw)
          wsstatus = i_sheared
       case(i_slibw,i_Wslbw)
          wsstatus = i_sheared
       case default
          wsstatus = i_undefined
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
  end subroutine update_WS_sector_DKKDx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 subroutine get_g2l_DKKDx(icdan,g2l)

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
   
 end subroutine get_g2l_DKKDx

 function get_icdtac_DKKDx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_DKKDx
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
         get_icdtac_DKKDx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKKDx::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_DKKDx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_DKKDx
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
         get_iantac_DKKDx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKKDx::get_icdtac','unknown contact index')
   

   get_iantac_DKKDx = this(icdan)%iantac

 end function

 subroutine clean_memory_DKKDx
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_DKKDx  = 0
   nb_vDKKDx = 0

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%DKwhich) ) deallocate(box(i,j)%DKwhich)
         if( associated(box(i,j)%KDwhich) ) deallocate(box(i,j)%KDwhich)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_DKKDx) ) deallocate(rough_DKKDx)

   nb_rough_DKKDx = 0
   nstep_rough_seek_DKKDx = 1
   nb_recup_DKKDx = 0

   RUN = .false.

   nb_DISKx = 0
   nb_xKSID = 0
   if( allocated(DKcoor) ) deallocate(DKcoor)
   if( allocated(KDcoor) ) deallocate(KDcoor)

   Reac_DKKDx_MAX = 0.D0

   !maxray, minray, maxalert, meanradius
   !Lbox,LBox_1,norm
   !maxpopul

   module_checked_ = .FALSE.
   check_DKKDx_    = .FALSE.

 end subroutine
 
 subroutine set_nb_DKKDx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_DKKDx = nb

 end subroutine

 subroutine redo_nb_adj_DKKDx()
   implicit none

   call redo_nb_adj_( get_nb_DISKx() )

   ! because DKcoor is needed in this case
   ! to write vloc_rloc
   if (allocated(DKcoor)) deallocate(DKcoor)
   allocate( DKcoor( 3, get_nb_DISKx() ) )
   if (allocated(KDcoor)) deallocate(KDcoor)
   allocate( KDcoor( 3, get_nb_xKSID() ) )
   call coor_prediction_DKKDx()

 end subroutine

end module DKKDx
