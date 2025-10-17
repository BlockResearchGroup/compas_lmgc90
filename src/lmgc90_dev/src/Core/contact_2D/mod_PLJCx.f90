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
module PLJCx

  !!****h* LMGC90.CORE/PLJCx
  !! NAME
  !!  module PLJCx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors POLYG and JONCx.
  !!  In this modulus candidate contactors are POLYG and antagonist 
  !!  contactors are JONCx.
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/JONCx
  !!  LMGC90.CORE/POLYG
  !!****

  use overall
  use tact_behaviour
  use POLYG, only : T_POLYG, get_nb_polyg, polyg2bdyty  , &
                    get_l_polyg, get_radius_polyg       , &
                    get_min_radius_polyg                , &
                    get_max_radius_polyg                , &
                    get_mean_radius_polyg               , &
                    get_Vd_polyg                        , &
                    add_stress_polyg                    , &
                    move_bdary_polyg, print_info_polyg  , &
                    get_inv_mass_polyg, get_mass_polyg  , &
                    get_ent_polyg       => get_ent      , &
                    get_ws_polyg                        , &
                    get_color_polyg     => get_color    , &
                    get_visible_polyg   => get_visible  , &
                    get_shiftTT_polyg   => get_shiftTT  , &
                    get_coorTT_polyg    => get_coorTT   , &
                    get_Vbegin_polyg    => get_Vbegin   , &
                    add_reac_polyg      => add_reac     , &
                    get_vlocy_polyg     => get_vlocy    , &
                    comp_vlocy_polyg    => comp_vlocy   , &
                    nullify_reac_polyg  => nullify_reac , &
                    nullify_vlocy_polyg => nullify_vlocy

  use JONCx, only : get_nb_joncx, joncx2bdyty           , &
                    get_min_radius_joncx                , &
                    get_max_radius_joncx                , &
                    get_axes_joncx, print_info_joncx    , &
                    get_ws_joncx                        , &
                    get_inv_mass_joncx                  , &
                    get_ent_joncx       => get_ent      , &
                    get_color_joncx     => get_color    , &
                    get_visible_joncx   => get_visible  , &
                    !get_coor_joncx      => get_coor     , &
                    get_coorTT_joncx    => get_coorTT   , &
                    get_Vbegin_joncx    => get_Vbegin   , &
                    get_V_joncx         => get_V        , &
                    add_reac_joncx      => add_reac     , &
                    get_vlocy_joncx     => get_vlocy    , &
                    comp_vlocy_joncx    => comp_vlocy   , &
                    nullify_reac_joncx  => nullify_reac , &
                    nullify_vlocy_joncx => nullify_vlocy

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use inter_meca_2D

  use parameters, only : i_pljcx, i_polyg, i_joncx, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

 type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

 integer         :: nb_PLJCx                          ! nb_PLJCx = number of selected candidate POLYG against POLYG
                                                      ! due to the fact that their might be 2 node_segment for each
                                                      ! entry in this it should be higher than size(this)
 integer         :: nb_vPLJCx                         ! nb_PLJCx = number of selected candidates DISKx against JONCx
                                                      ! <= size(this).


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdbdy): number of adjacent pairs body-contactor
                                                         ! to candidate body ibdycd.

!------------------------------------------------------------------------  

!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------ 

 type T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_DKJCx.
                              
   integer                               :: PLpopul   ! box(ibox1,ibox2)%popul: number of polyg in box ibox1,ibox2;
   
   integer, dimension(:), pointer        :: PLwhich   ! box(ibox1,ibox2)%which(ipopul): 
   integer                               :: JCpopul   ! box(ibox1,ibox2)%popul: number of joncs in box ibox1,ibox2;
   
   integer, dimension(:), pointer        :: JCwhich   ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 end type T_box 
!------------------------------------------------------------------------

 type(T_box), dimension(:,:),allocatable  :: box    ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.


 real(kind=8)                                    :: Reac_PLJCx_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------
 type T_rough_PLJCx                                                  ! définit le type de la liste des plus proches voisins
    integer :: cd                                                   ! le candidat, l'antagoniste et isee pour la loi de contact
    integer :: an
    integer :: isee
    real(kind=8),dimension(2) :: point,N
    real(kind=8) :: ax1, ax2
!!! > md > !!!
    real(kind=8) :: meff,reff                         ! effective mass and radius for md method 
!!! < md < !!!

 end type T_rough_PLJCx

type(T_rough_PLJCx),dimension(:),allocatable   :: rough_PLJCx        ! table  de visibilité

!------------------------------------------------------------------------
type T_link_rough_PLJCx                        ! liste chainée pour déterminer les listes de cand anta car onne connait pas le nb
                                               ! de cand -ant à priori
   type(T_link_rough_PLJCx), pointer :: p      ! pointeur sur le precedent

   type(T_rough_PLJCx) :: val                  ! les valeurs
  
   type(T_link_rough_PLJCx ), pointer :: n     ! pointeur sur le suivant

end type T_link_rough_PLJCx

type(T_link_rough_PLJCx),pointer                  :: Root,Current,Previous
!------------------------------------------------------------------------
type T_tmp_jonc
   real(kind=8),dimension(2) :: N
   real(kind=8),dimension(2) :: T
end type T_tmp_jonc


real(kind=8),dimension(:,:),allocatable            :: PL_coor          ! tableau (3,nb_POLYG) contenant les coordonnées 
                                                                       ! des centres au cours du calcul
real(kind=8),dimension(:,:),allocatable            :: JC_coor          ! tableau (3,nb_JONCx) contenant les coordonnées
                                                                       ! des centres au cours du calcul
type(T_tmp_jonc),dimension(:),allocatable          :: tmp_jonc

 integer                                       :: Nstep_rough_seek_PLJCx=1
 logical                                       :: write_creation_tab_visu

!------------------------------------------------------------------------
! variables attached to surrounding boxes

 real (kind=8)  :: maxray, minray,maxalert,meanradius
 real (kind=8)  :: Lbox,LBox_1,norm
 integer        :: nb_rough_PLJCx
 integer        :: nb_recup_PLJCx
 integer        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul

 !------------------------------------------------------------------------
 
 integer(kind=4)           :: nb_WSsect = 1
 integer(kind=4),parameter :: i_max_friction=0,i_min_friction=1,i_average_friction=2
 integer(kind=4)           :: i_friction_model = 2

 !------------------------------------------------------------------------
 logical :: RUN=.false.
 logical :: module_checked_ = .FALSE.
 logical :: check_PLJCx_    = .FALSE.


 integer            :: nb_ctc_simple     ! nombre de contact avec un seul point
 integer            :: nb_ctc_double     ! nombre de contact avec deux points

!------------------------------------------------------------------------
! liste des fonctions publiques
!
 public &
      compute_box_PLJCx, &
      compute_contact_PLJCx, &
      coor_prediction_PLJCx, &
      creation_tab_visu_PLJCx, &
      display_prox_tactors_PLJCx, &
      get_write_Vloc_Rloc_PLJCx, &
      get_eff_pljcx, &
      read_ini_Vloc_Rloc_PLJCx, &
      recup_rloc_PLJCx, &
      stock_rloc_PLJCx, &
      write_xxx_Vloc_Rloc_PLJCx, &
      RUN_PLJCx, &
      CHECK_PLJCx, &
      nullify_reac_PLJCx,&
      nullify_vlocy_PLJCx,&
      injj_PLJCx, prjj_PLJCx, vitrad_PLJCx, &
      get_nb_PLJCx, &
      PLJCx2POLYG, PLJCx2JONCx,&
!!$   compute_Wikik_PLJCx,compute_Wikjl_PLJCx,get_Wik_PLJCx, &
      print_info_PLJCx,get_type_PLJCx,& 
      get_g2l_PLJCx, &
      get_old_index_PLJCx, &
      get_length_PLJCx, get_beta_PLJCx, &
      compute_stress_PLJCx, &
      get_icdtac_PLJCx, &
      get_iantac_PLJCx, &
      clean_memory_PLJCx, &
!fd      get_h5_Vloc_Rloc_arg_PLJCx, &
!fd      get_nb_interaction_PLJCx, &
      set_friction_model_PLJCx, &
      update_fric_PLJCx
 
 !rm for handler
 public get_this    , &
        set_nb_PLJCx, &
        redo_nb_adj_PLJCx, &
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
  subroutine coor_prediction_PLJCx

    implicit none
    
    integer(kind=4) :: itact,errare
    integer(kind=4) :: nb_POLYG
    integer(kind=4) :: nb_JONCx

    
    nb_POLYG = get_nb_POLYG()
    nb_JONCx = get_nb_JONCx()

    ! Choosen coordinates are theta predicted coordinates (for consistency with theta method)
    do itact=1,nb_POLYG
       PL_coor(1:3,itact) = get_coorTT_POLYG(itact)
       call move_BDARY_POLYG(itact,PL_coor(1:3,itact))
    end do
    
    do itact=1,nb_JONCx
       JC_coor(1:3,itact) = get_coorTT_JONCx(itact)
       call ROT_JONCx(itact,JC_coor(1:3,itact))
    end do

  end subroutine coor_prediction_PLJCx
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PLJCx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PLJCx
!!!------------------------------------------------------------------------
  subroutine write_xxx_Vloc_Rloc_PLJCx(which)
    
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
    
  end subroutine write_xxx_Vloc_Rloc_PLJCx
!!!------------------------------------------------------------------------
 subroutine compute_box_PLJCx

   implicit none

   integer         :: isee,errare,ibdy
   real(kind=8)    :: minray_polyg,maxray_polyg,meanradius_polyg
   real(kind=8)    :: minray_joncx,maxray_joncx,meanradius_joncx
   integer         :: nb_POLYG
   integer         :: nb_JONCx

   character(len=80) :: cout
                              !1234567890123456789012
   character(len=22) :: IAM = 'mod_PLJCx::compute_box'

   ! on fait ici les choses qui ne doivent que lorsque nb_DISKx change
   nb_POLYG = get_nb_POLYG()
   nb_JONCx = get_nb_JONCx()

   minray_polyg     = get_min_radius_POLYG()
   maxray_polyg     = get_max_radius_POLYG()
   meanradius_polyg = get_mean_radius_POLYG()

   minray_joncx     = get_min_radius_JONCx()
   maxray_joncx     = get_max_radius_JONCx()

   minray=min(minray_polyg,minray_joncx)
   maxray=max(maxray_polyg,maxray_joncx)

   meanradius=meanradius_polyg

   if (minray > maxray ) then
    call faterr(IAM,'Messing error computing minray and maxray')
   end if

   ! computing largest alert distance between disks 
   maxalert=0.D0  
   do isee=1,size(see)
     if (see(isee)%cdtac == 'POLYG' .and. see(isee)%antac == 'JONCx') then
       maxalert=max(maxalert,see(isee)%alert)
     end if
   end do

   
   Lbox   = 1.01D0*(2.D0*maxray + maxalert)
   Lbox_1 = 1.D0/Lbox
   norm   = Lbox/minray



   if (.not. allocated(adjac))then
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
     end do
   endif  
  
   if (allocated(nb_adj)) deallocate(nb_adj)
   allocate(nb_adj(nb_POLYG),stat=errare)
   if (errare /=0 ) then
      call faterr(IAM,'Error allocating nb_adj')
   end if
  
   nb_adj=0

   ! DKcoor are coordinates of bodies owning DISKx to be used in selecting prox tactors
   ! JCcoor are coordinates of bodies owning JONCx to be used in selecting prox tactors
   if (allocated(PL_coor)) deallocate(PL_coor)
   allocate(PL_coor(3,nb_POLYG),stat=errare)   
   if (allocated(JC_coor)) deallocate(JC_coor)
   allocate(JC_coor(3,nb_JONCx),stat=errare)
   if (.not. allocated(tmp_jonc)) allocate(tmp_jonc(nb_JONCx))
  
 end subroutine compute_box_PLJCx
!------------------------------------------------------------------------ 
!--------------------------------------------------------------------------------------------------
subroutine creation_tab_visu_PLJCx
 
  implicit none 
 
  type(T_POLYG)                         :: PLibdy,PLicdbdy
  integer                               :: errare 

  integer                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
  integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty,i   
  real(kind=8)                          :: Bleft,Bright,Bup,Bdown,Lbox
  character(len=5)                      :: cdtac,cdcol,antac,ancol,cdbdyty,anbdyty
  real(kind=8),dimension(3)             :: coord,coordcd,coordan 
  real(kind=8)                          :: ax1,ax2,masscd
  real(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gap,norm1,norm2
  real(kind=8),dimension(2)             :: axean  
  integer         :: nb_POLYG
  integer         :: nb_JONCx

  character(len=80) :: cout
                             !1234567890123456789012345678
  character(len=28) :: IAM = 'mod_PLJCx::creation_tab_visu'

   nb_POLYG = get_nb_POLYG()
   nb_JONCx = get_nb_JONCx()

  ! Since the list of proximate contactors may not be updated at every time step,
  ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
  ! A warning condition prevents undue deallocation.

  if (allocated(box)) then
     do ibox1=minibox1,maxibox1
        do ibox2=minibox2,maxibox2
           if (associated(box(ibox1,ibox2)%PLwhich)) deallocate(box(ibox1,ibox2)%PLwhich)
           if (associated(box(ibox1,ibox2)%JCwhich)) deallocate(box(ibox1,ibox2)%JCwhich)
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

  Bleft    =  1.D24
  Bright   = -1.D24
  Bup      = -1.D24
  Bdown    =  1.D24

  do ibdy=1,nb_POLYG
     if (.not. get_visible_POLYG(ibdy)) cycle
     coord = PL_coor(:,ibdy)
     Bleft = min(coord(1),Bleft )
     Bright= max(coord(1),Bright)
     Bup   = max(coord(2),Bup   )
     Bdown = min(coord(2),Bdown )
  end do

  do ibdy=1,nb_JONCx
     if (.not. get_visible_JONCx(ibdy)) cycle
     coord = JC_coor(:,ibdy)
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

  minibox1 =-1
  maxibox1 = 1 + int((Bright-Bleft)*Lbox_1)
  minibox2 =-1
  maxibox2 = 1 + int((Bup - Bdown )*Lbox_1)
  maxpopul = (1+int(norm))*(1+int(norm))
  !
  ! for each box maxpopul is less than the total number of DISKx 
  !
  maxpopul=min(maxpopul,nb_POLYG+nb_JONCx)
  !   
  allocate(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)

  if (errare /=0 ) then
     call faterr(IAM,'Error allocating box')
  end if
  do ibox1=minibox1,maxibox1
  do ibox2=minibox2,maxibox2
     box(ibox1,ibox2)%PLpopul=0
     box(ibox1,ibox2)%JCpopul=0

     allocate(box(ibox1,ibox2)%PLwhich(maxpopul),stat=errare)
     if (errare /=0 ) then
        write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%which'
        call faterr(IAM,cout)
     end if

     allocate(box(ibox1,ibox2)%JCwhich(maxpopul),stat=errare)
     if (errare /=0 ) then
       write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%JCwhich'
       call faterr(IAM,cout)
     end if   
  end do
  end do
  ! filling boxes with POLYG
  ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
  ! box(ibox1,ibox2)%which(ipopul) is the rank of body DISKx labelled ipopul in the box

  ! filling boxes   
  do ibdy=1,nb_POLYG
     if (.not.get_visible_POLYG(ibdy)) cycle
     coord=PL_coor(:,ibdy)
     ibox1=1+int((coord(1)-Bleft )*Lbox_1)
     ibox2=1+int((coord(2)-Bdown )*Lbox_1)
     if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
        write(cout,'(A,I0,A,I0)')' maxibox1=',maxibox1,'maxibox2=',maxibox2
        write(cout,'(A,I0,A,I0)')'    ibox1=',ibox1,   '   ibox2=',ibox2
        write(cout,'(A,I0,A,I0)')' minibox1=',minibox1,'minibox2=',minibox2
        write(cout,'(A13,I5,A13)')'  body POLYG ',ibdy,' out of boxes'
        call faterr(IAM,cout)
     end if
     box(ibox1,ibox2)%PLpopul=box(ibox1,ibox2)%PLpopul+1
     if( box(ibox1,ibox2)%PLpopul > size(box(ibox1,ibox2)%PLwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for PL.")
     end if
     box(ibox1,ibox2)%PLwhich(box(ibox1,ibox2)%PLpopul)=ibdy
  end do

  do ibdy=1,nb_JONCx
     if (.not.get_visible_JONCx(ibdy)) cycle
     coord=JC_coor(1:3,ibdy)
     ibox1=1+int((coord(1)-Bleft )*Lbox_1)
     ibox2=1+int((coord(2)-Bdown )*Lbox_1)
     if (ibox1 < minibox1 .or. ibox1 > maxibox1 .or. ibox2 < minibox2 .or. ibox2 > maxibox2) then
       write(cout,'(A,I0,A,I0)')' maxibox1=',maxibox1,'maxibox2=',maxibox2
       write(cout,'(A,I0,A,I0)')'    ibox1=',ibox1,   '   ibox2=',ibox2
       write(cout,'(A,I0,A,I0)')' minibox1=',minibox1,'minibox2=',minibox2
       write(cout,'(A13,I5,A13)')'  body JONCx ',ibdy,' out of boxes'
       call faterr(IAM,cout)
     end if
     box(ibox1,ibox2)%JCpopul=box(ibox1,ibox2)%JCpopul+1
     if( box(ibox1,ibox2)%JCpopul > size(box(ibox1,ibox2)%JCwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for JC.")
     end if
     box(ibox1,ibox2)%JCwhich(box(ibox1,ibox2)%JCpopul)=ibdy
   end do  
  
  ! Detecting contacts; 
  ! contacts are being detected within a box and immediate surrounding boxes;  
  
  ! first reading: sizing array this
  
   nb_rough_PLJCx=0
  
  ! création de la liste de paire à examiner
  
  ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
  ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  nullify(Root) 
  nullify(Current)
  nullify(Previous)

  do ibox1cd=minibox1,maxibox1  
  do ibox2cd=minibox2,maxibox2
    do icdpop=1,box(ibox1cd,ibox2cd)%PLpopul
      icdtac=box(ibox1cd,ibox2cd)%PLwhich(icdpop)   
      cdcol=get_color_POLYG(icdtac)
      ! box loop investigating antagonist diskx
      do ibox1an=max(minibox1,ibox1cd-1),min(maxibox1,ibox1cd+1)                        
      do ibox2an=max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   
        do ianpop=1,box(ibox1an,ibox2an)%JCpopul
          iantac=box(ibox1an,ibox2an)%JCwhich(ianpop)
          ancol   = get_color_JONCx(iantac)
          anbdyty = get_body_model_name_from_id(joncx2bdyty(3,iantac))
          cdbdyty = get_body_model_name_from_id(polyg2bdyty(3,icdtac))
          isee=get_isee(cdbdyty,'POLYG',cdcol,anbdyty,'JONCx',ancol)
          ! if contactors are seeing each other
          if (isee /= 0) then
            adist=see(isee)%alert 
            ! checking ROUGHLY distance against alert distance           
            coordcd = PL_coor(:,icdtac)
            coordan = JC_coor(:,iantac)
            raycd = get_radius_POLYG(icdtac)
            axean = get_axes_JONCx(iantac)
            ax1=axean(1)
            ax2=axean(2)

            ! est on dans la zone d'alerte suivant la normale
            dist=raycd+ax2+adist

            norm2= (coordan(1)-coordcd(1))*tmp_jonc(iantac)%N(1)&
                  +(coordan(2)-coordcd(2))*tmp_jonc(iantac)%N(2)
   
            if (dabs(norm2)<dist) then

              ! est on dans la zone d'alerte suivant la tangente ?
              dist=raycd+ax1+adist

              norm1=(coordan(1)-coordcd(1))*tmp_jonc(iantac)%T(1) &
                   +(coordan(2)-coordcd(2))*tmp_jonc(iantac)%T(2)

              if (dabs(norm1)<dist) then
                nb_rough_PLJCx=nb_rough_PLJCx+1
                if ( nb_rough_PLJCx == 1) then
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

!fd on est du cote oppose a la normale on calcule la projection 
!fd du centre du polygone sur le joncx
                if (norm2>=0.D0) then
                  Current%val%N(1:2)        = -tmp_jonc(iantac)%N(1:2)
                  Current%val%point(1:2)    = coordcd(1:2)+(norm2-ax2)*tmp_jonc(iantac)%N(1:2)

!fd on est du cote de la normale
                else
                  Current%val%N(1:2)        = tmp_jonc(iantac)%N(1:2)
                  Current%val%point(1:2)    = coordcd(1:2)+(norm2+ax2)*tmp_jonc(iantac)%N(1:2)
                endif

                Current%val%ax1=axean(1)
                Current%val%ax2=axean(2)

                Current%p => Previous
                nullify(Current%n)
                Previous => Current
              endif
            endif
          end if
        enddo
      enddo
      end do
    end do
  end do
  end do

  write(cout,'(4X,I10,A20)') nb_rough_PLJCx,' PLJCx roughly found'
  call logmes(cout)

! on s'alloue la table de visibilité utilisée dans compute_contact
  if (allocated(rough_PLJCx)) deallocate(rough_PLJCx)
  allocate(rough_PLJCx(nb_rough_PLJCx))              
  
! on s'alloue un tableau temporaire de contact.On lui donne une taille 2*nb_rough_pljcx
! car il y a au maximun deux points de contact entre un candidat - antagoniste
  if (allocated(this)) deallocate(this)
  allocate(this(2*nb_rough_PLJCx))
                                            
  do i=nb_rough_PLJCx,1,-1
     
     Previous => Current%p
     rough_PLJCx(i)%cd         = Current%val%cd
     rough_PLJCx(i)%an         = Current%val%an
     rough_PLJCx(i)%isee       = Current%val%isee
     rough_PLJCx(i)%point(1:2) = Current%val%point(1:2)
     rough_PLJCx(i)%N(1:2)     = Current%val%N(1:2)
     rough_PLJCx(i)%ax1        = Current%val%ax1
     rough_PLJCx(i)%ax2        = Current%val%ax2
     
     raycd = get_radius_POLYG(Current%val%cd)
     
     rough_PLJCx(i)%reff = raycd
      
     masscd=get_mass_POLYG(polyg2bdyty(1,Current%val%cd))
     
     rough_PLJCx(i)%meff = masscd
      
     deallocate(Current)
     Current => Previous
   end do 
   
   nullify(Root)
   
end subroutine creation_tab_visu_PLJCx
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
subroutine compute_contact_pljcx
 
   implicit none  

   integer                               :: errare 

   type(T_POLYG)                         :: PLibdy,PLicdbdy

   integer                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
   integer                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty
   character(len=5)                      :: cdtac,cdcol,antac,ancol
   real(kind=8),dimension(3)             :: coord,coordcd,coordan,cd_Vbegin,an_Vbegin
   real(kind=8)                          :: ax1,ax2,yan,adist,dist,nonuc,gap,ut1,ut2,un1,un2,Gant3,Gann3,raycd

   integer                               :: i,id,j,nb_ctc
   real(kind=8)                          :: norme         ! scalaire contenant la norme de sep
   real(kind=8),dimension(2,2)           :: xco
   real(kind=8),dimension(2)             :: ovlap,cdlev,anlev,t,N,pt,cd_shift
   integer,dimension(2)                  :: vertex_candidat

   integer                               :: cd_ent,an_ent

   integer :: i1,i2
   real(kind=8) ::norme1,norme2

   integer :: nb_polyg

   character(len=80) :: cout

   icdan=0        
   nb_PLJCx=0
   nb_adj=0

   if (nb_rough_PLJCx /= 0 ) then
!
! preparation de la detection 
!
    icdtac=1 ! pour l'instant, c'est ok...
    iantac=1

    do i=1,nb_rough_PLJCx
      icdbdy   = rough_PLJCx(i)%cd
      ianbdy   = rough_PLJCx(i)%an
      N        = rough_PLJCx(i)%N
      pt       = rough_PLJCx(i)%point
      PLicdbdy = get_l_POLYG(icdbdy)
      !JCianbdy=get_JONCx(ianbdy)
      isee     = rough_PLJCx(i)%isee  
      adist=see(isee)%alert 
      coordcd = PL_coor(:,icdbdy)
      coordan = JC_coor(:,ianbdy)

      !fd c'est du luxe
      norme=dsqrt(N(1)*N(1)+N(2)*N(2))
      N=N/norme

      nb_ctc=0

      norme1= 1.d20 ; i1=0  ! le plus proche 
      norme2= 1.d20 ; i2=0  ! le second plus proche

      do j=1,PLicdbdy%nb_vertex

! on ejecte les vertex tels que le produit 
! scalaire entre les vecteur 
! (centre-vertex) et normale au jonc soit negatif
!

        norme = N(1)*(PLicdbdy%vertex(1,j)-coordcd(1)) &
               +N(2)*(PLicdbdy%vertex(2,j)-coordcd(2))


        if (norme.ge.0.D0) cycle


        T(1) = N(2) ; T(2)= -N(1) 

        norme = T(1)*(PLicdbdy%vertex(1,j)-coordan(1)) &
               +T(2)*(PLicdbdy%vertex(2,j)-coordan(2))


        if (dabs(norme).gt.rough_PLJCx(i)%ax1) cycle

! ensuite il ne faut garder que les 2 plus proches 

        norme=N(1)*(PLicdbdy%vertex(1,j)-pt(1)) &
             +N(2)*(PLicdbdy%vertex(2,j)-pt(2))

        if (norme < adist) then

          if (norme < norme1) then
            !on shift 1 dans 2
            i2=i1; norme2=norme1
            !on garde le nouveau 1
            i1=j; norme1=norme
          else if ( norme >= norme1 .and. norme < norme2) then 
            !on garde le nouveau 2
            i2=j; norme2=norme
          endif

        endif     
      enddo

      if (i1 /= 0) then
        nb_ctc=nb_ctc+1

        xco(1:2,nb_ctc)=PLicdbdy%vertex(1:2,i1)
        ovlap(nb_ctc)=norme1
        vertex_candidat(nb_ctc)=i1

        if (i2 /=0) then
          nb_ctc=nb_ctc+1
          xco(1:2,nb_ctc)=PLicdbdy%vertex(1:2,i2)
          ovlap(nb_ctc)=norme2
          vertex_candidat(nb_ctc)=i2
        endif
      endif

      do j=1,nb_ctc
        icdan=icdan+1
        this(icdan)%icocdan = 0
        this(icdan)%dct     = 0

        if(j == 2)then
          this(icdan-1)%dct= 1
          this(icdan)%dct  = 1
          this(icdan-1)%icocdan = icdan
          this(icdan)%icocdan   = icdan-1
        endif

        nb_adj(icdbdy)=nb_adj(icdbdy)+1
        iadj   = nb_adj(icdbdy)                            

        this(icdan)%iadj    = iadj

        this(icdan)%icdbtac = polyg2bdyty(2, icdtac)
        this(icdan)%ianbtac = joncx2bdyty(2, iantac)

        this(icdan)%icdbtyp = polyg2bdyty(3, icdtac)
        this(icdan)%ianbtyp = joncx2bdyty(3, iantac)

        this(icdan)%icdctyp = i_polyg
        this(icdan)%ianctyp = i_joncx

        this(icdan)%icdbdy  = polyg2bdyty(1, icdbdy)
        this(icdan)%icdtac  = icdbdy
        this(icdan)%icdsci  = vertex_candidat(j)
        this(icdan)%ianbdy  = joncx2bdyty(1, ianbdy)
        this(icdan)%iantac  = ianbdy
        this(icdan)%iansci  = 0
        this(icdan)%isee    = isee

        cd_shift = get_shiftTT_POLYG(icdtac)

        cd_ent = get_ent_POLYG(this(icdan)%icdtac)
        an_ent = get_ent_JONCx(this(icdan)%iantac)

        this(icdan)%icdent = cd_ent
        this(icdan)%ianent = an_ent

        if (cd_ent /= an_ent) then
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
          entity(an_ent)%nb = entity(an_ent)%nb+1
        else
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
        end if

        this(icdan)%coor(1) = xco(1, j)
        this(icdan)%coor(2) = xco(2, j)
                   
        this(icdan)%reff    = rough_PLJCx(i)%reff
        this(icdan)%meff    = rough_PLJCx(i)%meff

        cdlev = xco(1:2,j) + cd_shift(1:2) - PL_coor(1:2, icdbdy)
        anlev = xco(1:2,j) - JC_coor(1:2, ianbdy)

        this(icdan)%nuc(1:2) =  N
        t(1)=N(2);t(2)=-N(1)
        this(icdan)%tuc(1:2) =  t 
        this(icdan)%Gcdt3    = -cdlev(2)*t(1) + cdlev(1)*t(2)
        this(icdan)%Gcdn3    = -cdlev(2)*n(1) + cdlev(1)*n(2)
        this(icdan)%Gant3    = -anlev(2)*t(1) + anlev(1)*t(2)
        this(icdan)%Gann3    = -anlev(2)*n(1) + anlev(1)*n(2)

        cd_Vbegin = get_Vbegin_POLYG(icdtac)
        an_Vbegin = get_Vbegin_JONCx(iantac)

        this(icdan)%vltBEGIN  = ( cd_Vbegin(1)-an_Vbegin(1) ) * t(1) &
                              + ( cd_Vbegin(2)-an_Vbegin(2) ) * t(2) &
                              + cd_Vbegin(3)*this(icdan)%Gcdt3       &
                              - an_Vbegin(3)*this(icdan)%Gant3

        this(icdan)%vlnBEGIN  = ( cd_Vbegin(1)-an_Vbegin(1) ) * N(1) &
                              + ( cd_Vbegin(2)-an_Vbegin(2) ) * N(2) &
                              + cd_Vbegin(3)*this(icdan)%Gcdn3       &
                              - an_Vbegin(3)*this(icdan)%Gann3


        this(icdan)%gapTTBEGIN =  ovlap(j)

        this(icdan)%rlt        = 0.d0
        this(icdan)%rln        = 0.d0
        this(icdan)%vlt        = this(icdan)%vltBEGIN
        this(icdan)%vln        = this(icdan)%vlnBEGIN
        this(icdan)%gapTT      = this(icdan)%gapTTbegin
        this(icdan)%status     = i_nknow
      enddo
    enddo

    nb_PLJCx=icdan

  endif

  write(cout,'(1X,I10,A12)') nb_PLJCx,' PLJCx found'
  call logmes(cout)

  nb_POLYG = get_nb_POLYG()

  do ibdy=1,nb_POLYG
    if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
    if (nb_adj(ibdy) /= 0) then
      allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      if (errare /=0 ) then
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_PLJCx::compute_contact',cout)
      end if
    endif
  enddo

  do icdan=1,nb_PLJCx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
  end do

  
  do icdan = 1, nb_PLJCx
     call get_behaviour_( icdan, see, tact_behav )
  end do

  if (allocated(violation)) deallocate(violation)
  allocate(violation(nb_PLJCx),stat=errare)
   
end subroutine compute_contact_pljcx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine display_prox_tactors_PLJCx

   implicit none
   integer :: iadj,icdan,icdbdy,jbdycd,icdtac,ianbdy,iantac,isee,icdver,itact
   integer :: nb_polyg
   character(len=5) :: cdmodel, anmodel

   if (nb_PLJCx == 0) return

   nb_POLYG = get_nb_POLYG()   

   do itact=1,nb_POLYG    
     do iadj=1,nb_adj(itact)         
       icdan  = adjac(itact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       icdver = this(icdan)%icdsci
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( joncx2bdyty(3,iantac) )

       write(*,'(A1)')' '
       write(*,'(A6,2X,I5)')'$icdan',icdan
                       !123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
       write(*,'(A90)')' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr'
       write(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,12x)')   &
       cdmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',icdver, &
       see(this(icdan)%isee)%behav,  &
       anmodel,joncx2bdyty(1,iantac),'JONCx',joncx2bdyty(2,iantac)
       write(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       write(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       write(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
       write(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       write(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBEGIN
       write(*,'(A1)')' '               
     end do                           
   end do

104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_PLJCx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine stock_rloc_PLJCx
 
   !  
   ! get data from this and put into verlt
   !           
 
   implicit none

   integer :: errare
   integer :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   integer :: nb_polyg

   character(len=80) :: cout
                              !123456789012345678912
   character(len=22) :: IAM = 'mod_PLJCx::stock_rloc'

    nb_POLYG = get_nb_POLYG() 
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
   do icdan=1,nb_PLJCx

     icdtac = this(icdan)%icdtac ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac ! serial number of antagonist contactor for contact icdan
     iadj   = this(icdan)%iadj   ! serial adjacent number of pair body-contactor
                                 ! adjacent to candidate body for contact icdan
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdmodel         = polyg2bdyty(3,icdtac)
     verlt(icdtac)%cdbdy           = polyg2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = polyg2bdyty(2,icdtac)
     verlt(icdtac)%cdsci(iadj)     = this(icdan)%icdsci
     verlt(icdtac)%anmodel(iadj)   = joncx2bdyty(3,iantac)
     verlt(icdtac)%anbdy(iadj)     = joncx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = joncx2bdyty(2,iantac)
     verlt(icdtac)%ansci(iadj)     = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)       = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)       = this(icdan)%rln/H
     verlt(icdtac)%status(iadj)    = this(icdan)%status
     verlt(icdtac)%vlt(iadj)       = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)       = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)     = this(icdan)%gapTT
     verlt(icdtac)%nuc(1:2,iadj)   = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj)  = this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   end do

   nb_vPLJCx = nb_PLJCx

   WRITE(cout,'(1X,I10,A12)') nb_vPLJCx,' stock PLJCx'
   call logmes(cout)

 end subroutine stock_rloc_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine recup_rloc_PLJCx

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   implicit none
   integer :: icdan,icdtac,icdver,iantac,ianseg,iadj
   character(len=21) :: IAM = 'mod_PLJCx::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   if (nb_PLJCx == 0) return  

   nb_recup_PLJCx=0

   do icdan=1,nb_PLJCx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     icdver = this(icdan)%icdsci
     iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        
     if (verlt(icdtac)%adjsz /= 0) then
       if (verlt(icdtac)%cdmodel== polyg2bdyty(3,icdtac) .and. &
           verlt(icdtac)%cdbdy  == polyg2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == polyg2bdyty(2,icdtac)       &
          ) then
          do iadj = 1 , verlt(icdtac)%adjsz
            if (verlt(icdtac)%cdsci(iadj)  == icdver                .and. &
                verlt(icdtac)%anmodel(iadj)== joncx2bdyty(3,iantac) .and. &
                verlt(icdtac)%anbdy(iadj)  == joncx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == joncx2bdyty(2,iantac)       &
               ) then
               this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
               this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

               this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)

               nb_recup_PLJCx = nb_recup_PLJCx + 1
               exit
            end if
          end do
       end if
     endif
   end do

   write(cout,'(1X,I10,A12)') nb_recup_PLJCx,' recup PLJCx'
   call logmes(cout)

 end subroutine recup_rloc_PLJCx
!------------------------------------------------------------------------ 
 subroutine read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   implicit none

   integer(kind=4)                   :: icdan,icdbdy,icdtac,icdver,ianbdy,iantac
   integer(kind=4)                   :: iadj,icdtact,cdmodel,anmodel
   real(kind=8)                      :: rlt,rln,vlt,vln,gapTT
   real(kind=8),dimension(2)         :: nuc,coor
   character(len=5)                  :: cdbdy,cdtac,cdver,anbdy,antac,behav,sttus
   integer                           :: errare 
  
   integer :: ibehav,nb_internal,i_internal
   integer :: nb_polyg

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_PLJCx::read_ini_Vloc_Rloc'


   nb_POLYG=get_nb_POLYG()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contacts have to be selected.  
  ! For this purpose nb_adj is introduced.

   if (.not. allocated(nb_adj)) then
     allocate(nb_adj(nb_POLYG),stat=errare)
     if (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   end if    

   nb_adj=0
   do
     if ( .not. read_G_clin()) exit
     if (G_clin(2:6) /= 'icdan') cycle                  ! fishing for the keyword 'icdan'
     if (G_clin(9:13)/= 'PLJCx') cycle     
     if ( .not. read_G_clin()) exit
     if ( .not. read_G_clin()) exit
     read(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,5X,2X,5X,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,               & ! no anseg and ianseg for PLJCx contrary to PLJCx
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )

     if (cdtac /= 'POLYG' .or. antac /= 'JONCx') cycle
     do icdtact=1,nb_POLYG
       if (polyg2bdyty(3,icdtact) == cdmodel .and. &
           polyg2bdyty(1,icdtact) == icdbdy  .and. &
           polyg2bdyty(2,icdtact) == icdtac  ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
     cycle
   end do   

   if (.not. allocated(verlt)) then

     allocate(verlt(nb_POLYG),stat=errare)
     if (errare /=0 ) then
       call faterr(IAM,'Error allocating verlt')
     end if
     do icdbdy=1,nb_POLYG
       verlt(icdbdy)%adjsz=0
       iadj=nb_adj(icdbdy)
       if (iadj > 0) then
         verlt(icdbdy)%adjsz=iadj
         call new_verlet_(icdbdy, iadj, errare)
         if (errare /=0 ) then
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdbdy,')%.....'
           call faterr(IAM,cout)
         end if
       else
         call nullify_verlet_(icdbdy)
       endif
     end do
   else 
     do icdbdy=1,nb_POLYG
       call free_verlet_(icdbdy)
       verlt(icdbdy)%adjsz=0
       iadj=nb_adj(icdbdy)

       if (iadj > 0) then
         verlt(icdbdy)%adjsz=iadj
         call new_verlet_(icdbdy, iadj, errare)
       else
         call nullify_verlet_(icdbdy)
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
     if (G_clin(9:13)/= 'PLJCx') cycle     
     if ( .not. read_G_clin()) exit
     if ( .not. read_G_clin()) exit

     read(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,5X,2X,5X,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,               & !anseg,ianseg,  &
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )

     if (cdtac /= 'POLYG' .and. antac /= 'JONCx') cycle
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
         exit 
         verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
         ibehav = get_ibehav(behav)
         nb_internal = get_nb_internal(ibehav)
         if (nb_internal /= 0 ) then  
           if( .not. read_G_clin()) exit
           do i_internal=1, nb_internal
             read(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
           enddo
         endif



       end if
     enddo
     cycle
   end do

   nb_vPLJCx=0

   do icdtact=1,nb_POLYG
     nb_vPLJCx = nb_vPLJCx + nb_adj(icdtact)

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
   
   if (nb_PLJCx==0) return

   nb_POLYG=get_nb_POLYG()

   do icdtact=1,nb_POLYG   
     do iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( joncx2bdyty(3,iantac) )

       write(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PLJCx',icdan
       !1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
       ! RBDY2  12345  POLYG  12345  CDVER  12345  BEHAV  RBDY2  12345  JONCx  12345                STTUS 12345
       write(nfich,'(A103)') &
       ' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj '
       write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,16X,A5,1X,I5)')   &
       cdmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',this(icdan)%icdsci, &
       see(this(icdan)%isee)%behav,  &
       anmodel,joncx2bdyty(1,iantac),'JONCx',joncx2bdyty(2,iantac), &
       get_contact_status_name_from_id(this(icdan)%status),iadj
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
     end do                           
   end do

103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine write_out_Vloc_Rloc
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine nullify_reac_PLJCx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
    
   icdtac=this(icdan)%icdtac
   call nullify_reac_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_reac_JONCx(iantac,storage)
    
 end subroutine nullify_reac_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine nullify_vlocy_PLJCx(icdan,storage)

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
    
   icdtac=this(icdan)%icdtac
   call nullify_vlocy_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   call nullify_vlocy_JONCx(iantac,storage)
    
 end subroutine nullify_vlocy_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine vitrad_PLJCx( icdan, storage, need_full_V )

   implicit none
   integer,intent(in) :: icdan 
   integer            :: icdtac,iantac
   integer            :: storage
   logical, optional  :: need_full_V
    
  icdtac=this(icdan)%icdtac
  call comp_vlocy_POLYG(icdtac,storage)
    
  iantac=this(icdan)%iantac
  call comp_vlocy_JONCx(iantac,storage)
    
 end subroutine vitrad_PLJCx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 subroutine injj_PLJCx(icdan,RTIK,RNIK,storage)
 
   implicit none
   integer     ,intent(in)    :: icdan
   real(kind=8),intent(in)    :: RTIK,RNIK
   integer,     dimension(3)  :: cdccdof,anccdof
   real(kind=8),dimension(3)  :: cdreac, anreac
   integer                    :: icdtac,iantac
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

   call add_reac_POLYG(this(icdan)%icdtac,cdccdof,cdreac,storage)
   call add_reac_JONCx(this(icdan)%iantac,anccdof,anreac,storage)

 end subroutine injj_PLJCx 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 subroutine prjj_PLJCx(icdan,VTIK,VNIK,storage)
 
   implicit none
   integer     ,intent(in)   :: icdan
   real(kind=8),intent(out)  :: VTIK,VNIK
   real(kind=8),dimension(3) :: Vcd,Van

   real(kind=8),dimension(2) :: Vd

   integer(kind=4)             :: icdbdy
   integer(kind=4), intent(in) :: storage
   
   icdbdy=this(icdan)%icdbdy
   
   call get_vlocy_POLYG(this(icdan)%icdtac,storage,Vcd)
   call get_vlocy_JONCx(this(icdan)%iantac,storage,Van)


   if (storage == iVfree) then
     call get_Vd_POLYG(icdbdy,this(icdan)%coor,Vd)
   else
     Vd=0.d0
   endif

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3 &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3 &
        +Vd(1)*this(icdan)%tuc(1)+Vd(2)*this(icdan)%tuc(2)

   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3  &
        +Vd(1)*this(icdan)%nuc(1)+Vd(2)*this(icdan)%nuc(2)

 end subroutine prjj_PLJCx 
!!$!------------------------------------------------------------------------ 
!!$ subroutine compute_Wikik_PLJCx(icdan,WTT,WTN,WNT,WNN)
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
!!$  ianmass = get_inv_mass_JONCx(ianbdy)
!!$
!!$  WTT =  icdmass(1)+icdmass(3)*this(icdan)%Gcdt3*this(icdan)%Gcdt3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gant3*this(icdan)%Gant3
!!$  WNN =  icdmass(1)+icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdn3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gann3*this(icdan)%Gann3
!!$  WTN =  icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdt3 &
!!$       + ianmass(3)*this(icdan)%Gann3*this(icdan)%Gant3
!!$  WNT = WTN
!!$
!!$ end subroutine compute_Wikik_PLJCx
!!$!------------------------------------------------------------------------ 
!!$ subroutine get_Wik_PLJCx(icdan,ikcd,tik,nik,ikcdmass,ikGcdt,ikGcdn)
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
!!$  ikcdmass= get_inv_mass_POLYG(ikcd)
!!$  tik     = this(icdan)%tuc
!!$  nik     = this(icdan)%nuc
!!$
!!$ end subroutine get_Wik_PLJCx
!!$!------------------------------------------------------------------------ 
!!$ subroutine compute_Wikjl_PLJCx(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$  implicit none
!!$  integer                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy
!!$  real(kind=8)              :: WTT,WTN,WNT,WNN
!!$  real(kind=8),dimension(3) :: icdmass,ianmass,jcdmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$  jcdbdy=this(jcdan)%icdbdy
!!$
!!$  icdmass = get_inv_mass_POLYG(icdbdy)
!!$  ianmass = get_inv_mass_JONCx(ianbdy)
!!$  jcdmass = get_inv_mass_POLYG(icdbdy)
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
!!$ end subroutine compute_Wikjl_PLJCx
!------------------------------------------------------------------------ 
 subroutine compute_stress_PLJCx
   implicit none
   integer(kind=4)             :: icdan,ID_RBDY2,ID_TACTY
   integer(kind=4)             :: icdbdy,icdtac
   real(kind=8),dimension(2)   :: Fik,Lcd,coor
   real(kind=8),dimension(2,2) :: SIGMA
   real(kind=8),dimension(3)   :: coorTT

   do icdan=1,nb_PLJCx

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

   end do

 end subroutine compute_stress_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_nb_PLJCx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_PLJCx = nb_PLJCx
   case(i_verlet_tactor)
      get_nb_PLJCx = nb_vPLJCx
   case(i_rough_tactor)
      get_nb_PLJCx = nb_rough_PLJCx
   case(i_recup_tactor)
      get_nb_PLJCx = nb_recup_PLJCx
   end select

 end function get_nb_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
subroutine PLJCx2POLYG(icdan,icdtac)

   implicit none
   integer          :: icdan,icdtac
   
   icdtac = this(icdan)%icdtac

end subroutine PLJCx2POLYG
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
subroutine PLJCx2JONCx(icdan,iantac)

   implicit none
   integer          :: icdan,iantac
   
   iantac = this(icdan)%iantac

end subroutine PLJCx2JONCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer(kind=4) function get_type_PLJCx(icdan)
   implicit none
   integer :: icdan,type

   get_type_PLJCx = this(icdan)%dct
   
 end function get_type_PLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------------------------------
 subroutine get_numcorps_PLJCx(icdan,icdbdy,ianbdy)

   implicit none

   integer          :: icdan,icdbdy,ianbdy

   icdbdy   = this(icdan)%icdbdy
   ianbdy   = this(icdan)%ianbdy

 end subroutine get_numcorps_PLJCx
!------------------------------------------------------------------------------------------------
 subroutine ROT_JONCx(ibdyty,X)
  implicit none
  integer :: ibdyty,itacty,k
  
!a: angle de rotation
!Tx,Ty:translation
  real(kind=8) :: a,Tx,Ty                
!cos(a) et sin(a) pr les calculer qu'une seule fois
  real(kind=8) :: c,s                    
  real(kind=8),dimension(3)   :: X

  a = X(3)

  c=cos(a);s=sin(a)

  tmp_jonc(ibdyty)%N(1)=-s
  tmp_jonc(ibdyty)%N(2)= c
  tmp_jonc(ibdyty)%T(1)= c
  tmp_jonc(ibdyty)%T(2)= s

 end subroutine ROT_JONCx
!------------------------------------------------------------------------ 
!-------------------------------------------
subroutine print_info_PLJCx(icdan)
   implicit none
   integer          :: icdan,icdtac,iantac,icdbdy,ianbdy

   character(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   write(cout,1) icdtac,iantac
   call LOGMES(cout)

1  format(1X,'POLYG:',1x,I5,1x,'JONCx:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   call print_info_POLYG(icdbdy)
   call print_info_JONCx(ianbdy)

end subroutine print_info_PLJCx
!------------------------------------------------------------------------
logical function RUN_PLJCx(fantome)

  implicit none
  integer,optional :: fantome

  RUN_PLJCx = RUN_TACTOR

end function RUN_PLJCx
!------------------------------------------------------------------------
  logical function CHECK_PLJCx()
    implicit none
    !   
    integer :: isee, nb_POLYG, nb_JONCx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PLJCx = check_PLJCx_
      return
    end if

    con_pedigree%module_name = 'PLJCx'

    con_pedigree%id_cdan  = i_pljcx
    con_pedigree%id_cdtac = i_polyg
    con_pedigree%id_antac = i_joncx

    cdtact2bdyty => polyg2bdyty
    antact2bdyty => joncx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_JONCx = get_nb_JONCx()
    nb_POLYG = get_nb_POLYG()
    if( nb_JONCx == 0 .or. nb_POLYG == 0 ) then
      CHECK_PLJCx = check_PLJCx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'POLYG' .and. see(isee)%antac == 'JONCx') then
        check_PLJCx_ = .true.
        exit
      end if
    end do
  
    CHECK_PLJCx = check_PLJCx_
    return
  
  end function CHECK_PLJCx
!------------------------------------------------------------------------
  logical function get_write_Vloc_Rloc_PLJCx(fantome)

    implicit none
    integer,optional :: fantome

    get_write_Vloc_Rloc_PLJCx = write_Vloc_Rloc

  end function get_write_Vloc_Rloc_PLJCx
!------------------------------------------------------------------------
 subroutine get_eff_PLJCx(icdan,meff,reff)
  implicit none

   integer      :: icdan
   real(kind=8) :: meff,reff

   reff = this(icdan)%reff

   meff = this(icdan)%meff

 end subroutine get_eff_PLJCx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 real(kind=8) function get_length_PLJCx(icdan)

! fd le 04/11/07
! calcul de la longueur d'un contact PLJC
! soit 2 points -> distance entre les 2 points /2
! soit 1 point  -> rayon effectif (idem spheres) * 2*Pi/36 == la longueur de 10deg

   implicit none

   integer,intent(in)        :: icdan
   real(kind=8)              :: raycd,rayan
   real(kind=8),dimension(2) :: bord

   if (this(icdan)%dct /= 0) then

     bord = this(icdan)%coor - this(this(icdan)%icocdan)%coor

     get_length_PLJCx = 0.5*(dsqrt(dot_product(bord,bord)))

   else
     raycd   = get_radius_POLYG(this(icdan)%icdtac)
     rayan   = raycd!get_radius_POLYG(this(icdan)%iantac)
     get_length_PLJCx = ((raycd*rayan)/(rayan+raycd)) * PI_g/18.d0
   endif

 end function get_length_PLJCx
!------------------------------------------------------------------------
 subroutine get_beta_PLJCx(icdtac,iadj,beta) 

   implicit none
   integer     :: icdtac,iadj
   real(kind=8):: beta

!fd burk: il faudrait verifier qu'on n'a pas n'importe quoi dans cette valeur !!

   beta=verlt(icdtac)%internal(4,iadj)

 end subroutine get_beta_PLJCx
!------------------------------------------------------------------------
 subroutine get_g2l_PLJCx(icdan,g2l)

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
   
 end subroutine get_g2l_PLJCx


 ! rm : functions for siconos wrapper

 function get_old_index_PLJCx(icdan)
   implicit none
   integer(kind=4), intent(in) :: icdan
   integer(kind=4) :: get_old_index_PLJCx
   !
   integer :: icdtac,iantac,iadj

   get_old_index_PLJCx = 0

   if (.not. allocated(verlt)) then
      return
   endif
   
   icdtac = this(icdan)%icdtac ! serial number of candidate contactor for contact icdan
   iantac = this(icdan)%iantac ! serial number of antagonist contactor for contact icdan 

   if (verlt(icdtac)%adjsz /= 0) then
      if ( verlt(icdtac)%cdmodel== polyg2bdyty(3,icdtac) .and. &
           verlt(icdtac)%cdbdy  == polyg2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == polyg2bdyty(2,icdtac)       &
         ) then
         do iadj=1,verlt(icdtac)%adjsz

            if ( verlt(icdtac)%cdsci(iadj)  == this(icdan)%icdsci    .and. &
                 verlt(icdtac)%anmodel(iadj)== joncx2bdyty(3,iantac) .and. &
                 verlt(icdtac)%anbdy(iadj)  == joncx2bdyty(1,iantac) .and. &
                 verlt(icdtac)%antac(iadj)  == joncx2bdyty(2,iantac)       &
            ) then
              get_old_index_PLJCx = verlt(icdtac)%icdan(iadj)
              exit
            end if
         end do
      end if
   end if

 end function get_old_index_PLJCx

 function get_icdtac_PLJCx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_PLJCx
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
         get_icdtac_PLJCx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLJCx::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_PLJCx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_PLJCx
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
         get_iantac_PLJCx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLJCx::get_icdtac','unknown contact index')
   

   get_iantac_PLJCx = this(icdan)%iantac

 end function get_iantac_PLJCx

 subroutine clean_memory_PLJCx
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_PLJCx  = 0
   nb_vPLJCx = 0

   if( allocated(rough_PLJCx) ) deallocate(rough_PLJCx)

   nb_rough_PLJCx = 0
   nstep_rough_seek_PLJCx = 1
   nb_recup_PLJCx = 0

   RUN = .false.

   if( allocated(PL_coor) ) deallocate(PL_coor)
   if( allocated(JC_coor) ) deallocate(JC_coor)

   Reac_PLJCx_MAX = 0.D0

   module_checked_ = .FALSE.
   check_PLJCx_    = .FALSE.

 end subroutine clean_memory_PLJCx
!!!------------------------------------------------------------------------  

 subroutine set_nb_PLJCx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PLJCx = nb

 end subroutine

 subroutine redo_nb_adj_PLJCx()
   implicit none

   call redo_nb_adj_( get_nb_POLYG() )

 end subroutine

  !> \brief Set friction model for simulation using evolutive friction
  subroutine set_friction_model_PLJCx(FLAG)
    implicit none
    character(len=3) :: FLAG
    character(len=29) :: IAM
    IAM = 'mod_PLJCx::set_friction_model'
 
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
    
  end subroutine set_friction_model_PLJCx
!!!------------------------------------------------------------------------  
  subroutine update_fric_PLJCx(icdan,fric)
    implicit none
    integer(kind=4) :: icdan,icdtact,iantact,isect
    real(kind=8)    :: WScd,WSan,fric
    
    if (nb_WSsect .eq. 1)then
       isect = 1
       icdtact     = this(icdan)%icdtac
       iantact     = this(icdan)%iantac
    
       WScd = get_WS_POLYG(polyg2bdyty(1,icdtact),polyg2bdyty(2,icdtact),isect)
       WSan = get_WS_JONCx(joncx2bdyty(1,iantact),joncx2bdyty(2,iantact),isect)
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
    
  end subroutine update_fric_PLJCx

end module PLJCx
