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
MODULE PRPRx

  USE overall
  USE utilities
  USE tact_behaviour
  USE POLYR

  USE DiscreteGeometry, only : nodetonode_distance      , &
                               nodetoedge_distance      , &
                               nodetoface_distance      , &
                               edgetoedge_distance      , &
                               edgetoface_distance_wp   , &
                               node_triangle_projection , &
                               get_nodal_normals_HE_Hdl , &
                               node_HE_Hdl_proximity    , &
                               new_node_HE_Hdl_proximity, &
                               polytopes_intersection_wc, &
                               comp_rep, convex_hull    , &
                               display_polytope

  USE ExternalDetection

  use algebra, only : cross_product, &
                      length3

  use polygon, only : envelope, &
                      polygon_principal_properties, &
                      polygon_all_points_in_wc , &
                      polygon_simplification_wc, &
                      polygons_intersection_wc

  use meca_polygon, only : compute_central_kernel, &
                           compute_stress_field

  use MAILx, only : get_color_MAILx
  use RBDY3, only : get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color

  USE parameters, only : i_prprx, i_mailx, i_rbdy3, i_mbs3

  !am : modules utilises par les fonctions gerant la DDM
  use anonymous_ptr_container, only : get_object               => get_data                   , &
                                      get_nb_objects           => get_nb_data                , &
                                      close_container          => close_ptr_container        , &
                                      add_object_to_container  => add_object_to_ptr_container, &
                                      display_object_container => display_ptr_container      , &
                                      ptr_container
  USE anonymous

  use inter_meca_3D

  implicit none

  private

  CHARACTER(len=5) :: BOBO='PRPRx'
  INTEGER          :: nb_POLYR

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  real(kind=8) :: decompression       = 1.D0     ! pourcentage de decompression autorise
  logical      :: force_f2f_detection = .FALSE.  ! force la detection f2f meme pour les faces courbes
  logical      :: force_nc_detection  = .FALSE.  ! force la detection nc meme pour les faces planes

  TYPE T_visavis
   ! tactor cd,an
   INTEGER :: cd,an,an_fac,an_pt,isee
   ! pour f2f nb_ctc_rough est le nb de points de contact detecte au premier passage
   INTEGER :: nb_ctc,nb_ctc_rough
   ! faces topo cd/an
   integer :: id_f_cd,id_f_an
   ! element supports
   integer,dimension(:),pointer :: iff_cd => null()
   integer,dimension(:),pointer :: iff_an => null()
   ! local reduced coordinates (eta,xi,zet) in (-1,+1)
   !f2f barycentric coordinates
   REAL(kind=8),DIMENSION(:,:),pointer::cd_lcoor => null()
   REAL(kind=8),DIMENSION(:,:),pointer::an_lcoor => null()
   !fd index du groupe de contacts dans le tableau this ou dans la liste de face 
   integer,dimension(:),pointer :: index
   !fd pour savoir si c'est v2v entre surfaces planes ou non
   logical :: is_flat
   real(kind=8),dimension(:),pointer :: pt_area => null()
   !**** OBSOLETE ***
   ! utilise par l'ancien gestionnaire explicite a la alcan
   !! initial position relative to the brick
   REAL(kind=8),DIMENSION(3,4)::coorcd,cooran     

   !am: group to which belong an interaction. For ddm group could be
   !   - INTRF: for a body belonging to an interface between two sub-domains
   !   - NOINT: for a body living inside a sub-domain
   integer :: group

   ! STONO include
   ! Caracteristiques geometriques
   ! centre et normale
   real(kind=8), dimension(3)            :: centre, normal, perio_shift
   ! taux de decompression imposee sur cette face de contact
   real(kind=8)                          :: decompression
   ! definition de la face de contact (polygones : points + sizes)
   ! les points de contact sont relativement au repere du POLYR antagonist
   real(kind=8), dimension(:,:), pointer :: face_ctc   => null()
   integer,      dimension(:),   pointer :: face_sizes => null()
   
 END TYPE T_visavis

 TYPE(T_visavis),DIMENSION(:),ALLOCATABLE :: visavis
 INTEGER                                  :: nb_visavis = 0

!------------------------------------------------------------------------ 

 ! nb_PRPRx = number of selected candidates POLYR against POLYR
 INTEGER,PRIVATE :: nb_PRPRx=0 ,nb_vPRPRx=0 ,nb_recup_PRPRx=0                         

!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj ! nb_adj(icdtac): number of adjacent pairs POLYR-POLYR
                                                        ! to candidate contactor POLYR icdtac.

!------------------------------------------------------------------------ 


 type(T_verlet), dimension(:), allocatable, target ::verlt

 integer, dimension(:,:), allocatable :: this2verlet

!------------------------------------------------------------------------ 

 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_PRPRx.

   INTEGER                               :: popul     ! box(ibox1,ibox2)%popul: number of disks in box ibox1,ibox2;
   
   INTEGER, DIMENSION(:), POINTER        :: which     ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 END TYPE T_box 

 TYPE(T_box), DIMENSION(:,:,:),ALLOCATABLE  :: box    ! box(ibox1,ibox2,ibox3): box with integer coordinates ibox1,ibox2,ibox3.



!------------------------------------------------------------------------

 TYPE T_rough_PRPRx                                   ! définit le type de la liste des plus proches voisins

   ! le candidat, l'antagoniste et isee pour la loi de contact 
   INTEGER                   :: cd                                  
   INTEGER                   :: an
   INTEGER                   :: isee
   REAL(kind=8),DIMENSION(3) :: Vsep

   INTEGER      :: xperiodic,yperiodic

   !am: group to which belong an interaction. For ddm group could be
   !   - INTRF: for a body belonging to an interface between two sub-domains
   !   - NOINT: for a body living inside a sub-domain
   integer :: group
 END TYPE T_rough_PRPRx

 TYPE(T_rough_PRPRx),DIMENSION(:),ALLOCATABLE   :: rough_PRPRx        ! table  de visibilité
 INTEGER                                        :: nb_rough_PRPRx     ! nombre de paire de polyedre a analyser pour determiner
                                                                      ! s'il y a contact ou pas
 INTEGER                                        :: nb_rough_half
 integer                                        :: size_factor = 4

 TYPE T_link_rough_PRPRx                                           ! liste chainee pour determiner les listes de cand_ant car
                                                                   ! on ne connait pas a priori le nb de cand-ant 
    TYPE(T_link_rough_PRPRx), POINTER :: p                         ! pointeur sur le precedent
    TYPE(T_rough_PRPRx)               :: val                       ! les valeurs
    TYPE(T_link_rough_PRPRx), POINTER :: n                         ! pointeur sur le suivant

 END TYPE T_link_rough_PRPRx

 TYPE(T_link_rough_PRPRx),POINTER                  :: Root,Current,Previous
 
!------------------------------------------------------------------------
! variables attached to surrounding boxes

 REAL (kind=8)  :: maxray, minray, maxalert, meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,minibox3,maxibox3,maxpopul
!------------------------------------------------------------------------

!------------------------------------------------------------------------
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PRcoor
 REAL(kind=8)                                    :: Reac_PRPRx_MAX=0.D0,t3=0.D0,t4=0.D0
 INTEGER,PRIVATE                                 :: ii,l_ii,iv,restart=0,prox_tactors_to_file=0,prox_tactors_from_file=0
 INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
 LOGICAL,PRIVATE                                 :: write_creation_tab_visu
 real(kind=8), dimension(:), allocatable, target :: violation
!---------------------------------------------------------------------------
! The following variables are necessary to perform post processing
 REAL(kind=8),PRIVATE                            :: detection_time
 INTEGER                                         :: nb_detection_test,nb_tot_detect,nb_shadow,nb_ctc_state
!---------------------------------------------------------------------------
 REAL(kind=8)                        :: shrink=0.d0
 REAL(kind=8),DIMENSION(3,3),PRIVATE :: fformT3
 REAL(kind=8),DIMENSION(4,4),PRIVATE :: fformQ4
!----------------------------------------------------------------------------------

 LOGICAL      :: is_first_time_f2f   = .TRUE.
 LOGICAL      :: is_explicit         = .FALSE.  ! si la detection est explicite
 REAL(KIND=8) :: eps_simplification  = 1.D-3    ! tolerance pour la simplification des polygones (dans FC_planplan)

!!!---------------------------------------------------------
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(KIND=8) :: XPERIODE = 0.D0,YPERIODE = 0.D0
  REAL(KIND=8),dimension(3) :: perio_shift ! vector containing the translation of antagonist body when
                                           ! using periodic conditions 
!!!---------------------------------------------------------


!!!---------------------------------------------------------
 logical           :: with_f2f=.FALSE., f2f_skip_small_surface=.FALSE.
 real(kind=8),save :: f2f_tol=1e-3, f2f_tol_small_surface=1.d20
 ! ugly parameter for triangle intersection detection
 integer           :: max_nb_pt_select

 !fd pour debuger un contact en particulier 
 logical :: dbg = .false.
 integer :: dbg_idcd=0,dbg_idan=0

 real(kind=8) :: tol_recup_rloc = 1.d-6

 !rm global variable allowing to set surface of contact
 !   instead of computing them
 real(kind=8), public :: point_surf_PRPRx = 0.d0
 real(kind=8), public :: line_surf_PRPRx  = 0.d0
 real(kind=8), public :: surf_surf_PRPRx  = 0.d0

 !!!--- Cundall detection param
 INTEGER      :: cundall_iter=100

 real(kind=8) :: cundall_neighbor = 1.d-1,  &  ! proportion de distance pour chercher ce qu on projete sur le cp
                 cundall_gap=1d-6,          &  ! tol sur le gap pour arreter la recherche du cp
                 cundall_mergepoint=1d-6,   &  ! tol for merging points 
                 cundall_theta_max=0.087,   &  ! 5 deg
                 cundall_theta_min=0.0017      ! 0.1 deg (6 reductions !!)

                 !fd parametrage perales ... trop fin a mon gout
                 !cundall_theta_max=0.03, &    ! < 1.7 deg
                 !cundall_theta_min=0.0002     ! < 0.01146 deg (8 reductions !!)


 !!!---- Clipper library parameters
 !> clipper shrink value of candidate
 real(kind=8) :: cp_cd_shrink = 0.d0
 !> clipper shrink value of antagonist
 real(kind=8) :: cp_an_shrink = 0.d0
 !> clipper simplification distance for intersection polygon
 real(kind=8) :: cp_delta  = 0.d0


 ! use the new implementation of the computing contact point method
 logical :: new_ccpm = .TRUE.

!!!! PTA
 logical      :: is_firstime_reaction_tracking = .true.
 logical      :: is_reaction_tracking_length_setted = .false.
 real(kind=8) :: reaction_tracking_length 
 real(kind=8) :: c_min, c_max
!!!! PTA

 logical      :: nodal_contact=.FALSE.

 logical      :: module_checked_ = .FALSE.
 logical      :: check_PRPRx_    = .FALSE.

 ! fd fix for bug in recup at restart
 logical, public :: verlet_from_file = .TRUE.

 PUBLIC &
       coor_prediction_PRPRx,&
       CHECK_PRPRx,&
       RUN_PRPRx, &
       get_write_Vloc_Rloc_PRPRx, &
       read_ini_Vloc_Rloc_PRPRx,&
       write_xxx_Vloc_Rloc_PRPRx,&
       stock_rloc_PRPRx, &
       recup_rloc_PRPRx, &
       compute_box_PRPRx, &
       creation_tab_visu_PRPRx, &
       !compute_contact_PRPRx, &
       display_prox_tactors_PRPRx,&
       get_nb_PRPRx,&
       set_cundall_iteration_PRPRx, &
       set_cundall_neighbor_PRPRx, &
       set_clipper_parameters, &
       use_old_ccpm_PRPRx, &
       set_shrink_polyr_faces_PRPRx, &
       set_size_factor_polyr_PRPRx, &
       !fd obso compute_explicit_contact_PRPRx, &
       creation_tab_visu_to_file_PRPRx, &
       creation_tab_visu_from_file_PRPRx, &
       wcp_compute_contact_PRPRx, &
       !wed_compute_contact_PRPRx, &
       set_xperiodic_data_PRPRx, &
       set_yperiodic_data_PRPRx, &
       set_f2f_tol_PRPRx, &
       set_f2f_tol_small_surface_PRPRx, &
       set_max_nb_pt_select_PRPRx, &
       nc_compute_contact_PRPRx, &
       f2f4all_compute_contact_PRPRx, &
       wti_compute_contact_PRPRx, &
       verbose_f2f_PRPRx, &
       get_nb_f2f_PRPRx, &
       get_f2f2inters_PRPRx, &
       get_f2f_outlines, &
       get_f2f_all_idata, &
!       get_xperiode_PRPRx, &
!       get_yperiode_PRPRx
       !am DDM : declaration des fonctions necessaires a la DDM
       set_interactions_to_rough_PRPRx, &
       set_anonymous_to_rough_PRPRx, &
       get_nb_INTRF_PRPRx, &
       get_list_INTRF_PRPRx, &
       pair_reaction_PRPRx, &
       set_reaction_tracking_length_PRPRx, &
       set_tol_recup_rloc_PRPRx, &
       get_interaction_vector_PRPRx, &
       set_interaction_internal_PRPRx, &
       get_interaction_internal_PRPRx, &
       get_interaction_internal_comment_PRPRx, &
       with_nodal_contact_PRPRx, &
       put_icdan_group_PRPRx, &
       get_external_pressure_PRPRx,&
       print_info_PRPRx

! liste des fonctions publiques 
!
 PUBLIC &
      nullify_reac_PRPRx      ,&
      nullify_vlocy_PRPRx     ,&
      injj_PRPRx              ,&
      prjj_PRPRx              ,&
      vitrad_PRPRx            ,& 
      PRPRx2ENT               ,&
      PRPRx2POLYR             ,&
      get_detection_time_PRPRx,&
      get_type_PRPRx          ,&
      get_surf_PRPRx

  public clean_memory_PRPRx

  !rm for handler
  public get_this             , &
         set_nb_PRPRx         , &
         redo_nb_adj_PRPRx    , &
         get_an_tacty         , &
         get_verlet_tact_lawnb

 public STO_set_explicit_detection_PRPRx, &
        STO_set_decompression_PRPRx     , &
        STO_compute_contact_PRPRx       , &
        STO_force_f2f_detection_PRPRx   , &
        STO_force_nc_detection_PRPRx    , &
        get_f2f_central_kernel          , &
        get_f2f_stress

CONTAINS

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
  include 'interaction_common_3D.f90'
  ! defines the following subroutines
  !function get_verlet_tact_lawnb( icdtac, iadj )


  !------------------------------------------------------------------------
  SUBROUTINE compute_box_PRPRx

  IMPLICIT NONE

  INTEGER                     :: isee,errare,ibdy
  REAL(kind=8)                :: ksi,eta

                           !123456789012345678
  character(len=18) :: IAM='PRPRx::compute_box' 


  nb_POLYR = get_nb_POLYR()

  ! on ne fait ici que les choses qui changent lorsque nb_POLYR change

  minray     = get_min_radius_POLYR()
  maxray     = get_max_radius_POLYR()

  IF (minray > maxray ) CALL FATERR(IAM,'issue computing minray and maxray')

  IF (minray == 0.d0 ) CALL FATERR(IAM,' minray can t be equal to zero')

  ! computing largest alert distance between disks 
  maxalert=0.D0  
  DO isee=1,SIZE(see)
    IF (see(isee)%cdtac == 'POLYR' .AND. see(isee)%antac == 'POLYR') THEN
      maxalert=MAX(maxalert,see(isee)%alert)
    END IF
  END DO

  Lbox   = 1.05D0*(2.D0*maxray + maxalert)

  !fd je ne comprend pas bien a quoi ca sert ca !?

  IF (ABS(maxray-minray)<1.D-4) Lbox = 2.D0*Lbox

  norm   = Lbox/minray

  !   print*,"minray  = ",minray
  !   print*,"maxray  = ",maxray
  !   print*,"maxalert= ",maxalert
  !   print*,"LBox    = ",Lbox
  !   print*,"norm    = ",norm,INT(norm)

  Lbox_1 = 1.D0/Lbox
   
  minibox1= 1
  minibox2= 1
  minibox3= 1
  
  maxpopul = (1+INT(norm))*(1+INT(norm))*(1+INT(norm))

  !fd le 16/12/07 au cas ou maxpopul passe en neg car c'est un i4 et que
  !fd sur des grands domaines ca ne suffise pas:

  if (maxpopul < 0) maxpopul = nb_POLYR


  !for each box maxpopul is less than the total number of POLYR
  maxpopul=MIN(maxpopul,nb_POLYR)

  !print*,"maxpopul= ",maxpopul

  IF (.NOT. ALLOCATED(adjac)) THEN
    ALLOCATE(adjac(nb_POLYR),stat=errare)
    IF (errare /=0 ) CALL FATERR(IAM,'error in allocating adjac')

    DO ibdy=1,nb_POLYR
      NULLIFY(adjac(ibdy)%icdan)
    END DO
  ELSE
    DO ibdy=1,nb_POLYR
      IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
      NULLIFY(adjac(ibdy)%icdan)
    ENDDO
  ENDIF  
  
  IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
  ALLOCATE(nb_adj(nb_POLYR),stat=errare)
  IF (errare /=0 ) CALL FATERR(IAM,' error allocating nb_adj')

  nb_adj=0

  ! PRcoor are coordinates of bodies to be used in selecting prox tactors

  IF (ALLOCATED(PRcoor)) DEALLOCATE(PRcoor)
  ALLOCATE(PRcoor(3,nb_POLYR),stat=errare)

  ! TODO virer ca

   !fd @@@ calcul des fonctions de forme pour les vertex internes pour la detection du contact
   !fd @@@ on se sert de shrink pour calculer les noeuds. 
   !fd @@@ shrink=0 on est au noeud
   !fd @@@ shrink=1 on est au centre (ksi=1/3,eta=1/3)

   !fd @@@ cas du T3
   !fd noeud 1

    ksi=shrink/3.d0
    eta=shrink/3.d0
    fformT3(1,1)=1.d0-KSI-ETA
    fformT3(2,1)=ksi
    fformT3(3,1)=eta

   !fd noeud 2

    ksi=1.d0 - (2.d0*shrink/3.d0)
    eta=shrink/3.d0
    fformT3(1,2)=1.d0-KSI-ETA
    fformT3(2,2)=ksi
    fformT3(3,2)=eta

   !fd noeud 3

    ksi=shrink/3.d0
    eta=1.d0 - (2.d0*shrink/3.d0)
    fformT3(1,3)=1.d0-KSI-ETA
    fformT3(2,3)=ksi
    fformT3(3,3)=eta

   !fd @@@ cas du Q4

   !fd noeud 1

    ksi=shrink - 1.d0
    eta=shrink - 1.d0

    fformQ4(1,1)=0.25*(1.d0-KSI)*(1.d0-ETA) 
    fformQ4(2,1)=0.25*(1.d0+KSI)*(1.d0-ETA) 
    fformQ4(3,1)=0.25*(1.d0+KSI)*(1.d0+ETA) 
    fformQ4(4,1)=0.25*(1.d0-KSI)*(1.d0+ETA) 

   !fd noeud 2

    ksi=1.d0 - shrink
    eta=shrink - 1.d0

    fformQ4(1,2)=0.25*(1.d0-KSI)*(1.d0-ETA) 
    fformQ4(2,2)=0.25*(1.d0+KSI)*(1.d0-ETA) 
    fformQ4(3,2)=0.25*(1.d0+KSI)*(1.d0+ETA) 
    fformQ4(4,2)=0.25*(1.d0-KSI)*(1.d0+ETA) 

   !fd noeud 3

    ksi=1.d0 - shrink
    eta=1.d0 - shrink

    fformQ4(1,3)=0.25*(1.d0-KSI)*(1.d0-ETA) 
    fformQ4(2,3)=0.25*(1.d0+KSI)*(1.d0-ETA) 
    fformQ4(3,3)=0.25*(1.d0+KSI)*(1.d0+ETA) 
    fformQ4(4,3)=0.25*(1.d0-KSI)*(1.d0+ETA) 


   !fd noeud 4

    ksi=shrink - 1.d0
    eta=1.d0 - shrink

    fformQ4(1,4)=0.25*(1.d0-KSI)*(1.d0-ETA) 
    fformQ4(2,4)=0.25*(1.d0+KSI)*(1.d0-ETA) 
    fformQ4(3,4)=0.25*(1.d0+KSI)*(1.d0+ETA) 
    fformQ4(4,4)=0.25*(1.d0-KSI)*(1.d0+ETA) 

  END SUBROUTINE compute_box_PRPRx
  !------------------------------------------------------------------------ 

  !---------------------------------------------------------------------------
  ! Subroutine pour actualiser les positions des vertex des polyedres au cours du temps
  SUBROUTINE coor_prediction_PRPRx
  IMPLICIT NONE  

  INTEGER                                  :: errare 
  INTEGER                                  :: itac

                            !1234567890123456789012 
   character(len=22) :: IAM='PRPRx::coor_prediction'

  !fd dbg
  !print*,'coor prediction ',xperiodic,yperiodic

  
  call move_polyr

  IF (smooth_method) THEN
    DO itac=1,nb_POLYR
      PRcoor(1:3,itac) = get_coor_POLYR(itac)
    END DO
  ELSE
    DO itac=1,nb_POLYR
      PRcoor(1:3,itac) =0.d0
       
      IF (.NOT. get_visible_POLYR(itac)) CYCLE
       
      PRcoor(1:3,itac) = get_coorTT_POLYR(itac)

      IF ( XPERIODIC ) THEN

        IF ( PRcoor(1,itac) < 0.D0 .or. PRcoor(1,itac)  > xperiode ) call faterr(IAM,'x perio not verified') 

        !fd a virer il me semble
        IF ( PRcoor(1,itac)  > xperiode ) THEN
          PRcoor(1,itac) = PRcoor(1,itac) - xperiode
          !fd dbg
          !print*,'X- ',itac
          call faterr(IAM,' X-')          
        ELSE IF( PRcoor(1,itac) < 0.D0 ) THEN
          PRcoor(1,itac) = PRcoor(1,itac) + xperiode
          !fd dbg
          !print*,'X+ ',itac
          call faterr(IAM,' X+')          
        END IF
      END IF

      IF ( YPERIODIC ) THEN

        if (PRcoor(2,itac) < 0.D0 .or. PRcoor(2,itac)  > yperiode ) call faterr(IAM,'y perio not verified') 

        !fd a virer il me semble
        IF ( PRcoor(2,itac)  > yperiode ) THEN
          PRcoor(2,itac) = PRcoor(2,itac) - yperiode
          !fd dbg
          !print*,'Y- ',itac
          call faterr(IAM,' Y-')
        ELSE IF ( PRcoor(2,itac) < 0.D0 ) THEN
          PRcoor(2,itac) = PRcoor(2,itac) + yperiode
          !fd dbg
          !print*,'Y+ ',itac
          call faterr(IAM,' Y+')           
        END IF
      END IF
    END DO
  END IF

  END SUBROUTINE coor_prediction_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PRPRx(step)
  implicit none
  integer(kind=4), intent(in) :: step
  !
  integer(kind=4) :: nb_read
  
  G_nfich=get_io_unit()

  if(step == 0) then
    open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
  else if(step > 0) then
    open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(:))))
  else
    open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
  end if

  call read_ini_Vloc_Rloc(nb_read)
  close(G_nfich)
  
  end subroutine read_ini_Vloc_Rloc_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PRPRx(which)
    
    IMPLICIT NONE
    
    INTEGER :: which,nfich,lc
    
    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Vloc_Rloc)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Vloc_Rloc(1:lc))))
       CALL write_out_Vloc_Rloc(nfich)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Vloc_Rloc)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Vloc_Rloc(1:lc))))
       CALL write_out_Vloc_Rloc(nfich)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_Vloc_Rloc(6)
    END SELECT
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PRPRx
  !------------------------------------------------------------------------------------------------

  !------------------------------------------------------------------------------------------------
  SUBROUTINE display_prox_tactors_PRPRx

   IMPLICIT NONE

   INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee
   character(len=5) :: cdmodel,anmodel

   nb_POLYR=get_nb_POLYR()

   DO icdtac=1,nb_POLYR

      
     DO iadj=1,nb_adj(icdtac)         
       icdan  = adjac(icdtac)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
      !icdtac = this(icdan)%icdtac
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( polyr2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,icdbdy,'POLYR',icdtac,see(this(icdan)%isee)%behav,  &
       anmodel,ianbdy,'POLYR',iantac
                    
       WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
       WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
       WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
       WRITE(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
       WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
       WRITE(*,104) 'PTCx=',this(icdan)%coor(1),'PTCy=',this(icdan)%coor(2),'PTCz=',this(icdan)%coor(3)
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(27X,3(2X,A5,D14.7))
   
  END SUBROUTINE display_prox_tactors_PRPRx
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE stock_rloc_PRPRx
  !
  ! get data from this and put into verlt
  !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare

   character(len=80) :: cout
                            !12345678901234567 
   character(len=17) :: IAM='PRPRx::stock_rloc'

   nb_POLYR=get_nb_POLYR()

   ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,' error allocating verlt')
     END IF
     DO icdtac=1,nb_POLYR
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0
         
       ELSE

         call nullify_verlet_(icdtac) 
           
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_POLYR
       verlt(icdtac)%adjsz=0

       call free_verlet_(icdtac)

       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0
         
       ELSE

         call free_verlet_(icdtac)  
           
       END IF
     END DO
   END IF

   ! filling data:
   DO icdan=1,nb_PRPRx

     !   PRINT*,'yyy'
     !   PRINT*,icdan,this(icdan)%status
     !   PRINT*,this(icdan)%vls,this(icdan)%vlt,this(icdan)%vln
     !   PRINT*,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln
     !   PRINT*,this(icdan)%gapTT


     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan 
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor 
     iadj   = this(icdan)%iadj
     ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)    = icdan

     verlt(icdtac)%cdbdy          = polyr2bdyty(1,icdtac)
     verlt(icdtac)%cdtac          = polyr2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel        = polyr2bdyty(3,icdtac)
     verlt(icdtac)%anbdy(iadj)    = polyr2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)    = polyr2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)  = polyr2bdyty(3,iantac)
     
     !fd @@@ il manque clairement des choses pour retrouver les faces ....

     verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
     verlt(icdtac)%ansci(iadj)    = this(icdan)%iansci

     verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
     verlt(icdtac)%rls(iadj)      = this(icdan)%rls/H

     verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)      = this(icdan)%vln
     verlt(icdtac)%vls(iadj)      = this(icdan)%vls

     verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)   = this(icdan)%status
     verlt(icdtac)%coor(1:3,iadj) = this(icdan)%coor(1:3)
     verlt(icdtac)%tuc(:,iadj)    = this(icdan)%tuc(:)
     verlt(icdtac)%nuc(:,iadj)    = this(icdan)%nuc(:)
     verlt(icdtac)%suc(:,iadj)    = this(icdan)%suc(:)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

     verlt(icdtac)%id_f_cd(iadj)   = this(icdan)%id_f_cd
     verlt(icdtac)%id_f_an(iadj)   = this(icdan)%id_f_an

     verlt(icdtac)%icdcoor(:,iadj)   = this(icdan)%icdcoor(:)
     verlt(icdtac)%iancoor(:,iadj)   = this(icdan)%iancoor(:)

     !fd dbg
     !print*, icdtac, iantac, iadj
     !print*, verlt(icdtac)%icdcoor(:,iadj)
     !print*, verlt(icdtac)%iancoor(:,iadj)
     
   END DO
   
   nb_vPRPRx = nb_PRPRx

   WRITE(cout,'(1X,I10,A12)') nb_vPRPRx,' stock PRPRx'
   call logmes(cout)

   verlet_from_file = .FALSE.

  END SUBROUTINE stock_rloc_PRPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_PRPRx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER     :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   REAL(kind=8),DIMENSION(3) :: sep
   REAL(kind=8) :: val   
   logical:: is_found
   character(len=80) :: cout
                            !12345678901234567
   character(len=17) :: IAM='PRPRx::recup_rloc'

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if
   nb_recup_PRPRx=0

   !fd cette merde suppose que le nombre de POLYR n'a pas change 
   !fd par contre on pourrait avoir swappe cd et an ?

   DO icdan=1,nb_PRPRx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%rls=0.D0
     this(icdan)%statusBEGIN=i_nknow
     ! serial number of candidate contactor for contact icdan     
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan             
     iantac = this(icdan)%iantac 

     !fd dbg
     !print*, icdtac,iantac
     !print*, this(icdan)%icdcoor(1:3)
     !print*,'=============================='
     !print*,'contact: ',icdan 
     !print*,'pedigree local:'
     !print*,icdtac,iantac,this(icdan)%node_rank
     !print*,'ref global cd:'
     !print*,polyr2bdyty(3,icdtac),polyr2bdyty(1,icdtac),polyr2bdyty(2,icdtac)
     !print*,'ref global an:'
     !print*,polyr2bdyty(3,iantac),polyr2bdyty(1,iantac),polyr2bdyty(2,iantac)
     !print*,'--'
     !print*,'verlet ref global par cd'
     !print*,verlt(icdtac)%cdmodel,verlt(icdtac)%cdbdy,verlt(icdtac)%cdtac
     !print*,'nb adj: ',verlt(icdtac)%adjsz
     !print*,'verlet ref global par an'
     !print*,verlt(iantac)%cdmodel,verlt(iantac)%cdbdy,verlt(iantac)%cdtac
     !print*,'nb adj: ',verlt(iantac)%adjsz
     !print*,'--'

     !print*,'recherche par cd'
     is_found=.false.
     IF (verlt(icdtac)%adjsz /= 0) THEN

       if (verlt(icdtac)%cdbdy  == polyr2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == polyr2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== polyr2bdyty(3,icdtac) ) then

         !print*,' par cd possible'
         !print*,verlt(icdtac)%adjsz

         do iadj = 1, verlt(icdtac)%adjsz
           !print*,'verlet ref global adjacent: ',iadj
           !print*,verlt(icdtac)%anmodel(iadj), &
           !       verlt(icdtac)%anbdy(iadj),  &
           !       verlt(icdtac)%antac(iadj)

           !print*,'faces en vis a vis cd et an'
           !print*,verlt(icdtac)%id_f_cd(iadj),verlt(icdtac)%id_f_an(iadj)

           if (verlt(icdtac)%anbdy(iadj)  == polyr2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == polyr2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== polyr2bdyty(3,iantac) ) then

             !print*,'an possible'

             IF ( this(icdan)%icdsci /= 0 ) THEN

               !fd TODO uniformiser ce bordel
               !fd gestion face a face nc
               if (this(icdan)%id_f_cd /=0 .and. verlt(icdtac)%id_f_cd(iadj) /=0) then
                 if (this(icdan)%id_f_cd /= verlt(icdtac)%id_f_cd(iadj) .or. &
                     this(icdan)%id_f_an /= verlt(icdtac)%id_f_an(iadj)) cycle
               endif

               !print*,'?',verlt(icdtac)%cdsci(iadj)

               IF (this(icdan)%icdsci == verlt(icdtac)%cdsci(iadj)) THEN
                 nb_recup_PRPRx = nb_recup_PRPRx+1
                 this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
                 this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
                 this(icdan)%rls    = verlt(icdtac)%rls(iadj)*H
                 this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
                 this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                 is_found=.true.
                 EXIT
               ENDIF

             ELSE 

               if ( verlet_from_file ) then
                 !test par la position du point de contact (cdcoor+ancoor)/2   
              
                 !fd @@@ c'est pas mal comme facon de retrouver les points ... mais faudrait etre
                 !fd @@@ plus precis, la norme devrait dependre de la taille des particules ...
               
                 sep=verlt(icdtac)%coor(1:3,iadj)-this(icdan)%coor(1:3)
                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 

               else
                 !test par la position du point candidat  

                 sep = verlt(icdtac)%icdcoor(1:3,iadj)-this(icdan)%icdcoor(1:3)
                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 
                
               endif
              
               !print*,'icdan',icdan              
               !print*,icdtac,iantac,val

               IF (val < tol_recup_rloc .or. &
                   (xperiodic .and. dabs(xperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. &
                   (yperiodic .and. dabs(yperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. & 
                   (xperiodic .and. yperiodic .and. dabs(dsqrt(xperiode**2+yperiode**2)-dsqrt(val))< dsqrt(tol_recup_rloc)) &
                  ) THEN
                 nb_recup_PRPRx = nb_recup_PRPRx+1
                 this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
                 this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
                 this(icdan)%rls    = verlt(icdtac)%rls(iadj)*H
                 this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
                 this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                 is_found=.true.
                 EXIT
               ENDIF
             ENDIF
           ENDIF
         END DO
       ENDIF
     ENDIF

     if (is_found) cycle

     !print*,'recherche par an'

     if (verlt(iantac)%adjsz /= 0) then

       if (verlt(iantac)%cdmodel== polyr2bdyty(3,iantac) .and. &
           verlt(iantac)%cdbdy  == polyr2bdyty(1,iantac) .and. &
           verlt(iantac)%cdtac  == polyr2bdyty(2,iantac) ) then

         !print*,' par an possible'
         !print*,verlt(iantac)%adjsz

         do iadj = 1, verlt(iantac)%adjsz

           !print*,'verlet ref global adjacent: ',iadj
           !print*,verlt(iantac)%anmodel(iadj),verlt(iantac)%anbdy(iadj),verlt(iantac)%antac(iadj)

           if (verlt(iantac)%anmodel(iadj)== polyr2bdyty(3,icdtac) .and. &
               verlt(iantac)%anbdy(iadj)  == polyr2bdyty(1,icdtac) .and. &
               verlt(iantac)%antac(iadj)  == polyr2bdyty(2,icdtac) ) then

             IF ( this(icdan)%icdsci /=0 ) THEN

               !print*,'?',verlt(iantac)%cdsci(iadj)


               ! icdtac -> iantac
               !fd TODO uniformiser ce bordel
               !fd gestion face a face nc
               if (this(icdan)%id_f_an /=0 .and. verlt(iantac)%id_f_an(iadj) /=0) then
                 if (this(icdan)%id_f_cd /= verlt(iantac)%id_f_an(iadj) .or. &
                     this(icdan)%id_f_an /= verlt(iantac)%id_f_cd(iadj)) cycle
               endif

               IF (this(icdan)%icdsci == verlt(iantac)%cdsci(iadj)) THEN
                 nb_recup_PRPRx = nb_recup_PRPRx+1
                 this(icdan)%rlt    = verlt(iantac)%rlt(iadj)*H
                 this(icdan)%rln    = verlt(iantac)%rln(iadj)*H
                 this(icdan)%rls    = verlt(iantac)%rls(iadj)*H
                 this(icdan)%statusBEGIN = verlt(iantac)%status(iadj)
                 this(icdan)%internal(1:max_internal_tact) = verlt(iantac)%internal(1:max_internal_tact,iadj)
                 EXIT
               ENDIF

             ELSE 

               if ( verlet_from_file ) then

                 !fd @@@ c'est pas mal comme facon de retrouver les points ... mais faudrait etre
                 !fd @@@ plus precis, la norme devrait dependre de la taille des particules ...

                 sep=verlt(iantac)%coor(1:3,iadj)-this(icdan)%coor(1:3)

                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 

               else

                 sep = verlt(iantac)%icdcoor(1:3,iadj)-this(icdan)%icdcoor(1:3)
                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 

                
               endif

               !print*,icdtac,iantac,val

               IF (val < tol_recup_rloc .or. &
                   (xperiodic .and. dabs(xperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. &
                   (yperiodic .and. dabs(yperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. & 
                   (xperiodic .and. yperiodic .and. dabs(dsqrt(xperiode**2+yperiode**2)-dsqrt(val))< dsqrt(tol_recup_rloc)) &
                  ) THEN
                 nb_recup_PRPRx = nb_recup_PRPRx+1
                 this(icdan)%rlt    = verlt(iantac)%rlt(iadj)*H
                 this(icdan)%rln    = verlt(iantac)%rln(iadj)*H
                 this(icdan)%rls    = verlt(iantac)%rls(iadj)*H
                 this(icdan)%statusBEGIN = verlt(iantac)%status(iadj)
                 this(icdan)%internal(1:max_internal_tact) = verlt(iantac)%internal(1:max_internal_tact,iadj)
                 EXIT
               ENDIF
             ENDIF
           ENDIF
         END DO
       ENDIF 
     ENDIF

     !if (.not. is_found) then
     !  print*,'contact: ',icdan 
     !  print*,'pedigree local:'
     !  print*,icdtac,iantac,this(icdan)%node_rank


     !   print*,verlt(icdtac)%cdmodel,polyr2bdyty(3,icdtac)
     !   print*,verlt(icdtac)%cdbdy,polyr2bdyty(1,icdtac)
     !   print*,verlt(icdtac)%cdtac,polyr2bdyty(2,icdtac)


     !  do iadj=1,verlt(icdtac)%adjsz
     !     print*,iadj,verlt(icdtac)%id_f_cd(iadj),verlt(icdtac)%anbdy(iadj),verlt(icdtac)%antac(iadj),verlt(icdtac)%cdsci(iadj)
     !  enddo
     !endif


   END DO
  
   WRITE(cout,'(1X,I10,A12)') nb_recup_PRPRx,' recup PRPRx'
   call logmes(cout)

  END SUBROUTINE recup_rloc_PRPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  SUBROUTINE read_ini_Vloc_Rloc(nb_read) 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   IMPLICIT NONE

   CHARACTER(len=103)               :: clin
   INTEGER                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   REAL(kind=8)                     :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
   CHARACTER(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER                          :: errare


   INTEGER :: ibehav,nb_internal,i_internal
   INTEGER :: icdtact,nb_read
   CHARACTER(len=103) :: cout
                               !1234567890123456789012345
   CHARACTER(len=25)  :: IAM = 'PRPRx::read_ini_Vloc_Rloc'

   integer:: cdmodel,anmodel,icdver,j,k

   errare=0
   nb_read=0
   nb_POLYR=get_nb_POLYR()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_POLYR),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,' error allocating nb_adj')
   END IF

   DO icdtac=1,nb_POLYR
     nb_adj(icdtac)=0
   END DO

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
 
     cdmodel = get_body_model_id_from_name( cdbdy )

     IF (cdtac == 'POLYR' .AND. antac == 'POLYR') THEN
       do icdtact = 1, nb_POLYR
         if (polyr2bdyty(1,icdtact) == icdbdy .and. &
             polyr2bdyty(2,icdtact) == icdtac .and. &
             polyr2bdyty(3,icdtact) == cdmodel ) then
           nb_adj(icdtact) = nb_adj(icdtact) + 1
           exit
         end if
       end do
     END IF
   END DO

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,' error allocating verlt')
     END IF
     DO icdtac=1,nb_POLYR
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
          call faterr(IAM,'error in allocating verlt(icdtac)%.....')
         END IF
         
         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0

       ELSE

         call nullify_verlet_(icdtac)
           
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_POLYR
         
       call free_verlet_(icdtac)    

       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)
         
         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0
         
       ELSE

         call nullify_verlet_(icdtac)

       END IF
     END DO
   END IF    
  ! second reading: filling data
   REWIND(G_nfich)
   DO icdtac=1,nb_POLYR
     nb_adj(icdtac)=0
   END DO
   icdan=0
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                                 &
          sttus

     IF (cdtac == 'POLYR' .AND. antac == 'POLYR') THEN

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       do icdtact = 1, nb_POLYR
         IF (polyr2bdyty(1,icdtact) == icdbdy .and. &
             polyr2bdyty(2,icdtact) == icdtac .and. &
             polyr2bdyty(3,icdtact) == cdmodel ) then

           nb_read=nb_read+1

           nb_adj(icdtact)=nb_adj(icdtact)+1

           verlt(icdtact)%icdan( nb_adj(icdtact) ) = nb_read

           verlt(icdtact)%cdmodel = cdmodel
           verlt(icdtact)%cdbdy   = icdbdy
           verlt(icdtact)%cdtac   = icdtac
 
           if (icdver /= 0) verlt(icdtact)%cdsci(nb_adj(icdtact)) = icdver

           verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
           verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
           verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
           verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)


           IF( .NOT. read_G_clin()) EXIT
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') rlt,rln,rls
           verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
           verlt(icdtact)%rln(nb_adj(icdtact)) = rln
           verlt(icdtact)%rls(nb_adj(icdtact)) = rls
           
           IF( .NOT. read_G_clin()) CYCLE
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') vlt,vln,vls
           verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
           verlt(icdtact)%vln(nb_adj(icdtact)) = vln
           verlt(icdtact)%vls(nb_adj(icdtact)) = vls
           
           IF( .NOT. read_G_clin()) CYCLE 
           READ(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
           verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 'coo1=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%coor(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%coor(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%coor(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 't(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%tuc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%tuc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%tuc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 'n(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%nuc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%nuc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%nuc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF
          
           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 's(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF

           verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
           ibehav = get_ibehav(behav)
           nb_internal = get_nb_internal(ibehav)
           IF (nb_internal /= 0 ) THEN  
             IF( .NOT. read_G_clin()) EXIT
             DO i_internal=1, nb_internal
               READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
             ENDDO
           ENDIF
           EXIT
         ENDIF
       ENDDO
     ENDIF
   ENDDO
 
   nb_vPRPRx=0
    
   DO icdtact=1,nb_POLYR
     nb_vPRPRx = nb_vPRPRx + nb_adj(icdtact)
       
     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
            'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
       CALL FATERR(IAM,cout)
     END IF
   END DO

   !xxxx

   if (allocated(this2verlet)) deallocate(this2verlet)
   allocate(this2verlet(2,nb_vPRPRx))

   k=0
   DO icdtact=1,nb_POLYR
     do j=1,nb_adj(icdtact)
       k=k+1
       this2verlet(1,k) = icdtact
       this2verlet(2,k) = j
     enddo 
   enddo 

104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
  END SUBROUTINE read_ini_Vloc_Rloc
  !------------------------------------------------------------------------   

  !------------------------------------------------------------------------   
  SUBROUTINE write_out_Vloc_Rloc(nfich)
   !
   ! write into file out_Vloc_Rloc data from this, in verlt style
   !
   IMPLICIT NONE

   INTEGER :: iadj,icdtact
   INTEGER :: nfich,icdan,icdtac,iantac,icdver
   character(len=5) :: cdmodel,anmodel

   character(len=20) :: fmt
   
   nb_POLYR=get_nb_POLYR()

   IF (nb_PRPRx==0) RETURN

   DO icdtact=1,nb_POLYR    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac

         cdmodel = get_body_model_name_from_id( polyr2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )

         !mr must be defined during the detection
         icdver = 0
         if (this(icdan)%icdsci /=0 ) icdver=this(icdan)%icdsci

         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PRPRx',icdan  
         !1234567890123456789012345678901234567890123456789012345678901234567890124567
         WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
         
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdmodel,get_visibleID_POLYR(icdtac),'POLYR',polyr2bdyty(2,icdtac),icdver, &
              see(this(icdan)%isee)%behav,  &
              anmodel,get_visibleID_POLYR(iantac),'POLYR',polyr2bdyty(2,iantac), &
              get_contact_status_name_from_id(this(icdan)%status)
         WRITE(nfich,104) 'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls/H',this(icdan)%rls/H
         WRITE(nfich,104) 'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',this(icdan)%vls  
         WRITE(nfich,103) 'gapTT',this(icdan)%gapTT
         WRITE(nfich,104) 'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',this(icdan)%coor(3)
         WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
         WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
         WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)
         
         IF (this(icdan)%nb_internal /= 0) THEN
           CALL write_internal_comment(nfich,this(icdan)%lawnb)
           write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
           write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
         END IF
         WRITE(nfich,'(A1)')' '
         
      END DO
   END DO
   
104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)

  END SUBROUTINE write_out_Vloc_Rloc
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE nullify_reac_PRPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: storage
    
   CALL nullify_reac_POLYR(this(icdan)%icdtac,storage)
   
   CALL nullify_reac_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_reac_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_PRPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
    
   CALL nullify_vlocy_POLYR(this(icdan)%icdtac,storage)
   
   CALL nullify_vlocy_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_vlocy_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE vitrad_PRPRx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
    
   CALL comp_vlocy_POLYR(this(icdan)%icdtac,storage)
    
   CALL comp_vlocy_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE vitrad_PRPRx
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE injj_PRPRx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: ccdof = (/ 1,2,3,4,5,6 /)
   REAL(kind=8),DIMENSION(6)  :: cdreac, anreac

   INTEGER                    :: storage

   ! if (storage == iIaux_) then
   !   print*,icdan 
   !   print*,this(icdan)%Gcdt(1:3)
   !   print*,this(icdan)%Gcdn(1:3)
   !   print*,this(icdan)%Gcds(1:3)
     
   !   print*,this(icdan)%Gant(1:3)
   !   print*,this(icdan)%Gann(1:3)
   !   print*,this(icdan)%Gans(1:3)      
   ! endif
   
   cdreac(1)  = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   cdreac(2)  = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   cdreac(3)  = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
   cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
   cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK

   anreac(1)  = -cdreac(1)
   anreac(2)  = -cdreac(2)
   anreac(3)  = -cdreac(3)
   anreac(4) =-this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
   anreac(5) =-this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
   anreac(6) =-this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK

   !print*,'==='
   !print*,'injj prpr ',storage
   !write(*,'(6(1x,E12.5))') cdreac
   !write(*,'(6(1x,E12.5))') anreac

   CALL add_reac_POLYR(this(icdan)%icdtac,ccdof,cdreac,storage)

   CALL add_reac_POLYR(this(icdan)%iantac,ccdof,anreac,storage)

  END SUBROUTINE injj_PRPRx 
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE prjj_PRPRx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   REAL(kind=8),DIMENSION(3) :: Vth, Vthcd, Vthan
   
   Vcd = get_vlocy_POLYR(this(icdan)%icdtac,storage)
   Van = get_vlocy_POLYR(this(icdan)%iantac,storage)      

   ! if (storage == iVaux_) then
   ! print*,'prjj prpr '
   ! write(*,'(I0,1x,6(1x,E12.5))') this(icdan)%icdtac,vcd
   ! write(*,'(I0,1x,6(1x,E12.5))') this(icdan)%iantac,van
   ! print*,'==='
   ! endif

   if (storage == iVfree) then

     !if (is_explicit) then 
     !  call get_Vth_POLYR_ref(this(icdan)%icdtac,this(icdan)%coorcd,Vthcd)
     !  call get_Vth_POLYR_ref(this(icdan)%iantac,this(icdan)%cooran,Vthan)
     !else
     !  call get_Vth_POLYR(this(icdan)%icdtac,this(icdan)%coorcd,Vthcd)
     !  call get_Vth_POLYR(this(icdan)%iantac,this(icdan)%cooran,Vthan)
     !endif

     !Vth=Vthcd-Vthan

     if (with_f2f) then
         Vthcd = get_Vtherm_POLYR_face(this(icdan)%icdtac , &
                                       this(icdan)%id_f_cd, &
                                       this(icdan)%icdcoor  )
         Vthan = get_Vtherm_POLYR_face(this(icdan)%iantac , &
                                       this(icdan)%id_f_an, &
                                       this(icdan)%iancoor  )
     else
        Vthcd = 0.d0
        Vthan = 0.d0
     end if

     Vth=Vthcd-Vthan


!fd     print*,'>>>>>>>>>>>>>>>>>>>>>>>>>>>'
!fd     print*,icdan
!fd     print*,'Vthcd', this(icdan)%Vthcd
!fd     print*,'Vthan', this(icdan)%Vthan
!fd     print*,'Vth  ', Vth
!fd     print*,'<<<<<<<<<<<<<<<<<<<<<<<<<<<'

   else
     Vth=0.d0
   endif

!
!IF (storage == iVfree) THEN
!fd     PRINT*,'>>>>>>>>>>>>>>>>>>>>>>>>>>>'
!fd     PRINT*,icdan
!fd     PRINT*,'Vcd', Vcd
!fd     PRINT*,'Van', Van
!fd     PRINT*,'<<<<<<<<<<<<<<<<<<<<<<<<<<<'
!ENDIF

   VSIK = Vcd(1)*this(icdan)%suc(1) + Vcd(2)*this(icdan)%suc(2) + Vcd(3)*this(icdan)%suc(3)        &
        + Vcd(4)*this(icdan)%Gcds(1)+ Vcd(5)*this(icdan)%Gcds(2)+ Vcd(6)*this(icdan)%Gcds(3)       &
        - Van(1)*this(icdan)%suc(1) - Van(2)*this(icdan)%suc(2) - Van(3)*this(icdan)%suc(3)        &
        - Van(4)*this(icdan)%Gans(1)- Van(5)*this(icdan)%Gans(2)- Van(6)*this(icdan)%Gans(3)       &
        + Vth(1)*this(icdan)%suc(1) + Vth(2)*this(icdan)%suc(2) + Vth(3)*this(icdan)%suc(3)


   VTIK = Vcd(1)*this(icdan)%tuc(1) + Vcd(2)*this(icdan)%tuc(2) + Vcd(3)*this(icdan)%tuc(3)        &
        + Vcd(4)*this(icdan)%Gcdt(1)+ Vcd(5)*this(icdan)%Gcdt(2)+ Vcd(6)*this(icdan)%Gcdt(3)       &
        - Van(1)*this(icdan)%tuc(1) - Van(2)*this(icdan)%tuc(2) - Van(3)*this(icdan)%tuc(3)        &
        - Van(4)*this(icdan)%Gant(1)- Van(5)*this(icdan)%Gant(2)- Van(6)*this(icdan)%Gant(3)       &
        + Vth(1)*this(icdan)%tuc(1) + Vth(2)*this(icdan)%tuc(2) + Vth(3)*this(icdan)%tuc(3)


   VNIK = Vcd(1)*this(icdan)%nuc(1) + Vcd(2)*this(icdan)%nuc(2) + Vcd(3)*this(icdan)%nuc(3)        &
        + Vcd(4)*this(icdan)%Gcdn(1)+ Vcd(5)*this(icdan)%Gcdn(2)+ Vcd(6)*this(icdan)%Gcdn(3)       &
        - Van(1)*this(icdan)%nuc(1) - Van(2)*this(icdan)%nuc(2) - Van(3)*this(icdan)%nuc(3)        &
        - Van(4)*this(icdan)%Gann(1)- Van(5)*this(icdan)%Gann(2)- Van(6)*this(icdan)%Gann(3)       &
        + Vth(1)*this(icdan)%nuc(1) + Vth(2)*this(icdan)%nuc(2) + Vth(3)*this(icdan)%nuc(3)

  END SUBROUTINE prjj_PRPRx 
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PRPRx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_PRPRx = nb_PRPRx
    CASE(i_verlet_tactor)
       get_nb_PRPRx = nb_vPRPRx
    CASE(i_rough_tactor)
       get_nb_PRPRx = nb_rough_PRPRx
    CASE(i_recup_tactor)
       get_nb_PRPRx = nb_recup_PRPRx
    END SELECT

  END FUNCTION get_nb_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE get_detection_time_PRPRx(tps,nb_detec,nb_sha,nb_ctc)

  IMPLICIT NONE
  REAL(kind=8)        :: tps
  INTEGER             :: nb_detec,nb_sha,nb_ctc

  IF (nb_detection_test==0) THEN
    tps=0.D0
  ELSE 
    tps=detection_time
  ENDIF 

  nb_detec = nb_detection_test
  nb_sha   = nb_shadow
  nb_ctc   = nb_ctc_state

  END SUBROUTINE get_detection_time_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  SUBROUTINE PRPRx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_POLYR(this(icdan)%icdtac)
   ianent = get_ENT_POLYR(this(icdan)%iantac)

  END SUBROUTINE PRPRx2ENT
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE PRPRx2POLYR(icdan,icdtac,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

  END SUBROUTINE PRPRx2POLYR
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------ 
  INTEGER FUNCTION get_type_PRPRx(icdan)

   IMPLICIT NONE
   INTEGER       :: icdan
   
   get_type_PRPRx = this(icdan)%type_ctc
   
  END FUNCTION get_type_PRPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  REAL(kind=8) function get_surf_PRPRx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan

   get_surf_PRPRx = this(icdan)%area

  END function get_surf_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  LOGICAL FUNCTION RUN_PRPRx()

    IMPLICIT NONE
    
    RUN_PRPRx = RUN_TACTOR

  END FUNCTION RUN_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  logical function CHECK_PRPRx()
  implicit none
  !   
  integer :: isee

  ! if check already made just return result
  if( module_checked_ ) then
    CHECK_PRPRx = check_PRPRx_
    return
  end if

  con_pedigree%module_name = 'PRPRx'

  con_pedigree%id_cdan  = i_prprx
  con_pedigree%id_cdtac = i_polyr
  con_pedigree%id_antac = i_polyr

  cdtact2bdyty => polyr2bdyty
  antact2bdyty => polyr2bdyty

  ! check only once if module may be used
  module_checked_ = .TRUE.

  ! checking if enough cd/an
  nb_POLYR = get_nb_POLYR()
  if( nb_POLYR < 2 ) then
    CHECK_PRPRx = check_PRPRx_ ! still false
    return
  end if
  
  ! checking if any seetable with the good cd/an type
  do isee = 1, size(see)
    if (see(isee)%cdtac == 'POLYR' .and. see(isee)%antac == 'POLYR') then
      check_PRPRx_ = .true.
      exit
    end if
  end do

  CHECK_PRPRx = check_PRPRx_
  return

  end function CHECK_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_PRPRx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_PRPRx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_shrink_polyr_faces_PRPRx(shr)

    IMPLICIT NONE
    REAL(kind=8) :: shr
                             !12345678901234567890123456789
    character(len=29) :: IAM='PRPRx::set_shrink_polyr_faces' 

    SHRINK =  shr
 
    if( shr/= 0.d0 .and. (cp_cd_shrink/=0.d0 .or. cp_an_shrink/=0.d0) ) then
      call logmes('[WARNING] A shrink is provided here but a shrink parameter', .true.)
      call logmes('          was already provided as parameter when selecting', .true.)
      call logmes('          detection method.'                               , .true.)
    end if

    if (shrink < 0.d0 .or. shrink > 1.d0) then
      call logmes('shrink should be between 0. and 1.', .true.)
      call logmes('0. nothing happen', .true.)
      call FATERR(IAM,'incompatible value of shrink coefficient')
    endif

  END SUBROUTINE set_shrink_polyr_faces_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_size_factor_polyr_PRPRx(sf)

    IMPLICIT NONE
    integer :: sf

    !fd burk
    size_factor = sf

  END SUBROUTINE set_size_factor_polyr_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  subroutine set_clipper_parameters(cd_shrink, an_shrink, delta)
    implicit none
    real(kind=8), intent(in) :: cd_shrink
    real(kind=8), intent(in) :: an_shrink
    real(kind=8), intent(in) :: delta

    if( shrink /= 0.d0 .and. (cd_shrink/=0.d0 .or. an_shrink/=0.d0) ) then
      call logmes('[WARNING] A shrink is given when setting detection method', .true.)
      call logmes('          but a shrink parameter was already provided', .true.)
      call logmes('          with PRPRx_ShrinkPolyrFaces.', .true.)
    end if

    cp_cd_shrink = cd_shrink
    cp_an_shrink = an_shrink
    cp_delta     = delta

  end subroutine set_clipper_parameters
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_PRPRx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    xperiode  = per
    XPERIODIC = FLAG
    
  END SUBROUTINE set_xperiodic_data_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_PRPRx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    yperiode  = per
    YPERIODIC = FLAG
    
  END SUBROUTINE set_yperiodic_data_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !--- routines de pre-detection ----
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_PRPRx

   IMPLICIT NONE

   INTEGER                               :: errare,icdtac,iantac,isee,itacty,i,k
   INTEGER                               :: icdan,iadj,itac
   CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
   REAL(kind=8),DIMENSION(3)             :: sep,axe,point
   REAL(kind=8)                          :: dist,norm1,dist1,norm2,dist2,nonuc,gap,rayan,raycd,adist
   REAL(kind=8),DIMENSION(3)             :: coordcd,coordan
   INTEGER                               :: ibox1,ibox2,ibox3
   INTEGER                               :: ibox1cd,ibox2cd,ibox3cd,size_of_this
   INTEGER                               :: ibox1an,ibox2an,ibox3an,icdpop,ianpop
   REAL(kind=8)                          :: Xleft,Xright,Yleft,Yright,Zup,Zdown

                              !123456789012345678901234
   CHARACTER(len=24) :: IAM = 'PRPRx::creation_tab_visu'
   character(len=120):: cout
   logical           :: bavard=.false.

  ! Since the list of proximate contactors may not be updated at every time step,
  ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
  ! A warning condition prevents undue deallocation. 

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

 
   Xleft   =  1.D24
   Xright  = -1.D24
   Yleft   =  1.D24
   Yright  = -1.D24
   Zdown   =  1.D24
   Zup     = -1.D24

   DO itac=1,nb_POLYR

     IF (.NOT. get_visible_POLYR(itac)) CYCLE

     !cycling if tactor is a big polyr
     if (nb_big_polyr /= 0) then
        if ( any(big_polyr == itac) ) cycle
     endif   

     coordcd = PRcoor(1:3,itac)
     Xleft = MIN(coordcd(1),Xleft )
     Xright= MAX(coordcd(1),Xright)
     Yleft = MIN(coordcd(2),Yleft )
     Yright= MAX(coordcd(2),Yright)
     Zdown = MIN(coordcd(3),Zdown )
     Zup   = MAX(coordcd(3),Zup   )

   END DO

   IF (XPERIODIC) THEN
     IF (Xright>xperiode) THEN
       write(cout,*) 'The max right coordinate ', Xright, ' is greater than the periode'
       CALL FATERR(IAM,cout)
     END IF

     IF (Xleft<0.D0) THEN
       write(cout,*) 'The min left coordinate ', Xleft, ' is less than zero'
       CALL FATERR(IAM,cout)
     END IF

     Xright = xperiode
     Xleft  = 0.D0

     if (bavard) print*,'-------------XPERIODIC--------------------'

   END IF

   IF (YPERIODIC) THEN
     IF (Yright>yperiode) THEN
       write(cout,*) 'The max right coordinate ', Yright, ' is greater than the periode'
       CALL FATERR(IAM,cout)
     END IF

     IF (Yleft<0.D0) THEN
       write(cout,*) 'The min left coordinate ', Yleft, ' is less than zero'
       CALL FATERR(IAM,cout)
     END IF

     Yright = yperiode
     Yleft  = 0.D0

     if (bavard) print*,'-------------YPERIODIC--------------------'
   END IF

   if (bavard) then
     print*,'X- ',Xleft,' X+ ',Xright
     print*,'Y- ',Yleft,' Y+ ',Yright
     print*,'Z- ',Zdown,' Z+ ',Zup
   endif

   
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
   
   maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
   maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
   maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)

  !print*,"==========================="
  !print*,Xright,Xleft,Lbox_1,maxibox1
  !print*,Yright,Yleft,Lbox_1,maxibox2
  !print*,Zup,Zdown,Lbox_1,maxibox3
  !print*,"==========================="

   ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
   IF (errare /=0 ) call FATERR(IAM,'error allocating box')

   DO ibox3=minibox3,maxibox3
   DO ibox2=minibox2,maxibox2
   DO ibox1=minibox1,maxibox1

     box(ibox1,ibox2,ibox3)%popul=0
     ALLOCATE(box(ibox1,ibox2,ibox3)%which(maxpopul),stat=errare)
     IF (errare /=0 ) call faterr(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%which')

   END DO  
   END DO
   END DO
   
   ! filling boxes
   ! box(ibox1,ibox2)%popul is the number of polyr into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%which(ipopul) is the rank of body POLYR labelled ipopul in the box
   
   ! filling boxes   
   DO itac=1,nb_POLYR

     IF (.NOT. get_visible_POLYR(itac) ) CYCLE

     ! cycling if big POLYR
     if (nb_big_polyr /= 0) then
       if (any(big_polyr == itac) ) cycle
     endif
    
     coordcd=PRcoor(1:3,itac)
     ibox1=1+INT((coordcd(1)-Xleft )*Lbox_1)
     ibox2=1+INT((coordcd(2)-Yleft )*Lbox_1)
     ibox3=1+INT((coordcd(3)-Zdown )*Lbox_1)

     !print*,"polyr:",itac
     !print*,coordcd(1),Xleft,Lbox_1,ibox1
     !print*,coordcd(2),Yleft,Lbox_1,ibox2
     !print*,coordcd(3),Zdown,Lbox_1,ibox3

     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. &
         ibox2 < minibox2 .OR. ibox2 > maxibox2 .OR. &
         ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
       write(cout,*)' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
       call logmes(cout,.true.)
       write(cout,*)'    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
       call logmes(cout,.true.)
       write(cout,*)' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
       call logmes(cout,.true.)
       write(cout,'(A13,I5,A13)')'  body POLYR ',itac,' out of boxes'
       call logmes(cout,.true.)
       write(cout,'(A38)')'  see select_prox_tactors in mod_PRPRx'
       call logmes(cout,.true.)
       write(cout,*) 'Nstep',Nstep,'X',coordcd(1),'Y',coordcd(2),'Z',coordcd(3)
       call logmes(cout,.true.)
       call faterr(IAM,'issue filling boxes')
     END IF

     box(ibox1,ibox2,ibox3)%popul = box(ibox1,ibox2,ibox3)%popul+1
     if( box(ibox1,ibox2,ibox3)%popul > size(box(ibox1,ibox2,ibox3)%which) ) then
         call faterr(IAM, "Estimated max popul limit reached.")
     end if
     box(ibox1,ibox2,ibox3)%which(box(ibox1,ibox2,ibox3)%popul) = itac
     
   END DO  

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
     
   nb_rough_PRPRx=0
   nb_rough_half=0
   ! création de la liste de paire de polygones à examiner

   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

   NULLIFY(Root) 
   NULLIFY(Current)
   NULLIFY(Previous)

   DO ibox3cd=minibox3,maxibox3
   DO ibox2cd=minibox2,maxibox2
   DO ibox1cd=minibox1,maxibox1 

     DO icdpop=1,box(ibox1cd,ibox2cd,ibox3cd)%popul
       icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
       cdcol = get_color_POLYR(icdtac)
       ! box loop investigating antagonist POLYR
       DO ibox3an=MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
       DO ibox2an=MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
       DO ibox1an=MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)            

         DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
           iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)

           IF (iantac .LE. icdtac) CYCLE
           IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
           !
           ancol=get_color_POLYR(iantac)

           !fd todo a revoir pour traiter le cas MAILx
           ! en utilisant   cd_mdl = get_mdl_POLYR(icdtac)
  
           !print*,get_body_model_name_from_id(polyr2bdyty(3,icdtac)),cdcol,get_body_model_name_from_id(polyr2bdyty(3,iantac)),ancol

           if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
             isee = get_isee_specific('POLYR',cdcol,ancol)
             !print*,'same' 
           else
             isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                             get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
           end if

           !print*,isee

           
           IF (isee /= 0) THEN
             adist=see(isee)%alert 
             ! checking ROUGHLY distance against alert distance           
             raycd = get_radius_POLYR(icdtac)
             rayan = get_radius_POLYR(iantac)

             ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
             ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
             ! results might be different up to some non significant figures, but when comparing to
             ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

             adist=0.1005D+01*(adist+raycd+rayan)

             sep = PRcoor(:,icdtac)-PRcoor(:,iantac)
             dist = DOT_PRODUCT(sep,sep)
 
             IF (dist<adist*adist) THEN
               nb_rough_PRPRx=nb_rough_PRPRx+1

               !fd @@@ half half !? on est au carre alors pourquoi pas 0.25 ?

               IF (dist < 0.5D0*adist*adist) nb_rough_half=nb_rough_half+1
               IF ( nb_rough_PRPRx == 1) THEN
                 ALLOCATE(Root)
                 Current => Root
                 NULLIFY(Root%p)
               ELSE
                 ALLOCATE(Current)
                 Previous%n => Current
               ENDIF
               Current%val%cd       =icdtac
               Current%val%an       =iantac
               Current%val%isee     =isee
               Current%val%Vsep     = sep/dsqrt(dist)
               Current%val%xperiodic = 0
               Current%val%yperiodic = 0
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current
             ENDIF
           END IF
         END DO
       END DO
       END DO
       END DO
     END DO
   END DO
   END DO
   END DO

   !fd dbg
   !print*,'detection classique ',nb_rough_PRPRx

   ! on est oblige de faire ca car les big ne sont pas dans les boites

   !fd le big est un antagoniste 
   !fd => meilleur gestion du shrink

   DO i=1,nb_big_polyr
     iantac=big_polyr(i)

     IF (.NOT. get_visible_POLYR(iantac)) CYCLE

     ancol=get_color_POLYR(iantac)
     coordan = PRcoor(1:3,iantac)
     rayan = get_radius_POLYR(iantac)

      DO icdtac=1,nb_POLYR 
        IF (iantac == icdtac) CYCLE
        IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
        IF (.NOT. get_visible_POLYR(icdtac)) CYCLE
        !fd si 2 big_polyr on vire un cas
        if (any(big_polyr == icdtac)) then
          ! le an sera le plus gros 
          if (get_radius_POLYR(icdtac) > rayan) cycle
        endif
       
        cdcol=get_color_POLYR(icdtac)
        if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
          isee = get_isee_specific('POLYR',cdcol,ancol)
        else
          isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                          get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
        end if
        IF (isee /= 0) THEN
          adist=see(isee)%alert 
          coordcd = PRcoor(1:3,icdtac)
          raycd = get_radius_POLYR(icdtac)

          ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
          ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
          ! results might be different up to some non significant figures, but when comparing to
          ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

          adist=0.1005D+01*(adist+raycd+rayan)
          sep = coordcd(:)-coordan(:)
          dist = DOT_PRODUCT(sep,sep)

          IF (dist < adist*adist) THEN
            nb_rough_PRPRx=nb_rough_PRPRx+1

            IF (dist<0.5D0*adist*adist) nb_rough_half=nb_rough_half+1
            
            IF ( nb_rough_PRPRx == 1) THEN
              ALLOCATE(Root)
              Current => Root
              NULLIFY(Root%p)
            ELSE
              ALLOCATE(Current)
              Previous%n => Current
            ENDIF

            Current%val%cd       =icdtac
            Current%val%an       =iantac
            Current%val%isee     =isee                  
            Current%val%Vsep     =sep/dsqrt(dist)                
            Current%val%xperiodic = 0
            Current%val%yperiodic = 0

            !fd arbitrage entre 2 big
            !fd si le "cd" est plus gros que le "an" on swap
            if ( any(big_polyr == icdtac) .and. (raycd > rayan) ) then
                Current%val%cd       =iantac
                Current%val%an       =icdtac
                Current%val%isee     =isee                  
                Current%val%Vsep     =-sep/dsqrt(dist)                
                Current%val%xperiodic = 0
                Current%val%yperiodic = 0
            endif
            Current%p => Previous
            NULLIFY(Current%n)
            Previous => Current
          ENDIF
        END IF
      ENDDO
   ENDDO

   !fd dbg
   !print*,'+detection big polyr ',nb_rough_PRPRx
   

   
   IF (XPERIODIC) THEN
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = minibox2,maxibox2
     !bug si qu'une boite 
     !DO ibox1cd = maxibox1-1,maxibox1
     DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1        
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
         cdcol = get_color_POLYR(icdtac)
                   
         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)
         !bug si qu'une boite 
         !DO ibox1an = minibox1,minibox1+1
         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
             iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
             IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
             ancol = get_color_POLYR(iantac)
             if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
               isee = get_isee_specific('POLYR',cdcol,ancol)
             else
               isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                               get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
             end if
             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                              
             coordcd = PRcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_POLYR(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(1) = coordan(1) + xperiode

             adist=0.1005D+01*(adist+raycd+rayan)
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN

               nb_rough_PRPRx = nb_rough_PRPRx+1

               IF (dist<0.5D0*adist*adist) nb_rough_half=nb_rough_half+1

               IF ( nb_rough_PRPRx == 1) THEN
                 ALLOCATE(Root)
                 Current => Root
                 NULLIFY(Root%p)
               ELSE
                 ALLOCATE(Current)
                 Previous%n => Current
               END IF
               Current%val%cd        = icdtac
               Current%val%an        = iantac
               Current%val%isee      = isee
               Current%val%Vsep      = sep/dsqrt(dist)
               Current%val%xperiodic = 1
               Current%val%yperiodic = 0
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current

               if (bavard) then
                 print *,icdtac,' voit ', iantac
                 print *,'posi cd : ',coordcd
                 print *,'posi an : ',coordan
                 print *,'dist    : ',dsqrt(dist) 
                 print *,Current%val%xperiodic,Current%val%yperiodic
                 print *,maxibox1,maxibox2,maxibox3
                 print *,ibox1cd,ibox2cd,ibox3cd
                 print *,ibox1an,ibox2an,ibox3an
               end if
             END IF
           END DO
         END DO
         END DO
         END DO
       END DO
     END DO
     END DO
     END DO
   END IF

   !fd dbg
   !print*,'+xperiodic ',nb_rough_PRPRx

   
   IF (YPERIODIC) THEN
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2    
     DO ibox1cd = minibox1,maxibox1
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
         cdcol = get_color_POLYR(icdtac)
                   
         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)                       
         DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)  

           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul

             iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
             IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
             ancol = get_color_POLYR(iantac)
             if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
               isee = get_isee_specific('POLYR',cdcol,ancol)
             else
               isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                               get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
             end if
             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                               
             coordcd = PRcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_POLYR(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(2) = coordan(2) + yperiode

             adist=0.1005D+01*adist+raycd+rayan
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN
               nb_rough_PRPRx = nb_rough_PRPRx+1
               IF (dist<0.5D0*adist*adist) nb_rough_half=nb_rough_half+1

               IF ( nb_rough_PRPRx == 1) THEN
                 ALLOCATE(Root)
                 Current => Root
                 NULLIFY(Root%p)
               ELSE
                 ALLOCATE(Current)
                 Previous%n => Current
               END IF
               Current%val%cd        = icdtac
               Current%val%an        = iantac
               Current%val%isee      = isee
               Current%val%Vsep      = sep/dsqrt(dist)
               Current%val%xperiodic = 0
               Current%val%yperiodic = 1
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current
             END IF
           END DO
         END DO
         END DO
         END DO
       END DO
     END DO
     END DO
     END DO
   END IF

   !fd dbg
   !print*,'+yperiodic ',nb_rough_PRPRx


   
   !fd le 04/01/08 contrairement au 2D il faut traiter les coins !! 
   !fd A VOIR si une seule boite ca va deconner

    IF (XPERIODIC .AND. YPERIODIC) THEN

       !fd on commence par le coin extreme en haut qui voit le coin initial en bas

       DO ibox3cd = minibox3,maxibox3
       !DO ibox2cd = maxibox2-1,maxibox2
       DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2  
       !DO ibox1cd = maxibox1-1,maxibox1
       DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1        
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
            icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
            cdcol = get_color_POLYR(icdtac)
                  
            DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
            !DO ibox2an = minibox2,minibox2+1                   
            DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
            !DO ibox1an = minibox1,minibox1+1  
            DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
              DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
                ancol = get_color_POLYR(iantac)
                if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
                  isee = get_isee_specific('POLYR',cdcol,ancol)
                else
                  isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                                  get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
                end if
                IF (isee.EQ.0) CYCLE
                adist=see(isee)%alert 
                               
                coordcd = PRcoor(1:3,icdtac)
                coordan = PRcoor(1:3,iantac)
                raycd = get_radius_POLYR(icdtac)
                rayan = get_radius_POLYR(iantac)

                coordan(1) = coordan(1) + xperiode
                coordan(2) = coordan(2) + yperiode

                adist=0.1005D+01*adist+raycd+rayan
                               
                sep = coordcd(:)-coordan(:)
                dist = DOT_PRODUCT(sep,sep)

                IF (dist<adist*adist) THEN
                  nb_rough_PRPRx = nb_rough_PRPRx+1
                  IF (dist<0.5D0*adist*adist) nb_rough_half=nb_rough_half+1

                  IF ( nb_rough_PRPRx == 1) THEN
                    ALLOCATE(Root)
                    Current => Root
                    NULLIFY(Root%p)
                  ELSE
                    ALLOCATE(Current)
                    Previous%n => Current
                  END IF
                  Current%val%cd        = icdtac
                  Current%val%an        = iantac
                  Current%val%isee      = isee
                  Current%val%Vsep      = sep/dsqrt(dist)
                  Current%val%xperiodic = 1
                  Current%val%yperiodic = 1
                  Current%p => Previous
                  NULLIFY(Current%n)
                  Previous => Current
                END IF
              END DO
            END DO
            END DO
            END DO
         END DO
       END DO
       END DO
       END DO

       !fd on termine par le coin extreme en bas qui voit le coin initial en haut

       DO ibox3cd = minibox3,maxibox3
       !DO ibox2cd = minibox2,minibox2+1
       DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
       !DO ibox1cd = maxibox1-1,maxibox1
       DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1 
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
           icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
           cdcol = get_color_POLYR(icdtac)
                   
           DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
           !DO ibox2an = maxibox2-1,maxibox2
           DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2
           !DO ibox1an = minibox1,minibox1+1  
           DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
             DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
               IF (is_POLYR_same_RBDY3(icdtac,iantac)) CYCLE
               ancol = get_color_POLYR(iantac)
               if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
                 isee = get_isee_specific('POLYR',cdcol,ancol)
               else
                 isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                                 get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
               end if
               IF (isee.EQ.0) CYCLE
               adist=see(isee)%alert 
                               
               coordcd = PRcoor(1:3,icdtac)
               coordan = PRcoor(1:3,iantac)
               raycd = get_radius_POLYR(icdtac)
               rayan = get_radius_POLYR(iantac)
               coordan(1) = coordan(1) + xperiode
               coordan(2) = coordan(2) - yperiode

               adist=0.1005D+01*adist+raycd+rayan
                               
               sep = coordcd(:)-coordan(:)
               dist = DOT_PRODUCT(sep,sep)

               IF (dist<adist*adist) THEN

                 nb_rough_PRPRx = nb_rough_PRPRx+1

                 IF (dist<0.5D0*adist*adist) nb_rough_half=nb_rough_half+1
                 IF ( nb_rough_PRPRx == 1) THEN
                   ALLOCATE(Root)
                   Current => Root
                   NULLIFY(Root%p)
                 ELSE
                   ALLOCATE(Current)
                   Previous%n => Current
                 END IF
                 Current%val%cd        = icdtac
                 Current%val%an        = iantac
                 Current%val%isee      = isee
                 Current%val%Vsep      = sep/dsqrt(dist)
                 Current%val%xperiodic =  1
                 Current%val%yperiodic = -1
                 Current%p => Previous
                 NULLIFY(Current%n)
                 Previous => Current
               END IF
             END DO
           END DO
           END DO
           END DO
         END DO
       END DO
       END DO
       END DO

    END IF

   IF (ALLOCATED(box)) THEN
      DO ibox3=minibox3,maxibox3
      DO ibox2=minibox2,maxibox2
      DO ibox1=minibox1,maxibox1
        IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%which)) DEALLOCATE(box(ibox1,ibox2,ibox3)%which)
      ENDDO
      ENDDO
      ENDDO
      DEALLOCATE(box)
   ENDIF


   ! the visibility array used in compute_contact is allocated
   IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)
   ALLOCATE(rough_PRPRx(nb_rough_PRPRx),stat=errare)      
   IF (errare /=0 ) call FATERR(IAM,'error in allocating rough_PRPRx')

   IF (nb_rough_half == 0) nb_rough_half=nb_rough_PRPRx

   !fd a minima on considere qu'on aura une colonne de grains
   nb_rough_half=max(nb_POLYR,nb_rough_half)
   
   size_of_this = size_factor*nb_rough_half

   WRITE(cout,'(4X,I10,A20)') nb_rough_PRPRx,' PRPRx roughly found'       
   call logmes(cout)
   WRITE(cout,'(4X,I10,A25)') nb_rough_half, ' PRPRx half roughly found' 
   call logmes(cout)
   WRITE(cout,'(4X,I10,A25)') size_of_this, ' size of this array' 
   call logmes(cout)
   
   ! the oversized array this is temporaly allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(size_of_this),stat=errare) 
   IF (errare /=0 ) call faterr(IAM,'error in allocating this')

   DO icdan=nb_rough_PRPRx,1,-1
     
     Previous => Current%p
     rough_PRPRx(icdan)%cd        = Current%val%cd
     rough_PRPRx(icdan)%an        = Current%val%an
     rough_PRPRx(icdan)%isee      = Current%val%isee
     rough_PRPRx(icdan)%Vsep(1:3) = Current%val%Vsep

     !am: each rough interaction belongs to the default group
     rough_PRPRx(icdan)%group     = NOINT

     rough_PRPRx(icdan)%xperiodic = Current%val%xperiodic
     rough_PRPRx(icdan)%yperiodic = Current%val%yperiodic

     DEALLOCATE(Current)
     Current => Previous

     !print*,'rough detection'
     !print*,'rough contact ',icdan,' between cd ',rough_PRPRx(icdan)%cd,' and an ',rough_PRPRx(icdan)%an
 

   END DO 
   
   NULLIFY(Root)

   nb_rough_half=size_of_this

  END SUBROUTINE creation_tab_visu_PRPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_to_file_PRPRx
   INTEGER                               :: icdtac,iantac,isee,i,test,icdan
   CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
   REAL(kind=8),DIMENSION(3)             :: sep,coordcd,coordan
   REAL(kind=8)                          :: dist,rayan,raycd,adist
   LOGICAL                               :: visible
   TYPE(T_POLYR)                         :: PR1,PR2
   REAL(kind=8)                          :: scal,dist1,dist2,norme
   character(len=80)  :: cout
   character(len=500) :: lcout

   NULLIFY(Root) 
   NULLIFY(Current)
   NULLIFY(Previous)
   nb_rough_PRPRx=0

   DO icdtac=1,nb_POLYR-1
     visible=get_visible_POLYR(icdtac)
     IF (.NOT.visible) CYCLE
     cdcol=get_color_POLYR(icdtac)
     PR1=S_POLYR(icdtac)
     DO iantac=icdtac+1,nb_POLYR
       visible=get_visible_POLYR(iantac)
       IF (.NOT.visible) CYCLE
       ancol=get_color_POLYR(iantac)
       if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
         isee = get_isee_specific('POLYR',cdcol,ancol)
       else
         isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                         get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
       end if
       IF (isee /= 0) THEN
         adist=see(isee)%alert 
         coordcd = PRcoor(1:3,icdtac)
         coordan = PRcoor(1:3,iantac)

         raycd = get_radius_POLYR(icdtac)
         rayan = get_radius_POLYR(iantac)

         ! checking ROUGHLY distance against alert distance   
         ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
         ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
         ! results might be different up to some non significant figures, but when comparing to
         ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

         adist=0.1005D+01*adist+raycd+rayan
         dist= (coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) + &
               (coordcd(2)-coordan(2))*(coordcd(2)-coordan(2)) + &
               (coordcd(3)-coordan(3))*(coordcd(3)-coordan(3))

         IF (dist<adist*adist) THEN
           PR2 = S_POLYR(iantac)
           sep=PR2%center - PR1%center
           norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)
           sep=sep/SQRT(norme)

           dist1 = -10D20
           dist2 =  10D20
           DO i=1,PR1%nb_vertex
             scal=PR1%vertex(1,i)*sep(1)+PR1%vertex(2,i)*sep(2)+PR1%vertex(3,i)*sep(3)
             IF (scal > dist1) THEN
                dist1=scal
             ENDIF
           ENDDO

           DO i=1,PR2%nb_vertex
             scal=PR2%vertex(1,i)*sep(1)+PR2%vertex(2,i)*sep(2)+PR2%vertex(3,i)*sep(3)
             IF (scal < dist2) THEN
               dist2=scal
             ENDIF
           ENDDO

           IF ((dist1-dist2) > -see(isee)%alert) THEN
             nb_rough_PRPRx=nb_rough_PRPRx+1
             IF ( nb_rough_PRPRx == 1) THEN
               ALLOCATE(Root)
               Current => Root
               NULLIFY(Root%p)
             ELSE
               ALLOCATE(Current)
               Previous%n => Current
             ENDIF
             Current%val%cd       =icdtac
             Current%val%an       =iantac
             Current%val%isee     =isee                  
             Current%p => Previous
             NULLIFY(Current%n)
             Previous => Current
           ENDIF
         END IF
       ENDIF  
     END DO
   END DO

  IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)

  ALLOCATE(rough_PRPRx(nb_rough_PRPRx))     ! the visibility array used in compute_contact is allocated

  DO icdan=nb_rough_PRPRx,1,-1
     
    Previous => Current%p
    rough_PRPRx(icdan)%cd        = Current%val%cd
    rough_PRPRx(icdan)%an        = Current%val%an
    rough_PRPRx(icdan)%isee      = Current%val%isee
    rough_PRPRx(icdan)%Vsep(1:3) = 0.D0
    DEALLOCATE(Current)
    Current => Previous
  END DO 
   
  NULLIFY(Root)

  WRITE(cout,'(4X,I10,A20)') nb_rough_PRPRx,' PRPRx roughly found'       
  call logmes(cout)
  

  OPEN(unit=234,STATUS='REPLACE',file='OUTBOX/PROX_TACTORS.OUT')
  WRITE(234,*) nb_rough_PRPRx
  DO icdan=1,nb_rough_PRPRx
    WRITE(234,*)  rough_PRPRx(icdan)%cd,rough_PRPRx(icdan)%an,rough_PRPRx(icdan)%isee
  ENDDO   
  CLOSE(234)
                     !1234567890123456789012345678901234567890123456789012
  call logmes(' ---------------------------------------------------', .true.)
  call logmes(' | You use a special pre-detection routine which   |', .true.)
  call logmes(' | store the visibility table in the file:         |', .true.)
  call logmes(' | OUTBOX/PROX_TACTORS.OUT                         |', .true.)
  call logmes(' | Replace SAVE PROX TACTORS TO FILE by            |', .true.)
  call logmes(' | LOAD PROX TACTORS FROM FILE                     |', .true.)
  call logmes(' | and move OUTBOX/PROX_TACTORS.OUT to file        |', .true.)
  call logmes(' | DATBOX/PROX_TACTORS.DAT                         |', .true.)
  call logmes(' ---------------------------------------------------', .true.)
  call faterr('mod_PRPRx::creation_tab_visu_to_file','ERROR')

  END SUBROUTINE creation_tab_visu_to_file_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_from_file_PRPRx

  IMPLICIT NONE

  ! locals

  INTEGER                    :: isee, cd, an, i, j, err
  integer                    :: nfich
  character(len=5)           :: cdcol, ancol
  real(kind=8)               :: adist, raycd, rayan, dist
  real(kind=8), dimension(3) :: sep
 
  ! number of interactions stored in the file
  integer                    :: nb_interactions

  ! local variable used to compute size of the array this
  integer                    :: size_of_this

  ! with_see_table_index = "true" if the see table is index is given in the file and
  ! with_see_table_index = "false" otherwise
  logical                    :: with_see_table_index  

  character(len=128) :: cout
                           !1234567890123456789012345678901234
  character(len=34) :: IAM='PRPRx::creation_tab_visu_from_file' 

  nfich = get_io_unit()

  OPEN(unit=nfich, STATUS='OLD', file='DATBOX/PROX_TACTORS.DAT', IOSTAT=err)
  IF (err > 0) call faterr(IAM, 'file DATBOX/PROX_TACTORS.DAT cannot be opened!')

  READ(nfich, *) cd
  nb_interactions=cd

  ! first reading : determine if the see table index is given as the third column of the file

  ! assume the see table index is given as the third column of the file
  with_see_table_index = .true.
  ! try to read the file
  do i=1, nb_interactions
     READ(nfich, *, iostat=err) cd, an, isee
     ! catch a IO error
     if (err /= 0) then
       ! the see table index is not given in the file
        with_see_table_index = .false.
        exit
     end if
  end do

  rewind(nfich)

  !print *, 'with_see_table_index=', with_see_table_index

  ! read the file
  !    * first case : the table index is given on the third column
  if (with_see_table_index) then 
     ! in this case, we assume that the interactions were computed by calling "creation_tab_visu_to_file_PRPRx",
     ! so that we consider that all interactions are valid and no more checks are needed 
     nb_rough_PRPRx=nb_interactions

     IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)
     ALLOCATE(rough_PRPRx(nb_rough_PRPRx)) 

     ! second reading : fill the data

     ! forget the first line
     READ(nfich, *) cd

     DO i=1, nb_rough_PRPRx
        READ(nfich, *) cd, an, isee
        rough_PRPRx(i)%cd        = cd
        rough_PRPRx(i)%an        = an
        rough_PRPRx(i)%isee      = isee
     END DO 
  !    * second case : the table index is not given on the third column
  else
     ! in this case, we don't assume anything and we have to check the interactions

     ! second reading : check interactions

     ! forget the first line
     READ(nfich, *) cd

     nb_rough_PRPRx=0

     DO i=1, nb_interactions
        READ(nfich, *) cd, an

        ! if the two POLYR belong to a same body, forget it!
        if (is_POLYR_same_RBDY3(cd, an)) CYCLE

        ! Looking for the interaction law corresponding to the current interaction
        cdcol = get_color_POLYR(cd)
        ancol = get_color_POLYR(an)
        if( polyr2bdyty(3,an) == polyr2bdyty(3,cd) ) then
          isee = get_isee_specific('POLYR',cdcol,ancol)
        else
          isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,cd)),'POLYR',cdcol, &
                          get_body_model_name_from_id(polyr2bdyty(3,an)),'POLYR',ancol)
        end if

        ! if there's no interaction law corresponding to the current interaction, forget it!
        if (isee == 0) cycle

        ! here, we are sure that the current interaction is valid
        nb_rough_PRPRx=nb_interactions 
     end do

     rewind(nfich)

     IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)
     ALLOCATE(rough_PRPRx(nb_rough_PRPRx)) 

     ! third reading : fill the data

     ! forget the first line
     READ(nfich, *) cd

     j=1
     DO i=1, nb_interactions
        READ(nfich, *) cd, an

        ! if the two POLYR belong to a same body, forget it!
        if (is_POLYR_same_RBDY3(cd, an)) CYCLE

        ! Looking for the interaction law corresponding to the current interaction
        cdcol = get_color_POLYR(cd)
        ancol = get_color_POLYR(an)
        if( polyr2bdyty(3,an) == polyr2bdyty(3,cd) ) then
          isee = get_isee_specific('POLYR',cdcol,ancol)
        else
          isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,cd)),'POLYR',cdcol, &
                          get_body_model_name_from_id(polyr2bdyty(3,an)),'POLYR',ancol)
        end if

        ! if there's no interaction law corresponding to the current interaction, forget it!
        if (isee == 0) cycle

        ! here, we are sure that the current interaction is valid
        rough_PRPRx(j)%cd        = cd
        rough_PRPRx(j)%an        = an
        rough_PRPRx(j)%isee      = isee
        j=j + 1
     END DO 
  end if
 
  CLOSE(nfich)

  ! compute separation for each interaction and compute nb_rough_half
  nb_rough_half=0
  do i=1, nb_rough_PRPRx
     cd = rough_PRPRx(i)%cd
     an = rough_PRPRx(i)%an

     ! compute the corrected alert distance
     adist=see(isee)%alert

     raycd=get_radius_POLYR(cd)
     rayan=get_radius_POLYR(an)

     adist=0.1005d+01*(adist + raycd + rayan)

     ! compute the separation
     sep = PRcoor(1:3, cd) - PRcoor(1:3, an)
     dist=DOT_PRODUCT(sep, sep)

     ! update nb_rough_half
     !fd @@@ half half !? on est au carre alors pourquoi pas 0.25 ?
     IF (dist < 0.5d0*adist*adist) nb_rough_half=nb_rough_half + 1

     ! The new interaction is stored
     rough_PRPRx(i)%Vsep(1:3) = sep/dsqrt(dist)
    
     ! no periodic conditions
     rough_PRPRx(i)%xperiodic = 0
     rough_PRPRx(i)%yperiodic = 0
  end do

  WRITE(cout,'(4X,I10,A20)') nb_rough_PRPRx,' PRPRx roughly found'       
  call logmes(cout)
  WRITE(cout, '(4X,I10,A)') nb_rough_half,' POLYR half roughly found'
  call logmes(cout)

  ! "this" array is allocated
  IF (ALLOCATED(this)) DEALLOCATE(this)

  IF (nb_rough_half==0) nb_rough_half=nb_rough_PRPRx

  size_of_this=INT(ABS(size_factor*nb_rough_half))

!  IF (size_of_this==0) size_of_this=4*nb_rough_half

  ALLOCATE(this(size_of_this))            ! the oversized array this is temporaly allocated

  nb_rough_half=size_of_this

  END SUBROUTINE creation_tab_visu_from_file_PRPRx
  !------------------------------------------------------------------------

!------------------------------------------------------------------------
!--- routines de detection ----
!------------------------------------------------------------------------



  ! debut CP Cundall
  
  !------------------------------------------------------------------------  
  !fd detection du contact par "common plane"
  !fd 2 stratégies: 
  !fd   - Cundall qui cherche a placer un plan separateur a l'ensemble de l'objet et 
  !fd     qui ne gere pas la coherence temporelle; i.e. on redetecte from scratch a chaque pas 
  !fd   - f2f qui cherche des faces topologiques en vis a vis à la premiere detection et
  !fd     qui utilise exclusivement la persistence temporelle pour mettre a jour; i.e. on
  !fd     on ne redecte pas
   SUBROUTINE wcp_compute_contact_PRPRx(reset)
 
   IMPLICIT NONE  

   logical, optional :: reset

   INTEGER                     :: errare 
   INTEGER                     :: icdan,iadj,itac
   INTEGER                     :: icdtac,iantac,isee,itacty    
   integer                     :: group
   REAL(kind=8)                :: raycd,rayan,adist,dist,nonuc,gap
   INTEGER                     :: r_icdtac,r_iantac
   INTEGER                     :: i,id,j,nb_ctc,k
   INTEGER                     :: size_of_array_this
   ! position point de contact 
   REAL(kind=8),DIMENSION(3,4) :: xco 
   ! gap
   REAL(kind=8),DIMENSION(4)   :: ovlap
   ! vecteurs 
   REAL(kind=8),DIMENSION(3)   :: sep,t,n,s,cdlev,anlev,Point,Vsep
   REAL(kind=8)                :: norme,den,scal
   REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
   REAL(kind=8)                :: t1,t2,vls_cst,vln_cst,vlt_cst
   REAL(kind=8)                :: area,pt_surf = 1.d0 ! a modifier !!

   real(kind=8),dimension(3)   :: center

   integer :: cd_ent,an_ent

   ! f2f 
   logical, save :: is_first_time_f2f = .true.
   integer :: nb_potential_contact,i_visavis

   !fd index du visavis actif. necessaire pour calculer la vitesse thermique
   integer :: iv 

                             !12345678901234567890123456
   character(len=26)  :: IAM='PRPRx::wcp_compute_contact'
   character(len=80)  :: cout
   character(len=450) :: lcout

   ! fd le champion de la grosse merde
   integer(kind=4) :: nc_nbf,nc_nb_ctc
   integer(kind=4),dimension(:),allocatable :: nc_sort
   integer     ,dimension(:)  ,pointer  :: nc_status
   REAL(kind=8),DIMENSION(:,:),pointer  :: nc_xco,nc_t,nc_n,nc_s 
   REAL(kind=8),DIMENSION(:)  ,pointer  :: nc_ovlap,nc_surf
   real(kind=8) :: nc_gdist
   logical :: is_done

   !fd pour le calcul de la coordonnee reduite
   real(kind=8),dimension(3)   :: v
   REAL(kind=8),DIMENSION(3,3) :: frameTT

   logical                     :: was_used=.false.   
   
   if ( present(reset)) then
     if (reset) then 
       is_first_time_f2f = .true.
       return
     endif  
   end if

   icdan   = 0        
   nb_PRPRx= 0
   nb_adj  = 0
   nb_detection_test=0
   detection_time=0.D0
   nb_shadow=0
   nb_ctc_state=0

   xco=0.d0

   IF (nb_rough_PRPRx /= 0 ) THEN

     nb_potential_contact = nb_rough_PRPRx 

     size_of_array_this=SIZE(this)
  
     iv = -99 ! n existe pas

     if (with_f2f) then
       if (is_first_time_f2f) then
         ALLOCATE(visavis(size_of_array_this))
         nb_visavis = 0
         i_visavis=0

         !fd initialization on visavis data structure
         do i=1,size_of_array_this
           allocate(visavis(i)%iff_cd(4),visavis(i)%iff_an(4), &
                    visavis(i)%cd_lcoor(3,4),visavis(i)%an_lcoor(3,4), &
                    visavis(i)%index(4),visavis(i)%pt_area(4))
           visavis(i)%nb_ctc=0
           visavis(i)%nb_ctc_rough=0
         enddo

       else
         nb_potential_contact = nb_visavis  
       endif 
     endif

     !
     ! preparation de la detection 
     !
     DO i=1,nb_potential_contact
       icdtac    = rough_PRPRx(i)%cd
       iantac    = rough_PRPRx(i)%an 
       isee      = rough_PRPRx(i)%isee

       nb_ctc    = 0

       !am: get the group to which belongs the potential interaction
       group     = rough_PRPRx(i)%group

       !fd detection explicite on ecrabouille rough
       if (with_f2f) then
         if (.not. is_first_time_f2f) then
           icdtac    = visavis(i)%cd
           iantac    = visavis(i)%an 
           isee      = visavis(i)%isee
       
           !am: get the group to which belongs the potential interaction
           group     = visavis(i)%group

           iv = i

           if ( .NOT. get_visible_POLYR(icdtac) .or. .NOT. get_visible_POLYR(iantac) ) CYCLE

         endif

         visavis(i)%index=0
         visavis(i)%nb_ctc=0

       endif          
       
       adist=see(isee)%alert 

       perio_shift = 0.d0
       perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
       perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode

       if (.not. was_used .or. &
           .not. update_cohesive_existing_interaction(icdtac,iantac,perio_shift,4,nb_ctc,xco,t,n,s,ovlap) ) then

         dist=S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist
          
         sep=S_POLYR(icdtac)%center - (S_POLYR(iantac)%center + perio_shift)
         norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)

         IF (norme <1.D-24) THEN
           write(cout,'(A,I0)')        ' For rough contact ',i 
           call logmes(cout, .true.)
           write(cout,'(A,I0,A,I0)')   ' Distance between cd ',icdtac, ' and an ',iantac
           call logmes(cout, .true.)
           write(cout,'(A,3(1x,D14.7))')     'center of cd ', S_POLYR(icdtac)%center
           call logmes(cout, .true.)
           write(cout,'(A,3(1x,D14.7))')       'and center of an ',S_POLYR(iantac)%center
           call logmes(cout, .true.)
           write(cout,'(A,D14.7)')     'is ', norme 
           call logmes(cout, .true.)
           call faterr(IAM,'ERROR')
         ENDIF   

         !fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
         !fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
         !fd @@@ et il en manque je pense ...

         IF (norme < dist*dist) THEN

           IF (((S_POLYR(iantac)%maxpos(1)+perio_shift(1))- &
                 S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
           IF (((S_POLYR(iantac)%maxpos(2)+perio_shift(2))- &
                 S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
           IF (( S_POLYR(iantac)%maxpos(3)                - &
                 S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE

           IF ((S_POLYR(icdtac)%maxpos(1)- &
               (S_POLYR(iantac)%minpos(1)+perio_shift(1))+adist)<0.D0) CYCLE
           IF ((S_POLYR(icdtac)%maxpos(2)- &
               (S_POLYR(iantac)%minpos(2)+perio_shift(2))+adist)<0.D0) CYCLE
           IF ((S_POLYR(icdtac)%maxpos(3)- &
                S_POLYR(iantac)%minpos(3)                +adist)<0.D0) CYCLE

           sep = rough_PRPRx(i)%Vsep

           CALL cpu_time(t1)

           is_done=.False.
         
           if (with_f2f) then

             if (is_first_time_f2f) then

               CALL DETECTION_F2F_encoreplusnew(iantac,icdtac,sep,adist,&
                                                nb_ctc,xco,ovlap,n,t,s, &
                                                visavis(i_visavis+1))
               if (nb_ctc /= 0) then
                 i_visavis = i_visavis + 1
                 visavis(i_visavis)%isee = isee

                 !am: set the group to which belongs the visavis structure
                 visavis(i_visavis)%group = group
 
                 iv = i_visavis
                 visavis(iv)%nb_ctc_rough=visavis(iv)%nb_ctc
               
               endif
             else
               ! print*,icdan
               visavis(iv)%nb_ctc=visavis(iv)%nb_ctc_rough
               call update_f2f(visavis(iv),nb_ctc,xco,ovlap,n,t,s)
             endif

           else

             !fd 21/03/2016            
             if (nb_big_polyr /= 0) then
               if (any(big_polyr == iantac) ) then

                 nullify(nc_status,nc_xco,nc_ovlap,nc_t,nc_n,nc_s,nc_surf)

                 ! nc_gdist=see(isee)%global_alert
                 nc_gdist=max(see(isee)%global_alert,1.2*S_POLYR(iantac)%max_radius_face)

                 !fd on trim 
                 ! CALL DETECTION_non_convex(iantac,icdtac,nc_gdist,adist,nc_nb_ctc,nc_nbf, &
                 !      nc_status,nc_xco,nc_ovlap,nc_t,nc_n,nc_s,nc_surf,.false.)
                 CALL DETECTION_non_convex(iantac,icdtac,nc_gdist,adist,nc_nb_ctc,nc_nbf, &
                                           nc_status,nc_xco,nc_ovlap,nc_t,nc_n,nc_s,nc_surf,.true.)

                 !if (nc_nb_ctc > 4) then
                 !  print*,'XXXXXXXXXXXXXXX'
                 !  print*,nb_ctc,nc_nbf
                 !  print*,nc_ovlap
                 !  print*,nc_status
                 !  print*,adist,see(isee)%global_alert
                 !  print*,nc_surf
                 !  do j=1,nc_nbf
                 !    print*,nc_n(:,j)
                 !    print*,nc_xco(:,j)
                 !  enddo   
                 !  !   call faterr(IAM,'problem with big polyr detection')
                 !endif   

                 if (nc_nb_ctc == 0 ) then
                    nb_ctc = 0
                    deallocate(nc_status,nc_xco,nc_ovlap,nc_t,nc_n,nc_s,nc_surf)
                    cycle
                 endif
               
                 ! pour ne garder que les noeuds/face
                 allocate(nc_sort(nc_nbf))
                 do j=1,nc_nbf
                   !if (nc_status(j) < 3) then
                   if (nc_status(j) < 1) then                      
                     nc_sort(j)=-1
                   else
                     nc_sort(j)=0
                   endif
                 enddo 

                 nb_ctc = 0
                 area=0.d0
                 do j=1,nc_nb_ctc
                   if (any(nc_sort == 0)) then 
                     k = minloc(nc_ovlap,dim=1,mask=(nc_sort==0))
                     nb_ctc = nb_ctc + 1 
                     nc_sort(k)=nb_ctc
                     xco(:,nb_ctc)=nc_xco(:,k)
                     ovlap(nb_ctc)=nc_ovlap(k)                 
                     if (nb_ctc==1) then   
                       t(:)=nc_t(:,k)
                       n(:)=nc_n(:,k)
                       s(:)=nc_s(:,k)
                     endif  
                     area=area+nc_surf(k)
                     if (nb_ctc == 4) exit
                   else
                     exit  
                   endif  
                 enddo    
               
                 deallocate(nc_sort,nc_status,nc_xco,nc_ovlap,nc_t,nc_n,nc_s,nc_surf)

                 !if (nc_nb_ctc > 4) then
                 !  print*,ovlap
                 !  print*,n
                 !  do j=1,nb_ctc
                 !    print*,xco(:,j) 
                 !  enddo   
                 !  print*,'XXXXXXXXXXXXXXX'
                 !endif

                 !nb_ctc =min(4,nc_nb_ctc)
              
                 is_done=.True.

               endif
             endif          

             if (.not. is_done) then

               CALL DETECTION_COMMON_PLANE(iantac,icdtac,sep,adist,nb_ctc,xco,ovlap,n,t,s,center,area)

             endif

           endif ! 
        
           CALL cpu_time(t2)

           rough_PRPRx(i)%Vsep=sep

           nb_tot_detect=nb_tot_detect+1
           nb_detection_test=nb_detection_test+1
           detection_time=detection_time+t2-t1

         endif
       else
         if (with_f2f .and. .not. is_first_time_f2f) then
           if (nb_ctc /= visavis(iv)%nb_ctc_rough) call faterr(IAM,'not able to recover all cohesive contact points')  
           visavis(iv)%nb_ctc=visavis(iv)%nb_ctc_rough
         endif
       endif           

       IF (nb_ctc==0) CYCLE

         
       localframe_cd = get_inertia_frameTT_POLYR(icdtac)
       localframe_an = get_inertia_frameTT_POLYR(iantac)

       cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
       an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
           
       vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ &
               (cd_Vbegin(2)-an_Vbegin(2))*t(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*t(3)
       vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ &
               (cd_Vbegin(2)-an_Vbegin(2))*n(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*n(3)
       vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ &
               (cd_Vbegin(2)-an_Vbegin(2))*s(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*s(3)
       
       DO j=1,nb_ctc

           icdan = icdan + 1

           IF (icdan>size_of_array_this) THEN
                call logmes('---------------------------------------------', .true.)
                call logmes('ERROR filling this                           ', .true.)
                call logmes('you reach the allocated size                 ', .true.)
                call logmes('In your python script use                    ', .true.)
                call logmes('                                             ', .true.)
                call logmes('PRPRx_LowSizeArrayPolyr(sizefactor)          ', .true.)
                call logmes('                                             ', .true.)
                call logmes('where sizefactor is an integer specifiyng the', .true.)
                call logmes('ratio of memory you need (=4 by default)     ', .true.)
                call logmes('---------------------------------------------', .true.)

                write(cout,'(4x,I10,A25)') icdan, ' rank of wrong contact' 
                call logmes(cout, .true.)

                call faterr(IAM,'Error')
           ENDIF   

           if (with_f2f) visavis(iv)%index(j) = icdan

           this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
           this(icdan)%ianbtac = polyr2bdyty(2, iantac)
           
           this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
           this(icdan)%ianbtyp = polyr2bdyty(3, iantac)

           this(icdan)%icdctyp = i_polyr
           this(icdan)%ianctyp = i_polyr

           nb_adj(icdtac)      = nb_adj(icdtac) + 1
           iadj                = nb_adj(icdtac)
           this(icdan)%iadj    = iadj
           this(icdan)%icdbdy  = polyr2bdyty(1, icdtac)
           this(icdan)%icdtac  = icdtac
           this(icdan)%ianbdy  = polyr2bdyty(1, iantac)
           this(icdan)%iantac  = iantac
           this(icdan)%isee    = isee
           this(icdan)%tuc     = t
           this(icdan)%nuc     = n
           this(icdan)%suc     = s
           !am: set the group of the new interaction
           this(icdan)%group    = group
           this(icdan)%coor     = xco(1:3, j)
           this(icdan)%type_ctc = nb_ctc

           cd_ent = get_ent_POLYR(this(icdan)%icdtac)
           an_ent = get_ent_POLYR(this(icdan)%iantac) 
         
           this(icdan)%icdent = cd_ent
           this(icdan)%ianent = an_ent

           entity(cd_ent)%nb = entity(cd_ent)%nb + 1
           entity(an_ent)%nb = entity(an_ent)%nb + 1

           ! fd fait plus bas
           ! ! coor reduite cd 
           ! v = xco(1:3,j) - PRcoor(1:3,icdtac)
           ! frameTT= get_inertia_frameTT_POLYR(this(icdan)%icdtac)
           ! this(icdan)%icdcoor = matmul(frameTT,v)
           ! ! coor reduite an 
           ! v = xco(1:3,j) - (PRcoor(1:3,iantac)+ perio_shift)
           ! frameTT= get_inertia_frameTT_POLYR(this(icdan)%iantac)
           ! this(icdan)%iancoor = matmul(frameTT,v)
             
           !fd le 11/09/08 manque le shift. PRcoor c'est le centre du polyr pas le centre d'inertie
           cdlev = xco(1:3,j)  &
                 - (PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac))
           anlev = xco(1:3,j) &
                 - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift)

           ! if (icdan == 1896) then
           ! print*,'contact: ',icdan,j,icdtac,iantac
           ! write(*,'(A,3(1x,E12.5))') 'xco',xco(:,j)
           ! write(*,'(A,3(1x,E12.5))') 'cdcoor',PRcoor(:,icdtac)
           ! write(*,'(A,3(1x,E12.5))') 'ancoor',PRcoor(:,iantac)
           ! write(*,'(A,3(1x,E12.5))') 'cds'   ,get_shiftTT_POLYR(icdtac)
           ! write(*,'(A,3(1x,E12.5))') 'ans'   ,get_shiftTT_POLYR(iantac)
           ! write(*,'(A,3(1x,E12.5))') 'ps'    ,perio_shift           
           ! write(*,'(A,3(1x,E12.5))') 'anlev',anlev 
           ! write(*,'(A,3(1x,E12.5))') 'cdlev',cdlev 
           ! write(*,'(A,3(1x,E12.5))') 'anlev',anlev 
           ! write(*,'(A)')             'local frame cd'
           ! write(*,'(3(1x,E12.5))') localframe_cd
           ! write(*,'(A)')             'local frame an'
           ! write(*,'(3(1x,E12.5))') localframe_an
           ! endif
           ! 
           !fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
           !fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

           this(icdan)%icdcoor(1)=cdlev(1)*localframe_cd(1,1)+ &
                                  cdlev(2)*localframe_cd(2,1)+ &
                                  cdlev(3)*localframe_cd(3,1)
           this(icdan)%icdcoor(2)=cdlev(1)*localframe_cd(1,2)+ &
                                  cdlev(2)*localframe_cd(2,2)+ &
                                  cdlev(3)*localframe_cd(3,2)
           this(icdan)%icdcoor(3)=cdlev(1)*localframe_cd(1,3)+ &
                                  cdlev(2)*localframe_cd(2,3)+ &
                                  cdlev(3)*localframe_cd(3,3)

           this(icdan)%iancoor(1)=anlev(1)*localframe_an(1,1)+ &
                                  anlev(2)*localframe_an(2,1)+ &
                                  anlev(3)*localframe_an(3,1)
           this(icdan)%iancoor(2)=anlev(1)*localframe_an(1,2)+ &
                                  anlev(2)*localframe_an(2,2)+ &
                                  anlev(3)*localframe_an(3,2)
           this(icdan)%iancoor(3)=anlev(1)*localframe_an(1,3)+ &
                                  anlev(2)*localframe_an(2,3)+ &
                                  anlev(3)*localframe_an(3,3)

           ! On va calculer le passage rep inertie -> rep général pour l'antagoniste

           Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
           Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
           Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

           Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
           Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
           Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

           Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
           Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
           Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)

           this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                                Rc(1,2)*this(icdan)%tuc(2) + &
                                Rc(1,3)*this(icdan)%tuc(3) 
           this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
           this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

           this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
           this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
           this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

           this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
           this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
           this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

           ! On va calculer le passage rep inertie -> rep général pour le candidat

           Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
           Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
           Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

           Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
           Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
           Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

           Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
           Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
           Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)


           this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                                Rc(1,2)*this(icdan)%tuc(2) + &
                                Rc(1,3)*this(icdan)%tuc(3) 
           this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + &
                                Rc(2,2)*this(icdan)%tuc(2) + &
                                Rc(2,3)*this(icdan)%tuc(3) 
           this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + &
                                Rc(3,2)*this(icdan)%tuc(2) + & 
                                Rc(3,3)*this(icdan)%tuc(3) 

           this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
           this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
           this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

           this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
           this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
           this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 


           ! if (icdan == 1896) then
           ! print*,'contact: ',icdan,icdtac,iantac
           ! write(*,'(A,3(1x,E12.5))') 'Gcdt',this(icdan)%Gcdt
           ! write(*,'(A,3(1x,E12.5))') 'Gcdn',this(icdan)%Gcdn
           ! write(*,'(A,3(1x,E12.5))') 'Gcds',this(icdan)%Gcds
           ! write(*,'(A,3(1x,E12.5))') 'Gant',this(icdan)%Gant
           ! write(*,'(A,3(1x,E12.5))') 'Gann',this(icdan)%Gann
           ! write(*,'(A,3(1x,E12.5))') 'Gans',this(icdan)%Gans            
           ! endif


           
           this(icdan)%gapTTbegin = ovlap(j)

           ! Calcul des vitesses relatives

           this(icdan)%vltBEGIN = vlt_cst &
                                + cd_Vbegin(4)*this(icdan)%Gcdt(1) &
                                + cd_Vbegin(5)*this(icdan)%Gcdt(2) &
                                + cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                                - an_Vbegin(4)*this(icdan)%Gant(1) &
                                - an_Vbegin(5)*this(icdan)%Gant(2) &
                                - an_Vbegin(6)*this(icdan)%Gant(3)
           
           this(icdan)%vlnBEGIN = vln_cst &     
                                + cd_Vbegin(4)*this(icdan)%Gcdn(1) &
                                + cd_Vbegin(5)*this(icdan)%Gcdn(2) &
                                + cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                                - an_Vbegin(4)*this(icdan)%Gann(1) &
                                - an_Vbegin(5)*this(icdan)%Gann(2) &
                                - an_Vbegin(6)*this(icdan)%Gann(3)

           this(icdan)%vlsBEGIN = vls_cst &     
                                + cd_Vbegin(4)*this(icdan)%Gcds(1) &
                                + cd_Vbegin(5)*this(icdan)%Gcds(2) &
                                + cd_Vbegin(6)*this(icdan)%Gcds(3) &
                                - an_Vbegin(4)*this(icdan)%Gans(1) &
                                - an_Vbegin(5)*this(icdan)%Gans(2) &
                                - an_Vbegin(6)*this(icdan)%Gans(3)


           this(icdan)%rls      = 0.D0
           this(icdan)%rlt      = 0.D0
           this(icdan)%rln      = 0.D0
           this(icdan)%vls      = this(icdan)%vlsBEGIN
           this(icdan)%vlt      = this(icdan)%vltBEGIN
           this(icdan)%vln      = this(icdan)%vlnBEGIN
           this(icdan)%gapTT    = this(icdan)%gapTTbegin
           this(icdan)%status   = i_nknow

           if (with_f2f) then
             this(icdan)%area    = visavis(iv)%pt_area(j)
             this(icdan)%id_f_cd = visavis(iv)%id_f_cd
             this(icdan)%id_f_an = visavis(iv)%id_f_an
             this(icdan)%icdsci  = j
           else
             this(icdan)%area   = area/nb_ctc !pt_surf
             this(icdan)%icdsci = 0
           endif
           this(icdan)%iansci = 0

       end do
     end do


     was_used=.true.
     
     nb_PRPRx=icdan

     if (with_f2f .and. is_first_time_f2f) then
       is_first_time_f2f = .false.
       nb_visavis = i_visavis
     endif
    
   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
   call logmes(cout)
   write(cout,*) 'Total time of detection: ',detection_time
   call logmes(cout)
   write(cout,*) 'Nb detection tests :',REAL(nb_detection_test,8)     
   call logmes(cout)


   do icdan = 1, nb_PRPRx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   DO itac=1,nb_POLYR
     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
     IF (nb_adj(itac) /= 0) THEN
       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
       IF (errare /=0 ) THEN
         call faterr(IAM,' error in allocating adjac(icdtac)%.....')
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_PRPRx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PRPRx),stat=errare)

 END SUBROUTINE wcp_compute_contact_PRPRx

!------------------------------------------------------------------------
!fd travail conjoint avec robert perales de l'ema
!
!------------------------------------------------------------------------

 SUBROUTINE DETECTION_COMMON_PLANE(id1,id2,Nsep,adist, &
                                   nb_ctc,PT_CTC,overlap,Nr,t,s,mid_P,area)

! I
! id1 : id solide 1 (antagoniste)
! id2 : id solide 2 (candidat)
! Nsep: est la derniere normale au plan separateur connue
!       l'intercentre si on fait une recherche rough a chaque pas
! adist: distance d'alerte    
!
! O 
! nb_ctc            : nombre de points de contact
! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
!                     attention c'est le meme pour tous les contacts !!
! overlap(1:nb_ctc) : les gaps aux points de contact
! Nr,t,s            : repere local aux points de contact (si il y a contact)
! Nsep              : normale au plan separateur actualisee
! mid_P             : point de ref du plan separateur
! area              : surface du contact ; la meme pour tout le monde   

   IMPLICIT NONE


   INTEGER                          :: id1,id2,nb_ctc
   REAL(kind=8),DIMENSION(3)        :: Nsep,mid_P,Nr,t,S
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
   REAL(kind=8),DIMENSION(4)        :: overlap
   
   INTEGER,PARAMETER                :: nb_ptc_max=100
   INTEGER                          :: i,k,j,m,inc,inc1,nb_stored,i1,i2
   REAL(kind=8)                     :: dist1,dist2,norm,scal
   REAL(kind=8),DIMENSION(3)        :: PT,s1,s2,s3,r1,r2,r3,Nsep_o,PC
   REAL(kind=8)                     :: gap
   INTEGER                          :: nb_ptc1,nb_ptc2 


   REAL(kind=8),DIMENSION(3,nb_ptc_max)    :: PT1,PT2

   INTEGER,DIMENSION(2*nb_ptc_max)         :: is_ok
   
   !infos liees au contact [gap:normale]   
   REAL(kind=8),DIMENSION(5,nb_ptc_max)    :: PT1_loc,PT2_loc  

   TYPE(T_POLYR)                    :: PRan,PRcd
   INTEGER                          :: nb_face_cd=0,nb_face_an=0,errare,nb_select
   REAL(kind=8),DIMENSION(3)        :: Ncd
 
   REAL(kind=8)                     :: norm_max,norm_min,adist

   LOGICAL                          :: bavard=.false.   !.true.

   
   INTEGER                          :: nb_vertex_pran_min,i_max
   INTEGER                          :: nb_vertex_prcd_min,j_max

   REAL(kind=8)                     :: denom,num,ss,tt
   REAL(kind=8)                     :: max_theta

   REAL(kind=8),DIMENSION(3)        :: p1,p2,mid_normal,max_P1,listsup
   REAL(kind=8),DIMENSION(3)        :: s1moy,s2moy,s3moy,r1moy,r2moy,r3moy


   REAL(kind=8),DIMENSION(:,:), ALLOCATABLE   :: Pcd,Pan,rr1,ss1   
   REAL(kind=8)                     :: theta

   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_skip,an_skip
   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_face,an_face

  ! plan moyen

   INTEGER                        :: kk,imax1,jmin1,jmin,imax,jmax1,jmax

   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max, &
                                     G_ini,G_ini_0,angle,angle_min,angle_max,c,si,crit

   REAL(kind=8),DIMENSION(3,4)    :: normal

   REAL(kind=8),DIMENSION(3)      :: t_temp,n_ini,vec

   REAL(kind=8),DIMENSION(4)      :: G_max
   integer,DIMENSION(4)           :: all_min
   integer,DIMENSION(4)           :: all_max

   INTEGER :: irot

   REAL(kind=8),DIMENSION(2)      :: lvec

   !fd 

   REAL(kind=8)                   :: rd
   integer                        :: dump_fich

                               !12345678901234567890123456789
   CHARACTER(len=29)  :: IAM = 'PRPRx::detection_common_plane'


   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc

   character(len=11) :: nom

   logical :: is_reverse=.FALSE.,is_found

   integer :: ii,icheck

   integer,allocatable :: itmp(:)
   real(kind=8),allocatable :: vtmp(:,:)

   real(kind=8) ::norm1,norm2,ref_size,c_center(2),c_vec(2), &
                  zero,dir(2),max_norm,dt, rot(2,2)

   integer :: fi,fj,nb_iter

   real(kind=8) :: area

   integer :: je,js
   integer :: cd_status,cd_geo,an_status,an_geo

   integer :: max_set,min_set
   real(kind=8) :: min_dist,max_dist
   real(kind=8),dimension(:,:),allocatable :: sommets

   character(len=90) :: cout
   integer           :: err_

   ! angle 1d-3 == 2.5 deg ; 1d-2 == 8 deg ; 1.52d-2 = 10 deg ; 6e-2 = 20 deg
   real(kind=8),parameter :: tol_angle=1d-2
   !real(kind=8),parameter :: tol_angle=5d-2   
   

   real(kind=8) :: d1,d2,tol_proj,gapP

   !fd pas de surprise si j'initialise (dicton du jour le plus con) 

   nb_ctc=0

   Nr = 0.d0
   t  = 0.d0
   s  = 0.d0

   !fd
   Nsep_o = Nsep 

   !
   PRan    = S_POLYR(id1)
   PRcd    = S_POLYR(id2)

   ! gestion du bavardage
   bavard=.false.
   if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) bavard=.true.
   !write(6,*) PRcd%id, dbg_idcd, PRan%id, dbg_idan, ' BAVARD ', bavard

   ALLOCATE(cd_skip(PRcd%nb_vertex),an_skip(PRan%nb_vertex))
   cd_skip = 0
   an_skip = 0

   ALLOCATE(cd_face(PRcd%nb_faces),an_face(PRan%nb_faces))
   cd_face = 0
   an_face = 0

   !fd calcul du shadow overlap et des noeuds "visibles" (cd_skip/an_skip)
   !fd c'est une recherche rapide (directions connues) d un plan separateur
   !fd permet d'identifier les faces (et les noeuds) qui ne se voient pas 
      
   icheck=compute_shadow_overlap(PRcd,PRan,perio_shift,adist,Nsep,gap, &
                                 cd_skip,an_skip,cd_face,an_face,.false.)


   if (icheck == 0) then
     deallocate(cd_skip,an_skip)
     deallocate(cd_face,an_face)

     if (bavard) then
       write(cout,*) 'search contact of cd ',PRcd%Id,' with an ',PRan%id
       call logmes(cout, .true.)
       write(cout,*) '< so'
       call logmes(cout, .true.)
       write(cout,*) 'no contact'
       call logmes(cout, .true.)
       write(cout,*) 'so />'
       call logmes(cout, .true.)
     endif
    
     RETURN
   endif

   if (bavard) then
     write(cout,*) 'search contact of cd ',PRcd%Id,' with an ',PRan%id
     call logmes(cout, .true.)
     write(cout,*) '< so'
     call logmes(cout, .true.)
     write(cout,*) 'shadow overlap stage:'
     call logmes(cout, .true.)
     write(cout,*) 'ini n '  , nsep
     call logmes(cout, .true.)
     write(cout,*) 'ini gap ', gap
     call logmes(cout, .true.)
     ! write(cout,*) 'cd skip ', cd_skip
     ! call logmes(cout, .true.)
     ! write(cout,*) 'an skip ', an_skip
     ! call logmes(cout, .true.)
     write(cout,*) 'so />'
     call logmes(cout, .true.)
   endif

   !fd 19/03/2013 on ne garde que les sommets des faces
   !fd permet d'accelerer les calculs

   do i=1,PRcd%nb_vertex
     if (cd_skip(i) == 0) cycle
     if (count(PRcd%f2f_sommet == i) == 0) cd_skip(i) = 0
   enddo
   do i=1,PRan%nb_vertex
     if (an_skip(i) == 0) cycle
     if (count(PRan%f2f_sommet == i) == 0) an_skip(i) = 0
   enddo

   ! on admet que la valeur de Nsep (qui sort de so) n est pas trop deconnante 
   ! on la garde pour initialiser le plan separateur ...

   mid_normal(:)=Nsep(:)
   mid_P(:)=((PRan%center(:)+perio_shift(:))+PRcd%center(:))*0.5d0

   !fd on recherche le plan separateur: point + normale
   !fd tient compte des noeuds "visibles" et
   !fd met a jour en se basant sur le cundall_neighbor (modifiable)
   !fd la plus petite perturbation est de 0.1 deg !!

   if (bavard) then
     write(cout,*) '< ccp'
     call logmes(cout, .true.)
     write(cout,*) 'cundall common plane stage:'
     call logmes(cout, .true.)
     write(cout,*) 'normal prediction',mid_normal
     call logmes(cout, .true.)
     write(cout,*) 'point prediction',mid_P
     call logmes(cout, .true.)
   endif

   icheck = compute_cundall_common_plane(PRcd,PRan,perio_shift,&
                                         mid_P,t,mid_normal,s, &
                                         gap, &
                                         jmin,imax, &
                                         cd_skip,an_skip, &
                                         nb_iter,.false.)
   if (bavard) then
     write(cout,*) 'nb iter ', nb_iter
     call logmes(cout, .true.)
     write(cout,*) 'normal ' , mid_normal
     call logmes(cout, .true.)
     write(cout,*) 'point '  , mid_P
     call logmes(cout, .true.)
     write(cout,*) 'gap '    , gap
     call logmes(cout, .true.)
     write(cout,*) 'updt cd skip ', cd_skip
     call logmes(cout, .true.)
     write(cout,*) 'updt an skip ', an_skip
     call logmes(cout, .true.)
     write(cout,*) 'ccp />'
     call logmes(cout, .true.)
   endif

   if (icheck == 0) call logmes('Cundall common plane algorithm didn t converge')

   !fd new method

   if (new_ccpm) then

     if (bavard) then
       write(cout,*) '< ctd'
       call logmes(cout, .true.)
       write(cout,*) 'contact type definition stage:'
       call logmes(cout, .true.)
       write(cout,*) 'nb cd skip: ', count(cd_skip > 0),' nb an skip: ', count(an_skip > 0)
       call logmes(cout, .true.)
     endif

     !write(6,*) 'cd ******* '
     !write(6,*) 'sommets'
     !write(6,*) PRcd%f2f_sommet
     !write(6,*) 'actifs'
     !write(6,*) cd_skip
     !js=0
     !je=0
     !do i=1,size(cd_skip)
     !  if (cd_skip(i) == 0) cycle
     !  if (count(PRcd%f2f_sommet == i) /=0 ) then
     !    js=js+1
     !    do j=1,size(cd_skip)
     !      if (cd_skip(j) == 0) cycle
     !      if (j==i) cycle
     !      if (count(PRcd%f2f_sommet == j) ==0) cycle
     !      do k=1,size(PRcd%f2f_edge,dim=2)
     !        if (PRcd%f2f_edge(1,k) == i .and. &
     !            PRcd%f2f_edge(2,k) == j ) then
     !          write(6,*) i,j
     !          je=je+1
     !          exit
     !        endif
     !      enddo
     !    enddo
     !  endif
     !enddo
     !write(6,*) 'cd ',PRcd%id,' nb sommets actifs ',js ,' nb edge ',je


     !fd recherche le type de contact candidat
     !fd cd_status peut valoir 3 si face, 2 si ligne, 1 si point

     !fd attention :
     !fd on fait ca pour essayer d etre plus precis
     !fd mais si la solution trouvee est trop differente de
     !fd celle trouvee par le plan separateur alors on la vire
     !fd de toute facon on garde la normale du plan separateur


     gapP=gap

     
     !fd on compte le nombre de sommets dans le cundall_neighbor
     
     js = count(cd_skip > 0)

     cd_status=0
     cd_geo=0

     if (js > 2) then

       ! face ?
       ! on cherche avec un test d'alignement des normales (faces et plan separateur)

       cd_status= 3

       is_found=.FALSE.
       do k=1,size(PRcd%f2f_set)

         !fd test d'alignement des normales

         dist1 = dot_product(-PRcd%normal(:,PRcd%f2f_set(k)%G_i(1)),mid_normal)

         if ( dist1 >= 1.d0 - f2f_tol) then

           if (bavard) then
             je=0
             do i=1,size(cd_skip)
               if (cd_skip(i) <= 0) cycle
               if (count(PRcd%f2f_contour(k)%G_i == i) /=0 ) je=je+1
             enddo
               
             write(6,*) 'cd face',k,' is seen ...'
             write(6,*) '... it contains ',je,' nodes over ',js
           endif

           is_found=.TRUE.
           exit
         endif
       enddo

       if (.not. is_found) then

         ! le test ne passe pas on garde la face avec la moins mauvaise normale

         max_dist = 0.d0
         max_set = 0

         do k=1,size(PRcd%f2f_set)

           dist1 = dot_product(-PRcd%normal(:,PRcd%f2f_set(k)%G_i(1)),mid_normal)
           if (dist1 > max_dist) then
             max_set = k
             max_dist = dist1
           endif

         enddo

         k = max_set
         
         if (bavard) then
           write(cout,*) 'optimal face normal ',k,' is not aligned with cp normal '
           call logmes(cout, .true.)
           write(cout,*) 'maximal value of criteria ', max_dist , ' instead of threshold',1.d0 - f2f_tol
           call logmes(cout, .true.)
         endif

       endif

       cd_geo=k

     endif

     if ( js == 2 ) then

       ! edge ?
       ! recherche brutal force

       cd_status=2

       is_found=.FALSE.

       !fd on construit le edge qui lie les 2 node visibles
       i=0;j=0
       do k=1,size(cd_skip)
         if (cd_skip(k) <= 0) cycle
         if (i == 0) then
           i=k
           cycle
         endif
         j=k
         exit
       enddo

       !fd on verifie que ce edge existe
       do k=1,size(PRcd%f2f_edge,dim=2)
         if ((PRcd%f2f_edge(1,k) == i .and. PRcd%f2f_edge(2,k) == j) .or. &
             (PRcd%f2f_edge(1,k) == j .and. PRcd%f2f_edge(2,k) == i)) then

            if (bavard) then
              write(cout,*) 'cd edge',k,' is seen ...'
              write(cout,*) '... it contains vertex ',i,' and ',j
              call logmes(cout, .true.)
            endif

            is_found=.TRUE.
            exit
         endif
       enddo

       if (is_found) then

         cd_geo=k

       else

         ! cas moisi on degrade en noeud en gardant le plus favorable
         ! donne par le cundall_cp

         !write(6,*) 'cd ',PRcd%id, ' an ',PRan%id
         !write(6,*) 'dans cd on cherche arrete',i,j
         !write(6,*) 'on a'
         !do k=1,size(PRcd%f2f_edge,dim=2)
         !  write(6,*) k,PRcd%f2f_edge(1,k),PRcd%f2f_edge(2,k)
         !enddo

         !write(6,*) 'cd_skip :'
         !write(6,*) cd_skip

         !call logmes(IAM//' unable to find cd edge')

         ! on degrade
         js=1

       endif

     endif

     if (js == 1) then

       ! node !

       cd_status=1

       ! il va chercher jmin qui vaut 2
       cd_geo=maxloc(cd_skip,dim=1)

       if (bavard) then
         write(cout,*) 'cd vertex is seen ...'
         write(cout,*) '... it contains vertex ',cd_geo
         call logmes(cout, .true.)
       endif

     endif

     !write(6,*) 'an ******* '
     !write(6,*) 'sommets'
     !write(6,*) PRan%f2f_sommet
     !write(6,*) 'actifs'
     !write(6,*) an_skip
     !js=0
     !je=0
     !do i=1,size(an_skip)
     !  if (an_skip(i) <= 0) cycle
     !  if (count(PRan%f2f_sommet==i) /=0 ) then
     !    js=js+1
     !    do j=1,size(an_skip)
     !      if (an_skip(j)<= 0) cycle
     !      if (j==i) cycle
     !      if (count(PRan%f2f_sommet==j) == 0) cycle
     !      do k=1,size(PRan%f2f_edge,dim=2)
     !        if (PRan%f2f_edge(1,k) == i .and. &
     !            PRan%f2f_edge(2,k) == j ) then
     !          je=je+1
     !          write(6,*) i,j
     !          exit
     !        endif
     !       enddo
     !    enddo
     !  endif
     !enddo
     !write(6,*) 'an ',PRan%id,' nb sommets ',js,' nb edge ',je

     !fd recherche le type de contact antagoniste
     !fd an_status peut valoir 30 si face, 20 si ligne, 10 si point

     js = count(an_skip > 0)

     an_status=0
     an_geo=0

     if (js > 2) then

       ! face ?

       an_status=30

       is_found=.FALSE.
       do k=1,size(PRan%f2f_set)

         !fd test d'alignement des normales

         dist1 = dot_product(PRan%normal(:,PRan%f2f_set(k)%G_i(1)),mid_normal)

         if ( dist1 >= 1.d0 - f2f_tol) then

           if (bavard) then 
             je=0
             do i=1,size(an_skip)
               if (an_skip(i) <= 0) cycle
               if (count(PRan%f2f_contour(k)%G_i == i) /=0 ) je=je+1
             enddo

             write(cout,*) 'an face',k,' is seen ...'
             call logmes(cout, .true.)
             write(cout,*) '... it contains ',je,' nodes over ',js
             call logmes(cout, .true.)
           endif

           is_found=.TRUE.
           exit
         endif
       enddo

       if (.not. is_found) then

         ! on garde le moins pire
         max_dist = 0.d0
         max_set = 0
         do k=1,size(PRan%f2f_set)
           dist1 = dot_product(PRan%normal(:,PRan%f2f_set(k)%G_i(1)),mid_normal)
           if (dist1 > max_dist) then
             max_set = k
             max_dist = dist1
           endif

         enddo

         k = max_set

         if (bavard) then
           write(cout,*) 'optimal face normal ',k,' is not aligned with cp normal '
           call logmes(cout, .true.)
           write(cout,*) 'maximal value of criteria ', max_dist , ' instead of threshold',1.d0 - f2f_tol
           call logmes(cout, .true.)
         endif
         
       endif

       an_geo=k
       
     endif

     if ( js == 2 ) then

       ! edge ?

       an_status=20
       is_found=.FALSE.
       i=0;j=0
       do k=1,size(an_skip)
         if (an_skip(k) <= 0) cycle
         if (i == 0) then
           i=k
           cycle
         endif
         j=k
         exit
       enddo

       do k=1,size(PRan%f2f_edge,dim=2)
         if ((PRan%f2f_edge(1,k) == i .and. PRan%f2f_edge(2,k) == j) .or. &
             (PRan%f2f_edge(1,k) == j .and. PRan%f2f_edge(2,k) == i)) then

            if (bavard) then
              write(cout,*) 'an edge',k,' is seen ...'
              call logmes(cout, .true.)
              write(cout,*) '... it contains vertex ',i,' and ',j
              call logmes(cout, .true.)
            endif

            is_found=.TRUE.
            exit
         endif
       enddo

       if (is_found) then

         an_geo=k

       else

         ! on degrade
         js=1

       endif

     endif

     if (js == 1) then

       ! node

       an_status=10

       an_geo=maxloc(an_skip,dim=1)

       if (bavard) then
         write(cout,*) 'an vertex is seen ...'
         call logmes(cout, .true.)
         write(cout,*) '... it contains vertex ',an_geo
         call logmes(cout, .true.)
       endif

     endif

     if (bavard) then
       write(cout,*) 'ctd />'
       call logmes(cout, .true.)
     endif

     !write(6,*) 'POLYR ',PRcd%id,' voit POLYR ',PRan%id

     if (bavard) then
       write(cout,*) '< ccp'
       call logmes(cout, .true.)
       write(cout,*) 'computing contact point stage'
       call logmes(cout, .true.)
     endif

     ! calcul du gap et des normales
     !! attention toutes les fonctions appelees re-orientent normales et gap suivant mid_normal !!


     select case(cd_status+an_status)
     case(11)
       !cd node ; an node 

       nb_ctc = 1

       call nodetonode_distance(PRcd%vertex(:,cd_geo), &
                                PRan%vertex(:,an_geo)+perio_shift, &
                                mid_normal,pt_ctc(:,1),normal(:,1),gap)

       !fd on garde le gap ; la normale est celle du plan separateur
       
       overlap(1) = gap
       Nr = mid_normal
       Nsep=mid_normal
       area = 0.d0
       
       dist1 = dot_product(normal(:,1),mid_normal)
       
       if (bavard) then
         write(cout,*) 'vertex to vertex'
         call logmes(cout, .true.)
         write(cout,*) 'cd vertex ',cd_geo,' an vertex ',an_geo
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1), ' alert ', adist
         call logmes(cout, .true.)
         if (overlap(1) > adist) then
           write(cout,*) 'too far'
           call logmes(cout, .true.)
         end if
         write(cout,*) 'node-node normal ',normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'cp normal ',mid_normal
         call logmes(cout, .true.)
         if (dist1 < 1.d0 - tol_angle) then
           write(cout,*) 'node-node and cp normals not aligned'
           call logmes(cout, .true.)
           write(cout,*) dist1, ' < ', 1.d0 - tol_angle
           call logmes(cout, .true.)
        endif
       endif
       
       !fd 
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc = 0
         overlap=0.d0
         gap=0.d0
         normal = 0.d0
         pt_ctc = 0.d0
       endif   
       
     
     case(12)
       !cd edge ; an node 

       nb_ctc = 1


       !fd on inverse le sens de la direction de reference pour avoir le bon sens du gap
       !fd ca ne changera rien au signe du gap a la fin (meme si on garde la direction de reference)
       icheck = nodetoedge_distance(PRan%vertex(:,an_geo)+perio_shift, &
                                    PRcd%vertex(:,PRcd%f2f_edge(1,cd_geo)), &
                                    PRcd%vertex(:,PRcd%f2f_edge(2,cd_geo)), &
                                    -mid_normal,pt_ctc(:,1),normal(:,1),gap,err_)

       if (err_ > 0) then
         write(cout,'("PRan ",I0)') PRan%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while computing nodetoedge distance')
       endif   

       ! pas necessaire de changer le gap

       overlap(1) = gap
       Nr         = mid_normal
       Nsep       = mid_normal
       area       = 0.d0

       dist1 = dot_product(-normal(:,1),mid_normal)
       
       if (bavard) then
         write(cout,*) 'vertex to edge (permuted)'
         call logmes(cout, .true.)
         write(cout,*) 'an vertex ',an_geo,' cd edge ',cd_geo
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1), ' alert ', adist
         call logmes(cout, .true.)
         if (overlap(1) > adist) then
           write(cout,*) 'too far'
           call logmes(cout, .true.)
         end if
         write(cout,*) 'node-edge normal ',-normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'cp normal ',mid_normal
         call logmes(cout, .true.)
         if (dist1 < 1.d0 - tol_angle) then
           write(cout,*) 'node-edge and cp normals not aligned'
           call logmes(cout, .true.)
           write(cout,*) dist1, ' < ', 1.d0 - tol_angle
           call logmes(cout, .true.)
         endif  
       endif

       !fd
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc = 0
         overlap=0.d0
         gap=0.d0
         normal = 0.d0
         pt_ctc = 0.d0
       endif   

     case(13)
       !cd face ; an node
        
       allocate(sommets(3,size(PRcd%f2f_sommetofcontour(cd_geo)%G_i)))
       do i=1,size(PRcd%f2f_sommetofcontour(cd_geo)%G_i)
         sommets(:,i) = PRcd%vertex(:,PRcd%f2f_sommetofcontour(cd_geo)%G_i(i))
       end do

       ! la face devient an et le edge cd

       ! on passe la normale a la face cd comme direction de reference
       ! ca ne change rien sur le signe du gap lorsque'on changera le sens de la direction

       icheck = nodetoface_distance(PRan%vertex(:,an_geo)+perio_shift,          &
                                    sommets,                                    & 
                                    PRcd%normal(:,PRcd%f2f_set(cd_geo)%G_i(1)), &
                                    pt_ctc(:,1),normal(:,1),gap,err_)

       if (err_ > 0) then
         write(cout,'("PRan ",I0)') PRan%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while computing nodetoface distance')
       endif

       nb_ctc=1

       deallocate(sommets)

       ! pas necessaire de remettre dans la logique cd/an car fait par la fonction
       
       overlap(1) = gap
       Nr         = mid_normal
       Nsep       = mid_normal
       area       = 0.d0

       dist1 = dot_product(-normal(:,1),mid_normal)
       
       if (bavard) then
         write(cout,*) ' vertex to face (permuted)'
         call logmes(cout, .true.)
         write(cout,*) ' an vertex ',an_geo,' cd face ',cd_geo
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1), ' alert ', adist
         call logmes(cout, .true.)
         if (overlap(1) > adist) then
           write(cout,*) 'too far'
           call logmes(cout, .true.)
         end if
         write(cout,*) 'face normal ',-normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'cp normal ',mid_normal
         call logmes(cout, .true.)
         if ( dist1 < 1.d0 - tol_angle) then
           write(cout,*) 'face and cp normals not aligned'
           call logmes(cout, .true.)
           write(cout,*) dist1, ' < ', 1.d0 - tol_angle
           call logmes(cout, .true.)
         endif  
       endif
       
       !fd 
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc = 0
         overlap=0.d0
         gap=0.d0
         normal = 0.d0
         pt_ctc = 0.d0
       endif   
      
     case(21)
       !cd node ; an edge 
       nb_ctc = 1
        
       icheck = nodetoedge_distance(PRcd%vertex(:,cd_geo), &
                                    PRan%vertex(:,PRan%f2f_edge(1,an_geo))+perio_shift, &
                                    PRan%vertex(:,PRan%f2f_edge(2,an_geo))+perio_shift, &
                                    mid_normal,pt_ctc(:,1),normal(:,1),gap,err_)

       if (err_ > 0) then
          write(cout,'("PRan ",I0)') PRan%id
          call logmes(cout, .true.)
          call faterr(IAM,'unexpected problem while computing nodetoedge distance')
       endif   

       overlap(1) = gap
       Nr         = mid_normal
       Nsep       = mid_normal
       area       = 0.d0

       dist1 = dot_product(normal(:,1),mid_normal)
       
       if (bavard) then
         write(cout,*) 'vertex to edge'
         call logmes(cout, .true.)
         write(cout,*) 'cd vertex ',cd_geo,' an edge ',an_geo
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1), ' alert ', adist
         call logmes(cout, .true.)
         if (overlap(1) > adist) then
           write(cout,*) 'too far'
           call logmes(cout, .true.)
         end if
         write(cout,*) 'node-edge normal ',normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'cp normal ',mid_normal
         call logmes(cout, .true.)
         if ( dist1 < 1.d0 - tol_angle) then
           write(cout,*) 'node-edge and cp normals not aligned'
         call logmes(cout, .true.)
           write(cout,*) dist1, ' < ', 1.d0 - tol_angle
           call logmes(cout, .true.)
         endif  
       endif

       !fd 
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc  = 0
         overlap = 0.d0
         gap     = 0.d0
         normal  = 0.d0
         pt_ctc  = 0.d0
       endif   

       
     case(22)
       !cd edge ; an edge
        
       icheck = edgetoedge_distance(PRcd%vertex(:,PRcd%f2f_edge(1,cd_geo)), &
                                    PRcd%vertex(:,PRcd%f2f_edge(2,cd_geo)), &
                                    PRan%vertex(:,PRan%f2f_edge(1,an_geo))+perio_shift, &
                                    PRan%vertex(:,PRan%f2f_edge(2,an_geo))+perio_shift, &
                                    mid_normal,nb_ctc,pt_ctc(:,1:2),normal(:,1:2),overlap(1:2),err_)

       if (err_ > 0) then
         write(cout,'("PRan ",I0)') PRan%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while computing edgetoedge distance')
       endif
          
       Nr   = mid_normal
       Nsep = mid_normal
       area = 0.d0
       
       if (bavard) then
         write(cout,*) 'edge to edge'
         call logmes(cout, .true.)
         write(cout,*) 'cd edge ',cd_geo,' an edge ',an_geo
         call logmes(cout, .true.)
         write(cout,*) 'adist ', adist
         call logmes(cout, .true.)
         write(cout,*) 'nb_ctc ',nb_ctc
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1:nb_ctc)
         call logmes(cout, .true.)
         do i=1,nb_ctc 
           if (overlap(i) > adist) then
             write(cout,*) 'node ',i,' too far'
             call logmes(cout, .true.)
           end if
           write(cout,*) 'edge-edge normal ',normal(:,i)
           call logmes(cout, .true.)
           write(cout,*) 'cp normal ',mid_normal
           call logmes(cout, .true.)
           dist1 = dot_product(normal(:,i),mid_normal)           
           if (dist1 < 1.d0 - tol_angle) then
              write(cout,*) 'edge-edge and cp normals not aligned'
              call logmes(cout, .true.)
              write(cout,*) dist1, ' < ', 1.d0 - tol_angle
              call logmes(cout, .true.)
           endif   
         enddo 
       endif

       if (nb_ctc==2 ) then
         dist1 = dot_product(normal(:,2),mid_normal) 
         if ((overlap(2) > adist) .or. &
             (dist1 < 1.d0 - tol_angle)) then
           nb_ctc = nb_ctc - 1
           overlap(2:4)=0.d0
           normal(:,2:4) = 0.d0
           pt_ctc(:,2:4) = 0.d0
         endif           
       endif

       dist1 = dot_product(normal(:,1),mid_normal)       
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc = nb_ctc - 1          
         if (nb_ctc == 1) then
           overlap(1)    = overlap(2)
           normal(:,1)   = normal(:,2)
           pt_ctc(:,1)   = pt_ctc(:,2)
           overlap(2:4)  = 0.d0           
           normal(:,2:4) = 0.d0
           pt_ctc(:,2:4) = 0.d0
         else
           gap=0.d0
           overlap(1)   = 0.d0
           normal(:,1)  = 0.d0
           pt_ctc(:,1)  = 0.d0
         endif
       endif

       if (bavard) then
         write(cout,*) nb_ctc
         call logmes(cout, .true.)
         do i=1,2 
           write(cout,*) 'gap ', overlap(i)
           call logmes(cout, .true.)
           write(cout,*) 'n   ', normal(:,i)
           call logmes(cout, .true.)
           write(cout,*) 'xc  ', pt_ctc(:,i)
           call logmes(cout, .true.)
         enddo
       endif
       
    case(23)
       !cd face ; an edge 
        
       allocate(sommets(3,size(PRcd%f2f_sommetofcontour(cd_geo)%G_i)))
       do i=1,size(PRcd%f2f_sommetofcontour(cd_geo)%G_i)
         sommets(:,i) = PRcd%vertex(:,PRcd%f2f_sommetofcontour(cd_geo)%G_i(i))
       end do

       ! la face devient an et le edge cd

       !icheck = edgetoface_distance(PRan%vertex(:,PRan%f2f_edge(1,an_geo))+perio_shift, &
       !                             PRan%vertex(:,PRan%f2f_edge(2,an_geo))+perio_shift, &
       !                             sommets, &
       !                             mid_normal,nb_ctc,pt_ctc(:,1:2),normal(:,1:2),overlap(1:2),err_)


       !fd attention cette fonction tourne normales et distances
       
       icheck = edgetoface_distance_wp(PRan%vertex(:,PRan%f2f_edge(1,an_geo))+perio_shift, &
                                       PRan%vertex(:,PRan%f2f_edge(2,an_geo))+perio_shift, &
                                       sommets,                                            &
                                       PRcd%normal(:,PRcd%f2f_set(cd_geo)%G_i(1)),         &
                                       nb_ctc,pt_ctc(:,1:2),normal(:,1:2),overlap(1:2),err_,bavard)
        
       if (err_ > 0) then
         write(cout,'("PRan ",I0)') PRan%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while computing edgetoface distance')
       endif

       deallocate(sommets)

       Nr   = mid_normal
       Nsep = mid_normal
       area = 0.d0
       
       if (nb_ctc == 0) then
         overlap= 0.d0
         normal = 0.d0

         if (bavard) then
           write(cout,*) 'edge to face (permuted) - no contact'
           call logmes(cout, .true.)
         endif
          
       else if (nb_ctc == 1) then
          
         dist1 = dot_product(-normal(:,1),mid_normal)
         
         if (bavard) then
           write(cout,*) 'edge to face (permuted) - 1 point'
           call logmes(cout, .true.)
           write(cout,*) 'an edge ',an_geo,' cd face ',cd_geo
           call logmes(cout, .true.)
           write(cout,*) 'icheck ', icheck
           call logmes(cout, .true.)
           write(cout,*) 'gap ',overlap(1),' adist ',adist
           call logmes(cout, .true.)
           if (overlap(1) > adist) then
             write(cout,*) 'node ',1,' too far'
             call logmes(cout, .true.)
           end if
           write(cout,*) 'edge-face normal ',-normal(:,1)
           call logmes(cout, .true.)
           write(cout,*) 'cp normal ', mid_normal
           call logmes(cout, .true.)
           if ( dist1 < 1.d0 - tol_angle) then
              write(cout,*) 'edge-face and cp normals not aligned'
              call logmes(cout, .true.)
              write(cout,*) dist1, ' < ', 1.d0 - tol_angle
              call logmes(cout, .true.)
           endif
         endif
        
         if ((overlap(1) > adist) .or. &
             (dist1 < 1.d0 - tol_angle)) then
            nb_ctc  = 0   
            gap     = 0.d0
            overlap = 0.d0
            normal  = 0.d0
            pt_ctc  = 0.d0
         endif
        
       else if (nb_ctc == 2) then
          
         ! pas necessaire de remettre dans la logique cd/an car fait par la fonction 
         ! overlap(1) = -overlap(1)
         ! overlap(2) = -overlap(2)

         if (bavard) then
           write(cout,*) 'edge to face (permuted) - 2 points'
           call logmes(cout, .true.)
           write(cout,*) 'an edge ',an_geo,' cd face ',cd_geo
           call logmes(cout, .true.)
           write(cout,*) 'icheck ', icheck
           call logmes(cout, .true.)
           write(cout,*) 'adist ', adist
           call logmes(cout, .true.)
           write(cout,*) 'nb_ctc ',nb_ctc
           call logmes(cout, .true.)
           write(cout,*) 'gap ',overlap(1:nb_ctc)
           call logmes(cout, .true.)
           do i=1,nb_ctc 
             if (overlap(i) > adist) then
               write(cout,*) 'node ',i,' too far'
               call logmes(cout, .true.)
             end if
             write(cout,*) 'edge-face normal ',-normal(:,i)
             call logmes(cout, .true.)
             write(cout,*) 'cp normal ',mid_normal
             call logmes(cout, .true.)
             dist1 = dot_product(-normal(:,i),mid_normal)           
             if (dist1 < 1.d0 - tol_angle) then
               write(cout,*) 'edge-face and cp normals not aligned'
               call logmes(cout, .true.)
               write(cout,*) dist1, ' < ', 1.d0 - tol_angle
               call logmes(cout, .true.)
             endif
           enddo 
         endif

         dist1 = dot_product(-normal(:,2),mid_normal) 
         if ((overlap(2) > adist) .or. &
             (dist1 < 1.d0 - tol_angle)) then
           nb_ctc = nb_ctc - 1
           overlap(2:4)=0.d0
           normal(:,2:4) = 0.d0
           pt_ctc(:,2:4) = 0.d0
         endif

         dist1 = dot_product(-normal(:,1),mid_normal)       
         if ((overlap(1) > adist) .or. &
            (dist1 < 1.d0 - tol_angle)) then
           nb_ctc = nb_ctc - 1          
           if (nb_ctc == 1) then
             overlap(1) = overlap(2)
             normal(:,1) = normal(:,2)
             pt_ctc(:,1) = pt_ctc(:,2)
             overlap(2:4)=0.d0           
             normal(:,2:4) = 0.d0
             pt_ctc(:,2:4) = 0.d0
           else
             gap=0.d0
             overlap(1)=0.d0
             normal(:,1) = 0.d0
             pt_ctc(:,1) = 0.d0
           endif
         endif
       endif
      
     case(31)
       !cd node ; an face
        
       allocate(sommets(3,size(PRan%f2f_sommetofcontour(an_geo)%G_i)))
       do i=1,size(PRan%f2f_sommetofcontour(an_geo)%G_i)
         sommets(:,i) = PRan%vertex(:,PRan%f2f_sommetofcontour(an_geo)%G_i(i))+perio_shift
       end do

       ! on passe la normale a la face pour eviter de la recalculer
       ! a corriger plus tard ...

       normal(:,1) = PRan%normal(:,PRan%f2f_set(an_geo)%G_i(1))

       icheck = nodetoface_distance(PRcd%vertex(:,cd_geo), &
                                    sommets, &
                                    mid_normal,pt_ctc(:,1),normal(:,1),overlap(1),err_)

       if (err_ > 0) then
         write(cout,'("PRcd ",I0)') PRcd%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while computing nodetoface distance')
       endif
       
       nb_ctc=1
        
       deallocate(sommets)

       Nr = mid_normal
       Nsep=mid_normal
       area = 0.d0

       dist1 = dot_product(normal(:,1),mid_normal)
       
       if (bavard) then
         write(cout,*) 'vertex to face'
         call logmes(cout, .true.)
         write(cout,*) 'cd vertex ',cd_geo,' an face ',an_geo
         call logmes(cout, .true.)
         write(cout,*) 'gap ',overlap(1:nb_ctc), ' alert ', adist
         call logmes(cout, .true.)
         if (overlap(1) > adist) then
           write(cout,*) 'too far'
           call logmes(cout, .true.)
         end if
         write(cout,*) 'vertex-face normal ',normal(:,1)
         call logmes(cout, .true.)
         write(cout,*) 'cp normal ',mid_normal
         call logmes(cout, .true.)
         if ( dist1 < 1.d0 - tol_angle) then
           write(cout,*) 'vertex-face and cp normals not aligned'
           call logmes(cout, .true.)
           write(cout,*) dist1, ' < ', 1.d0 - tol_angle
           call logmes(cout, .true.)
         endif  
       endif

       !fd 
       if ((overlap(1) > adist) .or. &
           (dist1 < 1.d0 - tol_angle)) then
         nb_ctc = 0
         overlap=0.d0
         gap=0.d0
         normal = 0.d0
         pt_ctc = 0.d0
       endif   


     case(32)
       !cd edge ; an face
        
       allocate(sommets(3,size(PRan%f2f_sommetofcontour(an_geo)%G_i)))
       do i=1,size(PRan%f2f_sommetofcontour(an_geo)%G_i)
         sommets(:,i) = PRan%vertex(:,PRan%f2f_sommetofcontour(an_geo)%G_i(i))+perio_shift
       end do

       ! on passe la normale a la face pour eviter de la recalculer
       ! a corriger plus tard ...

       !normal(:,1) = PRan%normal(:,PRan%f2f_set(an_geo)%G_i(1))

       !icheck = edgetoface_distance(PRcd%vertex(:,PRcd%f2f_edge(1,cd_geo)), &
       !                             PRcd%vertex(:,PRcd%f2f_edge(2,cd_geo)), &
       !                             sommets, &
       !                             mid_normal,nb_ctc,pt_ctc(:,1:2),normal(:,1:2),overlap(1:2),err_)
       
       icheck = edgetoface_distance_wp(PRcd%vertex(:,PRcd%f2f_edge(1,cd_geo)),     &
                                       PRcd%vertex(:,PRcd%f2f_edge(2,cd_geo)),     &
                                       sommets,                                    &
                                       PRan%normal(:,PRan%f2f_set(an_geo)%G_i(1)), &
                                       nb_ctc,pt_ctc(:,1:2),normal(:,1:2),overlap(1:2),err_,bavard)

       if (err_ > 0) then
          write(cout,'("PRcd ",I0)') PRcd%id
          call logmes(cout, .true.)
          call faterr(IAM,'unexpected problem while computing edgetoface distance')
       endif   

       deallocate(sommets)

       Nr = mid_normal
       Nsep=mid_normal
       area = 0.d0

       if (nb_ctc == 0) then
          overlap=0.d0
          normal=0.d0

           if (bavard) then
              write(cout,*) 'edge to face  - no contact'
              call logmes(cout, .true.)
           endif
          
       else if (nb_ctc == 1) then

         dist1 = dot_product(normal(:,1),mid_normal)
          
         if (bavard) then
           write(cout,*) 'edge to face - 1 point'
           call logmes(cout, .true.)
           write(cout,*) ' cd edge ',cd_geo,' an face ',an_geo
           call logmes(cout, .true.)
           write(cout,*) 'icheck ', icheck
           call logmes(cout, .true.)
           write(cout,*) 'gap ',overlap(1),' adist ',adist
           call logmes(cout, .true.)
           if (overlap(1) > adist) then
             write(cout,*) 'node ',1,' too far'
             call logmes(cout, .true.)
           end if
           write(cout,*) 'edge-face normal ',normal(:,1)
           call logmes(cout, .true.)
           write(cout,*) 'cp normal ', mid_normal
           call logmes(cout, .true.)
           if ( dist1 < 1.d0 - tol_angle) then
             write(cout,*) 'edge-face and cp normals not aligned'
             call logmes(cout, .true.)
             write(cout,*) dist1, ' < ', 1.d0 - tol_angle
             call logmes(cout, .true.)
           endif  
         endif

         if ((overlap(1) > adist) .or. &
             (dist1 < 1.d0 - tol_angle)) then
           nb_ctc  = 0   
           gap     = 0.d0
           overlap = 0.d0
           normal  = 0.d0
           pt_ctc  = 0.d0
         endif

      else if (nb_ctc == 2) then
          
         if (bavard) then
           write(cout,*) 'edge to face - 2 points'
           call logmes(cout, .true.)
           write(cout,*) 'cd edge ',cd_geo,' an face ',an_geo
           call logmes(cout, .true.)
           write(cout,*) 'icheck ', icheck
           call logmes(cout, .true.)
           write(cout,*) 'adist ', adist
           call logmes(cout, .true.)
           write(cout,*) 'nb_ctc ',nb_ctc
           call logmes(cout, .true.)
           write(cout,*) 'gap ',overlap(1:nb_ctc)
           call logmes(cout, .true.)
           do i=1,nb_ctc 
             if (overlap(i) > adist) then
               write(cout,*) 'node ',i,' too far'
               call logmes(cout, .true.)
             end if
             write(cout,*) 'edge-face normal ',normal(:,i)
             call logmes(cout, .true.)
             write(cout,*) 'cp normal ',mid_normal
             call logmes(cout, .true.)
             dist1 = dot_product(normal(:,i),mid_normal)           
             if (dist1 < 1.d0 - tol_angle) then
               write(cout,*) 'edge-face and cp normals not aligned'
               call logmes(cout, .true.)
               write(cout,*) dist1, ' < ', 1.d0 - tol_angle
               call logmes(cout, .true.)
             endif  
           enddo 
         endif

         dist1 = dot_product(normal(:,2),mid_normal) 
         if ((overlap(2) > adist) .or. &
             (dist1 < 1.d0 - tol_angle)) then
           nb_ctc = nb_ctc - 1
           overlap(2:4)=0.d0
           normal(:,2:4) = 0.d0
           pt_ctc(:,2:4) = 0.d0
         endif

         dist1 = dot_product(normal(:,1),mid_normal)       
         if ((overlap(1) > adist) .or. &
            (dist1 < 1.d0 - tol_angle)) then
           nb_ctc = nb_ctc - 1          
           if (nb_ctc == 1) then
             overlap(1)=overlap(2)
             normal(:,1) = normal(:,2)
             pt_ctc(:,1) = pt_ctc(:,2)
             overlap(2:4)=0.d0           
             normal(:,2:4) = 0.d0
             pt_ctc(:,2:4) = 0.d0
           else
             gap=0.d0
             overlap(1)=0.d0
             normal(:,1) = 0.d0
             pt_ctc(:,1) = 0.d0
           endif
         endif
       endif
       
     case(33)
       !cd face ; an face

       call planplan(PRcd,PRan,perio_shift, &
                     mid_P,t,mid_normal,s, &
                     jmin,imax, &
                     cd_skip,an_skip, &
                     .false., &
                     nb_ctc,pt_ctc, &
                     area, &
                     .false.)

       !fd bidouille pour se debarrasser des surfaces trop petites qu'on
       !   peut estimer "parasites"
       if (f2f_skip_small_surface .and. area < f2f_tol_small_surface) then
         if ( bavard ) then
           write(cout,*) 'surface de contact trop petite'
           call logmes(cout, .true.)
           write(cout,*) area,' < ',f2f_tol_small_surface
           call logmes(cout, .true.)
         endif
         nb_ctc = 0
       endif

       overlap(1:nb_ctc) = gap
 
       Nr = mid_normal
       Nsep=mid_normal

       if (bavard) then
          write(cout,*) 'face to face'
          call logmes(cout, .true.)
          write(cout,*) 'cd face ',cd_geo,' an face ',an_geo
          call logmes(cout, .true.)
          write(cout,*) 'nb pt ctc ',nb_ctc
          call logmes(cout, .true.)
          if (nb_ctc > 0) write(6,*) 'gap ',overlap(1:nb_ctc)
       endif

     case default
       call faterr(IAM,'unsupported combination')
     end select

     if (gapP < adist .and. nb_ctc == 0) then
       ! recherche de la derniere chance on augmente tol_proj 
       ! on ne garde que les noeuds dans la bande [-tol_proj,+tol_proj] autour du CP

       tol_proj=10.d0*cundall_neighbor

       !on calcule une dimension de reference
       vec(:) = (PRan%vertex(:,imax)+perio_shift(:))-PRan%center(:)
       norm1 = dot_product(mid_normal,vec)
  
       vec(:) = PRcd%vertex(:,jmin)-PRcd%center(:)
       norm2 = dot_product(mid_normal,vec)
  
       ref_size=MIN(dabs(norm1),dabs(norm2))

       d1= MAX(0.d0,gapP*0.5d0 ) + tol_proj*ref_size

       ! recherche de ceux dans la bande neighbor 
       d_min=1.d20

       DO j=1,PRcd%nb_vertex 
         if (cd_skip(j) == 0) cycle
         if (j == jmin) cycle 
     
         vec(:)=(PRcd%vertex(:,j)) - mid_P(:)
         d2=DOT_PRODUCT(vec(:),mid_normal(:))

         IF (d2 > d1) cd_skip(j) = 0
         if (cd_skip(j) < 0) cd_skip(j) = -cd_skip(j)    
       enddo

      ! recherche de ceux dans la bande neighbor 
      d1 = -d1
      d_max=-1.d20
      
      DO i=1,PRan%nb_vertex 
        if (an_skip(i) == 0) cycle 
        if (i == imax) cycle
    
        vec(:)=(PRan%vertex(:,i)+perio_shift(:)) - mid_P(:)
        d2=DOT_PRODUCT(vec(:),mid_normal(:))

        IF (d2 < d1) an_skip(i) = 0
        if (an_skip(i) < 0) an_skip(i) = -an_skip(i)    
      enddo
        
      call planplan(PRcd,PRan,perio_shift, &
                    mid_P,t,mid_normal,s, &
                    jmin,imax, &
                    cd_skip,an_skip, &
                    .false., &
                    nb_ctc,pt_ctc, &
                    area, &
                    .false. )

       !fd bidouille pour se debarrasser des surfaces trop petites qu'on
       !   peut estimer "parasites"
       if (f2f_skip_small_surface .and. area < f2f_tol_small_surface) then
         if ( bavard ) then
           write(cout,*) 'surface de contact trop petite'
           call logmes(cout, .true.)
           write(cout,*) area,' < ',f2f_tol_small_surface
           call logmes(cout, .true.)
         endif
         nb_ctc = 0
       endif

       overlap(1:nb_ctc) = gapP
 
       Nr = mid_normal
       Nsep=mid_normal

       if (bavard) then
          write(cout,*) 'face to face'
          call logmes(cout, .true.)
          write(cout,*) 'cd face ',cd_geo,' an face ',an_geo
          call logmes(cout, .true.)
          write(cout,*) 'nb pt ctc ',nb_ctc
          call logmes(cout, .true.)
          if (nb_ctc > 0) then
            write(cout,*) 'gap ',overlap(1:nb_ctc)
            call logmes(cout, .true.)
          end if
       endif

     endif   

     if (bavard) then
       write(cout,*) 'ccp />'
       call logmes(cout, .true.)
     endif


     !write(6,*) ' ******* ' 

   else

     !fd old method

     if (bavard) then

       write(cout,*) 'iterations cundall ',nb_iter
       call logmes(cout, .true.)
       write(cout,*) 'point ', mid_P
       call logmes(cout, .true.)
       write(cout,*) 'n ', mid_normal
       call logmes(cout, .true.)
       write(cout,*) 'gap ', gap
       call logmes(cout, .true.)
       write(cout,*) 'cd skip ',cd_skip
       call logmes(cout, .true.)
       write(cout,*) 'an skip ',an_skip
       call logmes(cout, .true.)

     endif

     call planplan(PRcd,PRan,perio_shift, &
                   mid_P,t,mid_normal,s, &
                   jmin,imax, &
                   cd_skip,an_skip, &
                   .false., &
                   nb_ctc,pt_ctc, &
                   area, &
                   bavard)

     if (bavard) then
       write(cout,*) 'nb pt ctc ',nb_ctc
       call logmes(cout, .true.)
     endif


     !fd bidouille pour se debarrasser des surfaces trop petites qu'on
     !   peut estimer "parasites"
     if (f2f_skip_small_surface .and. area < f2f_tol_small_surface) then
       if ( bavard ) then
         write(cout,*) 'surface de contact trop petite'
         call logmes(cout, .true.)
         write(cout,*) area,' < ',f2f_tol_small_surface
         call logmes(cout, .true.)
       endif
       nb_ctc = 0
     endif

     ! ici connaissant la normale, la position des ptc il faut recalculer le gap !


     !call proj_f2f(PRcd,PRan,perio_shift,&
     !              fi,fj, &
     !              Nr, &
     !              nb_ctc,pt_ctc, &
     !              overlap, &
     !              id_face_cd,weight_face_cd, &
     !              id_face_an,weight_face_an, &
     !              bavard)



     overlap(1:nb_ctc) = gap

     Nr = mid_normal
     Nsep=mid_normal

   endif

   deallocate(cd_skip,an_skip)
   deallocate(cd_face,an_face)

   

  END SUBROUTINE DETECTION_COMMON_PLANE
  !-----------------------------------------------------------------------
  !-----------------------------------------------------------------------
  SUBROUTINE set_cundall_iteration_PRPRx(iter)

    IMPLICIT NONE
    INTEGER :: iter
    character(len=10):: ctemp

    cundall_iter = iter

    if (dbg) then
       write(ctemp,'(I0)') iter
       call logmes('nb cundall iter:'//ctemp)
    endif

  END SUBROUTINE set_cundall_iteration_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_cundall_neighbor_PRPRx(neighbor)

    IMPLICIT NONE
    real(kind=8) :: neighbor
    character(len=14):: ctemp

    cundall_neighbor = neighbor
   
    if (dbg) then
       write(ctemp,'(D14.5)') neighbor
       call logmes('nb cundall iter:'//ctemp)
    endif

  END SUBROUTINE set_cundall_neighbor_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE use_old_ccpm_PRPRx()

    IMPLICIT NONE

    new_ccpm = .FALSE.

  END SUBROUTINE use_old_ccpm_PRPRx
  !-----------------------------------------------------------------------

  ! fin CP Cundall  

  ! debut CP F2F

  !-----------------------------------------------------------------------  
  SUBROUTINE DETECTION_F2F_encoreplusnew(id1,id2,Nsep,adist,nb_ctc,PT_CTC,overlap,Nr,t,s,v2v)

  !fd le 14/10/2010
  ! detection face a face (plan, courbe) pour objets convexes et non convexes
  !

  ! I
  ! id1 : id solide 1 (antagoniste)
  ! id2 : id solide 2 (candidat)
  ! Nsep: est la derniere normale au plan separateur connue 
  !       l'intercentre si on fait une recherche rough a chaque pas
  !
  ! O 
  ! nb_ctct           : nombre de points de contact
  ! PT_CTC(1:nb_ctc)  : coordonnees des points de contact
  ! Nr(1)             : normales aux points de contact si il y a contact (bizarre bizarrre)
  ! r_cd,r_an         : qui est le vrai candidat et le vrai antagoniste ... comme en entre
  ! overlap(1:nb_ctc) : les gaps aux points de contact
  ! Nsep : normale au plan separateur actualisee ou ancienne


   IMPLICIT NONE

   INTEGER                          :: id1,id2,nb_ctc
   REAL(kind=8)                     :: Nsep(3),adist,Nr(3),t(3),s(3)
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
   REAL(kind=8),DIMENSION(4)        :: overlap
   type(T_visavis) :: v2v
   !

   INTEGER,PARAMETER                :: nb_ptc_max=100
   INTEGER                          :: i,k,j,m,inc,inc1
   TYPE(T_POLYR)                    :: PRan,PRcd

   REAL(kind=8)                     :: dist1,dist2,scal,norm,norm1,norm2
   REAL(kind=8),DIMENSION(3)        :: s1,r1,Nsep_o
   REAL(kind=8)                     :: gap
   INTEGER                          :: nb_ptc
  
   INTEGER,DIMENSION(2*nb_ptc_max)  :: is_ok


   INTEGER                          :: errare,nb_select
   REAL(kind=8),DIMENSION(3)        :: Ncd
 
   REAL(kind=8)                     :: norm_max,norm_min

   LOGICAL                          :: bavard=.false. !.false.

   INTEGER                          :: nb_vertex_pran_min, nb_vertex_prcd_min

   REAL(kind=8),DIMENSION(3)        :: mid_P,p1,p2
   REAL(kind=8),DIMENSION(2)        :: s1moy,r1moy

   REAL(kind=8),DIMENSION(:,:), ALLOCATABLE   :: Pcd,Pan,rr1,ss1   
   integer,dimension(:),allocatable :: id_Pan,id_Pcd

   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_skip,an_skip
   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: cd_face,an_face
   INTEGER     ,DIMENSION(:),   ALLOCATABLE   :: aux_cd_skip,aux_an_skip

   INTEGER                        :: jmin,imax

   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max

   REAL(kind=8),DIMENSION(3)      :: n_ini,vec

   REAL(kind=8),DIMENSION(2)      :: lvec,PT

!fd 

   REAL(kind=8)                   :: tol = 1.d-10,rd
   integer                        :: dump_fich

                               !1234567890123456789012345678901234
   CHARACTER(len=34)  :: IAM = 'PRPRx::DETECTION_F2F_encoreplusnew'


   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
   REAL(kind=8),dimension(:),allocatable :: dist

   character(len=11) :: nom

   logical :: is_reverse=.FALSE.

   integer :: ii
   real(kind=8) :: ref_size,dir(2),c_center(2),c_vec(2)

   logical :: is_inside
  ! f2f
   integer :: fi,fj,iff,my_cd_iff,my_an_iff,ic
   logical :: f2f_found

   real(kind=8) :: pp(3,3),shift_ptc(3)
   real(kind=8),pointer :: points(:,:)

   integer :: ix

   !fd par defaut on ne fait pas le so avec les normales des faces
   !fd TODO mettre ca en parametre

   logical :: skip_so = .true.


   real(kind=8) :: tmp,vec1(3),vec2(3),vec3(3)
   integer :: nb_ptc_qh

   real(kind=8),allocatable :: ptc(:,:),angle(:)
   integer,allocatable :: id_ptc(:)
    
   integer,dimension(4)             :: id_face_cd,id_face_an
   REAL(kind=8),DIMENSION(3,4)      :: weight_face_cd, weight_face_an

   real(kind=8) :: area

   integer :: icheck

   !fd pas de surprise si j'initialise (dicton du jour le plus con) 

   nb_ctc=0

   Nr = 0.d0
   t  = 0.d0
   s  = 0.d0

   Nsep_o = Nsep 

   PRan    = S_POLYR(id1)
   PRcd    = S_POLYR(id2)

   ALLOCATE(cd_skip(PRcd%nb_vertex),an_skip(PRan%nb_vertex))
   cd_skip = 0
   an_skip = 0

   ALLOCATE(cd_face(PRcd%nb_faces),an_face(PRan%nb_faces))
   cd_face = 0
   an_face = 0

   icheck=compute_visibility(PRcd,PRan,perio_shift, &
                             Nsep, &
                             cd_skip,an_skip,cd_face,an_face,.False.)

   if (icheck /= 1) call FATERR(IAM,'Strange result of compute_visibility')

   !fd a ce niveau on a 2 listes de faces qui vont bien 

   if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 
     print*,'nb cd set',size(PRcd%f2f_set)
     do fi=1,size(PRcd%f2f_set)
       print*,'set d elements',fi,PRcd%f2f_set(fi)%G_i(:)
     enddo
     print*,'nb an set',size(PRan%f2f_set)
     do fj=1,size(PRan%f2f_set)
       print*,'set d elements',fj,PRan%f2f_set(fj)%G_i(:)
     enddo
   endif
!

   allocate(aux_cd_skip(PRcd%nb_vertex),aux_an_skip(PRan%nb_vertex))

   !rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@


   !fd f2f donne mid_P et n_ini
   f2f_found = .FALSE.
   do fi=1,size(PRcd%f2f_set)
     do fj=1,size(PRan%f2f_set)

        !fd faudra ajouter un test sur plan/plan courbe/courbe etc


        !fd test d'alignement des normales

        dist1 = dot_product(PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)),PRan%normal(:,PRan%f2f_set(fj)%G_i(1)))

        if ( dist1 < -1.d0 + f2f_tol) then

          if (bavard .or. &
              (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 

            print*,'cas possible entre cd set ',fi,' an set ',fj
            print*,'1er face du set cd',PRcd%f2f_set(fi)%G_i(1)
            print*,'cd normale ',PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1))
            print*,'1er face du set an',PRan%f2f_set(fj)%G_i(1)
            print*,'an normale ',PRan%normal(:,PRan%f2f_set(fj)%G_i(1))

            vec1(:) = PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(1))) - PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(1)))
            vec2(:) = PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(1))) - PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(1)))
            vec3(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
            vec3(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
            vec3(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)
            print*,'cd normale ', vec3/sqrt(dot_product(vec3,vec3)) 
                                  

            vec1(:) = PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(1))) - PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(1)))
            vec2(:) = PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(1))) - PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(1)))
            vec3(1) = vec1(2)*vec2(3) - vec1(3)*vec2(2)
            vec3(2) = vec1(3)*vec2(1) - vec1(1)*vec2(3)
            vec3(3) = vec1(1)*vec2(2) - vec1(2)*vec2(1)
            print*,'an normale ', vec3/sqrt(dot_product(vec3,vec3)) 
                      
            print*,'test produit scalaire ', 1.d0 + dist1
            print*,'tol', f2f_tol
          endif 

          aux_cd_skip = cd_skip
          aux_an_skip = an_skip

          icheck = compute_f2f_common_plane(PRcd,PRan,perio_shift,adist, &
                                            mid_P,t,Nr,s, &
                                            jmin,imax,fi,fj, &
                                            aux_cd_skip,aux_an_skip, &
                                            bavard)

          if (icheck == 0 ) cycle

          !print*,aux_cd_skip
          !print*,aux_an_skip
          !print*,mid_P
          !print*,mid_normal
          !print*,jmin,imax,fi,fj

          call planplan(PRcd,PRan,perio_shift, &
                        mid_P,t,Nr,s, &
                        jmin,imax, &
                        aux_cd_skip,aux_an_skip, &
                        .true., &
                        nb_ctc,pt_ctc, &
                        area, &
                        bavard, &
                        v2v%face_ctc, v2v%face_sizes)

          if (bavard .or. &
              (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) print*,'nb points de contact',nb_ctc


          call proj_f2f(PRcd,PRan,perio_shift,&
                        fi,fj, &
                        Nr, &
                        nb_ctc,pt_ctc, &
                        overlap, &
                        id_face_cd,weight_face_cd, &
                        id_face_an,weight_face_an, &
                        bavard)


          !fd bidouille pour se debarrasser des surfaces trop petites qu'on
          !   peut estimer "parasites"
          if (f2f_skip_small_surface .and. area < f2f_tol_small_surface) then
            if (bavard .or. &
                (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
              print*,'surface de contact trop petite'
              print*,area,' < ',f2f_tol_small_surface
            endif
            nb_ctc = 0

          endif

          if (nb_ctc /= 0) then

            !print*,'on pousse dans visavis'
            !print*,'cd/an',PRcd%id,PRan%id
            !print*,'nb_ctc',nb_ctc

            v2v%cd=PRcd%Id
            v2v%an=PRan%Id
            v2v%id_f_cd=fi
            v2v%id_f_an=fj
            v2v%nb_ctc=nb_ctc
            v2v%iff_cd=id_face_cd
            v2v%cd_lcoor=weight_face_cd
            v2v%iff_an=id_face_an
            v2v%an_lcoor=weight_face_an       
            v2v%is_flat=.true.
            v2v%pt_area=area/nb_ctc
            v2v%normal=Nr
                       
            exit ! on sort de la boucle   
          endif

          !fd ajout dans une liste chainee ...


        endif
      enddo

      if (nb_ctc /= 0) exit ! apres faudra juste faire un cycle

    enddo

    DEALLOCATE(aux_cd_skip,aux_an_skip,cd_skip,an_skip)

  end subroutine
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  SUBROUTINE update_F2F(v2v,nb_ctc,xco,ovlap,n,t,s)
   implicit none
   integer :: nb_ctc
   real(kind=8) :: xco(3,4),ovlap(4),n(3),t(3),s(3)
   type(T_visavis) :: v2v
   ! local
   integer :: i,ic
   real(kind=8) :: pcd(3),pan(3)
   TYPE(T_POLYR)                    :: PRan,PRcd

   ! print*,'recuperation de visavis'
   ! print*,'cd/an',v2v%cd,v2v%an
   ! print*,'nb_ctc',v2v%nb_ctc

   PRan    = S_POLYR(v2v%an)
   PRcd    = S_POLYR(v2v%cd)

   nb_ctc = v2v%nb_ctc
   n(:)= 0.5 * (PRan%normal(:,v2v%iff_an(1)) - PRcd%normal(:,v2v%iff_cd(1))) 

   !print*,'n',n

   call comp_rep(t,n,s)

   do ic=1,nb_ctc

     !print*,'contact ',ic
     !print*,'faces ',v2v%iff_cd(ic),v2v%iff_an(ic)
     !print*,'noeud cd ', PRcd%face(:,v2v%iff_cd(ic))
     !print*,'noeud an ', PRan%face(:,v2v%iff_an(ic))

     pcd=0.d0
     pan=0.d0
     do i=1,3     
       pcd(:) = pcd(:) + v2v%cd_lcoor(i,ic)*PRcd%vertex(:,PRcd%face(i,v2v%iff_cd(ic)))      
       pan(:) = pan(:) + v2v%an_lcoor(i,ic)*PRan%vertex(:,PRan%face(i,v2v%iff_an(ic)))      
     enddo
     xco(:,ic) = 0.5 * (pcd(:) + pan(:)) 
     ovlap(ic) = dot_product(n,pcd - pan)     
   enddo
   
  end subroutine update_f2f
  !------------------------------------------------------------------------


  !-----------------------------------------------------------------------
  SUBROUTINE set_f2f_tol_PRPRx(tol)

    IMPLICIT NONE
    REAL(kind=8) :: tol 

    f2f_tol = tol
    with_f2f = .TRUE.
   
  END SUBROUTINE set_f2f_tol_PRPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_f2f_tol_small_surface_PRPRx(tol)

    IMPLICIT NONE
    REAL(kind=8) :: tol 

    f2f_tol_small_surface = tol
    f2f_skip_small_surface = .TRUE.
   
  END SUBROUTINE set_f2f_tol_small_surface_PRPRx
  !-----------------------------------------------------------------------

  !------------------------------------------------------------------------
  subroutine verbose_f2f_PRPRx(cd,an)
   implicit none
   integer :: cd,an
   dbg = .TRUE.
   dbg_idcd = cd
   dbg_idan = an   
  end subroutine
  !-----------------------------------------------------------------------

  ! fin CP F2F

  ! debut NC
  
  !------------------------------------------------------------------------
  ! non convex contact detection strategy
  !
  ! TODO : handle groups in order to sue this fine detetction method for DDM applications
  SUBROUTINE nc_compute_contact_PRPRx(gdist)
 
   IMPLICIT NONE  

   REAL(kind=8)                        :: gdist ! distance alerte globale
   !***
   INTEGER                             :: errare 
   INTEGER                             :: icdan,iadj,itac,i,j
   INTEGER                             :: icdtac,iantac,isee   
   REAL(kind=8)                        :: raycd,rayan,adist,dist
   INTEGER                             :: nb_ctc
   INTEGER                             :: size_of_array_this
   integer,dimension(:),pointer        :: status
   REAL(kind=8),DIMENSION(:,:),pointer :: xco,t,n,s 
   REAL(kind=8),DIMENSION(:),pointer   :: ovlap,surf
   REAL(kind=8),DIMENSION(3)           :: sep,cdlev,anlev
   REAL(kind=8)                        :: norme
   REAL(kind=8),DIMENSION(6)           :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3,3)         :: Rc,localframe_cd,localframe_an
   REAL(kind=8)                        :: t1,t2,vls_cst,vln_cst,vlt_cst

   integer :: cd_ent,an_ent

! f2f 
!!   logical :: is_first_time_f2f = .true.
!   integer :: i_visavis

   integer :: nbf
                            !1234567890123456789012345
   character(len=25) :: IAM='PRPRx::nc_compute_contact' 
   character(len=80) :: mes
   character(len=450):: lcout
 
   icdan   = 0        
   nb_PRPRx= 0
   nb_adj  = 0
   nb_detection_test=0
   detection_time=0.D0
   nb_shadow=0
   nb_ctc_state=0

   IF (nb_rough_PRPRx /= 0 ) THEN

     nullify(status,xco,ovlap,t,n,s,surf)

     size_of_array_this=SIZE(this)
  
 ! a reprendre
 !    if (is_first_time_f2f) then
 !      ALLOCATE(visavis(size_of_array_this))
 !      do i=1,size_of_array_this
 !        nullify(visavis(i)%iff_cd,visavis(i)%iff_an, &
 !                visavis(i)%cd_lcoor,visavis(i)%an_lcoor, &
 !                visavis(i)%index)
 !        visavis(i)%nb_ctc=0
 !        visavis(i)%nb_ctc_rough=0
 !      enddo
 !    endif
 !    nb_visavis = 0
 !    i_visavis=0

   !
   ! preparation de la detection 
   !
     DO i=1,nb_rough_PRPRx 

       icdtac    = rough_PRPRx(i)%cd
       iantac    = rough_PRPRx(i)%an 
       isee      = rough_PRPRx(i)%isee

       adist=see(isee)%alert 
       dist=S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist

       perio_shift = 0.d0
       perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
       perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode

       sep=S_POLYR(icdtac)%center - (S_POLYR(iantac)%center + perio_shift)
       norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)

       !fd il faut eviter ca car avec des corps non convexes emboites ca ne marche pas

       !IF (norme <1.D-24) THEN
       !  write(mes,'(A,I0,A,I0,A,D12.5)') 'Distance between ',icdtac,' and ',iantac,' is ',norme 
       !  call logmes(mes)  
       !  write(mes,'(A,I0,3(1x,D12.5))') 'cdtac ',icdtac,S_POLYR(icdtac)%center
       !  call logmes(mes)  
       !  write(mes,'(A,I0,3(1x,D12.5))') 'antac ',iantac,S_POLYR(iantac)%center
       !  call logmes(mes)  
       !  call faterr(IAM,'Center of inertia at the same place')
       !ENDIF   

!fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
!fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
!fd @@@ et il en manque je pense ...

       IF (norme < dist*dist) THEN

!fd @@@ ca n'est pas suffisant non ?

         IF (((S_POLYR(iantac)%maxpos(1)+perio_shift(1))-S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
         IF (((S_POLYR(iantac)%maxpos(2)+perio_shift(2))-S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
         IF ((S_POLYR(iantac)%maxpos(3)                 -S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE


!fd @@@ je rajoute ....

         IF ((S_POLYR(icdtac)%maxpos(1)-(S_POLYR(iantac)%minpos(1)+perio_shift(1))+adist)<0.D0) CYCLE
         IF ((S_POLYR(icdtac)%maxpos(2)-(S_POLYR(iantac)%minpos(2)+perio_shift(2))+adist)<0.D0) CYCLE
         IF ((S_POLYR(icdtac)%maxpos(3)- S_POLYR(iantac)%minpos(3)                +adist)<0.D0) CYCLE

         CALL cpu_time(t1)

         if (associated(status)) deallocate(status)
         if (associated(xco)) deallocate(xco)
         if (associated(ovlap)) deallocate(ovlap)
         if (associated(t)) deallocate(t)
         if (associated(n)) deallocate(n)
         if (associated(s)) deallocate(s)
         if (associated(surf)) deallocate(surf)

         nullify(status,xco,ovlap,t,n,s,surf)
                
         CALL DETECTION_non_convex(iantac,icdtac,gdist,adist, &
                                   nb_ctc,nbf,status,xco,ovlap, &
                                   t,n,s,surf,.true.)
         !fd a reprendre
         !if (nb_ctc /= 0) then
         !  i_visavis = i_visavis + 1
         !  visavis(i_visavis)%isee = isee
         !endif

         CALL cpu_time(t2)

         nb_tot_detect=nb_tot_detect+1
         nb_detection_test=nb_detection_test+1
         detection_time=detection_time+t2-t1
         
         IF (nb_ctc==0) CYCLE

           localframe_cd = get_inertia_frameTT_POLYR(icdtac)
           localframe_an = get_inertia_frameTT_POLYR(iantac)

           cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
           an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
           
           DO j=1,nbf
             if (status(j) <= 0) cycle
             icdan                   = icdan+1

             IF (icdan>size_of_array_this) THEN
                            !123456789012345678901234567890123456789012345
                call logmes('---------------------------------------------', .true.)
                call logmes('ERROR filling this                           ', .true.)
                call logmes('you rich the allocated size                  ', .true.)
                call logmes('In your python script use                    ', .true.)
                call logmes('                                             ', .true.)
                call logmes('PRPRx_LowSizeArrayPolyr(sizefactor)          ', .true.)
                call logmes('                                             ', .true.)
                call logmes('where sizefactor is an integer specifiyng the', .true.)
                call logmes('ratio of memory you need (=4 by default)     ', .true.)
                call logmes('---------------------------------------------', .true.)

                call faterr(IAM,'Error')
             ENDIF   

             vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1,j)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*t(2,j)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*t(3,j)
             vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1,j)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*n(2,j)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*n(3,j)
             vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1,j)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*s(2,j)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*s(3,j)

             this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
             this(icdan)%ianbtac = polyr2bdyty(2, iantac)

             this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
             this(icdan)%ianbtyp = polyr2bdyty(3, iantac)

             this(icdan)%icdctyp = i_polyr
             this(icdan)%ianctyp = i_polyr

             nb_adj(icdtac)      = nb_adj(icdtac) + 1
             iadj                = nb_adj(icdtac)
             this(icdan)%iadj    = iadj
             this(icdan)%icdbdy  = polyr2bdyty(1, icdtac)
             this(icdan)%icdtac  = icdtac
             this(icdan)%ianbdy  = polyr2bdyty(1, iantac)
             this(icdan)%iantac  = iantac
             this(icdan)%isee    = isee
             this(icdan)%tuc(:)  = t(:, j)
             this(icdan)%nuc(:)  = n(:, j)
             this(icdan)%suc(:)  = s(:, j)

             this(icdan)%coor     = xco(1:3, j)
             this(icdan)%type_ctc = nb_ctc

             cd_ent = get_ent_POLYR(this(icdan)%icdtac)
             an_ent = get_ent_POLYR(this(icdan)%iantac) 
         
             this(icdan)%icdent = cd_ent
             this(icdan)%ianent = an_ent

             entity(cd_ent)%nb = entity(cd_ent)%nb + 1
             entity(an_ent)%nb = entity(an_ent)%nb + 1

!fd le 11/09/08 manquait le shift. PRcoor c'est le centre du polyr pas le centre d'inertie
             cdlev = xco(1:3,j)  &
                   - (PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac))
             anlev = xco(1:3,j) &
                   - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift)


!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
!fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

             this(icdan)%icdcoor(1)=cdlev(1)*localframe_cd(1,1)+ &
                                    cdlev(2)*localframe_cd(2,1)+ &
                                    cdlev(3)*localframe_cd(3,1)
             this(icdan)%icdcoor(2)=cdlev(1)*localframe_cd(1,2)+ &
                                    cdlev(2)*localframe_cd(2,2)+ &
                                    cdlev(3)*localframe_cd(3,2)
             this(icdan)%icdcoor(3)=cdlev(1)*localframe_cd(1,3)+ &
                                    cdlev(2)*localframe_cd(2,3)+ & 
                                    cdlev(3)*localframe_cd(3,3)

             this(icdan)%iancoor(1)=anlev(1)*localframe_an(1,1)+ &
                                    anlev(2)*localframe_an(2,1)+ &
                                    anlev(3)*localframe_an(3,1)
             this(icdan)%iancoor(2)=anlev(1)*localframe_an(1,2)+ &
                                    anlev(2)*localframe_an(2,2)+ &
                                    anlev(3)*localframe_an(3,2)
             this(icdan)%iancoor(3)=anlev(1)*localframe_an(1,3)+ &
                                    anlev(2)*localframe_an(2,3)+ &
                                    anlev(3)*localframe_an(3,3)

!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!             print*,icdan
!             print*, xco(1:3,j)
!             print*,this(icdan)%coorcd(1:3)
!             print*,this(icdan)%cooran(1:3)
!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'


             ! On va calculer le passage rep inertie -> rep général pour l'antagoniste

             Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
             Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
             Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

             Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
             Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
             Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

             Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
             Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
             Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)

             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 


             ! On va calculer le passage rep inertie -> rep général pour le candidat

             Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
             Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
             Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

             Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
             Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
             Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

             Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
             Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
             Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)


             this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
           
             ! Calcul des vitesses relatives


             this(icdan)%gapTTbegin      = ovlap(j)


             this(icdan)%vltBEGIN = vlt_cst &
                  + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                  - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

             this(icdan)%vlnBEGIN = vln_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                  - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

             this(icdan)%vlsBEGIN= vls_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                  - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)


             this(icdan)%rls      = 0.D0
             this(icdan)%rlt      = 0.D0
             this(icdan)%rln      = 0.D0
             this(icdan)%vls      = this(icdan)%vlsBEGIN
             this(icdan)%vlt      = this(icdan)%vltBEGIN
             this(icdan)%vln      = this(icdan)%vlnBEGIN
             this(icdan)%gapTT    = this(icdan)%gapTTbegin
             this(icdan)%status   = i_nknow

             this(icdan)%area     = surf(j)

             this(icdan)%icdsci = j
             this(icdan)%iansci = 0

           ENDDO
         ENDIF  
     ENDDO
     nb_PRPRx=icdan

     !fd a reprendre
     !if (is_first_time_f2f) then
     !  is_first_time_f2f = .false.
     !  nb_visavis = i_visavis
     !endif       

     if (associated(status)) deallocate(status)
     if (associated(xco)) deallocate(xco)
     if (associated(ovlap)) deallocate(ovlap)
     if (associated(t)) deallocate(t)
     if (associated(n)) deallocate(n)
     if (associated(s)) deallocate(s)
     if (associated(surf)) deallocate(surf)

     nullify(status,xco,ovlap,t,n,s,surf)

   ENDIF

   WRITE(mes,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
   call logmes(mes)
   write(mes,*) 'Total time of detection: ',detection_time
   call logmes(mes)
   write(mes,*) 'Nb detection tests :',REAL(nb_detection_test,8)     
   call logmes(mes)


   do icdan = 1, nb_PRPRx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   DO itac=1,nb_POLYR
     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
     IF (nb_adj(itac) /= 0) THEN
       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
       IF (errare /=0 ) THEN
         call faterr(IAM,' error in allocating adjac(icdtac)%.....')
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_PRPRx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PRPRx),stat=errare)

  END SUBROUTINE nc_compute_contact_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !fd routine de detection entre objets de forme non convexe
  !fd 2 strategies: on considere l'objet dans son ensemble ou on travaille avec ses 
  !                 faces topologiques
  !fd
  SUBROUTINE DETECTION_non_convex(id1,id2,global_adist,local_adist, &
                                 nb_ctc,nbf,status,PT_CTC,overlap, &
                                 tt,nn,ss,surfaces,do_trim)

! I
! id1 : id solide 1 (antagoniste)
! id2 : id solide 2 (candidat)
! global_adist : distance d'alerte globale qui aide dans la recherche des 
!                contacts (notion de halo) 
! local_adist : distance d'alerte locale pour decider si un contact est actif
! do_trim: a flag to say if some triming on normals is performed (.true.) or not (.false.)
!           when using large triming should be avoided
   
!
! O 
! nb_ctc        : nombre reel de points de contact
! nbf           : taille des tableaux qu'on remonte (nombre de faces | nombre de sommets du cd)
! status        : =0 pas contact, =1 noeud, =2 arete, =3 face  
! PT_CTC(nbf)   : coordonnees des points de contact
! overlap(nbf)  : les gaps aux points de contact
! tt(3,nbf)     : tangentes aux points de contact si il y a contact
! nn(3,nbf)     : normales aux points de contact si il y a contact
! ss(3,nbf)     : tangentes aux points de contact si il y a contact
! surfaces(nbf) : aire du point de contact 

!fd ATTENTION TOUS LES POINTEURS DOIVENT ETRE A NUL !!

   IMPLICIT NONE

   INTEGER                                 :: id1,id2,nb_ctc,nbf
   REAL(kind=8)                            :: local_adist,global_adist
   integer,dimension(:),pointer            :: status
   REAL(kind=8),DIMENSION(:),pointer       :: overlap,surfaces
   REAL(kind=8),DIMENSION(:,:),pointer     :: PT_CTC,tt,nn,ss
   logical                                 :: do_trim
   !***
                                                !123456789012345678901234567
   CHARACTER(len=27)                   :: IAM = 'PRPRx::detection_non_convex'
   TYPE(T_POLYR)                       :: PRan,PRcd

   ! pour chaque face le centre et sa normale
   real(kind=8),dimension(:,:),pointer :: centres,cd_normales 
   real(kind=8),dimension(:,:),pointer :: an_normales
   integer                             :: if,jf,istat,ff,ppp
   real(kind=8)                        :: gap,point(3),t(3),n(3),s(3),weight(3),dist1

   integer,dimension(:),allocatable    :: good_nodes 

   integer :: i,j,fi,fj
   logical :: is_ok

   character(len=90) :: cout
   integer           :: err_

   nullify(centres)

   nb_ctc=0

   PRan    = S_POLYR(id1)
   PRcd    = S_POLYR(id2)

   ! dans le cas general (par f2f) on peut mettre les candidats au milieu des faces ou au sommet des faces  
   if (with_f2f .or. (.not. with_f2f .and. .not. nodal_contact)) then
    
     cd_normales => PRcd%normal   
     an_normales => PRan%normal   

     call get_centres_faces(PRcd%id,centres)  

     call get_surfaces_faces(PRcd%id,surfaces)  

     !fd on cherche l'intersection de chaque pic du herisson avec le he_hdl antagoniste

     nbf=PRcd%nb_faces
     allocate(status(nbf),overlap(nbf),pt_ctc(3,nbf),tt(3,nbf),nn(3,nbf),ss(3,nbf))

   else

     nbf=PRcd%nb_vertex

     nullify(cd_normales) 
     call get_nodal_normals_HE_Hdl(PRcd%HE_Hdl,cd_normales,err_)  
     if (err_ > 0) then
       write(cout,'("PRcd ",I0)') PRcd%id
       call logmes(cout, .true.)
       call faterr(IAM,'unexpected problem while getting nodal normals')
    endif   

     an_normales => PRan%normal   

     !print*,nbf

     !print*,'cd normales'
     !write(*,'(3(1x,D12.5))') cd_normales
     !print*,'---'
 
     if (associated(centres)) deallocate(centres)
     allocate(centres(3,PRcd%nb_vertex))

     !fd vu que c'est un peu complique de modifier l'antagoniste
     do i=1,PRcd%nb_vertex
       centres(:,i)=PRcd%vertex(:,i) - perio_shift(:)
     enddo
       
     !print*,'cd centres'
     !write(*,'(3(1x,D12.5))') centres
     !print*,'---'

     if (associated(surfaces)) deallocate(surfaces)
     allocate(surfaces(PRcd%nb_vertex))
     surfaces = 1.d0


     allocate(status(nbf),overlap(nbf),pt_ctc(3,nbf),tt(3,nbf),nn(3,nbf),ss(3,nbf))

   endif


   !fd deux facons de rechercher 
   !fd: logique "f2f" on teste face topo/face topo en essayant de ne considerer 
   !    que celles qui se voient
   !fd: logique "one to all" pour chaque point cd on cherche parmi toutes les elements an
   !fd on cherche l'intersection de chaque pic du herisson avec le he_hdl antagoniste

   if (with_f2f) then

     allocate(good_nodes(PRan%nb_vertex))

     !print*,'----------------------oooo-------------------------------'
     !print*,'recherche f2f nc' 
     !print*,'candidat   : ',PRcd%id,' antagoniste: ',PRan%id

     do fi=1,size(PRcd%f2f_set)
       do i=1,size(PRcd%f2f_set(fi)%G_i)

         if=PRcd%f2f_set(fi)%G_i(i)

         !print*,'face topo:',fi,' ele: ',if

         status(if) =0           
         overlap(if) =0.d0
         pt_ctc(:,if)=0.d0
         tt(:,if) = 0.d0
         nn(:,if) = 0.d0
         ss(:,if) = 0.d0

         if (surfaces(if) == 0.d0) then
            print*,'face trop petite'
            cycle
         endif       

         !fd on regarde si le set de faces convient
         
         do fj=1,size(PRan%f2f_set)
           is_ok = .false.
           do j=1,size(PRan%f2f_set(fj)%G_i)       
             ff = PRan%f2f_set(fj)%G_i(j)
             dist1 = dot_product(cd_normales(:,if),an_normales(:,ff))
             if ( dist1 < -1.d0 + f2f_tol) then 
               is_ok=.true.
               exit
             endif
           enddo
        
           ! on tag les noeuds de la face topologique  
           if (is_ok) then

             !print*,'voit la face topo',fj

             good_nodes=0
             do j=1,size(PRan%f2f_set(fj)%G_i)       
               good_nodes(PRan%face(1,PRan%f2f_set(fj)%G_i(j)))=1
               good_nodes(PRan%face(2,PRan%f2f_set(fj)%G_i(j)))=1
               good_nodes(PRan%face(3,PRan%f2f_set(fj)%G_i(j)))=1
             enddo

             ppp = 0
             status(if) = node_HE_Hdl_proximity(PRan%HE_Hdl,centres(:,if),global_adist, &
                                                cd_normales(:,if),.true.,ppp,gap, &
                                                point,t,n,s,ff,weight,.false.,err_,good_nodes=good_nodes)

             if (err_ > 0) then
               write(cout,'("PRan ",I0)') PRan%id
               call logmes(cout, .true.)
               call faterr(IAM,'unexpected problem while getting proximal node to HE')
             endif   

             
             !print*,'statut: ',status(if)
             !if (status(if) > 0) then
             !  print*,'cd: ',PRcd%id,' an: ',PRan%id
             !  print*,'face cd: ',if 
             !  print*,'normale: '
             !  print*,cd_normales(:,if)   
             !  print*,'face an: ', ff
             !  print*,'normale   : '
             !  print*,an_normales(:,ff)
             !  print*,'connectivite: ',PRan%face(1,ff),PRan%face(2,ff),PRan%face(3,ff)
             !  print*,'ppp: ',ppp
             !  print*,'vertex   : '
             !  print*,PRan%vertex(:,PRan%face(1,ff))
             !  print*,PRan%vertex(:,PRan%face(2,ff))
             !  print*,PRan%vertex(:,PRan%face(3,ff))
             !  print*,'poids: '
             !  print*,weight
             !  print*,'distance et rep local'
             !  print*,gap,local_adist
             !  print*,t
             !  print*,n
             !  print*,s
             !endif


             if (status(if) > 0 .and. gap < local_adist) then
               nb_ctc = nb_ctc + 1
               overlap(if) =gap
               !fd pour etre coherent avec nc_compute_contact
               pt_ctc(:,if)=point(:)+perio_shift(:)
               tt(:,if) = t(:)
               nn(:,if) = n(:)
               ss(:,if) = s(:)
             else
               ! on remet a 0 pour virer les cas avec local_adist trop grand
               status(if) =0     
               overlap(if) =0.d0
               pt_ctc(:,if)=0.d0
               tt(:,if) = 0.d0
               nn(:,if) = 0.d0
               ss(:,if) = 0.d0
             endif       
           endif             
           if (status(if) /= 0) exit ! on en a trouve un c est bon
         enddo
       enddo
     enddo

     deallocate(good_nodes)

   else

     allocate(good_nodes(PRan%nb_vertex))

     do if=1,nbf

       !print*,'----------------------oooo-------------------------------'
       !print*,'antagoniste: ',PRan%id
       !print*,'candidat   : ',PRcd%id,' face: ',if

       if (surfaces(if) == 0.d0) then
         ! on remet a 0 pour virer les cas avec local_adist trop grand
         status(if) =0           
         overlap(if) =0.d0
         pt_ctc(:,if)=0.d0
         tt(:,if) = 0.d0
         nn(:,if) = 0.d0
         ss(:,if) = 0.d0
         !print*,'face trop petite'
         cycle
       endif       

       good_nodes = 1
       !do jf=1,PRan%nb_faces 
       !  if ((dot_product(PRan%normal(:,jf),cd_normales(:,if))) < 0.) then
       !     good_nodes(PRan%face(1,jf))=1
       !     good_nodes(PRan%face(2,jf))=1
       !     good_nodes(PRan%face(3,jf))=1
       !   endif 
       !enddo

       !print*,'on degage ',count(good_nodes==1),' noeuds de ',PRan%id



       !print*,'centre et dir'
       !print*,centres(:,if)
       !print*,normales(:,if)


       ppp = 0
       status(if) = node_HE_Hdl_proximity(PRan%HE_Hdl,centres(:,if),global_adist, &
                                          cd_normales(:,if),do_trim,ppp,gap, &
                                          point,t,n,s,ff,weight,.false.,err_,good_nodes=good_nodes)

       if (err_ > 0) then
         write(cout,'("PRan ",I0)') PRan%id
         call logmes(cout, .true.)
         call faterr(IAM,'unexpected problem while getting proximal node to HE')
       endif   

       
       !print*,'statut: ',status(if)
       !if (status(if) > 0) then
       !  print*,'cd: ',PRcd%id,' an: ',PRan%id
       !  print*,'face cd: ',if 
       !  print*,'normale: '
       !  print*,cd_normales(:,if)   
       !  print*,'face an: ', ff
       !  print*,'normale   : '
       !  print*,an_normales(:,ff)
       !  print*,'connectivite: ',PRan%face(1,ff),PRan%face(2,ff),PRan%face(3,ff)
       !  print*,'ppp: ',ppp
       !  print*,'vertex   : '
       !  print*,PRan%vertex(:,PRan%face(1,ff))
       !  print*,PRan%vertex(:,PRan%face(2,ff))
       !  print*,PRan%vertex(:,PRan%face(3,ff))
       !  print*,'poids: '
       !  print*,weight
       !  print*,'distance et rep local'
       !  print*,gap
       !  print*,t
       !  print*,n
       !  print*,s
       !endif

       if (status(if) > 0 .and. gap < local_adist) then
         nb_ctc = nb_ctc + 1
         overlap(if) =gap
         !fd pour etre coherent avec nc_compute_contact         
         pt_ctc(:,if)=point(:)+perio_shift(:)
         tt(:,if) = t(:)
         nn(:,if) = n(:)
         ss(:,if) = s(:)
       else
         ! on remet a 0 pour virer les cas avec local_adist trop grand
         status(if) =0           
         overlap(if) =0.d0
         pt_ctc(:,if)=0.d0
         tt(:,if) = 0.d0
         nn(:,if) = 0.d0
         ss(:,if) = 0.d0
       endif       
     enddo
     deallocate(good_nodes)
   endif

   if (.not. associated(cd_normales,PRcd%normal)) deallocate(cd_normales)
   nullify(cd_normales)

   deallocate(centres) 
   nullify(centres)

  END SUBROUTINE DETECTION_non_convex
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !fd driver de la strategie de detection f2f pour face plane ou non  
  !fd
  ! TODO : handle groups in order to sue this fine detetction method for DDM applications
  SUBROUTINE f2f4all_compute_contact_PRPRx(gdist,reset)
 
   IMPLICIT NONE  

   logical, optional :: reset

   INTEGER                     :: errare 
   INTEGER                     :: icdan,iadj,itac,i,j
   INTEGER                     :: icdtac,iantac,isee    
   REAL(kind=8)                :: raycd,rayan,adist,dist,gdist
   INTEGER                     :: size_of_array_this

   
   !fd des structures de donnees pour vehiculer l'info
   !fd a cause d'une gestion differente des noeuds de contact 
   !fd (planplan -> 4, nc -> autant que d'ele dans la face topo) 
   !fd et de leur indexation possible
   !fd (planplan -> le rang convient, nc -> le num d'ele cd convient) 
   !fd les structures de donnees ont un indice (le 3eme) dependant de la face topo
   !fd ca fait des tableaux plus gros mais y a moins de risque d'ecrasement
   integer,DIMENSION(:,:),pointer :: status 
   REAL(kind=8),DIMENSION(:,:,:),pointer :: xco
   REAL(kind=8),DIMENSION(:,:,:),pointer :: t
   REAL(kind=8),DIMENSION(:,:,:),pointer :: n
   REAL(kind=8),DIMENSION(:,:,:),pointer :: s
   REAL(kind=8),DIMENSION(:,:),pointer   :: ovlap
   REAL(kind=8),DIMENSION(:),pointer   :: surf

   REAL(kind=8),DIMENSION(3)   :: sep,cdlev,anlev
   REAL(kind=8)                :: norme
   REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
   REAL(kind=8)                :: t1,t2,vls_cst,vln_cst,vlt_cst

! f2f 
   logical                     :: is_first_time_f2f = .true.
   integer                     :: nb_potential_contact,i_visavis

   !fd index du visavis actif. necessaire pour calculer la vitesse thermique
   integer                     :: iv,iv_begin,jv,ic,fi
                                      !123456789012345678901234567890
   character(len=30)           :: IAM='PRPRx::f2f4all_compute_contact'
   character(len=80)           :: cout
   character(len=450)          :: lcout

   integer :: cd_ent,an_ent,izob

   if ( present(reset) ) then
     if (reset ) then
       is_first_time_f2f = .true.
       return
     endif  
   end if

   if (.not. with_f2f) then
     call faterr(IAM,'use it with f2f')
   endif

   icdan   = 0        
   nb_PRPRx= 0
   nb_adj  = 0

   IF (nb_rough_PRPRx /= 0 ) THEN

     nullify(status, xco, t, n, s, ovlap, surf)

     iv = 0

     size_of_array_this=SIZE(this)


     if (is_first_time_f2f) then
       nb_potential_contact = nb_rough_PRPRx 
       ALLOCATE(visavis(size_of_array_this))
       nb_visavis = 0
       i_visavis=0

       !fd initialisation de la structure de donnees visavis
       do i=1,size_of_array_this
         nullify(visavis(i)%iff_cd,visavis(i)%iff_an, &
                 visavis(i)%cd_lcoor,visavis(i)%an_lcoor, &
                 visavis(i)%index)
         visavis(i)%nb_ctc=0
       enddo
     else
       nb_potential_contact = nb_visavis  
       !do i=1,nb_visavis
       !    print*,'v2v ',i  
       !    print*,visavis(i)%nb_ctc,visavis(i)%is_flat
       !enddo
     endif 
   !
   ! preparation de la detection 
   !
     DO i=1,nb_potential_contact

       if (is_first_time_f2f) then
         !au premier passage on se sert de rough
         icdtac    = rough_PRPRx(i)%cd
         iantac    = rough_PRPRx(i)%an 
         isee      = rough_PRPRx(i)%isee
         ! rien a foutre la je pense: visavis(i)%nb_ctc=0

         perio_shift = 0.d0
         perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
         perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode

       else
         !fd detection explicite on court - circuite rough
         icdtac    = visavis(i)%cd
         iantac    = visavis(i)%an 
         isee      = visavis(i)%isee
         iv = i

         if ( .NOT. get_visible_POLYR(icdtac) .or. .NOT. get_visible_POLYR(iantac) ) CYCLE


         !fd TODO gestion du periodique

         !print*,'on travaille sur le v2v ', iv

       endif          


       !print*,'on trace',i,visavis(i)%nb_ctc


       !fd maj de la recherche de proximite (utile si step sur compute_box)
       adist=see(isee)%alert 
       dist=S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist

       sep=S_POLYR(icdtac)%center - (S_POLYR(iantac)%center + perio_shift)
       norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)

       IF (norme <1.D-24) THEN
         write(cout,'(A,I0)')        ' For rough contact ',i 
         call logmes(cout, .true.)
         write(cout,'(A,I0,A,I0,A,D14.7)')   ' Distance between cd ',icdtac,' and an ',iantac,' is ',norme
         call logmes(cout, .true.)
         write(cout,'(A,3(1x,D14.7))')     '  --> center of cd ',S_POLYR(icdtac)%center
         call logmes(cout, .true.)
         write(cout,'(A,3(1x,D14.7))')     '  --> center of an ',S_POLYR(iantac)%center
         call logmes(cout, .true.)
         call faterr(IAM,"")
       ENDIF 

       IF (norme < dist*dist) THEN

       !fd narrow phase: on affine la recherche de proximite par intersection des AABB

         IF (((S_POLYR(iantac)%maxpos(1)+perio_shift(1)) - &
               S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
         IF (((S_POLYR(iantac)%maxpos(2)+perio_shift(2)) - &
               S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
         IF (( S_POLYR(iantac)%maxpos(3)                 - &
               S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE

         IF ((S_POLYR(icdtac)%maxpos(1) - &
             (S_POLYR(iantac)%minpos(1)+perio_shift(1))+adist)<0.D0) CYCLE
         IF ((S_POLYR(icdtac)%maxpos(2) - &
             (S_POLYR(iantac)%minpos(2)+perio_shift(2))+adist)<0.D0) CYCLE
         IF ((S_POLYR(icdtac)%maxpos(3) - &
              S_POLYR(iantac)%minpos(3)                +adist)<0.D0) CYCLE

         CALL cpu_time(t1)

         if (associated(status)) deallocate(status)
         if (associated(xco)) deallocate(xco)
         if (associated(ovlap)) deallocate(ovlap)
         if (associated(t)) deallocate(t)
         if (associated(n)) deallocate(n)
         if (associated(s)) deallocate(s)
         if (associated(surf)) deallocate(surf)
         nullify(status,xco,ovlap,t,n,s,surf)

         if (is_first_time_f2f) then

           iv_begin = iv
           
           !fd en sortie i_visavis contient le rang occupe par 
           !fd le dernier element ajoute dans le tableau visavis
           !fd attention on peut en ajouter plusieurs d'un coup

           CALL DETECTION_F2F4all(iantac,icdtac,isee,gdist,sep, &
                                  status,xco,ovlap,n,t,s, &
                                  surf,visavis,i_visavis)

           !print*,i_visavis
           !print*,size(status),size(xco),size(ovlap),size(n),size(t),size(s), &
           !       size(surf),size(visavis)


           !do izob=1,i_visavis-iv_begin
           !  print*,'on a ajoute le v2v ',iv_begin + izob
           !  print*,visavis(iv_begin + izob)%nb_ctc,visavis(iv_begin + izob)%is_flat
           !enddo 

           iv = i_visavis
         else
           iv_begin = iv - 1

           !print*,'actualisation du v2v ',iv  
           !print*,visavis(iv)%nb_ctc,visavis(iv)%is_flat

           call update_f2f4all(visavis(iv),xco,ovlap,n,t,s,surf)

         endif

         CALL cpu_time(t2)

         ! on a rien trouve de nouveau 
         IF (iv_begin == iv ) CYCLE

         localframe_cd = get_inertia_frameTT_POLYR(icdtac)
         localframe_an = get_inertia_frameTT_POLYR(iantac)

         cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
         an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)

         ! boucle sur les faces cd
         DO jv=iv_begin+1,iv

           ! la face topo cd concernee
           fi = visavis(jv)%id_f_cd

           ! boucle sur les ele de la face cd
           do ic=1,visavis(jv)%nb_ctc

             !fd calcul de l'indice pour retrouver les infos dans les tableaux (xco, ovlap, etc) remontes de la detection
             if (visavis(jv)%is_flat) then
               j = ic                     !<- rang du contact
             else
               j = visavis(jv)%iff_cd(ic) !<- rang de l'ele dans la face topo cd
             endif           
             
             icdan = icdan + 1

             IF (icdan>size_of_array_this) THEN
                            !123456789012345678901234567890123456789012345
                call logmes('---------------------------------------------', .true.)
                call logmes('ERROR filling this                           ', .true.)
                call logmes('you rich the allocated size                  ', .true.)
                call logmes('In your python script use                    ', .true.)
                call logmes('                                             ', .true.)
                call logmes('PRPRx_LowSizeArrayPolyr(sizefactor)          ', .true.)
                call logmes('                                             ', .true.)
                call logmes('where sizefactor is an integer specifiyng the', .true.)
                call logmes('ratio of memory you need (=4 by default)     ', .true.)
                call logmes('---------------------------------------------', .true.)

                call faterr(IAM,'Error')
             ENDIF   

             visavis(jv)%index(ic) = icdan

             this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
             this(icdan)%ianbtac = polyr2bdyty(2, iantac)

             this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
             this(icdan)%ianbtyp = polyr2bdyty(3, iantac)

             this(icdan)%icdctyp = i_polyr
             this(icdan)%ianctyp = i_polyr

             nb_adj(icdtac)      = nb_adj(icdtac) + 1
             iadj                = nb_adj(icdtac)
             this(icdan)%iadj    = iadj
             this(icdan)%icdbdy  = polyr2bdyty(1, icdtac)
             this(icdan)%icdtac  = icdtac
             this(icdan)%ianbdy  = polyr2bdyty(1, iantac)
             this(icdan)%iantac  = iantac
             this(icdan)%isee    = isee

             this(icdan)%tuc     = t(:, j, fi)
             this(icdan)%nuc     = n(:, j, fi)
             this(icdan)%suc     = s(:, j, fi)

             if (dbg .and. (icdtac == dbg_idcd .and. iantac == dbg_idan) ) then
               print*,'face ',fi,' contact ',ic,' rang tableaux ',j
               write(*,'(I0,3(1x,E12.5))') icdan, xco(:,j,fi)
             endif

             this(icdan)%coor        = xco(1:3,j,fi)
             this(icdan)%type_ctc    = visavis(jv)%nb_ctc
             this(icdan)%gapTTbegin  = ovlap(j,fi)

             cd_ent = get_ent_POLYR(this(icdan)%icdtac)
             an_ent = get_ent_POLYR(this(icdan)%iantac) 
         
             this(icdan)%icdent = cd_ent
             this(icdan)%ianent = an_ent

             entity(cd_ent)%nb = entity(cd_ent)%nb+1
             entity(an_ent)%nb = entity(an_ent)%nb+1

             cdlev = xco(1:3,j,fi)  &
                   - (PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac))
             anlev = xco(1:3,j,fi) &
                   - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift)

             !print*,'contact: ',icdan
             !write(*,'(A,3(1x,E12.5))') 'cdlev',cdlev 
             !write(*,'(A,3(1x,E12.5))') 'anlev',anlev 
             !write(*,'(A)')             'local frame cd'
             !write(*,'(3(1x,E12.5))') localframe_cd
             !write(*,'(A)')             'local frame an'
             !write(*,'(3(1x,E12.5))') localframe_an

!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
!fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

             this(icdan)%icdcoor(1)=cdlev(1)*localframe_cd(1,1) + &
                                    cdlev(2)*localframe_cd(2,1) + &
                                    cdlev(3)*localframe_cd(3,1)
             this(icdan)%icdcoor(2)=cdlev(1)*localframe_cd(1,2) + &
                                    cdlev(2)*localframe_cd(2,2) + &
                                    cdlev(3)*localframe_cd(3,2)
             this(icdan)%icdcoor(3)=cdlev(1)*localframe_cd(1,3) + &
                                    cdlev(2)*localframe_cd(2,3) + &
                                    cdlev(3)*localframe_cd(3,3)

             this(icdan)%iancoor(1)=anlev(1)*localframe_an(1,1) + &
                                    anlev(2)*localframe_an(2,1) + &
                                    anlev(3)*localframe_an(3,1)
             this(icdan)%iancoor(2)=anlev(1)*localframe_an(1,2) + &
                                    anlev(2)*localframe_an(2,2) + &
                                    anlev(3)*localframe_an(3,2)
             this(icdan)%iancoor(3)=anlev(1)*localframe_an(1,3) + &
                                    anlev(2)*localframe_an(2,3) + &
                                    anlev(3)*localframe_an(3,3)

!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'
!             print*,icdan
!             print*, xco(1:3,j)
!             print*,this(icdan)%coorcd(1:3)
!             print*,this(icdan)%cooran(1:3)
!             print*,'xxxxxxxxxxxxxxxxxxxxxxx'


             ! On va calculer le passage rep inertie -> rep général pour l'antagoniste

             Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
             Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
             Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

             Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
             Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
             Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

             Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
             Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
             Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)

             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                                  Rc(1,2)*this(icdan)%tuc(2) + &
                                  Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + &
                                  Rc(2,2)*this(icdan)%tuc(2) + & 
                                  Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + &
                                  Rc(3,2)*this(icdan)%tuc(2) + &
                                  Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + &
                                  Rc(1,2)*this(icdan)%nuc(2) + &
                                  Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + &
                                  Rc(2,2)*this(icdan)%nuc(2) + &
                                  Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + &
                                  Rc(3,2)*this(icdan)%nuc(2) + &
                                  Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + &
                                  Rc(1,2)*this(icdan)%suc(2) + &
                                  Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + &
                                  Rc(2,2)*this(icdan)%suc(2) + &
                                  Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + &
                                  Rc(3,2)*this(icdan)%suc(2) + &
                                  Rc(3,3)*this(icdan)%suc(3) 


             ! On va calculer le passage rep inertie -> rep général pour le candidat

             Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
             Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
             Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

             Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
             Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
             Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

             Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
             Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
             Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)


             this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                                  Rc(1,2)*this(icdan)%tuc(2) + &
                                  Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + &
                                  Rc(2,2)*this(icdan)%tuc(2) + &
                                  Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + &
                                  Rc(3,2)*this(icdan)%tuc(2) + &
                                  Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + &
                                  Rc(1,2)*this(icdan)%nuc(2) + &
                                  Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + &
                                  Rc(2,2)*this(icdan)%nuc(2) + &
                                  Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + &
                                  Rc(3,2)*this(icdan)%nuc(2) + &
                                  Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + &
                                  Rc(1,2)*this(icdan)%suc(2) + &
                                  Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + &
                                  Rc(2,2)*this(icdan)%suc(2) + &
                                  Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + &
                                  Rc(3,2)*this(icdan)%suc(2) + &
                                  Rc(3,3)*this(icdan)%suc(3) 
           
             ! Calcul des vitesses relatives

             vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%tuc(3)
             vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%nuc(3)
             vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%suc(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%suc(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%suc(3)

             this(icdan)%vltBEGIN = vlt_cst + &
                                    cd_Vbegin(4)*this(icdan)%Gcdt(1) + &
                                    cd_Vbegin(5)*this(icdan)%Gcdt(2) + &
                                    cd_Vbegin(6)*this(icdan)%Gcdt(3) - &
                                    an_Vbegin(4)*this(icdan)%Gant(1) - &
                                    an_Vbegin(5)*this(icdan)%Gant(2) - &
                                    an_Vbegin(6)*this(icdan)%Gant(3)

             this(icdan)%vlnBEGIN = vln_cst + &     
                                    cd_Vbegin(4)*this(icdan)%Gcdn(1) + &
                                    cd_Vbegin(5)*this(icdan)%Gcdn(2) + &
                                    cd_Vbegin(6)*this(icdan)%Gcdn(3) - &
                                    an_Vbegin(4)*this(icdan)%Gann(1) - &
                                    an_Vbegin(5)*this(icdan)%Gann(2) - &
                                    an_Vbegin(6)*this(icdan)%Gann(3)

             this(icdan)%vlsBEGIN= vls_cst + &     
                                   cd_Vbegin(4)*this(icdan)%Gcds(1) + &
                                   cd_Vbegin(5)*this(icdan)%Gcds(2) + &
                                   cd_Vbegin(6)*this(icdan)%Gcds(3) - &
                                   an_Vbegin(4)*this(icdan)%Gans(1) - &
                                   an_Vbegin(5)*this(icdan)%Gans(2) - & 
                                   an_Vbegin(6)*this(icdan)%Gans(3)

             this(icdan)%rls      = 0.D0
             this(icdan)%rlt      = 0.D0
             this(icdan)%rln      = 0.D0
             this(icdan)%vls      = this(icdan)%vlsBEGIN
             this(icdan)%vlt      = this(icdan)%vltBEGIN
             this(icdan)%vln      = this(icdan)%vlnBEGIN
             this(icdan)%gapTT    = this(icdan)%gapTTbegin
             this(icdan)%status   = i_nknow

             if (visavis(jv)%is_flat) then
               this(icdan)%area   = 0.d0
             else
               this(icdan)%area   = surf(j)
             endif

             this(icdan)%icdsci = 0
             this(icdan)%iansci = 0
             if (with_f2f) then
                this(icdan)%id_f_cd = visavis(jv)%id_f_cd
                this(icdan)%id_f_an = visavis(jv)%id_f_an
                this(icdan)%icdsci = ic
             endif

           ENDDO
         ENDDO
       ENDIF  
     ENDDO
     nb_PRPRx=icdan


     !fd pour limiter les fuites memoires on fait le menage
     !fd avant de partir
     if (associated(status)) deallocate(status)
     if (associated(xco)) deallocate(xco)
     if (associated(ovlap)) deallocate(ovlap)
     if (associated(t)) deallocate(t)
     if (associated(n)) deallocate(n)
     if (associated(s)) deallocate(s)
     if (associated(surf)) deallocate(surf)
     nullify(status,xco,ovlap,t,n,s,surf)

     if (with_f2f .and. is_first_time_f2f) then
       is_first_time_f2f = .false.
       nb_visavis = i_visavis
     endif       

   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'       
   call logmes(cout)
   write(cout,*) 'Total time of detection: ',detection_time
   call logmes(cout)
   write(cout,*) 'Nb detection tests :',REAL(nb_detection_test,8)     
   call logmes(cout)


   do icdan = 1, nb_PRPRx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   DO itac=1,nb_POLYR
     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
     IF (nb_adj(itac) /= 0) THEN
       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
       IF (errare /=0 ) THEN
         call faterr(IAM,' error in allocating adjac(icdtac)%.....')
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_PRPRx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PRPRx),stat=errare)

   ! do i=1,nb_visavis
   !    print*,'v2v ',i  
   !    print*,visavis(i)%nb_ctc,visavis(i)%is_flat
   ! enddo



  END SUBROUTINE f2f4all_compute_contact_PRPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !fd routine de detection avec la strategie de detection f2f pour face plane ou non  
  !fd nourrit les objets visavis  
  !
  SUBROUTINE DETECTION_f2f4all(id1,id2,isee,global_adist,sep,&
                              status,PT_CTC,overlap,nn,tt,ss,surfaces,v2v,iv)

!fd le 04/02/2011
! detection face a face (plan, courbe) pour objets convexes et non convexes
!

! I
! id1 : id solide 1 (antagoniste)
! id2 : id solide 2 (candidat)
! isee : index table de visibilite
! global_adist : halo de recherche
! sep : vecteur intercentre 
!
! O 
! nb_ctc             : nombre de points de contact
! nbf                : nombre de faces du corps candidat (c'est a partir de lui qu'on cherche)
! status(1:nbf)      : statut du point de contact
! PT_CTC(1:3,1:nbf)  : coordonnees des points de contact
! N(1:nbf),t(),s()   : normales aux points de contact si il y a contact (bizarre bizarrre)
! overlap(1:nbf)     : les gaps aux points de contact
! surfaces(1:nbf)    : la surface au point de contact

! v2v                : le tableau visavis
! iv                 : le dernier rang occupe

   IMPLICIT NONE

   INTEGER                                 :: id1,id2,isee,nb_ctc,nbf,nbft
   REAL(kind=8)                            :: local_adist,global_adist,sep(3)


   REAL(kind=8),DIMENSION(:),pointer       :: surfaces

   integer,dimension(:,:),pointer          :: status
   REAL(kind=8),DIMENSION(:,:),pointer     :: overlap
   REAL(kind=8),DIMENSION(:,:,:),pointer   :: PT_CTC
   REAL(kind=8),DIMENSION(:,:,:),pointer   :: tt
   REAL(kind=8),DIMENSION(:,:,:),pointer   :: nn
   REAL(kind=8),DIMENSION(:,:,:),pointer   :: ss

   integer                                 :: iv
   type(T_visavis),dimension(:)            :: v2v
   !
   TYPE(T_POLYR)                           :: PRan,PRcd

   !pour chaque face le centre et sa normale
   real(kind=8),dimension(:,:),pointer     :: centres
   real(kind=8),dimension(:,:),pointer     :: cd_normales
   real(kind=8),dimension(:,:),pointer     :: an_normales

   real(kind=8),dimension(:,:),allocatable :: weight_an
   integer,dimension(:),allocatable        :: id_ele_an

   integer                                 :: if,istat,ff,ppp,nb_ctc_f
   real(kind=8)                            :: gap,point(3),t(3),n(3),s(3),weight(3),dist1

   !fd pour degager des faces non vues
   integer,dimension(:),allocatable        :: cd_skip,an_skip
   integer,dimension(:),allocatable        :: aux_cd_skip,aux_an_skip

   integer :: i,j,fi,fj
   logical :: is_ok
   LOGICAL :: bavard=.false. !.false.
   real(kind=8) :: scal


   !fd gestion planplan
   integer :: nb_ctc_pp,nb_ctc_pp_new,ic
   logical :: flat_contact
   REAL(kind=8)                     :: n_pp(3),t_pp(3),s_pp(3),area
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC_pp
   REAL(kind=8),DIMENSION(4)        :: overlap_pp
   integer,dimension(4)             :: id_face_cd,id_face_an
   REAL(kind=8),DIMENSION(3,4)      :: weight_face_cd, weight_face_an

   real(kind=8), dimension(:,:), pointer :: face_ctc
   integer     , dimension(:)  , pointer :: face_sizes

   real(kind=8),parameter :: un_tiers=1.d0/3.d0 
   

   character(len=90) :: cout
   integer           :: err_
   
                               !123456789012345678901234
   CHARACTER(len=24)  :: IAM = 'PRPRx::detection_f2f_all'

   
   local_adist=see(isee)%alert 
   nb_ctc=0

   PRan    = S_POLYR(id1)
   PRcd    = S_POLYR(id2)

   IF (bavard) THEN
     PRINT*,'======================='
     PRINT*,'Detection entre le POLYR cd',PRcd%id,' et le POLYR an',PRan%id
     PRINT*,'sep',sep
     PRINT*,'vertex de cd:',PRcd%id 
     DO i=1,PRcd%nb_vertex
       PRINT*,PRcd%vertex(:,i)
     ENDDO
     PRINT*,'vertex de an:',PRan%id 
     DO i=1,PRan%nb_vertex
       PRINT*,PRan%vertex(:,i)+perio_shift(:)
     ENDDO
   ENDIF

!fd @@@ appartenant a des faces licites
!fd @@@ a l'initialisation aucun vertex valide

   ALLOCATE(cd_skip(PRcd%nb_vertex),an_skip(PRan%nb_vertex))
   cd_skip = 0
   an_skip = 0

   face_ctc   => null()
   face_sizes => null()

   DO i=1,PRan%nb_faces

      if (bavard) Then
        print*,'corps an - face ',i,'normale',PRan%normal(:,i)
      endif

!fd @@@ si la normale a la face n'est pas orientee sur la direction Nsep_o on jarte
!fd @@@ N.B: au premier pas c'est la ligne des centres, apres c'est la derniere direction separatrice
   
      scal=DOT_PRODUCT(PRan%normal(:,i),sep(:))

      IF (scal < -0.0001D0) THEN
        if (bavard) print*,'on exclue cette face'
        CYCLE
      ENDIF

!fd @@@ on valide les vertex
      an_skip(PRan%face(:,i)) = 1

   ENDDO

   DO i=1,PRcd%nb_faces

     if (bavard) Then
       print*,'corps cd - face ',i,'normale',PRcd%normal(:,i)
     endif

     scal=DOT_PRODUCT(PRcd%normal(:,i),sep(:))

     IF (scal > 0.0001D0) THEN
        if (bavard) print*,'on exclue cette face'
        CYCLE
     ENDIF

!fd @@@ on valide les vertex

     cd_skip(PRcd%face(:,i)) = 1

   ENDDO

!fd @@@ on verifie qu'il y a des vertex licites

   DO i=1,SIZE(cd_skip)
     IF (cd_skip(i) == 0) CYCLE
     IF (cd_skip(i) == 1) EXIT
     call faterr(IAM,'aucun cd_skip')
   ENDDO

   DO i=1,SIZE(an_skip)
     IF (an_skip(i) == 0) CYCLE
     IF (an_skip(i) == 1) EXIT
     call faterr(IAM,'aucun an_skip')
   ENDDO

   !fd a ce niveau on a 2 listes de faces qui vont bien 
   if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 
     print*,'nb cd set',size(PRcd%f2f_set)
     do fi=1,size(PRcd%f2f_set)
       print*,'set',fi,PRcd%f2f_set(fi)%G_i(:)
     enddo
     print*,'nb an set',size(PRan%f2f_set)
     do fj=1,size(PRan%f2f_set)
       print*,'set',fj,PRan%f2f_set(fj)%G_i(:)
     enddo
   endif
!
   allocate(aux_cd_skip(PRcd%nb_vertex),aux_an_skip(PRan%nb_vertex))
!
   nullify(centres)
   call get_centres_faces(PRcd%id,centres)  

   cd_normales => PRcd%normal   
   an_normales => PRan%normal   

   !fd surface vient d'au dessus
   call get_surfaces_faces(PRcd%id,surfaces)  

   !fd on cherche l'intersection de chaque pic du herisson avec le he_hdl antagoniste

   nbft = size(PRcd%f2f_set) ! nb de face topo
   nbf=PRcd%nb_faces        ! nb d ele total; totalement merdique
   nbf = max(4,nbf) 

   !print*,'nbf ',nbf,nbft
   allocate(status(nbf,nbft),overlap(nbf,nbft),pt_ctc(3,nbf,nbft),tt(3,nbf,nbft),nn(3,nbf,nbft),ss(3,nbf,nbft))
   allocate(weight_an(3,nbf),id_ele_an(nbf))

   status=0
   overlap=0.d0
   pt_ctc=0.d0
   tt=0.d0 
   nn=0.d0
   ss=0.d0  
   weight_an=0.d0
   id_ele_an = 0

   !print*,'----------------------oooo-------------------------------'
   !print*,'recherche f2f4all ' 
   !print*,'candidat   : ',PRcd%id,' antagoniste: ',PRan%id

   !fd on parcourt les faces topo candidates
   do fi=1,size(PRcd%f2f_set)

     !print*,'cd face topo',fi

     nb_ctc_f = 0
     nb_ctc_pp = 0

     flat_contact=.false.

     ! si c'est une face flat on cherche avec tous les autres flat
     if (PRcd%f2f_status(fi) == 0) then

       !fd on parcourt les faces topo antagonistes
       do fj=1,size(PRan%f2f_set)

         !print*,'an face topo',fj

         !fd si les faces topo sont planes on se simplifie la vie 
         !fd en travaillant avec le premier ele de chaque face
         if (PRan%f2f_status(fj) == 0) then  

           !print*,'recherche flat contact'

           if=PRcd%f2f_set(fi)%G_i(1)
           ff = PRan%f2f_set(fj)%G_i(1)

           dist1 = dot_product(cd_normales(:,if),an_normales(:,ff))

           if ( dist1 < -1.d0 + f2f_tol) then

             if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
               print*,' '
               print*,'>>>>>>>>>>>>>>>>>'
               print*,'PRcd ',PRcd%id,fi,' f2f ',PRan%id,fj
             endif

             is_ok=.true.
             aux_cd_skip = cd_skip
             aux_an_skip = an_skip
 
             id_face_cd=0
             id_face_an=0

             call planplan_contour(bavard,local_adist,PRcd,PRan, &
                                   aux_cd_skip,aux_an_skip,fi,fj, &
                                   nb_ctc_pp,pt_ctc_pp,overlap_pp, &
                                   n_pp,t_pp,s_pp, &
                                   id_face_cd,weight_face_cd, &
                                   id_face_an,weight_face_an, &
                                   face_ctc, face_sizes, area)

             if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
               print*,'nombre de points de contact planplan',nb_ctc_pp
               print*,'<<<<<<<<<<<<<<<<'
             endif

             ! planplan ne travaille que sur 4 points 
             if (nb_ctc_pp /= 0) then

               flat_contact=.true.

               !print*,nb_ctc_pp,' pp contact'
               !print*,overlap_pp
               !print*,pt_ctc_pp
               !print*,n_pp

               !fd gestion des faux contacts qui apparaissent quand on ne peut pas 
               !fd trouver une face en visavis dans planplan_contour
     
             
               nb_ctc_pp_new = count(id_face_cd /=0)
               if (nb_ctc_pp /= nb_ctc_pp_new) then
                  write(cout,*) 'faux contact ',nb_ctc_pp,nb_ctc_pp_new
                  call logmes(cout)
                  if (nb_ctc_pp_new > nb_ctc_pp) then
                    call faterr(IAM,'probleme dans le comptage des points de contact')
                  endif
               endif
               nb_ctc = nb_ctc+nb_ctc_pp_new

               iv = iv + 1
               allocate(v2v(iv)%iff_cd(nb_ctc_pp_new), v2v(iv)%iff_an(nb_ctc_pp_new), &
                        v2v(iv)%cd_lcoor(3,nb_ctc_pp_new),v2v(iv)%an_lcoor(3,nb_ctc_pp_new), &
                        v2v(iv)%index(nb_ctc_pp_new), v2v(iv)%pt_area(nb_ctc_pp_new))

               !print*,'detection flat - on pousse dans visavis'
               !print*,'cd/an',PRcd%id,PRan%id,iv
               !print*,'nb_ctc de plus',nb_ctc_pp_new

               v2v(iv)%cd=PRcd%Id
               v2v(iv)%an=PRan%Id
               v2v(iv)%nb_ctc=nb_ctc_pp_new
               v2v(iv)%isee = isee
               v2v(iv)%is_flat=.true.
               v2v(iv)%id_f_cd=fi
               v2v(iv)%id_f_an=fj
               v2v(iv)%index=0
               v2v(iv)%normal=n_pp
               v2v(iv)%pt_area=area/nb_ctc_pp_new
               v2v(iv)%face_ctc  => face_ctc
               v2v(iv)%face_sizes=> face_sizes
               nullify(face_ctc)
               nullify(face_sizes)
         
               !fd ici on ne peut pas adopter la meme logique qu'avec le nc
               !fd pour stocker les donnees (eg se servir du rang de l'ele candidat)
               !fd car un element cd peut contenir plusieurs points de contact !!
               !fd du coup on prend les 4 premiers elements du solide
             
               ic=0
               do j=1,nb_ctc_pp

                 if (id_face_cd(j)==0) cycle

                 ic = ic+1 
                 if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 
                   print*,'points retenus:'
                   print*,j,ic,id_face_cd(j)
                   print*,'--'
                 endif

                 status(ic,fi) = 4                
                 overlap(ic,fi) = overlap_pp(j)
                 pt_ctc(:,ic,fi) = pt_ctc_pp(:,j)
                 tt(:,ic,fi)=t_pp
                 nn(:,ic,fi)=n_pp
                 ss(:,ic,fi)=s_pp

                 v2v(iv)%iff_cd(ic)=id_face_cd(j)
                 v2v(iv)%cd_lcoor(1:3,ic)=weight_face_cd(1:3,j)
                 v2v(iv)%iff_an(ic)=id_face_an(j)  
                 v2v(iv)%an_lcoor(1:3,ic)=weight_face_an(1:3,j)       

               enddo
             endif
           endif
         endif 
         if (nb_ctc_pp /=0) exit ! on en a trouve une fj c est bon
       enddo ! boucle fj
     endif
      
     if (nb_ctc_pp /=0) cycle ! on en a trouve une fj c est bon

     !fd on parcourt les faces topo antagonistes 

     do fj=1,size(PRan%f2f_set)


       !fd et si cd est flat on ne teste que les an non flat
       if (PRcd%f2f_status(fi) == 0 .and. PRan%f2f_status(fj) == 0) cycle

       !print*,'an face topo',fj
       !print*,'recherche nc contact'

       id_ele_an=0
 
       !fd on parcourt les ele (i.e. les pt de contact) de la face cd
       do i=1,size(PRcd%f2f_set(fi)%G_i)

         ! numero de l'ele
         if=PRcd%f2f_set(fi)%G_i(i)

         !print*,'cd ele: ',if

         if (surfaces(if) == 0.d0) then
           print*,'========================================'
           print*,'candidat   : ',PRcd%Id,' antagoniste: ',PRan%Id
           print*,'face topo ',fi
           print*,'ele ',if,' trop petit'
           cycle
         endif       

         is_ok = .false.


         !TODO ameliorer cette partie car meme si 
         ! un noeud (avec sa normale) voit une face on n'est 
         ! pas sur qu'elle ne soit pas trop loin 

         do j=1,size(PRan%f2f_set(fj)%G_i)       
           ff = PRan%f2f_set(fj)%G_i(j)
           dist1 = dot_product(cd_normales(:,if),an_normales(:,ff))
           if ( dist1 < -1.d0 + f2f_tol) then 
             is_ok=.true.
             exit
           endif
         enddo
        
         ! on tag les noeuds de la face topologique  
         ! pour bien faire il faudrait tager les ele

         if (is_ok) then
           !print*,'voit un ele de ',fj  
           aux_an_skip=0
           do j=1,size(PRan%f2f_set(fj)%G_i)       
             aux_an_skip(PRan%face(1,PRan%f2f_set(fj)%G_i(j)))=1
             aux_an_skip(PRan%face(2,PRan%f2f_set(fj)%G_i(j)))=1
             aux_an_skip(PRan%face(3,PRan%f2f_set(fj)%G_i(j)))=1
           enddo

           ppp = 0
           status(if,fi) = node_HE_Hdl_proximity(PRan%HE_Hdl,centres(:,if),global_adist, &
                                                cd_normales(:,if),.true.,ppp,gap, &
                                                point,t,n,s,ff,weight,.false.,err_,good_nodes=aux_an_skip)

            if (err_ > 0) then
              write(cout,'("PRcd ",I0," PRan ",I0)') PRcd%id,PRan%id
              call logmes(cout, .true.)
              call faterr(IAM,'unexpected problem in node HE proximity')
            endif
           
             !if (status(if,fi) > 0) then
             !  print*,'statut: ',status(if,fi)
             !  print*,'ele an: ', ff
             !  if (count(PRan%f2f_set(fj)%G_i == ff) ==0) print*,'pas dans le set: cas degenere'
             !  dist1 = dot_product(cd_normales(:,if),an_normales(:,ff))
             !  if ( dist1 > -1.d0 + f2f_tol) print*,'pas bien oriente: cas degenere'
             !
             !  !print*,cd_normales(:,if)   
             !  !print*,an_normales(:,ff)
             !  print*,gap,local_adist
             !endif

           !fd cas degenere ou on a attrape un ele d'une autre face
           !fd ou les normales ne sont pas compatibles au sens de f2f
           !fd on le vire car ca n'est pas ce qu'on cherchait
           if (status(if,fi) > 0) then
             if (count(PRan%f2f_set(fj)%G_i == ff) == 0) status(if,fi) = 0
             dist1 = dot_product(cd_normales(:,if),an_normales(:,ff))
             if ( dist1 > -1.d0 + f2f_tol) status(if,fi) = 0
           endif

             !if (status(if) > 0) then
             !  print*,'cd: ',PRcd%id,' an: ',PRan%id
             !  print*,'face cd: ',if 
             !  print*,'normale: '
             !  print*,cd_normales(:,if)   
             !  print*,'face an: ', ff
             !  print*,'normale   : '
             !  print*,an_normales(:,ff)
             !  print*,'connectivite: ',PRan%face(1,ff),PRan%face(2,ff),PRan%face(3,ff)
             !  print*,'ppp: ',ppp
             !  print*,'vertex   : '
             !  print*,PRan%vertex(:,PRan%face(1,ff))
             !  print*,PRan%vertex(:,PRan%face(2,ff))
             !  print*,PRan%vertex(:,PRan%face(3,ff))
             !  print*,'poids: '
             !  print*,weight
             !  print*,'distance et rep local'
             !  print*,gap,local_adist
             !  print*,t
             !  print*,n
             !  print*,s
             !endif

           if (status(if,fi) > 0 .and. gap < local_adist) then
             !print*,'ajout nouveau contact au v2v',iv+1
             nb_ctc_f = nb_ctc_f + 1
             overlap(if,fi) = gap
             pt_ctc(:,if,fi)=point(:)
             tt(:,if,fi) = t(:)
             nn(:,if,fi) = n(:)
             ss(:,if,fi) = s(:)
             weight_an(:,if) = weight
             id_ele_an(if)= ff
           else
             ! on remet a 0 pour virer les cas avec local_adist trop grand
             status(if,fi) =0           
             overlap(if,fi) =0.d0
             pt_ctc(:,if,fi)=0.d0
             tt(:,if,fi) = 0.d0
             nn(:,if,fi) = 0.d0
             ss(:,if,fi) = 0.d0
             weight_an(:,if) = 0.d0
             id_ele_an(if)= 0
           endif       
         endif             
       enddo ! boucle i/if
       if (nb_ctc_f /=0) exit ! on en a trouve une fj c est bon
     enddo ! boucle fj

     !fd pour le cas nc remplissage de la structure visavis
     if (.not. flat_contact .and. nb_ctc_f /=0) then
       nb_ctc = nb_ctc + nb_ctc_f
       iv = iv + 1

       !print*,'v2v ',iv,' contient ',nb_ctc_f,' contacts'

       allocate(v2v(iv)%iff_cd(nb_ctc_f), v2v(iv)%iff_an(nb_ctc_f), &
                v2v(iv)%cd_lcoor(3,nb_ctc_f),v2v(iv)%an_lcoor(3,nb_ctc_f), &
                v2v(iv)%index(nb_ctc_f))

       !print*,'detection non convexe - on pousse dans visavis'
       !print*,'cd/an',PRcd%id,PRan%id,iv
       !print*,'nb_ctc de plus',nb_ctc_f



       v2v(iv)%cd=PRcd%Id
       v2v(iv)%an=PRan%Id
       v2v(iv)%nb_ctc=nb_ctc_f
       v2v(iv)%isee = isee
       v2v(iv)%is_flat=.false.
       v2v(iv)%id_f_cd=fi
       v2v(iv)%id_f_an=fj
       v2v(iv)%index=0

       j = 0
       do i=1,size(PRcd%f2f_set(fi)%G_i)   
         ! numero de l'ele
         if=PRcd%f2f_set(fi)%G_i(i)
         if (id_ele_an(if) == 0) cycle ! personne en face
         j = j + 1
         !print*,'on pousse dans v2v le contact ',j
         v2v(iv)%iff_cd(j)= if
         v2v(iv)%cd_lcoor(1:3,j)= un_tiers
         v2v(iv)%iff_an(j)=id_ele_an(if)  
         v2v(iv)%an_lcoor(1:3,j)=weight_an(1:3,if)       
       enddo
     endif
   enddo ! boucle fi

   DEALLOCATE(aux_cd_skip,aux_an_skip,cd_skip,an_skip)
   deallocate(weight_an,id_ele_an)
   deallocate(centres)

   if( associated(face_ctc)   ) deallocate(face_ctc)
   if( associated(face_sizes) ) deallocate(face_sizes)

   !print*,overlap


  end subroutine DETECTION_f2f4all
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  SUBROUTINE update_F2F4all(v2v,pt_ctc,overlap,nn,tt,ss,surfaces)
   implicit none
   type(T_visavis)                         :: v2v
   integer                                 :: nb_ctc,nbf,nbft

   REAL(kind=8),DIMENSION(:),pointer         :: surfaces

   REAL(kind=8),DIMENSION(:,:),pointer       :: overlap
   REAL(kind=8),DIMENSION(:,:,:),pointer     :: PT_CTC
   REAL(kind=8),DIMENSION(:,:,:),pointer     :: tt
   REAL(kind=8),DIMENSION(:,:,:),pointer     :: nn
   REAL(kind=8),DIMENSION(:,:,:),pointer     :: ss

   ! local
   integer                                 :: i,ic,if
   real(kind=8)                            :: pcd(3),pan(3),n(3),t(3),s(3)
   TYPE(T_POLYR)                           :: PRan,PRcd

   real(kind=8),dimension(:,:),pointer     :: cd_normales,an_normales 


   !print*,'on recupere de visavis'
   !print*,'cd/an',v2v%cd,v2v%an
   !print*,'nb_ctc',v2v%nb_ctc

   !print*,v2v%is_flat
   !print*,v2v%nb_ctc

   PRan    = S_POLYR(v2v%an)
   PRcd    = S_POLYR(v2v%cd)

   cd_normales => PRcd%normal   
   an_normales => PRan%normal   

   nbft=size(PRcd%f2f_set)
   nbf=PRcd%nb_faces
   nbf = max(4,nbf)

   allocate(overlap(nbf,nbft),pt_ctc(3,nbf,nbft),tt(3,nbf,nbft),nn(3,nbf,nbft),ss(3,nbf,nbft))
   overlap=0.d0
   pt_ctc=0.d0
   tt=0.d0 
   nn=0.d0
   ss=0.d0  

   call get_surfaces_faces(v2v%cd,surfaces)  

   nb_ctc = v2v%nb_ctc

   if (nb_ctc == 0) return

   if (v2v%is_flat) then
     n(:)= 0.5 * (PRan%normal(:,v2v%iff_an(1)) - PRcd%normal(:,v2v%iff_cd(1))) 
     !print*,'n',n
     call comp_rep(t,n,s)
   endif 

   do ic=1,nb_ctc
       
     if (v2v%is_flat) then
       if = ic
     else
       if = v2v%iff_cd(ic)
     endif

     !print*,'contact ',ic
     !print*,'element ',v2v%iff_cd(ic),v2v%iff_an(ic)
     !print*,'noeud cd ', PRcd%face(:,v2v%iff_cd(ic))
     !print*,'noeud an ', PRan%face(:,v2v%iff_an(ic))

     pcd=0.d0
     pan=0.d0
     do i=1,3     
       pcd(:) = pcd(:) + v2v%cd_lcoor(i,ic)*PRcd%vertex(:,PRcd%face(i,v2v%iff_cd(ic))) 
       pan(:) = pan(:) + v2v%an_lcoor(i,ic)*PRan%vertex(:,PRan%face(i,v2v%iff_an(ic))) 
     enddo
     pt_ctc(:,if,v2v%id_f_cd) = 0.5 * (pcd(:) + pan(:)) 

     if (v2v%is_flat) then

       nn(:,if,v2v%id_f_cd) = n
       tt(:,if,v2v%id_f_cd) = t
       ss(:,if,v2v%id_f_cd) = s
         
     else

       nn(:,if,v2v%id_f_cd) = 0.5 * (PRan%normal(:,v2v%iff_an(ic)) - PRcd%normal(:,v2v%iff_cd(ic))) 
       call comp_rep(t,nn(:,if,v2v%id_f_cd),s)
       tt(:,if,v2v%id_f_cd) = t
       ss(:,if,v2v%id_f_cd) = s

     endif

     overlap(if,v2v%id_f_cd) = dot_product(nn(:,if,v2v%id_f_cd),pcd - pan)     

   enddo

  end subroutine update_f2f4all
  !------------------------------------------------------------------------

  !fin f2f4all
  
  !debut wti
  
  !--------------------------------------------------------------------------------------------------  
  !> Contact detection with triangle intersection
  subroutine wti_compute_contact_PRPRx
    implicit none
    !
    integer                      :: errare
    integer                      :: cd_ent, an_ent, nb_perio
    integer                      :: icdan, iadj, itac, icdtac, iantac, isee
    integer                      :: i, j, nb_ctc, size_of_array_this
    real(kind=8)                 :: adist, dist, t1, t2
    real(kind=8)                 :: norme, den, scal, pt_surf
    real(kind=8)                 :: vls_cst, vln_cst, vlt_cst
    real(kind=8), dimension(3)   :: sep, t, n, s, cdlev, anlev
    real(kind=8), dimension(3)   :: ovlap
    real(kind=8), dimension(6)   :: cd_Vbegin, an_Vbegin
    real(kind=8), dimension(3,3) :: Rc, localframe_cd, localframe_an
    real(kind=8), dimension(3,3) :: xco
    !
    real(kind=8), dimension(3)   :: v1,v2,vv
    !
    logical, save                :: was_used=.false.

    character(len=80) :: cout
    !                                      12345678901234567890123456
    character(len=26), parameter :: IAM = "PRPRx::wti_compute_contact"

    icdan    = 0
    nb_PRPRx = 0
    nb_adj   = 0

    nb_detection_test = 0
    detection_time    = 0.d0

    nb_shadow    = 0
    nb_ctc_state = 0

    ! norme   = 0.d0


    if (nb_rough_PRPRx /= 0 ) then

      size_of_array_this = size(this)

      !
      ! preparation de la detection
      !

      nb_perio = 0
      do i = 1, nb_rough_PRPRx

        nb_ctc  = 0
        xco     = 0.d0
        pt_surf = 0.d0

        icdtac  = rough_PRPRx(i)%cd
        iantac  = rough_PRPRx(i)%an
        isee    = rough_PRPRx(i)%isee

        adist = see(isee)%alert

        perio_shift = 0.d0
        perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
        perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode

        if (.not. was_used .or. .not. update_cohesive_existing_interaction(icdtac,iantac,perio_shift,3,nb_ctc,xco,t,n,s,ovlap) ) then

          dist  = S_POLYR(icdtac)%radius+S_POLYR(iantac)%radius+adist

          sep   = S_POLYR(icdtac)%center - (S_POLYR(iantac)%center + perio_shift)
          norme = sep(1)*sep(1) + sep(2)*sep(2) + sep(3)*sep(3)

          if (norme <1.d-14) then
            write(cout,*) ' Distance between',icdtac, ' and ',iantac, 'is : ', norme 
            call faterr(IAM,cout)
          end if


          !fd @@@ ca a pas deja ete teste avant dans la partie rough ?
          !fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
          !fd @@@ et il en manque je pense ...

          IF (norme < dist*dist) THEN

            IF (((S_POLYR(iantac)%maxpos(1)+perio_shift(1))- &
                  S_POLYR(icdtac)%minpos(1)+adist)<0.D0) CYCLE
            IF (((S_POLYR(iantac)%maxpos(2)+perio_shift(2))- &
                  S_POLYR(icdtac)%minpos(2)+adist)<0.D0) CYCLE
            IF (( S_POLYR(iantac)%maxpos(3)                - &
                  S_POLYR(icdtac)%minpos(3)+adist)<0.D0) CYCLE

            IF ((S_POLYR(icdtac)%maxpos(1)- &
                (S_POLYR(iantac)%minpos(1)+perio_shift(1))+adist)<0.D0) CYCLE
            IF ((S_POLYR(icdtac)%maxpos(2)- &
                (S_POLYR(iantac)%minpos(2)+perio_shift(2))+adist)<0.D0) CYCLE
            IF ((S_POLYR(icdtac)%maxpos(3)- &
                 S_POLYR(iantac)%minpos(3)                +adist)<0.D0) CYCLE

            call cpu_time(t1)

            sep  = sep / sqrt(norme)
            scal = dot_product( sep(1:3), rough_PRPRx(i)%Vsep(1:3) )

            ! fd : paranoid test in case Vsep from rough detection is null
            if (scal == 0.d0) then
              rough_PRPRx(i)%Vsep = sep
            end if

            ! rm: warning rough%Vsep may change on output if there is no contact
            call DETECTION_TRIANGLES_INTERSECTION_(iantac, icdtac, nb_ctc, xco, n, ovlap, rough_PRPRx(i)%Vsep)

            call cpu_time(t2)

            nb_tot_detect     = nb_tot_detect + 1
            nb_detection_test = nb_detection_test + 1
            detection_time    = detection_time + t2-t1

            !PRINT*, 'Detect contact:', i, '/',icdbdy, ' vs ',ianbdy    !vhn
            !PRINT*, ' contact ', nb_ctc

            ! fd : seem useless
            !rough_PRPRx(i)%Vsep = sep

            if ( nb_ctc == 0 ) cycle

            ! vhn test check if we computed periodic contact
            if ( (rough_PRPRx(i)%yperiodic /= 0) .or. &
                 (rough_PRPRx(i)%xperiodic /= 0)      ) then
              nb_perio = nb_perio + 1
            end if

            if ( (n(1)*sep(1) + n(2)*sep(2) +n(3)*sep(3) ) < 0.d0 ) n = -n

            norme = sqrt( n(1)*n(1) + n(2)*n(2) + n(3)*n(3) )
            if (norme /= 0.d0) then
              n = n/norme
            else
              write(cout,*) 'null normal: ', n(1), n(2), n(3), norme
              call faterr(IAM,cout)
            end if
           
            !fd cas ou la normale est alignee sur un axe
 
            if( ( n(1)*n(2) == 0.d0 ) .and. &
                ( n(2)*n(3) == 0.d0 ) .and. &
                ( n(3)*n(1) == 0.d0 )       ) then


              if( n(1) /= 0.d0 .or. n(2) /= 0.d0 ) then
                s = (/ 0.d0, 0.d0, 1.d0 /)
                t(1) =  n(2)*s(3) - n(3)*s(2)
                t(2) =  n(3)*s(1) - n(1)*s(3)
                t(3) =  n(1)*s(2) - n(2)*s(1)
              else
                s = (/ 0.d0, 1.d0, 0.d0 /)
                t(1) =  n(2)*s(3) - n(3)*s(2)
                t(2) =  n(3)*s(1) - n(1)*s(3)
                t(3) =  n(1)*s(2) - n(2)*s(1)
              end if

            else

              if( dabs(n(1)) > 0.9d0 .or. dabs(n(2)) > 0.9d0 ) then
                s = (/ 0.d0, 0.d0, 1.d0 /)
                t(1) =  n(2)*s(3) - n(3)*s(2)
                t(2) =  n(3)*s(1) - n(1)*s(3)
                t(3) =  n(1)*s(2) - n(2)*s(1)
              else if (dabs(n(3)) > 0.9d0) then
                s = (/ 0.d0, 1.d0, 0.d0 /)
                t(1) =  n(2)*s(3) - n(3)*s(2)
                t(2) =  n(3)*s(1) - n(1)*s(3)
                t(3) =  n(1)*s(2) - n(2)*s(1)
              else
                den = 1.d0/sqrt( (n(2)*n(3))**2 &
                                +(n(1)*n(3))**2 &
                                +4.d0*(n(1)*n(2))**2)

                t(1)=       n(2)*n(3)*den
                t(2)=       n(3)*n(1)*den
                t(3)= -2.d0*n(1)*n(2)*den
              end if

             end if

            norme = sqrt( t(1)*t(1) + t(2)*t(2) + t(3)*t(3) )
            t = t/norme

            s(1) =  t(2)*n(3) - t(3)*n(2)
            s(2) =  t(3)*n(1) - t(1)*n(3)
            s(3) =  t(1)*n(2) - t(2)*n(1)

            norme = sqrt( s(1)*s(1) + s(2)*s(2) + s(3)*s(3) )
            s = s/norme

          endif
        endif
       
        if (nb_ctc > 0 ) then
           
          localframe_cd = get_inertia_frameTT_POLYR( icdtac )
          localframe_an = get_inertia_frameTT_POLYR( iantac )

          cd_Vbegin = get_vlocy_POLYR( icdtac, iVbeg_ )
          an_Vbegin = get_vlocy_POLYR( iantac, iVbeg_ )

          vlt_cst = dot_product( cd_Vbegin(1:3)-an_Vbegin(1:3), t(1:3) )
          vln_cst = dot_product( cd_Vbegin(1:3)-an_Vbegin(1:3), n(1:3) )
          vls_cst = dot_product( cd_Vbegin(1:3)-an_Vbegin(1:3), s(1:3) )

          ! calcul surface aux points de contact 

          if (nb_ctc == 1) then
            pt_surf = point_surf_PRPRx
          else if (nb_ctc == 2) then
            pt_surf = line_surf_PRPRx
          else if (nb_ctc == 3) then
            if (surf_surf_PRPRx /= 0.d0) then
                pt_surf = surf_surf_PRPRx
            else
                v1 = xco(1:3,2) - xco(1:3,1)
                v2 = xco(1:3,3) - xco(1:3,1)
                vv = cross_product(v1,v2)

                ! surface du triangle / 3
                pt_surf = length3(vv) / 6.d0
            end if
            
          else if (nb_ctc > 3) then
            call faterr(IAM,'wtf pt_surf')
          endif

          do j = 1, nb_ctc

            icdan = icdan+1

            if ( icdan>size_of_array_this ) then
              call logmes('---------------------------------------------', .true.)
              call logmes('ERROR filling this                           ', .true.)
              call logmes('you reach the allocated size                 ', .true.)
              call logmes('In your python script use                    ', .true.)
              call logmes('                                             ', .true.)
              call logmes('PRPRx_LowSizeArrayPolyr(sizefactor)          ', .true.)
              call logmes('                                             ', .true.)
              call logmes('where sizefactor is an integer specifiyng the', .true.)
              call logmes('ratio of memory you need (=4 by default)     ', .true.)
              call logmes('---------------------------------------------', .true.)

              write(cout,'(4x,I10,A25)') icdan, ' rank of wrong contact'
              call logmes(cout, .true.)

              call faterr(IAM,'Error')
            end if

            this(icdan)%icdbtac  = polyr2bdyty(2, icdtac)
            this(icdan)%ianbtac  = polyr2bdyty(2, iantac)

            this(icdan)%icdbtyp  = polyr2bdyty(3, icdtac)
            this(icdan)%ianbtyp  = polyr2bdyty(3, iantac)

            this(icdan)%icdctyp  = i_polyr
            this(icdan)%ianctyp  = i_polyr

            nb_adj(icdtac)       = nb_adj(icdtac)+1
            iadj                 = nb_adj(icdtac)
            this(icdan)%iadj     = iadj
            this(icdan)%icdbdy   = polyr2bdyty(1,icdtac)
            this(icdan)%icdtac   = icdtac
            this(icdan)%ianbdy   = polyr2bdyty(1,iantac)
            this(icdan)%iantac   = iantac
            this(icdan)%isee     = isee
            this(icdan)%tuc      = t
            this(icdan)%nuc      = n
            this(icdan)%suc      = s

            this(icdan)%coor     = xco(1:3,j)
            this(icdan)%type_ctc = nb_ctc

            cd_ent = get_ent_POLYR( icdtac )
            an_ent = get_ent_POLYR( iantac )

            this(icdan)%icdent = cd_ent
            this(icdan)%ianent = an_ent

            entity(cd_ent)%nb = entity(cd_ent)%nb + 1
            entity(an_ent)%nb = entity(an_ent)%nb + 1

            !cdlev = xco(1:3,j)-PRcoor(1:3,icdbdy)
            !anlev = xco(1:3,j)-PRcoor(1:3,ianbdy)
            !vhn add periodic condition

            cdlev = xco(1:3,j)  &
                  - (PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac))
            anlev = xco(1:3,j) &
                  - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift)

            this(icdan)%icdcoor(1) = dot_product( cdlev(1:3)+ ovlap(j)*n, localframe_cd(1:3,1) )
            this(icdan)%icdcoor(2) = dot_product( cdlev(1:3)+ ovlap(j)*n, localframe_cd(1:3,2) )
            this(icdan)%icdcoor(3) = dot_product( cdlev(1:3)+ ovlap(j)*n, localframe_cd(1:3,3) )

            this(icdan)%iancoor(1) = dot_product( anlev(1:3), localframe_an(1:3,1) )
            this(icdan)%iancoor(2) = dot_product( anlev(1:3), localframe_an(1:3,2) )
            this(icdan)%iancoor(3) = dot_product( anlev(1:3), localframe_an(1:3,3) )

            ! print*,xco(:,j)
            

            ! On va calculer le passage rep inertie -> rep global pour l'antagoniste

            Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
            Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
            Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

            Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
            Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
            Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

            Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
            Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
            Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)


            ! rm : equivalent to this(icdan)%Gant(:)= matmul(Rc(:,:),this(icdan)%tuc(:) no ?
            this(icdan)%Gant(1:3) = matmul( Rc(1:3,1:3), this(icdan)%tuc(1:3) )
            this(icdan)%Gann(1:3) = matmul( Rc(1:3,1:3), this(icdan)%nuc(1:3) )
            this(icdan)%Gans(1:3) = matmul( Rc(1:3,1:3), this(icdan)%suc(1:3) )

            this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + &
                                 Rc(1,2)*this(icdan)%suc(2) + &
                                 Rc(1,3)*this(icdan)%suc(3)
            this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + &
                                 Rc(2,2)*this(icdan)%suc(2) + &
                                 Rc(2,3)*this(icdan)%suc(3)
            this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + &
                                 Rc(3,2)*this(icdan)%suc(2) + &
                                 Rc(3,3)*this(icdan)%suc(3)

            ! On va calculer le passage rep inertie -> rep global pour le candidat

            Rc(1,1) = localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
            Rc(2,1) = localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
            Rc(3,1) = localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

            Rc(1,2) = localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
            Rc(2,2) = localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
            Rc(3,2) = localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

            Rc(1,3) = localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
            Rc(2,3) = localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
            Rc(3,3) = localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)

            this(icdan)%Gcdt(1:3) = matmul( Rc(1:3,1:3), this(icdan)%tuc(1:3) )
            this(icdan)%Gcdn(1:3) = matmul( Rc(1:3,1:3), this(icdan)%nuc(1:3) )
            this(icdan)%Gcds(1:3) = matmul( Rc(1:3,1:3), this(icdan)%suc(1:3) )

            ! Calcul des vitesses relatives

            this(icdan)%gapTTbegin = ovlap(j)

            this(icdan)%vltBEGIN = vlt_cst &
                                 + dot_product( cd_Vbegin(4:6), this(icdan)%Gcdt(1:3) ) &
                                 - dot_product( an_Vbegin(4:6), this(icdan)%Gant(1:3) )

            this(icdan)%vlnBEGIN = vln_cst &
                                 + dot_product( cd_Vbegin(4:6), this(icdan)%Gcdn(1:3) ) &
                                 - dot_product( an_Vbegin(4:6), this(icdan)%Gann(1:3) )

            this(icdan)%vlsBEGIN = vls_cst &
                                 + dot_product( cd_Vbegin(4:6), this(icdan)%Gcds(1:3) ) &
                                 - dot_product( an_Vbegin(4:6), this(icdan)%Gans(1:3) )

            this(icdan)%rls    = 0.d0
            this(icdan)%rlt    = 0.d0
            this(icdan)%rln    = 0.d0
            this(icdan)%vls    = this(icdan)%vlsBEGIN
            this(icdan)%vlt    = this(icdan)%vltBEGIN
            this(icdan)%vln    = this(icdan)%vlnBEGIN
            this(icdan)%gapTT  = this(icdan)%gapTTbegin
            this(icdan)%status = i_nknow

            this(icdan)%area   = pt_surf

            this(icdan)%icdsci = 0
            this(icdan)%iansci = 0

            this(icdan)%id_f_cd = 0
            this(icdan)%id_f_an = 0

          end do
        end if
      end do

      was_used=.true.
      
      nb_PRPRx = icdan

    end if

    !vhn
    !write(cout,'(1X,I10,A21)') nb_perio,' PRPRx get found'
    write(cout,'(1X,I10,A12)') nb_PRPRx,' PRPRx found'
    call logmes(cout)
    write(cout,*) 'Total time of detection: ', detection_time
    call logmes(cout)
    write(cout,*) 'Nb detection tests :', real(nb_detection_test,8)
    call logmes(cout)


    do icdan = 1, nb_PRPRx
       call get_behaviour_( icdan, see, tact_behav )
    end do


    do itac = 1 ,nb_POLYR

      if ( associated(adjac(itac)%icdan) )  deallocate( adjac(itac)%icdan )
      if ( nb_adj(itac) /= 0) THEN

        allocate( adjac(itac)%icdan( nb_adj(itac) ), stat=errare )
        if (errare /=0 ) THEN
          call faterr(IAM,' error in allocating adjac(icdtac)%.....')
        end if
      end if
    end do

    if ( nb_PRPRx /=0 ) then
      do icdan = 1, nb_PRPRx
        adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
      end do
    end if

    if (allocated(violation)) deallocate(violation)
    allocate(violation(nb_PRPRx), stat=errare)

  end subroutine wti_compute_contact_PRPRx
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine set_max_nb_pt_select_PRPRx(nb)
    implicit none
    integer, intent(in) :: nb

    max_nb_pt_select = nb

  end subroutine
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  !! Local detection between 2 faces
  subroutine DETECTION_TRIANGLES_INTERSECTION_(id_an, id_cd, nb_ctc, pt_ctc, Nr, overlap, Nsep)
    implicit none
    ! antagonist contactor id
    integer, intent(in)  :: id_an
    ! candidate contactor id
    integer, intent(in)  :: id_cd
    ! number of contact points found (always < 4)
    integer, intent(out) :: nb_ctc
    ! coordinates of contact points...
    real(kind=8), dimension(3,3), intent(out)   :: pt_ctc
    ! normal of local frame
    real(kind=8), dimension(3)  , intent(out)   :: Nr
    ! gap of each found contact points
    real(kind=8), dimension(3)  , intent(out)   :: overlap
    ! splitting plane (updated if no contact found)
    real(kind=8), dimension(3)  , intent(inout) :: Nsep
    !
    type(T_POLYR) :: PH_an, PH_cd
    integer       :: nb_face_cd, nb_face_an, nb_pt_select
    !integer       :: i, j, k, vert1, vert2, nb_test
    integer       :: i, j, vert1, vert2
    !real(kind=8)  :: scal, test, test1, test2, test3
    real(kind=8)  :: scal, test1, test2, test3
    real(kind=8)  :: l, dist1, dist2, div, gap, norme_face
    real(kind=8)  :: proj1   , proj2   , proj3
    real(kind=8)  :: proj_t_1, proj_t_2, proj_t_3
    real(kind=8)  :: proj_S_1, proj_S_2, proj_S_3
    !real(kind=8)  :: data_cd1, data_cd2, data_cd3
    !real(kind=8)  :: data_an1, data_an2, data_an3
    real(kind=8), dimension(3)    :: N_tests, Ncd, PT, S
    real(kind=8), dimension(3)    :: s1, s2, s3, S_1, S_2, S_3
    real(kind=8), dimension(3)    :: v1, v2
    ! initial guess of splitting plane
    real(kind=8), dimension(3)    :: sep
    real(kind=8), dimension(:,:), allocatable :: PT_SELECT
    integer     , dimension(:)  , allocatable :: face_cd, face_an
    !

    PH_an = S_POLYR(id_an)
    PH_cd = S_POLYR(id_cd)

    nb_ctc = 0
    PT_CTC = 0.d0

    dist1 = -1.d20
    dist2 =  1.d20

    sep = Nsep

    !
    ! sommet le plus loin de 1 en projection sens 1 vers 2
    !
    !vhn condition periodic
    do i = 1, PH_an%nb_vertex
      scal = dot_product( (PH_an%vertex(:,i)+perio_shift(:)), sep(:) )
      if (scal > dist1) then
        dist1 = scal
      end if
    end do

    !
    ! sommet le plus loin de 1 en projection sens 2 vers 1
    !
    do i = 1, PH_cd%nb_vertex
      scal = dot_product( PH_cd%vertex(:,i), sep(:) )
      if (scal < dist2) then
        dist2 = scal
      end if
    end do 
    ! test vhn
    if ( (dist1 - dist2) < 0.d0 ) then
      ! fd : wtf ?
      !if ( (perio_shift(1)==0) .and. (perio_shift(2)==0) ) return
      return
    end if

    ! fd : warning, gap is in fact overlap (-gap)
    gap       = dist1-dist2
    nb_shadow = nb_shadow + 1
    Nr(1:3)   = sep(1:3)

    scal       = 0.d0
    nb_face_cd = 0
    nb_face_an = 0

    ! test vhn
    !IF ((perio_shift(1) /= 0) .or. (perio_shift(2) /= 0)) THEN
    !   PRINT*, 'dist1 :', dist1, ' / Dist2 :', dist2, ' /gap :',gap
    !end if

    do i = 1, PH_an%nb_faces

         Ncd(1:3)=PH_an%normal(1:3,i)
         dist1 =  PH_an%val_support(i)
         dist2 =  1.d20

         do j = 1,PH_cd%nb_vertex

            ! rm&fd : add perio_shift managment !!!
            scal = dot_product( PH_cd%vertex(1:3,j)-perio_shift(1:3), Ncd(1:3) )
            if (scal < dist2) then
             dist2 = scal
            end if

         end do

         if ((dist1 - dist2) < 0.d0) then
             ! fd : wtf ?
             !IF ((perio_shift(1) == 0) .and. (perio_shift(2) == 0)) THEN
             Nsep = Ncd
             return
             !END IF
         end if

         ! fd : pourquoi ne pas garder le plus interpenetre
         if( (dist1-dist2) < gap ) then
             gap     = dist1-dist2
             Nr(1:3) = Ncd(1:3)
         end if

         nb_shadow = nb_shadow + 1

    end do


    do i = 1, PH_cd%nb_faces

       Ncd = PH_cd%normal(1:3,i)

       dist1 =  1.d20
       dist2 =  PH_cd%val_support(i)

       do j = 1,PH_an%nb_vertex
          ! vhn condition periodic
          scal = dot_product( PH_an%vertex(:,j)+perio_shift(:), Ncd(:) )
          if (scal < dist1) then
             dist1 = scal
          end if
       end do

       if ( (dist2 - dist1) < 0.d0 ) then
          ! fd : wtf ?
          !IF ((perio_shift(1) == 0) .and. (perio_shift(2) == 0)) THEN
             Nsep = -Ncd
             return
          !ENDIF
       end if

       ! fd : comme plus haut, pourquoi ne pas garder le plus grand recouvrement ?
       if( (dist2 - dist1) < gap ) then
          gap = dist2-dist1
          Nr(1:3) = -Ncd(1:3)
       end if
 
       nb_shadow = nb_shadow + 1

    end do



    allocate( face_an(PH_an%nb_faces) )
    allocate( face_cd(PH_cd%nb_faces) )

    do i = 1, PH_an%nb_faces

        scal = dot_product( PH_an%normal(1:3,i), Nr(1:3) )
        if (scal < 0.D0) cycle

        nb_face_an = nb_face_an + 1
        face_an(nb_face_an) = i

    end do

    do i = 1, PH_cd%nb_faces

        scal = dot_product( PH_cd%normal(1:3,i), Nr(1:3) )
        if (scal > 0.d0) cycle

        nb_face_cd = nb_face_cd + 1
        face_cd(nb_face_cd) = i

    end do

    !!
    allocate( PT_SELECT(3,max_nb_pt_select) )

    nb_pt_select = 0
    PT = 0.d0

    do i = 1, nb_face_an

       if (nb_pt_select > max_nb_pt_select-4) exit

       Ncd = PH_an%normal(1:3,face_an(i))
       ! vhn ajoute +perio_shift(:)
       s1(1:3) = PH_an%vertex(1:3,PH_an%face(1,face_an(i))) + perio_shift(1:3)
       s2(1:3) = PH_an%vertex(1:3,PH_an%face(2,face_an(i))) + perio_shift(1:3)
       s3(1:3) = PH_an%vertex(1:3,PH_an%face(3,face_an(i))) + perio_shift(1:3)

       proj_t_1 = dot_product( s1(1:3), Ncd(1:3) )
       proj_t_2 = dot_product( s2(1:3), Ncd(1:3) )
       proj_t_3 = dot_product( s3(1:3), Ncd(1:3) )

       v1 = s1-s2
       v2 = s1-s3
       N_tests(1)=v1(2)*v2(3)-v1(3)*v2(2)
       N_tests(2)=v1(3)*v2(1)-v1(1)*v2(3)
       N_tests(3)=v1(1)*v2(2)-v1(2)*v2(1)

       do j = 1, nb_face_cd

          if (nb_pt_select > max_nb_pt_select-4) exit

          S_1 = PH_cd%vertex(1:3,PH_cd%face(1,face_cd(j)))
          S_2 = PH_cd%vertex(1:3,PH_cd%face(2,face_cd(j)))
          S_3 = PH_cd%vertex(1:3,PH_cd%face(3,face_cd(j)))

          proj_S_1 = dot_product( S_1(1:3), Ncd(1:3) )
          proj_S_2 = dot_product( S_2(1:3), Ncd(1:3) )
          proj_S_3 = dot_product( S_3(1:3), Ncd(1:3) )

          ! rm : je comprend pas, mais alors pas du tout ca...
          proj1 = proj_t_1 - proj_S_1
          proj2 = proj_t_2 - proj_S_2
          proj3 = proj_t_3 - proj_S_3

          if ( proj1*proj2 < 0.d0) then
              div = proj_S_1-proj_S_2
              if ( dabs(div)>1.d-15 ) then
                l = proj2/div
                if ( l>=0.d0 .and. l<=1.D0 ) then
                    ! rm : je suppose que S est la coordonnee de
                    !      la projection de l'intersection du segment test [S_1,S_2]
                    !      sur le plan du triangle test [s1,s2,s3]
                    S = S_1*l + (1-l)*S_2
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

          if ( proj1*proj3 < 0.d0) then
              div = proj_S_1-proj_S_3
              if ( dabs(div)>1.d-15 ) then
                l = proj3/div
                if ( l>=0.d0 .and. l<=1.d0 ) then
                    S = S_1*l + (1-l)*S_3
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

          if ( proj2*proj3 < 0.d0) then
              div = proj_S_3-proj_S_2
              if ( dabs(div)>1.d-15 ) then
                l = proj2/div
                if ( l>=0.d0 .and. l<=1.D0 ) then
                    S = S_3*l + (1-l)*S_2
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

      end do
    end do

    do i = 1, nb_face_cd

       if (nb_pt_select > max_nb_pt_select-4) exit

       Ncd = PH_cd%normal(1:3,face_cd(i))

       s1 = PH_cd%vertex(1:3,PH_cd%face(1,face_cd(i)))
       s2 = PH_cd%vertex(1:3,PH_cd%face(2,face_cd(i)))
       s3 = PH_cd%vertex(1:3,PH_cd%face(3,face_cd(i)))

       proj_t_1 = dot_product( s1(1:3), Ncd(1:3) )
       proj_t_2 = dot_product( s2(1:3), Ncd(1:3) )
       proj_t_3 = dot_product( s3(1:3), Ncd(1:3) )

       v1 = s1-s2
       v2 = s1-s3
       N_tests(1) = v1(2)*v2(3) - v1(3)*v2(2)
       N_tests(2) = v1(3)*v2(1) - v1(1)*v2(3)
       N_tests(3) = v1(1)*v2(2) - v1(2)*v2(1)

       do j = 1, nb_face_an

          if (nb_pt_select > max_nb_pt_select-4) exit

          ! vhn ajoute +perio_shift(:)
          S_1 = PH_an%vertex(1:3,PH_an%face(1,face_an(j))) + perio_shift(1:3)
          S_2 = PH_an%vertex(1:3,PH_an%face(2,face_an(j))) + perio_shift(1:3)
          S_3 = PH_an%vertex(1:3,PH_an%face(3,face_an(j))) + perio_shift(1:3)

          proj_S_1 = dot_product( S_1(1:3), Ncd(1:3) )
          proj_S_2 = dot_product( S_2(1:3), Ncd(1:3) )
          proj_S_3 = dot_product( S_3(1:3), Ncd(1:3) )

          proj1 = proj_t_1-proj_S_1
          proj2 = proj_t_2-proj_S_2
          proj3 = proj_t_3-proj_S_3

          if ( proj1*proj2 < 0.d0 ) then
              div = proj_S_1-proj_S_2
              if ( dabs(div)>1.d-15 ) then
                l = proj2/div
                if ( l>=0.d0 .and. l<=1.d0 ) then
                    S = S_1*l + (1-l)*S_2
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

          if ( proj1*proj3 < 0.d0) then
              div = proj_S_1-proj_S_3
              if ( dabs(div)>1.d-15 ) then
                l = proj3/div
                if ( l>=0.d0 .and. l<=1.d0 ) then
                    S = S_1*l + (1-l)*S_3
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

          if ( proj2*proj3 < 0.d0) then
              div = proj_S_3-proj_S_2
              if ( dabs(div)>1.d-15 ) then
                l = proj2/div
                if ( l>=0.d0 .and. l<=1.D0 ) then
                    S = S_3*l + (1-l)*S_2
                    call point_in_triangle_( s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT )
                end if
              end if
          end if

       end do
    end do

    deallocate(face_cd)
    deallocate(face_an)



    if (nb_pt_select==0) then
      nb_ctc_state=nb_ctc_state+1
      deallocate(PT_SELECT)
      return
    end if


    if (nb_pt_select==1) then
      nb_ctc = 1
      PT_CTC(1:3,1) = PT_SELECT(1:3,1)
      ! fd : beurk...
      overlap = -abs(gap)
      deallocate(PT_SELECT)
      return
    end if


    if (PH_an%min_radius_face>5.D0*PH_cd%min_radius_face) then
       norme_face=PH_cd%min_radius_face*PH_cd%min_radius_face
       !print*,'CAS1'
    else if (PH_cd%min_radius_face>5.D0*PH_an%min_radius_face) then
       norme_face=PH_an%min_radius_face*PH_an%min_radius_face
       !print*,'CAS2'
    else
       norme_face  = max(PH_an%min_radius_face,PH_cd%min_radius_face)
       norme_face  = norme_face*norme_face
    end if
 
    if ( nb_pt_select==2 ) then

      test1 = dot_product( PT_SELECT(1:3,1)-PT_SELECT(1:3,2), PT_SELECT(1:3,1)-PT_SELECT(1:3,2) )
      if (test1 > norme_face) then
        nb_ctc = 2
        PT_CTC(1:3,1:2) = PT_SELECT(1:3,1:2)
        overlap = -abs(gap)
        deallocate(PT_SELECT)
        return
      else
        nb_ctc = 1
        PT_CTC(1:3,1) = 0.5d0 * ( PT_SELECT(1:3,1) + PT_SELECT(1:3,2) )
        overlap = -abs(gap)
        deallocate(PT_SELECT)
        return
      endif
    endif

    ! fd : pt est le barycentre des points de contacts
    PT = PT/real(nb_pt_select,8)

    test1 = 0.d0
    test2 = 0.d0
    test3 = 0.d0

    scal = -1.d20
    do i = 1, nb_pt_select

        norm = dot_product( PT_SELECT(1:3,i)-PT(1:3), PT_SELECT(1:3,i)-PT(1:3) )

        if ( norm > scal ) then
           scal = norm
           PT_CTC(1:3,1) = PT_SELECT(1:3,i)
           vert1 = i
        end if
    end do

    scal = -1.d20
    do i = 1, nb_pt_select

      if ( i==vert1 ) cycle

      norm = dot_product( PT_SELECT(1:3,i)-PT_CTC(1:3,1), PT_SELECT(1:3,i)-PT_CTC(1:3,1) )

      if (norm>scal) then
        scal = norm
        PT_CTC(1:3,2) = PT_SELECT(1:3,i)
        vert2 = i
      end if
    end do

    S = 0.5d0 * ( PT_CTC(1:3,1)+PT_CTC(1:3,2) )

    scal = -1.d20
    do i = 1, nb_pt_select

      if (i==vert1) cycle
      if (i==vert2) cycle

      norm = dot_product( PT_SELECT(1:3,i)-S(1), PT_SELECT(1:3,i)-S(1) )

      if ( norm > scal ) then
        scal = norm
        PT_CTC(1:3,3) = PT_SELECT(1:3,i)
      end if

    end do

    test2 = dot_product( PT_CTC(1:3,1)-PT_CTC(1:3,3), PT_CTC(1:3,1)-PT_CTC(1:3,3) )
    test1 = dot_product( PT_CTC(1:3,1)-PT_CTC(1:3,2), PT_CTC(1:3,1)-PT_CTC(1:3,2) )
    test3 = dot_product( PT_CTC(1:3,2)-PT_CTC(1:3,3), PT_CTC(1:3,2)-PT_CTC(1:3,3) )

    nb_ctc = 0

    overlap = -abs(gap)
 
    if      ( (test1>norme_face) .and. (test3>norme_face) .and. (test2>norme_face) ) then
        nb_ctc = 3
    else if ( (test1<norme_face) .and. (test3<norme_face) .and. (test2<norme_face) ) then
        nb_ctc = 1
        PT_CTC(1:3,1) = 0.33333333D0 * ( PT_CTC(1:3,1) + PT_CTC(1:3,2) + PT_CTC(1:3,3) )
    else if ( (test1>norme_face) .and. (test3>norme_face) .and. (test2<norme_face) ) then
        nb_ctc = 2
        PT_CTC(1:3,1) = 0.5d0 * ( PT_CTC(1:3,1) + PT_CTC(1:3,3) )
    else if ( (test1>norme_face) .and. (test2>norme_face) .and. (test3<norme_face) ) then
        nb_ctc = 2
        PT_CTC(1:3,2) = 0.5d0 * ( PT_CTC(1:3,2) + PT_CTC(1:3,3) )
    else if ( (test2>norme_face) .and. (test3>norme_face) .and. (test1<norme_face) ) then
        nb_ctc = 2
        PT_CTC(1:3,2) = 0.5d0 * ( PT_CTC(1:3,1) + PT_CTC(1:3,2) )
        PT_CTC(1:3,1) = PT_CTC(1:3,3)
    end if

    deallocate(PT_SELECT)

  end subroutine DETECTION_TRIANGLES_INTERSECTION_
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine point_in_triangle_(s1, s2, s3, S, N_tests, nb_pt_select, PT_SELECT, PT)
    implicit none
    !> first vertex of test triangle
    real(kind=8), dimension(3)  , intent(in)  :: s1
    !> second vertex of test triangle
    real(kind=8), dimension(3)  , intent(in)  :: s2
    !> third vertex of test triangle
    real(kind=8), dimension(3)  , intent(in)  :: s3
    !> normal to test triangle
    real(kind=8), dimension(3)  , intent(in)  :: N_tests
    !> test segment intersection point on the plane of test triangle
    real(kind=8), dimension(3)  , intent(in)  :: S
    !> number of point selected
    integer, intent(inout) :: nb_pt_select
    !> intersection points selected
    real(kind=8), dimension(:,:), intent(inout) :: PT_SELECT
    !> barycenter of the intersection points
    real(kind=8), dimension(3)  , intent(inout) :: PT
    !
    integer       :: k, nb_test
    real(kind=8)  :: test1, test2, test3, scal
    real(kind=8), dimension(3) :: v1, v2, v3, pv1  , pv2  , pv3
    logical       :: test

    test = .false.

    v1 = s1 - S
    v2 = s2 - S
    v3 = s3 - S

    pv1(1) = v1(2)*v2(3) - v1(3)*v2(2)
    pv1(2) = v1(3)*v2(1) - v1(1)*v2(3)
    pv1(3) = v1(1)*v2(2) - v1(2)*v2(1)
    test1 = dot_product( pv1(1:3), N_tests(1:3) )

    pv2(1) = v2(2)*v3(3) - v2(3)*v3(2)
    pv2(2) = v2(3)*v3(1) - v2(1)*v3(3)
    pv2(3) = v2(1)*v3(2) - v2(2)*v3(1)
    test2 = dot_product( pv2(1:3), N_tests(1:3) )

    pv3(1) = v3(2)*v1(3) - v3(3)*v1(2)
    pv3(2) = v3(3)*v1(1) - v3(1)*v1(3)
    pv3(3) = v3(1)*v1(2) - v3(2)*v1(1)
    test3 = dot_product( pv3(1:3), N_tests(1:3) )

    if ( test1 >= 0.d0 .and. test2 >= 0.d0 .and. test3 >= 0.d0 ) then
      test = .true.
    else if ( test1 <  0.d0 .and. test2 <  0.d0 .and. test3 <  0.d0 ) then
      test = .true.
    end if

    if ( .not. test ) return

    nb_test = 0

    ! check if new point is too close of an already selected point
    do k = 1, nb_pt_select
       scal = dot_product( PT_SELECT(1:3,k)-S(1:3), PT_SELECT(1:3,k)-S(1:3) )
       if ( scal < 1.d-15) exit
       nb_test = nb_test + 1
    end do

    ! if new point is valid, add it
    if ( nb_test==nb_pt_select ) then
       nb_pt_select = nb_pt_select+1
       PT_SELECT(1:3,nb_pt_select) = S(1:3)
       PT = PT+S
    end if

  end subroutine point_in_triangle_
  !--------------------------------------------------------------------------------------------------  

  ! fin WTI

  
!------------------------------------------------------------------------
! ---- routines utilitaires pour la detection
! -----------------------------------------------------------------------

  !------------------------------------------------------------------------  
  !> routine de test du shadow overlap avec les directions:
  !>  - valeur arbitraire (qui oriente la recherche)
  !>  - normales aux facettes (qui sont alignees (a +/- 90 deg) avec la direction de recherche initiale)
  !> necessite le pre-calcul de la fonction "support"
  !> 0: no overlap, 1: overlap

  integer function compute_shadow_overlap(PRcd,PRan,an_shift,adist,sep,gap, &
                                cd_vertex,an_vertex,cd_face,an_face,bavard)
  implicit none
  !> les 2 polyedres  
  TYPE(T_POLYR)        :: PRan,PRcd
  !> vecteur de decalage du corps an (utile pour condition periodique)
  real(kind=8)         :: an_shift(3)
  !> direction initiale du shadow overlap
  real(kind=8)         :: sep(3)
  !> distance d'alerte
  real(kind=8)         :: adist
  !> gap entre objet (valeur grossiere)
  real(kind=8)         :: gap
  !> les vertex et faces concernees 
  INTEGER,DIMENSION(:) :: cd_vertex,an_vertex,cd_face,an_face
  !> niveau de bavardage
  logical              :: bavard

  !
  integer         :: i,j
  real(kind=8)    :: dist1,dist2,ini_sep(3),scal,norm

  real(kind=8)         :: gap_cd,gap_an
  integer              :: e_cd,e_an
  ! tolerance sur le dot_product des normales
  ! ceci correspond a 90 + 0.5 degres
  real(kind=8)    :: tol_normales=1.d-3

  compute_shadow_overlap = 0

  !fd pour info sep est oriente de an -> cd
  ini_sep = sep

  IF (bavard) THEN
    PRINT*,'vertex de an:'
    DO i=1,PRan%nb_vertex
      PRINT*,PRan%vertex(:,i)+an_shift(:)
    ENDDO
    PRINT*,'vertex de cd:' 
    DO i=1,PRcd%nb_vertex
      PRINT*,PRcd%vertex(:,i)
    ENDDO
  ENDIF

  dist1 = -1.D+20
  DO i=1,PRan%nb_vertex
    scal= DOT_PRODUCT((PRan%vertex(:,i)+an_shift(:)),ini_sep(:))
    IF (scal > dist1) THEN
      dist1=scal
    ENDIF
  ENDDO

  dist2 =  1.D+20
  DO i=1,PRcd%nb_vertex
    scal= DOT_PRODUCT(PRcd%vertex(:,i),ini_sep(:))
    IF (scal < dist2) THEN
      dist2=scal
    ENDIF
  ENDDO 

  gap = dist2 - dist1

  IF (gap > adist) RETURN

  !print*,'inter centre', gap

  gap_an=gap
  e_an=0

  DO i=1,PRan%nb_faces

    if (bavard) Then
      print*,'corps an - face ',i,'normale',PRan%normal(:,i)
    endif

    ! si la normale a la face n'est pas orientee sur la direction ini_sep on jarte
    IF (DOT_PRODUCT(PRan%normal(:,i),ini_sep(:)) < -tol_normales) THEN
      if (bavard) print*,'on exclue cette face'
      CYCLE
    ENDIF

    dist1 =  PRan%val_support(i)
    dist2 =  1.D+20
    DO j=1,PRcd%nb_vertex  

      !fd new le 18/04/09
      ! comme la fonction support de l'an est calculee sans le shift
      ! et que c'est trop chiant a modifier, on decale le cd 

      scal=DOT_PRODUCT((PRcd%vertex(:,j) - an_shift(:)),PRan%normal(:,i))

      IF (scal < dist2) THEN
        dist2=scal
      ENDIF
    ENDDO
 
    ! print*,'Critère = ',dist1 - dist2

    IF ((dist2 - dist1) > adist) THEN

      IF (bavard) THEN
        PRINT*,'Ca ne passe pas le shadow overlap PRan face ',i,'avec les vertex du PRcd'
        PRINT*,'dist1= ',dist1,' dist2= ',dist2,' adist = ',adist
      ENDIF

      !fd @@@ on sort de la routine car pas contact 
      !fd @@@ la normale au plan separateur est Ncd

      sep=PRan%normal(1:3,i)
      RETURN

    ENDIF   

    !fd @@@ on valide la face et ses vertex

    an_face(i)=1
    DO j=1,3
      an_vertex(PRan%face(j,i)) = 1
    ENDDO 

    !fd comme pour la methode cundall on doit garder la plus grande valeur comme gap 
    !!fd @@@ on garde la plus petite valeur
    !IF ( (dist2 - dist1) < gap ) then
    IF ( (dist2 - dist1) > gap_an ) then
      gap_an=dist2-dist1
      e_an = i 
      !sep=PRan%normal(1:3,i)

      !print*,'an loop',i
      !print*,'an dist',dist2
      !print*,'cd dist',dist1
      !print*,'gap',gap
    ENDIF

  ENDDO

  gap_cd = gap
  e_cd=0

  DO i=1,PRcd%nb_faces

    if (bavard) Then
      print*,'corps cd - face ',i,'normale',PRcd%normal(:,i)
    endif

    ! si la normale a la face n'est pas orientee sur la direction ini_sep on jarte
    IF (DOT_PRODUCT(PRcd%normal(:,i),ini_sep(:)) > tol_normales) THEN
      if (bavard) print*,'on exclue cette face'
      CYCLE
    ENDIF

    dist1 =  1.D+20
    dist2 =  PRcd%val_support(i)

    DO j=1,PRan%nb_vertex
      scal= DOT_PRODUCT((PRan%vertex(:,j)+an_shift(:)),PRcd%normal(1:3,i))
      IF (scal < dist1) THEN
        dist1=scal
      ENDIF
    ENDDO

    IF ( (dist1 - dist2) > adist) THEN
      IF (bavard) THEN
        PRINT*,'Ca ne passe pas le shadow overlap PRcd face ',i,'avec les vertex du PRan'
        PRINT*,'dist1= ',dist2,' dist2= ',dist1,' adist = ',adist
      ENDIF

      !fd on sort de la routine car pas contact 
      !fd on garde la normal car elle permet de separer les corps ...

      sep=-PRcd%normal(1:3,i)

      RETURN
    ENDIF   

    !fd comme pour la methode cundall on garde la plus grande valeur comme gap 
    !IF ( (dist1 - dist2) < gap) then
    IF ( (dist1 - dist2) > gap_cd) then
      gap_cd = dist1 - dist2
      e_cd = i
      !sep = -PRcd%normal(1:3,i)
 
      !print*,'cd loop',i
      !print*,'cd dist',dist2
      !print*,'an dist',dist1
      !print*,'gap',gap
    endif

    cd_face(i)=1
    DO j=1,3
      cd_vertex(PRcd%face(j,i)) = 1
    ENDDO 
  ENDDO

  DO i=1,SIZE(cd_vertex)
    IF (cd_vertex(i) == 0) CYCLE
    IF (cd_vertex(i) == 1) EXIT
    call faterr('mod_PRPRx::compute_shadow_overlap','aucun cd_vertex')
  ENDDO

  DO i=1,SIZE(an_vertex)
    IF (an_vertex(i) == 0) CYCLE
    IF (an_vertex(i) == 1) EXIT
    call faterr('mod_PRPRx::compute_shadow_overlap','aucun an_vertex')
  ENDDO

  IF (bavard) THEN
    PRINT*,'xxxxxxx'
    PRINT*,'Ca passe le shadow overlap'
    PRINT*,'la distance la plus defavorable= ',gap
    PRINT*,'la normale la plus defavorable =',sep

    DO i=1,SIZE(cd_vertex)
      IF (cd_vertex(i) == 0) PRINT*,'corps cd - le vertex ',i,' est invisible par la suite'
    ENDDO

    DO i=1,SIZE(an_vertex)
      IF (an_vertex(i) == 0) PRINT*,'corps an - le vertex ',i,' est invisible par la suite'
    ENDDO
 
  ENDIF

  if (e_cd /= 0 .and. e_an /= 0) then
    sep = (PRan%normal(:,e_an) - PRcd%normal(:,e_cd))*0.5
    norm=length3(sep)
    sep = sep / norm
    gap = (gap_cd + gap_an) * 0.5
  else if ( e_cd /= 0 .and. e_an == 0) then
    sep = - PRcd%normal(:,e_cd)
    gap = gap_cd
  else if (e_cd == 0 .and. e_an /= 0) then
    sep = PRan%normal(:,e_an)
    gap = gap_an
  else
    sep=ini_sep
    ! gap = gap
  endif
  compute_shadow_overlap=1

  end function
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  integer function compute_visibility(PRcd,PRan,an_shift,ini_sep, &
                                    cd_vertex,an_vertex,cd_face,an_face,bavard)
  implicit none
  !> les 2 polyedres  
  TYPE(T_POLYR)        :: PRan,PRcd
  !> vecteur de decalage du corps an (utile pour condition periodique)
  real(kind=8)         :: an_shift(3)
  !> direction initiale du shadow overlap
  real(kind=8)         :: ini_sep(3)
  !> les vertex et faces concernees 
  INTEGER,DIMENSION(:) :: cd_vertex,an_vertex,cd_face,an_face
  !> niveau de bavardage
  logical              :: bavard

  !
  integer         :: i,j

  ! tolerance sur le dot_product des normales
  real(kind=8)    :: tol_normales=1.d-3
  !                          !12345678901234567890123456789
  character(len=29) :: IAM = 'mod_PRPRx::compute_visibility'

  compute_visibility = 0

  IF (bavard) THEN
    PRINT*,'vertex de an:'
    DO i=1,PRan%nb_vertex
      PRINT*,PRan%vertex(:,i)+an_shift(:)
    ENDDO
    PRINT*,'vertex de cd:' 
    DO i=1,PRcd%nb_vertex
      PRINT*,PRcd%vertex(:,i)
    ENDDO
  ENDIF

  DO i=1,PRan%nb_faces

    if (bavard) Then
      print*,'corps an - face ',i,'normale',PRan%normal(:,i)
    endif

    !fd @@@ si la normale a la face n'est pas orientee sur la direction ini_sep on jarte
    IF (DOT_PRODUCT(PRan%normal(:,i),ini_sep(:)) < -tol_normales) THEN
      if (bavard) print*,'on exclue cette face'
      CYCLE
    ENDIF

    !fd @@@ on valide la face et ses vertex

    an_face(i)=1
    DO j=1,3
      an_vertex(PRan%face(j,i)) = 1
    ENDDO 

  ENDDO

  DO i=1,PRcd%nb_faces

    if (bavard) Then
      print*,'corps cd - face ',i,'normale',PRcd%normal(:,i)
    endif

    IF (DOT_PRODUCT(PRcd%normal(:,i),ini_sep(:)) > tol_normales) THEN
      if (bavard) print*,'on exclue cette face'
      CYCLE
    ENDIF

    cd_face(i)=1
    DO j=1,3
      cd_vertex(PRcd%face(j,i)) = 1
    ENDDO 
  ENDDO

  DO i=1,SIZE(cd_vertex)
    IF (cd_vertex(i) == 0) CYCLE
    IF (cd_vertex(i) == 1) EXIT
    call faterr(IAM,'aucun cd_vertex')
  ENDDO

  DO i=1,SIZE(an_vertex)
    IF (an_vertex(i) == 0) CYCLE
    IF (an_vertex(i) == 1) EXIT
    call faterr(IAM,'aucun an_vertex')
  ENDDO

  compute_visibility=1

  end function
  !-----------------------------------------------------------------------------

  !-----------------------------------------------------------------------------
  !> iterative method to compute the commmon plane of 2 polyhedra
  !> (as proposed by P. Cundall)
  !> inputs : a trial for the plane (defined by a normal and a point) and
  !> a list of seen vertices
  !> outputs : a plane (defined by a normal and a point) and
  !> an updated list of active vertices
  integer function compute_cundall_common_plane(PRcd,PRan,an_shift, &
                                              center,t,normal,s,gap, &
                                              jmin,imax, &
                                              cd_vertex,an_vertex, &
                                              m,bavard)
  implicit none
  !> les 2 polyedres  
  TYPE(T_POLYR)        :: PRan,PRcd
  !> vecteur de decalage du corps an (utile pour condition periodique)
  real(kind=8)         :: an_shift(3)
  !> position et orientation du CP
  real(kind=8)         :: center(3),normal(3)
  !> gap entre objet (valeur grossiere)
  real(kind=8)         :: gap
  !> les vertex concernes
  INTEGER,DIMENSION(:) :: cd_vertex,an_vertex
  !> niveau de bavardage
  logical              :: bavard
  !>
  integer              :: m
  !>
  integer              :: jmin,imax

  ! le nombre de recherche par iteration
  integer,parameter :: nb_rot = 8 

  !
  REAL(kind=8) :: tol,angle,angle_min,angle_max,co,si,d_cd,d_an
  real(kind=8) :: ref_size

  real(kind=8) :: p1(3),t(3),s(3),n_ini(3),vec(3)
  real(kind=8) :: n_ite(3,nb_rot),t_ite(3,nb_rot),all_d_min(nb_rot),all_d_max(nb_rot)
  integer      :: all_jmin(nb_rot),all_imax(nb_rot)
  !
  real(kind=8) :: d_min,d_max,norm,G_ini,gap_ini,crit,& 
                  stored_angle(cundall_iter), &
                  stored_gap(cundall_iter), &
                  stored_crit(cundall_iter)
  
  integer :: j,i,k,kk,irot,dump_fich

  character(len=11) :: nom

  !fd pour comprendre ce qui pourrait deconner dans la detection
  real(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
  real(kind=8)::rd

  real(kind=8) :: norm1,norm2,tol_proj,d1,d2

  integer :: jj,ii

  !fd gestion de la rotation dans le plan tangent
  real(kind=8) :: co_rot,si_rot,angle_rot

                                        !12345678901234567890123456789012345
  character(len=35), parameter :: IAM = 'PRPRx::compute_cundall_common_plane'
  character(len=120)           :: cout

  ! pour calculer les perturbations autour de la normale (nb_rot est un parameter) 
  angle_rot = 2.d0*PI_g/real(nb_rot)
  si_rot = sin(angle_rot)
  co_rot = cos(angle_rot)

  compute_cundall_common_plane=0

  ! parametre de la methode de cundall
  tol=cundall_gap
  angle_max=cundall_theta_max
  angle_min=cundall_theta_min

  ref_size = get_min_radius_POLYR()  

  n_ini=normal
  g_ini=gap

  ! -- initialisation

  ! recherche du sommet cd le plus proche par projection suivant la normale moyenne 

  d_min=1.d+20
  jmin=0

  DO j=1,PRcd%nb_vertex 
    IF (cd_vertex(j) == 0) CYCLE

    vec(:)=PRcd%vertex(:,j)-center(:)

    d_cd=DOT_PRODUCT(normal(:),vec(:))

    IF ( d_min > d_cd) THEN
      d_min=d_cd
      jmin=j
    ENDIF
  ENDDO

  !print*,'d_min',d_min

  ! recherche du sommet an le plus loin par projection suivant -1*la normale moyenne 

  d_max=-1.D+20
  imax=0

  DO i=1,PRan%nb_vertex 
    IF (an_vertex(i) == 0) CYCLE
    vec(:)=(PRan%vertex(:,i)+an_shift(:))-center(:)

    d_an=DOT_PRODUCT(normal(:),vec(:))

    IF ( d_max < d_an ) THEN
      d_max=d_an
      imax=i
    ENDIF
  ENDDO

  if (imax==0 .or. jmin==0) then
    call faterr(IAM,'no nodes of the POLYR are seen check the contact detection')
  endif

  
  !print*,'d_max',d_max

  !fd on essaie de construire un repere intelligent ou 
  !fd t passe sur l'axe le plus defavorable

  IF (bavard) then
    print*,'--- tentative de prediction intelligente de t ---'
    print '(A10,I5,3(1x,D14.7))','vertex an: ',imax,PRan%vertex(:,imax)+an_shift(:)
    print '(A10,I5,3(1x,D14.7))','vertex cd: ',jmin,PRcd%vertex(:,jmin)
    print '(A10,3(1x,D14.7))','normale: ',normal(:)
  endif

  p1(:)=(PRan%vertex(:,imax)+an_shift(:))-PRcd%vertex(:,jmin)
  t(:)=p1(:)-(dot_product(p1(:),normal(:)))*normal(:)

  norm=SQRT(dot_product(t,t))

  IF (bavard) then
    print '(A20,4(1x,D14.7))','t predit: ',t,norm
    print *,'min_radius_POLYR',ref_size  
  endif

  If (norm < 1d-6*ref_size) then

     !fd cas degenere ou le t qu'on a construit est aligne avec la normale

     CALL comp_rep(t,normal,s)

     IF (bavard) then 
        print *,'on ne garde pas t predit'
        print '(A20,3(1x,D14.7))','on prend: ',t
     endif

  else

    t=t/norm        

    IF (bavard) then 
       print *,'on garde t predit'
       print '(A20,3(1x,D14.7))','une fois norme: ',t
    ENDIF

    s = cross_product(t,normal)
  endif

  ! print*,t
  ! print*,normal
  ! print*,s

  ! initialisation
  gap=d_min-d_max
  angle = angle_max
  irot = 0

  ! pour tracer  
  stored_gap = 0.d0
  stored_angle = 0.d0
  stored_crit=0.d0

  ! boucle de minimisation du gap maxi
  DO m=1,cundall_iter

    ! construction des nb_rot perturbations a la Cundall  
    co= COS(angle)
    si= SIN(angle)

    ! on calcule les nb_rot perturbations du repere local

    do irot=1,nb_rot

      t = (co_rot*t)+(si_rot*s) 
      norm=length3(t)
      t = t / norm
      t_ite(:,irot)=t(:)
      s = cross_product(t,normal)
      n_ite(:,irot)= co*normal(:) + si*t(:)
      norm=length3(n_ite(:,irot))
      n_ite(:,irot)=n_ite(:,irot)/norm

    enddo

    ! on recherche la perturbation qui maximise le gap

    all_jmin = 0
    all_imax = 0

    !d_min recherche du sommet le plus proche par projection suivant la normale moyenne 
    all_d_min=1.d+20
    DO j=1,PRcd%nb_vertex 
      IF (cd_vertex(j) == 0) CYCLE
      vec(:)=PRcd%vertex(:,j)-center(:)

      do k=1,nb_rot
        d_cd=DOT_PRODUCT(n_ite(:,k),vec(:))

        IF ( all_d_min(k) > d_cd ) THEN
          all_d_min(k)=d_cd
          all_jmin(k)=j
        ENDIF
      enddo
    ENDDO

    !d_max recherche du sommet le plus proche par projection suivant -1*la normale moyenne 
    all_d_max=-1.d+20
    DO i=1,PRan%nb_vertex 
      IF (an_vertex(i) == 0) CYCLE
      vec(:)=(PRan%vertex(:,i)+an_shift(:))-center(:)

      do k=1,nb_rot
        d_an=DOT_PRODUCT(n_ite(:,k),vec(:))

        IF ( all_d_max(k) < d_an ) THEN
          all_d_max(k)=d_an
          all_imax(k)=i
        ENDIF
      enddo
    ENDDO

    ! Test entre Gap_ini et Gap_max perturbe

    ! on cherche la direction qui maximise le Gap

    gap_ini = gap

    !print*,'=================='
    !print*,'iteration',m
    !print*,'gap_ini ',gap_ini

    kk=-1
    DO k=1,nb_rot
      !print*,'irot ',k,'gap ', all_d_min(k)-all_d_max(k)
      IF (gap < all_d_min(k)-all_d_max(k) ) THEN     
        gap = all_d_min(k)-all_d_max(k)
        kk = k
        jmin=all_jmin(k)
        imax=all_imax(k)
      ENDIF
    ENDDO

    crit=1.d+20
    if (kk > 0) crit = (gap - gap_ini)/ref_size

    !
    IF (bavard) THEN
      IF (kk > 0 ) THEN
        write(cout,*) '['//IAM//'] angle ',angle
        call logmes(cout, .true.)
        write(cout,*) '['//IAM//'] la perturbation d''orientation',kk
        call logmes(cout, .true.)
        write(cout,*) '['//IAM//'] augmente le gap qui vaut      ',gap
        call logmes(cout, .true.)
        write(cout,*) '['//IAM//'] avec variation sur le gap     ',crit
        call logmes(cout, .true.)
      ELSE
        write(cout,*) '['//IAM//'] angle ',angle
        call logmes(cout, .true.)
        write(cout,*) '['//IAM//'] n''arrange rien'
        call logmes(cout, .true.)
      ENDIF
    ENDIF 

    !
    ! pour comprendre si ca ne marche pas
    stored_gap(m) = gap
    stored_angle(m)=angle
    stored_crit(m)=crit

    ! test d'arret pour la minimisation de variation du gap 
    ! avec cet angle de perturbation de la normale
    IF (crit < tol) kk = 0

    ! on n'a pas converge et pour l angle donne on maximise encore le gap
    IF ( kk > 0 ) THEN

      ! on a trouve une direction qui fait augmenter le gap 
      ! mais la variation plus grande que tol

      ! on actualise le plan tangent suivant la valeur de n

      normal(:)=n_ite(:,kk)
      t(:) = t_ite(:,kk)
      s = cross_product(t,normal)

      IF (bavard) THEN
        PRINT*,' '
        PRINT*,'on conserve le repere local'
        PRINT*,'t',t
        PRINT*,'n',normal
        PRINT*,'s',s
      ENDIF

    ELSE

      ! on a tout essaye pour cet angle mais ca ne change plus
      ! on diminue l angle

      IF (bavard) THEN
        PRINT*,' '
        PRINT*,'on reduit l''angle'
      ENDIF

      IF (angle >= angle_min) THEN

        angle  = angle*0.5d0

      ELSE 

        ! on a trouve la direction
        EXIT

      ENDIF
    ENDIF
  ENDDO

  if (bavard) then
    PRINT*,m,cundall_iter
    if (m > cundall_iter) then

      print*,"==================================="
      print*,cd_vertex
      print*,an_vertex 
      print*,n_ini
      print*,normal
      print*,g_ini,gap
      dump_fich = get_io_unit()
      print*,'write gap evolution history in dump file',dump_fich
      nom =' '
      write(nom,'(I0,"_",I0)') PRcd%id,PRan%id
      open(dump_fich,file='pbdetect_'//trim(nom)//'.txt')
      do i=1,m
        write(dump_fich,'(1x,I5,3(1x,D14.7))') i,stored_gap(i),stored_crit(i),stored_angle(i)
      enddo
      close(dump_fich)
      print*,"==================================="
    endif
  endif

  if (m<= cundall_iter) compute_cundall_common_plane = 1

  ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


  norm=length3(normal)
  normal(:)=normal(:)/norm

  ! calcul repere local
  call comp_rep(t,normal,s)

  !print*,'normal',normal
  !print*, center

  ! Translation du point moyen
  ! @@@ Critere de contact

  vec(:)=PRcd%vertex(:,jmin)-center(:)
  d_min=DOT_PRODUCT(normal(:),vec(:))

  vec(:)=(PRan%vertex(:,imax)+an_shift(:))-center(:)
  d_max=DOT_PRODUCT(normal(:),vec(:))

  gap = d_min - d_max

  center(:)=((PRan%vertex(:,imax)+an_shift(:))+PRcd%vertex(:,jmin))*0.5d0

  !fd gmv is dead ; RIP 
  ! see obsolete directory to find
  ! back the bit of code drawing gmv file here

  if (bavard .or. &
      (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then

    write(cout,*) 'steps of maximisation of the minimum process (it, angle, gap, norm) :'
    call logmes(cout, .true.)
    do i=1,m
      write(cout,*) i,stored_angle(i),stored_gap(i),stored_crit(i)
      call logmes(cout, .true.)
    enddo
     
    write(cout,*) 'cd vertex min',jmin,' an vertex max ',imax
    call logmes(cout, .true.)
    write(cout,*) 'cd distance min ',d_min,' an distance max ',d_max
    call logmes(cout, .true.)
    write(cout,*) 'g = dmin-dmax ',gap
    call logmes(cout, .true.)
    write(cout,*) 'normal ',normal
    call logmes(cout, .true.)
    write(cout,*) 'point ',center
    call logmes(cout, .true.)
  endif

  ! on ne garde que les noeuds dans la bande [-tol_proj,+tol_proj] autour du CP

  tol_proj=cundall_neighbor

  !on calcule une dimension de reference
  vec(:) = (PRan%vertex(:,imax)+an_shift(:))-PRan%center(:)
  !norm1 = length3(vec)
  !norm1 = dabs(dot_product(normal,vec))
  norm1 = dot_product(normal,vec)
  
  vec(:) = PRcd%vertex(:,jmin)-PRcd%center(:)
  !norm2 = length3(vec)
  !norm2 = dabs(dot_product(normal,vec))
  norm2 = dot_product(normal,vec)
  
  ref_size=MIN(dabs(norm1),dabs(norm2))

  d1= MAX(0.d0,gap*0.5d0 ) + tol_proj*ref_size

  if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
     write(cout,*) 'cdmin - center . n',dot_product(PRcd%vertex(:,jmin)-center(:),normal)
     call logmes(cout, .true.)
     write(cout,*) 'anmax - center . n',dot_product(PRan%vertex(:,imax)+an_shift(:)-center(:),normal)
     call logmes(cout, .true.)
     write(cout,*) 'neighbor distance',d1
     call logmes(cout, .true.)
  endif
 
  ! on tag le plus proche
  cd_vertex(jmin) = 2

  ! recherche de ceux dans la bande neighbor 
  d_min=1.d20
  !jj=0
  DO j=1,PRcd%nb_vertex 
    if (cd_vertex(j) == 0) cycle
    if (j == jmin) cycle 
     
    vec(:)=(PRcd%vertex(:,j)) - center(:)
    d2=DOT_PRODUCT(vec(:),normal(:))

    if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
      write(cout,*) 'cd node ',j,' distance to cp ',d2
      call logmes(cout, .true.)
    endif

    IF (d2 > d1) cd_vertex(j) = -cd_vertex(j)
    
    !fd rustine pour les cas a 1 noeud 
    ! if (d2 < d_min) then
    !   jj=j
    !   d_min=d2
    ! endif  
  enddo

  ! on tag le plus proche
  an_vertex(imax) = 2

  ! recherche de ceux dans la bande neighbor
  d1 = -d1
  d_max=-1.d20
  !ii=0
  DO i=1,PRan%nb_vertex 
    if (an_vertex(i) == 0) cycle 
    if (i == imax) cycle
    
    vec(:)=(PRan%vertex(:,i)+an_shift(:)) - center(:)
    d2=DOT_PRODUCT(vec(:),normal(:))

    if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
       write(cout,*) 'an node ',i,' distance to cp ',d2
       call logmes(cout, .true.)
    endif

    IF (d2 < d1) an_vertex(i) = -an_vertex(i)

    !fd rustine pour les cas a 1 noeud 
    ! if (d2 > d_max) then
    !   ii=i
    !   d_max=d2
    ! endif  
  enddo

  ! ! rustine qui degage le cas vertex-(edge|face) pour edge-(edge|face)
  
  ! j=count(cd_vertex > 0)
  ! i=count(an_vertex > 0)

  ! if (j==1 .and. i>1) cd_vertex(jj) = 1
  ! if (i==1 .and. j>1) an_vertex(ii) = 1
  
  end function compute_cundall_common_plane
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  integer function compute_cundall_common_plane_old(PRcd,PRan,an_shift,adist, &
                                              center,t,normal,s,gap, &
                                              jmin,imax, &
                                              cd_vertex,an_vertex, &
                                              m,bavard)
  implicit none
  !> les 2 polyedres  
  TYPE(T_POLYR)        :: PRan,PRcd
  !> vecteur de decalage du corps an (utile pour condition periodique)
  real(kind=8)         :: an_shift(3)
  !> position et orientation du CP
  real(kind=8)         :: center(3),normal(3)
  !> distance d'alerte
  real(kind=8)         :: adist
  !> gap entre objet (valeur grossiere)
  real(kind=8)         :: gap
  !> les vertex concernes
  INTEGER,DIMENSION(:) :: cd_vertex,an_vertex
  !> niveau de bavardage
  logical              :: bavard
  !>
  integer              :: m
  !>
  integer              :: jmin,imax

  !
  REAL(kind=8) :: tol,angle,angle_min,angle_max,co,si,d_cd,d_an
  real(kind=8) :: ref_size
  !
  real(kind=8) :: p1(3),t(3),s(3),n_ini(3),vec(3),n_ite(3,4)
  !
  real(kind=8) :: d_min,d_max,norm,G_ini,gap_ini,all_gap(4),crit,& 
                  stored_angle(cundall_iter),stored_gap(cundall_iter)
  ! 
  integer      :: j,i,all_jmin(4),all_imax(4),k,kk,irot,dump_fich

  character(len=11) :: nom

  !fd pour comprendre ce qui pourrait deconner dans la detection
  real(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
  real(kind=8)::rd

  real(kind=8) :: norm1,norm2,tol_proj,d1,d2

  !fd gestion de la rotation dans le plan  
  integer :: nb_rot=2
  real(kind=8) :: co_rot,si_rot

  si_rot = sin(PI_g*0.5d0/real(nb_rot))
  co_rot = cos(PI_g*0.5d0/real(nb_rot))

  compute_cundall_common_plane_old=0

  ! parametre de la methode de cundall
  tol=cundall_gap
  angle_max=cundall_theta_max
  angle_min=cundall_theta_min

  ref_size = get_min_radius_POLYR()  

  n_ini=normal
  g_ini=gap

  ! Initialisation

  !d_cd recherche du sommet le plus proche par projection suivant la normale moyenne 

  d_min=1.D+20
  jmin=0

  DO j=1,PRcd%nb_vertex 
    IF (cd_vertex(j) == 0) CYCLE

    vec(:)=PRcd%vertex(:,j)-center(:)

    d_cd=DOT_PRODUCT(normal(:),vec(:))

    IF ( d_min > d_cd) THEN
      d_min=d_cd
      jmin=j
    ENDIF
  ENDDO

  !print*,'d_min',d_min

  !d_ant recherche du sommet le plus loin par projection suivant -1*la normale moyenne 

  d_max=-1.D+20
  imax=0

  DO i=1,PRan%nb_vertex 
    IF (an_vertex(i) == 0) CYCLE
    vec(:)=(PRan%vertex(:,i)+an_shift(:))-center(:)

    d_an=DOT_PRODUCT(normal(:),vec(:))

    IF ( d_max < d_an ) THEN
      d_max=d_an
      imax=i
    ENDIF
  ENDDO

  !print*,'d_max',d_max

  !fd on essaie de construire un repere intelligent ou 
  !fd t passe sur l'axe le plus defavorable

  IF (bavard) then
    print*,'--- tentative de prediction intelligente de t ---'
    print '(A10,I5,3(1x,D14.7))','vertex an: ',imax,PRan%vertex(:,imax)+an_shift(:)
    print '(A10,I5,3(1x,D14.7))','vertex cd: ',jmin,PRcd%vertex(:,jmin)
    print '(A10,3(1x,D14.7))','normale: ',normal(:)
  endif

  p1(:)=(PRan%vertex(:,imax)+an_shift(:))-PRcd%vertex(:,jmin)
  t(:)=p1(:)-(dot_product(p1(:),normal(:)))*normal(:)

  norm=SQRT(dot_product(t,t))

  IF (bavard) then
    print '(A20,4(1x,D14.7))','t predit: ',t,norm
    print *,'min_radius_POLYR',ref_size  
  endif

  If (norm < 1d-6*ref_size) then

     !fd cas degenere ou le t qu'on a construit est aligne avec la normale

     CALL comp_rep(t,normal,s)

     IF (bavard) then 
        print *,'on jete t predit'
        print '(A20,3(1x,D14.7))','on prend: ',t
     endif

  else

    t=t/norm        

    IF (bavard) then 
       print *,'on garde t predit'
       print '(A20,3(1x,D14.7))','une fois norme: ',t
    ENDIF

    s = cross_product(t,normal)
  endif

  ! print*,t
  ! print*,normal
  ! print*,s


  ! initialisation
  gap=d_min-d_max
  angle = angle_max

  !am & vv: erreur d'initialisation detectee par ifort, en mode check
  irot = 0

  ! pour tracer  
  stored_gap = 0.d0
  stored_angle = 0.d0

  DO m=1,cundall_iter

    !print*,'=================='
    !print*,'iteration',m
    !print*,'gap_ini ',gap_ini
    !print*,'angle ',angle,'rot ',irot

    ! construction des 4 perturbations a la CUNDALL  
    co= COS(angle)
    si= SIN(angle)
    !1
    n_ite(:,1)= co*normal(:) + si*t(:)
    norm=SQRT(DOT_PRODUCT(n_ite(:,1),n_ite(:,1)))
    n_ite(:,1)=n_ite(:,1)/norm
    !2
    n_ite(:,2)= co*normal(:) - si*t(:)
    norm=SQRT(DOT_PRODUCT(n_ite(:,2),n_ite(:,2)))
    n_ite(:,2)=n_ite(:,2)/norm
    !3
    n_ite(:,3)= co*normal(:) + si*s(:)
    norm=SQRT(DOT_PRODUCT(n_ite(:,3),n_ite(:,3)))
    n_ite(:,3)=n_ite(:,3)/norm
    !4
    n_ite(:,4)= co*normal(:) - si*s(:)
    norm=SQRT(DOT_PRODUCT(n_ite(:,4),n_ite(:,4)))
    n_ite(:,4)=n_ite(:,4)/norm

    ! on recherche celle qui maximise le gap

    all_gap  = -1.d+20
    all_jmin = 0
    all_imax = 0

    DO k=1,4
      !print*,'----'
      !print*,'perturbation ',k
      !print*,'n ',n_ite(:,k)

      !d_min recherche du sommet le plus proche par projection suivant la normale moyenne 
      d_min= 1.D+20
      DO j=1,PRcd%nb_vertex 
        IF (cd_vertex(j) == 0) CYCLE
        vec(:)=PRcd%vertex(:,j)-center(:)

        d_cd=DOT_PRODUCT(n_ite(:,k),vec(:))

        IF ( d_min > d_cd ) THEN
          d_min=d_cd
          all_jmin(k)=j
        ENDIF
      ENDDO

      !print*,'vertexmin',all_min(k),PRcd%vertex(:,all_min(k))

      !d_max recherche du sommet le plus proche par projection suivant -1*la normale moyenne 
      d_max=-1.D+20
      DO i=1,PRan%nb_vertex 
        IF (an_vertex(i) == 0) CYCLE
        vec(:)=(PRan%vertex(:,i)+an_shift(:))-center(:)

        d_an=DOT_PRODUCT(n_ite(:,k),vec(:))

        IF ( d_max < d_an ) THEN
          d_max=d_an
          all_imax(k)=i
        ENDIF
      ENDDO

      !print*,'vertexmax',all_max(k),PRan%vertex(:,all_max(k)) 

      !Gap max perturbe

      all_gap(k)=d_min-d_max

      !print*,'==================='
      !print*,'d_min',d_min,all_min(k)
      !print*,'d_max',d_max,all_max(k)
      !print*,'G_max(',k,')',G_max(k)
      !print*,'==================='

    ENDDO

    ! Test entre Gap_ini et Gap_max perturbe

    ! on maximise le Gap Gmax
    ! on regarde si la variation du Gmax diminue de qqch

    kk=0
    crit=1.d+20

    gap_ini = gap
    DO k=1,4
      IF (gap < all_gap(k)) THEN     
        gap=all_gap(k)
        kk = k
        jmin=all_jmin(k)
        imax=all_imax(k)
      ENDIF
    ENDDO

    if (kk /= 0) crit = (gap - gap_ini)/ref_size

    !
    IF (bavard) THEN
      IF (kk /=0 ) THEN
        PRINT*,'angle ',angle
        PRINT*,'perturbations ', all_gap
        PRINT*,'la perturbation d''orientation',kk
        PRINT*,'augmente le gap qui vaut      ',gap
        PRINT*,'avec variation sur le gap     ',crit
      ELSE
        PRINT*,'angle ',angle
        PRINT*,'n''arrange rien'
      ENDIF
    ENDIF 

    !
    ! pour comprendre si ca ne marche pas
    stored_gap(m) = gap
    stored_angle(m)=angle

    ! test d'arret pour la minimisation de variation du gap 
    IF (crit < tol) kk = 0


    ! on n'a pas converge et pour l angle donne on maximise encore le gap
    IF ( kk /= 0 ) THEN

      ! on a trouve une direction qui fait augmenter gap 
      ! mais la variation plus grande que tol

      irot=0

      ! on actualise le plan tangent suivant la valeur de n

      SELECT CASE (kk)
      CASE (1)
        t(:) = -si*normal(:) + co*t(:)

      CASE (2)
        t(:) =  si*normal(:) + co*t(:)

      CASE (3)
         s(:) = -si*normal(:) + co*s(:)
 
      CASE (4)
         s(:) =  si*normal(:) + co*s(:)

      END SELECT


      normal(:)=n_ite(:,kk)
      norm=SQRT(DOT_PRODUCT(normal(:),normal(:)))
      normal(:)=normal(:)/norm
     

      IF (bavard) THEN
        PRINT*,' '
        PRINT*,'on conserve le repere local'
        PRINT*,'t',t
        PRINT*,'n',normal
        PRINT*,'s',s
      ENDIF

    ELSE

      ! le gap n'augmente pas ou sa variation est plus petite que la tolerance
      ! on fait tourner le repere de perturbation

      irot=irot+1

      IF (irot < nb_rot) THEN
        ! on n a pas tout essaye pour cet angle 
        ! on tente la rotation du repere
        ! dans le plan tangent                         

        IF (bavard) THEN
          PRINT*,' '
          PRINT*,'on tourne le repere local'
        ENDIF

        !
        ! Rotation du repere de 90/nb_rot
        !
        t(:)=(co_rot*t(:))+(si_rot*s(:))
        norm=SQRT(DOT_PRODUCT(t,t))
        t=t/norm

        s = cross_product(t,normal)

      ELSE
         
        ! on a tout essaye pour cet angle
        ! on diminue l angle

        IF (bavard) THEN
          PRINT*,' '
          PRINT*,'on reduit l''angle'
        ENDIF

        irot=0

        IF (angle >= angle_min) THEN

          angle  = angle*0.5d0

        ELSE 

          ! on a trouve la direction
          EXIT

        ENDIF
      ENDIF
    ENDIF
  ENDDO

   if (bavard) then
     PRINT*,m,cundall_iter
     if (m > cundall_iter) then

       print*,"==================================="
       print*,cd_vertex
       print*,an_vertex 
       print*,n_ini
       print*,normal
       print*,g_ini,gap
       dump_fich = get_io_unit()
       print*,'write gap evolution history in dump file',dump_fich
       nom =' '
       write(nom,'(I0,"_",I0)') PRcd%id,PRan%id
       open(dump_fich,file='pbdetect_'//trim(nom)//'.txt')

       do i=2,cundall_iter
         write(dump_fich,'(1x,I5,3(1x,D14.7))') i,stored_gap(i),& 
                                                (stored_gap(i)-stored_gap(i-1))/dabs(stored_gap(i)), &
                                                 stored_angle(i)
       enddo
       close(dump_fich)
       print*,"==================================="
     endif
   endif

   if (m<= cundall_iter) compute_cundall_common_plane_old = 1

   ! @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


   norm=SQRT(DOT_PRODUCT(normal,normal))
   normal(:)=normal(:)/norm

   call comp_rep(t,normal,s)

   !print*,'normal',normal
   !print*, center

   ! Translation du point moyen
   ! @@@ Critere de contact

   vec(:)=PRcd%vertex(:,jmin)-center(:)
   d_min=DOT_PRODUCT(normal(:),vec(:))

   vec(:)=(PRan%vertex(:,imax)+an_shift(:))-center(:)
   d_max=DOT_PRODUCT(normal(:),vec(:))

   gap = d_min - d_max

   center(:)=((PRan%vertex(:,imax)+an_shift(:))+PRcd%vertex(:,jmin))*0.5d0
  
   ! calcul repere local


   ! fd: RIP GMV
   !     look for bit of codes in obsolete

   if (bavard) then
     print*,'n ',normal
     print*,'P ',center
     print*,'g ',d_min - d_max
   endif


   ! on ne garde que les noeuds dans la bande [-tol_proj,+tol_proj] autour du CP

   tol_proj=cundall_neighbor

   !on calcule une dimension de reference
   norm1=(PRan%vertex(1,imax)-PRan%center(1))**2+ &
         (PRan%vertex(2,imax)-PRan%center(2))**2+ &
         (PRan%vertex(3,imax)-PRan%center(3))**2

   norm2=(PRcd%vertex(1,jmin)-PRcd%center(1))**2+ &
         (PRcd%vertex(2,jmin)-PRcd%center(2))**2+ &
         (PRcd%vertex(3,jmin)-PRcd%center(3))**2

   ref_size=MIN(norm1,norm2)

   !fd tol_proj a mettre en parametre !!

   d1=-MAX(0.d0,(d_min-d_max)*0.5d0 ) - tol_proj*sqrt(ref_size)

   if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
      print*,'neighbor distance',d1
   endif

   DO i=1,PRan%nb_vertex 

     if (an_vertex(i) == 0) cycle 

     vec(:)=(PRan%vertex(:,i)+an_shift(:)) - center(:)
     d2=DOT_PRODUCT(vec(:),normal(:))

     if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
        print*,'an node distance',i,d2
     endif


     IF (d2 < d1) an_vertex(i) = 0
   enddo

   d1 = -d1
   DO i=1,PRcd%nb_vertex 

     if (cd_vertex(i) == 0) cycle 

     vec(:)=(PRcd%vertex(:,i)) - center(:)
     d2=DOT_PRODUCT(vec(:),normal(:))

     if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then
        print*,'cd node distance',i,-d2
     endif


     IF (d2 > d1) cd_vertex(i) = 0
   enddo

  end function compute_cundall_common_plane_old
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------ 
  integer function compute_f2f_common_plane(PRcd,PRan,an_shift,adist, &
                                           center,t,normal,s, &
                                           jmin,imax,fi,fj, &
                                           cd_vertex,an_vertex, &
                                           bavard)
   implicit none
   !> les 2 polyedres  
   TYPE(T_POLYR)        :: PRan,PRcd
   !> vecteur de decalage du corps an (utile pour condition periodique)
   real(kind=8)         :: an_shift(3)
   !> position et orientation du CP
   real(kind=8),dimension(3) :: center,t,normal,s
   !> distance d'alerte
   real(kind=8)         :: adist
   !> les vertex concernes
   INTEGER,DIMENSION(:) :: cd_vertex,an_vertex
   !> niveau de bavardage
   logical              :: bavard
   !>
   integer              :: jmin,imax,fi,fj

   real(kind=8) :: dist2,rd
   integer :: i
   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc

   character(len=80) :: cout

   compute_f2f_common_plane = 0

   !fd la normale est la "moyenne" de celles aux faces
   normal = 0.5d0*(PRan%normal(:,PRan%f2f_set(fj)%G_i(1)) - PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)))
   jmin = PRcd%face(1,PRcd%f2f_set(fi)%G_i(1))
   imax=  PRan%face(1,PRan%f2f_set(fj)%G_i(1))

   !fd pour eviter de garder des faces qui verifient le critere sur les normales 
   !fd mais appartiennent a des objets qui ne sont pas en vis a vis
   !fd inner radius est lie au rayon inscrit des polyedres
   !fd TODO virer ca en virant les set qui sont incompatibles avec l'orientation de l'inter - centre

   dist2 = dot_product(normal, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+an_shift(:)) 

   if (dist2 < -0.1*min(PRcd%inner_radius, PRan%inner_radius)) then

     if (bavard .or. & 
        (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
       write(cout,'(A)') 'inconsistent gap, not keps'
       call logmes(cout)
       write(cout,'(A,D12.4)') 'normal distance between sets ',dist2
       call logmes(cout)
       write(cout,'(A,D12.4)') 'cut off distance ', -0.1*min(PRcd%inner_radius, PRan%inner_radius)
       call logmes(cout)
       write(cout,'(I0,1x,I0,1x,A,I0,3(d12.4,1x))') PRcd%id,fi,'vcd ',jmin,PRcd%vertex(:,jmin)
       call logmes(cout)
       write(cout,'(I0,1x,I0,1x,A,I0,3(d12.4,1x))') PRan%id,fj,'van ',imax,PRan%vertex(:,imax)+an_shift(:)
       call logmes(cout)
       write(cout,'(3(d12.4,1x))') an_shift(:) 
       call logmes(cout)
     endif
     return
   endif

   !fd si la distance est trop grande on vire aussi 

   if (dist2 > adist) then

     if (bavard .or. & 
         (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
       write(cout,'(A)') 'gap incoherent on ne garde pas'
       call logmes(cout)
       write(cout,'(A,D12.4)') 'distance normale entre sets ',dist2
       call logmes(cout)
       write(cout,'(A,D12.4)') 'distance d''alerte ', adist
       call logmes(cout)
     endif 
     return
   endif

   center = 0.5*( PRcd%vertex(:,jmin) + (PRan%vertex(:,imax)+an_shift(:)))

   if (bavard .or. & 
       (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     write(cout,'(A,I0,A,I0)') 'cd set ',fi,' see an set ',fj
     call logmes(cout)
     write(cout,*) 'cd set 1st element',PRcd%f2f_set(fi)%G_i(1)
     call logmes(cout)
     write(cout,*) 'an set 1st element',PRan%f2f_set(fj)%G_i(1)
     call logmes(cout)
     write(cout,'(A,3(1x,D12.4))') 'normal',normal
     call logmes(cout)
     write(cout,'(A,I0,A,3(D12.4,1x))') 'cd node ',jmin,' position ',PRcd%vertex(:,jmin)
     call logmes(cout)
     write(cout,'()') 
     write(cout,'(A,I0,A,3(D12.4,1x))')'an node ',imax,' position ',PRan%vertex(:,imax)+an_shift(:)
     call logmes(cout)
     write(cout,'(A,D12.4)') 'gap ',dot_product(normal, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+an_shift(:))
     call logmes(cout)
     write(cout,'(A,D12.4,D12.4)')'inner_radius ',PRcd%inner_radius,PRan%inner_radius
     call logmes(cout)
   endif

   ! si on passe ici ca veut dire qu'on a trouve et qu'on va sortir des boucles de test 

   !fd on rend visible uniquement les noeuds du set
   cd_vertex = 0
   do i=1,size(PRcd%f2f_set(fi)%G_i)
     cd_vertex(PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))=1            
     cd_vertex(PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))=1            
     cd_vertex(PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))=1            
   enddo
   if (bavard) print*,'cd_skip ',cd_vertex

   an_vertex = 0
   do i=1,size(PRan%f2f_set(fj)%G_i)
     an_vertex(PRan%face(1,PRan%f2f_set(fj)%G_i(i)))=1            
     an_vertex(PRan%face(2,PRan%f2f_set(fj)%G_i(i)))=1            
     an_vertex(PRan%face(3,PRan%f2f_set(fj)%G_i(i)))=1            
   enddo

   if (bavard) print*,'an_skip ',an_vertex

   ! fd: RIP GMV
   !     look for bit of code in obsolete directory

   norm=SQRT(DOT_PRODUCT(normal,normal))
   normal(:)=normal(:)/norm

   call comp_rep(t,normal,s)

   center(:)=((PRan%vertex(:,imax)+an_shift(:))+PRcd%vertex(:,jmin))*0.5d0
  
   compute_f2f_common_plane = 1

   ! fd: RIP GMV
   !     look for bit of code in obsolete directory
   !!--------------------------
   !! pour afficher ce qu'on a calcule
   !!


   if (bavard) then
     print*,'n ',normal
     print*,'P ',center
     ! pas encore calcule print*,'g ',d_max-d_min
   endif
 
  end function compute_f2f_common_plane
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !> connaissant les points de contact on cherche les facettes d'accroche 
  !> et on calcule le gap
  subroutine proj_f2f(PRcd,PRan,an_shift, &
                     fi,fj, &
                     mid_normal,&
                     nb_ctc,pt_ctc, &
                     overlap, &
                     id_face_cd,weight_face_cd, &
                     id_face_an,weight_face_an, &
                     bavard)

   implicit none 

   LOGICAL                          :: bavard
   TYPE(T_POLYR)                    :: PRan,PRcd
   integer                          :: fi,fj
   integer                          :: nb_ctc
   real(kind=8)                     :: an_shift(3),mid_normal(3)
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
   REAL(kind=8),DIMENSION(3,3)      :: pp

   REAL(kind=8),DIMENSION(4)        :: overlap

   integer,dimension(4)             :: id_face_cd,id_face_an
   REAL(kind=8),DIMENSION(3,4)      :: weight_face_cd, weight_face_an

   integer                          :: ic,iff,i,my_cd_iff,my_an_iff,ix
   logical                          :: is_inside
   real(kind=8)                     :: d_min,d_max

                           !123456789012345
   character(len=15):: IAM='PRPRx::proj_f2f' 

   !fd pour comprendre ce qui pourrait deconner dans la detection
   real(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc

   character(len=108) :: cout

   DO ic=1,nb_ctc
     ! recherche de la face cd qui contient le noeud projete suivant la direction mid_nornal
     is_inside = .false.

     do iff=1,size(PRcd%f2f_set(fi)%G_i)
       do i=1,3
         pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
       enddo
       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min, &
                                            weight_face_cd(:,ic),bavard)  
       if (is_inside) then
         my_cd_iff = iff
         exit
       endif
     enddo

     if (.not. is_inside) then
       call logmes('===================================================================================')
       call logmes('Error '//IAM//': unable to find a candidate face')
       write(cout,'(a,i0,a,i0,a,i0)') 'Contact number ',ic,' between POLYR ',PRcd%id,' and POLYR ',PRan%id
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  position ',pt_ctc(:,ic)
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  normal   ',mid_normal
       call logmes(cout)
       write(cout,'(a)') '  the set contains the faces numbered'
       call logmes(cout)
       do iff=1,size(PRcd%f2f_set(fi)%G_i)
         write(cout,'(3x,A,I6,A,3(1x,D12.5),A)') '- ',PRcd%f2f_set(fi)%G_i(iff), &
                                                 '  =  vertex 1 : [',PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 2 : [',PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 3 : [',PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
       enddo

       ! fd: RIP GMV
       !     see obsolete to lookf for
       !     corresponding bit of code

       nb_ctc = 0
       return

     else
       id_face_cd(ic) = PRcd%f2f_set(fi)%G_i(my_cd_iff)

       !print*,'iff_cd ',id_face_cd(ic)
       !print*,'nodes ',PRcd%face(:,PRcd%f2f_set(fi)%G_i(my_cd_iff))
       !print*,'w ',weight_face_cd(:,ic)

     endif


     is_inside = .false.
     do iff=1,size(PRan%f2f_set(fj)%G_i)
       do i=1,3
         pp(:,i) = PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) + an_shift(:) 
       enddo
       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max, &
                                            weight_face_an(:,ic),bavard)  
       if (is_inside) then
         my_an_iff = iff
         exit
       endif
     enddo

     if (.not. is_inside) then
       call logmes('===================================================================================')
       call logmes('Error '//IAM//': unable to find an antagonist face')
       write(cout,'(a,i0,a,i0,a,i0)') 'Contact number ',ic,' between POLYR ',PRcd%id,' and POLYR ',PRan%id
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  position ',pt_ctc(:,ic)
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  normal   ',mid_normal
       call logmes(cout)
       write(cout,'(a)') '  the set contains the faces numbered'
       call logmes(cout)
       do iff=1,size(PRan%f2f_set(fj)%G_i)
         write(cout,'(4x,A,I6,A,3(1x,D12.5),A)') '- ',PRan%f2f_set(fj)%G_i(iff), &
                                                 '  =  vertex 1 : [',PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 2 : [',PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 3 : [',PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
       enddo


       nb_ctc = 0
       return

     else
       id_face_an(ic) = PRan%f2f_set(fj)%G_i(my_an_iff)

       !print*,'iff_an', id_face_an(ic)
       !print*,'nodes ',PRan%face(:,PRan%f2f_set(fj)%G_i(my_an_iff))
       !print*,'w',weight_face_an(:,ic)

     endif


     ! la distance entre les 2 points

     overlap(ic)=d_min-d_max

     if (bavard) then
       print *,'contact: ', ic
       print *,'distance: ',overlap(ic)
       print *,'distances cd/an',d_min,d_max
       print *,'weight cd',weight_face_cd(:,ic)
       print *,'weight an',weight_face_an(:,ic)
     endif
   enddo

  end subroutine proj_f2f
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !> calcul de l'intersection des projections des objets 
  !> connaissant l'orientation du plan separateur (Nr)
  subroutine planplan(PRcd,PRan,perio_shift, &
                     mid_P,t,mid_normal,s, &
                     jmin,imax, & 
                     cd_skip,an_skip, &
                     keep_only_f2f, &
                     nb_ctc,pt_ctc, &
                     area, & 
                     bavard, &
                     face_ctc, face_sizes)

   IMPLICIT NONE

   LOGICAL                          :: bavard
   TYPE(T_POLYR)                    :: PRan,PRcd
   INTEGER,DIMENSION(:)             :: cd_skip,an_skip
   logical                          :: keep_only_f2f ! necessary when performing f2f 

   integer                          :: nb_ctc
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC

   REAL(kind=8),dimension(3)        :: perio_shift,mid_P,t,mid_normal,s

   ! rm added : to store the geometrical contact surface in which contact points are chosen
   !            only for f2f dection
   real(kind=8), dimension(:,:), pointer, optional :: face_ctc
   integer     , dimension(:)  , pointer, optional :: face_sizes

   ! local variables
   INTEGER,PARAMETER                :: nb_ptc_max=100
   INTEGER                          :: i,k,j,m,inc,inc1

   REAL(kind=8)                     :: dist1,dist2,scal,norm,norm1,norm2
   REAL(kind=8),DIMENSION(3)        :: s1,r1
   REAL(kind=8)                     :: gap
   INTEGER                          :: nb_ptc
  
   INTEGER,DIMENSION(2*nb_ptc_max)  :: is_ok

   INTEGER                          :: errare,nb_select
   REAL(kind=8),DIMENSION(3)        :: Ncd
 
   REAL(kind=8)                     :: norm_max,norm_min

   INTEGER                          :: nb_vertex_pran_min, nb_vertex_prcd_min

   REAL(kind=8),DIMENSION(3)        :: p1,p2
   REAL(kind=8),DIMENSION(2)        :: s1moy,r1moy

   REAL(kind=8), DIMENSION(:,:), allocatable :: Pcd,Pan,rr1,ss1
   integer     , dimension(:)  , allocatable :: id_Pan,id_Pcd
   integer     , dimension(:)  , pointer     :: pts_sizes

   INTEGER                        :: jmin,imax

   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max
   REAL(kind=8),DIMENSION(3)      :: n_ini,vec
   REAL(kind=8),DIMENSION(2)      :: lvec,PT

!fd 

   REAL(kind=8)                   :: tol = 1.d-10,rd
   integer                        :: dump_fich

                               !123456789012345
   CHARACTER(len=15)  :: IAM = 'PRPRx::planplan'
   CHARACTER(len=80)  :: cout


   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
   REAL(kind=8),dimension(:)  ,allocatable :: dist

   character(len=11) :: nom

   logical :: is_reverse=.FALSE.

   integer :: ii
   real(kind=8) :: ref_size,dir(2),c_center(2),c_vec(2)

   logical :: is_inside
  ! f2f
   integer :: iff,my_cd_iff,my_an_iff,ic
   logical :: f2f_found

   real(kind=8) :: pp(3,3),shift_ptc(3)
   real(kind=8),pointer :: points(:,:)

   integer :: ix

   !fd par defaut on ne fait pas le so avec les normales des faces
   !fd TODO mettre ca en parametre

   real(kind=8) :: tmp,vec1(3),vec2(3),vec3(3)
   integer :: nb_ptc_qh

   real(kind=8),allocatable :: ptc(:,:),angle(:)
   integer,allocatable :: id_ptc(:)

   ! calcul qh contour a 3 ou 4 noeuds
   integer :: idc3(4),idc4(5)  
   real(kind=8) :: coorc3(3,3),coorc4(3,4)
   real(kind=8) :: vect(3),area
   !
   integer :: err_


   !if (dbg) then
   !  print*,'--------------'
   !  print*,IAM
   !  print*,'--------------'
   !  write(*,'(A,3(1x,D12.5))') 'mid_P=',mid_P
   !  write(*,'(A,3(1x,D12.5))') 't    =',t
   !  write(*,'(A,3(1x,D12.5))') 'n    =',mid_normal
   !  write(*,'(A,3(1x,D12.5))') 's    =',s
   !endif

   nb_ctc = 0    
   points => null()

   

!rp @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!fd @@@                              begin                                         @@@

!rp @@@ Projection des vertex "licites" de l'antagoniste sur le plan moyen 

! calcul d'une tolerance pour reduire le nombre de points licites a projeter
 
! pour an calcul de la distance centre inertie polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &- point critique de Cundall

   norm1=(PRan%vertex(1,imax)-PRan%center(1))**2+ &
         (PRan%vertex(2,imax)-PRan%center(2))**2+ &
         (PRan%vertex(3,imax)-PRan%center(3))**2

! pour cd calcul de la distance centre inertie - point critique de Cundall

   norm2=(PRcd%vertex(1,jmin)-PRcd%center(1))**2+ &
         (PRcd%vertex(2,jmin)-PRcd%center(2))**2+ &
         (PRcd%vertex(3,jmin)-PRcd%center(3))**2

! On trouve la distance minimale

   ref_size=MIN(norm1,norm2)

   if (bavard) print*,'ref_size ',ref_size

   ! Recuperation des vertex à projeter
   ! coordonnees des points dans le plan separateur
   ALLOCATE(ss1(2,PRan%nb_vertex),stat=errare)
   ss1 = 0.d0

   inc=0
   DO i=1,PRan%nb_vertex 

     if (an_skip(i) <= 0) cycle 

     inc=inc+1

     ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen    

     ! fd on ecrit tout dans le rep (mid_P,t,s)

     vec(:)=(PRan%vertex(:,i)+perio_shift(:))-mid_P(:)
     d_an=DOT_PRODUCT(vec(:),mid_normal(:))

     s1(:)= vec(:) - (d_an*mid_normal(:))

     ! Passage du repere general au repere du plan moyen (t,s)

     ss1(1,inc) = s(1)*s1(1)+s(2)*s1(2)+s(3)*s1(3)
     ss1(2,inc) = t(1)*s1(1)+t(2)*s1(2)+t(3)*s1(3)

   ENDDO

   nb_vertex_pran_min=inc

   !
   if (bavard .or. &
       (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
     print*,'PR an ',polyr2bdyty(2,PRan%id),' @ ',polyr2bdyty(1,PRan%id)
     print*,'vextex projetes ', nb_vertex_pran_min
   endif

   ! coordonnees des points dans le plan separateur
   ALLOCATE(Pan(2,nb_vertex_pran_min),stat=errare)
   Pan = 0.d0
   !tableau d'indice pour la suite
   allocate(id_Pan(nb_vertex_pran_min+1),stat=errare)
   id_Pan=0

   Pan=ss1(:,1:nb_vertex_pran_min)

   deallocate(ss1)

   if (bavard) then
     print*,'nbv an projetes ',nb_vertex_pran_min 
     do i=1,nb_vertex_pran_min
       print*,Pan(:,i)
     enddo
   endif

   call convex_hull(Pan,id_Pan,ref_size,err_)

   if (err_ > 0) then
     cout=''
     write(cout,'("POLYR ",I0)') PRan%id
     call logmes(cout, .true.)
     call faterr(IAM,'unexpected error in convex hull computation')
   endif   
   
   !fd le 1 est forcement dans la liste
   inc=1
   do i=2,nb_vertex_pran_min+1
     if (id_Pan(i) /= 0 .and. id_Pan(i) /= id_Pan(1)) inc=inc+1
   enddo

   if (bavard) then
     print*,'vertex anta qh (loop)'
     do i=1,nb_vertex_pran_min+1
       if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
     enddo
   endif

   if (bavard .or. &
     (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
     print*,'nb vertex candidat in qh', inc
     print*,'avant'
     do i=1,nb_vertex_pran_min
       print*,Pan(:,i)
     enddo

     print*,'apres'
     do i=1,nb_vertex_pran_min+1
       if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
     enddo
   endif

   ALLOCATE(rr1(2,PRcd%nb_vertex),stat=errare)
   rr1 = 0.d0

! Recuperation des vertex à projeter
   inc=0
   DO j=1,PRcd%nb_vertex 

     if (cd_skip(j) <= 0) cycle

     inc=inc+1

     ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen            
     !fd on ecrit tout dans le rep (mid_P,s,t)

     vec(:)=PRcd%vertex(:,j)-mid_P(:)
     d_cd=DOT_PRODUCT(vec(:),mid_normal(:))

     r1(:)=vec(:) - (d_cd*mid_normal(:))

     ! Passage du repere general au repere du plan moyen (t,s)

     rr1(1,inc)=s(1)*r1(1)+s(2)*r1(2)+s(3)*r1(3)
     rr1(2,inc)=t(1)*r1(1)+t(2)*r1(2)+t(3)*r1(3)

   ENDDO

   nb_vertex_prcd_min=inc

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'PR cd ',polyr2bdyty(2,PRcd%id),' @ ',polyr2bdyty(1,PRcd%id)
     print*,'vertex projetes ', nb_vertex_prcd_min
   endif

   ALLOCATE(Pcd(2,nb_vertex_prcd_min),stat=errare)
   Pcd = 0.d0

   !tableau d'indice pour la suite
   allocate(id_Pcd(nb_vertex_prcd_min+1),stat=errare)
   id_Pcd=0

   Pcd=rr1(:,1:nb_vertex_prcd_min)

   deallocate(rr1)


   if (bavard) then
     print*,'nbv cd projetes ',nb_vertex_prcd_min 
     do i=1,nb_vertex_prcd_min
       print*,Pcd(:,i)
     enddo
   endif

   call convex_hull(Pcd,id_Pcd,ref_size,err_)
   
   if (err_ > 0) then
     cout=''
     write(cout,'("POLYR ",I0)') PRcd%id
     call logmes(cout, .true.)
     call faterr(IAM,'unexpected error in convex hull computation')
   endif   

   !fd le 1 est forcement dans la liste
   inc=1
   do i=2,nb_vertex_prcd_min+1
     if (id_Pcd(i) /= 0 .and. id_Pcd(i) /= id_Pcd(1)) inc=inc+1
   enddo
   !
   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'nb vertex candidat dans qh', inc
     print*,'avant'
     do i=1,nb_vertex_prcd_min
       print*,Pcd(:,i)
     enddo

     print*,'apres'
     do i=1,nb_vertex_prcd_min+1
       if (id_Pcd(i) /= 0) print*,Pcd(:,id_Pcd(i))
     enddo
   endif

  
   if (bavard) then
     print*,'vertex candidat qh (loop)'
     do i=1,nb_vertex_prcd_min+1
       if (id_Pcd(i) /= 0) print*,Pcd(:,id_Pcd(i))
     enddo
   endif


   !fd TODO ce shrink la devrait automatique et tres faible pour gommer les 0 numeriques

   if (shrink > 0.d0) then

     !fd le 1 est forcement dans la liste
     inc=1
     c_center = Pcd(1:2,id_Pcd(1))

     do i=2,nb_vertex_prcd_min+1
       if (id_Pcd(i) /= 0 .and. id_Pcd(i) /= id_Pcd(1)) then
         inc = inc + 1
         c_center = c_center + Pcd(1:2,id_Pcd(i))
       endif
     enddo
     c_center = c_center / real(inc,8)

     !fd on shrink tous les points et pas juste le convex_hull
     do i=1,nb_vertex_prcd_min
       c_vec(1:2) = Pcd(1:2,i) - c_center(1:2)  
       Pcd(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
     enddo

     if (bavard) then
       print*,'vertex candidat enveloppe convexe shrink (loop)'
       do i=1,nb_vertex_prcd_min+1
         if (id_Pcd(i) /= 0) print *,Pcd(:,id_Pcd(i))
       enddo
     endif

   endif

!fd @@@                              end                                           @@@
! @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES enveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!fd @@@                              begin                                         @@@


   norm_min=MIN(PRan%min_radius_face,PRcd%min_radius_face) 
   norm_max=MAX(PRan%max_radius_face,PRcd%max_radius_face) 

   ref_size=norm_min

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
     !call polytopes_intersection(Pan,id_Pan,Pcd,id_Pcd,ref_size,points,nb_ptc,.true.)
     !call polytopes_intersection_wp(Pan,id_Pan,Pcd,id_Pcd,points,nb_ptc,.true.,err_)
     call polytopes_intersection_wc(Pan,id_Pan,Pcd,id_Pcd,points,pts_sizes,cp_an_shrink,cp_cd_shrink,cp_delta,area,.true.,err_)
   else
     !call polytopes_intersection(Pan,id_Pan,Pcd,id_Pcd,ref_size,points,nb_ptc,.false.)
     !call polytopes_intersection_wp(Pan,id_Pan,Pcd,id_Pcd,points,nb_ptc,.false.,err_)
     call polytopes_intersection_wc(Pan,id_Pan,Pcd,id_Pcd,points,pts_sizes,cp_an_shrink,cp_cd_shrink,cp_delta,area,.false.,err_)
   endif

   if( associated(pts_sizes) ) then
     nb_ptc = sum(pts_sizes)
   else
     nb_ptc = 0
   end if


   if (err_ > 0) then
     write(cout,'("PRcd ",I0," PRan ",I0)') PRcd%id,PRan%id
     call logmes(cout, .true.)
     call faterr(IAM,'unexpected problem in polytopes intersection')
   endif   

   !rm : store the intersection polytope expressed relatively to antagonist
   if( nb_ptc > 2 .and. present(face_ctc) .and. present(face_sizes) ) then

     if( associated(face_ctc)   ) deallocate(face_ctc)
     if( associated(face_sizes) ) deallocate(face_sizes)

     face_sizes => pts_sizes

     allocate(face_ctc(3,nb_ptc))
     do i = 1, nb_ptc
       ! express point in referential frame
       face_ctc(:,i) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))
     end do
     ! then express points in antagonist frame
     call glob2loc_frame_POLYR(PRan%id, face_ctc)

   end if

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then  
     print*,'cd ',PRcd%id,' an ',PRan%id
     print*,'nombre d''intersection', nb_ptc
     print*,'ref size',ref_size
     print*,'points de l''intersection dans le plan'
     do i=1,nb_ptc
       print*,points(:,i)  
     enddo 
   endif 

   !fd essaie pour virer les noeuds en trop
   if ( nb_ptc > 4 ) then
     
     allocate(ptc(2,nb_ptc),id_ptc(nb_ptc+1))  
     ptc(1:2,1:nb_ptc) = points(1:2,1:nb_ptc)
     id_ptc = 0

     !fd on calcule l'enveloppe convexe pour ne garder que ca 
     !fd probablement pas utile 
     !fd par contre en est sur que ca tourne dans le sens trigo

     !print*,'gestion noeuds en trop'

     call convex_hull(ptc,id_ptc,ref_size,err_)
     
     if (err_ > 0) then
       call logmes('ptc reduction', .true.)
       call faterr(IAM,'unexpected error in convex hull computation')
     endif   

     nb_ptc_qh = count(id_ptc /= 0) - 1

     !fd si l'enveloppe convexe en a encore trop 

     if (nb_ptc_qh > 4) then

       !fd on garde ceux qui ont les angles proches de 90 
       !fd vec1.vec2 / |vec1| |vec2| proche de 0

       !print*,'plus que 4'
       !print*,'indexes qh ',id_ptc

       allocate(angle(nb_ptc_qh))  

       vec1(1:2) = ptc(1:2,id_ptc(1)) - ptc(1:2,id_ptc(nb_ptc_qh))
       vec2(1:2) = ptc(1:2,id_ptc(2)) - ptc(1:2,id_ptc(1))
       angle(1) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
          (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))

       do i=2,nb_ptc_qh
         if (id_ptc(i) == 0) then
           write(cout,'(A)') 'strange id_ptc'
           call logmes(cout, .true.)
           write(cout,'(I0)') id_ptc
           call faterr(IAM,cout) 
         endif 
         !if (id_ptc(i) < 0) cycle

         vec1(1:2) = ptc(1:2,id_ptc(i)) - ptc(1:2,abs(id_ptc(i-1)))
         vec2(1:2) = ptc(1:2,id_ptc(i+1)) - ptc(1:2,id_ptc(i))
         angle(i) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
            (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))
       enddo

       !print*,'indexes qh ',id_ptc   

       do i=1,4
           inc = minloc(array=angle,dim=1)
           angle(inc)=1.d+20
           points(1:2,i) = ptc(1:2,id_ptc(inc))
       enddo

       nb_ptc = 4

       deallocate(angle)

       !print*,nb_ptc

     else 

        !print*,'moins que 4'
        !print*,'indexes qh ',id_ptc

        do i=1,count(id_ptc /= 0) -1
          if (id_ptc(i) == 0) then
            write(cout,'(A)') 'strange id_ptc'
            call logmes(cout, .true.)
            write(cout,'(I0)') id_ptc
            call faterr(IAM,cout)
          endif 
          points(1:2,i) = ptc(1:2,id_ptc(i))
        enddo
        nb_ptc = count(id_ptc /= 0) -1
     endif

     deallocate(ptc,id_ptc)

   endif

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then  
     print*,'cd ',PRcd%id,' an ',PRan%id
     print*,'nombre de points retenus', nb_ptc
     print*,'ref size',ref_size
     print*,'points retenus'
     do i=1,nb_ptc
       print*,points(:,i)  
     enddo 
   endif 

   DEALLOCATE(Pan,id_Pan,Pcd,id_Pcd)

!fd @@@                              end                                           @@@
!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES eveloppes convexes @@@@@@@@@@@@@@@@@@@@@


!fd @@@@@@@@@@@@ reduction du nombre de noeuds a 4 maxi          @@@@@@@@@@@@@@@@@@@@@

!fd on calcule le centre du nuage.
!fd on vire le plus proche
!fd on recommence ...

   if (nb_ptc > 4) then
     is_ok=1
     allocate(dist(nb_ptc))

     DO i=1,nb_ptc
       PT(:)=PT(:)+points(:,i)
     ENDDO

     PT=PT/REAL(nb_ptc,8)

     DO i=1,nb_ptc
       dist(i)=dot_product(PT(:)-points(:,i),PT(:)-points(:,i))
     ENDDO

     do i=1,4
       inc=maxloc(dist,dim=1)        
       is_ok(inc)=0
       dist(inc)=0.d0
     enddo
   else
     is_ok=0
   endif

   if (bavard) then
     print*,'mid_P ',mid_P
     print*,'s '    ,s
     print*,'t '    ,t 
  
     print*,'point de l''intersection dans le repere global' 
   endif


   !fd on retente un shrink

   inc=0
   c_center = 0.d0
   do i=1,nb_ptc
     if (is_ok(i) /= 0 ) cycle
     inc = inc + 1 
     c_center(1:2) = c_center(1:2) + points(1:2,i)
   enddo
   if (inc /= 0) then
     c_center = c_center / real(inc,8)

     !fd on shrink tous les points 
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       c_vec(1:2) = points(1:2,i) - c_center(1:2)  
       points(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
     enddo

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then  
     print*,'points retenus apres shrink'
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       print*,points(:,i)  
     enddo 
   endif 

   endif

!fd @@@@@@@@@@@@ construction des points de contact          @@@@@@@@@@@@@@@@@@@@@

   nb_select=0
   do i=1,nb_ptc
     if (is_ok(i) /= 0 ) cycle
     nb_select = nb_select + 1 
   enddo

   if (keep_only_f2f) then
     if (nb_select < 3) then
       if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) &
         print*,'Not enough contact nodes between the surfaces'
       nb_ctc = 0
       if (associated(points)) deallocate(points) 
       return
     else
       nb_ctc=nb_select 
     endif
   else
     if (nb_select == 0) then
       ! fd est ce utile ? print*,'Not enough contact nodes between the surfaces'
       nb_ctc = 0
       if (associated(points)) deallocate(points) 
       return
     else
       nb_ctc=nb_select 
     endif
   endif

   !fd tentative pour trier les noeuds de facon à pouvoir calculer la surface
   !fd on traite les cas a 3 et 4 noeuds explicitement pour eviter les alloc/dealloc
   if (nb_ctc < 3 .and. nb_ctc > 0) then
     nb_select=0
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       nb_select = nb_select + 1
       PT_CTC(:,nb_select) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))
     enddo    
     !area = 0.d0
       
   else if (nb_ctc == 3) then
     nb_select=0
     coorc3 = 0.d0
     idc3=0
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       nb_select = nb_select + 1
       coorc3(1:2,nb_select) = points(1:2,i)
     enddo

     if (bavard) then
       print*,'points'
       write(*,'(2(1x,D12.5))') points

       print*,'coorc3'
       write(*,'(3(1x,D12.5))') coorc3
     endif

     call convex_hull(coorc3,idc3,ref_size,err_)

     if (err_ > 0) then
       call logmes('3 nodes surface computation', .true.)
       call faterr(IAM,'unexpected error in convex hull computation')
     endif   

     
     if (count(idc3 > 0) < 3) then
       nb_ctc = 0
       if (associated(points)) deallocate(points) 
       return
       !write(*,'(3(1x,D12.5))') coorc3
       !write(*,'(3(1x,I0))') idc3
       !call faterr(IAM,'idc3 foireux')
     endif


     !vect = cross_product(coorc3(:,idc3(2)) - coorc3(:,idc3(1)), &
     !                     coorc3(:,idc3(3)) - coorc3(:,idc3(1)))

     !! on en deduit l'aire du joint  A = |12 ^ 13| / 2
     !area = 0.5*length3(vect)

     do i=1,3
       PT_CTC(:,i) = mid_P(:) + (coorc3(1,idc3(i))*s(:)) + (coorc3(2,idc3(i))*t(:))
     enddo    

   else if (nb_ctc == 4) then
     nb_select=0
     coorc4=0.d0
     idc4=0
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       nb_select = nb_select + 1
       coorc4(1:2,nb_select) = points(1:2,i)
     enddo

     call convex_hull(coorc4,idc4,ref_size, err_)

     if (err_ > 0) then
       call logmes('4 nodes surface computation', .true.)
       call faterr(IAM,'unexpected error in convex hull computation')
     endif   

     
     if (count(idc4 > 0) < 4) then
       nb_ctc = 0
       if (associated(points)) deallocate(points) 
       return
       !write(*,'(3(1x,D12.5))') coorc4
       !write(*,'(4(1x,I0))') idc4
       !call faterr(IAM,'idc4 foireux')
     endif
 
     !vect = cross_product(coorc4(:,idc4(2)) - coorc4(:,idc4(1)), &
     !                     coorc4(:,idc4(3)) - coorc4(:,idc4(1)))

     !! on en deduit l'aire du joint (A=0.5*|12 ^ 13|)
     !area = 0.5*length3(vect)

     !vect = cross_product(coorc4(:,idc4(3)) - coorc4(:,idc4(1)), &
     !                     coorc4(:,idc4(4)) - coorc4(:,idc4(1)))

     !! on en deduit l'aire du joint (A=0.5*|13 ^ 14|)
     !area = area + (0.5*length3(vect))

     do i=1,4
        PT_CTC(:,i) = mid_P(:) + (coorc4(1,idc4(i))*s(:)) + (coorc4(2,idc4(i))*t(:))
     enddo    
   else
     call FATERR(IAM,'unexpected number of contact point')
   endif 

   if (bavard) write(*,'(3(1x,D12.5))') pt_ctc(:,1:nb_ctc)


   !nb_select=0
   !do i=1,nb_ptc
   !  if (is_ok(i) /= 0 ) cycle
   !  nb_select = nb_select + 1 
   !  PT_CTC(:,nb_select) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))
   !  if (bavard) print*,pt_ctc(:,nb_select)
   !enddo

   if (associated(points)) deallocate(points) 

  END SUBROUTINE planplan
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  subroutine planplan_contour(bavard,adist,PRcd,PRan, &
                     cd_skip,an_skip, fi,fj, &
                     nb_ctc,pt_ctc,overlap, &
                     Nr,t,s, &
                     id_face_cd,weight_face_cd, &
                     id_face_an,weight_face_an, &
                     face_ctc, face_sizes, area)

   IMPLICIT NONE


   LOGICAL                          :: bavard
   TYPE(T_POLYR)                    :: PRan,PRcd
   INTEGER,DIMENSION(:)             :: cd_skip,an_skip
   integer                          :: fi,fj,nb_ctc
   REAL(kind=8),DIMENSION(3,4)      :: PT_CTC
   REAL(kind=8),DIMENSION(4)        :: overlap
   integer,dimension(4)             :: id_face_cd,id_face_an
   REAL(kind=8),DIMENSION(3,4)      :: weight_face_cd, weight_face_an

   REAL(kind=8)                     :: Nsep(3),adist,Nr(3),t(3),s(3)
   ! rm added : to store the geometrical contact surface in which contact points are chosen
   !            only for f2f dection
   real(kind=8), dimension(:,:), pointer :: face_ctc
   integer     , dimension(:)  , pointer :: face_sizes
   real(kind=8) :: area

   ! local variables

   INTEGER,PARAMETER                :: nb_ptc_max=100
   INTEGER                          :: i,k,j,m,inc,inc1


   REAL(kind=8)                     :: dist1,dist2,scal,norm,norm1,norm2
   REAL(kind=8),DIMENSION(3)        :: s1,r1,Nsep_o
   REAL(kind=8)                     :: gap
   INTEGER                          :: nb_ptc
  
   INTEGER,DIMENSION(2*nb_ptc_max)  :: is_ok


   INTEGER                          :: errare,nb_select
   REAL(kind=8),DIMENSION(3)        :: Ncd
 
   REAL(kind=8)                     :: norm_max,norm_min



   INTEGER                          :: nb_vertex_pran_min, nb_vertex_prcd_min

   REAL(kind=8),DIMENSION(3)        :: mid_P,p1,p2,mid_normal
   REAL(kind=8),DIMENSION(2)        :: s1moy,r1moy

   REAL(kind=8),DIMENSION(:,:), ALLOCATABLE   :: Pcd,Pan,rr1,ss1   
   integer,dimension(:),allocatable :: id_Pan,id_Pcd



   INTEGER                        :: jmin,imax

   REAL(kind=8)                   :: d_an,d_cd,d_min,d_max

   REAL(kind=8),DIMENSION(3)      :: n_ini,vec

   REAL(kind=8),DIMENSION(2)      :: lvec,PT

!fd 

   REAL(kind=8)                   :: tol = 1.d-10,rd
   integer                        :: dump_fich

                               !12345678901234567890123
   CHARACTER(len=23)  :: IAM = 'PRPRx::planplan_contour'


   REAL(kind=8),dimension(:,:),allocatable :: v_coor_cd,v_coor_an,v_coor_ptc
   REAL(kind=8),dimension(:),allocatable :: dist

   character(len=11) :: nom

   logical :: is_reverse=.FALSE.

   integer :: ii
   real(kind=8) :: ref_size,dir(2),c_center(2),c_vec(2)

   logical :: is_inside
  ! f2f
   integer :: iff,my_cd_iff,my_an_iff,ic
   logical :: f2f_found

   real(kind=8) :: pp(3,3),shift_ptc(3)
   real(kind=8),pointer :: points(:,:) => null()

   integer :: ix

   !fd par defaut on ne fait pas le so avec les normales des faces
   !fd TODO mettre ca en parametre

   logical :: skip_so = .true.
   integer :: err_


   real(kind=8) :: tmp,vec1(3),vec2(3),vec3(3)
   integer :: nb_ptc_qh

   real(kind=8),allocatable :: ptc(:,:),angle(:)
   integer,allocatable :: id_ptc(:)

   character(len=108) :: cout

   nb_ctc = 0    

   !fd la normale est la "moyenne" de celles aux faces
   n_ini = 0.5d0*(PRan%normal(:,PRan%f2f_set(fj)%G_i(1)) - PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)))
   jmin = PRcd%face(1,PRcd%f2f_set(fi)%G_i(1))
   imax=  PRan%face(1,PRan%f2f_set(fj)%G_i(1))

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
     print*,'cd: ',PRcd%id,fi,PRcd%f2f_set(fi)%G_i(1),jmin,PRcd%inner_radius
     print*,PRcd%vertex(:,jmin)
     print*,'an: ',PRan%id,fj,PRan%f2f_set(fj)%G_i(1),imax,PRan%inner_radius
     print*,PRan%vertex(:,imax)
     print*,n_ini
     print*,perio_shift

   endif 

   !fd pour eviter de garder des faces qui verifient le critere sur les normales 
   !fd mais appartiennent a des objets qui ne sont pas en vis a vis
   !fd inner radius est lie au rayon inscrit des polyedres
   !fd TODO virer ca en virant les set qui sont incompatibles avec l'orientation de l'inter - centre

   dist2 = dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:)) 

   ! burk
   if (dist2 < -0.1*min(PRcd%inner_radius, PRan%inner_radius)) then
     if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
       print*,'test rapide:'
       print*,'gap negatif incoherent on ne garde pas'
       print*,'distance normale entre sets ',dist2
       print*,'distance de coupure ', -0.1*min(PRcd%inner_radius, PRan%inner_radius)
     endif
     return
   endif

   !fd si la distance est trop grande on vire aussi 

   if (dist2 > adist) then
     if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
       print*,'test rapide:'
       print*,'gap positif incoherent on ne garde pas'
       print*,'distance normale entre sets ',dist2
       print*,'distance d''alerte ', adist
     endif 
     return
   endif

   mid_P = 0.5*( PRcd%vertex(:,jmin) + (PRan%vertex(:,imax)+perio_shift(:)))

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then
      print*,'le cd set ',fi,' voit le an set ',fj
     print*,'cd set ',PRcd%f2f_set(fi)%G_i(:)
     print*,'an set ',PRan%f2f_set(fj)%G_i(:)
     print*,'normale',n_ini
     print*,'noeud cd ',jmin,' position ',PRcd%vertex(:,jmin)
     print*,'noeud an ',imax,' position ',PRan%vertex(:,imax)+perio_shift(:)
     print*,'gap ',dot_product(n_ini, PRcd%vertex(:,jmin) - PRan%vertex(:,imax)+perio_shift(:))
     print*,'inner_radius ',PRcd%inner_radius,PRan%inner_radius
   endif

   ! si on passe ici ca veut dire qu'on a trouve et qu'on va sortir des boucles de test 

   !fd on rend visible uniquement les noeuds du set
   cd_skip = 0
   do i=1,size(PRcd%f2f_set(fi)%G_i)
     cd_skip(PRcd%face(1,PRcd%f2f_set(fi)%G_i(i)))=1            
     cd_skip(PRcd%face(2,PRcd%f2f_set(fi)%G_i(i)))=1            
     cd_skip(PRcd%face(3,PRcd%f2f_set(fi)%G_i(i)))=1            
   enddo
   if (bavard) print*,'cd_skip ',cd_skip

   an_skip = 0
   do i=1,size(PRan%f2f_set(fj)%G_i)
     an_skip(PRan%face(1,PRan%f2f_set(fj)%G_i(i)))=1            
     an_skip(PRan%face(2,PRan%f2f_set(fj)%G_i(i)))=1            
     an_skip(PRan%face(3,PRan%f2f_set(fj)%G_i(i)))=1            
   enddo

   if (bavard) print*,'an_skip ',an_skip

   f2f_found = .TRUE.

   ! fd: RIP GMV
   !     see obsolete to lookf for
   !     corresponding bit of code

   if (.not. f2f_found) return

!fd @@@                                 end                                    @@@
!rp @@@@@@ recherche de l'orientation du plan separateur à la F2F @@@@@@@@@@@@@@@@


!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!fd @@@                                begin                                       @@@

   norm=SQRT(DOT_PRODUCT(n_ini,n_ini))
   mid_normal(:)=n_ini(:)/norm

   mid_P(:)=((PRan%vertex(:,imax)+perio_shift(:))+PRcd%vertex(:,jmin))*0.5d0
  
!fd @@@ calcul repere local

   Nr = mid_normal

   call comp_rep(t,mid_normal,s)


   ! fd: RIP GMV
   !     see obsolete to lookf for
   !     corresponding bit of code


   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'t ',t
     print*,'n ',Nr
     print*,'s ',s
     print*,'P ',mid_P
     ! pas encore calcule print*,'g ',d_max-d_min
   endif

!fd @@@                                end                                         @@@
!rp @@@@@@@@@@@@@@@@@@@@@@@@@@@@@ mise au propre @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


!rp @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!fd @@@                              begin                                         @@@

!rp @@@ Projection des vertex "licites" de l'antagoniste sur le plan moyen 

! calcul d'une tolerance pour reduire le nombre de points licites a projeter
 
! pour an calcul de la distance centre inertie polyr2bdyty(1,PRcd%id),polyr2bdyty(1,PRan%id), &- point critique de Cundall

   norm1=(PRan%vertex(1,imax)-PRan%center(1))**2+ &
         (PRan%vertex(2,imax)-PRan%center(2))**2+ &
         (PRan%vertex(3,imax)-PRan%center(3))**2

! pour cd calcul de la distance centre inertie - point critique de Cundall

   norm2=(PRcd%vertex(1,jmin)-PRcd%center(1))**2+ &
         (PRcd%vertex(2,jmin)-PRcd%center(2))**2+ &
         (PRcd%vertex(3,jmin)-PRcd%center(3))**2

! On trouve la distance minimale

   ref_size=MIN(norm1,norm2)

   if (bavard) print*,'ref_size ',ref_size

   ! Recuperation des vertex à projeter
   ! coordonnees des points dans le plan separateur
   ALLOCATE(ss1(2,PRan%nb_vertex),stat=errare)
   ss1 = 0.d0

   inc=0
   DO ic=1,size(PRan%f2f_contour(fj)%G_i)/2
     
     inc=inc+1
      
     i = PRan%f2f_contour(fj)%G_i(2*ic - 1)

     ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen    

     ! fd on ecrit tout dans le rep (mid_P,s,t)

     vec(:)=(PRan%vertex(:,i)+perio_shift(:))-mid_P(:)
     d_an=DOT_PRODUCT(vec(:),mid_normal(:))

     s1(:)= vec(:) - (d_an*mid_normal(:))

     ! Passage du repere general au repere du plan moyen (s,t)

     ss1(1,inc) = s(1)*s1(1)+s(2)*s1(2)+s(3)*s1(3)
     ss1(2,inc) = t(1)*s1(1)+t(2)*s1(2)+t(3)*s1(3)

   ENDDO

   nb_vertex_pran_min=inc

   !
   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'PR an ',polyr2bdyty(2,PRan%id),' @ ',polyr2bdyty(1,PRan%id)
     print*,'vextex projetes ', nb_vertex_pran_min
   endif

   ! coordonnees des points dans le plan separateur
   ALLOCATE(Pan(2,nb_vertex_pran_min),stat=errare)
   Pan = 0.d0

   !tableau d'indice pour la suite
   allocate(id_Pan(nb_vertex_pran_min+1),stat=errare)
   id_Pan(1:nb_vertex_pran_min)= (/ (i,i=1,nb_vertex_pran_min) /) 
   id_Pan(nb_vertex_pran_min+1)= 1

   Pan=ss1(:,1:nb_vertex_pran_min)

   deallocate(ss1)

   if (bavard) then
     print*,'nbv an projetes ',nb_vertex_pran_min 
     do i=1,nb_vertex_pran_min
       print*,Pan(:,i)
     enddo
   endif

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'nb vertex candidat dans contour', nb_vertex_pran_min
     !print*,'avant'
     do i=1,nb_vertex_pran_min
       print*,Pan(:,i)
     enddo

     !print*,'apres'
     !do i=1,nb_vertex_pran_min+1
     !  if (id_Pan(i) /= 0) print*,Pan(:,id_Pan(i))
     !enddo
   endif

   ALLOCATE(rr1(2,PRcd%nb_vertex),stat=errare)
   rr1 = 0.d0

! Recuperation des vertex à projeter
!fd attention il faut inverser l'ordre de parcours du cd car la normale a la face est inversee
   inc=0
   DO ic=size(PRcd%f2f_contour(fi)%G_i)/2,1,-1
     inc=inc+1

     j=PRcd%f2f_contour(fi)%G_i(2*ic - 1)

     ! Projection des vertex satisfaisant le critere d'alerte sur le plan moyen            
     !fd on ecrit tout dans le rep (mid_P,s,t)

     vec(:)=PRcd%vertex(:,j)-mid_P(:)
     d_cd=DOT_PRODUCT(vec(:),mid_normal(:))

     r1(:)=vec(:) - (d_cd*mid_normal(:))

     ! Passage du repere general au repere du plan moyen (s,t)

     rr1(1,inc)=s(1)*r1(1)+s(2)*r1(2)+s(3)*r1(3)
     rr1(2,inc)=t(1)*r1(1)+t(2)*r1(2)+t(3)*r1(3)

   ENDDO

   nb_vertex_prcd_min=inc

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'PR cd ',polyr2bdyty(2,PRcd%id),' @ ',polyr2bdyty(1,PRcd%id)
     print*,'vertex projetes ', nb_vertex_prcd_min
   endif

   ALLOCATE(Pcd(2,nb_vertex_prcd_min),stat=errare)
   Pcd = 0.d0

   !tableau d'indice pour la suite
   allocate(id_Pcd(nb_vertex_prcd_min+1),stat=errare)
   id_Pcd(1:nb_vertex_prcd_min)= (/ (i,i=1,nb_vertex_prcd_min) /) 
   id_Pcd(nb_vertex_prcd_min+1)= 1

   Pcd=rr1(:,1:nb_vertex_prcd_min)

   deallocate(rr1)

   !fd TODO ce shrink la devrait automatique et tres faible pour gommer les 0 numeriques

   if (shrink > 0.d0) then

     !fd le 1 est forcement dans la liste
     inc=1
     c_center = Pcd(1:2,id_Pcd(1))

     do i=2,nb_vertex_prcd_min+1
       if (id_Pcd(i) /= 0 .and. id_Pcd(i) /= id_Pcd(1)) then
         inc = inc + 1
         c_center = c_center + Pcd(1:2,id_Pcd(i))
       endif
     enddo
     c_center = c_center / real(inc,8)

     !fd on shrink tous les points et pas juste le convex_hull
     do i=1,nb_vertex_prcd_min
       c_vec(1:2) = Pcd(1:2,i) - c_center(1:2)  
       Pcd(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
     enddo

     if (bavard) then
       print*,'vertex candidat enveloppe convexe shrink (loop)'
       do i=1,nb_vertex_prcd_min+1
         if (id_Pcd(i) /= 0) print *,Pcd(:,id_Pcd(i))
       enddo
     endif

   endif

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,'nb vertex candidat dans contour (avec shrink)', nb_vertex_prcd_min
     !print*,'avant'
     do i=1,nb_vertex_prcd_min
       print*,Pcd(:,i)
     enddo

     !print*,'apres'
     !do i=1,nb_vertex_prcd_min+1
     !  if (id_Pcd(i) /= 0) print*,Pcd(:,id_Pcd(i))
     !enddo
   endif



!fd @@@                              end                                           @@@
! @@@@@@@@@@@@@@@@@@@@ projection + enveloppe convexe @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES enveloppes convexes @@@@@@@@@@@@@@@@@@@@@
!fd @@@                              begin                                         @@@


   norm_min=MIN(PRan%min_radius_face,PRcd%min_radius_face) 
   norm_max=MAX(PRan%max_radius_face,PRcd%max_radius_face) 

   ref_size=norm_min

   if (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan)) then 
     call polytopes_intersection_wc(Pan,id_Pan,Pcd,id_Pcd,points,face_sizes,cp_an_shrink,cp_cd_shrink,cp_delta,area,.true.,err_)
   else
     call polytopes_intersection_wc(Pan,id_Pan,Pcd,id_Pcd,points,face_sizes,cp_an_shrink,cp_cd_shrink,cp_delta,area,.false.,err_)
   endif

   if( associated(face_sizes) ) then
     nb_ptc = sum(face_sizes)
   else
     nb_ptc = 0
   end if

   if (err_ > 0) then
     write(cout,'("PRcd ",I0," PRan ",I0)') PRcd%id,PRan%id
     call logmes(cout, .true.)
     call faterr(IAM,'unexpected problem in node HE proximity')
   endif   
   
   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,PRan%id,PRcd%id,'nombre d''intersection', nb_ptc
     print*,'ref size',ref_size
     print*,'points de l''intersection dans le plan'
     do i=1,nb_ptc
       print*,points(:,i)  
     enddo 
   endif 

   !rm : store the intersection polytope and compute area
   if (nb_ptc > 2) then
     
     if( associated(face_ctc)   ) deallocate(face_ctc)

     allocate(face_ctc(3,nb_ptc))
     do i = 1, nb_ptc
       face_ctc(:,i) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))
     end do
     call glob2loc_frame_POLYR(PRan%id, face_ctc)

   else
     area = 0.d0
   endif   


   !fd essaie pour virer les noeuds en trop
   if ( nb_ptc > 4 ) then
     
     allocate(ptc(2,nb_ptc),id_ptc(nb_ptc+1))  
     ptc(1:2,1:nb_ptc) = points(1:2,1:nb_ptc)
     id_ptc = 0

     !fd on calcule l'enveloppe convexe pour ne garder que ca 
     !fd probablement pas utile 
     !fd par contre en est sur que ca tourne dans le sens trigo
     call convex_hull(ptc,id_ptc,ref_size, err_)

     if (err_ > 0) then
       call logmes('polytope intersection', .true.)
       call faterr(IAM,'unexpected error in convex hull computation')
     endif   
     

     nb_ptc_qh = count(id_ptc /= 0) - 1

     !fd si l'enveloppe convexe en a encore trop 

     if (nb_ptc_qh > 4) then

       !fd on garde ceux qui ont les angles proches de 90 
       !fd vec1.vec2 / |vec1| |vec2| proche de 0

       !print*,'plus que 4'
       !print*,'indexes qh ',id_ptc

       allocate(angle(nb_ptc_qh))  

       vec1(1:2) = ptc(1:2,id_ptc(1)) - ptc(1:2,id_ptc(nb_ptc_qh))
       vec2(1:2) = ptc(1:2,id_ptc(2)) - ptc(1:2,id_ptc(1))
       angle(1) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
          (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))

       do i=2,nb_ptc_qh
         if (id_ptc(i) == 0) then
           write(cout,'(A)') 'strange id_ptc'
           call logmes(cout, .true.)
           write(cout,'(I0)') id_ptc
           call faterr(IAM,cout) 
         endif 
         !if (id_ptc(i) < 0) cycle

         vec1(1:2) = ptc(1:2,id_ptc(i)) - ptc(1:2,abs(id_ptc(i-1)))
         vec2(1:2) = ptc(1:2,id_ptc(i+1)) - ptc(1:2,id_ptc(i))
         angle(i) = (vec1(1)*vec2(1) + vec1(2)*vec2(2)) / &
            (sqrt(vec1(1)*vec1(1) + vec1(2)*vec1(2)) * sqrt(vec2(1)*vec2(1) + vec2(2)*vec2(2)))
       enddo

       !print*,'indexes qh ',id_ptc   

       do i=1,4
           inc = minloc(array=angle,dim=1)
           angle(inc)=1.d+20
           points(1:2,i) = ptc(1:2,id_ptc(inc))
       enddo

       nb_ptc = 4

       deallocate(angle)

       !print*,nb_ptc

     else 

        !print*,'moins que 4'
        !print*,'indexes qh ',id_ptc

        do i=1,count(id_ptc /= 0) -1
          if (id_ptc(i) == 0) then
            write(cout,'(A)') 'strange id_ptc'
            call logmes(cout, .true.)
            write(cout,'(I0)') id_ptc
            call faterr(IAM,cout) 
          endif 
          points(1:2,i) = ptc(1:2,id_ptc(i))
        enddo
        nb_ptc = count(id_ptc /= 0) -1
     endif

     deallocate(ptc,id_ptc)

   endif

   if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
     print*,PRan%id,PRcd%id,'nombre de points retenus', nb_ptc
     print*,'ref size',ref_size
     print*,'points retenus'
     do i=1,nb_ptc
       print*,points(:,i)  
     enddo 
   endif 

   DEALLOCATE(Pan,id_Pan,Pcd,id_Pcd)

!fd @@@                              end                                           @@@
!fd @@@@@@@@@@@@ CALCUL DE L'INTERSECTION DES eveloppes convexes @@@@@@@@@@@@@@@@@@@@@


!fd @@@@@@@@@@@@ reduction du nombre de noeuds a 4 maxi          @@@@@@@@@@@@@@@@@@@@@

!fd on calcule le centre du nuage.
!fd on vire le plus proche
!fd on recommence ...

   if (nb_ptc > 4) then
     is_ok=1
     allocate(dist(nb_ptc))

     DO i=1,nb_ptc
       PT(:)=PT(:)+points(:,i)
     ENDDO

     PT=PT/REAL(nb_ptc,8)

     DO i=1,nb_ptc
       dist(i)=dot_product(PT(:)-points(:,i),PT(:)-points(:,i))
     ENDDO

     do i=1,4
       inc=maxloc(dist,dim=1)        
       is_ok(inc)=0
       dist(inc)=0.d0
     enddo
   else
     is_ok=0
   endif

   if (bavard) then
     print*,'mid_P ',mid_P
     print*,'s '    ,s
     print*,'t '    ,t 
  
     print*,'point de l''intersection dans le repere global' 
   endif


   !fd on retente un shrink

   inc=0
   c_center = 0.d0
   do i=1,nb_ptc
     if (is_ok(i) /= 0 ) cycle
     inc = inc + 1 
     c_center(1:2) = c_center(1:2) + points(1:2,i)
   enddo
   if (inc /= 0) then
     c_center = c_center / real(inc,8)

     !fd on shrink tous les points 
     do i=1,nb_ptc
       if (is_ok(i) /= 0 ) cycle
       c_vec(1:2) = points(1:2,i) - c_center(1:2)  
       points(1:2,i) = c_center(1:2) + (1.d0 - shrink)*c_vec(1:2)
     enddo

   endif

!fd @@@@@@@@@@@@ construction des points de contact          @@@@@@@@@@@@@@@@@@@@@

   nb_select=0
   do i=1,nb_ptc
     if (is_ok(i) /= 0 ) cycle
     nb_select = nb_select + 1 
   enddo

   if (nb_select < 3) then
     ! fd est ce utile ? print*,'Not enough contact nodes between the surfaces'
     nb_ctc = 0
     return
   else
     nb_ctc=nb_select 
   endif


   nb_select=0
   do i=1,nb_ptc
     if (is_ok(i) /= 0 ) cycle
     nb_select = nb_select + 1 
     PT_CTC(:,nb_select) = mid_P(:) + (points(1,i)*s(:)) + (points(2,i)*t(:))

     if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
       if (nb_select==1) print*,'point de contact rep global:'
       write(*,'(I0,3(1x,E12.5))') nb_select,pt_ctc(:,nb_select)
     endif     
   enddo

   if (associated(points)) deallocate(points)

   DO ic=1,nb_ctc
     ! recherche de la face cd qui contient le noeud projete suivant la direction mid_nornal
     is_inside = .false.
     do iff=1,size(PRcd%f2f_set(fi)%G_i)
       do i=1,3
         pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(iff))) 
       enddo
       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_min, &
                                            weight_face_cd(:,ic),bavard)  
       if (is_inside) then
         my_cd_iff = iff
         exit
       endif
     enddo

     if (.not. is_inside) then
       call logmes('===================================================================================')
       call logmes('Error '//IAM//': unable to find a candidate face')
       write(cout,'(a,i0,a,i0,a,i0)') 'Contact number ',ic,' between POLYR ',PRcd%id,' and POLYR ',PRan%id
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  position ',pt_ctc(:,ic)
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  normal   ',mid_normal
       call logmes(cout)
       write(cout,'(a)') '  the set contains the faces numbered'
       call logmes(cout)
       do iff=1,size(PRcd%f2f_set(fi)%G_i)
         write(cout,'(3x,A,I6,A,3(1x,D12.5),A)') '- ',PRcd%f2f_set(fi)%G_i(iff), &
                                                 '  =  vertex 1 : [',PRcd%vertex(:,PRcd%face(1,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 2 : [',PRcd%vertex(:,PRcd%face(2,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 3 : [',PRcd%vertex(:,PRcd%face(3,PRcd%f2f_set(fi)%G_i(iff))),' ]'
         call logmes(cout)
       enddo

       ! fd: RIP GMV
       !     see obsolete to lookf for
       !     corresponding bit of code


       id_face_cd(ic) = 0

     else
       id_face_cd(ic) = PRcd%f2f_set(fi)%G_i(my_cd_iff)

       !print*,'iff_cd ',id_face_cd(ic)
       !print*,'nodes ',PRcd%face(:,PRcd%f2f_set(fi)%G_i(my_cd_iff))
       !print*,'w ',weight_face_cd(:,ic)

     endif


     is_inside = .false.
     do iff=1,size(PRan%f2f_set(fj)%G_i)
       do i=1,3
         pp(:,i) = PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(iff))) 
       enddo
       is_inside = node_triangle_projection(pp,PT_CTC(:,ic),mid_normal,d_max, &
                                            weight_face_an(:,ic),bavard)  
       if (is_inside) then
         my_an_iff = iff
         exit
       endif
     enddo

     if (.not. is_inside) then
       call logmes('===================================================================================')
       call logmes('Error '//IAM//': unable to find an antagonist face')
       write(cout,'(a,i0,a,i0,a,i0)') 'Contact number ',ic,' between POLYR ',PRcd%id,' and POLYR ',PRan%id
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  position ',pt_ctc(:,ic)
       call logmes(cout)
       write(cout,'(a,3(1x,D12.5))') '  normal   ',mid_normal
       call logmes(cout)
       write(cout,'(a)') '  the set contains the faces numbered'
       call logmes(cout)
       do iff=1,size(PRan%f2f_set(fj)%G_i)
         write(cout,'(4x,A,I6,A,3(1x,D12.5),A)') '- ',PRan%f2f_set(fj)%G_i(iff), &
                                                 '  =  vertex 1 : [',PRan%vertex(:,PRan%face(1,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 2 : [',PRan%vertex(:,PRan%face(2,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
         write(cout,'(17x,A,3(1x,D12.5),A)')          'vertex 3 : [',PRan%vertex(:,PRan%face(3,PRan%f2f_set(fj)%G_i(iff))),' ]'
         call logmes(cout)
       enddo

       ! fd: RIP GMV
       !     see obsolete to lookf for
       !     corresponding bit of code


       !fd on ejecte le noeud
       !fd nb_ctc = 0
       !fd return

       id_face_an(ic) = 0


     else
       id_face_an(ic) = PRan%f2f_set(fj)%G_i(my_an_iff)

       !print*,'iff_an', id_face_an(ic)
       !print*,'nodes ',PRan%face(:,PRan%f2f_set(fj)%G_i(my_an_iff))
       !print*,'w',weight_face_an(:,ic)

     endif


     ! la distance entre les 2 points

     if (id_face_cd(ic) /= 0 .and. id_face_an(ic) /= 0 ) then
       overlap(ic)=d_min-d_max
     else
       id_face_cd(ic) = 0
       id_face_an(ic) = 0
       overlap(ic)=0.d0
       pt_ctc(:,ic)=0.d0
       weight_face_cd(:,ic)=0.d0
       weight_face_an(:,ic)=0.d0
     endif

     if (bavard .or. (dbg .and. (PRcd%id == dbg_idcd .and. PRan%id == dbg_idan))) then 
       print *,'contact: ', ic
       write(*,'(3(1x,E12.5))') pt_ctc(:,ic)
       print *,'distance: ',overlap(ic)
       print *,'distances cd/an au plan',d_min,d_max
       print *,'weight cd',weight_face_cd(:,ic)
       print *,'weight an',weight_face_an(:,ic)
     endif
   enddo

  END SUBROUTINE planplan_contour
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  function get_nb_f2f_PRPRx()
    implicit none
    integer :: get_nb_f2f_PRPRx

    get_nb_f2f_PRPRx = nb_visavis

  end function get_nb_f2f_PRPRx

  subroutine get_f2f2inters_PRPRx(ivector)
    implicit none
    integer, dimension(:), pointer :: ivector
    !
    integer :: i_f2f, ictc, nb_ctc, idx, isize

    ! counting
    isize = 1
    do i_f2f = 1, nb_visavis
      if (.not. get_visible_POLYR(visavis(i_f2f)%cd)  .or. &
          .not. get_visible_POLYR(visavis(i_f2f)%an) ) cycle
      if (.not. visavis(i_f2f)%is_flat) cycle

      nb_ctc = visavis(i_f2f)%nb_ctc

      ! should check if < 3 ?
      if (nb_ctc == 0) cycle

      isize = isize + nb_ctc + 1
    end do

    allocate(ivector(isize))

    ivector(1) = 0
    idx        = 2
    do i_f2f = 1, nb_visavis

      ! fd a voir ... pour virer l'affichage des visavis avec des blocs invisibles
      if (.not. get_visible_POLYR(visavis(i_f2f)%cd)  .or. &
          .not. get_visible_POLYR(visavis(i_f2f)%an) ) cycle

      if (.not. visavis(i_f2f)%is_flat) cycle

      ! on recupere le nombre de points de contact de la structure visavis courante
      nb_ctc = visavis(i_f2f)%nb_ctc
      if (nb_ctc == 0) cycle

      ivector(1) = ivector(1) + 1
      ivector(idx) = nb_ctc
      do ictc = 1, nb_ctc
        ivector(idx+ictc) = visavis(i_f2f)%index(ictc)
      end do
      idx = idx + nb_ctc + 1
    end do
  end subroutine get_f2f2inters_PRPRx

  subroutine get_f2f_outlines(connec, points)
    implicit none
    integer     , dimension(:)  , pointer :: connec
    real(kind=8), dimension(:,:), pointer :: points
    !
    integer :: i_f2f, nb_ctc, isize, nb_pts, p_idx, c_idx

    ! counting
    nb_pts = 0
    isize  = 1

    do i_f2f = 1, nb_visavis
      if (.not. get_visible_POLYR(visavis(i_f2f)%cd)  .or. &
          .not. get_visible_POLYR(visavis(i_f2f)%an) ) cycle

      if (.not. visavis(i_f2f)%is_flat) cycle

      ! should check if < 3 ?
      nb_ctc = visavis(i_f2f)%nb_ctc
      if (nb_ctc == 0) cycle

      if( .not. associated(visavis(i_f2f)%face_sizes) ) then
        call faterr('PRPRx::get_f2f_outlines', 'getting visavis%face_sizes, but not associated')
      end if
      if( .not. associated(visavis(i_f2f)%face_ctc) ) then
        call faterr('PRPRx::get_f2f_outlines', 'getting visavis%face_ctc, but not associated')
      end if

      isize  = isize + 1 + size( visavis(i_f2f)%face_sizes )
      nb_pts = nb_pts + sum( visavis(i_f2f)%face_sizes(:) )

    end do

    allocate(connec(isize))
    allocate(points(3,nb_pts))

    p_idx = 0
    c_idx = 2
    connec(1) = 0

    do i_f2f = 1, nb_visavis

      ! fd a voir ... pour virer l'affichage des visavis avec des blocs invisibles
      if (.not. get_visible_POLYR(visavis(i_f2f)%cd)  .or. &
          .not. get_visible_POLYR(visavis(i_f2f)%an) ) cycle

      if (.not. visavis(i_f2f)%is_flat) cycle

      ! on recupere le nombre de points de contact de la structure visavis courante
      nb_ctc = visavis(i_f2f)%nb_ctc
      if (nb_ctc == 0) cycle

      connec(1) = connec(1) + 1


      isize = size( visavis(i_f2f)%face_sizes )

      connec(c_idx) = isize
      connec(c_idx+1:c_idx+isize) = visavis(i_f2f)%face_sizes(:)

      nb_pts = sum( visavis(i_f2f)%face_sizes(:) )
      points(1:3,p_idx+1:p_idx+nb_pts) = visavis(i_f2f)%face_ctc(:,:)
      call loc2glob_frame_POLYR(visavis(i_f2f)%an, points(1:3,p_idx+1:p_idx+nb_pts))

      p_idx = p_idx + nb_pts
      c_idx = c_idx + isize+1

    end do

  end subroutine get_f2f_outlines

  subroutine get_f2f_all_idata(idata)
    implicit none
    integer, dimension(:,:), pointer :: idata
    !
    integer :: i_f2f

    if( associated(idata) ) then
      deallocate(idata)
      nullify(idata)
    end if

    allocate( idata(4,nb_visavis) )
    do i_f2f = 1, nb_visavis
      idata(1,i_f2f) = visavis(i_f2f)%cd
      idata(2,i_f2f) = visavis(i_f2f)%an
      idata(3,i_f2f) = visavis(i_f2f)%id_f_cd
      idata(4,i_f2f) = visavis(i_f2f)%id_f_an
    end do

  end subroutine get_f2f_all_idata

  !-------------------------------------------------------------------- 
  subroutine set_reaction_tracking_length_PRPRx(length)
    IMPLICIT NONE

    REAL(KIND=8) :: length

    is_reaction_tracking_length_setted = .true.
    reaction_tracking_length = length

  end subroutine set_reaction_tracking_length_PRPRx
  !-------------------------------------------------------------------- 

  !------------------------------------------------------------------------
  !> computes the reaction of contact (ibdy,itac)-(jbdy,jtac) on ibdy 
  subroutine pair_reaction_PRPRx(ipolyr,ibdy,itac,jpolyr,jbdy,jtac,local_reac)   
    implicit none
    integer :: ipolyr,ibdy,itac,jpolyr,jbdy,jtac,nbc,i,ic,idx,icdan
    real(kind=8), dimension(6) :: local_reac

    logical :: skip_tangential_torque=.true.

    local_reac = 0.d0

    !print*,'pair_reaction_PRPRx'
    !print*,ipolyr,ibdy,itac,jpolyr,jbdy,jtac

    ! ipolyr comme le cdtac
    if (verlt(ipolyr)%adjsz /= 0) then
      ! on compte les contacts qui voient jbdy 
      nbc = count(verlt(ipolyr)%anbdy == jbdy) 
      !print*,'---------'
      !print*,ipolyr,ibdy,itac,jpolyr,jbdy,jtac,nbc
      !print*,verlt(ipolyr)%cdbdy
      !print*,verlt(ipolyr)%cdtac
      !print*,verlt(ipolyr)%anbdy
      !print*,verlt(ipolyr)%antac
      if (nbc /= 0) then
        idx=1
        do i=1,nbc
          ! on recupere un contact avec le bon jbdy           
          ic = minloc(verlt(ipolyr)%anbdy(idx:verlt(ipolyr)%adjsz),dim=1, &
                      mask=verlt(ipolyr)%anbdy(idx:verlt(ipolyr)%adjsz)==jbdy)
          ! si c'est le bon contacteur de jbdy on fait qqch
          if (verlt(ipolyr)%antac(ic) == jtac) then           
            ic = idx + ic - 1 
            icdan = verlt(ipolyr)%icdan(ic)

            !print*,'> ',ic,icdan

            local_reac(1) = local_reac(1) + &
                            THIS(ICDAN)%RLS*this(icdan)%suc(1) + &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(1) + & 
                            THIS(ICDAN)%RLN*this(icdan)%nuc(1)
            local_reac(2) = local_reac(2) + &
                            THIS(ICDAN)%RLS*this(icdan)%suc(2) + &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(2) + &
                            THIS(ICDAN)%RLN*this(icdan)%nuc(2)
            local_reac(3) = local_reac(3) + &
                            THIS(ICDAN)%RLS*this(icdan)%suc(3) + &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(3) + &
                            THIS(ICDAN)%RLN*this(icdan)%nuc(3)
            if (skip_tangential_torque) then
              local_reac(4) = local_reac(4) + &
                              this(icdan)%Gcdn(1)*THIS(ICDAN)%RLN
              local_reac(5) = local_reac(5) + &
                              this(icdan)%Gcdn(2)*THIS(ICDAN)%RLN
              local_reac(6) = local_reac(6) + &
                              this(icdan)%Gcdn(3)*THIS(ICDAN)%RLN
            else 
              local_reac(4) = local_reac(4) + &
                              this(icdan)%Gcds(1)*THIS(ICDAN)%RLS + &
                              this(icdan)%Gcdt(1)*THIS(ICDAN)%RLT + &
                              this(icdan)%Gcdn(1)*THIS(ICDAN)%RLN
              local_reac(5) = local_reac(5) + &
                              this(icdan)%Gcds(2)*THIS(ICDAN)%RLS + &
                              this(icdan)%Gcdt(2)*THIS(ICDAN)%RLT + &
                              this(icdan)%Gcdn(2)*THIS(ICDAN)%RLN
              local_reac(6) = local_reac(6) + &
                              this(icdan)%Gcds(3)*THIS(ICDAN)%RLS + &
                              this(icdan)%Gcdt(3)*THIS(ICDAN)%RLT + &
                              this(icdan)%Gcdn(3)*THIS(ICDAN)%RLN
            endif
          endif
          idx = ic + 1
        enddo  
      endif
    endif
    ! jpolyr comme le cdtac
    if (verlt(jpolyr)%adjsz /= 0) then
      ! on compte les contacts qui voient ibdy         
      nbc = count(verlt(jpolyr)%anbdy == ibdy) 
      !print*,jpolyr,jbdy,jtac,ipolyr,ibdy,itac,nbc
      if (nbc /= 0) then
        idx=1
        do i=1,nbc
          ! on recupere un contact avec le bon ibdy
          ic = minloc(verlt(jpolyr)%anbdy(idx:verlt(jpolyr)%adjsz),dim=1, &
                      mask=verlt(jpolyr)%anbdy(idx:verlt(jpolyr)%adjsz)==ibdy)
          ! si c'est le bon contacteur de ibdy on fait qqch
          if (verlt(jpolyr)%antac(ic) == itac) then           
            ic = idx + ic - 1 
            icdan = verlt(jpolyr)%icdan(ic)

            !print*,'> ',ic,icdan

            local_reac(1) = local_reac(1) - &
                            THIS(ICDAN)%RLS*this(icdan)%suc(1) - &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(1) - & 
                            THIS(ICDAN)%RLN*this(icdan)%nuc(1)
            local_reac(2) = local_reac(2) - &
                            THIS(ICDAN)%RLS*this(icdan)%suc(2) - &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(2) - &
                            THIS(ICDAN)%RLN*this(icdan)%nuc(2)
            local_reac(3) = local_reac(3) - &
                            THIS(ICDAN)%RLS*this(icdan)%suc(3) - &
                            THIS(ICDAN)%RLT*this(icdan)%tuc(3) - &
                            THIS(ICDAN)%RLN*this(icdan)%nuc(3)
            if (skip_tangential_torque) then
              local_reac(4) = local_reac(4) - &
                              this(icdan)%Gcdn(1)*THIS(ICDAN)%RLN
              local_reac(5) = local_reac(5) - &
                              this(icdan)%Gcdn(2)*THIS(ICDAN)%RLN
              local_reac(6) = local_reac(6) - &
                              this(icdan)%Gcdn(3)*THIS(ICDAN)%RLN
            else
              local_reac(4) = local_reac(4) - &
                              this(icdan)%Gcds(1)*THIS(ICDAN)%RLS - &
                              this(icdan)%Gcdt(1)*THIS(ICDAN)%RLT - &
                              this(icdan)%Gcdn(1)*THIS(ICDAN)%RLN
              local_reac(5) = local_reac(5) - &
                              this(icdan)%Gcds(2)*THIS(ICDAN)%RLS - &
                              this(icdan)%Gcdt(2)*THIS(ICDAN)%RLT - &
                              this(icdan)%Gcdn(2)*THIS(ICDAN)%RLN
              local_reac(6) = local_reac(6) - &
                              this(icdan)%Gcds(3)*THIS(ICDAN)%RLS - &
                              this(icdan)%Gcdt(3)*THIS(ICDAN)%RLT - &
                              this(icdan)%Gcdn(3)*THIS(ICDAN)%RLN
            endif
          endif
          idx = ic + 1
        enddo  
      endif
    endif

    local_reac = local_reac/H

  end subroutine
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  !am: declaration fonctions necessaires a la DDM  
  !am: all trivial tests (autocontacts, etc) are supposed to be made, 
  !am so that using a linked list is no more necessary ^^
  subroutine set_anonymous_to_rough_PRPRx(anonymous_rough, nb_anonymous_rough)
   implicit none

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! inputs 
   type(PTR_CONTAINER), intent(in)   :: anonymous_rough
   integer(kind=4)    , intent(in)   :: nb_anonymous_rough
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! implicit outputs (global variables set by this function) :
   ! type(T_rough_PRPRx), dimension(:), allocatable :: rough_PRPRx 
   ! integer(kind=4)                                :: nb_rough_PRPRx 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! local variables
   integer                                 :: icdan,icdtac,iantac,isee,group
   integer                                 :: nb_rough_tmp
   character(len=5)                        :: ancol,cdcol
   integer(kind=4),  dimension(:), pointer :: cdan              ! "i4" vector of the anonymous object
   real(kind=8), dimension(:), pointer     :: sep               ! "r8" vector of the anonymous object
   type(T_object)                          :: anonymous_contact ! used to visit "anonymous_rough"
 
   ! local variables used to compute nb_rough_half
   real(kind=8)                            :: dist, adist
   real(kind=8)                            :: raycd,rayan

   ! local variable used to compute size of the array this
   integer                                 :: size_of_this
   character(len=80)                       :: cout 

   ! The number of rough contacts is exactly the size of anonyous_contact
   nb_rough_PRPRx = nb_anonymous_rough

   IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)
   ALLOCATE(rough_PRPRx(nb_rough_PRPRx))     ! the visibility array used in compute_contact is allocated

   ! This step will store roughly detetcted interactions in rough_PRPRx. This step will also compute nb_rough_half.
   nb_rough_half=0
   do icdan=1,nb_anonymous_rough
      ! Object associated to icdan index is got
      anonymous_contact = get_object(anonymous_rough,icdan)    
      ! The corresponding "candidate index"/"natagoniste index" couple is got from "anonymous_contact"
      cdan => get_i4_vector(anonymous_contact)
      icdtac=cdan(1)
      iantac=cdan(2)

      ! The group to which belongs the interaction is got from "anonymous_contact"
      group=cdan(3)
      
      ! Looking for the interaction law corresponding to the current interaction
      cdcol = get_color_POLYR(icdtac)
      ancol = get_color_POLYR(iantac)
      if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
        isee = get_isee_specific('POLYR',cdcol,ancol)
      else
        isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                        get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
      end if

      ! paranoid check
      !if (isee==0) ...

      ! update nb_rough_half

      ! compute the corrected alert distance
      adist=see(isee)%alert

      raycd=get_radius_POLYR(icdtac)
      rayan=get_radius_POLYR(iantac)

      adist=0.1005d+01*(adist + raycd + rayan)

      ! get the separation
      sep => get_r8_vector(anonymous_contact)
      dist=DOT_PRODUCT(sep, sep)

      ! update nb_rough_half
      !fd @@@ half half !? on est au carre alors pourquoi pas 0.25 ?
      IF (dist < 0.5d0*adist*adist) nb_rough_half=nb_rough_half + 1

      ! The new interaction is stored
      rough_PRPRx(icdan)%cd         = icdtac
      rough_PRPRx(icdan)%an         = iantac
      rough_PRPRx(icdan)%isee       = isee
      rough_PRPRx(icdan)%Vsep(1:3)  = sep/dsqrt(dist)
      rough_PRPRx(icdan)%group      = group
      ! Periodic case is not supported yet!
      rough_PRPRx(icdan)%xperiodic  = 0
      rough_PRPRx(icdan)%yperiodic  = 0
   end do

   WRITE(cout,'(4X,I10,A20)') nb_rough_PRPRx,' PRPRx roughly found'       
   call logmes(cout)
                                             !1234567890123456789012345
   WRITE(cout, '(4X,I10,A25)') nb_rough_half,' POLYR half roughly found'
   call logmes(cout)

   ! "this" array is allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)

   IF (nb_rough_half==0) nb_rough_half=nb_rough_PRPRx

   size_of_this=INT(ABS(size_factor*nb_rough_half))

!   IF (size_of_this==0) size_of_this=4*nb_rough_half

   ALLOCATE(this(size_of_this))            ! the oversized array this is temporaly allocated

   nb_rough_half=size_of_this

  end subroutine set_anonymous_to_rough_PRPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  !am: all trivial tests (autocontact, etc) are supposed to be made, so that using a linked list is not necessary ^^
  subroutine set_interactions_to_rough_PRPRx(interactions, nb_interactions)
   implicit none

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! inputs 
   integer(kind=4)                              , intent(in)   :: nb_interactions
   integer(kind=4), dimension(3*nb_interactions), intent(in)   :: interactions
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! implicit outputs (global variables set by this function) :

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! local variables
   integer                                 :: icdan,icdtac,iantac,isee,group
   real(kind=8)                            :: masscd,massan,raycd,rayan
   real(kind=8)                            :: adist, dist
   real(kind=8), dimension(3)              :: sep
   character(len=5)                        :: cdcol,ancol
   integer                                 :: size_of_this
   character(len=80)                       :: cout

   ! The number of rough contacts is exactly the size of anonyous_contact
   nb_rough_PRPRx = nb_interactions

   WRITE(cout, '(4X,I10,A20)') nb_rough_PRPRx,' PRPRx roughly found'
   call logmes(cout)

   IF (ALLOCATED(rough_PRPRx)) DEALLOCATE(rough_PRPRx)
   ALLOCATE(rough_PRPRx(nb_rough_PRPRx))     ! the visibility array used in compute_contact is allocated

   ! This step will store roughly detetcted interactions in rough_PRPRx.

   ! This step will eliminate autocontacts or interactions associated to no interaction law
   ! That's why nb_rough_PRPRx <= nb_anonymous_rough
   nb_rough_half=0
   do icdan=1,nb_rough_PRPRx

      icdtac=interactions(3*(icdan-1)+1)
      iantac=interactions(3*(icdan-1)+2)

      ! The group to which belongs the interaction is got from "anonymous_contact"
      group=interactions(3*(icdan-1)+3)
      
      ! Looking for the interaction law corresponding to the current interaction
      cdcol = get_color_POLYR(icdtac)
      ancol = get_color_POLYR(iantac)
      if( polyr2bdyty(3,iantac) == polyr2bdyty(3,icdtac) ) then
        isee = get_isee_specific('POLYR',cdcol,ancol)
      else
        isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                        get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)
      end if

      ! paranoid check
      !if (isee==0) ...

      ! update nb_rough_half

      ! compute the corrected alert distance
      adist=see(isee)%alert

      raycd=get_radius_POLYR(icdtac)
      rayan=get_radius_POLYR(iantac)

      adist=0.1005d+01*(adist + raycd + rayan)

      ! compute the separation
      sep = PRcoor(:, icdtac) - PRcoor(:, iantac)
      dist=DOT_PRODUCT(sep, sep)

      ! update nb_rough_half
      !fd @@@ half half !? on est au carre alors pourquoi pas 0.25 ?
      IF (dist < 0.5d0*adist*adist) nb_rough_half=nb_rough_half + 1

      ! The new interaction is stored
      rough_PRPRx(icdan)%cd         = icdtac
      rough_PRPRx(icdan)%an         = iantac
      rough_PRPRx(icdan)%isee       = isee
      rough_PRPRx(icdan)%Vsep(1:3)  = sep/dsqrt(dist)
      rough_PRPRx(icdan)%group      = group

      ! still not supported yet
      rough_PRPRx(icdan)%xperiodic  = 0
      rough_PRPRx(icdan)%yperiodic  = 0 
   end do

   WRITE(cout, '(4X,I10,A20)') nb_rough_half,' POLYR half roughly found'
   call logmes(cout)

   ! "this" array is allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)

   IF (nb_rough_half==0) nb_rough_half=nb_rough_PRPRx

   size_of_this=INT(ABS(size_factor*nb_rough_half))

!   IF (size_of_this==0) size_of_this=4*nb_rough_half

   ALLOCATE(this(size_of_this))            ! the oversized array this is temporaly allocated

   nb_rough_half=size_of_this

  end subroutine set_interactions_to_rough_PRPRx
  !--------------------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------------------
  subroutine put_icdan_group_PRPRx(icdan,group)

   implicit none

   integer, intent(in) :: icdan, group

   this(icdan)%group=group 

  end subroutine put_icdan_group_PRPRx
  !--------------------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------------------
  SUBROUTINE get_nb_INTRF_PRPRx(nb_INTRF, list_bdyty)

   implicit none

   integer, dimension(:), intent(in), optional :: list_bdyty
   integer, intent(out) :: nb_INTRF ! Nombre de "contacts d'interface",
                                    ! i.e. de contacts dont le %cd et/ou
                                    ! le %an sont taggés "INTRF".
   integer :: i, j

   nb_INTRF=0

   if (nb_PRPRx == 0) return

   do i = 1, nb_PRPRx
      if ( this(i)%group == INTRF ) nb_INTRF=nb_INTRF+1
   end do

  END SUBROUTINE get_nb_INTRF_PRPRx
  !--------------------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------------------
  SUBROUTINE get_list_INTRF_PRPRx(nb_INTRF, liste_INTRF)

   implicit none

   integer, intent(in) :: nb_INTRF ! Nombre de "contacts d'interface",
                                   ! i.e. de contacts dont le %cd et/ou
                                   ! le %an sont taggés "INTRF".

   integer, dimension(nb_INTRF), intent(out) :: liste_INTRF

   integer           :: i, compteur
                             !123456789012345678901
   CHARACTER(len=21) :: IAM= 'PRPRx::get_list_INTRF'

   compteur=0
   do i = 1, nb_PRPRx
      if ( this(i)%group == INTRF ) then
         compteur = compteur + 1
         if (compteur>nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")
         liste_INTRF(compteur) = i
      end if
   end do

   if (compteur/=nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")

  END SUBROUTINE get_list_INTRF_PRPRx
  !--------------------------------------------------------------------------------------------------
  
  !--------------------------------------------------------------------------------------------------
  subroutine set_tol_recup_rloc_PRPRx(tol)
   implicit none
   real(kind=8), intent(in) :: tol

   tol_recup_rloc = tol
  end subroutine
  !--------------------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_vector_PRPRx(name,icdan,vect,sz)
   IMPLICIT NONE

   INTEGER :: icdan,sz
   REAL(kind=8),DIMENSION(sz) :: vect
   CHARACTER(len=5) :: name
   CHARACTER(len=80) :: cout

                              !12345678901234567890123456780'
   CHARACTER(len=30) :: IAM = 'PRPRx::get_interaction_vector'

   if (icdan > nb_PRPRx) call FATERR(IAM,'given PRPRx index greater than number of PRPRx')

   SELECT CASE(name)
   CASE('Coor_')
     vect=this(icdan)%coor
   CASE('N____')
     vect=this(icdan)%nuc
   CASE DEFAULT
     WRITE(cout,'(A,1x,A)') 'Sorry unknown id:',name
     CALL faterr(IAM,cout)
   END SELECT

  end subroutine 
  !--------------------------------------------------------------------------------------------------  
 
  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_internal_PRPRx(i,icdan,rvalue)
   IMPLICIT NONE

   INTEGER :: i,icdan
   REAL(kind=8) :: rvalue

   ! ***
                              !1234567890123456789012345678012'
   CHARACTER(len=32) :: IAM = 'PRPRx::get_interaction_internal'

   if (icdan > nb_PRPRx) call FATERR(IAM,'given PRPRx index greater than number of PRPRx')
   if (i > max_internal_tact ) call FATERR(IAM,'given internal index greater than max number of internal')

   rvalue = this(icdan)%internal(i)

  end subroutine 
  !--------------------------------------------------------------------------------------------------
 
  !--------------------------------------------------------------------------------------------------
  subroutine set_interaction_internal_PRPRx(i,icdan,rvalue)
   IMPLICIT NONE

   INTEGER :: i,icdan
   REAL(kind=8) :: rvalue

   ! ***
                              !1234567890123456789012345678012'
   CHARACTER(len=32) :: IAM = 'PRPRx::set_interaction_internal'

   if (icdan > nb_PRPRx) call FATERR(IAM,'given PRPRx index greater than number of PRPRx')
   if (i > max_internal_tact ) call FATERR(IAM,'given internal index greater than max number of internal')

   this(icdan)%internal(i)=rvalue 

  end subroutine 
  !--------------------------------------------------------------------------------------------------
 
  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_internal_comment_PRPRx(icdan,cvect)
   IMPLICIT NONE

   INTEGER :: icdan
   CHARACTER(len=100) :: cvect

   ! ***
                              !123456789012345678901234567801234567890'
   CHARACTER(len=40) :: IAM = 'PRPRx::get_interaction_internal_comment'

   if (icdan > nb_PRPRx) call FATERR(IAM,'given PRPRx index greater than number of PRPRx')

   cvect=''
   IF (this(icdan)%nb_internal /= 0) cvect=get_internal_comment(this(icdan)%lawnb)
 
  end subroutine get_interaction_internal_comment_PRPRx
  !--------------------------------------------------------------------------------------------------
 
  !--------------------------------------------------------------------------------------------------
  subroutine with_nodal_contact_PRPRx()
   implicit none
   nodal_contact = .TRUE.
  end subroutine
  !--------------------------------------------------------------------------------------------------  
 
  !--------------------------------------------------------------------------------------------------
  !--------------------------------------------------------------------------------------------------
  subroutine get_external_pressure_PRPRx(icdan,pext)
    implicit none
    integer(kind=4) :: icdan
    real(kind=8)    :: pext
   
    pext=0.d0

  end subroutine get_external_pressure_PRPRx

  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine print_info_PRPRx(icdan)
     implicit none
     integer          :: icdan,icdtac,iantac,icdbdy,ianbdy
  
     character(len=80) :: cout
  
     icdtac=this(icdan)%icdtac
     iantac=this(icdan)%iantac
  
     write(cout,1) icdtac,iantac
     call LOGMES(cout)
  
1    format(1X,'POLYR:',1x,I0,1x,'POLYR:',1x,I0)
  
     icdbdy=this(icdan)%icdbdy
     ianbdy=this(icdan)%ianbdy
  
     call print_info_POLYR(icdtac)
     call print_info_POLYR(iantac)
  
  end subroutine print_info_PRPRx
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------    
  subroutine set_nb_PRPRx(nb)
    implicit none
    integer(kind=4), intent(in) :: nb

    if( allocated(this) ) then
      deallocate(this)
    end if

    allocate( this(nb) )

    nb_PRPRx = nb

  end subroutine
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine redo_nb_adj_PRPRx()
    implicit none

    call redo_nb_adj_( get_nb_POLYR() )

  end subroutine
  !--------------------------------------------------------------------------------------------------  

  !--------------------------------------------------------------------------------------------------  
  logical function update_cohesive_existing_interaction(icdtac,iantac,perio_shift,max_nb_contacts,nb_ctc,pt_ctc,t,n,s,overlap)
    implicit none
    integer                                     :: icdtac,iantac,max_nb_contacts
    real(kind=8),dimension(3)                   :: perio_shift
    ! number of contact points found (always < 4)
    integer, intent(inout)                        :: nb_ctc
    ! coordinates of contact points...
    real(kind=8), dimension(3,max_nb_contacts), intent(inout)   :: pt_ctc
    ! normal of local frame
    real(kind=8), dimension(3)  , intent(inout)   :: t,n,s
    ! gap of each found contact points
    real(kind=8), dimension(max_nb_contacts)  , intent(out)   :: overlap

    !***
    integer          :: iadj,i,j
    character(len=5) :: status
    logical          :: found
    character(len=80):: cout
    TYPE(T_POLYR)    :: PRan,PRcd
    real(kind=8)     :: cdcooref(3),ancooref(3),cdcoor(3),ancoor(3)
    real(kind=8)     :: localframe_cd(3,3),localframe_an(3,3),localframeb_cd(3,3),localframeb_an(3,3)
    real(kind=8)     :: tb(3),nb(3),sb(3),R(3,3),vec(3)
    logical          :: bavard=.FALSE.
    
                            !123456789012345678901234567890123456
    character(len=36):: IAM='update_cohesive_existing_interaction'

    update_cohesive_existing_interaction=.FALSE.
    nb_ctc=0

    ! ce truc ne marche pas avec du f2f ; a corriger
    if (with_f2f) return 
    
    IF (verlt(icdtac)%adjsz /= 0) THEN

      if (verlt(icdtac)%cdbdy  == polyr2bdyty(1,icdtac) .and. &
          verlt(icdtac)%cdtac  == polyr2bdyty(2,icdtac) .and. &
          verlt(icdtac)%cdmodel== polyr2bdyty(3,icdtac) ) then

        ! on cherche un contact cohesif entre les deux objets
        found=.FALSE.
        do iadj = 1, verlt(icdtac)%adjsz
          if (verlt(icdtac)%anbdy(iadj)  == polyr2bdyty(1,iantac) .and. &
              verlt(icdtac)%antac(iadj)  == polyr2bdyty(2,iantac) .and. &
              verlt(icdtac)%anmodel(iadj)== polyr2bdyty(3,iantac) ) then
             
            if (verlt(icdtac)%status(iadj)==i_Cnnow .or. &
                verlt(icdtac)%status(iadj)==i_Cnctc .or. &
                verlt(icdtac)%status(iadj)==i_Cstck .or. &
                verlt(icdtac)%status(iadj)==i_Cslid      & 
               ) then
               
               found=.TRUE.
            endif
            nb_ctc = nb_ctc +1

          endif  
        enddo

        if (.not. found) return

        if (nb_ctc > max_nb_contacts) call FATERR(IAM,'nb_ctc greater than max number of contacts') 

        if (bavard) then
           print*,'-------------'
           write(cout,'(A,I0,A)') 'found ',nb_ctc,' cohesive contacts'
           call logmes(cout)
           print*,'-------------'
        endif

        
        ! on cherche un contact cohesif entre les deux objets
        nb_ctc = 0
        do iadj = 1, verlt(icdtac)%adjsz
          if (verlt(icdtac)%anbdy(iadj)  == polyr2bdyty(1,iantac) .and. &
              verlt(icdtac)%antac(iadj)  == polyr2bdyty(2,iantac) .and. &
              verlt(icdtac)%anmodel(iadj)== polyr2bdyty(3,iantac) ) then

            nb_ctc = nb_ctc +1

            localframe_cd = get_inertia_frameTT_POLYR(icdtac)
            localframe_an = get_inertia_frameTT_POLYR(iantac)
            localframeb_cd = get_inertia_frameIni_POLYR(icdtac)
            localframeb_an = get_inertia_frameIni_POLYR(iantac)

            !construction matrice de passage b->courant pour l'antagoniste 
            do i=1,3
               do j=1,3
                 R(i,j)=dot_product(localframeb_an(:,i),localframe_an(:,j)) 
               enddo
            enddo                  

            ! on entraine le repere de contact b -> courant
            tb = verlt(icdtac)%tuc(:,iadj)
            nb = verlt(icdtac)%nuc(:,iadj)
            sb = verlt(icdtac)%suc(:,iadj)

            t= matmul(R,tb)
            n= matmul(R,nb)
            s= matmul(R,sb)


            ! print*,icdtac,iantac,nb_ctc


    
            
            ! on construit la coordonnee des points d'arrimage du contact dans le repere courant
            cdcooref = verlt(icdtac)%icdcoor(:,iadj)
            ancooref = verlt(icdtac)%iancoor(:,iadj)

            ! print*,cdcooref,ancooref
            
            cdcoor=PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac)
            ancoor=PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift

            ! print*,cdcoor,ancoor
            
            do i=1,3
              cdcoor(:) = cdcoor(:) + cdcooref(i)*localframe_cd(:,i)
              ancoor(:) = ancoor(:) + ancooref(i)*localframe_an(:,i)
            enddo  

            
            !
            pt_ctc(:,nb_ctc) = ancoor
            
            vec = cdcoor - ancoor

            overlap(nb_ctc) = dot_product(vec,n)

            ! print*,overlap(nb_ctc)
            
          endif
       enddo
       update_cohesive_existing_interaction=.TRUE.


       ! print*,'YEEESS WE KEEP !',nb_ctc
       ! print*,'-------------'
       
      endif   
    endif     
     
  end function update_cohesive_existing_interaction
  !--------------------------------------------------------------------------------------------------    

  subroutine STO_set_explicit_detection_PRPRx(flag)
    implicit none
    logical :: flag

    is_explicit = flag

  end subroutine STO_set_explicit_detection_PRPRx

  subroutine STO_set_decompression_PRPRx(valeur)
    implicit none
    real(kind=8) :: valeur
 
    decompression = valeur

  end subroutine STO_set_decompression_PRPRx

  subroutine STO_compute_contact_PRPRx(reset)
    implicit none
    logical, optional :: reset
    !
    integer :: icdan,iadj,itac
    integer :: icdtac,iantac,isee,i_visavis
    integer :: cd_ent,an_ent
    integer :: errare,group
    integer :: i,j,fi,fj,nb_ctc,size_of_this
    real(kind=8) :: adist,gdist,dist
    real(kind=8) :: norme,t1,t2
    real(kind=8), dimension(3) :: sep
    ! points de contact 
    real(kind=8), dimension(:),   allocatable :: overlap
    real(kind=8), dimension(:,:), allocatable :: xco,n,t,s
    type(T_POLYR)                             :: PRan,PRcd
    ! f2f 
    type(T_visavis)                         :: v2v
    logical                                 :: bavard
                              !12345678901234567890123456
    character(len=26)  :: IAM='PRPRx::STO_compute_contact'
    character(len=80)  :: cout

    logical, save :: is_first_time_f2f = .true.

    !fd je rajoute ca pour pouvoir rendre invisible des corps
    integer,save :: nb_expected_PRPRx

    if ( present(reset)) then
       if (reset) then 
          is_first_time_f2f = .true.
          return
       endif  
    end if

    icdan             = 0
    i_visavis         = 0
    nb_adj            = 0
    nb_PRPRx          = 0

    ! si detection non explicite  OU  que c'est la 1ere detection
    IF ( .not.(is_explicit) .or. (is_explicit .and. is_first_time_f2f) ) THEN

       nb_expected_PRPRx = 0
       nb_detection_test = 0
       detection_time    = 0.D0
    
       IF (nb_rough_PRPRx /= 0 ) THEN

          size_of_this = SIZE(this)
          ! si deja alloue, on nettoie avant la reallocation
          IF ( ALLOCATED(visavis) ) THEN
             DO i = 1,size(visavis)
                if( associated(visavis(i)%iff_cd) )      deallocate(visavis(i)%iff_cd)
                if( associated(visavis(i)%iff_an) )      deallocate(visavis(i)%iff_an)
                if( associated(visavis(i)%cd_lcoor) )    deallocate(visavis(i)%cd_lcoor)
                if( associated(visavis(i)%an_lcoor) )    deallocate(visavis(i)%an_lcoor)
                if( associated(visavis(i)%pt_area) )     deallocate(visavis(i)%pt_area)
                if( associated(visavis(i)%index) )       deallocate(visavis(i)%index)
                if( associated(visavis(i)%face_ctc) )    deallocate(visavis(i)%face_ctc)
                if( associated(visavis(i)%face_sizes) )  deallocate(visavis(i)%face_sizes)
             END DO
             DEALLOCATE(visavis)
          END IF

          ALLOCATE(visavis(size_of_this))

          ! initialization of visavis data structure
          DO i = 1,size_of_this
             NULLIFY( visavis(i)%iff_cd,visavis(i)%iff_an,      &
                      visavis(i)%cd_lcoor,visavis(i)%an_lcoor,  &
                      visavis(i)%index,visavis(i)%pt_area,      &
                      visavis(i)%face_ctc,visavis(i)%face_sizes )
             visavis(i)%nb_ctc = 0
          END DO

          ! preparation de la detection 
          DO i = 1,nb_rough_PRPRx
         
             icdtac = rough_PRPRx(i)%cd
             iantac = rough_PRPRx(i)%an 
             isee   = rough_PRPRx(i)%isee

             !fd a voir
             if ( .NOT. get_visible_POLYR(icdtac) .or. .NOT. get_visible_POLYR(iantac) ) CYCLE

             ! numero des POLYR a suivre pour le debug
             if ( dbg .and. (( icdtac == dbg_idcd .and. iantac == dbg_idan ) .or. &
                             ( icdtac == dbg_idan .and. iantac == dbg_idcd )) ) then
                bavard = .TRUE.
             else
                bavard = .FALSE.
             endif 

             PRcd   = S_POLYR(icdtac)
             PRan   = S_POLYR(iantac)
             
             adist = see(isee)%alert 
             dist  = PRcd%radius + PRan%radius + adist

             perio_shift    = 0.d0
             perio_shift(1) = real(rough_PRPRx(i)%xperiodic,8) * xperiode
             perio_shift(2) = real(rough_PRPRx(i)%yperiodic,8) * yperiode
             
             sep   = PRcd%center - ( PRan%center + perio_shift )
             norme = DOT_PRODUCT(sep,sep)

             IF ( norme < 1.D-24 ) THEN
                write(cout,'(A,I0)')        ' For rough contact ',i 
                call logmes(cout)
                write(cout,'(A,I0,A,I0,A,D14.7)')   ' Distance between cd ',icdtac,' and an ',iantac,' is ',norme
                call logmes(cout)
                write(cout,'(A,3(1x,D14.7))')     '  --> center of cd ',PRcd%center
                call logmes(cout)
                write(cout,'(A,3(1x,D14.7))')     '  --> center of an ',PRan%center
                call logmes(cout)
                call faterr(IAM,"")
             ENDIF 

 !fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
 !fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
 !fd @@@ et il en manque je pense ...

             IF (norme > dist*dist) CYCLE

 !fd @@@ ca n'est pas suffisant non ?
             IF ( ((PRan%maxpos(1)+perio_shift(1)) - PRcd%minpos(1) + adist) < 0.D0 ) CYCLE
             IF ( ((PRan%maxpos(2)+perio_shift(2)) - PRcd%minpos(2) + adist) < 0.D0 ) CYCLE
             IF ( ( Pran%maxpos(3)                 - PRcd%minpos(3) + adist) < 0.D0 ) CYCLE

 !fd @@@ je rajoute ....
             IF ( (PRcd%maxpos(1) - (PRan%minpos(1)+perio_shift(1)) + adist) < 0.D0 ) CYCLE
             IF ( (PRcd%maxpos(2) - (PRan%minpos(2)+perio_shift(2)) + adist) < 0.D0 ) CYCLE
             IF ( (PRcd%maxpos(3) -  PRan%minpos(3)                 + adist) < 0.D0 ) CYCLE
             
             ! on boucle sur toutes les faces topos
             do fi = 1,size(PRcd%f2f_set)
                 do fj = 1,size(PRan%f2f_set)
          
                     CALL cpu_time(t1)
                     
                     CALL STO_DETECTION(iantac,icdtac,isee,fj,fi,nb_ctc,v2v,bavard)
                
                     CALL cpu_time(t2)
                     nb_tot_detect     = nb_tot_detect + 1
                     nb_detection_test = nb_detection_test + 1
                     detection_time    = detection_time + t2 - t1

                     ! si on a trouve des points de contact, on remplit le tableau this
                     IF ( nb_ctc > 0 ) THEN
                     
                         nb_expected_PRPRx  = nb_expected_PRPRx + v2v%nb_ctc
                         i_visavis          = i_visavis + 1
                         visavis(i_visavis) = v2v 
                         nullify( v2v%iff_cd, v2v%iff_an, v2v%cd_lcoor, v2v%an_lcoor, v2v%pt_area, &
                                  v2v%face_ctc, v2v%face_sizes )

                     ENDIF
                     
                 enddo
             enddo

          ENDDO

       ENDIF

       nb_visavis        = i_visavis
    
       WRITE(cout,'(1X,I10,A12)') nb_expected_PRPRx,' PRPRx found'       
       call logmes(cout)
       write(cout,'(1X,A,D8.2)') ' -> Total time of detection   : ',detection_time
       call logmes(cout)
       write(cout,'(1X,A,I8)')   ' -> Number of detection tests : ',nb_detection_test
       call logmes(cout)
       write(cout,'(1X,A,I8)')   ' -> Number of vis a vis       : ',nb_visavis
       call logmes(cout)

    ENDIF

    ! on prend les infos dans le visavis et on calcule les infos des contacts (position, repere, gap)
    DO i = 1,nb_visavis
    
       ! numero des POLYR a suivre pour le debug
       icdtac = visavis(i)%cd
       iantac = visavis(i)%an

       !fd a voir
       if ( .NOT. get_visible_POLYR(icdtac) .or. .NOT. get_visible_POLYR(iantac) ) CYCLE
       
       if ( dbg .and. (( icdtac == dbg_idcd .and. iantac == dbg_idan ) .or. &
                       ( icdtac == dbg_idan .and. iantac == dbg_idcd )) ) then
          bavard = .TRUE.
       else
          bavard = .FALSE.
       endif 
       ! numero des POLYR a suivre pour le debug
        
       call STO_update_f2f(visavis(i),xco,overlap,n,t,s,bavard)
       
       call STO_fill_this_array(icdan,visavis(i),xco,overlap,n,t,s,bavard)
          
       deallocate(xco,overlap,n,t,s)
          
    ENDDO

    nb_PRPRx = icdan
    
    is_first_time_f2f = .false.

    if ( nb_expected_PRPRx /= nb_PRPRx ) then
    !if ( icdan /= nb_PRPRx ) then       
    !   call FATERR(IAM,' error in filling array this, icdan /= nb_PRPRx')
       call LOGMES(IAM)       
       call LOGMES('Warning in filling array this, icdan /= nb_expected_PRPRx')
    endif

    do icdan = 1, nb_PRPRx       
       call get_behaviour_( icdan, see, tact_behav )
    end do

    DO itac = 1,nb_POLYR
      IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
      IF (nb_adj(itac) /= 0) THEN
        ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
        IF (errare /=0 ) THEN
          call faterr(IAM,' error in allocating adjac(icdtac)%.....')
        END IF
      ENDIF
    ENDDO 
  
    DO icdan=1,nb_PRPRx       
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
    END DO 

    IF (ALLOCATED(violation)) DEALLOCATE(violation)
    ALLOCATE(violation(nb_PRPRx),stat=errare)

  end subroutine STO_compute_contact_PRPRx

  subroutine STO_fill_this_array(icdan,v2v,xco,overlap,n,t,s,bavard)
    implicit none  
    integer                                   :: icdan
    real(kind=8), dimension(:),   allocatable :: overlap
    real(kind=8), dimension(:,:), allocatable :: xco,n,t,s
    type(T_visavis)                           :: v2v
    logical                                   :: bavard
    !
    integer                      :: j,icdtac,iantac
    integer                      :: cd_ent,an_ent
    integer                      :: ip,nprop,size_of_this
    real(kind=8)                 :: vls_cst,vln_cst,vlt_cst,adist
    real(kind=8), dimension(3)   :: cdlev,anlev,dV
    real(kind=8), dimension(6)   :: cd_Vbegin,an_Vbegin
    real(kind=8), dimension(3,3) :: Rc,localframe_cd,localframe_an
                              !12345678901234567890123456
    character(len=26)  :: IAM='PRPRx::STO_fill_this_array'

    icdtac = v2v%cd
    iantac = v2v%an 
    
    localframe_cd = get_inertia_frameTT_POLYR(icdtac)
    localframe_an = get_inertia_frameTT_POLYR(iantac)

    cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
    an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
    dV        = cd_Vbegin(1:3) - an_Vbegin(1:3)

    size_of_this = SIZE(this)

    DO j = 1,v2v%nb_ctc

       icdan = icdan + 1

              IF (icdan>size_of_this) THEN
                             !123456789012345678901234567890123456789012345
                 call logmes('---------------------------------------------', .true.)
                 call logmes('ERROR filling this                           ', .true.)
                 call logmes('you rich the allocated size                  ', .true.)
                 call logmes('In your python script use                    ', .true.)
                 call logmes('                                             ', .true.)
                 call logmes('PRPRx_LowSizeArrayPolyr(sizefactor)          ', .true.)
                 call logmes('                                             ', .true.)
                 call logmes('where sizefactor is an integer specifiyng the', .true.)
                 call logmes('ratio of memory you need (=4 by default)     ', .true.)
                 call logmes('---------------------------------------------', .true.)

                 call faterr(IAM,'Error')
              ENDIF   

       v2v%index(j)            = icdan

       this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
       this(icdan)%ianbtac = polyr2bdyty(2, iantac)
       this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
       this(icdan)%ianbtyp = polyr2bdyty(3, iantac)
       this(icdan)%icdctyp = i_polyr
       this(icdan)%ianctyp = i_polyr

       nb_adj(icdtac)          = nb_adj(icdtac)+1
       this(icdan)%iadj        = nb_adj(icdtac)
       this(icdan)%icdbdy      = polyr2bdyty(1,icdtac)
       this(icdan)%icdtac      = icdtac
       this(icdan)%ianbdy      = polyr2bdyty(1,iantac)
       this(icdan)%iantac      = iantac
       this(icdan)%isee        = v2v%isee
       this(icdan)%nuc         = n(:,j)
       this(icdan)%tuc         = t(:,j)
       this(icdan)%suc         = s(:,j)

       this(icdan)%coor        = xco(:,j)
       this(icdan)%type_ctc    = v2v%nb_ctc
       this(icdan)%gapTTbegin  = overlap(j)

       cd_ent                  = get_ent_POLYR(icdtac)
       an_ent                  = get_ent_POLYR(iantac) 
       this(icdan)%icdent      = cd_ent
       this(icdan)%ianent      = an_ent
       entity(cd_ent)%nb       = entity(cd_ent)%nb + 1
       entity(an_ent)%nb       = entity(an_ent)%nb + 1
       
! fd le 11/09/08 manque le shift. PRcoor c'est le centre du polyr pas le centre d'inertie
       cdlev = xco(1:3,j) - ( PRcoor(1:3,icdtac) - get_shiftTT_POLYR(icdtac)                        )
       anlev = xco(1:3,j) - ( PRcoor(1:3,iantac) - get_shiftTT_POLYR(iantac) + v2v%perio_shift(1:3) )

! fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
! fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

       this(icdan)%icdcoor = MATMUL( TRANSPOSE(localframe_cd), cdlev)
       this(icdan)%iancoor = MATMUL( TRANSPOSE(localframe_an), anlev)

       ! On va calculer le passage rep inertie -> rep general pour l'antagoniste
       Rc(1,1) = localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
       Rc(2,1) = localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
       Rc(3,1) = localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

       Rc(1,2) = localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
       Rc(2,2) = localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
       Rc(3,2) = localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

       Rc(1,3) = localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
       Rc(2,3) = localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
       Rc(3,3) = localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)

       this(icdan)%Gant = MATMUL(Rc, this(icdan)%tuc)
       this(icdan)%Gann = MATMUL(Rc, this(icdan)%nuc)
       this(icdan)%Gans = MATMUL(Rc, this(icdan)%suc)

       ! On va calculer le passage rep inertie -> rep general pour le candidat
       Rc(1,1) = localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
       Rc(2,1) = localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
       Rc(3,1) = localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

       Rc(1,2) = localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
       Rc(2,2) = localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
       Rc(3,2) = localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

       Rc(1,3) = localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
       Rc(2,3) = localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
       Rc(3,3) = localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)

       this(icdan)%Gcdt = MATMUL(Rc, this(icdan)%tuc)
       this(icdan)%Gcdn = MATMUL(Rc, this(icdan)%nuc)
       this(icdan)%Gcds = MATMUL(Rc, this(icdan)%suc)
    
       ! Calcul des vitesses relatives
       vlt_cst = DOT_PRODUCT(dV,t(:,j))
       vln_cst = DOT_PRODUCT(dV,n(:,j))
       vls_cst = DOT_PRODUCT(dV,s(:,j))

       this(icdan)%vltBEGIN = vlt_cst + DOT_PRODUCT(cd_Vbegin(4:6),this(icdan)%Gcdt) - DOT_PRODUCT(an_Vbegin(4:6),this(icdan)%Gant)
       this(icdan)%vlnBEGIN = vln_cst + DOT_PRODUCT(cd_Vbegin(4:6),this(icdan)%Gcdn) - DOT_PRODUCT(an_Vbegin(4:6),this(icdan)%Gann)
       this(icdan)%vlsBEGIN = vls_cst + DOT_PRODUCT(cd_Vbegin(4:6),this(icdan)%Gcds) - DOT_PRODUCT(an_Vbegin(4:6),this(icdan)%Gans)


       this(icdan)%rls      = 0.D0
       this(icdan)%rlt      = 0.D0
       this(icdan)%rln      = 0.D0
       this(icdan)%vls      = this(icdan)%vlsBEGIN
       this(icdan)%vlt      = this(icdan)%vltBEGIN
       this(icdan)%vln      = this(icdan)%vlnBEGIN
       this(icdan)%gapTT    = this(icdan)%gapTTbegin
       this(icdan)%status   = i_nknow
       this(icdan)%area     = v2v%pt_area(j)

       this(icdan)%id_f_cd  = v2v%id_f_cd
       this(icdan)%id_f_an  = v2v%id_f_an
       this(icdan)%icdsci   = j
       this(icdan)%iansci   = 0
       
    ENDDO

  end subroutine STO_fill_this_array


  ! detection face a face pour faces planes
  !           non convexe pour faces courbes
  !
  ! INPUT
  ! iantac, fj : id antagoniste, id face topologique
  ! icdtac, fi : id candidat,    id face topologique
  ! isee       : id visibilite
  !
  ! OUTPUT
  ! nb_ctc            : nombre de points de contact
  ! pt_ctc(1:nb_ctc)  : coordonnees des points de contact
  ! overlap(1:nb_ctc) : les gaps aux points de contact
  ! n, t, s           : repere local au point de contact
  ! area(1:nb_ctc)    : aire associee au contact
  ! v2v               : structure visavis du contact
  subroutine STO_DETECTION(iantac,icdtac,isee,fj,fi,nb_ctc,v2v,bavard)
    implicit none
    integer         :: iantac,icdtac,isee,fj,fi,nb_ctc
    logical         :: bavard
    type(T_visavis) :: v2v
    !
    TYPE(T_POLYR)                           :: PRan,PRcd
    LOGICAL                                 :: is_ok,is_flat
    INTEGER                                 :: i,j,ic,fii,fjj,nbv,imax,jmin,icheck,ppp,status
    REAL(kind=8)                            :: angle,dist,dist2,gdist,gdist2,adist,surface,gap,cd_height,an_height,tol_flatness,cd_mass,an_mass
    REAL(kind=8),DIMENSION(3)               :: mid_P,mid_t,mid_n,mid_s,nn,tt,ss,point,an_weight,vec
    REAL(kind=8),DIMENSION(3)               :: cd_centre,an_centre,cd_normal,an_normal
 
    INTEGER,     DIMENSION(:),  ALLOCATABLE :: an_good
    INTEGER,     DIMENSION(:),  ALLOCATABLE :: id_face_cd,id_face_an,face_sizes
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: weight_face_cd,weight_face_an
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: face_ctc
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: pts_ctc,n
    REAL(kind=8),DIMENSION(:),  ALLOCATABLE :: areas
 
    INTEGER                                 :: err_
                                !12345678901234567890
    CHARACTER(len=20)  :: IAM = 'PRPRx::STO_DETECTION'
    CHARACTER(len=108) :: cout
    
    if ( bavard ) print*,"--------------------------------------------------------------------------------------"
    if ( bavard ) print*,"in ",IAM
    if ( bavard ) print*,"PRcd = ",icdtac," fi = ",fi,"  -  Pran = ",iantac," fj = ",fj

    PRan = S_POLYR(iantac)
    PRcd = S_POLYR(icdtac)
    
    nb_ctc      = 0
    adist       = see(isee)%alert
    ! un essai
    gdist2 = (2.D0*adist)**2 + (1.1*PRan%max_radius_face)**2
    gdist  = sqrt(gdist2)
    
    if ( bavard ) then
       print*,"PRan%max_radius_face = ",PRan%max_radius_face
       print*,"gdist = ",gdist,"   ( ^2 = ",gdist2," )"
    endif
    
    ! si les faces cd et an sont flat, on fait du f2f
    if ( PRcd%f2f_status(fi) == 0 .and. PRan%f2f_status(fj) == 0 .and. .not. force_nc_detection ) then
    
       is_flat = .TRUE.
    
       if ( bavard ) print*,"--> flat"
    
       !fd test d'alignement des normales
       angle = dot_product( PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1)), PRan%normal(:,PRan%f2f_set(fj)%G_i(1)) )
       if ( bavard ) then
          print*," cd : normal = ",PRcd%normal(:,PRcd%f2f_set(fi)%G_i(1))
          print*," an : normal = ",PRan%normal(:,PRan%f2f_set(fj)%G_i(1))
          print*,"       angle = ",angle
       endif
       if ( angle < -1.d0 + f2f_tol) then
 
          ! recherche de l'orientation du plan separateur a la F2F
          icheck = STO_compute_f2f_common_plane( PRcd,PRan,perio_shift,  &
                                                isee,fi,fj,              &
                                                adist,                   &
                                                mid_P,mid_n,bavard       )
          if ( bavard ) print*,"icheck = ",icheck
          if ( icheck == 0 ) return
 
          call STO_planplan( PRcd,PRan,perio_shift,fi,fj,           &
                             mid_P,mid_n,decompression,             &
                             nb_ctc,pts_ctc,face_ctc,face_sizes,    &
                             surface,bavard                         )
                     
          if (nb_ctc > 0) then
        
             ALLOCATE( id_face_cd(nb_ctc),id_face_an(nb_ctc),                &
                       weight_face_cd(3,nb_ctc),weight_face_an(3,nb_ctc),    &
                       areas(nb_ctc)                                         )
 
             call STO_proj_f2f( PRcd,PRan,perio_shift,fi,fj,  &
                                mid_n,nb_ctc,pts_ctc,         &
                                id_face_cd,weight_face_cd,    &
                                id_face_an,weight_face_an     )
            ! au retour de FC_proj_f2f, le nb_ctc peut avoit diminue

            areas(:) = surface/nb_ctc

          endif
       endif
 
    ! si les faces topo ne sont pas flat, on fait du non-convexe
    else
       
       is_flat = .FALSE.
 
       if ( bavard ) print*,"--> courbe"
 
       allocate(an_good(PRan%nb_vertex))
 
       nbv = size(PRcd%f2f_set(fi)%G_i)
 
       allocate( pts_ctc(3,nbv),n(3,nbv),                      &
                 weight_face_cd(3,nbv),weight_face_an(3,nbv),  &
                 id_face_cd(nbv),id_face_an(nbv),              &
                 areas(nbv)                                    )
 
       ! on parcourt les elements de la face cd
       do i = 1,size(PRcd%f2f_set(fi)%G_i)
 
          ! numero de l'element
          fii = PRcd%f2f_set(fi)%G_i(i)
 
          ! les candidats sont places au centre des elements
          ! fd: on shift le candidat, vu que c'est complique de modifier l'antagoniste
          cd_centre(:) = ( PRcd%vertex(:,PRcd%face(1,fii)) + PRcd%vertex(:,PRcd%face(2,fii)) + PRcd%vertex(:,PRcd%face(3,fii)) )/3.D0 - perio_shift(:)
          
          ! existe-t-il au moins un couple d'element cd/an assez proches et dont les normales sont opposees ?
          if ( bavard ) print*,"element ",i,fii
 
          is_ok = .false.
          do j = 1,size(PRan%f2f_set(fj)%G_i)       
             fjj          = PRan%f2f_set(fj)%G_i(j)
             angle        = dot_product( PRcd%normal(:,fii), PRan%normal(:,fjj) )
             an_centre(:) = ( PRan%vertex(:,PRan%face(1,fjj)) + PRan%vertex(:,PRan%face(2,fjj)) + PRan%vertex(:,PRan%face(3,fjj)) )/3.D0
             vec          = cd_centre(:) - an_centre(:)
             dist2        = dot_product(vec, vec)
 !            if ( bavard ) print*,"     ",j,dist2,angle
             if ( (dist2 < gdist2) .and. (angle < -1.d0 + f2f_tol) ) then
                is_ok = .true.
                exit
             endif
          enddo
          
          if ( bavard ) print*,"  => ok ? = ",is_ok
     
          ! on a trouve au moins un couple d'element
          if ( is_ok ) then
          
             if ( bavard ) print*,"i=",i,"   j=",j,"     dist2 = ",dist2,"      angle = ",angle
             
             ! on tag les noeuds de la face topo concernee
             an_good = 0
             do fjj = 1,size(PRan%f2f_set(fj)%G_i)
                an_good( PRan%face(1,PRan%f2f_set(fj)%G_i(fjj)) ) = 1
                an_good( PRan%face(2,PRan%f2f_set(fj)%G_i(fjj)) ) = 1
                an_good( PRan%face(3,PRan%f2f_set(fj)%G_i(fjj)) ) = 1
             enddo
                           
             ppp = 0
             !status = STO_node_HE_Hdl_proximity( PRan%HE_Hdl,cd_centre(:),gdist,                   &
             !                                   PRcd%normal(:,fii),.true.,ppp,gap,                &
             !                                   point,tt,nn,ss,fjj,an_weight,bavard,err_,an_good  )
             status = new_node_HE_Hdl_proximity( PRan%HE_Hdl,cd_centre(:),gdist,                   &
                                                PRcd%normal(:,fii),.true.,ppp,gap,                &
                                                point,tt,nn,ss,fjj,an_weight,bavard,err_,good_nodes=an_good  )
             
             if ( bavard ) print*,"status = ",status,"      gap = ",gap
 
             ! si le T3 est trop petit, on vire
             surface = PRcd%areas(fii)
             if ( .NOT.( f2f_skip_small_surface .and. (surface < f2f_tol_small_surface) ) ) then
             
                 ! on ne conserve que les vrais contact face a face (status = 3)
                 if ( status == 3 .and. gap < adist .and. gap > -2.0*adist .and. gap > -1.0*min(PRcd%inner_radius, PRan%inner_radius) ) then
 
                    ! on ajoute un point de contact
                    nb_ctc                   = nb_ctc + 1
                    ! point de contact au milieu du couple cd / an
                    pts_ctc(:,nb_ctc)        = 0.5D0*( cd_centre(:) + perio_shift(:) + point(:) )
                    n(:,nb_ctc)              = nn
                    id_face_cd(nb_ctc)       = fii
                    weight_face_cd(:,nb_ctc) = 1.D0/3.D0
                    id_face_an(nb_ctc)       = fjj
                    weight_face_an(:,nb_ctc) = an_weight
                    areas(nb_ctc)            = PRcd%areas(fii)
 
                     if ( bavard ) then
                         print*,"### point de contact ###"
                         print*,"ictc = ",nb_ctc
                         print*,"coords = ",pts_ctc(:,nb_ctc)
                         print*,"normal = ",n(:,nb_ctc)
                     endif
                 endif
 
             endif
 
          endif
 
       enddo
       
       ! calcul le centre et la normale au visavis -> utilisée dans les Visavis Display Files
       ! c'est le common plane a partir des points de contact trouves en non-convexe
       surface = 0.D0
       mid_P   = 0.D0
       mid_n   = 0.D0
       do ic = 1,nb_ctc
          surface = surface + areas(ic)
          mid_P   = mid_P   + areas(ic) * pts_ctc(:,ic)
          mid_n   = mid_n   + areas(ic) * n(:,ic)
       enddo
       mid_P = mid_P / surface
       mid_n = mid_n / length3(mid_n)
       
       DEALLOCATE(an_good)
 
 ! methode pour reduire le nombre de points de contact
 
       if ( nb_ctc > 0 .and. .not. force_nc_detection ) then
 
           ! on regarde si la face de contact trouvee en non-convexe ne serait pas flat quand meme
           if ( .not.(force_f2f_detection) ) then
              ! calcul de l'ecart maxi des normales au common plane
              tol_flatness = cos(get_flatness_angle() * (PI_g/180.d0))
              is_flat = .TRUE.
              do ic = 1,nb_ctc
                 if ( abs(dot_product(mid_n,n(:,ic))) < tol_flatness ) then
                    is_flat = .FALSE.
                    exit
                 endif
              enddo
           endif
           
           ! est ce que la face de contact est flat (meme si les faces topo sont courbes)
           if ( is_flat .or. force_f2f_detection ) then
 
              if ( bavard ) print*,"--> mais flat quand meme !"
        
              is_flat = .TRUE.
           
              ! on nettoie pour pouvoir recommencer la detection des points de contact avec l'approche f2f
              deallocate( pts_ctc,n,areas,                                     &
                          id_face_cd,id_face_an,weight_face_cd,weight_face_an  )
            
              call STO_planplan( PRcd,PRan,perio_shift,fi,fj,           &
                                 mid_P,mid_n,decompression,             &
                                 nb_ctc,pts_ctc,face_ctc,face_sizes,    &
                                 surface,bavard                         )
                             
              if (nb_ctc > 0) then

                 ALLOCATE( id_face_cd(nb_ctc),id_face_an(nb_ctc),                &
                           weight_face_cd(3,nb_ctc),weight_face_an(3,nb_ctc),    &
                           areas(nb_ctc)                                         )
                           
                 call STO_proj_f2f( PRcd,PRan,perio_shift,fi,fj,  &
                                    mid_n,nb_ctc,pts_ctc,         &
                                    id_face_cd,weight_face_cd,    &
                                    id_face_an,weight_face_an     )

                ! au retour de FC_proj_f2f, le nb_ctc peut avoit diminue

                areas(:) = surface/nb_ctc

              end if
           end if
       endif
 
    endif
    
    ! si on a trouve des contacts
    if ( nb_ctc > 0 ) then
    
       ALLOCATE( v2v%iff_cd(nb_ctc),v2v%iff_an(nb_ctc),          &
                 v2v%cd_lcoor(3,nb_ctc),v2v%an_lcoor(3,nb_ctc),  &
                 v2v%index(nb_ctc),v2v%pt_area(nb_ctc)           )
                 
       v2v%index           = 0
       v2v%cd              = icdtac
       v2v%an              = iantac
       v2v%isee            = isee
       v2v%nb_ctc          = nb_ctc
       v2v%id_f_cd         = fi
       v2v%id_f_an         = fj
       v2v%centre          = mid_P
       v2v%normal          = mid_n
       v2v%perio_shift     = perio_shift
       v2v%is_flat         = is_flat
       v2v%iff_cd(:)       = id_face_cd(1:nb_ctc)
       v2v%cd_lcoor(:,:)   = weight_face_cd(:,1:nb_ctc)
       v2v%iff_an(:)       = id_face_an(1:nb_ctc)
       v2v%an_lcoor(:,:)   = weight_face_an(:,1:nb_ctc)
       v2v%pt_area(:)      = areas(1:nb_ctc)
       v2v%decompression   = decompression

       if ( is_flat ) then
          ! pour le contact f2f, on conserve la face de contact totale
          ALLOCATE( v2v%face_ctc(3,size(face_ctc,dim=2)), v2v%face_sizes(size(face_sizes)) )
          v2v%face_ctc   = face_ctc
          v2v%face_sizes = face_sizes

       endif
 
    endif
    
    IF ( ALLOCATED(pts_ctc) )        DEALLOCATE(pts_ctc)
    IF ( ALLOCATED(n) )              DEALLOCATE(n)
    IF ( ALLOCATED(id_face_cd) )     DEALLOCATE(id_face_cd)
    IF ( ALLOCATED(id_face_an) )     DEALLOCATE(id_face_an)
    IF ( ALLOCATED(weight_face_cd) ) DEALLOCATE(weight_face_cd)
    IF ( ALLOCATED(weight_face_an) ) DEALLOCATE(weight_face_an)
    IF ( ALLOCATED(areas) )          DEALLOCATE(areas)
    IF ( ALLOCATED(an_good) )        DEALLOCATE(an_good)
    IF ( ALLOCATED(face_ctc) )       DEALLOCATE(face_ctc)
    IF ( ALLOCATED(face_sizes) )     DEALLOCATE(face_sizes)

   if ( bavard ) print*,"nb_ctc = ",nb_ctc
   if ( bavard ) print*,"out ",IAM
   if ( bavard ) print*,"--------------------------------------------------------------------------------------"
    
  end subroutine STO_DETECTION
 
 
  !> calcul de l'intersection des projections des objets 
  !> connaissant l'orientation du plan separateur
 subroutine STO_planplan( PRcd,PRan,an_shift,fi,fj,               &
                          mid_P,mid_n,decompression,              &
                          nb_ptc,pts_ctc,face_ctc,face_sizes,     &
                          surface, bavard                         )
    implicit none
 
    type(T_POLYR)                             :: PRan,PRcd
    !> vecteur de decalage du corps an pour condition periodique
    real(kind=8)                              :: an_shift(3)   
    integer                                   :: fi,fj,nb_ctc,nb_ptc
    real(kind=8)                              :: surface,decompression
    real(kind=8), dimension(3)                :: mid_P,mid_n,mid_t,mid_s
    real(kind=8), dimension(:,:), allocatable :: pts_ctc,face_ctc
    real(kind=8), dimension(:)  , pointer     :: test_p
    integer,      dimension(:),   allocatable :: face_sizes
    integer,      dimension(:),   pointer     :: sizes1,sizes2
    logical                                   :: bavard
    !
    LOGICAL                                 :: check,ok
    INTEGER                                 :: i,ip,j,inc,errare,nbv,n1,n2,n3,nb_ch
    INTEGER                                 :: nb_vertex_pran_min,nb_vertex_prcd_min
    INTEGER,     DIMENSION(:),  ALLOCATABLE :: id_ch
    REAL(kind=8)                            :: dist1,dist2,norm_cd,norm_an,ref_size,area,Iu,Iv
    REAL(kind=8)                            :: delta,t1,t2,ratio, surf_tol
    REAL(kind=8),DIMENSION(2)               :: centre,u,v
    REAL(kind=8),DIMENSION(3)               :: vec
 
    REAL(kind=8),DIMENSION(:,:),POINTER    :: Pcd,Pan,points,points_sh,points_tc,points_ok
    REAL(kind=8),DIMENSION(:),  POINTER    :: areas
    INTEGER,     DIMENSION(:),  POINTER    :: sizes,stemp
    INTEGER                                :: err,err_, is_in
                                !1234567890123456789
    CHARACTER(len=19)  :: IAM = 'PRPRx::STO_planplan'
    CHARACTER(len=21)  :: nom
    CHARACTER(len=80)  :: cout
 
    NULLIFY(Pcd,Pan,points,points_sh,points_tc,points_ok,sizes,areas,stemp)
   
    nb_ctc = 0

    ! to define un polygon define par only one polytope
    allocate(sizes1(1),sizes2(1))

    call comp_rep(mid_t,mid_n,mid_s)
    
    if ( bavard ) then
        print*," -> mid_P = ",mid_P
        print*,"    mid_n = ",mid_n
        print*,"    mid_t = ",mid_t
        print*,"    mid_s = ",mid_s
    endif
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Projection des contours des faces topologiques sur le common plane

    ! Traitement de l'antagoniste
    ! Recuperation des vertex a projeter
    nbv = size(PRan%f2f_contour(fj)%G_i)/2
    ALLOCATE(Pan(2,nbv),stat=errare)
    Pan = 0.d0
    ! Projection des vertex du contour sur le plan moyen (passage en coords locales)
    DO i = 1,nbv
      vec(:)   = PRan%vertex(:,PRan%f2f_contour(fj)%G_i(2*(i-1)+1)) + an_shift(:) - mid_P(:)
      Pan(1,i) = DOT_PRODUCT(vec, mid_s)
      Pan(2,i) = DOT_PRODUCT(vec, mid_t)
    ENDDO

   if ( bavard ) then
      write(nom,'(a3,i4.4,a1,i3.3)') 'an_',PRan%id,'_',fj
      call Draw_Polygon(nom,Pan,P=mid_P,s=mid_s,t=mid_t)
   endif 

 ! Traitement du candidat
    ! Recuperation des vertex a projeter
    nbv = size(PRcd%f2f_contour(fi)%G_i)/2
    ALLOCATE(Pcd(2,nbv),stat=errare)
    Pcd = 0.d0
    ! Projection des vertex sur le plan moyen (passage en coords locales)
    DO i = 1,nbv
      vec(:)   = PRcd%vertex(:,PRcd%f2f_contour(fi)%G_i(2*(i-1)+1)) - mid_P(:)
      Pcd(1,i) = DOT_PRODUCT(vec, mid_s)
      Pcd(2,i) = DOT_PRODUCT(vec, mid_t)
    ENDDO

   if ( bavard ) then
      write(nom,'(a3,i4.4,a1,i3.3)') 'cd_',PRcd%id,'_',fj
      call Draw_Polygon(nom,Pcd,P=mid_P,s=mid_s,t=mid_t)
   endif 
 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! shrink des contours et calcul de leur intersection

   ! on impose un shrink (metrique) minimal pour eviter les problemes de projections dans proj_f2f
   shrink = max(shrink, 1.D-6)

   ! intersection des faces en contact
   ! ( on ne simplifie pas l'intersection obtenue pour la conserver intacte => delta = 1e-6 )
   sizes1(1) = size(Pan,dim=2)
   sizes2(1) = size(Pcd,dim=2)

   if( f2f_skip_small_surface ) then
     surf_tol = f2f_tol_small_surface
   else
     surf_tol = 0.d0
   end if
   
   call polygons_intersection_wc(Pan, sizes1, Pcd, sizes2, shrink, shrink, 1d-6, 3, surf_tol, points, sizes, areas)

   ! si il n'y a pas d'intersection entre Pan et Pcd
   if ( .not. associated(points) ) then
      nb_ptc = 0
      deallocate(Pcd,Pan)
      return
   endif

   ! recuperation du nb de points de contact
   nb_ctc = size(points,dim=2)
   surface = sum(areas)
   if( associated(areas) ) deallocate(areas)
   nullify(areas)

   if ( bavard ) then
      write(nom,'(a4,i4.4,a1,i3.3,a1,i4.4,a1,i3.3)') 'int_',PRcd%id,'_',fi,'=',PRan%id,'_',fj
      call Draw_Polygon(nom,points,sizes=sizes,P=mid_P,s=mid_s,t=mid_t)
   endif
   
   ! ces points sont conserves pour definir la face de contact complete (utilises en post-traitement dans visavis_vtk_draw_all)
   ! ensuite les points de contact sont disposes dans cette face,
   ! mais, en fonction de la decompression toleree, ils ne seront pas forcement sur les bords

   allocate(id_ch(nb_ctc))
   call envelope(points, nb_ctc, id_ch, nb_ch)

   ! retour en 3D : contour de la face de contact
   nb_ctc = sum(sizes)
   allocate(face_ctc(3,nb_ctc))
   do i = 1,nb_ctc
      face_ctc(:,i) = mid_P(:) + points(1,i)*mid_s(:) + points(2,i)*mid_t(:)
   enddo
   call glob2loc_frame_POLYR(PRan%id, face_ctc)

   ! sizes des polygones (pour gerer le cas ou la face de contact est composee de plusieurs zones)
   allocate(face_sizes(size(sizes)))
   face_sizes(:) = sizes(:)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! calcul du tiers central (avec ou sans swell)
    IF ( decompression >= 0.D0 ) THEN

        ! ratio de swell du tiers central, calcule a partir du taux de decompression autorise
        test_p => null()
        call compute_central_kernel( points, sizes, decompression, nb_ptc, points_tc, test_p, is_in, bavard )

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! applique un scale homothetique
    ELSE IF ( decompression < 0.D0 ) THEN

        ! si decompression imposee, applique un shrink homothetique sur le convex-hull de la face de contact
        nb_ptc = nb_ch
        allocate(points_tc(2,nb_ptc))
        IF ( abs(decompression) < 1.D0 ) THEN
            ratio = ( 1.D0 + 2.D0*abs(decompression) ) / 3.D0
            call polygon_principal_properties(points,sizes,area,centre,Iu,Iv,u,v)
            do i = 1,nb_ptc
                points_tc(:,i) = (1.D0-ratio) * centre(:) + ratio * points(:,id_ch(i))
            enddo
        ELSE
            do i = 1,nb_ptc
                points_tc(:,i) = points(:,id_ch(i))
            enddo
        ENDIF

    ENDIF
    deallocate(id_ch)

    if ( bavard ) then
        write(nom,'(a3,i4.4,a1,i3.3,a1,i4.4,a1,i3.3)') 'tc_',PRcd%id,'_',fi,'=',PRan%id,'_',fj
        call Draw_Polygon(nom,points_tc,P=mid_P,s=mid_s,t=mid_t)
    endif

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! cas tres bizarre ... que faire !
    if ( nb_ptc < 0 ) then
        print*,"area = ",surface
        print*,PRcd%id,'_',fi,' / ',PRan%id,'_',fj
        nb_ptc = 0

    ! en principe, on doit avoir au mons 3 points de contact
    else
    
        ! si on a qu'1 ou 2 points de contact -> pas terrible, mais on garde quand meme
        if ( nb_ptc < 3 ) then
            write(cout,'(a,a,i4,a,i4,a,i3,a)') IAM,' (central kernel) - ',PRcd%id,' / ',PRan%id,' : ',nb_ptc,' points de contact'
            call logmes(cout)
        endif
    
        ! verifier que les points de contact sont toujours dans les faces en vis a vis, car
        !   - le shrink peut faire des trucs bizarres avec les faces multi-polygones
        !   - la calcul du central kernel peut faire des trucs bizarres avecs des faces complexes
        ! sinon, on aura un probleme dans FC_proj_f2f
        ok = polygon_all_points_in_wc(points_tc, points, sizes)
        if ( bavard ) then
            if ( ok ) then
              write(cout,'(A,A,I4,A,I4,A,I3)') IAM,' (central kernel) - ',PRcd%id,' / ',PRan%id
              call logmes(cout, .true.)
              write(cout,'(A)') '    all ptc are inside the contact face'
              call logmes(cout, .true.)
            else
              write(cout,'(A,A,I4,A,I4,A,I3)') IAM,' (central kernel) - ',PRcd%id,' / ',PRan%id
              call logmes(cout)
              write(cout,'(A)') '    at least one ptc is outside of the contact face'
              call logmes(cout)
            endif
        endif

        ! si au moins un point de contact est en dehors de la face de contact
        if ( .not. ok ) then

            ! on fait l'intersection entre le polygone des ptc et la face de contact
            sizes2(1) = nb_ptc
            call polygons_intersection_wc(points, sizes, points_tc, sizes2, 0.D0, 0.D0, 1d-6, 0, 0.d0, points_ok, stemp, areas)

            ! si le calcul de l'intersection est ok
            if ( associated(points_ok) ) then
                nb_ptc = size(points_ok,dim=2)
                ok     = .TRUE.

                ! visu pour debug
                if ( bavard ) then
                    write(nom,'(a3,i4.4,a1,i3.3,a1,i4.4,a1,i3.3)') 'in_',PRcd%id,'_',fi,'=',PRan%id,'_',fj
                    call Draw_Polygon(nom,points_ok,sizes=stemp,P=mid_P,s=mid_s,t=mid_t)
                endif

                ! convex hull du polygone -> eliminier les ptc inutiles
                allocate(id_ch(nb_ptc))
                call envelope(points_ok, nb_ptc, id_ch, nb_ch)
                nb_ptc = nb_ch
                deallocate(points_tc)
                allocate(points_tc(2,nb_ptc))
                do i = 1,nb_ptc
                    points_tc(:,i) = points_ok(:,id_ch(i))
                enddo
                deallocate(id_ch, points_ok)
                
                if ( bavard ) then
                    write(nom,'(a3,i4.4,a1,i3.3,a1,i4.4,a1,i3.3)') 'ch_',PRcd%id,'_',fi,'=',PRan%id,'_',fj
                    call Draw_Polygon(nom,points_tc,P=mid_P,s=mid_s,t=mid_t)
                endif                
                
            endif

        endif
        if ( bavard ) print*,"ok = ",ok

        if( associated(points_ok) ) deallocate(points_ok)
        nullify(points_ok)

        ! si tout s'est bien passe
        if ( ok )  then

            ! simplification du polygone -> eliminier les ptc inutiles
            call polygon_simplification_wc(points_tc, eps_simplification, points_ok)
            if( associated( points_ok ) ) then
              nb_ptc = size(points_ok,dim=2)
            else
              nb_ptc = 0
            end if

            ! retour en 3D
            allocate(pts_ctc(3,nb_ptc))
            do i = 1,nb_ptc
                pts_ctc(:,i)  = mid_P(:) + points_ok(1,i)*mid_s(:) + points_ok(2,i)*mid_t(:)
            enddo
            if( associated(points_ok) ) deallocate(points_ok)
            nullify(points_ok)

        ! sinon, on prend le convex-hull de la face de contact, sans shrink et sans decompression
        ! -> pas terrible, mais on fait ce qu'on peut
        else
            nb_ptc = nb_ch
            allocate(pts_ctc(3,nb_ptc))
            do i = 1,nb_ptc
                pts_ctc(:,i) = mid_P(:) + points(1,id_ch(i))*mid_s(:) + points(2,id_ch(i))*mid_t(:)
            enddo
            
        endif

    endif

    if ( bavard ) then
        write(nom,'(a3,i4.4,a1,i3.3,a1,i4.4,a1,i3.3)') 'ok_',PRcd%id,'_',fi,'=',PRan%id,'_',fj
        call Draw_Polygon(nom,pts_ctc)
    endif

    if( associated(points_tc) ) deallocate(points_tc)
    if( associated(points_ok) ) deallocate(points_ok)
    if( associated(areas) ) deallocate(areas)
    if( associated(sizes) ) deallocate(sizes)
    if( associated(points)) deallocate(points)
    if( associated(stemp) ) deallocate(stemp)

    deallocate(Pcd,Pan,sizes1,sizes2)
  end subroutine STO_planplan


  integer function STO_compute_f2f_common_plane(PRcd,PRan,an_shift,isee,fi,fj,adist,center,normal,bavard)
    implicit none
    TYPE(T_POLYR)             :: PRan,PRcd
    integer                   :: isee,fi,fj
    real(kind=8),dimension(3) :: center,normal
    real(kind=8)              :: adist
    !> vecteur de decalage du corps an pour condition periodique
    real(kind=8)              :: an_shift(3)   
    !
    integer                   :: i
    real(kind=8)              :: dist,an_area,cd_area
    real(kind=8),dimension(3) :: an_centre,cd_centre,an_normal,cd_normal
    logical                   :: bavard

                                !12345678901234567890123456789012345
    CHARACTER(len=35)  :: IAM = 'PRPRx::STO_compute_f2f_common_plane'

    STO_compute_f2f_common_plane = 0

    call STO_compute_geometry_f2f(PRan,fj,an_centre,an_normal,an_area)
    call STO_compute_geometry_f2f(PRcd,fi,cd_centre,cd_normal,cd_area)

    !! si priorite a l'antagoniste, la normale est celle de l'antagoniste
    !if ( tact_behav(see(isee)%lawnb)%priorite_antagoniste ) then
    !    normal = an_normal(:)
    !    center = an_centre(:) + an_shift(:)
    !! sinon, la normale est la "moyenne ponderee" de celles aux faces
    !! la ponderation evite certains soucis de calcul de dist quand une face est tres grande devant l'autre
    !else   
    normal = ( an_area*an_normal(:) - cd_area*cd_normal(:) ) / ( an_area + cd_area )
    ! on normalise n au cas ou les faces cd et an ne sont pas parfaitement paralleles
    normal = normal / SQRT(DOT_PRODUCT(normal,normal))
    center = 0.5d0 * ( cd_centre(:) + an_centre(:) + an_shift(:) )
    !endif 

    !fd pour eviter de garder des faces qui verifient le critere sur les normales 
    !fd mais appartiennent a des objets qui ne sont pas en vis a vis
    !fd inner radius est lie au rayon inscrit des polyedres
    !fd TODO virer ca en virant les set qui sont incompatibles avec l'orientation de l'inter - centre

    dist  = dot_product(normal, cd_centre(:) - (an_centre(:) + an_shift(:)) )
    
    if (dist  < -0.5d0 * min(PRcd%inner_radius, PRan%inner_radius) ) then
       if ( bavard ) then
          print*,"   -> dist  < -0.5d0 * min(PRcd%inner_radius, PRan%inner_radius)"
          print*,"      dist = ",dist
          print*,"      PRcd%inner_radius = ",PRcd%inner_radius
          print*,"      PRan%inner_radius = ",PRan%inner_radius
       endif
       return
    endif 
    
    !fd si la distance est trop grande (ou trop petite) on vire aussi 
    if ( dist > adist .or. dist < -2.0*adist ) then
       if ( bavard ) then
          print*,"   -> dist > adist .or. dist < -2.0*adist"
          print*,"      dist  = ",dist
          print*,"      adist = ",adist
       endif
       return
    endif 

    STO_compute_f2f_common_plane = 1
    
    if ( bavard ) then
       print*,"an_normal = ",an_normal
       print*,"cd_normal = ",cd_normal
       print*,"mid_n     = ",normal
    endif 

  end function STO_compute_f2f_common_plane
 

  !> connaissant les points de contact on cherche les facettes d'accroche 
  subroutine STO_proj_f2f( PRcd,PRan,an_shift,fi,fj,     &
                           mid_n,                        &
                           nb_ctc,pt_ctc,                &
                           id_face_cd,weight_face_cd,    &
                           id_face_an,weight_face_an     )
    implicit none

    TYPE(T_POLYR)                           :: PRan,PRcd
    !> vecteur de decalage du corps an pour condition periodique
    REAL(kind=8)                            :: an_shift(3)
    INTEGER                                 :: fi,fj,nb_ctc
    INTEGER,     DIMENSION(:),  ALLOCATABLE :: id_face_cd,id_face_an
    REAL(kind=8),DIMENSION(3)               :: mid_n
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: pt_ctc
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: weight_face_cd,weight_face_an

    INTEGER                                 :: ic,fii,fjj,i,ip
    LOGICAL                                 :: is_inside_cd,is_inside_an
    REAL(kind=8)                            :: dist
    REAL(kind=8),DIMENSION(3)               :: weight_cd,weight_an
    REAL(kind=8),DIMENSION(3,3)             :: pp

                              !1234567890123456789
    character(len=19)  :: IAM='PRPRx::STO_proj_f2f'
    character(len=108) :: cout

    ip = 0

    DO ic = 1,nb_ctc

      ! recherche de la face cd qui contient le noeud projete suivant la direction mid_n
      is_inside_cd = .false.
      do fii = 1,size(PRcd%f2f_set(fi)%G_i)
        do i = 1,3
           pp(:,i) = PRcd%vertex(:,PRcd%face(i,PRcd%f2f_set(fi)%G_i(fii))) 
        enddo
        ! projection suivant la normale au common plane
        is_inside_cd = node_triangle_projection(pp,pt_ctc(:,ic),mid_n,dist,weight_cd,.false.)
        ! des qu'un element est trouve, on sort
        if ( is_inside_cd ) exit
      enddo

      ! recherche de la face an qui contient le noeud projete suivant la direction mid_n
      is_inside_an = .false.
      do fjj = 1,size(PRan%f2f_set(fj)%G_i)
        do i = 1,3
           pp(:,i) = PRan%vertex(:,PRan%face(i,PRan%f2f_set(fj)%G_i(fjj))) + an_shift(:)
        enddo
        ! on inverse la normale de projection pour l'antagoniste
        is_inside_an = node_triangle_projection(pp,pt_ctc(:,ic),-mid_n,dist,weight_an,.false.)
        ! des qu'un element est trouve, on sort
        if ( is_inside_an ) exit
      enddo

      ! si ce point de contact se projette correctement sur le candidat et l'antagoniste, on le garde
      if ( is_inside_cd .and. is_inside_an ) then
         ip                   = ip+1
         id_face_cd(ip)       = PRcd%f2f_set(fi)%G_i(fii)
         weight_face_cd(:,ip) = weight_cd(:)
         id_face_an(ip)       = PRan%f2f_set(fj)%G_i(fjj)
         weight_face_an(:,ip) = weight_an(:)

      ! sinon, on signale
      else
         call logmes('===================================================================================')
         if ( .not. is_inside_cd ) then
            call logmes('Error '//IAM//': unable to find a candidate face')
         endif
         if ( .not. is_inside_an ) then
            call logmes('Error '//IAM//': unable to find an antagonist face')
         endif
         write(cout,'(a,i0,a,i0,a,i0)') 'Contact number ',ic,' between POLYR ',PRcd%id,' and POLYR ',PRan%id
         call logmes(cout)
         write(cout,'(a,3(1x,D12.5))') '   position ',pt_ctc(:,ic)
         call logmes(cout)
         write(cout,'(a,3(1x,D12.5))') '   normal   ',mid_n
         call logmes(cout)
         write(cout,'(a)') 'This contact point is removed'
         call logmes(cout)
      endif

    enddo

    ! nombre de points de contact effectivement trouve
    nb_ctc = ip

  end subroutine STO_proj_f2f

  subroutine STO_update_f2f(v2v,xco,overlap,n,t,s,bavard)
    implicit none
    type(T_visavis)                         :: v2v
    REAL(kind=8),DIMENSION(:),  ALLOCATABLE :: overlap
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: xco
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: n,t,s
    LOGICAL                                 :: bavard
    ! local
    INTEGER                                 :: i,ic,nb_ctc
    REAL(kind=8),DIMENSION(3)               :: pcd,pan
    TYPE(T_POLYR)                           :: PRan,PRcd
    
                                !123456789012345678901
    CHARACTER(len=21)  :: IAM = 'PRPRx::STO_update_f2f'

    PRan    = S_POLYR(v2v%an)
    PRcd    = S_POLYR(v2v%cd)
    nb_ctc  = v2v%nb_ctc

    allocate( xco(3,nb_ctc),overlap(nb_ctc),n(3,nb_ctc),t(3,nb_ctc),s(3,nb_ctc) )

    overlap = 0.d0
    xco     = 0.d0
    t       = 0.d0 
    n       = 0.d0
    s       = 0.d0  

    do ic = 1,nb_ctc

      ! calcule les positions des projections des points de contact
      pcd = 0.d0
      pan = 0.d0
      do i = 1,3
         pcd(:) = pcd(:) + v2v%cd_lcoor(i,ic)*PRcd%vertex(:,PRcd%face(i,v2v%iff_cd(ic))) 
         pan(:) = pan(:) + v2v%an_lcoor(i,ic)*PRan%vertex(:,PRan%face(i,v2v%iff_an(ic))) 
      enddo
      ! decalage pour gestion du periodique
      pan(:) = pan(:) + v2v%perio_shift(:)
      
      ! si priorite a l'antagoniste, le pt de contact est sur l'antagoniste
      !                              la normale est celle de l'antagoniste
      !if ( tact_behav(see(v2v%isee)%lawnb)%priorite_antagoniste ) then
      !   xco(:,ic) = pan(:)
      !   n(:,ic)   = PRan%normal(:,v2v%iff_an(ic))
      !! sinon, le pt de contact est au milieu
      !!        la normale est la "moyenne" de celles aux faces
      !else
      xco(:,ic) = 0.5 * ( pcd(:) + pan(:) )
      n(:,ic)   = 0.5 * ( PRan%normal(:,v2v%iff_an(ic)) - PRcd%normal(:,v2v%iff_cd(ic)) )
      ! on normalise n au cas ou les faces cd et an ne sont pas parfaitement paralleles
      n(:,ic) = n(:,ic) / SQRT(DOT_PRODUCT(n(:,ic),n(:,ic)))
      !endif
      call comp_rep( t(:,ic),n(:,ic),s(:,ic) )
         
      overlap(ic) = dot_product( n(:,ic), pcd - pan )
      
      if ( bavard ) print*,"ctc ",ic,"     gap = ",overlap(ic),"     normale = ",n(:,ic)

    enddo
   
  end subroutine STO_update_f2f

  subroutine get_f2f_central_kernel(i_visavis, ck_coor, sn, is_in)
    implicit none
    !> f2f index
    integer, intent(in) :: i_visavis
    !> coordinates of the central kernel
    real(kind=8), dimension(:,:), pointer :: ck_coor
    !> mean normal compression stress
    real(kind=8), intent(out) :: sn
    !> is center of pressure inside the central kernel
    integer, intent(inout) :: is_in
    ! local variables
    logical :: bavard = .false.
    integer :: nb_ptc, nb_ctc, i_ctc, idx
    real(kind=8), dimension(:)  , pointer :: cop
    real(kind=8), dimension(:,:), pointer :: points
    integer     , dimension(:)  , pointer :: sizes
    !                           12345678901234567890123456789
    character(len=29) :: IAM = 'PRPRx::get_f2f_central_kernel'

    sn = 0.d0
    is_in = 0

    ck_coor  => null()

    if (.not. get_visible_POLYR(visavis(i_visavis)%cd)  .or. &
        .not. get_visible_POLYR(visavis(i_visavis)%an) ) return

    if ( .not. visavis(i_visavis)%is_flat ) return

    ! temporary array to work in global frame
    allocate( points, source=visavis(i_visavis)%face_ctc )
    call loc2glob_frame_POLYR(visavis(i_visavis)%an, points)

    allocate(cop(3))
    cop(:) = 0.d0
    nb_ctc = visavis(i_visavis)%nb_ctc
    do idx = 1, nb_ctc
      i_ctc = visavis(i_visavis)%index(idx)
      cop(:) = cop(:) + this(i_ctc)%coor * this(i_ctc)%rln
      sn     = sn     + this(i_ctc)%rln
    end do
    cop(:) = cop(:) / sn
    sn     = sn     / nb_ctc

    ! allocation of ck_coor inside this one
    call compute_central_kernel( points, visavis(i_visavis)%face_sizes, 0.d0, nb_ptc, ck_coor, cop, is_in, .false.)

    if( associated(points) ) deallocate(points)
    if( associated(cop)    ) deallocate(cop)
    nullify(points)
    nullify(cop)

  end subroutine get_f2f_central_kernel

  subroutine get_f2f_stress(i_visavis, faceC, sizeC, sigmas, faceD, sizeD, decomp)
    implicit none
    !> f2f index
    integer, intent(in) :: i_visavis
    !> number points defining each polygon (Compressed and Decompressed)
    integer,      dimension(:),   pointer     :: sizeC,sizeD
    !> coordinates of all polygon points (Compressed and Decompressed)
    real(kind=8), dimension(:,:), pointer     :: faceC,faceD
    !> decompression rate of the face
    real(kind=8)                              :: decomp
    ! stress for each Compressed points
    real(kind=8), dimension(:),   pointer     :: sigmas
    !
    integer                                   :: nb_ctc,nb_pts,nb_ch,ictc,icdan
    real(kind=8), dimension(:,:), pointer     :: contour
    real(kind=8), dimension(:,:), allocatable :: coor_xyz
    real(kind=8), dimension(:)  , allocatable :: rlt, rln, rls
    ! description des polygones des faces comprimees et decomprimees
    ! variable de calcul
    real(kind=8)                              :: Rtot, area, moySN, moyST, aireComp
    real(kind=8), dimension(3)                :: centre, normal
    integer                                   :: err_
    ! variables locales         123456789012345678901
    character(len=21) :: IAM = 'PRPRx::get_f2f_stress'

    ! print*,"ivisavis = ",i_visavis
    ! fd a voir ... pour virer l'affichage des visavis avec des blocs invisibles
    if (.not. get_visible_POLYR(visavis(i_visavis)%cd)  .or. &
        .not. get_visible_POLYR(visavis(i_visavis)%an) ) return

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! 1er cas :  les visavis flat
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( .not. visavis(i_visavis)%is_flat ) return

    ! on recupere le nombre de points de contact de la structure visavis courante
    nb_ctc = visavis(i_visavis)%nb_ctc
    ! on recupere le contour de la face de contact de la structure visavis courante
    nb_pts = size(visavis(i_visavis)%face_ctc,dim=2)
    if (nb_pts < 3) return

    ! recuperation de la liste des points de contour
    allocate(contour(3,nb_pts))
    contour(:,:) = visavis(i_visavis)%face_ctc(:,:)

    ! recuperation de la liste des points de contact
    allocate(coor_xyz(3,nb_ctc), rln(nb_ctc), rls(nb_ctc), rlt(nb_ctc))
    do ictc = 1,nb_ctc
        icdan            = visavis(i_visavis)%index(ictc)
        coor_xyz(:,ictc) = this(icdan)%coor(:)
        rln(ictc)        = this(icdan)%rln / H
        rls(ictc)        = this(icdan)%rls / H
        rlt(ictc)        = this(icdan)%rlt / H
    enddo

    ! normale au contact
    normal  = visavis(i_visavis)%normal

    ! on calcule le centre de pousse = barycentre des points de contact, ponderes par valeurs absolues des reactions normales
    ! si les reactions de contact sont toutes nulles
    if (maxval(dabs(rln)) < 1.e-14) then
        ! on prend l'isobarycentre des points de contact
        centre = sum(coor_xyz,dim=2) / nb_ctc
        Rtot   = 0.d0
    ! sinon,
    else
        ! on initialise les coordonnees du barycentre des points de contact
        centre = 0.d0
        Rtot   = 0.d0
        ! pour chaque point de contact
        do ictc = 1,nb_ctc
            ! on ajoute la contribution de la reaction normale du point de contact courant
            centre = centre + rln(ictc)*coor_xyz(:,ictc)
            Rtot   = Rtot   + rln(ictc)
        end do
        ! on divise par la somme des valeurs absolues des reactions normales pour obtenir le barycentre
        centre = centre / Rtot
    end if

    deallocate(coor_xyz,rln,rls,rlt)

    ! si la force transmise est trop faible, on considere qu'il n'y a pas contact
    ! on saute ce visavis
    ! if (abs(Rtot) < 1.d0) return

    ! calcule les faces comprimee / decomprimee et champ de contraintes
    ! print*,"iv = ",i_visavis,"   -   cd = ",visavis(i_visavis)%cd,"   -   an = ",visavis(i_visavis)%an
    call loc2glob_frame_POLYR(visavis(i_visavis)%an, contour)
    call compute_stress_field(contour, visavis(i_visavis)%face_sizes, centre, normal, Rtot, faceC, sizeC, sigmas, faceD, sizeD, decomp, err_, .FALSE.)

    deallocate(contour)
    nullify(contour)

    if( err_ > 0 ) then
      !print *, i_visavis
      !print *, visavis(i_visavis)%face_ctc
      !print *, centre
      !print *, normal
      !print *, Rtot
      !print *, associated(faceC)
      !print *, associated(sizeC)
      !print *, associated(sigmas)
      !print *, associated(faceD)
      !print *, associated(sizeD)
      !print *, decomp
      !print *, err_
      decomp = -99.d0
      call logmes('['//IAM//'] Error in stress computation',.true.)
    end if

  end subroutine get_f2f_stress

!-----------------------------------------------------------------------
! divers
!-----------------------------------------------------------------------

  !> calcule les proprietes geometriques moyennes des faces topologiques
  !> centre de section et normale
  subroutine STO_compute_geometry_f2f(PR,fi,centre,normal,area)
    implicit none

    TYPE(T_POLYR)                       :: PR
    INTEGER                             :: i,fi,fii
    REAL(kind=8)                        :: ael,area
    REAL(kind=8),DIMENSION(3)           :: cel,centre,normal
                                !1234567890123456789012345678901
    CHARACTER(len=31)  :: IAM = 'PRPRx::STO_compute_geometry_f2f'

    area    = 0.D0
    centre  = 0.D0
    normal  = 0.D0
       
    ! boucle sur les elements de la face topo
    do i = 1,size(PR%f2f_set(fi)%G_i)

       ! numero de l'element
       fii       = PR%f2f_set(fi)%G_i(i)
       ! surface et centre de l'element
       ael       = PR%areas(fii)
       cel(:)    = PR%vertex(:,PR%face(1,fii)) + PR%vertex(:,PR%face(2,fii)) + PR%vertex(:,PR%face(2,fii))
       ! cumul
       area      = area      + ael
       centre(:) = centre(:) + ael * cel(:)
       normal(:) = normal(:) + ael * PR%normal(:,fii)
          
    enddo
       
    centre(:) = centre(:) / 3.D0 / area
    normal(:) = normal(:) / length3(normal)

  end subroutine STO_compute_geometry_f2f
 
 
  subroutine STO_force_f2f_detection_PRPRx()
    implicit none
   
    force_f2f_detection = .TRUE.
   
  end subroutine STO_force_f2f_detection_PRPRx

  subroutine STO_force_nc_detection_PRPRx()
    implicit none
   
    force_nc_detection = .TRUE.
   
  end subroutine STO_force_nc_detection_PRPRx


  !--------------------------------------------------------------------------------------------------  
  subroutine clean_memory_PRPRx
    implicit none
    integer(kind=4) :: i, j, k

    point_surf_PRPRx = 0.d0
    line_surf_PRPRx  = 0.d0
    surf_surf_PRPRx  = 0.d0

    call clean_memory_inter_meca_()

    call wcp_compute_contact_PRPRx(.true.)
    call f2f4all_compute_contact_PRPRx(0.d0,.true.)
    
    nb_POLYR       = 0
    nb_PRPRx       = 0
    nb_vPRPRx      = 0
    nb_recup_PRPRx = 0

    if( allocated(visavis) ) then
      do i = 1, size(visavis)
        if( associated(visavis(i)%iff_cd) ) deallocate(visavis(i)%iff_cd)
        if( associated(visavis(i)%iff_an) ) deallocate(visavis(i)%iff_an)

        if( associated(visavis(i)%cd_lcoor) ) deallocate(visavis(i)%cd_lcoor)
        if( associated(visavis(i)%an_lcoor) ) deallocate(visavis(i)%an_lcoor)

        if( associated(visavis(i)%index)   ) deallocate(visavis(i)%index)
        if( associated(visavis(i)%pt_area) ) deallocate(visavis(i)%pt_area)

        if( associated(visavis(i)%face_ctc)   ) deallocate(visavis(i)%face_ctc)
        if( associated(visavis(i)%face_sizes) ) deallocate(visavis(i)%face_sizes)
      end do
      deallocate(visavis)
    end if

    if( allocated(box) ) then
      do i = 1, size(box,3)
        do j = 1, size(box,2)
          do k = 1, size(box,1)
            if( associated(box(k,j,i)%which) ) deallocate(box(k,j,i)%which)
          end do
        end do
      end do
      deallocate(box)
    end if

    nb_rough_PRPRx = 0
    if( allocated(rough_PRPRx) ) deallocate(rough_PRPRx)

    ! Root, Current and Previous should always be null outside creation_tab_visu

    if( allocated(PRcoor) ) deallocate(PRcoor)

    Reac_PRPRx_MAX = 0.D0

    module_checked_ = .FALSE.
    check_PRPRx_    = .FALSE.

  end subroutine
  !--------------------------------------------------------------------------------------------------  
 
 
SUBROUTINE Draw_Polygon(nom,points,sizes,idp,P,s,t)

    implicit none

    CHARACTER(len=21)                            :: nom
    REAL(kind=8),DIMENSION(:,:)                  :: points
    INTEGER,     DIMENSION(:),          OPTIONAL :: idp
    INTEGER,     DIMENSION(:),  POINTER,OPTIONAL :: sizes
    REAL(kind=8),DIMENSION(3),          OPTIONAL :: P,s,t
    
    INTEGER                                      :: nbp,ipoly,i,isum
    REAL(kind=8),DIMENSION(3)                    :: PP,ss,tt
    INTEGER,     DIMENSION(:),  ALLOCATABLE      :: sz
    REAL(kind=8),DIMENSION(:,:),ALLOCATABLE      :: xyz
    character(len=100)                           :: filename

                                !123456789012345678
    CHARACTER(len=18)  :: IAM = 'PRPRx::DrawPolygon'
    
    print*,IAM," -> ",nom

    nbp = size(points,dim=2)
    allocate(xyz(3,nbp))
    if ( present(sizes) ) then
        allocate(sz(size(sizes)))
        sz(:) = sizes(:)
    else
        allocate(sz(1))
        sz(1) = nbp
    endif
    if ( present(P) ) then
        PP(:) = P(:)
    else
        PP(:) = (/0d0, 0d0, 0d0/)
    endif
    if ( present(s) ) then
        ss(:) = s(:)
    else
        ss(:) = (/1d0, 0d0, 0d0/)
    endif
    if ( present(t) ) then
        tt(:) = t(:)
    else
        tt(:) = (/0d0, 1d0, 0d0/)
    endif

    if ( size(points,dim=1) == 2 ) then
    ! on est 2D, on revient dans le repere global
        if ( present(idp) ) then
            do i = 1,nbp
                xyz(:,i) = PP(:) + points(1,idp(i))*ss(:) + points(2,idp(i))*tt(:)
            enddo
        else
            do i = 1,nbp
                xyz(:,i) = PP(:) + points(1,i)*ss(:) + points(2,i)*tt(:)
            enddo
        endif
    else
    ! on est deja en 3D
        if ( present(idp) ) then
            do i = 1,nbp
                xyz(:,i) = points(:,idp(i))
            enddo
        else
            xyz(:,:) = points(:,1:nbp)
        endif
    endif

    filename = trim(location("DISPLAY/pg_"//nom//".vtk"))
    open(unit=87, file=filename)
    write(87,'(a)') "# vtk DataFile Version 2.0"
    write(87,'(a)') nom
    write(87,'(a)') "ASCII"
    write(87,'(a)') "DATASET POLYDATA"
    write(87,'(a,i3,a)') "POINTS",nbp," float"
    do i=1,nbp
        write(87,*) xyz(1,i),xyz(2,i),xyz(3,i)
    enddo
    ! pour chaque polygone
    isum = 0
    write(87,'(a,i3,i3)') "POLYGONS",size(sz),size(sz)+sum(sz)
    do ipoly = 1,size(sz)
        write(87,*) sz(ipoly),(/ (i-1, i=isum+1, isum+sz(ipoly)) /)
        isum = isum + sz(ipoly)
    enddo
    close(87)
    
    deallocate(sz,xyz)

  END SUBROUTINE Draw_Polygon
 
END MODULE PRPRx

