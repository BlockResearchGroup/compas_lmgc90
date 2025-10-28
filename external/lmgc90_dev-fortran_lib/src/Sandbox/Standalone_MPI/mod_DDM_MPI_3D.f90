          !----------------------------------------------------------------!
          !   NSCDD MPI 3D avec topoligie de communication maitre/esclave  !
          !         Écrit par : Vincent Visseq & Alexandre Martin          !
          !      Module importé dans SPSP_standalone_DDM_MPI_3D.f90        !
          !               et PRPR_standalone_DDM_MPI_3D.f90                !
          !----------------------------------------------------------------!

module DDM_MPI_3D 

use MPI

use overall
use parameters
use utilities
use RBDY3
use SPHER
use POLYR
use SPSPx, only : set_anonymous_to_rough_SPSPx,    & 
                  set_interactions_to_rough_SPSPx, &
                  get_nb_INTRF_SPSPx,              & 
                  get_list_INTRF_SPSPx,            &
                  get_nb_SPSPx,                    &
                  write_xxx_Vloc_Rloc_SPSPx
use PRPRx, only : set_anonymous_to_rough_PRPRx,    & 
                  set_interactions_to_rough_PRPRx, &
                  get_nb_INTRF_PRPRx,              & 
                  get_nb_PRPRx,                    & 
                  write_xxx_Vloc_Rloc_PRPRx,       &
                  get_list_INTRF_PRPRx
use inter_meca_handler_3D, only : get_gaps
use nlgs_3D, only : shift_icdan,                            &
                    RnodHRloc_nlgs,                         &
                    compute_local_free_vlocy,               &
                    get_nb_adjac_nlgs_3D,                   &
                    prep_check_nlgs, solve_nlgs, get_error, &
                    compute_convergence_norms_nlgs, check_convergence_nlgs
use anonymous_ptr_container, only : get_object               => get_data                   , &
                                    get_nb_objects           => get_nb_data                , &
                                    get_status                                             , &
                                    close_container          => close_ptr_container        , &
                                    open_container           => open_ptr_container         , &
                                    add_object_to_container  => add_object_to_ptr_container, &
                                    display_object_container => display_ptr_container      , &
                                    erase_container          => erase_ptr_container        , &
                                    container                => PTR_CONTAINER
use anonymous
use rough_detections
use tact_behaviour ! pour chercher la plus grande distance d'alerte
use a_matrix ! pour utiliser les G_matrix

! TEST TEST TEST
use timer 



implicit none

!am: on declare tout prive (a priori) pour eviter les effets de bord
private

! am : ancienne archi => on ne travaille qu'avec des disques (et des clusters de disques)

integer(kind=4)                                :: nbody          ! nombre "vrai" de RBDY3 
integer(kind=4)                                :: ntact_SPHER    ! nombre de contacteurs spheres
integer(kind=4)                                :: ntact_POLYR    ! nombre de contacteurs polyedres

logical                                        :: first_real_step = .true.      ! pour savoir si l'on est au premier pas de temps

!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
!                                       Base de donnée du processus maitre
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
! Pour le monitoring
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4) :: nb_RBDY3_interf4all
integer(kind=4) :: nb_fine_interactions4all
integer(kind=4) :: nb_INTRF4all    
integer(kind=4) :: nb_adjac4all
real(kind=8)    :: mean_nb_RBDY3_interf4all
real(kind=8)    :: mean_nb_interactions4all
real(kind=8)    :: mean_nb_fine_interactions4all
real(kind=8)    :: mean_nb_liens_interf4all
real(kind=8)    :: mean_nb_INTRF4all
real(kind=8)    :: mean_nb_adjac4all
real(kind=8)    :: mean_nb_migrations4all

!-------------------------------------------------------------------------------------------------------------------
! Tableaux des coordonnées d'entités et leur sous-domaine (un seul et unique)
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:, :), allocatable     :: coord_ci       ! coordonnees des centres d'inertie des RBDY3
real(kind=8), dimension(:, :), allocatable     :: coord_cc       ! coordonnees des centres des contacts
integer(kind=4), dimension(:), allocatable     :: repart_sdm_ci  ! listes des sdm auquels appartiennent les centres d'inertie des RBDY3
integer(kind=4), dimension(:), allocatable     :: repart_sdm_ct  ! listes des sdm auquels appartiennent les contacteurs 
integer(kind=4), dimension(:), allocatable     :: repart_sdm_cc  ! listes des sdm auquels appartiennent les centres des contacts 
real(kind=8), dimension(:, :), allocatable     :: coord_ct       ! coordonnees des centres d'inertie des contacteurs
real(kind=8), dimension(:), allocatable        :: radius_ct      ! rayons des contacteurs
real(kind=8), dimension(:,:), allocatable      :: minpos_POLYR   ! position du point (xmin, ymin, zmin) de la boite englobante de chaque POLYR
real(kind=8), dimension(:,:), allocatable      :: maxpos_POLYR   ! position du point (xmax, ymax, zmax) de la boite englobante de chaque POLYR

!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques (3 types de masques, car en multidomaine séquentiel ça complique un peut)
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4), dimension(:), allocatable     :: multiplicite   ! vecteur recenssant le nombre de participation aux sdm des corps
integer(kind=4), dimension(:), allocatable     :: multiplicite_old ! vecteur recenssant le nombre de participation aux sdm des corps
                                                                 ! à la répartition en sous-domaines précédente
integer(kind=4)                                :: max_multi      ! maximum de multiplicité pour une particule
integer(kind=4), dimension(:,:), allocatable   :: body_particip  ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                                 ! participent chaque corps (alloué dans init_dd)
integer(kind=4), dimension(:,:), allocatable   :: body_particip_old ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                                 ! participent chaque corps (alloué dans init_dd) à la répartiction en sous-domaine précédente
integer(kind=4), dimension(:,:), allocatable   :: migration_tab  ! table des migrations par sous-domaines
integer(kind=4)                                :: nb_migrations  ! nombre de migrations
logical, dimension(:,:), allocatable           :: mask_particip  ! matrice (/ nbody,Nsdm /) permettant de déterminer, pour
                                                                 ! chaque sous-domaines, quels sont les objets visibles par ce sdm


!-------------------------------------------------------------------------------------------------------------------
! Info pour la sous-structuration géométrique de l'échantillon
!-------------------------------------------------------------------------------------------------------------------
real(kind=8)                                   :: Xleft, Xright ! caractéristiques de la boite englobante : bornes en x 
real(kind=8)                                   :: Yleft, Yright ! caractéristiques de la boite englobante : bornes en y 
real(kind=8)                                   :: Zup, Zdown    ! caractéristiques de la boite englobante : bornes en z
integer(kind=4)                                :: Nsdm1, Nsdm2, Nsdm3 ! nombre de sous-domaines suivant chaque direction
integer(kind=4)                                :: Nsdm                ! nombre total de sous-domaines 
real(kind=8), dimension(3)                     :: dim_sdm             ! dimensions des sous domaines "DDM"
real(kind=8)                                   :: TimeStep            ! pas de temps (fixe) utilisé pour le calcul

logical, dimension(6)                          :: imposed_boundaries = (/ .false., .false., .false., .false., .false., .false. /)
                                                     ! vaut "vrai" ssi la borne courante a ete donnee
                                                     !   imposed_boundaries(1) = .true. <=> Xleft fixe
                                                     !   imposed_boundaries(2) = .true. <=> Xright fixe
                                                     !   imposed_boundaries(3) = .true. <=> Yleft fixe
                                                     !   imposed_boundaries(4) = .true. <=> Yright fixe
                                                     !   imposed_boundaries(5) = .true. <=> Zdown fixe
                                                     !   imposed_boundaries(6) = .true. <=> Zup fixe
real(kind=8), dimension(6)                     :: boundaries = (/ 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0 /) 
                                                     ! valeurs des bornes fixees
                                                     !    Xleft  = boundaries(1), si Xleft fixe
                                                     !    Xright = boundaries(2), si Xright fixe
                                                     !    Yleft  = boundaries(3), si Yleft fixe
                                                     !    Yright = boundaries(4), si Yright fixe
                                                     !    Zdown  = boundaries(5), si Zdown fixe
                                                     !    Zup    = boundaries(6), si Zup fixe
integer, parameter, public                     :: i_Xleft = 1, i_Xright = 2, i_Yleft = 3, i_Yright = 4, i_Zdown = 5, i_Zup = 6
                                                     ! corespondance indice / borne

!-------------------------------------------------------------------------------------------------------------------
! Structures associées à la detection des contacts grossiers
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER),save    :: rough_contact     ! table de visibilité
 integer                 :: nb_rough_contact  ! taille table de visibilité (tous contatcs confondus)
 integer                 :: nb_rough_contact_SPSPx ! taille table de visibilité (contacts sphere/sphere)
 integer                 :: nb_rough_contact_PRPRx ! taille table de visibilité (contacts polyedre/polyedre)
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), dimension(:), allocatable :: splitted_rough ! rough_contact découpé par sdm
 integer(kind=4), dimension(:), allocatable :: nb_splitted_rough ! nombre de contact par sous domaines
 integer(kind=4), dimension(:), allocatable :: nb_splitted_rough_SPSPx ! nombre de contact par sous domaines (contacts sphere/sphere)
 integer(kind=4), dimension(:), allocatable :: nb_splitted_rough_PRPRx ! nombre de contact par sous domaines (contatcs polyedre/polyedre)
 integer(kind=4), dimension(:), allocatable :: interactions4all     ! interactions contaténées 
 integer(kind=4)                            :: nb_interactions4all  ! nb_interactions totales
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
! Structures pour gérer les listes des interfaces (sous-domaines et globale)
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type T_ligne_i
      integer(kind=4), dimension(:), pointer :: particule => null()
 end type T_ligne_i
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type T_ligne_r
      real(kind=8), dimension(:,:), pointer :: particule => null()
 end type T_ligne_r
!-------------------------------------------------------------------------------------------------------------------

! Données des interfaces dans les sous-domaines
!-------------------------------------------------------------------------------------------------------------------
type(T_ligne_i), dimension(:), allocatable  :: liste_RBDY3_interf_sdm      ! structure des indices par sdm 
type(T_ligne_i), dimension(:), allocatable  :: liste_RBDY3_interf_sdm_old  ! structure des indices par sdm au pas -1
type(T_ligne_r), dimension(:), allocatable  :: V_RBDY3_interf_sdm          ! vitesses des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: V_RBDY3_interf_sdm_old      ! vitesses des RBDY3 d'interface par sdm old
type(T_ligne_r), dimension(:), allocatable  :: X_RBDY3_interf_sdm          ! positions des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: localFrame_RBDY3_interf_sdm ! reperes principaux d'inertie des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY3_interf_sdm         ! F_gamma des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY3_interf_sdm_old     ! F_gamma des RBDY3 d'interface par sdm (pas -1)
type(T_ligne_r), dimension(:), allocatable  :: DFg_RBDY3_interf_sdm        ! DF_gamma des RBDY3 d'interface par sdm 
integer(kind=4), dimension(:), allocatable  :: nb_RBDY3_interf_sdm         ! nombre de corps d'interface par sdm

! Données de l'interface globale 
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4)                             :: nb_RBDY3_interf_glob         ! nombre de particule de l'interface globale
integer(kind=4), dimension(:), allocatable  :: interface_globale            ! tableau (/ nb_RBDY3_interf_globale /), 
                                                                            ! avec pour chaque particule d'interface (indice de dim=2)
                                                                            ! son indice global
integer(kind=4)                             :: nb_liens_interf              ! nombre global de liens d'interface
real(kind=8), dimension(:,:), allocatable   :: saut_V_interf_glob           ! sauts de vitesses des RBDY3 d'interface globale 
real(kind=8), dimension(:,:), allocatable   :: V_RBDY3_interf_glob          ! vitesses à convergence des RBDY3 d'interface globale 
real(kind=8), dimension(:,:), allocatable   :: X_RBDY3_interf_glob          ! position à convergence des RBDY3 d'interface globale 
real(kind=8), dimension(:,:), allocatable   :: localFrame_RBDY3_interf_glob ! reperes principaux d'inertie à convergence des RBDY3 d'interface globale 

!-------------------------------------------------------------------------------------------------------------------
! BDD pour les communications MPI
!-------------------------------------------------------------------------------------------------------------------

integer     ,    dimension(:), allocatable :: vect_nb_send_host ! Vecteur du nombre d'éléments envoyés par l'hôte
integer                                    :: nb_send_host      ! Nombre d'éléments envoyés par l'hôte
integer     ,    dimension(:), allocatable :: vect_shift        ! Vecteur de décalage d'indice pour répartir un
                                                                ! vecteur sur les différents processus
integer     ,    dimension(:), allocatable :: vect_send_host_I  ! Vecteur d'entiers "concaténation" des éléments
                                                                ! à distribuer aux processus
real(kind=8),    dimension(:), allocatable :: vect_send_host_R  ! Vecteur de réels "concaténation" des éléments
                                                                ! à distribuer aux processus
integer     ,    dimension(:), allocatable :: vect_nb_recv_host ! Vecteur du nombre d'éléments recus par l'hôte
integer                                    :: nb_recv_host      ! Nombre d'éléments envoyés par l'hôte
real(kind=8),    dimension(:), allocatable :: vect_recv_host_R  ! Vecteur de réels "concaténation" des éléments
                                                                ! à récupérer des différents processus

logical     ,    dimension(:), allocatable :: mask_part4all     ! Vecteur (/nb_procs*nbody/)=(/mask(1),...,mask(n)/)

integer     ,    dimension(:), allocatable :: mig_indices4all   ! Vecteur des indices des particules migrantes
real(kind=8),    dimension(:), allocatable :: mig_etats4all     ! Vecteur des états des particules migrantes
integer     ,    dimension(:), allocatable :: mig_indices_host  ! Vecteur des indices des particules migrantes
real(kind=8),    dimension(:), allocatable :: mig_etats_host    ! Vecteur des états des particules migrantes

real(kind=8),    dimension(:), allocatable :: V_interf4all      ! Vecteur des vitesses d'interface concaténés
real(kind=8),    dimension(:), allocatable :: X_interf4all      ! Vecteur des deplacements d'interface concaténés
real(kind=8),    dimension(:), allocatable :: localFrame_interf4all      ! Vecteur des orientations des reperes principaux d'inertie d'interface concaténés
real(kind=8),    dimension(:), allocatable :: DFg_interf4all    ! Vecteur des incréments DFg concaténés
real(kind=8),    dimension(:), allocatable :: Vbeg4all  
real(kind=8),    dimension(:), allocatable :: Xbeg4all  
integer(kind=4), dimension(:), allocatable :: Ibeg4all  
real(kind=8),    dimension(:), allocatable :: localFrameTT4all  ! Matrice d'orientation du repere principal d'inertie
                                                                ! dans la configuration de detection.
real(kind=8),    dimension(:), allocatable :: V4all  
real(kind=8),    dimension(:), allocatable :: X4all  
integer(kind=4), dimension(:), allocatable :: I4all  
real(kind=8),    dimension(:), allocatable :: localFrame4all    ! Matrice d'orientation du repere principal d'inertie
                                                                ! dans la configuration en fin de pas de temps.
real(kind=8),    dimension(:), allocatable :: Bbox4all          ! Boite englobante des contacteurs POLYR

!-------------------------------------------------------------------------------------------------------------------
! Informations liées au postraitement
!-------------------------------------------------------------------------------------------------------------------
real(kind=8)      :: QuadDV, MaxmDV, QuadDVR, & ! Quantites necessaires pour statuer de la convergence
                     MaxmDVR, MeanDVoR          ! du solveur de contact
real(kind=8)      :: egluing                    ! Erreur de recollement sur l'ensemble de l'interface
integer(kind=4)   :: prevailing_criterion = 0   ! 0 => initialisation, 1 => dans les sdm, 2 => sur l'interface
integer(kind=4)   :: Nactif4all                 ! Nombre de contacts actifs dans l'ensemble de l'échantillon
integer(kind=4)   :: nfich_solv_inf             ! Numéro du fichier pour l'écriture de SOLVER_INFORMATIONS.DAT
integer(kind=4)   :: nfich_sdm_inf              ! Numéro du fichier pour l'écriture de SOLVER_INFORMATIONS.DAT
integer(kind=4)   :: nfich_mean_sdm_inf         ! Numéro du fichier pour l'écriture de SOLVER_INFORMATIONS.DAT
integer(kind=4)   :: nfich_sample_inf           ! Numéro du fichier pour l'écriture de SOLVER_INFORMATIONS.DAT
integer(kind=4)   :: nfich_mean_sample_inf      ! Numéro du fichier pour l'écriture de SOLVER_INFORMATIONS.DAT
integer(kind=4)   :: nfich_viol_evol            ! Numéro du fichier pour l'écriture de VIOLATION_EVOLUTION.DAT
integer(kind=4)   :: nfich_triax_compac         ! Numéro du fichier pour l'écriture de TRIAXIAL_COMPACITY.DAT
logical           :: solv_info = .false.        ! Pour savoir si on fait le postraitement correspondant
logical           :: sdm_info = .false.         ! Pour savoir si on fait le postraitement correspondant
logical           :: mean_sdm_info = .false.    ! Pour savoir si on fait le postraitement correspondant
logical           :: sample_info = .false.      ! Pour savoir si on fait le postraitement correspondant
logical           :: mean_sample_info = .false. ! Pour savoir si on fait le postraitement correspondant
logical           :: viol_evol = .false.        ! Pour savoir si on fait le postraitement correspondant
logical           :: triax_compac = .false.     ! Pour savoir si on fait le postraitement correspondant
character(len=31) :: clout_solv_inf             ! Nom pour les fichiers de postraitement
character(len=34) :: clout_sdm_inf              ! Nom pour les fichiers de postraitement
character(len=39) :: clout_mean_sdm_inf         ! Nom pour les fichiers de postraitement
character(len=31) :: clout_sample_inf           ! Nom pour les fichiers de postraitement
character(len=36) :: clout_mean_sample_inf      ! Nom pour les fichiers de postraitement
character(len=31) :: clout_viol_evol            ! Nom pour les fichiers de postraitement
character(len=30) :: clout_triax_compac         ! Nom pour les fichiers de postraitement

!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
!                                  Base de donnée d'un processus esclave 
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------

integer     :: nb_send_slave ! Nombre d'éléments envoyés par l'esclave
integer     :: nb_recv_slave ! Nombre d'éléments reçus par l'esclave

real(kind=8), dimension(:)  , allocatable :: Vbeg_slave  
real(kind=8), dimension(:)  , allocatable :: Xbeg_slave  
integer     , dimension(:)  , allocatable :: Ibeg_slave  
real(kind=8), dimension(:)  , allocatable :: localFrameTT_slave  ! Matrice d'orientation du repere principal d'inertie,
                                                                 ! dans la configuration de detection.
real(kind=8), dimension(:)  , allocatable :: V_slave  
real(kind=8), dimension(:)  , allocatable :: X_slave  
integer     , dimension(:)  , allocatable :: I_slave  
real(kind=8), dimension(:)  , allocatable :: localFrame_slave    ! Matrice d'orientation du repere principal d'inertie,
                                                                 ! dans la configuration en fin de pas de temps.
real(kind=8),    dimension(:), allocatable :: Bbox_slave         ! Boite englobante des contacteurs POLYR

!-------------------------------------------------------------------------------------------------------------------
! Pour le monitoring
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4) :: nb_RBDY3_slave
integer(kind=4) :: nb_fine_interactions_slave
integer(kind=4) :: nb_adjac_slave
real(kind=8)    :: mean_nb_RBDY3_slave
real(kind=8)    :: mean_nb_RBDY3_interf_slave
real(kind=8)    :: mean_nb_interactions_slave
real(kind=8)    :: mean_nb_fine_interactions_slave
real(kind=8)    :: mean_nb_INTRF
real(kind=8)    :: mean_nb_adjac_slave

!-------------------------------------------------------------------------------------------------------------------
! Gestion des masques et des migrations 
!-------------------------------------------------------------------------------------------------------------------
logical        , dimension(:), allocatable :: mask_in_slave      ! Vecteur (/nbody/) du statut de visibilité par sdm
integer(kind=4), dimension(:), allocatable :: mig_indices_slave  ! table indices des migrations vers le sdm courant
real(kind=8)   , dimension(:), allocatable :: mig_etats_slave    ! table états des migrations vers le sdm courant
integer(kind=4)                            :: nb_mig_slave=0     ! nombre de migrations vers le sdm courant

!-------------------------------------------------------------------------------------------------------------------
! Données des interfaces par sous-domaine
!-------------------------------------------------------------------------------------------------------------------
integer                                   :: nb_RBDY3_interf_slave           ! nombre de corps d'interface du sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY3_interf_slave        ! indices des RBDY3 d'interface sur sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY3_interf_slave_old    ! structure des indices du sdm au pas -1
real(kind=8), dimension(:)  , allocatable :: V_RBDY3_interf_slave            ! vitesses des RBDY3 d'interface du sdm courant 
real(kind=8), dimension(:)  , allocatable :: V_RBDY3_interf_slave_old        ! idem au pas -1 
real(kind=8), dimension(:)  , allocatable :: X_RBDY3_interf_slave            ! deplacements des RBDY3 d'interface du sdm courant 
real(kind=8), dimension(:)  , allocatable :: localFrame_RBDY3_interf_slave   ! orientations des reperes principaux d'inertie des RBDY3 d'interface du sdm courant 
real(kind=8), dimension(:)  , allocatable :: Fg_RBDY3_interf_slave           ! F_gamma des RBDY3 d'interface du sdm courant
real(kind=8), dimension(:)  , allocatable :: Fg_RBDY3_interf_slave_old       ! F_gamma des RBDY3 d'interface du sdm (pas-1)
real(kind=8), dimension(:)  , allocatable :: DFg_RBDY3_interf_slave          ! DF_gamma des RBDY3 d'interface du sdm courant
logical                                   :: repartition_just_made = .false. ! Permet de savoir si la répartition
                                                                             ! en sous-domaine viens d'être effectuée      
!-------------------------------------------------------------------------------------------------------------------
! Gestion des masses
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:), allocatable        :: masse_ref      ! vecteur contenant les masses de reference pour chaque corps
real(kind=8), dimension(:), allocatable        :: masses_courantes! vecteur contenant les masses courantes pour chaque corps
                                                                 ! taille (/Nsdm*nbody/)
!-------------------------------------------------------------------------------------------------------------------
! BDD associée à la detection des contacts
!-------------------------------------------------------------------------------------------------------------------
integer, dimension(:), allocatable :: interactions_slave          ! Tableau des intéractions du sdm
integer                            :: nb_interactions_slave       ! Nombre de contacts grossiers du sdm 
integer                            :: nb_interactions_slave_SPSPx ! Nombre de contacts grossiers SPSPx du sdm 
integer                            :: nb_interactions_slave_PRPRx ! Nombre de contacts grossiers PRPRx du sdm 
integer                            :: nb_INTRF                    ! Nombre de contacts fins taggés 'INTRF'
integer,dimension(:), allocatable  :: list_INTRF                  ! Liste des indices des contacts taggés 'INTRF'

!-------------------------------------------------------------------------------------------------------------------
! BDD pour les communications MPI
!-------------------------------------------------------------------------------------------------------------------
integer      :: nb_procs,rang,code
real(kind=8) :: secondsTT,MPI_MAX_time              ! Calcul du temps maximal passé par un processus pour
                                                    ! l'ensemble de la simulation 
integer      :: slave_io

! Timers internes au module
integer :: id_prep_scatter_DFg, id_scatter_DFg, id_post_scatter_DFg
integer :: id_prep_gather_V, id_gather_V, id_post_gather_V

!-------------------------------------------------------------------------------------------------------------------
! Fin des déclarations des variables globales au module
!-------------------------------------------------------------------------------------------------------------------


!am: on donne la liste des fonctions publiques (utilisables hors du module)
public                                &
   init_dd,                           &
   set_domain_boundary,               &
   init_MPI,                          &
   set_working_directory_in_DDM,      &
   mpi_finalize_process,              &
   new_dd_slave,                      &
   new_dd_host,                       &
   allocations_fantome_slave,         &
   gather_X_V_begin,                  &
   gather_X_V,                        &
   gather_POLYR_TT,                   &
   init_POLYR_TT,                     &
   scatter_creation_domaines,         &
   gather_V_interface,                &
   gather_X_V_localFrame_interface,   &
   scatter_DFg,                       &
   set_visibility_4all_in_DDM,        &
   set_skip_display_cluster,          & 
   creation_domaines,                 &
   fix_migrants,                      &
   set_modified_mass_in_DDM,          &
   set_interactions_to_rough_in_DDM,  &
   erase_rough_contact,               &
   erase_splitted_rough,              &
   get_list_INTRF_in_DDM,             &
   RnodHRloc_list_in_DDM,             &
   compute_local_free_vlocy_in_DDM,   &
   stock_Fg_liste_old,                &
   set_F_gamma,                       &                   
   prep_exchange_inloop,              &
   comp_V_list_RBDY3_in_DDM,          & 
   comp_Vfree_list_RBDY3_in_DDM,      &
   compute_DF_gamma,                  &
   set_DF_gamma,                      &                   
   compute_egluing,                   &
   check_convergence_in_DDM,          &
   scatter_V_interface,               &
   scatter_X_V_localFrame_interface,  &
   fix_V_interface,                   &
   fix_X_V_localFrame_interface,      &
   print_info,                        &
   print_step,                        &
   print_iter,                        &
   init_postpro_in_DDM,               &  ! Oh & Os
   postpro_during_in_DDM,             &  ! Oh & Os
   postpro_last_in_DDM,               &  ! Oh & Os
   write_OUT_in_DDM,                  &
   write_LAST_in_DDM,                 &
   clean_ddm

contains

!-------------------------------------------------------------------------------------------------------------------
 subroutine init_MPI

    implicit none

    call MPI_INIT(code)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs,code)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rang,code)

    print *,  "nb_procs=",nb_procs, "rang=",rang

    secondsTT = MPI_Wtime ( )

    slave_io=1000+rang


 end subroutine init_MPI
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 subroutine set_working_directory_in_DDM

    implicit none

    ! variables locales
    character(len=13) :: working_directory ! pour calculer le nom du dossier d'écriture des DOF et Vloc
    integer           :: sdm

    ! Détermination du sous-domaine qui écrit
    sdm=rang+1
    !                  1234567890123
    working_directory='DDM_WD_xxxxx/'
    write(working_directory(8:12), '(I5.5)') sdm

    call set_working_directory(working_directory)

 end subroutine set_working_directory_in_DDM
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 subroutine print_info(nb_time_steps,TimeStep,Theta_,freq_DISPLAY,freq_OUTBOX,DDMfreq, &
                        Nsdm1,Nsdm2,Nsdm3,CVTYPE,TOL,RELAX,itloop1,itloop2) 

    implicit none

    ! Variables d'entrée
    integer, intent(in)         :: nb_time_steps,freq_DISPLAY,freq_OUTBOX,DDMfreq, &
                                    Nsdm1,Nsdm2,Nsdm3,itloop1,itloop2
    real(kind=8),intent(in)     :: TimeStep,Theta_! Pas de temps et paramètre de la theta méthode
    real(kind=8),intent(in)     :: TOL,RELAX      ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
    CHARACTER(len=5),intent(in) :: CVTYPE         ! Type de convergence : QUAD, MAX, QUAD/16, etc...


                             !12345678901234567890
    character(len=18) :: IAM='DDM_3D::print_info'

    if ( rang /= 0 ) return

    !-------------------------------------------------------------------------------
    ! Ecriture à l'écran des mêmes paramètres
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! loading step            : ',nb_time_steps
    WRITE(*,*) '! TIME STEP               : ',TimeStep
    WRITE(*,*) '! THETA                   : ',Theta_
    WRITE(*,*) '! WRITE DISPLAY FREQUENCY : ',freq_DISPLAY
    WRITE(*,*) '! WRITE OUTBOX FREQUENCY  : ',freq_OUTBOX
    !-------------------------------------------------------------------------------
    ! Paramètres de découpages en sous_domaines
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! FREQUENCE DETECT/REPART : ',DDMfreq
    WRITE(*,*) '! Nsdm1, Nsdm2 et Nsdm3   : ',Nsdm1,Nsdm2,Nsdm3
    !-------------------------------------------------------------------------------
    ! Paramètres d'itération de la boucle DDM
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! NLGS CHECK TYPE         : ',CVTYPE,TOL
    WRITE(*,*) '! RELAX                   : ',RELAX
    WRITE(*,*) '! iteration & more        : ',itloop1,itloop2

 end subroutine print_info
!-------------------------------------------------------------------------------------------------------------------

 ! fonction qui initialise le module DDM
 function init_dd(Nsdm1_, Nsdm2_, Nsdm3_)

    implicit none

    ! variables d'entrée
    integer, intent(in)      :: Nsdm1_, Nsdm2_, Nsdm3_

    ! valeur de retour
    integer :: init_dd

    ! variables locales
                             !12345678901234567890
    character(len=15) :: IAM='DDM_3D::init_dd'

    Nsdm1=Nsdm1_
    Nsdm2=Nsdm2_
    Nsdm3=Nsdm3_

    ! on calcule le nombre total de sous-domaines
    Nsdm=Nsdm1*Nsdm2*Nsdm3
    ! calcul de la multiplicité maximale pour une particule
    max_multi=max(8,Nsdm1*Nsdm2,Nsdm1*Nsdm3,Nsdm2*Nsdm3) ! 8 pour le 3D. un cluster en diagonale ferait tout sauter
    ! on le renvoie
    init_dd=Nsdm

    if (Nsdm/=nb_procs)  call faterr(IAM, "Le nombre de processus doit etre &
            & strictement egal au nombre de sous-domaines !!")

 end function init_dd

 !am: fonction qui permet d'imposer un valeur a une borne Xleft, Xright, Yleft, Yright, Zdown ou Zup, afin de contraindre la
 !    repartition des sous-domaines dans l'espace
 subroutine set_domain_boundary(id_boundary, value)

    implicit none

    ! variables d'entree

    ! indice de la frontiere concerenee
    integer, intent(in) :: id_boundary
    ! valeu associee
    real(kind=8), intent(in) :: value

    ! variables locales         123456789012345678901234567
    character(len=27) :: IAM = 'DDM_3D::set_domain_boundary'

    select case(id_boundary)
       case(i_Xleft)
          imposed_boundaries(i_Xleft)  = .true.
          boundaries(i_Xleft)          = value
       case(i_Xright)
          imposed_boundaries(i_Xright) = .true.
          boundaries(i_Xright)         = value
       case(i_Yleft)
          imposed_boundaries(i_Yleft)  = .true.
          boundaries(i_Yleft)          = value
       case(i_Yright)
          imposed_boundaries(i_Yright) = .true.
          boundaries(i_Yright)         = value
       case(i_Zdown)
          imposed_boundaries(i_Zdown)  = .true.
          boundaries(i_Zdown)          = value
       case(i_Zup)
          imposed_boundaries(i_Zup)    = .true.
          boundaries(i_Zup)            = value
       case default
          call faterr(IAM, 'unexpected boundary index!')
    end select

 end subroutine

!am : procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
!     (les coordonnees des centre d'inertie, les numeros de sous-domaine pour chaque corps)
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd_slave(nb_RBDY3, ntact_SPHER_, ntact_POLYR_)

    implicit none

    ! variables d'entrée
    integer, intent(in)           :: nb_RBDY3
    integer, intent(in), optional :: ntact_SPHER_, ntact_POLYR_

    ! variables locales
    integer                       :: ibody, err

                                         !12345678901234567890
    character(len=20)             :: IAM='DDM_3D::new_dd_slave'
    character(len=20)             :: filename

    !print*, "nb_RBDY3=", nb_RBDY3
    !print*, "ntact_=", ntact_

    ! recuperation du nombre de corps
    nbody=nb_RBDY3
    ! recuperation du nombre de contacteurs
    !    * spheres
    if (present(ntact_SPHER_)) then
       ntact_SPHER=ntact_SPHER_
    else
       ntact_SPHER=0
    end if
    !    * polyedres
    if (present(ntact_POLYR_)) then
       ntact_POLYR=ntact_POLYR_
    else
       ntact_POLYR=0
    end if

!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques
!-------------------------------------------------------------------------------------------------------------------
    !    * table multiplicite
    if (.not. allocated(multiplicite)) then
       allocate(multiplicite(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de multiplicite")
    end if


    !    * tableau des corps visibles par sous-domaines
    if (.not. allocated(mask_in_slave)) then
       allocate(mask_in_slave(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_in_slave")
    end if

!-------------------------------------------------------------------------------------------------------------------
! Gestion des masses
!-------------------------------------------------------------------------------------------------------------------
    !    * table des masses de reference 
    if (.not. allocated(masse_ref)) then
       allocate(masse_ref(nbody), stat=err)
       if (err/=0) call faterr(IAM, "erreur d'allocation du vecteur des masses de reference")
    end if
    do ibody=1,nbody
       masse_ref(ibody) = get_mass(ibody)
    end do
    !    * table des masses courantes 
    if (.not. allocated(masses_courantes)) then
       allocate(masses_courantes(nbody), stat=err)
       if (err/=0) call faterr(IAM, "erreur d'allocation du vecteur des masses courantes")
    end if
    masses_courantes=0.D0







    ! TEST TEST TEST                        12345678901234567890
    id_prep_scatter_DFg = get_new_utimer_ID('PREP SCATTER DFg    ') 
    id_scatter_DFg      = get_new_utimer_ID('SCATTER DFg         ') 
    id_post_scatter_DFg = get_new_utimer_ID('POST SCATTER DFg    ') 
                                     !   12345678901234567890
    id_prep_gather_V = get_new_utimer_ID('PREP GATHER V       ') 
    id_gather_V      = get_new_utimer_ID('GATHER V            ') 
    id_post_gather_V = get_new_utimer_ID('POST GATHER V       ') 

 end subroutine new_dd_slave
!----------------------------------------------------------------

 !am : procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
 !     (les coordonnees des centre d'inertie, les numeros de sous-domaine pour chaque corps)
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd_host

    implicit none

    ! variables locales
    integer                       :: isdm, ibody, err
    integer                       :: isee

                                         !123456789012345678901
    character(len=21)             :: IAM='DDM_3D::new_dd_maitre'

    ! Seul le processus maitre alloue les structures qui y correspondent 
    if ( rang /= 0 ) return

    print *, "nombre_de_RBDY3", nbody

    print*, "nombre de SPHER", ntact_SPHER
    print*, "nombre de POLYR", ntact_POLYR

!-------------------------------------------------------------------------------------------------------------------
! Tableaux des coordonnées d'entités et leur sous-domaine (un seul et unique)
!-------------------------------------------------------------------------------------------------------------------
    !    * les coordonnees des centres d'inertie des RBDY3
    if (.not. allocated(coord_ci)) then
       !am: dans le cas des RBDY3, le calcul des rotations passe par le recalcul du repere
       !    => les coordonnees manipulees ne sont que des translations 
       allocate(coord_ci(3, nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des centres d'inerties des RBDY3")
    end if

    !    * numeros de sous-domaines des centres d'inertie des corps
    if (.not. allocated(repart_sdm_ci)) then
       allocate(repart_sdm_ci(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de la liste indicatrice des sous-domaines")
    end if

    !    * les coordonnees des centres d'inertie des contacteurs spheres et polyedres         !
    if (.not. allocated(coord_ct)) then                                                       !
       allocate(coord_ct(3, ntact_SPHER + ntact_POLYR), stat=err)                             !
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des centres d'inertie des "&
          &"contacteurs (SPHER + POLYR)")
    end if                                                                                    !
    !    * les rayons des contacteurs spheres et polyedres                                    !
    if (.not. allocated(radius_ct)) then                                                      !
       allocate(radius_ct(ntact_SPHER + ntact_POLYR), stat=err)                               !
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des rayons des contacteurs "&
          &"(SPHER + POLYR)")
    end if                                                                                    !

    if (nb_big_POLYR > 0) then
       !    * numeros de sous-domaines des contacteurs spheres et polyedres
       if (.not. allocated(repart_sdm_ct)) then
          allocate(repart_sdm_ct(ntact_SPHER + ntact_POLYR), stat=err)
          if (err/=0) call faterr(IAM, "Erreur d allocation de la liste indicatrice des sous-domaines")
       end if
    end if

    ! s'il ya des polyedres
    if (ntact_POLYR > 0) then
       !    * les coordonnees de la boite englobante des contacteures polyedres :
       !        - (xmin, ymin, zmin)
       if (.not. allocated(minpos_POLYR)) then                                                      !
          allocate(minpos_POLYR(3, ntact_POLYR), stat=err)                                          !
          if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees du points (xmin, ymin, zmin) "&
             &"des contacteurs POLYR")
       end if                                                                                    
       !        - (xmax, ymax, zmax)
       if (.not. allocated(maxpos_POLYR)) then                                                      !
          allocate(maxpos_POLYR(3, ntact_POLYR), stat=err)                                          !
          if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees du points (xmax, ymax, zmax) "&
             &"des contacteurs POLYR")
       end if                                                                                    !
    end if

!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques (3 types de masques, car en multidomaine séquentiel ça complique un peut)
!-------------------------------------------------------------------------------------------------------------------
    !    * tableau des corps visibles par sous-domaines
    if (.not. allocated(mask_particip)) then
       allocate(mask_particip(nbody, Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de la liste des corps visibles par sous-domaines")
    end if
    mask_particip=.false.

    !    * tableau de visibilité concaténé pour tous les sdm
    if (.not. allocated(mask_part4all)) then
       allocate(mask_part4all(nbody*Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_part4all")
    end if
    mask_part4all=.false.

    !    * table des sous-domaines auxquels participent les corps
    if (.not. allocated(body_particip)) then
       allocate(body_particip(max_multi, nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation des corps aux sdm")
    end if
    !    * table des sous-domaines auxquels participent les corps à la répartion DDM précédente
    if (.not. allocated(body_particip_old)) then
       allocate(body_particip_old(max_multi, nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation old des corps aux sdm")
    end if

    !    * table multiplicite_old
    if (.not. allocated(multiplicite_old)) then
       allocate(multiplicite_old(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de multiplicite_old")
    end if

    !    * tables des migrations
    if (.not. allocated(migration_tab)) then
       allocate(migration_tab(nbody,max_multi), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de migration_tab")
    end if

!-------------------------------------------------------------------------------------------------------------------
! Structures associées à la detection des contacts grossiers
!-------------------------------------------------------------------------------------------------------------------
    !    * table des contacts appartenants aux sdm
    if (.not. allocated(splitted_rough)) then
       allocate(splitted_rough(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de splitted_rough")
    end if

    !    * table des contacts appartenants aux sdm
    if (.not. allocated(nb_splitted_rough)) then
       allocate(nb_splitted_rough(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_splitted_rough")
    end if

    !    * table des contacts Sphere/Sphere appartenants aux sdm
    if (.not. allocated(nb_splitted_rough_SPSPx)) then
       allocate(nb_splitted_rough_SPSPx(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_splitted_rough_SPSPx")
    end if

    !    * table des contacts Polyedre/Polyedres appartenants aux sdm
    if (.not. allocated(nb_splitted_rough_PRPRx)) then
       allocate(nb_splitted_rough_PRPRx(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_splitted_rough_PRPRx")
    end if
!-------------------------------------------------------------------------------------------------------------------
! Données des interfaces par sous-domaine
!-------------------------------------------------------------------------------------------------------------------
    !    * nombre de grains d'interface appartenants aux sdm
    if (.not. allocated(nb_RBDY3_interf_sdm)) then
       allocate(nb_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_RBDY3_interf_sdm")
    end if

    !    * table des RBDY3 d'interface appartenants aux sdm
    if (.not. allocated(liste_RBDY3_interf_sdm)) then
       allocate(liste_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de liste_RBDY3_interf_sdm")
    end if
    !    * table des RBDY3 d'interface appartenants aux sdm du pas de temps précédent
    if (.not. allocated(liste_RBDY3_interf_sdm_old)) then
       allocate(liste_RBDY3_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de liste_RBDY3_interf_sdm_old")
    end if

    !    * table des vitesses d'interface appartenants aux sdm
    if (.not. allocated(V_RBDY3_interf_sdm)) then
       allocate(V_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de V_RBDY3_interf_sdm")
    end if
    !    * table des vitesses d'interface appartenants aux sdm
    if (.not. allocated(V_RBDY3_interf_sdm_old)) then
       allocate(V_RBDY3_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de V_RBDY3_interf_sdm_old")
    end if
    !    * table des positions d'interface appartenants aux sdm
    if (.not. allocated(X_RBDY3_interf_sdm)) then
       allocate(X_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de X_RBDY3_interf_sdm")
    end if
    !    * table des reperes principaux d'inertie d'interface appartenants aux sdm
    if (.not. allocated(localFrame_RBDY3_interf_sdm)) then
       allocate(localFrame_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de localFrame_RBDY3_interf_sdm")
    end if

    !    * table des DF_gamma sur les grains d'interface des sdm
    if (.not. allocated(DFg_RBDY3_interf_sdm)) then
       allocate(DFg_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de DFg_RBDY3_interf_sdm")
    end if

!-------------------------------------------------------------------------------------------------------------------
! Structures associées aux fonctions MPI
!-------------------------------------------------------------------------------------------------------------------

    !    * Vecteur du nombre d'éléments envoyés par l'hôte à chaque processus
    if (.not. allocated(vect_nb_send_host)) then
       allocate(vect_nb_send_host(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de vect_nb_elem_host")
    end if
    vect_nb_send_host=0
 
       !    * Vecteur du nombre d'éléments recus par l'hôte pour chaque processus
    if (.not. allocated(vect_nb_recv_host)) then
       allocate(vect_nb_recv_host(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de vect_nb_recv_host")
    end if
    vect_nb_recv_host=0

       !    * Vecteur de découpage du vecteur à distribuer aux processus
    if (.not. allocated(vect_shift)) then
       allocate(vect_shift(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de vect_shift")
    end if
    vect_shift=0

 end subroutine new_dd_host
!----------------------------------------------------------------

! Allocation des tableaux servants pour les échanges MPI mais non
! alloués chez les esclaves dans new_dd_slave.
! N.B. : allocation à "0" pour déclencher une erreur si on
! essaye un proc slave tente d'y accéder. 
!----------------------------------------------------------------
 subroutine allocations_fantome_slave

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine

    if ( rang == 0 ) return

    if (allocated(mask_part4all)) deallocate(mask_part4all)
    allocate(mask_part4all(0))

    if (allocated(vect_send_host_I)) deallocate(vect_send_host_I)
    allocate(vect_send_host_I(0))

    if (allocated(vect_nb_send_host)) deallocate(vect_nb_send_host)
    allocate(vect_nb_send_host(0))

    if (allocated(vect_nb_recv_host)) deallocate(vect_nb_recv_host)
    allocate(vect_nb_recv_host(0))

    if (allocated(vect_shift)) deallocate(vect_shift)
    allocate(vect_shift(0))

    if (allocated(interactions4all)) deallocate(interactions4all)
    allocate(interactions4all(0))

    if (allocated(V_interf4all)) deallocate(V_interf4all)
    allocate(V_interf4all(0))

    if (allocated(X_interf4all)) deallocate(X_interf4all)
    allocate(X_interf4all(0))

    if (allocated(localFrame_interf4all)) deallocate(localFrame_interf4all)
    allocate(localFrame_interf4all(0))

    if (allocated(DFg_interf4all)) deallocate(DFg_interf4all)
    allocate(DFg_interf4all(0))

    if (allocated(Ibeg4all)) deallocate(Ibeg4all)
    allocate(Ibeg4all(0))

    if (allocated(Xbeg4all)) deallocate(Xbeg4all)
    allocate(Xbeg4all(0))

    if (allocated(Vbeg4all)) deallocate(Vbeg4all)
    allocate(Vbeg4all(0))

    if (allocated(localFrameTT4all)) deallocate(localFrameTT4all)
    allocate(localFrameTT4all(0))

    if (allocated(I4all)) deallocate(I4all)
    allocate(I4all(0))

    if (allocated(X4all)) deallocate(X4all)
    allocate(X4all(0))

    if (allocated(V4all)) deallocate(V4all)
    allocate(V4all(0))

    if (allocated(localFrame4all)) deallocate(localFrame4all)
    allocate(localFrame4all(0))

    if (allocated(Bbox4all)) deallocate(Bbox4all)
    allocate(Bbox4all(0))

 end subroutine allocations_fantome_slave
!----------------------------------------------------------------

!----------------------------------------------------------------!
!----------------------------------------------------------------!
!              "Pseudo main" du module DDM_3D                    !
!----------------------------------------------------------------!
!----------------------------------------------------------------!

 subroutine creation_domaines

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine 
    integer                                    :: E_IO
    integer                                    :: i,j,iliens,tmp,err
    integer                                    :: itact, ibody, icdan, isdm
    real(kind=8)                               :: minray, maxray ! plus petit et plus grand rayon d'encombrements, pour la 
                                                                 ! methode des boites
    real(kind=8)                               :: alert          ! distance d'alerte globale utilisee pour la methode des boites
    character(len=10)                          :: step ! pour ecrire le numero du pas  
    character(len=10)                          :: sdm  ! pour ecrire le numero du du sous_domaine courant  

    integer                                    :: isee ! indice de boucle sur les lois d'interaction

    ! variables utilisees pour elaguer les interactions grossieres calculees par la methode de boites generique
    type(CONTAINER)                            :: rough_contact_gen ! table de visibilité obtenue apres l'appel a la methode
                                                     ! des boites generiques
    integer                                    :: nb_rough_contact_gen ! nombre de contacts dans rough_contact_gen
    type(T_object)                             :: contact ! pour recuperer le contact courant
    integer(kind=4), dimension(:), pointer     :: cdan ! pour recuperer la paire de contacteurs en interaction 
    real(kind=8)                               :: adist
    integer(kind=4)                            :: cdtac, antac
    character(len=5)                           :: cdcol, ancol
    real(kind=8)                               :: raycd, rayan
    real(kind=8), dimension(3)                 :: sep
    ! pour creer le nouvel objet anonyme a ajouter a rough_contact
    integer(kind=4), dimension(:), pointer     :: i4 
    real(kind=8), dimension(:), pointer        :: r8
    character(len=5), dimension(:), pointer    :: c5
    character(len=128), dimension(:), pointer  :: cx

    character(len=100)                         :: cout
                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_3D::creation_domaines"

    ! subroutine de rang 0
    if ( rang /= 0 ) return

    i4 => null()
    r8 => null()
    c5 => null()
    cx => null()

    ! Calcul dans mod_RBDY3 de la configuration de detection
    call compute_configurationTT_RBDY3

    ! recuperation des coordonnees des centre d'inertie des RBDY3
    do ibody=1, nbody
       !am : on utilise les coordonnees dans la configuration de detection
       coord_ci(1:3, ibody) = get_coorTT(ibody, 0)
    end do
    !--------------------------------------------------------------------------------------
    ! Calcul de la boite englobante de l'échantillon total, puis des caractéristiques de sous domaines 
    call caracteristique_domaine
    !--------------------------------------------------------------------------------------

    !---------------------------------------------------------------------------------------------------!
    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des centres d'inertie des RBDY3
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Nsdm3, Xleft, Yleft, Zdown, dim_sdm, &
                             3, nbody, coord_ci, repart_sdm_ci,   & 
                             .true.)                              
    !--------------------------------------------------------------------------------------

    ! Recuperation des coordonnees des centre d'inertie des SPHER
    do itact=1, ntact_SPHER

                 ! <=> get_coor |  indice du RBDY3    |  indice du contacteur dans la
                                                      ! liste des contacteurs de ce RBDY3
       !am : on utilise les coordonnees dans la configuration de detection
       coord_ct(1:3, itact) = get_coorTT(spher2bdyty(1, itact), spher2bdyty(2, itact))

       !print *, "coord_ct(", itact, ")=", coord_ct(:, itact)
       ! Pour la méthode des boites générique
       radius_ct(itact)=get_radius_SPHER(itact)

    end do

    ! Recuperation des coordonnees des centre d'inertie des POLYR
    do itact=1, ntact_POLYR

                 ! <=> get_coor |  indice du RBDY3    |  indice du contacteur dans la
                                                      ! liste des contacteurs de ce RBDY3
       !am : on utilise les coordonnees dans la configuration de detection
       coord_ct(1:3, ntact_SPHER + itact) = get_coorTT(polyr2bdyty(1, itact), polyr2bdyty(2, itact))

       !print *, "coord_ct(", itact, ")=", coord_ct(:, itact)
       ! Pour la méthode des boites générique
       radius_ct(ntact_SPHER + itact)=get_radius_POLYR(itact)
    end do
    !--------------------------------------------------------------------------------------


    !---------------------------------------------------------------------------------------------------!
    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des contacteurs
    if (nb_big_POLYR > 0) then
       call ventillation_ds_sdm(Nsdm1, Nsdm2, Nsdm3, Xleft, Yleft, Zdown, dim_sdm, &
                                3, nbody, coord_ct, repart_sdm_ct,   & 
                                .true.)
    end if
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Méthode des boites générique
    ! am: elle marche en 2D et en 3D
    !    * détection des contacts sphere/sphere
    if (ntact_SPHER > 0) then
       ! on recupere les rayons min et max
       minray = get_min_radius_SPHER()
       maxray = get_max_radius_SPHER()
       ! on recupere la plus grande distance d'alerte pour le contacts sphere/sphere
       alert=0.D0  
       do isee=1, size(see)
          if (see(isee)%cdtac == 'SPHER' .and. see(isee)%antac == 'SPHER') then
             alert=max(alert, see(isee)%alert)
          end if
       end do
       ! on realise la detection grossiere
       !am: implementation classique
       !call boxes_method(coord_ct(:, 1:ntact_SPHER), radius_ct(1:ntact_SPHER), alert, rough_contact_gen, minray, maxray)
       !am: implementation utilisant des listes chainees
       call boxes_method_lists(coord_ct(:, 1:ntact_SPHER), radius_ct(1:ntact_SPHER), alert, rough_contact_gen, minray, maxray)

       !call display_object_container(rough_contact)

       ! parmi les interactions detectees par la methode des boites generiques, on ne garde que celles qui :
       !    - ne sont pas des autocontacts 
       !    - sont associees a une loi d'interaction
       !    - sont suffisament proche, au sens de la distance d'alerte de leur loi d'interaction (et pas juste le max) 

       ! on recupere le nombre d'interactions detectees par la methode des boites generiques
       nb_rough_contact_gen = get_nb_objects(rough_contact_gen)
       ! aucune interaction n'a encore ete stockee
       nb_rough_contact = 0

       ! pour chaque contact detecte par la methode des boites generiques
       do icdan=1, nb_rough_contact_gen
          ! on recupere le contact courant
          contact = get_object(rough_contact_gen, icdan)    
          ! on recupere la paire (index candidat/index antagonist correspondante)
          cdan => get_i4_vector(contact)
          cdtac=cdan(1)
          antac=cdan(2)
          
          ! on recupere la loi d'interaction correpsondant a l'interaction courante
          cdcol = get_color_SPHER(cdtac)
          ancol = get_color_SPHER(antac)
          isee  = get_isee_specific('SPHER', cdcol, ancol)

          ! si l'interaction courant n'est associee a aucune loi d'interaction ou est un autocoantact, on passe a la suivante
          if (isee==0 .or. is_SPHER_same_RBDY3(cdtac, antac) ) CYCLE

          ! ici, on est en mesure de calculer la distance d'alerte

          ! on calcule la distance d'alerte pour cette interaction
          adist=see(isee)%alert

          raycd=radius_ct(cdtac)
          rayan=radius_ct(antac)

          adist=0.1005d+01*(adist + raycd + rayan)

          ! on calcule la separation entre les objets
          sep(1:3) = coord_ct(1:3, cdtac) - coord_ct(1:3, antac) 

          ! si les deux contacteurs sont suffisamment proches
          ! N.B. pour le contact SPSPx, on considere la norme sup
          if (all(dabs(sep) <= adist)) then
             ! on ajoute l'interaction dans rough_contact
             nb_rough_contact = nb_rough_contact + 1

             allocate(i4(2))
             i4(1:2) = cdan(1:2)

             call add_object_to_container(rough_contact, nb_rough_contact, c5, i4, r8, cx)
          end if
       end do

       ! on ferme rough_contact
       call close_container(rough_contact)
       ! on vide le container des interactions obtenues en utilisant la methode des boites generiques
       call erase_container(rough_contact_gen)

       ! on recupere le nombre de contacts sphere/sphere
       nb_rough_contact_SPSPx = get_nb_objects(rough_contact)
       WRITE(cout,*) "nb_rough_contact (SPSPx)", nb_rough_contact_SPSPx
       call logmes(cout)
        
       !call display_object_container(rough_contact)
    else
       nb_rough_contact_SPSPx = 0
    end if

    !    * détection des contacts polyedre/polyedre
    if (ntact_POLYR > 0) then
       ! on ouvre le container pour ajouter les contacts polyedre/polyedre
       if (.not. get_status(rough_contact)) call open_container(rough_contact)
       ! on recupere les rayons min et max
       ! N.B.: les "big POLYR" n'ont pas ete pris en compte lors du calcul du rayon maximum
       minray = get_min_radius_POLYR()
       maxray = get_max_radius_POLYR()
       ! on recupere la plus grande distance d'alerte pour le contacts polyedre/polyedre
       alert=0.D0  
       do isee=1, size(see)
          if (see(isee)%cdtac == 'POLYR' .and. see(isee)%antac == 'POLYR') then
             alert=max(alert, see(isee)%alert)
          end if
       end do
       ! on realise la detection grossiere (implementation "sparse")
       !    * cas ou des "gros" polyedres sont presents
       if (nb_big_POLYR /= 0) then
          ! N.B. on ne transmet que les numeros de gros polyedres corespondant au premier sous-domaine
          call boxes_method_sparse(coord_ct(:, ntact_SPHER + 1:ntact_SPHER + ntact_POLYR), &
             radius_ct(ntact_SPHER + 1:ntact_SPHER + ntact_POLYR), alert, rough_contact_gen, &
             minray, maxray, big_POLYR(1:nb_big_POLYR))
       !    * cas sans "gros" polyedres
       else
          call boxes_method_sparse(coord_ct(:, ntact_SPHER + 1:ntact_SPHER + ntact_POLYR), &
             radius_ct(ntact_SPHER + 1:ntact_SPHER + ntact_POLYR), alert, rough_contact_gen, &
             minray, maxray)
       end if

       ! parmi les interactions detectees par la methode des boites generiques, on ne garde que celles qui :
       !    - ne sont pas des autocontacts 
       !    - sont associees a une loi d'interaction
       !    - sont suffisament proche, au sens de la distance d'alerte de leur loi d'interaction (et pas juste le max) 

       ! on recupere le nombre d'interactions detectees par la methode des boites generiques
       nb_rough_contact_gen = get_nb_objects(rough_contact_gen)
       ! on recupere le nombre contacts deja stockes
       nb_rough_contact = get_nb_objects(rough_contact)

       ! pour chaque contact detecte par la methode des boites generiques
       do icdan=1, nb_rough_contact_gen
          ! on recupere le contact courant
          contact = get_object(rough_contact_gen, icdan)    
          ! on recupere la paire (index candidat/index antagonist correspondante)
          cdan => get_i4_vector(contact)
          cdtac=cdan(1)
          antac=cdan(2)
          
          ! on recupere la loi d'interaction correpsondant a l'interaction courante
          cdcol = get_color_POLYR(cdtac)
          ancol = get_color_POLYR(antac)
          isee  = get_isee_specific('POLYR', cdcol, ancol)

          ! si l'interaction courant n'est associee a aucune loi d'interaction ou est un autocoantact, on passe a la suivante
          if (isee==0 .or. is_POLYR_same_RBDY3(cdtac, antac) ) CYCLE

          ! ici, on est en mesure de calculer la distance d'alerte

          ! on recupere la distance d'alerte pour une interaction de ce type
          alert=see(isee)%alert

          ! on calcule la distance d'alerte pour cette interaction
          adist=alert

          raycd=radius_ct(ntact_SPHER + cdtac)
          rayan=radius_ct(ntact_SPHER + antac)

          adist=0.1005d+01*(adist + raycd + rayan)

          ! on calcule la separation entre les objets
          sep(1:3) = coord_ct(1:3, ntact_SPHER + cdtac) - coord_ct(1:3, ntact_SPHER + antac) 

          ! si les deux contacteurs sont trop eloignes, on passe a l'interaction suivante
          ! N.B. pour le contact PRPRx, on considere la norme euclidienne et pas la norme sup
          if (dot_product(sep, sep) >= adist*adist) cycle

          ! tests "norme de Manathan" projection sur les axes
          ! N.B. on ne tient pas compte de la periodicite!!
          if (any(maxpos_POLYR(1:3, antac) - &
                  minpos_POLYR(1:3, cdtac) + alert < 0.d0)) &
             cycle
          if (any(maxpos_POLYR(1:3, cdtac) - &
                  minpos_POLYR(1:3, antac) + alert < 0.d0)) &
             cycle

          ! on ajoute l'interaction dans rough_contact
          nb_rough_contact = nb_rough_contact + 1

          allocate(i4(2))
          i4(1:2) = cdan(1:2)

           call add_object_to_container(rough_contact, nb_rough_contact, c5, i4, r8, cx)
       end do

       ! on ferme rough_contact
       call close_container(rough_contact)
       ! on vide le container des interactions obtenues en utilisant la methode des boites generiques
       call erase_container(rough_contact_gen)

       ! on recupere le nombre de contacts polyedre/polyedre
       nb_rough_contact_PRPRx = get_nb_objects(rough_contact) - nb_rough_contact_SPSPx
       WRITE(cout,*) "nb_rough_contact (PRPRx)", nb_rough_contact_PRPRx
       call logmes(cout)

       !call display_object_container(rough_contact)
    else
       nb_rough_contact_PRPRx = 0
    end if

    !--------------------------------------------------------------------------------------
    ! On recupere le nombre de total de contacts grossiers.
    nb_rough_contact = get_nb_objects(rough_contact)
    WRITE(cout,*) "nb_rough_contact", nb_rough_contact
    call logmes(cout)
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! On peut maintenant allouer les tableaux ayants une dimension a nb_rough_contact
    if( allocated(coord_cc) ) deallocate(coord_cc)           ! 
    allocate(coord_cc(nbDIME, nb_rough_contact), stat=err)   ! 
    if( err/=0 ) call faterr(IAM, "pb d'allocation de coord_cc")

    if( allocated(repart_sdm_cc) ) deallocate(repart_sdm_cc) !   
    allocate(repart_sdm_cc(nb_rough_contact), stat=err)      ! 
    if( err/=0 ) call faterr(IAM, "pb d'allocation de repart_sdm_cc")

    !--------------------------------------------------------------------------------------
    ! Construction du tableau des coordonnes des centres des contacts
    call creation_coord_cc 
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Ventillation des centre des contacts
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Nsdm3, Xleft, Yleft, Zdown, dim_sdm, &
                             3, nb_rough_contact, coord_cc, repart_sdm_cc,   & 
                             .false.)                              
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Création des masques de partiticipation par sous-domaine
    ! et de la table de multiplicité
    call creation_body_particip
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Création des tableaux de contacts par sous-domaines
    call creation_splitted_rough 
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Calcul du nombre de grains d'interface globale et par sous-domaine.
    ! Allocation et construction de l'interface globale.
    ! Calcul du nobre de liens d'interface.
    call compute_interface
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Traitement de la migration
    if (.not. first_real_step) then
       call migration_treatment !(body_particip,body_particip_old,nbody,max_multi, &
                                ! Nsdm,migration_tab)
    end if
    !--------------------------------------------------------------------------------------
    
    !--------------------------------------------------------------------------------------
    ! Enregistrement de multiplicite dans multiplicite_old
    multiplicite_old=multiplicite
    ! Enregistrement de body_particip dans body_particip_old
    body_particip_old=body_particip
    !--------------------------------------------------------------------------------------

 end subroutine creation_domaines
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_creation_domaines

    implicit none
    
    integer :: isdm
    integer :: i
    integer :: shift
    logical :: mig_tab_vide
    integer, dimension( MPI_STATUS_SIZE ) :: statut
    integer, parameter :: etiquette=100
    
    !---------------------------
    ! MASK_PARTICIP
    !---------------------------
    
    ! Le processus hôte construit un vecteur nbody*nb_procs
    ! où mask_particip est répété nb_procs fois.
    if ( rang == 0 ) then
       do isdm=1,Nsdm
          mask_part4all((isdm-1)*nbody+1:isdm*nbody)=mask_particip(1:nbody,isdm)
       end do
    end if

    ! Ce vecteur est envoyé aux esclaves qui en récupèrent
    ! chacun une tranche de longueur nbody.
    call MPI_SCATTER(mask_part4all, nbody, MPI_LOGICAL, mask_in_slave, nbody, &
                      MPI_LOGICAL, 0, MPI_COMM_WORLD, code)
    ! L'échange s'est-il bien passé ?
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

       
 
    ! Écriture dans des fichiers séparé/procs du masque de visibilité
                           !12345678901234567890
    !call write_MPI_INFO_L('MASK_PART_IN_SLAVE__',mask_in_slave,(/(i,i=1,nbody)/),nbody)
    
    
    !---------------------------
    ! MULTIPLICITE
    !---------------------------
    
    call MPI_BCAST(multiplicite, nbody, MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    ! L'échange s'est-il bien passé ?
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    
                           !12345678901234567890
    !call write_MPI_INFO_I('MULTI_IN_SLAVE______',multiplicite,(/(i,i=1,nbody)/),nbody)
    
    !---------------------------
    ! LISTE D'INTERFACE
    !---------------------------
    
    ! 1) Nombre d'éléments dans l'interface
    
    if ( rang == 0 ) then
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_RBDY3_interf_sdm(isdm)
       end do
    end if


    ! Envoi du nombre de RBDY3 d'interface pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_RBDY3_interf_slave, 1, &
        MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    
    ! Allocation du vecteur dans lequel les indices des RBDY3 d'interface seront stockés
    if (allocated(liste_RBDY3_interf_slave)) deallocate(liste_RBDY3_interf_slave)
    allocate(liste_RBDY3_interf_slave(nb_RBDY3_interf_slave))
    liste_RBDY3_interf_slave=0
    
    ! 2) Numéros des éléments de l'interface

    if ( rang == 0 ) then
       ! Nombre total d'éléments à envoyer
       nb_send_host=sum(vect_nb_send_host)
       
       if (allocated(vect_send_host_I)) deallocate(vect_send_host_I)
       allocate(vect_send_host_I(nb_send_host))
       vect_send_host_I=0
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do
       
       do isdm=1,Nsdm-1
           vect_send_host_I(vect_shift(isdm)+1:vect_shift(isdm+1)) &
            = liste_RBDY3_interf_sdm(isdm)%particule(:)
       end do
       vect_send_host_I(vect_shift(Nsdm)+1:nb_send_host) &
          = liste_RBDY3_interf_sdm(Nsdm)%particule(:)
       
    end if 

    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(vect_send_host_I, vect_nb_send_host, vect_shift, MPI_INTEGER, &
    liste_RBDY3_interf_slave, nb_RBDY3_interf_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    
                           !12345678901234567890
    !call write_MPI_INFO_I('LISTE_RBDY3_INTERF__',liste_RBDY3_interf_slave,&
    !      (/(i,i=1,nb_RBDY3_interf_slave)/),nb_RBDY3_interf_slave)
    
    !---------------------------
    ! LISTE DES INTERACTIONS
    !---------------------------
    
    ! 1) Nombre d'interactions
    
    if ( rang == 0 ) then
       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_splitted_rough_SPSPx(isdm)
       end do
    end if
    
    ! Envoi du nombre d'interactions (SPSP+PRPR) pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_interactions_slave_SPSPx, 1, &
        MPI_INTEGER, 0, MPI_COMM_WORLD, code)

    if ( rang == 0 ) then
       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) =  nb_splitted_rough_PRPRx(isdm)
       end do
    end if

    ! Envoi du nombre d'interactions (SPSP) pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_interactions_slave_PRPRx, 1, &
        MPI_INTEGER, 0, MPI_COMM_WORLD, code)

    nb_interactions_slave=nb_interactions_slave_SPSPx+nb_interactions_slave_PRPRx
    
    ! Allocation du vecteur dans lequel les indices des RBDY3 d'interface seront stockés
    if (allocated(interactions_slave)) deallocate(interactions_slave)
    allocate(interactions_slave(3*nb_interactions_slave))
    interactions_slave=0
    
    ! 2) Distribution des interactions
    
    if ( rang == 0 ) then

       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = 3 * nb_splitted_rough(isdm)
       end do

       ! Nombre total d'éléments à envoyer
       nb_send_host=sum(vect_nb_send_host)
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do
       
       !write(slave_io, '(I6,I6,I6)') interactions4all
        
    end if 
    
    ! Envoi de la liste des interactions a chaque sdm
    call MPI_SCATTERV(interactions4all, vect_nb_send_host, vect_shift, MPI_INTEGER, &
    interactions_slave, 3*nb_interactions_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    
                           !12345678901234567890
    !call write_MPI_INFO_I('INTERACTIONS________',interactions_slave,&
    !      (/(i,i=1,3*nb_interactions_slave)/),nb_interactions_slave)
   
 
    !---------------------------
    ! MIGRANTS
    !---------------------------
    
    if (.not. first_real_step) then
    
       ! 1) Nombre de migrations
       
       if ( rang == 0 ) then
          vect_nb_send_host = 0
          nb_send_host = 0
       end if
      
       mig_tab_vide = .false. 
       nb_mig_slave = 0 
    
       ! ---> Fonction rang0 <---
       call nb_migrations2send(mig_tab_vide) ! calcule vect_nb_send_host

       call MPI_BCAST(mig_tab_vide, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, code)

       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

       if ( .not. mig_tab_vide ) then
    
          ! Envoi du nombre de RBDY3 migrant pour chaque sous domaine
          call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_mig_slave, 1, &
                  MPI_INTEGER, 0, MPI_COMM_WORLD, code)
      
          !if (rang ==0) print *, "rang0 vect_nb_send_host=", vect_nb_send_host
          !print *, "Rang=", rang, "nb_mig_slave=",nb_mig_slave
      
          if ( rang == 0 .or. nb_mig_slave /=0) then

             ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
             if (allocated(mig_indices_slave)) deallocate(mig_indices_slave)
             allocate(mig_indices_slave(nb_mig_slave))
             mig_indices_slave=0
      
             ! Allocation du vecteur dans lequel l'état des migrants vont être stockés
             if (allocated(mig_etats_slave)) deallocate(mig_etats_slave)
             allocate(mig_etats_slave(nb_mig_slave*21))
             mig_etats_slave=0.D0


             if ( rang == 0 ) then

                ! Nombre total d'éléments à envoyer
                nb_send_host=sum(vect_nb_send_host)
       
                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_indices4all)) deallocate(mig_indices4all)
                allocate(mig_indices4all(nb_send_host))
                mig_indices4all=0
        
                ! Allocation du vecteur dans lequel l'état des migrants vont être stockés
                if (allocated(mig_etats4all)) deallocate(mig_etats4all)
                allocate(mig_etats4all(nb_send_host*21))
                mig_etats4all=0.D0
       
                ! Vecteur de décalage d'indice pour le SCATTERV
                vect_shift=0
                do isdm=2,Nsdm 
                   vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
                end do

                ! ---> Fonction rang0 <---
                call compute_etats_indices_migration
               
             end if 

          end if 
      
          ! 2) Distribution des migrants

          do isdm = 2, Nsdm

             if ( rang == 0 ) then
                ! Nombre total d'éléments à envoyer
                nb_send_host=vect_nb_send_host(isdm)
         
                if (nb_send_host==0) cycle

                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_indices_host)) deallocate(mig_indices_host)
                allocate(mig_indices_host(nb_send_host))
                mig_indices_host=0

                mig_indices_host(:) = mig_indices4all( vect_shift(isdm)+1 : vect_shift(isdm) + nb_send_host )           

                call MPI_SEND (mig_indices_host, nb_send_host, MPI_INTEGER, isdm-1, etiquette, MPI_COMM_WORLD ,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

             end if

             if ( rang == isdm-1 ) then

                if ( nb_mig_slave == 0 ) cycle
                call MPI_RECV (mig_indices_slave, nb_mig_slave, MPI_INTEGER ,0,etiquette, MPI_COMM_WORLD ,statut,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

             end if

          end do

          ! Pareil, mais pour les états des particules migrantes
          do isdm = 2, Nsdm

             if ( rang == 0 ) then
                ! Nombre total d'éléments à envoyer
                nb_send_host=vect_nb_send_host(isdm)
         
                if (nb_send_host==0) cycle

                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_etats_host)) deallocate(mig_etats_host)
                allocate(mig_etats_host(21*nb_send_host))
                mig_etats_host=0.d0

                shift = 21*vect_shift(isdm)
                mig_etats_host(:) = mig_etats4all( shift+1 : shift + 21*nb_send_host )           

                call MPI_SEND (mig_etats_host, 21*nb_send_host, MPI_DOUBLE_PRECISION, isdm-1, etiquette, MPI_COMM_WORLD ,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
             end if

             if ( rang == isdm-1 ) then
                if ( nb_mig_slave ==0 ) cycle
                call MPI_RECV (mig_etats_slave, 21*nb_mig_slave, MPI_DOUBLE_PRECISION,0,etiquette, MPI_COMM_WORLD ,statut,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

             end if

          end do

       end if
   
    else 

     first_real_step=.false. 

   end if

   ! Allocation de l'espace memoire pour stocker l'agregation des forces de cohesion sur les corps
   ! d'interface du sdm courant (6 reels pour chaque corps d'interface)
   if (allocated(Fg_RBDY3_interf_slave)) deallocate(Fg_RBDY3_interf_slave)
   allocate(Fg_RBDY3_interf_slave(6*nb_RBDY3_interf_slave))
   Fg_RBDY3_interf_slave = 0.D0

 end subroutine scatter_creation_domaines 
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine stock_Fg_liste_old

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine 
    integer                                    :: itact, ibody, icdan, isdm, err
                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_3D::stock_Fg_liste_old"

    ! Gestion/récupération des F_gamma du pas précédent quand on le peut
    ! Allocation des listes de RBDY3 d'interface par sous-domaine du pas précédent
    if ( allocated(liste_RBDY3_interf_slave_old ) ) & 
         deallocate(liste_RBDY3_interf_slave_old)
    allocate(liste_RBDY3_interf_slave_old(nb_RBDY3_interf_slave), stat=err)
    if (err/=0) stop "Pb d'allocation de liste_RBDY3_interf_sdm_old"
    liste_RBDY3_interf_slave_old=liste_RBDY3_interf_slave
    
    if ( allocated(Fg_RBDY3_interf_slave_old) ) & 
         deallocate(Fg_RBDY3_interf_slave_old)
    allocate(Fg_RBDY3_interf_slave_old(6*nb_RBDY3_interf_slave), stat=err)
    if (err/=0) stop "Pb d'allocation de Fg_RBDY3_interf_slave_old"
    Fg_RBDY3_interf_slave_old=0.d0
    Fg_RBDY3_interf_slave_old=Fg_RBDY3_interf_slave


    repartition_just_made = .true.

 end subroutine stock_Fg_liste_old
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine caracteristique_domaine

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine 
    ! implicites, real : Xleft, Xright, Yleft, Yright, Zdown, Zup, dim_sdm 

    integer :: ibody ! inidce de boucle sur les corps
    logical :: visible ! pour tester la visibilite d'un corps 
   
    ! on initialise la visibilite des corps a vrai
    visible=.TRUE.

    ! on initialise les bornes en se servant des coordonnees du premier corps
    Xleft  = coord_ci(1,1)
    Xright = coord_ci(1,1)
    Yleft  = coord_ci(2,1)
    Yright = coord_ci(2,1)
    Zdown  = coord_ci(3,1)
    Zup    = coord_ci(3,1)

    ! pour chaque corps
    DO ibody=1, nbody
       ! on recupere la visibilite du corps courant
       visible=get_visible(ibody)
       ! s'il est invisible, on passe au suivant
       IF (.NOT.visible) CYCLE
       Xleft = MIN(coord_ci(1, ibody), Xleft )
       Xright= MAX(coord_ci(1, ibody), Xright)
       Yleft = MIN(coord_ci(2, ibody), Yleft )
       Yright= MAX(coord_ci(2, ibody), Yright)
       Zup   = MAX(coord_ci(3, ibody), Zup   )
       Zdown = MIN(coord_ci(3, ibody), Zdown )
    END DO 

    ! gestion des bornes imposees, s'il en existe
    if ( any(imposed_boundaries) ) then
       if ( imposed_boundaries(i_Xleft ) ) Xleft  = boundaries(i_Xleft )
       if ( imposed_boundaries(i_Xright) ) Xright = boundaries(i_Xright)
       if ( imposed_boundaries(i_Yleft ) ) Yleft  = boundaries(i_Yleft )
       if ( imposed_boundaries(i_Yright) ) Yright = boundaries(i_Yright)
       if ( imposed_boundaries(i_Zdown ) ) Zdown  = boundaries(i_Zdown )
       if ( imposed_boundaries(i_Zup   ) ) Zup    = boundaries(i_Zup   )
    end if

    !print*, 'Xleft=', Xleft, ' Xright=', Xright
    !print*, 'Yleft=', Yleft, ' Yright=', Yright
    !print*, 'Zdown=', Zdown, ' Zup   =', Zup

    !-----------------------------------------------!
    ! Caracteristiques du découpage en sous-domaines!
    !-----------------------------------------------!
 
    dim_sdm = (/ ((Xright - Xleft)/real(Nsdm1)), ((Yright - Yleft)/real(Nsdm2)), ((Zup - Zdown)/real(Nsdm3)) /)

    !print*, 'dim_sdm=', dim_sdm

 end subroutine caracteristique_domaine
!----------------------------------------------------------------

 subroutine creation_coord_cc 
    implicit none
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: nb_rough_contact, rough_contact%val%an (%cd), coord_ct 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: coord_cc
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    type(T_object)                         :: contact ! pour recuperer le contact courant
    integer(kind=4), dimension(:), pointer :: cdan ! pour recuperer le vecteur d'entier donnant les indices des deux contacteurs en interaction
    integer                                :: antac, cdtac ! respectivement les indices des contacteurs candidat et antagoniste
    real(kind=8), dimension(3)             :: coordan, coordcd ! pour recueprer les coordonnees des contacteurs candidat et antagoniste respectivement
    integer                                :: icdan ! inidice de boucle sur les contacts

    ! pour les contacts sphere/sphere
    do icdan=1, nb_rough_contact_SPSPx
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact, icdan)    
       ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
       cdtac = cdan(1)
       antac = cdan(2)
       ! on recupere les coordonnees des contacteurs candidats et antagonistes 
       coordcd(1:3) = coord_ct(1:3, cdtac)
       coordan(1:3) = coord_ct(1:3, antac)
       ! on calcule les coordonnees du centre de contact
       coord_cc(1:3, icdan) = (coordcd(1:3) + coordan(1:3))*0.5
    end do

    ! pour les contacts polyedre/polyedre
    do icdan=nb_rough_contact_SPSPx + 1, nb_rough_contact_SPSPx + nb_rough_contact_PRPRx
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact, icdan)    
       ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
       cdtac = cdan(1)
       antac = cdan(2)
       ! on recupere les coordonnees des contacteurs candidats et antagonistes 
       coordcd(1:3) = coord_ct(1:3, ntact_SPHER + cdtac)
       coordan(1:3) = coord_ct(1:3, ntact_SPHER + antac)
       ! on calcule les coordonnees du centre de contact
       coord_cc(1:3, icdan) = (coordcd(1:3) + coordan(1:3))*0.5
    end do

 end subroutine creation_coord_cc
!----------------------------------------------------------------

!----------------------------------------------------------------------------------------------------------------------------!
! Repartition d'entités dans les sous-domaines a partir de leurs coordonnees                                                 !
! Idée : construire un repere local 3D avec                                                                                  !
!		pour origine_loc le point O' avec OO' = (/ min(coord(dim=1)), min(coord(dim=2)), min(coord(dim=3)) /)        !
!		les distances reduites issues du repère global :                                                             !
!							x' = [X - OO'(1)] / (L1/Nsdm1)                                       !
!							y' = [Y - OO'(2)] / (L2/Nsdm2)                                       !
!							z' = [Z - OO'(3)] / (L3/Nsdm3)                                       !
! avec L1=max(coord(dim=1))-min(coord(dim=1), L2=max(coord(dim=2))-min(coord(dim=2)), L2=max(coord(dim=3))-min(coord(dim=3)) !
!      Nsdmi = nombre de subdivisions du domaine suivant l'axe i                                                             !
!                                                                                                                            !
! Les parties entières des coord_ci reduites pour chaque objet sont un doublet (2D) ou un triplet (3D)                       !
! donnant le numero du sous-domaine contenant cet objet                                                                      !
!----------------------------------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------
 subroutine ventillation_ds_sdm(Nsdm1_, Nsdm2_, Nsdm3_, Xleft_, Yleft_, Zdown_, dim_sdm_, & ! caractéristiques du découpage 
                                nb_ligne, nb_entity, coord, repart_sdm,   & ! objets à ventiller et vecteur résultat
                                visibilite)                                 ! paramètre optionnel pour traiter les
                                                                            ! objets potentiellement invisibles
    implicit none
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine
    integer, intent(in)                                     :: Nsdm1_, Nsdm2_, Nsdm3_
    real(kind=8), intent(in)                                :: Xleft_, Yleft_, Zdown_ 
    real(kind=8), dimension(nbDIME), intent(in)             :: dim_sdm_
    integer, intent(in)                                     :: nb_ligne, nb_entity
    real(kind=8), dimension(nb_ligne,nb_entity), intent(in) :: coord
    logical, intent(in), optional                           :: visibilite

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    integer, dimension(nb_entity), intent (out)             :: repart_sdm     
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                                 :: ientity
    integer, dimension(nbDIME)                              :: position_reduite
    logical                                                 :: visible

    ! on teste l'existance de visibilite et on stocke les résultat dans existe
    !existe=present(visibilite)

    !-------------------------------------------------------------!
    ! Pour chaque objet, on determine le sdm auquel il appartient !
    !-------------------------------------------------------------!

    do ientity=1,nb_entity

       ! recuperation du statut de visibilite du corps
       visible = .true.
       if (present(visibilite)) then
          if (visibilite) then
             visible=get_visible(ientity)
          end if
       end if

       ! si l'entité est invisible, on passe au suivant
       if (.not. visible) cycle 

       ! on détermine le sdm auquel appartient l'entité
       position_reduite = (/ 1 + floor( (coord(1,ientity) - Xleft_) / dim_sdm_(1) ), &
                             1 + floor( (coord(2,ientity) - Yleft_) / dim_sdm_(2) ), &
                             1 + floor( (coord(3,ientity) - Zdown_) / dim_sdm_(3) ) /)

       ! traitement des entités ayant une de leur coordonnées sur les bords
       ! de gauche, du bas, du haut ou de droite de la boite englobante 
       if( position_reduite(1) < 1 ) position_reduite(1) = 1
       if( position_reduite(2) < 1 ) position_reduite(2) = 1
       if( position_reduite(3) < 1 ) position_reduite(3) = 1
       if( position_reduite(1) > Nsdm1_ ) position_reduite(1) = Nsdm1_
       if( position_reduite(2) > Nsdm2_ ) position_reduite(2) = Nsdm2_
       if( position_reduite(3) > Nsdm3_ ) position_reduite(3) = Nsdm3_
       repart_sdm(ientity) = (position_reduite(1) - 1)*Nsdm2_*Nsdm3_ + (position_reduite(2) - 1)*Nsdm3_ + position_reduite(3)

       ! si besoin, pour checker la répartition en sdm 
       !print *, "Triplet :", position_reduite, "  Num sdm :", repart_sdm(ientity), "nb_entity=", nb_entity
    end do

 end subroutine ventillation_ds_sdm
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine creation_body_particip

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: rough_contact, repart_sdm_cc
    !                         nbody, repart_sdm_ci

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: body_particip, multiplicite, mask_particip
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                         :: icdan, ibody, isdm, imask
    integer                                         :: icdbdy, ianbdy, err
    integer(kind=4), dimension(:), pointer          :: cdan
    integer(kind=4)                                 :: icdtac, iantac

    logical, dimension(:), allocatable              :: mask_tmp ! variable servant a stocker le masque obtenu pour
       ! la ligne courante de body_particip
    integer                                         :: i ! indice de boucle anonyme
    logical                                         :: visible ! pour tester la visibilite d'un corps 
    type(T_object)                                  :: contact

 
    body_particip=0
    multiplicite=0

    ! gestion des contacts sphere/sphere
    do icdan=1, nb_rough_contact_SPSPx

       ! Éléments communs au cd et à l'an
       isdm=repart_sdm_cc(icdan)
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact, icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)

       ! traitement du candidat

       ! on recupere le numero du contacteur candidat
       icdtac = cdan(1)
       ! on en deduit le numero du corps candidat
       icdbdy=spher2bdyty(1, icdtac)

       if (count(body_particip(:,icdbdy)==isdm)==0) then
          multiplicite(icdbdy)=multiplicite(icdbdy)+1
          body_particip(multiplicite(icdbdy), icdbdy)=isdm
       end if

       ! cas de l'antagoniste

       ! on recupere le numero du contacteur antagoniste
       iantac = cdan(2)
       ! on en deduit le numero du corps antagoniste 
       ianbdy=spher2bdyty(1, iantac)

       if (count(body_particip(:,ianbdy)==isdm)==0) then
          multiplicite(ianbdy)=multiplicite(ianbdy)+1
          body_particip(multiplicite(ianbdy), ianbdy)=isdm
       end if

    end do

    ! gestion des contacts polyedre/polyedre
    do icdan=nb_rough_contact_SPSPx + 1, nb_rough_contact_SPSPx + nb_rough_contact_PRPRx

       ! Éléments communs au cd et à l'an
       isdm=repart_sdm_cc(icdan)

       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact, icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)

       ! on recupere le numero du contacteur candidat
       icdtac = cdan(1)

       ! on recupere le numero du contacteur antagoniste
       iantac = cdan(2)

       ! si l'echantillon ne contient aucun gros polyedre
       if (nb_big_POLYR == 0) then
          ! meme regle que pour les spheres : on affecte aux deux corps le numero de sdm du contact
          isdm = repart_sdm_cc(icdan)
       ! sinon, on choisit le numero de sdm en focntion du cas
       else
          ! si le candidat n'est pas un gros polyedre
          if (.not. any(big_POLYR == icdtac)) then
             ! si l'antagoniste n'est pas un gros polyedre
             if (.not. any(big_POLYR == iantac)) then
                ! on affecte aux deux corps le numero de sdm du contact
                isdm = repart_sdm_cc(icdan)
             ! sinon,
             else
                ! on affecte aux deux corps le numero de sdm du candidat (qui est sensiblement plus petit que l'antagoniste) 
                isdm = repart_sdm_ct(ntact_SPHER + icdtac)
             end if
          ! sinon,
          else
             !am: TODO trouver une regle mieux adaptee au cas "gros polyedre voit gros polyedre".
             !         En attendant, on utilise la regle par defaut...
             
             ! on affecte aux deux corps le numero de sdm du contact
             isdm = repart_sdm_cc(icdan)
          end if
       end if

       ! traitement du candidat

       ! on en recupere le numero du corps candidat
       icdbdy=polyr2bdyty(1, icdtac)

       if (count(body_particip(:,icdbdy)==isdm)==0) then
          multiplicite(icdbdy)=multiplicite(icdbdy)+1
          body_particip(multiplicite(icdbdy), icdbdy)=isdm
       end if

       ! cas de l'antagoniste

       ! on en recupere le numero du corps antagoniste 
       ianbdy=polyr2bdyty(1, iantac)

       if (count(body_particip(:,ianbdy)==isdm)==0) then
          multiplicite(ianbdy)=multiplicite(ianbdy)+1
          body_particip(multiplicite(ianbdy), ianbdy)=isdm
       end if

       ! pour checker
       !print *, "corps candidat   ", icdbdy, "sdm(s)", body_particip(:,icdbdy)
       !print *, "corps antagoniste", ianbdy, "sdm(s)", body_particip(:,ianbdy)
    end do

    ! traitement des neutrinos eventuels
    do ibody=1, nbody
       ! recuperation du statut de visibilite du corps
       visible = .true.
       visible=get_visible(ibody)
       ! si le corps est invisible, on passe au suivant
       if (.not. visible) cycle 
       ! si le corps courant est un neutrino (i.e. n'a pas de contact)
       if (multiplicite(ibody)==0) then
          ! on recupere le numero de sous-domaine auquel appartient le centre d'inertie du corps
          isdm = repart_sdm_ci(ibody)
          ! on inidique que le corps appartient a ce sous-domaine
          multiplicite(ibody)=multiplicite(ibody)+1
          body_particip(multiplicite(ibody), ibody)=isdm
       end if
   
       ! pour afficher body_particip par entité
       !print *, "body_particip(:,ibody)", body_particip(:,ibody)
    end do

    ! on initialise le masque a faux
    mask_particip=.false.

    ! on alloue l'espace memoire pour stocker le masque obtenu pour
    ! la ligne courante de body_particip
    allocate(mask_tmp(nbody)) 
    ! pour chaque sous-domaine 
    do isdm=1, Nsdm
       ! pour chaque ligne de body_particip
       do i=1, size(body_particip, dim=1)
          ! on cree un masque de participation pour la ligne courante
          mask_tmp=(body_particip(i, :) == isdm)
          ! on actualise le masque de participation
          mask_particip(:, isdm) = mask_particip(:, isdm) .or. mask_tmp
       end do

       ! pour afficher mask_particip par sous-domaines
       !print *, 'mask_particip(:,', isdm, ')=', mask_particip(:, isdm)

    end do

 end subroutine creation_body_particip
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine migration_treatment !(body_particip,body_particip_old,nb_entity,max_multi,Nsdm,migration_tab)

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::
    !integer(kind=4), intent(in) :: nb_entity,max_multi_,Nsdm_
    !integer(kind=4), dimension(max_multi_,nb_entity), intent(in) :: body_particip
    !integer(kind=4), dimension(max_multi_,nb_entity), intent(in) :: body_particip_old

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    !integer(kind=4), dimension(max_multi_,nb_entity,Nsdm_), intent(out) :: migration_tab 
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: ibody, imulti, imulti_old, isdm, isdm_old
    integer                                 :: multi, multi_old
    integer(kind=4)                         :: nb_new
    character(len=1000)                     :: cout

    if ( rang /= 0 ) return

    migration_tab=0
    nb_migrations=0

    do ibody = 1,nbody
       nb_new = 0
       multi=multiplicite(ibody)
       multi_old=multiplicite_old(ibody)

       ! Je pars du principe que seul l'hôte peut communiquer
       ! avec les autres processus.
       ! Si de nouveaux sous-domaines doivent gérer ibody, l'hôte
       ! envoyer l'état du ibody à ses nouveaux
       ! gestionnaires (ou co-gestionnaires).

       do imulti= 1, multi
          isdm=body_particip(imulti,ibody)

          if ( count(isdm==body_particip_old(1:multi_old,ibody)) /= 0 ) cycle

          !print *, 'ibody=', ibody
          WRITE(cout,*) 'body_particip=',body_particip(:,ibody)
          call logmes(cout)
          call logmes('-------------------------------------------')
          WRITE(cout,*) 'body_particip_old=',body_particip_old(:,ibody)
          call logmes(cout)
          call logmes('-------------------------------------------')

          nb_migrations=nb_migrations + 1
 
          if ( isdm == 1 ) then
             call logmes("Rien à faire pour gerer cette migration (dans le sdm num 1)") 
             cycle
          end if

          nb_new=nb_new + 1
          migration_tab(ibody,nb_new)=isdm
          WRITE(cout,*) 'migration_tab(',ibody,',',nb_new,')=',migration_tab(ibody,nb_new)
          call logmes(cout)

       end do
    end do

    WRITE(cout,*) "nb_migrations=",nb_migrations
    call logmes(cout)

 end subroutine migration_treatment
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine nb_migrations2send(mig_tab_vide)

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::
    !integer(kind=4), dimension(nb_entity,max_multi_), intent(in) :: migration_tab 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    logical, intent(out) :: mig_tab_vide
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: isdm

    if ( rang /= 0 ) return
    do isdm=2,Nsdm
       vect_nb_send_host(isdm) = (count(migration_tab == isdm))
    end do
    if ( all(vect_nb_send_host == 0) ) mig_tab_vide=.true. 

 end subroutine nb_migrations2send
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_etats_indices_migration

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::
    !integer(kind=4), dimension(nb_entity,max_multi_), intent(in) :: migration_tab 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: ibody, i
    integer                                 :: isdm, shift, nb_in_colonne
    integer                                 :: deb, icolonne
    integer, dimension(1)                   :: tmp
    real(kind=8), dimension(6)              :: Xbeg_tmp,Vbeg_tmp
    real(kind=8), dimension(3,3)            :: localFrame_tmp
    integer, dimension(:), allocatable      :: compteur

    if ( rang /= 0 ) return

    if ( allocated(compteur) ) deallocate(compteur)
    allocate(compteur(Nsdm))
    compteur=0

    do isdm=2,Nsdm
       do icolonne = 1, max_multi
          deb=1
          nb_in_colonne = count( migration_tab(:,icolonne)==isdm )

          do i = 1, nb_in_colonne

             !print *, "DEB=",deb

             tmp=minloc(migration_tab(deb:nbody,icolonne),&
                         migration_tab(deb:nbody,icolonne)==isdm)

             !print *, "migration_tab(icolonne,deb:nbody)=",migration_tab(icolonne,deb:nbody)

             ibody=tmp(1) + (deb-1) 
             ! Le sous-domaine auquel on veut donner X,V est :
             compteur(isdm) = compteur(isdm) + 1

             mig_indices4all(vect_shift(isdm) + compteur(isdm)) = ibody

             !print *, "mig_indices4all(",vect_shift(isdm)," +", compteur(isdm),") = ibody=",ibody

             shift = 21*( vect_shift(isdm) + ( compteur(isdm) - 1 ) )

             call get_vector_RBDY3('Xbeg_',ibody,Xbeg_tmp,6)
             mig_etats4all(shift+1:shift+6)=Xbeg_tmp

             !print *, "PROC",rang,"Xbeg_tmp",Xbeg_tmp

             call get_vector_RBDY3('Vbeg_',ibody,Vbeg_tmp,6)
             mig_etats4all(shift+7:shift+12)=Vbeg_tmp
             !print *, "PROC",rang,"Vbeg_tmp",Vbeg_tmp


             call get_matrix_RBDY3('IFTT_', ibody, localFrame_tmp, 3)

             mig_etats4all(shift+13:shift+21)=pack(localFrame_tmp, .true.)

             deb = ibody + 1
          end do
       end do
    end do

 end subroutine compute_etats_indices_migration
!----------------------------------------------------------------


!----------------------------------------------------------------
 subroutine fix_migrants

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: ibody, ibdy
    real(kind=8), dimension(6)              :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3)           :: localFrame_tmp

                             !12345678901234567890
    character(len=20) :: IAM="DDM_3D::fix_migrants"


    if ( nb_mig_slave == 0 ) return

    do ibdy = 1, nb_mig_slave

       ibody = mig_indices_slave(ibdy)
       !print *, "PROC",rang,"STORE ibody",ibody

       X_tmp = 0.d0
       X_tmp = mig_etats_slave(21*(ibdy-1)+1: 21*(ibdy-1)+6)
       call put_vector_RBDY3('Xbeg_',ibody,X_tmp,6)
       !print *, "PROC",rang,"Xbeg_tmp",mig_etats_slave(6*(ibdy-1)+1: 6*(ibdy-1)+3)

       V_tmp=0.d0
       V_tmp = mig_etats_slave(21*(ibdy-1)+7: 21*(ibdy-1)+12)
       call put_vector_RBDY3('Vbeg_',ibody,V_tmp,6)
       !print *, "PROC",rang,"Vbeg_tmp",mig_etats_slave(6*(ibdy-1)+4: 6*ibdy)

       localFrame_tmp=0.d0

       localFrame_tmp=reshape(mig_etats_slave(21*(ibdy-1)+13: 21*(ibdy-1)+21), (/3,3/))
 
       call put_matrix_RBDY3('IFTT_', ibody, localFrame_tmp, 3)

       !write(slave_io, *) "----------Ibeg, Xbeg, Vbeg : recus----------"
       !write(slave_io, *) 'ibody=', ibdy, ' : X_beg ', X_tmp
       !write(slave_io, *) 'ibody=', ibdy, ' : V_beg ', V_tmp
    end do

 end subroutine fix_migrants
!----------------------------------------------------------------

! Copie au premier pas de temps des boites englobantes des
! polyèdres dans les tableaux minpos et maxpos tenus dans le maitre.
!----------------------------------------------------------------
 subroutine init_POLYR_TT
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: ibody
    integer            :: itact
    integer            :: compteur
    logical            :: visible

    if ( rang /= 0 ) return

    do itact=1, ntact_POLYR

       ibody = polyr2bdyty(1, itact)
 
       visible=get_visible(ibody)
       if (.not. visible) cycle

       ! on recupere la boite englobante du polyedre
       !   - le point (xmin, ymin, zmin)
       minpos_POLYR(1:3, itact) = S_POLYR(itact)%minpos
       !   - le point (xmax, ymax, zmax)
       maxpos_POLYR(1:3, itact) = S_POLYR(itact)%maxpos
    end do

 end subroutine init_POLYR_TT
!----------------------------------------------------------------
!----------------------------------------------------------------
!am: fonction qui envoie dans la premiere tranche les boites englobantes des contacteurs
! polyedres, dans la configuration de de detection,

! Rq: la gestion des doublons (particules d'interface) se fait
! suivant la règle arbitraire suivante : les quantitées stockées
! à la sortie de cette fonction sont celles du sous-domaine
! de plus haut rang (correspondance rang/sous-domaine).
 subroutine gather_POLYR_TT
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm,i
    integer            :: ibody
    integer            :: itact, nb_visible_POLYR
    integer            :: irecv
    integer            :: compteur
    logical            :: visible
    character(len=100) :: cout

    WRITE(cout,*) "ntact_POLYR=",ntact_POLYR
    call logmes(cout)

    if ( ntact_POLYR == 0 ) return

    ! Calcul du nombre de polyèdres actifs par sous-domaines

    nb_visible_POLYR=0

    do itact=1,ntact_POLYR
       ibody=polyr2bdyty(1, itact)
       visible=get_visible(ibody)
       if ( .not. visible ) cycle
       nb_visible_POLYR=nb_visible_POLYR+1
    end do

    ! Chaque sdm envoi son nb_visible_POLYR au maitre
    call MPI_GATHER(nb_visible_POLYR,1,MPI_INTEGER, &
                   vect_nb_recv_host,1,MPI_INTEGER, &
                   0,MPI_COMM_WORLD,code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)


    if ( rang == 0 ) then

       vect_shift=0
       !write(slave_io, *) "vect_shift(1) = ",vect_shift(1) 

       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_recv_host(isdm-1)
          !write(slave_io, *) "vect_shift(", isdm,") = ",vect_shift(isdm) 
       end do

       nb_recv_host=sum(vect_nb_recv_host)
       !write(slave_io, *) "nb_recv_host = ",nb_recv_host
       if (allocated(I4all)) deallocate(I4all)
       allocate(I4all(nb_recv_host))
       I4all=0

       if (allocated(Bbox4all)) deallocate(Bbox4all)
       allocate(Bbox4all(6*nb_recv_host))
       Bbox4all=0.D0
    end if

    ! Nombre de polyèdres concernées par l'envoi
    nb_send_slave = nb_visible_POLYR

    WRITE(cout,*) "nb_visible_POLYR=",nb_visible_POLYR
    call logmes(cout)

    if (allocated(I_slave)) deallocate(I_slave)
    allocate(I_slave(nb_send_slave))
    I_slave=0

    if (allocated(Bbox_slave)) deallocate(Bbox_slave)
    allocate(Bbox_slave(6*nb_send_slave))
    Bbox_slave=0.d0
    
    compteur=0
    visible=.false.

    !print *, "compteur=",compteur
    do itact=1,ntact_POLYR

       ibody=polyr2bdyty(1, itact)
       visible=get_visible(ibody)
       if ( .not. visible ) cycle

       compteur=compteur+1

       I_slave( compteur ) = ibody

       Bbox_slave( 6*(compteur-1)+1 : 6*(compteur-1) + 3) = S_POLYR(itact)%minpos
       Bbox_slave( 6*(compteur-1)+4 : 6*(compteur-1) + 6) = S_POLYR(itact)%maxpos

    end do

    !print *, "compteur=",compteur

    !do i=1, nb_visible_POLYR
    !   write(slave_io, *) 'i=', i, ' : Bbox=', Bbox_slave( 6*(i-1)+1 : 6*i)
    !end do

    !                      !12345678901234567890
    !call write_MPI_INFO_R('Bbox_slave__________',Bbox_slave,&
    !      (/ ( i, i=1,6*nb_visible_POLYR ) /),6*nb_visible_POLYR)

    ! Récuperation des indices des contacteurs gatherisés
    CALL MPI_GATHERV(I_slave, nb_send_slave, MPI_INTEGER, &
       I4all, vect_nb_recv_host, vect_shift, MPI_INTEGER, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Bbox_slave, 6*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Bbox4all,6*vect_nb_recv_host,6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)


    if ( rang /= 0 ) return

    ! Pour le comportement de la boucle ci-dessous,
    ! voir la remarque en début de routine.

    minpos_POLYR=0.d0
    maxpos_POLYR=0.d0

    do isdm=1,Nsdm
       do irecv=1,vect_nb_recv_host(isdm)

          itact = I4all( vect_shift(isdm) + irecv  )

          minpos_POLYR(1:3,itact) = Bbox4all(6*vect_shift(isdm) + 6*(irecv-1)+1 : 6*vect_shift(isdm) + 6*(irecv-1)+3) 
          maxpos_POLYR(1:3,itact) = Bbox4all(6*vect_shift(isdm) + 6*(irecv-1)+4 : 6*vect_shift(isdm) + 6*(irecv-1)+6) 

          !print*, '-----------------------'
          !print *, "minpos_POLYR(1:3,",itact,")=",minpos_POLYR(1:3,itact)
          !print *, "maxpos_POLYR(1:3,",itact,")=",maxpos_POLYR(1:3,itact)

       end do 
    end do

 end subroutine gather_POLYR_TT
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine creation_splitted_rough

    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: nb_rough_contact, mask_contact, rough_contact 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: splitted_rough(isdm), nb_splitted_rough(isdm), nb_splitted_rough_SPSPx(isdm)
    !                        nb_splitted_rough_PRPRx(isdm)

 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: icdan, icdbdy, ianbdy, isdm, compteur
    integer(kind=4)                           :: icdtac, iantac, isee
    character(len=5)                          :: cdcol, ancol
    ! structure de l'anonymous container
    integer(kind=4),    dimension(:), pointer :: cdan
    integer(kind=4),    dimension(:), pointer :: xcdan
    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx
    type(T_object)                            :: contact
    character(len=100)                        :: cout
 
    if ( rang/=0 ) return

    c5 => null()
    r8 => null()
    cx => null()

    ! Pour chaque sous-domaine
    do isdm=1,Nsdm

       nb_splitted_rough(isdm)=0

       ! * gestion des contacts sphere/sphere
       nb_splitted_rough_SPSPx(isdm)=0

       ! Pour chaque contact sphere/sphere
       do icdan=1, nb_rough_contact_SPSPx

          if (repart_sdm_cc(icdan)/=isdm) cycle

          nb_splitted_rough_SPSPx(isdm)=nb_splitted_rough_SPSPx(isdm) + 1

          ! On récupère l'objet contact d'indice icdan
          contact = get_object(rough_contact,icdan)    
          ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
          cdan => get_i4_vector(contact)
        
          allocate(xcdan(3))

          xcdan(1:2)=cdan(1:2)

          ! numéro de corps associé au contacteur courant
          icdbdy=spher2bdyty(1, cdan(1))
          ianbdy=spher2bdyty(1, cdan(2))

          if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
             xcdan(3)=INTRF
          else
             xcdan(3)=NOINT
          end if

          call add_object_to_container(splitted_rough(isdm), nb_splitted_rough_SPSPx(isdm), &
             c5, xcdan, r8, cx)

       end do

       ! * gestion des contacts polyedre/polyedre
       nb_splitted_rough_PRPRx(isdm)=0

       ! Pour chaque contact polyedre/polyedre
       do icdan=nb_rough_contact_SPSPx + 1, nb_rough_contact_SPSPx + nb_rough_contact_PRPRx

          if (repart_sdm_cc(icdan)/=isdm) cycle

          nb_splitted_rough_PRPRx(isdm)=nb_splitted_rough_PRPRx(isdm) + 1
          ! On récupère l'objet contact d'indice icdan
          contact = get_object(rough_contact,icdan)    
          ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
          cdan => get_i4_vector(contact)
        
          allocate(xcdan(3))

          ! Pour le cas séquentiel only!!!!
          xcdan(1:2)=cdan(1:2)

          ! numéro de corps associé au contacteur courant
          icdbdy=polyr2bdyty(1, cdan(1))
          ianbdy=polyr2bdyty(1, cdan(2))
          if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
             xcdan(3)=INTRF
          else
             xcdan(3)=NOINT
          end if

          call add_object_to_container(splitted_rough(isdm), nb_splitted_rough_PRPRx(isdm), &
             c5, xcdan, r8, cx)

       end do

       nb_splitted_rough(isdm)= nb_splitted_rough_SPSPx(isdm) + nb_splitted_rough_PRPRx(isdm)


       ! pour tester que les splitted_rough sont bien remplis jusqu'au bout
       !print *, "display_splitted_rough"
       !call display_object_container(splitted_rough(isdm))
       WRITE(cout,*) "nb_splitted_rough(",isdm,")    =", nb_splitted_rough(isdm)
       call logmes(cout)
       call close_container(splitted_rough(isdm))
    end do

    nb_interactions4all=3*sum(nb_splitted_rough)
    if (allocated(interactions4all)) deallocate(interactions4all)
    allocate(interactions4all(nb_interactions4all))
    interactions4all=0

    compteur=0
    ! Pour chaque contact
    do isdm=1,Nsdm
       do icdan=1,nb_splitted_rough(isdm)
          contact = get_object(splitted_rough(isdm),icdan)    
          ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
          cdan => get_i4_vector(contact)

          compteur=compteur+1
          interactions4all(3*(compteur-1)+1)=cdan(1)
          interactions4all(3*(compteur-1)+2)=cdan(2)
          interactions4all(3*(compteur-1)+3)=cdan(3)
       end do
    end do
       

 end subroutine creation_splitted_rough
!----------------------------------------------------------------


!----------------------------------------------------------------
 subroutine set_interactions_to_rough_in_DDM
    implicit none
    !--------------------------------------
    ! Arguments d'entrée de la subroutine

    !--------------------------------------
    ! gestion des contacts sphere/sphere
    if (nb_interactions_slave_SPSPx > 0) then
       call set_interactions_to_rough_SPSPx(interactions_slave(1:nb_interactions_slave_SPSPx), &
                                            nb_interactions_slave_SPSPx)
    end if
    ! gestion des contacts polyedre/polyedre
    if (nb_interactions_slave_PRPRx > 0) then
       call set_interactions_to_rough_PRPRx(interactions_slave( &
               nb_interactions_slave_SPSPx + 1 : nb_interactions_slave_SPSPx + nb_interactions_slave_PRPRx), &
               nb_interactions_slave_PRPRx)
    end if

 end subroutine set_interactions_to_rough_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
! Pour le cas de la détection fine séquentielle
!----------------------------------------------------------------
! subroutine launch_set_anonymous_to_rough_4all
!    implicit none
!
!    !--------------------------------------
!    ! gestion des contacts sphere/sphere
!    if (nb_rough_contact_SPSPx > 0) then
!       call set_anonymous_to_rough_SPSPx(shiftted_rough_SPSPx, &
!          nb_shiftted_rough_SPSPx)
!    end if
!    ! gestion des contacts polyedre/polyedre
!    if (nb_rough_contact_PRPRx > 0) then
!       call set_anonymous_to_rough_PRPRx(shiftted_rough_PRPRx, &
!          nb_shiftted_rough_PRPRx)
!    end if
! end subroutine launch_set_anonymous_to_rough_4all
!!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_rough_contact
    implicit none
    !--------------------------------------
    if (rang == 0) call erase_container(rough_contact)
 end subroutine erase_rough_contact
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_splitted_rough
    implicit none
    !--------------------------------------
    integer :: isdm

    if (rang /=0 ) return
    do isdm=1,Nsdm
       call erase_container(splitted_rough(isdm))
    end do
 end subroutine erase_splitted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine get_list_INTRF_in_DDM
    implicit none

    ! variables locales
    integer :: nb_INTRF_SPSPx ! Nombre de contacts (sphere/sphere) fins taggés 'INTRF'
    integer :: nb_INTRF_PRPRx ! Nombre de contacts (polyedre/polyedre) fins taggés 'INTRF'

    !--------------------------------------

    !print*, 'rang=', rang, ' : nb_interactions_slave_SPSPx=', nb_interactions_slave_SPSPx  

    !write (slave_io,*) '------get_list_INTRF_in_DDM--------' 
    !write (slave_io,*) 'nb_rough_interactions_slave_SPSPx=', nb_interactions_slave_SPSPx 
    !write (slave_io,*) 'nb_fine_interactions_slave_SPSPx=', get_nb_SPSPx(i_real_tactor)


    ! Arguments de sortie de la subroutine
    ! implicite : nb_INTRF et list_INTRF

    ! Calcule le nombre de contacts taggés INTRF
    !    * cas des contacts sphere-sphere
    nb_INTRF_SPSPx = 0
    if (nb_interactions_slave_SPSPx > 0) then
       call get_nb_INTRF_SPSPx(nb_INTRF_SPSPx)

       !write (slave_io,*) 'nb_INTRF_SPSPx=',nb_INTRF_SPSPx 

       !print *, 'nb contacts interface (SPSPx)', nb_INTRF_SPSPx 
       !print *, 'rang=', rang, ' : nb contacts interface (SPSPx)', nb_INTRF_SPSPx 
    end if

    !    * cas des contacts polyedre-polyedre
    nb_INTRF_PRPRx = 0
    if (nb_interactions_slave_PRPRx > 0) then
       call get_nb_INTRF_PRPRx(nb_INTRF_PRPRx)
    
      ! print *, 'nb contacts interface (PRPRx)', nb_INTRF_PRPRx 
      ! print *, 'rang=', rang, ' : nb contacts interface (PRPRx)', nb_INTRF_PRPRx 
    end if

    ! calcul du nombre total de contacts taggés INTRF
    nb_INTRF = nb_INTRF_SPSPx + nb_INTRF_PRPRx 

    !print *, 'nb contacts interface', nb_INTRF
    !print *, 'rang=', rang, ' : nb contacts interface', nb_INTRF

    ! s'il n'y a aucun contact d'interface, on quitte la fonction
    if (nb_INTRF == 0) return

    if (allocated(list_INTRF)) deallocate(list_INTRF)
    allocate(list_INTRF(nb_INTRF))
    list_INTRF = 0

    ! Détermine la liste des contacts taggés INTRF
    !   * cas des contacts sphere-sphere
    if (nb_INTRF_SPSPx > 0) then
       call get_list_INTRF_SPSPx(nb_INTRF_SPSPx, list_INTRF(1:nb_INTRF_SPSPx))
    end if
    !   * cas des contacts polyedre-polyedre
    if (nb_INTRF_PRPRx > 0) then
       call get_list_INTRF_PRPRx(nb_INTRF_PRPRx, list_INTRF(nb_INTRF_SPSPx + 1:nb_INTRF_SPSPx + nb_INTRF_PRPRx))
    end if

    ! on passe la liste de contacts de la numerotation du module SPSPx a celle du module NLGS
    !   * cas des contacts sphere-sphere
    if (nb_INTRF_SPSPx > 0) then
       list_INTRF(1:nb_INTRF_SPSPx) = list_INTRF(1:nb_INTRF_SPSPx) + shift_icdan(i_spspx)
    end if
    !   * cas des contacts polyedre-polyedre
    if (nb_INTRF_PRPRx > 0) then
       list_INTRF(nb_INTRF_SPSPx + 1:nb_INTRF_SPSPx + nb_INTRF_PRPRx) = &
          list_INTRF(nb_INTRF_SPSPx + 1:nb_INTRF_SPSPx + nb_INTRF_PRPRx) + shift_icdan(i_prprx)
    end if

 end subroutine get_list_INTRF_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
!am: calcul des torseurs des reactions de contact at stockage dans Iaux
 subroutine RnodHRloc_list_in_DDM
    implicit none
    !--------------------------------------

    ! variable locale
    integer :: storage_reac

    ! stockage des reactions de contact dans Iaux
    storage_reac = iIaux_

    ! s'il n'y a aucun contact d'interface, on ne fait rien
    if (nb_INTRF == 0) return

    !write(slave_io,*) '-------RnodHRloc_list_in_DDM------' 
    !write(slave_io,*) 'nb_INTRF=', nb_INTRF 


    ! calcul des torseurs des reactions de contact
    call RnodHRloc_nlgs(list_INTRF, storage_reac)

 end subroutine RnodHRloc_list_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_local_free_vlocy_in_DDM
    implicit none
    !--------------------------------------

    ! s'il n'y a aucun contact d'interface, on ne fait rien
    if (nb_INTRF == 0) return

    !write(slave_io,*) '-------compute_local_free_vlocy_in_DDM------' 
    !write(slave_io,*) 'nb_INTRF=', nb_INTRF 

    call compute_local_free_vlocy(list_INTRF)

 end subroutine compute_local_free_vlocy_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_modified_mass
    
    implicit none
    
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 
    integer              :: isdm, i_rbdy3         ! indice de sous-domaine considere, indice de RBDY3
    
    do ibody = 1,nbody
    
       ! La multiplicite du RBDY3 pour ce pas de temps est...
       multi=multiplicite(ibody)
   
       ! Comme on ne sait pas quelle était la multiplicité du RBDY3 au pas précédent,
       ! on se base sur sa multiplicité courante et sa masse de référence pour 
       ! fixer la nouvelle masse.
       if (multiplicite(ibody) <= 1) then
          masses_courantes(ibody)=masse_ref(ibody)
       else  
          ! Sa nouvelle masse devient
          !am: bugge ou pas?
          !masses_courantes=masse_ref(ibody)/multi
          masses_courantes(ibody)=masse_ref(ibody)/multi
       end if
       
    end do
    
 end subroutine compute_modified_mass
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_modified_mass_in_DDM
    
    implicit none
    
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 

                             !12345678901234567890123456789012
    character(len=32) :: IAM='DDM_3D::set_modified_mass_in_DDM'

    do ibody = 1,nbody

       !am : le test suivant a ete recupere dans mod_DDM_MPI_2D...
       !     Il prevoit deux cas :
       !     1- si le processus courant est un esclave, on a besoin de modifier la masse des seuls corps visibles
       !     2- si le processus courant est le maitre, on doit modifier la masse de TOUS les corps, pour pouvoir ecrire le pb d'interface
       !        correctement!
       if ( rang /= 0 .and. (.not. mask_in_slave(ibody))) cycle

       ! La multiplicite du RBDY3 pour ce pas de temps est...
       multi=multiplicite(ibody)

       ! Comme on ne sait pas quelle était la multiplicité du RBDY3 au pas précédent,
       ! on se base sur sa multiplicité courante et sa masse de référence pour 
       ! fixer la nouvelle masse.
       if ( multi == 1 ) then
          masses_courantes(ibody)=masse_ref(ibody)
       else if ( multi > 1 ) then
          ! Sa nouvelle masse devient
          masses_courantes(ibody)=masse_ref(ibody)/multi
       else if ( multi < 1 ) then
          call faterr(IAM, "La multiplicite d une particule est < à 1!!")
       end if

       call set_mass_RBDY3(ibody, masses_courantes(ibody))
    
       !print *, "corps duplique=", i_rbdy3
       !print *, 
       !print *, "masse de ref  =", masse_ref(ibody) 
       !print *, 
       !print *, "nouvelle masse=", get_mass(i_rbdy3)
    end do
    
 end subroutine set_modified_mass_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_interface
                            
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), intent(in)                        :: Nsdm
    ! integer(kind=4), dimension(nbody,nsdm), intent(in) :: mask_particip(:,Nsdm)
    ! integer(kind=4), dimension(nbody), intent(in)      :: multiplicite 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! integer(kind=4), dimension(Nsdm), intent(out)      :: nb_RBDY3_interf_sdm(isdm)
    ! integer(kind=4), intent(out)                       :: nb_RBDY3_interf_glob
    ! integer(kind=4), dimension(:,:), intent(out)       :: interface_globale
    !   alloué dans cette fonction à (/ 2, nb_RBDY3_interf_glob /)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody,isdm,ipart,i_std_bdy,imulti,err,compteur
    integer(kind=4), dimension(:), allocatable :: isdm_multi_tab
    integer, dimension(:), allocatable :: nb_grains_multi
    logical                            :: visible
    character(len=100):: cout
                             !1234567890123456789012345
    character(len=25) :: IAM='DDM_3D::compute_interface'

    ! Déterminer le nombre de grains d'interface pour ce sous-domaine.
    ! Je le tente par la fonction intrinseque "merge"

    if (allocated(isdm_multi_tab)) deallocate(isdm_multi_tab)
    allocate(isdm_multi_tab(nbody), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de isdm_multi_tab")

    ! Traitement de l'interface pour chaque sous domaine
    do isdm=1,Nsdm
     
       ! On pourrait sans doute supprimer cette étape, mais elle fournit pour l'instant 
       ! une sécurité appréciable (redondance)
       ! Mise à 0 la valeur de multiplicité des RBDY3 qui ne sont pas dans isdm 
       isdm_multi_tab=merge(multiplicite, 0, mask_particip(:, isdm))
       ! Calcul du nombre de RBDY3 d'interface (pour le sdm considéré)
       nb_RBDY3_interf_sdm(isdm)=count(isdm_multi_tab > 1)

       ! On peut maintenant allouer les champs d'interface du sous-domaine avec cette valeur!
       if ( associated(liste_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(liste_RBDY3_interf_sdm(isdm)%particule)
       allocate(liste_RBDY3_interf_sdm(isdm)%particule(nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de liste_RBDY3_interf_sdm(isdm)%particule")


       ! On va maintenant remplir le vecteur des RBDY3 d'interface par sous-domaine
       compteur=0
       visible=.true.

       do ibody=1,nbody ! Boucle sur les corps
          visible=mask_particip(ibody, isdm)
          if ( .not. visible .or. multiplicite(ibody) < 2 ) cycle
          compteur=compteur + 1 

          liste_RBDY3_interf_sdm(isdm)%particule(compteur)=ibody
       end do

 
       ! test puis affichage du nombre de RBDY3 d'interface pour le sous-domaine courant
       if (compteur /= nb_RBDY3_interf_sdm(isdm))  call faterr(IAM, "Trop ou pas assez de grains " // &
                                                  "d'interface indentifies dans un des sous-domaines")

       WRITE(cout,*) "nb_RBDY3_interf_sdm de sdm num ", isdm, " = ", nb_RBDY3_interf_sdm(isdm)
       call logmes(cout)

       if ( associated(V_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(V_RBDY3_interf_sdm(isdm)%particule)
       allocate(V_RBDY3_interf_sdm(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de V_RBDY3_interf_sdm(isdm)%paticule")

       if ( associated(V_RBDY3_interf_sdm_old(isdm)%particule) ) & 
            deallocate(V_RBDY3_interf_sdm_old(isdm)%particule)
       allocate(V_RBDY3_interf_sdm_old(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de V_RBDY3_interf_sdm(isdm)%paticule")


       ! Dimensionnement des positions des RBDY3 d'interface
       ! N.B. stockage des coordonnees des centre d'inerties des corps, i.e. vecteurs a trois composantes
       if ( associated(X_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(X_RBDY3_interf_sdm(isdm)%particule)
       allocate(X_RBDY3_interf_sdm(isdm)%particule(3, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de X_RBDY3_interf_sdm(isdm)%paticule")

       ! Dimensionnement des reperes principaux d'inertie des RBDY3 d'interface 
       ! N.B. stockage des coordonnees des reperes principaux d'inerties des corps, i.e. matrices 3 x 3
       if ( associated(localFrame_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(localFrame_RBDY3_interf_sdm(isdm)%particule)
       allocate(localFrame_RBDY3_interf_sdm(isdm)%particule(9, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de localFrame_RBDY3_interf_sdm(isdm)%paticule")

       if ( associated(DFg_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(DFg_RBDY3_interf_sdm(isdm)%particule)
       allocate(DFg_RBDY3_interf_sdm(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de DFg_RBDY3_interf_sdm(isdm)%paticule")
       
       ! Mise à zero
       V_RBDY3_interf_sdm(isdm)%particule=0.D0
       V_RBDY3_interf_sdm_old(isdm)%particule=0.D0
       X_RBDY3_interf_sdm(isdm)%particule=0.D0
       localFrame_RBDY3_interf_sdm(isdm)%particule=0.D0
       DFg_RBDY3_interf_sdm(isdm)%particule=0.D0
       
    end do

    ! Traitement de l'interface globale
    ! Calcul du nombre de particule dans l'interface globale
    nb_RBDY3_interf_glob = count(multiplicite>1)
    WRITE(cout,*) "nombre de particule de l'interface globale=", nb_RBDY3_interf_glob
    call logmes(cout)

    if ( allocated(interface_globale) ) deallocate(interface_globale)
    allocate(interface_globale(nb_RBDY3_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de interface_globale")

    ! Allocation du tableau des positions à convergence des RBDY3 d'interface
    ! N.B. stockage des coordonnees des centre d'inerties des corps, i.e. vecteurs a trois composantes
    if ( allocated(X_RBDY3_interf_glob) ) deallocate(X_RBDY3_interf_glob)
    allocate(X_RBDY3_interf_glob(3, nb_RBDY3_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de X_RBDY3_interf_glob")
    ! Allocation du tableau des reperes pincipaux d'inertie à convergence des RBDY3 d'interface
    ! N.B. stockage des coordonnees des reperes principaux d'inerties des corps, i.e. matrices 3 x 3
    if ( allocated(localFrame_RBDY3_interf_glob) ) deallocate(localFrame_RBDY3_interf_glob)
    allocate(localFrame_RBDY3_interf_glob(9, nb_RBDY3_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de localFrame_RBDY3_interf_glob")

    ! Allocation du tableau des vitesses à convergence des RBDY3 d'interface
    if ( allocated(V_RBDY3_interf_glob) ) deallocate(V_RBDY3_interf_glob)
    allocate(V_RBDY3_interf_glob(6, nb_RBDY3_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de V_RBDY3_interf_glob")

    ! Construction de l'interface globale
    compteur=0
    do ibody=1,nbody
       if ( multiplicite(ibody)<=1 ) cycle
       compteur=compteur+1
       interface_globale(compteur)=ibody
    end do

    if (compteur/=nb_RBDY3_interf_glob) call faterr(IAM, "compteur/=nb_RBDY3_interf_glob")


    ! Calcul du nombre de grains d'interface
    nb_liens_interf=0
    if (.not. allocated(nb_grains_multi)) &
              allocate(nb_grains_multi(max_multi), stat=err)
    if ( err/=0 ) call faterr(IAM, "probleme d'allocation de nb_grains_multi")

    ! Pour les cluster, c'est peut-être différent!
    do imulti=2, max_multi ! i.e. la valeur max de multiplicité si on 
                           ! ne considère pas des cluster qui sont en diagonale
                           ! ou en serpentin
       nb_grains_multi(imulti)=count(multiplicite(:) == imulti)
       nb_liens_interf = nb_liens_interf + (imulti - 1)*nb_grains_multi(imulti)
    end do

    WRITE(cout,*) "nb_liens_interf=", nb_liens_interf
    call logmes(cout)

    ! Allocation du tableau des sauts de vitesse d'interface
    if ( allocated(saut_V_interf_glob) ) deallocate(saut_V_interf_glob)
    allocate(saut_V_interf_glob(6, nb_liens_interf), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de saut_V_interf_glob")

    deallocate(isdm_multi_tab)
    deallocate(nb_grains_multi)

 end subroutine compute_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine prep_exchange_inloop

    implicit none

    integer :: isdm
    integer :: i

    !---------------------------
    ! VITESSES D'INTERFACE & DFg
    !---------------------------

    if ( rang == 0 ) then

       do isdm=1,Nsdm
          vect_nb_recv_host(isdm) = nb_RBDY3_interf_sdm(isdm)
       end do

       nb_recv_host=sum(vect_nb_recv_host)

       if (allocated(V_interf4all)) deallocate(V_interf4all)
       allocate(V_interf4all(6*nb_recv_host))
       V_interf4all=0.D0

       nb_send_host=nb_recv_host

       if (allocated(DFg_interf4all)) deallocate(DFg_interf4all)
       allocate(DFg_interf4all(6*nb_send_host))
       DFg_interf4all=0.D0

       ! les tranches envoyées (DFg) ou reçues (V) par l'hôte
       ! auront les dimensions. 
       vect_nb_send_host=6*nb_RBDY3_interf_sdm
       vect_nb_recv_host=6*nb_RBDY3_interf_sdm
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do
       
    end if

    ! Calcul de la taille d'un message recu par le sdm (6 reels pour chaque corps d'interface)
    nb_send_slave = 6*nb_RBDY3_interf_slave
    nb_recv_slave = 6*nb_RBDY3_interf_slave

    ! on alloue l'espace memoire pour stocker l'agregation des vitesses des corps d'interface du sdm courant
    ! (6 reels pour chaque corps d'interface)
    if (allocated(V_RBDY3_interf_slave)) deallocate(V_RBDY3_interf_slave)
    allocate(V_RBDY3_interf_slave(6*nb_RBDY3_interf_slave))
    if (allocated(V_RBDY3_interf_slave_old)) deallocate(V_RBDY3_interf_slave_old)
    allocate(V_RBDY3_interf_slave_old(6*nb_RBDY3_interf_slave))
   
    V_RBDY3_interf_slave= 0.D0
    V_RBDY3_interf_slave_old= 0.D0
   
    ! Allocation de l'espace memoire pour stocker l'agregation des increments des forces 
    ! de cohesion sur les corps d'interface du sdm courant
    if (allocated(DFg_RBDY3_interf_slave)) deallocate(DFg_RBDY3_interf_slave)
    allocate(DFg_RBDY3_interf_slave(6*nb_RBDY3_interf_slave))
    DFg_RBDY3_interf_slave = 0.D0

 end subroutine prep_exchange_inloop 
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine gather_V_interface(id_vlocy)
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody)      :: multiplicite 
    !am: id pour recuperer les vitesses avec un get_vector
    character(len=5), intent(in) :: id_vlocy

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! V_RBDY3_interf_slave
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, isdm, i, i_std_bdy, i_RBDY3_interf_sdm, multi, compteur, err
    logical :: visible
    real(kind=8), dimension(6) :: vlocy

    !                         12345678901234567890123456
    character(len=26) :: IAM='DDM_3D::gather_V_interface'


    ! TEST TEST TEST
    call start_utimer(id_prep_gather_V)

    ! On rempli la table V_RBDY3_interf_sdm(rang)
    
    V_RBDY3_interf_slave_old=V_RBDY3_interf_slave

    do i_RBDY3_interf_sdm=1,nb_RBDY3_interf_slave 
       ibody=liste_RBDY3_interf_slave(i_RBDY3_interf_sdm)
       call get_vector_RBDY3(id_vlocy, ibody, vlocy, 6)
       V_RBDY3_interf_slave( 6*(i_RBDY3_interf_sdm-1)+1 : 6*(i_RBDY3_interf_sdm) )=vlocy

       !write(slave_io, *) 'i=', ibody, ' : V=', &
       !      V_RBDY3_interf_slave(6*(i_RBDY3_interf_sdm-1) + 1:6*(i_RBDY3_interf_sdm))
    end do
    call stop_utimer(id_prep_gather_V)
    
    call start_utimer(id_gather_V)
    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(V_RBDY3_interf_slave, nb_send_slave, MPI_DOUBLE_PRECISION, &
       V_interf4all, vect_nb_recv_host, vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)

    ! paranoid test
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    !write(slave_io,*) "-------------gather V--------------"
    !write(slave_io,*) "size(V_interf4all)=", size(V_interf4all)

    call stop_utimer(id_gather_V)

    call start_utimer(id_post_gather_V)
    if (rang /=0) return
                          !12345678901234567890
    !call write_MPI_INFO_R('V_INTERFACE_________',V_interf4all,&
    !      (/(i,i=1,sum(vect_nb_recv_host))/),sum(vect_nb_recv_host))

    do isdm = 1, Nsdm
       do i = 1, vect_nb_recv_host(isdm)/6
          V_RBDY3_interf_sdm(isdm)%particule(1:6,i) = & 
           V_interf4all(vect_shift(isdm) + 6*(i-1)+1 :  vect_shift(isdm) + 6*i)
       end do
    end do
   
    !do isdm = 1, Nsdm
    !   do i=1, nb_RBDY3_interf_sdm(isdm)
    !      print *, "V_RBDY3_interf_sdm(", isdm, ")%particule(:, ", i,")= "&
    !               ,V_RBDY3_interf_sdm(isdm)%particule(:, i)
    !   end do
    !end do
    call stop_utimer(id_post_gather_V)
    
 end subroutine gather_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine gather_X_V_localFrame_interface
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody)      :: multiplicite 
    !am: id pour recuperer les vitesses avec un get_vector

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! V_RBDY3_interf_slave
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, isdm, i, i_std_bdy, i_RBDY3_interf_sdm, multi, compteur, err
    logical :: visible
    real(kind=8), dimension(6) :: vlocy, X
    real(kind=8), dimension(3,3) :: localFrame

    !                         123456789012345678901234567890123456789
    character(len=39) :: IAM='DDM_3D::gather_X_V_localFrame_interface'

    if ( rang == 0 ) then

       do isdm=1,Nsdm
          vect_nb_recv_host(isdm) = nb_RBDY3_interf_sdm(isdm)
       end do

       nb_recv_host=sum(vect_nb_recv_host)

       if (allocated(X_interf4all)) deallocate(X_interf4all)
       allocate(X_interf4all(3*nb_recv_host))
       X_interf4all=0.D0

       if (allocated(localFrame_interf4all)) deallocate(localFrame_interf4all)
       allocate(localFrame_interf4all(9*nb_recv_host))
       localFrame_interf4all=0.D0

       ! les tranches envoyées (DFg) ou reçues (V) par l'hôte
       ! auront la forme. 
       vect_nb_recv_host=nb_RBDY3_interf_sdm
       vect_nb_send_host=nb_RBDY3_interf_sdm
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_recv_host(isdm-1)
       end do

    end if

    if (allocated(X_RBDY3_interf_slave)) deallocate(X_RBDY3_interf_slave)
    allocate(X_RBDY3_interf_slave(3*nb_RBDY3_interf_slave))
    X_RBDY3_interf_slave= 0.D0

    if (allocated(localFrame_RBDY3_interf_slave)) deallocate(localFrame_RBDY3_interf_slave)
    allocate(localFrame_RBDY3_interf_slave(9*nb_RBDY3_interf_slave))
    localFrame_RBDY3_interf_slave= 0.D0

    ! On rempli la table V_RBDY3_interf_sdm(rang)
    
    do i_RBDY3_interf_sdm=1,nb_RBDY3_interf_slave 
       ibody=liste_RBDY3_interf_slave(i_RBDY3_interf_sdm)
       call get_vector_RBDY3('V____', ibody, vlocy, 6)
       V_RBDY3_interf_slave( 6*(i_RBDY3_interf_sdm-1)+1 : 6*(i_RBDY3_interf_sdm) )=vlocy

       call get_vector_RBDY3('X____', ibody, X, 6)
       X_RBDY3_interf_slave( 3*(i_RBDY3_interf_sdm-1)+1 : 3*(i_RBDY3_interf_sdm) )=X(1:3)

       call get_matrix_RBDY3('IF___', ibody, localFrame, 3)
       localFrame_RBDY3_interf_slave( 9*(i_RBDY3_interf_sdm-1) + 1 : 9*(i_RBDY3_interf_sdm)) = pack(localFrame, .true.)

       !write(slave_io, *) '-----------------------------------'

       !write(slave_io, *) 'i_RBDY3_interf_sdm=', i_RBDY3_interf_sdm

       !write(slave_io, *) 'i=', ibody, ' : vlocy=', vlocy
       !write(slave_io, *) 'i=', ibody, ' : X_tmp=', X
       !write(slave_io, *) 'i=', ibody, ' : localFrame_tmp=', localFrame

       !write(slave_io, *) 'i=', ibody, ' : V=', &
       !      V_RBDY3_interf_slave(6*(i_RBDY3_interf_sdm-1) + 1:6*(i_RBDY3_interf_sdm))
       !write(slave_io, *) 'i=', ibody, ' : X=', &
       !      X_RBDY3_interf_slave(3*(i_RBDY3_interf_sdm-1) + 1:3*(i_RBDY3_interf_sdm))
       !write(slave_io, *) 'i=', ibody, ' : localFrame=', &
       !      localFrame_RBDY3_interf_slave(9*(i_RBDY3_interf_sdm-1) + 1:9*(i_RBDY3_interf_sdm))
    end do

    nb_send_slave = 6*nb_RBDY3_interf_slave

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(V_RBDY3_interf_slave, nb_send_slave, MPI_DOUBLE_PRECISION, &
       V_interf4all, 6*vect_nb_recv_host, 6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)

    nb_send_slave = 3*nb_RBDY3_interf_slave

    ! Récuperation des deplacements d'interface
    CALL MPI_GATHERV(X_RBDY3_interf_slave, nb_send_slave, MPI_DOUBLE_PRECISION, &
       X_interf4all, 3*vect_nb_recv_host, 3*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)

    nb_send_slave = 9*nb_RBDY3_interf_slave

    ! Récuperation des orientations des reperes prncipaux d'inertie d'interface
    CALL MPI_GATHERV(localFrame_RBDY3_interf_slave, nb_send_slave, MPI_DOUBLE_PRECISION, &
       localFrame_interf4all, 9*vect_nb_recv_host, 9*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)

    ! paranoid test
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if (rang /=0) return
                          !12345678901234567890
    !call write_MPI_INFO_R('V_INTERFACE_________',V_interf4all,&
    !      (/(i,i=1,sum(vect_nb_recv_host))/),sum(vect_nb_recv_host))

    do isdm = 1, Nsdm
       do i = 1, nb_RBDY3_interf_sdm(isdm)
          V_RBDY3_interf_sdm(isdm)%particule(1:6,i) = & 
           V_interf4all(6*vect_shift(isdm) + 6*(i-1)+1 :  6*vect_shift(isdm) + 6*i)
          X_RBDY3_interf_sdm(isdm)%particule(1:3,i) = & 
           X_interf4all(3*vect_shift(isdm) + 3*(i-1)+1 :  3*vect_shift(isdm) + 3*i)
          localFrame_RBDY3_interf_sdm(isdm)%particule(1:9,i) = & 
           localFrame_interf4all(9*vect_shift(isdm) + 9*(i-1)+1 : 9*vect_shift(isdm) + 9*i)
 
          !print*, 'i=', i, ' : V=', V_interf4all(6*vect_shift(isdm) + 6*(i-1)+1 :  6*vect_shift(isdm) + 6*i)
          !print*, 'i=', i, ' : X=', X_interf4all(3*vect_shift(isdm) + 3*(i-1)+1 :  3*vect_shift(isdm) + 3*i)
          !print*, 'i=', i, ' : localFrame=', localFrame_interf4all(9*vect_shift(isdm) + 9*(i-1)+1 :  9*vect_shift(isdm) + 9*i)
       end do
    end do
    
 end subroutine gather_X_V_localFrame_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_DF_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! type(T_ligne_i), dimension(Nsdm)   :: liste_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm)   :: V_RBDY3_interf_sdm(isdm)
    ! integer, dimension(/ max_multi, nbody /) :: body_particip
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! Liste des F_gamma signés par sous-domaines
    ! type(T_ligne_r), dimension(Nsdm)   :: DFg_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm)   :: Fg_RBDY3_interf_sdm(isdm)
    ! real, dimension(nb_liens_interf)   :: saut_V_interf_glob
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine

    integer :: nb_liens      ! nombre de liens de la particule considérée
    integer :: i_liens       ! indice du lien
    integer :: imulti, multi ! indice et multiplicité de la particule considérée
    integer :: ibody         ! indice du RBDY3 dans la liste globale
    integer :: i_parti       ! indice des particules d'interface
    integer :: isdm          ! indice sur les sous-domaines
    integer :: compteur      ! ce compteur doit être égal, en fin de boucle, à nb_liens_interf
    integer, dimension(1) :: tmp           ! variable temporaire
    integer, dimension(2) :: sdm           ! sous domaine courant
    integer, dimension(2) :: signe         ! pour faire alterner le signe
    real(kind=8), dimension(:), pointer :: mass ! matrice de masse du RBDY3 courant

                             !12345678901234567890123 
    character(len=23) :: IAM='DDM_3D::compute_DF_gamma'

    integer,      dimension(:), allocatable :: part_sdm ! indices de la particule courante de l'interface dans les deux sdm du lien considéré
    real(kind=8), dimension(:), allocatable :: DF_gamma ! increment de F_gamma calcule a partir du saut de vitesse courant [| V |]
                                                        ! taille : 6*nb_liens
    real(kind=8), dimension(:), allocatable :: V_jump   ! saut de vitesse courant
                                                        ! taille : 6*nb_liens

    type(G_matrix) :: A                ! matrice du systeme a resoudre pour calculer delta_F_gamma : A*DF = M*[| V |]
    character(len=8) :: matrix_type    ! type de stockage pour la G_matrix (i.e. matrice diagonale ou bande symetrique)
    integer :: bw                      ! largeur de bande de la matrice, dans le cas avec plusieurs liens
    integer, dimension(1) :: perm_fake ! tableau de pemutation bidon utilise pour delcarer la G_matrix 
    integer :: info                    ! variable indiquant si la resolution du systeme s'est bien passee
 
    integer :: i  ! indice de boucle

    ! Coefficients des matrices L^-1
    ! Pour nb_liens=2
    real(kind=8), parameter :: d2_3=(2.d0/3.d0), d1_3=(1.d0/3.d0)
    ! Pour nb_liens=3
    real(kind=8), parameter :: d3_4=0.75d0, d1_2=0.5d0, d1_4=0.25d0 ! 1=1.d0
    ! Pour nb_liens=4
    real(kind=8), parameter :: d4_5=0.8d0, d3_5=0.6d0, d2_5=0.4d0, d1_5=0.2d0, d6_5=1.2d0
    ! Pour nb_liens=5
    real(kind=8), parameter :: d5_6=(5.d0/6.d0), d1_6=(1.d0/6.d0), d4_3=(4.d0/3.d0), d3_2=1.5d0
    ! Pour nb_liens=6
    real(kind=8), parameter :: d6_7=(6.d0/7.d0), d5_7=(5.d0/7.d0), d4_7=(4.d0/7.d0), d3_7=(3.d0/7d0), d2_7=(2.d0/7.d0), &
                               d1_7=(1.d0/7.d0), d10_7=(10.d0/7.d0), d8_7=(8.d0/7.d0), d12_7=(12.d0/7.d0),              &
                               d9_7=(9.d0/7.d0)
    ! Pour nb_liens=7
    real(kind=8), parameter :: d7_8=0.875d0, d5_8=0.625d0, d3_8=0.375d0, d1_8=0.125d0, d5_4=1.25d0, d15_8=1.875d0, d9_8=1.125d0

    if ( rang /= 0 ) return

    compteur=0
    saut_V_interf_glob=0

    ! on fixe la regle pour l'affectation des signes
    signe(1)= +1
    signe(2)= -1

    ! Boucle sur tous les grains d'interface. Ils devraient contenir tous les liens 
    ! d'interface, ce que l'on vérifie à la fin de la boucle

    do i_parti = 1, nb_RBDY3_interf_glob

       ! RBDY3 sur lequel on travaille et sa multiplicité
       ibody = interface_globale(i_parti)
       !print *, "ibody=",ibody
       multi = multiplicite(ibody)  ! on va tapper dans multiplicité directement 
       ! Récupération des paramètres inertiels ! qu'est qu'on fait???
       mass => get_ptr_mass(ibody)
     
       ! calcul du nombre de liens pour le corps courant
       nb_liens=multi-1

       ! allocation de l'espace memoire pour les tabeleaux :
       !   * la table associant a une paire (indice de lien, sous-domaine du lien), le numero de RBDY3 corespondant
       !allocate(part_sdm(nb_liens, 2))
       allocate(part_sdm(multi))
       !   * le vecteur DF_gamma
       allocate(DF_gamma(6*nb_liens))
       !   * le vecteur [| V |]
       allocate(V_jump(6*nb_liens))

       ! pour chaque sous-domaine auquel appartient le corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
          ! On va pècher l'indice du RBDY3 ibody dans la liste des
          ! RBDY3 d'interface du sdm courant
          tmp=maxloc(liste_RBDY3_interf_sdm(sdm(1))%particule(:), &
                     liste_RBDY3_interf_sdm(sdm(1))%particule(:) == ibody)
          ! on le stocke
          part_sdm(isdm)=tmp(1)
       end do

       ! pour chaque lien
       do i_liens=1, nb_liens
          compteur=compteur+1
 
          ! on recupere les duex sous-domaines associes au lien courant
          sdm(1)=body_particip(i_liens, ibody)
          sdm(2)=body_particip(i_liens + 1, ibody)

          ! on peut alors calculer le saut de vitesse pour le lien courant
          V_jump(6*(i_liens - 1) + 1 : 6*i_liens) = &
             signe(1) * V_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) + &
             signe(2) * V_RBDY3_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1))

          ! Pour le post-traitement, stockage des sauts de vitesses des grains d'interface
          saut_V_interf_glob(:, compteur)=V_jump(6*(i_liens - 1) + 1 : 6*i_liens)
       end do

       ! on choisi la methode de resolution selon le nombre de liens
       if (nb_liens < 8) then

          SELECT CASE(nb_liens)
          CASE(1)
             DF_gamma(1:6)  = mass(1:6) * d1_2*V_jump(1:6)
          CASE(2)
             DF_gamma(1:6)  = mass(1:6) * ( d2_3*V_jump(1:6) + d1_3*V_jump(7:12) )
             DF_gamma(7:12) = mass(1:6) * ( d1_3*V_jump(1:6) + d2_3*V_jump(7:12) )
          CASE(3)
             DF_gamma(1:6)  = mass(1:6) * ( d3_4*V_jump(1:6) + d1_2*V_jump(7:12) + d1_4*V_jump(13:18) )
             DF_gamma(7:12) = mass(1:6) * ( d1_2*V_jump(1:6) + 1.d0*V_jump(7:12) + d1_2*V_jump(13:18) )
             DF_gamma(13:18)= mass(1:6) * ( d1_4*V_jump(1:6) + d1_2*V_jump(7:12) + d3_4*V_jump(13:18) )
          CASE(4)
             DF_gamma(1:6)  = mass(1:6) * ( d4_5*V_jump(1:6) + d3_5*V_jump(7:12) + d2_5*V_jump(13:18) + d1_5*V_jump(19:24) )
             DF_gamma(7:12) = mass(1:6) * ( d3_5*V_jump(1:6) + d6_5*V_jump(7:12) + d4_5*V_jump(13:18) + d2_5*V_jump(19:24) )
             DF_gamma(13:18)= mass(1:6) * ( d2_5*V_jump(1:6) + d4_5*V_jump(7:12) + d6_5*V_jump(13:18) + d3_5*V_jump(19:24) )
             DF_gamma(19:24)= mass(1:6) * ( d1_5*V_jump(1:6) + d2_5*V_jump(7:12) + d3_5*V_jump(13:18) + d4_5*V_jump(19:24) )
          CASE(5)
             DF_gamma(1:6)  = mass(1:6) * ( d5_6*V_jump(1:6) + d2_3*V_jump(7:12) + d1_2*V_jump(13:18) + d1_3*V_jump(19:24) + &
                                            d1_6*V_jump(25:30) )
             DF_gamma(7:12) = mass(1:6) * ( d2_3*V_jump(1:6) + d4_3*V_jump(7:12) + 1.d0*V_jump(13:18) + d2_3*V_jump(19:24) + &
                                            d1_3*V_jump(25:30) )
             DF_gamma(13:18)= mass(1:6) * ( d1_2*V_jump(1:6) + 1.d0*V_jump(7:12) + d3_2*V_jump(13:18) + 1.d0*V_jump(19:24) + &
                                            d1_2*V_jump(25:30) )
             DF_gamma(19:24)= mass(1:6) * ( d1_3*V_jump(1:6) + d2_3*V_jump(7:12) + 1.d0*V_jump(13:18) + d4_3*V_jump(19:24) + &
                                            d2_3*V_jump(25:30) )
             DF_gamma(25:30)= mass(1:6) * ( d1_6*V_jump(1:6) + d1_3*V_jump(7:12) + d1_2*V_jump(13:18) + d2_3*V_jump(19:24) + &
                                            d5_6*V_jump(25:30) )
          CASE(6)
             DF_gamma(1:6)  = mass(1:6) * ( d6_7*V_jump(1:6) + d5_7*V_jump(7:12) + d4_7*V_jump(13:18) + d3_7*V_jump(19:24) + &
                                            d2_7*V_jump(25:30)+d1_7*V_jump(31:36)                                           )
             DF_gamma(7:12) = mass(1:6) * ( d5_7*V_jump(1:6) +d10_7*V_jump(7:12)+ d8_7*V_jump(13:18) + d6_7*V_jump(19:24) +  &
                                            d4_7*V_jump(25:30)+d2_7*V_jump(31:36)                                           )
             DF_gamma(13:18)= mass(1:6) * ( d4_7*V_jump(1:6) + d8_7*V_jump(7:12) +d12_7*V_jump(13:18) + d9_7*V_jump(19:24) + &
                                            d6_7*V_jump(25:30)+d3_7*V_jump(31:36)                                           )
             DF_gamma(19:24)= mass(1:6) * ( d3_7*V_jump(1:6) + d6_7*V_jump(7:12) + d9_7*V_jump(13:18) + d12_7*V_jump(19:24)+ &
                                            d8_7*V_jump(25:30)+d4_7*V_jump(31:36)                                           )
             DF_gamma(25:30)= mass(1:6) * ( d2_7*V_jump(1:6) + d4_7*V_jump(7:12) + d6_7*V_jump(13:18) + d8_7*V_jump(19:24) + &
                                           d10_7*V_jump(25:30)+d5_7*V_jump(31:36)                                           )
             DF_gamma(31:36)= mass(1:6) * ( d1_7*V_jump(1:6) + d2_7*V_jump(7:12) + d3_7*V_jump(13:18) + d4_7*V_jump(19:24) + &
                                            d5_7*V_jump(25:30)+d6_7*V_jump(31:36)                                           )
          CASE(7)
             DF_gamma(1:6)  = mass(1:6) * ( d7_8*V_jump(1:6) + d3_4*V_jump(7:12) + d5_8*V_jump(13:18) + d1_2*V_jump(19:24) + &
                                            d3_8*V_jump(25:30)+d1_4*V_jump(31:36)+ d1_8*V_jump(37:42)                       )
             DF_gamma(7:12) = mass(1:6) * ( d3_4*V_jump(1:6) + d3_2*V_jump(7:12) + d5_4*V_jump(13:18) + 1.d0*V_jump(19:24) + &
                                            d3_4*V_jump(25:30)+d1_2*V_jump(31:36)+ d1_4*V_jump(37:42)                       )
             DF_gamma(13:18)= mass(1:6) * ( d5_8*V_jump(1:6) + d5_4*V_jump(7:12) +d15_8*V_jump(13:18) + d3_2*V_jump(19:24) + &
                                            d9_8*V_jump(25:30)+d3_4*V_jump(31:36)+ d3_8*V_jump(37:42)                       )
             DF_gamma(19:24)= mass(1:6) * ( d1_2*V_jump(1:6) + 1.d0*V_jump(7:12) + d3_2*V_jump(13:18) + 2.d0*V_jump(19:24)+ &
                                            d3_2*V_jump(25:30)+1.d0*V_jump(31:36)+ d1_2*V_jump(37:42)                       )
             DF_gamma(25:30)= mass(1:6) * ( d3_8*V_jump(1:6) + d3_4*V_jump(7:12) + d9_8*V_jump(13:18) + d3_2*V_jump(19:24) + &
                                           d15_8*V_jump(25:30)+d5_4*V_jump(31:36)+ d5_8*V_jump(37:42)                       )
             DF_gamma(31:36)= mass(1:6) * ( d1_4*V_jump(1:6) + d1_2*V_jump(7:12) + d3_4*V_jump(13:18) + 1.d0*V_jump(19:24) + &
                                            d5_4*V_jump(25:30)+d3_2*V_jump(31:36)+ d3_4*V_jump(37:42)                       )
             DF_gamma(37:42)= mass(1:6) * ( d1_8*V_jump(1:6) + d1_4*V_jump(7:12) + d3_8*V_jump(13:18) + d1_2*V_jump(19:24) + &
                                            d5_8*V_jump(25:30)+d3_4*V_jump(31:36)+ d7_8*V_jump(37:42)                       )
          END SELECT

       else
          ! au moins 8 liens : matrice bande symetrique
          matrix_type = 'sym_band'
          bw=7

          ! declaration de la matrice
          call G_declare(A, matrix_type, 6*nb_liens, .false., perm_fake, perm_fake) 

          ! definition de la largeur de bande de la matrice
          call G_settle(A, (/ 1, bw /))

          ! construction de la G_matrix (et initialisation a 0)
          call G_build(A)

          ! remplissage de la matrice

          ! dans tous les cas : partie diagonale
          do i=1, 6*nb_liens
             call G_add(A, 2.d0, i, i)
          end do
          ! si on a plusieurs liens : l'extra diagonale non nulle
          do i=7, 6*nb_liens
             call G_add(A, -1.d0, i - bw + 1, i)
          end do

          ! remplissage du tableau intermediraire utilise par la resolution
          call G_store(A)

          ! calcul du second membre du systeme : M*[|V|], dans le vecteur solution
          do i_liens=1, nb_liens
             DF_gamma(6*(i_liens - 1) + 1 : 6*i_liens) = mass(1 : 6)*V_jump(6*(i_liens - 1) + 1 : 6*i_liens)
          end do

          ! resolution du systeme : calcul de DF_gamma
          call G_solve_linear_system(A, DF_gamma, info)

          ! test paranoiaque
          if (info /= 0) then
             call FATERR(IAM, 'No solution')
          end if

          ! destruction de la matrice
          call G_free(A)
       end if


       !vv: ne pourrait-on pas le faire d'un coup avant cette boucle sur les 
       !    corps d'interface ??

       ! remise a 0 des DF_gamma pour chaque copie du corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
          ! on met a 0 le DF_gamma de la copie du corps appartenant au sdm courant
          DFg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) = 0.d0
       end do
      
       ! calcul de la resultante des DF_gamma pour chaque copie du corps
       ! pour chaque lien
       do i_liens=1, nb_liens
 
          ! on recupere les deux sous-domaines associes au lien courant
          sdm(1)=body_particip(i_liens, ibody)
          sdm(2)=body_particip(i_liens + 1, ibody)

          ! on ajoute la contribution du lien courant a la resultante des DF_gamma
          ! des deux copies du corps liees par le lien courant
          DFg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) = &
             DFg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) + &
             (-1) * signe(1) * DF_gamma(6*(i_liens - 1) + 1 : 6*i_liens)

          DFg_RBDY3_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1)) = &
             DFg_RBDY3_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1)) + &
             (-1) * signe(2) * DF_gamma(6*(i_liens - 1) + 1 : 6*i_liens)
       end do

       ! liberation de l'espace memoire occupe par les tableaux :
       !   * la table associant a une paire (indice de lien, sous-domaine du lien), le numero de RBDY3 corespondant
       deallocate(part_sdm)
       !   * DF_gamma
       deallocate(DF_gamma)
       !   * [| V |]
       deallocate(V_jump)
    end do

    if (compteur /= nb_liens_interf) then
       call FATERR(IAM, "compteur /= nb_liens_interf")
    end if
                  
 end subroutine compute_DF_gamma
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_DFg

    implicit none

    integer            :: ibody, isdm, j
    integer            :: i_part_sdm
    real(kind=8), dimension(6) :: DF_gamma


    ! TEST TEST TEST
    call start_utimer(id_prep_scatter_DFg)
 
    if ( rang == 0 ) then
       do isdm=1, Nsdm
          do ibody = 1, nb_RBDY3_interf_sdm(isdm)
             DFg_interf4all(vect_shift(isdm) + 6*(ibody-1)+1 : vect_shift(isdm) + 6*ibody) = &
              DFg_RBDY3_interf_sdm(isdm)%particule(:, ibody)
          end do
       end do
       !write(slave_io,*) "-------------scatter_DFg--------------"
       !write(slave_io,*) "size(DFg_interf4all)=", size(DFg_interf4all)
    end if


    call stop_utimer(id_prep_scatter_DFg)

    call start_utimer(id_scatter_DFg)
    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(DFg_interf4all, vect_nb_send_host, vect_shift, MPI_DOUBLE_PRECISION, &
       DFg_RBDY3_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    ! paranoid test
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call stop_utimer(id_scatter_DFg)

    ! ecriture des interactions recues dans le fichier attache au processus courant
    !write(slave_io, *) "forces grains d'interface recues :"
    !do j=1, nb_RBDY3_interf_slave
    !   write(slave_io, *) 'j=', j, ' : ', DFg_RBDY3_interf_slave(6*(j - 1) + 1:6*j)
    !end do


    call start_utimer(id_post_scatter_DFg)
    do i_part_sdm=1,nb_RBDY3_interf_slave
       ibody=liste_RBDY3_interf_slave(i_part_sdm)
       ! On stocke le DF_gamma qui nous intéresse pour ce corps
       ! Après avoir divisé par le pas de temps pour convertir
       ! l'impulsion calculée en force moyenne sur le pas de temps
       DF_gamma=(1.D0/H)*DFg_RBDY3_interf_slave(6 * (i_part_sdm - 1) + 1 : 6*i_part_sdm)
       call put_vector_RBDY3('Fext_',ibody,DF_gamma,6)
    end do

    ! Mise a jour des F_gamma pour chaque copie du corps
    Fg_RBDY3_interf_slave = Fg_RBDY3_interf_slave + DFg_RBDY3_interf_slave
    call stop_utimer(id_post_scatter_DFg)

 end subroutine scatter_DFg
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine comp_Vfree_list_RBDY3_in_DDM
    implicit none
    integer            :: ibody
    integer            :: i_part_sdm

    do i_part_sdm=1,nb_RBDY3_interf_slave
       ibody=liste_RBDY3_interf_slave(i_part_sdm)
       call comp_free_vlocy_one_RBDY3(ibody)
    end do

 end subroutine comp_Vfree_list_RBDY3_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
!am: calcul de la convergence avec reduction des normes calculees dans chaque sdm
 subroutine check_convergence_in_DDM(is_converged)
 
    implicit none
 
    ! variable de sortie
    ! pour statuer si on a converge, variable connue par tous les sdm mais stockee par le processus 0
    logical, intent(out) :: is_converged
 
    ! variables locales
 
    ! nombre de contacts actifs (Nactif: par sdm)
    integer :: Nactif
    ! vecteur des sommes a calculer, pour statuer de la convergence:
    !   * Sums(1) / Sums4all(1): SumWRWR
    !   * Sums(2) / Sums4all(2): SumDVDV
    !   * Sums(3) / Sums4all(3): SumWRR
    !   * Sums(4) / Sums4all(4): SumDVDVRR
    !   * Sums(5) / Sums4all(5): SumDVoR
    real(kind=8), dimension(5) :: Sums, Sums4all
    ! vecteurs des aximas a calculer, pour statuer de la convergence
    !   * Maxima(1) / Maxima4all(1): MaxDVDV
    !   * Maxima(2) / Maxima4all(2): MaxDVDVRR 
    real(kind=8), dimension(2) :: Maxima, Maxima4all
 
    ! tolerance du NLGS
    real(kind=8) :: tol
 
    ! pour recuperer le resulat de prep_check_nlgs
    integer :: iconv
    ! pour statuer si NLGS a converge, decision prise par le processus 0
    logical :: nlgs_converged
    ! pour statuer si le probleme d'interface a converge, decision prise par le processus 0
    logical :: intrf_converged

    ! initialisation du statut de convergence
    is_converged = .false.
    intrf_converged = .false.

    ! on prepare la verification de la convergence, dans le sdm courant
    call prep_check_nlgs(iconv)
 
    !print*, 'rang ', rang, ' : iconv=', iconv
 
    ! s'il n'y a aucun contact
    if (iconv == 0) then
       ! le nombre de contact actif est nul, par definition
       Nactif = 0
    ! sinon,
    else
       ! on calcule les nombre de contacts actifs, les sommes et les max necessaires pour la verif de la convergence 
       call solve_nlgs(2)
 
       ! on recupere le nombre de contact actif dans le sdm courant
       call get_error(Nactif_=Nactif) 
    end if
 
    !print*, 'rang ', rang, ' : Nactif=', Nactif
 
    ! on calcule le nombre total de contacts actifs et on l'envoie a tous les sdm
    call MPI_ALLREDUCE(Nactif, Nactif4all, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
 
    !print*, 'rang ', rang, ' : Nactif4all=', Nactif4all
 
    ! si le nombre total de contact actif est nul, on a converge
    if (Nactif4all == 0) then
       is_converged = .true.
       return
    end if
 
    ! ici, on sait qu'au moins un sdm a au moins un contact actif
 
    ! si le sdm courant n'a aucun contact actif
    if (Nactif == 0) then 
       ! les sommes et les maximas sont nuls, par definition
       Sums = 0.d0
       Maxima = 0.d0
    ! sinon,
    else
       ! on recupere les sommes et les maximas pour le sdm courant, ainsi que la tolerance utilisee 
       call get_error(SumWRWR_=Sums(1), SumDVDV_=Sums(2), SumWRR_=Sums(3), SumDVDVRR_=Sums(4), SumDVoR_=Sums(5), &
                      MaxDVDV_=Maxima(1), MaxDVDVRR_=Maxima(2), tol_=tol)
    end if
 
    !print*, 'rang ', rang, ' : Sums=', Sums
    !print*, 'rang ', rang, ' : Maxima=', Maxima
 
    ! on calcule :
    !    * pour chaque somme (SumWRWR, ...) la somme des sommes collectees sur chaque sdm et on l'envoie sur le processus 0
    call MPI_REDUCE(Sums, Sums4all, 5, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    !    * pour chaque max (MaxDVDV, MaxDVDVRR) le max des max collectes sur chaque sdm et on l'envoie sur le processus 0
    call MPI_REDUCE(Maxima, Maxima4all, 2, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
 
    ! on teste si le probleme d'interface a converge
    if (Nsdm > 1) then
       ! N.B.: cette fonction doit etre execute par tout le monde (maitre et esclaves)
       call compute_egluing(tol, intrf_converged)
    else
       intrf_converged = .true.
    end if

    !print*, 'INTRF a converge=', intrf_converged

    if (rang == 0) then
       !print*, 'Sums4all=', Sums4all
       !print*, 'Maxima4all=', Maxima4all
 
       ! on calcule les quantites necessaires pour statuer de la convergence sur le maitre
       call compute_convergence_norms_nlgs(Nactif4all, Sums4all(1), Sums4all(2), Maxima4all(1), &
                                           Sums4all(3), Sums4all(4), Maxima4all(2), Sums4all(5), tol, &
                                           QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR) 

       ! on teste la convergence du NLGS 
       call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, nlgs_converged)
 
       !print*, 'NLGS a converge=', nlgs_converged
 
       ! on a converge si le NLGS et le probleme d'interface ont converge
       is_converged = nlgs_converged .and. intrf_converged

       if (.not. is_converged) then
          if ( (.not. nlgs_converged) ) then
             prevailing_criterion = 1
          elseif ( (.not. intrf_converged) ) then 
             prevailing_criterion = 2
          end if
       end if

    end if
    
    ! le processus 0 propage le statut de convergence a tous les sdm
    call MPI_BCAST(is_converged, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, code)
 
 end subroutine check_convergence_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_egluing(TOL, is_converged) 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! saut_V_interf_glob
    ! argument "explicite"
    real(kind=8), intent(in)         :: TOL          ! Tolérance du critère de convergence

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    logical, intent(out)             :: is_converged ! Si la valeur d'entrée est .true. et 
                                                     ! egluing < TOL, on retourne is_converged à .true. 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine
    character(len=28) :: clout ! pour calculer le nom du fichier

    if (rang /= 0) return

    egluing=0.d0
    if (nb_liens_interf > 0) then

      ! Calcul de l'erreur globale de recollement
      egluing=sqrt(dot_product(saut_V_interf_glob(1, :), saut_V_interf_glob(1, :)) + &
                   dot_product(saut_V_interf_glob(2, :), saut_V_interf_glob(2, :)) + &
                   dot_product(saut_V_interf_glob(3, :), saut_V_interf_glob(3, :))) / &
                   (nb_liens_interf)

      is_converged = (egluing < TOL)
    else
      is_converged = .true.
    end if

 end subroutine compute_egluing
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_DF_gamma 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! Ce truc ci dessous va changer de forme
    ! type(T_ligne_r), dimension(Nsdm)   :: DFg_RBDY3_interf_sdm(isdm)
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody
    integer            :: i_part_sdm
    integer            :: i_std_bdy
    real(kind=8), dimension(6) :: DF_gamma

    do isdm=1, Nsdm
       do i_part_sdm=1, nb_RBDY3_interf_sdm(isdm)
          ! On récupère le numéro du RBDY3 dans le sous-domaine 
          ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_part_sdm)
          ! Recallage des indice pour le cas séquaentiel multidomaine
          i_std_bdy=(isdm - 1)*nbody + ibody
          ! On stocke le DF_gamma qui nous intéresse pour ce corps
          ! Après avoir divisé par le pas de temps pour convertir
          ! l'impulsion calculée en force moyenne sur le pas de temps
          DF_gamma=(1.D0/H)*DFg_RBDY3_interf_sdm(isdm)%particule(:, i_part_sdm)
          call put_vector_RBDY3('Fext_', i_std_bdy, DF_gamma, 6)
          call comp_free_vlocy_one_RBDY3(i_std_bdy)
       end do
    end do

 end subroutine set_DF_gamma
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_F_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! Ce truc ci dessous va changer de forme
    ! type(T_ligne_r), dimension(Nsdm)   :: Fg_RBDY3_interf_sdm(isdm)
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer                    :: isdm
    integer, dimension(1)      :: tmp
    integer                    :: ibody
    integer                    :: i_part_sdm
    integer                    :: old_i_part_sdm
    real(kind=8), dimension(6) :: F_gamma

    if (.not. repartition_just_made) then
       do i_part_sdm=1, nb_RBDY3_interf_slave
          ! On récupère le numéro du RBDY3 dans le sous-domaine 
          ibody=liste_RBDY3_interf_slave(i_part_sdm)
          ! On stocke le F_gamma qui nous intéresse pour ce corps
          ! Après avoir divisé par le pas de temps pour convertir
          ! l'impulsion calculée en force moyenne sur le pas de temps
          F_gamma=(1.D0/H)*Fg_RBDY3_interf_slave(6*(i_part_sdm-1)+1:6*i_part_sdm)
          call put_vector_RBDY3('Fext_', ibody, F_gamma, 6)
       end do
    else ! On vient donc de faire une nouvelle répartition en sous-domaines
       do i_part_sdm=1, nb_RBDY3_interf_slave
          ! On récupère le numéro du RBDY3 dans le sous-domaine 
          ibody=liste_RBDY3_interf_slave(i_part_sdm)

          if (any(liste_RBDY3_interf_slave_old(:) == ibody)) then
             tmp=maxloc(liste_RBDY3_interf_slave_old(:), &
                                   liste_RBDY3_interf_slave_old(:) == ibody)
             old_i_part_sdm=tmp(1)
             Fg_RBDY3_interf_slave(6*(i_part_sdm-1)+1:6*i_part_sdm) = &
                Fg_RBDY3_interf_slave_old(6*(old_i_part_sdm-1)+1:6*old_i_part_sdm) 
             ! On stocke le F_gamma qui nous intéresse pour ce corps
             ! Après avoir divisé par le pas de temps pour convertir
             ! l'impulsion calculée en force moyenne sur le pas de temps
             F_gamma=(1.D0/H)*Fg_RBDY3_interf_slave_old(6*(old_i_part_sdm-1)+1:6*old_i_part_sdm) 
             call put_vector_RBDY3('Fext_', ibody, F_gamma, 6)
          end if
       end do
    end if

    repartition_just_made=.false. 

 end subroutine set_F_gamma
!----------------------------------------------------------------

!am: fonction qui calcule, dans Vaux, les vitesses obtenues a partir des forces de contact, stokees dans Iaux, pour les grains d'interface
 subroutine comp_V_list_RBDY3_in_DDM 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: ibody
    integer            :: i_part_sdm
   
    do i_part_sdm=1, nb_RBDY3_interf_slave
       ! On récupère le numéro du RBDY3 dans le sous-domaine 
       ibody=liste_RBDY3_interf_slave(i_part_sdm)

       ! calcul de Vaux = Vfree + M^-1 Iaux (et CL appliquees a Vaux)
       call comp_vlocy(ibody, iVaux_e_invM_t_Iaux_p_Vfree)
    end do

 end subroutine comp_V_list_RBDY3_in_DDM
!----------------------------------------------------------------


! Rq: la gestion des doublons (particules d'interface) se fait
! suivant la règle arbitraire suivante : les déplacements, vitesses
! et orientations du repère d'inertie sont celles du sous-domaine
! de plus haut rang (correspondance rang/sous-domaine).
!----------------------------------------------------------------
 subroutine gather_X_V_begin
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY3_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, ibdy, imulti
    integer            :: irecv
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localFrame_tmp

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          vect_nb_recv_host(isdm) = count(mask_particip(:,isdm) .eqv..true.)
          !write(slave_io, *) "vect_nb_recv_host(", isdm,") = ",vect_nb_recv_host(isdm) 
       end do

       !write(slave_io, *) "vect_nb_recv_host = ",vect_nb_recv_host
       !write(slave_io, *) "6*vect_nb_recv_host = ",6*vect_nb_recv_host

       vect_shift=0
       !write(slave_io, *) "vect_shift(1) = ",vect_shift(1) 

       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_recv_host(isdm-1)
          !write(slave_io, *) "vect_shift(", isdm,") = ",vect_shift(isdm) 
       end do

       !write(slave_io, *) "vect_shift = ",vect_shift 
       !write(slave_io, *) "6*vect_shift = ",6*vect_shift

       nb_recv_host=sum(vect_nb_recv_host)
       !write(slave_io, *) "nb_recv_host = ",nb_recv_host
       if (allocated(Ibeg4all)) deallocate(Ibeg4all)
       allocate(Ibeg4all(nb_recv_host))
       Ibeg4all=0
       if (allocated(Xbeg4all)) deallocate(Xbeg4all)
       allocate(Xbeg4all(6*nb_recv_host))
       Xbeg4all=0.D0
       if (allocated(Vbeg4all)) deallocate(Vbeg4all)
       allocate(Vbeg4all(6*nb_recv_host))
       Vbeg4all=0.D0
       if (allocated(localFrameTT4all)) deallocate(localFrameTT4all)
       allocate(localFrameTT4all(9*nb_recv_host))
       localFrameTT4all=0.D0
    end if

    ! Nombre de particules concernées par l'envoi
    nb_send_slave = count(mask_in_slave .eqv. .true.)
    !write(slave_io, *) "nb_send_slave = ",nb_send_slave

    if (allocated(Ibeg_slave)) deallocate(Ibeg_slave)
    allocate(Ibeg_slave(nb_send_slave))
    Ibeg_slave=0

    if (allocated(Xbeg_slave)) deallocate(Xbeg_slave)
    allocate(Xbeg_slave(6*nb_send_slave))
    Xbeg_slave=0.d0


    if (allocated(Vbeg_slave)) deallocate(Vbeg_slave)
    allocate(Vbeg_slave(6*nb_send_slave))
    Vbeg_slave=0.d0
    
    if (allocated(localFrameTT_slave)) deallocate(localFrameTT_slave)
    allocate(localFrameTT_slave(9*nb_send_slave))
    localFrameTT_slave=0.d0
    
    compteur=0
    visible=.false.

    do ibody=1,nbody

       visible=mask_in_slave(ibody)
       if (.not. visible) cycle
       compteur=compteur+1

       Ibeg_slave( compteur ) = ibody

       call get_vector_RBDY3('Xbeg_',ibody,X_tmp, 6)
       Xbeg_slave( 6*(compteur-1)+1 : 6*compteur)=X_tmp      

       call get_vector_RBDY3('Vbeg_',ibody,V_tmp, 6)
       Vbeg_slave( 6*(compteur-1)+1 : 6*compteur)=V_tmp      

       call get_matrix_RBDY3('IFTT_', ibody, localFrame_tmp, 3)
       ! N.B.: pour les echanges, la matrice representant le repere principal d'inertie
       !       dans la configuration de detetction, est stockee en utilisant les notations de Voigt
       localFrameTT_slave( 9*(compteur-1) + 1 : 9*compteur) = pack(localFrame_tmp, .true.)

    end do

    ! Récuperation des indices des corps à gatheriser
    CALL MPI_GATHERV(Ibeg_slave, nb_send_slave, MPI_INTEGER, &
       Ibeg4all, vect_nb_recv_host, vect_shift, MPI_INTEGER, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des positions des corps à gatheriser
    CALL MPI_GATHERV(Xbeg_slave, 6*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Xbeg4all,6*vect_nb_recv_host,6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des vitesses des corps à gatheriser
    CALL MPI_GATHERV(Vbeg_slave, 6*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Vbeg4all,6*vect_nb_recv_host,6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des reperes principaux d'inertie dans la configuration de detection
    CALL MPI_GATHERV(localFrameTT_slave, 9*nb_send_slave, MPI_DOUBLE_PRECISION, &
       localFrameTT4all,9*vect_nb_recv_host,9*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if ( rang /= 0 ) return
    !vect_nb_recv_host=vect_nb_recv_host/9
    !vect_shift=vect_shift/9

    ! Pour le comportement de la boucle ci-dessous,
    ! voir la remarque en début de routine.
    do isdm=1,Nsdm
       do irecv=1,vect_nb_recv_host(isdm)
          ibdy = Ibeg4all( vect_shift(isdm)+irecv  )
          X_tmp = Xbeg4all( 6*vect_shift(isdm) + 6*(irecv-1)+1 : 6*vect_shift(isdm) + 6*irecv )
          call put_vector_RBDY3('Xbeg_',ibdy,X_tmp, 6)
          V_tmp = Vbeg4all( 6*vect_shift(isdm) + 6*(irecv-1)+1 : 6*vect_shift(isdm) + 6*irecv )
          call put_vector_RBDY3('Vbeg_',ibdy,V_tmp, 6)

          localFrame_tmp=reshape(localFrameTT4all(9*vect_shift(isdm) + 9*(irecv-1)+1 : 9*vect_shift(isdm) + 9*irecv), (/3,3/) )

          call put_matrix_RBDY3('IFTT_', ibdy, localFrame_tmp, 3)

       end do 
    end do

 end subroutine gather_X_V_begin
!----------------------------------------------------------------

!----------------------------------------------------------------
!am: fonction qui envoie dans la premiere tranche les deplacements,
! et vitesses a la fin du pas de temps + l'orientation du repere principal
! d'inertie dans la configuration a la fin du pas => calcul des coordonnees dans
! la configuration de la fin du pas de temps.

! Rq: la gestion des doublons (particules d'interface) se fait
! suivant la règle arbitraire suivante : les déplacements, vitesses
! et orientations du repère d'inertie sont celles du sous-domaine
! de plus haut rang (correspondance rang/sous-domaine).
 subroutine gather_X_V
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, ibdy
    integer            :: irecv
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localFrame_tmp

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          vect_nb_recv_host(isdm) = count(mask_particip(:,isdm) .eqv..true.)
       end do

       vect_shift=0

       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_recv_host(isdm-1)
       end do

       nb_recv_host=sum(vect_nb_recv_host)

       if (allocated(I4all)) deallocate(I4all)
       allocate(I4all(nb_recv_host))
       I4all=0
       if (allocated(X4all)) deallocate(X4all)
       allocate(X4all(6*nb_recv_host))
       X4all=0.D0
       if (allocated(V4all)) deallocate(V4all)
       allocate(V4all(6*nb_recv_host))
       V4all=0.D0
       if (allocated(localFrame4all)) deallocate(localFrame4all)
       allocate(localFrame4all(9*nb_recv_host))
       localFrame4all=0.D0
    end if

    ! Nombre de particules concernées par l'envoi
    nb_send_slave = count(mask_in_slave .eqv. .true.)

    if (allocated(I_slave)) deallocate(I_slave)
    allocate(I_slave(nb_send_slave))
    I_slave=0

    if (allocated(X_slave)) deallocate(X_slave)
    allocate(X_slave(6*nb_send_slave))
    X_slave=0.d0

    if (allocated(V_slave)) deallocate(V_slave)
    allocate(V_slave(6*nb_send_slave))
    V_slave=0.d0
    
    if (allocated(localFrame_slave)) deallocate(localFrame_slave)
    allocate(localFrame_slave(9*nb_send_slave))
    localFrame_slave=0.d0
    
    compteur=0
    visible=.false.

    do ibody=1,nbody

       visible=mask_in_slave(ibody)
       if (.not. visible) cycle
       compteur=compteur+1

       I_slave( compteur ) = ibody

       call get_vector_RBDY3('X____',ibody,X_tmp, 6)
       X_slave( 6*(compteur-1)+1 : 6*compteur)=X_tmp      

       call get_vector_RBDY3('V____',ibody,V_tmp, 6)
       V_slave( 6*(compteur-1)+1 : 6*compteur)=V_tmp      

       call get_matrix_RBDY3('IF___', ibody, localFrame_tmp, 3)
       ! N.B.: pour les echanges, la matrice representant le repere principal d'inertie
       !       dans la configuration de detetction, est stockee en utilisant les notations de Voigt
       localFrame_slave( 9*(compteur-1) + 1 : 9*compteur) = pack(localFrame_tmp, .true.)

    end do

    ! Récuperation des indices des corps à gatheriser
    CALL MPI_GATHERV(I_slave, nb_send_slave, MPI_INTEGER, &
       I4all, vect_nb_recv_host, vect_shift, MPI_INTEGER, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des positions des corps à gatheriser
    CALL MPI_GATHERV(X_slave, 6*nb_send_slave, MPI_DOUBLE_PRECISION, &
       X4all,6*vect_nb_recv_host,6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des vitesses des corps à gatheriser
    CALL MPI_GATHERV(V_slave, 6*nb_send_slave, MPI_DOUBLE_PRECISION, &
       V4all,6*vect_nb_recv_host,6*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des reperes principaux d'inertie dans la configuration de detection
    CALL MPI_GATHERV(localFrame_slave, 9*nb_send_slave, MPI_DOUBLE_PRECISION, &
       localFrame4all,9*vect_nb_recv_host,9*vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if ( rang /= 0 ) return

    do isdm=1,Nsdm
       do irecv=1,vect_nb_recv_host(isdm)

          ibdy = I4all( vect_shift(isdm)+irecv  )

          X_tmp = X4all( 6*vect_shift(isdm) + 6*(irecv-1)+1 : 6*vect_shift(isdm) + 6*irecv )
          call put_vector_RBDY3('X____',ibdy,X_tmp, 6)

          V_tmp = V4all( 6*vect_shift(isdm) + 6*(irecv-1)+1 : 6*vect_shift(isdm) + 6*irecv )
          call put_vector_RBDY3('V____',ibdy,V_tmp, 6)

          localFrame_tmp=reshape(localFrame4all(9*vect_shift(isdm) + 9*(irecv-1)+1 : 9*vect_shift(isdm) + 9*irecv), (/3,3/) )
          call put_matrix_RBDY3('IF___', ibdy, localFrame_tmp, 3)

       end do 
    end do

 end subroutine gather_X_V
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine print_iter(iterID,iter)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    integer, intent(in)  :: iterID,iter
 
    if ( rang /= 0 ) return

    if ( iterID == 0 ) then
       print *, "nb iterations effectuees=", iter
    elseif ( iterID == 1 ) then
       print *, "nb iterations totales effectuees=", iter
    end if

 end subroutine print_iter
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine print_step(step)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    integer, intent(in)  :: step
 
    if ( rang /= 0 ) return

    print *, "Nstep=", step

 end subroutine print_step
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine write_MPI_INFO_I(message,vecteur,indices,taille)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! argument "explicite"
    integer                   , intent(in) :: taille
    integer, dimension(taille), intent(in) :: vecteur
    integer, dimension(taille), intent(in) :: indices
    character(len=20)         , intent(in) :: message
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: nfich         ! numéro de io
    integer            :: lc,i
    integer, save      :: Nprint=0
    character(len=13)  :: clout ! pour calculer le nom du fichier
    character(len=4)   :: numfic

    WRITE(numfic,'(I4)') rang
    numfic = ADJUSTL(numfic)

    !      1234567890123 
    clout='MPI_INFO.xxxx'
    write(clout(10:13), '(A4)') numfic
   
    nfich=slave_io

    lc = LEN_TRIM(clout)
    open(unit=nfich, file=TRIM(clout(1:lc)), form='formatted', &
         action='write', position='append')
     
    write(nfich, '(A20)') message
    do i=1,taille
       write(nfich, '(I6,1X,I6)') indices(i), vecteur(i)
    end do
    close(nfich)

 end subroutine write_MPI_INFO_I
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine write_MPI_INFO_L(message,vecteur,indices,taille)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! argument "explicite"
    integer                   , intent(in) :: taille
    logical, dimension(taille), intent(in) :: vecteur
    integer, dimension(taille), intent(in) :: indices
    character(len=20)         , intent(in) :: message
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: nfich         ! numéro de io
    integer            :: lc,i
    integer, save      :: Nprint=0
    character(len=13)  :: clout ! pour calculer le nom du fichier
    character(len=4)   :: numfic

    WRITE(numfic,'(I4)') rang
    numfic = ADJUSTL(numfic)

    !      1234567890123 
    clout='MPI_INFO.xxxx'
    write(clout(10:13), '(A4)') numfic
   
    nfich=slave_io

    lc = LEN_TRIM(clout)
    open(unit=nfich, file=TRIM(clout(1:lc)), form='formatted', &
         action='write', position='append')
     
    write(nfich, '(A20)') message
    do i=1,taille
       write(nfich, '(I6,1X,L1)') indices(i), vecteur(i)
    end do
    close(nfich)

 end subroutine write_MPI_INFO_L
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine write_MPI_INFO_R(message,vecteur,indices,taille)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! argument "explicite"
    integer                        , intent(in) :: taille
    real(kind=8), dimension(taille), intent(in) :: vecteur
    integer     , dimension(taille), intent(in) :: indices
    character(len=20)              , intent(in) :: message
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: nfich         ! numéro de io
    integer            :: lc,i
    integer, save      :: Nprint=0
    character(len=13)  :: clout ! pour calculer le nom du fichier
    character(len=4)   :: numfic

    WRITE(numfic,'(I4)') rang
    numfic = ADJUSTL(numfic)

    !      1234567890123 
    clout='MPI_INFO.xxxx'
    write(clout(10:13), '(A4)') numfic
   
    nfich=slave_io

    lc = LEN_TRIM(clout)
    open(unit=nfich, file=TRIM(clout(1:lc)), form='formatted', &
         action='write', position='append')
     
    write(nfich, '(A20)') message
    do i=1,taille
       write(nfich, '(I6,1X,E12.6)') indices(i), vecteur(i)
    end do
    close(nfich)

 end subroutine write_MPI_INFO_R
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_visibility_4all_in_DDM(proc)
 
    implicit none
   
    integer, intent(in), optional :: proc
    integer                       :: i

    if ( .not. present(proc) ) then
       call set_visibility_4all_RBDY3(mask_in_slave,nbody)
    else
       if ( rang == proc ) then
          call set_visibility_4all_RBDY3((/ (.true., i=1, nbody) /),nbody)
       end if
    end if

 end subroutine set_visibility_4all_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_skip_display_cluster(value)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    logical, intent(in)  :: value
 
    skip_display_cluster = value

 end subroutine set_skip_display_cluster
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine init_postpro_in_DDM(solv_info_, viol_evol_, triax_compac_, &
                                 sdm_info_, mean_sdm_info_,             &
                                 sample_info_, mean_sample_info_)
  
    implicit none

    integer           :: sdm
    logical           :: solv_info_, viol_evol_, triax_compac_, sdm_info_, &
                         mean_sdm_info_, sample_info_, mean_sample_info_
                             !12345678901234567890123456789012345
    character(len=35) :: IAM="DDM_MPI_DECENT::init_postpro_in_DDM"

    solv_info=solv_info_
    viol_evol=viol_evol_
    triax_compac=triax_compac_
    sdm_info=sdm_info_
    mean_sdm_info=mean_sdm_info_
    sample_info=sample_info_
    mean_sample_info=mean_sample_info_

    if ( .not. sdm_info ) then
       if ( mean_sdm_info ) call faterr(IAM, "mean_sdm_info le peut pas etre &
                                         &actif si sdm_info ne l'est pas !!")
       if ( sample_info ) call faterr(IAM, "sample_info le peut pas etre &
                                         &actif si sdm_info ne l'est pas !!")
       if ( mean_sample_info ) call faterr(IAM, "mean_sample_info le peut pas etre &
                                         &actif si sdm_info ne l'est pas !!")
    end if

    if ( triax_compac ) then

       if (rang == 0 ) then
          if ( ntact_POLYR /=0 ) call faterr(IAM, "TRIAXIAL COMPACITY ONLY for SPHERs")
          nfich_triax_compac = get_io_unit()
                               !123456789012345678901234567890
          clout_triax_compac = "POSTPRO/TRIAXIAL_COMPACITY.DAT"

          OPEN(unit=nfich_triax_compac,file=clout_triax_compac,STATUS='REPLACE') 
          CLOSE(unit=nfich_triax_compac) 
       end if

       call triaxial_compacity_in_DDM(0)
    end if

    if (sdm_info) then
       sdm = rang + 1
       nfich_sdm_inf = get_io_unit()
       !              1234567890123456789012345678901234
       clout_sdm_inf='POSTPRO/SDM_INFORMATIONS.xxxxx.DAT'
       write(clout_sdm_inf(26:30), '(I5.5)') sdm
       OPEN(unit=nfich_sdm_inf,file=clout_sdm_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_sdm_inf) 
    end if

    if (mean_sdm_info) then
       sdm = rang + 1
       nfich_mean_sdm_inf = get_io_unit()
       !                   123456789012345678901234567890123456789
       clout_mean_sdm_inf='POSTPRO/MEAN_SDM_INFORMATIONS.xxxxx.DAT'
       write(clout_mean_sdm_inf(31:35), '(I5.5)') sdm
       OPEN(unit=nfich_mean_sdm_inf,file=clout_mean_sdm_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_mean_sdm_inf) 
       
       mean_nb_RBDY3_slave             = 0.d0
       mean_nb_RBDY3_interf_slave      = 0.d0
       mean_nb_interactions_slave      = 0.d0
       mean_nb_fine_interactions_slave = 0.d0
       mean_nb_INTRF                   = 0.d0
       mean_nb_adjac_slave             = 0.d0

    end if

    if ( rang /= 0 ) return
   
    if ( solv_info ) then
       nfich_solv_inf = get_io_unit()
                        !1234567890123456789012345678901
       clout_solv_inf = "POSTPRO/SOLVER_INFORMATIONS.DAT"

       OPEN(unit=nfich_solv_inf,file=clout_solv_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_solv_inf) 
    end if

    if ( viol_evol ) then
       nfich_viol_evol = get_io_unit()
                         !1234567890123456789012345678901
       clout_viol_evol = "POSTPRO/VIOLATION_EVOLUTION.DAT"

       OPEN(unit=nfich_viol_evol,file=clout_viol_evol,STATUS='REPLACE') 
       CLOSE(unit=nfich_viol_evol) 
    end if

    if (sample_info) then
       nfich_sample_inf = get_io_unit()
       !                 1234567890123456789012345678901
       clout_sample_inf='POSTPRO/SAMPLE_INFORMATIONS.DAT'
       OPEN(unit=nfich_sample_inf,file=clout_sample_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_sample_inf) 
    end if

    if (mean_sample_info) then
       !                      123456789012345678901234567890123456
       clout_mean_sample_inf='POSTPRO/MEAN_SAMPLE_INFORMATIONS.DAT'
       OPEN(unit=nfich_mean_sample_inf,file=clout_mean_sample_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_mean_sample_inf) 

       mean_nb_RBDY3_interf4all      = 0.d0
       mean_nb_interactions4all      = 0.d0
       mean_nb_fine_interactions4all = 0.d0
       mean_nb_liens_interf4all      = 0.d0
       mean_nb_INTRF4all             = 0.d0
       mean_nb_adjac4all             = 0.d0
       mean_nb_migrations4all        = 0.d0
    end if

  end subroutine init_postpro_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine postpro_during_in_DDM(iter)
  
    implicit none
  
    integer, intent(in) :: iter
                             !1234567890123456789012345678901234567
    character(len=37) :: IAM="DDM_MPI_DECENT::postpro_during_in_DDM"

    if ( solv_info ) then
       call solver_informations_in_DDM(iter)
    end if

    if ( sdm_info ) then
       call compute_nb_RBDY3_slave_in_DDM
       nb_fine_interactions_slave = 0
       nb_adjac_slave = get_nb_adjac_nlgs_3D()
       if (nb_interactions_slave_SPSPx > 0) then
          nb_fine_interactions_slave = get_nb_SPSPx(i_real_tactor)
       end if
       if (nb_interactions_slave_PRPRx > 0) then
          nb_fine_interactions_slave = get_nb_PRPRx(i_real_tactor)
       end if
       call write_SDM_INFORMATIONS

       if ( sample_info ) then
          call sample_informations_in_DDM
       end if 
       mean_nb_RBDY3_slave = mean_nb_RBDY3_slave &
                                     + real(nb_RBDY3_slave)
       mean_nb_RBDY3_interf_slave = mean_nb_RBDY3_interf_slave &
                                     + real(nb_RBDY3_interf_slave)
       mean_nb_interactions_slave = mean_nb_interactions_slave &
                                     + real(nb_interactions_slave)
       mean_nb_fine_interactions_slave = mean_nb_fine_interactions_slave &
                                     + real(nb_fine_interactions_slave)
       mean_nb_INTRF = mean_nb_INTRF + real(nb_INTRF)
       mean_nb_adjac_slave = mean_nb_adjac_slave + real(nb_adjac_slave)
    end if

    if ( viol_evol ) then
       call violation_evolution_in_DDM
    end if 

    if ( triax_compac ) then
       call triaxial_compacity_in_DDM(1)
    end if 


  end subroutine postpro_during_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine postpro_last_in_DDM(nb_steps,freq_postpro)
  
    implicit none
  
    integer, intent(in) :: nb_steps, freq_postpro
    real(kind=8)        :: nb_postpro
                             !1234567890123456789012345678901234567
    character(len=37) :: IAM="DDM_MPI_DECENT::postpro_last_in_DDM"

    nb_postpro = real(floor(real(nb_steps) / real(freq_postpro)))

    if ( mean_sdm_info ) then

       mean_nb_RBDY3_slave = mean_nb_RBDY3_slave &
                                    / nb_postpro
       mean_nb_RBDY3_interf_slave = mean_nb_RBDY3_interf_slave &
                                    / nb_postpro
       mean_nb_interactions_slave = mean_nb_interactions_slave &
                                    / nb_postpro
       mean_nb_fine_interactions_slave = mean_nb_fine_interactions_slave &
                                    / nb_postpro
       mean_nb_INTRF = mean_nb_INTRF / nb_postpro
       mean_nb_adjac_slave = mean_nb_adjac_slave / nb_postpro

       call write_MEAN_SDM_INFORMATIONS

    end if

    if ( mean_sample_info ) then

       mean_nb_RBDY3_interf4all = mean_nb_RBDY3_interf4all &
                                    / nb_postpro
       mean_nb_interactions4all = mean_nb_interactions4all &
                                    / nb_postpro
       mean_nb_fine_interactions4all = mean_nb_fine_interactions4all &
                                    / nb_postpro
       mean_nb_migrations4all = mean_nb_migrations4all / nb_postpro
       mean_nb_INTRF4all = mean_nb_INTRF4all / nb_postpro
       mean_nb_adjac4all = mean_nb_adjac4all / nb_postpro
       mean_nb_migrations4all = mean_nb_migrations4all / nb_postpro

       call write_MEAN_SAMPLE_INFORMATIONS

    end if

  end subroutine postpro_last_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine solver_informations_in_DDM(iter)
  
    implicit none
    integer, intent(in) :: iter
                             !123456789012345678901234567890123456789012
    character(len=42) :: IAM="DDM_MPI_DECENT::solver_informations_in_DDM"
  

    if ( rang /= 0 ) return

    OPEN(unit=nfich_solv_inf,file=clout_solv_inf,STATUS='OLD',POSITION='APPEND') 
    write(unit=nfich_solv_inf,fmt='(D14.7,1X,I8,4(1X,D14.7),1X,I8,I8)') TPS,iter,MeanDVoR,QuadDV, &
                                                  QuadDVR,egluing,Nactif4all,prevailing_criterion
    CLOSE(unit=nfich_solv_inf) 

    ! Remise à zéro de prevailing_criterion
    prevailing_criterion = 0

  end subroutine solver_informations_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine violation_evolution_in_DDM
  
    implicit none
                             !1234567890123456789012345678901234567890123
    character(len=43) :: IAM="DDM_MPI_DECENT::compute_violation_evolution"

    INTEGER      :: icdan
    REAL(kind=8) :: gapBegin,gap
    REAL(kind=8) :: max_gapBegin,total_gapBegin,max_gap,total_gap
    REAL(kind=8) :: max_gapBegin_4all,total_gapBegin_4all,max_gap_4all,total_gap_4all
    
    total_gapBegin = 0.D0
    max_gapBegin   = 0.D0

    total_gap = 0.D0
    max_gap   = 0.D0
    
    DO icdan=1,get_nb_SPSPx(i_real_tactor)
       CALL get_gaps(i_spspx, icdan, gapBegin, gap)

       gapBegin = MIN(0.D0,gapBegin)
       gap      = MIN(0.D0,gap)

       total_gapBegin = total_gapBegin - gapBegin     
       total_gap      = total_gap      - gap     

       max_gapBegin  =MAX(max_gapBegin,-gapBegin)
       max_gap       =MAX(max_gap,-gap)
    END DO

    DO icdan=1,get_nb_PRPRx(i_real_tactor)
       CALL get_gaps(i_prprx, icdan, gapBegin, gap)

       gapBegin = MIN(0.D0,gapBegin)
       gap      = MIN(0.D0,gap)

       total_gapBegin = total_gapBegin - gapBegin     
       total_gap      = total_gap      - gap     

       max_gapBegin  =MAX(max_gapBegin,-gapBegin)
       max_gap       =MAX(max_gap,-gap)
    END DO


    call MPI_REDUCE(total_gapBegin, total_gapBegin_4all, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call MPI_REDUCE(total_gap, total_gap_4all, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call MPI_REDUCE(max_gapBegin, max_gapBegin_4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call MPI_REDUCE(max_gap, max_gap_4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if (rang /= 0) return

    OPEN(unit=nfich_viol_evol,file=clout_viol_evol,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_viol_evol,fmt='(5(1X,D14.7))') TPS,total_gapBegin_4all/Nactif4all,total_gap_4all/Nactif4all, &
                                                    max_gapBegin_4all,max_gap_4all
    CLOSE(unit=nfich_viol_evol) 

  end subroutine violation_evolution_in_DDM
!----------------------------------------------------------------

! Que pour un échantillon de sphères avec des parois constituées de cluster de sphères !!
!----------------------------------------------------------------
  subroutine triaxial_compacity_in_DDM(info)
  
    implicit none

    integer :: i
    integer, intent(in)       :: info ! Indique si la fonction est appellée pr "init"->0 ou "during"->1
    integer     , save        :: iXmin,iXmax,iYmin,iYmax,iZmin,iZmax
    real(kind=8), save        :: volume0
    real(kind=8), save        :: width
    real(kind=8)              :: Xmin, Xmax, Ymin, Ymax, Zmin, Zmax
    real(kind=8)              :: Xmin4all, Xmax4all, Ymin4all, Ymax4all, Zmin4all, Zmax4all
    real(kind=8)              :: BoxVolume
    real(kind=8), dimension(3):: DATA
    real(kind=8), dimension(3):: coor
                             !1234567890123456789012345678901234
    character(len=43) :: IAM="DDM_MPI_DECENT::triaxial_compacity"

    SELECT CASE(info)

    CASE(0)

       ! Pas beau du tout!! on suppose que les parois sont 
       ! ajoutées en dernier dans le gen_sample
       ! et que leur ordre de création est :
       ! Up, Down, Left, Right, Front, Rear
       iXmin = nbody       ! Rear
       iXmax = nbody - 1   ! Front
       iYmin = nbody - 3   ! Left
       iYmax = nbody - 2   ! Right
       iZmin = nbody - 4   ! Down
       iZmax = nbody - 5   ! Up
       
       volume0 = 0.D0

       ! Très moche, mais très très... 
       ! On suppose que l'échantillon ne comprend pas de clusters
       ! (mis à part les parois).
       DO i=1, ntact_SPHER

          ! On skip les clusters (supposés être des parois)
          if ( spher2bdyty(1, i) == iXmin .or. spher2bdyty(1, i) == iXmax  .or. &
               spher2bdyty(1, i) == iYmin .or. spher2bdyty(1, i) == iYmax  .or. &
               spher2bdyty(1, i) == iZmin .or. spher2bdyty(1, i) == iZmax ) cycle

          volume0 = volume0 + get_volume(spher2bdyty(1,i))

       END DO

       IF(volume0.EQ.0)THEN
          CALL LOGMES(' @ WARNING: particle volume seems to be equal to zero')
          CALL LOGMES(' @ Command TRIAXIAL COMPACITY can not work')
          STOP
       END IF

       ! Les parois sont supposées avoir la même épaisseur       
       CALL get_data(iXMin,1,DATA)
       width = DATA(1)

    CASE(1)

       coor = 0.d0
       Xmin = 1.d24
       Xmax = -1.d24
       Ymin = 1.d24
       Ymax = -1.d24
       Zmin = 1.d24
       Zmax = -1.d24
       Xmin4all = 1.d24
       Xmax4all = -1.d24
       Ymin4all = 1.d24
       Ymax4all = -1.d24
       Zmin4all = 1.d24
       Zmax4all = -1.d24

       if ( get_visible(iXmin) ) then
          coor = get_coor(iXmin,0)
          Xmin = coor(1) + width
       end if
       if ( get_visible(iXmax) ) then
          coor = get_coor(iXmax,0)
          Xmax = coor(1) - width
       end if
       if ( get_visible(iYmin) ) then 
          coor = get_coor(iYmin,0)
          Ymin = coor(2) + width 
       end if
       if ( get_visible(iYmax) ) then
          coor = get_coor(iYmax,0)
          Ymax = coor(2) - width
       end if
       if ( get_visible(iZmin) ) then
          coor = get_coor(iZmin,0)
          Zmin = coor(3) + width
       end if
       if ( get_visible(iZmax) ) then
          coor = get_coor(iZmax,0)
          Zmax = coor(3) - width
       end if

       call MPI_REDUCE(Xmin, Xmin4all, 1, MPI_DOUBLE_PRECISION, MPI_MIN, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
       call MPI_REDUCE(Xmax, Xmax4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
       call MPI_REDUCE(Ymin, Ymin4all, 1, MPI_DOUBLE_PRECISION, MPI_MIN, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
       call MPI_REDUCE(Ymax, Ymax4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
       call MPI_REDUCE(Zmin, Zmin4all, 1, MPI_DOUBLE_PRECISION, MPI_MIN, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
       call MPI_REDUCE(Zmax, Zmax4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

       if ( rang /= 0 ) return
 
       BoxVolume = (Xmax4all-Xmin4all)*(Ymax4all-Ymin4all)*(Zmax4all-Zmin4all)
   
       OPEN(unit=nfich_triax_compac,file=clout_triax_compac,STATUS='OLD',POSITION='APPEND') 
       WRITE(unit=nfich_triax_compac,fmt='(2(1X,D14.7))') TPS,Volume0/BoxVolume
       CLOSE(unit=nfich_triax_compac) 

    CASE DEFAULT 
       call faterr(IAM, "PB dans le select case, l'etiquette est incorrecte")
    END SELECT

  end subroutine triaxial_compacity_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine compute_nb_RBDY3_slave_in_DDM
  
    implicit none

    nb_RBDY3_slave = count(mask_in_slave .eqv. .true.)

  end subroutine compute_nb_RBDY3_slave_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_SDM_INFORMATIONS
  
    implicit none

    OPEN(unit=nfich_sdm_inf,file=clout_sdm_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_sdm_inf,fmt='(D14.7,7(1X,I8))') TPS,nb_RBDY3_slave,nb_RBDY3_interf_slave, &
                     nb_interactions_slave,nb_fine_interactions_slave, 0, & ! pas de liens interf_slave en centralisé !!
                     nb_INTRF,nb_adjac_slave
    CLOSE(unit=nfich_sdm_inf) 

  end subroutine write_SDM_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_MEAN_SDM_INFORMATIONS
  
    implicit none

    OPEN(unit=nfich_mean_sdm_inf,file=clout_mean_sdm_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_mean_sdm_inf,fmt='(8(1X,D14.7))') TPS,mean_nb_RBDY3_slave,mean_nb_RBDY3_interf_slave, &
                     mean_nb_interactions_slave,mean_nb_fine_interactions_slave, 0.d0, & ! pas de liens interf_slave en centralisé !!
                     mean_nb_INTRF,mean_nb_adjac_slave
    CLOSE(unit=nfich_mean_sdm_inf) 

  end subroutine write_MEAN_SDM_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine sample_informations_in_DDM
  
    implicit none

    call MPI_REDUCE(nb_fine_interactions_slave,nb_fine_interactions4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call MPI_REDUCE(nb_INTRF,nb_INTRF4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    call MPI_REDUCE(nb_adjac_slave,nb_adjac4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if (rang /= 0) return

    nb_RBDY3_interf4all = count(multiplicite > 1)

    OPEN(unit=nfich_sample_inf,file=clout_sample_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_sample_inf,fmt='(D14.7,8(1X,I8))') TPS,nbody,nb_RBDY3_interf4all, &
                     nb_interactions4all,nb_fine_interactions4all,nb_liens_interf, &
                     nb_INTRF4all,nb_adjac4all, nb_migrations
    CLOSE(unit=nfich_sample_inf) 

    mean_nb_RBDY3_interf4all      = mean_nb_RBDY3_interf4all      + real(nb_RBDY3_interf4all)
    mean_nb_interactions4all      = mean_nb_interactions4all      + real(nb_interactions4all)
    mean_nb_fine_interactions4all = mean_nb_fine_interactions4all + real(nb_fine_interactions4all)
    mean_nb_liens_interf4all      = mean_nb_liens_interf4all      + real(nb_liens_interf)
    mean_nb_INTRF4all             = mean_nb_INTRF4all             + real(nb_INTRF4all)
    mean_nb_adjac4all             = mean_nb_adjac4all             + real(nb_adjac4all)
    mean_nb_migrations4all        = mean_nb_migrations4all        + real(nb_migrations) 

  end subroutine sample_informations_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_MEAN_SAMPLE_INFORMATIONS
  
    implicit none

    if (rang /= 0) return

    OPEN(unit=nfich_mean_sample_inf,file=clout_mean_sample_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_mean_sample_inf,fmt='(9(1X,D14.7))') TPS,real(nbody),mean_nb_RBDY3_interf4all, &
                     mean_nb_interactions4all,mean_nb_fine_interactions4all,mean_nb_liens_interf4all, &
                     mean_nb_INTRF4all,mean_nb_adjac4all,mean_nb_migrations4all
    CLOSE(unit=nfich_mean_sample_inf) 

  end subroutine write_MEAN_SAMPLE_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_OUT_in_DDM
  
    implicit none
  
    integer, save      :: Nprint=0
    integer            :: sdm, nfich, ibody, multi
    logical            :: visibility
    character(len=29)  :: clout_DDM_INFO ! pour calculer le nom du DDM.INFO

    ! l'entete du fichier DOF n'est ecrite que par le processus de rang 0
    call Write_xxx_dof_Ol(1, rang /= 0)
    CALL write_xxx_dof_RBDY3(1,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang 0
    call Write_xxx_Vloc_Rloc_Ol(1, rang /= 0)
    if ( ntact_SPHER /= 0 ) CALL write_xxx_Vloc_Rloc_SPSPx(1)
    if ( ntact_POLYR /= 0 ) CALL write_xxx_Vloc_Rloc_PRPRx(1)


    ! Handle unique pour chaque processus
    nfich=rang+1000 
    Nprint=Nprint+1
    sdm=rang+1

    !call write_DDM_INFO

    !               12345678901234567890123456789
    clout_DDM_INFO='OUTBOX/DDM.INFO.xxxxx.xxxxxxx'
    write(clout_DDM_INFO(17:21), '(I5.5)') sdm
    write(clout_DDM_INFO(23:29), '(I7.7)') Nprint 

    OPEN(unit=nfich,STATUS='REPLACE',file=clout_DDM_INFO)
    do ibody = 1,nbody

       visibility=mask_in_slave(ibody)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, L1, 1X, I3, 1X, I3)') ibody, visibility, sdm, multi

    end do
    CLOSE(nfich)
 
  end subroutine write_OUT_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_LAST_in_DDM
  
    implicit none

    integer            :: sdm, nfich, ibody, multi
    logical            :: visibility
    character(len=20)  :: clout_DDM_INFO ! pour calculer le nom du DDM.INFO

    ! l'entete du fichier DOF n'est ecrite que par le processus de rang 0
    call Write_xxx_dof_Ol(2, rang /= 0)
    CALL write_xxx_dof_RBDY3(2,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang 0
    call Write_xxx_Vloc_Rloc_Ol(2, rang /= 0)
    if ( ntact_SPHER /= 0 ) CALL write_xxx_Vloc_Rloc_SPSPx(2)
    if ( ntact_POLYR /= 0 ) CALL write_xxx_Vloc_Rloc_PRPRx(2)


    ! Handle unique pour chaque processus
    nfich=rang+1000 
    sdm=rang+1

    !call write_DDM_INFO

    !               12345678901234567890
    clout_DDM_INFO='OUTBOX/DDM.INFO.LAST'

    OPEN(unit=nfich,STATUS='REPLACE',file=location(clout_DDM_INFO))
    do ibody = 1,nbody

       visibility=mask_in_slave(ibody)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, L1, 1X, I3, 1X, I3)') ibody, visibility, sdm, multi

    end do
    CLOSE(nfich)
  
  end subroutine write_LAST_in_DDM
!----------------------------------------------------------------


! Procédure d'arrêt de l'environnement MPI
!----------------------------------------------------------------
  subroutine mpi_finalize_process
  
    implicit none

    secondsTT = MPI_Wtime ( ) - secondsTT
  
    call MPI_REDUCE(secondsTT,MPI_MAX_time,1,MPI_DOUBLE_PRECISION, &
                    MPI_MAX,0,MPI_COMM_WORLD,code)
  
    if (rang==0) then
       print *, "Elapsed MPI time=", MPI_MAX_TIME
    end if


    ! Désactivation de l'environnement MPI
    call MPI_FINALIZE(code)
  
  end subroutine mpi_finalize_process
!----------------------------------------------------------------
  

! procedure de nettoyage de la memoire encore allouee
subroutine clean_ddm

   implicit none

   ! nettoyage de la memoire encore allouee dans le module de detection grossiere
   call clean_module 

end subroutine clean_ddm
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine fix_X_V_localFrame_interface
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody,nsdm) :: body_particip
    ! integer(kind=4), dimension(nbody)      :: multiplicite 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! real(kind=8), dimension(6, nb_RBDY3_interf_glob) :: V_RBDY3_interf_glob
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, i_RBDY3_sdm, i_RBDY3_interf_glob, &
               imulti, multi, isdm       
    logical :: visible
    integer, dimension(1) :: tmp

                             !123456789012345678901234567890123456
    character(len=36) :: IAM="DDM_3D::fix_X_V_localFrame_interface"

    if ( rang /= 0 ) return

    V_RBDY3_interf_glob=0.d0
    X_RBDY3_interf_glob=0.d0
    localFrame_RBDY3_interf_glob=0.d0

    do i_RBDY3_interf_glob=1,nb_RBDY3_interf_glob
       ibody=interface_globale(i_RBDY3_interf_glob)
       multi=multiplicite(ibody)
       do imulti=1,multi
          isdm = body_particip(imulti,ibody)
          tmp=maxloc(liste_RBDY3_interf_sdm(isdm)%particule(:),&
                     liste_RBDY3_interf_sdm(isdm)%particule(:)==ibody)
          i_RBDY3_sdm=tmp(1)
          
          V_RBDY3_interf_glob(:,i_RBDY3_interf_glob)=V_RBDY3_interf_glob(:,i_RBDY3_interf_glob) &
                                       + V_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) / multi

          X_RBDY3_interf_glob(:, i_RBDY3_interf_glob)=X_RBDY3_interf_glob(:, i_RBDY3_interf_glob) &
                                       + X_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) / multi

          localFrame_RBDY3_interf_glob(:, i_RBDY3_interf_glob)=localFrame_RBDY3_interf_glob(:, i_RBDY3_interf_glob) &
                                       + localFrame_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) / multi

          ! gestion du repere principal d'inertie
       end do
    end do

    do isdm = 1, Nsdm
       do i_RBDY3_sdm = 1, nb_RBDY3_interf_sdm(isdm)
          ibody = liste_RBDY3_interf_sdm(isdm)%particule(i_RBDY3_sdm)
          tmp=maxloc(interface_globale, interface_globale==ibody)
          i_RBDY3_interf_glob=tmp(1)

          V_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) = &
              V_RBDY3_interf_glob(:,i_RBDY3_interf_glob)

          X_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) = &
              X_RBDY3_interf_glob(:,i_RBDY3_interf_glob)

          localFrame_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) = &
              localFrame_RBDY3_interf_glob(:,i_RBDY3_interf_glob)
       end do
    end do            

 end subroutine fix_X_V_localFrame_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_X_V_localFrame_interface

    implicit none

    integer            :: ibody, isdm, j
    integer            :: i_part_sdm
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localFrame_tmp

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          do ibody = 1, nb_RBDY3_interf_sdm(isdm)
             V_interf4all(6*vect_shift(isdm) + 6*(ibody-1)+1 : 6*vect_shift(isdm) + 6*ibody) = &
              V_RBDY3_interf_sdm(isdm)%particule(:, ibody)

             X_interf4all(3*vect_shift(isdm) + 3*(ibody-1)+1 : 3*vect_shift(isdm) + 3*ibody) = &
              X_RBDY3_interf_sdm(isdm)%particule(:, ibody)

             localFrame_interf4all(9*vect_shift(isdm) + 9*(ibody-1)+1 : 9*vect_shift(isdm) + 9*ibody) = &
              localFrame_RBDY3_interf_sdm(isdm)%particule(:, ibody)
          end do
       end do
    end if

    nb_recv_slave = 6*nb_RBDY3_interf_slave

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(V_interf4all, 6*vect_nb_send_host, 6*vect_shift, MPI_DOUBLE_PRECISION, &
       V_RBDY3_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    nb_recv_slave = 3*nb_RBDY3_interf_slave

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(X_interf4all, 3*vect_nb_send_host, 3*vect_shift, MPI_DOUBLE_PRECISION, &
       X_RBDY3_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    nb_recv_slave = 9*nb_RBDY3_interf_slave

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(localFrame_interf4all, 9*vect_nb_send_host, 9*vect_shift, MPI_DOUBLE_PRECISION, &
       localFrame_RBDY3_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Ecriture des interactions recues dans le fichier attache au processus courant
    !write(slave_io, *) "vitesses grains d'interface recues :"
    !do j=1, nb_RBDY3_interf_slave
    !   write(slave_io, *) 'j=', j, ' : ', V_RBDY3_interf_slave(6*(j - 1) + 1:6*j)
    !end do


    do i_part_sdm=1,nb_RBDY3_interf_slave
       ibody=liste_RBDY3_interf_slave(i_part_sdm)
       ! Stockage de V_tmp pour le corps couran courant
       V_tmp=V_RBDY3_interf_slave(6 * (i_part_sdm - 1) + 1 : 6*i_part_sdm)
       call put_vector_RBDY3('V____',ibody,V_tmp,6)

       X_tmp=0.d0
       X_tmp(1:3)=X_RBDY3_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm)
       call put_vector_RBDY3('X____',ibody,X_tmp,6)


       localFrame_tmp=0.d0
       localFrame_tmp=reshape(localFrame_RBDY3_interf_slave(9 * (i_part_sdm - 1) + 1 : 9*i_part_sdm), (/3,3/))
       call put_matrix_RBDY3('IF___', ibody, localFrame_tmp, 3)
    end do

 end subroutine scatter_X_V_localFrame_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine fix_V_interface
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody,nsdm) :: body_particip
    ! integer(kind=4), dimension(nbody)      :: multiplicite 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! real(kind=8), dimension(6, nb_RBDY3_interf_glob) :: V_RBDY3_interf_glob
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, i_RBDY3_sdm, i_RBDY3_interf_glob, &
               imulti, multi, isdm       
    logical :: visible
    integer, dimension(1) :: tmp

                             !1234567890123456789012345
    character(len=25) :: IAM="DDM_3D::fix_V_interface"

    if ( rang /= 0 ) return

    V_RBDY3_interf_glob=0.d0

    do i_RBDY3_interf_glob=1,nb_RBDY3_interf_glob
       ibody=interface_globale(i_RBDY3_interf_glob)
       multi=multiplicite(ibody)
       do imulti=1,multi
          isdm = body_particip(imulti,ibody)
          tmp=maxloc(liste_RBDY3_interf_sdm(isdm)%particule(:),&
                     liste_RBDY3_interf_sdm(isdm)%particule(:)==ibody)
          i_RBDY3_sdm=tmp(1)
          
          V_RBDY3_interf_glob(:,i_RBDY3_interf_glob)=V_RBDY3_interf_glob(:,i_RBDY3_interf_glob) &
                                       + V_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) / multi
       end do
    end do

    do isdm = 1, Nsdm
       do i_RBDY3_sdm = 1, nb_RBDY3_interf_sdm(isdm)
          ibody = liste_RBDY3_interf_sdm(isdm)%particule(i_RBDY3_sdm)
          tmp=maxloc(interface_globale, interface_globale==ibody)
          i_RBDY3_interf_glob=tmp(1)

          V_RBDY3_interf_sdm(isdm)%particule(:,i_RBDY3_sdm) = &
              V_RBDY3_interf_glob(:,i_RBDY3_interf_glob)
       end do
    end do            

 end subroutine fix_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_V_interface

    implicit none

    integer            :: ibody, isdm, j
    integer            :: i_part_sdm
    real(kind=8), dimension(6) :: V_tmp

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          do ibody = 1, nb_RBDY3_interf_sdm(isdm)
             V_interf4all(vect_shift(isdm) + 6*(ibody-1)+1 : vect_shift(isdm) + 6*ibody) = &
              V_RBDY3_interf_sdm(isdm)%particule(:, ibody)
          end do
       end do
    end if

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(V_interf4all, vect_nb_send_host, vect_shift, MPI_DOUBLE_PRECISION, &
       V_RBDY3_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Ecriture des interactions recues dans le fichier attache au processus courant
    !write(slave_io, *) "vitesses grains d'interface recues :"
    !do j=1, nb_RBDY3_interf_slave
    !   write(slave_io, *) 'j=', j, ' : ', V_RBDY3_interf_slave(6*(j - 1) + 1:6*j)
    !end do


    do i_part_sdm=1,nb_RBDY3_interf_slave
       ibody=liste_RBDY3_interf_slave(i_part_sdm)
       ! Stockage de V_tmp pour le corps couran courant
       V_tmp=V_RBDY3_interf_slave(6 * (i_part_sdm - 1) + 1 : 6*i_part_sdm)
       call put_vector_RBDY3('V____',ibody,V_tmp,6)
    end do

 end subroutine scatter_V_interface
!----------------------------------------------------------------


end module DDM_MPI_3D
