          !----------------------------------------------------------------!
          !   NSCDD MPI 2D avec topoligie de communication décentralisée   !
          !         Écrit par : Vincent Visseq & Alexandre Martin          !
          !     Module importé dans DKDK_standalone_DDM_MPI_DECENT.f90     !
          !----------------------------------------------------------------!

module DDM_MPI_2D_DECENT 

use LMGC90_MPI

use MPI

use timer

use parameters
use overall
use utilities
use RBDY2
use DISKx, only : diskx2bdyty, get_radius_DISKx, &
                  get_color_diskx => get_color , &
                  get_coor_diskx  => get_coor  , &
                  is_diskx_same_bdyty          , &
                  get_max_radius_DISKx         , &
                  get_min_radius_DISKx
use DKDKx, only : set_interactions_to_rough, &
                  set_anonymous_to_rough,    &
                  get_nb_INTRF_DKDKx,        & 
                  write_xxx_Vloc_Rloc_DKDKx, &
                  get_list_INTRF_DKDKx

use inter_meca_handler_2D, only : get_nb_inters, &
                                  get_gaps

use anonymous_ptr_container, only : get_object               => get_data                   , &
                                    get_nb_objects           => get_nb_data                , &
                                    get_status                                             , &
                                    close_container          => close_ptr_container        , &
                                    open_container           => open_ptr_container         , &
                                    add_object_to_container  => add_object_to_ptr_container, &
                                    display_object_container => display_ptr_container      , &
                                    erase_container          => erase_ptr_container        , &
                                    container                => PTR_CONTAINER
use nlgs, only : shift_icdan, RnodHRloc_nlgs, compute_local_free_vlocy, &
                 prep_check_nlgs, solve_nlgs, get_error, &
                 get_nb_adjac_nlgs_2D, &
                 compute_convergence_norms_nlgs, check_convergence_nlgs
use anonymous
use rough_detections
use tact_behaviour  ! pour chercher la plus grande distance d'alerte
use a_matrix        ! pour utiliser les G_matrix

implicit none

!am: on declare tout prive (a priori) pour eviter les effets de bord
private


logical :: NSCDD=.false. ! type d'algorithme DDM => Schwartz ou NSCDD  

!On ne travaille qu'avec des disques (et des clusters de disques)
integer(kind=4) :: nbody          ! nombre RBDY2 
integer(kind=4) :: ntact          ! nombre de contacteurs disques

logical         :: first_real_step      = .true. ! pour savoir si l'on est au premier pas de temps,
                                                 ! mise à .false. au premier scatter_creation_domaine
logical         :: xperiodic=.false.
real(kind=8)    :: xperiode = 0.d0

!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
!                                       Base de donnée du processus maitre
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
! Pour le monitoring
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4) :: nb_RBDY2_interf4all
integer(kind=4) :: nb_fine_interactions4all
integer(kind=4) :: nb_INTRF4all    
integer(kind=4) :: nb_adjac4all
real(kind=8)    :: mean_nb_RBDY2_interf4all
real(kind=8)    :: mean_nb_interactions4all
real(kind=8)    :: mean_nb_fine_interactions4all
real(kind=8)    :: mean_nb_liens_interf4all
real(kind=8)    :: mean_nb_INTRF4all
real(kind=8)    :: mean_nb_adjac4all
real(kind=8)    :: mean_nb_migrations4all

!-------------------------------------------------------------------------------------------------------------------
! Tableaux des coordonnées d'entités et leur sous-domaine (un seul et unique)
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:, :), allocatable     :: coord_ci       ! coordonnees des centres d'inertie des RBDY2
real(kind=8), dimension(:, :), allocatable     :: coord_cc       ! coordonnees des centres des contacts (uniquement entre disques, 
                                                                 ! mais il faut faire évoluer cela!!!) 
integer(kind=4), dimension(:), allocatable     :: repart_sdm_ci  ! listes des sdm auquels appartiennent les centres d'inertie des RBDY2
integer(kind=4), dimension(:), allocatable     :: repart_sdm_cc  ! listes des sdm auquels appartiennent les centres des contacts 
real(kind=8), dimension(:, :), allocatable     :: coord_ct       ! coordonnees des centres d'inertie des contacteurs disques
real(kind=8), dimension(:), allocatable        :: radius_ct      ! rayons des contacteurs diskx
!-------------------------------------------------------------------------------------------------------------------
! Pour box_detection
!-------------------------------------------------------------------------------------------------------------------
real(kind=8)                                   :: minray, maxray ! pour calculer alert  
real(kind=8)                                   :: alert          ! distance d'alerte globale utilisee pour la methode des boites

!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques (3 types de masques, car en multidomaine séquentiel ça complique un peut)
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4)                              :: max_multi      ! maximum de multiplicité pour une particule
integer(kind=4), dimension(:,:), allocatable :: body_particip  ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                               ! participent chaque corps (alloué dans init_dd)
integer(kind=4), dimension(:,:), allocatable :: body_particip_old ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                               ! participent chaque corps (alloué dans init_dd) à la répartiction en
                                                               ! sous-domaine précédente
integer(kind=4), dimension(:,:), allocatable :: migration_tab  ! table des migrations par sous-domaines
integer(kind=4)                              :: nb_migrations  ! nombre de migrations
logical, dimension(:,:), allocatable         :: mask_particip  ! matrice (/ nbody,Nsdm /) permettant de déterminer, pour
                                                               ! chaque sous-domaines, quels sont les objets visibles par ce sdm

!-------------------------------------------------------------------------------------------------------------------
! Info pour la sous-structuration géométrique de l'échantillon
!-------------------------------------------------------------------------------------------------------------------
real(kind=8)                                   :: Bleft, Bright, Bup, Bdown ! caractéristiques de la boite englobante 
integer(kind=4)                                :: Nsdm1, Nsdm2              ! nombre des sous-domaines en abscisse et en ordonnée 
integer(kind=4)                                :: Nsdm                      ! nombre total de sous-domaines 
real(kind=8), dimension(2)                     :: dim_sdm                   ! dimensions des sous domaines "DDM"

!-------------------------------------------------------------------------------------------------------------------
! Structures associées à la detection grossière des contacts
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), save    :: rough_contact     ! table  de visibilité
 integer                  :: nb_rough_contact  ! taille table  de visibilité
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), dimension(:), allocatable :: splitted_rough       ! rough_contact découpé par sdm
 integer(kind=4), dimension(:), allocatable :: nb_splitted_rough    ! nombre de contact par sous domaines
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
type T_matrice_i
     integer(kind=4), dimension(:,:), pointer :: particule => null()
end type T_matrice_i
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
type T_ligne_r
     real(kind=8), dimension(:), pointer :: particule => null()
end type T_ligne_r
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type T_matrice_r
      real(kind=8), dimension(:,:), pointer :: particule => null()
 end type T_matrice_r
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
! Données des interfaces par sous-domaine
!-------------------------------------------------------------------------------------------------------------------
type(T_ligne_i),   dimension(:), allocatable :: liste_RBDY2_interf_sdm     ! structure des indices par sdm 
type(T_ligne_i),   dimension(:), allocatable :: liste_RBDY2_interf_sdm_old ! structure des indices par sdm au pas -1
integer(kind=4),   dimension(:), allocatable :: nb_RBDY2_interf_sdm        ! nombre de corps d'interface par sdm

! "résidu" de l'interface globale
integer(kind=4)                             :: nb_liens_interf        ! nombre global de liens d'interface

!-------------------------------------------------------------------------------------------------------------------
! BDD pour les communications MPI
!-------------------------------------------------------------------------------------------------------------------

integer     , dimension(:), allocatable :: vect_nb_send_host ! Vecteur du nombre d'éléments envoyés par l'hôte
integer                                 :: nb_send_host      ! Nombre d'éléments envoyés par l'hôte
integer     , dimension(:), allocatable :: vect_shift        ! Vecteur de décalage d'indice pour répartir un
                                                             ! vecteur sur les différents processus
integer     , dimension(:), allocatable :: vect_send_host_I  ! Vecteur d'entiers "concaténation" des éléments
                                                             ! à distribuer aux processus
real(kind=8), dimension(:), allocatable :: vect_send_host_R  ! Vecteur de réels "concaténation" des éléments
                                                             ! à distribuer aux processus
integer     , dimension(:), allocatable :: vect_nb_recv_host ! Vecteur du nombre d'éléments recus par l'hôte
integer                                 :: nb_recv_host      ! Nombre d'éléments envoyés par l'hôte
real(kind=8), dimension(:), allocatable :: vect_recv_host_R  ! Vecteur de réels "concaténation" des éléments
                                                             ! à récupérer des différents processus

logical     , dimension(:), allocatable :: mask_part4all     ! Vecteur (/nb_procs_COMM_WORLD*nbody/)=(/mask(1),...,mask(n)/)

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
integer     , dimension(:), allocatable :: body_particip_interf_4all! Vecteur des body_particip_interf_sdm concaténés
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
integer     , dimension(:), allocatable    :: mig_indices4all   ! Vecteur des indices des particules migrantes
real(kind=8), dimension(:), allocatable    :: mig_etats4all     ! Vecteur des états des particules migrantes
integer     , dimension(:), allocatable    :: mig_indices_host  ! Vecteur des indices des particules migrantes
real(kind=8), dimension(:), allocatable    :: mig_etats_host    ! Vecteur des états des particules migrantes
integer(kind=4), dimension(:), allocatable :: I4all             ! Indices des RBDY2 

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
logical           :: solv_info = .false.        ! Pour savoir si on fait le postraitement correspondant
logical           :: sdm_info = .false.         ! Pour savoir si on fait le postraitement correspondant
logical           :: mean_sdm_info = .false.    ! Pour savoir si on fait le postraitement correspondant
logical           :: sample_info = .false.      ! Pour savoir si on fait le postraitement correspondant
logical           :: mean_sample_info = .false. ! Pour savoir si on fait le postraitement correspondant
logical           :: viol_evol = .false.        ! Pour savoir si on fait le postraitement correspondant
character(len=31) :: clout_solv_inf             ! Nom pour les fichiers de postraitement
character(len=34) :: clout_sdm_inf              ! Nom pour les fichiers de postraitement
character(len=39) :: clout_mean_sdm_inf         ! Nom pour les fichiers de postraitement
character(len=31) :: clout_sample_inf           ! Nom pour les fichiers de postraitement
character(len=36) :: clout_mean_sample_inf      ! Nom pour les fichiers de postraitement
character(len=31) :: clout_viol_evol            ! Nom pour les fichiers de postraitement

!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
!                                  Base de donnée d'un processus esclave 
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------

integer(kind=4)     :: real_max_multi ! maximum effectif de multiplicité pour une particule
integer             :: nb_send_slave  ! Nombre d'éléments envoyés par l'esclave
integer             :: nb_recv_slave  ! Nombre d'éléments reçus par l'esclave

real(kind=8), dimension(:)  , allocatable :: Vbeg_slave  
real(kind=8), dimension(:)  , allocatable :: Xbeg_slave  
integer     , dimension(:)  , allocatable :: Ibeg_slave  

!-------------------------------------------------------------------------------------------------------------------
! Pour le monitoring
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4) :: nb_RBDY2_slave
integer(kind=4) :: nb_fine_interactions_slave
integer(kind=4) :: nb_adjac_slave
real(kind=8)    :: mean_nb_RBDY2_slave
real(kind=8)    :: mean_nb_RBDY2_interf_slave
real(kind=8)    :: mean_nb_interactions_slave
real(kind=8)    :: mean_nb_fine_interactions_slave
real(kind=8)    :: mean_nb_liens_interf_slave
real(kind=8)    :: mean_nb_INTRF
real(kind=8)    :: mean_nb_adjac_slave
real(kind=8)    :: mean_nb_linked_sdm

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
integer                                   :: nb_RBDY2_interf_slave        ! nombre de corps d'interface du sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY2_interf_slave     ! indices des RBDY2 d'interface sur sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY2_interf_slave_old ! structure des indices du sdm au pas -1

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
type(T_matrice_i),dimension(:) , allocatable :: body_particip_interf_sdm  ! structure des tranches de body_particip
integer(kind=4), dimension(:,:), allocatable :: body_particip_interf_slave! body_particip des corps d'interface du sdm courant
integer(kind=4), dimension(:)  , allocatable :: multiplicite              ! vecteur recenssant le nombre de participation aux sdm des corps
integer(kind=4), dimension(:)  , allocatable :: multiplicite_old          ! vecteur recenssant le nombre de participation aux sdm des corps à la repart_sdm -1
real   (kind=8), dimension(:)  , allocatable :: inv_multi_interf_slave    ! vecteur des inverses des multiplicités des RBDY2 d'interface du sdm
                                                                          ! pour chaque lien d'interface associé à ces corps
integer(kind=4)                              :: nb_linked_sdm             ! Nombre de sdm possédant des corps d'interface en commun avec le
                                                                          ! sdm courant + le sdm courant lui-même 
integer(kind=4), dimension(:)  , allocatable :: liste_linked_sdm          ! Liste des sdm possédant des corps d'interface en commun avec le
                                                                          ! sdm courant + le sdm courant lui-même 
integer(kind=4), dimension(:)  , allocatable :: nb_RBDY2_shared           ! Liste des nombres de RBDY2 en commun avec les sdm "shared" 
type(T_ligne_i), dimension(:)  , allocatable :: liste_RBDY2_shared        ! Liste des indices des RBDY2 en commun avec les sdm "shared"

!-------------------------------------------------------------------------------------------------------------------
! Maps remplies par prep_compute_DF_gamma
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4), dimension(:,:), allocatable :: loc_RBDY2_in_liste_interf_slave ! Emplacement des RBDY2 dans liste_RBDY2_interf_slave pour chaque sdm
                                                                                ! auquel participe le RBDY2
integer(kind=4), dimension(:,:), allocatable :: loc_RBDY2_in_liste_linked_sdm   ! Emplacement des RBDY2 dans liste_linkes_sdm
integer(kind=4), dimension(:,:), allocatable :: loc_lien3D_in_liste_linked_sdm  ! Emplacement des liens 3d dans liste_linked_sdm

type(T_matrice_r), dimension(:), allocatable :: DATA_RBDY2_interf_slave       ! vitesses ou impulsions des RBDY2 d'interface du sdm courant 
type(T_ligne_r)  , dimension(:), allocatable :: DATA_RBDY2_interf_slave_2send ! vitesses ou impulsions des RBDY2 d'interface à envoyer aux sdm liés
type(T_ligne_r)  , dimension(:), allocatable :: DATA_RBDY2_interf_slave_2recv ! vitesses ou impulsions des RBDY2 d'interface à recevoir des sdm liés
real(kind=8),    dimension(:,:), allocatable :: saut_V_interf_slave           ! sauts de vitesses des RBDY2 d'interface globale 
integer(kind=4)                              :: nb_liens_interf_slave         ! nombre global de liens d'interface pour chaque procs
real(kind=8)                                 :: egluing_slave_square
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

real(kind=8), dimension(:)    , allocatable :: Fg_RBDY2_interf_slave        ! F_gamma des RBDY2 d'interfacedu sdm courant
real(kind=8), dimension(:)    , allocatable :: Fg_RBDY2_interf_slave_old    ! F_gamma des RBDY2 d'interface du sdm (pas -1)
real(kind=8), dimension(:,:)  , allocatable :: DFg_RBDY2_interf_slave       ! DF_gamma des RBDY2 d'interface du sdm courant
logical                                     :: repartition_just_made = .false. ! Permet de savoir si la répartition
                                                                            ! en sous-domaine viens d'être effectuée      

!-------------------------------------------------------------------------------------------------------------------
! Gestion des masses
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:), allocatable :: masse_ref        ! vecteur contenant les masses de reference pour chaque corps
real(kind=8), dimension(:), allocatable :: masses_courantes ! vecteur contenant les masses courantes pour chaque corps

!-------------------------------------------------------------------------------------------------------------------
! BDD associée à la detection des contacts
!-------------------------------------------------------------------------------------------------------------------
integer, dimension(:), allocatable :: interactions_slave    ! Tableau des intéractions du sdm
integer                            :: nb_interactions_slave ! Nombre de contacts grossiers du sdm 
integer                            :: nb_INTRF              ! Nombre de contacts fins taggés 'INTRF'
integer,dimension(:), allocatable  :: list_INTRF            ! Liste des indices des contacts taggés 'INTRF'

!-------------------------------------------------------------------------------------------------------------------
! BDD pour les communications MPI
!-------------------------------------------------------------------------------------------------------------------
!integer         :: nb_procs_COMM_WORLD,rang_COMM_WORLD,code_MPI
!real(kind=8)    :: secondsTT,MPI_MAX_time         ! Calcul du temps maximal passé par un processus pour
!integer         :: slave_io
integer(kind=4) :: id_prep_gather_V, id_gather_V  ! Pour calculer le temps passé dans les échanges "in loop"


!-------------------------------------------------------------------------------------------------------------------
! Fin des déclarations des variables globales au module
!-------------------------------------------------------------------------------------------------------------------


!am: on donne la liste des fonctions publiques (utilisables hors du module)
public                                        &
   init_dd,                                   &
   !init_MPI,                                  &
   !start_MPI_time,                            &  ! M
   !stop_MPI_time,                             &  ! M
   set_working_directory_in_DDM,              &
   !mpi_finalize_process,                      &
   new_dd_slave,                              &
   new_dd_host,                               &
   allocations_fantome_slave,                 &
   creation_domaines,                         &
   erase_rough_contact,                       &
   erase_splitted_rough,                      &
   set_visibility_4all_in_DDM,                &
   set_interactions_to_rough_in_DDM,          &
   get_list_INTRF_in_DDM,                     &
   RnodHRloc_list_in_DDM,                     &
   compute_local_free_vlocy_in_DDM,           &
   set_skip_display_cluster,                  &  ! Os
   write_OUT_in_DDM,                          &  ! Os
   write_LAST_in_DDM,                         &  ! Os
   init_postpro_in_DDM,                       &  ! Oh & Os
   postpro_during_in_DDM,                     &  ! Oh & Os
   postpro_last_in_DDM,                       &  ! Oh & Os
   stock_Fg_liste_old,                        &
   comp_V_list_RBDY2_in_DDM,                  &
   decentralise_prep_egluing,                 &  ! C
   compute_boundary_sample_in_DDM2d,          &
   set_F_gamma,                               &
   !set_F_gamma_NI,                            &
   comp_Vfree_Fg_list_RBDY2_in_DDM,           &  ! L
   comp_Vfree_Rddm_list_RBDY2_in_DDM,         &  ! L
   nullify_Iaux_for_vlc_drv_dof_RBDY2_in_DDM, &
   compute_Rddm_in_DDM,                       &
   gather_X_V_begin,                          &
   scatter_creation_domaines,                 &
   decentralise_prep_exchange_inloop,         &  ! EIL
   exchange_interface,                        &  ! EIL
   decentralise_fix_V_interface,              &  ! C
   prep_compute_DF_gamma,                     &  ! C
   decentralise_compute_DF_gamma,             &  ! C
   decentralise_compute_F_gamma,              &  ! C
   set_modified_mass_in_DDM,                  &
   fix_migrants,                              &
   print_info,                                &
   print_step,                                &
   print_iter,                                &
   check_convergence_in_DDM,                  &
   compute_shared_interfaces,                 &  ! BDDs
   !comp_vfree_e_prjj_invM_t_Fg_p_Vfree_list_RBDY2_in_DDM, &
   clean_ddm

contains


!!-------------------------------------------------------------------------------------------------------------------
! subroutine init_MPI
!
!    implicit none
!
!    call MPI_INIT(code_MPI)
!    call MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs_COMM_WORLD,code_MPI)
!    call MPI_COMM_RANK(MPI_COMM_WORLD,rang_COMM_WORLD,code_MPI)
!
!    slave_io=1000+rang_COMM_WORLD
!
! end subroutine init_MPI
!!-------------------------------------------------------------------------------------------------------------------
!
!!-------------------------------------------------------------------------------------------------------------------
! subroutine start_MPI_time
!
!    implicit none
!
!    secondsTT = MPI_Wtime ( )
!
! end subroutine start_MPI_time
!!-------------------------------------------------------------------------------------------------------------------
!
!!----------------------------------------------------------------
!  subroutine stop_MPI_time
!  
!    implicit none
!
!    secondsTT = MPI_Wtime ( ) - secondsTT
!  
!    call MPI_REDUCE(secondsTT,MPI_MAX_time,1,MPI_DOUBLE_PRECISION, &
!                    MPI_MAX,0,MPI_COMM_WORLD,code_MPI)
!  
!    if (rang_COMM_WORLD==0) then
!       print *, "Elapsed MPI time=", MPI_MAX_TIME
!    end if
!  
!  end subroutine stop_MPI_time
!!----------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 subroutine set_working_directory_in_DDM

    implicit none

    ! variables locales
    character(len=13) :: working_directory ! pour calculer le nom du dossier d'écriture des DOF et Vloc
    integer           :: sdm

    ! Détermination du sous-domaine qui écrit
    sdm=rang_COMM_WORLD+1
    !                  1234567890123
    working_directory='DDM_WD_xxxxx/'
    write(working_directory(8:12), '(I5.5)') sdm

    call set_working_directory(working_directory)

 end subroutine set_working_directory_in_DDM
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 subroutine print_info(nb_time_steps,TimeStep,Theta_,bavardage,freq_DISPLAY,freq_OUTBOX,freq_postpro,freq_last, &
                        DDMfreq,Nsdm1,Nsdm2,DDMtype,ixperiodic,xperiode,solving_scheme,CVTYPE,TOL,RELAX,itloop1,itloop2) 

    implicit none

    ! Variables d'entrée
    integer, intent(in)         :: nb_time_steps,bavardage,freq_DISPLAY,freq_OUTBOX,freq_postpro,freq_last, &
                                   DDMfreq,Nsdm1,Nsdm2,DDMtype,itloop1,itloop2,ixperiodic
    real(kind=8),intent(in)     :: TimeStep,Theta_     ! Pas de temps et paramètre de la theta méthode
    real(kind=8),intent(in)     :: TOL,RELAX,xperiode  ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
    CHARACTER(len=5),intent(in) :: CVTYPE              ! Type de convergence : QUAD, MAX, QUAD/16, etc...
    CHARACTER(len=3)            :: solving_scheme      ! Type de schema de resolution : SDL ou ELG



                             !12345678901234567890
    character(len=18) :: IAM='DDM_2D::print_info'

    if (DDMtype == 1) NSCDD=.true. 

    if ( rang_COMM_WORLD /= 0 ) return

    !-------------------------------------------------------------------------------
    ! Ecriture à l'écran des mêmes paramètres
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! loading step            : ',nb_time_steps
    WRITE(*,*) '! TIME STEP               : ',TimeStep
    WRITE(*,*) '! THETA                   : ',Theta_
    WRITE(*,*) '! BAVARDAGE               : ',bavardage
    WRITE(*,*) '! WRITE DISPLAY FREQUENCY : ',freq_DISPLAY
    WRITE(*,*) '! WRITE OUTBOX FREQUENCY  : ',freq_OUTBOX
    WRITE(*,*) '! WRITE POSTPRO FREQUENCY : ',freq_postpro
    WRITE(*,*) '! WRITE BACKUPS FREQUENCY : ',freq_last
    !-------------------------------------------------------------------------------
    ! Paramètres de découpages en sous_domaines
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! FREQUENCE DETECT/REPART : ',DDMfreq
    WRITE(*,*) '! Nsdm1 et Nsdm2          : ',Nsdm1,Nsdm2
    if (DDMtype == 0) then
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'SCHWARZ'
    elseif (DDMtype == 1) then
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'NSCDD'
    elseif (DDMtype == 2) then
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'NSCDD_NI'
    else
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'INCONNU !!!'
       call faterr(IAM,"Type d'algorithme DDM inconnu !!!")
    end if 
    WRITE(*,*) '! iperiodic & xperiode    : ',ixperiodic,xperiode
    !-------------------------------------------------------------------------------
    ! Paramètres d'itération de la boucle DDM
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! SDL ou ELG              : ',solving_scheme
    WRITE(*,*) '! NLGS CHECK TYPE         : ',CVTYPE,TOL
    WRITE(*,*) '! RELAX                   : ',RELAX
    WRITE(*,*) '! iteration & more        : ',itloop1,itloop2

 end subroutine print_info
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
 function init_dd(Nsdm1_,Nsdm2_) 

    implicit none

    ! Variables d'entrée
    integer, intent(in)      :: Nsdm1_,Nsdm2_

    ! Valeur de retour
    integer :: init_dd

    ! Variables locales
    integer :: isdm,err
                             !12345678901234567890
    character(len=15) :: IAM='DDM_2D::init_dd'

    Nsdm1=Nsdm1_
    Nsdm2=Nsdm2_

    ! Calcule du nombre total de sous-domaines
    Nsdm=Nsdm1*Nsdm2

    ! Calcul de la multiplicité maximale pour une particule
    max_multi=max(4,Nsdm1,Nsdm2) ! Un cluster en diagonale ferait tout sauter

    ! on le renvoie
    init_dd=Nsdm


    if (Nsdm/=nb_procs_COMM_WORLD)  call faterr(IAM, "Le nombre de processus doit etre &
            & strictement egal au nombre de sous-domaines !!")

 end function init_dd
!-------------------------------------------------------------------------------------------------------------------

!Procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd_slave(nb_RBDY2,ntact_,lperiodic,rperiode)

    implicit none

    ! variables d'entrée
    integer, intent(in)    :: nb_RBDY2,ntact_
    logical, intent(in)      :: lperiodic
    real(kind=8), intent(in) :: rperiode

    ! variables locales
    integer                :: isdm,ibody,err
    integer                :: isee

                             !12345678901234567890
    character(len=20) :: IAM='DDM_2D::new_dd_slave'

    nbody=nb_RBDY2
    ntact=ntact_

    xperiodic = lperiodic
    xperiode  = rperiode

!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques (3 types de masques, car en multidomaine séquentiel ça complique un peut)
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

                                     !   12345678901234567890
    id_prep_gather_V = get_new_itimer_ID('PREP Exchange V       ') 
    id_gather_V      = get_new_itimer_ID('Exchange V            ') 

 end subroutine new_dd_slave
!-------------------------------------------------------------------------------------------------------------------


!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd_host

    implicit none

    ! variables locales
    integer                :: isdm,ibody,err
    integer                :: isee

                             !123456789012345678901
    character(len=21) :: IAM='DDM_2D::new_dd_maitre'


    ! Seul le processus maitre alloue les structures qui y correspondent 
    if ( rang_COMM_WORLD /= 0 ) return

    print *, "nombre_de_RBDY2", nbody

!-------------------------------------------------------------------------------------------------------------------
! Tableaux des coordonnées d'entités et leur sous-domaine (un seul et unique)
!-------------------------------------------------------------------------------------------------------------------
    !    * les coordonnees des centres d'inertie des RBDY2
    if (.not. allocated(coord_ci)) then
       allocate(coord_ci(3,nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des centres d'inerties des RBDY2")
    end if

    !    * numeros de sous-domaines des centres d'inertie des corps
    if (.not. allocated(repart_sdm_ci)) then
       allocate(repart_sdm_ci(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de la liste indicatrice des sous-domaines")
    end if

    !    * les coordonnees des centres d'inertie des contacteurs disques                      
    if (.not. allocated(coord_ct)) then                                                       
       allocate(coord_ct(2,ntact), stat=err)                                                  
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des centres d'inertie des DISKx")
    end if                                                                                    
    !    * les rayons des contacteurs disques                                                
    if (.not. allocated(radius_ct)) then                                                      
       allocate(radius_ct(ntact), stat=err)                                                   
       if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees des rayons des DISKx")
    end if                                                                                    
    
!-------------------------------------------------------------------------------------------------------------------
! Pour box_detection
!-------------------------------------------------------------------------------------------------------------------
    ! Paramètres de la méthode des boites générique qu'il n'est pas
    ! nécessaire de recalculer à tous les pas de temps
    ! J'utilise ici des fonctions spécifiques au type contacteur DISKx
    minray = get_min_radius_DISKx()
    maxray = get_max_radius_DISKx()

    ! calcul de la plus grande distance d'alerte, parmi toutes les lois
    ! N.B. c'est bourrin!
    alert = 0.D0
    do isee=1, size(see)
       alert=max(alert, see(isee)%alert)
    end do
 
!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques
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
       allocate(body_particip(max_multi,nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation des corps aux sdm")
    end if
    !    * table des sous-domaines auxquels participent les corps à la répartion DDM précédente
    if (.not. allocated(body_particip_old)) then
       allocate(body_particip_old(max_multi,nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation old des corps aux sdm")
    end if

    ! Rq: multiplicite est déjà alloué pour le rang_COMM_WORLD 0 dans new_dd_slave
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

    !    * table des nombre de contacts appartenants aux sdm
    if (.not. allocated(nb_splitted_rough)) then
       allocate(nb_splitted_rough(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_splitted_rough")
    end if

!-------------------------------------------------------------------------------------------------------------------
! Données des interfaces par sous-domaine
!-------------------------------------------------------------------------------------------------------------------
    !    * nombre de grains d'interface appartenants aux sdm
    if (.not. allocated(nb_RBDY2_interf_sdm)) then
       allocate(nb_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de nb_RBDY2_interf_sdm")
    end if

    !    * table des RBDY2 d'interface appartenants aux sdm
    if (.not. allocated(liste_RBDY2_interf_sdm)) then
       allocate(liste_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de liste_RBDY2_interf_sdm")
    end if
    !    * table des RBDY2 d'interface appartenants aux sdm du pas de temps précédent
    if (.not. allocated(liste_RBDY2_interf_sdm_old)) then
       allocate(liste_RBDY2_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de liste_RBDY2_interf_sdm_old")
    end if

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
    !    * body_particip des RBDY2 d'interface appartenants aux sdm
    if (.not. allocated(body_particip_interf_sdm)) then
       allocate(body_particip_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de body_particip_interf_sdm")
    end if
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

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

 !fonction qui alloue, avec une taille de 0, sur les esclaves, les tableaux utilises par le maitre
 ! => ifort check ne plante pas!
 subroutine allocations_fantome_slave

    implicit none

    if ( rang_COMM_WORLD == 0 ) return

    if (allocated(mask_part4all)) deallocate(mask_part4all)
    allocate(mask_part4all(0))

    if (allocated(vect_nb_send_host)) deallocate(vect_nb_send_host)
    allocate(vect_nb_send_host(0))

    if (allocated(vect_shift)) deallocate(vect_shift)
    allocate(vect_shift(0))

    if (allocated(vect_nb_recv_host)) deallocate(vect_nb_recv_host)
    allocate(vect_nb_recv_host(0))

    if (allocated(vect_send_host_I)) deallocate(vect_send_host_I)
    allocate(vect_send_host_I(0))

    if (allocated(interactions4all)) deallocate(interactions4all)
    allocate(interactions4all(0))

    if (allocated(I4all)) deallocate(I4all)
    allocate(I4all(0))


 end subroutine allocations_fantome_slave

!--------------------------------------------------------------------------------------------------------------------------------!
!--------------------------------------------------------------------------------------------------------------------------------!
!                                             "Pseudo main" du module DDM_2D                                                     !
!--------------------------------------------------------------------------------------------------------------------------------!
!--------------------------------------------------------------------------------------------------------------------------------!

 subroutine creation_domaines(left_bound,right_bound,down_bound,up_bound)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine
    real(kind=8), optional :: left_bound,right_bound,down_bound,up_bound

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine 
    integer                                    :: E_IO
    integer                                    :: i,j,iliens,tmp,err
    integer                                    :: itact, ibody, icdan, isdm
    character(len=10)                          :: step     ! pour ecrire le numero du pas  
    character(len=10)                          :: sdm      ! pour ecrire le numero du du sous_domaine courant  
    integer                                    :: isee     ! indice de boucle sur les lois d'interaction
    real(kind=8), dimension(3)                 :: tmp_coor ! coordonnées temporaire du centre d'un diskx

    ! variables utilisees pour elaguer les interactions grossieres calculees par la methode de boites generique
    type(CONTAINER)                            :: rough_contact_gen ! table de visibilité obtenue apres l'appel a la methode
                                                                    ! des boites generiques
    integer                                    :: nb_rough_contact_gen ! nombre de contacts dans rough_contact_gen
    type(T_object)                             :: contact              ! pour recuperer le contact courant
    integer(kind=4), dimension(:), pointer     :: cdan                 ! pour recuperer la paire de contacteurs en interaction 
    real(kind=8)                               :: adist
    integer(kind=4)                            :: cdtac, antac
    character(len=5)                           :: cdcol, ancol
    real(kind=8)                               :: raycd, rayan
    real(kind=8), dimension(2)                 :: sep
    ! pour creer le nouvel objet anonyme a ajouter a rough_contact
    integer(kind=4), dimension(:), pointer     :: i4 
    real(kind=8), dimension(:), pointer        :: r8
    character(len=5), dimension(:), pointer    :: c5
    character(len=128), dimension(:), pointer  :: cx

    real(kind=8), dimension(2)                 :: coord_an_tmp  ! pour la prise en compte de la periodicite


    character(len=100)                         :: cout
                                                      !123456789012345678901234567889012 
    character(len=32)                          :: IAM="DDM_DECENT__2D::creation_domaines"

    ! subroutine de rang_COMM_WORLD 0
    if ( rang_COMM_WORLD /= 0 ) return

    i4 => null()
    r8 => null()
    c5 => null()
    cx => null()

    !--------------------------------------------------------------------------------------
    ! recuperation des coordonnees des centre d'inertie des RBDY2
    do ibody=1, nbody
      coord_ci(1:3, ibody) = get_coorTT(ibody,0)
    end do

    ! Calcul de la boite englobante de l'échantillon total, puis des caractéristiques de sous domaines 
    call caracteristique_domaine()

    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des centres d'inertie des RBDY2
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Bleft, Bdown, dim_sdm, &
                             3, nbody, coord_ci, repart_sdm_ci,   & 
                             .true.)                              

    ! Recuperation des coordonnees des centre d'inertie des DISKx
    do itact=1, ntact
                ! <=> get_coor |  indice du RBDY2    |  indice du contacteur dans la
                !                                    | liste des contacteurs de ce RBDY2
      tmp_coor = get_coorTT(diskx2bdyty(1, itact), diskx2bdyty(2, itact))
      coord_ct(1:2, itact) = tmp_coor(1:2)

      !vv&mr: periodic conditions
      if (xperiodic) then
         if ( coord_ct(1,itact) > xperiode) then
            !print *, "j y passe 1"
            coord_ct(1,itact) = coord_ct(1,itact) - xperiode
         else if (coord_ct(1,itact) < 0.d0 ) then
            !print *, "j y passe 2"
            coord_ct(1,itact) = coord_ct(1,itact) + xperiode
         end if
      end if

      ! Pour la méthode des boites générique
      radius_ct(itact)=get_radius_DISKx(itact)
    end do

    ! Méthode des boites générique
    call boxes_method(coord_ct,radius_ct,alert,rough_contact_gen,xperiodic,xperiode,min_bdr_rad=minray,max_bdr_rad=maxray)

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
       cdcol = get_color_DISKx(cdtac)
       ancol = get_color_DISKx(antac)
       isee  = get_isee_specific('DISKx', cdcol, ancol)

       ! si l'interaction courant n'est associee a aucune loi d'interaction ou est un autocoantact, on passe a la suivante
       if (isee==0 .or. is_DISKx_same_BDYTY(cdtac, antac) ) CYCLE

       ! ici, on est en mesure de calculer la distance d'alerte

       ! on calcule la distance d'alerte pour cette interaction
       adist=see(isee)%alert

       raycd=radius_ct(cdtac)
       rayan=radius_ct(antac)

       adist=0.1005d+01*(adist + raycd + rayan)

       coord_an_tmp(1:2)= coord_ct(1:2, antac)
       coord_an_tmp(1)  = coord_ct(1, antac) + real(cdan(3),8)*xperiode
      
       ! on calcule la separation entre les objets
       sep(1:2) = coord_ct(1:2, cdtac) - coord_an_tmp(1:2) 

       ! si les deux contacteurs sont suffisamment proches
       ! N.B. pour le contact SPSPx, on considere la norme sup
       if (all(dabs(sep) <= adist)) then
          ! on ajoute l'interaction dans rough_contact
          nb_rough_contact = nb_rough_contact + 1

          allocate(i4(3))
          i4(1:3) = cdan(1:3)

          call add_object_to_container(rough_contact, nb_rough_contact, c5, i4, r8, cx)
       end if
    end do

    ! on ferme rough_contact
    call close_container(rough_contact)
    ! on vide le container des interactions obtenues en utilisant la methode des boites generiques
    call erase_container(rough_contact_gen)

    nb_rough_contact = get_nb_objects(rough_contact)

    ! On peut maintenant allouer les tableaux ayants une dimension a nb_rough_contact
    if( allocated(coord_cc) ) deallocate(coord_cc)           ! 
    allocate(coord_cc(nbDIME,nb_rough_contact), stat=err)    ! 
    if( err/=0 ) call faterr(IAM, "pb d'allocation de coord_cc")

    if( allocated(repart_sdm_cc) ) deallocate(repart_sdm_cc) !   
    allocate(repart_sdm_cc(nb_rough_contact), stat=err)      ! 
    if( err/=0 ) call faterr(IAM, "pb d'allocation de repart_sdm_cc")


    ! Construction du tableau des coordonnées des centres des contacts
    call creation_coord_cc 


    ! Ventillation des centre des contacts
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Bleft, Bdown, dim_sdm,          &
                             2, nb_rough_contact, coord_cc, repart_sdm_cc, &
                             .false.)   


    ! Création des masques de partiticipation par sous-domaine
    ! et de la table de multiplicité
    call creation_body_particip

 
    ! Création des tableaux de contacts par sous-domaines
    call creation_splitted_rough 


    ! Calcul du nombre de grains d'interface globale et par sous-domaine.
    ! Allocation et construction de l'interface globale.
    ! Calcul du nobre de liens d'interface.
    call compute_interface


    ! Traitement de la migration
    if (.not. first_real_step) then
       call migration_treatment !(body_particip,body_particip_old,nbody,max_multi, &
                                ! Nsdm,migration_tab)
    end if


    ! Enregistrement de multiplicite dans multiplicite_old
    multiplicite_old=multiplicite
    ! Enregistrement de body_particip dans body_particip_old
    body_particip_old=body_particip
    ! Enregistrement des vitesses des particules d'interface par sous-domaines


 end subroutine creation_domaines
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_creation_domaines

    implicit none
    
    integer :: isdm
    integer :: i, i_part_sdm
    integer :: shift, err
    logical :: mig_tab_vide
    integer, dimension( MPI_STATUS_SIZE ) :: statut
    integer, parameter :: etiquette=100


    integer, dimension(:), allocatable :: body_particip_interf_tmp
    
                             !12345678901234567890123467890123456789012345
    character(len=45) :: IAM="DDM_MPI_DECENT_2D::scatter_creation_domaines"

    
    !---------------------------
    ! MASK_PARTICIP
    !---------------------------
    
    ! Le processus hôte construit un vecteur nbody*nb_procs_COMM_WORLD
    ! où mask_particip est répété nb_procs_COMM_WORLD fois.
    if ( rang_COMM_WORLD == 0 ) then
       do isdm=1,Nsdm
          mask_part4all((isdm-1)*nbody+1:isdm*nbody)=mask_particip(1:nbody,isdm)
       end do
    end if

    ! Ce vecteur est envoyé aux esclaves qui en récupèrent
    ! chacun une tranche de longueur nbody.
    call MPI_SCATTER(mask_part4all, nbody, MPI_LOGICAL, mask_in_slave, nbody, &
                      MPI_LOGICAL, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    !---------------------------
    ! MULTIPLICITE
    !---------------------------
    
    call MPI_BCAST(multiplicite, nbody, MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
  
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
    real_max_multi = maxval(multiplicite)
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
    
    !---------------------------
    ! LISTE D'INTERFACE
    !---------------------------
    
    ! 1) Nombre d'éléments dans l'interface
    
    if ( rang_COMM_WORLD == 0 ) then
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_RBDY2_interf_sdm(isdm)
       end do
    end if


    ! Envoi du nombre de RBDY2 d'interface pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_RBDY2_interf_slave, 1, &
        MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
    
    ! Allocation du vecteur dans lequel les indices des RBDY2 d'interface seront stockés
    if (allocated(liste_RBDY2_interf_slave)) deallocate(liste_RBDY2_interf_slave)
    allocate(liste_RBDY2_interf_slave(nb_RBDY2_interf_slave), stat=err)
    if (err/=0) call faterr(IAM, "erreur d'allocation de liste_RBDY2_interf_slave")
    liste_RBDY2_interf_slave=0
    
    ! 2) Numéros des éléments de l'interface

    if ( rang_COMM_WORLD == 0 ) then
       ! Nombre total d'éléments à envoyer
       nb_send_host=sum(vect_nb_send_host)
       
       if (allocated(vect_send_host_I)) deallocate(vect_send_host_I)
       allocate(vect_send_host_I(nb_send_host), stat=err)
       if (err/=0) call faterr(IAM, "erreur d'allocation de vect_send_host_I")
       vect_send_host_I=0
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do
       
       do isdm=1,Nsdm-1
           vect_send_host_I(vect_shift(isdm)+1:vect_shift(isdm+1)) &
            = liste_RBDY2_interf_sdm(isdm)%particule(:)
       end do
       vect_send_host_I(vect_shift(Nsdm)+1:nb_send_host) &
          = liste_RBDY2_interf_sdm(Nsdm)%particule(:)
       
    end if 

    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(vect_send_host_I, vect_nb_send_host, vect_shift, MPI_INTEGER, &
    liste_RBDY2_interf_slave, nb_RBDY2_interf_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
    !------------------------------------
    ! BODY PARTICIP DES CORPS D'INTERFACE
    !------------------------------------

    if (allocated(body_particip_interf_slave)) deallocate(body_particip_interf_slave)
    allocate(body_particip_interf_slave(real_max_multi,nb_RBDY2_interf_slave),stat=err)
    if (err/=0) call faterr(IAM, "erreur d'allocation de body_particip_interf_slave")

    if (allocated(body_particip_interf_tmp)) deallocate(body_particip_interf_tmp)
    allocate(body_particip_interf_tmp(real_max_multi*nb_RBDY2_interf_slave),stat=err)
    if (err/=0) call faterr(IAM, "erreur d'allocation de body_particip_interf_tmp")

    if ( rang_COMM_WORLD == 0 ) then

       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_RBDY2_interf_sdm(isdm)*real_max_multi
       end do

       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do

       if (allocated(body_particip_interf_4all)) deallocate(body_particip_interf_4all)
       allocate(body_particip_interf_4all(sum(vect_nb_send_host)),stat=err)
       if (err/=0) call faterr(IAM, "erreur d'allocation de body_particip_interf_4all")


       do isdm = 1, Nsdm
          do i_part_sdm = 1, nb_RBDY2_interf_sdm(isdm)
             body_particip_interf_4all(vect_shift(isdm) + (i_part_sdm - 1) * real_max_multi + 1 : &
                                       vect_shift(isdm) + i_part_sdm * real_max_multi) =      &
                                       body_particip_interf_sdm(isdm)%particule(1:real_max_multi,i_part_sdm)
          end do
       end do
    end if


    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(body_particip_interf_4all, vect_nb_send_host, vect_shift, MPI_INTEGER, &
    body_particip_interf_tmp, nb_RBDY2_interf_slave*real_max_multi, MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    
    body_particip_interf_slave = reshape(body_particip_interf_tmp, (/ real_max_multi, nb_RBDY2_interf_slave/))
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

    !---------------------------
    ! LISTE DES INTERACTIONS
    !---------------------------

    ! 1) Nombre d'interactions

    if ( rang_COMM_WORLD == 0 ) then
       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_splitted_rough(isdm)
       end do
    end if

    ! Envoi du nombre d'interactions DKDK pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_interactions_slave, 1, &
            MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)

    ! Allocation du vecteur dans lequel les indices des RBDY2 d'interface seront stockés
    if (allocated(interactions_slave)) deallocate(interactions_slave)
    allocate(interactions_slave(4*nb_interactions_slave))
    interactions_slave=0

    ! 2) Distribution des intéractions

    if ( rang_COMM_WORLD == 0 ) then

       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = 4 * nb_splitted_rough(isdm)
       end do

       ! Nombre total d'éléments à envoyer
       nb_send_host=sum(vect_nb_send_host)

       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do

    end if 

    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(interactions4all, vect_nb_send_host, vect_shift, MPI_INTEGER, &
       interactions_slave, 4*nb_interactions_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    
    !---------------------------
    ! MIGRANTS
    !---------------------------
    
    if (.not. first_real_step) then
    
       ! 1) Nombre de migrations
       
       if ( rang_COMM_WORLD == 0 ) then
          vect_nb_send_host = 0
          nb_send_host = 0
       end if
      
       mig_tab_vide = .false. 
       nb_mig_slave = 0 
    
       ! ---> Fonction rang_COMM_WORLD0 <---
       call nb_migrations2send(mig_tab_vide) ! calcule vect_nb_send_host

       call MPI_BCAST(mig_tab_vide, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, code_MPI)

       if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

       if ( .not. mig_tab_vide ) then
    
          ! Envoi du nombre de RBDY2 migrant pour chaque sous domaine
          call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_mig_slave, 1, &
                  MPI_INTEGER, 0, MPI_COMM_WORLD, code_MPI)
      
          !if (rang_COMM_WORLD ==0) print *, "rang_COMM_WORLD0 vect_nb_send_host=", vect_nb_send_host
          !print *, "Rang=", rang_COMM_WORLD, "nb_mig_slave=",nb_mig_slave
      
          if ( rang_COMM_WORLD == 0 .or. nb_mig_slave /=0) then

             ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
             if (allocated(mig_indices_slave)) deallocate(mig_indices_slave)
             allocate(mig_indices_slave(nb_mig_slave))
             mig_indices_slave=0
      
             ! Allocation du vecteur dans lequel l'état des migrants vont être stockés
             if (allocated(mig_etats_slave)) deallocate(mig_etats_slave)
             allocate(mig_etats_slave(nb_mig_slave*6))
             mig_etats_slave=0.D0


             if ( rang_COMM_WORLD == 0 ) then

                ! Nombre total d'éléments à envoyer
                nb_send_host=sum(vect_nb_send_host)
       
                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_indices4all)) deallocate(mig_indices4all)
                allocate(mig_indices4all(nb_send_host))
                mig_indices4all=0
        
                ! Allocation du vecteur dans lequel l'état des migrants vont être stockés
                if (allocated(mig_etats4all)) deallocate(mig_etats4all)
                allocate(mig_etats4all(nb_send_host*6))
                mig_etats4all=0.D0
       
                ! Vecteur de décalage d'indice pour le SCATTERV
                vect_shift=0
                do isdm=2,Nsdm 
                   vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
                end do

                ! ---> Fonction rang_COMM_WORLD0 <---
                call compute_etats_indices_migration
               
             end if 

          end if 
      
          ! 2) Distribution des migrants

          do isdm = 2, Nsdm

             if ( rang_COMM_WORLD == 0 ) then
                ! Nombre total d'éléments à envoyer
                nb_send_host=vect_nb_send_host(isdm)
         
                if (nb_send_host==0) cycle

                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_indices_host)) deallocate(mig_indices_host)
                allocate(mig_indices_host(nb_send_host))
                mig_indices_host=0

                mig_indices_host(1:nb_send_host) = mig_indices4all( vect_shift(isdm)+1 : vect_shift(isdm) + nb_send_host )

                call MPI_SEND (mig_indices_host, nb_send_host, MPI_INTEGER, isdm-1, etiquette, MPI_COMM_WORLD ,code_MPI)
                if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

             end if

             if ( rang_COMM_WORLD == isdm-1 ) then

                if ( nb_mig_slave == 0 ) cycle
                call MPI_RECV (mig_indices_slave, nb_mig_slave, MPI_INTEGER ,0,etiquette, MPI_COMM_WORLD ,statut,code_MPI)
                if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

             end if

          end do

          do isdm = 2, Nsdm

             if ( rang_COMM_WORLD == 0 ) then
                ! Nombre total d'éléments à envoyer
                nb_send_host=vect_nb_send_host(isdm)
         
                if (nb_send_host==0) cycle

                ! Allocation du vecteur dans lequel les indices des migrants vont être stockés
                if (allocated(mig_etats_host)) deallocate(mig_etats_host)
                allocate(mig_etats_host(6*nb_send_host))
                mig_etats_host=0.d0

                shift = 6*vect_shift(isdm)
                mig_etats_host(:) = mig_etats4all( shift+1 : shift + 6*nb_send_host )           

                call MPI_SEND (mig_etats_host, 6*nb_send_host, MPI_DOUBLE_PRECISION, isdm-1, etiquette, MPI_COMM_WORLD ,code_MPI)
                if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
             end if

             if ( rang_COMM_WORLD == isdm-1 ) then
                if ( nb_mig_slave ==0 ) cycle
                call MPI_RECV (mig_etats_slave, 6*nb_mig_slave, MPI_DOUBLE_PRECISION,0,etiquette, MPI_COMM_WORLD ,statut,code_MPI)
                if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

             end if

          end do

       end if
   
    else 

       first_real_step=.false. 

    end if

    ! Allocation de l'espace memoire pour stocker l'agregation des forces de cohesion sur les corps
    ! d'interface du sdm courant (3 reels pour chaque corps d'interface)
    if (allocated(Fg_RBDY2_interf_slave)) deallocate(Fg_RBDY2_interf_slave)
    allocate(Fg_RBDY2_interf_slave(3*nb_RBDY2_interf_slave))
    Fg_RBDY2_interf_slave = 0.D0

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
                                                      !12345678901234567890123456 
    character(len=26)                          :: IAM="DDM_2D::stock_Fg_liste_old"

    ! Gestion/récupération des F_gamma du pas précédent quand on le peut
    if (.not. first_real_step) then
       ! Allocation des listes de RBDY2 d'interface par sous-domaine du pas précédent
       if ( allocated(liste_RBDY2_interf_slave_old ) ) & 
            deallocate(liste_RBDY2_interf_slave_old)
       allocate(liste_RBDY2_interf_slave_old(nb_RBDY2_interf_slave), stat=err)
       if (err/=0) stop "Pb d'allocation de liste_RBDY2_interf_sdm_old"
       liste_RBDY2_interf_slave_old=liste_RBDY2_interf_slave
    
       if ( allocated(Fg_RBDY2_interf_slave_old) ) & 
            deallocate(Fg_RBDY2_interf_slave_old)
       allocate(Fg_RBDY2_interf_slave_old(3*nb_RBDY2_interf_slave), stat=err)
       if (err/=0) stop "Pb d'allocation de Fg_RBDY2_interf_slave_old"
       Fg_RBDY2_interf_slave_old=0.d0
       Fg_RBDY2_interf_slave_old=Fg_RBDY2_interf_slave
    end if

    if (.not. first_real_step) repartition_just_made = .true. ! repartition_just_made testé
                                                              ! puis mise à .false. dans set_F_gamma

 end subroutine stock_Fg_liste_old
!----------------------------------------------------------------

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
    integer                                :: icdan,antac,cdtac
    real(kind=8), dimension(2)             :: coordan, coordcd
    integer(kind=4), dimension(:), pointer :: cdan
    type(T_object)                         :: contact
    real(kind=8), dimension(2) :: coord_an_tmp

    do icdan=1, nb_rough_contact
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
       cdtac = cdan(1)
       antac = cdan(2)

       coordcd(1:2)      = coord_ct(1:2,cdtac)
       coord_an_tmp(1:2) = coord_ct(1:2,antac)
       coord_an_tmp(1)   = coord_an_tmp(1) + real(cdan(3),8)*xperiode 

       coord_cc(1:2,icdan) = (coordcd(1:2)+coord_an_tmp(1:2))*0.5
    end do

 end subroutine creation_coord_cc
!----------------------------------------------------------------

!----------------------------------------------------------------
! Par défaut, les frontières de la boite englobant les sous-domaines
! est définie à partir des positions des contacteurs de l'écantillon
! considéré.
! Si l'on précise les bornes en arguments (optionnels) de la routine,
! on peut alors forcer le quadrillage sous-jacent.
 subroutine caracteristique_domaine(Bdown_,Bup_,Bleft_,Bright_)

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! implicites : coord_ci
    real(kind=8), intent(in), optional :: Bdown_,Bup_,Bleft_,Bright_


    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine 
    ! implicites, real : Bleft, Bright, Bdown, Bdown, dim_sdm 

    integer :: ibody
    logical :: visible ! pour tester la visibilite d'un corps 

    Bleft  = coord_ci(1,1)
    Bright = coord_ci(1,1)
    Bdown  = coord_ci(2,1)
    Bup    = coord_ci(2,1)
    DO ibody=1, nbody
       visible=.TRUE.
       visible=get_visible(ibody)
       IF (.NOT.visible) CYCLE
       Bleft = MIN(coord_ci(1, ibody), Bleft )
       Bright= MAX(coord_ci(1, ibody), Bright)
       Bup   = MAX(coord_ci(2, ibody), Bup   )
       Bdown = MIN(coord_ci(2, ibody), Bdown )
    END DO

    if ( present(Bdown_) )  Bdown=Bdown_ 
    if ( present(Bup_) )    Bup=Bup_ 
    if ( present(Bleft_) )  Bleft=Bleft_
    if ( present(Bright_) ) Bright=Bright_ 

    !-----------------------------------------------!
    ! Caracteristiques du découpage en sous-domaines!
    !-----------------------------------------------!
 
    dim_sdm = (/ ((Bright-Bleft) / real(Nsdm1)) , ((Bup-Bdown) / real(Nsdm2)) /)

 end subroutine caracteristique_domaine
!----------------------------------------------------------------

!-----------------------------------------------------------------------------------------------------!
! Répartition d'entités dans les sous-domaines à partir de leurs coordonnées                          !
! Idée : construire un repère local 2D avec                                                           !
!		pour origine_loc le point O' avec OO' = (/ min(coord(dim=1)) , min(coord(dim=2)) /)   !
!		les distances reduites issues du repère global :                                      !
!							x' = [X - OO'(1)] / (L1/Nsdm1)                !
!							y' = [Y - OO'(2)] / (L2/Nsdm2)                !
! avec L1=max(coord(dim=1))-min(coord(dim=1), L2=max(coord(dim=2))-min(coord(dim=2)),                 !
!      Nsdmi = nombre de subdivisions du domaine suivant l'axe i                                      !
!                                                                                                     !
! Les parties entières des coord_ci reduites pour chaque objet sont un doublet (2D) ou un triplet (3D)!
! donnant le numero du sous-domaine contenant cet objet                                             - !
!-----------------------------------------------------------------------------------------------------!
!----------------------------------------------------------------
 subroutine ventillation_ds_sdm(Nsdm1_, Nsdm2_, Bleft_, Bdown_, dim_sdm_, & ! Caractéristiques du découpage 
                                nb_ligne, nb_entity, coord, repart_sdm,   & ! Objets à ventiller et vecteur résultat
                                visibilite)                                 ! Paramètre optionnel pour traiter les
                                                                            ! objets potentiellement invisibles
    implicit none
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine
    integer, intent(in)                                     :: Nsdm1_,Nsdm2_
    real(kind=8), intent(in)                                :: Bleft_,Bdown_
    real(kind=8), dimension(nbDIME), intent(in)             :: dim_sdm_
    integer, intent(in)                                     :: nb_ligne,nb_entity
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

    !-------------------------------------------------------------!
    ! Pour chaque objet, on determine le sdm auquel il appartient !
    !-------------------------------------------------------------!

    do ientity=1,nb_entity

       !! Récuperation du statut de visibilite du corps
       !visible = .true.
       !if (present(visibilite)) then
       !   if (visibilite) then
       !      visible=get_visible(ientity)
       !   end if
       !end if

       !! Si l'entité est invisible, on passe au suivant
       !if (.not. visible) cycle 

       ! On détermine le sdm auquel appartient l'entité
       position_reduite = (/ 1+floor( (coord(1,ientity)-Bleft_) / dim_sdm_(1) )   &
                           , 1+floor( (coord(2,ientity)-Bdown_) / dim_sdm_(2) ) /)

       ! Traitement des entités ayant une de leur coordonnées sur les bords
       ! de gauche, du bas, du haut ou de droite de la boite anglobante 
       if( position_reduite(1)<1 ) position_reduite(1)=1
       if( position_reduite(2)<1 ) position_reduite(2)=1
       if( position_reduite(1)>Nsdm1_ ) position_reduite(1)=Nsdm1_
       if( position_reduite(2)>Nsdm2_ ) position_reduite(2)=Nsdm2_
       repart_sdm(ientity) =(position_reduite(1)-1)*Nsdm2_ + position_reduite(2) 

      !! Si besoin, pour checker la répartition en sdm 
      ! print *, "Doublet :", position_reduite, "  Num sdm :", repart_sdm(ientity), "nb_entity=",nb_entity
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
    integer(kind=4)                                 :: icdtac,iantac

    logical, dimension(:), allocatable              :: mask_tmp ! Variable servant a stocker le masque obtenu pour
                                                                ! La ligne courante de body_particip
    integer                                         :: i        ! Indice de boucle anonyme
    logical                                         :: visible  ! Pour tester la visibilite d'un corps 
    type(T_object)                                  :: contact

 
    body_particip=0
    multiplicite=0

    do icdan=1, nb_rough_contact

       ! Éléments communs au cd et à l'an
       ! SDM du centre de contact
       isdm=repart_sdm_cc(icdan)
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)

       ! Traitement du candidat

       ! On recupere le numero du contacteur candidat
       icdtac = cdan(1)
       ! On en deduit le numero du corps candidat
       icdbdy=diskx2bdyty(1, icdtac)

       if (count(body_particip(:,icdbdy)==isdm)==0) then
          multiplicite(icdbdy)=multiplicite(icdbdy)+1
          body_particip(multiplicite(icdbdy), icdbdy)=isdm
       end if

       ! Cas de l'antagoniste

       ! On recupere le numero du contacteur antagoniste
       iantac = cdan(2)
       ! On en deduit le numero du corps antagoniste 
       ianbdy=diskx2bdyty(1, iantac)

       if (count(body_particip(:,ianbdy)==isdm)==0) then
          multiplicite(ianbdy)=multiplicite(ianbdy)+1
          body_particip(multiplicite(ianbdy), ianbdy)=isdm
       end if

    end do

    ! Traitement des neutrinos eventuels
    do ibody=1, nbody

       ! Récuperation du statut de visibilite du corps
       ! visible = .true.
       ! visible=get_visible(ibody)
       ! Si le corps courant est un neutrino (i.e. n'a pas de contact)
       if (multiplicite(ibody)==0) then
          ! On recupere le numero de sous-domaine auquel appartient le centre d'inertie du corps
          isdm = repart_sdm_ci(ibody)
          ! On inidique que le corps appartient a ce sous-domaine
          multiplicite(ibody)=multiplicite(ibody)+1
          body_particip(multiplicite(ibody), ibody)=isdm
       end if
   
    end do

    ! On initialise le masque a faux
    mask_particip=.false.

    ! On alloue l'espace memoire pour stocker le masque obtenu pour
    ! la ligne courante de body_particip
    allocate(mask_tmp(nbody)) 
    ! Pour chaque sous-domaine 
    do isdm=1, Nsdm

       ! Pour chaque ligne de body_particip
       do i=1, size(body_particip, dim=1)
          ! On cree un masque de participation pour la ligne courante
          mask_tmp=(body_particip(i, :) == isdm)
          ! On actualise le masque de participation
          mask_particip(:, isdm) = mask_particip(:, isdm) .or. mask_tmp
       end do

    end do

    deallocate(mask_tmp)

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
    integer(kind=4)                         :: nb_new !, first
    character(len=1000)                     :: cout

    if ( rang_COMM_WORLD /= 0 ) return

    migration_tab=0
    nb_migrations=0

    do ibody = 1,nbody
       nb_new = 0
       multi=multiplicite(ibody)
       multi_old=multiplicite_old(ibody)

       ! On stoque le num du premier sdm auquel participe ibody
       ! Si de nouveaux sous-domaines doivent gérer ibody, c'est le sdm 
       ! first qui devra envoyer le l'état du ibody à ces nouveaux gestionnaires
       ! (ou co-gestionnaires)

       ! Je pars du principe que seul l'hôte peut communiquer avec les autres processus.
       !first = body_particip_old(1,ibody)

       do imulti= 1, multi
          isdm=body_particip(imulti,ibody)

          if ( count(isdm==body_particip_old(1:multi_old,ibody)) /=0 ) cycle

          WRITE(cout,*) 'ibody=',ibody
          call logmes(cout)
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

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    logical, intent(inout) :: mig_tab_vide
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: isdm

    if ( rang_COMM_WORLD /= 0 ) return
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
    !integer(kind=4), dimension(max_multi_,nb_entity,Nsdm_), intent(in) :: migration_tab 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: ibody, i
    integer                                 :: isdm, shift, nb_in_colonne
    integer                                 :: nb_new_sdm, i_new_sdm, deb, icolonne
    integer, dimension(1)                   :: tmp
    real(kind=8), dimension(3)              :: Xbeg_tmp,Vbeg_tmp
    integer, dimension(:), allocatable      :: compteur

    if ( rang_COMM_WORLD /= 0 ) return

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

             shift = 6*( vect_shift(isdm) + ( compteur(isdm) - 1 ) )

             call get_vector_RBDY2('Xbeg_',ibody,Xbeg_tmp,3)
             mig_etats4all(shift+1:shift+3)=Xbeg_tmp

             !print *, "PROC",rang_COMM_WORLD,"Xbeg_tmp",Xbeg_tmp

             call get_vector_RBDY2('Vbeg_',ibody,Vbeg_tmp,3)
             mig_etats4all(shift+4:shift+6)=Vbeg_tmp
             !print *, "PROC",rang_COMM_WORLD,"Vbeg_tmp",Vbeg_tmp

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
    !integer(kind=4), dimension(max_multi_,nb_entity,Nsdm_), intent(in) :: migration_tab 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer                                 :: ibody, ibdy
    integer                                 :: isdm_new
    integer                                 :: nb_new_sdm, i_new_sdm
    real(kind=8), dimension(3)              :: Xbeg_tmp,Vbeg_tmp


    if ( nb_mig_slave == 0 ) return

    do ibdy = 1, nb_mig_slave

       ibody = mig_indices_slave(ibdy)
       !print *, "PROC",rang_COMM_WORLD,"STORE ibody",ibody


       Xbeg_tmp = 0.d0
       Xbeg_tmp = mig_etats_slave(6*(ibdy-1)+1: 6*(ibdy-1)+3)
       call put_vector_RBDY2('Xbeg_',ibody,Xbeg_tmp,3)
       !print *, "PROC",rang_COMM_WORLD,"Xbeg_tmp",mig_etats_slave(6*(ibdy-1)+1: 6*(ibdy-1)+3)

       Vbeg_tmp=0.d0
       Vbeg_tmp = mig_etats_slave(6*(ibdy-1)+4: 6*ibdy)
       call put_vector_RBDY2('Vbeg_',ibody,Vbeg_tmp,3)
       !print *, "PROC",rang_COMM_WORLD,"Vbeg_tmp",mig_etats_slave(6*(ibdy-1)+4: 6*ibdy)

    end do

 end subroutine fix_migrants
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine creation_splitted_rough

    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: nb_rough_contact, mask_contact, rough_contact 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: splitted_rough(isdm), nb_splitted_rough(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: icdan, icdbdy, ianbdy, isdm, compteur
    character(len=5)                          :: cdcol, ancol
    ! structure de l'anonymous container
    integer(kind=4),    dimension(:), pointer :: cdan
    integer(kind=4),    dimension(:), pointer :: xcdan
    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx
    type(T_object)                            :: contact
    character(len=100)                        :: cout

    if ( rang_COMM_WORLD/=0 ) return
 
    c5 => null()
    r8 => null()
    cx => null()

    ! Pour chaque contact
    do isdm=1,Nsdm
       nb_splitted_rough(isdm)=0
       do icdan=1,nb_rough_contact
          if (repart_sdm_cc(icdan)==isdm) then
             ! On récupère l'objet contact d'indice icdan
             contact = get_object(rough_contact,icdan)    
             ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
             cdan => get_i4_vector(contact)

             nb_splitted_rough(isdm)=nb_splitted_rough(isdm)+1

             allocate(xcdan(4))
             xcdan(1:2)=cdan(1:2)
             xcdan(4)  = cdan(3)
             ! numéro de corps associé au contacteur courant
             icdbdy=diskx2bdyty(1, cdan(1))
             ianbdy=diskx2bdyty(1, cdan(2))
             if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
                xcdan(3)=INTRF
             else
                xcdan(3)=NOINT
             end if
             call add_object_to_container(splitted_rough(isdm),nb_splitted_rough(isdm),c5,xcdan,r8,cx)
          end if
       end do

       ! pour tester que les splitted_rough sont bien remplis jusqu'au bout
       !print *, "display_splitted_rough"
       !call display_object_container(splitted_rough(isdm))
       WRITE(cout,*) "nb_splitted_rough(",isdm,")    =", nb_splitted_rough(isdm)
       call logmes(cout)
       call close_container(splitted_rough(isdm))
    end do

    nb_interactions4all=4*sum(nb_splitted_rough)
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
          interactions4all(4*(compteur-1)+1)=cdan(1)
          interactions4all(4*(compteur-1)+2)=cdan(2)
          interactions4all(4*(compteur-1)+3)=cdan(3)
          interactions4all(4*(compteur-1)+4)=cdan(4)
       end do
    end do

 end subroutine creation_splitted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_interactions_to_rough_in_DDM
    implicit none
    !--------------------------------------
    ! Arguments d'entrée de la subroutine
    call set_interactions_to_rough(interactions_slave,nb_interactions_slave)
 end subroutine set_interactions_to_rough_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_rough_contact
    implicit none
    !--------------------------------------
    if (rang_COMM_WORLD == 0) call erase_container(rough_contact)
 end subroutine erase_rough_contact
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_splitted_rough
    implicit none
    !--------------------------------------
    integer :: isdm

    if (rang_COMM_WORLD /=0 ) return
    do isdm=1,Nsdm
       call erase_container(splitted_rough(isdm))
    end do
 end subroutine erase_splitted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine get_list_INTRF_in_DDM
    implicit none
    !--------------------------------------

    ! Arguments de sortie de la subroutine
    ! implicite : nb_INTRF et list_INTRF

    ! Calcule le nombre de contacts taggés INTRF
    call get_nb_INTRF_DKDKx(nb_INTRF)
    
    if (allocated(list_INTRF)) deallocate(list_INTRF)
    allocate(list_INTRF(nb_INTRF))
    list_INTRF = 0

    ! Détermine la liste des contacts taggés INTRF
    call get_list_INTRF_DKDKx(nb_INTRF,list_INTRF)

    ! on passe la liste de contacts de la numerotation du module DKDKx a celle du module NLGS
    list_INTRF = list_INTRF + shift_icdan(i_dkdkx)

 end subroutine get_list_INTRF_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine RnodHRloc_list_in_DDM(istep,iiter)
    implicit none
    !--------------------------------------

    integer, intent(in) :: istep,iiter
    integer :: i_loc_RBDY2, ibody, multi, sdm
    !real(kind=8), dimension(3) :: R_DDM_TMP

    ! variable locale
    integer :: storage_reac

    ! on stocke les torseurs des reactions de contact dans Iaux
    storage_reac = iIaux_

    ! calcul des torseurs des reactions de contact
    call RnodHRloc_nlgs(list_INTRF, storage_reac)

    !write(slave_io, *) "===========ISTEP=",istep,"IITER=",iiter,"==========" 

    !do i_loc_RBDY2=1,nb_RBDY2_interf_slave

    !   R_DDM_TMP = 0.d0
    !   ibody=liste_RBDY2_interf_slave(i_loc_RBDY2)
    !   call get_vector_RBDY2('Iaux_',ibody,R_DDM_TMP,3)
    !   write(slave_io, *) "Iaux(",ibody,")=",R_DDM_TMP 
    !end do
    !write(slave_io, *) "=====================" 

 end subroutine RnodHRloc_list_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_local_free_vlocy_in_DDM
    implicit none
    !--------------------------------------

    call compute_local_free_vlocy(list_INTRF)

 end subroutine compute_local_free_vlocy_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_modified_mass_in_DDM
    
    implicit none
    
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 
    integer              :: i_rbdy2         ! indice de sous-domaine considere, indice de RBDY2

                             !12345678901234567890123456789012
    character(len=32) :: IAM='DDM_2D::set_modified_mass_in_DDM'

    do ibody = 1,nbody

       !am : le test suivant a ete recupere dans mod_DDM_MPI_2D...
       !     Il prevoit deux cas :
       !     1- si le processus courant est un esclave, on a besoin de modifier la masse des seuls corps visibles
       !     2- si le processus courant est le maitre, on doit modifier la masse de TOUS les corps, pour pouvoir ecrire le pb d'interface
       !        correctement!
       if ( .not. mask_in_slave(ibody) ) cycle

       ! La multiplicite du RBDY2 pour ce pas de temps est...
       multi=multiplicite(ibody)
   
       ! Comme on ne sait pas quelle était la multiplicité du RBDY2 au pas précédent,
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

       call set_mass(ibody,masses_courantes(ibody))
    
    !   print *, "corps duplique=", i_rbdy2
    !   print *, 
    !   print *, "masse de ref  =", masse_ref(ibody) 
    !   print *, 
    !   print *, "nouvelle masse=", get_mass(i_rbdy2)
    end do
    
 end subroutine set_modified_mass_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_interface ! (Nsdm,mask_particip(:,Nsdm), &
                              !  & nb_liens_interf, multiplicite, max_multi, nb_RBDY2_interf_sdm)
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
    ! integer(kind=4), dimension(Nsdm), intent(out)      :: nb_RBDY2_interf_sdm(isdm)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody,isdm,ipart,i_std_bdy,imulti,err,compteur
    integer(kind=4), dimension(:), allocatable :: isdm_multi_tab
    integer, dimension(:), allocatable :: nb_grains_multi
    logical                            :: visible
    character(len=100):: cout
                             !1234567890123456789012345
    character(len=25) :: IAM='DDM_2D::compute_interface'

    if (allocated(isdm_multi_tab)) deallocate(isdm_multi_tab)
    allocate(isdm_multi_tab(nbody), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de isdm_multi_tab")

    ! Traitement de l'interface pour chaque sous domaine
    do isdm=1,Nsdm
      
       !! On pourrait sans doute supprimer cette étape, mais elle fournit pour l'instant 
       !  une sécurité appréciable (redondance)
       ! Mise à 0 la valeur de multiplicité des RBDY2 qui ne sont pas dans isdm 
       isdm_multi_tab=merge(multiplicite,0,mask_particip(:,isdm))
       ! Calcul du nombre de RBDY2 d'interface (pour le sdm considéré)
       nb_RBDY2_interf_sdm(isdm)=count(isdm_multi_tab>=2)

       ! On peut maintenant allouer les champs d'interface du sous-domaine avec cette valeur!
       if ( associated(liste_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(liste_RBDY2_interf_sdm(isdm)%particule)
       allocate(liste_RBDY2_interf_sdm(isdm)%particule(nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de liste_RBDY2_interf_sdm(isdm)%particule")

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
       ! On va construire la portion du body particip a envoyer au processus
       ! gérant le sdm courant.
       if ( associated(body_particip_interf_sdm(isdm)%particule) ) & 
            deallocate(body_particip_interf_sdm(isdm)%particule)
       allocate(body_particip_interf_sdm(isdm)%particule(max_multi,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de body_particip_interf_sdm(isdm)%particule")
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

       compteur=0
       visible=.true.

       do ibody=1,nbody ! Boucle sur les corps (non-dupiqués)
          visible=mask_particip(ibody,isdm)
          if ( .not. visible .or. multiplicite(ibody) < 2 ) cycle
          compteur=compteur+1 

          ! On remplit le vecteur des RBDY2 d'interface par sous-domaine
          liste_RBDY2_interf_sdm(isdm)%particule(compteur)=ibody

          ! Idem pour body_particip
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
          body_particip_interf_sdm(isdm)%particule(:,compteur)=body_particip(:,ibody)
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
       end do
    end do

    ! Calcul du nombre de liens d'interface
    nb_liens_interf=0
    if (.not. allocated(nb_grains_multi)) &
              allocate(nb_grains_multi(max_multi), stat=err)
    if ( err/=0 ) call faterr(IAM, "probleme d'allocation de nb_grains_multi")

    ! Pour les cluster, c'est peut-être différent!
    do imulti=2,max_multi ! i.e. la valeur max de multiplicité si on 
                          ! ne considère pas des cluster qui sont en diagonale
                          ! ou en serpentin
       nb_grains_multi(imulti)=count(multiplicite(:)==imulti)
       nb_liens_interf= nb_liens_interf+(imulti-1)*nb_grains_multi(imulti)
    end do

    WRITE(cout,*) "nb_liens_interf=", nb_liens_interf
    call logmes(cout)

 end subroutine compute_interface
!----------------------------------------------------------------

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
! Cette routine a pour fonction de construire, à partir
! du body_particip_interf_slave, les listes des corps
! à envoyer/recevoir des sous-domaines adjacents et
! partageant des RBDY2 d'interface.
!----------------------------------------------------------------
 subroutine compute_shared_interfaces

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    !integer(kind=4)                            :: nb_linked_sdm 
    !integer(kind=4), dimension(:), allocatable :: liste_linked_sdm
    !integer(kind=4), dimension(:), allocatable :: nb_RBDY2_shared 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes à la subroutine
    integer  :: i, ipart, ilien, ibody, isdm, compteur, err
    integer  :: nb_grains_multi, nb_liens, imulti, multi
    integer, dimension(1)              :: tmp
    integer, dimension(:), allocatable :: compteur_sdm
    logical, dimension(:), allocatable :: mask_sdm

    character(len=100):: cout
                             !1234567890123456789012345678901234567890
    character(len=40) :: IAM="DDM_DECENT_2D::compute_shared_interfaces"

    if (allocated(mask_sdm)) deallocate(mask_sdm)
    allocate(mask_sdm(Nsdm),stat=err)
    if (err/=0) call faterr(IAM,"Erreur d'allocation de mask_sdm")
    mask_sdm=.false.

    do isdm = 1, Nsdm
       !if (isdm == rang_COMM_WORLD + 1) cycle
       if (count(body_particip_interf_slave(:,:) == isdm) >= 1) mask_sdm(isdm) = .true.
    end do


    ! Nombre de sdm avec lesquels des échanges seront nécessaires
    nb_linked_sdm = count(mask_sdm(:) .eqv. .true.)

    if (allocated(liste_linked_sdm)) deallocate(liste_linked_sdm)
    allocate(liste_linked_sdm(nb_linked_sdm),stat=err)
    if (err/=0) call faterr(IAM,"Erreur d'allocation de liste_linked_sdm")
    liste_linked_sdm=0

    ! Nombre de RBDY2 à échanger avec chacum de ces sdm
    if (allocated(nb_RBDY2_shared)) deallocate(nb_RBDY2_shared)
    allocate(nb_RBDY2_shared(nb_linked_sdm),stat=err)
    if (err/=0) call faterr(IAM,"Erreur d'allocation de nb_RBDY2_shared")
    nb_RBDY2_shared=0

    ! La liste des indices de ces derniers
    if (allocated(liste_RBDY2_shared)) deallocate(liste_RBDY2_shared)
    allocate(liste_RBDY2_shared(nb_linked_sdm),stat=err)
    if (err/=0) call faterr(IAM,"Erreur d'allocation de liste_RBDY2_shared")


    ! Liste des sdm avec lequels des échanges seront nécessaires
    compteur = 0
    do isdm = 1, Nsdm
       if (mask_sdm(isdm)  .eqv. .true.) then
          compteur = compteur + 1
          liste_linked_sdm(compteur) = isdm
       end if
    end do

    ! test paranoïaque
    if (compteur /= nb_linked_sdm) call faterr(IAM, "liste_linked_sdm a un pb!")

    ! Calcul du nombre de RBDY2 en commun avec chacun des sdm "liés"
    do i = 1, nb_linked_sdm
       isdm = liste_linked_sdm(i)
       nb_RBDY2_shared(i) = count(body_particip_interf_slave(:,:) == isdm)

       ! Allocation des listes de RBDY2 à échanger pour chaque sdm "lié"
       if (associated(liste_RBDY2_shared(i)%particule)) deallocate(liste_RBDY2_shared(i)%particule)
       allocate(liste_RBDY2_shared(i)%particule(nb_RBDY2_shared(i)),stat=err)
       if (err/=0) call faterr(IAM,"Erreur d'allocation de liste_RBDY2_shared")
       liste_RBDY2_shared(i)%particule=0
    end do

    if (allocated(compteur_sdm)) deallocate(compteur_sdm)
    allocate(compteur_sdm(nb_linked_sdm),stat=err)
    if (err/=0) call faterr(IAM,"Erreur d'allocation de compteur_sdm")
    compteur_sdm=0

    ! Remplissage des listes 
    do ipart = 1, nb_RBDY2_interf_slave
       do i = 1, real_max_multi
          if (body_particip_interf_slave(i,ipart) == 0) exit
          !if (body_particip_interf_slave(i,ipart) == rang_COMM_WORLD +1 ) cycle

          isdm = body_particip_interf_slave(i,ipart)
          tmp=maxloc(liste_linked_sdm(:), liste_linked_sdm(:) == isdm)
          compteur_sdm(tmp(1)) = compteur_sdm(tmp(1)) + 1

          liste_RBDY2_shared(tmp(1))%particule(compteur_sdm(tmp(1))) = liste_RBDY2_interf_slave(ipart)
       end do
    end do

    ! Test paranoïaque
    do isdm = 1, nb_linked_sdm
       if (compteur_sdm(isdm) /= nb_RBDY2_shared(isdm)) call faterr(IAM, "liste_RBDY2_shared a un pb! &
                                                         & compteur_sdm(isdm) /= nb_RBDY2_shared(isdm)")
    end do

    ! Calcul du nombre de grains d'interface
    nb_liens_interf_slave=0
    ! Pour les cluster, c'est peut-être différent!
    do imulti = 2, real_max_multi ! i.e. la valeur max de multiplicité si on 
                                ! ne considère pas des cluster qui sont en diagonale
                                ! ou en serpentin
       nb_grains_multi=count(merge(multiplicite, 0, mask_in_slave) == imulti)
       nb_liens_interf_slave = nb_liens_interf_slave + (imulti - 1)*nb_grains_multi
    end do

    WRITE(cout,*) "nb_liens_interf_slave=", nb_liens_interf_slave
    call logmes(cout)

    ! Allocation du tableau des sauts de vitesse d'interface
    if ( allocated(saut_V_interf_slave) ) deallocate(saut_V_interf_slave)
    allocate(saut_V_interf_slave(3, nb_liens_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de saut_V_interf_slave")
    saut_V_interf_slave = 0.d0


    ! Allocation du tableau des inverses des multiplicités des RBDY2 d'interface
    ! pour chaque lien correspondants à ces corps
    if ( allocated(inv_multi_interf_slave) ) deallocate(inv_multi_interf_slave)
    allocate(inv_multi_interf_slave(nb_liens_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de inv_multi_interf_slave")

    compteur = 0
    do ipart = 1, nb_RBDY2_interf_slave
       ibody = liste_RBDY2_interf_slave(ipart)
       multi = multiplicite(ibody)
       nb_liens = multi - 1
       do ilien = 1, nb_liens
          compteur = compteur + 1
          inv_multi_interf_slave(compteur) = 1.d0 / real(multi)
       end do
    end do
    ! Test paranoïaque
    if (compteur /= nb_liens_interf_slave) call faterr (IAM, " compteur /= nb_liens_interf_slave")

 end subroutine compute_shared_interfaces
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
!----------------------------------------------------------------
 subroutine prep_compute_DF_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! type(T_ligne_i), dimension(Nsdm)   :: liste_RBDY2_interf_sdm(isdm)
    ! integer, dimension(/ max_multi, nbody /) :: body_particip_interf_slave
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! Emplacement des RBDY2 dans liste_RBDY2_interf_slave pour chaque sdm
    ! auquel participe le RBDY2
    ! integer(kind=4), dimension(:,:), allocatable :: loc_RBDY2_in_liste_interf_slave
    ! Emplacement des liens 3d dans liste_linked_sdm  
    ! integer(kind=4), dimension(:,:), allocatable :: loc_lien3D_in_liste_linked_sdm  
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Argumentsinternes a la subroutine

    integer :: nb_liens_attendus ! nombre de liens de la particule dans le sdm "rang_COMM_WORLD + 1"
    integer :: i_liens, nb_liens ! indice du lien, nombres de liens par RBDY2
    integer :: imulti, multi     ! indice et multiplicité de la particule considérée
    integer :: ibody             ! indice du RBDY2 dans la liste globale
    integer :: i_parti           ! indice des particules d'interface
    integer :: isdm              ! indice sur les sous-domaines
    integer :: compteur          ! ce compteur doit être égal, en fin de boucle, à nb_liens_interf
    integer, dimension(1) :: tmp ! variable temporaire
    integer, dimension(2) :: sdm           ! sous domaine courant

                             !1234567890123456789012345678901
    character(len=31) :: IAM='DDM_DECENT_2D::compute_DF_gamma'

    integer :: i_part_sdm ! pour le write(slave_io,*)... en fin de routine
    integer :: err

    compteur=0

    ! Allocation du tableau des emplacements des RBDY2 d'interface dans
    ! liste_RBDY2_interf_slave pour chaque sdm auquel participe le RBDY2
    if ( allocated(loc_RBDY2_in_liste_interf_slave)) deallocate(loc_RBDY2_in_liste_interf_slave)
    allocate(loc_RBDY2_in_liste_interf_slave(real_max_multi,nb_RBDY2_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de loc_RBDY2_in_liste_interf_slave")
    loc_RBDY2_in_liste_interf_slave = 0

    ! Allocation du tableau des emplacements des RBDY2 d'interface dans
    ! liste_linked_sdm pour chaque ligne du bosy_particip du RBDY2
    if ( allocated(loc_RBDY2_in_liste_linked_sdm)) deallocate(loc_RBDY2_in_liste_linked_sdm)
    allocate(loc_RBDY2_in_liste_linked_sdm(real_max_multi,nb_RBDY2_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de loc_RBDY2_in_liste_linked_sdm")
    loc_RBDY2_in_liste_linked_sdm = 0

    ! Allocation du tableau des emplacements des liens3D d'interface dans liste_linked_sdm
    if ( allocated(loc_lien3D_in_liste_linked_sdm)) deallocate(loc_lien3D_in_liste_linked_sdm)
    allocate(loc_lien3D_in_liste_linked_sdm(2,nb_liens_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de loc_lien3D_in_liste_linked_sdm")
    loc_lien3D_in_liste_linked_sdm = 0

    ! Boucle sur tous les grains d'interface. Ils devraient contenir tous les liens 
    ! d'interface, ce que l'on vérifie à la fin de la boucle
    do i_parti = 1, nb_RBDY2_interf_slave

       ! RBDY2 sur lequel on travaille et sa multiplicité
       ibody = liste_RBDY2_interf_slave(i_parti)
       !print *, "ibody=",ibody
       multi = multiplicite(ibody)  ! on va tapper dans multiplicité directement 
     
       ! calcul du nombre de liens pour le corps courant
       nb_liens=multi-1

       ! pour chaque sous-domaine auquel appartient le corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip_interf_slave(isdm, i_parti)
          ! On va pècher l'indice du SDM dans liste_linked_sdm pour chaque 
          ! ligne du body_particip de ibody
          tmp=maxloc(liste_linked_sdm(:), liste_linked_sdm(:) == sdm(1))
          loc_RBDY2_in_liste_linked_sdm(isdm,i_parti) = tmp(1)

          ! On va pècher l'indice du RBDY2 ibody dans la liste des
          ! RBDY2 d'interface du sdm courant
          tmp=maxloc(liste_RBDY2_shared(tmp(1))%particule(:), &
                     liste_RBDY2_shared(tmp(1))%particule(:) == ibody)
          ! on le stocke
          loc_RBDY2_in_liste_interf_slave(isdm,i_parti)=tmp(1)
       end do

       ! pour chaque lien
       do i_liens=1, nb_liens
          compteur=compteur+1
 
          ! on recupere les duex sous-domaines associes au lien courant
          sdm(1)=body_particip_interf_slave(i_liens, i_parti)
          sdm(2)=body_particip_interf_slave(i_liens + 1, i_parti)

          tmp=maxloc(liste_linked_sdm(:), liste_linked_sdm(:)==sdm(1))
          loc_lien3D_in_liste_linked_sdm(1,compteur) = tmp(1)       
          tmp=maxloc(liste_linked_sdm(:), liste_linked_sdm(:)==sdm(2))
          loc_lien3D_in_liste_linked_sdm(2,compteur) = tmp(1)
       end do
    end do

    if (compteur /= nb_liens_interf_slave) then
       call FATERR(IAM, "compteur /= nb_liens_interf_slave")
    end if

 end subroutine prep_compute_DF_gamma
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
!----------------------------------------------------------------
 subroutine decentralise_compute_DF_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! type(T_ligne_i), dimension(Nsdm)   :: liste_RBDY2_interf_sdm(isdm)
    ! integer, dimension(/ max_multi, nbody /) :: body_particip
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! Liste des F_gamma signés par sous-domaines
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine

    integer :: nb_liens          ! nombre de liens de la particule considérée
    integer :: nb_liens_attendus ! nombre de liens de la particule dans le sdm "rang_COMM_WORLD + 1"
    integer :: i_liens           ! indice du lien
    integer :: imulti, multi     ! indice et multiplicité de la particule considérée
    integer :: ibody             ! indice du RBDY2 dans la liste globale
    integer :: i_parti           ! indice des particules d'interface
    integer :: isdm              ! indice sur les sous-domaines
    integer :: compteur          ! ce compteur doit être égal, en fin de boucle, à nb_liens_interf
    integer :: compteur_nb_liens_attendus ! ce compteur doit être égal, en fin de test à nb_liens_attendus
    integer, dimension(2) :: sdm           ! sous domaine courant
    integer, dimension(2) :: signe         ! pour faire alterner le signe
    real(kind=8), dimension(:), pointer :: mass ! matrice de masse du RBDY2 courant

                             !1234567890123456789012345678901
    character(len=31) :: IAM='DDM_DECENT_2D::compute_DF_gamma'

    real(kind=8), dimension(:), allocatable :: DF_gamma ! increment de F_gamma calcule a partir du saut de vitesse courant [| V |]
                                                          ! taille : 3*nb_liens
    real(kind=8), dimension(:), allocatable :: V_jump   ! saut de vitesse courant
                                                          ! taille : 3,*nb_liens

    type(G_matrix) :: A                ! matrice du systeme a resoudre pour calculer delta_F_gamma : A*DF = M*[| V |]
    character(len=8) :: matrix_type    ! type de stockage pour la G_matrix (i.e. matrice diagonale ou bande symetrique)
    integer :: bw                      ! largeur de bande de la matrice, dans le cas avec plusieurs liens
    integer, dimension(1) :: perm_fake ! tableau de permutation bidon utilise pour delcarer la G_matrix 
    integer :: info                    ! variable indiquant si la resolution du systeme s'est bien passee
 
    integer :: i  ! indice de boucle
    integer :: i_part_sdm ! pour le write(slave_io,*)... en fin de routine

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

    compteur=0
    saut_V_interf_slave=0.d0

    ! on fixe la regle pour l'affectation des signes
    signe(1)= +1
    signe(2)= -1

    ! Boucle sur tous les grains d'interface. Ils devraient contenir tous les liens 
    ! d'interface, ce que l'on vérifie à la fin de la boucle
    do i_parti = 1, nb_RBDY2_interf_slave

       ! RBDY2 sur lequel on travaille et sa multiplicité
       ibody = liste_RBDY2_interf_slave(i_parti)
       !print *, "ibody=",ibody
       multi = multiplicite(ibody)  ! on va tapper dans multiplicité directement 

       ! Récupération des paramètres inertiels
       mass => get_ptr_mass(ibody)
     
       ! calcul du nombre de liens pour le corps courant
       nb_liens=multi-1

       ! allocation de l'espace memoire pour les tableaux :
       !   * le vecteur DF_gamma
       allocate(DF_gamma(3*nb_liens))
       !   * le vecteur [| V |]
       allocate(V_jump(3*nb_liens))

       ! pour chaque lien
       do i_liens=1, nb_liens
          compteur=compteur+1

          ! Calcul du saut de vitesse pour le lien courant
          V_jump(3*(i_liens - 1) + 1 : 3*i_liens) = &
             signe(1) * DATA_RBDY2_interf_slave(loc_lien3D_in_liste_linked_sdm(1,compteur))%&
                                             particule(:,loc_RBDY2_in_liste_interf_slave(i_liens,i_parti)) + &
             signe(2) * DATA_RBDY2_interf_slave(loc_lien3D_in_liste_linked_sdm(2,compteur))%&
                                            particule(:,loc_RBDY2_in_liste_interf_slave(i_liens+1,i_parti))

          ! Pour le post-traitement, stockage des sauts de vitesses des grains d'interface
          saut_V_interf_slave(:, compteur)=V_jump(3*(i_liens - 1) + 1 : 3*i_liens)
       end do

       ! on choisi la methode de resolution selon le nombre de liens
       if (nb_liens < 8) then

          SELECT CASE(nb_liens)
 
          CASE(1)
             DF_gamma(1:3) = 0.5d0*mass(1:3)*V_jump(1:3)

          CASE(2)
             DF_gamma(1:3)  = mass(1:3) * ( d2_3*V_jump(1:3) + d1_3*V_jump(4:6) )
             DF_gamma(4:6)  = mass(1:3) * ( d1_3*V_jump(1:3) + d2_3*V_jump(4:6) )
          CASE(3)
             DF_gamma(1:3)  = mass(1:3) * ( d3_4*V_jump(1:3) + d1_2*V_jump(4:6) + d1_4*V_jump(7:9) )
             DF_gamma(4:6)  = mass(1:3) * ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d1_2*V_jump(7:9) )
             DF_gamma(7:9)  = mass(1:3) * ( d1_4*V_jump(1:3) + d1_2*V_jump(4:6) + d3_4*V_jump(7:9) )
          CASE(4)
             DF_gamma(1:3)  = mass(1:3) * ( d4_5*V_jump(1:3) + d3_5*V_jump(4:6) + d2_5*V_jump(7:9) + d1_5*V_jump(10:12) )
             DF_gamma(4:6)  = mass(1:3) * ( d3_5*V_jump(1:3) + d6_5*V_jump(4:6) + d4_5*V_jump(7:9) + d2_5*V_jump(10:12) )
             DF_gamma(7:9)  = mass(1:3) * ( d2_5*V_jump(1:3) + d4_5*V_jump(4:6) + d6_5*V_jump(7:9) + d3_5*V_jump(10:12) )
             DF_gamma(10:12)= mass(1:3) * ( d1_5*V_jump(1:3) + d2_5*V_jump(4:6) + d3_5*V_jump(7:9) + d4_5*V_jump(10:12) )
          CASE(5)
             DF_gamma(1:3)  = mass(1:3) * ( d5_6*V_jump(1:3) + d2_3*V_jump(4:6) + d1_2*V_jump(7:9) + d1_3*V_jump(10:12) + &
                                            d1_6*V_jump(13:15)                                                              )
             DF_gamma(4:6)  = mass(1:3) * ( d2_3*V_jump(1:3) + d4_3*V_jump(4:6) + 1.d0*V_jump(7:9) + d2_3*V_jump(10:12) + &
                                            d1_3*V_jump(13:15)                                                              )
             DF_gamma(7:9)  = mass(1:3) * ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d3_2*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                            d1_2*V_jump(13:15)                                                              )
             DF_gamma(10:12)= mass(1:3) * ( d1_3*V_jump(1:3) + d2_3*V_jump(4:6) + 1.d0*V_jump(7:9) + d4_3*V_jump(10:12) + &
                                            d2_3*V_jump(13:15)                                                              )
             DF_gamma(13:15)= mass(1:3) * ( d1_6*V_jump(1:3) + d1_3*V_jump(4:6) + d1_2*V_jump(7:9) + d2_3*V_jump(10:12) + &
                                            d5_6*V_jump(13:15)                                                              )
          CASE(6)
             DF_gamma(1:3)  = mass(1:3) * ( d6_7*V_jump(1:3) + d5_7*V_jump(4:6) + d4_7*V_jump(7:9) + d3_7*V_jump(10:12) + &
                                            d2_7*V_jump(13:15)+d1_7*V_jump(16:18)                                           )
             DF_gamma(4:6)  = mass(1:3) * ( d5_7*V_jump(1:3) +d10_7*V_jump(4:6)+ d8_7*V_jump(7:9) + d6_7*V_jump(10:12) +  &
                                            d4_7*V_jump(13:15)+d2_7*V_jump(16:18)                                           )
             DF_gamma(7:9)  = mass(1:3) * ( d4_7*V_jump(1:3) + d8_7*V_jump(4:6) +d12_7*V_jump(7:9) + d9_7*V_jump(10:12) + &
                                            d6_7*V_jump(13:15)+d3_7*V_jump(16:18)                                           )
             DF_gamma(10:12)= mass(1:3) * ( d3_7*V_jump(1:3) + d6_7*V_jump(4:6) + d9_7*V_jump(7:9) + d12_7*V_jump(10:12)+ &
                                            d8_7*V_jump(13:15)+d4_7*V_jump(16:18)                                           )
             DF_gamma(13:15)= mass(1:3) * ( d2_7*V_jump(1:3) + d4_7*V_jump(4:6) + d6_7*V_jump(7:9) + d8_7*V_jump(10:12) + &
                                            d10_7*V_jump(13:15)+d5_7*V_jump(16:18)                                           )
             DF_gamma(16:18)= mass(1:3) * ( d1_7*V_jump(1:3) + d2_7*V_jump(4:6) + d3_7*V_jump(7:9) + d4_7*V_jump(10:12) + &
                                            d5_7*V_jump(13:15)+d6_7*V_jump(16:18)                                           )
          CASE(7)
             DF_gamma(1:3)  = mass(1:3) * ( d7_8*V_jump(1:3) + d3_4*V_jump(4:6) + d5_8*V_jump(7:9) + d1_2*V_jump(10:12) + &
                                            d3_8*V_jump(13:15)+d1_4*V_jump(16:18)+ d1_8*V_jump(19:21)                       )
             DF_gamma(4:6)  = mass(1:3) * ( d3_4*V_jump(1:3) + d3_2*V_jump(4:6) + d5_4*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                            d3_4*V_jump(13:15)+d1_2*V_jump(16:18)+ d1_4*V_jump(19:21)                       )
             DF_gamma(7:9)  = mass(1:3) * ( d5_8*V_jump(1:3) + d5_4*V_jump(4:6) +d15_8*V_jump(7:9) + d3_2*V_jump(10:12) + &
                                            d9_8*V_jump(13:15)+d3_4*V_jump(16:18)+ d3_8*V_jump(19:21)                       )
             DF_gamma(10:12)= mass(1:3) * ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d3_2*V_jump(7:9) + 2.d0*V_jump(10:12)+ &
                                            d3_2*V_jump(13:15)+1.d0*V_jump(16:18)+ d1_2*V_jump(19:21)                       )
             DF_gamma(13:15)= mass(1:3) * ( d3_8*V_jump(1:3) + d3_4*V_jump(4:6) + d9_8*V_jump(7:9) + d3_2*V_jump(10:12) + &
                                            d15_8*V_jump(13:15)+d5_4*V_jump(16:18)+ d5_8*V_jump(19:21)                       )
             DF_gamma(16:18)= mass(1:3) * ( d1_4*V_jump(1:3) + d1_2*V_jump(4:6) + d3_4*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                            d5_4*V_jump(13:15)+d3_2*V_jump(16:18)+ d3_4*V_jump(19:21)                       )
             DF_gamma(19:21)= mass(1:3) * ( d1_8*V_jump(1:3) + d1_4*V_jump(4:6) + d3_8*V_jump(7:9) + d1_2*V_jump(10:12) + &
                                            d5_8*V_jump(13:15)+d3_4*V_jump(16:18)+ d7_8*V_jump(19:21)                       )
          END SELECT

       else
          ! au moins 8 liens : matrice bande symetrique
          matrix_type = 'sym_band'
          bw=4

          ! declaration de la matrice
          call G_declare(A, matrix_type, 3*nb_liens, .false., perm_fake, perm_fake) 

          ! definition de la largeur de bande de la matrice
          call G_settle(A, (/ 1, bw /))

          ! construction de la G_matrix (et initialisation a 0)
          call G_build(A)

          ! remplissage de la matrice

          ! dans tous les cas : partie diagonale
          do i=1, 3*nb_liens
             call G_add(A, 2.d0, i, i)
          end do
          ! si on a plusieurs liens : l'extra diagonale non nulle
          do i=4, 3*nb_liens
             call G_add(A, -1.d0, i - bw + 1, i)
          end do

          ! remplissage du tableau intermediraire utilise par la resolution
          call G_store(A)

          ! calcul du second membre du systeme : M*[|V|], dans le vecteur solution
          do i_liens=1, nb_liens
             DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens) = mass(1 : 3)*V_jump(3*(i_liens - 1) + 1 : 3*i_liens)
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

       ! remise a 0 du DF_gamma du corps courant
       DFg_RBDY2_interf_slave(:, i_parti) = 0.d0


       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       ! Commenté pour des raisons d'efficassité
       !
       !! On va déterminer si le sdm "rang_COMM_WORLD+1" supporte 1 ou 2 liens
       !tmp = maxloc( body_particip_interf_slave(:,i_parti),body_particip_interf_slave(:,i_parti)==rang_COMM_WORLD+1 )
       !if (tmp(1) < multi) then
       !   if (tmp(1) > 1) then
       !      nb_liens_attendus = 2
       !   else if (tmp(1) == 1) then
       !      nb_liens_attendus = 1
       !   else
       !      call faterr(IAM, "On a tmp(1) < 1 !!")
       !   end if
       !else if (tmp(1) == multi ) then
       !   nb_liens_attendus = 1
       !else
       !   call faterr(IAM, "On a tmp(1) > multi ??!!")
       !end if             
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       ! calcul de la resultante des DF_gamma pour chaque copie du corps
       ! pour chaque lien
       !compteur_nb_liens_attendus = 0
       do i_liens = 1, nb_liens
 
          ! on recupere les duex sous-domaines associes au lien courant
          sdm(1)=body_particip_interf_slave(i_liens, i_parti)
          sdm(2)=body_particip_interf_slave(i_liens + 1, i_parti)

          if (sdm(1) == rang_COMM_WORLD+1) then
             !compteur_nb_liens_attendus = compteur_nb_liens_attendus + 1
             DFg_RBDY2_interf_slave(1:3,i_parti) = &
                DFg_RBDY2_interf_slave(1:3,i_parti) + &
                (-1) * signe(1) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)

          else if (sdm(2) == rang_COMM_WORLD+1) then
             !compteur_nb_liens_attendus = compteur_nb_liens_attendus + 1
             DFg_RBDY2_interf_slave(1:3,i_parti) = &
                DFg_RBDY2_interf_slave(1:3,i_parti) + &
                (-1) * signe(2) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)
          else
             ! Rien à faire, ce lien ne concerne pas la copie de ibody du sdm "rang_COMM_WORLD+1"
          end if
       end do

       !if (compteur_nb_liens_attendus /= nb_liens_attendus) &
       !   call faterr(IAM, " compteur_nb_liens_attendus /= nb_liens_attendus !")

       ! liberation de l'espace memoire occupe par les tableaux :
       !   * DF_gamma
       deallocate(DF_gamma)
       !   * [| V |]
       deallocate(V_jump)
    end do

!    Test déjà fait dans prep_compute_DF_gamma
!    if (compteur /= nb_liens_interf_slave) then
!       call FATERR(IAM, "compteur /= nb_liens_interf_slave")
!    end if

    !write(slave_io,*) "---------------------------------------------------------------"
    !write(slave_io,*) IAM, "On va afficher DFg_RBDY2_interf_slave du sdm courant"
    !do i_part_sdm = 1, nb_RBDY2_interf_slave
    !   write(slave_io,*)  "Particule ::", liste_RBDY2_interf_slave(i_part_sdm)
    !   write(slave_io,*) DFg_RBDY2_interf_slave(1:3,i_part_sdm)
    !end do
    !write(slave_io,*) "---------------------------------------------------------------"

                  
 end subroutine decentralise_compute_DF_gamma
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

!----------------------------------------------------------------
 subroutine decentralise_compute_F_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! type(T_ligne_i), dimension(Nsdm)   :: liste_RBDY2_interf_sdm(isdm)
    ! integer, dimension(/ max_multi, nbody /) :: body_particip
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! Liste des F_gamma signés par sous-domaines
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine

    integer :: nb_liens          ! nombre de liens de la particule considérée
    integer :: nb_liens_attendus ! nombre de liens de la particule dans le sdm "rang_COMM_WORLD + 1"
    integer :: i_liens           ! indice du lien
    integer :: imulti, multi     ! indice et multiplicité de la particule considérée
    integer :: ibody             ! indice du RBDY2 dans la liste globale
    integer :: i_parti           ! indice des particules d'interface
    integer :: isdm              ! indice sur les sous-domaines
    integer :: compteur          ! ce compteur doit être égal, en fin de boucle, à nb_liens_interf
    integer :: compteur_nb_liens_attendus ! ce compteur doit être égal, en fin de test à nb_liens_attendus
    integer, dimension(2) :: sdm           ! sous domaine courant
    integer, dimension(2) :: signe         ! pour faire alterner le signe
    real(kind=8), dimension(:), pointer :: mass ! matrice de masse du RBDY2 courant

                             !1234567890123456789012345678901
    character(len=31) :: IAM='DDM_DECENT_2D::compute_DF_gamma'

    real(kind=8), dimension(:), allocatable :: DF_gamma ! increment de F_gamma calcule a partir du saut de vitesse courant [| V |]
                                                          ! taille : 3*nb_liens
    real(kind=8), dimension(:), allocatable :: V_jump   ! saut de vitesse courant
                                                          ! taille : 3,*nb_liens

    type(G_matrix) :: A                ! matrice du systeme a resoudre pour calculer delta_F_gamma : A*DF = M*[| V |]
    character(len=8) :: matrix_type    ! type de stockage pour la G_matrix (i.e. matrice diagonale ou bande symetrique)
    integer :: bw                      ! largeur de bande de la matrice, dans le cas avec plusieurs liens
    integer, dimension(1) :: perm_fake ! tableau de permutation bidon utilise pour delcarer la G_matrix 
    integer :: info                    ! variable indiquant si la resolution du systeme s'est bien passee
 
    integer :: i  ! indice de boucle
    integer :: i_part_sdm ! pour le write(slave_io,*)... en fin de routine

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

    compteur=0
    saut_V_interf_slave=0.d0

    ! on fixe la regle pour l'affectation des signes
    signe(1)= +1
    signe(2)= -1

    ! Boucle sur tous les grains d'interface. Ils devraient contenir tous les liens 
    ! d'interface, ce que l'on vérifie à la fin de la boucle
    do i_parti = 1, nb_RBDY2_interf_slave

       ! RBDY2 sur lequel on travaille et sa multiplicité
       ibody = liste_RBDY2_interf_slave(i_parti)
       !print *, "ibody=",ibody
       multi = multiplicite(ibody)  ! on va tapper dans multiplicité directement 

       ! Récupération des paramètres inertiels
       mass => get_ptr_mass(ibody)
     
       ! calcul du nombre de liens pour le corps courant
       nb_liens=multi-1

       ! allocation de l'espace memoire pour les tableaux :
       !   * le vecteur DF_gamma
       allocate(DF_gamma(3*nb_liens))
       !   * le vecteur [| V |]
       allocate(V_jump(3*nb_liens))

       ! pour chaque lien
       do i_liens=1, nb_liens
          compteur=compteur+1

          ! Calcul du saut de vitesse pour le lien courant
          V_jump(3*(i_liens - 1) + 1 : 3*i_liens) = &
             signe(1) * DATA_RBDY2_interf_slave(loc_lien3D_in_liste_linked_sdm(1,compteur))%&
                                             particule(:,loc_RBDY2_in_liste_interf_slave(i_liens,i_parti)) + &
             signe(2) * DATA_RBDY2_interf_slave(loc_lien3D_in_liste_linked_sdm(2,compteur))%&
                                            particule(:,loc_RBDY2_in_liste_interf_slave(i_liens+1,i_parti))

          ! Pour le post-traitement, stockage des sauts de vitesses des grains d'interface
          saut_V_interf_slave(:, compteur)=V_jump(3*(i_liens - 1) + 1 : 3*i_liens)
       end do

       ! on choisi la methode de resolution selon le nombre de liens
       if (nb_liens < 8) then

          SELECT CASE(nb_liens)
 
          CASE(1)
             DF_gamma(1:3) = 0.5d0*V_jump(1:3)

          CASE(2)
             DF_gamma(1:3)  =  ( d2_3*V_jump(1:3) + d1_3*V_jump(4:6) )
             DF_gamma(4:6)  =  ( d1_3*V_jump(1:3) + d2_3*V_jump(4:6) )
          CASE(3)
             DF_gamma(1:3)  =  ( d3_4*V_jump(1:3) + d1_2*V_jump(4:6) + d1_4*V_jump(7:9) )
             DF_gamma(4:6)  =  ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d1_2*V_jump(7:9) )
             DF_gamma(7:9)  =  ( d1_4*V_jump(1:3) + d1_2*V_jump(4:6) + d3_4*V_jump(7:9) )
          CASE(4)
             DF_gamma(1:3)  =  ( d4_5*V_jump(1:3) + d3_5*V_jump(4:6) + d2_5*V_jump(7:9) + d1_5*V_jump(10:12) )
             DF_gamma(4:6)  =  ( d3_5*V_jump(1:3) + d6_5*V_jump(4:6) + d4_5*V_jump(7:9) + d2_5*V_jump(10:12) )
             DF_gamma(7:9)  =  ( d2_5*V_jump(1:3) + d4_5*V_jump(4:6) + d6_5*V_jump(7:9) + d3_5*V_jump(10:12) )
             DF_gamma(10:12)=  ( d1_5*V_jump(1:3) + d2_5*V_jump(4:6) + d3_5*V_jump(7:9) + d4_5*V_jump(10:12) )
          CASE(5)
             DF_gamma(1:3)  =  ( d5_6*V_jump(1:3) + d2_3*V_jump(4:6) + d1_2*V_jump(7:9) + d1_3*V_jump(10:12) + &
                                 d1_6*V_jump(13:15)                                                              )
             DF_gamma(4:6)  =  ( d2_3*V_jump(1:3) + d4_3*V_jump(4:6) + 1.d0*V_jump(7:9) + d2_3*V_jump(10:12) + &
                                 d1_3*V_jump(13:15)                                                              )
             DF_gamma(7:9)  =  ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d3_2*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                 d1_2*V_jump(13:15)                                                              )
             DF_gamma(10:12)=  ( d1_3*V_jump(1:3) + d2_3*V_jump(4:6) + 1.d0*V_jump(7:9) + d4_3*V_jump(10:12) + &
                                 d2_3*V_jump(13:15)                                                              )
             DF_gamma(13:15)=  ( d1_6*V_jump(1:3) + d1_3*V_jump(4:6) + d1_2*V_jump(7:9) + d2_3*V_jump(10:12) + &
                                 d5_6*V_jump(13:15)                                                              )
          CASE(6)
             DF_gamma(1:3)  =  ( d6_7*V_jump(1:3) + d5_7*V_jump(4:6) + d4_7*V_jump(7:9) + d3_7*V_jump(10:12) + &
                                 d2_7*V_jump(13:15)+d1_7*V_jump(16:18)                                           )
             DF_gamma(4:6)  =  ( d5_7*V_jump(1:3) +d10_7*V_jump(4:6)+ d8_7*V_jump(7:9) + d6_7*V_jump(10:12) +  &
                                 d4_7*V_jump(13:15)+d2_7*V_jump(16:18)                                           )
             DF_gamma(7:9)  =  ( d4_7*V_jump(1:3) + d8_7*V_jump(4:6) +d12_7*V_jump(7:9) + d9_7*V_jump(10:12) + &
                                 d6_7*V_jump(13:15)+d3_7*V_jump(16:18)                                           )
             DF_gamma(10:12)=  ( d3_7*V_jump(1:3) + d6_7*V_jump(4:6) + d9_7*V_jump(7:9) + d12_7*V_jump(10:12)+ &
                                 d8_7*V_jump(13:15)+d4_7*V_jump(16:18)                                           )
             DF_gamma(13:15)=  ( d2_7*V_jump(1:3) + d4_7*V_jump(4:6) + d6_7*V_jump(7:9) + d8_7*V_jump(10:12) + &
                                 d10_7*V_jump(13:15)+d5_7*V_jump(16:18)                                           )
             DF_gamma(16:18)=  ( d1_7*V_jump(1:3) + d2_7*V_jump(4:6) + d3_7*V_jump(7:9) + d4_7*V_jump(10:12) + &
                                 d5_7*V_jump(13:15)+d6_7*V_jump(16:18)                                           )
          CASE(7)
             DF_gamma(1:3)  =  ( d7_8*V_jump(1:3) + d3_4*V_jump(4:6) + d5_8*V_jump(7:9) + d1_2*V_jump(10:12) + &
                                 d3_8*V_jump(13:15)+d1_4*V_jump(16:18)+ d1_8*V_jump(19:21)                       )
             DF_gamma(4:6)  =  ( d3_4*V_jump(1:3) + d3_2*V_jump(4:6) + d5_4*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                 d3_4*V_jump(13:15)+d1_2*V_jump(16:18)+ d1_4*V_jump(19:21)                       )
             DF_gamma(7:9)  =  ( d5_8*V_jump(1:3) + d5_4*V_jump(4:6) +d15_8*V_jump(7:9) + d3_2*V_jump(10:12) + &
                                 d9_8*V_jump(13:15)+d3_4*V_jump(16:18)+ d3_8*V_jump(19:21)                       )
             DF_gamma(10:12)=  ( d1_2*V_jump(1:3) + 1.d0*V_jump(4:6) + d3_2*V_jump(7:9) + 2.d0*V_jump(10:12)+ &
                                 d3_2*V_jump(13:15)+1.d0*V_jump(16:18)+ d1_2*V_jump(19:21)                       )
             DF_gamma(13:15)=  ( d3_8*V_jump(1:3) + d3_4*V_jump(4:6) + d9_8*V_jump(7:9) + d3_2*V_jump(10:12) + &
                                 d15_8*V_jump(13:15)+d5_4*V_jump(16:18)+ d5_8*V_jump(19:21)                       )
             DF_gamma(16:18)=  ( d1_4*V_jump(1:3) + d1_2*V_jump(4:6) + d3_4*V_jump(7:9) + 1.d0*V_jump(10:12) + &
                                 d5_4*V_jump(13:15)+d3_2*V_jump(16:18)+ d3_4*V_jump(19:21)                       )
             DF_gamma(19:21)=  ( d1_8*V_jump(1:3) + d1_4*V_jump(4:6) + d3_8*V_jump(7:9) + d1_2*V_jump(10:12) + &
                                          d5_8*V_jump(13:15)+d3_4*V_jump(16:18)+ d7_8*V_jump(19:21)                       )
          END SELECT

       else
          ! au moins 8 liens : matrice bande symetrique
          matrix_type = 'sym_band'
          bw=4

          ! declaration de la matrice
          call G_declare(A, matrix_type, 3*nb_liens, .false., perm_fake, perm_fake) 

          ! definition de la largeur de bande de la matrice
          call G_settle(A, (/ 1, bw /))

          ! construction de la G_matrix (et initialisation a 0)
          call G_build(A)

          ! remplissage de la matrice

          ! dans tous les cas : partie diagonale
          do i=1, 3*nb_liens
             call G_add(A, 2.d0, i, i)
          end do
          ! si on a plusieurs liens : l'extra diagonale non nulle
          do i=4, 3*nb_liens
             call G_add(A, -1.d0, i - bw + 1, i)
          end do

          ! remplissage du tableau intermediraire utilise par la resolution
          call G_store(A)

          ! calcul du second membre du systeme : M*[|V|], dans le vecteur solution
          do i_liens=1, nb_liens
             DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens) = mass(1 : 3)*V_jump(3*(i_liens - 1) + 1 : 3*i_liens)
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

       ! remise a 0 du DF_gamma du corps courant
       DFg_RBDY2_interf_slave(:, i_parti) = 0.d0


       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       ! Commenté pour des raisons d'efficassité
       !
       !! On va déterminer si le sdm "rang_COMM_WORLD+1" supporte 1 ou 2 liens
       !tmp = maxloc( body_particip_interf_slave(:,i_parti),body_particip_interf_slave(:,i_parti)==rang_COMM_WORLD+1 )
       !if (tmp(1) < multi) then
       !   if (tmp(1) > 1) then
       !      nb_liens_attendus = 2
       !   else if (tmp(1) == 1) then
       !      nb_liens_attendus = 1
       !   else
       !      call faterr(IAM, "On a tmp(1) < 1 !!")
       !   end if
       !else if (tmp(1) == multi ) then
       !   nb_liens_attendus = 1
       !else
       !   call faterr(IAM, "On a tmp(1) > multi ??!!")
       !end if             
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       
       ! calcul de la resultante des DF_gamma pour chaque copie du corps
       ! pour chaque lien
       !compteur_nb_liens_attendus = 0
       do i_liens = 1, nb_liens
 
          ! on recupere les duex sous-domaines associes au lien courant
          sdm(1)=body_particip_interf_slave(i_liens, i_parti)
          sdm(2)=body_particip_interf_slave(i_liens + 1, i_parti)

          if (sdm(1) == rang_COMM_WORLD+1) then
             !compteur_nb_liens_attendus = compteur_nb_liens_attendus + 1
             DFg_RBDY2_interf_slave(1:3,i_parti) = &
                DFg_RBDY2_interf_slave(1:3,i_parti) + &
                (-1) * signe(1) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)

          else if (sdm(2) == rang_COMM_WORLD+1) then
             !compteur_nb_liens_attendus = compteur_nb_liens_attendus + 1
             DFg_RBDY2_interf_slave(1:3,i_parti) = &
                DFg_RBDY2_interf_slave(1:3,i_parti) + &
                (-1) * signe(2) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)
          else
             ! Rien à faire, ce lien ne concerne pas la copie de ibody du sdm "rang_COMM_WORLD+1"
          end if
       end do

       !if (compteur_nb_liens_attendus /= nb_liens_attendus) &
       !   call faterr(IAM, " compteur_nb_liens_attendus /= nb_liens_attendus !")

       ! liberation de l'espace memoire occupe par les tableaux :
       !   * DF_gamma
       deallocate(DF_gamma)
       !   * [| V |]
       deallocate(V_jump)
    end do

!    Test déjà fait dans prep_compute_DF_gamma
!    if (compteur /= nb_liens_interf_slave) then
!       call FATERR(IAM, "compteur /= nb_liens_interf_slave")
!    end if

    !write(slave_io,*) "---------------------------------------------------------------"
    !write(slave_io,*) IAM, "On va afficher DFg_RBDY2_interf_slave du sdm courant"
    !do i_part_sdm = 1, nb_RBDY2_interf_slave
    !   write(slave_io,*)  "Particule ::", liste_RBDY2_interf_slave(i_part_sdm)
    !   write(slave_io,*) DFg_RBDY2_interf_slave(1:3,i_part_sdm)
    !end do
    !write(slave_io,*) "---------------------------------------------------------------"

 end subroutine decentralise_compute_F_gamma
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine comp_Vfree_Fg_list_RBDY2_in_DDM
    implicit none
    integer            :: ibody,i
    integer            :: i_part_sdm
    real(kind=8), dimension(3) :: DF_gamma

    do i_part_sdm=1,nb_RBDY2_interf_slave
       ibody=liste_RBDY2_interf_slave(i_part_sdm)
       ! On stocke le DF_gamma qui nous intéresse pour ce corps
       ! Après avoir divisé par le pas de temps pour convertir
       ! l'impulsion calculée en force moyenne sur le pas de temps
       DF_gamma=(1.D0/H)*DFg_RBDY2_interf_slave(1:3,i_part_sdm)
       call put_vector_RBDY2('Fext_',ibody,DF_gamma,3)
       call comp_free_vlocy_one_RBDY2(ibody)
    end do

    ! Mise a jour des F_gamma pour chaque copie du corps
    Fg_RBDY2_interf_slave = Fg_RBDY2_interf_slave + pack(DFg_RBDY2_interf_slave,.true.)

    !write(slave_io,*) " " 
    !!write(slave_io,*) " Fg_RBDY2_interf_slave =", Fg_RBDY2_interf_slave
    !do i=1, nb_RBDY2_interf_slave
    !   ibody=liste_RBDY2_interf_slave(i)
    !   write(slave_io,*) " Fg_RBDY2_interf_slave(", ibody,") =", Fg_RBDY2_interf_slave(3*(i-1)+1:3*i)
    !end do

 end subroutine comp_Vfree_Fg_list_RBDY2_in_DDM
!----------------------------------------------------------------

!!----------------------------------------------------------------
! subroutine comp_vfree_e_prjj_invM_t_Fg_p_Vfree_list_RBDY2_in_DDM
!    implicit none
!    integer            :: ibody,i
!    integer            :: i_part_sdm
!    real(kind=8), dimension(3) :: DF_gamma
!    real(kind=8), dimension(3) :: V_tmp
!
!    do ibody=1,nbody
!       if (.not. mask_in_slave(ibody)) cycle
!       call get_vector_RBDY2('Vfree',ibody,V_tmp,3)
!       call put_vector_RBDY2('Vaux_',ibody,V_tmp,3)
!    end do
!
!    do i_part_sdm=1,nb_RBDY2_interf_slave
!       ibody=liste_RBDY2_interf_slave(i_part_sdm)
!       ! On stocke le DF_gamma qui nous intéresse pour ce corps
!       ! DF_gamma est homogene a une impulsion
!       DF_gamma=DFg_RBDY2_interf_slave(1:3,i_part_sdm)
!       !call put_vector_RBDY2('Iaux_',ibody,DF_gamma,3)
!       call comp_vlocy(ibody, iVaux_e_invM_t_Ropt_p_Vfree, DF_gamma)
!    end do
!
!    call compute_local_free_vlocy(list_INTRF,storage_vlocy=iVaux_)
!
!    ! Mise a jour des F_gamma pour chaque copie du corps
!    Fg_RBDY2_interf_slave = pack(DFg_RBDY2_interf_slave,.true.)
!
!    !write(slave_io,*) " " 
!    !!write(slave_io,*) " Fg_RBDY2_interf_slave =", Fg_RBDY2_interf_slave
!    !do i=1, nb_RBDY2_interf_slave
!    !   ibody=liste_RBDY2_interf_slave(i)
!    !   write(slave_io,*) " Fg_RBDY2_interf_slave(", ibody,") =", Fg_RBDY2_interf_slave(3*(i-1)+1:3*i)
!    !end do
!
!    !write(slave_io,*) " " 
!    !do i=1, nb_RBDY2_interf_slave
!    !   write(slave_io,*) " Fg_RBDY2_interf_slave(", 3*(i-1)+1,":",3*i,") =", Fg_RBDY2_interf_slave(3*(i-1)+1:3*i)
!    !end do
!    !write(slave_io,*) " Fg_RBDY2_interf_slave 1 =", Fg_RBDY2_interf_slave(1:3)
!    !write(slave_io,*) " Fg_RBDY2_interf_slave 2 =", Fg_RBDY2_interf_slave(4:6)
!    !write(slave_io,*) "-------sortie CGLP-------- "
!
! end subroutine comp_vfree_e_prjj_invM_t_Fg_p_Vfree_list_RBDY2_in_DDM
!!----------------------------------------------------------------

!----------------------------------------------------------------
!am: calcul de la convergence avec reduction des normes calculees dans chaque sdm
 subroutine check_convergence_in_DDM(is_converged)
 
    implicit none
 
    ! variable de sortie
    ! pour statuer si on a converge, variable connue par tous les sdm mais stockee par le processus 0
    logical, intent(out) :: is_converged
 
    ! variables locales
 
    ! nombre de contacts actifs (Nactif: par sdm, Nactif4all: pour tout l'echantillon)
    integer :: Nactif !, Nactif4all
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
 

    real(kind=8) :: egluing_4all_square
 
    ! tolerance du NLGS
    real(kind=8) :: tol
 
    ! pour recuperer le resulat de prep_check_nlgs
    integer :: iconv
    ! pour statuer si NLGS a converge, decision prise par le processus 0
    logical :: nlgs_converged
    ! pour statuer si le probleme d'interface a converge, decision prise par le processus 0
    logical :: intrf_converged
                             !1234567890123456789012345678901234567890
    character(len=40) :: IAM="DDM_MPI_DECENT::check_convergence_in_DDM"

    ! initialisation du statut de convergence
    is_converged = .false.
    intrf_converged = .false.

    ! on prepare la verification de la convergence, dans le sdm courant
    call prep_check_nlgs(iconv)
 
    !print*, 'rang_COMM_WORLD ', rang_COMM_WORLD, ' : iconv=', iconv
 
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
 
    ! on calcule le nombre total de contacts actifs et on l'envoie a tous les sdm
    call MPI_ALLREDUCE(Nactif, Nactif4all, 1, MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

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
 
    ! on calcule :
    !    * pour chaque somme (SumWRWR, ...) la somme des sommes collectees sur chaque sdm et on l'envoie sur le processus 0
    call MPI_REDUCE(Sums, Sums4all, 5, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    !    * pour chaque max (MaxDVDV, MaxDVDVRR) le max des max collectes sur chaque sdm et on l'envoie sur le processus 0
    call MPI_REDUCE(Maxima, Maxima4all, 2, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    if (NSCDD) then
       call decentralise_prep_egluing
       ! on calcule :
       !    * pour chaque somme des "contributions" des sous-domaines à gluing
       !      pour plus de détails voir présentation théorique de cette estimateur
       call MPI_REDUCE(egluing_slave_square, egluing_4all_square, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
       if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    end if

    if (rang_COMM_WORLD == 0) then
 
       ! on calcule les quantites necessaires pour statuer de la convergence sur le maitre
       call compute_convergence_norms_nlgs(Nactif4all, Sums4all(1), Sums4all(2), Maxima4all(1), &
                                           Sums4all(3), Sums4all(4), Maxima4all(2), Sums4all(5), tol, &
                                           QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR) 


       ! on teste la convergence du NLGS 
       call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, nlgs_converged)

       ! Test de la convergence du problème d'interface
       if (Nsdm > 1 .and. NSCDD) then
          !print *, "egluing_slave_square", egluing_4all_square
          egluing = sqrt(egluing_4all_square) / nb_liens_interf

          if (egluing < tol) then
             intrf_converged = .true.
          else if (egluing > tol) then
             ! Rien à faire dans ce cas
          else if (egluing <= 0.d0) then
             call faterr(IAM, "egluing <= 0 !!")
          end if
       else
          intrf_converged = .true.
       end if

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
    call MPI_BCAST(is_converged, 1, MPI_LOGICAL, 0, MPI_COMM_WORLD, code_MPI)
 
 end subroutine check_convergence_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine decentralise_prep_egluing
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! saut_V_interf_slave
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine
    integer(kind=4)           :: ilien, err
    character(len=28)         :: clout ! pour calculer le nom du fichier
                                     !12345678901234567890123456789012345678901234
    character(len=44)         :: IAM="DDM_MPI_DECENT_2D::decentralise_prep_egluing"
    real(kind=8), allocatable :: saut_V_interf_slave_pondere(:,:)

    ! Allocation du tableau des sauts de vitesse d'interface pondérées
    allocate(saut_V_interf_slave_pondere(2, nb_liens_interf_slave), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de saut_V_interf_slave_pondere")

    do ilien = 1, nb_liens_interf_slave
       saut_V_interf_slave_pondere(1:2,ilien) = inv_multi_interf_slave(ilien) * &
                                               saut_V_interf_slave(1:2,ilien)
    end do

    !write(slave_io,*) "---------------------------------------------------------------"
    !write(slave_io,*) IAM, "On va afficher les saut_V_interf_slave"
    !do ilien = 1, nb_liens_interf_slave
    !   write(slave_io,*)  "saut_V_interf_slave(:,",ilien,") =", saut_V_interf_slave(:,ilien)
    !   write(slave_io,*)  "saut_V_interf_slave_pondere(:,",ilien,") =", saut_V_interf_slave_pondere(:,ilien)
    !   write(slave_io,*)  "---------------------------------------------------------"
    !end do
    !write(slave_io,*) "---------------------------------------------------------------"


    egluing_slave_square=0.d0
    if (nb_liens_interf_slave > 0) then

      ! Calcul de l'erreur globale de recollement
      egluing_slave_square = dot_product(saut_V_interf_slave_pondere(1, :), saut_V_interf_slave(1, :)) + &
                             dot_product(saut_V_interf_slave_pondere(2, :), saut_V_interf_slave(2, :))
    end if

    deallocate(saut_V_interf_slave_pondere)

 end subroutine decentralise_prep_egluing
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_F_gamma 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! Ce truc ci dessous va changer de forme
    ! Fg_RBDY2_interf_slave
    ! Fg_RBDY2_interf_slave_old
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
    integer                    :: i_std_bdy
    real(kind=8), dimension(3) :: F_gamma


    if (.not. repartition_just_made) then
       do i_part_sdm=1,nb_RBDY2_interf_slave
          ! On récupère le numéro du RBDY2 dans le sous-domaine 
          ibody=liste_RBDY2_interf_slave(i_part_sdm)
          ! Recallage des indice pour le cas séquaentiel multidomaine
          F_gamma=(1.D0/H)*Fg_RBDY2_interf_slave( 3*(i_part_sdm-1)+1 : 3*i_part_sdm )
          call put_vector_RBDY2('Fext_',ibody,F_gamma,3)
       end do
   
    else ! On vient donc de faire une nouvelle répartition en sous-domaines

       do i_part_sdm=1,nb_RBDY2_interf_slave
          ! On récuère le numéro du RBDY2 dans le sous-domaine 
          ibody=liste_RBDY2_interf_slave(i_part_sdm)
          ! Recallage des indice pour le cas séquaentiel multidomaine
          if (any(liste_RBDY2_interf_slave_old(:)==ibody)) then
             tmp=maxloc(liste_RBDY2_interf_slave_old(:), &
                                   liste_RBDY2_interf_slave_old(:)==ibody)
             old_i_part_sdm=tmp(1)

             Fg_RBDY2_interf_slave( 3*(i_part_sdm-1)+1 : 3*i_part_sdm ) = &
                Fg_RBDY2_interf_slave_old( 3*(old_i_part_sdm-1)+1 : 3*old_i_part_sdm)
             ! On stocke le F_gamma qui nous intéresse pour ce corps
             ! Après avoir divisé par le pas de temps pour convertir
             ! l'impulsion calculée en force moyenne sur le pas de temps
             F_gamma=(1.D0/H)*Fg_RBDY2_interf_slave_old( 3*(old_i_part_sdm-1)+1 : 3*old_i_part_sdm )
             call put_vector_RBDY2('Fext_',ibody,F_gamma,3)
          end if
       end do
    end if

    repartition_just_made=.false. 

 end subroutine set_F_gamma
!----------------------------------------------------------------

!! set_F_gamma pour la version non incrementale de la NSCDD
!!----------------------------------------------------------------
! subroutine set_F_gamma_NI
!    implicit none
!
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! Arguments d'entree de la subroutine 
!    ! arguments implicites
!    ! Ce truc ci dessous va changer de forme
!    ! Fg_RBDY2_interf_slave
!    ! Fg_RBDY2_interf_slave_old
!    ! argument "explicite"
!
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! Arguments de sortie de la subroutine
! 
!    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    ! Arguments internes a la subroutine
!    integer                    :: isdm
!    integer, dimension(1)      :: tmp
!    integer                    :: ibody
!    integer                    :: i_part_sdm
!    integer                    :: old_i_part_sdm
!    integer                    :: i_std_bdy
!    real(kind=8), dimension(3) :: F_gamma
!    real(kind=8), dimension(3) :: V_tmp
!
!    do ibody=1,nbody
!       if (.not. mask_in_slave(ibody)) cycle
!       call get_vector_RBDY2('Vfree',ibody,V_tmp,3)
!       call put_vector_RBDY2('Vaux_',ibody,V_tmp,3)
!       !call nullify_reac(ibody,iIaux_)
!    end do
!
!    if (.not. repartition_just_made) then
!       do i_part_sdm=1,nb_RBDY2_interf_slave
!          ! On récupère le numéro du RBDY2 dans le sous-domaine 
!          ibody=liste_RBDY2_interf_slave(i_part_sdm)
!          ! Recallage des indice pour le cas séquaentiel multidomaine
!          F_gamma=Fg_RBDY2_interf_slave(3*(i_part_sdm-1)+1 : 3*i_part_sdm)
!          call comp_vlocy(ibody, iVaux_e_invM_t_Ropt_p_Vfree, F_gamma)
!       end do
!    else ! On vient donc de faire une nouvelle répartition en sous-domaines
!       do i_part_sdm=1,nb_RBDY2_interf_slave
!          ! On récuère le numéro du RBDY2 dans le sous-domaine 
!          ibody=liste_RBDY2_interf_slave(i_part_sdm)
!          ! Recallage des indice pour le cas séquaentiel multidomaine
!          if (any(liste_RBDY2_interf_slave_old(:)==ibody)) then
!             tmp=maxloc(liste_RBDY2_interf_slave_old(:), &
!                         liste_RBDY2_interf_slave_old(:)==ibody)
!             old_i_part_sdm=tmp(1)
!
!             Fg_RBDY2_interf_slave( 3*(i_part_sdm-1)+1 : 3*i_part_sdm ) = &
!                Fg_RBDY2_interf_slave_old( 3*(old_i_part_sdm-1)+1 : 3*old_i_part_sdm)
!             ! On stocke le F_gamma qui nous intéresse pour ce corps
!             ! Après avoir divisé par le pas de temps pour convertir
!             ! l'impulsion calculée en force moyenne sur le pas de temps
!             F_gamma=Fg_RBDY2_interf_slave_old(3*(old_i_part_sdm-1)+1 : 3*old_i_part_sdm)
!             call comp_vlocy(ibody, iVaux_e_invM_t_Ropt_p_Vfree, F_gamma)
!          end if
!       end do
!    end if
!
!    call compute_local_free_vlocy(list_INTRF,storage_vlocy=iVaux_)
!
!    repartition_just_made=.false. 
!
! end subroutine set_F_gamma_NI
!!----------------------------------------------------------------

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
!----------------------------------------------------------------
 subroutine decentralise_prep_exchange_inloop

    implicit none

    integer :: isdm
    integer :: i, compteur, err
                             !1234567890123456789012345678901234567890123456789012 
    character(len=52) :: IAM='DDM_MPI_DECENT_2D::decentralise_prep_exchange_inloop'

    if ( rang_COMM_WORLD == 0 ) then

       do isdm=1,Nsdm
          vect_nb_recv_host(isdm) = nb_RBDY2_interf_sdm(isdm)
       end do

       nb_recv_host=sum(vect_nb_recv_host)

       nb_send_host=nb_recv_host

       ! les tranches envoyées (DFg) ou reçues (V) par l'hôte
       ! auront la forme. 
       vect_nb_send_host=3*nb_RBDY2_interf_sdm
       vect_nb_recv_host=3*nb_RBDY2_interf_sdm
       
       ! Vecteur de décalage d'indice pour le SCATTERV
       vect_shift=0
       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
       end do
       
    end if

    !---------------------
    ! VITESSES D'INTERFACE
    !---------------------

    ! on alloue l'espace memoire pour stocker l'agregation des vitesses des corps d'interface du sdm courant
    ! (6 reels pour chaque corps d'interface)
    if (allocated(DATA_RBDY2_interf_slave)) deallocate(DATA_RBDY2_interf_slave)
    allocate(DATA_RBDY2_interf_slave(nb_linked_sdm), stat=err)
    if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave")

    if (allocated(DATA_RBDY2_interf_slave_2send)) deallocate(DATA_RBDY2_interf_slave_2send)
    allocate(DATA_RBDY2_interf_slave_2send(nb_linked_sdm - 1), stat=err)
    if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave_2send")

    if (allocated(DATA_RBDY2_interf_slave_2recv)) deallocate(DATA_RBDY2_interf_slave_2recv)
    allocate(DATA_RBDY2_interf_slave_2recv(nb_linked_sdm - 1), stat=err)
    if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave2recv")

    compteur = 0

    do isdm = 1, nb_linked_sdm
       if (associated(DATA_RBDY2_interf_slave(isdm)%particule)) deallocate(DATA_RBDY2_interf_slave(isdm)%particule)
       allocate(DATA_RBDY2_interf_slave(isdm)%particule(3,nb_RBDY2_shared(isdm)), stat=err)
       if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave(isdm)%particule")
       DATA_RBDY2_interf_slave(isdm)%particule = 0.d0

       ! Allocation des tableaux temporaires pour les envois/réceptions MPI
       ! On skip le sous-domaine courant
       i = liste_linked_sdm(isdm)
       if (i == rang_COMM_WORLD + 1) cycle

       compteur = compteur + 1

       if (associated(DATA_RBDY2_interf_slave_2send(compteur)%particule)) &
            deallocate(DATA_RBDY2_interf_slave_2send(compteur)%particule)
       allocate(DATA_RBDY2_interf_slave_2send(compteur)%particule(3*nb_RBDY2_shared(isdm)), stat=err)
       if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave_2send(isdm)%particule")
       DATA_RBDY2_interf_slave_2send(compteur)%particule = 0.d0

       if (associated(DATA_RBDY2_interf_slave_2recv(compteur)%particule)) &
            deallocate(DATA_RBDY2_interf_slave_2recv(compteur)%particule)
       allocate(DATA_RBDY2_interf_slave_2recv(compteur)%particule(3*nb_RBDY2_shared(isdm)), stat=err)
       if (err/=0) call faterr(IAM, " erreur d'allocation de DATA_RBDY2_interf_slave_2recv(isdm)%particule")
       DATA_RBDY2_interf_slave_2recv(compteur)%particule = 0.d0
    end do

    ! Ce test prend comme convention nb_linked_sdm = sdm_connexes + sdm courant.
    ! Cas particuliers :
    !                    - Nsdm = 1, nb_linked_sdm = 0
    !                    - aucun RBDY2 d'interface (--> nb_RBDY2_interf_slave=0)
    !                      --> nb_linked_sdm = 0
    ! Test paranoiaque
    SELECT CASE(Nsdm)
    CASE (1)
       if (compteur /= 0) call faterr(IAM, "Nsdm=1 et compteur/=0 !!")
    CASE DEFAULT
       if (compteur /= (nb_linked_sdm - 1) .and. nb_RBDY2_interf_slave /= 0) &
                       call faterr(IAM, "Nsdm/=1 et compter/=nb_linked_sdm-1 !!")
    END SELECT
   
    ! Allocation de l'espace memoire pour stocker l'agregation des increments des forces 
    ! de cohesion sur les corps d'interface du sdm courant
    if (allocated(DFg_RBDY2_interf_slave)) deallocate(DFg_RBDY2_interf_slave)
    allocate(DFg_RBDY2_interf_slave(3,nb_RBDY2_interf_slave), stat=err)
    if (err/=0) call faterr(IAM, " erreur d'allocation de DFg_RBDY2_interf_slave")
    DFg_RBDY2_interf_slave = 0.d0

 end subroutine decentralise_prep_exchange_inloop 
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
!----------------------------------------------------------------
 subroutine exchange_interface(id_vlocy)
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
    ! DATA_RBDY2_interf_slave
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 


    integer :: ibody, isdm, i, i_std_bdy, i_RBDY2_interf_sdm, multi, compteur, err
    logical :: visible
    integer, parameter :: etiquette = 100
    integer, dimension(MPI_STATUS_SIZE) :: statut
    real(kind=8), dimension(3) :: vlocy

    !                         1234567890123456789012345678901234567
    character(len=37) :: IAM='DDM_MPI_DECENT_2D::exchange_interface'


    call start_itimer(id_prep_gather_V)

    compteur = 0
    do i = 1, nb_linked_sdm
       if (liste_linked_sdm(i) /= rang_COMM_WORLD+1) then
          compteur = compteur + 1
          do i_RBDY2_interf_sdm=1,nb_RBDY2_shared(i)
             ibody=liste_RBDY2_shared(i)%particule(i_RBDY2_interf_sdm)

             !write(slave_io,*)  "Particule ::", liste_RBDY2_shared(i)%particule(i_RBDY2_interf_sdm)
             !write(slave_io,*)  "SDM       ::", liste_linked_sdm(i)

             call get_vector_RBDY2(id_vlocy, ibody, vlocy, 3)
             DATA_RBDY2_interf_slave_2send(compteur)%particule(3*(i_RBDY2_interf_sdm - 1) + 1 :&
                                                             3*i_RBDY2_interf_sdm) = vlocy ! real(rang_COMM_WORLD + 1)

            !write(slave_io,*) "V::", DATA_RBDY2_interf_slave_2send(compteur)%particule(6*(i_RBDY2_interf_sdm - 1) + 1 :&
            !                                                                        6*i_RBDY2_interf_sdm)

          end do

       ! A voir si on ne peut pas le faire plus proprement ailleurs
       else if (liste_linked_sdm(i) == (rang_COMM_WORLD +1)) then
          do i_RBDY2_interf_sdm=1,nb_RBDY2_shared(i)
             ibody=liste_RBDY2_shared(i)%particule(i_RBDY2_interf_sdm)

             !write(slave_io,*)  "Particule ::", liste_RBDY2_shared(i)%particule(i_RBDY2_interf_sdm)
             !write(slave_io,*)  "SDM       ::", liste_linked_sdm(i)

             call get_vector_RBDY2(id_vlocy, ibody, vlocy, 3)
             DATA_RBDY2_interf_slave(i)%particule(1:3,i_RBDY2_interf_sdm) = vlocy

             !write(slave_io,*) "V::", DATA_RBDY2_interf_slave(i)%particule(1:3,i_RBDY2_interf_sdm)

          end do
       else
          call faterr(IAM,"Probleme dans le test sur liste_linked_sdm")
       end if
    end do

    !write(slave_io,*) "---------------------------------------------------------------"

    ! Ce test prend comme convention nb_linked_sdm = sdm_conexes + sdm courant.
    ! Cas particuliers :
    !                    - Nsdm = 1, nb_linked_sdm = 0
    !                    - aucun RBDY2 d'interface (--> nb_RBDY2_interf_slave=0)
    !                      --> nb_linked_sdm = 0
    ! Test paranoÃ¯aque
    SELECT CASE(Nsdm)
    CASE (1)
       if (compteur /= 0) call faterr(IAM, "Nsdm=1 et compteur/=0 !!")
    CASE DEFAULT
       if (compteur /= (nb_linked_sdm - 1) .and. nb_RBDY2_interf_slave /= 0) &
                       call faterr(IAM, "Nsdm/=1 et compter/=nb_linked_sdm-1 !!")
    END SELECT

    call stop_itimer(id_prep_gather_V)

    call start_itimer(id_gather_V)
    
    ! Echanges entre processus --> foire d'empoigne?? (30/01/12), pas tant (12/07/12)


    !write(slave_io,*) "---------------------------------------------------------------"
    !write(slave_io,*) IAM, "On va afficher les vecteurs de vitesse recus"

    compteur = 0
    do i = 1, nb_linked_sdm

       if (liste_linked_sdm(i) /= rang_COMM_WORLD+1) then
          compteur = compteur + 1
          call MPI_SENDRECV(DATA_RBDY2_interf_slave_2send(compteur)%particule(1:3*nb_RBDY2_shared(i)), 3*nb_RBDY2_shared(i), &
                            MPI_DOUBLE_PRECISION, liste_linked_sdm(i)-1, etiquette,                     &
                            DATA_RBDY2_interf_slave_2recv(compteur)%particule(1:3*nb_RBDY2_shared(i)), 3*nb_RBDY2_shared(i), &
                            MPI_DOUBLE_PRECISION, liste_linked_sdm(i)-1, etiquette,                     &
                            MPI_COMM_WORLD, statut, code_MPI) 

          DATA_RBDY2_interf_slave(i)%particule =  &
           reshape(DATA_RBDY2_interf_slave_2recv(compteur)%particule, (/3,nb_RBDY2_shared(i)/))

          !! Pour l'affichage
          !do i_RBDY2_interf_sdm = 1, nb_RBDY2_shared(i)
          !   write(slave_io,*) "Particule ::", liste_RBDY2_shared(i)%particule(i_RBDY2_interf_sdm)
          !   write(slave_io,*) "V::", DATA_RBDY2_interf_slave(i)%particule(1:3,i_RBDY2_interf_sdm)
          !end do

       else if (liste_linked_sdm(i) == (rang_COMM_WORLD + 1)) then
          ! rien à faire
       else
          call faterr(IAM, "Probleme dans liste_linked_sdm (2)")
       end if

    end do

    !write(slave_io,*) "---------------------------------------------------------------"

    ! Ce test prend comme convention nb_linked_sdm = sdm_connexes + sdm courant.
    ! Cas particuliers :
    !                    - Nsdm = 1, nb_linked_sdm = 0
    !                    - aucun RBDY2 d'interface (--> nb_RBDY2_interf_slave=0)
    !                      --> nb_linked_sdm = 0
    ! Test paranoïaque
    SELECT CASE(Nsdm)
    CASE (1)
       if (compteur /= 0) call faterr(IAM, "Nsdm=1 et compteur/=0 !!")
    CASE DEFAULT
       if (compteur /= (nb_linked_sdm - 1) .and. nb_RBDY2_interf_slave /= 0) &
                       call faterr(IAM, "Nsdm/=1 et compter/=nb_linked_sdm-1 !!")
    END SELECT

    call stop_itimer(id_gather_V)
    
 end subroutine exchange_interface
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!

!----------------------------------------------------------------
 subroutine compute_Rddm_in_ddm
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody,nsdm) :: body_particip
    ! integer(kind=4), dimension(nbody)      :: multiplicite 
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, i_loc_RBDY2, multi, imulti, sdm       
    real(kind=8), dimension(3) :: R_DDM_TMP

                             !12345678901234567890123456789012345678
    character(len=38) :: IAM="DDM_MPI_DECENT_2D::compute_Rddm_in_DDM"

    DFg_RBDY2_interf_slave = 0.d0
    do i_loc_RBDY2=1,nb_RBDY2_interf_slave

       R_DDM_TMP = 0.d0
       ibody=liste_RBDY2_interf_slave(i_loc_RBDY2)
       multi=multiplicite(ibody)

       do imulti=1,multi
          sdm=body_particip_interf_slave(imulti, i_loc_RBDY2)
          if (sdm == rang_COMM_WORLD + 1) cycle

          R_DDM_TMP(1:3) = R_DDM_TMP(1:3) +                                     &
           DATA_RBDY2_interf_slave(loc_RBDY2_in_liste_linked_sdm(imulti,i_loc_RBDY2))% &
            particule(1:3, loc_RBDY2_in_liste_interf_slave(imulti,i_loc_RBDY2))

       end do

       DFg_RBDY2_interf_slave(1:3,i_loc_RBDY2) = R_DDM_TMP(1:3) - &
                                    Fg_RBDY2_interf_slave(3*(i_loc_RBDY2-1)+1 : 3*i_loc_RBDY2) 

       Fg_RBDY2_interf_slave(3*(i_loc_RBDY2-1)+1 : 3*i_loc_RBDY2) = R_DDM_TMP(1:3)

    end do

 end subroutine compute_Rddm_in_ddm
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine comp_Vfree_Rddm_list_RBDY2_in_DDM
    implicit none
    integer            :: ibody
    integer            :: i_part_sdm
    real(kind=8), dimension(3) :: DF_gamma

    do i_part_sdm=1,nb_RBDY2_interf_slave
       ibody=liste_RBDY2_interf_slave(i_part_sdm)
       ! On stocke le DF_gamma qui nous intéresse pour ce corps
       ! Après avoir divisé par le pas de temps pour convertir
       ! l'impulsion calculée en force moyenne sur le pas de temps
       DF_gamma=(1.D0/H)*DFg_RBDY2_interf_slave(1:3,i_part_sdm)
       call put_vector_RBDY2('Fext_',ibody,DF_gamma,3)
       call comp_free_vlocy_one_RBDY2(ibody)
    end do

 end subroutine comp_Vfree_Rddm_list_RBDY2_in_DDM
!----------------------------------------------------------------


!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!
!----------------------------------------------------------------
 subroutine decentralise_fix_V_interface
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody,nsdm) :: body_particip
    ! integer(kind=4), dimension(nbody)      :: multiplicite 

 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, i_RBDY2_sdm, i_loc_RBDY2, &
               imulti, multi, isdm       
    logical :: visible
    real(kind=8), dimension(3) :: V_RBDY2_tmp

                             !12345678901234567890123456789012345678901234567
    character(len=47) :: IAM="DDM_MPI_DECENT_3D::decentralise_fix_V_interface"

    do i_loc_RBDY2=1,nb_RBDY2_interf_slave

       V_RBDY2_tmp = 0.d0
       ibody=liste_RBDY2_interf_slave(i_loc_RBDY2)
       multi=multiplicite(ibody)
       do imulti=1,multi 
             V_RBDY2_tmp(1:3) = V_RBDY2_tmp(1:3) +                                     &
              DATA_RBDY2_interf_slave(loc_RBDY2_in_liste_linked_sdm(imulti,i_loc_RBDY2))% &
               particule(1:3, loc_RBDY2_in_liste_interf_slave(imulti,i_loc_RBDY2)) / multi 
       end do
       call put_vector_RBDY2('V____',ibody,V_RBDY2_tmp,3)
    end do

 end subroutine decentralise_fix_V_interface
!----------------------------------------------------------------
!------------------------!
!  DECENT DECENT DECENT  !
!------------------------!


 !am: fonction qui calcule, dans Vaux, les vitesses obtenues a partir des forces de contact, stokees dans Iaux, pour les grains d'interface
!----------------------------------------------------------------
 subroutine comp_V_list_RBDY2_in_DDM 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: i_part_sdm
   
    do i_part_sdm=1, nb_RBDY2_interf_slave
       ! calcul de Vaux = Vfree + M^-1 Iaux (et CL appliquees a Vaux)
       call comp_vlocy(liste_RBDY2_interf_slave(i_part_sdm), iVaux_e_invM_t_Iaux_p_Vfree)
    end do

 end subroutine comp_V_list_RBDY2_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine nullify_Iaux_for_vlc_drv_dof_RBDY2_in_DDM 
    implicit none

    integer            :: i_part_sdm
   
    do i_part_sdm=1, nb_RBDY2_interf_slave
       call nullify_reac_of_vlocy_driven_dof_RBDY2(liste_RBDY2_interf_slave(i_part_sdm),iIaux_)
    end do

 end subroutine nullify_Iaux_for_vlc_drv_dof_RBDY2_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine gather_X_V_begin
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
    ! type(T_matrice_r), dimension(Nsdm) :: V_RBDY2_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, ibdy, imulti
    integer            :: irecv
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(3) :: X_tmp, V_tmp
    real(kind=8), dimension(:), allocatable :: Vbeg_slave  
    real(kind=8), dimension(:), allocatable :: Xbeg_slave  
    integer     , dimension(:), allocatable :: Ibeg_slave  
                                                                     ! dans la configuration de detection.
    real(kind=8), dimension(:), allocatable :: Vbeg4all  
    real(kind=8), dimension(:), allocatable :: Xbeg4all  
    integer     , dimension(:), allocatable :: Ibeg4all  

    if ( rang_COMM_WORLD == 0 ) then
       do isdm=1, Nsdm
          vect_nb_recv_host(isdm) = count(mask_particip(:,isdm) .eqv..true.)
          !write(slave_io, *) "vect_nb_recv_host(", isdm,") = ",vect_nb_recv_host(isdm) 
       end do

       !write(slave_io, *) "vect_nb_recv_host = ",vect_nb_recv_host
       !write(slave_io, *) "3*vect_nb_recv_host = ",3*vect_nb_recv_host

       vect_shift=0
       !write(slave_io, *) "vect_shift(1) = ",vect_shift(1) 

       do isdm=2,Nsdm 
          vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_recv_host(isdm-1)
          !write(slave_io, *) "vect_shift(", isdm,") = ",vect_shift(isdm) 
       end do

       !write(slave_io, *) "vect_shift = ",vect_shift 
       !write(slave_io, *) "3*vect_shift = ",3*vect_shift

       nb_recv_host=sum(vect_nb_recv_host)
       !write(slave_io, *) "nb_recv_host = ",nb_recv_host
       if (allocated(Ibeg4all)) deallocate(Ibeg4all)
       allocate(Ibeg4all(nb_recv_host))
       Ibeg4all=0
       if (allocated(Xbeg4all)) deallocate(Xbeg4all)
       allocate(Xbeg4all(3*nb_recv_host))
       Xbeg4all=0.D0
       if (allocated(Vbeg4all)) deallocate(Vbeg4all)
       allocate(Vbeg4all(3*nb_recv_host))
       Vbeg4all=0.D0

    else
       if (allocated(Ibeg4all)) deallocate(Ibeg4all)
       allocate(Ibeg4all(0))
       if (allocated(Xbeg4all)) deallocate(Xbeg4all)
       allocate(Xbeg4all(0))
       if (allocated(Vbeg4all)) deallocate(Vbeg4all)
       allocate(Vbeg4all(0))

    end if

    ! Nombre de particules concernées par l'envoi
    nb_send_slave = count(mask_in_slave .eqv. .true.)
    !write(slave_io, *) "nb_send_slave = ",nb_send_slave

    if (allocated(Ibeg_slave)) deallocate(Ibeg_slave)
    allocate(Ibeg_slave(nb_send_slave))
    Ibeg_slave=0

    if (allocated(Xbeg_slave)) deallocate(Xbeg_slave)
    allocate(Xbeg_slave(3*nb_send_slave))
    Xbeg_slave=0.d0


    if (allocated(Vbeg_slave)) deallocate(Vbeg_slave)
    allocate(Vbeg_slave(3*nb_send_slave))
    Vbeg_slave=0.d0
    
    compteur=0
    visible=.false.

    do ibody=1,nbody

       visible=mask_in_slave(ibody)
       if (.not. visible) cycle
       compteur=compteur+1

       Ibeg_slave( compteur ) = ibody

       call get_vector_RBDY2('Xbeg_',ibody,X_tmp,3)
       Xbeg_slave( 3*(compteur-1)+1 : 3*compteur)=X_tmp      

       call get_vector_RBDY2('Vbeg_',ibody,V_tmp,3)
       Vbeg_slave( 3*(compteur-1)+1 : 3*compteur)=V_tmp      

    end do

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Ibeg_slave, nb_send_slave, MPI_INTEGER, &
       Ibeg4all, vect_nb_recv_host, vect_shift, MPI_INTEGER, &
       0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    !write(slave_io, *) "size(Xbeg_slave)",size(Xbeg_slave)
    !write(slave_io, *) "3*nb_send_slave = ",3*nb_send_slave

    if ( rang_COMM_WORLD == 0 )  then
       vect_nb_recv_host=3*vect_nb_recv_host
       vect_shift=3*vect_shift
       !write(slave_io, *) "size(Xbeg4all)",size(Xbeg4all)
       !write(slave_io, *) "3*nb_recv_host = ",3*nb_recv_host
       !write(slave_io, *) "vect_nb_recv_host = ",vect_nb_recv_host
       !write(slave_io, *) "vect_shift = ",vect_shift
    end if

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Xbeg_slave, 3*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Xbeg4all,vect_nb_recv_host,vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Vbeg_slave, 3*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Vbeg4all, vect_nb_recv_host, vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    deallocate(Xbeg_slave)
    deallocate(Vbeg_slave)
    deallocate(Ibeg_slave)

    if ( rang_COMM_WORLD /= 0 ) return
    vect_nb_recv_host=vect_nb_recv_host/3
    vect_shift=vect_shift/3

    do isdm=1,Nsdm
       do irecv=1,vect_nb_recv_host(isdm)
          ibdy = Ibeg4all( vect_shift(isdm)+irecv  )
          X_tmp = Xbeg4all( 3*vect_shift(isdm) + 3*(irecv-1)+1 : 3*vect_shift(isdm) + 3*irecv )
          call put_vector_RBDY2('Xbeg_',ibdy,X_tmp,3)
          V_tmp = Vbeg4all( 3*vect_shift(isdm) + 3*(irecv-1)+1 : 3*vect_shift(isdm) + 3*irecv )
          call put_vector_RBDY2('Vbeg_',ibdy,V_tmp,3)

          !write(slave_io, *) "----------Ibeg, Xbeg, Vbeg : recus----------"
          !write(slave_io, *) 'ibody=', ibdy, ' : X_beg ', X_tmp
          !write(slave_io, *) 'ibody=', ibdy, ' : V_beg ', V_tmp
       end do 
    end do

    deallocate(Xbeg4all)
    deallocate(Vbeg4all)

 end subroutine gather_X_V_begin
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_visibility_4all_in_DDM(proc)
 
    implicit none
   
    integer, intent(in), optional :: proc
    integer                       :: i

    if ( .not. present(proc) ) then
       call set_visibility_4all_RBDY2(mask_in_slave,nbody)
    else
       if ( rang_COMM_WORLD == proc ) then
          call set_visibility_4all_RBDY2((/ (.true., i=1, nbody) /),nbody)
       end if
    end if

 end subroutine set_visibility_4all_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_boundary_sample_in_DDM2d(nb_RBDY2_, walls_behav,xmax_bound,xmin_bound)
 
    implicit none

    integer, intent(in)         :: nb_RBDY2_
    character(len=5), intent(in):: walls_behav
    real(kind=8), intent(out)   :: xmax_bound,xmin_bound

    integer                     :: i_RBDY2
    real(kind=8)                :: xmax,xmin, & !,xmax_bound,xmin_bound, &
                                    ymax,ymin,ymax_bound,ymin_bound
    real(kind=8)                :: eps=1.D-5 
    real(kind=8), dimension(3)  :: coor_tmp
    logical                     :: visible
    logical, save               :: premiere_passe = .true.
    character(len=5)            :: behav

    if (premiere_passe) then
       xmax  =-1.D20
       xmin  = 1.D20
    end if

    ymax  =-1.D20
    ymin  = 1.D20

    do i_RBDY2=1,nb_RBDY2_
       
       visible = .false.
       visible = get_visible(i_RBDY2)
       if ( .not. visible ) CYCLE

       call get_behav(i_RBDY2,1,behav)
       if ( behav == walls_behav ) then 
          coor_tmp = get_coorTT(i_RBDY2,0)
          ymax = MAX(ymax,coor_tmp(2))
          ymin = MIN(ymin,coor_tmp(2))
       end if
       
       if (premiere_passe) then
          coor_tmp = get_coorTT(i_RBDY2,0)
          xmax = MAX(xmax,coor_tmp(1))
          xmin = MIN(xmin,coor_tmp(1))
       end if

    END DO

    ymin_bound=ymin-eps
    call set_init_boundary_RBDY2(1,ymin_bound)
    ymax_bound=ymax+eps
    call set_init_boundary_RBDY2(2,ymax_bound)

    if (premiere_passe) then
       xmin_bound= ( (xmax+xmin) / 2 ) - (xmax-xmin)
       call set_init_boundary_RBDY2(3,xmin_bound)

       xmax_bound= ( (xmax+xmin) / 2 ) + (xmax-xmin)
       call set_init_boundary_RBDY2(4,xmax_bound)

       print *, "xmin      =", xmin
       print *, "xmax      =", xmax
       print *, "xmin_bound=", xmin_bound
       print *, "xmax_bound=", xmax_bound
       print *, "ymin_bound=", ymin
       print *, "ymax_bound=", ymax

    end if

    premiere_passe=.false.

 end subroutine compute_boundary_sample_in_DDM2d
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine print_iter(iterID,iter)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    integer, intent(in)  :: iterID,iter
 
    if ( rang_COMM_WORLD /= 0 ) return

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
 
    if ( rang_COMM_WORLD /= 0 ) return

    print *, "Nstep=", step

 end subroutine print_step
!----------------------------------------------------------------

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
!Début des routines de sortie : DISPLAY, OUTBOX et POSTPRO (during computation)!
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

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
  subroutine init_postpro_in_DDM(solv_info_, viol_evol_,                &
                                 sdm_info_, mean_sdm_info_,             &
                                 sample_info_, mean_sample_info_)
  
    implicit none

    integer           :: sdm
    logical           :: solv_info_, viol_evol_, sdm_info_, &
                         mean_sdm_info_, sample_info_, mean_sample_info_
                             !12345678901234567890123456789012345
    character(len=35) :: IAM="DDM_MPI_DECENT::init_postpro_in_DDM"

    solv_info=solv_info_
    viol_evol=viol_evol_
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

    if (sdm_info) then
       sdm = rang_COMM_WORLD + 1
       nfich_sdm_inf = get_io_unit()
       !              1234567890123456789012345678901234
       clout_sdm_inf='POSTPRO/SDM_INFORMATIONS.xxxxx.DAT'
       write(clout_sdm_inf(26:30), '(I5.5)') sdm
       OPEN(unit=nfich_sdm_inf,file=clout_sdm_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_sdm_inf) 
    end if

    if (mean_sdm_info) then
       sdm = rang_COMM_WORLD + 1
       nfich_mean_sdm_inf = get_io_unit()
       !                   123456789012345678901234567890123456789
       clout_mean_sdm_inf='POSTPRO/MEAN_SDM_INFORMATIONS.xxxxx.DAT'
       write(clout_mean_sdm_inf(31:35), '(I5.5)') sdm
       OPEN(unit=nfich_mean_sdm_inf,file=clout_mean_sdm_inf,STATUS='REPLACE') 
       CLOSE(unit=nfich_mean_sdm_inf) 
       
       mean_nb_RBDY2_slave             = 0.d0
       mean_nb_RBDY2_interf_slave      = 0.d0
       mean_nb_interactions_slave      = 0.d0
       mean_nb_fine_interactions_slave = 0.d0
       mean_nb_liens_interf_slave   = 0.d0
       mean_nb_INTRF                   = 0.d0
       mean_nb_adjac_slave             = 0.d0

    end if

    if ( rang_COMM_WORLD /= 0 ) return
   
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

       mean_nb_RBDY2_interf4all = 0
       mean_nb_interactions4all = 0
       mean_nb_fine_interactions4all = 0
       mean_nb_liens_interf4all = 0
       mean_nb_INTRF4all = 0
       mean_nb_adjac4all = 0
       mean_nb_migrations4all = 0
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
       call compute_nb_RBDY2_slave_in_DDM
       nb_fine_interactions_slave = 0
       nb_adjac_slave = get_nb_adjac_nlgs_2D()
       nb_fine_interactions_slave = get_nb_inters( i_dkdkx )
       call write_SDM_INFORMATIONS

       if ( sample_info ) then
          call sample_informations_in_DDM
       end if 
       mean_nb_RBDY2_slave = mean_nb_RBDY2_slave &
                                     + real(nb_RBDY2_slave)
       mean_nb_RBDY2_interf_slave = mean_nb_RBDY2_interf_slave &
                                     + real(nb_RBDY2_interf_slave)
       mean_nb_interactions_slave = mean_nb_interactions_slave &
                                     + real(nb_interactions_slave)
       mean_nb_fine_interactions_slave = mean_nb_fine_interactions_slave &
                                     + real(nb_fine_interactions_slave)
       mean_nb_liens_interf_slave = mean_nb_liens_interf_slave  &
                                     + real(nb_liens_interf_slave)
       mean_nb_INTRF = mean_nb_INTRF + real(nb_INTRF)
       mean_nb_adjac_slave = mean_nb_adjac_slave + real(nb_adjac_slave)
       mean_nb_linked_sdm = mean_nb_linked_sdm + real(nb_linked_sdm)
    end if

    if ( viol_evol ) then
       call violation_evolution_in_DDM
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

       mean_nb_RBDY2_slave = mean_nb_RBDY2_slave &
                                    / nb_postpro
       mean_nb_RBDY2_interf_slave = mean_nb_RBDY2_interf_slave &
                                    / nb_postpro
       mean_nb_interactions_slave = mean_nb_interactions_slave &
                                    / nb_postpro
       mean_nb_fine_interactions_slave = mean_nb_fine_interactions_slave &
                                    / nb_postpro
       mean_nb_INTRF = mean_nb_INTRF / nb_postpro
       mean_nb_adjac_slave = mean_nb_adjac_slave / nb_postpro
       mean_nb_linked_sdm = mean_nb_linked_sdm /nb_postpro

       call write_MEAN_SDM_INFORMATIONS

    end if

    if ( mean_sample_info ) then

       mean_nb_RBDY2_interf4all = mean_nb_RBDY2_interf4all &
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
  

    if ( rang_COMM_WORLD /= 0 ) return

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
    
    DO icdan = 1, get_nb_inters( i_dkdkx )
       CALL get_gaps( i_dkdkx, icdan, gap, gapBegin)

       gapBegin = MIN(0.D0,gapBegin)
       gap      = MIN(0.D0,gap)

       total_gapBegin = total_gapBegin - gapBegin     
       total_gap      = total_gap      - gap     

       max_gapBegin  =MAX(max_gapBegin,-gapBegin)
       max_gap       =MAX(max_gap,-gap)
    END DO

    call MPI_REDUCE(total_gapBegin, total_gapBegin_4all, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    call MPI_REDUCE(total_gap, total_gap_4all, 1, MPI_DOUBLE_PRECISION, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    call MPI_REDUCE(max_gapBegin, max_gapBegin_4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    call MPI_REDUCE(max_gap, max_gap_4all, 1, MPI_DOUBLE_PRECISION, MPI_MAX, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    if (rang_COMM_WORLD /= 0) return

    OPEN(unit=nfich_viol_evol,file=clout_viol_evol,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_viol_evol,fmt='(5(1X,D14.7))') TPS,total_gapBegin_4all/Nactif4all,total_gap_4all/Nactif4all, &
                                                    max_gapBegin_4all,max_gap_4all
    CLOSE(unit=nfich_viol_evol) 

  end subroutine violation_evolution_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine compute_nb_RBDY2_slave_in_DDM
  
    implicit none

    nb_RBDY2_slave = count(mask_in_slave .eqv. .true.)

  end subroutine compute_nb_RBDY2_slave_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_SDM_INFORMATIONS
  
    implicit none

    OPEN(unit=nfich_sdm_inf,file=clout_sdm_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_sdm_inf,fmt='(D14.7,8(1X,I8))') TPS,nb_RBDY2_slave,nb_RBDY2_interf_slave, &
                     nb_interactions_slave,nb_fine_interactions_slave,nb_liens_interf_slave, &
                     nb_INTRF,nb_adjac_slave, nb_linked_sdm
    CLOSE(unit=nfich_sdm_inf) 

  end subroutine write_SDM_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_MEAN_SDM_INFORMATIONS
  
    implicit none

    OPEN(unit=nfich_mean_sdm_inf,file=clout_mean_sdm_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_mean_sdm_inf,fmt='(9(1X,D14.7))') TPS,mean_nb_RBDY2_slave,mean_nb_RBDY2_interf_slave, &
                     mean_nb_interactions_slave,mean_nb_fine_interactions_slave,mean_nb_liens_interf_slave, &
                     mean_nb_INTRF,mean_nb_adjac_slave,mean_nb_linked_sdm
    CLOSE(unit=nfich_mean_sdm_inf) 

  end subroutine write_MEAN_SDM_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine sample_informations_in_DDM
  
    implicit none

    call MPI_REDUCE(nb_fine_interactions_slave,nb_fine_interactions4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    call MPI_REDUCE(nb_INTRF,nb_INTRF4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)
    call MPI_REDUCE(nb_adjac_slave,nb_adjac4all, 1, MPI_INTEGER, MPI_SUM, 0, MPI_COMM_WORLD, code_MPI)
    if (code_MPI /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code_MPI)

    if (rang_COMM_WORLD /= 0) return

    nb_RBDY2_interf4all = count(multiplicite > 1)

    OPEN(unit=nfich_sample_inf,file=clout_sample_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_sample_inf,fmt='(D14.7,8(1X,I8))') TPS,nbody,nb_RBDY2_interf4all, &
                     nb_interactions4all,nb_fine_interactions4all,nb_liens_interf, &
                     nb_INTRF4all,nb_adjac4all, nb_migrations
    CLOSE(unit=nfich_sample_inf) 

    mean_nb_RBDY2_interf4all      = mean_nb_RBDY2_interf4all      + real(nb_RBDY2_interf4all)
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

    if (rang_COMM_WORLD /= 0) return

    OPEN(unit=nfich_mean_sample_inf,file=clout_mean_sample_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_mean_sample_inf,fmt='(9(1X,D14.7))') TPS,real(nbody),mean_nb_RBDY2_interf4all, &
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
    character(len=23)  :: clout_DDM_INFO ! pour calculer le nom du DDM.INFO

    ! l'entete du fichier DOF n'est ecrite que par le processus de rang_COMM_WORLD 0
    call Write_xxx_dof_Ol(1, rang_COMM_WORLD /= 0)
    CALL write_xxx_dof_RBDY2(1,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang_COMM_WORLD 0
    call Write_xxx_Vloc_Rloc_Ol(1, rang_COMM_WORLD /= 0)
    if ( ntact /= 0 ) CALL write_xxx_Vloc_Rloc_DKDKx(1)


    ! Handle unique pour chaque processus
    ! Handle unique pour chaque processus
    nfich=rang_COMM_WORLD+1000 
    Nprint=Nprint+1

    if (rang_COMM_WORLD/=0) return

    !               12345678901234567890123
    clout_DDM_INFO='OUTBOX/DDM.INFO.xxxxxxx'
    write(clout_DDM_INFO(17:23), '(I7.7)') Nprint 

    OPEN(unit=nfich,STATUS='REPLACE',file=clout_DDM_INFO)
    do ibody = 1,nbody

       sdm=repart_sdm_ci(ibody)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, I4, 1X, I4)') ibody, sdm, multi

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

    ! l'entete du fichier DOF n'est ecrite que par le processus de rang_COMM_WORLD 0
    call Write_xxx_dof_Ol(2, rang_COMM_WORLD /= 0)
    CALL write_xxx_dof_RBDY2(2,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang_COMM_WORLD 0
    call Write_xxx_Vloc_Rloc_Ol(2, rang_COMM_WORLD /= 0)
    if ( ntact /= 0 ) CALL write_xxx_Vloc_Rloc_DKDKx(2)

    ! Handle unique pour chaque processus
    nfich=rang_COMM_WORLD+1000 

    if (rang_COMM_WORLD/=0) return

    !               12345678901234567890
    clout_DDM_INFO='OUTBOX/DDM.INFO.LAST'

    OPEN(unit=nfich,STATUS='REPLACE',file=clout_DDM_INFO)
    do ibody = 1,nbody

       sdm=repart_sdm_ci(ibody)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, I4, 1X, I4)') ibody, sdm, multi

    end do
    CLOSE(nfich)
  
  end subroutine write_LAST_in_DDM
!----------------------------------------------------------------

!! Procédure d'arrêt de MPI
!!----------------------------------------------------------------
!subroutine mpi_finalize_process
!
!   implicit none
!
!   ! Désactivation de l'environnement MPI
!   call MPI_FINALIZE(code_MPI)
!
!end subroutine mpi_finalize_process
!!----------------------------------------------------------------

! Procédure de nettoyage de la memoire encore allouee
!----------------------------------------------------------------
subroutine clean_ddm

   implicit none

   ! Nettoyage de la memoire encore allouee dans le module de detection grossiere
   call clean_module 

end subroutine clean_ddm
!----------------------------------------------------------------

end module DDM_MPI_2D_DECENT
