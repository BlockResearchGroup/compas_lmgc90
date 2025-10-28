            !---------------------------------------------------!
            !            NSCD en Multidomaines MPI 2d           !
            !   Écrit par : Vincent Visseq & Alexandre Martin   !
            !  Module importé dans DKDK_standalone_DDM_MPI.f90  !
            !---------------------------------------------------!

module DDM_MPI_2D 

use MPI

use parameters
use overall
use utilities
use RBDY2
use DISKx, only : diskx2bdyty, get_radius_DISKx       , &
                  get_color_diskx     => get_color    , &
                  get_coor_diskx      => get_coor     , &
                  is_diskx_same_bdyty                 , &
                  get_max_radius_DISKx                , &
                  get_min_radius_DISKx
use DKDKx, only : set_interactions_to_rough, &
                  set_anonymous_to_rough,    &
                  write_xxx_Vloc_Rloc_DKDKx, &
                  get_nb_INTRF_DKDKx,        & 
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

!On ne travaille qu'avec des disques (et des clusters de disques)
integer(kind=4)                                :: nbody          ! nombre RBDY2 
integer(kind=4)                                :: ntact          ! nombre de contacteurs disques

logical         :: first_real_step=.true.    

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
integer(kind=4), dimension(:), allocatable   :: multiplicite   ! vecteur recenssant le nombre de participation aux sdm des corps
integer(kind=4), dimension(:), allocatable   :: multiplicite_old ! vecteur recenssant le nombre de participation aux sdm des corps
                                                               ! à la répartition en sous-domaines précédente
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
 type T_ligne_r
      real(kind=8), dimension(:,:), pointer :: particule => null()
 end type T_ligne_r
!-------------------------------------------------------------------------------------------------------------------

!-------------------------------------------------------------------------------------------------------------------
! Données des interfaces par sous-domaine
!-------------------------------------------------------------------------------------------------------------------
type(T_ligne_i), dimension(:), allocatable  :: liste_RBDY2_interf_sdm     ! structure des indices par sdm 
type(T_ligne_i), dimension(:), allocatable  :: liste_RBDY2_interf_sdm_old ! structure des indices par sdm au pas -1
type(T_ligne_r), dimension(:), allocatable  :: V_RBDY2_interf_sdm         ! vitesses des RBDY2 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: V_RBDY2_interf_sdm_old     ! vitesses des RBDY2 d'interface par sdm old
type(T_ligne_r), dimension(:), allocatable  :: X_RBDY2_interf_sdm         ! positions des RBDY2 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: DFg_RBDY2_interf_sdm       ! DF_gamma des RBDY2 d'interface par sdm 
integer(kind=4), dimension(:), allocatable  :: nb_RBDY2_interf_sdm        ! nombre de corps d'interface par sdm

!-------------------------------------------------------------------------------------------------------------------
! Données de l'interface globale 
!-------------------------------------------------------------------------------------------------------------------
integer(kind=4)                             :: nb_RBDY2_interf_glob   ! nombre de particule de l'interface globale
integer(kind=4), dimension(:), allocatable  :: interface_globale      ! tableau (/ nb_RBDY2_interf_globale /), 
                                                                      ! avec pour chaque particule d'interface (indice de dim=2)
                                                                      ! son indice global
integer(kind=4)                             :: nb_liens_interf        ! nombre global de liens d'interface
real(kind=8), dimension(:,:), allocatable   :: saut_V_interf_glob     ! sauts de vitesses des RBDY2 d'interface globale 
real(kind=8), dimension(:,:), allocatable   :: V_RBDY2_interf_glob    ! vitesses à convergence des RBDY2 d'interface globale 
real(kind=8), dimension(:,:), allocatable   :: X_RBDY2_interf_glob    ! position à convergence des RBDY2 d'interface globale 


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

logical     , dimension(:), allocatable :: mask_part4all     ! Vecteur (/nb_procs*nbody/)=(/mask(1),...,mask(n)/)

integer     , dimension(:), allocatable :: mig_indices4all   ! Vecteur des indices des particules migrantes
real(kind=8), dimension(:), allocatable :: mig_etats4all     ! Vecteur des états des particules migrantes
integer     , dimension(:), allocatable :: mig_indices_host  ! Vecteur des indices des particules migrantes
real(kind=8), dimension(:), allocatable :: mig_etats_host    ! Vecteur des états des particules migrantes

real(kind=8), dimension(:), allocatable :: V_interf4all      ! Vecteur des vitesses d'interface concaténés
real(kind=8), dimension(:), allocatable :: DFg_interf4all    ! Vecteur des incréments DFg concaténés
real(kind=8), dimension(:), allocatable :: Vbeg4all  
real(kind=8), dimension(:), allocatable :: Xbeg4all  
integer(kind=4), dimension(:), allocatable :: Ibeg4all  

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

integer     :: nb_send_slave ! Nombre d'éléments envoyés par l'esclave
integer     :: nb_recv_slave ! Nombre d'éléments reçus par l'esclave

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
integer                                   :: nb_RBDY2_interf_slave        ! nombre de corps d'interface du sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY2_interf_slave     ! indices des RBDY2 d'interface sur sdm courant
integer     , dimension(:)  , allocatable :: liste_RBDY2_interf_slave_old ! structure des indices du sdm au pas -1
real(kind=8), dimension(:)  , allocatable :: V_RBDY2_interf_slave         ! vitesses des RBDY2 d'interface du sdm courant 
real(kind=8), dimension(:)  , allocatable :: V_RBDY2_interf_slave_old     ! idem au pas -1 
real(kind=8), dimension(:)  , allocatable :: Fg_RBDY2_interf_slave        ! F_gamma des RBDY2 d'interfacedu sdm courant
real(kind=8), dimension(:)  , allocatable :: Fg_RBDY2_interf_slave_old    ! F_gamma des RBDY2 d'interface du sdm (pas -1)
real(kind=8), dimension(:)  , allocatable :: DFg_RBDY2_interf_slave       ! DF_gamma des RBDY2 d'interface du sdm courant
logical                                   :: repartition_just_made = .false. ! Permet de savoir si la répartition
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
integer      :: nb_procs,rang,code
real(kind=8) :: secondsTT,MPI_MAX_time              ! Calcul du temps maximal passé par un processus pour
                                                    ! l'ensemble de la simulation 
real(kind=8) :: secondsCREADOM,MPI_MAX_CREADOM_time ! Calcul du temps maximal passé dans création_domaines
integer      :: slave_io

logical      :: xperiodic=.false.
real(kind=8) :: xperiode = 0.d0

!-------------------------------------------------------------------------------------------------------------------
! Fin des déclarations des variables globales au module
!-------------------------------------------------------------------------------------------------------------------


!am: on donne la liste des fonctions publiques (utilisables hors du module)
public                                &
   init_dd,                           &
   init_MPI,                          &
   set_working_directory_in_DDM,      &
   mpi_finalize_process,              &
   new_dd_slave,                      &
   new_dd_host,                       &
   allocations_fantome_slave,         &
   creation_domaines,                 &
   erase_rough_contact,               &
   erase_splitted_rough,              &
   set_visibility_4all_in_DDM,        &
   set_interactions_to_rough_in_DDM,  &
   get_list_INTRF_in_DDM,             &
   RnodHRloc_list_in_DDM,             &
   compute_local_free_vlocy_in_DDM,   &
   set_skip_display_cluster,          &  ! Os
   write_OUT_in_DDM,                  &  ! Os
   write_LAST_in_DDM,                 &  ! Os
   init_postpro_in_DDM,               &  ! Oh & Os
   postpro_during_in_DDM,             &  ! Oh & Os
   postpro_last_in_DDM,               &  ! Oh & Os
   stock_Fg_liste_old,                &
   comp_V_list_RBDY2_in_DDM,          &
   compute_DF_gamma,                  &
   compute_egluing,                   &
   compute_boundary_sample_in_DDM2d,  &
   set_F_gamma,                       &
   gather_X_V_begin,                  &
   scatter_creation_domaines,         &
   gather_V_interface,                &
   scatter_DFg,                       &
   scatter_V_interface,               &
   fix_V_interface,                   &
   fix_migrants,                      &
   set_modified_mass_in_DDM,          &
   write_DDM_INFO,                    &
   prep_exchange_inloop,              &
   print_info,                        &
   print_step,                        &
   print_iter,                        &
   test_is_converged,                 &
   check_convergence_in_DDM,          &
   clean_ddm

contains


!-------------------------------------------------------------------------------------------------------------------
 subroutine init_MPI

    implicit none

    call MPI_INIT(code)
    call MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs,code)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rang,code)

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
 subroutine print_info(nb_time_steps,TimeStep,Theta_,freq_DISPLAY,freq_OUTBOX,freq_postpro, &
                        DDMfreq,Nsdm1,Nsdm2,CVTYPE,TOL,RELAX,itloop1,itloop2) 

    implicit none

    ! Variables d'entrée
    integer, intent(in)         :: nb_time_steps,freq_DISPLAY,freq_OUTBOX,freq_postpro,DDMfreq, &
                                   Nsdm1,Nsdm2,itloop1,itloop2
    real(kind=8),intent(in)     :: TimeStep,Theta_! Pas de temps et paramètre de la theta méthode
    real(kind=8),intent(in)     :: TOL,RELAX      ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
    CHARACTER(len=5),intent(in) :: CVTYPE         ! Type de convergence : QUAD, MAX, QUAD/16, etc...


                             !12345678901234567890
    character(len=18) :: IAM='DDM_2D::print_info'

    if ( rang /= 0 ) return

    !-------------------------------------------------------------------------------
    ! Ecriture à l'écran des mêmes paramètres
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! loading step            : ',nb_time_steps
    WRITE(*,*) '! TIME STEP               : ',TimeStep
    WRITE(*,*) '! THETA                   : ',Theta_
    WRITE(*,*) '! WRITE DISPLAY FREQUENCY : ',freq_DISPLAY
    WRITE(*,*) '! WRITE OUTBOX FREQUENCY  : ',freq_OUTBOX
    WRITE(*,*) '! WRITE POSTPRO FREQUENCY : ',freq_postpro
    !-------------------------------------------------------------------------------
    ! Paramètres de découpages en sous_domaines
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! FREQUENCE DETECT/REPART : ',DDMfreq
    WRITE(*,*) '! Nsdm1 et Nsdm2          : ',Nsdm1,Nsdm2
    !-------------------------------------------------------------------------------
    ! Paramètres d'itération de la boucle DDM
    !-------------------------------------------------------------------------------
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
    print *, Nsdm

    ! Calcul de la multiplicité maximale pour une particule
    max_multi=max(4,Nsdm1,Nsdm2) ! Un cluster en diagonale ferait tout sauter

    ! on le renvoie
    init_dd=Nsdm


    if (Nsdm/=nb_procs)  call faterr(IAM, "Le nombre de processus doit etre &
            & strictement egal au nombre de sous-domaines !!")

 end function init_dd
!-------------------------------------------------------------------------------------------------------------------

!Procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd_slave(nb_RBDY2,ntact_,lperiodic,rperiode)

    implicit none
    ! variables d'entrée
    integer, intent(in)      :: nb_RBDY2,ntact_
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
    if ( rang /= 0 ) return

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
       allocate(body_particip(max_multi,nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation des corps aux sdm")
    end if
    !    * table des sous-domaines auxquels participent les corps à la répartion DDM précédente
    if (.not. allocated(body_particip_old)) then
       allocate(body_particip_old(max_multi,nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de la table de participation old des corps aux sdm")
    end if

    ! multiplicite est déjà alloué pour le rang 0 dans new_dd_slave
    !!    * table multiplicite
    !if (.not. allocated(multiplicite)) then
    !   allocate(multiplicite(nbody), stat=err)
    !   if (err/=0) call faterr(IAM, "Erreur d'allocation de multiplicite")
    !end if

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

    !    * table des vitesses d'interface appartenants aux sdm
    if (.not. allocated(V_RBDY2_interf_sdm)) then
       allocate(V_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de V_RBDY2_interf_sdm")
    end if
    !    * table des vitesses d'interface appartenants aux sdm au pas -1
    if (.not. allocated(V_RBDY2_interf_sdm_old)) then
       allocate(V_RBDY2_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) stop "Erreur d'allocation du nombre de colonne de V_RBDY2_interf_sdm_old"
    end if
    !    * table des positions d'interface appartenants aux sdm
    if (.not. allocated(X_RBDY2_interf_sdm)) then
       allocate(X_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de X_RBDY2_interf_sdm")
    end if

    !    * table des DF_gamma sur les grains d'interface des sdm
    if (.not. allocated(DFg_RBDY2_interf_sdm)) then
       allocate(DFg_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de DFg_RBDY2_interf_sdm")
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

 !fonction qui alloue, avec une taille de 0, sur les esclaves, les tableaux utilises par le maitre
 ! => ifort check ne plante pas!
!----------------------------------------------------------------
 subroutine allocations_fantome_slave

    implicit none

    if ( rang == 0 ) return

    if (allocated(mask_part4all)) deallocate(mask_part4all)
    allocate(mask_part4all(0))

    if (allocated(vect_nb_send_host)) deallocate(vect_nb_send_host)
    allocate(vect_nb_send_host(0))

    if (allocated(vect_shift)) deallocate(vect_shift)
    allocate(vect_shift(0))

    if (allocated(vect_nb_recv_host)) deallocate(vect_nb_recv_host)
    allocate(vect_nb_recv_host(0))

    if (allocated(interactions4all)) deallocate(interactions4all)
    allocate(interactions4all(0))

    if (allocated(vect_send_host_I)) deallocate(vect_send_host_I)
    allocate(vect_send_host_I(0))

    if (allocated(V_interf4all)) deallocate(V_interf4all)
    allocate(V_interf4all(0))

    if (allocated(DFg_interf4all)) deallocate(DFg_interf4all)
    allocate(DFg_interf4all(0))

    if (allocated(Ibeg4all)) deallocate(Ibeg4all)
    allocate(Ibeg4all(0))

    if (allocated(Xbeg4all)) deallocate(Xbeg4all)
    allocate(Xbeg4all(0))

    if (allocated(Vbeg4all)) deallocate(Vbeg4all)
    allocate(Vbeg4all(0))

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

    character(len=100)                         :: cout
                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_2D::creation_domaines"

    ! subroutine de rang 0
    if ( rang /= 0 ) return

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
      if ( xperiodic ) then
         if ( coord_ct(1,itact) .gt. xperiode) then
            coord_ct(1,itact) = coord_ct(1,itact) - xperiode
         else if (coord_ct(1,itact) .lt. 0.d0 ) then
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

       ! on calcule la separation entre les objets
       sep(1:2) = coord_ct(1:2, cdtac) - coord_ct(1:2, antac) 

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
    integer :: i
    integer :: shift
    logical :: mig_tab_vide, mig_tab_vide4all
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

    ! 1) Nombre d'élément dans l'inteface

    if ( rang == 0 ) then
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = nb_RBDY2_interf_sdm(isdm)
       end do
    end if


    ! Envoi du nombre de RBDY2 d'interface pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_RBDY2_interf_slave, 1, &
            MPI_INTEGER, 0, MPI_COMM_WORLD, code)

    ! Allocation du vecteur dans lequel les indices des RBDY2 d'interface seront stockés
    if (allocated(liste_RBDY2_interf_slave)) deallocate(liste_RBDY2_interf_slave)
    allocate(liste_RBDY2_interf_slave(nb_RBDY2_interf_slave))
    liste_RBDY2_interf_slave=0
 
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
          = liste_RBDY2_interf_sdm(isdm)%particule(:)
      end do
      vect_send_host_I(vect_shift(Nsdm)+1:nb_send_host) &
       = liste_RBDY2_interf_sdm(Nsdm)%particule(:)

    end if 

    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(vect_send_host_I, vect_nb_send_host, vect_shift, MPI_INTEGER, &
       liste_RBDY2_interf_slave, nb_RBDY2_interf_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

                          !12345678901234567890
    !call write_MPI_INFO_I('LISTE_RBDY2_INTERF__',liste_RBDY2_interf_slave,&
    !      (/(i,i=1,nb_RBDY2_interf_slave)/),nb_RBDY2_interf_slave)

    !---------------------------
    ! LISTE DES INTERACTIONS
    !---------------------------

    ! 1) Nombre d'interactions

    if ( rang == 0 ) then
       vect_nb_send_host=0
       do isdm=1,Nsdm
          vect_nb_send_host(isdm) = 4 * nb_splitted_rough(isdm)
       end do
    end if


    ! Envoi du nombre de RBDY2 d'interface pour le sdm courant
    call MPI_SCATTER(vect_nb_send_host, 1, MPI_INTEGER, nb_interactions_slave, 1, &
            MPI_INTEGER, 0, MPI_COMM_WORLD, code)

    ! Allocation du vecteur dans lequel les indices des RBDY2 d'interface seront stockés
    if (allocated(interactions_slave)) deallocate(interactions_slave)
    allocate(interactions_slave(nb_interactions_slave))
    interactions_slave=0

    ! 2) Distribution des intéractions

    if ( rang == 0 ) then
      ! Nombre total d'éléments à envoyer
      nb_send_host=sum(vect_nb_send_host)

      ! Vecteur de décalage d'indice pour le SCATTERV
      vect_shift=0
      do isdm=2,Nsdm 
         vect_shift(isdm)=vect_shift(isdm-1) + vect_nb_send_host(isdm-1)
      end do

      !write(slave_io, '(I6,I6,I6)') interactions4all
         
    end if 

    ! Envoi de la liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(interactions4all, vect_nb_send_host, vect_shift, MPI_INTEGER, &
       interactions_slave, nb_interactions_slave, MPI_INTEGER, 0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
    
                          !12345678901234567890
    !call write_MPI_INFO_I('INTERACTIONS________',interactions_slave,&
    !      (/(i,i=1,nb_interactions_slave)/),nb_interactions_slave)

    nb_interactions_slave=nb_interactions_slave/4

    !print *, "rang=", rang, nb_interactions_slave 

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
       mig_tab_vide4all = .true.
       nb_mig_slave = 0 

       ! ---> Fonction rang0 <---
       call nb_migrations2send(mig_tab_vide) ! calcule vect_nb_send_host

       call MPI_ALLREDUCE(mig_tab_vide, mig_tab_vide4all, 1, MPI_LOGICAL, MPI_LOR, MPI_COMM_WORLD, code)
       if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

       if ( .not. mig_tab_vide4all ) then
    
          ! Envoi du nombre de RBDY2 d'interface pour le sdm courant
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
             allocate(mig_etats_slave(nb_mig_slave*6))
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
                allocate(mig_etats4all(nb_send_host*6))
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
                allocate(mig_etats_host(6*nb_send_host))
                mig_etats_host=0.d0

                shift = 6*vect_shift(isdm)
                mig_etats_host(:) = mig_etats4all( shift+1 : shift + 6*nb_send_host )           

                call MPI_SEND (mig_etats_host, 6*nb_send_host, MPI_DOUBLE_PRECISION, isdm-1, etiquette, MPI_COMM_WORLD ,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)
             end if

             if ( rang == isdm-1 ) then
                if ( nb_mig_slave ==0 ) cycle
                call MPI_RECV (mig_etats_slave, 6*nb_mig_slave, MPI_DOUBLE_PRECISION,0,etiquette, MPI_COMM_WORLD ,statut,code)
                if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

             end if

          end do

       end if
   
    else 

       first_real_step=.false. 

    end if


    ! on alloue l'espace memoire pour stocker l'agregation des vitesses des corps d'interface du sdm courant
    ! (3 reels pour chaque corps d'interface)
    if (allocated(V_RBDY2_interf_slave)) deallocate(V_RBDY2_interf_slave)
    allocate(V_RBDY2_interf_slave(3*nb_RBDY2_interf_slave))
    if (allocated(V_RBDY2_interf_slave_old)) deallocate(V_RBDY2_interf_slave_old)
    allocate(V_RBDY2_interf_slave_old(3*nb_RBDY2_interf_slave))
   
    V_RBDY2_interf_slave= 0.D0
    V_RBDY2_interf_slave_old= 0.D0
   
    ! Allocation de l'espace memoire pour stocker l'agregation des increments des forces 
    ! de cohesion sur les corps d'interface du sdm courant
    if (allocated(DFg_RBDY2_interf_slave)) deallocate(DFg_RBDY2_interf_slave)
    allocate(DFg_RBDY2_interf_slave(3*nb_RBDY2_interf_slave))
    DFg_RBDY2_interf_slave = 0.D0

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
                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_2D::stock_Fg_liste_old"

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

    if (.not. first_real_step) repartition_just_made = .true.

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

    do icdan=1, nb_rough_contact
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la paire num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
       cdtac = cdan(1)
       antac = cdan(2)

       coordcd(1:2) = coord_ct(1:2,cdtac)
       coordan(1:2) = coord_ct(1:2,antac)
 
       coord_cc(1:2,icdan) = (coordcd(1:2)+coordan(1:2))*0.5
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

    if ( rang /= 0 ) return

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

          nb_new=nb_new + 1
          migration_tab(ibody,nb_new)=isdm
          WRITE(cout,*) 'migration_tab(',ibody,',',nb_new,')=',migration_tab(ibody,nb_new)
          call logmes(cout)
          nb_migrations=nb_migrations + 1
 
          if ( isdm == 1 ) then
             call logmes("Rien à faire pour gerer cette migration (dans le sdm num 1)") 
          end if

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
    !integer(kind=4), dimension(max_multi_,nb_entity,Nsdm_), intent(in) :: migration_tab 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    logical, intent(inout) :: mig_tab_vide
 
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

             shift = 6*( vect_shift(isdm) + ( compteur(isdm) - 1 ) )

             call get_vector_RBDY2('Xbeg_',ibody,Xbeg_tmp,3)
             mig_etats4all(shift+1:shift+3)=Xbeg_tmp

             !print *, "PROC",rang,"Xbeg_tmp",Xbeg_tmp

             call get_vector_RBDY2('Vbeg_',ibody,Vbeg_tmp,3)
             mig_etats4all(shift+4:shift+6)=Vbeg_tmp
             !print *, "PROC",rang,"Vbeg_tmp",Vbeg_tmp

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
       !print *, "PROC",rang,"STORE ibody",ibody


       Xbeg_tmp = 0.d0
       Xbeg_tmp = mig_etats_slave(6*(ibdy-1)+1: 6*(ibdy-1)+3)
       call put_vector_RBDY2('Xbeg_',ibody,Xbeg_tmp,3)
       !print *, "PROC",rang,"Xbeg_tmp",mig_etats_slave(6*(ibdy-1)+1: 6*(ibdy-1)+3)

       Vbeg_tmp=0.d0
       Vbeg_tmp = mig_etats_slave(6*(ibdy-1)+4: 6*ibdy)
       call put_vector_RBDY2('Vbeg_',ibody,Vbeg_tmp,3)
       !print *, "PROC",rang,"Vbeg_tmp",mig_etats_slave(6*(ibdy-1)+4: 6*ibdy)

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

    if ( rang/=0 ) return
 
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
             xcdan(1:2)= cdan(1:2)
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
 subroutine RnodHRloc_list_in_DDM
    implicit none
    !--------------------------------------

    ! variable locale
    integer :: storage_reac

    ! on stocke les torseurs des reactions de contact dans Iaux
    storage_reac = iIaux_

    ! calcul des torseurs des reactions de contact
    call RnodHRloc_nlgs(list_INTRF, storage_reac)

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
       if ( rang /= 0 .and. (.not. mask_in_slave(ibody))) cycle

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
    ! integer(kind=4), intent(out)                       :: nb_RBDY2_interf_glob
    ! integer(kind=4), dimension(:,:), intent(out)       :: interface_globale
    !   alloué dans cette fonction à (/ 2,nb_RBDY2_interf_glob /)

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody,isdm,ipart,i_std_bdy,imulti,err,compteur
    integer(kind=4), dimension(:), allocatable :: isdm_multi_tab
    integer, dimension(:), allocatable :: nb_grains_multi
    logical                            :: visible
    character(len=100):: cout
                             !1234567890123456789012345
    character(len=25) :: IAM='DDM_2D::compute_interface'

    ! Déterminer le nombre de grains d'interface pour ce sous-domaine.
    ! Je le tente par la fonction intrinseque "merge"

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


       ! On remplit le vecteur des RBDY2 d'interface par sous-domaine
       compteur=0
       visible=.true.

       do ibody=1,nbody ! Boucle sur les corps (non-dupiqués)
          visible=mask_particip(ibody,isdm)
          if ( .not. visible .or. multiplicite(ibody) < 2 ) cycle
          compteur=compteur+1 

          liste_RBDY2_interf_sdm(isdm)%particule(compteur)=ibody
       end do

       ! test puis affichage du nombre de RBDY2 d'interface pour le sous-domaine courant
       if (compteur/=nb_RBDY2_interf_sdm(isdm)) call faterr(IAM, "Trop ou pas assez de grains " // &
                                                 "d'interface indentifies dans un des sous-domaines")
       
       WRITE(cout,*) "nb_RBDY2_interf_sdm de sdm num ",isdm," = ",nb_RBDY2_interf_sdm(isdm)
       call logmes(cout)

       ! Dimensionnement des vitesses des RBDY2 d'interface par sdm
       if ( associated(V_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(V_RBDY2_interf_sdm(isdm)%particule)
       allocate(V_RBDY2_interf_sdm(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de V_RBDY2_interf_sdm(isdm)%paticule")
       ! Dimensionnement des vitesses des RBDY2 d'interface par sdm
       if ( associated(V_RBDY2_interf_sdm_old(isdm)%particule) ) & 
            deallocate(V_RBDY2_interf_sdm_old(isdm)%particule)
       allocate(V_RBDY2_interf_sdm_old(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) stop "Pb d'allocation de V_RBDY2_interf_sdm_old(isdm)%paticule"

       ! Dimensionnement des positions des RBDY2 d'interface 
       if ( associated(X_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(X_RBDY2_interf_sdm(isdm)%particule)
       allocate(X_RBDY2_interf_sdm(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de X_RBDY2_interf_sdm(isdm)%paticule")

       if ( associated(DFg_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(DFg_RBDY2_interf_sdm(isdm)%particule)
       allocate(DFg_RBDY2_interf_sdm(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de DFg_RBDY2_interf_sdm(isdm)%paticule")
       ! Mise à zero
       V_RBDY2_interf_sdm(isdm)%particule=0.D0
       V_RBDY2_interf_sdm_old(isdm)%particule=0.D0
       X_RBDY2_interf_sdm(isdm)%particule=0.D0
       DFg_RBDY2_interf_sdm(isdm)%particule=0.D0
    end do

    ! Traitement de l'interface globale
    ! Calcul du nombre de particule dans l'interface globale
    nb_RBDY2_interf_glob = count(multiplicite>1)

    WRITE(cout,*) "nombre de particule de l'interface globale=", nb_RBDY2_interf_glob
    call logmes(cout)

    if ( allocated(interface_globale) ) deallocate(interface_globale)
    allocate(interface_globale(nb_RBDY2_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de interface_globale")
    ! Allocation du tableau des positions à convergence des RBDY2 d'interface
    if ( allocated(X_RBDY2_interf_glob) ) deallocate(X_RBDY2_interf_glob)
    allocate(X_RBDY2_interf_glob(3,nb_RBDY2_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de X_RBDY2_interf_glob")
    ! Allocation du tableau des vitesses à convergence des RBDY2 d'interface
    if ( allocated(V_RBDY2_interf_glob) ) deallocate(V_RBDY2_interf_glob)
    allocate(V_RBDY2_interf_glob(3,nb_RBDY2_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de V_RBDY2_interf_glob")

    ! Construction de l'interface globale
    compteur=0
    do ibody=1,nbody
       if ( multiplicite(ibody)<=1 ) cycle
       compteur=compteur+1
       interface_globale(compteur)=ibody
    end do

    if (compteur/=nb_RBDY2_interf_glob) call faterr(IAM, "compteur/=nb_RBDY2_interf_glob")

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

    ! Allocation du tableau des sauts de vitesse d'interface
    if ( allocated(saut_V_interf_glob) ) deallocate(saut_V_interf_glob)
    allocate(saut_V_interf_glob(3,nb_liens_interf), stat=err)
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
          vect_nb_recv_host(isdm) = nb_RBDY2_interf_sdm(isdm)
       end do

       nb_recv_host=3*sum(vect_nb_recv_host)

       if (allocated(V_interf4all)) deallocate(V_interf4all)
       allocate(V_interf4all(nb_recv_host))
       V_interf4all=0.D0

       nb_send_host=nb_recv_host

       if (allocated(DFg_interf4all)) deallocate(DFg_interf4all)
       allocate(DFg_interf4all(nb_send_host))
       DFg_interf4all=0.D0

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

    ! Pour le sdm courant :
    ! Calcul de la taille d'un message recu par le sdm (3 reels pour chaque corps d'interface)
    nb_send_slave = 3*nb_RBDY2_interf_slave
    nb_recv_slave = 3*nb_RBDY2_interf_slave

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
    ! V_RBDY2_interf_slave
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, isdm, i, i_std_bdy, i_RBDY2_interf_sdm, multi, compteur, err
    logical :: visible
    real(kind=8), dimension(3) :: vlocy

    !                         12345678901234567890123456
    character(len=26) :: IAM='DDM_2D::gather_V_interface'

    ! on va chercher la vitesse dans Vaux
    !id_vlocy='Vaux_'

    ! On rempli la table V_RBDY2_interf_sdm(rang)
    
    V_RBDY2_interf_slave_old=V_RBDY2_interf_slave

    do i_RBDY2_interf_sdm=1,nb_RBDY2_interf_slave 
       ibody=liste_RBDY2_interf_slave(i_RBDY2_interf_sdm)
       call get_vector_RBDY2(id_vlocy, ibody, vlocy, 3)
       V_RBDY2_interf_slave( 3*(i_RBDY2_interf_sdm-1)+1 : 3*(i_RBDY2_interf_sdm) )=vlocy

       !write(slave_io, *) 'i=', ibody, ' : V=', &
       !      V_RBDY2_interf_slave(3*(i_RBDY2_interf_sdm-1) + 1:3*(i_RBDY2_interf_sdm))
    end do
    
    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(V_RBDY2_interf_slave, nb_send_slave, MPI_DOUBLE_PRECISION, &
       V_interf4all, vect_nb_recv_host, vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)

    ! paranoid test
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if (rang /=0) return
                          !12345678901234567890
    !call write_MPI_INFO_R('V_INTERFACE_________',V_interf4all,&
    !      (/(i,i=1,sum(vect_nb_recv_host))/),sum(vect_nb_recv_host))

    do isdm = 1, Nsdm
       do i = 1, vect_nb_recv_host(isdm)/3
          V_RBDY2_interf_sdm(isdm)%particule(1:3,i) = & 
           V_interf4all(vect_shift(isdm) + 3*(i-1)+1 :  vect_shift(isdm) + 3*i)
       end do
    end do
    
 end subroutine gather_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_DF_gamma
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! type(T_ligne_i), dimension(Nsdm)   :: liste_RBDY2_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm)   :: V_RBDY2_interf_sdm(isdm)
    ! integer, dimension(/ max_multi, nbody /) :: body_particip
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! Liste des F_gamma signés par sous-domaines
    ! type(T_ligne_r), dimension(Nsdm)   :: DFg_RBDY2_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm)   :: Fg_RBDY2_interf_sdm(isdm)
    ! real, dimension(nb_liens_interf)   :: saut_V_interf_glob
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine

    integer :: nb_liens      ! nombre de liens de la particule considérée
    integer :: i_liens       ! indice du lien
    integer :: imulti, multi ! indice et multiplicité de la particule considérée
    integer :: ibody         ! indice du RBDY2 dans la liste globale
    integer :: i_parti       ! indice des particules d'interface
    integer :: isdm          ! indice sur les sous-domaines
    integer :: compteur      ! ce compteur doit être égal, en fin de boucle, à nb_liens_interf
    integer, dimension(1) :: tmp ! variable temporaire
    integer, dimension(2) :: sdm           ! sous domaine courant
    integer, dimension(2) :: signe         ! pour faire alterner le signe
    real(kind=8), dimension(:), pointer :: mass ! matrice de masse du RBDY2 courant

                             !123456789012345678901234
    character(len=24) :: IAM='DDM_2D::compute_DF_gamma'

    integer,      dimension(:), allocatable :: part_sdm ! indices de la particule courante de l'interface dans les deux sdm du lien considéré
    real(kind=8), dimension(:), allocatable :: DF_gamma ! increment de F_gamma calcule a partir du saut de vitesse courant [| V |]
                                                        ! taille : 3*nb_liens
    real(kind=8), dimension(:), allocatable :: V_jump   ! saut de vitesse courant
                                                        ! taille : 3*nb_liens

    type(G_matrix) :: A                ! matrice du systeme a resoudre pour calculer delta_F_gamma : A*DF = M*[| V |]
    character(len=8) :: matrix_type    ! type de stockage pour la G_matrix (i.e. matrice diagonale ou bande symetrique)
    integer :: bw                      ! largeur de bande de la matrice, dans le cas avec plusieurs liens
    integer, dimension(1) :: perm_fake ! tableau de pemuetation bidon utilise pour delcarer la G_matrix 
    integer :: info                    ! variable inidquant si la resolution du systeme s'est bien passee
 
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

    do i_parti = 1,nb_RBDY2_interf_glob

       ! RBDY2 sur lequel on travaille et sa multiplicité
       ibody = interface_globale(i_parti)
       !print *, "ibody=",ibody
       multi = multiplicite(ibody)  ! on va tapper dans multiplicité directement 
       ! Récupération des paramètres inertiels ! qu'est qu'on fait???
       mass => get_ptr_mass(ibody)
      
       ! calcul du nombre de liens pour le corps courant
       nb_liens=multi-1

       ! allocation de l'espace memoire pour les tabeleaux :
       !   * la table associant a une paire (indice de lien, sous-domaine du lien), le numero de RBDY2 corespondant
       !allocate(part_sdm(nb_liens, 2))
       allocate(part_sdm(multi))
       !   * le vecteur DF_gamma
       allocate(DF_gamma(3*nb_liens))
       !   * le vecteur [| V |]
       allocate(V_jump(3*nb_liens))

       ! pour chaque sous-domaine auquel appartient le corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
           ! On va pècher l'indice du RBDY2 ibody dans la liste des
          ! RBDY2 d'interface du sdm courant
          tmp=maxloc(liste_RBDY2_interf_sdm(sdm(1))%particule(:),&
                     liste_RBDY2_interf_sdm(sdm(1))%particule(:)==ibody)
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
          V_jump(3*(i_liens - 1) + 1 : 3*i_liens) = &
             signe(1) * V_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) + &
             signe(2) * V_RBDY2_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1))

          ! Pour le post-traitement, stockage des sauts de vitesses des grains d'interface
          saut_V_interf_glob(:, compteur)=V_jump(3*(i_liens - 1) + 1 : 3*i_liens)
       end do

!       ! on choisi la methide de resolution selon le nombre de liens
!       if (nb_liens == 1) then
!          ! 1 lien : matrice diagonale
!
!          ! on resoud directement
!          DF_gamma(1 : 3) = 0.5d0*mass(1 : 3)*V_jump(1 : 3)          

! 26/05/2012
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
          ! au moins 2 liens : matrice bande symetrique
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
             call faterr(IAM, 'No solution')
          end if

          ! destruction de la matrice
          call G_free(A)
       end if

       ! remise a 0 des DF_gamma pour chaque copie du corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
          ! on met a 0 le DF_gamma de la copie du corps appartenant au sdm courant
          DFg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) = 0.d0
       end do
      
       ! calcul de la resultante des DF_gamma pour chaque copie du corps

       ! pour chaque lien
       do i_liens=1, nb_liens
 
          ! on recupere les deux sous-domaines associes au lien courant
          sdm(1)=body_particip(i_liens, ibody)
          sdm(2)=body_particip(i_liens + 1, ibody)

          ! on ajoute la contribution du lien courant a la resultante des DF_gamma
          ! des deux copies du corps liees par le lien courant
          DFg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) = &
             DFg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(i_liens)) + &
             (-1) * signe(1) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)

          DFg_RBDY2_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1)) = &
             DFg_RBDY2_interf_sdm(sdm(2))%particule(:, part_sdm(i_liens + 1)) + &
             (-1) * signe(2) * DF_gamma(3*(i_liens - 1) + 1 : 3*i_liens)
       end do


       ! liberation de l'espace memoire occupe par les tableaux :
       !   * la table associant a une paire (indice de lien, sous-domaine du lien), le numero de RBDY2 corespondant
       deallocate(part_sdm)
       !   * DF_gamma
       deallocate(DF_gamma)
       !   * [| V |]
       deallocate(V_jump)
    end do

    if (compteur /= nb_liens_interf) then
       call faterr(IAM, "compteur /= nb_liens_interf")
    end if
                  
 end subroutine compute_DF_gamma
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_DFg

    implicit none

    integer            :: ibody, isdm, j
    integer            :: i_part_sdm
    real(kind=8), dimension(3) :: DF_gamma

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          do ibody = 1, nb_RBDY2_interf_sdm(isdm)
             DFg_interf4all(vect_shift(isdm) + 3*(ibody-1)+1 : vect_shift(isdm) + 3*ibody) = &
              DFg_RBDY2_interf_sdm(isdm)%particule(:, ibody)
          end do
       end do
    end if

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(DFg_interf4all, vect_nb_send_host, vect_shift, MPI_DOUBLE_PRECISION, &
       DFg_RBDY2_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    ! paranoid test
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! ecriture des interactions recues dans le fichier attache au processus courant
    !write(slave_io, *) "forces grains d'interface recues :"
    !do j=1, nb_RBDY2_interf_slave
    !   write(slave_io, *) 'j=', j, ' : ', DFg_RBDY2_interf_slave(3*(j - 1) + 1:3*j)
    !end do


    do i_part_sdm=1,nb_RBDY2_interf_slave
       ibody=liste_RBDY2_interf_slave(i_part_sdm)
       ! On stocke le DF_gamma qui nous intéresse pour ce corps
       ! Après avoir divisé par le pas de temps pour convertir
       ! l'impulsion calculée en force moyenne sur le pas de temps
       DF_gamma=(1.D0/H)*DFg_RBDY2_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm)
       call put_vector_RBDY2('Fext_',ibody,DF_gamma,3)
       call comp_free_vlocy_one_RBDY2(ibody)

       ! Mise a jour des F_gamma pour chaque copie du corps
       Fg_RBDY2_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm) = & 
        Fg_RBDY2_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm) + &
         DFg_RBDY2_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm)
    end do

 end subroutine scatter_DFg
!----------------------------------------------------------------

!----------------------------------------------------------------
!am: calcul de la convergence avec reduction des normes calculees dans chaque sdm
subroutine check_convergence_in_DDM(is_converged)

   implicit none

   ! variable de sortie
   ! pour statuer si on a converge, variable connue par tous les sdm mais stockee par le processus 0
   logical, intent(out) :: is_converged

   ! variables locales

   ! nombre de contacts actifs (Nactif: par sdm, Nactif4all: pour tout l'echantillon)
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

   if (rang == 0) then
      !print*, 'Sums4all=', Sums4all
      !print*, 'Maxima4all=', Maxima4all

      ! on calcule les quantites necessaires pour statuer de la convergence sur le maitre
      call compute_convergence_norms_nlgs(Nactif4all, Sums4all(1), Sums4all(2), Maxima4all(1), &
                                          Sums4all(3), Sums4all(4), Maxima4all(2), Sums4all(5), tol, &
                                          QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR) 

      !print*, 'QuadDV=', QuadDV
      !print*, 'MaxmDV=', MaxmDV
      !print*, 'QuadDVR=', QuadDVR
      !print*, 'MaxmDVR=', MaxmDVR
      !print*, 'MeanDVoR=', MeanDVoR

      ! on teste la convergence du NLGS 
      call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, nlgs_converged)

      !print*, 'NLGS a converge=', nlgs_converged

      ! on teste si le probleme d'interface a converge
      !am: on devrait pouvoir virer l'argument entier, non?
      call compute_egluing(1, tol, intrf_converged)

      !print*, 'INTRF a converge=', intrf_converged

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

end subroutine

!----------------------------------------------------------------
 subroutine test_is_converged(is_converged,is_converged4all)

    implicit none
    
    logical, intent(in)  :: is_converged
    logical, intent(out) :: is_converged4all

    ! chaque sdm envoie son etat de convergence
    call MPI_ALLREDUCE(is_converged, is_converged4all, 1, MPI_LOGICAL, MPI_LAND, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

 end subroutine test_is_converged
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_egluing(iddm,TOL,is_converged) 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! saut_V_interf_glob
    ! argument "explicite"
    integer                          :: iddm         ! Sndice d'itération ddm 
    logical, intent(out)             :: is_converged ! Si la valeur d'entrée est .true. et 
                                                     ! egluing < TOL, on retourne is_converged à .true. 
    real(kind=8), intent(in)         :: TOL          ! Tolérance du critère de convergence

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine
    character(len=28) :: clout ! pour calculer le nom du fichier
    integer           :: nfich ! io unit
    integer           :: isdm, i_RBDY2

    if ( rang /= 0 ) return

    egluing=0.d0
    if (nb_liens_interf > 0) then

       !! Calcul de l'erreur globale de recollement
       egluing=sqrt(dot_product(saut_V_interf_glob(1,:),saut_V_interf_glob(1,:)) +&
                   dot_product(saut_V_interf_glob(2,:),saut_V_interf_glob(2,:)))/&
                   (nb_liens_interf)

       !print* , "####################"
       !print* , "egluing=", egluing

       is_converged = (egluing < TOL)

       !!      1234567890123456789012345678 
       !clout='POSTPRO/MY_ERROR_xxxxxxx.DAT'
       !write(clout(18:24), '(I7.7)') Nstep
       !nfich=get_io_unit()

       !! Ecriture du champs saut_Vinterf_glob pour le liens numéro 1
       !!open(unit=100, file='POSTPRO/MY_ERROR.DAT', form='formatted', action='write')
       !open(unit=nfich, file=clout, form='formatted', action='write')
       !write(nfich, '(I7, 1X, D14.7, 1X, D14.7, 1X, D14.7)') iddm, egluing1, egluing2, egluing3 
       !close(nfich)
    end if

 end subroutine compute_egluing
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

 !am: fonction qui calcule, dans Vaux, les vitesses obtenues a partir des forces de contact, stokees dans Iaux, pour les grains d'interface
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
 subroutine gather_X_V_begin
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY2_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, ibdy, imulti
    integer            :: irecv
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(3) :: X_tmp, V_tmp

    if ( rang == 0 ) then
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

       !write(slave_io, *) "Ibeg, Xbeg, Vbeg: compteur =", compteur
       !write(slave_io, *) 'ibody=', ibody, ' : Xbeg(f:f) ', Xbeg_slave( 3*(compteur-1)+1 : 3*compteur)
       !write(slave_io, *) 'ibody=', ibody, ' : Xbeg ', X_tmp
       !write(slave_io, *) 'ibody=', ibody, ' : Vbeg(f:f) ', Vbeg_slave( 3*(compteur-1)+1 : 3*compteur)
       !write(slave_io, *) 'ibody=', ibody, ' : Vbeg ', V_tmp

    end do

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Ibeg_slave, nb_send_slave, MPI_INTEGER, &
       Ibeg4all, vect_nb_recv_host, vect_shift, MPI_INTEGER, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)


    if ( rang == 0 ) then
    end if
    !write(slave_io, *) "size(Xbeg_slave)",size(Xbeg_slave)
    !write(slave_io, *) "3*nb_send_slave = ",3*nb_send_slave

    if ( rang == 0 )  then
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
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Récuperation des vitesses d'interface
    CALL MPI_GATHERV(Vbeg_slave, 3*nb_send_slave, MPI_DOUBLE_PRECISION, &
       Vbeg4all, vect_nb_recv_host, vect_shift, MPI_DOUBLE_PRECISION, &
       0, MPI_COMM_WORLD, code)
    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    if ( rang /= 0 ) return
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

 end subroutine gather_X_V_begin
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
    ! real(kind=8), dimension(3,nb_RBDY2_interf_glob) :: V_RBDY2_interf_glob
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, i_RBDY2_sdm, i_RBDY2_interf_glob, imulti, multi, isdm       
    integer     , dimension(1) :: tmp

    if ( rang /= 0 ) return

    V_RBDY2_interf_glob=0

    do i_RBDY2_interf_glob=1,nb_RBDY2_interf_glob
       ibody=interface_globale(i_RBDY2_interf_glob)
       multi=multiplicite(ibody)
       do imulti=1,multi
          isdm = body_particip(imulti,ibody)
          tmp=maxloc(liste_RBDY2_interf_sdm(isdm)%particule(:),&
                     liste_RBDY2_interf_sdm(isdm)%particule(:)==ibody)
          i_RBDY2_sdm=tmp(1)
          
          V_RBDY2_interf_glob(:,i_RBDY2_interf_glob)=V_RBDY2_interf_glob(:,i_RBDY2_interf_glob) &
                                       + V_RBDY2_interf_sdm(isdm)%particule(:,i_RBDY2_sdm) / multi
       end do
    end do

    do isdm = 1, Nsdm
       do i_RBDY2_sdm = 1, nb_RBDY2_interf_sdm(isdm)
          ibody = liste_RBDY2_interf_sdm(isdm)%particule(i_RBDY2_sdm)
          tmp=maxloc(interface_globale, interface_globale==ibody)
          i_RBDY2_interf_glob=tmp(1)

          V_RBDY2_interf_sdm(isdm)%particule(:,i_RBDY2_sdm) = &
              V_RBDY2_interf_glob(:,i_RBDY2_interf_glob)
       end do
    end do            

 end subroutine fix_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine scatter_V_interface

    implicit none

    integer            :: ibody, isdm, j
    integer            :: i_part_sdm
    real(kind=8), dimension(3) :: V_tmp

    if ( rang == 0 ) then
       do isdm=1, Nsdm
          do ibody = 1, nb_RBDY2_interf_sdm(isdm)
             V_interf4all(vect_shift(isdm) + 3*(ibody-1)+1 : vect_shift(isdm) + 3*ibody) = &
              V_RBDY2_interf_sdm(isdm)%particule(:, ibody)
          end do
       end do
    end if

    ! Envoi de sa liste de grains d'interface a chaque sdm
    call MPI_SCATTERV(V_interf4all, vect_nb_send_host, vect_shift, MPI_DOUBLE_PRECISION, &
       V_RBDY2_interf_slave, nb_recv_slave, MPI_DOUBLE_PRECISION, 0, MPI_COMM_WORLD, code)

    if (code /= MPI_SUCCESS) call MPI_ABORT(MPI_COMM_WORLD, 2, code)

    ! Ecriture des interactions recues dans le fichier attache au processus courant
    !write(slave_io, *) "vitesses grains d'interface recues :"
    !do j=1, nb_RBDY2_interf_slave
    !   write(slave_io, *) 'j=', j, ' : ', V_RBDY2_interf_slave(3*(j - 1) + 1:3*j)
    !end do


    do i_part_sdm=1,nb_RBDY2_interf_slave
       ibody=liste_RBDY2_interf_slave(i_part_sdm)
       ! Stockage de V_tmp pour le corps couran courant
       V_tmp=V_RBDY2_interf_slave(3 * (i_part_sdm - 1) + 1 : 3*i_part_sdm)
       call put_vector_RBDY2('V____',ibody,V_tmp,3)
    end do

 end subroutine scatter_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_visibility_4all_in_DDM(proc)
 
    implicit none
   
    integer, intent(in), optional :: proc
    integer                       :: i

    if ( .not. present(proc) ) then
       call set_visibility_4all_RBDY2(mask_in_slave,nbody)
    else
       if ( rang == proc ) then
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
 subroutine write_DDM_INFO
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! argument "explicite"
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer :: ibody         ! indice du RBDY2 dans la liste réduite
    integer :: i_RBDY2       ! indice du RBDY2 dans la liste globale
    integer :: isdm          ! indice sur les sous-domaines
    integer :: multi         ! multiplicité
    integer :: nfich         ! numéro de io
    integer :: lc            
    logical :: ivisibility   ! visibilité

    integer, save :: Nprint=0

    character(len=27)  :: clout ! pour calculer le nom du fichier
    character(len=7)   :: numfic

    Nprint=Nprint+1

    WRITE(numfic,'(I7)') Nprint
    numfic = ADJUSTL(numfic)

    !      123456789012345678901234567 
    clout='OUTBOX/DDM_INFO.OUT.xxxxxxx'
    write(clout(21:27), '(A7)') numfic

    nfich=get_io_unit()

    lc = LEN_TRIM(clout)
    open(unit=nfich, file=TRIM(clout(1:lc)), form='formatted', action='write')
    do i_RBDY2 = 1,get_nb_RBDY2()

       isdm= floor ( real( (i_RBDY2 - 1 ) / nbody) ) + 1
       ibody=i_RBDY2 - (isdm-1)*nbody
       ivisibility=mask_particip(ibody,isdm)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, L1, 1X, I3, 1X, I3)') i_RBDY2, ivisibility, isdm, multi

    end do
    close(nfich)

 end subroutine write_DDM_INFO
!----------------------------------------------------------------

!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!
!Début des routines de sortie : DISPLAY, OUTBOX et POSTPRO (during computation)!
!------------------------------------------------------------------------------!
!------------------------------------------------------------------------------!

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
       
       mean_nb_RBDY2_slave             = 0.d0
       mean_nb_RBDY2_interf_slave      = 0.d0
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
       mean_nb_INTRF = mean_nb_INTRF + real(nb_INTRF)
       mean_nb_adjac_slave = mean_nb_adjac_slave + real(nb_adjac_slave)
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
    
    DO icdan=1,get_nb_inters( i_dkdkx )
       CALL get_gaps( i_dkdkx, icdan, gap, gapBegin)

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
    WRITE(unit=nfich_sdm_inf,fmt='(D14.7,6(1X,I8))') TPS,nb_RBDY2_slave,nb_RBDY2_interf_slave, &
                     nb_interactions_slave,nb_fine_interactions_slave, &
                     nb_INTRF,nb_adjac_slave
    CLOSE(unit=nfich_sdm_inf) 

  end subroutine write_SDM_INFORMATIONS
!----------------------------------------------------------------

!----------------------------------------------------------------
  subroutine write_MEAN_SDM_INFORMATIONS
  
    implicit none

    OPEN(unit=nfich_mean_sdm_inf,file=clout_mean_sdm_inf,STATUS='OLD',POSITION='APPEND') 
    WRITE(unit=nfich_mean_sdm_inf,fmt='(7(1X,D14.7))') TPS,mean_nb_RBDY2_slave,mean_nb_RBDY2_interf_slave, &
                     mean_nb_interactions_slave,mean_nb_fine_interactions_slave, &
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

    if (rang /= 0) return

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
    character(len=29)  :: clout_DDM_INFO ! pour calculer le nom du DDM.INFO

    ! l'entete du fichier DOF n'est ecrite que par le processus de rang 0
    call Write_xxx_dof_Ol(1, rang /= 0)
    CALL write_xxx_dof_RBDY2(1,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang 0
    call Write_xxx_Vloc_Rloc_Ol(1, rang /= 0)
    if ( ntact /= 0 ) CALL write_xxx_Vloc_Rloc_DKDKx(1)


    ! Handle unique pour chaque processus
    nfich=rang+1000 
    Nprint=Nprint+1
    sdm=rang+1


    !               12345678901234567890123456789
    clout_DDM_INFO='OUTBOX/DDM.INFO.xxxxx.xxxxxxx'
    write(clout_DDM_INFO(17:21), '(I5.5)') sdm
    write(clout_DDM_INFO(23:29), '(I7.7)') Nprint 

    OPEN(unit=nfich,STATUS='REPLACE',file=clout_DDM_INFO)
    do ibody = 1,nbody

       visibility=mask_in_slave(ibody)
       multi=multiplicite(ibody)
       
       write(nfich, '(I8, 1X, L1, 1X, I3)') ibody, visibility, multi

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
    CALL write_xxx_dof_RBDY2(2,1,nbody)

    ! l'entete du fichier VlocRloc n'est ecrite que par le processus de rang 0
    call Write_xxx_Vloc_Rloc_Ol(2, rang /= 0)
    if ( ntact /= 0 ) CALL write_xxx_Vloc_Rloc_DKDKx(2)

    ! Handle unique pour chaque processus
    nfich=rang+1000 
    sdm=rang+1

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

! Procédure d'arrêt de MPI
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

! Procédure de nettoyage de la memoire encore allouee
subroutine clean_ddm

   implicit none

   ! Nettoyage de la memoire encore allouee dans le module de detection grossiere
   call clean_module 

end subroutine clean_ddm


end module DDM_MPI_2D
