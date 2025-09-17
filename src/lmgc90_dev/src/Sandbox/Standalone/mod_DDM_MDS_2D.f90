            !---------------------------------------------------!
            !          NSCDD Multidomaines Séquentiel 2d        !
            !   Écrit par : Vincent Visseq & Alexandre Martin   !
            ! Module importé dans DKDK_standalone_DDa_MDSM.f90  !
            !---------------------------------------------------!

module DDM_MDS_2D 

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
                  get_list_INTRF_DKDKx
use anonymous_ptr_container, only : get_object               => get_data                   , &
                                    get_nb_objects           => get_nb_data                , &
                                    get_status                                             , &
                                    close_container          => close_ptr_container        , &
                                    open_container           => open_ptr_container         , &
                                    add_object_to_container  => add_object_to_ptr_container, &
                                    display_object_container => display_ptr_container      , &
                                    erase_container          => erase_ptr_container        , &
                                    container                => PTR_CONTAINER
use nlgs, only : shift_icdan, RnodHRloc_nlgs, compute_local_free_vlocy
use anonymous
use rough_detections
use tact_behaviour  ! pour chercher la plus grande distance d'alerte
use a_matrix        ! pour utiliser les G_matrix


implicit none

!am: on declare tout prive (a priori) pour eviter les effets de bord
private

!am: ancienne archi => on ne travaille qu'avec des disques (et des clusters de disques)
integer(kind=4)                                :: nbody          ! nombre "vrai" de RBDY2 
integer(kind=4)                                :: ntact          ! nombre de contacteurs disques
logical         :: xperiodic=.false.
real(kind=8)    :: xperiode = 0.d0

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
integer(kind=4), dimension(:), allocatable     :: multiplicite   ! vecteur recenssant le nombre de participation aux sdm des corps
integer(kind=4), dimension(:), allocatable     :: multiplicite_old ! vecteur recenssant le nombre de participation aux sdm des corps
                                                                 ! à la répartition en sous-domaines précédente
integer(kind=4)                                :: max_multi      ! maximum de multiplicité pour une particule
integer(kind=4), dimension(:,:), allocatable   :: body_particip  ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                                 ! participent chaque corps (alloué dans init_dd)
integer(kind=4), dimension(:,:), allocatable   :: body_particip_old ! tableau de dimension (max_multi, nbody) recenssant les sdm auquels 
                                                                 ! participent chaque corps (alloué dans init_dd) à la répartiction en
                                                                 ! sous-domaine précédente
integer(kind=4), dimension(:,:,:), allocatable :: migration_tab  ! table des migrations par sous-domaines
integer(kind=4)                                :: nb_migrations  ! nombre de migrations
logical, dimension(:,:), allocatable           :: mask_particip  ! matrice (/ nbody,Nsdm /) permettant de déterminer, pour
                                                                 ! chaque sous-domaines, quels sont les objets visibles par ce sdm
logical, dimension(:,:), allocatable           :: mask_redim     ! matrice (/ Nb_RBDY2,Nsdm /) permettant de déterminer, pour
                                                                 ! chaque sous-domaines, quels sont les objets visibles par ce sdm
                                                                 ! dans la liste complète des particules dupliquées
logical, dimension(:), allocatable             :: mask_RBDY2     ! pour le cas multi-domaine séquentiel : tableau de 

!-------------------------------------------------------------------------------------------------------------------
! Gestion des masses
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:), allocatable        :: masse_ref        ! vecteur contenant les masses de reference pour chaque corps
real(kind=8), dimension(:), allocatable        :: masses_courantes ! vecteur contenant les masses courantes pour chaque corps
                                                                   ! taille (/Nsdm*nbody/)

!-------------------------------------------------------------------------------------------------------------------
! Info pour la sous-structuration géométrique de l'échantillon
!-------------------------------------------------------------------------------------------------------------------
real(kind=8)                                   :: Bleft, Bright, Bup, Bdown ! caractéristiques de la boite englobante 
integer(kind=4)                                :: Nsdm1, Nsdm2              ! nombre des sous-domaines en abscisse et en ordonnée 
integer(kind=4)                                :: Nsdm                      ! nombre total de sous-domaines 
real(kind=8), dimension(2)                     :: dim_sdm                   ! dimensions des sous domaines "DDM"
real(kind=8)                                   :: TimeStep                  ! pas de temps (fixe) utilisé pour le calcul


!-------------------------------------------------------------------------------------------------------------------
! Structures associées à la detection des contacts grossiers
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), save    :: rough_contact     ! table  de visibilité
 integer                  :: nb_rough_contact  ! taille table  de visibilité
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), save    :: shiftted_rough    ! table  de visibilité
 integer                  :: nb_shiftted_rough ! taille table  de visibilité
!-------------------------------------------------------------------------------------------------------------------
integer                            :: nb_INTRF              ! Nombre de contacts fins taggés 'INTRF'
integer,dimension(:), allocatable  :: list_INTRF            ! Liste des indices des contacts taggés 'INTRF'

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
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY2_interf_sdm        ! F_gamma des RBDY2 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY2_interf_sdm_old    ! F_gamma des RBDY2 d'interface par sdm (pas -1)
type(T_ligne_r), dimension(:), allocatable  :: DFg_RBDY2_interf_sdm       ! DF_gamma des RBDY2 d'interface par sdm 
integer(kind=4), dimension(:), allocatable  :: nb_RBDY2_interf_sdm        ! nombre de corps d'interface par sdm
logical                                     :: repartition_just_made = .false. ! Permet de savoir si la répartition
                                                                               ! en sous-domaine viens d'être effectuée      


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
! Fin des déclarations des variables globales au module
!-------------------------------------------------------------------------------------------------------------------


!am: on donne la liste des fonctions publiques (utilisables hors du module)
public                                &
   init_dd,                           &
   new_dd,                            &
   get_multiplicite_in_DDM,           &
   set_visibility_4all_in_DDM,        &
   launch_set_anonymous_to_rough_4all,&
   erase_rough_contact,               &
   erase_shiftted_rough,              &
   get_list_INTRF_in_DDM,             &
   RnodHRloc_list_in_DDM,             &
   compute_local_free_vlocy_in_DDM,   &
   stock_V_interface,                 &
   compute_F_gamma,                   &
   compute_egluing,                   &
   compute_boundary_sample_in_DDM2d,  &
   comp_V_list_RBDY2_in_DDM,          &
   set_F_gamma,                       &
   set_DF_gamma,                      &
   gather_Xbeg_Vbeg,                  &
   fix_migrants,                      &
   fix_V_interface,                   &
   compute_V_interface_sdm,           &
   set_V_interface,                   &
   set_modified_mass_in_DDM,          &
   write_DDM_INFO,                    &
   creation_domaines,                 &
   clean_ddm

contains

!-------------------------------------------------------------------------------------------------------------------
 function init_dd(Nsdm1_,Nsdm2_, TimeStep_)

    implicit none

    ! Variables d'entrée
    integer, intent(in)      :: Nsdm1_,Nsdm2_
    real(kind=8), intent(in) :: TimeStep_

    ! Valeur de retour
    integer :: init_dd

    ! Variables locales
    integer :: isdm,err

    TimeStep=TimeStep_
    Nsdm1=Nsdm1_
    Nsdm2=Nsdm2_

    ! Calcule du nombre total de sous-domaines
    Nsdm=Nsdm1*Nsdm2
    ! Calcul de la multiplicité maximale pour une particule
    max_multi=max(4,Nsdm1,Nsdm2) ! Un cluster en diagonale ferait tout sauter
    ! on le renvoie
    init_dd=Nsdm

 end function init_dd
!-------------------------------------------------------------------------------------------------------------------

!am: procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
!     (les coordonnees des centre d'inertie, les numeros de sous-domaine pour chaque corps)
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd(nb_RBDY2,ntact_)

    implicit none

    ! variables d'entrée
    integer, intent(in)    :: nb_RBDY2,ntact_

    ! variables locales
    integer                :: isdm,ibody,err
    integer                :: isee
                                  !12345678901234
    character(len=14)      :: IAM='DDM_2D::new_dd'

    ! MDS --> on a duplique les corps dans le standalone!! 
    ! on alloue un masque de taille (/Nsdm,nbody/) i.e. de la taille de tous les
    ! corps (y compris les dupliqués!)     
    if (.not. allocated(mask_RBDY2)) then
       allocate(mask_RBDY2(nb_RBDY2), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_RBDY2")
    end if
    print *, "new_dd::shape(mask_RBDY2)", shape(mask_RBDY2)
    mask_RBDY2=.true.

    if (.not. allocated(mask_redim)) then
       allocate(mask_redim(nb_RBDY2,Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_redim")
    end if
    print *, "new_dd::shape(mask_redim)", shape(mask_redim)
    ! et on divise par le nombre
    ! de sous-domaines pour retrouver le nombre de corps original
    nbody=nb_RBDY2/Nsdm
    ntact=ntact_/Nsdm

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
    ! les deux lignes suivantes sont donc à modifier pour être générique!
    minray     = get_min_radius_DISKx()
    maxray     = get_max_radius_DISKx()

    ! calcul de la plus grande distance d'alerte, parmi toutes les lois
    ! N.B. c'est bourrin!
    alert = see(1)%alert
    do isee=1, size(see)
       if (see(isee)%alert > alert) alert = see(isee)%alert
    end do
 
!-------------------------------------------------------------------------------------------------------------------
! Gestion des multiplicitées et des masques (3 types de masques, car en multidomaine séquentiel ça complique un peut)
!-------------------------------------------------------------------------------------------------------------------
    !    * tableau des corps visibles par sous-domaines
    if (.not. allocated(mask_particip)) then
       allocate(mask_particip(nbody, Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de la liste des corps visibles par sous-domaines")
    end if

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

    !    * table multiplicite
    if (.not. allocated(multiplicite)) then
       allocate(multiplicite(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de multiplicite")
    end if

    !    * table multiplicite_old
    if (.not. allocated(multiplicite_old)) then
       allocate(multiplicite_old(nbody), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de multiplicite_old")
    end if

    ! pas très malin d'allouer un tableau aussi gros, pour au final y mettre 
    ! très peu de données utiles!!!

    !    * tables des migrations
    if (.not. allocated(migration_tab)) then
       allocate(migration_tab(max_multi,nbody,Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation de migration_tab")
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

    !    * table des F_gamma sur les grains d'interface des sdm
    if (.not. allocated(Fg_RBDY2_interf_sdm)) then
       allocate(Fg_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de Fg_RBDY2_interf_sdm")
    end if
    !    * table des F_gamma sur les grains d'interface des sdma (pas -1)
    if (.not. allocated(Fg_RBDY2_interf_sdm_old)) then
       allocate(Fg_RBDY2_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de Fg_RBDY2_interf_sdm_old")
    end if
    !    * table des DF_gamma sur les grains d'interface des sdm
    if (.not. allocated(DFg_RBDY2_interf_sdm)) then
       allocate(DFg_RBDY2_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de DFg_RBDY2_interf_sdm")
    end if

 end subroutine new_dd
!----------------------------------------------------------------



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
    real(kind=8), dimension(3)                 :: tmp_coor ! coordonnées temporaire du centre d'un diskx

    logical, save                              :: first_real_step=.true.    

                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_2D::creation_domaines"

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
    integer                                    :: isee     ! indice de boucle sur les lois d'interaction
    real(kind=8), dimension(2)                 :: sep
    ! pour creer le nouvel objet anonyme a ajouter a rough_contact
    integer(kind=4), dimension(:), pointer     :: i4 
    real(kind=8), dimension(:), pointer        :: r8
    character(len=5), dimension(:), pointer    :: c5
    character(len=128), dimension(:), pointer  :: cx

    i4 => null()
    r8 => null()
    c5 => null()
    cx => null()

    !--------------------------------------------------------------------------------------
    ! recuperation des coordonnees des centre d'inertie des RBDY2
    do ibody=1, nbody
      coord_ci(1:3, ibody) = get_coorTT(ibody,0)
    end do

    ! Recuperation des coordonnees des centre d'inertie des DISKx
    do itact=1, ntact
                ! <=> get_coor |  indice du RBDY2    |  indice du contacteur dans la
                !                                    | liste des contacteurs de ce RBDY2
      tmp_coor = get_coorTT(diskx2bdyty(1, itact), diskx2bdyty(2, itact))
      coord_ct(1:2, itact) = tmp_coor(1:2)
      ! Pour la méthode des boites générique
      radius_ct(itact)=get_radius_DISKx(itact)
    end do


    ! Calcul de la boite englobante de l'échantillon total, puis des caractéristiques de sous domaines 
    call caracteristique_domaine()


    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des centres d'inertie des RBDY2
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Bleft, Bdown, dim_sdm, &
                             3, nbody, coord_ci, repart_sdm_ci,   & 
                             .true.)                              


    ! Méthode des boites générique
    call boxes_method(coord_ct,radius_ct,alert,rough_contact,xperiodic,xperiode,minray,maxray)

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

          allocate(i4(2))
          i4(1:2) = cdan(1:2)

          call add_object_to_container(rough_contact, nb_rough_contact, c5, i4, r8, cx)
       end if
    end do

    ! on ferme rough_contact
    call close_container(rough_contact)
    ! on vide le container des interactions obtenues en utilisant la methode des boites generiques
    call erase_container(rough_contact_gen)

    ! On calcule le nombre de contacts grossiers connus à cette étape.
    ! Certains vont être suprimés car ils concernent des contacts entre
    ! deux disques d'un même cluster ou encore ne se voient pas du fait
    ! la table de visibilité. Cet écrémage aura lieu dans la fonction
    ! creation_shiftted_rough. Dans tous les cas, ces contacts seront 
    ! écrémés dans set_anonymous_to_rough du module DKDKx, si ce n'était
    ! fait ici.
    nb_rough_contact = get_nb_objects(rough_contact)
    !print *, "nb_rough_contact", nb_rough_contact


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
    call creation_shiftted_rough 
 
    ! todo :  sortir l'appel de cette fonction de ce module et la placer dans le standelone
    ! modification des proprietes des RBDY2 d'interface (dupliques)
    call compute_modified_mass


    ! Gestion/récupération des F_gamma du pas précédent quand on le peut
    if (.not. first_real_step) then
       ! Allocation des listes de RBDY2 d'interface par sous-domaine du pas précédent
       do isdm=1,Nsdm
          if ( associated(liste_RBDY2_interf_sdm_old(isdm)%particule) ) & 
               deallocate(liste_RBDY2_interf_sdm_old(isdm)%particule)
          allocate(liste_RBDY2_interf_sdm_old(isdm)%particule(nb_RBDY2_interf_sdm(isdm)), stat=err)
          if (err/=0) stop "Pb d'allocation de liste_RBDY2_interf_sdm_old(isdm)%particule"
          liste_RBDY2_interf_sdm_old(isdm)%particule(:)=liste_RBDY2_interf_sdm(isdm)%particule(:)
    
          if ( associated(Fg_RBDY2_interf_sdm_old(isdm)%particule) ) & 
               deallocate(Fg_RBDY2_interf_sdm_old(isdm)%particule)
          allocate(Fg_RBDY2_interf_sdm_old(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
          if (err/=0) stop "Pb d'allocation de Fg_RBDY2_interf_sdm_old(isdm)%paticule"
          Fg_RBDY2_interf_sdm_old(isdm)%particule(:,:)=0.d0
          Fg_RBDY2_interf_sdm_old(isdm)%particule(:,:)=Fg_RBDY2_interf_sdm(isdm)%particule(:,:)
       end do 
    end if


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

    if (.not. first_real_step) repartition_just_made = .true.
    if (first_real_step) first_real_step=.false. 

 end subroutine creation_domaines
!----------------------------------------------------------------

! Création du point milieu entre les centres de gravité des particules
! en contact. Ce "centre de contact" permettra ensuite de déterminer les 
! particules possédant des contacts dans plus de deux sous-domaines.
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
! sont définies à partir des positions des centre d'inertie des RBDY2
! de l'échantillon considéré.
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


! Routine créant la table de participation des particules aux différents
! sous-domaines --> body_particip.
! Le masque de participation par sous-domaines mask_particip en est déduit.
! La multiplicité des particules finalement stockée dans le vecteur multiplicity.
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
    mask_RBDY2=.false.
    mask_redim=.false.

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

       mask_RBDY2(((isdm-1)*nbody)+1:isdm*nbody)=mask_particip(:,isdm)
       mask_redim(((isdm-1)*nbody)+1:isdm*nbody,isdm)=mask_particip(:,isdm)

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
    integer(kind=4)                         :: first, nb_new

    migration_tab = 0
    nb_migrations = 0

    do ibody = 1,nbody
       nb_new = 0
       multi=multiplicite(ibody)
       multi_old=multiplicite_old(ibody)

       ! On stoque le num du premier sdm auquel participe ibody
       ! Si de nouveaux sous-domaines doivent gérer ibody, c'est le sdm 
       ! first qui devra envoyer le l'état du ibody à ces nouveaux gestionnaires
       ! (ou co-gestionnaires)
       first = body_particip_old(1,ibody)

       do imulti= 1, multi
          isdm=body_particip(imulti,ibody)

          if ( count(isdm==body_particip_old(1:multi_old,ibody)) /=0 ) cycle

          print *, 'ibody=', ibody
          print *, 'body_particip=',body_particip(:,ibody)
          print *, '-------------------------------------------' 
          print *, 'body_particip_old=',body_particip_old(:,ibody)
          print *, '-------------------------------------------' 

          nb_new=nb_new + 1
          migration_tab(nb_new,ibody,first)=isdm
          print *, 'migration_tab(',nb_new,',',ibody,',',first,')=',migration_tab(nb_new,ibody,first)
          nb_migrations=nb_migrations + 1

       end do
    end do

    print *, "nb_migrations=",nb_migrations

 end subroutine migration_treatment
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
    integer                                 :: ibody, ibody_new, ibody_old
    integer                                 :: isdm, isdm_new
    integer                                 :: nb_new_sdm, i_new_sdm
    integer, dimension(1)                   :: tmp
    real(kind=8), dimension(3)              :: Xbeg_tmp,Vbeg_tmp

    do isdm=1,Nsdm
       do
          ! S'il n'y à plus de grains à faire connaitre, on sort
          if ( count(migration_tab(1,:,isdm) /= 0)==0) exit

          ! S'il y en a, on procède par ordre croissant (au sens
          ! de l'emplacement mémoire)
          tmp = maxloc(migration_tab(1,:,isdm))

          ibody = tmp(1)
          nb_new_sdm = count(migration_tab(:,ibody,isdm)/=0)

          print *, "le sdm", isdm, "envoi les informations du corps,", ibody
 
          do i_new_sdm = 1, nb_new_sdm
             ! Le sous-domaine auquel on veut donner X,V est :
             isdm_new = migration_tab(i_new_sdm,ibody,isdm)

             ibody_new = (isdm_new-1)*nbody+ibody
             ibody_old = (isdm-1)*nbody+ibody
            
             Xbeg_tmp=0.d0 
             Vbeg_tmp=0.d0
             call get_vector_RBDY2('Xbeg_',ibody_old,Xbeg_tmp,3)
             call put_vector_RBDY2('Xbeg_',ibody_new,Xbeg_tmp,3)
             call get_vector_RBDY2('Vbeg_',ibody_old,Vbeg_tmp,3)
             call put_vector_RBDY2('Vbeg_',ibody_new,Vbeg_tmp,3)

             ! On met à zéro la case de migration_tab
             ! que l'on vient de traiter 
             migration_tab(i_new_sdm,ibody,isdm)=0
          end do
       end do
    end do

 end subroutine fix_migrants
!----------------------------------------------------------------

!----------------------------------------------------------------
 function get_multiplicite_in_DDM(ibody)

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: multiplicite
    integer(kind=4), intent(in) :: ibody
    ! Argument de sortie
    integer(kind=4)             :: get_multiplicite_in_DDM
    

    get_multiplicite_in_DDM=multiplicite(ibody)

 end function get_multiplicite_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine creation_shiftted_rough

    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: nb_rough_contact, mask_contact, rough_contact 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: shiftted_rough, nb_shiftted_rough
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: icdan, icdbdy, ianbdy, isdm
    integer(kind=4)                           :: icdtac, iantac, isee
    character(len=5)                          :: cdcol, ancol
    ! structure de l'anonymous container
    integer(kind=4),    dimension(:), pointer :: cdan
    integer(kind=4),    dimension(:), pointer :: shiftted_xcdan
    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    type(T_object)                            :: contact
  
    c5 => null()
    r8 => null()
    cx => null()
   
    shiftted_xcdan => null()

    ! Pour chaque contact
    nb_shiftted_rough=0
    do icdan=1,nb_rough_contact
       isdm=repart_sdm_cc(icdan)
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
 
       nb_shiftted_rough=nb_shiftted_rough+1
       allocate(shiftted_xcdan(3))

       ! Pour le cas séquentiel only!!!!
       shiftted_xcdan(1)=cdan(1)+((isdm-1)*ntact)!
       shiftted_xcdan(2)=cdan(2)+((isdm-1)*ntact)!

       ! numéro de corps associé au contacteur courant
       icdbdy=diskx2bdyty(1, cdan(1))
       ianbdy=diskx2bdyty(1, cdan(2))
       if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
          shiftted_xcdan(3)=INTRF
       else
          shiftted_xcdan(3)=NOINT
       end if

       call add_object_to_container(shiftted_rough,nb_shiftted_rough,c5,shiftted_xcdan,r8,cx)

    end do

    ! pour tester que les shiftted_rough sont bien remplis jusqu'au bout
    ! print *, "display_shiftted_rough"
    ! call display_object_container(shiftted_rough)
    print *, "nb_shiftted_rough =", nb_shiftted_rough
    call close_container(shiftted_rough)

 end subroutine creation_shiftted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
! Pour le cas de la détection fine séquentielle
!----------------------------------------------------------------
 subroutine launch_set_anonymous_to_rough_4all
    implicit none
    !--------------------------------------
    call set_anonymous_to_rough(shiftted_rough,nb_shiftted_rough)
 end subroutine
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_rough_contact
    implicit none
    !--------------------------------------
    call erase_container(rough_contact)
 end subroutine erase_rough_contact
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine erase_shiftted_rough
    implicit none
    !--------------------------------------
    call erase_container(shiftted_rough)
 end subroutine erase_shiftted_rough
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
!am: calcul des torseurs des reactions de contact at stockage dans Iaux
 subroutine RnodHRloc_list_in_DDM
    implicit none
    !--------------------------------------

    ! variable locale
    integer :: storage_reac

    ! stockage des reactions de contact dans Iaux
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
 subroutine compute_modified_mass
    
    implicit none
    
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 
    integer              :: isdm, i_rbdy2         ! indice de sous-domaine considere, indice de RBDY2
    
    do ibody = 1,nbody
    
       ! La multiplicite du RBDY2 pour ce pas de temps est...
       multi=multiplicite(ibody)
   
       ! Comme on ne sait pas quelle était la multiplicité du RBDY2 au pas précédent,
       ! on se base sur sa multiplicité courante et sa masse de référence pour 
       ! fixer la nouvelle masse.
       if (multiplicite(ibody) <= 1) then
          masses_courantes(ibody)=masse_ref(ibody)
       else  
          ! Sa nouvelle masse devient
          masses_courantes(ibody)=masse_ref(ibody)/multi
       end if
       
    end do
    
 end subroutine compute_modified_mass
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_modified_mass_in_DDM(isdm_)
    
    implicit none
    
    integer, intent(in),optional :: isdm_
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 
    integer              :: i_rbdy2         ! indice de sous-domaine considere, indice de RBDY2

                             !12345678901234567890123456789012
    character(len=32) :: IAM='DDM_2D::set_modified_mass_in_DDM'

    if (.not. present(isdm_)) call faterr(IAM, "ne gere pas (encore) sans l'argument optionnel isdm_")
    do ibody = 1,nbody
       ! On balaye tous les sous-domaines auquel appartient le RBDY2 pour modifier la masse là où il faut
       ! indice du RBDY2 a updater
       i_rbdy2=(isdm_-1)*nbody+ibody
       call set_mass(i_rbdy2,masses_courantes(ibody))
    
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


       ! On va maintenant remplir le vecteur des RBDY2 d'interface par sous-domaine
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
       print *, "nb_RBDY2_interf_sdm de sdm num ",isdm," = ",nb_RBDY2_interf_sdm(isdm)

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

       if ( associated(Fg_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(Fg_RBDY2_interf_sdm(isdm)%particule)
       allocate(Fg_RBDY2_interf_sdm(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de Fg_RBDY2_interf_sdm(isdm)%paticule")

       if ( associated(DFg_RBDY2_interf_sdm(isdm)%particule) ) & 
            deallocate(DFg_RBDY2_interf_sdm(isdm)%particule)
       allocate(DFg_RBDY2_interf_sdm(isdm)%particule(3,nb_RBDY2_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de DFg_RBDY2_interf_sdm(isdm)%paticule")
       ! Mise à zero
       V_RBDY2_interf_sdm(isdm)%particule=0.D0
       V_RBDY2_interf_sdm_old(isdm)%particule=0.D0
       X_RBDY2_interf_sdm(isdm)%particule=0.D0
       Fg_RBDY2_interf_sdm(isdm)%particule=0.D0
       DFg_RBDY2_interf_sdm(isdm)%particule=0.D0
    end do

    ! Traitement de l'interface globale
    ! Calcul du nombre de particule dans l'interface globale
    nb_RBDY2_interf_glob = count(multiplicite>1)
    print *, "nombre de particule de l'interface globale=", nb_RBDY2_interf_glob

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


    ! Calcul du nombre de grains d'interface
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

    print *, "nb_liens_interf=",nb_liens_interf

    ! Allocation du tableau des sauts de vitesse d'interface
    if ( allocated(saut_V_interf_glob) ) deallocate(saut_V_interf_glob)
    allocate(saut_V_interf_glob(3,nb_liens_interf), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de saut_V_interf_glob")

    deallocate(isdm_multi_tab)
    deallocate(nb_grains_multi)

 end subroutine compute_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine stock_V_interface
    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! integer(kind=4), dimension(nbody,nsdm) :: body_particip
    ! integer(kind=4), dimension(nbody)      :: multiplicite 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite ::
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY2_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, isdm, i_std_bdy, i_RBDY2_interf_sdm, multi, compteur, err
    logical :: visible
    real(kind=8), dimension(3) :: vlocy

    !am: id pour recuperer les vitesses avec un get_vector
    character(len=5) :: id_vlocy
    !                         1234567890123456789012345
    character(len=25) :: IAM='DDM_2D::stock_V_interface'

    ! on va chercher la vitesse dans Vaux
    id_vlocy='Vaux_'

    do isdm=1,Nsdm
       V_RBDY2_interf_sdm_old(isdm)%particule=V_RBDY2_interf_sdm(isdm)%particule
       ! On remplit la table V_RBDY2_interf_sdm(isdm)
       do i_RBDY2_interf_sdm=1,nb_RBDY2_interf_sdm(isdm) 
          ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_RBDY2_interf_sdm)
          i_std_bdy=(isdm-1)*nbody+ibody

          if (.not. get_visible(i_std_bdy)) stop "DDM::stock_V_interface : Pb sur get_visible!!!"

          call get_vector_RBDY2(id_vlocy, i_std_bdy, vlocy, 3)
          V_RBDY2_interf_sdm(isdm)%particule(:,i_RBDY2_interf_sdm)=vlocy
       end do
    end do

 end subroutine stock_V_interface
!----------------------------------------------------------------


! Routine réalisant
! # XDF=[|V|]
! # F = F + DF
!----------------------------------------------------------------
 subroutine compute_F_gamma
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

                             !12345678901234567890123 
    character(len=23) :: IAM='DDM_2D::compute_F_gamma'

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

       ! mise a jour des F_gamma pour chaque copie du corps

       ! pour chaque sous-domaine auquel appartient le corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
          ! on met a jour le F_gamma de la copie du corps appartenant au sdm courant
          Fg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) = & 
             Fg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) + &
             DFg_RBDY2_interf_sdm(sdm(1))%particule(:, part_sdm(isdm))
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

 end subroutine compute_F_gamma
!----------------------------------------------------------------

! Pas encore au point!!
!----------------------------------------------------------------
 subroutine compute_egluing(iddm,egluing) 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! saut_V_interf_glob
    ! argument "explicite"
    !real(kind=8), intent(in)         :: Vref ! Vitesse de référence du problème considéré
                                             ! Ex : sédimentation sous gravité : Vref=g*Dt
    integer                          :: iddm ! indice d'itération ddm 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    real(kind=8),intent(out) :: egluing  ! erreur de recollement globale

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine
    real(kind=8)             :: egluing1  ! erreur de recollement globale quad
    real(kind=8)             :: egluing2  ! erreur de recollement globale saut
    real(kind=8)             :: egluing3  ! erreur de recollement globale max
    real(kind=8)             :: quad_DV, normVup2, max_DV, max_DV1_sdm, &
                                max_DV2_sdm, max_DV3_sdm, max_DV_sdm
    real(kind=8)             :: sum_quad_DV
    character(len=28) :: clout ! pour calculer le nom du fichier
    integer           :: nfich ! io unit
    integer           :: isdm, i_RBDY2

    egluing=0.d0

    egluing1=0.d0
    egluing2=0.d0
    egluing3=0.d0

    if (nb_liens_interf > 0) then

       ! Calcul de l'évolution (norme 2 moyennée) de la différence
       ! de vitesse des particules d'interfaces entre deux itérations
       ! NSCDD.
       quad_DV=0.D0
       sum_quad_DV=0.D0
       max_DV_sdm=0.D0
       max_DV=0.D0
       do isdm = 1, Nsdm
          normVup2= dot_product((V_RBDY2_interf_sdm(isdm)%particule(1,:)           &
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(1,:)),   &
                                (V_RBDY2_interf_sdm(isdm)%particule(1,:)           &
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(1,:))) + &
                    dot_product((V_RBDY2_interf_sdm(isdm)%particule(2,:)           &
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(2,:)),   &
                                (V_RBDY2_interf_sdm(isdm)%particule(2,:)           &
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(2,:))) + &
                    dot_product((V_RBDY2_interf_sdm(isdm)%particule(3,:)           & 
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(3,:)),   &
                                (V_RBDY2_interf_sdm(isdm)%particule(3,:)           &
                                 - V_RBDY2_interf_sdm_old(isdm)%particule(3,:))) 

          quad_DV = sqrt( normVup2  ) / nb_RBDY2_interf_sdm(isdm)
          sum_quad_DV = sum_quad_DV + quad_DV


          do i_RBDY2 = 1, nb_RBDY2_interf_sdm(isdm)
             max_DV1_sdm = max(max_DV_sdm, dabs(V_RBDY2_interf_sdm(isdm)%particule(1,i_RBDY2) &
                                                - V_RBDY2_interf_sdm_old(isdm)%particule(1,i_RBDY2)))

             max_DV2_sdm = max(max_DV_sdm, dabs(V_RBDY2_interf_sdm(isdm)%particule(2,i_RBDY2) &
                                                - V_RBDY2_interf_sdm_old(isdm)%particule(2,i_RBDY2)))

             max_DV3_sdm = max(max_DV_sdm, dabs(V_RBDY2_interf_sdm(isdm)%particule(3,i_RBDY2) &
                                                - V_RBDY2_interf_sdm_old(isdm)%particule(3,i_RBDY2)))
          end do

          max_DV_sdm=max_DV1_sdm+max_DV2_sdm+max_DV3_sdm 
          max_DV = max(max_DV, max_DV_sdm)
          max_DV1_sdm= 0.D0 ; max_DV2_sdm =0.D0 ; max_DV3_sdm =0.D0 
       end do

       !egluing1=sum_quad_DV
       !egluing3=max_DV

       !! Calcul de l'erreur globale de recollement
       egluing2=sqrt(dot_product(saut_V_interf_glob(1,:),saut_V_interf_glob(1,:)) +&
                   dot_product(saut_V_interf_glob(2,:),saut_V_interf_glob(2,:)))/&
                   (nb_liens_interf)

       !print* , "####################"
       !print* , "egluing1=", egluing1
       !print* , "egluing2=", egluing2
       egluing=max(egluing1,egluing2,egluing3)

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
 subroutine set_DF_gamma 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! Ce truc ci dessous va changer de forme
    ! type(T_ligne_r), dimension(Nsdm)   :: DFg_RBDY2_interf_sdm(isdm)
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody
    integer            :: i_part_sdm
    integer            :: i_std_bdy
    real(kind=8), dimension(3) :: DF_gamma

    do isdm=1,Nsdm
       do i_part_sdm=1,nb_RBDY2_interf_sdm(isdm)
          ! On récuère le numéro du RBDY2 dans le sous-domaine 
          ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_part_sdm)
          ! Recallage des indice pour le cas séquaentiel multidomaine
          i_std_bdy=(isdm-1)*nbody+ibody
          ! On stocke le DF_gamma qui nous intéresse pour ce corps
          ! Après avoir divisé par le pas de temps pour convertir
          ! l'impulsion calculée en force moyenne sur le pas de temps
          DF_gamma=(1.D0/H)*DFg_RBDY2_interf_sdm(isdm)%particule(:,i_part_sdm)
          call put_vector_RBDY2('Fext_',i_std_bdy,DF_gamma,3)
          call comp_free_vlocy_one_RBDY2(i_std_bdy)
       end do
    end do

 end subroutine set_DF_gamma
!----------------------------------------------------------------

! Routine de récupération des Fg du pas précédent quand cela est 
! possible, i.e. pas de migration du grain.
!----------------------------------------------------------------
 subroutine set_F_gamma 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! Ce truc ci dessous va changer de forme
    ! type(T_ligne_r), dimension(Nsdm)   :: Fg_RBDY2_interf_sdm(isdm)
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
       do isdm=1,Nsdm
          do i_part_sdm=1,nb_RBDY2_interf_sdm(isdm)
             ! On récupère le numéro du RBDY2 dans le sous-domaine 
             ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_part_sdm)
             ! Recallage des indice pour le cas séquaentiel multidomaine
             i_std_bdy=(isdm-1)*nbody+ibody
             ! On stocke le DF_gamma qui nous intéresse pour ce corps
             ! Après avoir divisé par le pas de temps pour convertir
             ! l'impulsion calculée en force moyenne sur le pas de temps
             F_gamma=(1.D0/H)*Fg_RBDY2_interf_sdm(isdm)%particule(:,i_part_sdm)
             call put_vector_RBDY2('Fext_',i_std_bdy,F_gamma,3)
          end do
       end do
   
    else ! On vient donc de faire une nouvelle répartition en sous-domaines

       do isdm=1,Nsdm
          do i_part_sdm=1,nb_RBDY2_interf_sdm(isdm)
             ! On récupère le numéro du RBDY2 dans le sous-domaine 
             ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_part_sdm)
             ! Recallage des indice pour le cas séquentiel multidomaine
             i_std_bdy=(isdm-1)*nbody+ibody

             if (any(liste_RBDY2_interf_sdm_old(isdm)%particule(:)==ibody)) then
                tmp=maxloc(liste_RBDY2_interf_sdm_old(isdm)%particule(:), &
                                      liste_RBDY2_interf_sdm_old(isdm)%particule(:)==ibody)
                old_i_part_sdm=tmp(1)
                Fg_RBDY2_interf_sdm(isdm)%particule(:,i_part_sdm) = &
                Fg_RBDY2_interf_sdm_old(isdm)%particule(:,old_i_part_sdm)
                ! On stocke le F_gamma qui nous intéresse pour ce corps
                ! Après avoir divisé par le pas de temps pour convertir
                ! l'impulsion calculée en force moyenne sur le pas de temps
                F_gamma=(1.D0/H)*Fg_RBDY2_interf_sdm_old(isdm)%particule(:,old_i_part_sdm)
                call put_vector_RBDY2('Fext_',i_std_bdy,F_gamma,3)
             end if
          end do
       end do
    end if

    repartition_just_made=.false. 

 end subroutine set_F_gamma
!----------------------------------------------------------------

!! Application de fonctions classsiques de LMGC90 à des listes
!! d'indices non-contigues
!!----------------------------------------------------------------

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
    integer            :: ibody
    integer            :: i_part_sdm
    integer            :: i_std_bdy
    integer            :: isdm
   
    do isdm=1,Nsdm 
       do i_part_sdm=1,nb_RBDY2_interf_sdm(isdm)
          ! On récupère le numéro du RBDY2 dans le sous-domaine 
          ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_part_sdm)
          ! Recallage des indices pour le cas séquentiel multidomaine
          i_std_bdy=(isdm-1)*nbody+ibody

          ! calcul de Vaux = Vfree + M^-1 Iaux (et CL appliquees a Vaux)
          call comp_vlocy(i_std_bdy, iVaux_e_invM_t_Iaux_p_Vfree)
       end do
    end do

 end subroutine comp_V_list_RBDY2_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine gather_Xbeg_Vbeg
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
    integer            :: ibody, imulti
    integer            :: i_std_bdy
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(3) :: X_tmp, V_tmp
    
    compteur=0
    visible=.false.
    do isdm=2,Nsdm
       do ibody=1,nbody
          i_std_bdy = (isdm-1)*nbody+ibody
          visible=get_visible(i_std_bdy)
          imulti=multiplicite(ibody)
          if (.not. visible) cycle
          compteur=compteur+1
          call get_vector_RBDY2('Xbeg_',i_std_bdy,X_tmp,3)
          call put_vector_RBDY2('Xbeg_',ibody,X_tmp,3)
          call get_vector_RBDY2('Vbeg_',i_std_bdy,V_tmp,3)
          call put_vector_RBDY2('Vbeg_',ibody,V_tmp,3)
       end do

       ! Le test ci dessous donne une erreur inexistante si d'autres critères que 
       ! l'appartenance au sous-domaine "isdm" entrent en jeux pour définir la
       ! visibilité des corps.

       !if (compteur/=(count(mask_particip(:,isdm) .eqv. .true.)-nb_RBDY2_interf_sdm(isdm))) stop &
       !   "DDM_gather_X_V::compteur/=visibles-nb_RBDY2_interf_sdm(isdm)"
       compteur=0
    end do

 end subroutine gather_Xbeg_Vbeg
!----------------------------------------------------------------

! Moyenne sur les V des grains d'interface
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
    integer :: ibody, i_std_bdy, i_RBDY2_interf_glob, &
               imulti, multi, isdm       
    logical :: visible
    real(kind=8), dimension(3) :: V_tmp

    V_RBDY2_interf_glob=0

    do i_RBDY2_interf_glob=1,nb_RBDY2_interf_glob
       ibody=interface_globale(i_RBDY2_interf_glob)
       multi=multiplicite(ibody)
       do imulti=1,multi
          isdm = body_particip(imulti,ibody)
          i_std_bdy=(isdm-1)*nbody+ibody
          call get_vector_RBDY2('V____',i_std_bdy,V_tmp,3)
          V_RBDY2_interf_glob(:,i_RBDY2_interf_glob)=V_RBDY2_interf_glob(:,i_RBDY2_interf_glob) &
                                       + V_tmp / multi
       end do
       ! Mise en donnée de la position et vitesse moyenne de la particule 
       ! dans la première tranche --> processus hôte
       call put_vector_RBDY2('V____',ibody,V_RBDY2_interf_glob(:,i_RBDY2_interf_glob),3)
    end do     


 end subroutine fix_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
! On va mettre les positions et vistesses moyennes calculées dans
! fix_interface dans les listes par sous-domaines à envoyer aux 
! processus esclaves.
 subroutine compute_V_interface_sdm
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::
    ! real(kind=8), dimension(3,nb_RBDY2_interf_glob) :: V_RBDY2_interf_glob
    ! arguments "explicites"
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! real(kind=8), dimension(3,nb_RBDY2_interf_sdm) :: V_RBDY2_interf_sdm
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: i_RBDY2_glob, i_RBDY2_sdm
    integer            :: ibody,i_std_bdy
    integer            :: isdm
    integer            :: multi,imulti
    integer, dimension(1) :: tmp
   
    do i_RBDY2_glob=1,nb_RBDY2_interf_glob
       ibody=interface_globale(i_RBDY2_glob)
       multi=multiplicite(ibody)
       do imulti=1,multi
          isdm=body_particip(imulti,ibody)
          ! On va pècher l'indice du RBDY2 "ibody" dans la liste des
          ! RBDY2 d'interface du sdm "isdm"
          tmp=maxloc(liste_RBDY2_interf_sdm(isdm)%particule(:),&
                     liste_RBDY2_interf_sdm(isdm)%particule(:)==ibody)
          i_RBDY2_sdm=tmp(1)
          V_RBDY2_interf_sdm(isdm)%particule(:,i_RBDY2_sdm)=V_RBDY2_interf_glob(:,i_RBDY2_glob)
       end do
    end do

 end subroutine compute_V_interface_sdm
!----------------------------------------------------------------

!----------------------------------------------------------------
! Mise en donnée par les processus esclaves des positions et
! vitesses moyennes à convergence (du moins on l'espère).
 subroutine set_V_interface
     implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! real(kind=8), dimension(3,nb_RBDY2_interf_sdm) :: V_RBDY2_interf_sdm
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer                    :: isdm
    integer                    :: i_RBDY2_sdm
    integer                    :: ibody,i_std_bdy
    real(kind=8), dimension(3) :: V_tmp

    do isdm=1,Nsdm
       do i_RBDY2_sdm=1,nb_RBDY2_interf_sdm(isdm)
          ibody=liste_RBDY2_interf_sdm(isdm)%particule(i_RBDY2_sdm)
          i_std_bdy=(isdm-1)*nbody+ibody
          V_tmp=V_RBDY2_interf_sdm(isdm)%particule(:,i_RBDY2_sdm)
          call put_vector_RBDY2('V____',i_std_bdy,V_tmp,3)
       end do
    end do

 end subroutine set_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_visibility_4all_in_DDM(nb_RBDY2__,isdm)
 
    implicit none
    
    integer, intent(in)           :: nb_RBDY2__
    integer, intent(in), optional :: isdm
    
    if (present(isdm)) then
       call set_visibility_4all_RBDY2(mask_redim(:,isdm),nb_RBDY2__)
    else
       call set_visibility_4all_RBDY2(mask_RBDY2,nb_RBDY2__)
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

! Ecriture de'un fichier avec iRBDY2, visibilite, sdm, multi
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

! procedure de nettoyage de la memoire encore allouee
subroutine clean_ddm

   implicit none

   ! nettoyage de la memoire encore allouee dans le module de detection grossiere
   call clean_module 

end subroutine clean_ddm

end module DDM_MDS_2D
