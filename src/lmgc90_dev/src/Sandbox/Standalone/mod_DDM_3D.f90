! Module allant avec le SPS_standalone_DDM multidomaine séquentiel
! avec détection fine séquentielle

module DDM_3D 

use overall
use parameters
use utilities
use RBDY3
use SPHER
use POLYR
use SPSPx, only : set_anonymous_to_rough_SPSPx, & 
                  get_nb_INTRF_SPSPx,           & 
                  get_list_INTRF_SPSPx
use PRPRx, only : set_anonymous_to_rough_PRPRx, & 
                  get_nb_INTRF_PRPRx,           & 
                  get_list_INTRF_PRPRx
use nlgs_3D, only : shift_icdan, &
                    RnodHRloc_nlgs, &
                    compute_local_free_vlocy
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

implicit none

!am: on declare tout prive (a priori) pour eviter les effets de bord
private

! am : ancienne archi => on ne travaille qu'avec des disques (et des clusters de disques)

integer(kind=4)                                :: nbody          ! nombre "vrai" de RBDY3 
integer(kind=4)                                :: ntact_SPHER    ! nombre de contacteurs spheres
integer(kind=4)                                :: ntact_POLYR    ! nombre de contacteurs polyedres

!-------------------------------------------------------------------------------------------------------------------
! Tableaux des coordonnées d'entités et leur sous-domaine (un seul et unique)
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:, :), allocatable     :: coord_ci       ! coordonnees des centres d'inertie des RBDY2
real(kind=8), dimension(:, :), allocatable     :: coord_cc       ! coordonnees des centres des contacts (uniquement entre disques, mais il faut faire évoluer cela!!!) 
integer(kind=4), dimension(:), allocatable     :: repart_sdm_ci  ! listes des sdm auquels appartiennent les centres d'inertie des RBDY2
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
integer(kind=4), dimension(:,:,:), allocatable :: migration_tab  ! table des migrations par sous-domaines
integer(kind=4)                                :: nb_migrations  ! nombre de migrations
logical, dimension(:,:), allocatable           :: mask_particip  ! matrice (/ nbody,Nsdm /) permettant de déterminer, pour
                                                                 ! chaque sous-domaines, quels sont les objets visibles par ce sdm
logical, dimension(:,:), allocatable           :: mask_redim     ! matrice (/ Nb_RBDY3, Nsdm /) permettant de déterminer, pour
                                                                 ! chaque sous-domaines, quels sont les objets visibles par ce sdm
                                                                 ! dans la liste complète des particules dupliquées
logical, dimension(:), allocatable             :: mask_RBDY3     ! pour le cas multi-domaine séquentiel : tableau de 

!-------------------------------------------------------------------------------------------------------------------
! Gestion des masses
!-------------------------------------------------------------------------------------------------------------------
real(kind=8), dimension(:), allocatable        :: masse_ref      ! vecteur contenant les masses de reference pour chaque corps
real(kind=8), dimension(:), allocatable        :: masses_courantes! vecteur contenant les masses courantes pour chaque corps
                                                                 ! taille (/Nsdm*nbody/)

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
 type(CONTAINER),save    :: shiftted_rough_SPSPx    ! table  de visibilité (contacts sphere/sphere)
 integer                 :: nb_shiftted_rough_SPSPx ! taille table  de visibilité (contacts sphere/sphere)
 type(CONTAINER),save    :: shiftted_rough_PRPRx    ! table  de visibilité (contatcs polyedre/polyedre)
 integer                 :: nb_shiftted_rough_PRPRx ! taille table  de visibilité (contatcs polyedre/polyedre)
!-------------------------------------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------------------------------------
 type(CONTAINER), dimension(:), allocatable :: splitted_rough ! rough_contact découpé par sdm
 integer(kind=4), dimension(:), allocatable :: nb_splitted_rough ! nombre de contact par sous domaines
!-------------------------------------------------------------------------------------------------------------------
integer                            :: nb_INTRF       ! Nombre total de contacts fins taggés 'INTRF'
integer,dimension(:), allocatable  :: list_INTRF     ! Liste de tous les indices des contacts taggés 'INTRF'

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
type(T_ligne_r), dimension(:), allocatable  :: X_RBDY3_interf_sdm          ! positions des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: localframe_RBDY3_interf_sdm ! reperes principaux d'inertie des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY3_interf_sdm         ! F_gamma des RBDY3 d'interface par sdm 
type(T_ligne_r), dimension(:), allocatable  :: Fg_RBDY3_interf_sdm_old     ! F_gamma des RBDY3 d'interface par sdm (pas -1)
type(T_ligne_r), dimension(:), allocatable  :: DFg_RBDY3_interf_sdm        ! DF_gamma des RBDY3 d'interface par sdm 
integer(kind=4), dimension(:), allocatable  :: nb_RBDY3_interf_sdm         ! nombre de corps d'interface par sdm
logical                                     :: repartition_just_made = .false. ! Permet de savoir si la répartition
                                                                               ! en sous-domaine viens d'être effectuée      

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
real(kind=8), dimension(:,:), allocatable   :: localframe_RBDY3_interf_glob ! reperes principaux d'inertie à convergence des RBDY3 d'interface globale 

!-------------------------------------------------------------------------------------------------------------------
! Fin des déclarations des variables globales au module
!-------------------------------------------------------------------------------------------------------------------


!am: on donne la liste des fonctions publiques (utilisables hors du module)
public                                &
   init_dd,                           &
   new_dd,                            &
   gather_X_V_begin,                  &
   gather_POLYR_TT,                   &
   gather_X_V,                        &
   set_visibility_4all_in_DDM,        &
   creation_domaines,                 &
   fix_migrants,                      &
   launch_set_anonymous_to_rough_4all,&
   erase_rough_contact,               &
   erase_shiftted_rough,              &
   get_list_INTRF_in_DDM,             &
   RnodHRloc_list_in_DDM,             &
   compute_local_free_vlocy_in_DDM,   &
   set_modified_mass_in_DDM,          &
   comp_V_list_RBDY3_in_DDM,          & 
   stock_V_interface,                 &
   compute_F_gamma,                   &
   set_DF_gamma,                      &                   
   compute_egluing,                   &
   set_F_gamma,                       &                   
   fix_V_interface,                   &
   compute_V_interface_sdm,           &
   set_V_interface,                   &               
   clean_ddm                

contains

 ! fonction qui initialise le module DDM
 function init_dd(Nsdm1_, Nsdm2_, Nsdm3_, TimeStep_)

    implicit none

    ! variables d'entrée
    integer, intent(in)      :: Nsdm1_, Nsdm2_, Nsdm3_
    real(kind=8), intent(in) :: TimeStep_

    ! valeur de retour
    integer :: init_dd

    ! variables locales
    integer :: isdm,err

    TimeStep=TimeStep_
    Nsdm1=Nsdm1_
    Nsdm2=Nsdm2_
    Nsdm3=Nsdm3_

    ! on calcule le nombre total de sous-domaines
    Nsdm=Nsdm1*Nsdm2*Nsdm3
    ! calcul de la multiplicité maximale pour une particule
    max_multi=max(8,Nsdm1,Nsdm2,Nsdm3) ! 8 pour le 3D. un cluster en diagonale ferait tout sauter
    ! on le renvoie
    init_dd=Nsdm

 end function init_dd

 !am : procedure qui alloue l'espace memoire pour stocker les tableaux partages entre les differentes fonctions du module
 !     (les coordonnees des centre d'inertie, les numeros de sous-domaine pour chaque corps)
!-------------------------------------------------------------------------------------------------------------------
 subroutine new_dd(nb_RBDY3, ntact_SPHER_, ntact_POLYR_)

    implicit none

    ! variables d'entrée
    integer, intent(in)           :: nb_RBDY3
    integer, intent(in), optional :: ntact_SPHER_, ntact_POLYR_

    ! variables locales
    integer                       :: isdm, ibody, err

                                         !12345678901234
    character(len=14)             :: IAM='DDM_3D::new_dd'

    !print*, "nb_RBDY3=", nb_RBDY3
    !print*, "ntact_=", ntact_

    ! si on est en sequentiel, on a duplique les corps dans le standalone! 
    ! on alloue un masque de taille Nsdm*nbody i.e. de la taille de tous les
    ! corps (y compris les dupliqués!)     
    if (.not. allocated(mask_RBDY3)) then
       allocate(mask_RBDY3(nb_RBDY3), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_RBDY3")
    end if
    print *, "new_dd::shape(mask_RBDY3)", shape(mask_RBDY3)
    ! on initialise le masque a "vrai" pour tous les corps
    mask_RBDY3=.true.

    if (.not. allocated(mask_redim)) then
       allocate(mask_redim(nb_RBDY3, Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de mask_redim")
    end if
    print *, "new_dd::shape(mask_redim)", shape(mask_redim)
    ! et on divise par le nombre
    ! de sous-domaines pour retrouver le nombre de corps original
    nbody=nb_RBDY3/Nsdm
    ! recuperation du nombre de contacteurs
    !    * spheres
    if (present(ntact_SPHER_)) then
       ntact_SPHER=ntact_SPHER_/Nsdm
    else
       ntact_SPHER=0
    end if
    !    * polyedres
    if (present(ntact_POLYR_)) then
       ntact_POLYR=ntact_POLYR_/Nsdm
    else
       ntact_POLYR=0
    end if

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
    !    * numeros de sous-domaines des contacteurs spheres et polyedres
    if (.not. allocated(repart_sdm_ct)) then
       allocate(repart_sdm_ct(ntact_SPHER + ntact_POLYR), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d allocation de la liste indicatrice des sous-domaines")
    end if

    ! s'il ya des polyedres
    if (ntact_POLYR > 0) then
       !    * les coordonnees de la boite englobante des contacteures polyedres :
       !        - (xmin, ymin, zmin)
       if (.not. allocated(minpos_POLYR)) then                                                      !
          allocate(minpos_POLYR(3, ntact_POLYR), stat=err)                                          !
          if (err/=0) call faterr(IAM, "Erreur d allocation des coordonnees du points (xmin, ymin, zmin) "&
             &"des contacteurs POLYR")
       end if                                                                                    !
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
       allocate(migration_tab(max_multi, nbody, Nsdm), stat=err)
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
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de V_RBDY2_interf_sdm")
    end if
    !    * table des positions d'interface appartenants aux sdm
    if (.not. allocated(X_RBDY3_interf_sdm)) then
       allocate(X_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de X_RBDY3_interf_sdm")
    end if
    !    * table des reperes principaux d'inertie d'interface appartenants aux sdm
    if (.not. allocated(localframe_RBDY3_interf_sdm)) then
       allocate(localframe_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de localframe_RBDY3_interf_sdm")
    end if

    !    * table des F_gamma sur les grains d'interface des sdm
    if (.not. allocated(Fg_RBDY3_interf_sdm)) then
       allocate(Fg_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de Fg_RBDY3_interf_sdm")
    end if
    !    * table des F_gamma sur les grains d'interface des sdma (pas -1)
    if (.not. allocated(Fg_RBDY3_interf_sdm_old)) then
       allocate(Fg_RBDY3_interf_sdm_old(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de Fg_RBDY3_interf_sdm_old")
    end if
    !    * table des DF_gamma sur les grains d'interface des sdm
    if (.not. allocated(DFg_RBDY3_interf_sdm)) then
       allocate(DFg_RBDY3_interf_sdm(Nsdm), stat=err)
       if (err/=0) call faterr(IAM, "Erreur d'allocation du nombre de colonne de DFg_RBDY3_interf_sdm")
    end if

 end subroutine new_dd
!----------------------------------------------------------------

!----------------------------------------------------------------!
!----------------------------------------------------------------!
!              "Pseudo main" du module DDM_3D                    !
!----------------------------------------------------------------!
!----------------------------------------------------------------!

 subroutine creation_domaines(Xleft_bound,Xright_bound, &
                               Yleft_bound,Yright_bound,Zdown_bound,Zup_bound)
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine
    real(kind=8), optional :: Xleft_bound,Xright_bound, &
                               Yleft_bound,Yright_bound,Zdown_bound,Zup_bound

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine 
    integer                                    :: E_IO
    integer                                    :: i,j,iliens,tmp,err
    integer                                    :: itact, ibody, icdan, isdm
    real(kind=8)                               :: minray, maxray ! plus petit et plus grand rayon d'ecnombrements, pour la 
       ! methode des boites
    real(kind=8)                               :: alert          ! distance d'alerte globale utilisee pour la methode des boites
    character(len=10)                          :: step ! pour ecrire le numero du pas  
    character(len=10)                          :: sdm  ! pour ecrire le numero du du sous_domaine courant  

    logical, save                              :: first_real_step=.true.    

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

                                                      !1234567890123456789012345 
    character(len=25)                          :: IAM="DDM_3D::creation_domaines"

    i4 => null()
    r8 => null()
    c5 => null()
    cx => null()

    !(pas ici en fait!) besoin d'un switch pour le cas où l'on ventille les corps
 
    ! recuperation des coordonnees des centre d'inertie des RBDY3
    do ibody=1, nbody
      !am : on utilise les coordonnees dans la configuration de detection
      coord_ci(1:3, ibody) = get_coorTT(ibody, 0)

      !print *, "coord_ci(", ibody, ")=", coord_ci(:, ibody) 
    end do
   
    ! pour vérifier la valeur de ntact
    !print *, "ntact=", ntact

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

    !--------------------------------------------------------------------------------------
    ! Calcul de la boite englobante de l'échantillon total, puis des caractéristiques de sous domaines 
    call caracteristique_domaine()!(Xleft_bound,Xright_bound)
    !--------------------------------------------------------------------------------------

    !---------------------------------------------------------------------------------------------------!
    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des centres d'inertie des RBDY3
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Nsdm3, Xleft, Yleft, Zdown, dim_sdm, &
                             3, nbody, coord_ci, repart_sdm_ci,   & 
                             .true.)                              
    !--------------------------------------------------------------------------------------

    !---------------------------------------------------------------------------------------------------!
    ! Ici fonction retournant le tableau repart_sdm de profil=(/ nb_points /), contenant l'indice
    ! du sous-domaine auquel appartient l'entité repérée par sa position dans le tableau
    ! ventilation des contacteurs
    call ventillation_ds_sdm(Nsdm1, Nsdm2, Nsdm3, Xleft, Yleft, Zdown, dim_sdm, &
                             3, nbody, coord_ct, repart_sdm_ct,   & 
                             .true.)                              
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
       print *, "nb_rough_contact (SPSPx)", nb_rough_contact_SPSPx
        
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
             minray, maxray, big_POLYR(1:nb_big_POLYR/Nsdm))
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
       print *, "nb_rough_contact (PRPRx)", nb_rough_contact_PRPRx

       !call display_object_container(rough_contact)
    else
       nb_rough_contact_PRPRx = 0
    end if

    !--------------------------------------------------------------------------------------
    ! On recupere le nombre de total de contacts grossiers.
    nb_rough_contact = get_nb_objects(rough_contact)
    print *, "nb_rough_contact", nb_rough_contact
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
    call creation_shiftted_rough 
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! todo :  sortir l'appel de cette fonction de ce module et la placer dans le standelone
    ! modification des proprietes des RBDY3 d'interface (dupliques)
    call compute_modified_mass
    !--------------------------------------------------------------------------------------

    !--------------------------------------------------------------------------------------
    ! Gestion/récupération des F_gamma du pas précédent
    if (.not. first_real_step) then
       ! Allocation des listes de RBDY3 d'interface par sous-domaine du pas précédent
       do isdm=1, Nsdm
          if ( associated(liste_RBDY3_interf_sdm_old(isdm)%particule) ) & 
               deallocate(liste_RBDY3_interf_sdm_old(isdm)%particule)
          allocate(liste_RBDY3_interf_sdm_old(isdm)%particule(nb_RBDY3_interf_sdm(isdm)), stat=err)
          if (err/=0) call faterr(IAM, "Pb d'allocation de liste_RBDY3_interf_sdm_old(isdm)%particule")
          liste_RBDY3_interf_sdm_old(isdm)%particule(:)=liste_RBDY3_interf_sdm(isdm)%particule(:)
    
          if ( associated(Fg_RBDY3_interf_sdm_old(isdm)%particule) ) & 
               deallocate(Fg_RBDY3_interf_sdm_old(isdm)%particule)
          allocate(Fg_RBDY3_interf_sdm_old(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
          if (err/=0) call faterr(IAM, "Pb d'allocation de Fg_RBDY2_interf_sdm_old(isdm)%paticule")
          Fg_RBDY3_interf_sdm_old(isdm)%particule(:, :)=0.d0
          Fg_RBDY3_interf_sdm_old(isdm)%particule(:, :)=Fg_RBDY3_interf_sdm(isdm)%particule(:, :)
       end do 
    end if
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
    if (.not. first_real_step) repartition_just_made = .true.
    if (first_real_step) first_real_step=.false. 

 end subroutine creation_domaines
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine caracteristique_domaine(Xleft_, Xright_, Yleft_, Yright_, Zdown_, Zup_)

    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! implicites : coord_ci
    real(kind=8), intent(in), optional :: Xleft_, Xright_, Yleft_, Yright_, Zdown_, Zup_

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

    if ( present(Xleft_) )  Xleft=Xleft_
    if ( present(Xright_) ) Xright=Xright_
    if ( present(Yleft_) )  Yleft=Yleft_
    if ( present(Yright_) ) Yright=Yright_
    if ( present(Zup_) )    Zup=Zup_
    if ( present(Zdown_) )  Zdown=Zdown_

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

       ! pour checker
       !print *, "corps candidat   ", icdbdy, "sdm(s)", body_particip(:,icdbdy)
       !print *, "corps antagoniste", ianbdy, "sdm(s)", body_particip(:,ianbdy)
    end do

    ! gestion des contacts polyedre/polyedre
    do icdan=nb_rough_contact_SPSPx + 1, nb_rough_contact_SPSPx + nb_rough_contact_PRPRx

       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact, icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)

       ! on recupere le numero du contacteur candidat
       icdtac = cdan(1)

       ! on recupere le numero du contacteur antagoniste
       iantac = cdan(2)

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
    mask_RBDY3=.false.
    mask_redim=.false.

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

       ! pour le séquentiel, on rempli mask_RBDY3 
       
       mask_RBDY3(((isdm-1)*nbody)+1:isdm*nbody)=mask_particip(:,isdm)
       mask_redim(((isdm-1)*nbody)+1:isdm*nbody,isdm)=mask_particip(:,isdm)

       ! pour checker
       !print *, 'creation_body::shape(mask_RBDY2)', shape(mask_RBDY3)
       !print *, 'isdm=', isdm, ' : mask_RBDY3=', mask_RBDY3(((isdm-1)*nbody)+1:isdm*nbody)
       !print *, 'isdm=', isdm, ' : mask_redim=', mask_redim(((isdm-1)*nbody)+1:isdm*nbody, isdm)
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
    integer(kind=4)                         :: first, nb_new

    migration_tab = 0
    nb_migrations = 0

    do ibody = 1, nbody
       nb_new = 0
       multi=multiplicite(ibody)
       multi_old=multiplicite_old(ibody)

       ! On stoque le num du premier sdm auquel participe ibody
       ! Si de nouveaux sous-domaines doivent gérer ibody, c'est le sdm 
       ! first qui devra envoyer le l'état du ibody à ces nouveaux gestionnaires
       ! (ou co-gestionnaires)
       first = body_particip_old(1, ibody)

       do imulti= 1, multi
          isdm=body_particip(imulti, ibody)

          if ( count(isdm == body_particip_old(1:multi_old, ibody)) /=0 ) cycle

          print *, 'ibody=', ibody
          print *, 'body_particip=', body_particip(:, ibody)
          print *, '-------------------------------------------' 
          print *, 'body_particip_old=', body_particip_old(:, ibody)
          print *, '-------------------------------------------' 

          nb_new=nb_new + 1
          migration_tab(nb_new,ibody, first)=isdm
          print *, 'migration_tab(', nb_new, ',', ibody, ',', first, ')=', migration_tab(nb_new, ibody, first)
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
    real(kind=8), dimension(6)              :: X_tmp, Xbeg_tmp, V_tmp, Vbeg_tmp
    real(kind=8), dimension(3, 3)           :: localframebeg_tmp, localframe_tmp, localframeTT_tmp

                             !12345678901234567890
    character(len=20) :: IAM="DDM_3D::fix_migrants"

    do isdm=1, Nsdm
       do
          ! S'il n'y à plus de grains à faire connaitre, on sort
          if ( count(migration_tab(1, :, isdm) /= 0) == 0) exit

          ! S'il y en a, on procède par ordre croissant (au sens
          ! de l'emplacement mémoire)
          tmp = maxloc(migration_tab(1, :, isdm))

          ibody = tmp(1)
          nb_new_sdm = count(migration_tab(:, ibody, isdm) /= 0)

          print *, "le sdm", isdm, "envoi les informations du corps,", ibody
 
          do i_new_sdm = 1, nb_new_sdm
             ! Le sous-domaine auquel on veut donner X, V est :
             isdm_new = migration_tab(i_new_sdm, ibody, isdm)

             ibody_new = (isdm_new - 1)*nbody + ibody
             ibody_old = (isdm - 1)*nbody + ibody
            
             Xbeg_tmp=0.d0 
             localframebeg_tmp=0.d0
             Vbeg_tmp=0.d0
             X_tmp=0.d0
             localframe_tmp=0.d0
             V_tmp=0.d0
             localframeTT_tmp=0.d0
             ! * champs au debut du pas de temps
             call get_vector_RBDY3('Xbeg_', ibody_old, Xbeg_tmp, 6)
             call put_vector_RBDY3('Xbeg_', ibody_new, Xbeg_tmp, 6)
             ! gestion du repere principal d'inertie
             call get_matrix_RBDY3('IFbeg', ibody_old, localframebeg_tmp, 3)
             call put_matrix_RBDY3('IFbeg', ibody_new, localframebeg_tmp, 3)

             call get_vector_RBDY3('Vbeg_', ibody_old, Vbeg_tmp, 6)
             call put_vector_RBDY3('Vbeg_', ibody_new, Vbeg_tmp, 6)

             ! * champs a la fin du pas de temps
             call get_vector_RBDY3('X____', ibody_old, X_tmp, 6)
             call put_vector_RBDY3('X____', ibody_new, X_tmp, 6)
             ! gestion du repere principal d'inertie
             call get_matrix_RBDY3('IF___', ibody_old, localframe_tmp, 3)
             call put_matrix_RBDY3('IF___', ibody_new, localframe_tmp, 3)

             call get_vector_RBDY3('V____', ibody_old, V_tmp, 6)
             call put_vector_RBDY3('V____', ibody_new, V_tmp, 6)

             ! * repere principal d'inertie dans la configuration de detection
             call get_matrix_RBDY3('IFTT_', ibody_old, localframeTT_tmp, 3)
             call put_matrix_RBDY3('IFTT_', ibody_new, localframeTT_tmp, 3)

             ! On met à zéro la case de migration_tab
             ! que l'on vient de traiter 
             migration_tab(i_new_sdm, ibody, isdm)=0
          end do
       end do
    end do

 end subroutine fix_migrants
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine creation_shiftted_rough

    implicit none
   
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites :: nb_rough_contact, mask_contact, rough_contact 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! argument implicite :: splitted_rough(isdm), nb_splitted_rough(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: icdan, icdbdy, ianbdy, isdm
    integer(kind=4)                           :: cdtac, antac
    ! structure de l'anonymous container
    integer(kind=4),    dimension(:), pointer :: cdan
    integer(kind=4),    dimension(:), pointer :: shiftted_cdan
    character(len=5),   dimension(:), pointer :: c5 
    real(kind=8),       dimension(:), pointer :: r8
    character(len=128), dimension(:), pointer :: cx

    type(T_object)                            :: contact


    c5 => null()
    r8 => null()
    cx => null()
   
    shiftted_cdan => null()

    ! gestion des contacts sphere/sphere
    nb_shiftted_rough_SPSPx=0

    ! Pour chaque contact
    do icdan=1, nb_rough_contact_SPSPx
       isdm=repart_sdm_cc(icdan)
       nb_shiftted_rough_SPSPx=nb_shiftted_rough_SPSPx + 1
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
     
       allocate(shiftted_cdan(3))

       ! Pour le cas séquentiel only!!!!
       shiftted_cdan(1)=cdan(1) + ((isdm - 1)*ntact_SPHER)!
       shiftted_cdan(2)=cdan(2) + ((isdm - 1)*ntact_SPHER)!

       ! numéro de corps associé au contacteur courant
       icdbdy=spher2bdyty(1, cdan(1))
       ianbdy=spher2bdyty(1, cdan(2))
       if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
          shiftted_cdan(3)=INTRF
       else
          shiftted_cdan(3)=NOINT
       end if

       call add_object_to_container(shiftted_rough_SPSPx, nb_shiftted_rough_SPSPx, &
          c5, shiftted_cdan, r8, cx)

    end do

    ! pour tester que les shiftted_rough sont bien remplis jusqu'au bout
    print *, "nb_shiftted_rough_SPSPx =", nb_shiftted_rough_SPSPx
    call close_container(shiftted_rough_SPSPx)
    !print *, "display_shiftted_rough_SPSPx"
    !call display_object_container(shiftted_rough_SPSPx)

    ! gestion des contacts polyedre/polyedre
    nb_shiftted_rough_PRPRx=0

    ! Pour chaque contact
    do icdan=nb_rough_contact_SPSPx + 1, nb_rough_contact_SPSPx + nb_rough_contact_PRPRx
       isdm=repart_sdm_cc(icdan)
       nb_shiftted_rough_PRPRx=nb_shiftted_rough_PRPRx + 1
       ! On récupère l'objet contact d'indice icdan
       contact = get_object(rough_contact,icdan)    
       ! On récupère la pair num candidat/num antagoniste dans l'objet contact                
       cdan => get_i4_vector(contact)
     
       allocate(shiftted_cdan(3))

       ! Pour le cas séquentiel only!!!!
       shiftted_cdan(1)=cdan(1) + ((isdm - 1)*ntact_POLYR)!
       shiftted_cdan(2)=cdan(2) + ((isdm - 1)*ntact_POLYR)!

       ! numéro de corps associé au contacteur courant
       icdbdy=polyr2bdyty(1, cdan(1))
       ianbdy=polyr2bdyty(1, cdan(2))
       if (multiplicite(icdbdy) > 1 .or. multiplicite(ianbdy) > 1) then
          shiftted_cdan(3)=INTRF
       else
          shiftted_cdan(3)=NOINT
       end if

       allocate(r8(3))

       ! calcul de la separation entre les deux polyedres : coord_ct(:, icdtac) - coord_ct(:, iantac)
       r8(1:3)=coord_ct(1:3, ntact_SPHER + cdan(1)) - coord_ct(1:3, ntact_SPHER + cdan(2)) 

       call add_object_to_container(shiftted_rough_PRPRx, nb_shiftted_rough_PRPRx, &
          c5, shiftted_cdan, r8, cx)

    end do

    ! pour tester que les shiftted_rough sont bien remplis jusqu'au bout
    print *, "nb_shiftted_rough_PRPRx =", nb_shiftted_rough_PRPRx
    call close_container(shiftted_rough_PRPRx)
    !print *, "display_shiftted_rough_PRPRx"
    !call display_object_container(shiftted_rough_PRPRx)

 end subroutine creation_shiftted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
! Pour le cas de la détection fine séquentielle
!----------------------------------------------------------------
 subroutine launch_set_anonymous_to_rough_4all
    implicit none

    !--------------------------------------
    ! gestion des contacts sphere/sphere
    if (nb_rough_contact_SPSPx > 0) then
       call set_anonymous_to_rough_SPSPx(shiftted_rough_SPSPx, &
          nb_shiftted_rough_SPSPx)
    end if
    ! gestion des contacts polyedre/polyedre
    if (nb_rough_contact_PRPRx > 0) then
       call set_anonymous_to_rough_PRPRx(shiftted_rough_PRPRx, &
          nb_shiftted_rough_PRPRx)
    end if
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

    ! gestion des contacts sphere/sphere
    call erase_container(shiftted_rough_SPSPx)

    ! gestion des contacts polyedre/polyedre
    call erase_container(shiftted_rough_PRPRx)
 end subroutine erase_shiftted_rough
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine get_list_INTRF_in_DDM
    implicit none

    ! variables locales
    integer :: nb_INTRF_SPSPx ! Nombre de contacts (sphere/sphere) fins taggés 'INTRF'
    integer :: nb_INTRF_PRPRx ! Nombre de contacts (polyedre/polyedre) fins taggés 'INTRF'

    !--------------------------------------

    ! Arguments de sortie de la subroutine
    ! implicite : nb_INTRF et list_INTRF

    ! Calcule le nombre de contacts taggés INTRF
    !    * cas des contacts sphere-sphere
    nb_INTRF_SPSPx = 0
    if (nb_rough_contact_SPSPx > 0) then
       call get_nb_INTRF_SPSPx(nb_INTRF_SPSPx)

       print *, 'nb contacts interface (SPSPx)', nb_INTRF_SPSPx 
    end if

    !    * cas des contacts polyedre-polyedre
    nb_INTRF_PRPRx = 0
    if (nb_rough_contact_PRPRx > 0) then
       call get_nb_INTRF_PRPRx(nb_INTRF_PRPRx)
    
       print *, 'nb contacts interface (PRPRx)', nb_INTRF_PRPRx 
    end if

    ! calcul du nombre total de contacts taggés INTRF
    nb_INTRF = nb_INTRF_SPSPx + nb_INTRF_PRPRx 

    print *, 'nb contacts interface', nb_INTRF

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
 subroutine set_modified_mass_in_DDM(isdm_)
    
    implicit none
    
    integer, intent(in),optional :: isdm_
    integer              :: ibody, imulti, multi  ! indice de corps et de multiplicite courant, degre de multiplicite 
    integer              :: i_rbdy3               ! indice de sous-domaine considere, indice de RBDY3

                             !12345678901234567890123456789012
    character(len=32) :: IAM='DDM_3D::set_modified_mass_in_DDM'

    if (.not. present(isdm_)) call faterr(IAM, "ne gere pas (encore) sans l'argument optionnel isdm_")
    do ibody = 1,nbody
       ! On balaye tous les sous-domaines auquel appartient le RBDY3 pour modifier la masse là où il faut
       ! indice du RBDY3 a updater
       i_rbdy3=(isdm_-1)*nbody + ibody
       call set_mass_RBDY3(i_rbdy3, masses_courantes(ibody))
    
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

                             !1234567890123456789012345
    character(len=25) :: IAM='DDM_3D::compute_interface'

    ! Déterminer le nombre de grains d'interface pour ce sous-domaine.
    ! Je le tente par la fonction intrinseque "merge"

    if (allocated(isdm_multi_tab)) deallocate(isdm_multi_tab)
    allocate(isdm_multi_tab(nbody), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de isdm_multi_tab")

    ! Traitement de l'interface pour chaque sous domaine
    do isdm=1,Nsdm
      
       !! On pourrait sans doute supprimer cette étape, mais elle fournit pour l'instant 
       !  une sécurité appréciable (redondance)
       ! Mise à 0 la valeur de multiplicité des RBDY3 qui ne sont pas dans isdm 
       isdm_multi_tab=merge(multiplicite, 0, mask_particip(:, isdm))
       ! Calcul du nombre de RBDY3 d'interface (pour le sdm considéré)
       nb_RBDY3_interf_sdm(isdm)=count(isdm_multi_tab >= 2)

       ! On peut maintenant allouer les champs d'interface du sous-domaine avec cette valeur!
       if ( associated(liste_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(liste_RBDY3_interf_sdm(isdm)%particule)
       allocate(liste_RBDY3_interf_sdm(isdm)%particule(nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de liste_RBDY3_interf_sdm(isdm)%particule")


       ! On va maintenant remplir le vecteur des RBDY3 d'interface par sous-domaine
       compteur=0
       visible=.true.

       do ibody=1,nbody ! Boucle sur les corps (non-dupiqués)
          visible=mask_particip(ibody, isdm)
          if ( .not. visible .or. multiplicite(ibody) < 2 ) cycle
          compteur=compteur + 1 

          liste_RBDY3_interf_sdm(isdm)%particule(compteur)=ibody
       end do

       ! test puis affichage du nombre de RBDY3 d'interface pour le sous-domaine courant
       if (compteur /= nb_RBDY3_interf_sdm(isdm))  call faterr(IAM, "Trop ou pas assez de grains " // &
                                                  "d'interface indentifies dans un des sous-domaines")
       print *, "nb_RBDY3_interf_sdm de sdm num ", isdm, " = ", nb_RBDY3_interf_sdm(isdm)

       ! Dimensionnement des vitesses des RBDY3 d'interface et les F_gamma
       if ( associated(V_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(V_RBDY3_interf_sdm(isdm)%particule)
       allocate(V_RBDY3_interf_sdm(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de V_RBDY2_interf_sdm(isdm)%paticule")

       ! Dimensionnement des positions des RBDY3 d'interface
       ! N.B. stockage des coordonnees des centre d'inerties des corps, i.e. vecteurs a trois composantes
       if ( associated(X_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(X_RBDY3_interf_sdm(isdm)%particule)
       allocate(X_RBDY3_interf_sdm(isdm)%particule(3, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de X_RBDY3_interf_sdm(isdm)%paticule")

       ! Dimensionnement des reperes principaux d'inertie des RBDY3 d'interface 
       ! N.B. stockage des coordonnees des reperes principaux d'inerties des corps, i.e. matrices 3 x 3
       if ( associated(localframe_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(localframe_RBDY3_interf_sdm(isdm)%particule)
       allocate(localframe_RBDY3_interf_sdm(isdm)%particule(9, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de localframe_RBDY3_interf_sdm(isdm)%paticule")

       if ( associated(Fg_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(Fg_RBDY3_interf_sdm(isdm)%particule)
       allocate(Fg_RBDY3_interf_sdm(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de Fg_RBDY3_interf_sdm(isdm)%paticule")

       if ( associated(DFg_RBDY3_interf_sdm(isdm)%particule) ) & 
            deallocate(DFg_RBDY3_interf_sdm(isdm)%particule)
       allocate(DFg_RBDY3_interf_sdm(isdm)%particule(6, nb_RBDY3_interf_sdm(isdm)), stat=err)
       if (err/=0) call faterr(IAM, "Pb d'allocation de DFg_RBDY3_interf_sdm(isdm)%paticule")
       ! Mise à zero
       Fg_RBDY3_interf_sdm(isdm)%particule=0.D0
       DFg_RBDY3_interf_sdm(isdm)%particule=0.D0
    end do

    ! Traitement de l'interface globale
    ! Calcul du nombre de particule dans l'interface globale
    nb_RBDY3_interf_glob = count(multiplicite>1)
    print *, "nombre de particule de l'interface globale=", nb_RBDY3_interf_glob

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
    if ( allocated(localframe_RBDY3_interf_glob) ) deallocate(localframe_RBDY3_interf_glob)
    allocate(localframe_RBDY3_interf_glob(9, nb_RBDY3_interf_glob), stat=err)
    if ( err/=0 ) call faterr(IAM, "Pb d'allocation de localframe_RBDY3_interf_glob")

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

    print *, "nb_liens_interf=", nb_liens_interf

    ! Allocation du tableau des sauts de vitesse d'interface
    if ( allocated(saut_V_interf_glob) ) deallocate(saut_V_interf_glob)
    allocate(saut_V_interf_glob(6, nb_liens_interf), stat=err)
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
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY3_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine 
    integer :: ibody, isdm, i_std_bdy, i_RBDY3_interf_sdm, multi, compteur, err
    logical :: visible
    real(kind=8), dimension(6) :: vlocy

    !am: id pour recuperer les vitesses avec un get_vector
    character(len=5) :: id_vlocy
                             !1234567890123456789012345
    character(len=25) :: IAM="DDM_3D::stock_V_interface"

    ! on va chercher la vitesse dans Vaux
    id_vlocy='Vaux_'

    do isdm=1,Nsdm
       ! On rempli la table V_RBDY3_interf_sdm(isdm)
       do i_RBDY3_interf_sdm=1, nb_RBDY3_interf_sdm(isdm) 
          ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_RBDY3_interf_sdm)
          i_std_bdy=(isdm - 1)*nbody + ibody

          if (.not. get_visible(i_std_bdy)) call faterr(IAM, "Pb sur get_visible!!!")

          !vlocy=get_V(i_std_bdy)
          call get_vector_RBDY3(id_vlocy, i_std_bdy, vlocy, 6)
          V_RBDY3_interf_sdm(isdm)%particule(:, i_RBDY3_interf_sdm)=vlocy
       end do
    end do
    
    !do isdm=1, Nsdm
    !   do i_RBDY3_interf_sdm=1, nb_RBDY3_interf_sdm(isdm)
    !      print *, "V_RBDY3_interf_sdm(", isdm, ")%particule(:, ", i_RBDY3_interf_sdm,")= "&
    !               ,V_RBDY3_interf_sdm(isdm)%particule(:, i_RBDY3_interf_sdm)
    !   end do
    !end do
 
 end subroutine stock_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_F_gamma
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
    integer, dimension(1) :: tmp ! variable temporaire
    integer, dimension(2) :: sdm           ! sous domaine courant
    integer, dimension(2) :: signe         ! pour faire alterner le signe
    real(kind=8), dimension(:), pointer :: mass ! matrice de masse du RBDY3 courant

                             !12345678901234567890123 
    character(len=23) :: IAM='DDM_3D::compute_F_gamma'

    integer,      dimension(:), allocatable :: part_sdm ! indices de la particule courante de l'interface dans les deux sdm du lien considéré
    real(kind=8), dimension(:), allocatable :: DF_gamma ! increment de F_gamma calcule a partir du saut de vitesse courant [| V |]
                                                        ! taille : 6*nb_liens
    real(kind=8), dimension(:), allocatable :: V_jump   ! saut de vitesse courant
                                                        ! taille : 6*nb_liens

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

    do i_parti = 1, nb_RBDY3_interf_glob

       ! RBDY2 sur lequel on travaille et sa multiplicité
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
          ! RBDY2 d'interface du sdm courant
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
          ! au moins 2 liens : matrice bande symetrique
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

       ! mise a jour des F_gamma pour chaque copie du corps

       ! pour chaque sous-domaine auquel appartient le corps
       do isdm=1, multi
          ! on recupere le sous-domaine courant
          sdm(1)=body_particip(isdm, ibody)
          ! on met a jour le F_gamma de la copie du corps appartenant au sdm courant
          Fg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) = & 
             Fg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(isdm)) + &
             DFg_RBDY3_interf_sdm(sdm(1))%particule(:, part_sdm(isdm))
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
       call FATERR(IAM, "compteur /= nb_liens_interf")
    end if
                  
 end subroutine compute_F_gamma
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine compute_egluing(Vref, iddm, egluing) 
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites
    ! saut_V_interf_glob
    ! argument "explicite"
    real(kind=8), intent(in)         :: Vref ! Vitesse de référence du problème considéré
                                             ! Ex : sédimentation sous gravité : Vref=g*Dt
    integer, intent(in)              :: iddm ! indice d'itération ddm 

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    real(kind=8), intent(out) :: egluing  ! erreur de recollement globale

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes de la subroutine
    character(len=28) :: clout ! pour calculer le nom du fichier

    egluing=0.d0
    if (nb_liens_interf > 0) then

      !! Calcul de l'erreur globale de recollement
      egluing=sqrt(dot_product(saut_V_interf_glob(1, :), saut_V_interf_glob(1, :)) + &
                   dot_product(saut_V_interf_glob(2, :), saut_V_interf_glob(2, :)) + &
                   dot_product(saut_V_interf_glob(3, :), saut_V_interf_glob(3, :))) / &
                   !(Vref*nb_liens_interf)
                   (nb_liens_interf)

    !  !      1234567890123456789012345678 
    !  clout='POSTPRO/MY_ERROR_xxxxxxx.DAT'
    !  write(clout(18:24), '(I7.7)') Nstep

    !  ! Ecriture du champs saut_Vinterf_glob pour le liens numéro 1
    !  !open(unit=100, file='POSTPRO/MY_ERROR.DAT', form='formatted', action='write')
    !  open(unit=100, file=clout, form='formatted', action='write')
    !  write(100, '(I7, 1X, D14.7)') iddm, egluing
    !  close(100)
    end if
!    write(100, '(I7, 1X, D14.7, 1X, D14.7)') iddm, &
!          sqrt(saut_V_interf_glob(1,1)*saut_V_interf_glob(1,1) + saut_V_interf_glob(2,1)*saut_V_interf_glob(2,1)), &
!          sqrt(saut_V_interf_glob(1,nb_liens_interf/2)*saut_V_interf_glob(1,nb_liens_interf/2) +                   &
!               saut_V_interf_glob(2,nb_liens_interf/2)*saut_V_interf_glob(2,nb_liens_interf/2))                   

!    do isdm=1,Nsdm
!       do i_RBDY2_sdm=1,nb_RBDY2_interf_sdm(i_sdm)
!          print *, "Champs F_gamma(2) de la particule ",i_RBDY2_sdm," du sdm num "&
!                   ,i_sdm," = ", Fg_RBDY2_interf_sdm(i_sdm)%particule(2,i_RBDY2_sdm) 
!       end do
!    end do 

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
    real(kind=8), dimension(6) :: F_gamma

    if (.not. repartition_just_made) then
       do isdm=1, Nsdm
          do i_part_sdm=1, nb_RBDY3_interf_sdm(isdm)
             ! On récupère le numéro du RBDY3 dans le sous-domaine 
             ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_part_sdm)
             ! Recallage des indice pour le cas séquaentiel multidomaine
             i_std_bdy=(isdm - 1)*nbody + ibody
             ! On stocke le F_gamma qui nous intéresse pour ce corps
             ! Après avoir divisé par le pas de temps pour convertir
             ! l'impulsion calculée en force moyenne sur le pas de temps
             F_gamma=(1.D0/H)*Fg_RBDY3_interf_sdm(isdm)%particule(:, i_part_sdm)
             call put_vector_RBDY3('Fext_', i_std_bdy, F_gamma, 6)
          end do
       end do
    else ! On vient donc de faire une nouvelle répartition en sous-domaines
       do isdm=1, Nsdm
          do i_part_sdm=1, nb_RBDY3_interf_sdm(isdm)
             ! On récupère le numéro du RBDY3 dans le sous-domaine 
             ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_part_sdm)
             ! Recallage des indice pour le cas séquentiel multidomaine
             i_std_bdy=(isdm - 1)*nbody + ibody

             if (any(liste_RBDY3_interf_sdm_old(isdm)%particule(:) == ibody)) then
                tmp=maxloc(liste_RBDY3_interf_sdm_old(isdm)%particule(:), &
                                      liste_RBDY3_interf_sdm_old(isdm)%particule(:) == ibody)
                old_i_part_sdm=tmp(1)
                Fg_RBDY3_interf_sdm(isdm)%particule(:, i_part_sdm) = &
                Fg_RBDY3_interf_sdm_old(isdm)%particule(:, old_i_part_sdm)
                ! On stocke le F_gamma qui nous intéresse pour ce corps
                ! Après avoir divisé par le pas de temps pour convertir
                ! l'impulsion calculée en force moyenne sur le pas de temps
                F_gamma=(1.D0/H)*Fg_RBDY3_interf_sdm_old(isdm)%particule(:, old_i_part_sdm)
                call put_vector_RBDY3('Fext_', i_std_bdy, F_gamma, 6)
             end if
          end do
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
    integer            :: i_std_bdy
    integer            :: isdm
   
    do isdm=1, Nsdm 
       do i_part_sdm=1, nb_RBDY3_interf_sdm(isdm)
          ! On récupère le numéro du RBDY3 dans le sous-domaine 
          ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_part_sdm)
          ! Recallage des indices pour le cas séquentiel multidomaine
          i_std_bdy=(isdm-1)*nbody+ibody

          ! calcul de Vaux = Vfree + M^-1 Iaux (et CL appliquees a Vaux)
          call comp_vlocy(i_std_bdy, iVaux_e_invM_t_Iaux_p_Vfree)
       end do
    end do

 end subroutine comp_V_list_RBDY3_in_DDM
!----------------------------------------------------------------

!----------------------------------------------------------------
!am: fonction qui envoie dans la premiere tranche les deplacements,
! et vitesses au debut du pas de temps + l'orientation du repere principal
! d'inertie dans la configuration de detection => calcul des coordonnees dans
! la configuration de detetcion
 subroutine gather_X_V_begin
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
    ! type(T_ligne_r), dimension(Nsdm) :: X_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm) :: localframe_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY3_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, imulti
    integer            :: i_std_bdy
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localframe_tmp

                             !123456789012345678901234
    character(len=24) :: IAM="DDM_3D::gather_X_V_begin"
   
    logical :: test
 
    test = .false.

    compteur=0
    visible=.false.
    do isdm=2, Nsdm
       do ibody=1, nbody
          i_std_bdy = (isdm - 1)*nbody + ibody
          visible=get_visible(i_std_bdy)
          imulti=multiplicite(ibody)
          !am : le test sur la multiplicite n'est plus licite au-dela de deux sous-domaines.
          ! L'envoi des informations d'interfaces au sous-domaines met a jour les donnees
          ! des grains d'interface du sous-domaine 1. Ainsi, on peut sauter les grains d'interfaces
          ! du sous-domaine 1, lors de l'envoi des infos dans la premiere tranche. Mais, les grains
          ! d'interface n'appartenant pas au sous-domaine 1 ne sont jamais envoyees! Exemple :
          !  1 | 2 | 3
          ! dans ce cas, les infos des grains de l'interface 2-3 ne peuvent jamais etre envoyes.
          !if (.not. visible .or. imulti > 1) cycle
          if (.not. visible) cycle
          compteur=compteur + 1

          ! * champs au debut du pas de temps
          call get_vector_RBDY3('Xbeg_', i_std_bdy, X_tmp, 6)
          call put_vector_RBDY3('Xbeg_', ibody, X_tmp, 6)

          call get_vector_RBDY3('Vbeg_', i_std_bdy, V_tmp, 6)
          call put_vector_RBDY3('Vbeg_', ibody, V_tmp, 6)

          ! * repere principal d'inertie dans la configuration de detection
          call get_matrix_RBDY3('IFTT_', i_std_bdy, localframe_tmp, 3)
          call put_matrix_RBDY3('IFTT_', ibody, localframe_tmp, 3)
       end do
       !am : maj du test de coherence, suite a la maj du test dans la boucle sur les corps
       !if (compteur /= (count(mask_particip(:, isdm) .eqv. .true.) - nb_RBDY3_interf_sdm(isdm))) &
       !   call faterr(IAM, "compteur /= visibles - nb_RBDY3_interf_sdm(isdm)")
       test = compteur /= (count(mask_particip(:, isdm) .eqv. .true.))
       !if (Nstep /= 1 .and. compteur /= (count(mask_particip(:, isdm) .eqv. .true.))) &
       if (Nstep /= 1 .and. test) &
          call faterr(IAM, "compteur /= visibles")
       compteur=0
    end do

 end subroutine gather_X_V_begin

!----------------------------------------------------------------
!am: fonction qui envoie dans la premiere tranche les boites englobantes des contacteurs
! polyedres, dans la configuration de de detection,
 subroutine gather_POLYR_TT
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
    integer            :: i_std_bdy
    integer            :: itact
    integer            :: i_std_tact
    integer            :: compteur
    logical            :: visible

                             !12345678901234567890123
    character(len=23) :: IAM="DDM_3D::gather_POLYR_TT"

    ! s'il n'y a aucun contacteur polyedre, on sort
    if (ntact_POLYR == 0) return
    
    compteur=0
    visible=.false.
    !am: attention, tous les sdm doivent remplir les tableaux min_post et max_pos, meme le premier!
    !do isdm=2, Nsdm
    do isdm=1, Nsdm
       do itact=1, ntact_POLYR
          i_std_tact = (isdm - 1)*ntact_POLYR + itact

          i_std_bdy = polyr2bdyty(1, i_std_tact)
 
          visible=get_visible(i_std_bdy)
          if (.not. visible) cycle
          compteur=compteur + 1

          ! on recupere la boite englobante du polyedre
          !   - le point (xmin, ymin, zmin)
          minpos_POLYR(:, itact) = S_POLYR(i_std_tact)%minpos
          !   - le point (xmax, ymax, zmax)
          maxpos_POLYR(:, itact) = S_POLYR(i_std_tact)%maxpos
       end do
       compteur=0
    end do

 end subroutine gather_POLYR_TT

!----------------------------------------------------------------
!am: fonction qui envoie dans la premiere tranche les deplacements,
! et vitesses a la fin du pas de temps + l'orientation du repere principal
! d'inertie dans la configuration a la fin du pas => calcul des coordonnees dans
! la configuration de la fin du pas de temps
 subroutine gather_X_V
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! arguments implicites
    ! type(T_ligne_r), dimension(Nsdm) :: X_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm) :: localframe_RBDY3_interf_sdm(isdm)
    ! type(T_ligne_r), dimension(Nsdm) :: V_RBDY3_interf_sdm(isdm)
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: isdm
    integer            :: ibody, imulti
    integer            :: i_std_bdy
    integer            :: compteur
    logical            :: visible
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localframe_tmp

                             !123456789012345678
    character(len=18) :: IAM="DDM_3D::gather_X_V"
    
    compteur=0
    visible=.false.
    do isdm=2, Nsdm
       do ibody=1, nbody
          i_std_bdy = (isdm - 1)*nbody + ibody
          visible=get_visible(i_std_bdy)
          imulti=multiplicite(ibody)
          !am : le test sur la multiplicite n'est plus licite au-dela de deux sous-domaines.
          ! L'envoi des informations d'interfaces au sous-domaines met a jour les donnees
          ! des grains d'interface du sous-domaine 1. Ainsi, on peut sauter les grains d'interfaces
          ! du sous-domaine 1, lors de l'envoi des infos dans la premiere tranche. Mais, les grains
          ! d'interface n'appartenant pas au sous-domaine 1 ne sont jamais envoyees! Exemple :
          !  1 | 2 | 3
          ! dans ce cas, les infos des grains de l'interface 2-3 ne peuvent jamais etre envoyes.
          !if (.not. visible .or. imulti > 1) cycle
          if (.not. visible) cycle
          compteur=compteur + 1

          ! * champs a la fin du pas de temps
          call get_vector_RBDY3('X____', i_std_bdy, X_tmp, 6)
          call put_vector_RBDY3('X____', ibody, X_tmp, 6)
          ! gestion du repere principal d'inertie
          call get_matrix_RBDY3('IF___', i_std_bdy, localframe_tmp, 3)
          call put_matrix_RBDY3('IF___', ibody, localframe_tmp, 3)

          call get_vector_RBDY3('V____', i_std_bdy, V_tmp, 6)
          call put_vector_RBDY3('V____', ibody, V_tmp, 6)
       end do
       !am : maj du test de coherence, suite a la maj du test dans la boucle sur les corps
       !if (compteur /= (count(mask_particip(:, isdm) .eqv. .true.) - nb_RBDY3_interf_sdm(isdm))) &
       !   call faterr(IAM, "compteur /= visibles - nb_RBDY3_interf_sdm(isdm)")
       if (compteur /= (count(mask_particip(:, isdm) .eqv. .true.))) &
          call faterr(IAM, "compteur /= visibles")
       compteur=0
    end do

 end subroutine gather_X_V

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
    integer :: ibody, i_std_bdy, i_RBDY3_interf_glob, &
               imulti, multi, isdm       
    logical :: visible
    real(kind=8), dimension(6) :: V_tmp

                             !1234567890123456789012345
    character(len=25) :: IAM="DDM_3D::fix_X_V_interface"

    V_RBDY3_interf_glob=0.d0

    do i_RBDY3_interf_glob=1, nb_RBDY3_interf_glob
       ibody=interface_globale(i_RBDY3_interf_glob)
       multi=multiplicite(ibody)
       do imulti=1, multi
          isdm = body_particip(imulti, ibody)
          i_std_bdy=(isdm - 1)*nbody + ibody
 
          call get_vector_RBDY3('V____', i_std_bdy, V_tmp, 6)
          V_RBDY3_interf_glob(:, i_RBDY3_interf_glob)=V_RBDY3_interf_glob(:, i_RBDY3_interf_glob) &
                                       + V_tmp / multi
       end do
       ! Mise en donnée de la vitesse moyenne de la particule 
       ! dans la première tranche --> processus hôte
       call put_vector_RBDY3('V____', ibody, V_RBDY3_interf_glob(:, i_RBDY3_interf_glob), 6)
    end do     

 end subroutine fix_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
! On va mettre les vistesses moyennes calculées dans
! fix_interface dans les listes par sous-domaines à envoyer aux 
! processus esclaves.
 subroutine compute_V_interface_sdm
    implicit none

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments d'entree de la subroutine 
    ! arguments implicites ::
    ! real(kind=8), dimension(3,nb_RBDY3_interf_glob) :: V_RBDY3_interf_glob
    ! arguments "explicites"
    
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
    ! real(kind=8), dimension(3,nb_RBDY3_interf_sdm) :: V_RBDY3_interf_sdm
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer            :: i_RBDY3_glob, i_RBDY3_sdm
    integer            :: ibody,i_std_bdy
    integer            :: isdm
    integer            :: multi,imulti
    integer, dimension(1) :: tmp
   
    do i_RBDY3_glob=1, nb_RBDY3_interf_glob
       ibody=interface_globale(i_RBDY3_glob)
       multi=multiplicite(ibody)
       do imulti=1, multi
          isdm=body_particip(imulti, ibody)
          ! On va pècher l'indice du RBDY3 "ibody" dans la liste des
          ! RBDY2 d'interface du sdm "isdm"
          tmp=maxloc(liste_RBDY3_interf_sdm(isdm)%particule(:), &
                     liste_RBDY3_interf_sdm(isdm)%particule(:) == ibody)
          i_RBDY3_sdm=tmp(1)
          V_RBDY3_interf_sdm(isdm)%particule(:, i_RBDY3_sdm)=V_RBDY3_interf_glob(:, i_RBDY3_glob)
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
    ! real(kind=8), dimension(6, nb_RBDY3_interf_sdm) :: V_RBDY3_interf_sdm
    ! argument "explicite"

    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments de sortie de la subroutine
 
    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ! Arguments internes a la subroutine
    integer                    :: isdm
    integer                    :: i_RBDY3_sdm
    integer                    :: ibody, i_std_bdy
    real(kind=8), dimension(6) :: X_tmp, V_tmp
    real(kind=8), dimension(3, 3) :: localframe_tmp

    do isdm=1,Nsdm
       do i_RBDY3_sdm=1, nb_RBDY3_interf_sdm(isdm)
          ibody=liste_RBDY3_interf_sdm(isdm)%particule(i_RBDY3_sdm)
          i_std_bdy=(isdm - 1)*nbody + ibody
          V_tmp=0.d0
          V_tmp=V_RBDY3_interf_sdm(isdm)%particule(:, i_RBDY3_sdm)
          call put_vector_RBDY3('V____', i_std_bdy, V_tmp, 6)
       end do
    end do

 end subroutine set_V_interface
!----------------------------------------------------------------

!----------------------------------------------------------------
 subroutine set_visibility_4all_in_DDM(nb_RBDY3__, isdm)
 
    implicit none
    
    integer, intent(in)           :: nb_RBDY3__
    integer, intent(in), optional :: isdm
    
    if (present(isdm)) then
       call set_visibility_4all_RBDY3(mask_redim(:, isdm), nb_RBDY3__)
    else
       !am : print de parano
       !print*, 'mask_RBDY3=', mask_RBDY3
       !print*, 'mask_redim(:, 1)=', mask_redim(:, 1)
       !print*, 'mask_redim(:, 2)=', mask_redim(:, 2)
       call set_visibility_4all_RBDY3(mask_RBDY3, nb_RBDY3__)
    end if

 end subroutine set_visibility_4all_in_DDM
!----------------------------------------------------------------

! procedure de nettoyage de la memoire encore allouee
subroutine clean_ddm

   implicit none

   ! nettoyage de la memoire encore allouee dans le module de detection grossiere
   call clean_module 

end subroutine clean_ddm

end module DDM_3D
