!> module dedicated to deposit in a tridimensional container : a box, a cylinder or a sphere
module deposit3D

   ! am : import du module utilities pour acceder a la procedure
   ! qui initialise le generateur de nombres pseudo-aleatoires
   use utilities, only : init_random_seed
   use overall, only : PI_g

   implicit none 
 
   private
 
   integer,parameter                       :: n_verlet=2000 !nombre maximum de voisins lors du depot d 1 part
   
   !> type used to describe a particle
   type part                                               !liste d informations definissant une particule
      integer                               :: p_Flag         !marqueur de validite
      integer                               :: p_TactNbr      !nb de contacts geometriques
      integer                               :: p_VerNbr       !nb de voisins lors du depot
      integer,dimension(n_verlet)           :: p_Verlet       !liste des voisins
      real(kind=8),dimension(3)             :: p_Centre       !centre de la particule
      real(kind=8)                          :: p_Ray          !rayon de la particule
      integer                               :: p_Mat          !marqueur de materiau
   end type part 

   !> type used to describe a container sphere
   type spher                                              !liste d informations definissant une sphere
      real(kind=8),dimension(3)             :: s_Centre       !centre de la sphere
      real(kind=8)                          :: s_Ray          !rayon de la sphere
   end type spher 

   !> type used to describe a container cylinder
   type cylinder                                           !liste d informations definissant un cylindre
      real(kind=8),dimension(3)             :: c_Centre       !un point sur l axe du cylindre
      real(kind=8),dimension(3)             :: c_Vector       !vecteur directeur du cylindre
      real(kind=8)                          :: c_Ray          !rayon du cylindre
   end type cylinder 

   !> type used to describe a container : a box, a sphere or a cylinder
   type contenant                                          !liste d informations definissant un contenant
      character(len=4)                      :: Type           !mot cle indiquant le type de contenant
      real(kind=8),dimension(6,5)           :: Plan           !liste de 6 plans definis comme tel :
                                                             ! Plan(:,1)X+Plan(:,2)Y+Plan(:,3)Z+Plan(:,4)=0
      type (cylinder)                       :: Cyli           !informations du cylindre
      type (spher)                          :: Spher          !informations de la sphere
   end type contenant
 
   type (part),allocatable,dimension(:)    :: list         !tableau definissant l ensemble des particules
   type (contenant)                        :: cont         !contenant
   integer                                 :: n_body       !nombre maximum de particules
   integer                                 :: i_list       !taille reelle du tableau 'list'
   integer                                 :: n_init       !nombre de particules initiales, i.e. deja deposees

   real(kind=8)                            :: R            ! rayon du cercle inscrit dans le contenant (dans le plan z=0)
   real(kind=8)                            :: lz           ! hauteur du point le plus haut de l'echantillon 
                                                           ! (i.e. max(altitude + rayon), pour toutes les spheres deposees)

   public :: &
      set_box, &
      set_cylinder, &
      set_sphere, &
      new_deposit, &
      deposit, &
      get_computed_particles

contains

   !> this function initializes a container box
   subroutine set_box(lx,ly,lz)

      implicit none

      ! variables d'entree :
      real(kind=8), intent(in) :: lx !< length of the box following the axis (Ox)
      real(kind=8), intent(in) :: ly !< length of the box following the axis (Oy)
      real(kind=8), intent(in) :: lz !< length of the box following the axis (Oz)

      ! affectation du type de contenant
      cont%Type = 'box_'
      ! calcul du rayon du cercle inscrit dans la boite
      R = min( lx, ly )

      ! affectation des coeff des equations de plans
      cont%Plan( 1, 1 ) =  0.D0
      cont%Plan( 1, 2 ) =  0.D0
      cont%Plan( 1, 3 ) =  1.D0
      cont%Plan( 1, 4 ) =  0.D0
      cont%Plan( 1, 5 ) = -1.D0

      cont%Plan( 2, 1 ) =  0.D0
      cont%Plan( 2, 2 ) =  0.D0
      cont%Plan( 2, 3 ) =  1.D0
      cont%Plan( 2, 4 ) = -lz
      cont%Plan( 2, 5 ) =  1.D0

      cont%Plan( 3, 1 ) =  0.D0
      cont%Plan( 3, 2 ) =  1.D0
      cont%Plan( 3, 3 ) =  0.D0
      cont%Plan( 3, 4 ) =  ly * 0.5D0
      cont%Plan( 3, 5 ) = -1.D0

      cont%Plan( 4, 1 ) =  1.D0
      cont%Plan( 4, 2 ) =  0.D0
      cont%Plan( 4, 3 ) =  0.D0
      cont%Plan( 4, 4 ) =  lx * 0.5D0
      cont%Plan( 4, 5 ) = -1.D0

      cont%Plan( 5, 1 ) =  1.D0
      cont%Plan( 5, 2 ) =  0.D0
      cont%Plan( 5, 3 ) =  0.D0
      cont%Plan( 5, 4 ) = -lx * 0.5D0
      cont%Plan( 5, 5 ) =  1.D0

      cont%Plan( 6, 1 ) =  0.D0
      cont%Plan( 6, 2 ) =  1.D0
      cont%Plan( 6, 3 ) =  0.D0
      cont%Plan( 6, 4 ) = -ly * 0.5D0
      cont%Plan( 6, 5 ) =  1.D0

   end subroutine set_box

   !> this function initializes a container cylinder
   subroutine set_cylinder(l_R,lz)

      implicit none

      ! variables d'entree :
      real(kind=8), intent(in) :: l_R !< radius of the cylinder
      real(kind=8), intent(in) :: lz !< height of the cylinder

      R=l_R
    
      cont%Type='cyli'                                 !affectation du type de contenant
      cont%Cyli%c_Centre(1)=0.D0                       !affectation des coeff du cylindre
      cont%Cyli%c_Centre(2)=0.D0 
      cont%Cyli%c_Centre(3)=0.D0 
      cont%Cyli%c_Vector(1)=0.D0 
      cont%Cyli%c_Vector(2)=0.D0 
      cont%Cyli%c_Vector(3)=1.D0 
      cont%Cyli%c_Ray=R 
      cont%Plan(1,1)=0.D0                              !affectation des coeff des plans fermant le cylindre
      cont%Plan(1,2)=0.D0 
      cont%Plan(1,3)=1.D0 
      cont%Plan(1,4)=0.0D0 
      cont%Plan(1,5)=-1.D0 
      cont%Plan(2,1)=0.D0 
      cont%Plan(2,2)=0.D0 
      cont%Plan(2,3)=1.D0 
      cont%Plan(2,4)=-lz 
      cont%Plan(2,5)=1.D0 

   end subroutine set_cylinder

   !> this function initializes a container sphere
   subroutine set_sphere(l_R,coor)

      implicit none

      ! variables d'entree :
      real(kind=8), intent(in) :: l_R !< radius of the sphere
      real(kind=8), intent(in) :: coor(3) !< position of the center of the sphere

      cont%Type='sphr'                                !affectation du type de contenant
      cont%Spher%s_Centre = coor 
      R = l_R !< --- a voir
      cont%Spher%s_Ray=R                               !affectation des coeff de la sphere
      cont%Plan(1,1)=0.D0                              !affectation des coeff des plans fermant la sphere
      cont%Plan(1,2)=0.D0                              ! ... (valable pour le depot de fils)
      cont%Plan(1,3)=1.D0 
      cont%Plan(1,4)=0.8*R-cont%Spher%s_Centre(3) 
      cont%Plan(1,5)=-1.D0 
      cont%Plan(2,1)=0.D0 
      cont%Plan(2,2)=0.D0 
      cont%Plan(2,3)=1.D0 
      cont%Plan(2,4)=-0.8*R-cont%Spher%s_Centre(3) 
      cont%Plan(2,5)=1.D0 

   end subroutine set_sphere

   !> this function initializes a new deposit : allocate memory, store informations about deposited particles, 
   !> compute the position of the first particle
   subroutine new_deposit(nb_particles, given_radii, deposited_radii, deposited_coor, seed, bavard)
      implicit none
      !> number of particles
      integer, intent(in) :: nb_particles
      !> given radii list (i.e. granulometry)
      real(kind=8), dimension(nb_particles), intent(in) :: given_radii
      ! optionals (may be null on input):
      !> radii of already deposited particles
      real(kind=8), dimension(:)  , pointer :: deposited_radii
      !> coordinates of already deposited particles
      real(kind=8), dimension(:,:), pointer :: deposited_coor
      !> an input seed to control the randomness
      integer     , dimension(:)  , pointer :: seed
      !> to decide if to log
      logical, intent(in) :: bavard
      ! local variables 
      logical :: valid ! indicateur de validite pour le depot de la premiere particule
      integer(kind=4) :: i

      ! si on a donne un nombre de particules deposees initialement
      if( associated( deposited_radii ) ) then
         ! on le stocke dans le module
         n_init = size(deposited_radii)
   
         ! on teste la compatibilite des donnees :
         ! * completude des donnees
         if (.not. associated(deposited_coor) ) then
            print *,'[deposit3D::new_deposit]: FATAL ERROR: missing deposited_coor with deposited_radii'
            stop
         endif
         ! * coherence avec les coordonnes des particules deja deposees
         if (size(deposited_coor, 2) /= n_init) then
            print *,'[deposit3D::new_deposit]: FATAL ERROR: non conforming size', &
                    ' for coordinates of deposited particles and deposited radii'
            stop
         endif
         if (size(deposited_coor, 1) /= 3 ) then
            print *,'[deposit3D::new_deposit]: FATAL ERROR: non conforming size', &
                    ' for coordinates of deposited particles and space dim'
            stop
         endif
      ! sinon,
      else
         ! il n'y a pas de particule deja deposees
         n_init = 0
      endif

      ! on calcule le nombre total de particules (granulo + particules deja deposees)
      n_body = nb_particles + n_init

      ! on alloue l'espace memoire pour stocker la liste de particules
      if (allocated(list)) deallocate(list) 
      allocate(list(n_body)) 

      ! on enregistre les informations relatives aux particules deja deposees
      do i=1, n_init
         list(i)%p_Centre(:)=deposited_coor(:,i)
         list(i)%p_Ray      =deposited_radii(i) 
         list(i)%p_Flag     =0 
         list(i)%p_Mat      =1 
      end do

      i_list=n_init+1 !initialisation de la dimension du tableau 'list'

      ! on stocke la granulometrie des particules a deposer
      do i=1, nb_particles
         list(n_init + i)%p_Ray=given_radii(i) 
      end do 

      ! on initialise le generateur de nombre pseudo_aleatoires
      if( associated(seed) ) then
        call init_random_seed(seed)
      else
        call init_random_seed()
      end if
      
      ! on positionne la premiere sphere
      valid=.false. 
      if( bavard ) then
        write(*,*)'==> CREATING FIRST POINT' 
      end if
      do while (.not. valid) 
         !   depot aleatoire d une particule dans son contenant
         call first_point(cont,list(i_list),list(i_list)%p_Ray) 
         valid=.true. 
         !   test de non interpenetration avec les particules initiales
         do i=1,i_list-1 
            if (norme(list(i_list)%p_Centre,list(i)%p_Centre).lt.(list(i_list)%p_Ray+list(i)%p_Ray)) then 
               valid=.false. 
               exit 
            endif 
         end do 
      end do    

      ! on reinitialise certains parametres, en vue d'appeler le depot :
      lz=0.d0 ! lz sert a stocker la hauteur de la plus haute particule pendant le depot 

   end subroutine new_deposit

   !> this function achieves the deposit
   !> \warning a new deposit and a container must have been initialized 
   subroutine deposit(bavard)
      implicit none
      !> to decide if to log
      logical, intent(in) :: bavard
 
      ! on appelle la routine de depot sur les variables globales stockees dans le module
      call depot_( n_body, cont, n_verlet, i_list, list, bavard )

   end subroutine deposit

   !> Gets the radii and coordinates of deposited particles
   !> \warning allocate the memory
   subroutine get_computed_particles(comp_radii, comp_coor)
      implicit none
      !> radii of the deposited particles
      real(kind=8), dimension(:)   , pointer :: comp_radii
      !> coordinates of the deposited particles
      real(kind=8), dimension(:, :), pointer :: comp_coor
      ! variable locale :
      integer :: nb_comp_particles, i
  
      ! really paranoid...
      if( associated( comp_radii) ) deallocate(comp_radii)
      if( associated( comp_coor ) ) deallocate(comp_coor )

      ! on recupere le nombre de particules reellement deposees
      nb_comp_particles = i_list - n_init

      ! just in case...
      if( nb_comp_particles < 1 ) then
        comp_radii => null()
        comp_coor  => null()
        return
      end if

      ! size arrays
      allocate(comp_radii(nb_comp_particles))
      allocate(comp_coor(3,nb_comp_particles))

      ! on ne recupere que le rayon et les coordonnees des particules deposees par la
      ! routine de depos et pas celle deposees initialement
      do i = 1, nb_comp_particles
         comp_radii(i) = list(i + n_init)%p_Ray
         comp_coor(:, i) = list(i + n_init)%p_Centre(:)
      end do   

      deallocate( list )

   end subroutine get_computed_particles

   !============================================================================
   !                      Subroutine depot 
   ! 
   ! Depose au maximum 'n_body' spheres (en realite 'i_list' spheres) de rayons 
   ! 'rayons_part' en contact geometrique les unes avec les autres dans le contenant 
   ! 'cont'. Il en resulte le tableau de particules 'list' a l interieur duquel est 
   ! centralise toutes les informations relatives a l echantillon. Le tableau 'list' 
   ! peut etre non vide dans ce cas les elements presents a l interieur seront pris 
   ! en compte
   !=============================================================================
   !> this function deposits particles (spheres) in a given container. Before the 
   !> calling of this function, some particles may be deposited (index in the 
   !> range [1, i_list]) and the others (index in the range [i_list + 1, n_body]) 
   !> are only caracterized by their radius. After the calling particles having 
   !> their index in the range [1, i_list] are deposited, i.e. their positions are 
   !> computed.
   subroutine depot_( n_body, cont, n_verlet, i_list, list, bavard )

     implicit none

     ! variables d'entree :
     !> maximal number of particles, i.e. size of list     
     integer          , intent( in )    :: n_body   
     !> the given container : a box, a cylinder or a sphere
     type( contenant ), intent( in )    :: cont     
     !> maximal number of neighbors for a particle
     integer          , intent( in )    :: n_verlet 

     ! variable d'entree-sortie :
     !> number of deposited particles
     integer          , intent( inout ) :: i_list   

     ! nombre reel de particules :
     ! - en entree : nombre de particules deja deposees (grosses particules)
     ! - en sortie : nombre total de particules deposees
     type( part ), dimension( n_body ), intent( inout ) :: list !< list of all particles (deposited or not)
     ! tableau des informations des 'n_body' part :
     ! - en entree : caracteristiques des particules deja deposees (grosses particules)
     ! - en sortie : caracteristiques de toutes les particules deposees

     !> to decide if to log
     logical, intent(in) :: bavard


     ! variables locales :
     integer         , parameter :: n_actif = 100     ! nombre maximum de part de reference
     real( kind = 8 ), parameter :: tol     = 0.5D0   ! tolerence

     type( part )                        :: candidate      ! info d une particule candidate au depot
     integer                             :: i,j,k,l,m,n    ! compteurs incrementaux
     integer                             :: i_num          ! nombre d part de reference
     integer                             :: i_box          ! nombre de plans a verifier
     integer                             :: compt          ! compteur du nombre de solutions
     integer                             :: n_init         ! nombre de particules initiales
     integer, dimension( n_actif + 10 )  :: num            ! tableau de numeros des part de reference
     integer, dimension( 3 )             :: triplet        ! tableau des numeros des elements supports
     real( kind = 8 ), dimension( 2 )    :: X,Y,Z,W        ! coef pour la resolution du depot
     real( kind = 8 )                    :: norm1,norm2    ! normes
     real( kind = 8 )                    :: nrj1,nrj2,nrjc ! parametres energetiques
     real( kind = 8 )                    :: tamp,k1,k2     ! variables tampons
     real( kind = 8 )                    :: theta,phi      ! angles aleatoires
     real( kind = 8 )                    :: interpen       ! interpenetration totale
     real( kind = 8 )                    :: interpen_unit  ! interpenetration du contact considere
     real( kind = 8 ), dimension( 3 )    :: pt1,pt2        ! points
     real( kind = 8 ), dimension( 2, 3 ) :: sol            ! couple de solutions directionelles
     real( kind = 8 ), dimension( 2, 3 ) :: centre_sol     ! couple de solutions potentielles

     logical :: exist                              ! label d existance de solution
     logical :: intersec1, intersec2               ! labels d inter-penetration
     logical :: out1, out2                         ! labels de sortie du contenant
     logical :: condition1, condition2, condition3 ! labels divers
     logical :: first                              ! label de premier passage
     logical :: no_iter                            ! label de non iteration

     ! real(kind=8),dimension(n_body) :: rayons_part !tableau des rayons des part a deposer

     !- ------------------------------------------------------------------------
     !  Initialisation des parametres du depot
     !- ------------------------------------------------------------------------
     ! determination du nombre de plan a prendre en compte
     if ( cont%Type == 'box_' ) then
        i_box = 6
     elseif ( cont%Type == 'cyli' ) then 
        i_box = 2
     elseif ( cont%Type == 'sphr' ) then 
        i_box = 0
     endif

     n_init   = i_list - 1 ! affectation du nombre de particules initiales
     no_iter  = .false.    ! initialisation du label de non iteration
     interpen = 0.D0       ! initialisation de l interpenetration totale

     !- ------------------------------------------------------------------------
     !  Boucle de depot d une particule i
     !- ------------------------------------------------------------------------
     do i = i_list + 1, n_body
        if ( .not. no_iter ) then
           ! info utilisateurs
           if( bavard ) then
             write( *, * ) '******** Depot de Part', i_list + 1, '********'
           end if
        end if

        ! actualisation de la liste des particules pouvant servir de particule 
        ! de reference ( liste 'num' )
        call update_num( i_list, list, n_body, i_num, num, n_actif + 10 ) 

        ! actualisation de la liste des voisins des particules de la liste 'num'
        call update_verlet( i_list, list, i_num, num, n_actif + 10, &
                            ( list( i_list + 1 )%p_Ray * 1.5 ), n_verlet )

        ! initialisation des caracteristiques de la particule candidate 
        call zero_part( candidate ) 

        candidate%p_Ray     = list( i_list + 1 )%p_Ray  ! affectation du rayon de la part candidate
        candidate%p_TactNbr = 3                         ! affectation du nombre de contact initial
        first               = .true.                    ! initialisation du label de premier passage
        compt               = 0                         ! initialisation compteur du nbr de solutions
        interpen_unit       = 0.D0                      ! initialisation de l interpenetration unitr

        ! choix de la particule de ref ( 1er contact )
        do j = 1, i_num
           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! Cas: 1 sphere 2 plans 
           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! parcours des plans (second contact) 
           do k = 1, i_box - 1
              ! parcours des plans (troisieme contact) 
              do l = k + 1, i_box
                 ! resolution du probleme de contact
                 ! affectation des coefs correspondant au second contact
                 call eq_plan( X( 1 ), Y( 1 ), Z( 1 ), W( 1 ), &
                               list( num( j ) )%p_Centre, list( num( j ) )%p_Ray, &
                               cont%Plan( k, : ), candidate%p_Ray )
                 ! affectation des coefs correspondant au troisieme contact
                 call eq_plan( X( 2 ), Y( 2 ),Z( 2 ), W( 2 ), &
                               list( num( j ) )%p_Centre, list( num( j ) )%p_Ray, &
                               cont%Plan( l, : ), candidate%p_Ray )


                 ! resolution du probleme
                 call solve_tact( X, Y, Z, W, sol, exist )

                 ! test d'existence
                 if ( exist ) then
                    ! calcul des centres solutions du probleme de contact  

                    centre_sol( 1, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 1, : )
                    centre_sol( 2, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 2, : )

                    ! incrementation du compteur du nombre de solutions
                    compt = compt + 2
                    !    -   -   -   -   -   -   -   -   -   -   -   -   -   -  
                    ! Test des solutions sur une intersection possible avec les autres particules 
                    !    -   -   -   -   -   -   -   -   -   -   -   -   -   -  
                    ! initialisation des labels d inter-penetration des solutions
                    intersec1 = .false.
                    intersec2 = .false.

                    ! boucle de parcours des voisins de part ref
                    do m = 1, list( num( j ) )%p_VerNbr
                       ! distance inter-centre
                       norm1 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 1, : ) )
                       if ( norm1 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                         candidate%p_Ray ) ) then
                          intersec1 = .true.
                       end if

                       ! distance inter-centre
                       norm2 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 2, : ) )
                       if ( norm2 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                         candidate%p_Ray ) ) then
                          intersec2=.true.
                       end if

                       if ( intersec1 .and. intersec2 ) then
                          exit !test de sortie de la boucle
                       end if
                    end do

                    !    -   -   -   -   -   -   -   -   -   -   -   -   -   -  
                    ! Test de sortie des solutions 
                    !    -   -   -   -   -   -   -   -   -   -   -   -   -   -  
                    ! initialisation des labels de sortie des deux solutions
                    out1 = .false.
                    out2 = .false.

                    ! boucle de parcours des plans a prendre en compte
                    do m = 1, i_box
                       ! cf. 'tex_position'
                       tamp = dsqrt( cont%Plan( m, 1 )**2.D0 + &
                                     cont%Plan( m, 2 )**2.D0 + &
                                     cont%Plan( m, 3 )**2.D0 )

                       pt1( 1 ) = centre_sol( 1, 1 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 1 ) / tamp 
                       pt1( 2 ) = centre_sol( 1, 2 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 2 ) / tamp 
                       pt1( 3 ) = centre_sol( 1, 3 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 3 ) / tamp 

                       norm1 = cont%Plan( m, 1 ) * pt1( 1 ) + &
                               cont%Plan( m, 2 ) * pt1( 2 ) + &
                               cont%Plan( m, 3 ) * pt1( 3 ) + &
                               cont%Plan( m, 4 )

                       pt2( 1 ) = centre_sol( 2, 1 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 1 ) / tamp 
                       pt2( 2 ) = centre_sol( 2, 2 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 2 ) / tamp 
                       pt2( 3 ) = centre_sol( 2, 3 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 3 ) / tamp

                       norm2 = cont%Plan( m, 1 ) * pt2( 1 ) + &
                               cont%Plan( m, 2 ) * pt2( 2 ) + &
                               cont%Plan( m, 3 ) * pt2( 3 ) + &
                               cont%Plan( m, 4 )

                       if ( norm1 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out1=.true. !test de sortie de la solution 1
                       end if
                       if ( norm2 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out2=.true. !test de sortie de la solution 2
                       end if
                       if ( out1 .and. out2 ) then
                          exit        !test de sortie de la boucle
                       end if

                    end do

                    ! ici le contenant est forcement une boite car il ne peut 
                    ! pas y avoir de contact sphere plan plan
                    ! dans les autres cas

                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Changement conditionelle de la solution finale 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! calcul de l'energie potentielle de la solution 1
                    nrj1 = energy( centre_sol( 1, : ) )
                    ! calcul de l'energie potentielle de la solution 2
                    nrj2 = energy( centre_sol( 2, : ) )
                    ! calcul de l'energie potentielle de la candidate
                    nrjc = energy( candidate%p_Centre )

                    norm1 = norme( centre_sol( 1, : ), list( num( j ) )%p_Centre )
                    ! distance inter-particulaire de la solution 1
                    norm1 = norm1 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    norm2 = norme ( centre_sol( 2, : ), list( num( j ) )%p_Centre )
                    ! distance inter-particulaire de la solution 2
                    norm2 = norm2 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    ! les deux tests suivant visent a reperer si l une ou l autre 
                    ! des solution que l on propose est valide par rapport aux 
                    ! criteres : de non penetration, de sortie du contenant, d energie 
                    ! minimum mais aussi si elle ne fournissent pas des resultats 
                    ! non conforme (non penetration de la candidate
                    ! avec la part de ref et un eloignement inferieur a une fraction 'tol' du rayon)
                    if ( ( .not. intersec1 ) .and. ( .not. out1 ) .and. &
                         ( ( nrj1 < nrjc ) .or. first ) .and. &
                         ( norm1 < ( candidate%p_Ray *tol ) ) .and. ( norm1 > 0.D0 ) ) then 

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 1, : )
                       ! determination du triplet support
                       triplet( 1 ) = num( j )
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )

                       ! infirmation du label de premier passage
                       first = .false.

                       ! calcul de l interpenetration sur le support
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + &
                                                    list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ), &
                                                           list( triplet( 1 ) )%p_Centre ) ) )

                    elseif ( ( .not. intersec2 ) .and. ( .not. out2 ) .and. &
                             ( ( nrj2 < nrjc ) .or. first ) .and. &
                             ( norm2 < ( candidate%p_Ray * tol ) ) .and. (norm2 > 0.D0 ) ) then 

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 2, : )
                       ! determination du triplet support
                       triplet( 1 ) = num( j )
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )
 
                       ! infirmation du label de premier passage
                       first = .false.
                 
                       ! calcul de l interpenetration sur le support
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + &
                                                    list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 2 , : ), &
                                                           list( triplet( 1 ) )%p_Centre ) ) )
                    endif
                 endif  ! END - if ( exist ) then
              end do  ! END - do l = k + 1, i_box
           end do  ! END - do k = 1, i_box - 1

           !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           ! Cas: 2 spheres 1 plans 
           !  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
           ! parcours des particules positionnees (second contact) 
           do k = 1, list( num( j ) )%p_VerNbr
              ! parcours des plans (troisieme contact) 
              do l = 1, i_box
                 ! resolution du probleme 
                 ! affectation des coefs correspondant au second contact
                 call eq_spher( X( 1 ), Y( 1 ), Z( 1 ), W( 1 ), &
                                list( num( j ) )%p_Centre, list( num( j ) )%p_Ray, &
                                list( list( num( j ) )%p_Verlet( k ) )%p_Centre, &
                                list( list( num( j ) )%p_Verlet( k ) )%p_Ray, &
                                candidate%p_Ray )
                 ! affectation des coefs correspondant au troisieme contact
                 call eq_plan( X( 2 ), Y( 2 ), Z( 2 ), W( 2 ), &
                               list( num( j ) )%p_Centre, &
                               list( num( j ) )%p_Ray, &
                               cont%Plan( l , : ), candidate%p_Ray )

                 ! resolution du probleme
                 call solve_tact( X, Y, Z, W, sol, exist )

                 ! test d existence
                 if ( exist ) then
                    ! calcul des centres solutions du probleme de contact 
                    centre_sol( 1, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 1, : )
                    centre_sol( 2, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 2, : )

                    ! incrementation du compteur du nombre de solutions
                    compt = compt + 2
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Test des solutions sur une intersection possible avec les autres particules 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! initialisation des labels d inter-penetration des solutions
                    intersec1 = .false.
                    intersec2 = .false.

                    ! boucle de parcours des voisins de part ref
                    do m = 1, list( num( j ) )%p_VerNbr
                       ! distance inter-centre
                       norm1 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 1, : ) )

                       if ( norm1 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                          candidate%p_Ray ) ) then 
                          intersec1 = .true. ! test
                       end if

                       ! distance inter-centre
                       norm2 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 2, : ) )

                       if ( norm2 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                         candidate%p_Ray ) ) then
                          intersec2 = .true. ! test
                       end if

                       if ( intersec1 .and. intersec2 ) then
                          exit ! test de sortie de la boucle
                       end if
                    end do

                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Test de sortie des solutions 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! initialisation des labels de sortie des deux solutions
                    out1 = .false.
                    out2 = .false.

                    if ( cont%Type == 'cyli' ) then
                       ! projection (cf. 'tex_position')
                       k1 = ( cont%Cyli%c_Vector( 1 ) * &
                              ( centre_sol( 1, 1 ) - cont%Cyli%c_Centre( 1 ) ) + &
                              cont%Cyli%c_Vector( 2 ) * &
                              ( centre_sol( 1, 2 ) - cont%Cyli%c_Centre( 2 ) ) + &
                              cont%Cyli%c_Vector( 3 ) * &
                              ( centre_sol( 1, 3 ) - cont%Cyli%c_Centre( 3 ) ) ) / &
                            ( cont%Cyli%c_Vector( 1 )**2 + cont%Cyli%c_Vector( 2 )**2 + &
                              cont%Cyli%c_Vector( 3 )**2 )

                       pt1( : ) = cont%Cyli%c_Centre( : ) + k1 * cont%Cyli%c_Vector( : )

                       !calcul de la distance solution 1 / axe du cylindre
                       norm1 = norme( pt1, centre_sol( 1, : ) )

                       if ( norm1 > ( cont%Cyli%c_Ray - candidate%p_Ray ) ) then
                          out1 = .true. !test de sortie de la solution 1
                       end if

                       ! projection (cf. 'tex_position')
                       k2 = ( cont%Cyli%c_Vector( 1 ) * &
                              ( centre_sol( 2, 1 ) - cont%Cyli%c_Centre( 1 ) ) + &
                              cont%Cyli%c_Vector( 2 ) * &
                              ( centre_sol( 2, 2 ) - cont%Cyli%c_Centre( 2 ) ) + &
                              cont%Cyli%c_Vector( 3 ) * &
                              ( centre_sol( 2, 3 ) - cont%Cyli%c_Centre( 3 ) ) ) / &
                            ( cont%Cyli%c_Vector( 1 )**2 + cont%Cyli%c_Vector( 2 )**2 + &
                              cont%Cyli%c_Vector( 3 )**2 )

                       pt2( : ) = cont%Cyli%c_Centre( : ) + k2 * cont%Cyli%c_Vector( : ) 

                       ! calcul de la distance solution 2 / axe du cylindre
                       norm2 = norme( pt2, centre_sol( 2, : ) )

                       if ( norm2 > ( cont%Cyli%c_Ray - candidate%p_Ray ) ) then
                          out2=.true. !test de sortie de la solution 2
                       end if

                    endif  ! END - if ( cont%Type == 'cyli' ) then

                    ! boucle de parcours des plans a prendre en compte
                    do m = 1, i_box
                       
                       ! cf. 'tex_position'
                       tamp = dsqrt( cont%Plan( m, 1 )**2.D0 + cont%Plan( m, 2 )**2.D0 + &
                                     cont%Plan( m, 3 )**2.D0 )
                       pt1( 1 ) = centre_sol( 1, 1 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 1 ) / tamp
                       pt1( 2 ) = centre_sol( 1, 2 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 2 ) / tamp
                       pt1( 3 ) = centre_sol( 1, 3 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 3 ) / tamp

                       norm1 = cont%Plan( m, 1 ) * pt1( 1 ) + &
                               cont%Plan( m, 2 ) * pt1( 2 ) + &
                               cont%Plan( m, 3 ) * pt1( 3 ) + &
                               cont%Plan( m, 4 )

                       pt2( 1 ) = centre_sol( 2, 1 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 1 ) / tamp 
                       pt2( 2 ) = centre_sol( 2, 2 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 2 ) / tamp 
                       pt2( 3 ) = centre_sol( 2, 3 ) + cont%Plan( m, 5 ) * candidate%p_Ray * &
                                  cont%Plan( m, 3 ) / tamp 
                       norm2 = cont%Plan( m, 1 ) * pt2( 1 ) + &
                               cont%Plan( m, 2 ) * pt2( 2 ) + &
                               cont%Plan( m, 3 ) * pt2( 3 ) + &
                               cont%Plan( m, 4 ) 

                       if ( norm1 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out1=.true. !test de sortie de la solution 1
                       end if

                       if ( norm2 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out2=.true. !test de sortie de la solution 2
                       end if

                       if ( out1 .and. out2 ) then
                          exit        !test de sortie de la boucle
                       end if

                    end do  ! END - do m = 1, i_box

                    ! ici le contenant est forcement une boite ou un cylindre 
                    ! car il ne peut pas y avoir de contact sphere sphere plan 
                    ! avec un contenant spherique

                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Changement conditionelle de la solution finale 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! calcul de l'energie potentielle de la solution 1
                    nrj1 = energy( centre_sol( 1, : ) )
                    ! calcul de l'energie potentielle de la solution 2
                    nrj2 = energy( centre_sol( 2, : ) )
                    ! calcul de l'energie potentielle de la candidate
                    nrjc = energy( candidate%p_Centre )

                    norm1 = norme( centre_sol( 1, : ), list( num( j ) )%p_Centre )
                    ! distance inter-particulaire de la solution 1
                    norm1 = norm1 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    norm2 = norme( centre_sol( 2, : ), list( num( j ) )%p_Centre )
                    ! distance inter-particulaire de la solution 2
                    norm2 = norm2 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    ! cf. contact spher/plan/plan (+ haut ^)
                    if ( ( .not. intersec1 ) .and. ( .not. out1 ) .and. &
                         ( ( nrj1 < nrjc ) .or. first ) .and. &
                         ( norm1 < ( candidate%p_Ray * tol ) ) .and. &
                         ( norm1 > 0.D0 ) ) then

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 1, : )
                       ! determination du triplet support
                       triplet( 1 ) = num( j )
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )
                       ! infirmation du label de premier passage
                       first = .false.

                       ! calcul de l interpenetration
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ), &
                                                           list( triplet( 1 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 2 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ), &
                                                    list( triplet( 2 ) )%p_Centre ) ) )

                    elseif ( ( .not. intersec2 ) .and. ( .not. out2 ) .and. &
                             ( ( nrj2 < nrjc ) .or. first ) .and. &
                             ( norm2 < ( candidate%p_Ray * tol ) ) .and. &
                             ( norm2 > 0.D0 ) ) then

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 2, : )
                       ! determination du triplet support
                       triplet( 1 ) = num( j )
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )

                       ! infirmation du label de premier passage
                       first = .false.

                       ! calcul de l interpenetration
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 2, : ), &
                                                           list( triplet( 1 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 2 ) )%p_Ray - &
                                                    norme( centre_sol( 2, : ), &
                                                           list( triplet( 2 ) )%p_Centre ) ) )
                    end if
                 end if  ! END - if ( exist ) then
              end do  ! END - do l = 1, i_box
           end do  ! END - do k = 1, list( num( j ) )%p_VerNbr

           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! Cas: 3 spheres 
           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! parcours des particules positionnees (second contact) 
           do k = 1, list( num( j ) )%p_VerNbr - 1
              ! parcours des particules positionnees (troisieme contact) 
              do l = k + 1, list( num( j ) )%p_VerNbr
                 ! resolution du probleme
                 ! affectation des coefs correspondant au second contact
                 call eq_spher( X( 1 ), Y( 1 ), Z( 1 ), W( 1 ), &
                                list( num( j ) )%p_Centre, list( num( j ) )%p_Ray, &
                                list( list( num( j ) )%p_Verlet( k ) )%p_Centre, &
                                list( list( num( j ) )%p_Verlet( k ) )%p_Ray, & 
                                candidate%p_Ray )

                 ! affectation des coefs correspondant au troisieme contact
                 call eq_spher( X( 2 ), Y( 2 ), Z( 2 ), W( 2 ), &
                                list( num( j ) )%p_Centre, list( num( j ) )%p_Ray, & 
                                list( list( num( j ) )%p_Verlet( l ) )%p_Centre, &
                                list( list( num( j ) )%p_Verlet( l ) )%p_Ray, & 
                                candidate%p_Ray )
                 
                 ! resolution du probleme
                 call solve_tact( X, Y, Z, W, sol, exist )
                 
                 ! test d existence
                 if ( exist ) then
                    ! calcul des centres solutions du probleme de contact 
                    centre_sol( 1, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 1, : ) 
                    centre_sol( 2, : ) = list( num( j ) )%p_Centre( : ) + &
                                         ( list( num( j ) )%p_Ray + candidate%p_Ray ) * sol( 2, : ) 

                    ! incrementation du compteur du nombre de solutions
                    compt = compt + 2
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Test des solutions sur une intersection possible avec les autres particules 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! initialisation des labels d inter-penetration des solutions
                    intersec1 = .false.
                    intersec2 = .false.

                    ! boucle de parcours des voisins de part ref
                    do m = 1, list( num( j ) )%p_VerNbr
                       ! distance inter-centre
                       norm1 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 1, : ) )

                       if ( norm1 < ( list ( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                         candidate%p_Ray ) ) then
                          intersec1=.true. !test
                       end if

                       ! distance inter-centre
                       norm2 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, &
                                      centre_sol( 2, : ) )

                       if ( norm2 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                         candidate%p_Ray ) ) then
                          intersec2=.true. !test 
                       end if

                       if ( intersec1 .and. intersec2 ) then
                          exit !test de sortie de la boucle
                       end if

                    end do

                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Test de sortie des solutions 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! initialisation des labels de sortie des deux solutions
                    out1 = .false.
                    out2 = .false.

                    if ( cont%Type == 'cyli' ) then

                       ! projection (cf. 'tex_position')
                       k1 = ( cont%Cyli%c_Vector( 1 ) * &
                              ( centre_sol( 1, 1 ) - cont%Cyli%c_Centre( 1 ) ) + &
                              cont%Cyli%c_Vector( 2 ) * &
                              ( centre_sol( 1, 2 ) - cont%Cyli%c_Centre( 2 ) ) + &
                              cont%Cyli%c_Vector( 3 ) * ( centre_sol( 1, 3 ) - &
                              cont%Cyli%c_Centre( 3 ) ) ) / &
                            ( cont%Cyli%c_Vector( 1 )**2 + cont%Cyli%c_Vector( 2 )**2 + &
                              cont%Cyli%c_Vector( 3 )**2 )

                       pt1( : ) = cont%Cyli%c_Centre( : ) + k1 * cont%Cyli%c_Vector( : )

                       ! calcul de la distance solution 1 / axe du cylindre
                       norm1 = norme( pt1, centre_sol( 1, : ) )

                       if ( norm1 > ( cont%Cyli%c_Ray - candidate%p_Ray ) ) then
                          out1 = .true. !test de sortie de la solution 1
                       end if

                       ! projection (cf. 'tex_position')
                       k2 = ( cont%Cyli%c_Vector( 1 ) * &
                              ( centre_sol( 2, 1 ) - cont%Cyli%c_Centre( 1 ) ) + &
                              cont%Cyli%c_Vector( 2 ) * &
                              ( centre_sol( 2, 2 ) - cont%Cyli%c_Centre( 2 ) ) + &
                              cont%Cyli%c_Vector( 3 ) * &
                              ( centre_sol( 2, 3 ) - cont%Cyli%c_Centre( 3 ) ) ) / &
                            ( cont%Cyli%c_Vector( 1 )**2 + cont%Cyli%c_Vector( 2 )**2 + &
                              cont%Cyli%c_Vector( 3 )**2 )

                       pt2( : ) = cont%Cyli%c_Centre( : ) + k2 * cont%Cyli%c_Vector( : )

                       ! calcul de la distance solution 2 / axe du cylindre
                       norm2 = norme( pt2, centre_sol( 2, : ) )

                       if ( norm2 > ( cont%Cyli%c_Ray - candidate%p_Ray ) ) then
                          out2=.true. !test de sortie de la solution 2
                       end if

                    elseif ( cont%Type == 'sphr' ) then

                       ! calcul de la dist sol 1 / centre de la sphere
                       norm1 = norme( cont%Spher%s_Centre, centre_sol( 1, : ) )

                       if ( norm1 > ( cont%Spher%s_Ray - candidate%p_Ray ) ) then
                          out1 = .true. !test de sortie de la solution 1
                       end if

                       ! calcul de la dist sol 2 / centre de la sphere
                       norm2 = norme( cont%Spher%s_Centre, centre_sol( 2, : ) )

                       if ( norm2 > ( cont%Spher%s_Ray - candidate%p_Ray ) ) then
                          out2 = .true. !test de sortie de la solution 2
                       end if

                    endif

                    ! boucle de parcours des plans a prendre en compte
                    do m = 1, i_box

                       ! cf. 'tex_position'
                       tamp = dsqrt( cont%Plan( m, 1 )**2.D0 + &
                                     cont%Plan( m, 2 )**2.D0 + &
                                     cont%Plan( m, 3 )**2.D0 )

                       pt1( 1 ) = centre_sol( 1, 1) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 1 ) / tamp
                       pt1( 2 ) = centre_sol( 1, 2 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 2 ) / tamp
                       pt1( 3 ) = centre_sol( 1, 3 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 3 ) / tamp 

                       norm1 = cont%Plan( m, 1 ) * pt1( 1 ) + &
                               cont%Plan( m, 2 ) * pt1( 2 ) + &
                               cont%Plan( m, 3 ) * pt1( 3 ) + &
                               cont%Plan( m, 4 )

                       pt2( 1 ) = centre_sol( 2, 1 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 1 ) / tamp
                       pt2( 2 ) = centre_sol( 2, 2 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 2 ) / tamp
                       pt2( 3 ) = centre_sol( 2, 3 ) + &
                                  cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 3 ) / tamp

                       norm2 = cont%Plan( m, 1 ) * pt2( 1 ) + &
                               cont%Plan( m, 2 ) * pt2( 2 ) + &
                               cont%Plan( m, 3 ) * pt2( 3 ) + &
                               cont%Plan( m, 4 )

                       if ( norm1 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out1=.true. !test de sortie de la solution 1
                       end if

                       if ( norm2 * cont%Plan( m, 5 ) > 0.D0 ) then
                          out2=.true. !test de sortie de la solution 2
                       end if
                       if ( out1 .and. out2 ) then
                          exit        !test de sortie de la boucle
                       end if

                    end do  ! END - do m = 1, i_box

                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   
                    ! Changement conditionelle de la solution finale 
                    !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   

                    ! calcul de l'energie potentielle de la solution 1
                    nrj1 = energy( centre_sol( 1, : ) )
                    ! calcul de l'energie potentielle de la solution 2
                    nrj2 = energy( centre_sol( 2, : ) )
                    ! calcul de l'energie potentielle de la candidate
                    nrjc = energy( candidate%p_Centre )

                    norm1 = norme( centre_sol( 1, : ), list( num( j ) )%p_Centre )
                    ! distance inter-particulaire de la solution 1
                    norm1 = norm1 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    norm2 = norme( centre_sol( 2, : ),list( num( j ) )%p_Centre )
                    !distance inter-particulaire de la solution 2
                    norm2 = norm2 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

                    ! cf. contact spher/plan/plan (+ haut ^)
                    if ( ( .not. intersec1 ) .and. ( .not. out1 ) .and. &
                         ( ( nrj1 < nrjc ) .or. first ) .and. &
                         ( norm1 < ( candidate%p_Ray * tol ) ) .and. &
                         ( norm1 > 0.D0 ) ) then

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 1, : )
                       ! determination du triplet support
                       triplet( 1 ) = num( j )              
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )
                       !infirmation du label de premier passage
                       first = .false.

                       ! calcul de l interpenetration
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ) , &
                                                           list( triplet( 1 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 2 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ), &
                                                           list( triplet( 2 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 3 ) )%p_Ray - &
                                                    norme( centre_sol( 1, : ), &
                                                           list( triplet( 3 ) )%p_Centre ) ) )

                    else if ( ( .not. intersec2 ) .and. ( .not. out2 ) .and. &
                              ( ( nrj2 < nrjc ) .or. first ) .and. &
                              ( norm2 < ( candidate%p_Ray*tol ) ) .and. &
                              ( norm2 > 0.D0 ) ) then

                       ! affectation des nouvelles coordonnees pour la candidate
                       candidate%p_Centre = centre_sol( 2, : ) 
                       ! determination du triplet support
                       triplet( 1 ) = num( j )
                       triplet( 2 ) = list( num( j ) )%p_Verlet( k )
                       triplet( 3 ) = list( num( j ) )%p_Verlet( l )

                       ! infirmation du label de premier passage
                       first = .false.

                       ! calcul de l interpenetration
                       interpen_unit = max( 0.D0, ( candidate%p_Ray + list( triplet( 1 ) )%p_Ray - &
                                                    norme( centre_sol( 2, : ), &
                                                           list( triplet( 1 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 2 ) )%p_Ray - &
                                                    norme( centre_sol( 2, : ), &
                                                           list( triplet( 2 ) )%p_Centre ) ) ) + &
                                       max( 0.D0, ( candidate%p_Ray + list( triplet( 3 ) )%p_Ray - &
                                                    norme( centre_sol( 2, : ), &
                                                           list( triplet( 3 ) )%p_Centre ) ) )
                    end if
                 end if  ! END - if ( exist ) then
              end do  ! END - do l = k + 1, list( num( j ) )%p_VerNbr
           end do  ! END - do k = 1, list( num( j ) )%p_VerNbr - 1

           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! Cas d'une sphere seule 
           ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
           ! cette solution n'est envisageable que pour les 3 1eres billes
           if ( i_list .le. ( n_init + 3 ) ) then

              call random_number( tamp )

              ! angles aleatoires, entre 0 et 2pi radians (i.e. entre 0° et 360°) et 
              ! calcul d une solution potentielle
              theta = 2.d0 * PI_g * tamp

              call random_number( tamp )
              phi = 2.d0 * PI_g * tamp 

              centre_sol( 1, 1 ) = list( num( j ) )%p_Centre( 1 ) + &
                                   ( list( num( j ) )%p_Ray + candidate%p_Ray ) * &
                                   sin( phi ) * cos( theta )
              centre_sol( 1, 2 ) = list( num( j ) )%p_Centre( 2 ) + &
                                   ( list( num( j ) )%p_Ray + candidate%p_Ray ) * &
                                   sin( phi ) * sin( theta )

              centre_sol( 1, 3 ) = list( num( j ) )%p_Centre( 3 ) + &
                                   ( list( num( j ) )%p_Ray + candidate%p_Ray ) * cos( phi ) 

              !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - 
              ! Test des solutions sur une intersection possible avec les autres particules 
              !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - 
              ! initialisation du label d inter-penetration de la solution
              intersec1 = .false.

              ! boucle de parcours des voisins de part ref
              do m = 1, list( num( j ) )%p_VerNbr
                 ! distance inter-centre
                 norm1 = norme( list( list( num( j ) )%p_Verlet( m ) )%p_Centre, centre_sol( 1, : ) )

                 if ( norm1 < ( list( list( num( j ) )%p_Verlet( m ) )%p_Ray + &
                                   candidate%p_Ray ) ) then
                    ! validation du label d inter-penetration
                    intersec1 = .true.
                    ! sortie de la boucle
                    exit
                 end if

              end do

              !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - 
              ! Test de sortie des solutions 
              !   -   -   -   -   -   -   -   -   -   -   -   -   -   -   -   - 
              ! initialisation du label de sortie de la solution
              out1 = .false.

              if ( cont%Type == 'cyli' ) then 
                 ! projection (cf. 'tex_position')
                 k1 = ( cont%Cyli%c_Vector( 1 ) * &
                        ( centre_sol( 1, 1 ) - cont%Cyli%c_Centre( 1 ) ) + &
                        cont%Cyli%c_Vector( 2 ) * &
                        ( centre_sol( 1, 2 ) - cont%Cyli%c_Centre( 2 ) ) + &
                        cont%Cyli%c_Vector( 3 ) * &
                        ( centre_sol( 1, 3 ) - cont%Cyli%c_Centre( 3 ) ) ) / &
                      ( cont%Cyli%c_Vector( 1 )**2 + cont%Cyli%c_Vector( 2 ) **2 + &
                        cont%Cyli%c_Vector( 3 )**2 )

                 pt1( : ) = cont%Cyli%c_Centre( : ) + k1 * cont%Cyli%c_Vector( : )

                 ! calcul de la distance solution 1 / axe du cylindre
                 norm1 = norme( pt1, centre_sol( 1, : ) )
                 
                 if ( norm1 > ( cont%Cyli%c_Ray - candidate%p_Ray ) ) then 
                    out1=.true. !test de sortie de la solution 1
                 end if

              else if ( cont%Type == 'sphr' ) then 
                 ! calcul de la dist sol 1 / centre de la sphere
                 norm1 = norme( cont%Spher%s_Centre, centre_sol( 1, : ) )

                 if ( norm1 > ( cont%Spher%s_Ray - candidate%p_Ray ) ) then
                    out1=.true. !test de sortie de la solution 1
                 end if

              end if

              ! boucle de parcours des plans a prendre en compte
              do m = 1, i_box
                 ! cf. 'tex_position'
                 tamp = dsqrt( cont%Plan( m, 1 )**2.D0 + &
                               cont%Plan( m, 2 )**2.D0 + &
                               cont%Plan( m, 3 )**2.D0 )

                 pt1( 1 ) = centre_sol( 1, 1 ) + &
                            cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 1 ) / tamp
                 pt1( 2 ) = centre_sol( 1, 2 ) + &
                            cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 2 ) / tamp
                 pt1( 3 ) = centre_sol( 1, 3 ) + &
                            cont%Plan( m, 5 ) * candidate%p_Ray * cont%Plan( m, 3 ) / tamp

                 norm1 = cont%Plan( m, 1 ) * pt1( 1 ) + &
                         cont%Plan( m, 2 ) * pt1( 2 ) + &
                         cont%Plan( m, 3 ) * pt1( 3 ) + &
                         cont%Plan( m, 4 )

                 if ( norm1 * cont%Plan( m, 5 ) > 0.D0) then
                    ! validation du label de sortie de la solution
                    out1 = .true.
                    ! sortie de la boucle
                    exit
                 end if

              end do

              !   -    -   -   -   -   -   -   -   -   -   -   -   -   -   -   -
              ! Changement conditionelle de la solution finale 
              !   -    -   -   -   -   -   -   -   -   -   -   -   -   -   -   -

              ! calcul de l'energie potentielle de la solution 1
              nrj1 = energy( centre_sol( 1, : ) )
              ! calcul de l'energie potentielle de la candidate
              nrjc = energy( candidate%p_Centre )

              norm1 = norme( centre_sol( 1, : ), list( num( j ) )%p_Centre )
              ! distance inter-particulaire de la solution 1
              norm1 = norm1 - ( candidate%p_Ray + list( num( j ) )%p_Ray )

              ! cf. contact spher/plan/plan (+ haut ^)
              if ( ( .not. intersec1 ) .and. ( .not. out1 ) .and. &
                   ( ( nrj1 < nrjc ) .or. first ) .and. &
                   ( norm1 < ( candidate%p_Ray * tol ) ) .and. &
                   ( norm1 > 0.D0 ) ) then

                 ! affectation des nouvelles coordonnees pour la candidate
                 candidate%p_Centre = centre_sol( 1, : )

                 ! determination du triplet support
                 ! am: on initialise le triplet a 0
                 triplet = 0
                 ! am: le premier membre du triplet est la particule elle-meme
                 triplet( 1 ) = num( j )                                   

                 ! am: le deuxieme membre du triplet est la premiere particule
                 !    voisine, si elle existe
                 if ( list( num( j ) )%p_VerNbr >= 1) then
                    triplet( 2 ) = list( num( j ) )%p_Verlet( 1 )
                 end if

                 ! am: la troisieme membre du triplet est la deuxieme particule
                 !    voisine, si elle existe
                 if ( list( num( j ) )%p_VerNbr >= 2 ) then
                    triplet( 2 ) = list( num( j ) )%p_Verlet( 2 ) 
                 end if

                 ! infirmation du label de premier passage
                 first = .false.

                 ! calcul de l interpenetration
                 interpen_unit = max( 0.D0, ( candidate%p_Ray + list( triplet( 1 ) )%p_Ray - &
                                              norme( centre_sol( 1, : ), &
                                                     list( triplet( 1 ) )%p_Centre ) ) )
              end if
           end if  ! END - if ( i_list .le. ( n_init + 3 ) ) then

        end do  ! END - do j = 1, i_num

        ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        ! Clause d'arret critique 
        ! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        ! test de premier passage
        if ( first ) then
           if ( .not. no_iter ) then
              ! info utilisateur
              if( bavard ) then
                write( *, * ) 'Depot plante a :', i_list + 1
              end if
           end if

           ! validation du label de non iteration
           no_iter = .true.
           ! test d arret du depot si plus de 100 part sont deposes
           if ( ( i_list - n_init ) > 100 ) then
              exit
           end if

           cycle

        end if


        do j = 1, 3 
           !incrementation du nombre de contact
           if ( triplet( j ) .ne. 0 ) then
              list( triplet( j ) )%p_TactNbr = list( triplet( j ) )%p_TactNbr + 1
           end if

        end do

        ! actualisation du nombre de part reel
        i_list = i_list + 1

        ! affectation des caracteristiques de la nouvelle particule
        list( i_list ) = candidate

        if ( ( dsqrt( list( i_list )%p_Centre( 1 )**2. + list( i_list )%p_Centre( 2 ) **2. ) + &
               list( i_list )%p_Ray ) > R ) then 
           ! affectation d un materiau different pour la sur-couche (cyli)
           list( i_list )%p_Mat = 5
        end if

        ! lz update
        if ( lz < ( candidate%p_Centre( 3 ) + candidate%p_Ray ) ) then
           lz = candidate%p_Centre( 3 ) + candidate%p_Ray
        end if

        if( bavard ) then
          ! affichage du triplet support de la solution
          write( *, * ) 'Triplet:', triplet( 1 )
          write( *, * ) '       :', triplet( 2 )
          write( *, * ) '       :', triplet( 3 )
          ! affichage de la particule de reference
          write( *, * ) 'NbCheck:', i_num
          ! affichage du rayon
          write( *, * ) 'Ray    :', list( i_list )%p_Ray
          ! affichage des coordonnees du centrede la solution
          write( *, * ) 'Centre :', list( i_list )%p_Centre( 1 )
          write( *, * ) '       :', list( i_list )%p_Centre( 2 )
          write( *, * ) '       :', list( i_list )%p_Centre( 3 )
        end if

        ! infirmation du label de non iteration 
        no_iter = .false.
        ! boucle d actualisation des particules pouvant servir de particule de reference
        do j = 1, i_list
           ! le nbr de contact d une particule est plus grand que 7
           condition1 = ( list( j )%p_TactNbr .ge. 8 )
           ! la particule ne fait pas partie des 'n_actif' dernieres
           condition2 = ( j < ( i_list - n_actif ) )

           ! si l une ou l autre de ses conditions est vraie
           if ( condition1 .or. condition2 ) then
              ! la particule ne pourra pas etre une part de reference
              list( j )%p_Flag = 0
           end if
        end do

     end do  ! END - do i = i_list + 1, n_body

     !- ------------------------------------------------------------------------
     ! Calcul de l'interpenetration totale
     !- ------------------------------------------------------------------------
     ! initialisation de l'inter-penetration totale
     interpen = 0.D0
     do i = 1, i_list
        do j = i + 1, i_list
           ! calcul de l inter-penetration unitaire et actualisation de 
           ! l inter-penetration totale
           interpen_unit = max( 0.D0, ( list( i )%p_Ray + list( j )%p_Ray - &
                                        norme( list( i )%p_Centre, list( j )%p_Centre ) ) )

           interpen = interpen + interpen_unit

           ! marquage des particules en inter-penetration
           if ( interpen_unit > 0.D0 ) then
              list( i )%p_Mat = 2
              list( j )%p_Mat = 2
              ! information utilisateur
              if( bavard ) then
                write( *, * ) 'Intrpnt:', i, j
              end if
           end if
        end do
     end do

     ! information utilisateur 
     if( bavard ) then
       write( *, * ) 'Intrpnt:', interpen
     end if

   end subroutine depot_

   !============================================================================================================ 
   !                      Fonction energy 
   ! 
   ! renvoie une energy en fonction d'une position spaciale (cette fonction peut bien entendu etre modifiee
   ! selon le mode de depot que l on souhaite avoir)
   !============================================================================================================ 
   !> this function computes the enregy of a particle, depending on tis position
   function energy(centre) 
    
      implicit none

      ! variable d'entree :
      real(kind=8), dimension(3), intent(in) :: centre !< position of the given particle
         !position
      ! valeur de retour :
      real(kind=8) :: energy !< energy of the particle

      energy=centre(3) !ici l energie est proportionelle a l altitude selon z
     
   end function energy 

   !============================================================================
   !                      Subroutine first_point 
   ! 
   ! definie une particule 'this' dans un contenant vide
   !============================================================================
   !> this function puts a first particle in a given empty container
   subroutine first_point( cont, this, ray ) 

     implicit none

     ! variables d'entree :
     ! contenant
     type( contenant ), intent( in ) :: cont !< empty container                          
     ! rayon de la particule
     real( kind = 8 ) , intent( in ) :: ray  !< radius of the particle to deposit

     ! variabes de sortie :
     ! info particule
     type( part ), intent( out ) :: this !< caracteristics of the deposited particle

     real( kind = 8 ), dimension( 3 ) :: pt1,pt2,pt3,pt4               ! points
     real( kind = 8 ), dimension( 5 ) :: plan1,plan3,plan4,plan5,plan6 ! plans
     real( kind = 8 )                 :: tamp                          ! reel tampon
     real( kind = 8 )                 :: theta,phi                     ! angles aleatoires

     ! resserage du contenant par un decalage 'ray' des plans
     plan1( : ) = cont%Plan( 1, : )
     tamp       = ( cont%Plan( 1, 1 )**2. + cont%Plan( 1, 2 )**2. + cont%Plan( 1, 3 )**2. )**0.5
     plan1( 4 ) = plan1( 4 ) + ray * cont%Plan( 1, 5 ) * tamp

     plan3( : ) = cont%Plan( 3, : )
     tamp       = ( cont%Plan( 3, 1 )**2. + cont%Plan( 3, 2 )**2. + cont%Plan( 3, 3 )**2. )**0.5
     plan3( 4 ) = plan3( 4 ) + ray * cont%Plan( 3, 5 ) * tamp

     plan4( : ) = cont%Plan( 4, : )
     tamp       = ( cont%Plan( 4, 1 )**2. + cont%Plan( 4, 2 )**2. + cont%Plan( 4, 3 )**2. )**0.5
     plan4( 4 ) = plan4( 4 ) + ray * cont%Plan( 4, 5 ) * tamp

     plan5( : ) = cont%Plan( 5, : )
     tamp       = ( cont%Plan( 5, 1 )**2. + cont%Plan( 5, 2 )**2. + cont%Plan( 5, 3 )**2. )**0.5
     plan5( 4 ) = plan5( 4 ) + ray * cont%Plan( 5, 5 ) * tamp

     plan6( : ) = cont%Plan( 6, : )
     tamp       = ( cont%Plan( 6, 1 )**2. + cont%Plan( 6, 2 )**2. + cont%Plan( 6, 3 )**2. )**0.5
     plan6( 4 ) = plan6( 4 ) + ray * cont%Plan( 6, 5 ) * tamp

     ! cas de la boite :
     if ( cont%Type == 'box_' ) then
        ! definition des 4 pts d intersection du 'plan1'
        call intersec3plans( plan1, plan3, plan4, pt1 )
        call intersec3plans( plan1, plan3, plan5, pt2 )
        call intersec3plans( plan1, plan6, plan4, pt3 )
        call intersec3plans( plan1, plan6, plan5, pt4 )
        ! nombre aleatoire compris entre 0 et 1
        call random_number( tamp )
        ! definition d un pt situe entre 'pt1' et 'pt2'
        pt1( : ) = pt1( : ) * ( 1 - tamp ) + pt2( : ) * tamp
        ! nombre aleatoire compris entre 0 et 1
        call random_number( tamp )
        ! definition d un pt situe entre 'pt3' et 'pt4'
        pt2( : ) = pt3( : ) * ( 1 - tamp ) + pt4( : ) * tamp
        ! nombre aleatoire compris entre 0 et 1
        call random_number( tamp )
        ! definition d un pt situe entre 'pt1' et 'pt2'
        this%p_Centre( : ) = pt1( : ) * ( 1 - tamp ) + pt2( : ) * tamp

        ! affectation du nombre de contact de cette particule
        this%p_TactNbr = 1

        ! cas du cylindre :
     else if ( cont%Type == 'cyli' ) then 
        ! definition pt d intersection
        call intersec_plan_droite( plan1, cont%Cyli%c_Centre, cont%Cyli%c_Vector, pt1 )

        ! pt aleatoire dans un disque
        call random_plan_circ( cont%Plan( 1, : ), cont%Cyli%c_Ray - ray, pt1, this%p_Centre )

        ! affectation du nombre de contact de cette particule
        this%p_TactNbr = 1

        ! cas de la sphere :
     else if ( cont%Type == 'sphr' ) then 

        ! angle aleatoire compris entre 0 et 2pi radians (i.e. 0° et 360°)
        call random_number( tamp )
        theta = 2.d0 * PI_g * tamp

        ! angle aleatoire compris entre 0 et pi/4 radians (i.e. entre 0° et 45°)
        call random_number( tamp )                        
        phi = 0.25d0 * PI_g * tamp

        tamp = cont%Spher%s_Ray - ray 

        ! pt aleatoire dans une sphere
        this%p_Centre( 1 ) = cont%Spher%s_Centre( 1 ) - tamp * sin( phi ) * cos( theta )
        this%p_Centre( 2 ) = cont%Spher%s_Centre( 2 ) - tamp * sin( phi ) * sin( theta )
        this%p_Centre( 3 ) = cont%Spher%s_Centre( 3 ) - tamp * cos( phi )

        ! affectation du nombre de contact de cette particule
        this%p_TactNbr = 0
     else

     endif

     ! affectation de ses caracteristiques
     this%p_Ray         = ray
     this%p_Flag        = 1
     this%p_Mat         = 0
     this%p_VerNbr      = 0
     this%p_Verlet( : ) = 0

   end subroutine first_point

   !============================================================================================================ 
   !                      Subroutine zero_part 
   ! 
   ! mise a zero d'une particule 'this' 
   !============================================================================================================ 
   !> this function nullifies a given particle
   subroutine zero_part(this) 
    
      implicit none

      ! variable de sortie :
      type (part), intent(out) :: this !< the given particle
         !liste d information de la particule a mettre a zero
    
     this%p_Flag=1      !'this' peut etre une ref
     this%p_TactNbr=0   !pas de contact
     this%p_Verlet(:)=0 !pas de reference voisin
     this%p_Centre(:)=0 !centree
     this%p_Ray=0       !pas de rayon
     this%p_VerNbr=0    !pas de voisins
     this%p_Mat=0       !pas de materiau
    
   end subroutine zero_part 

   !============================================================================================================ 
   !                      Subroutine update_num 
   ! 
   ! Mise a jour de la table des part de reference 'num' et de son nbr d'elements 'i_num' 
   !============================================================================================================ 
   !> this function updates the list of interesting (freshly deposited) particles in a given list of particle
   subroutine update_num(i_list,list,n_body,i_num,num,maxsz) 
    
      implicit none

      ! variables d'entree :
      !> total number of deposited particles 
      integer, intent(in)                        :: i_list 
      !nombre de particules a tester
      !> maximal number of particles, i.e. size of list
      integer, intent(in)                        :: n_body
      !nombre max de particules
      !> maximal number of intersting (freshly deposited) particles
      integer, intent(in)                        :: maxsz
      !tableau des particules a tester
      !> the given list of particles
      type (part), dimension(n_body), intent(in) :: list
      

      ! variables de sortie :
      ! nombre de part de references
      !>number of interesting (freshly deposited) particles
      integer, intent(out)                 :: i_num 
      !tableau des numeros des part de references
      !> indices of the interesting (freshly deposited) particles in list 
      integer, dimension(maxsz), intent(out) :: num 
  
      ! variables locales :
      !compteur incremental
      integer                       :: i       
     
      !------------------------------------------------------------------------------------------------------------ 
      ! Initialisation du nombre de part de references et m.a.z du tableau de part de references
      !------------------------------------------------------------------------------------------------------------ 
      i_num=0 
      do i=1,maxsz 
         num(i)=0 
      enddo 
      !------------------------------------------------------------------------------------------------------------ 
      ! Boucle de test des part de references
      !------------------------------------------------------------------------------------------------------------ 
      do i=1,i_list 
         if (list(i)%p_Flag.ne.0) then   !test
            i_num=i_num+1                !incrementation du nombre de part ref
            if (i_num > maxsz) then
              print*,'size of num array is reached '
              print*,'you should increase n_actif which is equal to ',maxsz-10 
              stop
            endif  
            num(i_num)=i                 !ajout de la nouvelle part ref
         end if 
      end do 
    
   end subroutine update_num 
    
   !============================================================================================================ 
   !                      Subroutine update_verlet 
   ! 
   ! reactualise la liste de voisin (p_Verlet,n_verlet) de la liste de 'n' 
   ! particules 'list' en fonction de l'ajout d'une nouvelle particule 'candidate' 
   !============================================================================================================ 
   !> this function updates the list of neighbours of some intersting (freshly deposited) particles 
   !> in a given list of particles; the neighborood of a particle is caracterized by a given radius
   subroutine update_verlet(l,list,n,num,maxsz,ray,n_verlet) 
     
     implicit none

     ! variables d'entree :
     !> number of particles in the list
     integer, intent(in)              :: l 
     !> number of interesting (freshly deposited) particles, i.e. less than size of num       
     integer, intent(in)              :: n 
     !> size of num       
     integer, intent(in)              :: maxsz
     !> indices of interesting (freshly deposited) particles     
     integer,dimension(maxsz), intent(in) :: num 
     !> radius used to caracterize the neighborhood of a particle
     real(kind=8), intent(in)         :: ray 
     !> maximal number of neighbors for a particle
     integer, intent(in)              :: n_verlet 
 
     ! variables d'entree-sortie :
     !> list of particles
     type (part),dimension(l)    :: list 

     ! variables locales :
     integer                     :: i,j,k    !compteurs incrementaux
     real(kind=8)                :: norm_ver !distance de recherche des voisins
     real(kind=8)                :: norm_can !distance test
    
     do i=1,n                              !boucle sur les elements valides de la 'list'
       k=0                                 !initialisation du compteur 'k'
       do j=1,n_verlet                     !maz des references voisins
         list(num(i))%p_Verlet(j)=0 
       enddo 
       do j=1,l                            !boucle sur les elements de la 'list'
         if (j.ne.num(i)) then             !une particule ne peut pas etre voisine d elle meme
           norm_can=norme(list(num(i))%p_Centre,list(j)%p_Centre) !calcul de la distance test
           norm_ver=list(num(i))%p_Ray+list(j)%p_Ray+2*ray !calcul de la distance de recherche
           if (norm_can.lt.norm_ver) then  !test d appartenance a la liste des voisins
             k=k+1                         !incrementation du compteur de voisins
             if (k > n_verlet) then
               print*,'size of verlet array is reached '
               print*,'you should increase n_verlet which is equal to ',n_verlet
               stop 
             endif 
             list(num(i))%p_Verlet(k)=j    !ajout du numero du voisin
           endif 
         endif 
       enddo 
       list(num(i))%p_VerNbr=k             !affectation du nombre total de voisin pour cette particule
     enddo 
    
   end subroutine update_verlet 

   !============================================================================================================ 
   !                      Subroutine intersec3plans 
   ! 
   ! permet de trouver les coordonnees d un point 'pt' intersection de trois plans 'plan1', 'plan2', 'plan3'
   !============================================================================================================ 
   !> this function compute the intersection point of three planes
   subroutine intersec3plans(plan1,plan2,plan3,pt) 
    
      implicit none

      ! variables d'entree :
      real(kind=8), dimension(5), intent(in) :: plan1 !< first plane
      real(kind=8), dimension(5), intent(in) :: plan2 !< second plane
      real(kind=8), dimension(5), intent(in) :: plan3 !< third plane

      real(kind=8), dimension(3), intent(out) :: pt !< intersection point
         !point solution
 
      ! variables locales :
      real(kind=8), dimension(2) :: GA,GB,GC          !coef des 2 equations reduites
      real(kind=8), dimension(3) :: a,b,c,d           !coef des 3 equations de base
     
      a(1)=plan1(1)                        !affectation des coef des 3 equations de base
      b(1)=plan1(2) 
      c(1)=plan1(3) 
      d(1)=plan1(4) 
      a(2)=plan2(1) 
      b(2)=plan2(2) 
      c(2)=plan2(3) 
      d(2)=plan2(4) 
      a(3)=plan3(1) 
      b(3)=plan3(2) 
      c(3)=plan3(3) 
      d(3)=plan3(4) 
      ! resolution dans trois cas particuliers
      if (c(3).ne.0.D0) then               !cas 1
         GA(1)=a(1)*c(3)-a(3)*c(1)          !affectation des coef des 2 equations reduites
         GB(1)=b(1)*c(3)-b(3)*c(1) 
         GC(1)=d(1)*c(3)-d(3)*c(1) 
         GA(2)=a(2)*c(3)-a(3)*c(2) 
         GB(2)=b(2)*c(3)-b(3)*c(2) 
         GC(2)=d(2)*c(3)-d(3)*c(2) 
         if (GB(2).ne.0.D0) then            !cas 1.1
            pt(1)=-(GC(1)*GB(2)-GC(2)*GB(1))/(GA(1)*GB(2)-GA(2)*GB(1)) 
            pt(2)=-(GA(2)*pt(1)+GC(2))/GB(2) 
         elseif (GA(2).ne.0.D0) then        !cas 1.2
            pt(2)=-(GC(1)*GA(2)-GC(2)*GA(1))/(GB(1)*GA(2)-GB(2)*GA(1)) 
            pt(1)=-(GB(2)*pt(2)+GC(2))/GA(2) 
         else 
            write(*,*)'Error' 
         endif 
         pt(3)=-(d(3)+a(3)*pt(1)+b(3)*pt(2))/c(3) 
      elseif (b(3).ne.0.D0) then           !cas 2
         GA(1)=a(1)*b(3)-a(3)*b(1)          !affectation des coef des 2 equations reduites
         GB(1)=c(1)*b(3)-c(3)*b(1) 
         GC(1)=d(1)*b(3)-d(3)*b(1) 
         GA(2)=a(2)*b(3)-a(3)*b(2) 
         GB(2)=c(2)*b(3)-c(3)*b(2) 
         GC(2)=d(2)*b(3)-d(3)*b(2) 
         if (GB(2).ne.0.D0) then            !cas 2.1
            pt(1)=-(GC(1)*GB(2)-GC(2)*GB(1))/(GA(1)*GB(2)-GA(2)*GB(1)) 
            pt(3)=-(GA(2)*pt(1)+GC(2))/GB(2) 
         elseif (GA(2).ne.0.D0) then        !cas 2.2
            pt(3)=-(GC(1)*GA(2)-GC(2)*GA(1))/(GB(1)*GA(2)-GB(2)*GA(1)) 
            pt(1)=-(GB(2)*pt(3)+GC(2))/GA(2)
         else 
            write(*,*)'Error' 
         endif 
         pt(2)=-(d(3)+a(3)*pt(1)+c(3)*pt(3))/b(3) 
      elseif (a(3).ne.0.D0) then           !cas 3
         GA(1)=c(1)*a(3)-c(3)*a(1)          !affectation des coef des 2 equations reduites
         GB(1)=b(1)*a(3)-b(3)*a(1) 
         GC(1)=d(1)*a(3)-d(3)*a(1) 
         GA(2)=c(2)*a(3)-c(3)*a(2) 
         GB(2)=b(2)*a(3)-b(3)*a(2) 
         GC(2)=d(2)*a(3)-d(3)*a(2) 
         if (GB(2).ne.0.D0) then            !cas 3.1
            pt(3)=-(GC(1)*GB(2)-GC(2)*GB(1))/(GA(1)*GB(2)-GA(2)*GB(1)) 
            pt(2)=-(GA(2)*pt(3)+GC(2))/GB(2)
         elseif (GA(2).ne.0.D0) then        !cas 3.2
            pt(2)=-(GC(1)*GA(2)-GC(2)*GA(1))/(GB(1)*GA(2)-GB(2)*GA(1)) 
            pt(3)=-(GB(2)*pt(2)+GC(2))/GA(2)
         else 
            write(*,*)'Error' 
         endif 
         pt(1)=-(d(3)+b(3)*pt(2)+c(3)*pt(3))/a(3) 
      else 
         write(*,*)'Error' 
      endif 

   end subroutine intersec3plans 

   !============================================================================================================ 
   !                      Subroutine intersec_plan_droite 
   ! 
   ! permet de trouver les coordonnees d un point 'pt' intersection d un plan 'plan' et d une droite representee
   ! par un centre et un vecteur directeur respectivement 'ctr' et 'vct'
   !============================================================================================================ 
   !> this function computes the intersection between a plane and a line
   subroutine intersec_plan_droite(plan,ctr,vct,pt) 

      implicit none
    
      ! variables d'entree :
      real(kind=8), dimension(5), intent(in) :: plan !< the given plane
      real(kind=8), dimension(3), intent(in) :: ctr !< coordinates of a point on the given line
         !coordonnees d un point de la droite
      real(kind=8), dimension(3), intent(in) :: vct !< vecteur directeur of the line
         !vecteur directeur unitaire de la droite

      ! variable de sortie :
      real(kind=8), dimension(3), intent(out) :: pt !< intersection point
         !point d intersection

      ! variables locales :
      real(kind=8)                :: k,a,b,c,d  !coef tampons
       
      a=plan(1)                                       !reecriture des coef de l equation de plan
      b=plan(2) 
      c=plan(3) 
      d=plan(4) 
      if ((a*vct(1)+b*vct(2)+c*vct(3)).eq.0.D0) then  !cas particulier
          write(*,*)'Error' 
      endif 
      k=-(a*ctr(1)+b*ctr(2)+c*ctr(3)+d)/(a*vct(1)+b*vct(2)+c*vct(3)) !calcul de la distance 'ctr' 'pt'
      pt(1)=ctr(1)+k*vct(1)                           !affectation de 'pt'
      pt(2)=ctr(2)+k*vct(2) 
      pt(3)=ctr(3)+k*vct(3) 
    
   end subroutine intersec_plan_droite 

   !============================================================================================================ 
   !                      Subroutine random_plan_circ 
   ! 
   ! Defini un point aleatoire 'pt' dans un disque de rayon 'R' et de centre 'ctr' // au 'plan'
   !============================================================================================================ 
   !> this function computes random coordinates for a point in a disk, caracterized by a plane, a center and a radius
   subroutine random_plan_circ(plan,R,ctr,pt) 
    
      implicit none

      ! variables d'entree :
      real(kind=8),dimension(5) :: plan !< given plane
         !plan
      real(kind=8)              :: R !< radius of the given disk
         !rayon maxi de recherche
      real(kind=8),dimension(3) :: ctr !< coordinates of the center of the given disk      
         !centre du repere

      ! variables de sortie :
      real(kind=8),dimension(3) :: pt !< intersection point
         !pt solution

      ! variables locales :
      real(kind=8)              :: n         !norme
      real(kind=8)              :: ray       !distance aleatoire
      real(kind=8)              :: theta     !angle aleatoire
      real(kind=8),dimension(3) :: ex,ey,ez  !base orthonormee attachee au plan
      real(kind=8)              :: tamp      !variable tampon
     
      ez(1)=-plan(5)*plan(1)/dsqrt(plan(1)**2.D0+plan(2)**2.D0+plan(3)**2.D0) !calcul du vecteur 'ez' de la base
      ez(2)=-plan(5)*plan(2)/dsqrt(plan(1)**2.D0+plan(2)**2.D0+plan(3)**2.D0) 
      ez(3)=-plan(5)*plan(3)/dsqrt(plan(1)**2.D0+plan(2)**2.D0+plan(3)**2.D0) 
      if (plan(3).ne.0.D0) then                              !cas 1 :
         n=dsqrt(2.D0+((plan(1)+plan(2))/plan(3))**2.D0)        !norme
         ex(1)=1/n                                              !calcul du vecteur 'ex' de la base
         ex(2)=1/n 
         ex(3)=-(plan(1)+plan(2))/(n*plan(3)) 
      elseif (plan(2).ne.0.D0) then                          !cas 2 :
         n=dsqrt(2.D0+((plan(1)+plan(3))/plan(2))**2.D0)        !norme
         ex(1)=1/n                                              !calcul du vecteur 'ex' de la base
         ex(2)=-(plan(1)+plan(3))/(n*plan(2)) 
         ex(3)=1/n 
      elseif (plan(1).ne.0.D0) then                          !cas 3 :
         n=dsqrt(2.D0+((plan(2)+plan(3))/plan(1))**2.D0)        !norme
         ex(1)=-(plan(2)+plan(3))/(n*plan(1))                   !calcul du vecteur 'ex' de la base
         ex(2)=1/n 
         ex(3)=1/n 
      endif 
      ey(1)=ez(2)*ex(3)-ez(3)*ex(2)                          !calcul du vecteur 'ey' de la base
      ey(2)=ez(3)*ex(1)-ez(1)*ex(3) 
      ey(3)=ez(1)*ex(2)-ez(2)*ex(1) 
     
      call random_number(tamp)                                   
      theta=PI_g*(2.d0*tamp - 1.d0)                            !angle aleatoire compris entre -pi et pi radians (i.e. -180° et 180°)
      call random_number(tamp)                                   
      ray=R*tamp                                             !distance aleatoire comprise entre 0 er 'R'
      pt(1)=ctr(1)+ray*(cos(theta)*ex(1)+sin(theta)*ey(1))   !calcul du pt solution
      pt(2)=ctr(2)+ray*(cos(theta)*ex(2)+sin(theta)*ey(2)) 
      pt(3)=ctr(3)+ray*(cos(theta)*ex(3)+sin(theta)*ey(3)) 
    
   end subroutine random_plan_circ 

   !============================================================================
   !                      Subroutine eq_plan 
   ! 
   ! Affecte les parametres de resolution de contact avec un plan X Y Z W
   !============================================================================
   !> this function computes parameters (X, Y, Z, W) needed to solve a contact involving a plane
   subroutine eq_plan( X, Y, Z, W, centre1, ray1, plan, ray2 ) 

      implicit none

      ! variables d'entree :
      ! centre de particule de reference
      real(kind=8), dimension(3), intent(in) :: centre1 !< position of the given interesting (freshly deposited) particle

      ! plan (second contact)
      real(kind=8), dimension(5), intent(in) :: plan !< the given plane            

      ! rayon de la particule de reference
      real(kind=8), intent(in)               :: ray1 !< radius of the given interesting (freshly deposited) particle

      ! rayon de la particule candidate
      real(kind=8), intent(in)               :: ray2 !< radius of the candidate particle

      ! variables de sortie :
      real( kind = 8 ), intent( out ) :: X !< first coefficient used to solve the problem of contact
      real( kind = 8 ), intent( out ) :: Y !< second coefficient used to solve the problem of contact
      real( kind = 8 ), intent( out ) :: Z !< third coefficient used to solve the problem of contact 
      real( kind = 8 ), intent( out ) :: W !< fourth coefficient used to solve the problem of contact

      X = plan( 1 ) * ( ray1 + ray2 ) 
      Y = plan( 2 ) * ( ray1 + ray2 ) 
      Z = plan( 3 ) * ( ray1 + ray2 ) 
      W = plan( 5 ) * dsqrt( plan( 1 )**2.D0 + plan( 2 )**2.D0 + plan( 3 )**2.D0 ) * ray2 + &
           plan( 1 ) * centre1( 1 ) + plan( 2 ) * centre1( 2 ) + plan( 3 ) * centre1( 3 ) + &
           plan( 4 ) 
    
   end subroutine eq_plan 

   !============================================================================================================ 
   !                      Subroutine eq_spher 
   ! 
   ! Affecte les parametres de resolution de contact avec une sphere X Y Z W 
   !============================================================================================================ 
   !> this function computes parameters (X, Y, Z, W) needed to solve a contact involving a sphere
   subroutine eq_spher(X,Y,Z,W,centre1,ray1,centre2,ray2,ray3) 

      implicit none
    
      real(kind=8), dimension(3), intent(in) :: centre1  !< position of the given interesting (freshly deposited) particle
         ! centre de la particule de reference
      real(kind=8), dimension(3), intent(in) :: centre2  !< position of the particle of the second contact
         ! centre de la particule du second contact 
      real(kind=8), intent(in)               :: ray1 !< radius of the the given interesting (freshly deposited) particle       
         !rayon de la particule de reference
      real(kind=8), intent(in)               :: ray2 !< radius of the particle of the second contact        
         !rayon de la particule du second contact
      real(kind=8), intent(in)               :: ray3 !< radius of the candidate particle            
         !rayon de la particule candidate

      ! variables de sortie :
      real(kind=8), intent(out)              :: X !< first coefficient used to solve the problem of contact
      real(kind=8), intent(out)              :: Y !< second coefficient used to solve the problem of contact
      real(kind=8), intent(out)              :: Z !< third coefficient used to solve the problem of contact 
      real(kind=8), intent(out)              :: W !< fourth coefficient used to solve the problem of contact
     
      X=centre1(1)-centre2(1) 
      Y=centre1(2)-centre2(2) 
      Z=centre1(3)-centre2(3) 
      W=(X**2.D0+Y**2.D0+Z**2.D0+(ray1+ray3)**2.D0-(ray2+ray3)**2.D0)/(2.D0* & 
        (ray1+ray3)) 
    
   end subroutine eq_spher 

   !============================================================================
   !                      Subroutine solve_tact 
   ! 
   ! Trouve les deux solutions possibles ou non du probleme de depot
   !============================================================================
   !> this function computes the two possible solutions of the contact problem, if they exist
   subroutine solve_tact( X, Y, Z, W, sol, exist ) 
    
      implicit none

      ! variables d'entree :
      real(kind=8), dimension(2), intent(in) :: X !< first coefficient used to solve the problem of contact
      real(kind=8), dimension(2), intent(in) :: Y !< second coefficient used to solve the problem of contact
      real(kind=8), dimension(2), intent(in) :: Z !< third coefficient used to solve the problem of contact 
      real(kind=8), dimension(2), intent(in) :: W !< fourth coefficient used to solve the problem of contact

      ! variables de sortie :
      real(kind=8), dimension(2,3), intent(out) :: sol !< the two solutions, defined only if they exist     
         !solution directionelle unitaire
      logical, intent(out)                      :: exist !< flag indicating existence (or not) of solution for the problem   
         !label d existance de solution

      ! variables locales :
      real(kind=8)                :: A,B,C,D  !coef de l equation du second degres
      real(kind=8)                :: delta    !discriminant
     
      if (X(1)*Y(2)-X(2)*Y(1)/=0.D0) then              !cas1
         A=(X(2)*Z(1)-X(1)*Z(2))/(X(1)*Y(2)-X(2)*Y(1))    !affectation des coef de l equation du second degres
         B=(X(2)*W(1)-X(1)*W(2))/(X(1)*Y(2)-X(2)*Y(1)) 
         C=(Y(1)*Z(2)-Y(2)*Z(1))/(X(1)*Y(2)-X(2)*Y(1)) 
         D=(Y(1)*W(2)-Y(2)*W(1))/(X(1)*Y(2)-X(2)*Y(1)) 
         delta=4.D0*(1.D0+A**2.D0-B**2.D0+C**2.D0-D**2.D0-(A*D-C*B)**2.D0) !calcul du discriminant
         if (delta .lt. 0.D0) then                        !test d existance
            exist=.false. 
         else 
            exist=.true. 
            sol(1,3)=(-2.D0*(A*B-C*D)+dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) !affectation de la solution
            sol(1,1)=C*sol(1,3)+D 
            sol(1,2)=A*sol(1,3)+B 
            sol(2,3)=(-2.D0*(A*B-C*D)-dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) 
            sol(2,1)=C*sol(2,3)+D 
            sol(2,2)=A*sol(2,3)+B 
         end if 
      else if (X(1)*Z(2)-X(2)*Z(1)/=0.D0) then          !cas2
         A=(X(2)*Y(1)-X(1)*Y(2))/(X(1)*Z(2)-X(2)*Z(1))    !affectation des coef de l equation du second degres
         B=(X(2)*W(1)-X(1)*W(2))/(X(1)*Z(2)-X(2)*Z(1)) 
         C=(Z(1)*Y(2)-Z(2)*Y(1))/(X(1)*Z(2)-X(2)*Z(1)) 
         D=(Z(1)*W(2)-Z(2)*W(1))/(X(1)*Z(2)-X(2)*Z(1)) 
         delta=4.D0*(1.D0+A**2.D0-B**2.D0+C**2.D0-D**2.D0-(A*D-C*B)**2.D0) !calcul du discriminant
         if (delta .lt. 0.D0) then                        !test d existance
            exist=.false. 
         else 
            exist=.true. 
            sol(1,2)=(-2.D0*(A*B-C*D)+dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) !affectation de la solution
            sol(1,1)=C*sol(1,2)+D
            sol(1,3)=A*sol(1,2)+B
            sol(2,2)=(-2.D0*(A*B-C*D)-dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) 
            sol(2,1)=C*sol(2,2)+D
            sol(2,3)=A*sol(2,2)+B
         end if 
      else if (Y(1)*Z(2)-Y(2)*Z(1)/=0.D0) then          !cas3
         ! affectation des coef de l equation du second degres
         A=(Y(2)*X(1)-Y(1)*X(2))/(Y(1)*Z(2)-Y(2)*Z(1))
         B=(Y(2)*W(1)-Y(1)*W(2))/(Y(1)*Z(2)-Y(2)*Z(1)) 
         C=(Z(1)*X(2)-Z(2)*X(1))/(Y(1)*Z(2)-Y(2)*Z(1)) 
         D=(Z(1)*W(2)-Z(2)*W(1))/(Y(1)*Z(2)-Y(2)*Z(1))
         ! calcul du discriminant
         delta = 4.D0 * ( 1.D0 + A**2.D0 - B**2.D0 + C**2.D0 - D**2.D0 - ( A * D - C * B )**2.D0 )
         if (delta .lt. 0.D0) then                        !test d existance
            exist=.false. 
         else 
            exist=.true. 
            sol(1,1)=(-2.D0*(A*B-C*D)+dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) !affectation de la solution
            sol(1,2)=C*sol(1,1)+D
            sol(1,3)=A*sol(1,1)+B
            sol(2,1)=(-2.D0*(A*B-C*D)-dsqrt(delta))/(2.D0*(1+A**2.D0+C**2.D0)) 
            sol(2,2)=C*sol(2,1)+D
            sol(2,3)=A*sol(2,1)+B
         end if 
      else 
         exist=.false. 
      endif 
    
   end subroutine solve_tact 

   !============================================================================================================ 
   !                      Fonction norme 
   ! 
   ! Renvoie la norme cartesienne entre deux points 
   !============================================================================================================ 
   !> this function computes the euclidian distance between two points
   function norme(pt1,pt2) 

      implicit none

      ! variables d'entree : 
      real(kind=8), dimension(3), intent(in) :: pt1 !< first point
      real(kind=8), dimension(3), intent(in) :: pt2 !< second point
 
      ! valeur de retour :
      real(kind=8) :: norme !< euclidian distance between the two given points

      norme=dsqrt((pt1(1)-pt2(1))**2.D0+(pt1(2)-pt2(2))**2.D0+(pt1(3)-pt2(3))**2.D0) !calcul de la norme
 
   end function norme 
 
end module deposit3D 
