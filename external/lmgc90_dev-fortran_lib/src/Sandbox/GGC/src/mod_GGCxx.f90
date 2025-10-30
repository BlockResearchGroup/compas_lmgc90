
!fd a voir: ne marche qu'avec des maillages a 1 type d'element
!fd 


!> module de couplage gaz-grains :
!>  - les grains sont des DISKx
!>  - l'evolution de la pression du gaz est calculee en utilisant
!>    le module de thermique et des fields adaptes
module gas_grains_coupling
   ! importation du module d'utilitaires de LMGC90 (pour disposer
   ! de la routine de gestion des erreurs fatales)
   use utilities
   ! importation du module stockant les donnees communes de LMG90
   ! (pour acceder a la dimension spatiale)
   use overall
   ! importation du module permettant d'utiliser les routines
   ! de projection de DD
   use projection
   ! importation des modules pour la gestion du maillage utilise
   ! pour la thermique
   use therMAILx, only : get_nb_therMAILx, &
                         get_nb_nodes_therMAILx => get_nb_nodes, &
                         get_nb_ele_therMAILx, &
                         only_one_elem_type_therMAILx, &
                         get_N_NODE_therMAILx, &
                         get_T_FONC_FORME_therMAILx, &
                         get_conn_therMAILx, &
                         get_cooref_node_therMAILx, &
                         compute_element_volume_therMAILx, &
                         element_volume_by_node_therMAILx, &
                         compute_elementary_F_Biot_therMAILx, &
                         compute_elementary_F_advection_therMAILx, &
                         compute_grad_T_ele_therMAILx, &
                         get_field_rank_therMAILx, &
                         set_field_bynode, &
                         add_external_flux_ele_therMAILx, &
                         add_theta_external_flux_ele_therMAILx, &
                         get_Tbegin_nodty_therMAILx, &
                         get_T_nodty_therMAILx
   ! importation des modules pour acceder aux grains
   ! hypothese forte : on ne considere que des disques!
   use DISKx, only : get_DISKx2RBDY2, &
                     get_nb_diskx, &
                     get_min_radius_DISKx, &
                     get_mean_radius_DISKx, &
                     get_max_radius_DISKx, &
                     get_radius_DISKx
   use RBDY2, only : increment_RBDY2, &
                     comp_dof_RBDY2, &
                     get_vector_RBDY2, &
                     put_vector_RBDY2, &
                     get_area, &
                     get_mass_RBDY2 => get_mass 

   ! TEST timer:
   use timer, only : is_started_timer, start_timer, stop_timer

   implicit none
   ! tout est prive, par defaut
   private
   ! declaration de la structure de donnee utilisee pour
   ! stocker le maillage, de façon a utiliser les routines de
   ! projection de DD
   type :: T_DD_mesh
         
      ! table de connectivite
      integer, dimension(:, :), pointer :: conn
           
      ! coordonnees des noeuds
      real(kind=8), dimension(:, :), pointer :: coor
         
   end type T_DD_mesh
         
   ! declaration de la structure de donnee utilisee pour
   ! rechercher, pour chaque grain, l'element le contenant
   type :: T_DD_search
       
      real(kind=8), dimension(:, :), pointer :: grains_coor ! stocke
                                                            ! les coordonnees des grains

      integer, dimension(:), pointer :: Elem ! stocke le numero de l'element dans lequel
                                             ! se trouve chaque grain (le centre d'inertie)
                                             ! Elem(iG) contient le numero de l'element dans 
                                             ! lequel se trouve le grain d'index iG
       
      real(kind=8), dimension(:, :), pointer :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, au centre d'inertie
                                          ! de chaque grain 
                                          ! Alpha(i, iG) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le grain d'index iG, au point x_iG
                                          ! i.e. N_i(x_iG)
            
   end type T_DD_search

   ! declaration de la structure de donnee utilisee pour realiser
   ! les projections a l'aide des routines de DD
   type :: T_DD_fields
       
      real(kind=8), dimension(:, :), pointer :: grains_fields
               ! pour stocker les champs definis sur les grains,
               ! a interpoler sur les noeuds:
               !  * grains_fields(iG, 1) : le volume du grain iG 
               !  * grains_fields(iG, 2) : le poids a appliquer pour calculer
               !                       la vitesse moyenne aux noeuds:
               !                          - 1.d0 : cas mono-disperse, pour
               !                                   un seul materiau
               !                          - volume du grain : cas polydisperse,
               !                                   pour un seul materiau
               !                          - masse du grain : cas general 
               !                                   (polydisperse, plusieurs
               !                                   materiaux)
               !  * grains_fields(iG, 3 : nbDIME + 2) : vecteur vitesse
               !                       du grain iG, pondere par le volume
               !                       du grain
      real(kind=8), dimension(:, :), pointer :: nodes_fields
               ! pour stocker les champs definis sur les noeuds,
               ! resultant de l'interpolation des champs definis
               ! sur les grains 
               !  * nodes_fields(iN, 1) : la somme des volumes des grains
               !                         voisins du noeuds, ponderes par
               !                         la valeur de la fonction de forme
               !                         du noeud iN aux centres de gravite
               !                         des grains
               !  * nodes_fields(iN, 2) : la somme des poids utilises pour
               !                         le calcul de la vitesse moyenne, ponderes
               !                         par le avaleur de la fonction de forme
               !                         au centre du gravite du noeud
               !  * nodes_fields(iN, 3 : nbDIME + 2) : la somme des vecteurs vitesse
               !                         des grains voisins du noeud iN, ponderes par
               !                         la valeur de la fonction de forme
               !                         du noeud iN aux centres de gravite
               !                         des grains et le poids considere
      real(kind=8), dimension(:, :, :), pointer :: ele_fields
               ! pour stocker les champs definis par noeuds a projeter
               ! sur les grains 
               !  * ele_fields(i, 1 : nbDIME, iEl) : pour l'element iEl,
               !                         1/(1 - phi(x_i))*[grad(P)](x_i)
               !                         , pour le noeud i, dans la 
               !                         numerotation locale de l'element
   end type T_DD_fields
       
         ! declaration d'un type adapte pour stocker des vecteurs elementaires
         ! N.B.: faire un type contenant un pointeur vers un vecteur permet
         !       de creer un tableau ou chaque case pointe vers le vecteur elementaire
         !       d'un element, dont la taille peut varier en fonction du type de 
         !       l'element
   type T_elementary_vector
            real(kind=8), dimension(:), pointer :: ttF_loc 
               ! pour stocker le vecteur elementaire correspondant a l'element considere
               ! a l'instant milieu
   end type T_elementary_vector
    
   ! declaration du maillage
   type (T_DD_mesh) :: mesh
       
   ! declaration de structure de donnees permettant la recherche
   ! de l'element contenant chacun des grains
   type (T_DD_search) :: grains2elem
       
   ! declarations des champs a projeter/interpoler et des 
   ! resultats des projections/interpolations
   type (T_DD_fields) :: fields
         
   ! declaration des constantes symboliques indiquant le type de poids
   ! a utiliser pour le calcul de la vitesse moyenne des grains
   integer, parameter :: weight_mass = 1 ! masse du grain
   integer, parameter :: weight_volume = 2 ! volume du grain
   integer, parameter :: weight_1 = 3 ! poids unitaire (1.d0)
   
   ! declaration du type de poids utilise pour le calcul de la vitesse
   ! moyenne des grains
   integer :: weight_type = weight_mass ! par defaut on pondere avec la masse
      ! des grains
   
   ! daclaration du tableu servant a stocker les vecteurs elementaires
   ! (un au debut et un a la fin du pas) corespondant a la contribution
   ! du terme de Biot (dans le cas lineaire)
   type (T_elementary_vector), dimension(:), allocatable :: Biot
   
   ! declaration du tableau contenant les volumes de reference pour 
   ! le calcul de la porosite locale en chaque noeud
   real(kind=8), dimension(:), allocatable :: V_loc
         
   ! declaration des objets permettant d'acceder au maillage
   ! sur lequel repose le modele de thermique :
         
   integer :: iTh_bdyty ! indice du modele de thermique surcharge
      ! pour resoudre l'equation d'evolution de la pression, dans
      ! therMAILx
   integer :: nb_ele ! nombre d'elements du maillage, sur lequel
      ! repose le modele de thermique
   integer :: NbNo ! nombre de noeuds par element
   character(len=4) :: T_Fonc_Forme ! type de fonction de forme utilisee:
                                    ! T_P1, Q_P1
                                    ! (nomenclature du module a_EF)
   integer :: nb_nodes ! nombre de noeuds du maillage, sur lequel
      ! repose le modele de thermique
         
   ! declaration des objets permettant d'acceder aux grains :
         
   integer :: nb_grains ! le nombre de grains
   integer, dimension(:), allocatable :: grains2rbdyx ! table de
      ! correspondance entre les grains et les corps correspondant
      ! dans :
      !  - RBDY2 : en 2D
      !  - RBDY3 : en 3D
         
   ! declaration des constantes indiquant les types de distributions
   ! de rayons geres par le code
   integer, parameter :: distribution_mono = 1 ! cas mono-disperse
   integer, parameter :: distribution_uniform = 2 ! cas de rayons
      ! distribues suivant une loi uniforme
   integer, parameter :: distribution_general = 3 ! cas general ou
      ! on ne sait rien de la distribution des rayons   

   ! declaration de la variable indiquant le type de distribution
   ! choisi
   integer :: distribution_type = distribution_mono ! par defaut,
      ! on est en monodisperse

   ! declaration du rayon moyen des grains : 
   !   * rayon de tous les grains, en MONODISPERSE
   !   * rayon moyen effectif des grains, en POLYDISPERSE
   !     (calcule en connaissant la repartition des rayons)
         real(kind=8) :: mean_radius
         
   ! declaration des champs, pour estimer la
   ! convergence du point fixe (si on a atteint un nouvel etat 
   ! d'equilibre)
   real(kind=8), dimension(:, :), allocatable :: v_G ! vitesses des
      ! grains au debut de l'iteration du point fixe courante
   real(kind=8), dimension(:, :), allocatable :: v_G_new ! vitesses
   ! des grains a la fin de l'iteration du point fixe courante
   ! N.B.: * v_G et v_G_new contiennent chacun une approximation de la vitesse
   !         des grains a la fin du pas (l'inconnue!)
   !       * v_G(i, j) contient la composante j du vecteur vitesse du grain i
   !         i dans {1, ..., nb_grains}, j dans {1, ..., nbDIME}
   
   real(kind=8), dimension(:, :), allocatable :: Fh_old 
      ! force hydrodynamique, appliquee sur les grains, calculee a
      ! l'itere de point fixe precedent
   
   ! declaration des champs moyennes/projetes :
         
   !  * porosite:
   real(kind=8), dimension(:), allocatable :: phi_begin ! au debut du pas de temps
   real(kind=8), dimension(:), allocatable :: phi ! a la fin du pas de temps
   real(kind=8), dimension(:), allocatable :: phi_m ! approximation de la porosite
                                                          ! dans la configuration mediane:
                                                          ! t_m = theta_t*t_f + (1 - theta_t)*t_i
         
   !  * vitesse barycentrique des grains :
   real(kind=8), dimension(:, :), allocatable :: u_begin ! au debut du pas de temps
   real(kind=8), dimension(:, :), allocatable :: u ! a la fin du pas de temps
   real(kind=8), dimension(:, :), allocatable :: u_m ! approximation de la vitesse
                                                     ! barycentrique des grains
                                                     ! a l'instant milieu :
                                                     ! t_m = theta_t*t_f + (1 - theta_t)*t_i   
      
   !  * force hydrodynamique :
   real(kind=8), dimension(:, :), allocatable :: Fh_begin ! au debut du pas de temps
   real(kind=8), dimension(:, :), allocatable :: Fh ! a la fin du pas de temps
   real(kind=8), dimension(:, :), allocatable :: Fh_m ! approxiamtion de la force
                                                      ! hydrodynamique dans la configuration
                                                      ! mediane:
                                                      ! t_m = theta_t*t_f + (1 - theta_t)*t_i
                                                      ! (ou moyenne sur le pas de temps de Fh?)
         
   ! declaration des variables servant au calcul du champ de coefficient de diffusion
         
   real(kind=8), parameter :: phi_max = 0.75 ! porosite maximale pour le calcul de
      ! la permeabilite et de la force hydrodynamique. pour Carman-Kozeny, on doit avoir
      !  c > c_min = 0.25, ce qui revient a ecrire: phi < phi_max = 0.75
   real(kind=8) :: eta ! viscosite du fluide
   real(kind=8) :: P0 ! pression moyenne du fluide (equivalent de la temperature de
      ! reference en thermique) 
   real(kind=8), dimension(:), allocatable :: DiCo ! champ de 
      ! coefficient de diffusion (equivalent de COCO en thermique):
         
   !  declaration des variables servant a deposer les fields :
   integer :: COCO_rank=0 ! rang du field correpondant a la conductivite thrmique
      ! , pour y deposer le champ de coefficient de diffusion
   integer :: SPHV_rank=0 ! rang du field correpondant a la capacite thrmique
      ! , pour y deposer le champ de porosite
         
   ! declaration de variables servant a des routines de post-traitement
   real(kind=8) :: mean_phi ! porosite moyenne theorique pour tout l'echantillon

   ! declaration des variables fixant le type de l'equation a resoudre pour le fluide :
   !   * linearisee (gaz parfaits + linearisation ou fluide incompressible) ou non (gaz parfaits)
   logical :: is_linearized=.false.        
   !   * stationnaire (fluide incompressible) ou non (gaz parfaits, avec ou sans linearisation)
   logical :: is_steady=.false.

   ! definition des methodes publiques
   public new_gas_grains_coupling, &
          set_fluid_model, &
          set_distribution_type, &
          set_weight_type, &
          set_fluid_viscosity, &
          set_average_pressure, &
          get_max_height, &
          get_min_height, &
          delete_gas_grains_coupling, &
          Grains_In_Mesh, &
          Compute_Average_Node_Fields, &
          Compute_Elementary_Biot, &
          Compute_Hydrodynamic_Force, &
          set_gas_grains_coupling_fields, &
          Add_Biot_To_External_Flux_By_Element, &
          Set_Hydrodynamic_Force_By_Grain, &
          Update_Fields, &
          Init_Fixed_Point_Algorithm, &
          Increment_Fixed_Point_Algorithm, &
          Check_Fixed_Point_Algorithm_Convergence, &
          Update_Fixed_Point_Algorithm, &
          compute_mean_porosity_error, &
          compute_mean_pressure
   
! declaration des routines du module
contains 

   ! fonction qui permet de fixer le type d'equation a resoudre,
   ! en fonction du modele pour le fluide
   subroutine set_fluid_model(model)

      implicit none

      ! variable d'entree :
      character(len=7) :: model ! modele a utiliser pour le fluide :
         ! * 'gaz_lin' : gaz parfait, avec le modele linearise
         ! * 'gaz_std' : gaz parfait, avec le modele standard (i.e. non lineaire)
         ! * 'incomp_' : fluide incompressible
      
      ! variables locales         1234567890123456789012
      character(len=22) :: IAM = 'GGCxx::put_fluid_model'

      ! en fonction du modele a utiliser pour le fluide
      select case(model)
         ! cas du gaz parfait, avec le modele linearise
         case('gaz_lin')
            ! on utilise le modele linearise
            is_linearized=.true.
            ! et instationnaire (Crank-Nicholson)
            is_steady=.false.
         ! cas du gaz parfait, avec le modele standard (i.e. non lineaire)
         case('gaz_std')
            ! on utilise le modele non lineaire
            is_linearized=.false.
            ! et instationnaire (Crank-Nicholson)
            is_steady=.false.         
         ! cas du fluide incompressible :
         case('incomp_') 
            ! on utilise le modele linearise
            is_linearized=.true.
            ! et stationnaire (solveur de Poisson)
            is_steady=.true.
            ! on "corromp" le modele linearise en forcant 
            ! la pression moyenne (devenue artificielle) a 1
            P0 = 1.d0 
         ! si on ne reconnait pas le modele
         case default
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown fluid model!')
            ! on quitte le programme
            stop
      end select

   end subroutine set_fluid_model 

   ! constructeur : 
   ! * recupere les donnees pour le couplage :
   !   - le maillage de thermique
   !   - les numeros des disques dans RBDY2 
   subroutine new_gas_grains_coupling

      implicit none
        
      ! variables locales         123456789012345678901234567890
      character(len=30) :: IAM = 'GGCxx::new_gas_grains_coupling'
      integer, dimension(:), allocatable :: conn ! pour recuperer
         ! la table de connectivite d'un element
      integer :: iccdof ! indice de la case situee avant la 
         ! definition de coordonnees du noeud courant
      integer :: i, j ! indices de boucle
            
      ! on recupere le nombre de grains
      
      ! en fonction de la dimension :
      select case(nbDIME)
            
         case(2) ! cas 2D:
          
            nb_grains = get_nb_DISKx()
            
         case(3) ! cas 3D:
           
            ! pas encore implemente... 
            call faterr(IAM, '3D grains mapping is not implemented yet!')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop
            
      end select
            
      ! si on ne trouve pas de grains
      if (nb_grains < 1) then
            
         ! on affiche un message d'erreur
         call faterr(IAM, 'sans grains le couplage gaz-grains est impossible!')
            
         ! on quitte le programme
         stop
            
      end if
            
      ! on alloue l'espace memoire pour stocker la table de corespondance
      ! entre les grains et les corps rigides correspondants
      allocate(grains2rbdyx(nb_grains))
            
      ! on recupere la table de corespondance
      ! entre les grains et les corps rigides correspondants
            
      ! en fonction de la dimension :
      select case(nbDIME)
            
         case(2) ! cas 2D:
          
            call get_DISKx2RBDY2(grains2rbdyx, nb_grains)
            
         case(3) ! cas 3D:
           
            ! pas encore implemente... 
            call faterr(IAM, '3D grains mapping is not implemented yet!')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop
            
      end select
            
      !print*, 'grains2rbdyx=', grains2rbdyx
            
      ! selon la distribution de rayons condiree
      select case(distribution_type)

         case(distribution_mono) ! cas monodisperse

            ! on recupere le rayon moyen des grains (qui corespond au
            ! rayon de chaque grain)
            mean_radius = get_mean_radius()

         case(distribution_uniform) ! cas de la distribution uniforme
 
            call compute_mean_radius_uniform_distribution

         case(distribution_general) ! cas ou on ne sait rien a priori

            call compute_mean_radius_general_distribution

      end select 
 
      print*, 'mean_radius=', mean_radius

      !print*, 'r_min=', get_min_radius_DISKx()
      !print*, 'r_mean=', get_mean_radius_DISKx()
      !print*, 'r_max=', get_max_radius_DISKx()
         
      ! on alloue l'espace memoire pour stocker les vitesses des grains
      ! approximees, pour etudier la convergence du point fixe
      allocate(v_G(nb_grains, nbDIME))
      allocate(v_G_new(nb_grains, nbDIME))

      ! on alloue l'espace memoire pour stocker la force hydrodynamique
      ! calculee sur les grains a l'itere precedent, pour etudier la 
      ! convergence du point fixe
      allocate(Fh_old(nb_grains, nbDIME))
   
      ! si on ne trouve aucun modele de thermique
      if (get_nb_therMAILx() < 1) then

         ! on affiche un message d'erreur
         call faterr(IAM, &
                    'sans modele de thermique a surcharger le couplage gaz-grains est impossible!')

         ! on quitte le programme
         stop

      end if

      !print*, 'nb_therMAILx=', get_nb_therMAILx()

      ! hypothese forte : on suppose que le modele de thermique
      ! a utiliser est le premier de la liste (c'est le cas s'il
      ! est seul!)
      iTh_bdyty = 1
            
      ! on recupere le nombre d'elements du maillage
      !nb_ele = get_nb_ele_therMAILx(bdyty2M_bdyty)
      nb_ele = get_nb_ele_therMAILx(iTh_bdyty)
            
      ! si tous les elements ne sont pas du même type
      if (.not. only_one_elem_type_therMAILx(iTh_bdyty)) then
            
         ! on affiche un message d'erreur
         call faterr(IAM, 'tous les elemnts doivent etre du meme type')
                   
         ! on quitte le programme
         stop
            
      end if
         
      ! on recupere le type de fonction de forme d'un element
      ! N.B.: ici, on sait que tous les elements sont du même 
      ! type, on peut donc recuperer le type de fonction de forme
      ! du premier
      T_Fonc_Forme = get_T_FONC_FORME_therMAILx(iTh_bdyty, 1) 
            
      !print*, 'T_Fonc_Forme=', T_Fonc_Forme
           
      ! recuperation la table de connectivite
            
      ! recuperation du nombre de noeuds d'un element
            
      ! ici, on sait que tous les elements sont du même type, on
      ! peut donc recuperer le nombre de noeud du premier
      NbNo = get_N_NODE_therMAILx(iTh_bdyty, 1)
            
      ! on alloue la memoire pour stocker la table de connectivite
      allocate(mesh%conn(nb_ele, NbNo))
            
      ! on alloue l'espace memoire pour recuperer la
      ! table de connectivite d'un element
      allocate(conn(NbNo))
   
      ! on la remplit, en utilisant la numerotation locale
      ! de therMAILx
      do i=1, nb_ele
         ! on recupere la table de donnectivite de l'element courant
         call get_conn_therMAILx(iTh_bdyty, i, conn, NbNo)
      
         ! on la stocke dans la sructure mesh    
         mesh%conn(i, :) = conn
      end do
            
      ! on desalloue l'espace memoire occupe pour recuperer la
      ! table de connectivite d'un element
      deallocate(conn)
   
      !do i=1, nb_ele
      !   do j=1, NbNo
      !      print*, 'mesh%conn(', i, ', ', j, ')=', mesh%conn(i, j)
      !    end do
      !end do
            
      ! on recupere le nombre de noeuds du maillage
      nb_nodes = get_nb_nodes_therMAILx(iTh_bdyty)
            
      !print*, 'nb_nodes=', nb_nodes
            
      ! on peut recuperer les coordonnees des noeuds            
            
      ! on alloue l'espace emoire pour stocker les cooordonnees des
      ! noeuds
      allocate(mesh%coor(nb_nodes, nbDIME))
            
      ! on recupere les coordonnees des noeuds
       do i=1, nb_nodes
         mesh%coor(i, :) = get_cooref_node_therMAILx(iTh_bdyty, i)
      end do
            
      !do i=1, nb_nodes
      !   do j=1, nbDIME
      !      print*, 'mesh%coor(', i, ', ', j, ')=', mesh%coor(i, j)
      !    end do
      !end do
            
      ! on recupere le rang du champ de conductivite thermique, dans
      ! les fields
      COCO_rank = get_field_rank_therMAILx(iTh_bdyty, 'COCO')
            
      !print*, 'COCO_rank=', COCO_rank
      
      ! si on travaille avec un modele instationnaire
      if (.not. is_steady) then      
         ! on recupere le rang du champ de capacite thermique specifique, dans
         ! les fields
         SPHV_rank = get_field_rank_therMAILx(iTh_bdyty, 'SPHV')
      end if
            
      !print*, 'SPHV_rank=', SPHV_rank
            
      ! on alloue l'espace memoire pour stocker les volumes de reference
      ! pour le calcul de la porosite locale en chaque noeud
      allocate(V_loc(nb_nodes))
            
      ! on les calcule
      call compute_V_loc
            
      !print*, 'V_loc=', V_loc
              
      ! on alloue l'espace memoire pour stocker la stucture de
      ! donnees permettant d'effectuer la recherche de l'element
      ! contenant chacun des grains
            
      ! les coordonnees des grains 
      allocate(grains2elem%grains_coor(nb_grains, nbDIME))
            
      ! la table donnant le numero de l'element contenant un grain
      allocate(grains2elem%Elem(nb_grains))
          
      ! les valeurs des fonctions de formes au cnetres d'inertie des
      ! grains
      allocate(grains2elem%Alpha(NbNo, nb_grains))
          
      ! on cherche dans quel element se trouve chaque grain, 
      ! initialement
      ! N.B. : a l'initialisation, les coordonnees des grains a la fin
      !        du pas sont les coordonnees initiales des grains 
      call Grains_In_Mesh
           
      ! on alloue l'espace memoire pour stocker les champs a
      ! interpoler/projeter ainsi que le resultat de 
      ! l'interpolation/projection
      allocate(fields%grains_fields(nb_grains, nbDIME + 2))
      allocate(fields%nodes_fields(nb_nodes , nbDIME + 2))
      allocate(fields%ele_fields(NbNo, nbDIME, nb_ele))
            
      ! on remplit les champs, defini sur les grains, a interpoler
      ! qui ne changent pas  au cours du calcul : 
      !    * le volume des grains
      call get_grains_volume(fields%grains_fields(:, 1))
             
      !print*, 'V_G=', fields%grains_fields(:, 1)
            
      !    * le poids a utiliser pour le calcul de la vitesse moyenne
      !      des grains aux noeuds
  
      !print*, 'weight_type=', weight_type
   
      ! en fonction du type de poids choisi
      select case(weight_type)
   
         ! cas de la masse des grains
         case(weight_mass)
      
            ! on recupere la masse des grains
            call get_grains_mass(fields%grains_fields(:, 2))
      
         ! cas du volume des grains
         case(weight_volume)
      
            ! on recupere le volume des grains
            ! N.B.: il est deja stocke dans fields%grains_fields(:, 1)
            fields%grains_fields(:, 2) = fields%grains_fields(:, 1)
      
         ! cas du poids unitaire
         case(weight_1)
      
            ! on applique le poids unitaire
            fields%grains_fields(:, 2) = 1.d0
   
         ! si on ne reconnait le type de poids utilise
         case default
      
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown weight type')
      
         ! on quitte le programme
         stop
   
      end select
   
      !print*, 'fields%grains_fields(:, 2)=', fields%grains_fields(:, 2)
   
      ! on calcule la porosite moyenne theorique pour tout l'echantillon
      ! N.B.: necessaire pour analyser l'efficacite de la methode de calcul
      !       du champ de porosite aux noeuds en fonction du type d'element
      !       de la finesse du maillage, etc
      call compute_mean_porosity

      print*, 'mean_phi=', mean_phi
 
      ! on alloue les champs moyennes/projetes :
       
      !  * porosite:
      allocate(phi_begin(nb_nodes)) ! au debut du pas de temps
      allocate(phi(nb_nodes)) ! a la fin du pas de temps
      allocate(phi_m(nb_nodes)) ! dans la configuration intermediaire:
                                ! t_m = theta_t*t_i + (1 - theta_t)*t_f
         
      !  * vtesse barycentrique des grains :
      allocate(u_begin(nb_nodes, nbDIME)) ! au debut du pas de temps
      allocate(u(nb_nodes, nbDIME)) ! a la fin du pas de temps
      allocate(u_m(nb_nodes, nbDIME)) ! dans la configuration intermediaire:
                                      ! t_m = theta_t*t_i + (1 - theta_t)*t_f      
   
      !  * force hydrodynamique :
      allocate(Fh_begin(nb_grains, nbDIME)) ! au debut du pas de temps
      allocate(Fh(nb_grains, nbDIME)) ! a la fin du pas de temps
      allocate(Fh_m(nb_grains, nbDIME)) ! dans la configuration intermediaire:
                                        ! t_m = theta_t*t_i + (1 - theta_t)*t_f
       
      ! on calcule les champs de porosite et de vitesse barycentrique 
      ! initiaux
      ! N.B. : a l'initialisation, les vitesses des grains a la fin
      !        du pas sont les vitesses initiales des grains 
      call Compute_Average_Node_Fields
            
      ! on alloue l'espace memoire pour stockers les vecteurs elementaires
      ! correspondant a la contribution du terme de Biot (cas lineaire)
   
      ! on alloue le tableau de vecteur elementaires
      allocate(Biot(nb_ele))
   
      ! pour chaque element
      do i=1, nb_ele
         ! on annulle le pointeur corespondant au vecteur elementaire
         ! courant
         nullify(Biot(i)%ttF_loc)
      
         ! on alloue l'espace memoire pour stocker le vecteur elementaire courant
         ! coreepondant a l'insntant milieu
         allocate(Biot(i)%ttF_loc(NbNo))

         ! initialement, la contribution du terme de Biot et le terme d'advection
         ! a l'instant milieu n'a pas de sens!
         Biot(i)%ttF_loc = 0.d0

      end do
 
      ! on calcule la force hydrodynamique appliquee sur les grains,
      ! initialement
      ! N.B. : a l'initialisation, les presions aux noeuds des a la fin
      !        du pas sont les pressions initiales aux noeuds 
      call Compute_Hydrodynamic_Force
            
      ! on bascule les champs calcules (phi, u, Fh), stockes dans 
      ! les champs a la fin du pas (pour des raisons de commodite)
      ! dans les champs au debut du pas
      call Update_Fields
      
      ! on alloue l'espace memoire pour stocker le champ de 
      ! coefficient de diffusion
      allocate(DiCo(nb_nodes))
            
   end subroutine new_gas_grains_coupling

   ! fonctions qui permettent de donner les parametres et la forme
   ! de la distribution des rayons dans le cas polydisperse:
   
   ! fonction qui calcule le rayon effectif pour le calcul de la
   ! permeabilite par la regle de Kozeny-Carman, dans le cas d'une 
   ! distribution uniforme de rayon 
   subroutine compute_mean_radius_uniform_distribution

      implicit none

      ! variables locales:      
      real(kind=8) :: R_min, R_max ! intervalle [R_min, R_max] ou 
         ! sont tires les rayons
      
      ! on recupere le rayon min et le rayon max
      R_min = get_min_radius_DISKx()
      R_max = get_max_radius_DISKx()      

      ! on calcule le rayon moyen effectif, pour le calcul de la
      ! permeabilite par la formule de Carman-Kozeny
      ! mean_radius = 3/4*(R_max^4 - R_min^4)/(R_max^4 - R_min^4)
      mean_radius = 0.75d0*(R_max*R_max*R_max*R_max - R_min*R_min*R_min*R_min)/ &
         (R_max*R_max*R_max - R_min*R_min*R_min)

   end subroutine compute_mean_radius_uniform_distribution

   ! fonction calculant le rayon moyen effectif, pour le calcul de
   ! la permeabilite par la formule de Carman-Kozeny, par une methode
   ! statistique :
   ! r = <r^3>/<r^2>
   subroutine compute_mean_radius_general_distribution

      implicit none

      real(kind=8) :: r ! rayon courant
      real(kind=8) :: sum_r3 ! somme des r^3
      real(kind=8) :: sum_r2 ! somme des r^2
      integer :: i ! indice de boucle

      ! on initilalise les sommes à 0
      sum_r3 = 0.d0
      sum_r2 = 0.d0

      ! pour chaque grain
      do i=1, nb_grains
         ! on recupere le rayon du grain courant
         r = get_radius(i)
         ! on ajoute la contribution du grain courant
         sum_r3 = sum_r3 + r*r*r
         sum_r2 = sum_r2 + r*r
      end do

      ! on en deduit le rayon moyen effectif
      mean_radius = sum_r3/sum_r2

   end subroutine compute_mean_radius_general_distribution
 
   ! fonction qui permet de choisir la distribution des rayons
   subroutine set_distribution_type(distribution)
   
      implicit none
   
      ! variable d'entree :
      character(len=5) :: distribution ! chaine indiquant quel type de 
         ! distribution on souhaite utiliser :
         !    * 'mono_' : on est dans le cas monodisperse
         !    * 'unifo' : on considere une distribution uniforme
         !    * 'gener' : cas general ou on ne connait pas la distribution
         !                utilisee a priori
   
      ! variable locale :         1234567890123456789012345678
      character(len=28) :: IAM = 'GGCxx::set_distribution_type'
            
      select case(distribution)
   
         case('mono_') ! cas ou on considere un echantillon monodisperse
      
            distribution_type = distribution_mono
      
         case('unifo') ! cas ou on considere une distribution uniforme 
      
            distribution_type = distribution_uniform
   
         case('gener') ! cas ou on ne sait rien a priori  
      
            distribution_type = distribution_general
   
         ! si on ne reconnait pas le type de ditribution a utiliser
      case default
      
         ! on affiche un message d'erreur
         call faterr(IAM, 'unknown ditribution type')
      
         ! on quitte le programme
         stop
   
      end select
   
   end subroutine set_distribution_type
         
   ! fin des definition de fonctions qui permettent de donner les 
   ! parametres et la forme de la distribution des rayons dans le 
   ! cas polydisperse
   
   ! fonction qui permet de changer la strategie de calcul de la 
   ! vitesse moyennee des grains
   ! N.B.: utiliser les masses et la strategie par defaut
   !       (adaptee au cas general : polydisperse, avec plusieurs
   !       materiaux)
   subroutine set_weight_type(weight)
   
      implicit none
   
      ! variable d'entree :
      character(len=5) :: weight ! chaine indiquant quel type de poids
         ! on souhaite utiliser :
         !    * 'vol__' : on pondere avec le volume des grains
         !    * 'none_' : on ne pondere pas (i.e. on applique un
         !                poids de 1 a chaque grain)
   
      ! variable locale :         1234567890123456789012
      character(len=22) :: IAM = 'GGCxx::set_weight_type'
            
      select case(weight)
   
         case('mass_') ! cas ou le poids applique est la masse du grain
      
            weight_type = weight_mass
      
         case('vol__') ! cas ou le poids applique est le volume du grain 
      
            weight_type = weight_volume
   
         case('none_') ! cas ou aucun poids n'est applique (i.e. le 
                          ! poids est 1 pour chaque grain) 
      
            weight_type = weight_1
   
         ! si on ne reconnait pas le type de poids a appliquer
      case default
      
         ! on affiche un message d'erreur
         call faterr(IAM, 'unknown weight type')
      
         ! on quitte le programme
         stop
   
      end select
   
   end subroutine set_weight_type
   
   ! fonction qui recupere la viscosite du fluide
   subroutine set_fluid_viscosity(eta_)
            
      implicit none
         
      ! variable d'entree :
      real(kind=8) :: eta_ ! viscosite du fluide
         
      ! on stocke la viscosite dans le module
      eta = eta_
          
      !print*, 'eta=', eta
         
   end subroutine set_fluid_viscosity

   ! fonction qui recupere la pression moyenne du fluide
   subroutine set_average_pressure(P0_)
            
      implicit none
       
      ! variable d'entree :
      real(kind=8) :: P0_ ! pression moyenne du fluide
      
      ! varaible locale :       123456789012345678901234567
      character(len=27) :: IAM='GGCxx::set_average_pressure'     

      ! si on utilise le modele stationnaire, on ne DOIT PAS
      ! donner la pression moyenne du fluide
      if (is_steady) then
         ! on affiche un message d'erreur
         call faterr(IAM, & 
         'you MUST NOT give P0 for this fluid model!')
         ! on quitte le programme
         stop
      end if
   
      ! on stocke la pression moyennedu fluide dans le module
      P0 = P0_
          
      !print*, 'P0=', P0
         
   end subroutine set_average_pressure

   ! fonction qui renvoie l'altitude du grain centre d'inertie du
   ! grain le plus haut de l'echantillon
   ! N.B.: cette fonction se sert des coordonnes des grains
   !       stockees dans le module
   function get_max_height()

      implicit none

      ! valeur de retour :
      real(kind=8) :: get_max_height

      get_max_height = maxval(grains2elem%grains_coor(:, nbDIME))

   end function get_max_height
         
   ! fonction qui renvoie l'altitude du grain centre d'inertie du
   ! grain le plus bas de l'echantillon
   ! N.B.: cette fonction se sert des coordonnes des grains
   !       stockees dans le module
   function get_min_height()

      implicit none

      ! valeur de retour :
      real(kind=8) :: get_min_height

      get_min_height = minval(grains2elem%grains_coor(:, nbDIME))

   end function get_min_height

   ! destructeur: libere l'espace memoire occuppe par le couplage
   ! gaz-grains
   subroutine delete_gas_grains_coupling
         
      implicit none
         
      ! variables locales :
      integer :: i ! indice de boucle
   
      ! on desalloue la table de corespondance
      ! entre les grains et les corps rigides correspondants
      deallocate(grains2rbdyx)
         
      ! on desalloue l'espace memoire utilise pour stocker les 
      ! vitesses des grains approximees, pour etudier la 
      ! convergence du point fixe
      deallocate(v_G)
      deallocate(v_G_new)
   
      ! on desalloue l'espace memoire utilise pour stocker la force
      ! hydrodynamique calculee sur les grains a l'itere precedent,
      ! pour etudier la convergence du point fixe
      deallocate(Fh_old)
   
      ! on desalloue la table de connectivite
      deallocate(mesh%conn)
         
      ! on desalloue la liste des coordonnees des noeuds
      deallocate(mesh%coor)
         
      ! on desalloue l'espace memoire pour stocker les volumes de
      ! reference pour le calcul de la porosite locale en chaque noeud
      deallocate(V_loc)
        
      ! on desalloue l'espace memoire pour stocker la stucture de
      ! donnees permettant d'effectuer la recherche de l'element
      ! contenant chacun des grains
      deallocate(grains2elem%grains_coor)
      deallocate(grains2elem%Elem)
      deallocate(grains2elem%Alpha)
            
      ! on desalloue l'espace memoire pour stocker les champs a
      ! interpoler/projeter ainsi que le resultat de 
      ! l'interpolation/projection
      deallocate(fields%grains_fields)
      deallocate(fields%nodes_fields)
      deallocate(fields%ele_fields)
            
      ! on desalloue les champs moyennes/projetes :
         
      !  * porosite:
      deallocate(phi_begin) ! au debut du pas de temps
      deallocate(phi) ! a la fin du pas de temps
      deallocate(phi_m) ! dans la configuration intermediaire:
                        ! t_m = theta_t*t_i + (1 - theta_t)*t_f
                              
      !  * vitesse barycentrique des grains :
      deallocate(u_begin) ! au debut du pas de temps
      deallocate(u) ! a la fin du pas de temps
      deallocate(u_m) ! dans la configuration intermediaire:
                      ! t_m = theta_t*t_i + (1 - theta_t)*t_f        
 
      !  * force hydrodynamique :
      deallocate(Fh_begin) ! au debut du pas de temps
      deallocate(Fh) ! a la fin du pas de temps
      deallocate(Fh_m) ! dans la configuration intermediaire:
                       ! t_m = theta_t*t_i + (1 - theta_t)*t_f
             
      ! on desalloue l'espace memoire utilise pour stocker les vecteurs elementaires
      ! correspondant a la contribution du terme de Biot (cas lineaire)
   
      ! pour chaque element
      do i=1, nb_ele
         ! on desalloue l'espace memoire utilise pour stocker la valeur a 
         ! l'instant milieu du vecteur elementaire courant
         deallocate(Biot(i)%ttF_loc)
      
         ! on annulle le pointeur corespondant au vecteur elementaire
         ! courant
         nullify(Biot(i)%ttF_loc)
      end do
   
      ! on desalloue le tableau de vecteur elementaires
      deallocate(Biot)
   
      ! on desalloue l'espace memoire pour stocker le champ de 
      ! coefficient de diffusion
      deallocate(DiCo)
         
   end subroutine delete_gas_grains_coupling

   ! fonction qui calcule la porosite moyenne theorique pour tout l'echantillon
   ! precondition : on a lu le volume des grains
   subroutine compute_mean_porosity

      implicit none

      ! variables locales :
      real(kind=8) :: V ! pour calculer le volume du domaine
      integer :: iEl ! indice de boucle sur les elements

      ! on calcule le volume du domaine
      
      ! on l'initialise a 0
      V = 0.d0

      ! pour chaque element
      do iEl=1, nb_ele
         ! on ajoute la contribtion de l'element courant au volume du domaine
         V = V + compute_element_volume_therMAILx(iTh_bdyty, iEl)
      end do

      ! la porosite moyenne theorique est le complementaire de la compacite 
      ! moyenne theorique, obtenue en divisiant la somme des volumes des grains
      ! par le volume du domaine
      mean_phi = 1.d0 - sum(fields%grains_fields(:, 1))/V
      
      ! si on est en 2D :
      if (nbDIME == 2) then
         ! on a corrige la porosite moyenne 2D obtenue precedemment
         ! de sorte que la porosite calculee pour reseau carre de disques
         ! correponde a la porosite calcule pour un resau cubique de spheres
         mean_phi = (2.d0*mean_phi + 1)/3.d0
      end if

   end subroutine compute_mean_porosity

   ! fonction qui calcule les volumes de references pour le calcul de
   ! la porosite locale en chaque noeud
   subroutine compute_V_loc
         
      implicit none
         
      ! variables locales
      real(kind=8), dimension(nb_ele, NbNo) :: V_nod ! V_nod(iEl, i) contient
         ! la part du volume de l'element a affecter au noeud i de l'element iEl
      integer :: i, iEl, iNod ! indices de boucle
            
      ! pour chaque element
      do iEl=1, nb_ele
   
         ! on recupere, pour chaque element, la part du volume de l'element
         ! a affecter aux noeuds de l'element
         call element_volume_by_node_therMAILx(iTh_bdyty, iEl, V_nod(iEl, :))
   
         !print*, 'V_nod(', iEl, ', :)=', V_nod(iEl, :)
   
         !print*, 'sum(V_nod(', iEl, ', :))=', sum(V_nod(iEl, :))
   
      end do
   
      !print*, 'V_ele=', V_ele
         
      ! on initialise a 0 les volumes de referencs associes a
      ! chaque noeud et les degres associes a chaque noeud
      V_loc = 0.d0
            
      ! pour chaque element
      do iEl=1, nb_ele
            
         ! pour chaque noeud de l'element courant
         do i=1, NbNo
               
            ! on recupere le numero du sommet courant dans la 
            ! numerotation locale
            iNod = mesh%conn(iEl, i)
               
            ! on ajoute la part du volume de l'element courant, 
            ! pour le noeud courant, au volume
            ! de reference associe au noeud courant
            V_loc(iNod) = V_loc(iNod) + V_nod(iEl, i)
      
         end do
            
      end do
   
   end subroutine compute_V_loc

   ! fonction qui recupere un champ vetoriel sur les grains
   ! N.B.: LMGC90 rend un champ a trois composantes, dont la troisieme
   !       (angle de rotation, vitesse de rotation), n'a pas de sens 
   !       dans ce cadre, elle est donc occultee
   subroutine get_grains_vector_field(label, grains_vector_field)
         
      implicit none
            
      ! variable d'entree :
      character(len=5), intent(in) :: label ! etiquette indiquant quel champ
         ! on souhaite recuperer :
         !  * 'Coor_' : pour recuperer les coordonnees des grains
         !              a la fin du pas de temps
         !  * 'Vbeg_' : pour recuperer les vitesses des grains au
         !              debut du pas de temps
         !  * 'V____' : pour recuperer les vitesses des grains a
         !              la fin du pas de temps
         !  * 'Reac_' : pour recuperer les impulsions de contact 
         !              sur le pas de temps
         !  * 'Fext_' : pour recuperer la moyennes sur le pas de
         !              temps des forces exterieures
   
      ! variable de sortie :
      real(kind=8), dimension(nb_grains, nbDIME), intent(out) :: grains_vector_field
         ! champ vectoriel defini sur les grains
         
      ! variables locales:        123456789012345678901234567890
      character(len=30) :: IAM = 'GGCxx::get_grains_vector_field'
            
      ! en fonction de la dimension
      select case(nbDIME)
            
         case(2) ! cas 2D :
           
            ! on appelle la fonction qui recupere un champ vectoriel
            ! definit sur les grains, en 2D
            call get_grains_vector_field_2D(label, grains_vector_field)
           
         case(3) ! cas 3D :
            
            ! pas encore implemente
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
            
      end select
         
   end subroutine get_grains_vector_field

   ! fonction qui recupere un champ vetoriel sur les grains, en 2D
   ! N.B.: LMGC90 rend un champ a trois composantes, dont la troisieme
   !       (angle de rotation, vitesse de rotation), n'a pas de sens 
   !       dans ce cadre, elle est donc occultee
   subroutine get_grains_vector_field_2D(label, grains_vector_field)
         
      implicit none
            
      ! variable d'entree :
      character(len=5), intent(in) :: label ! etiquette indiquant quel champ
         ! on souhaite recuperer :
         !  * 'Coor_' : pour recuperer les coordonnees des grains
         !              a la fin du pas de temps
         !  * 'Vbeg_' : pour recuperer les vitesses des grains au
         !              debut du pas de temps
         !  * 'V____' : pour recuperer les vitesses des grains a
         !              la fin du pas de temps
         
      ! variable de sortie :
      real(kind=8), dimension(nb_grains, 2), intent(out) :: grains_vector_field
         ! champ vectoriel defini sur les grains
         
      ! variables locales
      real(kind=8), dimension(3) :: vector ! pour recupreer le 
         ! vecteur de chaque grain
      integer :: i ! indice de boucle
         
      ! pour chaque grain 
      do i=1, nb_grains
            
         ! on recupere le vecteur du grain courant
         call get_vector_RBDY2(label, grains2rbdyx(i), vector, 3)
               
         ! on stocke les deux premieres composantes du vecteur 
         grains_vector_field(i, :) = vector(1 : 2)
               
      end do
         
   end subroutine get_grains_vector_field_2D
         
   ! fonction qui pose un champ vetoriel sur les grains
   ! N.B.: LMGC90 attend un champ a trois composantes, dont la troisieme
   !       (vitesse de rotation, couple), n'a pas de sens 
   !       dans ce cadre, elle est donc mise a 0
   subroutine put_grains_vector_field(label, grains_vector_field)
         
      implicit none
            
      ! variable d'entree :
      character(len=5), intent(in) :: label ! etiquette indiquant quel champ
         ! on souhaite donner :
         !  * 'Vfree' : pour donner les vitesses libres des grains a
         !              la fin du pas de temps
         !  * 'Fext_': pour donner une force exterieure moyennee sur le pas
         !             de pas de temps, i.e. 
         !               Fext_m = (1 - theta)*Fext(t_i) + theta*Fext(t_f)
      real(kind=8), dimension(nb_grains, nbDIME), intent(in) :: grains_vector_field
         ! champ vectoriel defini sur les grains
         
      ! variables locales:        123456789012345678901234567890
      character(len=30) :: IAM = 'GGCxx::get_grains_vector_field'
            
      ! en fonction de la dimension
      select case(nbDIME)
            
         case(2) ! cas 2D :
            
            ! on appelle la fonction qui donne un champ vectoriel
            ! definit sur les grains, en 2D
            call put_grains_vector_field_2D(label, grains_vector_field)
            
         case(3) ! cas 3D :
               
            ! pas encore implemente
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
            
      end select
         
   end subroutine put_grains_vector_field

   ! fonction qui donne un champ vetoriel sur les grains, en 2D
   ! N.B.: LMGC90 attend un champ a trois composantes, dont la troisieme
   !       (vitesse de rotation, couple), n'a pas de sens 
   !       dans ce cadre, elle est donc mise a 0
   subroutine put_grains_vector_field_2D(label, grains_vector_field)
         
      implicit none
            
      ! variable d'entree :
      character(len=5), intent(in) :: label ! etiquette indiquant quel champ
         ! on souhaite donner :
         !  * 'Vfree' : pour donner les vitesses libres des grains a
         !              la fin du pas de temps
         !  * 'Fext_': pour donner une force exterieure moyennee sur le pas
         !             de pas de temps, i.e. 
         !               Fext_m = (1 - theta)*Fext(t_i) + theta*Fext(t_f)
      real(kind=8), dimension(nb_grains, nbDIME), intent(in) :: grains_vector_field
         ! champ vectoriel defini sur les grains
         
      ! variables locales
      real(kind=8), dimension(3) :: vector ! pour donner le 
         ! vecteur de chaque grain
      integer :: i ! indice de boucle
         
      ! pour chaque grain 
      do i=1, nb_grains
            
         ! on annule le vecteur
         vector = 0.d0
      
         ! on donne les deux premieres composantes du vecteur 
         vector(1 : 2) = grains_vector_field(i, :)
               
         ! on donne le vecteur du grain courant
         call put_vector_RBDY2(label, grains2rbdyx(i), vector, 3)
               
      end do
         
   end subroutine put_grains_vector_field_2D
         
   ! fonction qui recupere le volume des grains 
   subroutine get_grains_volume(V_G)
         
      implicit none
            
      ! variable de sortie :
      real(kind=8), dimension(nb_grains), intent(out) :: V_G ! pour recuperer
         ! les volumes des grains 
         
      ! variables locales:        123456789012345678901234
      character(len=24) :: IAM = 'GGCxx::get_grains_volume'
         
      ! en fonction de la dimension
      select case(nbDIME)
           
         case(2) ! cas 2D :
            
            ! on recupere des surfaces
            call get_grains_volume_2D(V_G)
                    
         case(3) ! cas 3D :
               
            ! pas encore implemente!
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
            
      end select
            
   end subroutine get_grains_volume
         
   ! fonction qui recupere le volume des grains, en 2D
   subroutine get_grains_volume_2D(V_G)
         
      implicit none
            
      ! variable de sortie :
      real(kind=8), dimension(nb_grains), intent(out) :: V_G ! pour recuperer
         ! les volumes des grains 
            
      ! variable locale :
      integer :: i ! indice de boucle
            
      ! pour chaque grain 
      do i=1, nb_grains
  
         ! on recupere la surface su grain courant
         V_G(i) = get_area(grains2rbdyx(i))
 
      end do
        
   end subroutine get_grains_volume_2D
         
   ! fonction qui recupere la masse des grains 
   subroutine get_grains_mass(m_G)
         
      implicit none
            
      ! variable de sortie :
      real(kind=8), dimension(nb_grains), intent(out) :: m_G ! pour recuperer
         ! les masses des grains 
         
      ! variables locales:        1234567890123456789012
      character(len=22) :: IAM = 'GGCxx::get_grains_mass'
         
      ! en fonction de la dimension
      select case(nbDIME)
            
         case(2) ! cas 2D :
            
            ! on recupere les masses de rigides 2D
            call get_grains_mass_2D(m_G)
                     
         case(3) ! cas 3D :
               
            ! pas encore implemente!
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
         
      end select
            
   end subroutine get_grains_mass
         
   ! fonction qui recupere la masse des grains, en 2D
   subroutine get_grains_mass_2D(m_G)
         
      implicit none
            
      ! variable de sortie :
      real(kind=8), dimension(nb_grains), intent(out) :: m_G ! pour recuperer
         ! les masses des grains 
            
      ! variable locale :
      integer :: i ! indice de boucle
            
      ! pour chaque grain 
      do i=1, nb_grains
  
         ! on recupere la masse du grain courant
         m_G(i) = get_mass_RBDY2(grains2rbdyx(i))
 
      end do
         
   end subroutine get_grains_mass_2D
  
   ! fonction qui recupere le rayon d'un grain donne:
   function get_radius(i)

      implicit none
      
      ! variable d'entree :
      integer :: i ! numero du grain dont on veut le rayon

      ! variable de sortie :
      real(kind=8) :: get_radius ! rayon du grain i

      ! variables locales:        12345678901234567
      character(len=17) :: IAM = 'GGCxx::get_radius'

      ! en fonction de la dimension
      select case(nbDIME)
            
         case(2) ! cas 2D :
           
            get_radius = get_radius_2D(i)
                     
         case(3) ! cas 3D :
               
            ! pas encore implemente!
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
            
      end select

   end function get_radius

   ! fonction qui recupere le rayon d'un grain donne, en 2D:
   function get_radius_2D(i) 

      implicit none

      ! variable d'entree :
      integer :: i ! numero du grain dont on veut le rayon

      ! variable de sortie :
      real(kind=8) :: get_radius_2D ! rayon du grain i

      ! on recupere le rayon du DISKx d'indice i
      get_radius_2D = get_radius_DISKx(i)

   end function get_radius_2D

   ! fonction qui recupere le rayon moyen des grains:
   !    * rayon des grains, en MONODISPERSE
   !    * rayon moyen des grains, en POLYDISPERSE
   function get_mean_radius()
         
      implicit none
            
      ! valeur de retour
      real(kind=8) :: get_mean_radius ! rayon moyen des grains
         
      ! variables locales:        123456789012345678901234
      character(len=24) :: IAM = 'GGCxx::get_mean_radius'
            
      ! en fonction de la dimension
      select case(nbDIME)
            
         case(2) ! cas 2D :
           
            get_mean_radius = get_mean_radius_DISKx()
                     
         case(3) ! cas 3D :
               
            ! pas encore implemente!
            call faterr(IAM, '3D is not implemented yet')
            
            stop
            
         case default
            
            ! on affiche un message d'erreur
            call faterr(IAM, 'unknown space dimension!')
              
            ! on quitte le programme
            stop      
            
      end select
            
   end function get_mean_radius
         
   ! fonction qui cherche dans quel element se trouve chaque grain
   ! a partir des coordonnees des grains (a la fin du pas de temps)
   ! apres les avoir recuperes
   subroutine Grains_In_Mesh

      implicit none
            
      ! variables locales:
      integer :: i, j, k ! indices de boucle

      ! TEST timer:
      ! si les timer ont ete initialises, on lance le timer qui mesure
      ! le temps pris par GetVector 
      if (is_started_timer()) call start_timer('GetVector___________')
  
      ! on recupere les coordonnees des grains, a la fin du pas 
      ! de temps : 
      call get_grains_vector_field('Coor_', grains2elem%grains_coor)
            
      ! TEST timer:
      ! si les timer ont ete initialises, on arrete le timer qui mesure
      ! le temps pris par GetVector 
      if (is_started_timer()) call stop_timer('GetVector___________')
      
      !do i=1, nb_grains
      !   do j=1, nbDIME
      !      print*, 'grains2elem%grains_coor(', i, ', ', j, ')=', &
      !         grains2elem%grains_coor(i, j)
      !   end do
      !end do
            
      ! TEST timer:
      ! si les timer ont ete initialises, on lance le timer qui mesure
      ! le temps pris par GetVector             
      if (is_started_timer()) call start_timer('PointsInMesh________')
      
      ! on cherche dans quel element se trouve chaque grain et
      ! les valeurs des fonctions de forme assoicees aux noeuds
      ! d l'element au centre d'inertie de chaque grain 
      call Points_In_Mesh(nb_nodes, nbDIME, nb_ele, &
                          NbNo, nb_grains, &
                          mesh%conn, T_Fonc_Forme, mesh%coor, &
                          grains2elem%grains_coor, 'NEWTON_', &
                          grains2elem%Elem, grains2elem%Alpha)
            
      ! TEST timer:
      ! si les timer ont ete initialises, on lance le timer qui mesure
      ! le temps pris par GetVector            
      if (is_started_timer()) call stop_timer('PointsInMesh________')
      
      !print*, 'grains2elem%Elem=', grains2elem%Elem
         
      ! affichage des alpha pour tous grains, i.e
      !do k=1, nb_ele
      !   do j=4*(k - 1) + 1, 4*k
      !      do i=1, NbNo
      !         write(*, '(D14.7,1X,A1)', advance='no') &
      !            grains2elem%Alpha(i, j), ' '
      !      end do
      !      write(*, *)
      !   end do
      !   write(*, *)
      !end do
   
   end subroutine Grains_In_Mesh

   ! fonction qui calcule la porosite et le champ de vitesse
   ! barycentrque des grains, apres avoir recupere les vitesses
   ! des grains a la fin du pas de temps
   subroutine Compute_Average_Node_Fields

      implicit none
           
      ! variables locales
      integer :: i, j ! indices de boucle

      ! TEST timer:
      ! si les timer ont ete initialises, on lance le timer qui mesure
      ! le temps pris par GetVector 
      if (is_started_timer()) call start_timer('GetVector___________')
      
      ! on recupere les vitesses des grains, a la fin du pas 
      ! de temps : 
      call get_grains_vector_field('V____', &
              fields%grains_fields(:, 3 : nbDIME + 2))
            
      ! TEST timer:
      ! si les timer ont ete initialises, on arrete le timer qui mesure
      ! le temps pris par GetVector 
      if (is_started_timer()) call stop_timer('GetVector___________')
      
      ! pour chaque grain 
      do i=1, nb_grains
         ! on multiplie le vecteur vitesse du grain par le poids a utiliser
         fields%grains_fields(i, 3 : nbDIME + 2) = fields%grains_fields(i, 2)* &
         fields%grains_fields(i, 3 : nbDIME + 2)
      end do
   
      !do i=1, nb_grains
      !   do j=1, nbDIME + 1
      !      print*, 'fields%grains_fields(', i, ', ', j, ')=', &
      !         fields%grains_fields(i, j)
      !   end do
      !end do
           
      ! on assemble les champs aux noeuds :
      !  1 : la somme des volumes des grains voisins d'un noeud
      !      , ponderes par la valeur de sa fonction de forme aux
      !      centres de gavites de ceux-ci 
      !  2 -> nbDIME + 1 : la somme des vecteurs vitesse des 
      !      grains voisins d'un noeud, ponderes par la valeur 
      !      de sa fonction de forme aux centres de gravites de ceux-ci 
      !      et les volumes des grains
      call Assemb_Node_Field(nb_nodes, nb_ele, NbNo, &
                             nb_grains, nbDIME + 2, &
                             mesh%conn, grains2elem%Elem, &
                             grains2elem%Alpha, fields%grains_fields, &
                             fields%nodes_fields)

      !do i=1, nb_nodes
      !   do j=1, nbDIME + 2
      !      print*, 'fields%nodes_fields(', i, ', ', j, ')=', &
      !         fields%nodes_fields(i, j)
      !   end do
      !end do
            
      ! affichage compacite
      !print*, 'c=', fields%nodes_fields(:, 1)/V_loc
   
      ! on peut maintenant calculer le champ de porosite aux noeuds:
      ! pour un noeud : 
      phi = 1.d0 - fields%nodes_fields(:, 1)/V_loc

      ! si on est en 2D
      if (nbDIME == 2) then

         ! on applique une transformation qui passe la porosite 2D
         ! calculee en porosite 3D
         phi = (2.d0*phi + 1.d0)/3.d0

      end if

      !print*, 'phi=', phi

      ! on calcule le champ de vitesse barycentrique aux noeuds
         
      ! pour chaque noeud
      do i=1, nb_nodes
            
         ! si le noeud courant est voisin de grains (i.e. la somme
         ! des poids utilises pour le calcul de la vitesse moyenne
         ! aux neouds, ponderes par les valeurs des fonctions de 
         ! formes calculees aux centres d'inertie des grains 
         ! voisins est non nulle)
         if (abs(fields%nodes_fields(i, 2)) > epsilon(1.d0)) then
      
            ! on calcule la vitesse barycentrique au noeud courant: 
            !    somme des vitesses des grains voisins du noeud, ponderes
            !    par la valeur de la fonction de forme au centre 
            !    d'inertie du grain et le volume du grains / somme 
            !    des poids appliques au grains voisins du noeud (valeur
            !    de la fonction de forme au centre d'inertie du grain *
            !    volume du grain)
            u(i, :) = fields%nodes_fields(i, 3 : nbDIME + 2)/&
                      fields%nodes_fields(i, 2)
                   
         ! sinon, 
         else
           
            ! la vitesse barycentrique au noeud courant est nulle
             u(i, :) = 0.d0
               
         end if
           
      end do

      !print*, 'u='
      !do i=1, nb_nodes
      !   do j=1, nbDIME
      !      write(*, '(D14.7,1X,A1)', advance='no') u(i, j)
      !   end do
      !   write(*, *)
      !end do

   end subroutine Compute_Average_Node_Fields

   ! fonction qui calcule les vecteurs elementaires corespondant
   ! a la contribution du terme de Biot, a la fin du pas
   ! de temps
   subroutine Compute_Elementary_Biot
   
      implicit none
   
      ! variables locales
      integer :: iNod ! pour recuperer l'indice dans la numerotation globale
         ! d'un noeud de l'element
      integer :: iEl  ! indice de boucle sur les elements
      integer :: i    ! indice de boucle sur les neouds de l'element
      real(kind=8), dimension(NbNo, nbDIME) :: u_ele ! pour recuperer
         ! les vitesses barycentriques des grains a l'instant milieu
      real(kind=8), dimension(NbNo) :: F_Biot_loc ! pour recuperer
         ! le resultat du calcul du vecteur elementaire, provenant du terme de
         ! Biot, courant
      real(kind=8), dimension(NbNo) :: F_advection_loc ! pour
         ! recuperer le resultat du calcul du vecteur elementaire provenant
         ! du terme d'advection (seulement dans le cas non-linearise)
 
      ! TEST: calcul de la divergence
   
      ! on force la vitesse barycentrique des grains
      !do iNod=1, nb_nodes
      ! * div(u) = 1:
      !   u(iNod, 1) = mesh%coor(iNod, 1)
      !   u(iNod, 2) = 0.d0
      ! * div(u) = 0:
      !   u(iNod, 1) = mesh%coor(iNod, 1)
      !   u(iNod, 2) = -mesh%coor(iNod, 2)
      ! * div(u) = 1:
      !   u(iNod, 1) = 0.d0
      !   u(iNod, 2) = mesh%coor(iNod, 2)
      ! * div(u) = 2:
      !   u(iNod, 1) = mesh%coor(iNod, 1)
      !   u(iNod, 2) = mesh%coor(iNod, 2)
      !end do
   
      ! FIN TEST
  
      ! si on utilise le modele stationnaire
      if (is_steady) then
         ! on recupere les vitesses barycentriques a la fin du pas de temps
         u_m = u
      ! si on utilise le modele instationnaire
      else
         ! on calcule les vitesses barycentriques des grains, a l'insant milieu
         u_m = (1.d0 - theta_t)*u_begin + theta_t*u
      end if 

      ! pour chaque element
      do iEl = 1, nb_ele
   
         ! on recupere les vitesses barycentriques au noeuds de 
         ! l'element, a la fin du pas de temps
      
         ! pour chaque noeud de l'element
         do i = 1, NbNo
            ! on recupere l'indice dans la numerotation globale du 
            ! noeud de l'element
            iNod = mesh%conn(iEl, i)
      
            ! on recupere la vitesse barycentrique, a l'instant considere
            ! (milieu : modele instationnaire, fin du pas : modele stationnaire),
            ! du noeud courant
            u_ele(i, :) = u_m(iNod, :)
         end do
     
         ! si on est dans le cas linearise 
         if (is_linearized) then

            ! on calcule le vecteur elementaire courant, provenant du terme de
            ! Biot, a partir de la pression moyenne P0
            call compute_elementary_F_Biot_therMAILx(iTh_bdyty, iEl, &
                                                     NbNo, &
                                                     u_ele, F_Biot_loc, P0)
         ! sinon,        
         else 
            ! on calcule le vecteur elementaire courant, provenant du terme de
            ! Biot, a partir du champ de pression au debut du pas de temps
            call compute_elementary_F_Biot_therMAILx(iTh_bdyty, iEl, &
                                                     NbNo, &
                                                     u_ele, F_Biot_loc)

            ! on calcule la vitesse d'advection, produit de la porosite et de la vitesse
            ! barycentrique des grains a la fin du pas de temps 
      
            ! pour chaque noeud de l'element
            do i = 1, NbNo
               ! on recupere l'indice dans la numerotation globale du 
               ! noeud de l'element
               iNod = mesh%conn(iEl, i)
      
               ! on calcule la vitesse vitesse d'advection, a l'instant milieu,
               ! du noeud courant
               u_ele(i, :) = phi_begin(iNod)*u_m(iNod, :)
            end do

            ! on calcule le vecteur elementaire, provenant du terme d'advection 
            call compute_elementary_F_advection_therMAILx(iTh_bdyty, iEl, &
                                                          NbNo, &
                                                          u_ele, F_advection_loc)

            ! on ajoute la contribution du terme d'advection au flux eterne
            ! provenant du terme de Biot
            F_Biot_loc = F_Biot_loc + F_advection_loc

         endif

         ! on le stocke dans le vecteur elementaire, corespondant 
         ! au terme de Biot, a la fin du pas
         Biot(iEl)%ttF_loc = F_Biot_loc
   
      end do
   
      !print*, 'Biot='
      !do iEl=1, nb_ele
      !   print*, Biot(iEl)%ttF_loc
      !end do
   
   end subroutine Compute_Elementary_Biot

   ! fonction qui calcule la force hydrodynamique sur les grains,
   ! a la fin du pas de temps, a partir du champ de pression
   ! (temperature) a la fin du pas de temps et du champ de porosite
   ! a la fin du pas de temps
   subroutine Compute_Hydrodynamic_Force
         
      implicit none
         
      ! variables locales :
      real(kind=8) :: phi_cut ! porosite tenant compte du cutoff
      real(kind=8), dimension(NbNo, nbDIME) :: grad_P_ele ! pour recuperer
         ! les gradients de pressions aux noeuds, pour un element
      integer :: iEl ! indice de boucle sur les elements
      integer :: iNod ! pour recuperer le numero d'un noeud dans
                      ! la numerotation globale
      integer :: i, j, k ! indices de boucles
            
      ! TEST
      integer :: iPt
            
      ! pour chaque element
      do iEl=1, nb_ele
            
         ! on recupere le gradient de pression aux noeuds, a la fin
         ! du pas de temps
         ! N.B.: comme on surcharge un :odele de thermique pour calculer
         !       la pression, on recupere en fait un gradient de temperature
         call compute_grad_T_ele_therMAILx(iTh_bdyty, iEl, NbNo, &
                                           grad_P_ele)
               
         !print*, 'grad_P_ele='
         !do i=1, NbNo
         !  do j=1, nbDIME
         !     write(*, '(D14.7,1X,A1)', advance='no') &
         !         grad_P_ele(i, j)
         !  end do
         !  write (*, *)
         !end do
              
         ! on le stocke dans le champ defini element et par noeud
         ! a projeter pour calculer la force hydrodynamique  
               
         ! pour chaque noeud de l'element
         do i=1, NbNo
            ! on recupere son indice dans la numerotation globale
            iNod = mesh%conn(iEl, i)
               
            ! on calcule la porosite tenant compte du cutoff a ce 
            ! noeud
            phi_cut = min(phi(i), phi_max)
               
            ! on en deduit la valeur du champ a projeter pour 
            ! calculer la force hydrodynamique aux noeuds :
            ! 1/(1 -phi)*grad(P)
            fields%ele_fields(i, :, iEl) = grad_P_ele(i, :)/&
                                           (1.d0 - phi_cut) 
         end do
               
      end do
         
      !print*, 'fields%ele_fields='
      !do k=1, nb_ele
      !   do j=1, nbDIME
      !     do i=1, NbNo
      !        write(*, '(D14.7,1X,A1)', advance='no') &
      !            fields%ele_fields(i, k, j)
      !     end do
      !     write (*, *)
      !   end do
      !   write (*, *)
      !end do
         
      ! on projette le champ ainsi construit sur les grains
      call Project_Node_Field(nb_ele, NbNo, nb_grains, nbDIME, &
                              grains2elem%Elem, grains2elem%Alpha, &
                              fields%ele_fields, &
                              Fh)
      
      ! on applique la contribution des volumes des grains pour 
      ! obtenir la force appliquee sur chaque grain :
      !  -V_G(i)*(1/(1 - phi)*grad(P))((x_G)_i) 
      do i=1, nb_grains
         Fh(i, :) = -fields%grains_fields(i, 1)*Fh(i, :)
      end do
   
   end subroutine Compute_Hydrodynamic_Force
 
   ! fonction qui calcule le champ de pemreabilite de la matrice granualire, en 
   ! fonction du champ de porosite
   function kappa(phi) 

      implicit none

      ! variable d'entree :
      real(kind=8), dimension(nb_nodes) :: phi ! champ de porosite aux noeuds

      ! valeur de retour :
      real(kind=8), dimension(nb_nodes) :: kappa ! champ de permeabilite

      ! variables locales :
      real(kind=8) :: phi_cut ! porosite tenant compte du cutoff
      integer :: i ! indice de boucle

      ! pour chaque noeud
      do i=1, nb_nodes
            
         ! on appllique le cutoff
         phi_cut = min(phi(i), phi_max)
           
         ! on calcule la permeabilite, au noeud courant,
         ! par la formule de Carman-Kozeny
         kappa(i) = mean_radius*mean_radius*&
                    phi_cut*phi_cut*phi_cut/&
                    (45.d0*(1.d0 - phi_cut)*(1.d0 - phi_cut))

      end do

   end function kappa

   ! fonction qui depose le champ de proosite aux points de Gauss
   ! (dans le "field" 'SPHV') et de coefficient de diffusion aux points de
   ! Gauss (dans le "filed" 'COCO'), apres l'avoir calcule
   subroutine set_gas_grains_coupling_fields
         
      implicit none
!fd 
      integer :: inode ! indice de boucle dur les noeuds
      real(kind=8)::l_kappa(nb_nodes) ! pour calculer le champ de
         ! permeabilite au temps milieu
      real(kind=8)::i_eta ! pour calculer l'inverse d'eta

      ! si on utilise le modele stationnaire
      if (is_steady) then
         ! on recupere le champ de porosite a la fin du pas de temps
         phi_m = phi
      ! si on utilise le modele stationnaire
      else
         ! on depose le champ de porosite, au debut du pas, dans le field 'SPHV'
         call set_field_bynode(iTh_bdyty, SPHV_rank, nb_nodes, &
                 phi_begin)

         ! on calcule le champ de porosite dans la position intermediaire
         ! t_m = theta_t*t_f + (1 - theta_t)*t_i
         phi_m = theta_t*phi + (1.d0 - theta_t)*phi_begin
      end if      
      
      !print*, 'phi_m=', phi_m
           
      ! on calcule le champ de coefficient de diffusion (aussi dans la position
      ! intermediaire)

      ! si on est dans le cas linearise
      if (is_linearized) then
         ! le coefficient de diffusion est obtenu a partir de la pression
         ! moyenne : P0*kappa(phi_m)/eta
         DiCo = P0*kappa(phi_m)/eta
      ! sion
      else
         ! le coefficient de diffusion est obtenu a partir de la pression
         ! au debut du pas de temps : P_n*kappa(phi_m)/eta
               
         ! on calcule kappa(phi_m)
         l_kappa = kappa(phi_m)
         ! on calcle l'inverse d'eta
         i_eta=1.d0/eta  
         ! pour chaque noeud
         do inode=1,nb_nodes
            ! le coefficient de diffusion au noeud courant est calcule a
            ! partir de la pression (i.e. temperature) de ce noeud, au debut du pas de temps
            Dico(inode) = get_Tbegin_nodty_thermailx(iTh_bdyty,inode)*l_kappa(inode)*i_eta                 
         end do
      end if         
      !print*, 'DiCo=', DiCo
   
      ! on depose le champ de coefficient de diffusion, dans le field 'COCO'
      call set_field_bynode(iTh_bdyty, COCO_rank, nb_nodes, &
              DiCo)
         
   end subroutine set_gas_grains_coupling_fields

   ! fonction qui ajoute la contribution du terme de Biot aux flux
   ! externes par element, a l'instant milieu
   subroutine Add_Biot_To_External_Flux_By_Element

      implicit none
   
      ! variable locales :
      integer :: iEl ! indice de boucle sur les elements
   
      ! pour chaque element
      do iEl=1, nb_ele
         ! on recupere le vecteur elementraire provenant du terme 
         ! de Biot, pour l'element courant
         !F_Biot_loc = Biot(iEl)%F_loc(:, 1)
         ! on ajoute la contribution du terme de Biot, au flux
         ! externe de l'element
         call add_theta_external_flux_ele_therMAILx(iTh_bdyty, iEl, NbNo, &
                                                    Biot(iEl)%ttF_loc)
      
      end do

   end subroutine Add_Biot_To_External_Flux_By_Element
         
   ! fonction qui depose la force hydrodynamique, moyennee sur le
   ! pas de temps, sur les grains a la fin du pas de temps ; apres
   ! l'avoir calculee
   subroutine Set_Hydrodynamic_Force_By_Grain
   
      implicit none
   
      ! variables locales :     12345678901234567890123456789012345678
      character(len=38) :: IAM='GGCxx::Set_Hydrodynamic_Force_By_Grain'
      integer :: i, j ! indices de boucle
   
      ! en fonction de l'integrateur en temps
      select case(INTEGRATOR_ID)
   
         ! cas de la theta-methode :
         case(INTEGRATOR_MOREAU)
      
            ! on calcule la force hydrodynamique moyenne sur le
            ! pas de temps
            Fh_m = (1.d0 - theta)*Fh_begin + theta*Fh
      
            !print*, 'Fh_m='
            !do i=1, nb_grains
            !  do j=1, nbDIME
            !     write(*, '(D14.7,1X,A1)', advance='no') Fh_begin(i, j)
            !   end do
            !   write(*, *)
            !end do
      
            ! on la depose sur les grains
            call put_grains_vector_field('Fext_', Fh_m)
      
         ! si on ne reconnait pas l'integrateur en temps
         case default
      
            ! on affiche un message d'erreur
            call faterr(IAM, 'unsupported time integrator')
      
            ! on quitte le programme
            stop
      
      end select
   
   end subroutine Set_Hydrodynamic_Force_By_Grain
 
   ! fonction qui met a jour les donnees du module pour le prochain
   ! pas de temps
   subroutine Update_Fields

      implicit none

      ! variables locales :
      integer :: i, j ! indices de boucles pour l'affichage

      ! on met a jour la porosite
      phi_begin = phi

      ! on met a jour la vitesse barycentrique des grains 
      u_begin = u
 
      ! on met a jour la force hydrodynamique exercee sur les grains 
      Fh_begin = Fh
           
      !print*, 'phi_begin=', phi_begin
          
      !print*, 'u_begin='
      !do i=1, nb_nodes
      !   do j=1, nbDIME
      !      write(*, '(D14.7,1X,A1)', advance='no') u_begin(i, j)
      !   end do
      !   write(*, *)
      !end do
            
      !print*, 'Fh_begin='
      !do i=1, nb_grains
      !  do j=1, nbDIME
      !     write(*, '(D14.7,1X,A1)', advance='no') Fh_begin(i, j)
      !   end do
      !   write(*, *)
      !end do

   end subroutine Update_Fields

   ! fonction qui initialise le point fixe sur les vitesses des
   ! grains a la fin du pas de temps, pour ammorcer un nouveau pas
   ! de temps
   subroutine Init_Fixed_Point_Algorithm
   
      implicit none
   
      ! on recupere les vitesses des grains, au debut du pas 
      ! de temps : 
      call get_grains_vector_field('Vbeg_', v_G)
            
      ! on initialise la force hydrodynamique calculee a l'itere de
      ! point fixe precedent, a la force hydrodynamique au debut du
      ! pas de temps
      Fh_old = Fh_begin
   
   end subroutine Init_Fixed_Point_Algorithm
   
   ! fonction qui prepare la nouvelle iteration du point fixe : 
   !   * force la vitesse a la fin du pas a être la derniere 
   !     approximation calculee par le point fixe
   !   * calcule les positions corespondantes
   ! N.B. : il faut appeler increment_RBDY2, avant de faire le
   !        calcul de la cinematique des grains, pour ne pas la 
   !        fausser!
   subroutine Increment_Fixed_Point_Algorithm
   
      implicit none
   
      ! variables locales :     12345678901234567890123456789012345678
      character(len=38) :: IAM='GGCxx::Increment_Fixed_Point_Algorithm'
   
      ! en fonction de l'integrateur en temps:
      select case(INTEGRATOR_ID)
   
         ! cas de la theta-methode :
         case(INTEGRATOR_MOREAU)
      
            ! N.B.: l'idee est de donner la vitesse a la fin du pas :
            !         V <- v_G
            !       calculer les nouvelles positions corespondantes :
            !         X = Xbegin + (1 - theta)*Vbegin + theta*v_G
   
            ! on fait :
            ! X <- Xbegin + (1 - theta)*Vbegin
            ! hR <- 0
            ! (et accessoirement : V <- Vbegin)
            call increment_RBDY2
   
            ! on fait :
            ! Vfree <- v_G
            call put_grains_vector_field('Vfree', v_G)
   
            ! on fait :
            ! V <- Vfree + inv_M*hR
            ! X <- X + theta*V
            ! soit,
            ! V <- v_G
            ! X <- Xbegin + (1 - theta)*Vbegin + theta*v_G
            call comp_dof_RBDY2
   
         ! si on a pas reconnu l'interateur en temps
         case default
      
            ! on affiche un message d'erreur
            call faterr(IAM, 'unsupported time integrator')
      
            ! on quitte le porgramme
            stop
   
      end select
   
      ! on ne fait rien pour la force hydrodynamique calculee a 
      ! l'itere de point fixe precedent, car elle est calculee plus
      ! tard
   
   end subroutine Increment_Fixed_Point_Algorithm
   
   ! fonction qui calcule le carre de la norme quadratique d'un 
   ! champ de vecteurs, a nbDIME composnates, defini sur les grains
   function Compute_Square_Norm_Vector_Field(v)
   
      implicit none
   
      ! variables d'entree :
      real(kind=8), dimension(nb_grains, nbDIME), intent(in) :: v
   
      ! variable de retour :
      real(kind=8) :: Compute_Square_Norm_Vector_Field ! carre de la 
         ! norme de v
   
      ! variables locales :
      real(kind=8) :: square_norm ! pour calculer le carre de la 
         ! norme de v
      integer :: i ! inidce de boucle sur les grains
      
      ! on initialise le carre de la norme de v a 0
      square_norm = 0.d0
   
      ! pour chaque grain
      do i=1, nb_grains
   
         ! on ajoute le carre de la norme du vecteur du grain courant
         square_norm = square_norm + dot_product(v(i, :), v(i, :))
   
      end do
   
      ! on divise par le nombre de grains pour obtenir le carre de la norme de v
      ! N.B.: on est sur que le nombre de grains n'est pas nul, le test etant 
      !       effectue au debut du module
      square_norm = square_norm/nb_grains
   
      ! on renvoie le carre de la norme de v
      Compute_Square_Norm_Vector_Field = square_norm
   
   end function Compute_Square_Norm_Vector_Field
   
   ! fonction qui calcule le produit scalaire associe a la norme 
   ! quadratique d'un champ de vecteurs, a nbDIME composantes, 
   ! defini sur les grains
   function Compute_Dot_Product_Vector_Field(v, w)
   
      implicit none
   
      ! variables d'entree :
      real(kind=8), dimension(nb_grains, nbDIME), intent(in) :: v, w
   
      ! variable de retour :
      real(kind=8) :: Compute_Dot_Product_Vector_Field ! produit scalaire
         ! v.w
   
      ! variables locales :
      real(kind=8) :: dot_product_ ! pour calculer le produit scalaire
         ! v.w
   
      integer :: i ! inidce de boucle sur les grains
      
      ! on initialise le produit scalaire v.w a 0
      dot_product_ = 0.d0
   
      ! pour chaque grain
      do i=1, nb_grains
   
         ! on ajoute le prosuit scalaire des vecteurs du grain courant
         dot_product_ = dot_product_ + dot_product(v(i, :), w(i, :))
   
      end do
   
      ! on divise par le nombre de grains pour obtenir le produit
      ! scalaire v.w
      ! N.B.: on est sur que le nombre de grains n'est pas nul, le test etant 
      !       effectue au debut du module
      dot_product_ = dot_product_/nb_grains
   
      ! on renvoie le produit scalaire v.w
      Compute_Dot_Product_Vector_Field = dot_product_
   
   end function Compute_Dot_Product_Vector_Field
   
   ! fonction qui recupere la nouvelle approximation des vitesses 
   ! a la fin du pas de temps et renvoie vrai si l'algorithme 
   ! de point fixe a converge 
   function Check_Fixed_Point_Algorithm_Convergence(norm_type, tol)
   
      implicit none
   
      ! variables d'entree :
      character(len=5), intent(in) :: norm_type ! type de norme utilisee pour
         ! estimer la convergence
      real(kind=8), intent(in) :: tol ! tolerance pour estimer la convergence
   
      ! valeur de retour :
      logical :: Check_Fixed_Point_Algorithm_Convergence ! vaut "vrai"
         ! ssi on a atteint un nouvel etat d'equilibre
   
      ! variables locales :       1234567890123456789012345678901234567890123456
      character(len=46) :: IAM = 'GGCxx::Check_Fixed_Point_Algorithm_Convergence'
      real(kind=8), dimension(nb_grains, nbDIME) :: diff ! pour
         ! recuperer le champ : v_G_new - v_G ou Fh - Fh_begin, selon le
         ! type de norme demande
   
      ! en focntion du type de norme de convergence utilisee
      select case(norm_type)
   
         case('Vlocy')
      
            ! on recupere la nouvelle approximation des vitesses a la
            ! fin du pas de temps
            call get_grains_vector_field('V____', v_G_new)
   
            ! on calcule le champ : v_G_new - v_G
            diff = v_G_new - v_G
   
            print*, '| v_G_new - v_G |^2 =', &
                    Compute_Square_Norm_Vector_Field(diff)
            print*, '| v_G_new - v_G |^2 =', &
                    Compute_Dot_Product_Vector_Field(diff, diff)
            print*, 'max(tol^2*| v_G |^2, epsilon(tol))=', &
                 max(tol*tol*Compute_Square_Norm_Vector_Field(v_G), &
                     epsilon(tol))
   
            ! on a converge si :
            !   | v_G_new - v_G |/|v_G| < tol
            ! soit,
            !   | v_G_new - v_G |^2 < tol^2*|v_G|^2
            ! ou encore, pour eviter l'underflow
            !   | v_G_new - v_G |^2 < max(tol^2*|v_G|^2, epsilon(tol))
            Check_Fixed_Point_Algorithm_Convergence = &
              (Compute_Square_Norm_Vector_Field(diff) < &
                 max(tol*tol*Compute_Square_Norm_Vector_Field(v_G), &
                     epsilon(tol)))
      
         case('Force')
   
            ! on calcule le champ : Fh - Fh_old
            diff = Fh - Fh_old
   
            print*, '| Fh - Fh_old |^2 =', &
                    Compute_Square_Norm_Vector_Field(diff)
            print*, 'max(tol^2*| Fh_old |^2, epsilon(tol))=', &
                 max(tol*tol*Compute_Square_Norm_Vector_Field(Fh_old), &
                     epsilon(tol))
   
            ! on a converge si :
            !   | Fh - Fh_old |/| Fh_old | < tol
            ! soit,
            !   | Fh - Fh_old |^2 < tol^2*| Fh_old |^2
            ! ou encore, pour eviter l'underflow
            !   | Fh - Fh_old |^2 < max(tol^2*| Fh_old |^2, epsilon(tol))
            Check_Fixed_Point_Algorithm_Convergence = &
               (Compute_Square_Norm_Vector_Field(diff) < &
                  max(tol*tol*Compute_Square_Norm_Vector_Field(Fh_old), &
                      epsilon(tol)))
     
         case('NRJ__')
   
            ! on recupere la nouvelle approximation des vitesses a la
            ! fin du pas de temps
            call get_grains_vector_field('V____', v_G_new)
   
            print*, '| (Fh, v_G_new) - (Fh_old, v_G) | =', &
                    abs(Compute_Dot_Product_Vector_Field(Fh, v_G_new) - &
                        Compute_Dot_Product_Vector_Field(Fh_old, v_G))
            print*, 'max(tol*| (Fh_old, v_G) |, sqrt(epsilon(tol)))=', &
                    max(tol*abs(Compute_Dot_Product_Vector_Field(Fh_old, v_G)), &
                        sqrt(epsilon(tol)))
   
            ! on a converge si :
            !   | (Fh, v_G_new) - (Fh_old, v_G) |/| (Fh_old, v_G) | < tol
            ! soit,
            !   | (Fh, v_G_new) - (Fh_old, v_G) | < tol*| (Fh_old, v_G) |
            ! ou encore, pour eviter l'underflow
            !   | (Fh, v_G_new) - (Fh_old, v_G) | < max(tol*| (Fh_old, v_G) |, 
            !                                         sqrt(epsilon(tol)))
            Check_Fixed_Point_Algorithm_Convergence = &
               (abs(Compute_Dot_Product_Vector_Field(Fh, v_G_new) - &
                    Compute_Dot_Product_Vector_Field(Fh_old, v_G)) < &
                max(tol*Compute_Square_Norm_Vector_Field(Fh_old), &
                    sqrt(epsilon(tol))))
     
         case default
      
           ! si on n'a pas reconnu l'integrateur en temps
      
           ! on affiche un message d'erreur
              call faterr(IAM, 'unknown norm type')
   
           ! on quitte le programme
           stop
      
      end select 
   
   end function Check_Fixed_Point_Algorithm_Convergence
   
   ! fonction qui met a jour l'algorithme de point fixe :
   ! v_G <- v_G_new
   subroutine Update_Fixed_Point_Algorithm
   
      implicit none
   
      ! on fait : v_G <- v_G_new
      v_G = v_G_new
   
      ! on fait : Fh_old <- Fh
      Fh_old = Fh
   
   end subroutine Update_Fixed_Point_Algorithm

   ! declarations de fonctions permettant de realiser du post-traitement
 
   ! fonction qui calcule l'erreur sur la porosite moyenne, i.e. l'erreur
   ! entre la porosite moyenne calculee a partir des porosites aux noeuds
   ! et la porosite moyenne theorique calculee directement a partir des 
   ! volumes des grains et du volume du domaine, a la fin du pas de temps
   function compute_mean_porosity_error()

      implicit none

      ! valeur de retour :
      real(kind=8) :: compute_mean_porosity_error ! erreur relative sur la
         ! moyenne

      ! variables locales :
      real(kind=8) :: approximated_mean_porosity ! pour calculer la porosite
         ! moyenne a partir des porosites aux noeuds

      ! on calcule la porosite moyenne, a partir des porosites aux noeuds :
      ! la moyenne des porosites aux noeuds, ponderes par les parts du
      ! du volume du domaine affectes aux noeuds
      approximated_mean_porosity = sum(V_loc*phi)/sum(V_loc)

      ! on en deuit l'erreur sur la porosite moyenne (par rapport a la
      ! valeur theorique)
      compute_mean_porosity_error = (mean_phi - &
                                       approximated_mean_porosity)/ &
                                       mean_phi

   end function compute_mean_porosity_error

   ! fonction qui calcule la pression moyenne et l'ecart type sur la pression
   ! , a la fin du pas de temps
   subroutine compute_mean_pressure(P_mean, P_stdev)

      implicit none

      ! variables de sorties :
      real(kind=8), intent(out) :: P_mean ! pression moyenne
      real(kind=8), intent(out) :: P_stdev ! ecart-type sur la pression

      ! variables locales :
      real(kind=8), dimension(nb_nodes) :: P ! champ de pression, a la fin du 
         ! pas de temps
      integer :: i ! indice de boucle

      ! on recupere le champ de pression a la fin du pas de temps
      do i=1, nb_nodes        
         P(i) = get_T_nodty_therMAILx(iTh_bdyty, i)
      end do

      ! on calcule la pression moyenne
      P_mean = sum(P)/nb_nodes

      ! on en deduit l'ecart -type sur la pression
      P_stdev = sqrt(dot_product(P - P_mean, P - P_mean)/nb_nodes)

   end subroutine compute_mean_pressure

end module gas_grains_coupling
