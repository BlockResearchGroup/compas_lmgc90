      ! module fourbissant des fonctions de projections de champs entre
      ! un maillage elements finis et un nuage de points      
      module projection
      
         
         use overall ! on importe les variables communes de LMGC90 
            ! pour connaitre la dimension du probleme
         
         use utilities ! on importe les utilitaires de LMGC90
      
         ! on importe les routines de calcul des fonctions de 
         ! formes et de leurs derivees
         use a_EF
         
         ! on importe les routines d'inversion de systemes 2x2 et 3x3
         use algebra

         ! on importe les routines pour utiliser les timers de LMGC90
         use timer, only : is_started_timer, start_timer, stop_timer

         ! par defaut, tout est prive
         private
         
         ! on definit les routines publiques
         public Points_In_Mesh, &
                Project_Node_Field, &
                Assemb_Node_Field, &      
                compute_coor_ref_Q4, &
                compute_coor_ref_ele
      
      contains
      
         ! fonction qui permet de determiner dans quel element se 
         ! trouve chaque point d'un nuage de points donne
         ! hypothese : le maillage est constutue d'un seul type d'element!
         subroutine Points_In_Mesh(NbNoT, IDim, NbElm, NbNo, NbPt, &
                                  Mail, T_Fonc_Forme, XCoor, XcoorC, &
                                  OptionG, &
                                  Elem, Alpha)   
            
            implicit none
            
            ! variables d'entree : 
            integer, intent(in) :: NbNoT ! definition du nombre total de noeuds
            integer, intent(in) :: IDim ! definition de la dimension spatiale
            integer, intent(in) :: NbElm ! nombre d'elements
            integer, intent(in) :: NbNo ! nombre de noeuds par element
            integer, intent(in) :: NbPt ! nombre de points cibles
            integer, dimension(NbElm, NbNo), intent(in) :: Mail ! connectivite du maillage
            character(len=4) :: T_Fonc_Forme ! type de la fonction de
               ! forme de l'element (dans la nmenclature de a_EF) :
               ! * T_P1: pour un triangle a trois noeuds
               ! * Q_P1: pour un quadrangle a quatre noeuds
            real(kind=8), dimension(NbNoT, IDim), intent(in) :: XCoor ! coordonnees des noeuds 
                                                  ! du maillage : 
                                                  ! XCoor(iNode, iC) contient la coordonnee iC
                                                  ! du noeud d'index iNode (iC=1 : x, iC=2 : y, ...)
            real(kind=8), dimension(NbPT, IDim), intent(in) :: XCoorC ! coordonnees des noeuds cibles
                                                  ! XCoorC(iPt, iC) contient la coordonnee iC
                                                  ! du point cible d'index iPt
            character(len=7), intent(in) :: OptionG ! definit la strategie de recherche de la valeurs
                ! des fonctions de formes aux noeuds de l'element englobant un point :
                !  * 'SIMPLEX' : on utilise les routines de DD sui decoupent chaque element en
                !                simplexes pour approximer les valeurs des fonctions de formes
                !  * 'NEWTON' : utilise les routines de DD de la meme façon que dans l'option
                !               ci-dessus, mais se sert du resultat pour calculer une approximation
                !               des coordonnees de references seravant de point de depart pour trouver
                !               les vraies coordonnees dans l'element de reference par un Newton, afin
                !               de calculer les vraies valeurs des fonctions de forme
            
            ! variables de sortie :
            integer, dimension(NbPt), intent(out) :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numero de l'element dans 
                                          ! lequel se trouve le point cible d'index iPt
            real(kind=8), dimension(NbNo, NbPt), intent(out) :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
            
            ! variables locales :
            integer :: NbIterMax ! nombre maximal d'ierations pour le
                                ! le calcul des coordonnees reduites,
                                ! par la methode de Newton
            real(kind=8) :: XPrecR ! precision relative pour determiner
                                ! dans quel element se trouve chaque noeud
                                ! et la convergence de l'algorithme de calcul
                                ! des coordonnees reduites            
            character(len=8) :: Optio ! option choisie pour trouver dans quel element se
                                   ! toruve chaque noeud
            character(len=8) :: CType ! type des elements du maillage
            real(kind=8), dimension(NbNo, NbPt) :: N ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! N(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
                                          ! (calcul fait a partir des fonctions de forme de LMGC90)
            ! nom de la routine         12345678901234567890123456
            character(len=26) :: IAM = 'projection::Points_In_Mesh'
            
            integer :: i, j, k

            ! on verifie que l'option demandee a bien un sens
            
            ! si elle n'est pas reconnue
            if (OptionG .ne. 'SIMPLEX' .and. OptionG .ne. 'NEWTON_') then
            
               ! on affiche un message d'erreur
               call faterr(IAM, 'unknown option')
            
               ! on quitte le programme
               stop
               
            end if
            
            ! dans tous les cas, on fait la recherche avec le decoupage en
            ! simplexes
            
            ! on fixe les parametres de la methode de decoupage en simplexes:
            
            ! la precidion relative :
            XPrecR = 1.0D-6 
            
            ! la methode de recherche de de points dans un element
            Optio = 'SIMPLEX ' ! on decoupe l'element en simplexes
               ! (triangles, en 2D ; tetrahedres, en 3D) et on fait
               ! la recherche par simplex, avant de remonter les resultats
               ! au niveau de l'element
            
            ! on donne le type de l'element, en fonction de la fonction
            ! de forme considere
            select case(T_Fonc_Forme)
            
               case('T_P1') ! cas du triangle a 3 noeuds
               
                  CType = 'TRI3    '
               
               case('Q_P1') ! cas du quadrangle a 4 noeuds
            
                  CType = 'QUA4    '
            
               case default
                  
                  ! si on ne reconnait pas le type delement
                  ! on affiche un message d'erreur
                  call faterr(IAM, 'unknown element type')
                  ! on quitte le programme
                  stop
            
            end select
            
            ! on initialise a 0 les tableaux servant a contenir les resultats
            Elem = 0
            Alpha = 0.d0
      
            !print*, 'XCoorC='
            !do i=1, 4
            !   do j=1, nbDIME
            !      write(*, '(D14.7,1X,A1)', advance='no') XCoorC(i, j)
            !   end do
            !   write(*, *)
            !end do
  
            ! on appelle la routine de DD pour faire la recherche
            call NodesInMesh ( &
                                NBNOT , IDIM , NBELM , NBNO   , NBPT , &
                                XCOOR , MAIL , CTYPE , XPRECR , XCOORC , &
                                OPTIO , &
                                ELEM  , ALPHA )
      
            !print*, 'Elem=', Elem
            !print*, 'Alpha='
            !do j=231, 233
            !   do i=1, 4
            !      write(*, '(D14.7,1X,A1)', advance='no') Alpha(i, j)
            !   end do
            !   write(*, *)
            !end do
              
            ! si l'utilisateur l'a demande, on raffine avec la methode de Newton
            if (OptionG == 'NEWTON_') then      
      
               ! on fixe les parametres de la methode :
               
               ! le nombre maximum d'iterations a effectuer :
               NbIterMax = 10
      
               ! on prend la meme precision que pour le decoupage en simplexes
      
               ! on calcule la valeur des fonctions de formes sur les points
               ! , pour l'element conteant chaque point, a l'aide des
               ! fonctions de forme de LMGC90
               call Compute_N_Points(NbNoT, NbElm, NbNo, NbPt, &
                                     Mail, T_Fonc_Forme, NbIterMax, &
                                     XPrecR, &
                                     XCoor, XcoorC, Elem, Alpha, &
                                     N)
               
               ! les nouvelles valeurs remplacent les approxiamtions
               Alpha = N

            end if
            
         end subroutine Points_In_Mesh

         ! fonction qui permet de calculer la valeur de champs 
         ! aux points du nuage, en fonction de la valeur des champs 
         ! aux noeuds
         subroutine Project_Node_Field(NbElm, NbNo, NbPt, NbVal, &
                                       Elem, Alpha, XVal, &
                                       YVal)
          
            implicit none
            
            ! variables d'entree :
            integer, intent(in) :: NbElm ! nombre d'elements
            integer, intent(in) :: NbNo ! nombre de noeuds par element
            integer, intent(in) :: NbPt ! nombre de points cibles
            integer, intent(in) :: NbVal ! nombre de valeurs par noeuds (1 par
                          ! champ scalaire ou par coposante d'un
                          ! champ vectoriel)
            integer, dimension(NbPt), intent(in) :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numero de l'element dans 
                                          ! lequel se trouve le point cible d'index iPt
            real(kind=8), dimension(NbNo, NbPt), intent(in) :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
            real(kind=8), dimension(NbNo, NbVal, NbElm), intent(in) :: XVal ! valeur des champs par
                                          ! element, donnes par noeud, dans la numerotaion 
                                          ! locale de l'element, par indice de champ scalaire
                                          ! XVal(i, j, iEl) contient, pour l'element d'index iEl
                                          ! la valeur au noeud d'index i dans la numerotation locale
                                          ! de l'element iEl, du champ d'index j
            
            ! variable de sortie : 
            real(kind=8), dimension(NbPt, NbVal), intent(out) :: YVal ! valeur de chaque champ aux points cibles
                                          ! YVal(iPt, j) contient la valeur au point cible
                                          ! d'index iPt, du champ d'index j
 
            ! variables locales :
            integer :: iEl, iNloc, iNglob, iPt ! indices de boucle
       
            ! on initialise a 0 les champs sur les points cibles
            YVal = 0.d0
            
            ! pour chaque point cible
            do iPt=1, NbPt
               
               ! on recupere le numero de l'element contenant le point courant
               iEl = Elem(iPt) 
               
               ! si aucun element ne contient le point courant, on passe au
               ! suivant (car le point courant n'a aucune influence)
               if (iEl == 0) cycle

               ! pour chaque noeud de l'element
               do iNloc=1, NbNo
               
                  ! on ajoute la contribution du noeud courant au point cible
                  ! courant
                  ! N.B.: on traite tous les champs d'un coup
                  YVal(iPt, :) = YVal(iPt, :) + &
                                    Alpha(iNloc, iPt)*XVal(iNloc, :, iEl)
               
               end do     
            
            end do
         
            ! N.B.: on a remplace l'appel a la routine de David par sa
            ! reecriture pour :
            !    * eviter un appel a LAPACK
            !    * gerer le cas du point qui n'est dans aucun element
            ! on appelle la routine de DD pour faire la projection
            !call ProjectChamno ( & 
            !                    NBPT , NBNO  , NBELM , NBVAL , &
            !                    ELEM , ALPHA , XVAL  , &
            !                    YVAL )

         end subroutine Project_Node_Field
      
         ! fonction qui permet de calculer la valeur de champs 
         ! aux noeuds, en fonction de la valeur des champs 
         ! aux points du nuage
         subroutine Assemb_Node_Field(NbNoT, NbElm, NbNo, NbPt, NbVal, &
                                      Mail, Elem, Alpha, YVal, YChPo)
         
            implicit none
            
            ! variables d'entree :
            integer, intent(in) :: NbNoT ! definition du nombre total de noeuds
            integer, intent(in) :: NbElm ! nombre d'elements
            integer, intent(in) :: NbNo ! nombre de noeuds par element
            integer, intent(in) :: NbPt ! nombre de points cibels (i.e. portants le champs a interpoler)
            integer, intent(in) :: NbVal ! nombre de valeurs par noeuds (1 par
            integer, dimension(NbElm, NbNo), intent(in) :: Mail ! connectivite du maillage
            integer, dimension(NbPt), intent(in) :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numero de l'element dans 
                                          ! lequel se trouve le point cible d'index iPt
            real(kind=8), dimension(NbNo, NbPt), intent(in) :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
            real(kind=8), dimension(NbPt, NbVal), intent(in) :: YVal ! valeur de chaque champ aux points cibles
                                          ! YVal(iPt, j) contient la valeur au point cible
                                          ! d'index iPt, du champ d'index j
            
            ! variable de sortie :
            real(kind=8), dimension(NbNoT, NbVal), intent(out) :: YChPo ! pour donner les valeurs nodales
                                           ! de chaque champ
                                           ! YChPo(iNode, j) contient la valeur au noeud d'index
                                           ! iNode (dans la numerotation global), du champ j
         
            ! variables locales :
            integer :: iEl, iNloc, iNglob, iPt ! indices de boucle
       
            ! on initialise a 0 le champ aux noeuds
            YChPo = 0.d0
         
            ! pour chaque point cible
            do iPt=1, NbPt
               
               ! on recupere le numero de l'element contenant le point courant
               iEl = Elem(iPt) 
               
               ! si aucun element ne contient le point courant, on passe au
               ! suivant (car le point courant n'a aucune influence)
               if (iEl == 0) cycle

               ! pour chaque noeud de l'element
               do iNloc=1, NbNo
               
                  ! on recupere le numero du noeud dans la numerotation globale
                  iNglob = Mail(iEl, iNloc)
               
                  ! on ajoute la contribution du point cible courant a ce noeud
                  ! N.B.: on traite tous les champs d'un coup
                  YChPo(iNglob, :) = YChPo(iNglob, :) + &
                                     Alpha(iNloc, iPt)*YVal(iPt, :)
               
               end do     
            
            end do
         
         end subroutine Assemb_Node_Field
      
         ! fonction qui calcule la valeur des fonctions de forme  
         ! en chaque point d'un nuage de points donne, en connaissant le
         ! numero de l'element conteant chque point
         ! hypothese : le maillage est constutue d'un seul type d'element!
         subroutine Compute_N_Points(NbNoT, NbElm, NbNo, NbPt, &
                                     Mail, T_Fonc_Forme, NbIterMax, &
                                     XPrecR, &
                                     XCoor, XcoorC, Elem, Alpha, &
                                     N)   
            
            implicit none
            
            ! variables d'entree : 
            integer, intent(in) :: NbNoT ! definition du nombre total de noeuds
            integer, intent(in) :: NbElm ! nombre d'elements
            integer, intent(in) :: NbNo ! nombre de noeuds par element
            integer, intent(in) :: NbPt ! nombre de points cibles
            integer, dimension(NbElm, NbNo), intent(in) :: Mail ! connectivite du maillage
            character(len=4) :: T_Fonc_Forme ! type de la fonction de
               ! forme de l'element (dans la nmenclature de a_EF) :
               ! * T_P1: pour un triangle a trois noeuds
               ! * Q_P1: pour un quadrangle a quatre noeuds
            integer, intent(in) :: NbIterMax ! nombre maximum d'iterations pour l'algorithme de Newton
               ! seravnt  calculer les coordonnees reduites
            real(kind=8), intent(in) :: XPrecR ! precision relative pour determiner
                                ! la convergence de l'algo de calcul les coordonnees
                                ! reduite d'un point dans un element
            real(kind=8), dimension(NbNoT, nbDIME), intent(in) :: XCoor ! coordonnees des noeuds 
                                                  ! du maillage : 
                                                  ! XCoor(iNode, iC) contient la coordonnee iC
                                                  ! du noeud d'index iNode (iC=1 : x, iC=2 : y, ...)
            real(kind=8), dimension(NbPT, nbDIME), intent(in) :: XCoorC ! coordonnees des noeuds cibles
                                                  ! XCoorC(iPt, iC) contient la coordonnee iC
                                                  ! du point cible d'index iPt
            integer, dimension(NbPt), intent(in) :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numero de l'element dans 
                                          ! lequel se trouve le point cible d'index iPt
            real(kind=8), dimension(NbNo, NbPt), intent(in) :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
                                          ! (calcul fait dans les routines de DD : aproximation par
                                          !  decoupage de l'element en simplexes)
            
            ! variable de sortie :
            real(kind=8), dimension(NbNo, NbPt), intent(out) :: N ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! N(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, ou i est l'index dans la numerotation locale
                                          ! de l'element contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
            
            ! variables locales :       1234567890123456789012345678
            real(kind=8), dimension(nbDIME) :: CoorC ! pour recuperer les 
               ! coordonnees d'un point cible
            real(kind=8), dimension(:, :), pointer :: Coor_Ref_N ! pour
               ! recuperer les coordonnees des noeuds
               ! dans l'element de reference
            real(kind=8), dimension(NbNo, nbDIME) :: CoorEle ! coordonnees
               ! des noeuds de l'element contenant le point cible courant
            real(kind=8), dimension(nbDIME) :: CoorRef0 ! approximation
               ! des coordonnees reduites du point cible courant, obtenue
               ! a partir des routines de DD 
            real(kind=8), dimension(nbDIME) :: CoorRef ! coordonnees 
               ! reduites du point cible courant 
            real(kind=8), dimension(:), pointer :: NRef ! pour recuperer 
               ! les valeurs des fonctions de formes, en CoorRef
            integer :: iEl ! pour recuperer le numero de l'element courant
            integer :: iPt, iNod ! indices de boucle 
       
            integer :: i, j
   
            ! pour chaque point cible
            do iPt=1, NbPt
               
               ! on recupere le numero de l'element contenant le point courant
               iEl = Elem(iPt) 
               
               ! si aucun element ne contient le point courant
               if (iEl == 0) then
                  ! on annulle les valeurs des fonctions de formes aux noeuds
                  ! de l'element qui contient le point courant, puisqu'il 
                  ! n'existe pas!
                  N(:, iPt) = 0.d0
                  ! on passe au point suivant
                  cycle
               end if
               
               ! on recupere les coordonnees des noeuds de cet element:
               do iNod=1, NbNo
                   CoorEle(iNod, :) = XCoor(Mail(iEl, iNod), :)
               end do
               
               ! on initialise le pointeur Coor_Ref_N a 0
               nullify(Coor_Ref_N)
   
               ! on recupere les coordonnees dans l'element de reference
               ! des noeuds dans cet element
               call get_Nodes_Coor_Ref(T_Fonc_Forme, Coor_Ref_N)
               
               ! on en deduit une approximation des coordonnees reduites
               ! du point cible courant, a partir des valeurs des fonctions
               ! de formes approximees par le decoupage en simplexes de DD
               CoorRef0 = matmul(Alpha(:, iPt), transpose(Coor_Ref_N))
               
               ! on calcule les coordonnees reduites du point cible, en
               ! partant de l'approximation ci-dessus
               CoorC = XCoorC(iPt, :)
   
               call compute_coor_ref_ele(T_Fonc_Forme, CoorEle, NbNo, &
                                    CoorC, NbIterMax, XPrecR, &
                                    CoorRef0, &
                                    CoorRef)
               
               ! on initialise le pointeur NRef a 0
               nullify(NRef)
            
               ! on recupere les valeurs des fonctions de formes, en CoorRef
               call FONCT_FORME(T_Fonc_Forme, CoorRef, NRef)
 
               ! on les stocke dans N
               N(:, iPt) = NRef
               
               ! on desalloue l'espace memoire occupe pour stocker les 
               ! coordonnees de reference, pour preparer la prochaine
               ! iteartion
               deallocate(Coor_Ref_N)
   
               ! on desalloue l'espace memoire occupe par NRef
               ! , pour rpeparer le prochain appel
               deallocate(NRef)
                    
            end do
      
         end subroutine Compute_N_Points
      
         ! fonction qui calcule les coordonnees, dans l'element de reference, 
         ! d'un point appartenant a un element deforme
         subroutine compute_coor_ref_ele(T_Fonc_Forme, coor_ele, NbNo, &
                                         coor, nb_iter_max, tol, &
                                         coor_ref0, coor_ref)
      
            implicit none
            
            ! variables d'entree :
            character(len=4) :: T_Fonc_Forme ! type de la fonction de
               ! forme de l'element (dans la nmenclature de a_EF) :
               ! * T_P1: pour un triangle a trois noeuds
               ! * Q_P1: pour un quadrangle a quatre noeuds
            integer :: NbNo ! nombre de noeuds dans l'element
            real(kind=8), dimension(NbNo, nbDIME), intent(in) :: coor_ele 
               ! coordonnees des noeuds de l'element reel
               ! coor_ele(i, :) : coordonnees du noeud i
            real(kind=8), dimension(nbDIME), intent(in) :: coor ! coordonnees
               ! d'un point dans l'element reel
               ! coor = (x, y), en 2D
               !        (x, y, z), en 3D
            real(kind=8), dimension(nbDIME), intent(in) :: coor_ref0 ! coordonnees
               ! dans l'element de reference, servant de point de depar pour
               ! la recherche des coordonnees reduites de l'element courant
               ! coor_ref0 = (ksi, eta), en 2D
               !             (ksi, eta, zeta), en 3D
            integer :: nb_iter_max ! nombre maximal d'iteration
            real(kind=8) :: tol ! tolerance relative pour determiner si 
               ! l'algo a converge
            
            ! variable de sortie :
            real(kind=8), dimension(nbDIME), intent(out) :: coor_ref ! coordonnees
               ! corsepondant, dans l'element de reference, au point repere par
               ! coor dans l'element deforme
               ! coor_ref = (ksi, eta), en 2D
               !            (ksi, eta, zeta), en 3D
            
            ! variables locales :
            real(kind=8), dimension(nbDIME) :: coor_ref_new ! pour stocker 
               ! la nouvelle valeur des coordonnees de reference 
               ! construites par la methode de Newton
            real(kind=8), dimension(nbDIME) :: delta_coor_ref ! pour stocker
               ! l'increment entre les coordonnees de references actuelles
               ! et la prochaine aproxiamtion
            real(kind=8), dimension(nbDIME) :: G_ ! pour claculer 
               ! le residu, de façon generique
            real(kind=8), dimension(nbDIME, nbDIME) :: dG_ ! pour claculer
               ! la derivee du residu de façon generique
            real(kind=8), dimension(nbDIME) :: RHS ! pour stocker le second
               ! membre du systeme
            integer :: k ! pour compter les iterations effectuees
            integer :: i, j ! pour l'affichage de dG_
            
            ! on part du point de depart passe en argument
            coor_ref = coor_ref0
            
            ! on initialise la nombre d'iterations
            k = 0
            
            ! tant qu'on pas converge
            do
            
               ! on incremente le nombre d'iterations effectuees
               k = k + 1
               
               ! on calcule le residu pour les coordonnees de reference
               ! courantes
               call G_ele(T_Fonc_Forme, coor_ele, NbNo, coor, coor_ref, G_)
               
               !print*, 'G_=', G_
               
               ! on en deduit le second membre pour la methode de Newton
               RHS = -G_
               
               ! on calcule la derivee du residu pour les coordonnees de
               ! reference courantes
               call dG_ele(T_Fonc_Forme, coor_ele, NbNo, coor_ref, dG_)
               
               !print*, 'dG_='
               !do i=1, 2
               !   do j=1, 2
               !      write(*, '(D14.7,1X,A1)', advance='no') &
               !         dG_(i, j), ' '
               !   end do
               !   write(*, *)
               !end do
               
               ! on resoud le systeme :
               ! dG_*delta_coor_ref = RHS
               ! pour obtenir l'increment entre les nouvelles coordonnees
               ! et les coordonnees courantes
               call solve_linear_pb(dG_, RHS, delta_coor_ref)
               
               ! on calcule les nouvelles coordonnees de reference
               coor_ref_new = coor_ref + delta_coor_ref
               
               ! si on converge ou que le nombre maximal d'iterations 
               ! a ete atteint
               if (dot_product(delta_coor_ref, delta_coor_ref) < &
                   tol*tol*dot_product(coor_ref_new, coor_ref_new) .or. &
                   k > nb_iter_max) then
                   
                   ! on sort de la boucle
                   exit
                   
               end if
               
               ! sinon, on passe a l'itaration suivante
               coor_ref = coor_ref_new
               
            end do
            
            ! on affiche un warning si la method n'a pas converge
            if (k > nb_iter_max) then
            
               print*, 'Warning: Newton n a pas converge!'
            
            end if
            
            ! on recupere les coordonnees de references finales
            coor_ref = coor_ref_new
            
            !print*, 'k=', k
            
         end subroutine compute_coor_ref_ele
      
         ! fonction qui calcule le residu :
         ! en 2D: G(ksi, eta) = (x, y) - F(ksi, eta)
         ! en 3D: G(ksi, eta, zeta) = (x, y, z) - F(ksi, eta, zeta)
         ! ou, F est la fonction qui calcule les cordonnees dans
         ! un element deforme, a partir des coordonnees dans l'element
         ! de reference 
         subroutine G_ele(T_Fonc_Forme, coor_ele, NbNo, coor, coor_ref, G_)
         
            implicit none
            
            ! variables d'entree : 
            character(len=4) :: T_Fonc_Forme ! type de la fonction de
               ! forme de l'element (dans la nmenclature de a_EF) :
               ! * T_P1: pour un triangle a trois noeuds
               ! * Q_P1: pour un quadrangle a quatre noeuds
            integer :: NbNo ! nombre de noeuds dans l'element
            real(kind=8), dimension(NbNo, nbDIME), intent(in) :: coor_ele 
               ! coordonnees des noeuds de l'element reel
               ! coor_ele(i, :) : coordonnees du noeud i
            real(kind=8), dimension(nbDIME), intent(in) :: coor ! coordonnees
               ! d'un point dans l'element reel
               ! coor = (x, y), en 2D
               !        (x, y, z), en 3D
            real(kind=8), dimension(nbDIME), intent(in) :: coor_ref ! coordonnees
               ! du point dans l'element de reference, pour lequel on cherche la
               ! valeur du residu
               ! coor_ref = (ksi, eta), en 2D
               !            (ksi, eta, zeta), en 3D
         
            ! variable de sortie :
            real(kind=8), dimension(nbDIME), intent(out) :: G_ ! valeur du residu
               ! au point de coordonnees coor_ref dans l'element de reference
         
            ! variables locales :
            real(kind=8), dimension(:), pointer :: N ! pour recuperer 
               ! les valeurs des fonctions de formes, en coor_ref
            integer :: i ! indice de boucle
         
            ! on initialise le pointeur N
            nullify(N)
            
            ! on recupere les valeurs des fonctions de formes, en coor_ref
            call FONCT_FORME(T_Fonc_Forme, coor_ref, N)
         
            ! on en deduit la valeur du residu au point de coordonnees
            ! coor_ref, dans l'element de reference
            
            ! on initilaise le residu aux coordonnees du point dans 
            ! l'element reel
            G_ = coor
            
            ! pour chaque noeud de l'element
            do i=1, NbNo
         
               ! on ajoute la contribution du noeud courant
               G_ = G_ - N(i)*coor_ele(i, :)
            
            end do
         
            ! on desalloue l'espace memoire alloue pour le calcul des 
            ! fonctions de forme
            deallocate(N)
            nullify(N)
         
         end subroutine G_ele
         
         ! fonction qui calcule la derivee du residu :
         ! en 2D: G(ksi, eta) = (x, y) - F(ksi, eta)
         ! en 3D: G(ksi, eta, zeta) = (x, y, z) - F(ksi, eta, zeta)
         ! ou, F est la fonction qui calcule les cordonnees dans
         ! un element deforme, a partir des coordonnees dans l'element
         ! de reference 
         subroutine dG_ele(T_Fonc_Forme, coor_ele, NbNo, coor_ref, dG_)
         
            ! variables d'entree : 
            character(len=4) :: T_Fonc_Forme ! type de la fonction de
               ! forme de l'element (dans la nmenclature de a_EF) :
               ! * T_P1: pour un triangle a trois noeuds
               ! * Q_P1: pour un quadrangle a quatre noeuds
            integer :: NbNo ! nombre de neouds dans l'element
            real(kind=8), dimension(NbNo, nbDIME), intent(in) :: coor_ele 
               ! coordonnees des noeuds de l'element reel
               ! coor_ele(i, :) : coordonnees du noeud i
            real(kind=8), dimension(nbDIME), intent(in) :: coor_ref ! coordonnees
               ! du point dans l'element de reference, pour lequel on cherche la
               ! valeur du residu
               ! coor_ref = (ksi, eta), en 2D
               !            (ksi, eta, zeta), en 3D
         
            ! variable de sortie :
            real(kind=8), dimension(nbDIME, nbDIME), intent(out) :: dG_ 
               ! valeur de la derivee du residu au point de coordonnees
               ! coor_ref dans l'element de reference
         
            ! variables locales :
            real(kind=8), dimension(:, :), pointer :: DN ! pour recuperer 
               ! les valeurs des derivees des fonctions de formes, en coor_ref
            integer :: i, j ! indices de boucle
         
            ! on initialise le pointeur DN
            nullify(DN)
      
            ! on calcule les derivees des fonctions de forme, au point
            ! de coordonees coor_ref, dans l'element de reference
            call DERIVE_FORME(T_Fonc_Forme, coor_ref, DN)
            
            ! on initilaise la derivee du residu a 0
            dG_ = 0.d0
            
            ! pour chaque composante du residu
            do i=1, nbDIME
               ! pour chaque composante des coordonnees de reference
               do j=1, nbDIME
                  
                  ! on ajoute la contribution du noeud courant
                  dG_(i, j) = dG_(i, j) - &
                              dot_product(DN(j, :), coor_ele(:, i))
            
               end do
            
            end do
            
            ! Rq: dG serait-il l'oppose du transpose de la jacobienne?
         
            ! on desalloue l'espace memoire alloue pour le calcul des 
            ! fonctions de forme
            deallocate(DN)
            nullify(DN)
      
         end subroutine dG_ele
      
         ! fonction qui resoud un systeme lineaire de taille 2 ou 3
         subroutine solve_linear_pb(A, b, x)
         
            implicit none
            
            ! variables d'entree : 
            real(kind=8), dimension(nbDIME, nbDIME), intent(in) :: A ! matrice du syteme
            real(kind=8), dimension(nbDIME), intent(in) :: b ! second membre
            
            ! variables de sortie :
            real(kind=8), dimension(nbDIME), intent(out) :: x ! solution
         
            ! variables locales :     123456789012345678901234567890123
            character(len=33) :: IAM='mod_a_projection::solve_linear_pb'
            real(kind=8) :: det_A ! determinant de A
            real(kind=8), dimension(nbDIME, nbDIME) :: inv_A ! inverse de A
            integer :: err ! pour appeler la fonction d'inversion de matrice
            
            ! on copie A dans inv_A, pour que l'appel de la fonction d'inversion
            ! de matrice ne touche pas A
            inv_A = A
            
            ! on appelle la routine d'inversion de matrice 
            
            ! en fonction de la dimension
            select case(nbDIME)
            
               case(2) ! cas 2D
            
                  call inverse22(err, inv_A, IAM)
         
               case(3) ! cas 3D
               
                  call inverse33(err, inv_A, IAM)
               
               ! si on ne rconnait pas la dimension
               case default
               
                  ! on affiche un message d'erreur
                  call faterr(IAM, 'unknown space dimension!')
               
                  ! on quitte le programme
                  stop
               
            end select
              
            ! on en deduit la solution du systeme
            x = matmul(inv_A, b)
         
         end subroutine solve_linear_pb
      
!----- fonctions particularisant les fonctions servant au calcul des 
!----- coordonnees reduites au cas du Q4
            
         ! fonction qui calcule les coordonnees, dans le Q4 de reference, 
         ! d'un point appartenant a un Q4 deforme
         subroutine compute_coor_ref_Q4(coor_ele, coor, nb_iter_max, tol, coor_ref)
      
            implicit none
            
            ! variables d'entree :
            real(kind=8), dimension(4, 2), intent(in) :: coor_ele
               ! coordonnees des noeuds du Q4 (deforme)
               ! coor_ele(i, 1) : x, pour le noeud i 
               ! coor_ele(i, 2) : y, pour le noeud i     
            real(kind=8), dimension(2), intent(in) :: coor ! coordonnees
               ! d'un point dans le Q4 deforme
            integer :: nb_iter_max ! nombre maximal d'iteration
            real(kind=8) :: tol ! tolerance relative pour determiner si 
               ! l'algo a converge
            
            ! variable de sortie :
            real(kind=8), dimension(2), intent(out) :: coor_ref ! coordonnees 
               ! du point considere, dans l'element de reference
               
            ! variables locales :
            real(kind=8), dimension(2, 4) :: coeff ! matrice
               ! contenant les coefficients permettant de calculer F_Q :
               ! F_Q = ( (a1*ksi + b1*eta + c1*ksi*eta + d1)
               !         (a2*ksi + b2*eta + c2*ksi*eta + d2) )
            real(kind=8), dimension(2) :: coor_ref_new ! pour stocker 
               ! la nouvelle valeur des coordonnees de reference 
               ! construites par la methode de Newton
            real(kind=8), dimension(2) :: delta_coor_ref ! pour stocker
               ! l'increment entre les coordonnees de references actuelles
               ! et la prochaine aproxiamtion
            real(kind=8), dimension(2) :: G ! pour stocker le residu
            real(kind=8), dimension(2, 2) :: dG ! pour stocker la derivee
               ! du residu (matrice des iterations)
            real(kind=8), dimension(2) :: RHS ! pour stocker le second
               ! membre du systeme
            integer :: k ! pour compter les iterations effectuees
            
            ! on calcule les coefficients permattant de calculer F_Q :
            
            ! a1 = (-x1 + x2 + x3 - x4) / 4
            coeff(1, 1) = 0.25*(-coor_ele(1, 1) + coor_ele(2, 1) + &
                                 coor_ele(3, 1) - coor_ele(4, 1))
                                 
            ! b1 = (-x1 - x2 + x3 + x4)/4
            coeff(1, 2) = 0.25*(-coor_ele(1, 1) - coor_ele(2, 1) + &
                                 coor_ele(3, 1) + coor_ele(4, 1))
                                 
            ! c1 = ( x1 - x2 + x3 - x4)/4
            coeff(1, 3) = 0.25*(coor_ele(1, 1) - coor_ele(2, 1) + &
                                coor_ele(3, 1) - coor_ele(4, 1))
            
            ! d1 = ( x1 + x2 + x3 + x4)/4
            coeff(1, 4) = 0.25*(coor_ele(1, 1) + coor_ele(2, 1) + &
                                coor_ele(3, 1) + coor_ele(4, 1))
                                
            ! a2 = (-y1 + y2 + y3 - y4) / 4
            coeff(2, 1) = 0.25*(-coor_ele(1, 2) + coor_ele(2, 2) + &
                                 coor_ele(3, 2) - coor_ele(4, 2))
                                 
            ! b2 = (-y1 - y2 + y3 + y4)/4
            coeff(2, 2) = 0.25*(-coor_ele(1, 2) - coor_ele(2, 2) + &
                                 coor_ele(3, 2) + coor_ele(4, 2))
                                 
            ! c2 = ( y1 - y2 + y3 - y4)/4
            coeff(2, 3) = 0.25*(coor_ele(1, 2) - coor_ele(2, 2) + &
                                coor_ele(3, 2) - coor_ele(4, 2))
            
            ! d2 = ( y1 + y2 + y3 + y4)/4
            coeff(2, 4) = 0.25*(coor_ele(1, 2) + coor_ele(2, 2) + &
                                coor_ele(3, 2) + coor_ele(4, 2))
            
            !print*, 'coeff='
            !do i=1, 2
            !   do j=1, 4
            !      write(*, '(D14.7,1X,A1)', advance='no') &
            !         coeff(i, j), ' '
            !   end do
            !   write(*, *)
            !end do
            
            ! on part du point (0, 0), centre de l'element de reference
            coor_ref = (/0.d0 , 0.d0/)
            
            ! on initialise la nombre d'iterations
            k = 0
            
            ! tant qu'on pas converge
            do
            
               ! on incremente le nombre d'iterations effectuees
               k = k + 1
               
               ! on calcule le residu pour les coordonnees de reference
               ! courantes
               call G_Q4(coeff, coor, coor_ref, G)
               
               ! on en deduit le second membre pour la methode de Newton
               RHS = -G
               
               !print*, 'dG='
               !do i=1, 2
               !   do j=1, 2
               !      write(*, '(D14.7,1X,A1)', advance='no') &
               !         dG(i, j), ' '
               !   end do
               !   write(*, *)
               !end do
            
               ! on resoud le systeme :
               ! dG*delta_coor_ref = RHS
               ! pour obtenir l'increment entre les nouvelles coordonnees
               ! et les coordonnees courantes
               call solve_linear_2(dG, RHS, delta_coor_ref)
               
               ! on calcule les nouvelles coordonnees de reference
               coor_ref_new = coor_ref + delta_coor_ref
               
               ! si on converge ou que le nombre maximal d'iterations 
               ! a ete atteint
               if (dot_product(delta_coor_ref, delta_coor_ref) < &
                   tol*dot_product(coor_ref_new, coor_ref_new) .or. &
                   k > nb_iter_max) then
                   
                   ! on sort de la boucle
                   exit
                   
               end if
               
               ! sinon, on passe a l'itaration suivante
               coor_ref = coor_ref_new
               
            end do
            
            ! on affiche un warning si la method n'a pas converge
            if (k > nb_iter_max) then
            
               print*, 'Warning: Newton n a pas converge!'
            
            end if
            
            ! on recupere les coordonnees de references finales
            coor_ref = coor_ref_new
            
         end subroutine compute_coor_ref_Q4

         ! fonction qui calcule le residu :
         ! G_Q4(ksi, eta) = (x, y) - F_Q(ksi, eta)
         ! ou, F_Q est la fonction qui calcule les cordonnees dans
         ! un Q4 deforme, a partir des coordonnees dans le Q4 de reference 
         subroutine G_Q4(coeff, coor, coor_ref, G)
         
            implicit none
            
            ! variables d'entree : 
            real(kind=8), dimension(2, 4), intent(in) :: coeff ! matrice
               ! contenant les coefficients permettant de calculer F_Q :
               ! F_Q = ( (a1*ksi + b1*eta + c1*ksi*eta + d1)
               !         (a2*ksi + b2*eta + c2*ksi*eta + d2) )
            real(kind=8), dimension(2), intent(in) :: coor ! coordonnees
               ! d'un point dans l'element deforme
            real(kind=8), dimension(2), intent(in) :: coor_ref ! coordonnees
               ! du point dans l'element de reference, pour lequel on cherche la
               ! valeur du residu
         
            ! variable de sortie :
            real(kind=8), dimension(2), intent(out) :: G ! valeur du residu
               ! au point de coordonnees coor_ref dans l'element de reference
         
            ! G_Q4_1 = x - (a1*ksi + b1*eta + c1*ksi*eta + d1)
            G(1) = coor(1) - (coeff(1, 1)*coor_ref(1) + &
                              coeff(1, 2)*coor_ref(2) + &
                              coeff(1, 3)*coor_ref(1)*coor_ref(2) + &
                              coeff(1, 4))
            ! G_Q4_2 = x - (a2*ksi + b2*eta + c2*ksi*eta + d2)
            G(2) = coor(2) - (coeff(2, 1)*coor_ref(1) + &
                              coeff(2, 2)*coor_ref(2) + &
                              coeff(2, 3)*coor_ref(1)*coor_ref(2) + &
                              coeff(2, 4))
         
         end subroutine G_Q4
         
         ! fonction qui calcule la derivee du residu :
         ! G_Q4(ksi, eta) = (x, y) - F_Q(ksi, eta)
         ! ou, F_Q est la fonction qui calcule les cordonnees dans
         ! un Q4 deforme, a partir des coordonnees dans le Q4 de reference 
         subroutine dG_Q4(coeff, coor_ref, dG)

            implicit none
            
            ! variables d'entree : 
            real(kind=8), dimension(2, 4), intent(in) :: coeff ! matrice
               ! contenant les coefficients permettant de calculer F_Q :
               ! F_Q = ( (a1*ksi + b1*eta + c1*ksi*eta + d1)
               !         (a2*ksi + b2*eta + c2*ksi*eta + d2) )
            real(kind=8), dimension(2), intent(in) :: coor_ref ! coordonnees
               ! du point dans l'element de reference, pour lequel on cherche la
               ! valeur du residu
         
            ! variable de sortie :
            real(kind=8), dimension(2, 2), intent(out) :: dG ! valeur de la
               ! derivee du residu au point de coordonnees coor_ref dans 
               ! l'element de reference
         
            ! dG_Q4_1/dksi = - (a1 + c1*eta)
            dG(1, 1) = - (coeff(1, 1) + coeff(1, 3)*coor_ref(2)) 
            
            ! dG_Q4_1/deta = - (b1 + c1*ksi)
            dG(1, 2) = - (coeff(1, 2) + coeff(1, 3)*coor_ref(1))
            
            ! dG_Q4_2/dksi = - (a2 + c2*eta)
            dG(2, 1) = - (coeff(2, 1) + coeff(2, 3)*coor_ref(2))
            
            ! dG_Q4_2/deta = - (b2 + c2*ksi)
            dG(2, 2) = - (coeff(2, 2) + coeff(2, 3)*coor_ref(1))
      
         end subroutine dG_Q4
         
         ! fonction qui resoud un systeme lineaire de taille 2
         subroutine solve_linear_2(A, b, x)
         
            implicit none
            
            ! variables d'entree : 
            real(kind=8), dimension(2, 2), intent(in) :: A ! matrice du syteme
            real(kind=8), dimension(2), intent(in) :: b ! second membre
            
            ! variables de sortie :
            real(kind=8), dimension(2), intent(out) :: x ! solution
         
            ! variables locales :
            real(kind=8) :: det_A ! determinant de A
            real(kind=8), dimension(2, 2) :: inv_A ! inverse de A
            
            ! calcul du determinant de A
            det_A=A(1,1)*A(2,2) - A(1,2)*A(2,1)
         
            ! calcul de l'inverse de A
            if (abs(det_A) < 1.E-10) then
            
               print*, 'Matrice non inversible!'
               
               stop
               
            end if
             
            inv_A(1,:)=(/  A(2,2)/det_A ,-A(1,2)/det_A /)
            inv_A(2,:)=(/ -A(2,1)/det_A , A(1,1)/det_A /)
         
            ! on en deduit la solution du systeme
            x = matmul(inv_A, b)
         
         end subroutine solve_linear_2         
      
      end module projection
