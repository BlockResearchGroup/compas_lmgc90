      program test
      
         implicit none
         
         ! défintions des tableaux utilisés pour le test
         integer, dimension(:, :), allocatable :: Mail ! connectivité du maillage
         real(kind=8), dimension(:, :), allocatable :: XCoor ! coordonnées des noeuds 
                                                  ! du maillage : 
                                                  ! XCoor(iNode, iC) contient la coordonnée iC
                                                  ! du noeud d'index iNode (iC=1 : x, iC=2 : y, ...)
         real(kind=8), dimension(:, :), allocatable :: XCoorC ! coordonnées des noeuds cibles
                                                  ! XCoorC(iPt, iC) contient la coordonnée iC
                                                  ! du point cible d'index iPt
         integer, dimension(:), allocatable :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numéro de l'élément dans 
                                          ! lequel se trouve le point cible d'index iPt
         real(kind=8), dimension(:, :), allocatable :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, où i est l'index dans la numérotation locale
                                          ! de l'élément contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
         real(kind=8), dimension(:, :, :), allocatable :: XVal ! valeur des champs par
                                          ! element, donnes par noeud, dans la numerotaion 
                                          ! locale de l'element, par indice de champ scalaire
                                          ! XVal(i, j, iEl) contient, pour l'élément d'index iEl
                                          ! la valeur au noeud d'index i dans la numérotation locale
                                          ! de l'élément iEl, du champ d'index j
         real(kind=8), dimension(:, :), allocatable :: YVal ! valeur de chaque champ aux points cibles
                                          ! YVal(iPt, j) contient la valeur au point cible
                                          ! d'index iPt, du champ d'index j
          
         ! définitions des variables uilisées pour transformer la donnée du champ par noeud
         ! en champ par élément
         integer, dimension(:), allocatable :: LPt1 ! liste des noeuds du maillage concernés par la
                                           ! transformation (il s'agit d'un ensemble, l'indice n'a pas
                                           ! vraiment de sens)
         real(kind=8), dimension(:, :), allocatable :: YChPo1 ! pour donner les valeurs nodales
                                           ! de chaque champ
                                           ! YChPo1(iNode, j) contient la valeur au noeud d'index
                                           ! iNode (dans la numérotation global), du champ j
         
         ! défintion des paramètres du test
         integer :: NbNoT ! définition du nombre total de noeuds
         integer :: IDim ! définition de la dimension spatiale
         integer :: NbElm ! nombre d'éléments
         integer :: NbNo ! nombre de noeuds par élément
         integer :: NbPt ! nombre de points cibles
         integer :: NbVal ! nombre de valeurs par noeuds (1 par
                          ! champ scalaire ou par coposante d'un
                          ! champ vectoriel)
         real(kind=8) :: XPrecR ! precision relative pour determiner
                                ! dans quel élément se trouve chaque noeud
         character(len=8) :: Optio ! option choisie pour trouver dans quel élément se
                                   ! toruve chaque noeud
         character(len=8) :: CType ! type des éléments du maillage
         
         ! indices de boucles
         integer :: IPt, IEl, K ! indices de boucle
      
         ! on donne la valeur des paramètres du test
         NbNoT = 4
         IDim = 2
         NbElm = 1
         NbNo = 4
         NbPt = 4
         NbVal = 1
         XPrecR = 1.D-6
         
         ! allocation dynamique des tableaux utilisés par le test
         allocate(Mail(NbElm, NbNo))
         allocate(XCoor(NbNoT, IDim))
         allocate(XCoorC(NbPT, IDim))
         allocate(Elem(NbPt))
         allocate(Alpha(NbNo, NbPt))
         allocate(XVal(NbNo, NbVal, NbElm))
         allocate(YVal(NbPt, NbVal))
         
         ! allocation dynanmique des tableaux utilisés pour transformer
         ! la donnée du champ par noeud en champ par élément
         allocate(LPt1(NbNoT))
         allocate(YChPo1(NbNoT, NbVal))
         
         ! description du maillage:
         
         ! on donne les coordonnées des noeuds
         XCoor(1,1) =  0.0D0
         XCoor(1,2) =  0.0D0
         XCoor(2,1) =  1.0D0
         XCoor(2,2) =  0.0D0
         XCoor(3,1) =  1.0D0
         XCoor(3,2) =  1.0D0
         XCoor(4,1) =  0.0D0
         XCoor(4,2) =  1.0D0
      
         ! on donne le type d'éléments
         CType = 'QUA4    ' ! ici, des Q4
      
         ! on donne la connectivité du maillage
         Mail(1,1) = 1
         Mail(1,2) = 2
         Mail(1,3) = 3
         Mail(1,4) = 4
      
         ! on donne les coordonnées des points cibles
         XCoorC(1,1) =  0.1D0
         XCoorC(1,2) =  0.1D0
         XCoorC(2,1) =  0.9D0
         XCoorC(2,2) =  0.1D0
         XCoorC(3,1) =  0.9D0
         XCoorC(3,2) =  0.9D0
         XCoorC(4,1) =  0.1D0
         XCoorC(4,2) =  0.9D0
      
         ! on donne la valeur du champ considere aux noeuds
         YChPo1(1,1) = 1.0D0
         YChPo1(2,1) = 2.0D0
         YChPo1(3,1) = 3.0D0
         YChPo1(4,1) = 4.0D0
      
         ! on donne la liste des noeuds consideres pour la transformation
         ! de la donnée du champ par noeud, en champ par element 
         LPt1(1) = 1
         LPt1(2) = 2
         LPt1(3) = 3
         LPt1(4) = 4
                      
         ! on transforme la donnee du champ par noeud en champ par element
         call ChpoToChamno ( &
         ! Inputs 
                                NBNOT, NBNO, NBELM, NBVAL, &
                                YCHPO1, LPT1, MAIL, &
         ! Outputs
                                XVAL  )

         ! on choisi la méthode de découpage en triangles pour trouver a
         ! quel element appartient chaque point cible
         Optio = 'SIMPLEX'

         ! on initialise à 0 les tableaux servant à contenir les résultats
         call IANUL ( ELEM , NBPT )
         call ZDANUL ( ALPHA , NBNO*NBPT )
      
         ! on cherche dans quel elment se trouve chaque noeud
         call NodesInMesh ( &
         ! Inputs
                                NBNOT , IDIM , NBELM , NBNO   , NBPT , &
                                XCOOR , MAIL , CTYPE , XPRECR , XCOORC , &
                                OPTIO , &
         ! Outputs
                                ELEM  , ALPHA)
         !call Nodes_In_Mesh(NbNoT, IDim, NbElm, NbNo, NbPt, &
         !                         Mail, 'Q4xxx', XCoor, XcoorC, &
         !                         Elem, Alpha) 
                               
         ! on affiche le résulat à l'écran
         !call IMPTDC ( 'test' , 1 , 'ALPHA ' , ALPHA  , NBNO , NBPT )
         !call IMPTEC ( 'test' , 1 , 'ELEM' , ELEM , 1 , NBPT )
         print*, 'Alpha='
         do k = 1, NbNo
            do iPt = 1, NbPt
               write(*, '(D14.7,1X,A1)', advance='no') Alpha(k, iPt), ' '
            end do
            write (*,*)
         end do

         print*, 'Elem=', Elem

         ! on initialise à 0 la valeur du champ aux noeuds cibles
         call ZDANUL ( YVAL , NBPT*NBVAL )

         ! on realise la projection : calcul de la valeur du champ aux 
         ! points cibles, en fonction de la valeur du champ aux noeuds
         call ProjectChamno ( & 
         ! Inputs
                                NBPT , NBNO  , NBELM , NBVAL , &
                                ELEM , ALPHA , XVAL  , &
         ! Outputs
                                YVAL )

         ! on affiche le reésultat à l'écran
         !call IMPTDC ( 'test' , 1 , 'YVAL' , YVAL , NBPT , NBVAL )
         print*, 'YVal='
         do k = 1, NbVal
            do iPt = 1, NbPt
               write(*, '(D14.7,1X,A1)', advance='no') YVal(iPt, k), ' '
            end do
            write (*,*)
         end do
         
         ! désallocation dynamique des tableaux utilisés par le test
         deallocate(Mail)
         deallocate(XCoor)
         deallocate(XCoorC)
         deallocate(Elem)
         deallocate(Alpha)
         deallocate(XVal)
         deallocate(YVal)
         
         ! allocation dynanmique des tableaux utilisés pour transformer
         ! la donnée du champ par noeud en champ par élément
         deallocate(LPt1)
         deallocate(YChPo1)
         
      end program test
