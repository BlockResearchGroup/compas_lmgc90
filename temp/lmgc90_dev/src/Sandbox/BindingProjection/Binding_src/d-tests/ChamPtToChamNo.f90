      ! fonction qui permet d'interpoler des
      ! valeurs aux noeuds, en fonction de valeurs de champs données
      ! sur un nuage de points
      ! N.B. on suppose que les tableaux Elem (numéro de l'élément 
      ! contenant chaque point cible) et Alpha (valeurs des fonctions
      ! de forme aux points cibles) ont déjà été calculées (avec les 
      ! routines de DD...)! 
      
      ! fonction qui interpole des champs aux noeuds, en fonction des 
      ! valeurs de ces champs sur un nuage de points
      subroutine AssembChamNo(NbNoT, NbElm, NbNo, NbPt, NbVal, &
                                 Mail, Elem, Alpha, YVal, YChPo)
         
         implicit none
            
         ! variables d'entrée :
         integer, intent(in) :: NbNoT ! définition du nombre total de noeuds
         integer, intent(in) :: NbElm ! nombre d'éléments
         integer, intent(in) :: NbNo ! nombre de noeuds par élément
         integer, intent(in) :: NbPt ! nombre de points cibels (i.e. portants le champs à interpoler)
         integer, intent(in) :: NbVal ! nombre de valeurs par noeuds (1 par
         integer, dimension(NbElm, NbNo), intent(in) :: Mail ! connectivité du maillage
         integer, dimension(NbPt), intent(in) :: Elem ! stocke le numero de l'element dans lequel
                                          ! se trouve chaque point cible
                                          ! Elem(iPt) contient le numéro de l'élément dans 
                                          ! lequel se trouve le point cible d'index iPt
         real(kind=8), dimension(NbNo, NbPt), intent(in) :: Alpha ! valeur de la fonction de forme
                                          ! pour chaque noeud du maillage, pour chaque
                                          ! point cible
                                          ! Alpha(i, iPt) contient la valeur de la fonction de
                                          ! forme N_i, où i est l'index dans la numérotation locale
                                          ! de l'élément contenant le point d'index iPt, au point iPt
                                          ! i.e. N_i(x_iPt)
         real(kind=8), dimension(NbPt, NbVal), intent(in) :: YVal ! valeur de chaque champ aux points cibles
                                          ! YVal(iPt, j) contient la valeur au point cible
                                          ! d'index iPt, du champ d'index j
            
         ! variable de sortie :
         real(kind=8), dimension(NbNoT, NbVal), intent(out) :: YChPo ! pour donner les valeurs nodales
                                           ! de chaque champ
                                           ! YChPo(iNode, j) contient la valeur au noeud d'index
                                           ! iNode (dans la numérotation global), du champ j
         
         ! variables locales :
         integer :: iEl, iNloc, iNglob, iPt ! indices de boucle
       
         ! on initialise à 0 le champ aux noeuds
         YChPo = 0.d0
         
         ! pour chaque point cible
         do iPt=1, NbPt
               
            ! on récupère le numéro de l'élément contenant le point courant
            iEl = Elem(iPt) 
               
            ! pour chaque noeud de l'élément
            do iNloc=1, NbNo
               
               ! on récupère le numéro du noeud dans la numérotation globale
               iNglob = Mail(iEl, iNloc)
               
               ! on ajoute la contribution du point cible courant à ce noeud
               ! N.B.: on traite tous les champs d'un coup
               YChPo(iNglob, :) = YChPo(iNglob, :) + &
                                  Alpha(iPt, iNloc)*YVal(iPt, :)
               
            end do     
            
         end do
         
      end subroutine AssembChamNo
      
