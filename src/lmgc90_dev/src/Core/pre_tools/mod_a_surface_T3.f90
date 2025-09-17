! module consacre a la generation d'objets a partir d'une description de leur enveloppe comme une surface triangulee

!> Module dedicated to the generation of avatars described by a triangulated surface
module surface_T3

   use overall, only : faterr

   ! import du module utilities Discrete_Geometry pour acceder aux fonctions qui gerent ces surfaces
   ! reorientation des elements, calcul du volume, du centre d'inertie, de la matrice d'inertie
   use DiscreteGeometry

   implicit none

contains

   ! procedure qui calcule le volume, les coordonnees du centre d'inertie et la matrice d'inertie d'un objet decrit par une 
   ! triangulation de son enveloppe
   !> Computes the volume of an object described by a triangulated surface
   subroutine compute_volume_inertia(nbnode, nbele, connec_vec, coor_vec, vol, x_G, I_vec)

      implicit none

      ! variables d'entree
      integer, intent(in) :: nbnode ! nombre de noeuds
      integer, intent(in) :: nbele ! nombre d'elements
      real(kind=8), intent(in) :: coor_vec(3*nbnode) ! coordonnees des noeuds, stockees sous la forme d'un vecteur

      ! variables d'entree-sortie
      integer, intent(inout) :: connec_vec(3*nbele) ! tables de connectivite des elements, stockees sous la forme d'un vecteur

      ! variables de sortie
      real(kind=8), intent(out) :: vol ! volume de l'objet
      real(kind=8), intent(out) :: x_G(3) ! centre d'inertie de l'objet
      real(kind=8), intent(out) :: I_vec(9) ! matrice d'inertie de l'objet, stockee sous la forme d'un vecteur

      ! variables locales
      integer :: ie ! indice de boucle sur les elements
      real(kind=8) :: coor(3, nbnode) ! coordonnees des noeuds, stockees sous la forme d'une matrice
      integer :: connec(3, nbele) ! tables de connectivite des elements, stockees sous la forme d'une matrice

      real(kind=8) :: I(3, 3) ! matrice d'inertie de l'objet, stockee sous la forme d'une matrice

      integer :: err_

      
      ! on redimensionne les tableaux passes en entree, pour les avoir sous forme de matrice
      !   * les coordonnees des noeuds
      coor = reshape(coor_vec, (/3, nbnode/)) 
      !   * les connectivites des elements
      connec = reshape(connec_vec, (/3, nbele/)) 

      ! on calcule le volume, les coordonnees du centre d'inertie et la matrice d'inertie dans le repere global
      ! N.B. cette procedure modifie la connectivite des elements, de sorte que toutes les normales soient orientees vers
      ! l'exterieur
      call compute_volume_inertia_global_frame_surface_T3(nbnode, nbele, connec, coor, .true., vol, x_G, I, err_)

      if (err_ > 0) then
        call faterr('a_surface_T3::compute_volume_inertia', 'while computing volume and inertia')
      endif   

      ! on renvoie la nouvelle  connectivite des elements
      connec_vec = pack(connec, .true.)

      ! on renvoie la matrice d'inertie sous la forme d'un vecteur
      I_vec = pack(I, .true.)

   end subroutine compute_volume_inertia

   ! procedure qui attribue un numero d'entite aux triangles, a partir d'une recherche de composantes connexes
   subroutine identify_entities(nbnode, nbele, connec_vec, max_adj_ele_2_node, ele2entity)

      implicit none
  
      ! variables d'entree
      integer, intent(in) :: nbnode ! nombre de noeuds
      integer, intent(in) :: nbele ! nombre d'elements
      integer, intent(in) :: connec_vec(3*nbele) ! tables de connectivite des elements, stockees sous la forme d'un vecteur
      integer, intent(in) :: max_adj_ele_2_node ! nombre maximal d'elements adjacents a un noeud

      ! variable de sortie
      integer, intent(out) :: ele2entity(nbele) ! tableau donnant le numero d'entite affecte a chaque element

      ! variable locale
      integer :: connec(3, nbele) ! connectivite des elements, stockee sous la forme d'une matrice

      integer :: err_
      
      ! on redimensionne le vecteur stockant les tables de connectivite sous la forme d'un tableau, 
      ! pour les avoir sous forme de matrice
      connec = reshape(connec_vec, (/3, nbele/)) 

      ! on attribue un numero d'entite aux triangles
      call identify_entities_surface_T3(nbnode, nbele, connec, max_adj_ele_2_node, ele2entity, err_)

      if (err_ > 0) then
        call faterr('a_surface_T3::identify_entities', 'while identifying entities')
      endif   

      
   end subroutine identify_entities

end module surface_T3
