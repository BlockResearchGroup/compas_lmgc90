! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_surface_T3

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de gestion des objets decrits par une surface triangulee
   use surface_T3

   implicit none

contains

   ! procedure qui calcule le volume, les coordonnees du centre d'inertie et la matrice d'inertie d'un objet decrit par une 
   ! triangulation de son enveloppe
   subroutine surface_T3_compute_volume_inertia(coor, size_coor, connec, size_connec, x_G, size_x_G, I, size_I, vol) &
      bind(c, name='surface_T3_compute_volume_inertia')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: size_coor ! taille du vecteur contenant les coordonnees des noeuds, i.e. 3
         ! fois le nombre de noeuds
      real(c_double), dimension(size_coor), intent(in) :: coor ! vecteur contenant les coordonnees des noeuds
      integer(c_int), intent(in), value :: size_connec ! taille du vecteur contenant les connectivites des elements, i.e. 3
         ! fois le nombre d'elements
      integer(c_int), intent(in), value :: size_x_G ! taille du vecteur qui va recevoir les coordonnees du centre d'inertie, 
         ! i.e. 3
      integer(c_int), intent(in), value :: size_I ! taille du vecteur qui va recevoir la matrice d'inertie, i.e. 9

      ! variables d'entree-sortie :
      integer(c_int), dimension(size_connec), intent(inout) :: connec ! vecteur contenant les connectivites des elements

      ! variables de sortie :
      real(c_double), intent(out) :: vol ! volume de l'objet delimite par la surface triangulee
      real(c_double), dimension(size_x_G), intent(out) :: x_G ! coordonnees du centre d'inertie de l'objet delimite par la 
         ! surface triangulee
      real(c_double), dimension(size_I), intent(out) :: I ! matrice d'inertie de l'objet delimite par la surface triangulee

      ! variables locales :
      integer :: nbnode ! nombre de noeuds
      integer :: nbele ! nombre d'elements

      ! on teste la compatibilite des donnees :
      if (mod(size_coor, 3) /= 0) then
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'for node coordinates'
         stop 
      end if

      if (mod(size_connec, 3) /= 0) then
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'for elements connectivity'
         stop 
      end if

      if (size_x_G /= 3) then  
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'for mass center coordinates'
         stop
      end if

      if (size_I /= 9) then  
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'for inertia matrix'
         stop
      end if

      ! on calcule le nombre de noeuds
      nbnode = size_coor/3
      ! on calcule le nombre d'elements
      nbele = size_connec/3

      ! on calcule le volume, les coordonnees du centre d'inertie et la matrice d'inertie de de l'objet
      call compute_volume_inertia(nbnode, nbele, connec, coor, vol, x_G, I)

   end subroutine surface_T3_compute_volume_inertia

   ! procedure qui attribue un numero d'entite aux triangles, a partir d'une recherche de composantes connexes
   subroutine surface_T3_identify_entities(nbnode, max_adj_ele_2_node, connec, size_connec, ele2entity, size_ele2entity) &
      bind(c, name='surface_T3_identify_entities')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nbnode ! nombre de noeuds du maillage
      integer(c_int), intent(in), value :: max_adj_ele_2_node ! nombre maximal d'elements adjacents a un noeud
      integer(c_int), intent(in), value :: size_connec ! taille du vecteur contenant les connectivites des elements, i.e. 3
         ! fois le nombre d'elements
      integer(c_int), dimension(size_connec), intent(in) :: connec ! vecteur contenant les connectivites des elements
      integer(c_int), intent(in), value :: size_ele2entity ! taille du vecteur contenant le numero d'entite pour chaque 
         ! element, i.e. le nombre d'elements

      ! variables de sortie :
      integer(c_int), dimension(size_ele2entity), intent(out) :: ele2entity ! tableau donnant le numero d'entite affecte a chaque element

      ! variables locales :
      integer :: nbele ! nombre d'elements

      ! on teste la compatibilite des donnees :
      if (mod(size_connec, 3) /= 0) then
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'for elements connectivity'
         stop 
      end if

      ! on calcule le nombre d'elements
      nbele = size_connec/3

      if (size_ele2entity /= nbele) then  
         print *,'wrap_surface_T3::compute_volume: FATAL ERROR: non conforming size ',&
                 'element to entity map'
         stop
      end if

      ! on attribue un numero d'entite aux triangles, a partir d'une recherche de composantes connexes
      call identify_entities(nbnode, nbele, connec, max_adj_ele_2_node, ele2entity)

   end subroutine surface_T3_identify_entities

end module wrap_surface_T3
