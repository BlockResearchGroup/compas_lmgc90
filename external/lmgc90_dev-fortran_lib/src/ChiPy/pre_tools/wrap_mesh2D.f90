! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_mesh2D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de generation de maillage de rectangle
   use mesh2D

   implicit none

contains

   ! procedure qui calcule les indices correpsondant a un noeud, pour un maillage Q4
   subroutine GetIndicesMeshQ4(n, i, j) &
      bind(c, name='mesh2D_GetIndicesMeshQ4')

      ! variables d'entree :
      integer(c_int), intent(in), value :: n !< the given node

      ! variables de sortie :
      integer(c_int), intent(out) :: i !< index in the u direction
      integer(c_int), intent(out) :: j !< index in the v direction     

      call get_indices_mesh2D('_Q4', n, i, j)

   end subroutine GetIndicesMeshQ4

   ! procedure qui calcule les tailles des vecteurs pour un nouveau maillage
   ! en Q4
   subroutine SizeMeshQ4(nb_elem_x, nb_elem_y, size_coor, size_nb_node_by_ele, &
      size_conn) bind(c, name='mesh2D_SizeMeshQ4')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale

      ! variables de sortie :
      integer(c_int), intent(out) :: size_coor ! taille de coor
      integer(c_int), intent(out) :: size_nb_node_by_ele ! taille de 
         ! nb_node_by_ele
      integer(c_int), intent(out) :: size_conn ! taille de conn

      ! on appele la routine qui calcule les tailes des vecteurs, dans le cas
      ! d'un maillage en Q4
      call size_mesh2D('_Q4', nb_elem_x, nb_elem_y, size_coor, &
         size_nb_node_by_ele, size_conn)

   end subroutine SizeMeshQ4

   ! procedure qui calcule les tailles des vecteurs pour un nouveau maillage en
   ! Q4, splitte en 2 T3
   subroutine SizeMesh2T3(nb_elem_x, nb_elem_y, size_coor, &
      size_nb_node_by_ele, size_conn) bind(c, name='mesh2D_SizeMesh2T3')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale

      ! variables de sortie :
      integer(c_int), intent(out) :: size_coor ! taille de coor
      integer(c_int), intent(out) :: size_nb_node_by_ele ! taille de 
         ! nb_node_by_ele
      integer(c_int), intent(out) :: size_conn ! taille de conn

      ! on peut alors appeler la routine qui calcule les tailes des vecteurs
      ! pour un maillage avec un Q4 splitte en 2 T3
      call size_mesh2D('2T3', nb_elem_x, nb_elem_y, size_coor, &
         size_nb_node_by_ele, size_conn)

   end subroutine SizeMesh2T3

   ! procedure qui calcule les tailles des vecteurs pour un nouveau maillage en
   ! Q4, splitte en 4 T3
   subroutine SizeMesh4T3(nb_elem_x, nb_elem_y, size_coor, &
      size_nb_node_by_ele, size_conn) bind(c, name='mesh2D_SizeMesh4T3')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale

      ! variables de sortie :
      integer(c_int), intent(out) :: size_coor ! taille de coor
      integer(c_int), intent(out) :: size_nb_node_by_ele ! taille de 
         ! nb_node_by_ele
      integer(c_int), intent(out) :: size_conn ! taille de conn

      ! on peut alors appeler la routine qui calcule les tailes des vecteurs
      ! pour un maillage avec un Q4 splitte en 4 T3
      call size_mesh2D('4T3', nb_elem_x, nb_elem_y, size_coor, &
         size_nb_node_by_ele, size_conn)

   end subroutine SizeMesh4T3

   ! procedure qui calcule les tailles des vecteurs pour un nouveau maillage
   ! en Q8
   subroutine SizeMeshQ8(nb_elem_x, nb_elem_y, size_coor, &
      size_nb_node_by_ele, size_conn) bind(c, name='mesh2D_SizeMeshQ8')

      implicit none

      ! variables d'entree :
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale

      ! variables de sortie :
      integer(c_int), intent(out) :: size_coor ! taille de coor
      integer(c_int), intent(out) :: size_nb_node_by_ele ! taille de 
         ! nb_node_by_ele
      integer(c_int), intent(out) :: size_conn ! taille de conn

      ! on appele la routine qui calcule les tailes des vecteurs, dans le cas
      ! d'un maillage en Q8
      call size_mesh2D('_Q8', nb_elem_x, nb_elem_y, size_coor, &
         size_nb_node_by_ele, size_conn)

   end subroutine SizeMeshQ8

   ! procedure qui calcule un nouveau maillage en Q4
   subroutine MeshQ4(x0, y0, lx, ly, nb_elem_x, nb_elem_y, comp_coor, &
      size_coor, nb_node_per_ele, size_nb_node_per_ele, conn, size_conn) &
      bind(c, name='mesh2D_MeshQ4')
  
      implicit none 

      ! variables d'entree :
      real(c_double), intent(in), value :: x0 ! abscisse du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: y0 ! ordonnee du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: lx ! longueur du rectangle, suivant 
         ! l'axe (Ox) 
      real(c_double), intent(in), value :: ly ! longueur du rectangle, suivant 
         ! l'axe (Oy) 
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans 
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale     
      integer(c_int), intent(in), value :: size_coor ! taille de comp_coor
      integer(c_int), intent(in), value :: size_nb_node_per_ele ! taille de 
         ! nb_node_per_ele
      integer(c_int), intent(in), value :: size_conn ! taille de conn
     
      ! variables de sortie :
      real(c_double), dimension(size_coor), intent(out) :: comp_coor 
         ! coordonnees des noeuds du maillage
      integer(c_int), dimension(size_nb_node_per_ele), intent(out) :: nb_node_per_ele ! nb_nodes_per_ele(i) donne le nombre de noeuds de l'element i
      integer(c_int), dimension(size_conn), intent(out) :: conn ! connectivite
         ! du maillage
 
      ! on nettoie la memoire, pour stocker un nouveau maillage
      call free_mesh2D

      ! on initialise les caracteristiques du nouveau maillage
      call init_mesh2D(x0, y0, lx, ly, nb_elem_x, nb_elem_y)

      ! on calcule un nouveau maillage en Q4
      call maillage_Q4

      ! on recupere le maillage calcule, dans un format generique
      call get_mesh2D(comp_coor, size_coor, nb_node_per_ele, &
         size_nb_node_per_ele, conn, size_conn)

   end subroutine MeshQ4

   ! procedure qui calcule un nouveau maillage en T3, obtenu en coupant en 2
   ! des Q4
   subroutine Mesh2T3(x0, y0, lx, ly, nb_elem_x, nb_elem_y, comp_coor, &
      size_coor, nb_node_per_ele, size_nb_node_per_ele, conn, size_conn) &
      bind(c, name='mesh2D_Mesh2T3')
  
      implicit none 

      ! variables d'entree :
      real(c_double), intent(in), value :: x0 ! abscisse du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: y0 ! ordonnee du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: lx ! longueur du rectangle, suivant 
         ! l'axe (Ox) 
      real(c_double), intent(in), value :: ly ! longueur du rectangle, suivant 
         ! l'axe (Oy) 
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans 
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale     
      integer(c_int), intent(in), value :: size_coor ! taille de comp_coor
      integer(c_int), intent(in), value :: size_nb_node_per_ele ! taille de 
         ! nb_node_per_ele
      integer(c_int), intent(in), value :: size_conn ! taille de conn
     
      ! variables de sortie :
      real(c_double), dimension(size_coor), intent(out) :: comp_coor 
         ! coordonnees des noeuds du maillage
      integer(c_int), dimension(size_nb_node_per_ele), intent(out) :: nb_node_per_ele ! nb_nodes_per_ele(i) donne le nombre de noeuds de l'element i
      integer(c_int), dimension(size_conn), intent(out) :: conn ! connectivite
         ! du maillage
 
      ! on nettoie la memoire, pour stocker un nouveau maillage
      call free_mesh2D

      ! on initialise les caracteristiques du nouveau maillage
      call init_mesh2D(x0, y0, lx, ly, nb_elem_x, nb_elem_y)

      ! on calcule un nouveau maillage en T3, obtenus en coupant en deux des Q4 
      call maillage_2T3

      ! on recupere le maillage calcule, dans un format generique
      call get_mesh2D(comp_coor, size_coor, nb_node_per_ele, &
         size_nb_node_per_ele, conn, size_conn)

   end subroutine Mesh2T3

   ! procedure qui calcule un nouveau maillage en T3, obtenu en coupant en 4
   ! des Q4
   subroutine Mesh4T3(x0, y0, lx, ly, nb_elem_x, nb_elem_y, comp_coor, &
      size_coor, nb_node_per_ele, size_nb_node_per_ele, conn, size_conn) &
      bind(c, name='mesh2D_Mesh4T3')
  
      implicit none 

      ! variables d'entree :
      real(c_double), intent(in), value :: x0 ! abscisse du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: y0 ! ordonnee du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: lx ! longueur du rectangle, suivant 
         ! l'axe (Ox) 
      real(c_double), intent(in), value :: ly ! longueur du rectangle, suivant 
         ! l'axe (Oy) 
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans 
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale     
      integer(c_int), intent(in), value :: size_coor ! taille de comp_coor
      integer(c_int), intent(in), value :: size_nb_node_per_ele ! taille de 
         ! nb_node_per_ele
      integer(c_int), intent(in), value :: size_conn ! taille de conn
     
      ! variables de sortie :
      real(c_double), dimension(size_coor), intent(out) :: comp_coor 
         ! coordonnees des noeuds du maillage
      integer(c_int), dimension(size_nb_node_per_ele), intent(out) :: nb_node_per_ele ! nb_nodes_per_ele(i) donne le nombre de noeuds de l'element i
      integer(c_int), dimension(size_conn), intent(out) :: conn ! connectivite
         ! du maillage
 
      ! on nettoie la memoire, pour stocker un nouveau maillage
      call free_mesh2D

      ! on initialise les caracteristiques du nouveau maillage
      call init_mesh2D(x0, y0, lx, ly, nb_elem_x, nb_elem_y)

      ! on calcule un nouveau maillage en T3, obtenus en coupant en quatre des 
      ! Q4 
      call maillage_4T3

      ! on recupere le maillage calcule, dans un format generique
      call get_mesh2D(comp_coor, size_coor, nb_node_per_ele, &
         size_nb_node_per_ele, conn, size_conn)

   end subroutine Mesh4T3

   ! procedure qui calcule un nouveau maillage en Q8
   subroutine MeshQ8(x0, y0, lx, ly, nb_elem_x, nb_elem_y, comp_coor, &
      size_coor, nb_node_per_ele, size_nb_node_per_ele, conn, size_conn) &
      bind(c, name='mesh2D_MeshQ8')
  
      implicit none 

      ! variables d'entree :
      real(c_double), intent(in), value :: x0 ! abscisse du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: y0 ! ordonnee du coin inferieur 
         ! gauche du rectangle
      real(c_double), intent(in), value :: lx ! longueur du rectangle, suivant 
         ! l'axe (Ox) 
      real(c_double), intent(in), value :: ly ! longueur du rectangle, suivant 
         ! l'axe (Oy) 
      integer(c_int), intent(in), value :: nb_elem_x ! nombre d'elements dans 
         ! la direction horizontale
      integer(c_int), intent(in), value :: nb_elem_y ! nombre d'elements dans 
         ! la direction verticale     
      integer(c_int), intent(in), value :: size_coor ! taille de comp_coor
      integer(c_int), intent(in), value :: size_nb_node_per_ele ! taille de 
         ! nb_node_per_ele
      integer(c_int), intent(in), value :: size_conn ! taille de conn
     
      ! variables de sortie :
      real(c_double), dimension(size_coor), intent(out) :: comp_coor 
         ! coordonnees des noeuds du maillage
      integer(c_int), dimension(size_nb_node_per_ele), intent(out) :: nb_node_per_ele ! nb_nodes_per_ele(i) donne le nombre de noeuds de l'element i
      integer(c_int), dimension(size_conn), intent(out) :: conn ! connectivite
         ! du maillage
 
      ! on nettoie la memoire, pour stocker un nouveau maillage
      call free_mesh2D

      ! on initialise les caracteristiques du nouveau maillage
      call init_mesh2D(x0, y0, lx, ly, nb_elem_x, nb_elem_y)

      ! on calcule un nouveau maillage en Q8
      call maillage_Q8

      ! on recupere le maillage calcule, dans un format generique
      call get_mesh2D(comp_coor, size_coor, nb_node_per_ele, &
         size_nb_node_per_ele, conn, size_conn)

   end subroutine MeshQ8

end module wrap_mesh2D
