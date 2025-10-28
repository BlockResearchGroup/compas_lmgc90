!> Module dedicated to mesh generation for a rectangular domain
module mesh2D

   implicit none

   private

   integer :: nelemx !< number of elements in the x direction
   integer :: nelemy !< number of elements in the y direction
   real(kind=8) :: x0 !< abscissa of the lower left corner of the rectangle
   real(kind=8) :: y0 !< ordinate of the lower left corner of the rectangle
   real(kind=8) :: lx !< length of the mesh, following the axis (Ox) 
   real(kind=8) :: ly !< length of the mesh, following the axis (Oy) 

   character(len=1) :: direction !< favorite direction to number the nodes
   real(kind=8) :: xpas !< length of an element, following the x direcion
   real(kind=8) :: ypas !< length of an element, following the y direcion

   integer :: NbNode !< number of nodes in the mesh
   integer :: NbEle !< number of elements in the mesh
   integer :: NbNpE !< number of nodes per element

   real(kind=8), allocatable, dimension(:,:) :: coor !< coordinates of the nodes
      ! coordonnees de tous les noeuds

   !> user type used to decribed a finite element
   type T_ele
      character(len=5) :: ID !< name of the element: 'T3xxx', 'Q4xxx', ...
      integer, dimension(:), pointer :: l_con !< connectivity of the element
   end type T_ele 

   type(T_ele),allocatable,dimension(:) :: connect !< connectivity of the mesh
         ! table de connectivite

   public :: &
      size_mesh2D, &
      get_indices_mesh2D, &
      init_mesh2D, &
      free_mesh2D, &
      maillage_Q4, &
      maillage_2T3, &
      maillage_4T3, &
      maillage_Q8, &
      get_mesh2D

contains 

   !> this function computes the sizes of vectors used to store a mesh in
   !> the following generic format:\n
   !>    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
   !>    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
   !>         element i, i in [1, number of elements]
   !>    - conn: vector storing the connectivity of the elements
   !>         [n11, n12n n13, n21, n22, n23, n24, ...]
   !> consider the following little mesh:\n
   !>    2   4   6\n
   !>    *---*---*\n
   !>    | 1 | 2 |\n
   !>    *---*---*\n
   !>    1   3   5\n
   !> the vectors for this mesh read:
   !>    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6]
   !>    - nb_node_per_ele = [4, 4]
   !>    - conn = [1, 3, 4, 2, 3, 5, 6, 4]
   subroutine size_mesh2D(mesh_type, nb_elem_x, nb_elem_y, size_coor, &
      size_nb_node_per_ele, size_conn)

      implicit none

      ! variables d'entree :
      character(len=3), intent(in) :: mesh_type !< type of mesh: '_Q4', '_Q8', '2T3' or '4T3'
      integer, intent(in) :: nb_elem_x !< number of elements in the horizontal direction
      integer, intent(in) :: nb_elem_y !< number of elements in the vertical direction

      ! variables de sortie :
      integer, intent(out) :: size_coor !< size of coor
      integer, intent(out) :: size_nb_node_per_ele !< size of nb_node_per_ele
      integer, intent(out) :: size_conn !< size of conn

      ! en fonction du type du maillage
      select case(mesh_type) 
         case('_Q4') ! cas du maillage en Q4
            size_coor = 2*(nb_elem_x + 1)*(nb_elem_y + 1) ! 2 fois le nombre de
               ! noeuds
            size_nb_node_per_ele = nb_elem_x*nb_elem_y ! nombre d'elements
            size_conn = 4*size_nb_node_per_ele
         case('_Q8') ! cas du maillage en Q8
            size_coor = 2*((3*nb_elem_y + 2)*nb_elem_x + 1 + 2*nb_elem_y)
               ! 2 fois le nombre de noeuds
            size_nb_node_per_ele = nb_elem_x*nb_elem_y ! nombre d'elements
            size_conn = 8*size_nb_node_per_ele
         case('2T3') ! cas du maillage en Q4, ou chaque element est splitte en
               ! 2 T3
            size_coor = 2*(nb_elem_x + 1)*(nb_elem_y + 1) ! 2 fois le nombre de
               ! noeuds
            size_nb_node_per_ele = 2*nb_elem_x*nb_elem_y ! nombre d'elements
            size_conn = 3*size_nb_node_per_ele
         case('4T3') ! cas du maillage en Q4, ou chaque element est splitte en
               ! 4 T3
            size_coor = 2*((nb_elem_x + 1)*(nb_elem_y + 1) + &
               nb_elem_x*nb_elem_y) ! 2 fois le nombre de
               ! noeuds
            size_nb_node_per_ele = 4*nb_elem_x*nb_elem_y ! nombre d'elements
            size_conn = 3*size_nb_node_per_ele
         case default ! cas general
            print *,'mesh2D::size_mesh_2D: Error: unknown type of mesh!'
      end select

   end subroutine size_mesh2D

   !> this function gives the couple (i, j) of indices coresponding to a given node 
   subroutine get_indices_mesh2D(mesh_type, n, i, j)

      implicit none

      ! variables d'entree :
      character(len=3), intent(in) :: mesh_type !< type of mesh: '_Q4', '_Q8', '2T3' or '4T3'
      integer, intent(in) :: n !< the given node

      ! variables de sortie :
      integer, intent(out) :: i !< index in the u direction
      integer, intent(out) :: j !< index in the v direction

      ! en fonction du type du maillage
      select case(mesh_type) 
         case('_Q4') ! cas du maillage en Q4
            ! cas ou la direction v est privelegiee
            if (direction == 'Y') then
               i = (n-1)/(nelemy + 1) + 1
               j = mod(n-1, nelemy + 1) + 1
            else 
               ! cas ou la direction u est privelegiee
               if (direction == 'X') then
                  j = (n-1)/(nelemx + 1) + 1
                  i = mod(n-1, nelemx + 1) + 1
               else
                  ! si on a pas compris la direction privelegiee
                  print*,'mesh_2D::maillage_Q4: Error: unkown direction!'
                  stop
               end if
            end if
         case default ! cas general
            print *,'mesh2D::size_mesh_2D: Error: unsupported type of mesh!'
      end select

   end subroutine get_indices_mesh2D

   !> this function intializes the module to compute a new mesh 
   subroutine init_mesh2D(x0_, y0_, lx_, ly_, nb_elem_x, nb_elem_y)
 
      implicit none

      ! variables d'entree
      real(kind=8), intent(in) :: x0_ !< abscissa of the lower left corner of the rectangle
      real(kind=8), intent(in) :: y0_ !< ordinate of the lower left corner of the rectangle
      real(kind=8), intent(in) :: lx_ !< length of the mesh, following the axis (Ox) 
      real(kind=8), intent(in) :: ly_ !< length of the mesh, following the axis (Oy) 
      integer, intent(in) :: nb_elem_x !< number of elements in the horizontal direction
      integer, intent(in) :: nb_elem_y !< number of elements in the vertical direction

      ! stockage des donnees fournies
      x0 = x0_
      y0 = y0_
      lx = lx_
      ly = ly_
      nelemx = nb_elem_x
      nelemy = nb_elem_y

      ! calcul du pas en x
      xpas = lx/nelemx
      ! calcul du pas en y
      ypas = ly/nelemy

      ! on decide de la direction a privilegier selon les dimensions du 
      ! rectangle
      
      ! si le rectangle est plus court selon la direction x
      if (lx < ly) then
         ! on privelegie la direction x
         direction = 'X'
      else
         ! sinon, on privelegie la direction y
         direction = 'Y'      
      end if
 
   end subroutine init_mesh2D

   !> this function free a stored mesh, i.e. deallocate memory occupied by a
   !> computed mesh
   subroutine free_mesh2D

      implicit none

      ! variables locales :
      integer :: ie ! indice de boucle sur les elements

      ! desallocation de la table des coordonnees
      if (allocated(coor)) deallocate(coor)

      ! desallocation de la table de connectivite
      
      ! pour chaque element
      do ie=1, NbEle
         ! on desalloue la connectivite de l'element
         if (associated(connect(ie)%l_con)) &
            deallocate(connect(ie)%l_con)
         nullify(connect(ie)%l_con)
      end do

      ! finalement, on desalloue la table de connectivite
      if (allocated(connect)) deallocate(connect)

   end subroutine free_mesh2D

   ! generation d'un maillage en Q4
   !> this function computes a mesh with Q4 elements
   !> \warning the initialization of the mesh caracteristics is supposed to be
   !> done
   subroutine maillage_Q4( )

      implicit none 

      integer                                 :: idim=2
      integer                                 :: nbn,ix,iy,i,j,k,nelem,lbn,lbt

!
!     nbre d'elements totaux
!
      Nbele=nelemy*nelemx
!            
!     nbre de noeuds
!
      Nbnode=(nelemy + 1)*(nelemx + 1)

      ! nombre de noeuds par element
      NbNpE = 4
!
      allocate(coor(idim,Nbnode),connect(Nbele))
!
!     calcul du maillage          
!
!        * calcul des coordonnee     
      if (direction == 'Y') then
!
!     la direction privilegiee est y, 
!     on construit les elements
!     par bandes verticales.
!
         nbn=0
         do ix=1, nelemx + 1
            do iy=1, nelemy + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas)
            end do
         end do
      
      else if (direction == 'X') then
!
!     la direction privilegiee est x, 
!     on construit les elements
!     par bandes horizontales.
!
         nbn=0
         do iy=1, nelemy + 1
            do ix=1, nelemx + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas) 
            end do
         end do
      
      else
         ! si on a pas compris la direction privelegiee
         print*,'mesh_2D::maillage_Q4: Error: unkown direction!'
         stop
      endif

      ! test de parano
      if(nbn.ne.NbNode) then
         print*,'mesh_2D::maillage_Q4: Error: bad computation of the coordinates!'
         stop
      endif
!
!        * calcul de la connectivite     
      if (direction == 'Y') then
!
         nelem=0
         do i=1, nelemx      
            do j=1, nelemy
               nelem = nelem + 1

               connect(nelem)%ID='Q4xxx'

               allocate(connect(nelem)%l_con(4))
               connect(nelem)%l_con(1)=1 + ((i - 1)*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(2)=1 + (i*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(3)=2 + (i*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(4)=2 + ((i - 1)*(nelemy + 1)) + (j - 1)
            end do
         end do

      else if(direction == 'X') then
        nelem=0
        do i=1, nelemy      
           do j=1, nelemx
              nelem=nelem + 1
              connect(nelem)%ID='Q4xxx'

              allocate(connect(nelem)%l_con(4))
              connect(nelem)%l_con(1)=1 + ((i - 1)*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(2)=2 + ((i - 1)*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(3)=2 + (i*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(4)=1 + (i*(nelemx + 1)) + (j - 1)
           end do
        end do
     else
        print*,'mesh_2D::maillage_Q4: Error: unkown direction!'
        stop
     endif
!
     ! test de parano
     if(nelem.ne.NbEle) then
        print*,'mesh2D::maillage_Q4: Error: bad computation of the connectivity!'
        stop
     endif
!
!      CALCUL DE LA LARGEUR DE BANDE
!
     lbn=0
     do i=1, nelem
        do j=1, nbnpe-1
           do k=j + 1, nbnpe 
              lbt=iabs(connect(i)%l_con(1) - connect(i)%l_con(j))
              if (lbn .lt. lbt) lbn=lbt
           end do
        end do
     end do

     PRINT*,'Largeur de bande estimee:',lbn

   end subroutine maillage_Q4

   ! generation d'un maillage en T3, obtenu a partir d'un maillage en Q4, en
   ! coupant en deux les elements
   !> this function computes a mesh with T3 elements --- obtained by splitting
   !> one Q4 in two T3
   !> \warning the initialization of the mesh caracteristics is supposed to be
   !> done
   subroutine maillage_2T3( )

      implicit none 

      integer                                 :: idim=2
      integer                                 :: nbn,ix,iy,i,j,k,nelem,lbn,lbt

!
!     nbre d'elements totaux
!
      Nbele=2*nelemy*nelemx
!            
!     nbre de noeuds
!
      Nbnode=(nelemy + 1)*(nelemx + 1)

      ! nombre de noeuds par element
      NbNpE = 3
!
      allocate(coor(idim,Nbnode),connect(Nbele))
!
!     calcul du maillage          
!
!        * calcul des coordonnee     
      if (direction == 'Y') then
!
!     la direction privilegiee est y, 
!     on construit les elements
!     par bandes verticales.
!
         nbn=0
         do ix=1, nelemx + 1
            do iy=1, nelemy + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas)
            end do
         end do
      
      else if (direction == 'X') then
!
!     la direction privilegiee est x, 
!     on construit les elements
!     par bandes horizontales.
!
         nbn=0
         do iy=1, nelemy + 1
            do ix=1, nelemx + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas) 
            end do
         end do
      
      else
         ! si on a pas compris la direction privelegiee
         print*,'mesh_2D::maillage_2T3: Error: unkown direction!'
         stop
      endif

      ! test de parano
      if(nbn.ne.NbNode) then
         print*,'mesh_2D::maillage_2T3: Error: bad computation of the coordinates!'
         stop
      endif
!
!        * calcul de la connectivite     
      if (direction == 'Y') then
!
         nelem=0
         do i=1, nelemx      
            do j=1, nelemy
               
               nelem = nelem + 1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)= 1 + ((i - 1)*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(2)= 1 + (i*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(3)= 2 + ((i - 1)*(nelemy + 1)) + (j - 1)

               nelem=nelem+1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)=1 + (i*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(2)=2 + (i*(nelemy + 1)) + (j - 1)
               connect(nelem)%l_con(3)=2 + ((i - 1)*(nelemy + 1)) + (j - 1)
 
            end do
         end do

      else if(direction == 'X') then
        nelem=0
        do i=1, nelemy      
           do j=1, nelemx

              nelem=nelem+1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))

              connect(nelem)%l_con(1)=1 + ((i - 1)*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(2)=2 + ((i - 1)*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(3)=1 + (i*(nelemx + 1)) + (j - 1)

              nelem=nelem+1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))

              connect(nelem)%l_con(1)=2 + ((i - 1)*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(2)=2 + (i*(nelemx + 1)) + (j - 1)
              connect(nelem)%l_con(3)=1 + (i*(nelemx + 1)) + (j - 1)

           end do
        end do
     else
        print*,'mesh_2D::maillage_2T3: Error: unkown direction!'
        stop
     endif
!
     ! test de parano
     if(nelem.ne.NbEle) then
        print*,'mesh2D::maillage_2T3: Error: bad computation of the connectivity!'
        stop
     endif
!
!      CALCUL DE LA LARGEUR DE BANDE
!
     lbn=0
     do i=1, nelem
        do j=1, nbnpe-1
           do k=j + 1, nbnpe 
              lbt=iabs(connect(i)%l_con(1) - connect(i)%l_con(j))
              if (lbn .lt. lbt) lbn=lbt
           end do
        end do
     end do

     PRINT*,'Largeur de bande estimee:',lbn

   end subroutine maillage_2T3

   ! generation d'un maillage en T3, obtenu a partir d'un maillage en Q4, en
   ! coupant en quatre les elements
   !> this function computes a mesh with T3 elements --- obtained by splitting
   !> one Q4 in four T3
   !> \warning the initialization of the mesh caracteristics is supposed to be
   !> done
   subroutine maillage_4T3( )

      implicit none 

      integer                                 :: idim=2
      integer                                 :: nbn,ix,iy,i,j,k,nelem,lbn,lbt
      integer                                 :: ie_base 
      integer, dimension(:,:), allocatable    :: cell

!
!     nbre d'elements totaux
!
      Nbele=4*nelemy*nelemx
!            
!     nbre de noeuds
!
      Nbnode=((nelemy + 1)*(nelemx + 1)) + (nelemy*nelemx)

      ! nombre de noeuds par element
      NbNpE = 3
!
      allocate(coor(idim,Nbnode),connect(Nbele))

!     usage interne
      allocate(cell(5,nelemy*nelemx))

!
!     calcul du maillage          
!
!        * calcul des coordonnee     
      if (direction == 'Y') then
!
!     la direction privilegiee est y, 
!     on construit les elements
!     par bandes verticales.
!
!       2 - 4
!       | 5 |
!       1 - 3

         nbn=0
         ie_base=0
         do ix=1, nelemx + 1

! on parcourt les mailles quadrangulaires 
! d'abord les noeuds des coins

            do iy=1, nelemy + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas)

               if (ix /= nelemx + 1 .and. iy /= nelemy + 1) &
                  cell(1,ie_base + iy)=nbn
               if (ix /= nelemx + 1 .and. iy /= 1) cell(2,ie_base + iy - 1)=nbn
               if (ie_base /= 0) then
                  if (iy /= nelemy + 1) &
                     cell(3,ie_base + iy - nelemy)=nbn
                  if (iy /= 1) cell(4,ie_base + iy - (nelemy + 1))=nbn
               endif
            end do

! ensuite le noeud au centre
           
            if (ix /= nelemx + 1) then
               do iy=1, nelemy
                  nbn=nbn+1
                  coor(1,nbn)=x0 + ((ix - 1)*xpas) + (0.5*xpas)
                  coor(2,nbn)=y0 + ((iy - 1)*ypas) + (0.5*ypas)

                  cell(5,ie_base + iy)=nbn
               end do
               ie_base=ie_base + nelemy
            end if
         end do
      
      else if (direction == 'X') then
!
!     la direction privilegiee est x, 
!     on construit les elements
!     par bandes horizontales.
!
!       3 - 4
!       | 5 |
!       1 - 2
!
         nbn=0
         ie_base=0
         do iy=1, nelemy + 1
            do ix=1, nelemx + 1
               nbn=nbn+1
               coor(1,nbn)=x0 + ((ix-1)*xpas)
               coor(2,nbn)=y0 + ((iy-1)*ypas) 

               if (ix /= nelemx + 1 .and. iy /= nelemy + 1) &
                  cell(1,ie_base + ix)=nbn
               if (ix /= 1 .and. iy /= nelemy + 1) cell(2,ie_base + ix - 1)=nbn
               if (ie_base /= 0) then
                  if (ix /= nelemx + 1) cell(3,ie_base + ix - nelemx)=nbn
                  if (ix /= 1) cell(4,ie_base + ix - (nelemx + 1))=nbn
               endif
            end do

! on ajoute le noeud au centre de l'element
            if (iy /= nelemy + 1) then
               do ix=1, nelemx
                  nbn=nbn + 1
                  coor(1,nbn)=x0 + ((ix - 1)*xpas) + (0.5*xpas)
                  coor(2,nbn)=y0 + ((iy - 1)*ypas) + (0.5*ypas)

                  cell(5,ie_base + ix)=nbn
               end do
               ie_base=ie_base + nelemx
            end if
         end do
      
      else
         ! si on a pas compris la direction privelegiee
         print*,'mesh_2D::maillage_4T3: Error: unkown direction!'
         stop
      endif

      ! test de parano
      if(nbn.ne.NbNode) then
         print*,'mesh_2D::maillage_4T3: Error: bad computation of the coordinates!'
         stop
      endif
!
!        * calcul de la connectivite     
      if (direction == 'Y') then
!
         nelem=0
         do i=1, nelemx      
            do j=1, nelemy

               ie_base=((i - 1)*nelemy) + j
 
               nelem=nelem + 1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)= cell(5,ie_base)
               connect(nelem)%l_con(2)= cell(1,ie_base)
               connect(nelem)%l_con(3)= cell(3,ie_base)

               nelem=nelem+1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)= cell(5,ie_base)
               connect(nelem)%l_con(2)= cell(3,ie_base)
               connect(nelem)%l_con(3)= cell(4,ie_base)

               nelem=nelem+1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)= cell(5,ie_base)
               connect(nelem)%l_con(2)= cell(4,ie_base)
               connect(nelem)%l_con(3)= cell(2,ie_base)

               nelem=nelem+1

               connect(nelem)%ID='T3xxx'

               allocate(connect(nelem)%l_con(3))
               connect(nelem)%l_con(1)= cell(5,ie_base)
               connect(nelem)%l_con(2)= cell(2,ie_base)
               connect(nelem)%l_con(3)= cell(1,ie_base)

            end do
         end do
 
      else if(direction == 'X') then
        nelem=0
        do i=1, nelemy      
           do j=1, nelemx

              ie_base=((i - 1)*nelemx) + j

              nelem=nelem + 1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))
              connect(nelem)%l_con(1)= cell(5,ie_base)
              connect(nelem)%l_con(2)= cell(3,ie_base)
              connect(nelem)%l_con(3)= cell(1,ie_base)

              nelem=nelem+1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))
              connect(nelem)%l_con(1)= cell(5,ie_base)
              connect(nelem)%l_con(2)= cell(4,ie_base)
              connect(nelem)%l_con(3)= cell(3,ie_base)

              nelem=nelem+1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))
              connect(nelem)%l_con(1)= cell(5,ie_base)
              connect(nelem)%l_con(2)= cell(2,ie_base)
              connect(nelem)%l_con(3)= cell(4,ie_base)

              nelem=nelem+1

              connect(nelem)%ID='T3xxx'

              allocate(connect(nelem)%l_con(3))
              connect(nelem)%l_con(1)= cell(5,ie_base)
              connect(nelem)%l_con(2)= cell(1,ie_base)
              connect(nelem)%l_con(3)= cell(2,ie_base)

           end do
        end do
     else
        print*,'mesh_2D::maillage_4T3: Error: unkown direction!'
        stop
     endif
!
     ! test de parano
     if(nelem.ne.NbEle) then
        print*,'mesh2D::maillage_4T3: Error: bad computation of the connectivity!'
        stop
     endif

!
!      CALCUL DE LA LARGEUR DE BANDE
!
     lbn=0
     do i=1, nelem
        do j=1, nbnpe-1
           do k=j + 1, nbnpe 
              lbt=iabs(connect(i)%l_con(1) - connect(i)%l_con(j))
              if (lbn .lt. lbt) lbn=lbt
           end do
        end do
     end do

     PRINT*,'Largeur de bande estimee:',lbn

     ! on desalloue la memoire occuppee par le tableau de travail interne
     deallocate(cell) 

   end subroutine maillage_4T3

   ! generation d'un maillage en Q8
   !> this function computes a mesh with Q8 elements
   !> \warning the initialization of the mesh caracteristics is supposed to be
   !> done
   subroutine maillage_Q8()

      implicit none

      integer ::   idim=2 
      real(kind=8) :: X,Y
      integer :: nbn,i,j,IX1,IY1,IX,IY,IAVX,IAVY,IAVX_I,IAVY_I,IAVX_P,IAVY_P,NELEM

!     nbre d'elements
!
      nbele=NELEMX*NELEMY
!            
!     nbre de noeuds
!
      nbnode=((3*NELEMY + 2)*NELEMX) + 1 + 2*NELEMY

!     nombre de noeuds par element
      NbNpE = 8

      allocate(coor(idim,Nbnode),connect(nbele))

!
!     calcul du maillage          
!
      X=X0
      Y=Y0
!
      NBN=0
      if (direction == 'Y') then
         DO I=1, NELEMX
            DO J=1, 1 + 2*NELEMY
               NBN=NBN + 1
               COOR(1,NBN)=X
               COOR(2,NBN)=Y + (J - 1)*0.5*ypas
            END DO
            X=X + 0.5*xpas
            DO J=1, 1 + NELEMY
               NBN=NBN + 1
               COOR(1,NBN)=X
               COOR(2,NBN)=Y + ((J - 1)*ypas)
            END DO
            X=X + 0.5*xpas
         END DO
         DO J=1, 1 + 2*NELEMY
            NBN=NBN+1
            COOR(1,NBN)=X
            COOR(2,NBN)=Y + (J - 1)*0.5*ypas
         END DO
      else 
         if (direction == 'X') then
            DO I=1, NELEMY
               DO J=1,1 + 2*NELEMX
                  NBN=NBN + 1
                  COOR(1,NBN)=X + (J - 1)*0.5*xpas
                  COOR(2,NBN)=Y
               END DO
               Y=Y + 0.5*ypas
               DO J=1,1 + NELEMX
                  NBN=NBN + 1
                  COOR(1,NBN)=X + ((J - 1)*xpas)
                  COOR(2,NBN)=Y
               END DO
               Y=Y + 0.5*ypas
            END DO
            DO J=1, 1 + 2*NELEMX
               NBN=NBN + 1
               COOR(1,NBN)=X + (J - 1)*0.5*xpas
               COOR(2,NBN)=Y
            END DO
         else
            ! si on a pas compris la direction privelegiee
            print*,'mesh_2D::maillage_Q8: Error: unkown direction!'
            stop
         end if
      end if
!
      IF(NBN.NE.NBNode) THEN
         print*,'mesh_2D::maillage_Q8: Error: bad computation of the coordinates!'
         stop
      ENDIF
!
      NELEM=0
      IF (direction == 'Y') then
         IY1=1 + 2*NELEMY
         IY=IY1 + 1 + NELEMY
         DO I=1, NELEMX      
            IAVX=(I - 1)*IY
            DO J=1,NELEMY
               IAVY_I=(J - 1)*2
               IAVY_P=J - 1
               NELEM=NELEM + 1

               connect(nelem)%ID='Q8xxx'

               allocate(connect(nelem)%l_con(8))

               connect(nelem)%l_con(1)=1 + IAVX + IAVY_I
               connect(nelem)%l_con(5)=1 + IY1 + IAVX + IAVY_P
               connect(nelem)%l_con(2)=1 + IY + IAVX + IAVY_I
               connect(nelem)%l_con(6)=2 + IY + IAVX + IAVY_I
               connect(nelem)%l_con(3)=3 + IY + IAVX + IAVY_I
               connect(nelem)%l_con(7)=2 + IY1 + IAVX + IAVY_P
               connect(nelem)%l_con(4)=3 + IAVX + IAVY_I
               connect(nelem)%l_con(8)=2 + IAVX + IAVY_I
            END DO
         END DO
      else 
         if (direction == 'X') then 
            IX1=1 + 2*NELEMX
            IX=IX1 + 1 + NELEMX
            DO I=1, NELEMY      
               IAVY=(I - 1)*IX
               DO J=1, NELEMX
                  IAVX_I=(J - 1)*2
                  IAVX_P=J - 1
                  NELEM=NELEM + 1
                  connect(nelem)%ID='Q8xxx'

                  allocate(connect(nelem)%l_con(8))
      
                  connect(nelem)%l_con(1)=1 + IAVY + IAVX_I
                  connect(nelem)%l_con(8)=1 + IX1 + IAVY + IAVX_P
                  connect(nelem)%l_con(4)=1 + IX + IAVY + IAVX_I
                  connect(nelem)%l_con(7)=2 + IX + IAVY + IAVX_I
                  connect(nelem)%l_con(3)=3 + IX + IAVY + IAVX_I
                  connect(nelem)%l_con(6)=2 + IX1 + IAVY + IAVX_P
                  connect(nelem)%l_con(2)=3 + IAVY + IAVX_I
                  connect(nelem)%l_con(5)=2 + IAVY + IAVX_I

               END DO
            END DO
         else
            ! si on a pas compris la direction privelegiee
            print*,'mesh_2D::maillage_Q8: Error: unkown direction!'
            stop
         end if
      END IF
!
      IF(NELEM.NE.NBEle) THEN
         print*,'mesh2D::maillage_Q8: Error: bad computation of the connectivity!'
         stop
      ENDIF

   end subroutine maillage_q8 

   !> this function returns the mesh stored in the module, in the following 
   !> generic format:
   !>    - coor: vector of coordinates of the nodes [x1, y1, x2, y2, ...]
   !>    - nb_node_per_ele: nb_node_per_ele(i) contains the number of nodes for
   !>         element i, i in [1, number of elements]
   !>    - conn: vector storing the connectivity of the elements
   !>         [n11, n12n n13, n21, n22, n23, n24, ...]
   !> consider the following little mesh:\n
   !>    2   4   6\n
   !>    *---*---*\n
   !>    | 1 | 2 |\n
   !>    *---*---*\n
   !>    1   3   5\n
   !> the vectors for this mesh read:
   !>    - coor = [x1, y1, x2, y2, x3, y3, x4, y4, x5, y5, x6, y6]
   !>    - nb_node_per_ele = [4, 4]
   !>    - conn = [1, 3, 4, 2, 3, 5, 6, 4]
   subroutine get_mesh2D(comp_coor, size_coor, nb_node_per_ele, &
      size_nb_node_per_ele, conn, size_conn)

      implicit none
   
      ! variables d'entree :
      integer, intent(in) :: size_coor !< size of comp_coor
      integer, intent(in) :: size_nb_node_per_ele !< size of nb_node_per_ele
      integer, intent(in) :: size_conn !< size of conn

      ! variables de sortie :
      real(kind=8), dimension(size_coor), intent(out) :: comp_coor !< coordinates of nodes in the mesh
      integer, dimension(size_nb_node_per_ele), intent(out) :: nb_node_per_ele !< nb_nodes_per_ele(i) gives the number of nodes for the element i
      integer, dimension(size_conn), intent(out) :: conn !< connectivity of the mesh

      ! variables locales :
      integer :: i ! indice de boucle

      ! test de la compatibilite des donnees :
      !    * coordonnees des noeuds
      if (size_coor /= 2*NbNode) then
         print*, 'mesh2D::get_mesh2D: Error: non conforming size for node coordinates!'
         stop
      end if
      !    * nombres de noeuds des elements : 
      if (size_nb_node_per_ele /= NbEle) then
         print*, 'mesh2D::gest_mesh2D: Error: non conforming size for the number of nodes of each element!'
         stop
      end if
      !    * table de connectivite :
      if (size_conn /= NbNpE*NbEle) then
         print*, 'mesh2D::gest_mesh2D: Error: non conforming size for connectivity!'
         stop
      end if
          
      ! recuperation des coordonnees :
      do i=1, NbNode
         comp_coor(2*(i -1) + 1 : 2*(i - 1) + 2) = coor(:, i)
      end do 
 
      ! recuperation du nombre de noeuds par element, pour chaque element
      nb_node_per_ele = NbNpE

      ! recuperation de la connectivite
      do i=1, NbEle
         conn(NbNpE*(i - 1) + 1 : NbNpE*i) = connect(i)%l_con(:) 
      end do 

   end subroutine get_mesh2D

end module mesh2D
