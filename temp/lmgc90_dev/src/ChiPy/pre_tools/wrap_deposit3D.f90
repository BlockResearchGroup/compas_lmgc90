! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_deposit3D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de depot sous gravite
   use deposit3D

   implicit none

contains

   ! procedure qui realise un nouveau depot sous gravite dans une forme
   subroutine deposit3D_deposit(radii , nb_radii , i_shape, p1, p2, p3, &
                                dr, nb_dr, coor, sdim, nb_c, &
                                dradii, nb_dradii, dcoor, nb_dc, dim_dc, &
                                seed, ssize, with_log) bind(c, name='deposit3D_InContainer')
      implicit none
      !> input granulometry
      real(c_double), dimension(nb_radii), intent(in) :: radii
      !> number of particle in input granulometry
      integer(c_int), intent(in), value :: nb_radii
      !> integer id of shape : 0->box, 1->cylinder, 2-> sphere
      integer(c_int), intent(in), value :: i_shape
      !> box->lx, cylinder->R, sphere->R
      real(c_double), intent(in), value :: p1
      !> box->ly, cylinder->lz, sphere->not used
      real(c_double), intent(in), value :: p2
      !> box->lz, cylinder->not used, sphere->not used
      real(c_double), intent(in), value :: p3
      !> pointer on deposited radii
      type(c_ptr)                       :: dr
      !> dim of dr
      integer(c_int)                    :: nb_dr
      !> pointer on computed coordinates
      type(c_ptr)                       :: coor
      !> dim 1 of coor
      integer(c_int)                    :: sdim
      !> dim 2 of coor
      integer(c_int)                    :: nb_c
      !> radii of already deposited (big) particles
      type(c_ptr), value :: dradii
      !> number of already deposited radii
      integer(c_int), intent(in), value :: nb_dradii
      !> coordinates of already deposited (big) particles
      type(c_ptr), value :: dcoor
      !> space dim
      integer(c_int), intent(in), value :: dim_dc
      !> number of already deposited coor
      integer(c_int), intent(in), value :: nb_dc
      !> an input seed to control randomness
      type(c_ptr)   , intent(in), value :: seed
      !> size of seed array
      integer(c_int), intent(in), value :: ssize
      !> with/without log
      integer(c_int), intent(in), value :: with_log
      !
      real(kind=8)   , dimension(:,:), pointer :: comp_coor
      real(kind=8)   , dimension(:)  , pointer :: comp_radii
      real(kind=8)   , dimension(:,:), pointer :: depo_coor
      real(kind=8)   , dimension(:)  , pointer :: depo_radii
      integer(kind=4), dimension(:)  , pointer :: s
      real(kind=8)   , dimension(3), parameter :: s_coor = (/0.d0, 0.d0, 0.d0/)
      logical :: logmes

      ! ugly downcast
      logmes = .false.
      if( with_log /= 0 ) logmes = .true.

      comp_coor  => null()
      comp_radii => null()
      depo_coor  => null()
      depo_radii => null()
      s          => null()

      ! initialisation du conteneur boite
      select case( i_shape )
      case( 0 )
        call set_box(p1, p2, p3)
      case( 1 )
        call set_cylinder(p1, p2)
      case( 2 )
        ! sphere always centered on 0.
        call set_sphere(p1, s_coor)
      case default
        print *, "[ERROR::deposit3D] unknown shape should be 0 (box), 1 (cylinder) or 2 (sphere)"
        stop
      end select

      ! check optional params
      if( c_associated(dradii) .and. nb_dradii > 0 ) then
        call c_f_pointer(cptr=dradii, fptr=depo_radii, shape=(/nb_dradii/))
      end if
      if( c_associated(dcoor) .and. dim_dc > 0 .and. nb_dc > 0 ) then
        call c_f_pointer(cptr=dcoor , fptr=depo_coor , shape=(/dim_dc,nb_dc/))
      end if
      if( c_associated(seed) .and. ssize > 0) then
        call c_f_pointer(cptr=seed, fptr=s, shape=(/ssize/))
      end if
      ! initialisation d'un nouveau depot sous gravite
      call new_deposit(nb_radii, radii, depo_radii, depo_coor, s, logmes)

      ! realisation du depot
      call deposit(logmes)

      ! recuperation des rayons et des coordonnees des particules vraiment deposees :

      ! on recupere les coordonnees sous la forme d'une matrice
      call get_computed_particles(comp_radii, comp_coor)

      ! get outputs
      if( associated(comp_radii) ) then
        dr = c_loc( comp_radii(1) )
        nb_dr = size(comp_radii)
      end if

      if( associated(comp_coor) ) then
        coor = c_loc( comp_coor(1,1) )
        sdim = size(comp_coor,1)
        nb_c = size(comp_coor,2)
      end if

   end subroutine deposit3D_deposit

end module wrap_deposit3D
