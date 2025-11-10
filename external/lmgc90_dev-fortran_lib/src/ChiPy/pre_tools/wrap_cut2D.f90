! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_cut2D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de decoupe
   use cut2D

   implicit none

contains

   ! procedure qui realise une nouvelle decoupe (alloue les pointeurs)
   subroutine Cut(i_r, ni_r, i_c, ni_c, di_c, s_c, ns_c, ds_c, &
                  o_r, no_r, o_c, do_c, no_c) bind(c, name='cut2D_Cut')
      implicit none
      !> input radius
      real(c_double), dimension(ni_r) :: i_r
      !> number of input radius
      integer(c_int), intent(in), value :: ni_r
      !> input coordinates
      real(c_double), dimension(ni_c,di_c) :: i_c
      !> number of input coor
      integer(c_int), intent(in), value :: ni_c
      !> space dim of input coordinates
      integer(c_int), intent(in), value :: di_c
      !> slope coordinates
      real(c_double), dimension(ns_c,ds_c) :: s_c
      !> space dim of slope coordinates
      integer(c_int), intent(in), value :: ds_c
      !> number of slope coor
      integer(c_int), intent(in), value :: ns_c
      !> output radius
      type(c_ptr) :: o_r
      !> number of output radius
      integer(c_int), intent(out) :: no_r
      !> output coordinates
      type(c_ptr) :: o_c
      !> space dim of output coordinates
      integer(c_int), intent(out) :: do_c
      !> number of output coordinates
      integer(c_int), intent(out) :: no_c
      ! variables locales :
      real(c_double), dimension(:)  , pointer :: radii
      real(c_double), dimension(:,:), pointer :: comp_coor

      radii     => null()
      comp_coor => null()

      no_r = 0
      do_c = 0
      no_c = 0

      o_r = c_null_ptr
      o_c = c_null_ptr

      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des particules
      if( ni_r /= ni_c ) then
         print *,'[wrap_cut_2D::Cut]: FATAL ERROR: non conforming size for ', &
                 'radius and particles coordinates'
         stop
      end if
      ! on teste la compatibilite des donnees :
      !   * pour les coordonnees des points
      if ( ds_c /= 2 ) then
         print *,'[wrap_cut_2D::Cut]: FATAL ERROR: non conforming size for ', &
                 'slope points coordinates (must be 2)'
         stop
      end if
      if ( di_c /= 2 ) then
         print *,'[wrap_cut_2D::Cut]: FATAL ERROR: non conforming size for ', &
                 'input points coordinates (must be 2)'
         stop
      end if

      ! si tout est bon, on initialise une nouvelle decoupe
      call new_cut(ni_r, i_r, i_c, ns_c, s_c)

      ! realisation de la decoupe et recuperartion du nombre de particules a
      ! l'interieur du contour
      no_r = compute()

      if( no_r < 1 ) return

      ! on recupere les rayons directement et les coordonnees sous la dorme 
      ! d'une matrice
      call get_inner_particles(radii, comp_coor, no_r)

      ! too paranaoid
      !if( associated( radii ) then
      !if( size(radii) == no_r ) then
      !if( associated( comp_coor ) then
      !if( size(comp_coor,2) == no_r ) then
      !if( size(comp_coor,1) == 2    ) then

      o_r = c_loc(radii(1))
      o_c = c_loc(comp_coor(1,1))
      do_c = 2
      no_c = no_r

   end subroutine Cut
  
end module wrap_cut2D
