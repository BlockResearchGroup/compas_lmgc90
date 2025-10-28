! module d'interface entre le fortran et le C++, pour generer le module python
! avec swig
module wrap_deposit2D

   ! on utilise l'ISO C BINDING
   use iso_c_binding
   ! on utilise le module de depot dans une boite
   use deposit2D

   implicit none

contains

   ! procedure qui realise un nouveau depot sous gravite
   subroutine deposit2D_deposit(given_radii, nb_particles, lx, potential, &
                                coor, sdim, nb_c, &
                                dradii, nb_dradii, dcoor, nb_dc, dim_dc &
                               ) bind(c, name='deposit2D_Potential')
      implicit none
      !> input granulometry
      real(c_double), dimension(nb_particles), intent(in) :: given_radii
      !> number of particle in input granulometry
      integer(c_int), intent(in), value :: nb_particles
      !> box width
      real(c_double), intent(in), value :: lx
      !> potential for the deposit 1->gravity, 2->wall, 3->big_particles
      integer(c_int), intent(in), value :: potential
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
      !
      real(kind=8)   , dimension(:,:), pointer :: comp_coor
      real(kind=8)   , dimension(:,:), pointer :: depo_coor
      real(kind=8)   , dimension(:)  , pointer :: depo_radii

      comp_coor  => null()
      depo_coor  => null()
      depo_radii => null()

      ! check optional params
      if( c_associated(dradii) .and. nb_dradii > 0 ) then
        call c_f_pointer(cptr=dradii, fptr=depo_radii, shape=(/nb_dradii/))
      end if
      if( c_associated(dcoor) .and. dim_dc > 0 .and. nb_dc > 0 ) then
        call c_f_pointer(cptr=dcoor , fptr=depo_coor , shape=(/dim_dc,nb_dc/))
      end if

      ! initialisation d'un nouveau depot sous potentiel
      call new_deposit(nb_particles, given_radii, lx, potential, depo_radii, depo_coor)

      ! realisation du depot
      call depot()

      ! recuperation des coordonnees ;

      ! on recupere les coordonnees sous la dorme d'une matrice
      call get_coor(comp_coor)

      ! get outputs
      if( associated(comp_coor) ) then
        coor = c_loc( comp_coor(1,1) )
        sdim = size(comp_coor,1)
        nb_c = size(comp_coor,2)
      end if

   end subroutine deposit2D_deposit

end module wrap_deposit2D
