! dummy file to compile to generate the lib using
! all symbols of lmgc90

module dumby

  use postpro_3D, only : init_3D => init_postpro_command
  use postpro   , only : init_2D => init_postpro_command
  use timer

  implicit none

  contains

  subroutine dummy()
    implicit none
    integer :: iunit

    iunit = init_3D()
    iunit = init_2D()
    call initialize_itimer()

  end subroutine

end module dumby
