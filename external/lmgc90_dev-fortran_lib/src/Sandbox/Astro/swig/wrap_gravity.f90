module wrap_self_gravity

  use ISO_C_BINDING

  use self_gravity, only: &
         compute_fext_2d, &
         compute_fext_3d
  
  public computeFext2D, &
         computeFext3D

CONTAINS

  subroutine computeFext2D(pos, dim1, nb_part1,  masses, nb_part2, &
             theta, dt, fext, sizefext) bind(c, name='self_gravity_ComputeFext2D')
    implicit none
    !input
    integer(c_int), intent(in), value :: nb_part1, dim1, nb_part2, sizefext
    real(c_double), intent(in), value :: theta, dt
    real(c_double), dimension(nb_part1,dim1) :: pos
    real(c_double), dimension(nb_part2)      :: masses
    !output
    real(c_double), dimension(dim1,nb_part1) :: fext
  
    if( dim1*nb_part1 /= sizefext) then
      print *,dim1, nb_part1, sizefext
      print *,"ERROR [self_gravity_computeFext2D] : hummmm"
      stop
    end if
  
    if( nb_part1 /= nb_part2 ) then
      print *,nb_part1, nb_part2
      print *,"ERROR [self_gravity_computeFext2D] : hummmm"
      stop
    end if
  
    if( dim1 /= 3 ) then
      print *,"ERROR [self_gravity_computeFext2D] : wrong dimension, should be 3 and is ", dim1
      stop
    end if

    call compute_fext_2d(pos, masses, fext, nb_part1, theta, dt)
  end subroutine
    
  subroutine computeFext3D(pos, dim1, nb_part1, masses, nb_part2, &
             theta, dt, fext, sizefext) bind(c, name='self_gravity_ComputeFext3D')
    implicit none
    !input
    integer(c_int), intent(in), value :: nb_part1, dim1, nb_part2, sizefext
    real(c_double), intent(in), value :: theta, dt
    real(c_double), dimension(nb_part1,dim1) :: pos
    real(c_double), dimension(nb_part2)     :: masses
    !output
    real(c_double), dimension(dim1,nb_part1) :: fext

    if( dim1*nb_part1 /= sizefext ) then
      print *,"ERROR [self_gravity_computeFext3D] : hummmm"
      stop
    end if
  
    if( nb_part1 /= nb_part2 ) then
      print *,"ERROR [self_gravity_computeFext3D] : hummmm"
      stop
    end if
  
    if( dim1 /= 6 ) then
      print *,"ERROR [self_gravity_computeFext3D] : wrong dimension, should be 6 and is ", dim1
      stop
    end if

    call compute_fext_3d(pos, masses, fext, nb_part1, theta, dt)
  end subroutine

end module wrap_self_gravity

