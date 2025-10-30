
program tst_mbs

  use parameters, only : i_polyg, i_joncx

  use mbs2d

  implicit none

  integer(kind=4), dimension(:), pointer :: nb_vertices
  real(kind=8)   , dimension(:), pointer :: vertices
  real(kind=8)   , dimension(:), pointer :: axes

  ! test to add an mbs

  call set_nb(2)

  ! adding a square contactor

  call set_nb_tacty(1,1)

  allocate(nb_vertices(1))
  nb_vertices(1) = 4

  allocate(vertices(8))
  vertices(1:2) = (/-1.,-1./)
  vertices(3:4) = (/ 1.,-1./)
  vertices(5:6) = (/ 1., 1./)
  vertices(7:8) = (/-1., 1./)

  call add_tacty(1,1,1,i_POLYG,'color',nb_vertices,vertices)

  ! adding a jonc contactor
  call set_nb_tacty(2,1)
  allocate(axes(2))
  axes(1) = 1.d0
  axes(2) = 0.01d0
  call add_tacty(2,1,1,i_JONCx,'color',null(),axes)

  ! check getter
  if( 2 /= get_nb() ) then
    write(*,*) 'error in set/get_nb'
    stop 1
  end if

  if( 1 /= get_nb_tacty(1) ) then
    write(*,*) 'error in set/get_nb_tacty'
    stop 1
  end if

  if( i_polyg /= get_tacID(1,1) ) then
    write(*,*) 'error when getting tacID'
    stop 1
  end if

  call display_all()

  deallocate(nb_vertices,vertices,axes)
  nullify(nb_vertices,vertices,axes)

end program

