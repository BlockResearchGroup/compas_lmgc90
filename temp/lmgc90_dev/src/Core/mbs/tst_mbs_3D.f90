
program tst_mbs

  use parameters, only : i_polyr

  use mbs3d

  implicit none

  integer(kind=4), dimension(:), pointer :: faces
  real(kind=8)   , dimension(:), pointer :: vertices
  integer(kind=4), dimension(:), pointer :: idata
  real(kind=8)   , dimension(:), pointer :: rdata

  ! test to add an mbs

  call set_nb(1)

  ! adding a cube contactor

  call set_nb_tacty(1,1)

  allocate(vertices(24))
  vertices( 1:3)  = (/-1.,-1.,-1./)
  vertices( 4:6)  = (/ 1.,-1.,-1./)
  vertices( 7:9)  = (/-1., 1.,-1./)
  vertices(10:12) = (/ 1., 1.,-1./)
  vertices(13:15) = (/-1.,-1., 1./)
  vertices(16:18) = (/ 1.,-1., 1./)
  vertices(19:21) = (/-1., 1., 1./)
  vertices(22:24) = (/ 1., 1., 1./)

  allocate(faces(36))
  faces( 1: 3) = (/1, 3, 2/)
  faces( 4: 6) = (/2, 3, 4/)
  faces( 7: 9) = (/1, 6, 5/)
  faces(10:12) = (/1, 2, 6/)
  faces(13:15) = (/2, 8, 6/)
  faces(16:18) = (/2, 4, 8/)
  faces(19:21) = (/4, 3, 8/)
  faces(22:24) = (/3, 7, 8/)
  faces(25:27) = (/3, 1, 7/)
  faces(28:30) = (/1, 5, 7/)
  faces(31:33) = (/5, 6, 8/)
  faces(34:36) = (/5, 8, 7/)

  ! need set_coor
  call add_tacty(1,1,1,i_POLYR,'color',faces,vertices)


  ! check getter
  if( 1 /= get_nb() ) then
    write(*,*) 'error in set/get_nb'
    stop 1
  end if

  if( 1 /= get_nb_tacty(1) ) then
    write(*,*) 'error in set/get_nb_tacty'
    stop 1
  end if

  if( i_polyr /= get_tacID(1,1) ) then
    write(*,*) 'error when getting tacID'
    stop 1
  end if

  idata => get_ptr_idata(1,1)
  rdata => get_ptr_rdata(1,1)

  if( any(faces(:)/=idata(3:)) .or. idata(1)/=8 .or. idata(2)/=12 ) then
    write(*,*) 'error in idata'
    stop 1
  end if

  if( any(vertices /= rdata) ) then
    write(*,*) 'error in rdata'
    stop 1
  end if

  call display_all()

  deallocate(faces,vertices)
  nullify(faces,vertices)

end program

