program test_predicates

  use predicates

  implicit none

  real(kind=8), dimension(2) :: p2a, p2b, p2c, p2d
  real(kind=8), dimension(3) :: p3a, p3b, p3c, p3d, p3e
  real(kind=8)    :: tol
  real(kind=8)    :: x1,x2,x3,x4
  integer(kind=4) :: i
  logical, dimension(3,4) :: all_res
  logical :: croise
  real(kind=8), dimension(3,4) :: res

  all_res = .false.

  tol = f_exactinit()
  print *, 'tol : ', tol
  tol = 1.000001 * tol

  ! check circle
  p2a = (/1.d0,0.d0     /); p2b = (/0.d0,1.d0     /); p2c = (/-1.d0,0.d0     /)
  ! in
  p2d = (/1.d0-tol, 0.d0/)
  res(1,1) = f_incircle(p2a, p2b, p2c, p2d)
  ! on
  p2d = (/0.d0,-1.d0/)
  res(2,1) = f_incircle(p2a, p2b, p2c, p2d)
  ! out
  p2d = (/-1.d0-tol, 0.d0/)
  res(3,1) = f_incircle(p2a, p2b, p2c, p2d)

  ! check sphere
  p3a = (/1.d0,0.d0,0.d0/); p3b = (/0.d0,1.d0,0.d0/); p3c = (/-1.d0,0.d0,0.d0/); p3d = (/0.d0, 0.d0,-1.d0/)
  ! in
  p3e = (/1.d0-tol, 0.d0, 0.d0/)
  res(1,2) = f_insphere(p3a, p3b, p3c, p3d, p3e)
  ! on
  p3e = (/1.d0, 0.d0, 0.d0/)
  res(2,2) = f_insphere(p3a, p3b, p3c, p3d, p3e)
  ! out
  p3e = (/1.d0+tol, 0.d0, 0.d0/)
  res(3,2) = f_insphere(p3a, p3b, p3c, p3d, p3e)

  ! check axis
  p2a = (/0.d0,0.d0     /); p2b = (/1.d0,1.d0     /)
  ! left
  p2c = (/0.5d0, 0.5d0+tol/)
  res(1,3) = f_orient2d(p2a, p2b, p2c)
  ! on
  p2c = (/0.5d0, 0.5d0/)
  res(2,3) = f_orient2d(p2a, p2b, p2c)
  ! rigth
  p2c = (/0.5d0, 0.5d0-tol/)
  res(3,3) = f_orient2d(p2a, p2b, p2c)

  ! check plan
  p3a = (/0.d0,0.d0,0.d0/); p3b = (/1.d0,0.d0,0.d0/); p3c = (/0.d0,1.d0,0.d0/)
  ! below
  p3d = (/0.d0, 0.d0, 0.d0-tol/)
  res(1,4) = f_orient3d(p3a, p3b, p3c, p3d)
  ! on
  p3d = (/0.d0, 0.d0, 0.d0/)
  res(2,4) = f_orient3d(p3a, p3b, p3c, p3d)
  ! above
  p3d = (/0.d0, 0.d0, 0.d0+tol/)
  res(3,4) = f_orient3d(p3a, p3b, p3c, p3d)

  all_res(1,:) = res(1,:) > 0.d0
  all_res(2,:) = res(2,:) == 0.d0
  all_res(3,:) = res(3,:) < 0.d0

  do i = 1,3
    write(*,'(4(D14.7,1x))') res(i,:)
  end do
  do i = 1,3
    print *, all_res(i,:)
  end do

  if( .not. all(all_res) ) stop 1

  ! test intersection

  ! axis qui se croisent
  p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
  p2c = (/-10.0d0,-tol  /); p2d = (/10.d0,+tol    /)

  x1 = f_orient2d(p2a, p2b, p2c)
  x2 = f_orient2d(p2a, p2b, p2d)  

  croise=.false.
  if (x1*x2 <= 0.d0) then
    x3 = f_orient2d(p2c, p2d, p2a)
    x4 = f_orient2d(p2c, p2d, p2b)  
    if (x3*x4 <= 0.d0) then
      croise=.true.
    endif
  endif

  if (croise) then
    print*,'ok ca croise' 
  else
    print*,'erreur test d intersection'
  endif

  if (.not. croise) stop 1

  ! axis qui ne se croisent pas
  p2a = (/0.d0,0.d0     /); p2b = (/1.d0,0.d0     /)
  p2c = (/-10.0d0,-tol  /); p2d = (/10.d0,-tol    /)

  x1 = f_orient2d(p2a, p2b, p2c)
  x2 = f_orient2d(p2a, p2b, p2d)  

  croise=.false.
  if (x1*x2 <= 0.d0) then
    x3 = f_orient2d(p2c, p2d, p2a)
    x4 = f_orient2d(p2c, p2d, p2b)  
    if (x3*x4 <= 0.d0) then
      croise=.true.
    endif
  endif

  if (croise) then
    print*,'erreur test d intersection'
  else
    print*,'ok ca ne croise pas' 
  endif

  if (croise) stop 1
    

end program test_predicates
