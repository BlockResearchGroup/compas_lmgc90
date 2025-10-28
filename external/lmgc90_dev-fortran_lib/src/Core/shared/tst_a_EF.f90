
program test

  use a_EF

  implicit none

  integer :: is_good, nb_err

  nb_err = 0

  print *,'----------------'
  is_good = triangle_test()
  nb_err  = nb_err + is_good
  print *,'----------------'
  is_good = quadrangle_test()
  nb_err  = nb_err + is_good
  print *,'----------------'
  is_good = second_form_test()
  nb_err  = nb_err + is_good
  print *,'----------------'


  if( nb_err > 0 ) then
    stop 1
  end if

  contains

  integer function triangle_test()
    implicit none
    !
    real(kind=8) :: S(2,3), center(2), ref(2), tol

    triangle_test = 0

    tol = 1.d-8
    ref = (/ 1.08333333d0, 0.75d0 /)

    S(:,1) = (/ 0.5d0, 0.5d0 /)
    S(:,2) = (/ 0.75d0, 1.0d0 /)
    S(:,3) = (/ 2.0d0, 0.75d0 /)

    call Compute_center(i_t_p1,S,center)

    if ( any( abs(center - ref) > tol ) ) then
      triangle_test = 1
      print *, "Triangle center computation : WTF?!", center, ref
    else 
      print *,"Triangle center computation : ok !!!!"
    end if

  end function

  integer function quadrangle_test()
    implicit none
    !
    real(kind=8) :: S(2,4), center(2), ref(2), tol, f(4)

    quadrangle_test = 0

    tol = 1.d-16
    ref = (/ 0.5d0, 1.d0 /)

    S(:,1) = (/ 0.5d0, 0.5d0 /)
    S(:,2) = (/ 1.d0 , 1.0d0 /)
    S(:,3) = (/ 0.5d0, 1.5d0 /)
    S(:,4) = (/ 0.d0 , 1.0d0 /)

    call Compute_center(i_q_p1,S,center)

    if ( any( abs(center - ref) > tol ) ) then
      quadrangle_test = 1
      print *, "Quadrangle center computation : WTF?!", center, ref
    else
      print *,"Quadrangle center computation : ok !!!!"
    end if

    f = (/0.d0,1.d0,2.d0,2.d0/)
    center(:) = (/-1.d0,-1.d0/)
    call interpolate_field(f,4,i_q_p1,2,center,ref(1))
    if( ref(1) /= 0.d0 ) then
      quadrangle_test = quadrangle_test + 1
      print *, "Quadrangle interpolation : WTF?!", center, ref(1)
    else
      print *,"Quadrangle interpolation 1 : ok !!!!"
    end if
    center(:) = (/0.d0,-1.d0/)
    call interpolate_field(f,4,i_q_p1,2,center,ref(1))
    if( ref(1) /= 0.5d0 ) then
      quadrangle_test = quadrangle_test + 1
      print *, "Quadrangle interpolation : WTF?!", center, ref(1)
    else
      print *,"Quadrangle interpolation 2 : ok !!!!"
    end if
    center(:) = (/0.d0,0.d0/)
    call interpolate_field(f,4,i_q_p1,2,center,ref(1))
    if( ref(1) /= 1.25d0 ) then
      quadrangle_test = quadrangle_test + 1
      print *, "Quadrangle interpolation : WTF?!", center, ref(1)
    else
      print *,"Quadrangle interpolation 3 : ok !!!!"
    end if
    center(:) = (/0.d0,1.d0/)
    call interpolate_field(f,4,i_q_p1,2,center,ref(1))
    if( ref(1) /= 2.d0 ) then
      quadrangle_test = quadrangle_test + 1
      print *, "Quadrangle interpolation : WTF?!", center, ref(1)
    else
      print *,"Quadrangle interpolation 4 : ok !!!!"
    end if

  end function

  integer function second_form_test()
    implicit none
    integer(kind=4) :: i
    real(kind=8), dimension(3) :: x
    !real(kind=8), dimension(:)  , pointer :: N
    !real(kind=8), dimension(:,:), pointer :: DN
    real(kind=8), dimension(:,:), pointer :: DDN => null()

    second_form_test = 0

    x = (/0.2d0, 0.5d0, 1.d0/)
    call second_forme(i_h_p1, x, DDN)

    print *, 'h_p1', shape(DDN)
    do i = 1, 8
      print *, ddn(i,:)
    end do

    call second_forme(i_h_p2, x, DDN)
    print *, 'h_p2', shape(DDN)
    do i = 1, 20
      print *, ddn(i,:)
    end do

  end function

end program test

