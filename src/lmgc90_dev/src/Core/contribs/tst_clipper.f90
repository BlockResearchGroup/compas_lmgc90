
program test_clipper 
  use clipper
    
  implicit none

  real(kind=8)    :: offset
  integer(kind=4) :: err, nb_err

  ! global tolerance value
  real(kind=8) :: TOL = 1e-08

  nb_err = 0

  offset = 0.d0

  ! some examples from 'Computational Geometry in C' (J. O'Rourke)
  ! page 261

  ! First convexes

  ! _____
  !|  __|
  !| |__|  ! edge/edge all in
  !|____|
  call edge_edge_in(err)
  nb_err = nb_err + err
  call edge_edge_in(err, offset)
  nb_err = nb_err + err

  ! ___
  !|  _|
  !|_|_|   ! edge/edge with partial overlaping
  !  |_|
  call edge_edge_overlap(err)
  nb_err = nb_err + err
  call edge_edge_overlap(err, offset)
  nb_err = nb_err + err

  !   ___
  ! _|___| ! edge/edge all out
  !|___|
  call edge_edge_out(err)
  nb_err = nb_err + err
  call edge_edge_out(err, offset)
  nb_err = nb_err + err

  ! __
  !|/\|
  !|\/|   ! corners on edges all in
  ! -- 
  call corner_edge_in(err)
  nb_err = nb_err + err
  call corner_edge_in(err, offset)
  nb_err = nb_err + err

  ! ___
  !|   |
  !| |\|    
  !| |/|  ! corner/edge in
  !|   |
  !+---+
  call corner_edge_in2(err)
  nb_err = nb_err + err
  !call corner_edge_in2(err, offset)
  !nb_err = nb_err + err

  ! __
  !|  |/|
  !|  |\|  ! corner/edge out
  ! -- 
  call corner_edge_out(err)
  nb_err = nb_err + err
  call corner_edge_out(err, offset)
  nb_err = nb_err + err

  ! A kind of complex overalp
  ! with some align nodes to check
  ! they are removed
  call complex_overlap(err)
  nb_err = nb_err + err
 

  ! Starting non convex test

  ! 2 simple stars
  call stars(err)
  nb_err = nb_err + err

  ! a comb to test non connex intersection
  !   __   __
  ! _|__|_|__|_
  !| |  | |  | |
  !|_|__|_|__|_|
  !  |  |_|  | 
  !  |_______| 
  call comb(err)
  nb_err = nb_err + err

  ! to check the is_in function
  !    _
  !  _| |_
  ! |_____|
  call is_in(err)
  nb_err = nb_err + err

  if( nb_err > 0 ) then
    write(*,'(I2,1x,A)') nb_err, 'errors'
    stop 1
  end if

contains


  subroutine edge_edge_in(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !12345678901234567890123
    character(len=23), parameter :: IAM = '[clipper::edge_edge_in]'
    !
    real(kind=8), dimension(2,4) :: ref

    !print*,IAM
    
    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/ 1.d0, 0.75d0, 0.5d0, 0.75d0, 0.5d0, 0.25d0, 1.d0, 0.25d0 /), shape=(/2,4/) )
    if( present(offset) ) then
      ref(:,1) = ref(:,1) - offset
      ref(:,3) = ref(:,3) + offset
      ref(1,2) = ref(1,2) + offset
      ref(2,2) = ref(2,2) - offset
      ref(1,4) = ref(1,4) - offset
      ref(2,4) = ref(2,4) + offset
    end if

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 4) )

    polyg1 = reshape( (/ 0.d0 , 0.d0  , 1.d0, 0.d0  , 1.d0 , 1.d0  , 0.d0 , 1.d0  /), (/2,4/) )
    polyg2 = reshape( (/ 0.5d0, 0.25d0, 1.d0, 0.25d0, 1.d0 , 0.75d0, 0.5d0, 0.75d0/), (/2,4/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    res = check_result( IAM, points, sizes, ref, 1, (/4/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)

  end subroutine edge_edge_in



  subroutine edge_edge_overlap(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !1234567890123456789012345678
    character(len=28), parameter :: IAM = '[clipper::edge_edge_overlap]'
    !
    real(kind=8), dimension(2,4) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/ 1.d0, 0.75d0, 0.5d0, 0.75d0, 0.5d0, 0.d0, 1.d0, 0.d0 /), shape=(/2,4/) )
    if( present(offset) ) then
      ref(:,1) = ref(:,1) - offset
      ref(:,3) = ref(:,3) + offset
      ref(1,2) = ref(1,2) + offset
      ref(2,2) = ref(2,2) - offset
      ref(1,4) = ref(1,4) - offset
      ref(2,4) = ref(2,4) + offset
    end if

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 4) )

    polyg1 = reshape( (/ 0.d0 , 0.d0  , 1.d0, 0.d0  , 1.d0 , 1.d0  , 0.d0 , 1.d0  /), (/2,4/) )
    polyg2 = reshape( (/ 0.5d0,-0.25d0, 1.d0,-0.25d0, 1.d0 , 0.75d0, 0.5d0, 0.75d0/), (/2,4/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if
    

    res = check_result( IAM, points, sizes, ref, 1, (/4/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)
    
  end subroutine edge_edge_overlap

  subroutine edge_edge_out(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !123456789012345678901234
    character(len=24), parameter :: IAM = '[clipper::edge_edge_out]'
    !
    real(kind=8), dimension(2,2) :: ref

    points => null()
    area   => null()
    sizes  => null()

    ref = reshape( (/ 0.5d0, 0.75d0, 0.5d0, 0.75d0 /), shape=(/2,2/) )

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 4) )

    polyg1 = reshape( (/ 0.d0 , 0.d0 , 1.d0 , 0.d0 , 1.d0 , 0.5d0, 0.d0 , 0.5d0 /), (/2,4/) )
    polyg2 = reshape( (/ 0.5d0, 0.5d0, 1.5d0, 0.5d0, 1.5d0, 1.d0 , 0.5d0, 1.d0  /), (/2,4/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    ! with or without offset
    res = check_result( IAM, points, sizes, ref, 0, (/0/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)

  end subroutine edge_edge_out


  subroutine corner_edge_in(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !1234567890123456789012345
    character(len=25), parameter :: IAM = '[clipper::corner_edge_in]'
    !
    real(kind=8), dimension(2,4) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/ 1.d0, 0.5d0, 0.5d0, 1.d0, 0.d0, 0.5d0, 0.5d0, 0.d0 /), shape=(/2,4/) )
    if( present(offset) ) then
      ref(1,1) = ref(1,1) - offset * sqrt(2.d0)
      ref(2,2) = ref(2,2) - offset * sqrt(2.d0)
      ref(1,3) = ref(1,3) + offset * sqrt(2.d0)
      ref(2,4) = ref(2,4) + offset * sqrt(2.d0)
    end if

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 4) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    polyg1 = reshape( (/ 0.d0 , 0.d0, 1.d0, 0.d0 , 1.d0 , 1.d0, 0.d0, 1.d0  /), (/2,4/) )
    polyg2 = reshape( (/ 0.5d0, 0.d0, 1.d0, 0.5d0, 0.5d0, 1.d0, 0.d0, 0.5d0 /), (/2,4/) )

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    res = check_result( IAM, points, sizes, ref, 1, (/4/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)

  end subroutine corner_edge_in


  subroutine corner_edge_in2(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !12345678901234567890123456
    character(len=26), parameter :: IAM = '[clipper::corner_edge_in2]'
    !
    real(kind=8), dimension(2,3) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/ 1.d0, 0.5d0, 0.5d0, 0.8d0, 0.5d0, 0.2d0/), shape=(/2,3/) )
    !if( present(offset) ) then
    !  ref(1,1) = ref(1,1) - offset * sqrt(2.d0)
    !  ref(2,2) = ref(2,2) - offset * sqrt(2.d0)
    !  ref(1,3) = ref(1,3) + offset * sqrt(2.d0)
    !  ref(2,4) = ref(2,4) + offset * sqrt(2.d0)
    !end if

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 3) )

    polyg1 = reshape( (/ 0.d0 , 0.d0 , 1.d0 , 0.d0 , 1.d0, 1.d0 , 0.d0, 1.d0/), (/2,4/) )
    polyg2 = reshape( (/ 0.5d0, 0.2d0, 1.d0, 0.5d0, 0.5d0, 0.8d0            /), (/2,3/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    res = check_result( IAM, points, sizes, ref, 1, (/3/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)
    
  end subroutine corner_edge_in2


  subroutine corner_edge_out(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !12345678901234567890123456
    character(len=26), parameter :: IAM = '[clipper::corner_edge_out]'
    !
    real(kind=8), dimension(2,1) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/ 0.5d0, 1.d0 /), shape=(/2,1/) )

    allocate( polyg1(2, 4) )
    allocate( polyg2(2, 3) )

    polyg1 = reshape( (/ 0.d0 , 0.d0, 1.d0 , 0.d0 , 1.d0, 1.d0 , 0.d0, 1.d0/), (/2,4/) )
    polyg2 = reshape( (/ 1.5d0, 2.d0, 1.d0, 0.5d0, 1.5d0, 0.8d0            /), (/2,3/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    ! with or without offset
    res = check_result( IAM, points, sizes, ref, 0, (/0/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)
    
  end subroutine corner_edge_out

  subroutine complex_overlap(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !12345678901234567890123456
    character(len=26), parameter :: IAM = '[clipper::complex_overlap]'
    !
    real(kind=8), dimension(2,7) :: ref
    CHARACTER(len=5)             :: nom

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/11.d0   , 1.d0      , &
                      7.d0   , 5.d0      , &
                      5.d0   , 17.d0/3.d0, &
                      1.875d0, 4.625d0   , &
                      1.d0   , 2.d0      , &
                      3.d0   , 0.d0      , &
                      7.d0   , 0.d0      /), shape=(/2,7/) )

    allocate( polyg1(2,12) )
    allocate( polyg2(2, 8) )

    polyg1 = reshape( (/ 0.d0 , 4.d0, &
                         1.d0 , 2.d0, &
                         2.d0 , 0.d0, &
                         5.d0 , 0.d0, &
                         8.d0 , 0.d0, &
                        11.d0 , 1.d0, &
                        10.d0 , 2.d0, &
                         9.d0 , 3.d0, &
                         8.d0 , 4.d0, &
                         7.d0 , 5.d0, &
                         6.d0 , 6.d0, &
                         3.d0 , 5.d0  /), shape=(/2,12/) )
!    call Draw_Polygon("poly1",polyg1)

    polyg2 = reshape( (/ 1.d0 , 2.d0, &
                         3.d0 , 0.d0, &
                         5.d0 , 0.d0, &
                         7.d0 , 0.d0, &
                        11.d0 , 1.d0, &
                        10.d0 , 4.d0, &
                         4.d0 , 6.d0, &
                         2.d0 , 5.d0  /), shape=(/2,8/) )
!    call Draw_Polygon("poly2",polyg2)

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if
!    call Draw_Polygon("inter",points)

    ! with or without offset
    res = check_result( IAM, points, sizes, ref, 1, (/7/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)    
    call clipper_free(area)

  end subroutine complex_overlap

  subroutine stars(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !1234567890123456
    character(len=16), parameter :: IAM = '[clipper::stars]'
    !
    real(kind=8), parameter :: dso = 2.d0 / 11.d0
    real(kind=8), parameter :: sso = 6.d0 / 11.d0
    real(kind=8), dimension(2,16) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/1.d0,-1.d0, 1.d0+sso,-1.d0+dso, &
                     1.d0, 0.d0, 1.d0+sso, 1.d0-dso, &
                     1.d0, 1.d0, 1.d0-dso, 1.d0+sso, &
                     0.d0, 1.d0,-1.d0+dso, 1.d0+sso, &
                    -1.d0, 1.d0,-1.d0-sso, 1.d0-dso, &
                    -1.d0, 0.d0,-1.d0-sso,-1.d0+dso, &
                    -1.d0,-1.d0,-1.d0+dso,-1.d0-sso, &
                     0.d0,-1.d0, 1.d0-dso,-1.d0-sso  /), shape=(/2,16/) )

    allocate( polyg1(2,8) )
    allocate( polyg2(2,8) )

    polyg1 = reshape( (/ 1.d0 , 1.d0, &
                         4.d0 , 0.d0, &
                         1.d0 ,-1.d0, &
                         0.d0 ,-4.d0, &
                        -1.d0 ,-1.d0, &
                        -4.d0 , 0.d0, &
                        -1.d0 , 1.d0, &
                         0.d0 , 4.d0  /), shape=(/2,8/) )

    polyg2 = reshape( (/ 0.d0 , 1.d0, &
                         3.d0 , 3.d0, &
                         1.d0 , 0.d0, &
                         3.d0 ,-3.d0, &
                         0.d0 ,-1.d0, &
                        -3.d0 ,-3.d0, &
                        -1.d0 , 0.d0, &
                        -3.d0 , 3.d0  /), shape=(/2,8/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    ! with or without offset
    res = check_result( IAM, points, sizes, ref, 1, (/16/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)
    
  end subroutine stars

  subroutine comb(res, offset)
    implicit none
    integer(kind=4)          , intent(inout) :: res
    real(kind=8)   , optional, intent(in)    :: offset 
    !
    real(kind=8), dimension(:,:), pointer :: polyg1, polyg2
    real(kind=8), dimension(:,:), pointer :: points
    real(kind=8), dimension(:)  , pointer :: area
    integer     , dimension(:)  , pointer :: sizes, sizes1, sizes2
                                          !123456789012345
    character(len=15), parameter :: IAM = '[clipper::comb]'
    !
    real(kind=8), dimension(2,8) :: ref

    points => null()
    sizes  => null()
    area   => null()

    ref = reshape( (/2.d0, 2.d0, &
                     2.d0, 3.d0, &
                     1.d0, 3.d0, &
                     1.d0, 2.d0, &
                     4.d0, 2.d0, &
                     4.d0, 3.d0, &
                     3.d0, 3.d0, &
                     3.d0, 2.d0  /), shape=(/2,8/) )

    allocate( polyg1(2,8) )
    allocate( polyg2(2,4) )

    ! a u shape
    polyg1 = reshape( (/ 1.d0 , 0.d0, &
                         4.d0 , 0.d0, &
                         4.d0 , 4.d0, &
                         3.d0 , 4.d0, &
                         3.d0 , 1.d0, &
                         2.d0 , 1.d0, &
                         2.d0 , 4.d0, &
                         1.d0 , 4.d0  /), shape=(/2,8/) )

    ! a - shape
    polyg2 = reshape( (/ 0.d0 , 2.d0, &
                         5.d0 , 2.d0, &
                         5.d0 , 3.d0, &
                         0.d0 , 3.d0  /), shape=(/2,4/) )

    allocate(sizes1(1),sizes2(1))
    sizes1(1) = size(polyg1, dim=2)
    sizes2(1) = size(polyg2, dim=2)

    if( present(offset) ) then
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, offset, offset, 0.d0, points, sizes, area)
    else
      call polygones_intersection(polyg1, sizes1, polyg2, sizes2, 0.d0, 0.d0, 0.d0, points, sizes, area)
    end if

    ! with or without offset
    res = check_result( IAM, points, sizes, ref, 2, (/4,4/) )

    deallocate(polyg1)
    deallocate(polyg2)
    deallocate(sizes1,sizes2)
    call clipper_free(points)
    call clipper_free(sizes)
    call clipper_free(area)
    
  end subroutine comb

  subroutine is_in(res)
    implicit none
    integer, intent(inout) :: res
    !
    real(kind=8), dimension(:,:), pointer :: polyg1
    real(kind=8), dimension(:,:), pointer :: points
    integer,      dimension(:)  , pointer :: sizes1, refult
                                          !1234567890123456
    character(len=16), parameter :: IAM = '[clipper::is_in]'
    !
    integer :: i_point, inside

    polyg1 => null()
    points => null()
    refult => null()

    allocate( polyg1(2,8) )

    polyg1 = reshape( (/-2.d0 , 1.d0, &
                         2.d0 , 1.d0, &
                         2.d0 , 2.d0, &
                         1.d0 , 2.d0, &
                         1.d0 , 4.d0, &
                        -1.d0 , 4.d0, &
                        -1.d0 , 2.d0, &
                        -2.d0 , 2.d0  /), shape=(/2,8/) )
    allocate(sizes1(1))
    sizes1(1) = size(polyg1, dim=2)

    allocate( points(2,6) )

    points = reshape( (/ 0.d0    , 2.d0    , &
                        -1.d0    ,-1.d0    , &
                        -1.d0    , 2.d0    , &
                         1.d0-TOL, 4.d0-TOL, &
                         1.d0+TOL, 4.d0+TOL, &
                         1.d0    , 4.d0     /), shape=(/2,6/) )

    allocate( refult(6)   )
    refult(1) = 1
    refult(2) =-1
    refult(3) = 0
    refult(4) = 1
    refult(5) =-1
    refult(6) = 0

    res = 0
    do i_point = 1, 6
        inside = polygone_pointinpolygon(points(:,i_point),polyg1, sizes1)
        if( inside /= refult(i_point) ) then
          write(*,'(A,1x,I1,1x,I2,1x,A,1x,I2)') IAM//'testing point: ', i_point, inside, 'different from', refult(i_point)
          res = res+1
        end if
    end do
    
  end subroutine

  function check_result(IAM, points, sizes, ref, nb_zones, nb_points)
    implicit none
    character(len=*)                , intent(in) :: IAM
    real(kind=8)    , dimension(:,:), pointer    :: points
    integer(kind=4) , dimension(:)  , intent(in) :: sizes
    real(kind=8)    , dimension(:,:), intent(in) :: ref
    integer(kind=4)                 , intent(in) :: nb_zones
    integer(kind=4) , dimension(:)  , intent(in) :: nb_points
    integer(kind=4) :: check_result
    !
    logical :: found
    integer :: i, j

    check_result = 0

    if( nb_zones == 0 ) then
      if( .not. associated(points) ) then
        return
      else
        write(*,'(A,1x,A)') IAM, 'point found when not expecting any'
        check_result = 1
        return
      end if
    end if

    if( .not. associated(points) ) then
      write(*,'(A,1x,A,1x,I2)') IAM, 'no point found when expecting', sum(nb_points)
      check_result = 1
      return
    end if

    if( size(sizes) /= nb_zones ) then
      write(*,'(A,1x,A)') IAM, 'did not found the expected number of connex zones'
      write(*,'(A,1x,I2,1x,A,1x,I2)') 'found:', size(sizes)-1, 'expected:', nb_zones
      check_result = 1
      return
    end if
    
    if( size(points,2) /= sum(nb_points) ) then
      write(*,'(A,1x,A)') IAM, 'did not found the expected number of points'
      write(*,'(A,1x,I2,1x,A,1x,I2)') 'found:', size(points,2), 'expected:', sum(nb_points)
      check_result = 1
      return
    end if

    do i = 1, size(points,2)
      found = .false.
      do j = 1, size(points,2)
        if( all( abs( ref(:,j)-points(:,i) ) < TOL ) ) then
          found = .true.
          exit
        end if
      end do
      if( .not. found ) then
        write(*,'(A,1x,A)') IAM, 'not expected result'
        write(*,*) '  got     : ', points
        write(*,*) '  expected: ', ref
        write(*,*) '  diff    : ', points-ref
        check_result = 1
        return
      end if
    end do

  end function check_result
  
  SUBROUTINE Draw_Polygon(nom,points)

    implicit none

    CHARACTER(len=5)               :: nom
    REAL(kind=8),DIMENSION(:,:)    :: points
    INTEGER                        :: nbp,i
    character(len=100)             :: filename


    nbp      = size(points,dim=2)
    filename = trim(nom//".vtk")

    open(unit=87, file=filename)
    write(87,'(a)') "# vtk DataFile Version 2.0"
    write(87,'(a)') nom
    write(87,'(a)') "ASCII"
    write(87,'(a)') "DATASET POLYDATA"
    write(87,'(a,i3,a)') "POINTS",nbp," float"
    do i=1,nbp
        write(87,*) points(1,i),points(2,i),"0.000"
    enddo
    write(87,'(a,i3)') "POLYGONS 1",nbp+1
    write(87,*) nbp,(/ (i-1, i=1, nbp) /)
    close(87)

END SUBROUTINE Draw_Polygon

end program ! test_clipper
