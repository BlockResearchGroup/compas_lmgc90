
program test_ann

  use ann

  implicit none

  ! space dimension
  integer(kind=4), parameter :: dime = 2
  ! number of nodes in each tree
  integer(kind=4), parameter :: nb_nodes = 5
  ! definition of two trees
  real(kind=8), dimension(:,:), pointer :: tree, tree2
  ! a test point
  real(kind=c_double), dimension(:), pointer :: point
  ! a list of boundary radii
  real(kind=c_double), dimension(:), pointer :: radii
  ! a translation of nodes of the tree
  real(kind=c_double), dimension(:,:), pointer :: move
  ! reference solution of nearest points index
  integer(kind=c_int), dimension(:), pointer :: sol_n
  ! reference solution of distance between nodes and testin point
  real(kind=c_double), dimension(:), pointer :: sol_dist
  ! the number of error found
  integer(kind=4) :: nb_err
  ! real parameters to compute and store nodes coordinates
  real(kind=8)    :: dx, dy, x, y

  integer(kind=4) :: i
  dx = 1.d0
  dy = 1.d0

  ! generation of two trees: first is *, second is +
  ! 0 is origin of global frame
  ! + + +
  ! +     *
  ! +     *
  ! 0 * * *
  allocate(tree(dime,nb_nodes),tree2(dime,nb_nodes))

  do i = 1, nb_nodes
    if( i <= nb_nodes/2 + 1 ) then
      x = i*dx
      y = 0.d0
     else
      x = (nb_nodes/2 + 1) * dx
      y = (i - nb_nodes/2 - 1) * dy
    end if
    tree(:,i) = (/ x, y /)
    !print *,'adding node : ', i
    !print *, tree(:,i)
    tree2(:,i) =  (/ y, x /)
    !print *,'adding node2 : ', i
    !print *, node(:)
  end do

  ! position of testing point 
  allocate(point(dime))
  point(:) = (/2.6d0, 0.4d0 /)

  ! list of testing radii
  allocate(radii(4))
  radii(:) = (/0.3d0, 0.5d0, 1.3d0, 2.8d0 /)

  ! translation to apply to nodes
  allocate(move(dime,nb_nodes))
  !move(:,1) = (/-3.d0, 4.d0/)
  !move(:,2) = (/-3.d0, 3.d0/)
  !move(:,3) = (/-3.d0, 2.d0/)
  !move(:,4) = (/-2.d0, 0.d0/)
  !move(:,5) = (/-1.d0,-2.d0/)
  move(1,:) = -3.d0
  move(2,:) =  0.d0

  ! reference solution for the test point and each trees
  allocate(sol_n(2*nb_nodes))
  allocate(sol_dist(2*nb_nodes))

  sol_n = (/ 3, 4, 2, 5, 1 , &
             5, 1, 4, 2, 3 /)
  sol_dist = (/ 0.32d0, 0.52d0, 0.52d0, 2.72d0,  2.72d0, &
                7.12d0, 7.12d0, 9.32d0, 9.32d0, 13.52d0 /)

  print *, 'test nearest'
  nb_err = kd_test_nearest(nb_nodes,dime,tree,point,1,sol_n,sol_dist)
  print *, 'test radius'
  nb_err = nb_err + kd_test_radius(nb_nodes,dime,tree,point,radii,sol_n,sol_dist)
  print *, 'test 2 trees'
  nb_err = nb_err + kd_test_2_trees(nb_nodes,dime,tree,tree2,point,1,sol_n,sol_dist)
  print *, 'testing done'

  !!!moving points without creating a new tree... sometime doesn't work
  !!nb_err = nb_err + kd_test_moving_tree(nb_nodes,dime,tree,point,move,sol_n,sol_dist)

  !> \todo : test bd trees vs kd trees
  !nb_err = bd_test_nearest()
  !nb_err = nb_err + bd_test_radius()
  !nb_err = nb_err + bd_test_2_trees()

  !> \todo : time comparison

  deallocate(tree)
  deallocate(tree2)

  deallocate(point,radii)
  deallocate(sol_n,sol_dist)

  if( nb_err > 0 ) then
    stop 1
  end if

contains

  function kd_test_nearest(nb_nodes,dime,tree,point,nb_max_near,sol_n,sol_dist)
    implicit none
    !> space dimension
    integer(kind=4), intent(in) :: dime
    !> number of nodes in the tree
    integer(kind=4), intent(in) :: nb_nodes
    !> the nodes in the tree
    real(kind=8), dimension(:,:), pointer :: tree
    !> a test point
    real(kind=c_double), dimension(:), pointer :: point
    !> maximum number of nearest nodes to look for
    integer(kind=4), intent(in) :: nb_max_near
    !> reference solution of nearest points index
    integer(kind=c_int), dimension(:), pointer :: sol_n
    !> reference solution of distance between nodes and testin point
    real(kind=c_double), dimension(:), pointer :: sol_dist
    !> error
    integer(kind=4) :: kd_test_nearest
    !
    integer(kind=c_int), dimension(:), pointer :: nearests
    real(kind=c_double), dimension(:), pointer :: dists

    kd_test_nearest = 0

    call set_nb_kd(1)
    call add_kd_tree(1, tree, nb_nodes, dime)

    allocate( nearests(nb_max_near) )
    allocate( dists(nb_max_near) )

    do i = 1, nb_max_near
      !print *,'looking for ', i, ' nearest points of ', point
      nearests = 0
      dists = -1.
      call search_nearest_kd_tree(1, point, nearests(1), dists(1))
      !print *,'nearest search done'
      if( any( nearests(1:i) /= sol_n(1:i) ) .or. &
          any( abs(dists(1:i) - sol_dist(1:i))>1.d-8 ) ) then
        kd_test_nearest = kd_test_nearest + 1
        print *,'in tree : '
        print *, nearests(1:i)
        print *, dists(1:i)
        print *,'ref :'
        print *, sol_n(1:i)
        print *, sol_dist(1:i)
      end if

    end do

    deallocate(nearests,dists)
    call clean_memory()

  end function

  function kd_test_radius(nb_nodes,dime,tree,point,radii,sol_n,sol_dist)
    implicit none
    !> space dimension
    integer(kind=4), intent(in) :: dime
    !> number of nodes in the tree
    integer(kind=4), intent(in) :: nb_nodes
    !> the nodes in the tree
    real(kind=8), dimension(:,:), pointer :: tree
    !> a test point
    real(kind=c_double), dimension(:), pointer :: point
    !> list of radii to search in
    real(kind=8), dimension(:), intent(in) :: radii
    !> reference solution of nearest points index
    integer(kind=c_int), dimension(:), pointer :: sol_n
    !> reference solution of distance between nodes and testin point
    real(kind=c_double), dimension(:), pointer :: sol_dist
    !> error
    integer(kind=4) :: kd_test_radius
    !
    integer(kind=c_int), dimension(:,:), pointer :: nearests
    real(kind=c_double), dimension(:,:), pointer :: dists, points
    integer(kind=c_int) :: nb_found

    nearests => null()
    dists    => null()

    allocate( points(size(point),1) )
    points(:,1) = point(:)

    kd_test_radius = 0

    call set_nb_kd(1)
    call add_kd_tree(1, tree, nb_nodes, dime)

    do i = 1, size(radii)

      call radii_search_kd_tree(1, points, 1, radii(i), nearests, dists, nb_found)

      ! two first tests should not found any contact
      if( i<=2 ) then
        if(nb_found > 0 ) then
          print *, 'should not have any neighbour'
          kd_test_radius = kd_test_radius + 1
        end if
        cycle
      end if
  
      ! should find something afterward
      if( any( nearests(1:nb_found,1) /= sol_n(1:nb_found) ) .or. &
          any( abs(dists(1:nb_found,1) - sol_dist(1:nb_found))>1.d-8 ) ) then
        kd_test_radius = kd_test_radius + 1
        print *,'in tree : '
        print *, nearests(1:nb_found,1)
        print *, dists(1:nb_found,1)
        print *,'ref :'
        print *, sol_n(1:nb_found)
        print *, sol_dist(1:nb_found)
      end if

      if( associated(nearests) )then
        call ann_free(nearests)
        call ann_free(dists)
      end if
    end do

    if( associated(nearests) )then
      call ann_free(nearests)
      call ann_free(dists)
    end if

    deallocate(points)
    call clean_memory()

  end function

  function kd_test_2_trees(nb_nodes,dime,tree,tree2,point,nb_max_near,sol_n,sol_dist)
    implicit none
    !> space dimension
    integer(kind=4), intent(in) :: dime
    !> number of nodes in the tree
    integer(kind=4), intent(in) :: nb_nodes
    !> the nodes in the first tree
    real(kind=8), dimension(:,:), pointer :: tree
    !> the nodes in the second tree
    real(kind=8), dimension(:,:), pointer :: tree2
    !> a test point
    real(kind=c_double), dimension(:), pointer :: point
    !> maximum number of nearest nodes to look for
    integer(kind=4), intent(in) :: nb_max_near
    !> reference solution of nearest points index
    integer(kind=c_int), dimension(:), pointer :: sol_n
    !> reference solution of distance between nodes and testin point
    real(kind=c_double), dimension(:), pointer :: sol_dist
    !> error
    integer(kind=4) :: kd_test_2_trees
    !
    integer(kind=c_int), dimension(:), pointer :: nearests
    real(kind=c_double), dimension(:), pointer :: dists

    kd_test_2_trees = 0

    call set_nb_kd(2)

    call add_kd_tree(1, tree, nb_nodes, dime)

    call add_kd_tree(2, tree2, nb_nodes, dime)

    allocate( nearests(nb_max_near) )
    allocate( dists(nb_max_near) )

    do i = 1, nb_max_near
      !print *,'looking for ', i, ' nearest points'
      nearests = 0
      dists = -1.
      call search_nearest_kd_tree(1, point, nearests(1), dists(1))
      if( any( nearests(1:i) /= sol_n(1:i) ) .or. &
          any( abs(dists(1:i) - sol_dist(1:i))>1.d-8 ) ) then
        kd_test_2_trees = kd_test_2_trees + 1
        print *,'in tree 1: '
        print *, nearests(1:i)
        print *, dists(1:i)
        print *,'ref :'
        print *, sol_n(1:i)
        print *, sol_dist(1:i)
      end if

      nearests = 0
      dists = -1.
      call search_nearest_kd_tree(2, point, nearests(1), dists(1))
      if( any( nearests(1:i) /= sol_n(nb_nodes+1:nb_nodes+i) ) .or. &
          any( abs(dists(1:i) - sol_dist(nb_nodes+1:nb_nodes+i))>1.d-8 ) ) then
        print *,'in tree 2: '
        print *, nearests(1:i)
        print *, dists(1:i)
        print *,'ref :'
        print *, sol_n(nb_nodes+1:nb_nodes+i)
        print *, sol_dist(nb_nodes+1:nb_nodes+i)
        kd_test_2_trees = kd_test_2_trees + 1
      end if

    end do

    call clean_memory()

    deallocate(nearests)
    deallocate(dists)

  end function

  function kd_test_moving_tree(nb_nodes,dime,tree,point,move,sol_n,sol_dist)
    implicit none
    !> space dimension
    integer(kind=4), intent(in) :: dime
    !> number of nodes in the tree
    integer(kind=4), intent(in) :: nb_nodes
    !> the nodes in the tree
    real(kind=8), dimension(:,:), pointer :: tree
    !> a test point
    real(kind=c_double), dimension(:), pointer :: point
    !> a vector to translate the nodes of the tree
    real(kind=c_double), dimension(:,:), pointer :: move
    !> reference solution of nearest points index
    integer(kind=c_int), dimension(:), pointer :: sol_n
    !> reference solution of distance between nodes and testin point
    real(kind=c_double), dimension(:), pointer :: sol_dist
    !> error
    integer(kind=4) :: kd_test_moving_tree
    !
    integer(kind=c_int), dimension(:), pointer :: nearests
    real(kind=c_double), dimension(:), pointer :: dists

    kd_test_moving_tree = 0

    call set_nb_kd(1)

    print *, 'setting nodes in tree'
    do i = 1, size(tree)
      tree(i,:) = (/i*1.d0,0.d0/)
      print *, tree(i,:)
    end do
    call add_kd_tree(1, tree, nb_nodes, dime)

    allocate( nearests(1) )
    allocate( dists(1) )

    nearests = 0
    dists = -1.d0
    call search_nearest_kd_tree(1, point, nearests(1), dists(1))
    print *, 'first search'
    print *, nearests(1)
    print *, dists(1)

    print *, 'moving nodes in tree'
    do i = 1, size(tree)
      tree(:,i) = tree(:,i) + move(:,i)
      print *, tree(:,i)
    end do

    print *, 'testing point'
    print *, point
    nearests = 0
    dists = -1.d0
    call search_nearest_kd_tree(1, point, nearests(1), dists(1))
    print *, 'second search'
    print *, nearests(1)
    print *, dists(1)

    call set_nb_kd(1)
    call add_kd_tree(1, tree, nb_nodes, dime)
    nearests = 0
    dists = -1.d0
    call search_nearest_kd_tree(1, point, nearests(1), dists(1))
    print *, 'second first search'
    print *, nearests(1)
    print *, dists(1)

  end function

end program
