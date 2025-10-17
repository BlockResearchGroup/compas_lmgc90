
module ann

  use iso_c_binding

  use timer, only: get_new_etimer_ID, &
                   start_etimer     , &
                   stop_etimer

  implicit none

  interface
    subroutine c_free(ptr) bind(c, name='free')
      import c_ptr
      type(c_ptr), value :: ptr
    end subroutine
  end interface

  interface ann_free
    module procedure ann_free_double
    module procedure ann_free_int
  end interface ann_free

!  private

  public set_nb_kd             , &
         add_kd_tree           , &
         search_nearest_kd_tree, &
         radii_search_kd_tree  , &
         !set_nb_bd             , &
         !add_bd_tree           , &
         !search_nearest_bd_tree, &
         !radius_search_bd_tree , &
         ann_free              , &
         clean_memory

  !private ann_add_kd_tree, &
  !        ann_search_nearest_kd_tree, &
  !        ann_radius_search_kd_tree

  interface
    !> Set the number of trees
    subroutine set_nb_kd(nb) bind(c, name="annSetNbKdTrees")
      import c_int
      !> number of KD trees to store
      integer(c_int), intent(in), value :: nb
    end subroutine set_nb_kd
  end interface

  interface
    !> Add a tree
    subroutine ann_add_kd_tree(i_tree, nodes, nb_nodes, space_dim) bind(c, name="annAddKdTree")
      import c_ptr, c_int
      !> index of tree to add
      integer(kind=c_int), intent(in), value :: i_tree
      !> array of pointers on nodes to add to the tree
      type(c_ptr), value :: nodes
      !> number of elements in nodes array
      integer(kind=c_int), intent(in), value :: nb_nodes
      !> size of each array referenced in each nodes
      integer(kind=c_int), intent(in), value :: space_dim
    end subroutine ann_add_kd_tree
  end interface

  interface
    !> Search nearest nodes of a tree to a point
    subroutine ann_search_nearest_kd_tree(i_tree, test, nb_near, nearests, dists) bind(c, name="annSearchNearestKd")
      import c_ptr, c_int, c_double
      !> index of tree to search in
      integer(kind=c_int), intent(in), value :: i_tree
      !> point to search nearest point
      type(c_ptr), value :: test
      !> number of nearest to look for
      integer(kind=c_int), intent(in), value :: nb_near
      !> the list of nearest nodes found
      type(c_ptr), value :: nearests
      !> the square distance between nearests nodes and the test point
      type(c_ptr), value :: dists
    end subroutine ann_search_nearest_kd_tree
  end interface

  interface
    !> Search all nodes of a tree around a point
    subroutine ann_radii_search_kd_tree(i_tree, tests, nb_tests, radius, founds, dists, max_found) bind(c, name="annRadiiSearchKd")
      import c_ptr, c_int, c_double
      !> index of tree to search in
      integer(kind=c_int), intent(in), value :: i_tree
      !> point to search nearest point
      type(c_ptr), value :: tests
      !> number of point in tests
      integer(kind=c_int), intent(in), value :: nb_tests
      !> radius to search around test point
      real(kind=c_double), intent(in), value :: radius
      !> the list of nearest nodes found
      type(c_ptr) :: founds
      !> the square distance between nearests nodes and the test point
      type(c_ptr) :: dists
      !> max number of nodes found in the radius around test points
      integer(kind=c_int) :: max_found
    end subroutine ann_radii_search_kd_tree
  end interface

  interface
    !> Clean memory allocated in library
    subroutine clean_memory() bind(c, name="annFinalize")
    end subroutine
  end interface

contains

  !> Add a kd tree in ANN library
  subroutine add_kd_tree(i_tree, nodes, nb_nodes, space_dim)
    implicit none
    !> index of tree to add
    integer(kind=4), intent(in), value :: i_tree
    !> array of pointers on nodes to add to the tree
    real(kind=8), dimension(:,:), pointer :: nodes
    !> number of elements in nodes array
    integer(kind=4), intent(in) :: nb_nodes
    !> size of each array referenced in each nodes
    integer(kind=4), intent(in) :: space_dim
    !
    integer(kind=4) :: i_node
    type(c_ptr), dimension(:), pointer :: tree
    real(kind=c_double), dimension(:), pointer :: node
    !
    integer(kind=4), save :: timer_id = 0
    !$omp threadprivate(timer_id)
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[ANN] add KD tree   ')
    call start_etimer(timer_id)

    allocate( tree(nb_nodes) )
    do i_node = 1, nb_nodes
      tree(i_node) = c_loc( nodes(1,i_node) )
    end do

    call ann_add_kd_tree(i_tree-1, c_loc(tree(1)), nb_nodes, space_dim)

    call stop_etimer(timer_id)

  end subroutine add_kd_tree

  !> Search nearest nodes of a tree to a point
  subroutine search_nearest_kd_tree(i_tree, test, i_nearest, dist2)
    !> index of tree to search in
    integer(kind=4)              , intent(in) :: i_tree
    !> point to search nearest point
    real(kind=8)   , dimension(:), pointer    :: test
    !> the index of nearest node found
    integer(kind=4)              , target     :: i_nearest
    !> the square distance between nearest node and the test point
    real(kind=8)                 , target     :: dist2
    !
    integer(kind=4), save :: timer_id = 0
    !$omp threadprivate(timer_id)
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[ANN] search nearest')
    call start_etimer(timer_id)

    i_nearest = -1
    dist2     = -1.d0

    call ann_search_nearest_kd_tree(i_tree-1, c_loc(test(1)), 1, c_loc(i_nearest), c_loc(dist2))

    i_nearest = i_nearest + 1

    call stop_etimer(timer_id)

  end subroutine search_nearest_kd_tree

  !> Search nearest nodes of a tree to a point
  subroutine radii_search_kd_tree(i_tree, tests, nb_tests, radius, i_nearests, sqrdists, max_nearests)
    !> index of tree to search in
    integer, intent(in) :: i_tree
    !> point to search around
    real(kind=8), dimension(:,:), pointer :: tests
    !> number of test points
    integer, intent(in) :: nb_tests
    !> radius aroung test point
    real(kind=8), intent(in) :: radius
    !> the indices of nearest nodes found
    integer     , dimension(:,:), pointer :: i_nearests
    !> the squared distance between the nearest nodes and test point
    real(kind=8), dimension(:,:), pointer :: sqrdists
    !> number of point found around test point
    integer, intent(out) :: max_nearests
    !
    type(c_ptr) :: founds, dists
    !
    integer(kind=4), save :: timer_id = 0
    !$omp threadprivate(timer_id)
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_etimer_ID('[ANN] search radius ')
    call start_etimer(timer_id)

    ! should never happen...
    if( associated(i_nearests) ) then
      write(*,*) '[FATAL ERROR] Wrong input parameter when calling radii_search_kd_tree function'
      stop 1
    end if

    if( associated(sqrdists) ) then
      write(*,*) '[FATAL ERROR] Wrong input parameter when calling radii_search_kd_tree function'
      stop 1
    end if

    founds = c_null_ptr
    dists  = c_null_ptr

    call ann_radii_search_kd_tree(i_tree-1, c_loc(tests(1,1)), nb_tests, radius*radius, founds, dists, max_nearests)

    if( max_nearests > 0 ) then
      call c_f_pointer( founds, i_nearests, (/max_nearests, nb_tests/) )
      call c_f_pointer( dists , sqrdists  , (/max_nearests, nb_tests/) )
      i_nearests(:,:) = i_nearests(:,:) + 1
    end if

    call stop_etimer(timer_id)

  end subroutine radii_search_kd_tree

  subroutine ann_free_double( array )
    implicit none
    real(kind=c_double), dimension(:,:), pointer :: array
    !
    type(c_ptr) :: ptr

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1,1)) )
    nullify( array )

  end subroutine ann_free_double

  subroutine ann_free_int( array )
    implicit none
    integer(kind=c_int), dimension(:,:), pointer :: array
    !
    type(c_ptr) :: ptr

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1,1)) )
    nullify( array )

  end subroutine ann_free_int

end module
