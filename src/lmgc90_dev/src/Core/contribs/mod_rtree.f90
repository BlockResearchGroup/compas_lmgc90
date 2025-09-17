
module rtree

  use iso_c_binding

  implicit none

  private 

  public f_reset_tree2    , &
         f_reset_tree3    , &
         f_add2tree2      , &
         f_add2tree3      , & 
         f_search_in_tree2, &
         f_search_in_tree3

  interface
    !> Reset the RTree for a 2D search
    subroutine f_reset_tree2() bind(c, name="reset_tree2")
    end subroutine f_reset_tree2
  end interface

  interface
    !> Reset the RTree for a 3D search
    subroutine f_reset_tree3() bind(c, name="reset_tree3")
    end subroutine f_reset_tree3
  end interface

  interface
    !> Add a rectangle to the RTree
    subroutine f_add2tree2(bmin, bmax, id) bind(c, name="add_to_tree2")
      import c_double, c_int
      !> bottom left corner of the rectangle to add
      real(c_double), dimension(2), intent(in) :: bmin
      !> top right corner of the rectangle to add
      real(c_double), dimension(2), intent(in) :: bmax
      !> rectangle id
      integer(c_int), value, intent(in) :: id
    end subroutine f_add2tree2
  end interface

  interface
    !> Add a box to the RTree
    subroutine f_add2tree3(bmin, bmax, id) bind(c, name="add_to_tree3")
      import c_double, c_int
      !> bottom rear left corner of the box to add
      real(c_double), dimension(3), intent(in) :: bmin
      !> top front right corner of the  box to add
      real(c_double), dimension(3), intent(in) :: bmax
      !> box id
      integer(c_int), value, intent(in) :: id
    end subroutine f_add2tree3
  end interface

  interface
    !> Search list of rectangle intersecting with input
    subroutine f_search_in_tree2(bmin, bmax, list) bind(c, name="search_in_tree2")
      import c_double, c_ptr
      !> bottom left corner of the rectangle to add
      real(c_double), dimension(2), intent(in) :: bmin
      !> top right corner of the rectangle to add
      real(c_double), dimension(2), intent(in) :: bmax
      !> list of id intersecting with input
      type(c_ptr), value :: list
    end subroutine f_search_in_tree2
  end interface

  interface
    !> Search list of box intersecting with input
    subroutine f_search_in_tree3(bmin, bmax, list) bind(c, name="search_in_tree3")
      import c_double, c_ptr
      !> bottom rear left corner of the box to add
      real(c_double), dimension(3), intent(in) :: bmin
      !> top front right corner of the  box to add
      real(c_double), dimension(3), intent(in) :: bmax
      !> list of id intersecting with input
      type(c_ptr), value :: list
    end subroutine f_search_in_tree3
  end interface

end module rtree

