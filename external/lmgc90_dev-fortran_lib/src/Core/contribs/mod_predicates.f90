
module predicates

  use iso_c_binding

  implicit none

  public f_exactinit, &
         f_incircle , &
         f_insphere , &
         f_orient2d , &
         f_orient3d

  interface
    !> initialize predicate functions
    function f_exactinit() bind(c, name="exactinit")
      import C_DOUBLE
      !> epsilon : largest power of 2 such that 1.0 + epsilon = 1.0 ?
      real(c_double) :: f_exactinit
    end function f_exactinit
  end interface

  interface
    !> Check if pd 2D point is in the circle defined by pa, pb and pc points
    !> pa pb and pc must be in counterclockwise order or result sign is reversed.
    function f_incircle(pa, pb, pc, pd) bind(c, name="incircle")
      import C_DOUBLE
      !> first point defining the circle
      real(c_double), dimension(2), intent(in) :: pa
      !> second point defining the circle
      real(c_double), dimension(2), intent(in) :: pb
      !> third point defining the circle
      real(c_double), dimension(2), intent(in) :: pc
      !> point to check if in our out the circle
      real(c_double), dimension(2), intent(in) :: pd
      !> positive value if pd in circle, 0 if on circle, negative value otherwise
      real(c_double) :: f_incircle
    end function f_incircle
  end interface

  interface
    !> Check if pe 3D point is in the sphere defined by pa, pb, pc and pd points
    !> The points must be ordered so that they have a positive orientation
    !> (otherwise result sign is reversed)
    function f_insphere(pa, pb, pc, pd, pe) bind(c, name="insphere")
      import C_DOUBLE
      !> first point defining the sphere
      real(c_double), dimension(3), intent(in) :: pa
      !> second point defining the sphere
      real(c_double), dimension(3), intent(in) :: pb
      !> third point defining the sphere
      real(c_double), dimension(3), intent(in) :: pc
      !> fourth point defining the sphere
      real(c_double), dimension(3), intent(in) :: pd
      !> point to check if in our out the sphere
      real(c_double), dimension(3), intent(in) :: pe
      !> positive value if pd in sphere, 0 if on sphere, negative value otherwise
      real(c_double) :: f_insphere
    end function f_insphere
  end interface

  interface
    !> Check if pc 2D point is left of oriented axes going from pa to pb
    function f_orient2d(pa, pb, pc) bind(c, name="orient2d")
      import C_DOUBLE
      !> first point defining the oriented axis
      real(c_double), dimension(2), intent(in) :: pa
      !> second point defining the oriented axis
      real(c_double), dimension(2), intent(in) :: pb
      !> point to check if is on left of axis
      real(c_double), dimension(2), intent(in) :: pc
      !> positive value if pc is left of axis, 0 if on axis, negative value otherwise
      real(c_double) :: f_orient2d
    end function f_orient2d
  end interface

  interface
    !> Check if pd 3D point is below the oriented surface defined by pa, pb and pc
    !> "below" is defined so that pa, pb, pc appear in counterclockwise order when view from abose the plan.
    !> (otherwise result sign is reversed)
    function f_orient3d(pa, pb, pc, pd) bind(c, name="orient3d")
      import C_DOUBLE
      !> first point defining the plan
      real(c_double), dimension(3), intent(in) :: pa
      !> second point defining the plan
      real(c_double), dimension(3), intent(in) :: pb
      !> third point defining the plan
      real(c_double), dimension(3), intent(in) :: pc
      !> point to check if is above the plan
      real(c_double), dimension(3), intent(in) :: pd
      !> positive value if pd is below the plan, 0 if on the plan, negative value otherwise
      real(c_double) :: f_orient3d
    end function f_orient3d
  end interface

end module predicates
