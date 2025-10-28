
module clipper

  use iso_c_binding

  implicit none

  interface
    subroutine c_free(ptr) bind(c, name='free')
      import c_ptr
      type(c_ptr), value :: ptr
    end subroutine
  end interface

  public polygones_intersection , &
         polygone_simplification, &
         clipper_free

  interface clipper_free
     module procedure clipper_free_double_2D
     module procedure clipper_free_double_1D
     module procedure clipper_free_int
  end interface
     
  interface
    !> Compute the intersection of 2 polygons
    subroutine f_clipper_intersection(p1, n1, size_n1, p2, n2, size_n2, shrink1, shrink2, delta, p3, n3, size_n3, area) bind(c, name="clipper_intersection")
      import c_int, c_ptr, c_double
      !> 'subject' input polygon (real array of size 2xn1)
      type(c_ptr),                value :: p1
      !> the number of vertices of each polytope composing p1
      type(c_ptr),                value :: n1
      !> number of poltyope composing p1
      integer(c_int), intent(in), value :: size_n1
      !> 'clip' input polygon (real array of size 2xn2)
      type(c_ptr),                value :: p2
      !> the number of vertices of each polytope composing p2
      type(c_ptr),                value :: n2
      !> number of poltyope composing p2
      integer(c_int), intent(in), value :: size_n2
      !> first polygon's offset
      real(c_double), intent(in), value :: shrink1
      !> second polygon's offset
      real(c_double), intent(in), value :: shrink2
      !> simplification length
      real(c_double), intent(in), value :: delta
      !> 'result' output polygons (real array of size 2xsum(n3))
      type(c_ptr)                       :: p3
      !> sizes of each polytope composing the intersection
      type(c_ptr)                       :: n3
      !> number of poltyope composing the intersection
      integer(c_int)                    :: size_n3
      !> area of each polytope composing the interseciton
      type(c_ptr)                       :: area
    end subroutine f_clipper_intersection
  end interface
  
  interface
    !> Simplify a polygon
    subroutine f_clipper_simplification(pin, nin, delta, pout, nout) bind(c, name="clipper_simplification")
      import c_int, c_ptr, c_double
      !> input polygon (real array of size 2xn1)
      type(c_ptr)               , value  :: pin
      !> number of vertices of pin
      integer(c_int), intent(in), value  :: nin
      !> simplification length
      real(c_double), intent(in), value  :: delta
      !> output polygon (real array of size 2xn2)
      type(c_ptr)                        :: pout
      !> number of vertices of pout
      integer(c_int)                     :: nout
    end subroutine f_clipper_simplification
  end interface

  interface
    !> check if a point is inside a polygon
    subroutine f_clipper_pointinpolygon(px, py, pin, nin, size_nin, resul) bind(c, name="clipper_pointinpolygon")
      import c_int, c_ptr, c_double
      !> 'point' to check (real array of size 2)
      real(c_double), intent(in), value :: px
      real(c_double), intent(in), value :: py
      !> 'subject' input polygon (real array of size 2xn1)
      type(c_ptr),                value :: pin
      !> the number of vertices of each polytope composing p1
      type(c_ptr),                value :: nin
      !> number of poltyope composing pin
      integer(c_int), intent(in), value :: size_nin
      !> result : -1 outside, 1 inside and 0 on edge
      integer(c_int)                    :: resul
    end subroutine f_clipper_pointinpolygon
  end interface

contains

  !> \brief Compute the intersection of polygons with clipper library
  !> Manage non-connexe intersctions
  subroutine polygones_intersection(p1, n1, p2, n2, shrink1, shrink2, delta, p3, n3, area)
    implicit none
    !> [in] vertices of the first polygon
    real(kind=8), dimension(:,:), pointer :: p1
    !> [in] the number of vertices of each polytope composing p1
    integer,      dimension(:),   pointer :: n1
    !> [in] vertices of the second polygon
    real(kind=8), dimension(:,:), pointer :: p2
    !> [in] the number of vertices of each polytope composing p2
    integer,      dimension(:),   pointer :: n2
    !> [out] vertices of the intersection polygons (null if empty)
    real(kind=8), dimension(:,:), pointer :: p3
    !> [out] the number of vertices of each polytope composing the intersection (null if empty)
    integer,      dimension(:),   pointer :: n3
    !> [out] the surface of each polytope composing the intersection
    real(kind=8), dimension(:)  , pointer :: area
    !> [in] the shrink to use for the intersection computation on first polygon
    real(kind=8), intent(in)              :: shrink1
    !> [in] the shrink to use for the intersection computation on second polygon
    real(kind=8), intent(in)              :: shrink2
    !> [in] a size for simplification of intersection polygon
    real(kind=8), intent(in)              :: delta
    !
    integer(c_int) :: size_n3
    type(c_ptr)    :: c_p1, c_n1, c_p2, c_n2, c_p3, c_n3, c_area

    if( associated(p3) ) then
      deallocate(p3)
      nullify(p3)
    end if

    if( associated(n3) ) then
      deallocate(n3)
      nullify(n3)
    end if

    if( associated(area) ) then
      deallocate(area)
      nullify(area)
    end if

    c_p1    = c_loc( p1(1,1) )
    c_n1    = c_loc( n1(1) )
    c_p2    = c_loc( p2(1,1) )
    c_n2    = c_loc( n2(1) )
    c_p3    = c_null_ptr
    c_n3    = c_null_ptr
    c_area  = c_null_ptr

    call f_clipper_intersection(c_p1, c_n1, size(n1), c_p2, c_n2, size(n2), shrink1, shrink2, &
                                delta, c_p3, c_n3, size_n3, c_area)

    if (size_n3 > 0) then
      call c_f_pointer( cptr=c_n3  , fptr=n3  , shape=(/ size_n3 /) )
      call c_f_pointer( cptr=c_area, fptr=area, shape=(/ size_n3 /) )
      call c_f_pointer( cptr=c_p3  , fptr=p3  , shape=(/ 2, sum(n3(1:size_n3)) /) )
    end if

  end subroutine polygones_intersection
  
  
  !> \brief Simplify a polygon with clipper library
  subroutine polygone_simplification(pin, delta, pout)
    implicit none
    !> [in] polygon
    real(kind=8), dimension(:,:), pointer :: pin
    !> [out] vertices of the simplified (null if empty)
    real(kind=8), dimension(:,:), pointer :: pout
    !> [in] a length for simplification of polygon
    real(kind=8), intent(in)              :: delta
    !
    integer(c_int) :: nout
    type(c_ptr)    :: c_pin, c_pout

    if( associated(pout) ) then
      deallocate(pout)
      nullify(pout)
    end if

    c_pin   = c_loc( pin(1,1) )
    c_pout  = c_null_ptr

    call f_clipper_simplification(c_pin, size(pin,2), delta, c_pout, nout)

    if (nout > 0) then
      call c_f_pointer( cptr=c_pout  , fptr=pout  , shape=(/ 2, nout /) )
    end if

  end subroutine polygone_simplification

  !> \brief Check if a point is inside a polygon
  integer function polygone_pointinpolygon(point, pin, nin)
    implicit none
    !> [in] point to check
    real(kind=8), dimension(2), intent(in) :: point
    !> [in] polygon
    real(kind=8), dimension(:,:), pointer  :: pin
    !> [in] the number of vertices of each polytope composing pin
    integer,      dimension(:),   pointer  :: nin
    !> [ou] result
    integer(c_int)                         :: resul
    !
    type(c_ptr)    :: c_pin, c_nin

    c_pin   = c_loc( pin(1,1) )
    c_nin   = c_loc( nin(1) )

    call f_clipper_pointinpolygon(point(1), point(2), c_pin, c_nin, size(nin), resul)

    polygone_pointinpolygon = resul

  end function polygone_pointinpolygon
  

  subroutine clipper_free_double_2D(array)
    implicit none
    real(kind=c_double), dimension(:,:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1,1)) )
    nullify(array)

  end subroutine clipper_free_double_2D

  subroutine clipper_free_double_1D(array)
    implicit none
    real(kind=c_double), dimension(:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1)) )
    nullify(array)

  end subroutine clipper_free_double_1D
  
  subroutine clipper_free_int(array)
    implicit none
    integer(kind=c_int), dimension(:), pointer :: array

    if( .not. associated(array) ) return

    call c_free( c_loc(array(1)) )
    nullify(array)

  end subroutine clipper_free_int

end module clipper
