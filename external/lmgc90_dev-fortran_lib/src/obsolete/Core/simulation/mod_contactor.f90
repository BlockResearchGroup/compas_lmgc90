!===========================================================================
!
! Copyright 2000-2022 CNRS-UM.
!
! This file is part of a software (LMGC90) which is a computer program 
! which purpose is to modelize interaction problems (contact, multi-Physics,etc).
!
! This software is governed by the CeCILL license under French law and
! abiding by the rules of distribution of free software.  You can  use, 
! modify and/ or redistribute the software under the terms of the CeCILL
! license as circulated by CEA, CNRS and INRIA at the following URL
! "http://www.cecill.info". 
!
! As a counterpart to the access to the source code and  rights to copy,
! modify and redistribute granted by the license, users are provided only
! with a limited warranty  and the software's author,  the holder of the
! economic rights,  and the successive licensors  have only  limited
! liability. 
!
! In this respect, the user's attention is drawn to the risks associated
! with loading,  using,  modifying and/or developing or reproducing the
! software by the user in light of its specific status of free software,
! that may mean  that it is complicated to manipulate,  and  that  also
! therefore means  that it is reserved for developers  and  experienced
! professionals having in-depth computer knowledge. Users are therefore
! encouraged to load and test the software's suitability as regards their
! requirements in conditions enabling the security of their systems and/or 
! data to be ensured and,  more generally, to use and operate it in the 
! same conditions as regards security. 
!
! The fact that you are presently reading this means that you have had
! knowledge of the CeCILL license and that you accept its terms.
!
! To report bugs, suggest enhancements, etc. to the Authors, contact
! Frederic Dubois.
!
! frederic.dubois@umontpellier.fr
!
!===========================================================================

!> manage the various contactors
module contactor

  use overall
  use parameters

  use DISKx, only : set_data_DISKx
  use JONCx, only : set_data_JONCx
  use SPHER, only : set_data_SPHER
  use PLANx, only : set_data_PLANx
  use ALpxx, only : set_data_ALPxx
  use CLxxx, only : set_data_CLxxx

  implicit none
  private

  !> Generic contactor type
  type, public :: T_contactor
    !> the index of model the contactor belongs to
    integer(kind=4)  :: model_ID
    !> disk, polygon, ...
    integer(kind=4)  :: geometric_ID
    !> color of the contactor
    character(len=5) :: color

    !> anonymous integer data
    integer(kind=4), dimension(:), pointer :: idata => null()
    !> anonymous real data
    real(kind=8),    dimension(:), pointer :: rdata => null()

    !> list of index of the nodes of the model_handle to use
    integer(kind=4), dimension(:), allocatable :: mdlhdl_node_list

    !> number of support nodes of the contactor
    integer(kind=4) :: nb_support
    !> support nodes in reference coordinates
    real(kind=8), dimension(:,:), allocatable :: support_nodes
    !> support nodes in contact detection configuration
    real(kind=8), dimension(:,:), allocatable :: support_nodesTT

    !> radius of the encompassing circle/sphere
    real(kind=8) :: Brd
    !> Axis Aligned Bounding Box (/ (/xmin,xmax/)^T, (/ymin,ymax/)^T, (/zmin, zmax/)^T/) 
    real(kind=8) :: AABB(2,3)
    !> big contactor
    logical      :: is_big

  end type T_contactor

  type :: T_contactor_ptr
    type(T_contactor), pointer :: contactor => null()
  end type

  !> all the ponters on contactor in the system
  type(T_contactor_ptr), dimension(:), allocatable :: all_contactors

  !!> number of point describing the contactor outline
  !integer(kind=4) :: nbpto=20
  !!!> number of scalar field on each contactor
  !!integer(kind=4) :: nbsf=9 
  !!> the number of points defining each outline
  !integer(kind=4),dimension(:),pointer :: nb_point_outlines => null() 
  !!> the outline of the contactors
  !real(kind=8),dimension(:,:),pointer  :: outlines          => null()
  !!> the scalar fields of the contactors 
  !real(kind=8),dimension(:),pointer    :: scalarfields      => null()

  private set_data

  public set_nb_contactors     , &
         add_contactor         , &
         get_nb_contactors     , &
         get_contactor         , &
         compute_support_points, &
         get_rough_boundaries  , &
         get_nb_point_outline  , &
         get_outline           , &
         !get_model_ID, &
         !get_geometry, &
         !get_color, &
         !get_radius, &
         !get_support_point, &
         !get_data, &
         !is_big, &
         !set_is_big, &
         !init_outlines        , &
         clean_module

  contains

  !> \brief Allocate all_contactors array
  subroutine set_nb_contactors(nb_contactors)
    implicit none
    !> [in] number of contactors
    integer(kind=4), intent(in) :: nb_contactors
    !
    integer(kind=4)   :: i
    character(len=80) :: cout
    character(len=28) :: IAM
    !      1234567890123456789012345678
    IAM = 'contactor::set_nb_contactors'

    if( nb_contactors < 1 ) then
      return
    end if

    if( allocated(all_contactors) ) then
      write(cout,'(A,A,A)') '[',IAM,'] all_contactors already allocated'
      call logmes(cout)
      deallocate(all_contactors)
    end if

    allocate( all_contactors(nb_contactors), stat=i )

    if( i /= 0 ) then
      write(cout,*) 'Problem while allocating all_contactors'
      call faterr(IAM,cout)
    end if

  end subroutine

  !> \brief Get the number of contactors
  function get_nb_contactors()
    implicit none
    !> [return] the number of contactors
    integer(kind=4) :: get_nb_contactors

    get_nb_contactors = 0
    if( allocated(all_contactors) ) then
      get_nb_contactors = size(all_contactors)
    end if

  end function

  !> \brief Add one contactor
  subroutine add_contactor(i_contactor, i_model, c5, i4, r8)
    implicit none
    !> [in] index of the contactor
    integer(kind=4), intent(in) :: i_contactor
    !> [in] index of the model owning the contactor
    integer(kind=4), intent(in) :: i_model
    !> [in] anonymous string data
    character(len=5), dimension(:), pointer, intent(in) :: c5
    !> [in] anonymous integer data
    integer(kind=4),  dimension(:), pointer, intent(in) :: i4
    !> [in] anonymous real data
    real(kind=8),     dimension(:), pointer, intent(in) :: r8
    !
    integer(kind=4) :: geometric_id

    if( associated(all_contactors(i_contactor)%contactor) ) then
      nullify(all_contactors(i_contactor)%contactor)
    end if
    allocate(all_contactors(i_contactor)%contactor)

    all_contactors(i_contactor)%contactor%model_ID = i_model
    all_contactors(i_contactor)%contactor%is_big   = .false.

    geometric_id = get_contactor_id_from_name(c5(1))

    all_contactors(i_contactor)%contactor%geometric_id = geometric_id
    all_contactors(i_contactor)%contactor%color = c5(2)

    if( associated(i4) ) then
      allocate(all_contactors(i_contactor)%contactor%idata(size(i4)))
      all_contactors(i_contactor)%contactor%idata = i4
    else
      nullify( all_contactors(i_contactor)%contactor%idata )
    end if

    if( associated(r8) ) then
      allocate(all_contactors(i_contactor)%contactor%rdata(size(r8)))
      all_contactors(i_contactor)%contactor%rdata = r8
    else
      nullify( all_contactors(i_contactor)%contactor%rdata )
    end if

    call set_data(all_contactors(i_contactor)%contactor)

    if( allocated(all_contactors(i_contactor)%contactor%support_nodesTT) ) then
      deallocate(all_contactors(i_contactor)%contactor%support_nodesTT)
    end if
    allocate(all_contactors(i_contactor)%contactor%support_nodesTT(nbDIME,all_contactors(i_contactor)%contactor%nb_support))
    all_contactors(i_contactor)%contactor%support_nodesTT = 0.d0

  end subroutine

  !> \brief Intrepret anonymous data to initialize common data of contactors
  !> Set the shift, the frame, get the supporting nodes and their interpolation weight
  !> and finally get the radius of the encompassing circle/sphere
  subroutine set_data(contactor)
    implicit none
    type(T_contactor) :: contactor !< [in] a contactor
    character(len=80) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = 'contactor::set_data'

    select case( contactor%geometric_id )
    case( i_diskx )
      call set_data_diskx(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_xksid )
    !case( i_polyg )
    case( i_joncx )
      call set_data_joncx(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_pt2dx )
    case( i_spher )
      call set_data_spher(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_cylnd )
    !case( i_dnlyc )
    !case( i_polyr )
    case( i_planx )
      call set_data_planx(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_pt3dx )
    case( i_alpxx )
      call set_data_alpxx(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_aspx3 )
    !case( i_aspx4 )
    case( i_clxxx )
      call set_data_clxxx(contactor%idata, contactor%rdata, contactor%mdlhdl_node_list, &
                          contactor%nb_support, contactor%support_nodes, contactor%Brd)
    !case( i_csxx3 )
    case default
      write(cout,'(A,1x,I0)') 'unknown contactor geometry:', get_contactor_name_from_id(contactor%geometric_id)
      call faterr(IAM,cout)
    end select
    !print *,'add contactor : '
    !print *,contactor%model_ID
    !print *,get_geometry_from_id(contactor%geometric_ID) !disk, polygon, ...
    !print *,contactor%color
    !if( associated(contactor%idata) ) print *,contactor%idata
    !if( associated(contactor%rdata) ) print *,contactor%rdata
    !print *,contactor%shift
    !print *,contactor%frame
    !print *,contactor%nb_inter
    !print *,contactor%Brd
  end subroutine

  !> \brief Get a pointer onto a contactor
  function get_contactor(i_contactor)
    implicit none
    !> [in] index of the desired contactor
    integer(kind=4), intent(in) :: i_contactor
    !> [return] pointer on the contactor
    type(T_contactor), pointer :: get_contactor

    get_contactor => all_contactors(i_contactor)%contactor

  end function

  !> \brief Update support_pointTT depending on a TT configuration
  subroutine compute_support_points(contactor, frameTT, dispTT)
    implicit none
    !> [in,out] contactor to work on
    type(T_contactor), pointer :: contactor
    !> [in] inertia frame ( center + basis )
    real(kind=8), dimension(:,:), allocatable :: frameTT
    !> [in] nodes displacement
    real(kind=8), dimension(:,:), allocatable :: dispTT 
    !
    integer(kind=4) :: i

    ! coor = coorTT + frameTT( support + detTT )
    ! if rigid coorTT and frameTT are not null (and stacked in frameTT variable)
    ! if defor coorTT and frameTT are null and dispTT is in fact the cooref + dispTT but support is not used.

    if( allocated(dispTT) ) then
      contactor%support_nodesTT = dispTT(1:nbDIME, contactor%mdlhdl_node_list)
    else
      contactor%support_nodesTT = contactor%support_nodes
    end if

    if( allocated(frameTT) ) then
      do i = 1, contactor%nb_support
        contactor%support_nodesTT(1:nbDIME,i) = frameTT(1:nbDIME,1) + &
                                                matmul(frameTT(1:nbDIME,2:nbDIME+1),contactor%support_nodesTT(1:nbDIME,i))
      end do
    end if

    ! \todo : if( contactor%quadrature /= 0 ) do something !

  end subroutine

  !> \brief Get the list of boundaries to use in rough detection
  subroutine get_rough_boundaries(contactor, nb_rb, rb)
    implicit none
    !> [in,out] contactor to work on
    type(T_contactor), pointer :: contactor
    !> [out] number of rough boundaries
    integer(kind=4), intent(out) :: nb_rb
    !> [in,out] pointer with real values corresponding to rough boundaries
    real(kind=8), dimension(:), pointer :: rb
    !
    integer(kind=4)   :: taille, i
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'contactor::get_rough_boundaries'

    if( associated(rb) ) deallocate(rb)

    select case( contactor%geometric_id )
    case( i_diskx )
      nb_rb = 1
      allocate( rb(3) )
      rb(1:2) = contactor%support_nodesTT(1:2,1)
      rb(3)   = contactor%Brd
    case( i_joncx )
      nb_rb = 1
      allocate( rb(3) )
      rb(1:2) = sum(contactor%support_nodesTT(1:nbDIME, :), dim=2) / contactor%nb_support
      rb(3)   = contactor%Brd
    case( i_spher )
      nb_rb = 1
      allocate( rb(4) )
      rb(1:3) = contactor%support_nodesTT(1:3,1)
      rb(4)   = contactor%Brd
    case( i_planx )
      nb_rb = 1
      allocate( rb(4) )
      rb(1:3) = sum(contactor%support_nodesTT(1:nbDIME, :), dim=2) / contactor%nb_support
      rb(4)   = contactor%Brd
    case( i_alpxx )
      nb_rb = contactor%nb_support/2
      allocate( rb(nb_rb*3) )
      do i = 1, nb_rb
        rb(i*3-2:i*3-1) = sum(contactor%support_nodesTT(1:nbDIME, 2*i-1:2*i), dim=2) / 2
        rb(i*3)         = sqrt( dot_product(contactor%support_nodesTT(1:nbDIME, 2*i)-rb(i*3-2:i*3-1), &
                                            contactor%support_nodesTT(1:nbDIME, 2*i)-rb(i*3-2:i*3-1)  ) ) 
      end do
    case( i_clxxx )
      nb_rb = contactor%nb_support/2
      allocate( rb(nb_rb*3) )
      do i = 1, nb_rb
        ! depends on quadrature ?
        rb(i*3-2:i*3-1) = contactor%support_nodesTT(1:nbDIME, 2*i-1) * contactor%rdata(i) + &
                          contactor%support_nodesTT(1:nbDIME,2*i) * (1.d0-contactor%rdata(i))
        rb(i*3)         = contactor%Brd ! in set_data...
      end do
    case default
      write(cout,'(A,1x,I0)') 'unknown contactor geometry:', get_contactor_name_from_id(contactor%geometric_id)
      call faterr(IAM,cout)
    end select

  end subroutine

  !> \brief Get the number of outline point of the a contactor
  !> \todo API not consistent, everywhere the pointer on the contactor is used... except here :s
  function get_nb_point_outline(i_contactor)
    implicit none
    !> [in] i_contactor: number of contactor
    integer(kind=4), intent(in) :: i_contactor
    !> [return] number of points defining the outline of the contactor
    integer(kind=4) :: get_nb_point_outline
    !
    character(len=80) :: cout
    character(len=31) :: IAM
    !      1234567890123456789012345678901
    IAM = 'contactor::get_nb_point_outline'

    select case( all_contactors(i_contactor)%contactor%geometric_id )
    case(i_diskx )
      get_nb_point_outline = 20
    case(i_joncx )
      get_nb_point_outline = 40
    case(i_polyg, i_alpxx, i_clxxx )
      get_nb_point_outline = all_contactors(i_contactor)%contactor%nb_support
    case default
      write(cout,'(A,1x,I0)') 'unknown contactor geometry:', &
                              get_contactor_name_from_id(all_contactors(i_contactor)%contactor%geometric_id)
      call faterr(IAM,cout)
    end select

  end function

  !> \brief Get the number of outline point of the a contactor
  !> \todo API not consistent, everywhere the pointer on the contactor is used... except here :s
  subroutine get_outline(contactor, coor, outline, nb_po)
    implicit none
    !> [in,out] contactor to work on
    type(T_contactor), pointer :: contactor
    !> [in] coordinates of the nodes of the model_handle of the contactor
    real(kind=8), dimension(:,:) :: coor
    !> [out] outline
    real(kind=8), dimension(nbDIME,nb_po) :: outline
    !> [in] number of outline point
    integer(kind=4) :: nb_po
    !
    integer(kind=4)   :: i
    character(len=80) :: cout
    character(len=22) :: IAM
    !      1234567890123456789012
    IAM = 'contactor::get_outline'
    

    ! coor = coorTT + frameTT( outline + detTT )
    ! if rigid: coorTT and frameTT are not null (and stacked in frameTT variable)
    ! if defor: coorTT and frameTT are null and dispTT is in fact the cooref + dispTT but support is not used.
    select case( contactor%geometric_id )
    case(i_diskx )
      outline(1:2,1) = coor(1:2,1) + matmul(coor(1:2,2:3),contactor%support_nodes(1:2,1))
      do i = 2, nb_po
        outline(1,i) = outline(1,1) + contactor%Brd*cos( (i-1)*2*3.14159 / (nb_po-2) )
        outline(2,i) = outline(2,1) + contactor%Brd*sin( (i-1)*2*3.14159 / (nb_po-2) )
      end do
    !case(i_joncx )
    !  get_nb_point_outline = 40
    !case(i_polyg, i_alpxx, i_clxxx )
    !  get_nb_point_outline = all_contactors(i_contactor)%contactor%nb_support
    !case(i_alpxx, i_clxxx )
    !  outline = dispTT(1:nbDIME, contactor%mdlhdl_node_list)
    case default
      write(cout,'(A,1x,A)') 'unknown contactor geometry:', get_contactor_name_from_id(contactor%geometric_id)
      call faterr(IAM,cout)
    end select

    ! \todo : if( contactor%quadrature /= 0 ) do something !

  end subroutine

  !!> \brief Initialization of the geometric outline of the contactors
  !function init_outlines()
  !  implicit none
  !  !> [return] the outline of the contactors
  !  real(kind=8), dimension(:,:), pointer :: init_outlines
  !  !
  !  integer(kind=4) :: nb_contactor, i_tact, sz
 
  !  nb_contactors = get_nb_contactors()

  !  if( nb_contactors < 1 ) then
  !    init_outlines => null()
  !    return
  !  endif 
 
  !  if (associated(nb_point_outlines)) deallocate(nb_point_outlines)
  !  allocate(nb_point_outlines(nb_contactors+1)) 
  !  nb_point_outlines(1) = 0
  !  do i_tact = 1, nb_contactors
  !    nb_point_outlines(i_tact+1) = nb_point_outlines(i_tacty) + nbpto - 1
  !  end do
 
  !  sz = nb_point_outlines(nb_contactors+1)
 
  !  if (associated(outlines)) deallocate(outlines)
  !  allocate(outlines(nbDIME,sz)) 
 
  !  outlines(1:nbDIME,1:sz) = 0.d0
 
  !  init_outlines => outlines
 
  !end function init_outlines

  !!> \brief Update the outlines of the contactors
  !!> \todo should be done by interaction_handler to provide coordinates in input...
  !!>       and should choose among the contactors type to call the get_outline of each sub module
  !subroutine update_postdata()
  !  implicit none
  !  integer(kind=4) :: i_tact, iszo, iszsf, nb_contactors
  !  real(kind=8) :: outline(nbDIME,0:nbpto)
 
  !  nb_contactors = get_nb_contactors()

  !  iszo  = 0
  !  iszsf = 0

  !  do i_tact = 1, nb_contacotrs
  !    ! rm : only DISKx currently...
  !    call get_outline2_DISKx(outline, all_contactors(i_tact)%contactor%rdata, coor)
  !    outlines(:,iszo+1:iszo+nbpto-1) = outline(:,1:nbpto-1)
  !    iszo = iszo + nbpto-1
  !    !call get_scalarfields_DISKx(itacty,scalarfields_DISKx(iszsf+1:iszsf+nbsf))
  !    !iszsf = iszsf +nbsf
  !  end do
 
  !end subroutine update_postdata

  !> \brief Free memory allocated within the module
  subroutine clean_module()
    implicit none
    integer(kind=4) :: i_tact

    if( allocated(all_contactors) ) then
      do i_tact = 1, size(all_contactors)
        if( associated(all_contactors(i_tact)%contactor) ) then
          if( associated(all_contactors(i_tact)%contactor%idata) ) then
            deallocate(all_contactors(i_tact)%contactor%idata)
            nullify(all_contactors(i_tact)%contactor%idata)
          end if
          if( associated(all_contactors(i_tact)%contactor%rdata) ) then
            deallocate(all_contactors(i_tact)%contactor%rdata)
            nullify(all_contactors(i_tact)%contactor%rdata)
          end if
          if( allocated(all_contactors(i_tact)%contactor%mdlhdl_node_list) ) then
            deallocate(all_contactors(i_tact)%contactor%mdlhdl_node_list)
          end if
          if( allocated(all_contactors(i_tact)%contactor%support_nodes) ) then
            deallocate(all_contactors(i_tact)%contactor%support_nodes)
          end if
          if( allocated(all_contactors(i_tact)%contactor%support_nodesTT) ) then
            deallocate(all_contactors(i_tact)%contactor%support_nodesTT)
          end if
        end if
        deallocate(all_contactors(i_tact)%contactor)
        nullify(all_contactors(i_tact)%contactor)
      end do
      deallocate(all_contactors)
    end if

  end subroutine

end module contactor
