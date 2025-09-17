!===========================================================================
!
! Copyright 2000-2025 CNRS-UM.
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
module MBS2D

  use overall, only : get_nb_entity, &
                      add_nb_entity, &
                      logmes, faterr, &
                      h, theta, TPSbegin, PI_g

  use parameters, only : iV____ , iVbeg_ , iVfree, &
                         iIReac , iIaux_ , &
                         i_polyg, i_joncx, i_diskx, &
                         get_contactor_name_from_id

  use externalMBS, only : external_initialize         => initialize        , &
                          external_finalize           => finalize          , &
                          external_increment          => increment         , &
                          external_compute_free_vlocy => compute_free_vlocy, &
                          external_update_nodes       => update_nodes_2D   , &
                          external_nullify_reac       => nullify_reac      , &
                          external_add_reac           => add_reac_2D       , &
                          external_nullify_vlocy      => nullify_vlocy     , &
                          external_comp_vlocy         => comp_vlocy        , &
                          external_get_vlocy          => get_vlocy_2D      , &
                          external_compute_dof        => compute_dof       , &
                          external_update_dof         => update_dof

  implicit none

  private

  !> dimension of space in which the mbss live
  integer(kind=4), parameter :: space_dim = 2
  !> dof size
  integer(kind=4), parameter :: dof_dim = 3

  !> when registering new kind of body to entity list
  integer :: nb_existing_entities = 0

  !> contactor boundary type definition
  type T_BDARY
     !>  idata : anonymous real data space necessary to define the boundary
     integer(kind=4), dimension(:), pointer :: idata => null()
     !>  rdata  : anonymous real data space necessary to define the boundary
     real(kind=8)   , dimension(:), pointer :: rdata => null()
     !>  shift : vector from "support" node to contactor center (expressed in node local frame) \n
     !> a.k.a coordinate of center of contactor in node
     real(kind=8)   , dimension(space_dim)  :: shift

     !> temporary values necessary to build data base
     real(kind=8) :: area, grdg

     ! what's this ?
     logical :: boundary

  end type T_BDARY

  !> A type to store contactors
  type T_tacty
     !> the type of the contactor
     integer(kind=4)  :: tacID
     !> the color of the contactor
     character(len=5) :: color
     !> the node index of which the contactor is tied to
     integer(kind=4)  :: nodID
     !> the description of the boundary of the contactor
     type(T_BDARY)    :: BDARY
  end type T_tacty

  !> A type to store multi-body system data
  type T_MBS

    !> number of nodes in the mbs
    integer(kind=4) :: nb_nodes = 0

    !> space coordinates of the nodes of the mbs
    real(kind=8), dimension(:,:), pointer :: coor => null()
    !> space coordinates of the nodes of the mbs in detection configuration
    real(kind=8), dimension(:,:), pointer :: coorTT => null()

    !> dof of the nodes of the mbs
    !real(kind=8), dimension(:,:), pointer :: dof =>  null()

    !> number of contactor tied to the body
    integer(kind=4) :: nb_tacty = 0
    !> list of contactors tied to the body
    type(T_tacty), dimension(:), allocatable :: tacty

  end type

  !> number of mbs
  integer(kind=4) :: nb_mbs

  !> list of mbs
  type(T_MBS), dimension(:), allocatable :: bdyty

  public set_nb, get_nb                              , &
         set_nb_tacty, get_nb_tacty, add_tacty       , &
         set_nb_nodes                                , &
         get_ptr_idata, get_ptr_rdata                , &
         get_tacID, get_nodeID, get_color            , &
         get_coor                                    , &
         get_coorTT, get_shiftTT                     , &
         get_entity                                  , &
         initialize                                  , &
         finalize                                    , &
         increment                                   , &
         compute_free_vlocy                          , &
         compute_dof                                 , &
         update_dof                                  , &
         display_all                                 , &
         nullify_reac, add_reac                      , &
         nullify_vlocy, comp_vlocy, get_vlocy        , &
         get_ptr_coor, get_ptr_coorTT

contains


  ! remplissage / interrogation de la base de donnees

  !> Set the number of mbs to store
  subroutine set_nb(nb)
    implicit none
    !> number of mbs
    integer(kind=4), intent(in) :: nb

    if( nb_mbs > 0 ) then
      deallocate(bdyty)
    end if

    nb_mbs = nb
    allocate(bdyty(nb_mbs))

  end subroutine

  !> Get the number of mbs stored
  function get_nb()
    implicit none
    !> number of mbs
    integer(kind=4) :: get_nb

    get_nb = nb_mbs

  end function


  !> Set the number contactor on an mbs
  subroutine set_nb_nodes(ibdyty,nb_nodes)
    implicit none
    !> if of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> number of contactors
    integer(kind=4), intent(in) :: nb_nodes
    !
    integer(kind=4)   :: i_node
    character(len=80) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = "MBS2D::set_nb_nodes"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if (associated(bdyty(ibdyty)%coor)) then
       deallocate(bdyty(ibdyty)%coor)
       nullify(bdyty(ibdyty)%coor)
    endif
    if (associated(bdyty(ibdyty)%coorTT)) then
       deallocate(bdyty(ibdyty)%coorTT)
       nullify(bdyty(ibdyty)%coorTT)
    endif

    bdyty(ibdyty)%nb_nodes = nb_nodes

    if ( bdyty(ibdyty)%nb_nodes > 0 ) then
       allocate(bdyty(ibdyty)%coor(dof_dim,nb_nodes))
       allocate(bdyty(ibdyty)%coorTT(dof_dim,nb_nodes))
    end if

    do i_node = 1, bdyty(ibdyty)%nb_nodes
      bdyty(ibdyty)%coor(:,i_node) = 0.d0
      bdyty(ibdyty)%coorTT(:,i_node) = 0.d0
    end do

  end subroutine

  !> Set the number of contactors on an mbs
  subroutine set_nb_tacty(ibdyty,nb_tacty)
    implicit none
    !> if of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> number of contactors
    integer(kind=4), intent(in) :: nb_tacty
    !
    character(len=80) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = "MBS2D::set_nb_tacty"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( bdyty(ibdyty)%nb_tacty > 0 ) then
      allocate(bdyty(ibdyty)%tacty(nb_tacty))
    end if

    allocate(bdyty(ibdyty)%tacty(nb_tacty))
    bdyty(ibdyty)%nb_tacty = nb_tacty

  end subroutine

  !> Get the number of contactors on an mbs
  function get_nb_tacty(ibdyty)
    implicit none
    !> if of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> number of contactors
    integer(kind=4) :: get_nb_tacty
    !
    character(len=80) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = "MBS2D::get_nb_tacty"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    get_nb_tacty = bdyty(ibdyty)%nb_tacty

  end function get_nb_tacty

  !> Add a contactor to an MBS body
  subroutine add_tacty(ibdyty, inodty, itacty, tacID, color, idata, rdata)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> index of the node the new contactor is tied to
    integer(kind=4), intent(in) :: inodty
    !> id of new contactor
    integer(kind=4), intent(in) :: itacty
    !> id of contactor type
    integer(kind=4), intent(in) :: tacID
    !> color of new contactor
    character(len=5), intent(in) :: color
    !> integer data of new contactor
    integer(kind=4), dimension(:), pointer :: idata
    !> real data of new contactor
    real(kind=8), dimension(:), pointer :: rdata
    !
    character(len=80) :: cout
    character(len=16) :: IAM
    character(len=5)  :: tact_type
    !      1234567890123456
    IAM = "MBS2D::add_tacty"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    bdyty(ibdyty)%tacty(itacty)%tacID = tacID
    bdyty(ibdyty)%tacty(itacty)%color = color
    bdyty(ibdyty)%tacty(itacty)%nodID = inodty

    !null initialize
    bdyty(ibdyty)%tacty(itacty)%BDARY%area = 1.d0
    bdyty(ibdyty)%tacty(itacty)%BDARY%grdg = 1.d0

    select case(tacID)
    case(i_diskx)
      call load_diskx_boundary(bdyty(ibdyty)%tacty(itacty)%BDARY, rdata)
    case(i_polyg)
      call load_polyg_boundary(bdyty(ibdyty)%tacty(itacty)%BDARY, idata, rdata)
    case(i_joncx)
      call load_joncx_boundary(bdyty(ibdyty)%tacty(itacty)%BDARY, rdata)
    case default
      tact_type = get_contactor_name_from_id(tacID)
      write(cout,'(A,2(A,1x,I0),A)') '[WARNING]'//IAM,' body', ibdyty,' | contactor', &
                                     itacty, ', contactor not supported : '//tact_type
      call logmes(cout)
    end select

  end subroutine

  !> Read idata and rdata to fill a BDARY object of i_diskx type
  subroutine load_diskx_boundary(bdary, rdata)
    implicit none
    !> boundary object to set
    type(T_BDARY) :: bdary
    !> real data
    real(kind=8), dimension(:), pointer :: rdata
    !rdata(i) have to contains:
    !(1)      -> radius
    !(2:3)    -> shift : coord1, coord2 in node local frame
    !
    character(len=26) :: IAM
    !      12345678901234567890123456
    IAM = "MBS2D::load_diskx_boundary"
    !
    allocate(BDARY%rdata(1))

    ! radius loading
    BDARY%rdata(1) = rdata(1)

    ! Computing mechanical properties
    BDARY%area = PI_g * rdata(1) * rdata(1)

    BDARY%grdg = rdata(1)/SQRT(2.D0)

    !Retrieve shift
    BDARY%shift(1:2) = rdata(2:3)

  end subroutine

  !> Read rdata to fill a BDARY object of i_joncx type
  subroutine load_joncx_boundary(bdary, rdata)
    implicit none
    !> boundary object to set
    type(T_BDARY) :: bdary
    !> real data
    real(kind=8), dimension(:), pointer :: rdata
    !rdata(i) have to contains:
    !(1:2)   -> axis 1 to 2
    !
    character(len=26) :: IAM
    !      12345678901234567890123456
    IAM = "MBS2D::load_joncx_boundary"

    allocate(BDARY%rdata(2))

    ! axis loading
    BDARY%rdata(1) = rdata(1)
    BDARY%rdata(2) = rdata(2)

    ! Computing mechanical properties
    ! Vol = L1*L2 = 2*ax1 * 2*ax2 + pi*ax2**2
    BDARY%area = 4.d0 * rdata(1) * rdata(2) + PI_g*rdata(2)*rdata(2)

    BDARY%grdg = sqrt(( PI_g*rdata(2)*rdata(2)*( rdata(1)*rdata(1)+0.5d0*rdata(2)*rdata(2) ) &
                       +2.d0*rdata(1)*rdata(2)*( rdata(1)*rdata(1)+rdata(2)*rdata(2) ) )/BDARY%area)

    !Impose shift to [0,0] in order to handle it for general case
    BDARY%shift(1:2) = 0.d0

  end subroutine

  !> Read idata and rdata to fill a BDARY object
  subroutine load_polyg_boundary(bdary, idata, rdata)
    implicit none
    !> boundary object to set
    type(T_BDARY) :: bdary
    !> integer data
    integer(kind=4), dimension(:), pointer :: idata
    !> real data
    real(kind=8), dimension(:), pointer :: rdata
    !
    integer(kind=4) :: i, nb_vertices
    real(kind=8)    :: tri_aire, tri_inert, un_3, un_18
    real(kind=8), dimension(2) :: OG, tri_grav
    real(kind=8), dimension(:,:), allocatable :: vertices
    !
    character(len=26) :: IAM
    character(len=80) :: cout
    !      12345678901234567890123456
    IAM = "MBS2D::load_polyg_boundary"

    un_3  = 1.d0 /  3.d0
    un_18 = 1.d0 / 18.d0

    nb_vertices = idata(1)

    ! nb vertices
    allocate(BDARY%idata(1))
    allocate(BDARY%rdata(2*idata(1)))
    BDARY%idata(1) = idata(1)

    allocate( vertices(2,nb_vertices) )
    vertices = reshape(source=rdata,shape=(/2,nb_vertices/))

    ! geometric center of the polygon
    OG(1:2) = 0.d0
    do i= 1, nb_vertices
      OG(1:2) = OG(1:2) + vertices(:,i)
    end do
    OG(1:2) = OG(1:2) / nb_vertices

    BDARY%area = 0.d0
    BDARY%grdg = 0.d0

    do i = 1, nb_vertices-2

      tri_grav = un_3  * ( vertices(:,1)+vertices(:,i+1)+vertices(:,i+2) ) - OG(:)

      tri_aire = 0.5d0 * ( ( ( vertices(1,i+1)-vertices(1,1) )   &
                            *( vertices(2,i+2)-vertices(2,1) ) ) &
                          -( ( vertices(2,i+1)-vertices(2,1) )   &
                            *( vertices(1,i+2)-vertices(1,1) ) ) )

      tri_inert = dot_product( vertices(:,i+1)-vertices(:,1),vertices(:,i+1)-vertices(:,1)) &
                 +dot_product( vertices(:,i+2)-vertices(:,1),vertices(:,i+2)-vertices(:,1)) &
                 -dot_product( vertices(:,i+1)-vertices(:,1),vertices(:,i+2)-vertices(:,1))

      tri_inert = tri_inert*tri_aire*un_18

      BDARY%area = BDARY%area + tri_aire
      BDARY%grdg = BDARY%grdg + tri_inert + tri_aire*dot_product(tri_grav,tri_grav)

    end do

    BDARY%grdg =  BDARY%grdg / BDARY%area

    do i = 1, nb_vertices
      vertices(1:2,i) = vertices(1:2,i) - OG(1:2)
    end do
    BDARY%rdata(:) = reshape(source=vertices,shape=(/2*nb_vertices/))
    BDARY%shift(1:2) = OG(1:2)

    deallocate(vertices)

  end subroutine

  !> Get the identifier type of a contactor of an mbs
  function get_tacID(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> contactor type id
    integer(kind=4) :: get_tacID
    !
    character(len=80) :: cout
    character(len=16) :: IAM
    !      1234567890123456
    IAM = "MBS2D::get_tacID"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    get_tacID = bdyty(ibdyty)%tacty(itacty)%tacID

  end function
  
  !> Get the node index of which the contactor is tied to
  function get_nodeID(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> contactor type id
    integer(kind=4) :: get_nodeID
    !
    character(len=80) :: cout
    character(len=17) :: IAM
    !      12345678901234567
    IAM = "MBS2D::get_nodeID"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if
    
    get_nodeID = bdyty(ibdyty)%tacty(itacty)%nodID
    
  end function
    

  !> Get color of a contactor of an mbs
  function get_color(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> contactor color
    character(len=5) :: get_color
    !
    character(len=80) :: cout
    character(len=16) :: IAM
    !      1234567890123456
    IAM = "MBS2D::get_color"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    get_color = bdyty(ibdyty)%tacty(itacty)%color

  end function

  ! les pointeurs pour exposer la memoire du mbs a python

  !> Get a reference on coor
  function get_ptr_coor(ibdyty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> reference on integer data array
    real(kind=8), dimension(:,:), pointer :: get_ptr_coor
    !
    character(len=80) :: cout
    character(len=19) :: IAM
    !      1234567890123456789
    IAM = "MBS2D::get_ptr_coor"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    get_ptr_coor => bdyty(ibdyty)%coor

  end function

  !> Get a reference on coorTT
  function get_ptr_coorTT(ibdyty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> reference on integer data array
    real(kind=8), dimension(:,:), pointer :: get_ptr_coorTT
    !
    character(len=80) :: cout
    character(len=21) :: IAM
    !      123456789012345678901
    IAM = "MBS2D::get_ptr_coorTT"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    get_ptr_coorTT => bdyty(ibdyty)%coorTT

  end function

  !> Get a reference on integer data array of a contactor
  function get_ptr_idata(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> reference on integer data array
    integer(kind=4), dimension(:), pointer :: get_ptr_idata
    !
    character(len=80) :: cout
    character(len=20) :: IAM
    !      12345678901234567890
    IAM = "MBS2D::get_ptr_idata"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:',nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    get_ptr_idata => bdyty(ibdyty)%tacty(itacty)%BDARY%idata

  end function

  !> Get a reference on real data array of a contactor
  function get_ptr_rdata(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> reference on real data array
    real(kind=8), dimension(:), pointer :: get_ptr_rdata
    !
    character(len=80) :: cout
    character(len=20) :: IAM
    !      12345678901234567890
    IAM = "MBS2D::get_ptr_rdata"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    get_ptr_rdata => bdyty(ibdyty)%tacty(itacty)%BDARY%rdata

  end function

  !--- gestion du calcul

  subroutine initialize()
    implicit none
    integer(kind=4) :: ibdyty

    ! on ferme la base de donnees

    nb_existing_entities = get_nb_entity()
    call add_nb_entity(nb_mbs)

    ! il faudrait charger robotran ici (a voir)
    call external_initialize()

    ! update the position of nodes (bearing contactors) to the initial position
    do ibdyty = 1, nb_mbs
      call external_update_nodes(ibdyty, bdyty(ibdyty)%coor, iV____)
    end do

    !todo
    ! recompute the position of contactors (POLYR) in the node localFrame

    call display_all()

  end subroutine

  ! debut boucle en temps

  subroutine increment()
    implicit none
    integer(kind=4) :: ibdyty

    ! set Ireac and Iaux to zero:
    !     Reac is set to 0 in the beginning of prep_nlgs function while iterating on actual contact.
    !     If all contact impacting a body (mbs in this case) vanish, Reac keep the last impulsions values.
    !     So they have to be set at 0 in the beginning of the step.
    do ibdyty = 1, nb_mbs
        call nullify_reac(ibdyty, iIreac)
        call nullify_reac(ibdyty, iIaux_)
    end do

    ! on avance d'un pas de temps
    call external_increment(TPSbegin)

  end subroutine

  ! debut boucle Newton

  subroutine compute_free_vlocy
    implicit none
    integer(kind=4) :: ibdyty

    ! calcul vitesse libre
    ! ...
    ! on donne coorTT, localFrameTT

    ! compute the contact config and the free velocity
    call external_compute_free_vlocy(H, THETA)

    ! update the position of nodes (bearing contactors)
    do ibdyty = 1, nb_mbs
      call external_update_nodes(ibdyty, bdyty(ibdyty)%coorTT, iVfree)
    end do

  end subroutine

  subroutine compute_dof
    implicit none
    integer(kind=4) :: ibdyty

    ! actualisation vitesse vfree + M^-1 Reac
    call external_compute_dof(H, THETA)

    ! update the position of nodes (bearing contactors)
    do ibdyty = 1, nb_mbs
      call external_update_nodes(ibdyty, bdyty(ibdyty)%coor, iV____)
    end do

  end subroutine

  ! fin boucle newton

  subroutine update_dof
    implicit none

    ! calcul nouvelle configuration

    ! update integration
    call external_update_dof()

  end subroutine

  !fin boucle en temps

  subroutine finalize
    implicit none

    ! on termine le calcul
    call external_finalize()
    call clean_memory()

  end subroutine

  subroutine clean_memory()
    implicit none
    integer(kind=4) :: ibdyty, itacty

    ! on nettoie la memoire de LMGC90

    nb_mbs = 0

    if( allocated(bdyty) ) then
      do ibdyty = 1, size(bdyty)
        if( associated(bdyty(ibdyty)%coor) ) then
          deallocate(bdyty(ibdyty)%coor)
          nullify(bdyty(ibdyty)%coor)
        end if
        if( associated(bdyty(ibdyty)%coorTT) ) then
          deallocate(bdyty(ibdyty)%coorTT)
          nullify(bdyty(ibdyty)%coorTT)
        end if
        !if( associated(bdyty(ibdyty)%dof) ) then
        !  deallocate(bdyty(ibdyty)%dof)
        !  nullify(bdyty(ibdyty)%dof)
        !end if
        if( allocated(bdyty(ibdyty)%tacty) ) then
          do itacty = 1, size(bdyty(ibdyty)%tacty)
            if( associated(bdyty(ibdyty)%tacty(itacty)%BDARY%idata) ) then
              deallocate(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
              nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%idata)
            end if
            if( associated(bdyty(ibdyty)%tacty(itacty)%BDARY%rdata) ) then
              deallocate(bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
              nullify(bdyty(ibdyty)%tacty(itacty)%BDARY%rdata)
            end if
          end do
          deallocate(bdyty(ibdyty)%tacty)
        end if
      end do
      deallocate(bdyty)
    end if

  end subroutine

  !--- les choses necessaires pour le dialogue avec le solver

  !--- interaction solver / mbs

  !> set reac to 0. for a body
  subroutine nullify_reac(ibdyty,storage)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4), intent(in) :: storage

    call external_nullify_reac(ibdyty,storage)
  end subroutine

  !> add reac to a node of a body
  subroutine add_reac(ibdyty,itacty,xxccdof,xxreac,storage)
    implicit none
    !> body number
    integer(kind=4), intent(in) :: ibdyty
    !> contactor number of the body
    integer(kind=4), intent(in) :: itacty
    !> where to store the reaction
    integer(kind=4), intent(in) :: storage
    !> useless in this particular case
    integer(kind=4), dimension(dof_dim), intent(in) :: xxccdof
    !> Forces are expressed in global frame
    !> Torques are expressed in body frame in the contact detection configuration.
    real(kind=8)   , dimension(dof_dim), intent(in) :: xxreac
    !
    integer(kind=4)             :: inodty

    ! get the id of the node corresponding to the given contactor
    inodty = bdyty(ibdyty)%tacty(itacty)%nodID

    ! call the external mbs module (robotran , ...)
    call external_add_reac(ibdyty, inodty, xxreac,storage)

  end subroutine


  !> set velocity to 0. for a multibody
  subroutine nullify_vlocy(ibdyty,storage)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4), intent(in) :: storage

    call external_nullify_vlocy(ibdyty,storage)

  end subroutine

  !> compute velocity of a multibody
  subroutine comp_vlocy(ibdyty,storage)
    implicit none
    integer(kind=4), intent(in) :: ibdyty
    integer(kind=4), intent(in) :: storage

    call external_comp_vlocy(ibdyty,storage)

  end subroutine

  !> get velocity
  function get_vlocy(ibdyty,itacty,storage)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> kind of velocity
    integer(kind=4), intent(in) :: storage
    !> Linear velocities have to be expressed in global frame
    !> Angular velocities have to be expressed in body frame in the contact detection configuration.
    real(kind=8), dimension(dof_dim)  :: get_vlocy
    !
    integer(kind=4)   :: inodty
    character(len=80) :: cout
    character(len=16) :: IAM
    !      1234567890123456
    IAM = "MBS2D::get_vlocy"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    ! get the id of the node corresponding to the given contactor
    inodty = bdyty(ibdyty)%tacty(itacty)%nodID

    call external_get_vlocy(ibdyty, inodty, get_vlocy, storage)

  end function get_vlocy

  !--- dialogue visualisation et detection

  !> Get coordinates of the center of a contactor of a mbs
  function get_coor(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> coordinates of the center of contactor
    real(kind=8), dimension(dof_dim) :: get_coor
    !
    integer(kind=4)   :: inodty, i
    real(kind=8)      :: cos_res, sin_res
    character(len=80) :: cout
    character(len=15) :: IAM
    !      123456789012345
    IAM = "MBS2D::get_coor"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    inodty = bdyty(ibdyty)%tacty(itacty)%nodID

    get_coor(1:2) = bdyty(ibdyty)%coor(1:2,inodty)
    get_coor(3)   = bdyty(ibdyty)%coor(3,inodty)
    
    if (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) /=0.0 .or. &
        bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2) /=0.0 ) then
        cos_res = cos(get_coor(3))
        sin_res = sin(get_coor(3))
        get_coor(1) = get_coor(1) + cos_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                                  - sin_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
        get_coor(2) = get_coor(2) + sin_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) &
                                  + cos_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
    
    end if


  end function get_coor

  !> Get detection coordinates of a contactor of a mbs
  function get_coorTT(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> detection coordinates of the center of contactor
    real(kind=8), dimension(dof_dim) :: get_coorTT
    !
    integer(kind=4)   :: inodty
    character(len=80) :: cout
    character(len=17) :: IAM
    !      12345678901234567
    IAM = "MBS2D::get_coorTT"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    inodty = bdyty(ibdyty)%tacty(itacty)%nodID

    get_coorTT(1:2) = bdyty(ibdyty)%coorTT(1:2,inodty) + get_shiftTT(ibdyty,itacty)
    get_coorTT(3)   = bdyty(ibdyty)%coorTT(3,inodty)

  end function get_coorTT

  !> get distance between center of contactor and node
  function get_shiftTT(ibdyty,itacty)
    implicit none
    !> id of mbs
    integer(kind=4), intent(in) :: ibdyty
    !> id of contactor
    integer(kind=4), intent(in) :: itacty
    !> shift in detection configuration  of a contactor
    real(kind=8), dimension(space_dim) :: get_shiftTT
    !
    integer(kind=4)   :: inodty
    real(kind=8)      :: cos_res, sin_res
    character(len=80) :: cout
    character(len=18) :: IAM
    !      123456789012345678
    IAM = "MBS2D::get_shiftTT"

    if( ibdyty < 1 .or. ibdyty > nb_mbs ) then
      write(cout,'(A,1x,I0,1x,A,I0,A)') 'body', ibdyty, 'not in range [0:', nb_mbs,']'
      call faterr(IAM,cout)
    end if

    if( itacty < 1 .or. itacty > bdyty(ibdyty)%nb_tacty ) then
      write(cout,'(A,1x,I0,1x,A,1x,I0,1x,A,I0,A)') 'tacty', itacty, 'of body', ibdyty, &
                                                   'not in range [0:', bdyty(ibdyty)%nb_tacty,']'
      call faterr(IAM,cout)
    end if

    
    if (bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) /=0.0 .or. &
        bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2) /=0.0 ) then
        inodty = bdyty(ibdyty)%tacty(itacty)%nodID
        cos_res = cos(bdyty(ibdyty)%coorTT(3,inodty))
        sin_res = sin(bdyty(ibdyty)%coorTT(3,inodty))
        get_shiftTT(1) = cos_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) - &
                         sin_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
        get_shiftTT(2) = sin_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(1) + &
                         cos_res*bdyty(ibdyty)%tacty(itacty)%BDARY%shift(2)
    else
        get_shiftTT(:) = bdyty(ibdyty)%tacty(itacty)%BDARY%shift(:)
    end if
  
  end function get_shiftTT

  integer function get_entity(ibdyty)
    implicit none
    integer(kind=4), intent(in) :: ibdyty

    get_entity = nb_existing_entities + ibdyty

  end function get_entity


  !--- verbose

  !> Display the content of the bdyty array
  subroutine display_all()
    implicit none
    integer(kind=4)  :: ibdyty, itacty,i_node
    character(len=5) :: tact_type

    do ibdyty = 1, nb_mbs
      write(*,*) 'MBS2D : ', ibdyty, ' has '
      write(*,*) '        ', bdyty(ibdyty)%nb_nodes, ' nodes'
      do i_node=1, bdyty(ibdyty)%nb_nodes
        write(*,*) '        node ',i_node, ' of coordinates: '
        write(*,'(3(1x,D12.5))') bdyty(ibdyty)%coor(:,i_node)
      enddo
      write(*,*) '        ', bdyty(ibdyty)%nb_tacty, ' contactors'
      do itacty = 1, bdyty(ibdyty)%nb_tacty

        tact_type = get_contactor_name_from_id(bdyty(ibdyty)%tacty(itacty)%tacID)
        write(*,*) '    tact: ', itacty
        write(*,*) '    of type ', tact_type
        write(*,*) '    of color : ', bdyty(ibdyty)%tacty(itacty)%color
        write(*,*) '          BDARY : area  = ', bdyty(ibdyty)%tacty(itacty)%BDARY%area
        write(*,*) '                  shift = ', bdyty(ibdyty)%tacty(itacty)%BDARY%shift
        write(*,*) '                  grdg  = ', bdyty(ibdyty)%tacty(itacty)%BDARY%grdg
      end do
    end do
  end subroutine

end module MBS2D
