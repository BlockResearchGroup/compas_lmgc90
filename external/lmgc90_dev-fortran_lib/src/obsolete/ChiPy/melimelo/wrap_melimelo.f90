!> LMGC90 python wrap of melImelO
!> \author F. Dubois
!> \date   January 2009
!>
module wrap_melI_melO

  use ISO_C_BINDING

  use melI_melO

  implicit none

  public createEntityContainer, &
         eraseEntityContainer , &
         addEntity , &
         addAvatar , &
         addBulk   , &
         addNode   , &
         addTact   , &
         addDrvDof , &
         addInitVal, &
         openEntityContainer        , &
         closeEntityContainer       , &
         displayEntityContainer     , &
         pushEntityContainerInMdlHdl, &
         pushInitialValuesInMdlHdl
         !visitEntityContainer

  contains

  !> \brief create entities container
  subroutine createEntityContainer() bind(c, name="melimelo_createEntityContainer")
    implicit none

    call create_entity_container()

  end subroutine

  !> \brief erase entities container
  subroutine eraseEntityContainer() bind(c, name="melimelo_eraseEntityContainer")
    implicit none

    call erase_entity_container()

  end subroutine

  !> \brief add an entity
  subroutine addEntity(e_rank) bind(c, name="melimelo_addEntity")
    implicit none
    integer(c_int), intent(in), value :: e_rank !< [in] entity rank

    call add_entity(e_rank)

  end subroutine

  !> \brief add an avatar to an entity
  subroutine addAvatar(a_rank, type, modelType) bind(c, name="melimelo_addAvatar")
    implicit none
    integer(c_int), intent(in), value :: a_rank   !< [in] avatar rank
    character(c_char), dimension(5) :: type       !< [in] type of avatar (C string)
    character(c_char), dimension(5) ::  modelType !< [in] model type (C string)
    !
    character(len=5) :: type_for, modelType_for ! fortran string
    integer(kind=4)  :: i
    
    type_for=''
    modelType_for=''
    do i = 1, 5
       type_for = type_for(1:i - 1) // type(i)
       modelType_for = modelType_for(1:i - 1) // modelType(i)
    end do

    call add_avatar(a_rank, type_for, modelType_for)

  end subroutine

  !> \brief add a bulk to an entity
  subroutine addBulk(b_rank, type, model, behav, c_real_vect, size_crv, c_integer_vect, size_civ) bind(c, name="melimelo_addBulk")
    implicit none
    integer(c_int), intent(in), value :: b_rank !< [in] bulk rank
    character(c_char), dimension(5)   :: type   !< [in] bulk type (C string)
    character(c_char), dimension(5)   :: model  !< [in] model nickname (C string)
    character(c_char), dimension(5)   :: behav  !< [in] material parameter nickname (C string)
    type(c_ptr), value :: c_real_vect     !< [in] anonymous real vector
    type(c_ptr), value :: c_integer_vect  !< [in] anonymous integer vector
    integer(kind=c_int),value :: size_crv !< [in] size of anonymous real vector
    integer(kind=c_int),value :: size_civ !< [in] size of anonymous integer vector
    !
    character(len=5), dimension(:), pointer :: c5_vect
    integer(kind=4),  dimension(:), pointer :: i4_vect, i4_out
    real(kind=8),     dimension(:), pointer :: r8_vect, r8_out
    character(len=5) :: type_for  ! chaine de 5 caracteres format fortran 
    character(len=5) :: model_for ! idem 
    character(len=5) :: behav_for ! idem
    integer(kind=4)  :: i ! indice de boucle

    c5_vect => null()
    i4_vect => null()
    r8_vect => null()

    ! from C string to Fortran string
    type_for=''
    model_for=''
    behav_for=''
    do i=1, 5
       type_for = type_for(1:i - 1) // type(i)
       model_for = model_for(1:i - 1) // model(i)
       behav_for = behav_for(1:i - 1) // behav(i)
    end do

    allocate(c5_vect(3))
    c5_vect(1) = type_for; c5_vect(2) = model_for; c5_vect(3) = behav_for

    ! if size > 0, get back the fortran array with the right shape
    if( size_crv > 0 ) then
      call c_f_pointer(cptr=c_real_vect, fptr=r8_out, shape=(/ size_crv /) )
      allocate(r8_vect(size_crv))
      r8_vect(1:size_crv) = r8_out(1:size_crv)
    end if
    if( size_civ > 0 ) then
      call c_f_pointer(cptr=c_integer_vect, fptr=i4_out, shape=(/size_civ/) )
      allocate(i4_vect(size_civ))
      i4_vect(1:size_civ) = i4_out(1:size_civ)
    end if

    call add_bulk(b_rank, c5_vect, i4_vect, r8_vect)

  end subroutine

  !> \brief add a node to an entity
  subroutine addNode(n_rank, type, c_coor, size_coor) bind(c, name="melimelo_addNode")
    implicit none
    integer(c_int), intent(in), value :: n_rank !< [in] node rank
    character(c_char), dimension(5)   :: type   !< [in] type of node: NO1xx ... NO6xx  (C string)
    type(c_ptr), value    :: c_coor    !< [in] coordinates of the node (depends on type)
    integer(c_int), value :: size_coor !< [in] size of c_coor array
    !
    character(len=5), dimension(:), pointer :: c5
    real(kind=8),     dimension(:), pointer :: coor, coor_out
    character(len=5) :: type_for ! fortran string
    integer(kind=4)  :: i
    
    c5   => null()
    coor => null()

    type_for=''
    do i=1, 5
      type_for = type_for(1:i - 1) // type(i)
    end do

    allocate(c5(1)); c5(1) = type_for

    if( size_coor > 0 ) then
      call c_f_pointer(cptr=c_coor, fptr=coor_out, shape=(/size_coor/) )
      allocate(coor(size_coor))
      coor(1:size_coor) = coor_out(1:size_coor)
    end if

    call add_node(n_rank, c5, coor)

  end subroutine

  !> \brief add a contactor to an entity
  subroutine addTact(t_rank, type, color, c_real_vect, size_crv, c_integer_vect, size_civ) bind(c, name="melimelo_addTact")
    implicit none
    integer(c_int), intent(in), value :: t_rank !< [in] tact rank
    character(c_char), dimension(5)   :: type   !< [in] type of tact: DISKx, xKSID, POLYG, .... (C string)
    character(c_char), dimension(5)   :: color  !< [in] color of the object  (C string)
    type(c_ptr), value :: c_real_vect    !< [in] anonymous real vector
    type(c_ptr), value :: c_integer_vect !< [in] anonymous integer vector
    integer(c_int), value :: size_crv    !< [in] size of anonymous real vector
    integer(c_int), value :: size_civ    !< [in] size of anonymous integer vector
    !
    character(len=5), dimension(:), pointer :: c5_vect
    real(kind=8),     dimension(:), pointer :: r8_vect, r8_out
    integer(kind=4),  dimension(:), pointer :: i4_vect, i4_out
    character(len=5) :: type_for
    character(len=5) :: color_for
    integer(kind=4)  :: i

    c5_vect => null()
    i4_vect => null()
    r8_vect => null()

    type_for=''
    color_for=''
    do i=1, 5
       type_for = type_for(1:i - 1) // type(i)
       color_for = color_for(1:i - 1) // color(i)
    end do

    allocate(c5_vect(2))
    c5_vect(1) = type_for; c5_vect(2) = color_for

    ! if size > 0, get back the fortran array with the right shape
    if( size_crv > 0 ) then
      call c_f_pointer(cptr=c_real_vect, fptr=r8_out, shape=(/size_crv/) )
      allocate(r8_vect(size_crv))
      r8_vect(1:size_crv) = r8_out(1:size_crv)
    end if
    if( size_civ > 0 ) then
      call c_f_pointer(cptr=c_integer_vect, fptr=i4_out, shape=(/size_civ/) )
      allocate(i4_vect(size_civ))
      i4_vect(1:size_civ) = i4_out(1:size_civ)
    end if

    call add_tact(t_rank, c5_vect, i4_vect, r8_vect)

  end subroutine

  !> \brief add a driven dof to an entity
  subroutine addDrvDof(d_rank, c_c5_vect, size_cc5v, c_integer_vect, size_civ, &
                       c_real_vect, size_crv, c_cx_vect, size_ccxv) bind(c, name="melimelo_addDrvDof")
    implicit none
    integer(c_int), intent(in), value :: d_rank !< [in] drvdof rank
    type(c_ptr), value    :: c_c5_vect          !< [in] anonymous char[5] vector
    integer(c_int), value :: size_cc5v          !< [in] size of anonymous char[5] vector
    type(c_ptr), value    :: c_integer_vect     !< [in] anonymous integer vector
    integer(c_int), value :: size_civ           !< [in] size of anonymous integer vector
    type(c_ptr), value    :: c_real_vect        !< [in] anonymous real vector
    integer(c_int), value :: size_crv           !< [in] size of anonymous real vector
    type(c_ptr), value    :: c_cx_vect          !< [in] anonymous string vector
    integer(c_int), value :: size_ccxv          !< [in] size of anonymous string vector
    !
    real(kind=8),       dimension(:), pointer :: r8_vect, r8_out
    integer(kind=4),    dimension(:), pointer :: i4_vect, i4_out
    character(len=5),   dimension(:), pointer :: c5_vect
    character(len=128), dimension(:), pointer :: cx_vect
    type(c_ptr),        dimension(:), pointer :: sx_vect
    character(c_char),  dimension(:), pointer :: sx
    integer(kind=4)  :: i, j

    c5_vect => null()
    i4_vect => null()
    r8_vect => null()
    cx_vect => null()

    ! if size > 0, get back the fortran array with the right shape
    if( size_cc5v > 0 ) then
      call c_f_pointer(cptr=c_c5_vect, fptr=sx_vect, shape=(/size_cc5v/))
      allocate(c5_vect(size_cc5v))
      do i = 1, size_cc5v
        call c_f_pointer(cptr=sx_vect(i), fptr=sx, shape=(/5/))
        c5_vect(i) = ''
        do j = 1, 5
          c5_vect(i) = c5_vect(i)(1:j-1) // sx(j)
        end do
      end do
    end if
    if( size_civ > 0 ) then
      call c_f_pointer(cptr=c_integer_vect, fptr=i4_out, shape=(/size_civ/))
      allocate(i4_vect(size_civ))
      i4_vect(1:size_civ) = i4_out(1:size_civ)
    end if
    if( size_crv > 0 ) then
      call c_f_pointer(cptr=c_real_vect, fptr=r8_out, shape=(/size_crv/))
      allocate(r8_vect(size_crv))
      r8_vect(1:size_crv) = r8_out(1:size_crv)
    end if
    if( size_ccxv > 0 ) then
      call c_f_pointer(cptr=c_cx_vect, fptr=sx_vect, shape=(/size_ccxv/))
      allocate(cx_vect(size_ccxv))
      do i = 1, size_ccxv
        call c_f_pointer(cptr=sx_vect(i), fptr=sx, shape=(/128/))
        cx_vect(i) = ''
        j = 0
        do while( sx(j+1) /= c_null_char )
          cx_vect(i) = c5_vect(i)(1:j-1) // sx(j)
        end do
      end do
    end if

    call add_drvdof(d_rank, c5_vect, i4_vect, r8_vect, cx_vect)

  end subroutine

  !> \brief add a initial value to a dof of an entity
  subroutine addInitVal(i_rank, c_integer_vect, size_civ, c_real_vect, size_crv) bind(c, name="melimelo_addInitValues")
    implicit none
    integer(c_int), intent(in), value :: i_rank !< [in] initial value rank
    type(c_ptr), value    :: c_integer_vect     !< [in] anonymous integer vector
    integer(c_int), value :: size_civ           !< [in] size of anonymous integer vector
    type(c_ptr), value    :: c_real_vect        !< [in] anonymous real vector
    integer(c_int), value :: size_crv           !< [in] size of anonymous real vector
    !
    real(kind=8),       dimension(:), pointer :: r8_vect, r8_out
    integer(kind=4),    dimension(:), pointer :: i4_vect, i4_out

    i4_vect => null()
    r8_vect => null()

    if( size_civ > 0 ) then
      call c_f_pointer(cptr=c_integer_vect, fptr=i4_out, shape=(/size_civ/))
      allocate(i4_vect(size_civ))
      i4_vect(1:size_civ) = i4_out(1:size_civ)
    end if
    if( size_crv > 0 ) then
      call c_f_pointer(cptr=c_real_vect, fptr=r8_out, shape=(/size_crv/))
      allocate(r8_vect(size_crv))
      r8_vect(1:size_crv) = r8_out(1:size_crv)
    end if

    call add_initval(i_rank, i4_vect, r8_vect)

  end subroutine

  !> \brief close entities container
  subroutine closeEntityContainer() bind(c, name='melimelo_closeEntityContainer')
    implicit none

    call close_entity_container

  end subroutine

  !> \brief open entities container
  subroutine openEntityContainer() bind(c, name='melimelo_openEntityContainer')
    implicit none

    call open_entity_container

  end subroutine

  !> \brief display entities container
  subroutine displayEntityContainer() bind(c, name='melimelo_displayEntityContainer')
    implicit none

    call display_entity_container

  end subroutine

  !> \brief push every avatar of entities container in model_handler
  subroutine pushEntityContainerInMdlHdl() bind(c, name='melimelo_pushEntityContainerInMdlHdl')
    implicit none

    call push_entity_container_in_mdl_hdl

  end subroutine

  !> \brief push initial values of every avatar of entities container in model_handler
  subroutine pushInitialValuesInMdlHdl() bind(c, name='melimelo_pushInitialValuesInMdlHdl')
    implicit none

    call push_initial_values_in_mdl_hdl

  end subroutine

end module
