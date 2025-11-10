!> LMGC90 python wrap of model_handler
!> \author F. Dubois
!> \date   January 2010
!>

module wrap_model_handler

  use ISO_C_BINDING

  use parameters, only : get_storage_id_from_type

  use mdl_hdl_parameters

  use model_handler, only : initialize                       , &
                            change_system_storage            , &
                            get_nb_models                    , &
                            !get_nb_mecaModels    , &
                            !get_nb_therModels    , &
                            compute_elementary_masses        , &
                            compute_elementary_capacities    , &
                            compute_elementary_conductivities, &
                            compute_elementary_bulks         , &
                            compute_elementary_fext          , &
                            assemble_elementary_rhs_4all     , &
                            assemble_elementary_lhs_4all     , &
                            compute_free_state               , &
                            compute_dofs                     , &
                            compute_residue_norm             , &
                            !compute_coor_detec   , &
                            !get_inertia          , &
                            get_type                         , &
                            get_nb_nodal_field               , &
                            get_nb_elementary_rhs_contrib    , &
                            get_connectivities               , &
                            get_coor                         , &
                            get_nodal_field                  , &
                            get_ptr_nodal_field              , &
                            set_nodal_field                  , &
                            get_rhs_contribution             , &
                            get_elementary_field             , &
                            update                           , &
                            clean_module

  IMPLICIT NONE

  public initializeModelHandles              , &
         changeSystemStorage                 , &
         getNbModelHandles                   , &
         getModelHandleType                  , &
         computeElementaryMasses4All         , &
         computeElementaryMasses4One         , &
         computeElementaryMasses4Some        , &
         computeElementaryCapacities4All     , &
         computeElementaryCapacities4One     , &
         computeElementaryCapacities4Some    , &
         computeElementaryConductivities4All , &
         computeElementaryConductivities4One , &
         computeElementaryConductivities4Some, &
         computeElementaryBulks4All          , &
         computeElementaryBulks4One          , &
         computeElementaryBulks4Some         , &
         computeElementaryFext4All           , &
         computeElementaryFext4One           , &
         computeElementaryFext4Some          , &
         computeFreeState4all                , &
         computeDofs4All                     , &
         computeResidueNorm                  , &
         !computeDetectionCoor4All, &
         !getInertia              , &
         getCoordinates                      , &
         getNodalField                       , &
         getNodalFieldPtr                    , &
         setNodalField                       , &
         getNodalFieldNames                  , &
         getRhsContribution                  , &
         getRhsContributionNames             , &
         getElementaryField                  , &
         updateState4All                     , &
         updateState4One                     , &
         updateState4Some                    , &
         cleanMemory

contains

  function getNbModelHandles() bind(c, name='model_handler_getNb')
    implicit none
    integer(c_int) :: getNbModelHandles

    getNbModelHandles = get_nb_models()
  end function

  subroutine getModelHandleType(id, string_out, string_size) bind(c, name='model_handler_getModelType')
    implicit none
    integer(c_int), intent(in), value :: id
    type(c_ptr)                       :: string_out
    integer(c_int), intent(out)       :: string_size
    !
    character(len=9)     , pointer :: model
    character(len=c_char), pointer :: gna

    allocate(model)
    model = get_model_type_from_id( get_type(id) )

    string_size = len(trim(model))
    gna         => model(1:1)
    string_out  = c_loc( gna )
    
  end subroutine

  subroutine initializeModelHandles() bind(c, name='model_handler_initialize')
    implicit none

    call initialize()

  end subroutine

  subroutine changeSystemStorage(id, storage) bind(c, name='model_handler_changeSystemStorage')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: storage
    !
    character(len=8) :: string
    integer(kind=4)  :: i, i_storage

    string = ''
    i = 1
    do while( storage(i) /= C_NULL_CHAR .and. i < 9 )
      string = string(1:i-1) // storage(i)
      i = i + 1
    end do

    i_storage = get_storage_id_from_type(string)

    call change_system_storage(id, i_storage)

  end subroutine

  subroutine computeElementaryMasses4All() bind(c, name="model_handler_computeElementaryMasses4All")
    implicit none
    integer(kind=4) :: id

    !> \todo get a list of mechanical model_handles instead
    do id = 1, get_nb_models()
      call compute_elementary_masses(id)
    end do

  end subroutine

  subroutine computeElementaryMasses4One(id) bind(c, name="model_handler_computeElementaryMasses4One")
    implicit none
    integer(c_int), intent(in), value :: id

    call compute_elementary_masses(id)

  end subroutine

  subroutine computeElementaryMasses4Some(list_ids, length) bind(c, name="model_handler_computeElementaryMasses4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call compute_elementary_masses(list(i))
    end do

  end subroutine

  subroutine computeElementaryCapacities4All() bind(C, name="model_handler_computeElementaryCapacities4All")
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call compute_elementary_capacities(id)
    end do

  end subroutine

  subroutine computeElementaryCapacities4One(id) bind(C, name="model_handler_computeElementaryCapacities4One")
    implicit none
    integer(kind=c_int), intent(in), value :: id

    call compute_elementary_capacities(id)

  end subroutine

  subroutine computeElementaryCapacities4Some(list_ids, length) bind(c, name="model_handler_computeElementaryCapacities4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call compute_elementary_capacities(list(i))
    end do

  end subroutine

  subroutine computeElementaryConductivities4All() bind(C, name="model_handler_computeElementaryConductivities4All")
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call compute_elementary_conductivities(id)
    end do

  end subroutine

  subroutine computeElementaryConductivities4One(id) bind(C, name="model_handler_computeElementaryConductivities4One")
    implicit none
    integer(kind=c_int), intent(in), value :: id

    call compute_elementary_conductivities(id)

  end subroutine

  subroutine computeElementaryConductivities4Some(list_ids, length) &
             bind(c, name="model_handler_computeElementaryConductivities4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call compute_elementary_conductivities(list(i))
    end do

  end subroutine

  subroutine computeElementaryBulks4All() bind(c, name="model_handler_computeElementaryBulks4All")
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call compute_elementary_bulks(id)
    end do

  end subroutine

  subroutine computeElementaryBulks4One(id) bind(c, name="model_handler_computeElementaryBulks4One")
    implicit none
    integer(c_int), intent(in), value :: id

    call compute_elementary_bulks(id)

  end subroutine

  subroutine computeElementaryBulks4Some(list_ids, length) bind(c, name="model_handler_computeElementaryBulks4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call compute_elementary_bulks(list(i))
    end do

  end subroutine

  subroutine computeElementaryFext4All() bind(c, name="model_handler_computeElementaryFext4All")
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call compute_elementary_fext(id)
    end do

  end subroutine

  subroutine computeElementaryFext4One(id) bind(c, name="model_handler_computeElementaryFext4One")
    implicit none
    integer(c_int), intent(in), value :: id

    call compute_elementary_fext(id)

  end subroutine

  subroutine computeElementaryFext4Some(list_ids, length) bind(c, name="model_handler_computeElementaryFext4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call compute_elementary_fext(list(i))
    end do

  end subroutine

  subroutine cleanMemory() bind(c, name='model_handler_cleanMemory')
    implicit none

    call clean_module
  end subroutine

  subroutine updateState4All() bind(C, name="model_handler_updateState4All")
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call update(id)
    end do

  end subroutine

  subroutine updateState4One(id) bind(c, name="model_handler_updateState4One")
    implicit none
    integer(c_int), intent(in), value :: id

    call update(id)

  end subroutine

  subroutine updateState4Some(list_ids, length) bind(c, name="model_handler_updateState4Some")
    implicit none
    type(c_ptr),    intent(in), value :: list_ids
    integer(c_int), intent(in), value :: length
    !
    integer(kind=4) :: i
    integer(kind=4), dimension(:), pointer :: list

    call c_f_pointer(cptr=list_ids, fptr=list, shape=(/length/))
    do i = 1, length
      call update(list(i))
    end do

  end subroutine

  subroutine assemble4all() bind(C, name='model_handler_assemble4All')
    implicit none

    ! \todo : model_handler parameters with p_RHS_something instead of 1
    call assemble_elementary_rhs_4all(1)
    call assemble_elementary_lhs_4all()

  end subroutine

  subroutine computeFreeState4all() bind(c, name='model_handler_computeFreeState4All')
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()
      call compute_free_state(id,1,1)
    end do

  end subroutine

  subroutine computeDofs4All() bind(c, name='model_handler_computeDofs4All')
    implicit none
    integer(kind=4) :: id

    do id = 1, get_nb_models()

      ! rm : change [1,2] in [2] because Vfree is now added in compute_free_state
      call compute_dofs(id,2,2,(/2/))

    end do

  end subroutine

  function computeResidueNorm() bind(c, name='model_handler_computeResidueNorm')
    implicit none
    real(c_double) :: computeResidueNorm

    ! use free + reac contributions to compute convergence norm
    computeResidueNorm = compute_residue_norm(1,(/1,2/))

  end function

!  function getNbMecaModels() bind(C, name='model_handler_getNbMecaModels')
!    implicit none
!    integer(kind=c_int) :: getNbMecaModels
!
!    getNbMecaModels = get_nb_mecaModels()
!
!  end function
!
!  function getNbTherModels() bind(C, name='model_handler_getNbTherModels')
!    implicit none
!    integer(kind=c_int) :: getNbTherModels
!
!    getNbTherModels = get_nb_therModels()
!
!  end function
!
!  subroutine computeDetectionCoor4All() bind(C, name="model_handler_computeDetectionCoor4All")
!    implicit none
!    integer(kind=4) :: id
!
!    do id = 1, get_nb_mecaModels()
!      call compute_coor_detec(id)
!    end do
!
!  end subroutine
!
!  subroutine getInertia(id, inertia, length) bind(C, name="model_handler_getInertia")
!    implicit none
!    integer(c_int), intent(in) :: id, length
!    real(c_double) :: inertia(length)
!
!    !call get_inertia(id, inertia)
!
!  end subroutine

  subroutine getConnectivities(id,vector,length) bind(c, name='model_handler_getConnectivities')
    implicit none
    integer(c_int), intent(in), value :: id
    type(c_ptr)    :: vector
    integer(c_int) :: length
    !
    integer(kind=4), dimension(:), pointer :: connectivities

    connectivities => get_connectivities(id)

    if( associated(connectivities) ) then
      vector = c_loc(connectivities(1))
      length = size(connectivities)
    else
      vector = c_null_ptr
      length = 0
    end if

  end subroutine

  ! ref (meca/ther), detec, begin, now (meca)
  subroutine getCoordinates(id,what,vector,dime,nb_nodes) bind(c, name='model_handler_getCoordinates')
    implicit none
    integer(c_int) , intent(in), value  :: id
    character(len=c_char), dimension(*) :: what
    type(c_ptr)    :: vector
    integer(c_int) :: dime, nb_nodes
    !
    integer(kind=4)  :: i_what, i_type, i
    character(len=5) :: f_what
    real(kind=8), dimension(:,:), pointer :: coor

    coor => null()

    i_what   = 0
    dime     = 0
    nb_nodes = 0
    vector   = c_null_ptr

    f_what = ''
    i = 1
    do while( what(i) /= C_NULL_CHAR )
      f_what = f_what(1:i-1) // what(i)
      i = i + 1
    end do

    i_type = get_type(id)
    i_what = get_coor_id_from_type(i_type,f_what)

    if( i_what < 1 ) return

    call get_coor(id, i_what, coor)

    if( associated(coor) ) then
      dime     = size(coor,1)
      nb_nodes = size(coor,2)
      vector   = c_loc(coor(1,1))
    end if

  end subroutine

  ! displacement, velocity (meca), temperature (ther)
  subroutine getNodalField(id,nf_name,when,matrix,dim1,dim2) bind(c, name='model_handler_getNodalField')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: nf_name
    integer(c_int)                 , intent(in), value :: when
    type(c_ptr)                    , intent(out)       :: matrix
    integer(c_int)                                     :: dim1
    integer(c_int)                                     :: dim2
    !
    real(kind=8), dimension(:,:), pointer :: nodal_field
    character(len=30) :: string
    integer(kind=4)   :: i, i_nf, i_type

    nodal_field => null()

    i_nf = 0
    dim1 = 0
    dim2 = 0
    matrix = c_null_ptr

    string = ''
    i = 1
    do while( nf_name(i) /= C_NULL_CHAR .and. i < 30 )
      string = string(1:i-1) // nf_name(i)
      i = i + 1
    end do

    i_type = get_type(id)
    i_nf = get_nf_id_from_type(i_type, trim(string))

    if( i_nf < 1 ) return

    call get_nodal_field(id, i_nf, when, nodal_field)

    if( associated(nodal_field) ) then
      dim1 = size(nodal_field,1)
      dim2 = size(nodal_field,2)
      matrix = c_loc(nodal_field(1,1))
    end if

  end subroutine

  ! displacement, velocity (meca), temperature (ther)
  subroutine getPtrNodalField(id,nf_name,when,ptr,dim1,dim2) bind(c, name='model_handler_getNodalFieldPtr')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: nf_name
    integer(c_int)                 , intent(in), value :: when
    type(c_ptr)                                        :: ptr
    integer(c_int)                                     :: dim1
    integer(c_int)                                     :: dim2
    !
    !real(kind=8), dimension(:,:), pointer :: nodal_field
    real(kind=8), dimension(:), pointer :: nodal_field
    character(len=30) :: string
    integer(kind=4)   :: i, i_nf, i_type, nb_nodes

    nodal_field => null()

    i_nf = 0
    dim1 = 0
    dim2 = 0
    ptr  = c_null_ptr

    string = ''
    i = 1
    do while( nf_name(i) /= C_NULL_CHAR .and. i < 30 )
      string = string(1:i-1) // nf_name(i)
      i = i + 1
    end do

    i_type = get_type(id)
    i_nf = get_nf_id_from_type(i_type, trim(string))

    if( i_nf < 1 ) return

    nodal_field => get_ptr_nodal_field(id, i_nf, when, nb_nodes)

    if( associated(nodal_field) ) then
      !dim1 = size(nodal_field,1)
      !dim2 = size(nodal_field,2)
      !ptr  = c_loc(nodal_field(1,1))
      dim1 = size(nodal_field)/nb_nodes
      dim2 = nb_nodes
      ptr  = c_loc(nodal_field(1))
    end if

  end subroutine

  ! displacement, velocity (meca), temperature (ther)
  subroutine setNodalField(id,nf_name,when,vector_in,length) bind(c, name='model_handler_setNodalField')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: nf_name
    integer(c_int)                 , intent(in), value :: when
    real(c_double)                 , intent(in)        :: vector_in(length)
    integer(c_int)                 , intent(in), value :: length
    !
    character(len=30) :: string
    integer(kind=4)   :: i, i_nf, i_type

    string = ''
    i = 1
    do while( nf_name(i) /= C_NULL_CHAR .and. i < 30 )
      string = string(1:i-1) // nf_name(i)
      i = i + 1
    end do

    i_type = get_type(id)
    i_nf = get_nf_id_from_type(i_type, trim(string))

    if( i_nf < 1 ) return

    call set_nodal_field(id, i_nf, when, vector_in)

  end subroutine

  subroutine getNodalFieldNames(id, names, nb_nf) bind(c, name='model_handler_getNodalFieldNames')
    implicit none
    integer(c_int), intent(in), value :: id
    type(c_ptr)    :: names
    integer(c_int) :: nb_nf
    !
    integer(kind=4) :: i_type, i_nf, lt
    character(len=128), dimension(:), pointer :: nf_names

    names = c_null_ptr

    i_type = get_type(id)
    nb_nf  = get_nb_nodal_field(id)

    if( nb_nf < 1 ) return

    allocate( nf_names(nb_nf) )

    do i_nf = 1, nb_nf
      nf_names(i_nf) = trim(get_nf_type_from_id(i_type, i_nf))
      !lt = min(len(trim(nf_names(i_nf)))+1,128)
      !nf_names(:)(lt:lt) = c_null_char
    end do

    names = c_loc(nf_names(1))

  end subroutine

  subroutine getRhsContribution(id,erc_name,when,matrix,dim1,dim2) bind(c, name='model_handler_getRhsContribution')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: erc_name
    integer(c_int)                 , intent(in), value :: when
    type(c_ptr)                    , intent(out)       :: matrix
    integer(c_int)                                     :: dim1
    integer(c_int)                                     :: dim2
    !
    real(kind=8), dimension(:,:), pointer :: rhs_contrib
    character(len=5) :: string
    integer(kind=4)   :: i, i_erc, i_type

    rhs_contrib => null()

    i_erc = 0
    dim1  = 0
    dim2  = 0
    matrix = c_null_ptr

    string = ''
    i = 1
    do while( erc_name(i) /= C_NULL_CHAR .and. i < 5 )
      string = string(1:i-1) // erc_name(i)
      i = i + 1
    end do

    i_type = get_type(id)
    i_erc  = get_erc_id_from_type(i_type, trim(string))

    if( i_erc < 1 ) return

    call get_rhs_contribution(id, i_erc, when, rhs_contrib)

    if( associated(rhs_contrib) ) then
      dim1 = size(rhs_contrib,1)
      dim2 = size(rhs_contrib,2)
      matrix = c_loc(rhs_contrib(1,1))
    end if

  end subroutine

  subroutine getRhsContributionNames(id, names, nb_erc) bind(c, name='model_handler_getRhsContributionNames')
    implicit none
    integer(c_int), intent(in), value :: id
    type(c_ptr)    :: names
    integer(c_int) :: nb_erc
    !
    integer(kind=4) :: i_type, i_erc
    character(len=5), dimension(:), pointer :: erc_names

    names = c_null_ptr

    i_type = get_type(id)
    nb_erc = get_nb_elementary_rhs_contrib(id)

    if( nb_erc < 1 ) return

    allocate( erc_names(nb_erc) )

    do i_erc = 1, nb_erc
      erc_names(i_erc) = trim(get_erc_type_from_id(i_type, i_erc))
    end do

    names = c_loc(erc_names(1))

  end subroutine

  subroutine getElementaryField(id,ef_name,when,matrix,dim1,dim2) bind(c, name='model_handler_getElementaryField')
    implicit none
    integer(c_int)                 , intent(in), value :: id
    character(c_char), dimension(*), intent(in)        :: ef_name
    integer(c_int)                 , intent(in), value :: when
    type(c_ptr)                    , intent(out)       :: matrix
    integer(c_int)                                     :: dim1
    integer(c_int)                                     :: dim2
    !
    real(kind=8), dimension(:,:), pointer :: elementary_field
    character(len=30) :: string
    integer(kind=4)   :: i, i_ef, i_type

    elementary_field => null()

    i_ef = 0
    dim1 = 0
    dim2 = 0
    matrix = c_null_ptr

    string = ''
    i = 1
    do while( ef_name(i) /= C_NULL_CHAR .and. i < 30 )
      string = string(1:i-1) // ef_name(i)
      i = i + 1
    end do

    call get_elementary_field(id, string, when, elementary_field)

    if( associated(elementary_field) ) then
      dim1 = size(elementary_field,1)
      dim2 = size(elementary_field,2)
      matrix = c_loc(elementary_field(1,1))
    end if

  end subroutine

end module
