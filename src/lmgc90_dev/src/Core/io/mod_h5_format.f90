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

!> Define the format of hdf5 file

module h5_format

  use utilities, only : faterr  , &
                        G_i_list, &
                        T_string_array

  use overall, only : nbDIME, &
                      get_need_internal_tact, &
                      internal_tact_comment_length

  use models, only : get_max_field_sizes

  use tact_behaviour, only : get_ilaw_list, &
                             tact_behav_info_by_id

  use parameters, only : i_mecax, i_therx            , &
                         i_porox, i_multi            , &
                         get_physic_type_name_from_id, &
                         get_physic_type_names       , &
                         get_physic_type_id_from_name, &
                         get_body_model_names        , &
                         get_body_model_id_from_name , &
                         get_contactor_names         , &
                         get_contactor_id_from_name  , &
                         get_interaction_names       , &
                         get_interaction_id_from_name, &
                         get_inter_law_name_from_id  , &
                         get_integrator_names        , &
                         get_integrator_id_from_name , &
                         get_dime_mode_names         , &
                         get_dime_mode_id_from_name  , &
                         get_contact_status_names    , &
                         get_contact_status_id_from_name


  implicit none

  private

  !> Internal Evolution group numbering
  integer, public :: evolution_id = 0

  !> Group id definition
  integer, parameter, public :: gr_root       =                 1, &
                                gr_simulation = gr_root       + 1, &
                                gr_rbdy2      = gr_simulation + 1, &
                                gr_rbdy3      = gr_rbdy2      + 1, &
                                gr_mecax      = gr_rbdy3      + 1, &
                                gr_therx      = gr_mecax      + 1, &
                                gr_porox      = gr_therx      + 1, &
                                gr_multi      = gr_porox      + 1, &
                                gr_vlocrloc   = gr_multi      + 1

  !> Dataset id definition
  integer, parameter, public ::  ds_idata        =                 + 1, &
                                 ds_rdata        = ds_idata        + 1, &
                                 ds_disp         = ds_rdata        + 1, &
                                 ds_dofs         = ds_disp         + 1, &
                                 ds_grad         = ds_dofs         + 1, &
                                 ds_flux         = ds_grad         + 1, &
                                 ds_internal     = ds_flux         + 1, &
                                 ds_dimension    = ds_internal     + 1, &
                                 ds_dime_mode    = ds_dimension    + 1, &
                                 ds_m_integrator = ds_dime_mode    + 1, &
                                 ds_t_integrator = ds_m_integrator + 1, &
                                 ds_m_integ_param= ds_t_integrator + 1, &
                                 ds_t_integ_param= ds_m_integ_param+ 1, &
                                 ds_nstep        = ds_t_integ_param+ 1, &
                                 ds_tps          = ds_nstep        + 1, &
                                 ds_dt           = ds_tps          + 1, &
                                 ds_tini         = ds_dt           + 1, &
                                 ds_tend         = ds_tini         + 1, &
                                 ds_nb_record    = ds_tend         + 1, &
                                 ds_release      = ds_nb_record    + 1, &
                                 ds_git_branch   = ds_release      + 1, &
                                 ds_git_rev      = ds_git_branch   + 1


  !> A type to define a component of a dataset
  type, private :: T_dataset_format
    character(len=40) :: name = ''
    integer :: ibeg = 0
    integer :: iend = 0
  end type

  !> rigid bodies formatting
  type, public :: T_rigid_format

    integer :: visible  = -99

    integer :: idata_sz = 0
    type(T_dataset_format), dimension(:), pointer :: idata => null()

    integer :: X = -99
    integer :: V = -99
    integer :: LF= -99

    integer :: rdata_sz = 0
    type(T_dataset_format), dimension(:), pointer :: rdata => null()

  end type

  !> meshed bodies formatting
  type, public :: T_meshx_format

    integer :: node_idx = -99
    integer :: dofs_idx = -99
    integer :: gps_idx  = -99
    integer :: is_rigid = -99
    integer :: is_coro  = -99

    integer :: idata_sz = 0
    type(T_dataset_format), dimension(:), pointer :: idata      => null()

    integer :: RX       = -99
    integer :: RV       = -99
    integer :: LF       = -99
    integer :: rdata_sz =   0
    type(T_dataset_format), dimension(:), pointer :: rdata      => null()

    integer :: disp          = -99
    integer :: disp_field_sz =   0
    type(T_dataset_format), dimension(:), pointer :: disp_field => null()

    integer :: dofs          = -99
    integer :: dofs_field_sz =   0
    type(T_dataset_format), dimension(:), pointer :: dofs_field => null()

    integer :: stress     = -99
    integer :: ther_grad  = -99
    integer :: multi_grad = -99
    integer :: grad_sz    =   0
    type(T_dataset_format), dimension(:), pointer :: grad       => null()

    integer :: strain     = -99
    integer :: ther_flux  = -99
    integer :: multi_flux = -99
    integer :: flux_sz    =   0
    type(T_dataset_format), dimension(:), pointer :: flux       => null()

    integer :: meca_internal  = -99
    integer :: ther_internal  = -99
    integer :: multi_internal = -99
    integer :: internal_sz    =   0
    type(T_dataset_format), dimension(:), pointer :: internal   => null()

  end type

  !> interactions formatting
  type, public :: T_inter_format

    integer :: inter_id    = -99
    integer :: icdan       = -99
    integer :: ent         = -99
    integer :: bdyty       = -99
    integer :: ibdyty      = -99
    integer :: tactype     = -99
    integer :: itacty      = -99
    integer :: itacbdy     = -99
    integer :: iadj        = -99
    integer :: isee        = -99
    integer :: lawnb       = -99
    integer :: i_law       = -99
    integer :: status      = -99
    integer :: igeo        = -99 ! old
    integer :: isci        = -99
    integer :: nb_internal = -99

    integer :: idata_sz    = 0
    type(T_dataset_format), dimension(:), pointer :: idata => null()

    integer :: rl        = -99
    integer :: vl        = -99
    integer :: gapTT     = -99
    integer :: coor      = -99
    integer :: uc        = -99
    integer :: internals = -99

    integer :: rdata_sz  = 0
    type(T_dataset_format), dimension(:), pointer :: rdata => null()

  end type


  !> The rigid format
  type(T_rigid_format), public :: fmt_rigid

  !> The interaction format
  type(T_inter_format), public :: fmt_inter

  !> The format for different meshed models
  type(T_meshx_format), public :: fmt_mecax, fmt_therx, fmt_porox, fmt_multi

  public :: get_gr_name       , &
            get_ds_name       , &
            set_all_format    , &
            clean_all_format  , &
            get_format_help   , &
            get_parameter_help, &
            get_internal_law_help

  private :: set_rigid_format_  , &
             set_mecax_format_  , &
             set_therx_format_  , &
             set_porox_format_  , &
             set_multi_format_  , &
             set_inter_format_  , &
             clean_rigid_format_, &
             clean_meshx_format_, &
             clean_inter_format_, &
             get_meshx_help_

  public  :: get_gr_name_v0_0, &
             get_ds_name_v0_0, &
             get_ds_name_v0_1, &
             get_ds_name_v0_2

  procedure( get_gr_name_v0_0 ), pointer :: get_gr_name => null()
  procedure( get_ds_name_v0_0 ), pointer :: get_ds_name => null()

  private :: set_rigid_format_v0_, &
             set_mecax_format_v0_, &
             set_therx_format_v0_, &
             set_porox_format_v0_, &
             set_multi_format_v0_, &
             set_inter_format_v0_

contains

  include 'format_v0.f90'

  subroutine set_all_format(major, minor)
    implicit none
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=20) :: cout

    select case( major )
    case( 0 )
      select case( minor )
      case( 0 )
        get_gr_name => get_gr_name_v0_0
        get_ds_name => get_ds_name_v0_0
      case( 1, 2 )
        get_gr_name => get_gr_name_v0_0
        get_ds_name => get_ds_name_v0_1
      case( 3 )
        get_gr_name => get_gr_name_v0_0
        get_ds_name => get_ds_name_v0_2
      case default
        write(cout, '(A,1x,I0,A1,I0)') 'unknown format:', major, '.', minor
        call faterr('h5_format::set_all_format', cout)
      end select
    case default
      write(cout, '(A,1x,I0,A1,I0)') 'unknown format:', major, '.', minor
      call faterr('h5_format::set_all_format', cout)
    end select

    call set_rigid_format_(fmt_rigid, major, minor)
    call set_mecax_format_(fmt_mecax, major, minor)
    call set_therx_format_(fmt_therx, major, minor)
    call set_porox_format_(fmt_porox, major, minor)
    call set_multi_format_(fmt_multi, major, minor)
    call set_inter_format_(fmt_inter, major, minor)

  end subroutine set_all_format


  !> \brief Set the dataset and group description of rigids for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_rigid_format_(f_rigid, major, minor)
    implicit none
    !> rigid format to set
    type(T_rigid_format), intent(inout) :: f_rigid
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_rigid_format_'

    call clean_rigid_format_(f_rigid)
    select case(major)
    case( 0 )
      call set_rigid_format_v0_(f_rigid, minor)
    end select

  end subroutine set_rigid_format_

  !> \brief Set the dataset and group description of meca mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_mecax_format_(f_mecax, major, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_mecax
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_mecax_format_'

    call clean_meshx_format_(f_mecax)
    select case(major)
    case( 0 )
      call set_mecax_format_v0_(f_mecax, minor)
    end select

  end subroutine set_mecax_format_

  !> \brief Set the dataset and group description of a ther mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_therx_format_(f_therx, major, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_therx
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_therx_format_'

    call clean_meshx_format_(f_therx)
    select case(major)
    case( 0 )
      call set_therx_format_v0_(f_therx, minor)
    end select

  end subroutine set_therx_format_

  !> \brief Set the dataset and group description of poro a mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_porox_format_(f_porox, major, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_porox
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_porox_format_'

    call clean_meshx_format_(f_porox)
    select case(major)
    case( 0 )
      call set_porox_format_v0_(f_porox, minor)
    end select


  end subroutine set_porox_format_

  !> \brief Set the dataset and group description of a multi mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_multi_format_(f_multi, major, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_multi
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_multi_format_'

    call clean_meshx_format_(f_multi)
    select case(major)
    case( 0 )
      call set_multi_format_v0_(f_multi, minor)
    end select

  end subroutine set_multi_format_

  !> \brief Set the dataset and group description of interactions for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_inter_format_(f_inter, major, minor)
    implicit none
    !> interaction format to set
    type(T_inter_format), intent(inout) :: f_inter
    !> major version of file format
    integer, intent(in) :: major
    !> minor version of file format
    integer, intent(in) :: minor
    !
    character(len=28), parameter :: IAM = 'h5_format::set_inter_format_'

    call clean_inter_format_(f_inter)
    select case(major)
    case( 0 )
      call set_inter_format_v0_(f_inter, minor)
    end select

  end subroutine set_inter_format_

  !> \brief (private function) to clean rigid format
  subroutine clean_rigid_format_(f_rigid)
    implicit none
    !> rigid format to clean
    type(T_rigid_format), intent(inout) :: f_rigid

    f_rigid%visible  = -99
    f_rigid%idata_sz = 0
    if( associated(f_rigid%idata) ) then
       deallocate(f_rigid%idata)
       nullify(f_rigid%idata)
    end if

    f_rigid%X = -99
    f_rigid%V = -99
    f_rigid%LF= -99
    f_rigid%rdata_sz = 0
    if( associated(f_rigid%rdata) ) then
       deallocate(f_rigid%rdata)
       nullify(f_rigid%rdata)
    end if
    
  end subroutine clean_rigid_format_

  !> \brief (private function) to clean meshx format
  subroutine clean_meshx_format_(f_meshx)
    implicit none
    !> mesh format to clean
    type(T_meshx_format), intent(inout) :: f_meshx

    f_meshx%node_idx = -99
    f_meshx%dofs_idx = -99
    f_meshx%gps_idx  = -99
    f_meshx%is_rigid = -99
    f_meshx%is_coro  = -99

    f_meshx%idata_sz = 0
    if( associated(f_meshx%idata) ) then
      deallocate(f_meshx%idata)
      nullify(f_meshx%idata)
    end if

    f_meshx%RX       = -99
    f_meshx%RV       = -99
    f_meshx%LF       = -99
    f_meshx%rdata_sz =   0
    if( associated(f_meshx%rdata) ) then
      deallocate(f_meshx%rdata)
      nullify(f_meshx%rdata)
    end if

    f_meshx%disp          = -99
    f_meshx%disp_field_sz =   0
    if( associated(f_meshx%disp_field) ) then
      deallocate(f_meshx%disp_field)
      nullify(f_meshx%disp_field)
    end if

    f_meshx%dofs          = -99
    f_meshx%dofs_field_sz =   0
    if( associated(f_meshx%dofs_field) ) then
      deallocate(f_meshx%dofs_field)
      nullify(f_meshx%dofs_field)
    end if

    f_meshx%stress     = -99
    f_meshx%ther_grad  = -99
    f_meshx%multi_grad = -99
    f_meshx%grad_sz    =   0
    if( associated(f_meshx%grad) ) then
      deallocate(f_meshx%grad)
      nullify(f_meshx%grad)
    end if

    f_meshx%strain     = -99
    f_meshx%ther_flux  = -99
    f_meshx%multi_flux = -99
    f_meshx%flux_sz    =   0
    if( associated(f_meshx%flux) ) then
      deallocate(f_meshx%flux)
      nullify(f_meshx%flux)
    end if

    f_meshx%meca_internal  = -99
    f_meshx%ther_internal  = -99
    f_meshx%multi_internal = -99
    f_meshx%internal_sz    =   0
    if( associated(f_meshx%internal) ) then
      deallocate(f_meshx%internal)
      nullify(f_meshx%internal)
    end if

  end subroutine clean_meshx_format_

  !> \brief (private function) to clean inter format
  subroutine clean_inter_format_(f_inter)
    implicit none
    !> interaction format to clean
    type(T_inter_format), intent(inout) :: f_inter

    f_inter%inter_id    =-99
    f_inter%icdan       =-99
    f_inter%ent         =-99
    f_inter%bdyty       =-99
    f_inter%ibdyty      =-99
    f_inter%tactype     =-99
    f_inter%itacty      =-99
    f_inter%itacbdy     =-99
    f_inter%iadj        =-99
    f_inter%isee        =-99
    f_inter%lawnb       =-99
    f_inter%i_law       =-99
    f_inter%status      =-99
    f_inter%igeo        =-99
    f_inter%isci        =-99
    f_inter%nb_internal =-99
    f_inter%idata_sz = 0
    if( associated(f_inter%idata) ) then
       deallocate(f_inter%idata)
       nullify(f_inter%idata)
    end if

    f_inter%rl        = -99
    f_inter%vl        = -99
    f_inter%gapTT     = -99
    f_inter%coor      = -99
    f_inter%uc        = -99
    f_inter%internals = -99
    f_inter%rdata_sz = 0
    if( associated(f_inter%rdata) ) then
       deallocate(f_inter%rdata)
       nullify(f_inter%rdata)
    end if
    
  end subroutine clean_inter_format_

  !> \brief Clean all stored format
  subroutine clean_all_format()
    implicit none

    get_gr_name => null()
    get_ds_name => null()

    call clean_rigid_format_(fmt_rigid)
    call clean_meshx_format_(fmt_mecax)
    call clean_meshx_format_(fmt_therx)
    call clean_meshx_format_(fmt_porox)
    call clean_meshx_format_(fmt_multi)
    call clean_inter_format_(fmt_inter)

  end subroutine

  !> \brief Get some help on the format to access a specific field of a data
  subroutine get_format_help(data_group, data_name, data_bound)
    implicit none
    !> list of data in groups
    character(len=42), dimension(:)  , pointer :: data_group
    !> help string on each field
    character(len=40), dimension(:)  , pointer :: data_name
    !> bound for each field in data
    integer,           dimension(:,:), pointer :: data_bound
    !
    integer          :: i_data, nb_data
    character(len=1) :: str_dim

    ! -------------------  counting  ------------------- !

    nb_data = 0

    if( associated(fmt_rigid%idata) ) nb_data = nb_data + size(fmt_rigid%idata)
    if( associated(fmt_rigid%rdata) ) nb_data = nb_data + size(fmt_rigid%rdata)

    if( associated(fmt_mecax%idata     ) ) nb_data = nb_data + size(fmt_mecax%idata     )
    if( associated(fmt_mecax%rdata     ) ) nb_data = nb_data + size(fmt_mecax%rdata     )
    if( associated(fmt_mecax%disp_field) ) nb_data = nb_data + size(fmt_mecax%disp_field)
    if( associated(fmt_mecax%dofs_field) ) nb_data = nb_data + size(fmt_mecax%dofs_field)
    if( associated(fmt_mecax%grad      ) ) nb_data = nb_data + size(fmt_mecax%grad      )
    if( associated(fmt_mecax%flux      ) ) nb_data = nb_data + size(fmt_mecax%flux      )
    if( associated(fmt_mecax%internal  ) ) nb_data = nb_data + size(fmt_mecax%internal)

    if( associated(fmt_therx%idata     ) ) nb_data = nb_data + size(fmt_therx%idata     )
    if( associated(fmt_therx%rdata     ) ) nb_data = nb_data + size(fmt_therx%rdata     )
    if( associated(fmt_therx%disp_field) ) nb_data = nb_data + size(fmt_therx%disp_field)
    if( associated(fmt_therx%dofs_field) ) nb_data = nb_data + size(fmt_therx%dofs_field)
    if( associated(fmt_therx%grad      ) ) nb_data = nb_data + size(fmt_therx%grad      )
    if( associated(fmt_therx%flux      ) ) nb_data = nb_data + size(fmt_therx%flux      )
    if( associated(fmt_therx%internal  ) ) nb_data = nb_data + size(fmt_therx%internal  )

    if( associated(fmt_porox%idata     ) ) nb_data = nb_data + size(fmt_porox%idata     )
    if( associated(fmt_porox%rdata     ) ) nb_data = nb_data + size(fmt_porox%rdata     )
    if( associated(fmt_porox%disp_field) ) nb_data = nb_data + size(fmt_porox%disp_field)
    if( associated(fmt_porox%dofs_field) ) nb_data = nb_data + size(fmt_porox%dofs_field)
    if( associated(fmt_porox%grad      ) ) nb_data = nb_data + size(fmt_porox%grad      )
    if( associated(fmt_porox%flux      ) ) nb_data = nb_data + size(fmt_porox%flux      )
    if( associated(fmt_porox%internal  ) ) nb_data = nb_data + size(fmt_porox%internal  )

    if( associated(fmt_multi%idata     ) ) nb_data = nb_data + size(fmt_multi%idata     )
    if( associated(fmt_multi%rdata     ) ) nb_data = nb_data + size(fmt_multi%rdata     )
    if( associated(fmt_multi%disp_field) ) nb_data = nb_data + size(fmt_multi%disp_field)
    if( associated(fmt_multi%dofs_field) ) nb_data = nb_data + size(fmt_multi%dofs_field)
    if( associated(fmt_multi%grad      ) ) nb_data = nb_data + size(fmt_multi%grad      )
    if( associated(fmt_multi%flux      ) ) nb_data = nb_data + size(fmt_multi%flux      )
    if( associated(fmt_multi%internal  ) ) nb_data = nb_data + size(fmt_multi%internal  )

    if( associated(fmt_inter%idata) ) nb_data = nb_data + size(fmt_inter%idata)
    if( associated(fmt_inter%rdata) ) nb_data = nb_data + size(fmt_inter%rdata)


    ! ---------------  allocating outuput  ------------- !

    allocate( data_group(nb_data)   )
    allocate( data_name(nb_data)    )
    allocate( data_bound(2,nb_data) )


    ! --------------------  filling  ------------------- !

    ! rigid part
    i_data = 0

    if( nbDIME == 2 ) then
      str_dim = '2'
    else
      str_dim = '3'
    end if

    if( fmt_rigid%visible  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/RBDY"//str_dim//"/idata/visible"
      data_name (i_data)    = fmt_rigid%idata( fmt_rigid%visible )%name
      data_bound(1,i_data)  = fmt_rigid%idata( fmt_rigid%visible )%ibeg
      data_bound(2,i_data)  = fmt_rigid%idata( fmt_rigid%visible )%iend
    end if

    if( fmt_rigid%X  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/RBDY"//str_dim//"/rdata/X"
      data_name (i_data)    = fmt_rigid%rdata( fmt_rigid%X )%name
      data_bound(1,i_data)  = fmt_rigid%rdata( fmt_rigid%X )%ibeg
      data_bound(2,i_data)  = fmt_rigid%rdata( fmt_rigid%X )%iend
    end if
    if( fmt_rigid%V  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/RBDY"//str_dim//"/rdata/V"
      data_name (i_data)    = fmt_rigid%rdata( fmt_rigid%V )%name
      data_bound(1,i_data)  = fmt_rigid%rdata( fmt_rigid%V )%ibeg
      data_bound(2,i_data)  = fmt_rigid%rdata( fmt_rigid%V )%iend
    end if
    if( fmt_rigid%LF > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/RBDY"//str_dim//"/rdata/LF"
      data_name (i_data)    = fmt_rigid%rdata( fmt_rigid%LF )%name
      data_bound(1,i_data)  = fmt_rigid%rdata( fmt_rigid%LF )%ibeg
      data_bound(2,i_data)  = fmt_rigid%rdata( fmt_rigid%LF )%iend
    end if


    ! mehsx part
    call get_meshx_help_(i_data, fmt_mecax, i_mecax, data_group, data_name, data_bound)
    call get_meshx_help_(i_data, fmt_therx, i_therx, data_group, data_name, data_bound)
    call get_meshx_help_(i_data, fmt_porox, i_porox, data_group, data_name, data_bound)
    call get_meshx_help_(i_data, fmt_multi, i_multi, data_group, data_name, data_bound)

    ! inter part
    if( fmt_inter%inter_id  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/inter_id"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%inter_id )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%inter_id )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%inter_id )%iend
    end if
    if( fmt_inter%icdan  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/icdan"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%icdan )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%icdan )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%icdan )%iend
    end if
    if( fmt_inter%ent  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/ent"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%ent )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%ent )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%ent )%iend
    end if
    if( fmt_inter%bdyty  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/bdyty"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%bdyty )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%bdyty )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%bdyty )%iend
    end if
    if( fmt_inter%ibdyty  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/ibdyty"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%ibdyty )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%ibdyty )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%ibdyty )%iend
    end if
    if( fmt_inter%tactype  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/tactype"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%tactype )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%tactype )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%tactype )%iend
    end if
    if( fmt_inter%itacty  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/itacty"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%itacty )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%itacty )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%itacty )%iend
    end if
    if( fmt_inter%itacbdy  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/itacbdy"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%itacbdy )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%itacbdy )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%itacbdy )%iend
    end if
    if( fmt_inter%iadj  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/iadj"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%iadj )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%iadj )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%iadj )%iend
    end if
    if( fmt_inter%isee  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/isee"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%isee )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%isee )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%isee )%iend
    end if
    if( fmt_inter%lawnb  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/lawnb"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%lawnb )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%lawnb )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%lawnb )%iend
    end if
    if( fmt_inter%i_law  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/i_law"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%i_law )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%i_law )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%i_law )%iend
    end if
    if( fmt_inter%status  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/status"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%status )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%status )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%status )%iend
    end if
    if( fmt_inter%igeo  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/igeo"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%igeo )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%igeo )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%igeo )%iend
    end if
    if( fmt_inter%isci  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/isci"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%isci )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%isci )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%isci )%iend
    end if
    if( fmt_inter%nb_internal  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/idata/nb_internal"
      data_name (i_data)    = fmt_inter%idata( fmt_inter%nb_internal )%name
      data_bound(1,i_data)  = fmt_inter%idata( fmt_inter%nb_internal )%ibeg
      data_bound(2,i_data)  = fmt_inter%idata( fmt_inter%nb_internal )%iend
    end if

    if( fmt_inter%rl  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/rl"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%rl )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%rl )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%rl )%iend
    end if
    if( fmt_inter%vl  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/vl"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%vl )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%vl )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%vl )%iend
    end if
    if( fmt_inter%gapTT  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/gapTT"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%gapTT )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%gapTT )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%gapTT )%iend
    end if
    if( fmt_inter%coor  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/coor"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%coor )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%coor )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%coor )%iend
    end if
    if( fmt_inter%uc  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/uc"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%uc )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%uc )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%uc )%iend
    end if
    if( fmt_inter%internals  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/VlocRloc/rdata/internals"
      data_name (i_data)    = fmt_inter%rdata( fmt_inter%internals )%name
      data_bound(1,i_data)  = fmt_inter%rdata( fmt_inter%internals )%ibeg
      data_bound(2,i_data)  = fmt_inter%rdata( fmt_inter%internals )%iend
    end if

  end subroutine get_format_help

  subroutine get_meshx_help_(i_data, f_meshx, i_physics, data_group, data_name, data_bound)
    implicit none
    !> last written i_data
    integer, intent(inout) :: i_data
    !> mesh format to document
    type(T_meshx_format), intent(in) :: f_meshx
    !> type of physics of the format
    integer, intent(in) :: i_physics
    !> list of data in groups
    character(len=42), dimension(:)  , pointer :: data_group
    !> help string on each field
    character(len=40), dimension(:)  , pointer :: data_name
    !> bound for each field in data
    integer,           dimension(:,:), pointer :: data_bound
    !
    character(len=5) :: physics

    physics = get_physic_type_name_from_id( i_physics )

    ! idata

    if( f_meshx%node_idx > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/idata/node_idx"
      data_name (i_data)    = f_meshx%idata( f_meshx%node_idx )%name
      data_bound(1,i_data)  = f_meshx%idata( f_meshx%node_idx )%ibeg
      data_bound(2,i_data)  = f_meshx%idata( f_meshx%node_idx )%iend
    end if
    if( f_meshx%dofs_idx > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/idata/dofs_idx"
      data_name (i_data)    = f_meshx%idata( f_meshx%dofs_idx )%name
      data_bound(1,i_data)  = f_meshx%idata( f_meshx%dofs_idx )%ibeg
      data_bound(2,i_data)  = f_meshx%idata( f_meshx%dofs_idx )%iend
    end if
    if( f_meshx%gps_idx > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/idata/gps_idx"
      data_name (i_data)    = f_meshx%idata( f_meshx%gps_idx )%name
      data_bound(1,i_data)  = f_meshx%idata( f_meshx%gps_idx )%ibeg
      data_bound(2,i_data)  = f_meshx%idata( f_meshx%gps_idx )%iend
    end if
    if( f_meshx%is_rigid > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/idata/is_rigid"
      data_name (i_data)    = f_meshx%idata( f_meshx%is_rigid )%name
      data_bound(1,i_data)  = f_meshx%idata( f_meshx%is_rigid )%ibeg
      data_bound(2,i_data)  = f_meshx%idata( f_meshx%is_rigid )%iend
    end if
    if( f_meshx%is_coro > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/idata/is_coro"
      data_name (i_data)    = f_meshx%idata( f_meshx%is_coro )%name
      data_bound(1,i_data)  = f_meshx%idata( f_meshx%is_coro )%ibeg
      data_bound(2,i_data)  = f_meshx%idata( f_meshx%is_coro )%iend
    end if


    ! rdata

    if( f_meshx%RX  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/rdata/RX"
      data_name (i_data)    = f_meshx%rdata( f_meshx%RX )%name
      data_bound(1,i_data)  = f_meshx%rdata( f_meshx%RX )%ibeg
      data_bound(2,i_data)  = f_meshx%rdata( f_meshx%RX )%iend
    end if
    if( f_meshx%RV  > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/rdata/RV"
      data_name (i_data)    = f_meshx%rdata( f_meshx%RV )%name
      data_bound(1,i_data)  = f_meshx%rdata( f_meshx%RV )%ibeg
      data_bound(2,i_data)  = f_meshx%rdata( f_meshx%RV )%iend
    end if
    if( f_meshx%LF > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/rdata/LF"
      data_name (i_data)    = f_meshx%rdata( f_meshx%LF )%name
      data_bound(1,i_data)  = f_meshx%rdata( f_meshx%LF )%ibeg
      data_bound(2,i_data)  = f_meshx%rdata( f_meshx%LF )%iend
    end if


    ! disp

    if( f_meshx%disp > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/disp_field/disp"
      data_name (i_data)    = f_meshx%disp_field( f_meshx%disp )%name
      data_bound(1,i_data)  = f_meshx%disp_field( f_meshx%disp )%ibeg
      data_bound(2,i_data)  = f_meshx%disp_field( f_meshx%disp )%iend
    end if


    ! dofs

    if( f_meshx%dofs > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/dofs_field/dofs"
      data_name (i_data)    = f_meshx%dofs_field( f_meshx%dofs )%name
      data_bound(1,i_data)  = f_meshx%dofs_field( f_meshx%dofs )%ibeg
      data_bound(2,i_data)  = f_meshx%dofs_field( f_meshx%dofs )%iend
    end if


    ! grad

    if( f_meshx%stress > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/grad/stress"
      data_name (i_data)    = f_meshx%grad( f_meshx%stress )%name
      data_bound(1,i_data)  = f_meshx%grad( f_meshx%stress )%ibeg
      data_bound(2,i_data)  = f_meshx%grad( f_meshx%stress )%iend
    end if

    if( f_meshx%ther_grad > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/grad/ther_grad"
      data_name (i_data)    = f_meshx%grad( f_meshx%ther_grad )%name
      data_bound(1,i_data)  = f_meshx%grad( f_meshx%ther_grad )%ibeg
      data_bound(2,i_data)  = f_meshx%grad( f_meshx%ther_grad )%iend
    end if

    if( f_meshx%multi_grad > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/grad/multi_grad"
      data_name (i_data)    = f_meshx%grad( f_meshx%multi_grad )%name
      data_bound(1,i_data)  = f_meshx%grad( f_meshx%multi_grad )%ibeg
      data_bound(2,i_data)  = f_meshx%grad( f_meshx%multi_grad )%iend
    end if


    ! flux

    if( f_meshx%strain > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/flux/strain"
      data_name (i_data)    = f_meshx%flux( f_meshx%stress )%name
      data_bound(1,i_data)  = f_meshx%flux( f_meshx%stress )%ibeg
      data_bound(2,i_data)  = f_meshx%flux( f_meshx%stress )%iend
    end if

    if( f_meshx%ther_flux > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/flux/ther_flux"
      data_name (i_data)    = f_meshx%flux( f_meshx%ther_flux )%name
      data_bound(1,i_data)  = f_meshx%flux( f_meshx%ther_flux )%ibeg
      data_bound(2,i_data)  = f_meshx%flux( f_meshx%ther_flux )%iend
    end if

    if( f_meshx%multi_flux > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/flux/multi_flux"
      data_name (i_data)    = f_meshx%flux( f_meshx%multi_flux )%name
      data_bound(1,i_data)  = f_meshx%flux( f_meshx%multi_flux )%ibeg
      data_bound(2,i_data)  = f_meshx%flux( f_meshx%multi_flux )%iend
    end if

    ! internal

    if( f_meshx%meca_internal > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/internal/meca_internal"
      data_name (i_data)    = f_meshx%internal( f_meshx%stress )%name
      data_bound(1,i_data)  = f_meshx%internal( f_meshx%stress )%ibeg
      data_bound(2,i_data)  = f_meshx%internal( f_meshx%stress )%iend
    end if

    if( f_meshx%ther_internal > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/internal/ther_internal"
      data_name (i_data)    = f_meshx%internal( f_meshx%ther_internal )%name
      data_bound(1,i_data)  = f_meshx%internal( f_meshx%ther_internal )%ibeg
      data_bound(2,i_data)  = f_meshx%internal( f_meshx%ther_internal )%iend
    end if

    if( f_meshx%multi_internal > 0 ) then
      i_data = i_data + 1
      data_group(i_data)    = "/Help/MAILx/"//physics//"/internal/multi_internal"
      data_name (i_data)    = f_meshx%internal( f_meshx%multi_internal )%name
      data_bound(1,i_data)  = f_meshx%internal( f_meshx%multi_internal )%ibeg
      data_bound(2,i_data)  = f_meshx%internal( f_meshx%multi_internal )%iend
    end if

  end subroutine get_meshx_help_

  subroutine get_parameter_help(data_group, data_cvalue, data_ivalue)
    implicit none
    !> list of data in groups
    character(len=42), dimension(:), pointer :: data_group
    !> help string on each field
    character(len=40), dimension(:), pointer :: data_name
    !> bound for each field in data
    type(G_i_list)      , dimension(:), pointer :: data_ivalue
    type(T_string_array), dimension(:), pointer :: data_cvalue
    !
    integer :: i_param, i_data, nb_data

    ! list of parameters stored :
    ! - body_model       -> bdyty
    ! - physcis_type     -> mecax/therx/porox/multi
    ! - contactor_type   -> tactype
    ! - interaction_type -> inter_id
    ! - integrator_type  -> integrator
    ! - dime_mode_type   -> dime_mode
    ! - contact_status   -> status
    ! - interaction_law  -> ilaw
    !
    ! list of parameters ignored :
    ! - matrix_storage
    ! - matrix_shape
    ! - generalized_coordinates
    ! - surface_energy_status
    ! - node_id
    ! - vector_type

    character(len=5) , dimension(:), pointer :: body_model_names
    character(len=5) , dimension(:), pointer :: physic_type_names
    character(len=5) , dimension(:), pointer :: contactor_names
    character(len=5) , dimension(:), pointer :: interaction_names
    character(len=7) , dimension(:), pointer :: integrator_names
    character(len=10), dimension(:), pointer :: dime_mode_names
    character(len=5) , dimension(:), pointer :: contact_status_names

    body_model_names     => get_body_model_names()
    physic_type_names    => get_physic_type_names()
    contactor_names      => get_contactor_names()
    interaction_names    => get_interaction_names()
    integrator_names     => get_integrator_names()
    dime_mode_names      => get_dime_mode_names()
    contact_status_names => get_contact_status_names()

    ! -------------------  counting  ------------------- !

    nb_data = 7
    !nb_data =   size(body_type_names      ) &
    !          + size(physic_type_names    ) &
    !          + size(contactor_names      ) &
    !          + size(interaction_names    ) &
    !          + size(integrator_names     ) &
    !          + size(dime_mode_names      ) &
    !          + size(contact_status_names ) &

    ! ---------------  allocating outuput  ------------- !

    allocate( data_group(nb_data)  )
    allocate( data_cvalue(nb_data) )
    allocate( data_ivalue(nb_data) )


    ! --------------------  filling  ------------------- !

    i_data = 1

    ! body type
    data_group(i_data)  = "/Help/parameters/bdyty"
    allocate( data_ivalue(i_data)%G_i(   size(body_model_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(body_model_names) ) )
    do i_param = 1, size(body_model_names)
      data_ivalue(i_data)%G_i(i_param)   = get_body_model_id_from_name( body_model_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(body_model_names(i_param))
    end do

    ! physic type
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/mdlty"
    allocate( data_ivalue(i_data)%G_i(   size(physic_type_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(physic_type_names) ) )
    do i_param = 1, size(physic_type_names)
      data_ivalue(i_data)%G_i(i_param)   = get_physic_type_id_from_name( physic_type_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(physic_type_names(i_param))
    end do

    ! contactor
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/tactype"
    allocate( data_ivalue(i_data)%G_i(   size(contactor_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(contactor_names) ) )
    do i_param = 1, size(contactor_names)
      data_ivalue(i_data)%G_i(i_param)   = get_contactor_id_from_name( contactor_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(contactor_names(i_param))
    end do

    ! interaction
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/inter_id"
    allocate( data_ivalue(i_data)%G_i(   size(interaction_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(interaction_names) ) )
    do i_param = 1, size(interaction_names)
      data_ivalue(i_data)%G_i(i_param)   = get_interaction_id_from_name( interaction_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(interaction_names(i_param))
    end do

    ! integrator
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/integrator"
    allocate( data_ivalue(i_data)%G_i(   size(integrator_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(integrator_names) ) )
    do i_param = 1, size(integrator_names)
      data_ivalue(i_data)%G_i(i_param)   = get_integrator_id_from_name( integrator_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(integrator_names(i_param))
    end do

    ! dime mode
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/dime_mode"
    allocate( data_ivalue(i_data)%G_i(   size(dime_mode_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(dime_mode_names) ) )
    do i_param = 1, size(dime_mode_names)
      data_ivalue(i_data)%G_i(i_param)   = get_dime_mode_id_from_name( dime_mode_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(dime_mode_names(i_param))
    end do

    ! contact status
    i_data = i_data + 1
    data_group(i_data)  = "/Help/parameters/status"
    allocate( data_ivalue(i_data)%G_i(   size(contact_status_names) ) )
    allocate( data_cvalue(i_data)%sdata( size(contact_status_names) ) )
    do i_param = 1, size(contact_status_names)
      data_ivalue(i_data)%G_i(i_param)   = get_contact_status_id_from_name( contact_status_names(i_param) )
      data_cvalue(i_data)%sdata(i_param) = trim(contact_status_names(i_param))
    end do

  end subroutine get_parameter_help

  subroutine get_internal_law_help(ilaw_group, law_ids, law_names, law_comments)
    implicit none
    character(len=42) :: ilaw_group
    integer          , dimension(:), pointer :: law_ids
    character(len=40), dimension(:), pointer :: law_names
    character(len=internal_tact_comment_length), dimension(:), pointer :: law_comments
    !
    integer :: i_param, ilaw
    !
    ! dummy parameters to call tact_behav_info_by_id
    integer :: nb_p,nb_int
    character(len=5),dimension(:),pointer :: p_name

    ilaw_group = "/Help/parameters/inter_law"

    call get_ilaw_list(law_ids)
     
    allocate( law_names(    size(law_ids) ) )
    allocate( law_comments( size(law_ids) ) )
    nullify( p_name )

    do i_param = 1, size(law_ids)
      ilaw = law_ids(i_param)
      law_names(i_param) = trim(get_inter_law_name_from_id(ilaw))
      call tact_behav_info_by_id(ilaw,nb_p,p_name,nb_int,law_comments(i_param))
      if( associated(p_name) ) then
        deallocate(p_name)
        nullify(p_name)
      end if
    end do

  end subroutine get_internal_law_help

end module h5_format
