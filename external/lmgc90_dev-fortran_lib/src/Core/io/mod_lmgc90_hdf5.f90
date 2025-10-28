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

!> Define low level function about hdf5 read/write

module lmgc90_hdf5

  use HDF5

  use utilities, only : faterr, logmes, &
                        T_string_array, &
                        G_i_list

  use overall, only : location, Nstep , &
                      TPSbegin, TPS, H, &
                      nbDIME, dime_mod, &
                      M_INTEGRATOR_ID , &
                      T_INTEGRATOR_ID , &
                      THETA, THETA_T, BETA2 , &
                      init_gear_integrator  , &
                      init_verlet_integrator, &
                      init_theta_integrator , &
                      init_beta2_integrator , &
                      init_qs_integrator    , &
                      init_cn_integrator    , &
                      release, git_branch   , &
                      git_revision          , &
                      internal_tact_comment_length


  use h5_format, only : evolution_id      , &
                        get_gr_name       , &
                        get_ds_name       , &
                        gr_simulation     , &
                        ds_dimension      , &
                        ds_dime_mode      , &
                        ds_m_integrator   , &
                        ds_t_integrator   , &
                        ds_m_integ_param  , &
                        ds_t_integ_param  , &
                        ds_nstep          , &
                        ds_tps            , &
                        ds_dt             , &
                        ds_tini           , &
                        ds_tend           , &
                        ds_nb_record      , &
                        ds_release        , &
                        ds_git_branch     , &
                        ds_git_rev        , &
                        get_format_help   , &
                        get_parameter_help, &
                        get_internal_law_help

  use parameters, only : INTEGRATOR_GEAR  , &
                         INTEGRATOR_VERLET, &
                         INTEGRATOR_MOREAU, &
                         INTEGRATOR_BETA2

  implicit none

  private

  public :: open_read_h5       , open_write_h5       , &
            read_h5_header     , write_h5_header     , &
            read_h5_step_header, write_h5_step_header, &
            read_h5            , write_h5            , &
            close_h5                                 , &
            reset_last_filename, version_check


  interface write_h5
     module procedure write_array_d_2d_
     module procedure write_array_d_1d_
     module procedure write_array_d_0d_
     module procedure write_array_i_2d_
     module procedure write_array_i_1d_
     module procedure write_array_i_0d_
     module procedure write_array_c_1d_
     module procedure write_array_c_0d_
  end interface write_h5

  interface read_h5
     module procedure read_array_d_2d_
     module procedure read_array_i_2d_
     module procedure read_array_d_0d_
     module procedure read_array_i_0d_
     module procedure read_array_i_1d_
  end interface read_h5

  integer, parameter, public :: major_hdf5_format = 0
  integer, parameter, public :: minor_hdf5_format = 3

  ! File identifier
  integer(HID_T)     :: file_id_
  ! File base name
  character(len=256) :: last_filename_ = ""

  ! Ugly fixer flag
  integer, public :: ugly_fix = -1

  ! list of private functions
  private write_help_        , &
          open_read_group_   , &
          open_write_group_  , &
          open_read_dataset_ , &
          open_write_dataset_, &
          read_version_      , &
          close_dataset_

contains

  !> \brief Open the input HDF5 file for reading
  subroutine open_read_h5( filename, major, minor )
    implicit none
    !> HDF5 file to open
    character(len=*), intent(in) :: filename
    !> internal version major number
    integer, intent(out) :: major
    !> internal version minor number
    integer, intent(out) :: minor
    !
    integer :: error
    logical :: fexist
    !
    character(len=256) :: wk_filename

    wk_filename = location(filename)

    ! Initialize FORTRAN interface
    call H5open_f( error )

    inquire( file=wk_filename, exist=fexist )

    ! Open the HDF5 file
    if( fexist ) then
      call H5Fopen_f( wk_filename, H5F_ACC_RDONLY_F, file_id_, error )
      if( error /= 0 ) call faterr("hd5::open_read_h5","Error opening file : "//wk_filename)
    else
      call faterr("hd5::open_read_h5","No file named : "//filename)
    end if

    ! Get the version of the hdf5 file format
    call read_version_(major, minor)

  end subroutine open_read_h5

!!!------------------------------------------------------------------------

  !> \brief Create or open the input HDF5 file for writing
  subroutine open_write_h5( major, minor, filename, last )
    !> internal version major number
    integer :: major
    !> internal version minor number
    integer :: minor
    !> HDF5 file to open
    character(len=*), optional :: filename
    !> Last file writing
    logical, optional :: last
    !
    integer :: error
    logical :: fexist, save_fname
    character(len=256) :: wk_filename

    if( present(filename) ) then
      wk_filename = location(trim(filename))
    else
      wk_filename = last_filename_
    end if

    ! Initialize FORTRAN interface
    call H5open_f( error )

    inquire( file=wk_filename, exist=fexist )

    ! Open the HDF5 file
    if( fexist ) then
      call H5Fopen_f( wk_filename, H5F_ACC_RDWR_F, file_id_, error )
      if( error /= 0 ) call faterr("hd5::open_write_h5","Error opening file : "//wk_filename)
    else
      call H5Fcreate_f( wk_filename, H5F_ACC_TRUNC_F, file_id_, error )
      if( error /= 0 ) call faterr("hd5::open_write_h5","Error creating file : "//wk_filename)
    end if

    ! Write the version of the hdf5 file format
    save_fname = .true.
    if( present(last) ) then
       if( last ) then
         save_fname = .false.
         call write_h5( "/", "version", (/major_hdf5_format,minor_hdf5_format/) )
       end if
    end if
    if( save_fname .and. last_filename_ /= wk_filename ) then
      call write_h5( "/", "version", (/major_hdf5_format,minor_hdf5_format/) )
      last_filename_ = location(trim(filename))
    end if

    major = major_hdf5_format
    minor = minor_hdf5_format

  end subroutine open_write_h5

!!!------------------------------------------------------------------------

  !> Close HDF5 file and library
  subroutine close_h5( )

    integer :: error ! Error flag

    ! Close the only HDF5 file
    call H5Fclose_f( file_id_, error )
    file_id_ = 0

    ! Finalize FORTRAN interface
    call H5close_f( error )

  end subroutine close_h5

!!!------------------------------------------------------------------------

  !> \brief Read header of HDF5 file
  subroutine read_h5_header()
    implicit none
    !
    real(kind=8) :: m_val, t_val
    character(len=35) :: gr_name

    gr_name = get_gr_name(gr_simulation)

    call read_h5( gr_name, get_ds_name(ds_dimension)    , nbDIME          )
    call read_h5( gr_name, get_ds_name(ds_dime_mode)    , dime_mod        )
    call read_h5( gr_name, get_ds_name(ds_m_integrator) , M_INTEGRATOR_ID )
    call read_h5( gr_name, get_ds_name(ds_t_integrator) , T_INTEGRATOR_ID )
    call read_h5( gr_name, get_ds_name(ds_m_integ_param), m_val           )
    call read_h5( gr_name, get_ds_name(ds_t_integ_param), t_val           )
    call read_h5( gr_name, get_ds_name(ds_tend)         , TPSbegin        )
    call read_h5( gr_name, get_ds_name(ds_nb_record)    , evolution_id    )

    TPS = TPSbegin

    ! integrator with initialization...
    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_GEAR )
      call init_gear_integrator()
    case( INTEGRATOR_VERLET )
      call init_verlet_integrator()
    case( INTEGRATOR_MOREAU )
      call init_theta_integrator(m_val)
    case( INTEGRATOR_BETA2 )
      call init_Beta2_integrator(m_val)
    end select

    select case( T_INTEGRATOR_ID )
    case( INTEGRATOR_MOREAU )
      call Init_CN_integrator(t_val)
    end select

  end subroutine read_h5_header
!!!------------------------------------------------------------------------

  !> \brief Check reset of Evolution group
  subroutine reset_evolution_(t_ini, t_end, is_last)
    implicit none
    real(kind=8), intent(in) :: t_ini
    real(kind=8), intent(in) :: t_end
    logical     , intent(in) :: is_last
    !
    logical           :: group_exists
    real(kind=8)      :: tfile
    integer           :: nlinks, storage_type, max_corder, error
    integer(HSIZE_T)  :: nlink
    integer(HID_T)    :: gr_id, gr_id2, dspace_id, dset_id
    type(H5O_INFO_T)  :: object_info
    integer(HSIZE_T), dimension(1) :: dims
    !
    character(len=35) :: gr_name, groupname

    ! if last, erase everything blindly and get out before messing with evolution_id
    if( is_last ) then
      call H5Gunlink_f( file_id_, "Evolution", error )
      return
    end if

    gr_name = get_gr_name(gr_simulation)
    call read_h5( gr_name, get_ds_name(ds_nb_record), evolution_id  )

    ! some checks to insure consistency
    if( t_ini >= 0.d0 .and. t_end < 0.d0 ) call faterr('lmgc90_hdf5::write_h5_header', 'Corrupted file, T_ini is present but no T_end')
    if( t_end <  0.d0 .and. t_ini > 0.d0 ) call faterr('lmgc90_hdf5::write_h5_header', 'Corrupted file, T_end is present but no T_ini')

    ! now do some cleaning if need arises
    call H5Lexists_f( file_id_, "Evolution", group_exists, error )
    if( group_exists ) then

      if( t_ini < 0.d0 .or. TPS <= t_ini ) then
        ! case where everything is deleted...
        call H5Gunlink_f( file_id_, "Evolution", error )
        evolution_id = 0
      else if( TPS < t_end ) then

        ! otherwise look for what to delete
        call H5Gopen_f( file_id_, "Evolution", gr_id, error )
        call H5Gget_info_f(gr_id, storage_type, nlinks, max_corder, error)

        nlink = 0
        do while (nlink < nlinks)

          ! look for other groups among all links
          call h5oget_info_by_idx_f(file_id_, "Evolution", H5_INDEX_NAME_F, H5_ITER_NATIVE_F, &
                                    nlink, object_info, error)
          if( object_info%type == H5O_TYPE_GROUP_F ) then
            call H5Lget_name_by_idx_f( file_id_, "Evolution", H5_INDEX_NAME_F, H5_ITER_NATIVE_F, nlink, groupname, error)
            call H5Gopen_f( gr_id, groupname, gr_id2, error)
            call H5Dopen_f( gr_id2, get_ds_name(ds_TPS), dset_id, error )
            call H5Dget_space_f( dset_id, dspace_id, error )
            call H5Dread_f( dset_id, H5T_NATIVE_DOUBLE, tfile, dims, error )
            call H5Dclose_f( dset_id  , error )
            call H5Sclose_f( dspace_id, error )
            call H5Gclose_f( gr_id2   , error )
            if( tfile > TPSbegin ) then
              call H5Ldelete_by_idx_f(  file_id_, "Evolution", H5_INDEX_NAME_F, H5_ITER_NATIVE_F, nlink, error)
              nlink  = nlink  - 1
              nlinks = nlinks - 1
              evolution_id = evolution_id - 1
            end if
          end if
          nlink = nlink + 1
        end do

        call H5Gclose_f( gr_id, error )

      end if

    else

      evolution_id = 0

    end if

  end subroutine reset_evolution_

  !> \brief Write header of HDF5 file
  subroutine write_h5_header(last)
    implicit none
    !> Last file writing
    logical, optional :: last
    !
    logical           :: is_last
    real(kind=8)      :: t_ini, t_end, m_val, t_val
    character(len=35) :: gr_name

    ! check if only writing last
    is_last = .false.
    if( present( last ) ) then
      is_last = last
    end if

    ! store LMGC90 version
    call write_h5( '/', get_ds_name(ds_release)   , release     ,  9)
    call write_h5( '/', get_ds_name(ds_git_branch), git_branch  , 40)
    call write_h5( '/', get_ds_name(ds_git_rev)   , git_revision, 40)

    gr_name = get_gr_name(gr_simulation)

    ! reset evolution group numbering

    call write_h5( gr_name, get_ds_name(ds_dimension)   , nbDIME          )
    call write_h5( gr_name, get_ds_name(ds_dime_mode)   , dime_mod        )
    call write_h5( gr_name, get_ds_name(ds_m_integrator), M_INTEGRATOR_ID )
    call write_h5( gr_name, get_ds_name(ds_t_integrator), T_INTEGRATOR_ID )
    select case( M_INTEGRATOR_ID )
    case( INTEGRATOR_MOREAU )
      m_val = THETA
    case( INTEGRATOR_BETA2 )
      m_val = BETA2
    case default
      m_val = 0.d0
    end select

    select case( T_INTEGRATOR_ID )
    case( INTEGRATOR_MOREAU )
      t_val = THETA_T
    case default
      t_val = 0.d0
    end select
    call write_h5( gr_name, get_ds_name(ds_m_integ_param), m_val)
    call write_h5( gr_name, get_ds_name(ds_t_integ_param), t_val)

    ! Try to get Tini and Tend in file (and set it if needed)
    t_ini = -1.d0
    t_end = -1.d0
    call read_h5( gr_name, get_ds_name(ds_tini), t_ini )
    call read_h5( gr_name, get_ds_name(ds_tend), t_end )

    call reset_evolution_(t_ini, t_end, is_last)

    if( is_last ) then
      call write_h5( gr_name, get_ds_name(ds_nb_record), 1)
    else
      call write_h5( gr_name, get_ds_name(ds_nb_record), evolution_id )
    end if

    if( is_last .or. t_ini < 0.d0 .or. TPSbegin <= t_ini ) then
      call write_h5( gr_name, get_ds_name(ds_tini), TPSbegin )
    end if
    call write_h5( gr_name, get_ds_name(ds_tend), TPSbegin )

    call write_help_()

  end subroutine write_h5_header

!!!------------------------------------------------------------------------

  !> \brief Read header of specific time step
  subroutine read_h5_step_header(step, success)
    implicit none
    !> time step to read
    integer, intent(in)  :: step
    !> is reading of desired a success
    logical, intent(out) :: success
    !
    integer           :: error
    integer(HID_T)    :: gr_id
    character(len=35) :: gr_name

    gr_name = get_gr_name(0, step)

    call open_read_group_(get_gr_name(0,step), gr_id)
    if( gr_id == 0 ) then
      success = .false.
      call logmes('[HDF5::read_dof] desired step does not exist : '//gr_name)
      return
    else
      call H5Gclose_f(gr_id, error)
    end if

    call read_h5( gr_name, get_ds_name(ds_nstep), NStep   )
    call read_h5( gr_name, get_ds_name(ds_tps  ), TPSbegin)
    call read_h5( gr_name, get_ds_name(ds_dt   ), H       )

    TPS = TPSbegin

    success = .true.

  end subroutine read_h5_step_header

!!!------------------------------------------------------------------------

  !> \brief Write header of specific time step
  subroutine write_h5_step_header(last)
    implicit none
    logical :: last
    !
    integer           :: error, evol_id
    integer(HID_T)    :: gr_id
    character(len=35) :: gr_name

    if( last ) then
      evol_id = 1
    else
      evolution_id = evolution_id + 1
      evol_id = evolution_id
    end if

    gr_name = get_gr_name(0, evol_id)

    call write_h5( gr_name, get_ds_name(ds_nstep) , NStep)
    call write_h5( gr_name, get_ds_name(ds_tps  ) , TPS  )
    call write_h5( gr_name, get_ds_name(ds_dt   ) , H    )

    !!! create empty group MAILx to avoid error in future
    call open_write_group_( trim(adjustl(gr_name))//'/MAILx', gr_id )
    call H5Gclose_f(gr_id, error)

    ! store last time step written
    call write_h5( get_gr_name(gr_simulation), get_ds_name(ds_tend     ), TPS         )
    call write_h5( get_gr_name(gr_simulation), get_ds_name(ds_nb_record), evol_id)

  end subroutine write_h5_step_header

!!!------------------------------------------------------------------------

  !> \brief Write the Help group
  subroutine write_help_()
    implicit none
    !
    integer :: i_data, nb_data, error
    logical :: group_exists
    !
    character(len=42), dimension(:)  , pointer :: data_group
    character(len=40), dimension(:)  , pointer :: data_name
    integer,           dimension(:,:), pointer :: data_bound
    !
    type(T_string_array), dimension(:), pointer :: data_cvalue
    type(G_i_list)      , dimension(:), pointer :: data_ivalue
    !
    character(len=40), dimension(:), pointer :: str_buff
    !
    character(len=42) :: ilaw_group
    integer                                    , dimension(:), pointer :: law_ids
    character(len=40)                          , dimension(:), pointer :: law_names
    character(len=internal_tact_comment_length), dimension(:), pointer :: law_comments

    allocate( str_buff(1) )

    ! erase pre-existing Help field
    call H5Lexists_f( file_id_, "Help", group_exists, error )
    if( group_exists ) then
      ! case where everything is deleted...
      call H5Gunlink_f( file_id_, "Help", error )
    end if

    data_group => null()
    data_name  => null()
    data_bound => null()

    call get_format_help( data_group, data_name, data_bound )

    nb_data = size(data_group)

    do i_data = 1, nb_data

      str_buff(1) = data_name(i_data)
      call write_h5( data_group(i_data), 'name' , str_buff, 40)
      call write_h5( data_group(i_data), 'bound', data_bound(:,i_data) )

    end do

    deallocate(data_group)
    deallocate(data_name )
    deallocate(data_bound)

    deallocate(str_buff)

    ! writing parameters
    data_group  => null()
    data_cvalue => null()
    data_ivalue => null()

    call get_parameter_help( data_group, data_cvalue, data_ivalue )
    nb_data = size(data_group)
    do i_data = 1, nb_data
      call write_h5( data_group(i_data), 'name', data_cvalue(i_data)%sdata, 40)
      call write_h5( data_group(i_data), 'id'  , data_ivalue(i_data)%G_i)
    end do

    deallocate(data_group )
    do i_data = 1, nb_data
      if( associated(data_ivalue(i_data)%G_i) ) then
        deallocate(data_ivalue(i_data)%G_i)
        nullify(data_ivalue(i_data)%G_i)
      end if
      if( associated(data_cvalue(i_data)%sdata) ) then
        deallocate(data_cvalue(i_data)%sdata)
        nullify(data_cvalue(i_data)%sdata)
      end if
    end do
    deallocate(data_ivalue)
    deallocate(data_cvalue)

    ! writing ilaw parameters and internal comments
    call get_internal_law_help(ilaw_group, law_ids, law_names, law_comments)
    if( size(law_ids) > 0 ) then
        call write_h5( ilaw_group, 'name'   , law_names   , 40)
        call write_h5( ilaw_group, 'id'     , law_ids      )
        call write_h5( ilaw_group, 'comment', law_comments, internal_tact_comment_length)
    end if

    deallocate(law_names   )
    deallocate(law_ids     )
    deallocate(law_comments)

  end subroutine write_help_

!!!------------------------------------------------------------------------

  !> \brief Read a 2D array of double
  subroutine read_array_d_2d_( groupname, datasetname, data )
    !> name of group in which to read
    character(len=*), intent(in) :: groupname
    !> name of dataset to read
    character(len=*), intent(in) :: datasetname
    !> data read
    real(kind=8), dimension(:,:), pointer :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 2
    integer(HSIZE_T), dimension(nb_rank) :: dims, max_dims

    plist_id = 0

    call open_read_group_( trim( adjustl( groupname ) ), group_id )
    if( group_id == 0 ) return

    call open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    call H5Sget_simple_extent_dims_f( dspace_id, dims, max_dims, error )
    if( associated(data) ) then
      if( any( shape(data)/=dims ) ) deallocate(data)
    end if
    if( .not. associated(data) ) allocate( data(dims(1), dims(2)) )

    call H5Dread_f( dset_id, H5T_NATIVE_DOUBLE, data, dims, error )

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_array_d_2d_

!!!------------------------------------------------------------------------

  !> \brief Read a 2D array of integer
  subroutine read_array_i_2d_( groupname, datasetname, data )
    !> name of group in which to read
    character(len=*), intent(in) :: groupname
    !> name of dataset to read
    character(len=*), intent(in) :: datasetname
    !> data read
    integer, dimension(:,:), pointer :: data
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 2
    integer(HSIZE_T), dimension(nb_rank) :: dims, max_dims

    plist_id = 0

    call open_read_group_( trim( adjustl( groupname ) ), group_id )
    if( group_id == 0 ) return
    call open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    call H5Sget_simple_extent_dims_f( dspace_id, dims, max_dims, error )
    if( associated(data) ) then
      if( any( shape(data)/=dims ) ) deallocate(data)
    end if
    if( .not. associated(data) ) allocate( data(dims(1), dims(2)) )

    call H5Dread_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error )

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_array_i_2d_

!!!------------------------------------------------------------------------

  !> \brief Read a 1D array of integer
  subroutine read_array_i_1d_( groupname, datasetname, data )
    !> name of group in which to read
    character(len=*), intent(in) :: groupname
    !> name of dataset to read
    character(len=*), intent(in) :: datasetname
    !> data read
    integer, dimension(:), pointer :: data
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 1
    integer(HSIZE_T), dimension(nb_rank) :: dims, max_dims

    plist_id = 0

    call open_read_group_( trim( adjustl( groupname ) ), group_id )
    if( group_id == 0 ) return
    call open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    call H5Sget_simple_extent_dims_f( dspace_id, dims, max_dims, error )
    if( associated(data) ) then
      if( any( shape(data)/=dims ) ) deallocate(data)
    end if
    if( .not. associated(data) ) allocate( data(dims(1)) )

    call H5Dread_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error )

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_array_i_1d_

!!!------------------------------------------------------------------------


  !> \brief Read a double
  subroutine read_array_d_0d_( groupname, datasetname, data )
    !> name of group in which to read
    character(len=*), intent(in)  :: groupname
    !> name of dataset to read
    character(len=*), intent(in)  :: datasetname
    !> data read
    real(kind=8)    , intent(out) :: data
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer(HSIZE_T), dimension(1) :: dims

    plist_id = 0

    call open_read_group_( trim( adjustl( groupname ) ), group_id )
    if( group_id == 0 ) return

    call open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    call H5Dread_f( dset_id, H5T_NATIVE_DOUBLE, data, dims, error )

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_array_d_0d_

!!!------------------------------------------------------------------------

  !> \brief Read an integer
  subroutine read_array_i_0d_( groupname, datasetname, data )
    !> name of group in which to read
    character(len=*), intent(in)  :: groupname
    !> name of dataset to read
    character(len=*), intent(in)  :: datasetname
    !> data read
    integer         , intent(out) :: data
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer(HSIZE_T), dimension(1) :: dims

    plist_id = 0

    call open_read_group_( trim( adjustl( groupname ) ), group_id )
    if( group_id == 0 ) return

    call open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    call H5Dread_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error )

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_array_i_0d_

!!!------------------------------------------------------------------------
  subroutine read_version_(major, minor)
    implicit none
    !> major version number read
    integer, intent(out) :: major
    !> minor version number read
    integer, intent(out) :: minor
    !
    integer :: rank, error
    logical :: dataset_exists
    integer, dimension(2) :: version
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    integer(HSIZE_T), dimension(1) :: dims, max_dims

    ! check that the group exists
    call H5Lexists_f( file_id_, "version", dataset_exists, error )
    if ( .not. dataset_exists ) then
      call faterr("hd5::read_version","Version not found in file")
    end if

    plist_id = 0
    call open_read_group_( '/', group_id )
    if( group_id == 0 ) return
    call open_read_dataset_(group_id, 'version', dset_id, dspace_id)
    if( dset_id == 0 ) then
      call H5Gclose_f( group_id, error )
      return
    end if

    ! get the rank of version 'file' to handle v0 format...
    call H5Sget_simple_extent_ndims_f( dspace_id, rank, error )
    if( rank == 0 ) then
      major = 0
      call H5Dread_f( dset_id, H5T_NATIVE_INTEGER, minor, dims, error )
    else if( rank == 1 ) then
      call H5Sget_simple_extent_dims_f( dspace_id, dims, max_dims, error )
      if( dims(1) == 2 ) then
        call H5Dread_f( dset_id, H5T_NATIVE_INTEGER, version, dims, error )
        major = version(1)
        minor = version(2)
      else
        call faterr("hd5::read_version","Version not readable (too many values)")
      end if
    else
      call faterr("hd5::read_version","Version not readable (too high rank)")
    end if

    call close_dataset_( dset_id, dspace_id, plist_id )
    call H5Gclose_f( group_id, error )

  end subroutine read_version_

!!!------------------------------------------------------------------------

  !> \brief Write a 2D array of double
  subroutine write_array_d_2d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in)      :: groupname
    !> name of dataset to write
    character(len=*), intent(in)      :: datasetname
    !> data to write
    real(kind=8)    , dimension(:,:), intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 2            ! Dataset nb_rank
    integer(HSIZE_T), dimension(nb_rank) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    dims = shape( data )

    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_DOUBLE, dims, &
                             dset_id, dspace_id, plist_id, .false.           )

    call H5Dwrite_f( dset_id, H5T_NATIVE_DOUBLE, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_d_2d_

!!!------------------------------------------------------------------------

  !> \brief Write a 1D array of double
  subroutine write_array_d_1d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in)      :: groupname
    !> name of dataset to write
    character(len=*), intent(in)      :: datasetname
    !> data to write
    real(kind=8)    , dimension( : ), intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier

    integer, parameter :: nb_rank = 1            ! Dataset nb_rank
    integer(HSIZE_T), dimension(nb_rank) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )


    dims = shape( data )

    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_DOUBLE, dims, &
                             dset_id, dspace_id, plist_id, .false.           )

    call H5Dwrite_f( dset_id, H5T_NATIVE_DOUBLE, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_d_1d_

!!!------------------------------------------------------------------------

  !> \brief Write a double
  subroutine write_array_d_0d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in) :: groupname
    !> name of dataset to write
    character(len=*), intent(in) :: datasetname
    !> data to write
    real(kind=8)    , intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    dims(1) = 1
    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_DOUBLE, dims, &
                             dset_id, dspace_id, plist_id, .true.            )

    call H5Dwrite_f( dset_id, H5T_NATIVE_DOUBLE, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_d_0d_

!!!------------------------------------------------------------------------

  !> \brief Write a 2D array of integer
  subroutine write_array_i_2d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in)      :: groupname
    !> name of dataset to write
    character(len=*), intent(in)      :: datasetname
    !> data to write
    integer         , dimension(:,:), intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 2            ! Dataset nb_rank
    integer(HSIZE_T), dimension(nb_rank) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )


    ! Write the array data
    dims = shape( data )

    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_INTEGER, dims, &
                             dset_id, dspace_id, plist_id, .false.            )

    call H5Dwrite_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_i_2d_

!!!------------------------------------------------------------------------

  !> \brief Write a 1D array of integer
  subroutine write_array_i_1d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in)    :: groupname
    !> name of dataset to write
    character(len=*), intent(in)    :: datasetname
    !> data to write
    integer         , dimension(:), intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer, parameter :: nb_rank = 1            ! Dataset nb_rank
    integer(HSIZE_T), dimension(nb_rank) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    ! Write the array data
    dims = shape( data )

    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_INTEGER, dims, &
                             dset_id, dspace_id, plist_id, .false.            )

    call H5Dwrite_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_i_1d_

!!!------------------------------------------------------------------------

  !> \brief Write am integer
  subroutine write_array_i_0d_( groupname, datasetname, data )
    !> name of group in which to write
    character(len=*), intent(in) :: groupname
    !> name of dataset to write
    character(len=*), intent(in) :: datasetname
    !> data to write
    integer         , intent(in) :: data
    !
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id                     ! property list identifier
    !
    integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    dims(1) = 1
    call open_write_dataset_(group_id, datasetname, H5T_NATIVE_INTEGER, dims, &
                             dset_id, dspace_id, plist_id, .true.             )

    call H5Dwrite_f( dset_id, H5T_NATIVE_INTEGER, data, dims, error, xfer_prp=plist_id )

    call close_dataset_( dset_id, dspace_id, plist_id )

    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_i_0d_

!!!------------------------------------------------------------------------

  !> \brief Write clen characters string array
  subroutine write_array_c_1d_( groupname, datasetname, data, clen )
    !> name of group in which to write
    character(len=*) , intent(in) :: groupname
    !> name of dataset to write
    character(len=*) , intent(in) :: datasetname
    !> data to write
    character(len=*), dimension(:), pointer :: data
    !> len of string
    integer, intent(in) :: clen
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id, string_type
    !
    integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
    integer(SIZE_T)                :: slen
    type(c_ptr) :: f_ptr

    slen = clen

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    ! Write the array data
    dims = (/ size(data) /)

    call H5Tcopy_f( H5T_FORTRAN_S1, string_type, error )
    call H5Tset_size_f( string_type, slen, error )

    call open_write_dataset_(group_id, datasetname, string_type, dims, &
                             dset_id, dspace_id, plist_id, .true.      )

    f_ptr = c_loc( data(1)(1:1) )
    call H5Dwrite_f( dset_id, string_type, f_ptr, error, dspace_id )
    !call H5Dwrite_vl_f( dset_id, string_type, data, dims, slen, error, dspace_id )

    call close_dataset_( dset_id, dspace_id, plist_id )


    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_c_1d_
!!!------------------------------------------------------------------------

  !> \brief Write clen characters string
  subroutine write_array_c_0d_( groupname, datasetname, data, clen )
    !> name of group in which to write
    character(len=*) , intent(in) :: groupname
    !> name of dataset to write
    character(len=*) , intent(in) :: datasetname
    !> data to write
    character(len=*) :: data
    !> len of strings
    integer, intent(in) :: clen
    ! Local variables
    integer        :: error
    integer(HID_T) :: group_id, dset_id, dspace_id ! group/dataset/dataspace identifier
    integer(HID_T) :: plist_id, string_type
    !
    integer(HSIZE_T), dimension(1) :: dims ! Dataset dimensions
    integer(SIZE_T)                :: slen
    type(c_ptr) :: f_ptr
    character(len=40), target :: ldata

    slen = clen

    ! Get the HDF5 group
    call open_write_group_( trim( adjustl( groupname ) ), group_id )

    ! Write the array data
    dims(1) = 1

    call H5Tcopy_f( H5T_FORTRAN_S1, string_type, error )
    call H5Tset_size_f( string_type, slen, error )

    call open_write_dataset_(group_id, datasetname, string_type, dims, &
                             dset_id, dspace_id, plist_id, .true.      )

    ldata = data
    call H5Dwrite_f( dset_id, string_type, c_loc(ldata(1:1)), error, dspace_id )
    !call H5Dwrite_vl_f( dset_id, string_type, data, dims, slen, error, dspace_id )

    call close_dataset_( dset_id, dspace_id, plist_id )


    ! Close the HDF5 group
    call H5Gclose_f( group_id, error )

  end subroutine write_array_c_0d_

  !!!------------------------------------------------------------------------

  !> \brief Open an existing group
  subroutine open_read_group_( groupname, group_id )
    !> name of group to open
    character(len=*), intent(in)  :: groupname
    !> id of opened group (0 if group not found)
    integer(HID_T)  , intent(out) :: group_id
    !
    ! Local variables
    integer(HID_T) :: gr_id
    integer        :: error, i, nb_slash
    logical        :: group_exists

    group_id = 0

    call H5Lexists_f( file_id_, groupname, group_exists, error )
    if( error /= 0 ) then
      call faterr( 'hdf5::open_read_group','error when checking existence of '//groupname)
    end if

    if ( group_exists ) then
       call H5Gopen_f( file_id_, groupname, group_id, error )
       if( error /= 0 ) then
         call faterr( 'hdf5::open_read_group','error when opening '//groupname)
       end if
    end if

  end subroutine open_read_group_
  !!!------------------------------------------------------------------------

  !> \brief Try to open a group and create it if it does not exists
  subroutine open_write_group_( groupname, group_id )
    !> name of group to open
    character(len=*), intent(in)  :: groupname
    !> id of opened group (0 on error)
    integer(HID_T)  , intent(out) :: group_id
    !
    ! Local variables
    integer(HID_T) :: gr_id
    integer        :: error, i, nb_slash
    logical        :: group_exists

    group_id = 0

    if ( groupname == "/" ) then
       call H5Gopen_f( file_id_, groupname, group_id, error )
       if( error /= 0 ) group_id = 0
       return
    end if

    ! Check if the components of a group name exist.
    ! If they don't, create them on fly.
    nb_slash = 0
    do i = 1, len( groupname )

       if ( groupname( i : i ) == "/" ) then
          nb_slash = nb_slash + 1
          if ( 1 < nb_slash ) then

             call H5Lexists_f( file_id_, groupname( : i - 1 ), group_exists, error )
             if( error /= 0 ) return

             if ( .NOT. group_exists ) then

                call H5Gcreate_f( file_id_, groupname( : i - 1 ), gr_id, error )
                if( error /= 0 ) return

                call H5Gclose_f( gr_id, error )
                if( error /= 0 ) return

             end if
          end if

       end if

    end do

    ! Create/open a group in the file
    call H5Lexists_f( file_id_, groupname, group_exists, error )
    if( error /= 0 ) return

    if ( group_exists ) then
       call H5Gopen_f( file_id_, groupname, group_id, error )
       if( error /= 0 ) return
    else
       call H5Gcreate_f( file_id_, groupname, group_id, error )
       if( error /= 0 ) return
    end if

  end subroutine open_write_group_

!!!------------------------------------------------------------------------

  !> \brief Open an existing dataset
  subroutine open_read_dataset_(group_id, datasetname, dset_id, dspace_id)
    !> id of group owning the dataset to open
    integer(HID_T)  , intent(in)  :: group_id
    !> name of dataset to open
    character(len=*), intent(in)  :: datasetname
    !> id of open dataset (0 on error)
    integer(HID_T)  , intent(out) :: dset_id         ! Dataset identifier
    !> id of open dataspace associated with dataset (0 on error)
    integer(HID_T)  , intent(out) :: dspace_id       ! Dataspace identifier
    !
    logical :: dset_exists
    integer :: error

    dset_id         = 0
    dspace_id       = 0

    call H5Lexists_f( group_id, datasetname, dset_exists, error )
    if( error /= 0 ) return

    if( dset_exists ) then

      call H5Dopen_f( group_id, datasetname, dset_id, error )
      if( error /= 0 ) return

      call H5Dget_space_f( dset_id, dspace_id, error )
      if( error /= 0 ) then
        call H5Dclose_f( dset_id, error )
        dset_id = 0
      end if

      return

    end if

  end subroutine open_read_dataset_

!!!------------------------------------------------------------------------

  !!> \brief Try to open a dataset and create it if it does not exist
  subroutine open_write_dataset_(group_id, datasetname, dtype, dims, &
                                 dset_id, dspace_id, plist_id, compact)
    !> id of group owning the dataset to open
    integer(HID_T)                , intent(in)    :: group_id
    !> name of dataset to open
    character(len=*)              , intent(in)    :: datasetname
    !> type of data in the set (used if dataset does not already exist)
    integer(HID_T)                , intent(in)    :: dtype
    !> dimensions of data in the set (used if dataset does not already exist)
    integer(HSIZE_T), dimension(:), intent(inout) :: dims
    !> id of dataset
    integer(HID_T)                , intent(out)   :: dset_id
    !> id of dataspace associated to dataset
    integer(HID_T)                , intent(out)   :: dspace_id
    !> property list id associated to dataset (created if dataset does not already exist)
    integer(HID_T)                , intent(out)   :: plist_id
    !> if true, change the layout of the dataset (used if dataset does not already exist) (only for small dataset)
    logical                       , intent(in)    :: compact
    !
    integer(HID_T)   :: plist_create_id ! Property list identifier
    integer(HSIZE_T) :: nb_dims
    integer :: error
    logical :: dset_exists

    dset_id         = 0
    dspace_id       = 0
    plist_id        = 0

    ! If dataset already exists, just open it and get the size of data and quit
    call H5Lexists_f( group_id, datasetname, dset_exists, error )
    if( error /= 0 ) return

    if( dset_exists ) then

      call H5Dopen_f( group_id, datasetname, dset_id, error )
      if( error /= 0 ) return

      call H5Dget_space_f( dset_id, dspace_id, error )
      if( error /= 0 ) then
        call H5Dclose_f( dset_id, error )
        dset_id = 0
      end if

      return

    end if

    ! No error check since this does not depend on external input

    ! Create this property to preserve partially initialise fields
    call H5Pcreate_f( H5P_DATASET_XFER_F, plist_id, error )
    call H5Pset_preserve_f( plist_id, .TRUE., error )

    ! Create this property to minimized the space consumption
    call H5Pcreate_f( H5P_DATASET_CREATE_F, plist_create_id, error )
    if( compact ) then
      call H5Pset_layout_f( plist_create_id, H5D_COMPACT_F, error )
    else
      call H5Pset_layout_f( plist_create_id, H5D_CONTIGUOUS_F, error )
    end if

    ! Create data_space
    if( dims(1) /= 1 .or. sum(dims) /= 1 ) then
      call H5Screate_simple_f( size(dims), dims, dspace_id, error )
    else
      call H5Screate_f( H5S_SCALAR_F, dspace_id, error )
    end if

    if( error /= 0 ) then
      call H5Dclose_f( dset_id, error )
      dset_id = 0
      return
    end if

    ! Create a dataset in the file
    call H5Dcreate_f( group_id, datasetname, dtype, dspace_id, &
                      dset_id, error, dcpl_id = plist_create_id )
    if( error /= 0 ) then
      call H5Dclose_f( dset_id  , error )
      call H5Sclose_f( dspace_id, error )
      dset_id   = 0
      dspace_id = 0
      return
    end if


    call H5Pclose_f( plist_create_id, error )

  end subroutine open_write_dataset_

  !> \brief close dataset, dataspace and property_list
  subroutine close_dataset_(dset_id, dspace_id, plist_id)
    !> id of dataset to close
    integer(HID_T), intent(in) :: dset_id
    !> id of dataspace to close
    integer(HID_T), intent(in) :: dspace_id
    !> id of property list to close
    integer(HID_T), intent(in) :: plist_id
    !
    integer :: error

    ! Close every HDF5 things
    if( dspace_id > 0 ) call H5Sclose_f( dspace_id, error )
    if( dset_id   > 0 ) call H5Dclose_f( dset_id  , error )
    if( plist_id  > 0 ) call H5Pclose_f( plist_id , error )

  end subroutine close_dataset_

  !!!------------------------------------------------------------------------

  !> \brief Reset the stored name of hdf5 file and reset evolution group numbering
  subroutine reset_last_filename()
    implicit none

    last_filename_ = ""
    !evolution_id   = 0

  end subroutine reset_last_filename

  !!!------------------------------------------------------------------------

  
  subroutine version_check(major, minor)
    implicit none
    integer, intent(in) :: major, minor
    !                           12345678901234567890123457
    character(len=26) :: IAM = "[h5_format::version_check]"
    character(len=80) :: cout
    logical :: group_exists
    integer :: error

    ! because it has been forgotten to change
    ! format when DISPx and xPSID module were removed
    ! something may need to be done : by hand !
    ! warn the user in this case
    if( major == 0 .and. minor == 0 .and. ugly_fix /= 0 ) then
        ! if this group exists... then it is ok
        ! the file has been writen before the execution of PNEUx....
        call H5Lexists_f( file_id_, "Help/parameters", group_exists, error )
        if( .not. group_exists ) then
          write(cout , '(A,I0,A1,I0)') "[HDF5:WARNING] format of current file: ", major, ".", minor
          call logmes( cout, .true. )
          write(cout , *) "               there is an uncertainty on the version of LMGC90"
          call logmes( cout, .true. )
          write(cout , *) "               which wrote this HDF5 file."
          call logmes( cout, .true. )
          write(cout , *) "               You may have problem when reading interactions."
          call logmes( cout, .true. )
          write(cout , *) "               If it is the case, you may fix it by running"
          call logmes( cout, .true. )
          write(cout , *) "               'chipy.io_hdf5_FixVersion(0)' before reading"
          call logmes( cout, .true. )
        endif
    endif

  end subroutine version_check

end module lmgc90_hdf5
