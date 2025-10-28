! Layout of HDF5 output file (version 0):
! /
! | -> minor version (dataset)
! |
! | -> Simulation (group)
! |    |
! |    | -> nbDIME          (dataset integer)     ('header' part)
! |    | -> dime_mod        (dataset integer)     ('header' part)
! |    | -> M_INTEGRATOR_ID (dataset integer)     ('header' part)
! |    | -> T_INTEGRATOR_ID (dataset integer)     ('header' part)
! |    | -> M_INTEG_PARAM   (dataset real   )     ('header' part)
! |    | -> T_INTEG_PARAM   (dataset real   )     ('header' part)
! |    | -> T_ini           (dataset real   )     ('header' part)
! |    | -> T_end           (dataset real   )     ('header' part)
! |    | -> nb_record       (dataset integer)     ('header' part)
! |
! | -> Evolution (group)
! |    | -> ID_xx (group)
! |    |    |
! |    |    | -> NStep (dataset integer)        ('step header' part)
! |    |    | -> TPS   (dataset real   )        ('step header' part)
! |    |    | -> H     (dataset real   )        ('step header' part)
! |    |    |
! |    |    | -> RBDY2/3 (group)
! |    |    |    |
! |    |    |    | idata (dataset integer)
! |    |    |    | rdata (dataset real   )
! |    |    |
! |    |    | -> MAILx (group)
! |    |    |    |
! |    |    |    | -> mecax (group)
! |    |    |    |    |
! |    |    |    |    | -> idata    (dataset integer)
! |    |    |    |    | -> rdata    (dataset real   )
! |    |    |    |    | -> disp     (dataset real   )
! |    |    |    |    | -> dofs     (dataset real   )
! |    |    |    |    | -> grad     (dataset real   )
! |    |    |    |    | -> flux     (dataset real   )
! |    |    |    |    | -> internal (dataset real   )
! |    |    |    |
! |    |    |    | -> therx/porox/multi (group)
! |    |    |    |    |
! |    |    |    |    | -> idata    (dataset integer)
! |    |    |    |    | -> dofs     (dataset real   )
! |    |    |    |    | -> grad     (dataset real   )
! |    |    |    |    | -> flux     (dataset real   )
! |    |    |    |    | -> internal (dataset real   )
! |    |    |    |
! |    |    |    | -> porox/multi (group)
! |    |    |    |    |
! |    |    |    |    | -> idata    (dataset integer)
! |    |    |    |    | -> disp     (dataset real   )
! |    |    |    |    | -> dofs     (dataset real   )
! |    |    |    |    | -> grad     (dataset real   )
! |    |    |    |    | -> flux     (dataset real   )
! |    |    |    |    | -> internal (dataset real   )
! |    |    |
! |    |    | -> VlocRloc (group)
! |    |    |    |
! |    |    |    | ->idata (dataset integer)
! |    |    |    | ->rdata (dataset real   )
! |    |    |
! |    |
! |    | -> ID_xx+1 (group)
! |    |    |
! |    |    | ...
!
! And there is a Help section describing this layout
! with the parameters association

  !> \brief Get the name of a group
  function get_gr_name_v0_0( i_group, id_number )
    implicit none
    !> id of the group
    integer, intent(in) :: i_group
    !> name of group
    character(len=35)   :: get_gr_name_v0_0
    !> id_number
    integer, intent(in), optional :: id_number
    !
    character(len=27), parameter :: IAM = 'h5_format::get_gr_name_v0_0'
    character(len=12) :: groupname
    character(len=6 ) :: cid
    logical           :: with_step
  
    select case( i_group )
    case( gr_root )
      groupname = '/'
      with_step = .false.
    case( gr_simulation )
      groupname = '/Simulation'
      with_step = .false.
    case( gr_rbdy2 )
      groupname = '/RBDY2'
      with_step = .true.
    case( gr_rbdy3 )
      groupname = '/RBDY3'
      with_step = .true.
    case( gr_mecax )
      groupname = '/MAILx/mecax'
      with_step = .true.
    case( gr_therx )
      groupname = '/MAILx/therx'
      with_step = .true.
    case( gr_porox )
      groupname = '/MAILx/porox'
      with_step = .true.
    case( gr_multi )
      groupname = '/MAILx/multi'
      with_step = .true.
    case( gr_vlocrloc )
      groupname = '/VlocRloc'
      with_step = .true.
    case default
      groupname = ''
      with_step = .true.
    end select
  
    if( present(id_number) ) then
      write(cid,'(I0)') id_number
      get_gr_name_v0_0 = '/Evolution/ID_'//trim(cid)//trim(groupname)
    else if( with_step ) then
      write(cid,'(I0)') evolution_id
      get_gr_name_v0_0 = '/Evolution/ID_'//trim(cid)//trim(groupname)
    else
      get_gr_name_v0_0 = trim(groupname)
    end if
  
  end function get_gr_name_v0_0
  
  !> \brief Get the name of dataset to write
  function get_ds_name_v0_0( i_dataset )
    implicit none
    !> id of dataset
    integer, intent(in) :: i_dataset
    !> name of dataset
    character(len=15)   :: get_ds_name_v0_0
    !
    character(len=27), parameter :: IAM = 'h5_format::get_ds_name_v0_0'
  
    select case( i_dataset )
    case( ds_idata )
      get_ds_name_v0_0 = 'idata'
    case( ds_rdata )
      get_ds_name_v0_0 = 'rdata'
    case( ds_disp )
      get_ds_name_v0_0 = 'displacement'
    case( ds_dofs )
      get_ds_name_v0_0 = 'dofs'
    case( ds_grad )
      get_ds_name_v0_0 = 'grad'
    case( ds_flux )
      get_ds_name_v0_0 = 'flux'
    case( ds_internal )
      get_ds_name_v0_0 = 'internal'
    case( ds_dimension )
      get_ds_name_v0_0 = 'dimension'
    case( ds_dime_mode )
      get_ds_name_v0_0 = 'dime_mode'
    case( ds_m_integrator )
      get_ds_name_v0_0 = 'Integrator'
    case( ds_t_integrator )
      get_ds_name_v0_0 = 'Integrator'
    case( ds_m_integ_param)
      get_ds_name_v0_0 = 'Theta'
    case( ds_t_integ_param)
      get_ds_name_v0_0 = 'Theta'
    case( ds_nstep )
      get_ds_name_v0_0 = 'NStep'
    case( ds_tps )
      get_ds_name_v0_0 = 'TPS'
    case( ds_dt )
      get_ds_name_v0_0 = 'dt'
    case( ds_tini )
      get_ds_name_v0_0 = 'Tini'
    case( ds_tend )
      get_ds_name_v0_0 = 'Tend'
    case( ds_nb_record )
      get_ds_name_v0_0 = 'nb_record'
    case default
      get_ds_name_v0_0 = ''
    end select
  
  end function get_ds_name_v0_0
  
  !> \brief Get the name of dataset to write
  function get_ds_name_v0_1( i_dataset )
    implicit none
    !> id of dataset
    integer, intent(in) :: i_dataset
    !> name of dataset
    character(len=15)   :: get_ds_name_v0_1
    !
    character(len=25), parameter :: IAM = 'h5_format::get_ds_name_v0_1'
  
    select case( i_dataset )
    case( ds_release )
      get_ds_name_v0_1 = 'release'
    case( ds_git_branch )
      get_ds_name_v0_1 = 'git_branch'
    case( ds_git_rev )
      get_ds_name_v0_1 = 'git_revision'
    case default
      get_ds_name_v0_1 = get_ds_name_v0_0( i_dataset )
    end select
  
  end function get_ds_name_v0_1

  !> \brief Get the name of dataset to write
  function get_ds_name_v0_2( i_dataset )
    implicit none
    !> id of dataset
    integer, intent(in) :: i_dataset
    !> name of dataset
    character(len=15)   :: get_ds_name_v0_2
    !
    character(len=25), parameter :: IAM = 'h5_format::get_ds_name_v0_2'

    select case( i_dataset )
    case( ds_m_integrator )
      get_ds_name_v0_2 = 'MecaIntegrator'
    case( ds_t_integrator )
      get_ds_name_v0_2 = 'TherIntegrator'
    case( ds_m_integ_param)
      get_ds_name_v0_2 = 'MecaIntegParam'
    case( ds_t_integ_param)
      get_ds_name_v0_2 = 'TherIntegParam'
    case default
      get_ds_name_v0_2 = get_ds_name_v0_1( i_dataset )
    end select

  end function get_ds_name_v0_2

  
  !> \brief Set the dataset and group description of rigids for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_rigid_format_v0_(f_rigid, minor)
    implicit none
    !> rigid format to set
    type(T_rigid_format), intent(inout) :: f_rigid
    !> version of file format
    integer, intent(in) :: minor
    !
    character(len=31), parameter :: IAM = 'h5_format::set_rigid_format_v0_'
  
    call clean_rigid_format_(f_rigid)
  
    allocate( f_rigid%idata(1) )
  
    f_rigid%visible = 1
  
    f_rigid%idata( f_rigid%visible )%name = 'visible'
    f_rigid%idata( f_rigid%visible )%ibeg = 1
    f_rigid%idata( f_rigid%visible )%iend = 1
  
    f_rigid%idata_sz = f_rigid%idata( size(f_rigid%idata) )%iend
  
    if( nbDIME == 2 ) then
  
      allocate( f_rigid%rdata(2) )
  
      f_rigid%X = 1
      f_rigid%V = 2
  
      f_rigid%rdata( f_rigid%X )%name = 'X (x,y,theta)'
      f_rigid%rdata( f_rigid%X )%ibeg = 1
      f_rigid%rdata( f_rigid%X )%iend = 3
  
      f_rigid%rdata( f_rigid%V )%name = 'V (x,y,rot)'
      f_rigid%rdata( f_rigid%V )%ibeg = 4
      f_rigid%rdata( f_rigid%V )%iend = 6
  
      f_rigid%rdata_sz = f_rigid%rdata( size(f_rigid%rdata) )%iend
  
    else if( nbDIME == 3 ) then
  
      allocate( f_rigid%rdata(3) )
  
      f_rigid%X  = 1
      f_rigid%V  = 2
      f_rigid%LF = 3
  
      f_rigid%rdata( f_rigid%X )%name = 'X (x,y,z,phi,theta,psi)'
      f_rigid%rdata( f_rigid%X )%ibeg = 1
      f_rigid%rdata( f_rigid%X )%iend = 6
  
      f_rigid%rdata( f_rigid%V )%name = 'V (x,y,z,phi,theta,psi)'
      f_rigid%rdata( f_rigid%V )%ibeg = 7
      f_rigid%rdata( f_rigid%V )%iend = 12
  
      f_rigid%rdata( f_rigid%LF )%name = 'LocalFrame (xx,yx,zx,yx,yy,yz,zx,zy,zz)'
      f_rigid%rdata( f_rigid%LF )%ibeg = 13
      f_rigid%rdata( f_rigid%LF )%iend = 21
  
      f_rigid%rdata_sz = f_rigid%rdata( size(f_rigid%rdata) )%iend
  
    end if
  
  end subroutine set_rigid_format_v0_
  
  !> \brief Set the dataset and group description of meca mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_mecax_format_v0_(f_mecax, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_mecax
    !> version of file format
    integer, intent(in) :: minor
    !
    integer :: nb_ext, nb_int
    character(len=31), parameter :: IAM = 'h5_format::set_mecax_format_v0_'
    character(len=22) :: tensor_order
  
    call clean_meshx_format_(f_mecax)
  
    allocate( f_mecax%idata(5) )
    f_mecax%node_idx = 1
    f_mecax%dofs_idx = 2
    f_mecax%gps_idx  = 3
    f_mecax%is_rigid = 4
    f_mecax%is_coro  = 5
  
    f_mecax%idata( f_mecax%node_idx )%name = 'cc_node_index'
    f_mecax%idata( f_mecax%node_idx )%ibeg = 1
    f_mecax%idata( f_mecax%node_idx )%iend = 1
  
    f_mecax%idata( f_mecax%dofs_idx )%name = 'cc_dof_index'
    f_mecax%idata( f_mecax%dofs_idx )%ibeg = 2
    f_mecax%idata( f_mecax%dofs_idx )%iend = 2
  
    f_mecax%idata( f_mecax%gps_idx )%name = 'cc_gp_index'
    f_mecax%idata( f_mecax%gps_idx )%ibeg = 3
    f_mecax%idata( f_mecax%gps_idx )%iend = 3
  
    f_mecax%idata( f_mecax%is_rigid )%name = 'is_rigid'
    f_mecax%idata( f_mecax%is_rigid )%ibeg = 4
    f_mecax%idata( f_mecax%is_rigid )%iend = 4
  
    f_mecax%idata( f_mecax%is_coro )%name = 'is_coro'
    f_mecax%idata( f_mecax%is_coro )%ibeg = 5
    f_mecax%idata( f_mecax%is_coro )%iend = 5
  
    f_mecax%idata_sz = f_mecax%idata( size(f_mecax%idata) )%iend
  
    if( nbDIME == 2 ) then
  
      allocate( f_mecax%rdata(2) )
  
      f_mecax%RX = 1
      f_mecax%RV = 2
  
      f_mecax%rdata( f_mecax%RX )%name = 'RX (x,y,theta)'
      f_mecax%rdata( f_mecax%RX )%ibeg = 1
      f_mecax%rdata( f_mecax%RX )%iend = 3
  
      f_mecax%rdata( f_mecax%RV )%name = 'RV (x,y,rot)'
      f_mecax%rdata( f_mecax%RV )%ibeg = 4
      f_mecax%rdata( f_mecax%RV )%iend = 6
  
      f_mecax%rdata_sz = f_mecax%rdata( size(f_mecax%rdata) )%iend
  
      tensor_order = 'xx, xy, yy'
  
    else if( nbDIME == 3 ) then
  
      allocate( f_mecax%rdata(3) )
  
      f_mecax%RX = 1
      f_mecax%RV = 2
      f_mecax%LF = 3
  
      f_mecax%rdata( f_mecax%RX )%name = 'RX (x,y,z,phi,theta,psi)'
      f_mecax%rdata( f_mecax%RX )%ibeg = 1
      f_mecax%rdata( f_mecax%RX )%iend = 6
  
      f_mecax%rdata( f_mecax%RV )%name = 'RV (x,y,z,phi,theta,psi)'
      f_mecax%rdata( f_mecax%RV )%ibeg = 7
      f_mecax%rdata( f_mecax%RV )%iend = 12
  
      f_mecax%rdata( f_mecax%LF )%name = 'LocalFrame (xx,yx,zx,yx,yy,yz,zx,zy,zz)'
      f_mecax%rdata( f_mecax%LF )%ibeg = 13
      f_mecax%rdata( f_mecax%LF )%iend = 21
  
      f_mecax%rdata_sz = f_mecax%rdata( size(f_mecax%rdata) )%iend
  
      tensor_order = 'xx, xy, xz, yy, yz, zz'
  
    end if
  
    allocate( f_mecax%disp_field(1) )
    f_mecax%disp = 1
    f_mecax%disp_field( f_mecax%disp )%name = 'disp (x, y, z) for all nodes'
    f_mecax%disp_field( f_mecax%disp )%ibeg = 1
    f_mecax%disp_field( f_mecax%disp )%iend = nbDIME
    f_mecax%disp_field_sz = nbDIME
  
    allocate( f_mecax%dofs_field(1) )
    f_mecax%dofs = 1
    f_mecax%dofs_field( f_mecax%dofs )%name = 'dofs (all dofs in a 1D vector)'
    f_mecax%dofs_field( f_mecax%dofs )%ibeg = 1
    f_mecax%dofs_field( f_mecax%dofs )%iend = 1
    f_mecax%dofs_field_sz = 1
  
    !> \todo replace 1 by i_mecaM
    call get_max_field_sizes(1, nb_ext, nb_int)
  
    allocate( f_mecax%grad(1) )
    f_mecax%strain = 1
    f_mecax%grad( f_mecax%strain )%name = 'strain ('//trim(tensor_order)//')'
    f_mecax%grad( f_mecax%strain )%ibeg = 1
    f_mecax%grad( f_mecax%strain )%iend = nb_ext
    f_mecax%grad_sz = nb_ext
  
    allocate( f_mecax%flux(1) )
    f_mecax%stress = 1
    f_mecax%flux( f_mecax%stress )%name = 'stress ('//trim(tensor_order)//')'
    f_mecax%flux( f_mecax%stress )%ibeg = 1
    f_mecax%flux( f_mecax%stress )%iend = nb_ext
    f_mecax%flux_sz = nb_ext
  
    allocate( f_mecax%internal(1) )
    f_mecax%meca_internal = 1
    f_mecax%internal( f_mecax%meca_internal )%name = 'internal field'
    f_mecax%internal( f_mecax%meca_internal )%ibeg = 1
    f_mecax%internal( f_mecax%meca_internal )%iend = nb_int
    f_mecax%internal_sz = nb_int
  
  end subroutine set_mecax_format_v0_
  
  !> \brief Set the dataset and group description of a ther mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_therx_format_v0_(f_therx, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_therx
    !> version of file format
    integer, intent(in) :: minor
    !
    integer :: nb_ext, nb_int
    character(len=28), parameter :: IAM = 'h5_format::set_therx_format_v0_'
    character(len=22) :: tensor_order
  
    call clean_meshx_format_(f_therx)
  
    allocate( f_therx%idata(3) )
    f_therx%node_idx = 1
    f_therx%dofs_idx = 2
    f_therx%gps_idx  = 3
  
    f_therx%idata( f_therx%node_idx )%name = 'cc_node_index'
    f_therx%idata( f_therx%node_idx )%ibeg = 1
    f_therx%idata( f_therx%node_idx )%iend = 1
  
    f_therx%idata( f_therx%dofs_idx )%name = 'cc_dof_index'
    f_therx%idata( f_therx%dofs_idx )%ibeg = 2
    f_therx%idata( f_therx%dofs_idx )%iend = 2
  
    f_therx%idata( f_therx%gps_idx )%name = 'cc_gp_index'
    f_therx%idata( f_therx%gps_idx )%ibeg = 3
    f_therx%idata( f_therx%gps_idx )%iend = 3
  
    f_therx%idata_sz = f_therx%idata( size(f_therx%idata) )%iend
  
    if( nbDIME == 2 ) then
      tensor_order = 'xx, xy, yy'
    else if( nbDIME == 3 ) then
      tensor_order = 'xx, xy, xz, yy, yz, zz'
    end if
  
    allocate( f_therx%dofs_field(1) )
    f_therx%dofs = 1
    f_therx%dofs_field( f_therx%dofs )%name = 'all dofs (T) in a 1D vector)'
    f_therx%dofs_field( f_therx%dofs )%ibeg = 1
    f_therx%dofs_field( f_therx%dofs )%iend = 1
    f_therx%dofs_field_sz = 1
  
    !> \todo replace 2 by i_therM
    call get_max_field_sizes(2, nb_ext, nb_int)
  
    allocate( f_therx%grad(1) )
    f_therx%ther_grad = 1
  
    f_therx%grad( f_therx%ther_grad )%name = 'thermal gradient ('//trim(tensor_order)//')'
    f_therx%grad( f_therx%ther_grad )%ibeg = 1
    f_therx%grad( f_therx%ther_grad )%iend = nb_ext
  
    f_therx%grad_sz = f_therx%grad( size(f_therx%grad) )%iend
  
    allocate( f_therx%flux(1) )
    f_therx%ther_flux = 1
  
    f_therx%flux( f_therx%ther_flux )%name = 'thermal flux ('//trim(tensor_order)//')'
    f_therx%flux( f_therx%ther_flux )%ibeg = 1
    f_therx%flux( f_therx%ther_flux )%iend = nb_ext
  
    f_therx%flux_sz = f_therx%flux( size(f_therx%flux) )%iend
  
    allocate( f_therx%internal(1) )
    f_therx%ther_internal = 1
  
    f_therx%internal( f_therx%ther_internal )%name = 'ther internal field'
    f_therx%internal( f_therx%ther_internal )%ibeg = 1
    f_therx%internal( f_therx%ther_internal )%iend = nb_int
  
    f_therx%internal_sz = f_therx%internal( size(f_therx%internal) )%iend
  
  end subroutine set_therx_format_v0_
  
  !> \brief Set the dataset and group description of poro a mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_porox_format_v0_(f_porox, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_porox
    !> version of file format
    integer, intent(in) :: minor
    !
    integer :: nb_ext, nb_int
    character(len=31), parameter :: IAM = 'h5_format::set_porox_format_v0_'
    character(len=22) :: tensor_order
  
    call clean_meshx_format_(f_porox)
  
    if( nbDIME == 2 ) then
      tensor_order = 'xx, xy, yy'
    else if( nbDIME == 3 ) then
      tensor_order = 'xx, xy, xz, yy, yz, zz'
    end if
  
    allocate( f_porox%idata(3) )
    f_porox%node_idx = 1
    f_porox%dofs_idx = 2
    f_porox%gps_idx  = 3
  
    f_porox%idata( f_porox%node_idx )%name = 'cc_node_index'
    f_porox%idata( f_porox%node_idx )%ibeg = 1
    f_porox%idata( f_porox%node_idx )%iend = 1
  
    f_porox%idata( f_porox%dofs_idx )%name = 'cc_dof_index'
    f_porox%idata( f_porox%dofs_idx )%ibeg = 2
    f_porox%idata( f_porox%dofs_idx )%iend = 2
  
    f_porox%idata( f_porox%gps_idx )%name = 'cc_gp_index'
    f_porox%idata( f_porox%gps_idx )%ibeg = 3
    f_porox%idata( f_porox%gps_idx )%iend = 3
  
    f_porox%idata_sz = f_porox%idata( size(f_porox%idata) )%iend
  
    allocate( f_porox%disp_field(1) )
    f_porox%disp = 1
    f_porox%disp_field( f_porox%disp )%name = 'disp (x, y, z) for all nodes'
    f_porox%disp_field( f_porox%disp )%ibeg = 1
    f_porox%disp_field( f_porox%disp )%iend = nbDIME
    f_porox%disp_field_sz = nbDIME
  
    allocate( f_porox%dofs_field(1) )
    f_porox%dofs = 1
    f_porox%dofs_field( f_porox%dofs )%name = 'all dofs (V,P) in a 1D vector)'
    f_porox%dofs_field( f_porox%dofs )%ibeg = 1
    f_porox%dofs_field( f_porox%dofs )%iend = 1
    f_porox%dofs_field_sz = 1
  
    !> \todo replace 3 by i_poroM
    call get_max_field_sizes(3, nb_ext, nb_int)
  
    allocate( f_porox%grad(2) )
    f_porox%strain    = 1
    f_porox%ther_grad = 2
  
    f_porox%grad( f_porox%strain )%name = 'strain ('//trim(tensor_order)//')'
    f_porox%grad( f_porox%strain )%ibeg = 1
    f_porox%grad( f_porox%strain )%iend = nb_ext
  
    f_porox%grad( f_porox%ther_grad )%name = 'thermal gradient ('//trim(tensor_order)//')'
    f_porox%grad( f_porox%ther_grad )%ibeg =   nb_ext + 1
    f_porox%grad( f_porox%ther_grad )%iend = 2*nb_ext
  
    f_porox%grad_sz = f_porox%grad( size(f_porox%grad) )%iend
  
    allocate( f_porox%flux(2) )
    f_porox%stress    = 1
    f_porox%ther_flux = 2
  
    f_porox%flux( f_porox%stress )%name = 'stress ('//trim(tensor_order)//')'
    f_porox%flux( f_porox%stress )%ibeg = 1
    f_porox%flux( f_porox%stress )%iend = nb_ext
  
    f_porox%flux( f_porox%ther_flux )%name = 'thermal flux ('//trim(tensor_order)//')'
    f_porox%flux( f_porox%ther_flux )%ibeg =   nb_ext + 1
    f_porox%flux( f_porox%ther_flux )%iend = 2*nb_ext
  
    f_porox%flux_sz = f_porox%flux( size(f_porox%flux) )%iend
  
    allocate( f_porox%internal(2) )
    f_porox%meca_internal = 1
    f_porox%ther_internal = 2
  
    f_porox%internal( f_porox%meca_internal )%name = 'meca internal field'
    f_porox%internal( f_porox%meca_internal )%ibeg = 1
    f_porox%internal( f_porox%meca_internal )%iend = nb_int
  
    f_porox%internal( f_porox%ther_internal )%name = 'ther internal field'
    f_porox%internal( f_porox%ther_internal )%ibeg =   nb_int + 1
    f_porox%internal( f_porox%ther_internal )%iend = 2*nb_int
  
    f_porox%internal_sz = f_porox%internal( size(f_porox%internal) )%iend
  
  end subroutine set_porox_format_v0_
  
  !> \brief Set the dataset and group description of a multi mesh for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_multi_format_v0_(f_multi, minor)
    implicit none
    !> rigid format to set
    type(T_meshx_format), intent(inout) :: f_multi
    !> version of file format
    integer, intent(in) :: minor
    !
    integer :: nb_ext, nb_int
    character(len=31), parameter :: IAM = 'h5_format::set_multi_format_v0_'
    character(len=22) :: tensor_order
  
    call clean_meshx_format_(f_multi)
  
    if( nbDIME == 2 ) then
      tensor_order = 'xx, xy, yy'
    else if( nbDIME == 3 ) then
      tensor_order = 'xx, xy, xz, yy, yz, zz'
    end if
  
    allocate( f_multi%idata(3) )
    f_multi%node_idx = 1
    f_multi%dofs_idx = 2
    f_multi%gps_idx  = 3
  
    f_multi%idata( f_multi%node_idx )%name = 'cc_node_index'
    f_multi%idata( f_multi%node_idx )%ibeg = 1
    f_multi%idata( f_multi%node_idx )%iend = 1
  
    f_multi%idata( f_multi%dofs_idx )%name = 'cc_dof_index'
    f_multi%idata( f_multi%dofs_idx )%ibeg = 2
    f_multi%idata( f_multi%dofs_idx )%iend = 2
  
    f_multi%idata( f_multi%gps_idx )%name = 'cc_gp_index'
    f_multi%idata( f_multi%gps_idx )%ibeg = 3
    f_multi%idata( f_multi%gps_idx )%iend = 3
  
    f_multi%idata_sz = f_multi%idata( size(f_multi%idata) )%iend
  
    allocate( f_multi%disp_field(1) )
    f_multi%disp = 1
    f_multi%disp_field( f_multi%disp )%name = 'disp (x, y, z) for all nodes'
    f_multi%disp_field( f_multi%disp )%ibeg = 1
    f_multi%disp_field( f_multi%disp )%iend = nbDIME
    f_multi%disp_field_sz = nbDIME
  
    allocate( f_multi%dofs_field(1) )
    f_multi%dofs = 1
    f_multi%dofs_field( f_multi%dofs )%name = 'all dofs (V,P1, P2) in a 1D vector)'
    f_multi%dofs_field( f_multi%dofs )%ibeg = 1
    f_multi%dofs_field( f_multi%dofs )%iend = 1
    f_multi%dofs_field_sz = 1
  
    !> \todo replace 4 by i_multi
    call get_max_field_sizes(4, nb_ext, nb_int)
  
    allocate( f_multi%grad(1) )
    f_multi%multi_grad = 1
  
    f_multi%grad( f_multi%multi_grad )%name = 'multi_grad ('//trim(tensor_order)//')'
    f_multi%grad( f_multi%multi_grad )%ibeg = 1
    f_multi%grad( f_multi%multi_grad )%iend = nb_ext
  
    f_multi%grad_sz = f_multi%grad( size(f_multi%grad) )%iend
  
    allocate( f_multi%flux(1) )
    f_multi%multi_flux = 1
  
    f_multi%flux( f_multi%multi_flux )%name = 'multi flux ('//trim(tensor_order)//')'
    f_multi%flux( f_multi%multi_flux )%ibeg = 1
    f_multi%flux( f_multi%multi_flux )%iend = nb_ext
  
    f_multi%flux_sz = f_multi%flux( size(f_multi%flux) )%iend
  
    allocate( f_multi%internal(1) )
    f_multi%multi_internal = 1
  
    f_multi%internal( f_multi%multi_internal )%name = 'multi internal field'
    f_multi%internal( f_multi%multi_internal )%ibeg = 1
    f_multi%internal( f_multi%multi_internal )%iend = nb_int
  
    f_multi%internal_sz = f_multi%internal( size(f_multi%internal) )%iend
  
  end subroutine set_multi_format_v0_
  
  !> \brief Set the dataset and group description of interactions for a given format version
  !> Beware: the format depends on 'version' and space dimension !
  subroutine set_inter_format_v0_(f_inter, minor)
    implicit none
    !> interaction format to set
    type(T_inter_format), intent(inout) :: f_inter
    !> minor version of file format
    integer, intent(in) :: minor
    !
    integer :: offset
    character(len=31), parameter :: IAM = 'h5_format::set_inter_format_v0_'
  
    call clean_inter_format_(f_inter)
  
    allocate( f_inter%idata(15) )
  
    f_inter%inter_id    = 1
    f_inter%idata( f_inter%inter_id )%name = 'inter_id'
    f_inter%idata( f_inter%inter_id )%ibeg = 1
    f_inter%idata( f_inter%inter_id )%iend = 1
  
    f_inter%icdan       = 2
    f_inter%idata( f_inter%icdan )%name = 'icdan'
    f_inter%idata( f_inter%icdan )%ibeg = 2
    f_inter%idata( f_inter%icdan )%iend = 2
  
    f_inter%ent         = 3
    f_inter%idata( f_inter%ent )%name = 'entity id (cd,an)'
    f_inter%idata( f_inter%ent )%ibeg = 3
    f_inter%idata( f_inter%ent )%iend = 4
  
    f_inter%bdyty       = 4
    f_inter%idata( f_inter%bdyty )%name = 'bdyty (cd,an)'
    f_inter%idata( f_inter%bdyty )%ibeg = 5
    f_inter%idata( f_inter%bdyty )%iend = 6
  
    f_inter%ibdyty      = 5
    f_inter%idata( f_inter%ibdyty )%name = 'ibdyty (cd,an)'
    f_inter%idata( f_inter%ibdyty )%ibeg = 7
    f_inter%idata( f_inter%ibdyty )%iend = 8
  
    f_inter%tactype     = 6
    f_inter%idata( f_inter%tactype )%name = 'tactype (cd,an)'
    f_inter%idata( f_inter%tactype )%ibeg = 9
    f_inter%idata( f_inter%tactype )%iend = 10
  
    f_inter%itacty      = 7
    f_inter%idata( f_inter%itacty )%name = 'itacty (cd,an)'
    f_inter%idata( f_inter%itacty )%ibeg = 11
    f_inter%idata( f_inter%itacty )%iend = 12
  
    f_inter%itacbdy     = 8
    f_inter%idata( f_inter%itacbdy )%name = 'itacbdy (cd,an)'
    f_inter%idata( f_inter%itacbdy )%ibeg = 13
    f_inter%idata( f_inter%itacbdy )%iend = 14
  
    f_inter%iadj        = 9
    f_inter%idata( f_inter%iadj )%name = 'iadj'
    f_inter%idata( f_inter%iadj )%ibeg = 15
    f_inter%idata( f_inter%iadj )%iend = 15
  
    f_inter%isee        = 10
    f_inter%idata( f_inter%isee )%name = 'isee'
    f_inter%idata( f_inter%isee )%ibeg = 16
    f_inter%idata( f_inter%isee )%iend = 16
  
    f_inter%lawnb       = 11
    f_inter%idata( f_inter%lawnb )%name = 'lawnb'
    f_inter%idata( f_inter%lawnb )%ibeg = 17
    f_inter%idata( f_inter%lawnb )%iend = 17
  
    f_inter%i_law       = 12
    f_inter%idata( f_inter%i_law )%name = 'i_law'
    f_inter%idata( f_inter%i_law )%ibeg = 18
    f_inter%idata( f_inter%i_law )%iend = 18
  
    f_inter%status      = 13
    f_inter%idata( f_inter%status )%name = 'status'
    f_inter%idata( f_inter%status )%ibeg = 19
    f_inter%idata( f_inter%status )%iend = 19
  
    offset = 20
    if( minor > 1 ) then
        f_inter%isci        = 14
        f_inter%idata( f_inter%isci )%name = 'isci (cd, an)'
        f_inter%idata( f_inter%isci )%ibeg = offset
        f_inter%idata( f_inter%isci )%iend = offset+1
        offset = offset+2
    else
        f_inter%igeo        = 14
        f_inter%idata( f_inter%igeo )%name = '(icdver, ianal, ianseg)'
        f_inter%idata( f_inter%igeo )%ibeg = offset
        f_inter%idata( f_inter%igeo )%iend = offset+2
        offset = offset+3
    end if

    f_inter%nb_internal = 15
    f_inter%idata( f_inter%nb_internal )%name = 'nb_internal'
    f_inter%idata( f_inter%nb_internal )%ibeg = offset
    f_inter%idata( f_inter%nb_internal )%iend = offset
  
    f_inter%idata_sz = offset
  
    if( nbDIME == 2 ) then
  
      allocate( f_inter%rdata(6) )
  
      f_inter%rl        = 1
      f_inter%rdata( f_inter%rl )%name = 'rl (t,n)'
      f_inter%rdata( f_inter%rl )%ibeg = 1
      f_inter%rdata( f_inter%rl )%iend = 2
  
      f_inter%vl        = 2
      f_inter%rdata( f_inter%vl )%name = 'vl (t,n)'
      f_inter%rdata( f_inter%vl )%ibeg = 3
      f_inter%rdata( f_inter%vl )%iend = 4
  
      f_inter%gapTT     = 3
      f_inter%rdata( f_inter%gapTT )%name = 'gapTT'
      f_inter%rdata( f_inter%gapTT )%ibeg = 5
      f_inter%rdata( f_inter%gapTT )%iend = 5
  
      f_inter%coor      = 4
      f_inter%rdata( f_inter%coor )%name = 'coor (x,y)'
      f_inter%rdata( f_inter%coor )%ibeg = 6
      f_inter%rdata( f_inter%coor )%iend = 7
  
      f_inter%uc       = 5
      f_inter%rdata( f_inter%uc )%name = 'uc (nx,ny)'
      f_inter%rdata( f_inter%uc )%ibeg = 8
      f_inter%rdata( f_inter%uc )%iend = 9
  
      f_inter%internals = 6
      f_inter%rdata( f_inter%internals )%name = 'internals'
      f_inter%rdata( f_inter%internals )%ibeg = 10
      f_inter%rdata( f_inter%internals )%iend = 10+get_need_internal_tact()-1
  
      f_inter%rdata_sz = f_inter%rdata( size(f_inter%rdata) )%iend
  
    else if( nbDIME == 3 ) then
  
      allocate( f_inter%rdata(6) )
  
      f_inter%rl        = 1
      f_inter%rdata( f_inter%rl )%name = 'rl (t,n,s)'
      f_inter%rdata( f_inter%rl )%ibeg = 1
      f_inter%rdata( f_inter%rl )%iend = 3
  
      f_inter%vl        = 2
      f_inter%rdata( f_inter%vl )%name = 'vl (t,n,s)'
      f_inter%rdata( f_inter%vl )%ibeg = 4
      f_inter%rdata( f_inter%vl )%iend = 6
  
      f_inter%gapTT     = 3
      f_inter%rdata( f_inter%gapTT )%name = 'gapTT'
      f_inter%rdata( f_inter%gapTT )%ibeg = 7
      f_inter%rdata( f_inter%gapTT )%iend = 7
  
      f_inter%coor      = 4
      f_inter%rdata( f_inter%coor )%name = 'coor (x,y,z)'
      f_inter%rdata( f_inter%coor )%ibeg = 8
      f_inter%rdata( f_inter%coor )%iend = 10
  
      f_inter%uc       = 5
      f_inter%rdata( f_inter%uc )%name = 'uc (tx,ty,tz,nx,ny,nz,sx,ny,nz)'
      f_inter%rdata( f_inter%uc )%ibeg = 11
      f_inter%rdata( f_inter%uc )%iend = 19
  
      f_inter%internals = 6
      f_inter%rdata( f_inter%internals )%name = 'internals'
      f_inter%rdata( f_inter%internals )%ibeg = 20
      f_inter%rdata( f_inter%internals )%iend = 20+get_need_internal_tact()-1
  
      f_inter%rdata_sz = f_inter%rdata( size(f_inter%rdata) )%iend
  
    end if
  
  end subroutine set_inter_format_v0_
  
