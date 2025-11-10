
program compas_lmgc90

  use parameters, only : i_prprx

  use utilities, only : faterr

  use overall, only : init_dimension         , &
                      set_time_step          , &
                      init_theta_integrator  , &
                      read_in_dof_ol         , &
                      write_out_bodies_ol    , &
                      write_out_driven_dof_ol, &
                      write_xxx_dof_ol       , &
                      write_xxx_Vloc_Rloc_ol , &
                      clean_writing_flags    , &
                      Init_EntityList        , &
                      set_run_contactor      , &
                      time_increment         , &
                      display_time           , &
                      Updt_time_begin        , &
                      get_NSTEP


  use bulk_behaviour, only : read_in_bulk_behav  , &
                             write_out_bulk_behav, &
                             clean_memory_bulk_behav => clean_memory

  use tact_behaviour, only : open_tact_behav_ll , &
                             open_see_ll        , &
                             read_xxx_tact_behav, &
                             close_tact_behav_ll, &
                             close_see_ll       , &
                             write_xxx_tact_behav, &
                             clean_memory_tact_behav => clean_memory

  use RBDY3, only: increment_RBDY3               , &
                   read_in_bodies_RBDY3          , &
                   read_in_dof_RBDY3             , &
                   read_in_driven_dof_RBDY3      , &
                   read_behaviours_RBDY3         , &
                   update_existing_entities_RBDY3, &
                   get_write_DOF_RBDY3           , &
                   write_out_bodies_RBDY3        , &
                   write_xxx_dof_RBDY3           , &
                   write_out_driven_dof_RBDY3    , &
                   get_nb_RBDY3                  , &
                   comp_mass_RBDY3               , &
                   comp_free_vlocy_RBDY3         , &
                   comp_Fext_RBDY3               , &
                   comp_Fint_RBDY3               , &
                   comp_dof_RBDY3                , &
                   update_dof_RBDY3              , &
                   clean_memory_RBDY3

  use POLYR, only : read_bodies_POLYR, &
                    clean_memory_POLYR

  use PRPRx, only: RUN_PRPRx                       , &
                   CHECK_PRPRx                     , &
                   read_ini_Vloc_Rloc_PRPRx        , &
                   write_xxx_Vloc_Rloc_PRPRx       , &
                   get_write_Vloc_Rloc_PRPRx       , &
                   compute_box_PRPRx               , &
                   coor_prediction_PRPRx           , &
                   creation_tab_visu_PRPRx         , &
                   wcp_compute_contact_PRPRx       , &
                   nc_compute_contact_PRPRx        , &
                   f2f4all_compute_contact_PRPRx   , &
                   wti_compute_contact_PRPRx       , &
                   sto_compute_contact_PRPRx       , &
                   set_cundall_iteration_PRPRx     , &
                   set_f2f_tol_PRPRx               , &
                   with_nodal_contact_PRPRx        , &
                   set_max_nb_pt_select_PRPRx      , &
                   sto_set_explicit_detection_PRPRx, &
                   sto_set_decompression_PRPRx     , &
                   set_size_factor_POLYR_PRPRx     , &
                   set_clipper_parameters          , &
                   clean_memory_PRPRx

  use inter_meca_handler_3D, only: stock_rloc_3D => stock_rloc, &
                                   recup_rloc_3D => recup_rloc

  use nlgs_3D, only : set_nlgs_3D_parameter         => set_nlgs_parameter        , &
                      assume_is_initialized_3D      => assume_is_initialized     , &
                      active_diagonal_resolution_3D => active_diagonal_resolution, &
                      prep_nlgs_3D                  => prep_nlgs                 , &
                      solve_nlgs_3D                 => solve_nlgs                , &
                      prep_check_nlgs_3D            => prep_check_nlgs           , &
                      comp_check_nlgs_3D            => comp_check_nlgs           , &
                      quick_scramble_nlgs_3D        => quick_scramble_nlgs       , &
                      RnodHRloc_nlgs_3D             => RnodHRloc_nlgs            , &
                      update_tact_behav_nlgs_3D     => update_tact_behav_nlgs    , &
                      nullify_entitylist_nlgs_3D    => nullify_entitylist_nlgs

  use postpro_3D, only : init_postpro_command_3D       => init_postpro_command      , &
                         start_postpro_3D              => start_postpro             , &
                         messages_for_users_3D         => messages_for_users        , &
                         postpro_during_computation_3D => postpro_during_computation, &
                         close_postpro_files_3D        => close_postpro_files

  implicit none

  ! set some data
  real(kind=8)      :: dt   = 1e-3

  integer           :: nb_steps = 100
  
  real(kind=8)      :: theta = 0.5
  
  real(kind=8)      :: tol = 1.666e-4
  real(kind=8)      :: relax = 0.1

  integer           :: gs_it1 = 50
  integer           :: gs_it2 = 500
  character(len=5)  :: norm = 'QM/16'
                                   !123456789012345678901234567890
  !character(len=30) :: solver_type='Stored_Delassus_Loops         '
  logical           :: SDLactif = .true.
  logical           :: with_quick_scramble = .true.

  integer            :: freq_detec = 1
  integer            :: freq_write = 1

  !
  integer            :: record  = 0
  integer            :: restart = 0
  integer            :: reset = 0

  logical :: is_detec_init, expl
  integer :: i_step, ifrom, ito, i, j, iconv, iter, postpro_unit
  integer :: cundall_it, nb_max_pt, lsap, detection_method
  real(kind=8) :: cds, ans, delta, f2f_tol, halo, decomp
  character(len=21) :: PRPRx_detection

  !
  lsap       = 10      ! Low Size Array Polyr
  PRPRx_detection = 'CpCundall'
  cundall_it = 200     ! for Cundall iteration
  cds        = 0.d0    ! cd shrink
  ans        = 0.d0    ! an shrink
  delta      = 0.d0    ! intersection simplification parameter for clipper
  ! not used here
  f2f_tol    = 1.d-3   ! for face to face
  halo       = 1.d-3   ! global distance for non convex
  nb_max_pt  = 6       ! for triangles intersection
  expl       = .false. ! explicit for STO detection
  decomp     = 0.d0    ! decompression for STO detection must be <1. and >-1.

  ! hope there is no need..
  !call init_MPI
  !call start_MPI_time

  !call initialize_utimer

  call active_diagonal_resolution_3D


  select case( trim(PRPRx_detection) )
  case( 'CpCundall' )
    detection_method = 2
    call set_cundall_iteration_PRPRx(cundall_it)
    call set_clipper_parameters(cds, ans, delta)
    call with_nodal_contact_PRPRx()
  case( 'CpF2fExplicit' )
    detection_method = 2
    call set_f2f_tol_PRPRx(f2f_tol)
    call set_clipper_parameters(cds, ans, delta)
  case( 'CpF2f' )
    detection_method = 2
    call set_f2f_tol_PRPRx(f2f_tol)
    CALL set_cundall_iteration_PRPRx(cundall_it)
    call set_clipper_parameters(cds, ans, delta)
  case( 'Nc' )
    detection_method = 3
  case( 'NcF2f' )
    detection_method = 3
    call set_f2f_tol_PRPRx(f2f_tol)
  case( 'NcF2fExplicit' )
    detection_method = 4
    call set_f2f_tol_PRPRx(f2f_tol)
  case( 'TrianglesIntersection' )
    detection_method = 6
    call set_max_nb_pt_select_PRPRx(nb_max_pt)
  case( 'STO' )
    detection_method      = 7
    call set_f2f_tol_PRPRx(f2f_tol)
    call STO_set_explicit_detection_PRPRx(expl)
    call STO_set_decompression_PRPRx(decomp)
  end select
  !optional
  !call STO_force_f2f_detection_PRPRx()
  !call set_f2f_tol_small_surface_PRPRx(small_tol)
  !call set_shrink_polyr_faces_PRPRx(shrink)

  !low size array polyr
  call set_size_factor_polyr_PRPRx(lsap)


  call init_dimension('3D        ')
  
  call set_time_step(dt)
  call init_theta_integrator(theta)
  
  call read_in_bulk_behav 
  call open_tact_behav_ll()
  call open_see_ll()
  call read_xxx_tact_behav(1)
  call close_tact_behav_ll()
  call close_see_ll()
  
  call read_in_bodies_RBDY3(1)
  call update_existing_entities_RBDY3
  call read_behaviours_RBDY3
  
  call read_in_dof_ol(record)
  call read_in_dof_RBDY3(record)
  call read_in_driven_dof_RBDY3
  call read_bodies_POLYR

  call Init_EntityList
  
  if( check_PRPRx() ) call read_ini_Vloc_Rloc_PRPRx(record)

  ! no write
  !io_hdf5_initOutFile( 'lmgc90.h5' )
  ! no display
  
  postpro_unit = init_postpro_command_3D()
  call start_postpro_3D(postpro_unit, restart)
  call messages_for_users_3D
  
  call comp_mass_RBDY3()
  
  do i_step = 1, nb_steps
  
    call Clean_writing_flags
    call time_increment
    call display_time
    call increment_RBDY3
    call comp_Fext_RBDY3
    call comp_Fint_RBDY3
    call comp_free_vlocy_RBDY3
  
    call Init_EntityList
    call set_run_contactor(freq_detec)

    if( check_PRPRx() ) then

      if( .not. is_detec_init ) then
       call compute_box_PRPRx
       is_detec_init = .true.
      end if

      call coor_prediction_PRPRx

      if( run_PRPRx() ) call creation_tab_visu_PRPRx

      select case(detection_method)
      case(2)
        call wcp_compute_contact_PRPRx
      case(3)
        call nc_compute_contact_PRPRx(halo)
      case(4)
        call f2f4all_compute_contact_PRPRx(halo)
      case(6)
        call wti_compute_contact_PRPRx
      case(7)
        call STO_compute_contact_PRPRx
      case default
        call faterr('prprx select prox tactor','Detection method unknown')
      end select

    end if

    !inter_handler_3D
    call recup_rloc_3D(i_prprx)

    call set_nlgs_3D_parameter(norm,tol,RELAX)
    call prep_nlgs_3D(SDLactif)
    iter = 0
    do i = 1, gs_it2
      if( with_quick_scramble ) call quick_scramble_nlgs_3D
      do j = 1, gs_it1
        iter = iter + 1
        call solve_nlgs_3D(1)
      end do
      call prep_check_nlgs_3D(iconv)
      !if (iconv == 0 ) stop 'prep_nlgs not converged'
      call solve_nlgs_3D(2)
      call comp_check_nlgs_3D(iconv)
      if (iconv == 0) exit
    end do
    call RnodHRloc_nlgs_3D
    call solve_nlgs_3D(3)
    call Nullify_EntityList_nlgs_3D

    call update_tact_behav_nlgs_3D
  
    !inter_handler_3D
    call stock_rloc_3D(i_prprx)
  
    call comp_dof_RBDY3
    call Updt_time_begin
    call update_dof_RBDY3

    ! no hdf5...
    !if TimeEvolution_GetStep()%freq == 0
    !  io_hdf5_write( )
    if( modulo(get_NSTEP(),freq_write) == 0 ) then
      call write_xxx_dof_ol(1)
      call write_xxx_Vloc_Rloc_ol(1)
    end if
    ifrom = 1
    ito   = get_nb_RBDY3()
    if (get_write_DOF_RBDY3())        call write_xxx_dof_RBDY3(1,ifrom,ito)
    if( get_write_Vloc_Rloc_PRPRx() ) call write_xxx_Vloc_Rloc_PRPRx(1)
    !WriteHDF5(nsteps)

    call postpro_during_computation_3D
  
  end do
  
  !call write_utimer     

  !nlgs 3d
  call assume_is_initialized_3D(0)

  is_detec_init = .false.

  !call stop_MPI_time
  !call mpi_finalize_process
  
  call clean_memory_bulk_behav
  call clean_memory_tact_behav
  
  call clean_memory_PRPRx

  call clean_memory_POLYR
  
  call clean_memory_RBDY3
  

end program
