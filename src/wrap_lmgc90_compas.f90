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
module wrap_lmgc90_compas

  use iso_c_binding

  use parameters, only : i_polyr, i_prprx, &
                         m2n => get_body_model_name_from_id    , &
                         c2n => get_contactor_name_from_id     , &
                         i2n => get_interaction_name_from_id   , &
                         s2n => get_contact_status_name_from_id

  use utilities, only : logmes, &
                        faterr, &
                        enable_logmes, &
                        disable_logmes

  use overall, only : max_internal_tact      , &
                      init_dimension         , &
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
                      get_NSTEP              , &
                      overall_clean_memory   ! resets entity registry, NSTEP, time, global flags


  use bulk_behaviour, only : set_nb_bulks        , &
                             add_one_bulk        , &
                             set_scalar_param    , &
                             set_gravity         , &
                             write_out_bulk_behav, &
                             clean_memory_bulk_behav => clean_memory

  use tact_behaviour, only : open_tact_behav_ll  , &
                             add_to_tact_behav_ll, &
                             close_tact_behav_ll , &
                             open_see_ll         , &
                             add_to_see_ll       , &
                             close_see_ll        , &
                             write_xxx_tact_behav, &
                             get_nb_internal     , &
                             get_tact_behav_name , &
                             clean_memory_tact_behav => clean_memory

  use RBDY3, only: increment_RBDY3               , &
                   set_nb_RBDY3                  , &
                   add_one_RBDY3                 , &
                   set_one_tactor_RBDY3          , &
                   set_blmty_RBDY3               , &
                   add_predef_driven_dof_RBDY3   , &
                   add_evol_driven_dof_RBDY3     , &
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
                   get_coor_RBDY3 => get_coor    , &
                   get_inertia_frame             , &
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

  use inter_meca_handler_3D, only: T_verlet, T_con, &
                                   get_nb_inters_3D  => get_nb_inters , &
                                   stock_rloc_3D     => stock_rloc    , &
                                   recup_rloc_3D     => recup_rloc    , &
                                   get_ptr_verlet_3D => get_ptr_verlet, &
                                   get_ptr_con_3D    => get_ptr_con   , &
                                   get_verlet_tact_lawnb, redo_nb_adj

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
                      nullify_entitylist_nlgs_3D    => nullify_entitylist_nlgs   , &
                      clean_memory_nlgs_3D          => clean_memory                ! solver `this` ledger + scratch arrays

  use postpro_3D, only : init_postpro_command_3D       => init_postpro_command      , &
                         start_postpro_3D              => start_postpro             , &
                         messages_for_users_3D         => messages_for_users        , &
                         postpro_during_computation_3D => postpro_during_computation, &
                         close_postpro_files_3D        => close_postpro_files       , &
                         clean_memory_postpro_3D                                       ! file unit handles, energy accumulators

  ! `models` is used transitively by RBDY3/POLYR through the bulk_behav/ppset
  ! chain; it owns its own `modelz`/`ppset` allocations that don't otherwise
  ! get cleaned. Aliased to avoid name clash with bulk/tact_behav clean_memory.
  use models, only : clean_memory_models => clean_memory

  implicit none

  private

  ! derived type to store everything
  ! regarding mechanical interaction in 3D
  type, bind(c) :: lmgc90_inter_meca_3D
    character(kind=c_char) ::  cdan(5)
    integer(kind=c_int)    :: icdan
    character(kind=c_char) :: cdbdy(5)
    integer(kind=c_int)    :: icdbdy
    character(kind=c_char) :: anbdy(5)
    integer(kind=c_int)    :: ianbdy
    character(kind=c_char) :: cdtac(5)
    integer(kind=c_int)    :: icdtac
    character(kind=c_char) :: antac(5)
    integer(kind=c_int)    :: iantac
    integer(kind=c_int)    :: icdsci
    integer(kind=c_int)    :: iansci
    character(kind=c_char) :: behav(5)
    character(kind=c_char) :: status(5)
    real(kind=c_double)    :: coor(3)
    real(kind=c_double)    :: uc(9)
    real(kind=c_double)    :: rloc(3)
    real(kind=c_double)    :: vloc(3)
    real(kind=c_double)    :: gap
    integer(kind=c_int)    :: nb_int
    real(kind=c_double)    :: internals(max_internal_tact)
  end type lmgc90_inter_meca_3D

  type, bind(c) :: lmgc90_rigid_body_3D
    real(kind=c_double) :: coor(3)
    real(kind=c_double) :: frame(9)
  end type lmgc90_rigid_body_3D
  ! data

  ! contact solver solver
  real(kind=8)      :: tol = 1.666e-4
  real(kind=8)      :: relax = 0.1
  integer           :: gs_it1 = 50
  integer           :: gs_it2 = 500
  character(len=5)  :: norm = 'QM/16'
  logical           :: SDLactif = .true.
  logical           :: with_quick_scramble = .false.
  ! I/O
  integer            :: freq_write = 1
  integer            :: record  = 0
  integer            :: restart = 0
  integer            :: reset = 0
  ! detection
  integer            :: lsap       = 10      ! Low Size Array Polyr
  character(len=21)  :: PRPRx_detection = 'STO'
  !character(len=21)  :: PRPRx_detection = 'CpCundall'  
  !character(len=21)  :: PRPRx_detection = 'CpF2f'
  
  integer            :: cundall_it = 200     ! for Cundall iteration
  real(kind=8)       :: cds        = 0.d0    ! cd shrink
  real(kind=8)       :: ans        = 0.d0    ! an shrink
  real(kind=8)       :: delta      = 0.d0    ! intersection simplification parameter for clipper
  ! still detection, but not used yet
  real(kind=8)       :: f2f_tol    = 1.d-3   ! for face to face
  real(kind=8)       :: halo       = 1.d-3   ! global distance for non convex
  integer            :: nb_max_pt  = 6       ! for triangles intersection
  ! STO detection
  logical            :: expl       = .true.  ! explicit
  real(kind=8)       :: decomp     = -1.      ! decompression for STO detection must be <1. and >-1.
  ! internal parameter for detection
  integer            :: freq_detec = 1
  logical            :: is_detec_init = .false.
  ! this is set depending on PRPRx_detection 
  integer            :: detection_method = -99

  ! output for debug purpose
  logical :: debug = .false.

  public initialize      , &
         compute_one_step, &
         finalize

contains

  !> a short utility to convert char(len=5) to char(len=ch_char), dimension(5)
  !> hopefully inlined and optimized by compilers...
  function string_f2c_(fromF)
    implicit none
    character(len=5), intent(in) :: fromF
    character(len=c_char), dimension(5) :: string_f2c_
    !
    integer :: i
    do i = 1, 5
      string_f2c_(i) = fromF(i:i)
    end do

  end function

  ! ------------------------------------------------- !
  ! first : some simple functions to run a simulation !
  ! ------------------------------------------------- !

  !> Tear down every LMGC90 module state we touch, in reverse-dependency
  !> order (consumers first, providers last). Used both at finalize-time
  !> and defensively at the START of initialize, so even when a caller
  !> drops the previous Solver instance without explicit finalize (the
  !> usual Python/Rhino case where the destructor timing is at the
  !> garbage collector's discretion), the next initialize starts on a
  !> truly clean slate.
  !>
  !> Order rationale:
  !>   1. PRPRx — contact ledger references POLYR + RBDY3 entities.
  !>   2. nlgs_3D — its `this` array points at PRPRx entries; clean
  !>      AFTER PRPRx so we don't briefly hold dangling refs, and the
  !>      deallocate sequence is always upstream-then-downstream-clean.
  !>   3. POLYR — contactor geometry that lived on RBDY3 bodies.
  !>   4. RBDY3 — the bodies themselves.
  !>   5. tact_behav / bulk_behav — registered behaviors and bulks.
  !>   6. models — modelz/ppset arrays from the materials chain.
  !>   7. postpro_3D — file unit handles (no-op if postpro never started).
  !>   8. overall — entity registry, NSTEP, time, every global flag.
  !>      Last because every module above can register entities into it.
  subroutine reset_all_state()
    implicit none

    ! Wrapper-local state first.
    call assume_is_initialized_3D(0)
    is_detec_init    = .false.
    detection_method = 0

    ! LMGC90 module state.
    call clean_memory_PRPRx
    call clean_memory_nlgs_3D
    call clean_memory_POLYR
    call clean_memory_RBDY3
    call clean_memory_tact_behav
    call clean_memory_bulk_behav
    call clean_memory_models
    call clean_memory_postpro_3D
    call overall_clean_memory

  end subroutine reset_all_state

  subroutine initialize(dt, theta, with_log) bind(c, name='lmgc90_initialize')
    implicit none
    real(kind=8)        , intent(in), value :: dt, theta
    logical(kind=c_bool), intent(in), value :: with_log

    ! Defensive: if a previous Solver instance left state in this process
    ! (Python `__del__` may run late, or not at all on interpreter exit),
    ! reset everything before our init paths run. This is what makes
    ! `Solver(...)` re-instantiation safe in long-lived processes like
    ! Rhino's ScriptEditor — the user never has to call finalize().
    call reset_all_state()

    debug = with_log
    if( debug ) then
      call enable_logmes()
    else
      call disable_logmes()
    end if

    call active_diagonal_resolution_3D()

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

    call set_time_step(dt, .false.)
    call init_theta_integrator(theta, .true.)

    call open_tact_behav_ll()

  end subroutine initialize

  subroutine set_materials(nb, c_densities) bind(c, name='lmgc90_set_materials')
    implicit none
    integer(c_int), intent(in), value :: nb
    type(c_ptr)   , intent(in), value :: c_densities
    !!
    integer :: i_mat, ret
    character(len=30) :: mat_type
    character(len=5)  :: material
    !character(len=5), dimension(:), pointer :: materials
    real(c_double)  , dimension(:), pointer :: densities

    mat_type = 'RIGID'

    ! Convert C pointer to Fortran array
    call c_f_pointer(cptr=c_densities, fptr=densities, shape=(/nb/))
    !call c_f_pointer(cptr=c_names    , fptr=materials, shape=(/nb/))

    call set_gravity( (/0.d0, 0.d0, -9.81d0/) )
    ! LMGC90 uses single material type, so we use the first density
    ! TODO: Support multiple material types when LMGC90 allows per-body materials
    call set_nb_bulks( nb )
    do i_mat = 1, nb
      write(material, '(A1,I4.4)') 's', i_mat
      ret = add_one_bulk( material, mat_type )
      call set_scalar_param(ret, 'density', densities(i_mat))
    end do

  end subroutine set_materials

  subroutine add_one_tact_behav(c_name, c_law, nb_p, c_params) bind(c, name='lmgc90_add_one_tact_behav')
    implicit none
    type(c_ptr)   , intent(in), value :: c_name !char[5]
    type(c_ptr)   , intent(in), value :: c_law  !char[30]
    integer(c_int), intent(in), value :: nb_p
    type(c_ptr)   , intent(in), value :: c_params
    !!
    integer :: i_behav
    character(len=5) , pointer :: nickname
    character(len=30), pointer :: law
    real(kind=8)     , dimension(:), pointer :: params

    call c_f_pointer( cptr=c_name  , fptr=nickname)
    call c_f_pointer( cptr=c_law   , fptr=law     )
    call c_f_pointer( cptr=c_params, fptr=params, shape=(/nb_p/))

    ! so ugly and dumb...
    do i_behav = 1, 30
      if(  law(i_behav:i_behav) == c_null_char ) then
        law(i_behav:) = " "
        exit
      end if
    end do
    call add_to_tact_behav_ll(law, nickname, params)

    nickname => null()
    law      => null()
    params   => null()

  end subroutine add_one_tact_behav

  subroutine set_see_tables(alert) bind(c, name='lmgc90_set_see_tables')!cd, an, behav, alert, halo)
    implicit none
    real(c_double), intent(in), value :: alert

    call close_tact_behav_ll()

    call open_see_ll()
    call add_to_see_ll('RBDY3', 'POLYR', 'REDxx', 'iqsc0',     &
                       'RBDY3', 'POLYR', 'REDxx',  alert , 0.d0 )
    call close_see_ll()

  end subroutine set_see_tables

  subroutine set_nb_bodies(nb) bind(c, name='lmgc90_set_nb_bodies')
    implicit none
    integer(c_int), intent(in), value :: nb

    call set_nb_RBDY3( nb )

  end subroutine

  subroutine set_one_polyr(c_behav, coor, c_connec, nb_tri, c_vertices, nb_v, nb_v_ddof, nb_f_ddof) &
               bind(c, name='lmgc90_set_one_polyr')
    implicit none
    type(c_ptr), intent(in), value :: c_behav
    real(kind=c_double), dimension(3), intent(in) :: coor
    type(c_ptr)                , value :: c_connec
    integer(c_int) , intent(in), value :: nb_tri
    type(c_ptr)                , value :: c_vertices
    integer(c_int) , intent(in), value :: nb_v
    integer(c_int) , intent(in), value :: nb_v_ddof
    integer(c_int) , intent(in), value :: nb_f_ddof
    !
    integer     , dimension(:), pointer :: connec
    real(kind=8), dimension(:), pointer :: vertices
    !
    integer     , dimension(:), pointer :: idata
    real(kind=8), dimension(:), pointer :: rdata
    real(kind=8), dimension(3,3)        :: frame      = 0.d0
    real(kind=8), dimension(3)          :: inertia    = 0.d0
    real(kind=8), dimension(3)          :: shift      = 0.d0
    real(kind=8), dimension(6)          :: drv_values = 0.d0
    real(kind=8) :: vol = 0.d0
    character(len=5) :: color
    character(len=5), pointer :: behav
    integer :: i_bdyty, i_dof

    call c_f_pointer( cptr=c_connec  , fptr=connec  , shape=(/3*nb_tri/) )
    call c_f_pointer( cptr=c_vertices, fptr=vertices, shape=(/3*nb_v/) )
    call c_f_pointer( cptr=c_behav, fptr=behav )

    color = 'REDxx'

    ! always only 1 contactor
    call add_one_RBDY3(coor, 1, nb_v_ddof, nb_f_ddof)
    i_bdyty = get_nb_RBDY3()

    ! now set contactor:
    allocate( idata(2 + 3*nb_tri) )
    idata(1) = nb_v
    idata(2) = nb_tri
    idata(3:) = connec(:)
    rdata => vertices
    call set_one_tactor_RBDY3(i_bdyty, 1, i_polyr, color, vol, inertia, &
                              frame, shift, idata, rdata                )

    ! finally info of center of inertia
    call set_blmty_RBDY3(i_bdyty, behav, vol, inertia, frame)

  end subroutine

  subroutine set_drvdof(i_bdyty, i_dof, drv_values, drv_size, velocity, evolution) &
               bind(c, name='lmgc90_set_drvdof')
    implicit none
    integer(c_int) , intent(in), value :: i_bdyty
    integer(c_int) , intent(in), value :: i_dof
    type(c_ptr)    , intent(in), value :: drv_values
    integer(c_int) , intent(in), value :: drv_size
    logical(c_bool), intent(in), value :: velocity
    logical(c_bool), intent(in), value :: evolution
    !
    integer, parameter :: lk = kind(.true.)
    real(kind=8), dimension(:)  , pointer :: predef_values
    real(kind=8), dimension(:,:), pointer :: evol_values
    if( evolution ) then
      call c_f_pointer(cptr=drv_values, fptr=evol_values, shape=(/drv_size/2,2/))
      call add_evol_driven_dof_RBDY3(i_bdyty, logical(velocity,lk), i_dof, evol_values)
    else
      call c_f_pointer(cptr=drv_values, fptr=predef_values, shape=(/drv_size/))
      call add_predef_driven_dof_RBDY3(i_bdyty, logical(velocity,lk), i_dof, predef_values)
    end if

  end subroutine set_drvdof

  subroutine close_before_computing( ) bind(c, name='lmgc90_close_before_computing')
    implicit none

    integer :: postpro_unit

    call update_existing_entities_RBDY3
    call read_behaviours_RBDY3

    call read_bodies_POLYR

    call Init_EntityList

    call redo_nb_adj(i_prprx)
    call stock_rloc_3D(i_prprx)

    !! write out
    if( debug ) then
      call write_out_bulk_behav()
      call write_xxx_tact_behav(2)

      call write_out_bodies_ol()
      call write_out_bodies_RBDY3(1)

      call write_out_driven_dof_ol()
      call write_out_driven_dof_RBDY3()

      ! write out first *.ini as *.out.1
      call write_xxx_dof_ol(1)
      call write_xxx_dof_RBDY3(1,1,get_nb_RBDY3())
      call write_xxx_vloc_rloc_ol(1)
      call write_xxx_vloc_rloc_PRPRx(1)
    end if

    ! no write
    !io_hdf5_initOutFile( 'lmgc90.h5' )
    ! no display

    !!postpro_unit = init_postpro_command_3D()
    !call start_postpro_3D(postpro_unit, restart)
    !call messages_for_users_3D

    call comp_mass_RBDY3()

  end subroutine close_before_computing

  subroutine compute_one_step() bind(c, name='lmgc90_compute_one_step')
    implicit none
    integer :: ifrom, ito, i, j, iconv, iter

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

    !no write at all
    if( debug ) then
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
    end if

    !call postpro_during_computation_3D

  end subroutine compute_one_step

  subroutine finalize() bind(c, name='lmgc90_finalize')
    implicit none

    call reset_all_state()

  end subroutine finalize

  ! ------------------------------------------------------ !
  ! second : some accessors to get interactions and bodies !
  ! ------------------------------------------------------ !

  integer(c_int) function get_nb_inters() bind(c, name='lmgc90_get_nb_inters')
    implicit none
    ! should sum on all interaction type
    get_nb_inters = get_nb_inters_3D(i_prprx)
  end function get_nb_inters

  subroutine get_all_inters(inter_array, size_array) bind(c, name='lmgc90_get_all_inters')
    implicit none

    type(c_ptr)        , intent(in), value :: inter_array
    integer(kind=c_int), intent(in), value :: size_array
    !
    integer :: nb_inter, i_inter, i_cdtac, i_adj, i_law
    type(lmgc90_inter_meca_3D), dimension(:), pointer :: inter
    type(T_verlet)            , dimension(:), pointer :: verlet
    type(T_con)                             , pointer :: con

    ! this is important... becaus it calls 'select_this_' inside the handler
    nb_inter = get_nb_inters_3D(i_prprx)
    if( nb_inter /=  size_array ) then
      call logmes('[ERROR::get_all_inters] Wrong size of input array', .true.)
      return
    end if

    ! just in case
    if( nb_inter < 1 ) return

    ! C to Fortran
    call c_f_pointer(cptr=inter_array, fptr=inter, shape=(/nb_inter/))

    verlet => get_ptr_verlet_3D( i_prprx )
    con    => get_ptr_con_3D( i_prprx )

    ! really paranoid...
    if( .not. associated(verlet) ) then
      call logmes('[ERROR::get_all_inters] No verlet interactions found', .true.)
      return
    end if

    ! running throug data structure to copy value to C memory
    do i_cdtac = 1, size(verlet)

      if (verlet(i_cdtac)%adjsz == 0) cycle

      do i_adj = 1, verlet(i_cdtac)%adjsz

        i_law = get_verlet_tact_lawnb(i_prprx, i_cdtac, i_adj)

        ! no need to manage a global numbering... yet !
        i_inter = verlet(i_cdtac)%icdan(i_adj)

        ! starting to copy... first inter numbering info
        inter(i_inter)%icdan       = i_inter
        inter(i_inter)%cdan(1:5)   = string_f2c_( i2n(i_prprx) )

        ! getting info relative to cd/an
        inter(i_inter)%cdbdy(1:5)  = string_f2c_( m2n( verlet(i_cdtac)%cdmodel ) )
        inter(i_inter)%icdbdy      = verlet(i_cdtac)%cdbdy
        inter(i_inter)%anbdy(1:5)  = string_f2c_( m2n( verlet(i_cdtac)%anmodel(i_adj) ) )
        inter(i_inter)%ianbdy      = verlet(i_cdtac)%anbdy(i_adj)

        inter(i_inter)%cdtac(1:5)  = string_f2c_( c2n( con%id_cdtac ) )
        inter(i_inter)%icdtac      = verlet(i_cdtac)%cdtac

        inter(i_inter)%antac(1:5)  = string_f2c_( c2n( con%id_antac ) )
        inter(i_inter)%iantac      = verlet(i_cdtac)%antac(i_adj)

        inter(i_inter)%icdsci      = verlet(i_cdtac)%cdsci(i_adj)
        inter(i_inter)%iansci      = verlet(i_cdtac)%ansci(i_adj)

        ! contact law and status info
        inter(i_inter)%behav(1:5)  = string_f2c_( get_tact_behav_name(i_law) )
        inter(i_inter)%status(1:5) = string_f2c_( s2n( verlet(i_cdtac)%status(i_adj) ) )

        ! all real data
        inter(i_inter)%coor(1:3) = verlet(i_cdtac)%coor(1:3,i_adj)
        inter(i_inter)%uc(1:3)   = verlet(i_cdtac)%tuc(1:3,i_adj)
        inter(i_inter)%uc(4:6)   = verlet(i_cdtac)%nuc(1:3,i_adj)
        inter(i_inter)%uc(7:9)   = verlet(i_cdtac)%suc(1:3,i_adj)
        inter(i_inter)%rloc(1)   = verlet(i_cdtac)%rlt(i_adj)
        inter(i_inter)%rloc(2)   = verlet(i_cdtac)%rln(i_adj)
        inter(i_inter)%rloc(3)   = verlet(i_cdtac)%rls(i_adj)
        inter(i_inter)%vloc(1)   = verlet(i_cdtac)%vlt(i_adj)
        inter(i_inter)%vloc(2)   = verlet(i_cdtac)%vln(i_adj)
        inter(i_inter)%vloc(3)   = verlet(i_cdtac)%vls(i_adj)
        inter(i_inter)%gap       = verlet(i_cdtac)%gapTT(i_adj)
        inter(i_inter)%nb_int    = get_nb_internal(i_law)
        inter(i_inter)%internals(1:max_internal_tact) = verlet(i_cdtac)%internal(1:max_internal_tact,i_adj)

      end do

    end do

    verlet => null()
    con    => null()

  end subroutine

  integer(c_int) function get_nb_bodies() bind(c, name='lmgc90_get_nb_bodies')
    implicit none
    get_nb_bodies = get_nb_RBDY3()
  end function get_nb_bodies

  subroutine get_all_bodies(bodies_array, size_array) bind(c, name='lmgc90_get_all_bodies')
    implicit none
    !
    type(c_ptr)        , intent(in), value :: bodies_array
    integer(kind=c_int), intent(in), value :: size_array
    !
    integer :: nb_rbdy3, i_bdyty
    type(lmgc90_rigid_body_3D), dimension(:), pointer :: bodies

    nb_rbdy3 = get_nb_RBDY3()
    if( nb_rbdy3 /=  size_array ) then
      call logmes('[ERROR::get_all_bodies] Wrong size of input array', .true.)
      return
    end if

    ! just in case
    if( nb_rbdy3 < 1 ) return

    ! C to Fortran
    call c_f_pointer(cptr=bodies_array, fptr=bodies, shape=(/nb_rbdy3/))

    do i_bdyty = 1, nb_rbdy3
      ! the second parameter is to get the coordinates of a contactor
      ! and not the coordinates of the center of inertia
      bodies(i_bdyty)%coor(1:3)  = get_coor_RBDY3(i_bdyty, 0)
      !print *, i_bdyty, bodies(i_bdyty)%coor(1:3)
      bodies(i_bdyty)%frame(1:9) = reshape( get_inertia_frame(i_bdyty), shape=(/9/) )
    end do

  end subroutine get_all_bodies

end module wrap_lmgc90_compas
