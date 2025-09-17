PROGRAM handmadecomputation

  USE utilities 

  USE RBDY2, ONLY: &
       increment_RBDY2, &
       comp_dof_RBDY2, &
       update_dof_RBDY2, &
       comp_free_vlocy_RBDY2, &
       comp_Fext_RBDY2, &
       comp_Fint_RBDY2, &
       check_equilibrium_state_RBDY2, &
       ghost2invisible_RBDY2, &
       check_source_point_RBDY2, &
       out_of_bounds_RBDY2, &
       fatal_damping_RBDY2, &
       partial_damping_RBDY2, &
       init_construct_wall, &
       construct_wall, &
       read_in_bodies_RBDY2, &
       update_existing_entities_RBDY2, &
       read_in_dof_RBDY2, &
       read_in_driven_dof_RBDY2, &
       read_behaviours_RBDY2, &
       write_out_bodies_RBDY2, &
       write_out_cleared_bodies_RBDY2, &
       write_xxx_dof_RBDY2, &
       write_xxx_Rnod_RBDY2, &
       write_out_driven_dof_RBDY2, &
       comp_mass_RBDY2, &
       read_mp_behaviours_rbdy2, &
       set_periodic_data_RBDY2, &
       resize_RBDY2, &
       nullify_X_dof_RBDY2, &
       nullify_V_dof_RBDY2, &
       init_source_point_RBDY2, &
       set_init_boundary_RBDY2, &
       set_data_equilibrium_RBDY2, &
       add_dof2bodies_RBDY2, &
       get_nb_RBDY2, &
       get_write_Rnod_RBDY2, &
       get_write_DOF_RBDY2, &
!!$       read_extra_behaviours_rbdy2,&
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2, &
       get_area ! <- am: debut des fonctions supplementaires

  USE bulk_behaviour

  USE tact_behaviour

  USE diskx,ONLY:&
      read_bodies_DISKx, &
      get_nb_DISKx, &
      get_DISKx2RBDY2, & ! <- am: debut des fonctions supplementaires
      get_radius_DISKx, &
      get_mean_radius_DISKx

  USE JONCx,ONLY:&
       read_bodies_JONCx

  USE overall, ONLY: &
      Write_out_bodies_Ol, &
      Clean_out_bodies,&
      Clean_in_bodies, &
      Write_out_driven_dof_Ol, &
      Read_in_dof_Ol, &
      Write_xxx_dof_Ol, &
      Read_in_Vloc_Rloc_Ol, &
      Write_xxx_Vloc_Rloc_Ol,&
      Read_in_gpv_Ol, &
      Write_xxx_gpv_Ol,&
      Display_prox_tactors_Ol,&
      Write_xxx_Rnod_Ol,&
      Clean_entitylist, &
      time_increment, & !!      Updt_time, &
      Updt_time_begin, & 
      Set_newton_tolerance, &
      Set_newton_maxloop, &
      Set_newton_badloop, &
      Set_newton_goodloop, &
      Set_newton_rate_step, & 
      Set_newton_loop, &
      Incre_newton_loop, &
      Comp_NR_time_step, &
      DISPLAY_TIME, &
      Clean_writing_flags, &
      Get_NSTEP, &
      get_time, &
      Init_dimension,&
      Set_time_step, &
      Init_gear_integrator, &
      Init_theta_integrator, &
      init_verlet_integrator, &
      Init_CN_integrator, &
      Set_final_time, &
      Set_min_time_step, &
      Set_max_time_step, &
      Acti_large_computation, &
      set_run_contactor, &
      init_post_data_ol, &
      update_post_data_ol, &
      write_out_gmv_Ol, &
      write_out_MP_values_Ol, &
      set_working_directory, &
      Init_EntityList, &
      Clean_writing_flags, &
      set_with_experimental_dev

  USE DKDKx,ONLY: &
       get_nb_DKDKx,&
       get_nb_recup_DKDKx, &
       stock_rloc_DKDKx, &
       recup_rloc_DKDKx, &
       smooth_computation_DKDKx, &
       compute_box_DKDKx, &
       read_ini_Vloc_Rloc_DKDKx, &
       write_xxx_Vloc_Rloc_DKDKx, &
       set_periodic_data_DKDKx, &
       coor_prediction_DKDKx, &
       creation_tab_visu_DKDKx, &
       compute_contact_DKDKx, &
       display_prox_tactors_DKDKx, &
       RUN_DKDKx, &
       CHECK_DKDKx, &
       get_write_Vloc_Rloc_DKDKx

  USE post2D,ONLY: &
       init_GMV_post2D, &
       update_post2D, &
       write_GMV_post2D, &
       set_ref_reac_max_post2D, &
       set_displ_ampl_post2D, &
       set_ref_radius_post2D, &
       set_periodic_data_post2D, &
       set_extra_material_post2D, &
       active_GMV,&
       put_material_ID_post2D, &
       GMV_circular_selection, &
       get_write_gmv_post2D

  USE nlgs,ONLY:&
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       bimodal_list_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       display_rlocn_sum_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs

  USE MP_SOLVER,ONLY:&
       read_in_mp_behaviour_mp_solver, &
       write_out_mp_behaviour_mp_solver, &
       read_ini_mp_values_mp_solver, &
       write_out_mp_values_mp_solver, &
       solve_electro1G, &
       solve_nl_electro1G, &
       solve_thermo_mp_solver, &
       get_write_mp_values, &
       update_compuctivity_mp_solver, &
       update_thermo_mp_solver, &
       active_recup, &
       init_mp_solver

  USE POSTPRO,ONLY:&
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &  
       postpro_during_computation, &
       circular_selection_postpro, &
       selection_translation_postpro

  USE timer, ONLY: &
       initialize_utimer, &
       write_utimer, &
       get_new_utimer_ID, &
       start_utimer,stop_utimer 

  IMPLICIT NONE

  INTEGER :: i,ib,ik,iconv,iter
  INTEGER :: id_read,id_free,id_nlgs_prep,id_nlgs_iter, &
             id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_updt,id_post
  integer :: postpro_unit
  INTEGER :: id_mp_th

  INTEGER          :: ERR,GMVfreq
  REAL(kind=8)     :: TimeStep,Theta,RefRadius,MPvar,Periode
  INTEGER          :: iTimeLoop,itloop1,itloop2
  REAL(kind=8)     :: TOL,RELAX
  CHARACTER(len=5) :: CVTYPE

!!!!!!!!!!!!!!!!

  CALL set_with_experimental_dev

  CALL initialize_utimer
  !12345678901234567890
  id_read       = get_new_utimer_ID('READ                ')
  id_free       = get_new_utimer_ID('INIT                ')
  id_nlgs_prep  = get_new_utimer_ID('NLGS PREP           ')
  id_nlgs_iter  = get_new_utimer_ID('NLGS ITER           ')
  id_nlgs_post1 = get_new_utimer_ID('NLGS POST 1         ')
  id_nlgs_post2 = get_new_utimer_ID('NLGS POST 2         ')
  id_nlgs_post3 = get_new_utimer_ID('NLGS POST 3         ')
  id_select     = get_new_utimer_ID('Select Prox Tactors ')
  id_updt       = get_new_utimer_ID('Update              ')
  id_post       = get_new_utimer_ID('POST                ')
  id_mp_th      = get_new_utimer_ID('MP THERMAL          ')
!!!!!!!

  ERR = 0

  OPEN(UNIT=1,FILE='standalone.in',STATUS='OLD',IOSTAT=ERR)

  IF (ERR.NE.0) THEN
     WRITE(*,*) 'file standalone.in is missing'
     STOP
  END IF

  READ(1,*) TimeStep
  READ(1,*) Theta
  READ(1,*) RefRadius
  READ(1,*) MPvar
  READ(1,*) Periode
  READ(1,*) GMVfreq

  READ(1,*) iTimeLoop
  READ(1,*) CVTYPE,TOL
  READ(1,*) RELAX
  READ(1,*) itloop1,itloop2

  WRITE(*,*) '! TIME STEP            : ',TimeStep
  WRITE(*,*) '! THETA                : ',Theta
  WRITE(*,*) '! REFERENCE RADIUS     : ',RefRadius
  WRITE(*,*) '! READ MP BEHAVIOUR    : ',MPvar
  WRITE(*,*) '! PERIODIC CONDITION   : ',Periode
  WRITE(*,*) '! WRITE OUTPUT GMV STEP: ',GMVfreq
  WRITE(*,*) '! loading step         : ',iTimeLoop
  WRITE(*,*) '! NLGS CHECK TYPE      : ',CVTYPE,TOL
  WRITE(*,*) '! RELAX                : ',RELAX
  WRITE(*,*) '! iteration & more     : ',itloop1,itloop2

  CLOSE(1)

! </read
  CALL start_utimer(id_read)
  
  CALL read_in_bodies_RBDY2
  CALL update_existing_entities_RBDY2
  CALL read_in_bulk_behav 
  CALL read_xxx_tact_behav(1)
  CALL read_bodies_DISKx
  CALL read_bodies_JONCx
  CALL init_entitylist
  CALL read_behaviours_RBDY2   
  
  CALL read_mp_behaviours_RBDY2(MPvar)
  CALL init_mp_solver
  CALL read_in_mp_behaviour_mp_solver
  CALL active_recup('T')
  
  CALL read_in_dof_ol(0)
  CALL read_in_dof_RBDY2(0)
  CALL read_in_Vloc_Rloc_ol(0)
  CALL read_ini_Vloc_Rloc_DKDKx(0)
  CALL read_in_driven_dof_RBDY2

  CALL set_periodic_data_RBDY2(Periode)
  CALL set_periodic_data_DKDKx(Periode,.TRUE.)
  CALL set_periodic_data_post2D(Periode,.TRUE.)

  CALL set_time_step(TimeStep)        
  CALL init_theta_integrator(Theta)       
  
  CALL set_ref_radius_post2D(RefRadius)
  
  CALL active_GMV('STRSS')
  CALL active_GMV('DISPL')
  CALL active_GMV('VELOC')
  CALL active_GMV('TCTRS')
  CALL active_GMV('INTER')
  CALL active_GMV('HEAT_')
  CALL active_GMV('WEAR_')
  CALL init_GMV_post2D
  
  postpro_unit = init_postpro_command()
  call start_postpro(postpro_unit, 0)
  CALL messages_for_users
  
  CALL compute_box_DKDKx
  CALL Init_entitylist
  CALL comp_mass_RBDY2
  
  CALL stop_utimer(id_read)
  ! read/>
  
  DO i=1,iTimeLoop
     ! </free
     PRINT*,'iter',i
     CALL start_utimer(id_free)
     
     CALL  time_increment                  !Updt_time
     !         CALL  Set_newton_loop(0)
     CALL increment_RBDY2
     CALL comp_Fext_RBDY2
     CALL comp_Fint_RBDY2
     
     CALL comp_free_vlocy_RBDY2
     CALL stop_utimer(id_free)
     ! free/>
!!!!!
     ! </detect
     CALL start_utimer(id_select)
     CALL Clean_EntityList
     CALL set_run_contactor(1)
     
     CALL coor_prediction_DKDKx
     IF (RUN_DKDKx()) THEN
        CALL creation_tab_visu_DKDKx
        !            WRITE(*,'(1X,I10,A20)') get_nb_DKDKx(),' DKDKx roughly found'       
     END IF
     
     CALL compute_contact_DKDKx
     !         WRITE(*,'(1X,I10,A12)') get_nb_DKDKx(),' DKDKx found'       
     
     CALL stop_utimer(id_select)
     ! detect />
!!!! 
     !</ nlgs
     CALL start_utimer(id_nlgs_prep)
     
     CALL recup_rloc_DKDKx
     !         WRITE(*,'(1X,I10,A12)') get_nb_recup_DKDKx(),' recup DKDKx'
     
     CALL set_nlgs_parameter(CVTYPE,TOL,RELAX)
!ELG     CALL prep_nlgs(.FALSE.)
     CALL prep_nlgs(.TRUE.)
     
     CALL stop_utimer(id_nlgs_prep)
     CALL start_utimer(id_nlgs_iter)

     iter = 0

     DO ib=1,itloop2
        DO ik=1,itloop1
           iter = iter + 1
           CALL solve_nlgs(1)
        END DO
        CALL prep_check_nlgs(iconv)
        IF (iconv == 0 ) EXIT
        CALL solve_nlgs(2)
        CALL comp_check_nlgs(iconv)
        IF (iconv == 0) EXIT
     END DO
     CALL stop_utimer(id_nlgs_iter)
     
     CALL start_utimer(id_nlgs_post1)
     
     CALL RnodHRloc_nlgs
     
     CALL stop_utimer(id_nlgs_post1)
     CALL start_utimer(id_nlgs_post2)
     
     CALL solve_nlgs(3)
     
     CALL Nullify_EntityList_nlgs
     
     CALL stop_utimer(id_nlgs_post2)
     
     CALL start_utimer(id_nlgs_post3)
     
     CALL stock_rloc_DKDKx 
     
     CALL stop_utimer(id_nlgs_post3)
     ! nlgs />
     
     ! </mpth
     CALL start_utimer(id_mp_th)
     
     CALL solve_thermo_mp_solver
     
     CALL stop_utimer(id_mp_th)
     ! mpth />
     
     ! </updt
     CALL start_utimer(id_updt)
     
     CALL comp_dof_RBDY2

     CALL Updt_time_begin
     CALL update_dof_RBDY2
     
     CALL stop_utimer(id_updt)
     ! updt />
     
     ! </post
     CALL start_utimer(id_post)
     
     IF (MODULO(get_NSTEP(),GMVfreq) == 0) THEN 
        CALL write_out_gmv_Ol
        CALL update_post2D(0)
        CALL write_GMV_post2D
     ENDIF
     
     CALL postpro_during_computation
     
     CALL stop_utimer(id_post)
     call clean_writing_flags
     ! post />
     
  ENDDO
  
  CALL close_postpro_files
  CALL write_utimer
  
END PROGRAM handmadecomputation
