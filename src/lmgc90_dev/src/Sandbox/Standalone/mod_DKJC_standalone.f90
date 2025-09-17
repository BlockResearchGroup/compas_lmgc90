MODULE LMGC_C_INTERFACE

  USE ISO_C_BINDING

  USE UTILITIES 

  USE RBDY2, only: &
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

  USE DKJCx,ONLY: &
       get_nb_DKJCx, &
       get_nb_recup_DKJCx, &
       stock_rloc_DKJCx, &
       recup_rloc_DKJCx, &
       smooth_computation_DKJCx, &
       compute_box_DKJCx, &
       read_ini_Vloc_Rloc_DKJCx, &
       write_xxx_Vloc_Rloc_DKJCx, &
       display_prox_tactors_DKJCx, &
       coor_prediction_DKJCx, &
       creation_tab_visu_DKJCx, &
       compute_contact_DKJCx, &
       RUN_DKJCx, &
       CHECK_DKJCx, &
       get_write_Vloc_Rloc_DKJCx

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

  USE POSTPRO,ONLY:&
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &  
       postpro_during_computation, &
       circular_selection_postpro, &
       selection_translation_postpro


  IMPLICIT NONE

  integer :: i,ib,ik,iconv,iter
  integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_nlgs_check
  integer :: id_nlgs_post1,id_nlgs_post2,id_nlgs_post3, id_select,id_updt,id_post
  integer :: postpro_unit
  
  ! Simulation parameters
  real(kind=8) :: HH,TT,tol,RELAX=1.d0
  integer      :: nb_time_steps,freq_detec,gs_it1, gs_it2,freq_gmv,freq_vlocrloc


  CONTAINS


  SUBROUTINE set_simulation_parameters(dt, nb_dt, theta, freq1, eps, gs1, gs2, freq2, freq3) BIND(C)

    real (C_DOUBLE) :: dt, theta, eps
    integer (C_INT) :: nb_dt, gs1, gs2, freq1, freq2, freq3

    HH = dt
    nb_time_steps = nb_dt
    TT = theta
    freq_detec = freq1
    tol = eps
    gs_it1 = gs1
    gs_it2 = gs2
    freq_gmv = freq2
    freq_vlocrloc = freq3

  END SUBROUTINE

!!!!!!!
!       call set_with_experimental_dev
! </read

  SUBROUTINE init_simulation() BIND(C)

    CALL read_in_bodies_RBDY2
    CALL update_existing_entities_RBDY2
    CALL read_in_bulk_behav 
    CALL read_xxx_tact_behav(1)
    CALL read_bodies_DISKx
    CALL read_bodies_JONCx
    CALL init_entitylist
    CALL read_behaviours_RBDY2   
    CALL read_in_dof_ol(0)
    CALL read_in_dof_RBDY2(0)
    CALL read_in_Vloc_Rloc_ol(0)
    CALL read_ini_Vloc_Rloc_DKDKx(0)
    CALL read_ini_Vloc_Rloc_DKJCx(0)
    CALL read_in_driven_dof_RBDY2
    CALL set_time_step(HH)        
    CALL init_theta_integrator(TT)       

    print*,'mean radius diskx:',get_mean_radius_DISKx()

!    CALL set_ref_radius_post2D(0.0005_8)
    CALL active_GMV('TCTRS')
    CALL active_GMV('TCTPT')
    CALL init_GMV_post2D

    postpro_unit = init_postpro_command()
    call start_postpro(postpro_unit, 0)
    CALL messages_for_users

    CALL compute_box_DKDKx
    CALL compute_box_DKJCx
    CALL Init_entitylist
    CALL comp_mass_RBDY2

  END SUBROUTINE

! read/>


  SUBROUTINE run_simulation() BIND(C)

    do i=1,nb_time_steps

! </free
      CALL  time_increment                  !Updt_time
!      CALL  Set_newton_loop(0)
      CALL increment_RBDY2
      CALL comp_Fext_RBDY2
      CALL comp_Fint_RBDY2

      CALL comp_free_vlocy_RBDY2
! free/>
         !!!!!
! </detect
      CALL Clean_EntityList
      CALL set_run_contactor(freq_detec)

      CALL coor_prediction_DKDKx
      IF (RUN_DKDKx()) THEN
         CALL creation_tab_visu_DKDKx
!         WRITE(*,'(1X,I10,A20)') get_nb_DKDKx(),' DKDKx roughly found'       
      END IF

      CALL compute_contact_DKDKx
!      WRITE(*,'(1X,I10,A12)') get_nb_DKDKx(),' DKDKx found'       

      CALL coor_prediction_DKJCx
      IF (RUN_DKJCx()) THEN
         CALL creation_tab_visu_DKJCx
!         WRITE(*,'(1X,I10,A20)') get_nb_DKJCx(),' DKJCx roughly found'       
      END IF

      CALL compute_contact_DKJCx
!      WRITE(*,'(1X,I10,A12)') get_nb_DKJCx(),' DKJCx found'       
! detect />
         !!!! 
!</ nlgs
      CALL recup_rloc_DKJCx
!      WRITE(*,'(1X,I10,A12)') get_nb_recup_DKJCx(),' recup DKJCx'

      CALL recup_rloc_DKDKx
!      WRITE(*,'(1X,I10,A12)') get_nb_recup_DKDKx(),' recup DKDKx'

      CALL set_nlgs_parameter('Quad ',tol,RELAX)

!ELG
      CALL prep_nlgs(.FALSE.)
!SDL      CALL prep_nlgs(.TRUE.)


      iter = 0
      DO ib=1,gs_it2
        DO ik=1,gs_it1
          iter = iter + 1
          CALL solve_nlgs(1)
        END DO

        CALL prep_check_nlgs(iconv)
        IF (iconv == 0 ) exit
        CALL solve_nlgs(2)
        CALL comp_check_nlgs(iconv)
        IF (iconv == 0) EXIT
      END DO

      CALL RnodHRloc_nlgs

      CALL solve_nlgs(3)
      CALL Nullify_EntityList_nlgs

      CALL stock_rloc_DKDKx 
      CALL stock_rloc_DKJCx 
! nlgs />

! </updt
      CALL comp_dof_RBDY2
      CALL Updt_time_begin
      CALL update_dof_RBDY2
! updt />

! </post
      IF (MODULO(get_NSTEP(),freq_gmv) == 0) then 
        CALL write_out_gmv_Ol
        CALL update_post2D(0)
        CALL write_GMV_post2D
      ENDIF

      CALL postpro_during_computation
! post />

      IF (MODULO(get_NSTEP(),freq_vlocrloc) == 0) then 
        call Write_xxx_Vloc_Rloc_Ol(1)
        call Write_xxx_Vloc_Rloc_DKDKx(1)
        call Write_xxx_Vloc_Rloc_DKJCx(1)
      ENDIF

      call clean_writing_flags

    enddo

    CALL close_postpro_files

  END SUBROUTINE

END MODULE
