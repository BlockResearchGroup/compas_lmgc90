program handmadecomputation

  use utilities 

  USE RBDY3, only: &
       increment_RBDY3, &
       comp_dof_RBDY3, &
       update_dof_RBDY3, &
       comp_free_vlocy_RBDY3, &
       comp_Fext_RBDY3, &
       comp_Fint_RBDY3, &
       set_skip_invisible_RBDY3, &
       check_source_point_RBDY3, &
       out_of_bounds_RBDY3, &
       fatal_damping_RBDY3, &
       read_in_bodies_RBDY3, &
       update_existing_entities_RBDY3, &
       read_in_dof_RBDY3, &
       read_in_driven_dof_RBDY3, &
       read_behaviours_RBDY3, &
       write_out_bodies_RBDY3, &
       write_xxx_dof_RBDY3, &
       write_xxx_Rnod_RBDY3, &
       write_out_driven_dof_RBDY3, &
       comp_mass_RBDY3, &
       set_xperiodic_data_RBDY3, &
       set_yperiodic_data_RBDY3, &
       init_source_point_RBDY3, &
       set_init_boundary_RBDY3, &
       get_nb_RBDY3, &
       get_write_Rnod_RBDY3, &
       get_write_DOF_RBDY3, &
       put_vector_RBDY3, get_vector_RBDY3, &
       get_volume

  USE bulk_behaviour

  USE tact_behaviour

  USE SPHER,ONLY:&
      read_bodies_SPHER, &
      get_nb_SPHER, &
      get_radius_SPHER, &
      get_mean_radius_SPHER

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
      time_increment, & 
      Updt_time_begin, & 
      Set_newton_tolerance, &
      Set_newton_maxloop, &
      Set_newton_badloop, &
      Set_newton_goodloop, &
      Set_newton_rate_step, & 
      Set_newton_loop, &
      Incre_newton_loop, &
      !Comp_NR_time_step, &
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
      set_working_directory, &
      Init_EntityList, &
      Clean_writing_flags, &
      set_with_experimental_dev, &
      i_real_tactor, &
      i_rough_tactor, &
      i_recup_tactor

  USE SPSPx,ONLY: &
       get_nb_SPSPx,&
       stock_rloc_SPSPx, &
       recup_rloc_SPSPx, &
       smooth_computation_SPSPx, &
       compute_box_SPSPx, &
       read_ini_Vloc_Rloc_SPSPx, &
       write_xxx_Vloc_Rloc_SPSPx, &
       set_xperiodic_data_SPSPx, &
       set_yperiodic_data_SPSPx, &
       coor_prediction_SPSPx, &
       creation_tab_visu_SPSPx, &
       compute_contact_SPSPx, &
       display_prox_tactors_SPSPx, &
       RUN_SPSPx, &
       CHECK_SPSPx, &
       get_write_Vloc_Rloc_SPSPx

  USE nlgs_3D, ONLY:&
       solve_nlgs, &
       comp_check_nlgs, &
       write_norm_check_nlgs, &
       scramble_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       display_check_nlgs, &
       scale_rloc_nlgs, &
       RnodHRloc_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs

  USE POSTPRO_3D,ONLY:&
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &  
       postpro_during_computation

  USE timer, ONLY: &
       initialize_utimer, &
       write_utimer, &
       get_new_utimer_ID, &
       start_utimer,stop_utimer 

  implicit none



integer :: i,ib,ik,iconv,iter
integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_nlgs_check,id_nlgs_post1,id_nlgs_post2,id_nlgs_post3, &
           id_select,id_updt,id_post
integer :: postpro_unit

real(kind=8) :: HH,TT,tol,RELAX=1.d0
integer      :: nb_time_steps,freq_detec,gs_it1, gs_it2,freq_gmv,freq_vlocrloc

       read(5,*) HH
       read(5,*) nb_time_steps
       read(5,*) TT
       read(5,*) freq_detec
       read(5,*) tol
       read(5,*) gs_it1
       read(5,*) gs_it2
       read(5,*) freq_gmv
       read(5,*) freq_vlocrloc
!!!!!!!!!!!!!!!!

       CALL initialize_utimer
                                       !12345678901234567890
       id_read       = get_new_utimer_ID('READ                ')
       id_free       = get_new_utimer_ID('INIT                ')
       id_nlgs_prep  = get_new_utimer_ID('NLGS PREP           ')
       id_nlgs_iter  = get_new_utimer_ID('NLGS ITER           ')
       id_nlgs_check = get_new_utimer_ID('NLGS CHECK          ')
       id_nlgs_post1 = get_new_utimer_ID('NLGS POST 1         ')
       id_nlgs_post2 = get_new_utimer_ID('NLGS POST 2         ')
       id_nlgs_post3 = get_new_utimer_ID('NLGS POST 3         ')
       id_select     = get_new_utimer_ID('Select Prox Tactors ')
       id_updt       = get_new_utimer_ID('Update              ')
       id_post       = get_new_utimer_ID('POST                ')

!!!!!!!
       call set_with_experimental_dev
! </read
       call start_utimer(id_read)

       CALL set_time_step(HH)        
       CALL init_theta_integrator(TT)       

       CALL read_in_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3

       CALL read_in_dof_ol(0)
       CALL read_in_dof_RBDY3(0)

       CALL read_bodies_SPHER
       CALL init_entitylist

       CALL read_in_bulk_behav 
       !am: lecture du fichier TACT_BEHAV.DAT
       CALL open_tact_behav_ll()
       CALL open_see_ll()
       CALL read_xxx_tact_behav(1)
       CALL close_tact_behav_ll()
       CALL close_see_ll()
       ! fin lecture du fichier TACT_BEHAV.DAT

       CALL read_behaviours_RBDY3   

       CALL read_in_Vloc_Rloc_ol(0)
       if ( check_SPSPx() ) CALL read_ini_Vloc_Rloc_SPSPx(0)

       CALL read_in_driven_dof_RBDY3

       print*,'mean radius spher:',get_mean_radius_SPHER()

       postpro_unit = init_postpro_command()
       call start_postpro(postpro_unit, 0)
       CALL messages_for_users

       if ( check_SPSPx() ) CALL compute_box_SPSPx
       CALL comp_mass_RBDY3

       call stop_utimer(id_read)
! read/>

       do i=1,nb_time_steps
! </free
         call start_utimer(id_free)

         CALL  time_increment                  !Updt_time
         CALL increment_RBDY3
         CALL comp_Fext_RBDY3
         CALL comp_Fint_RBDY3

         CALL comp_free_vlocy_RBDY3
         call stop_utimer(id_free)
! free/>
         !!!!!
! </detect
         call start_utimer(id_select)
       
         CALL Init_entitylist

         CALL set_run_contactor(freq_detec)

         if ( check_SPSPx() ) then 
            CALL coor_prediction_SPSPx
            IF (RUN_SPSPx()) THEN
               CALL creation_tab_visu_SPSPx
               !WRITE(*,'(1X,I10,A20)') get_nb_SPSPx(i_rough_tactor),' SPSPx roughly found'       
            END IF

            CALL compute_contact_SPSPx
            !WRITE(*,'(1X,I10,A12)') get_nb_SPSPx(i_real_tactor),' SPSPx found'       
         end if

         call stop_utimer(id_select)
! detect />
         !!!! 

!</ nlgs
         call start_utimer(id_nlgs_prep)

         if ( check_SPSPx() ) CALL recup_rloc_SPSPx
         !WRITE(*,'(1X,I10,A12)') get_nb_SPSPx(i_recup_tactor),' recup SPSPx'

         CALL set_nlgs_parameter('Quad ',tol,RELAX)

!ELG
         CALL prep_nlgs(.FALSE.)
!SDL         CALL prep_nlgs(.TRUE.)

         call stop_utimer(id_nlgs_prep)

         iter = 0
         DO ib=1,gs_it2
           call start_utimer(id_nlgs_iter)
           DO ik=1,gs_it1
             iter = iter + 1
             CALL solve_nlgs(1)
           END DO
           call stop_utimer(id_nlgs_iter)

           call start_utimer(id_nlgs_check)
           CALL prep_check_nlgs(iconv)
           call stop_utimer(id_nlgs_check)
           IF (iconv == 0 ) exit
           call start_utimer(id_nlgs_check)
           CALL solve_nlgs(2)
           CALL comp_check_nlgs(iconv)
           call stop_utimer(id_nlgs_check)
           IF (iconv == 0) EXIT
         END DO

         call start_utimer(id_nlgs_post1)

         CALL RnodHRloc_nlgs

         call stop_utimer(id_nlgs_post1)

         call start_utimer(id_nlgs_post2)

         CALL solve_nlgs(3)

         CALL Nullify_EntityList_nlgs

         call stop_utimer(id_nlgs_post2)

         call start_utimer(id_nlgs_post3)

         if ( check_SPSPx() ) CALL stock_rloc_SPSPx 

         call stop_utimer(id_nlgs_post3)
! nlgs />

! </updt
         call start_utimer(id_updt)

         CALL comp_dof_RBDY3
         CALL Updt_time_begin
         CALL update_dof_RBDY3

         call stop_utimer(id_updt)
! updt />

! </post
         call start_utimer(id_post)

         CALL postpro_during_computation

         call stop_utimer(id_post)
! post />

         IF (MODULO(get_NSTEP(),freq_vlocrloc) == 0) then 
           call Write_xxx_Vloc_Rloc_Ol(1)
           if ( check_SPSPx() ) call Write_xxx_Vloc_Rloc_SPSPx(1)
         ENDIF

         call clean_writing_flags

       enddo

       CALL close_postpro_files
       CALL write_utimer

end program
