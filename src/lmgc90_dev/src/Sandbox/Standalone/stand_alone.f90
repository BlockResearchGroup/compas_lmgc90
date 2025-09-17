
program lmgc90_standalone

   use LMGC90_MPI, only : init_MPI      , &
                          start_MPI_time, &
                          stop_MPI_time , &
                          mpi_finalize_process

  use timer, only : initialize_utimer, &
                    write_utimer

  use utilities, only : faterr, &
                        logmes, &
                        disable_logmes

  use overall, only : init_dimension         , &
                      set_time_step          , &
                      init_theta_integrator  , &
                      init_CN_integrator     , &
                      read_in_dof_ol         , &
                      read_in_gpv_ol         , &
                      write_out_bodies_ol    , &
                      write_out_driven_dof_ol, &
                      write_xxx_dof_ol       , &
                      write_xxx_gpv_ol       , &
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

  use models, only : read_models , &
                     init_models , &
                     write_models, &
                     init_ppset  , &
                     store_ppset , &
                     clean_memory_models => clean_memory

  use ExternalModelsHandler, only : init_external_Models, &
                                    check_external_ppset, &
                                    clean_memory_ExternalModels => clean_memory

  use MAILx, only : read_in_bodies_MAILx     , &
                    write_out_bodies_MAILx   , &
                    write_xxx_gpv_MAILx      , &
                    get_write_GPV_actif_MAILx, &
                    clean_memory_MAILx

  use mecaMAILx, only : increment_mecaMAILx                     , &
                        read_in_driven_dof_mecaMAILx            , &
                        read_in_gpv_mecaMAILx                   , &
                        read_in_dof_mecaMAILx                   , &
                        get_write_dof_mecaMAILx                 , &
                        write_out_driven_dof_mecaMAILx          , &
                        write_xxx_dof_mecaMAILx                 , &
                        load_behaviours_mecaMAILx               , &
                        load_models_mecaMAILx                   , &
                        update_existing_entities_mecaMAILx      , &
                        push_ppset_mecaMAILx                    , &
                        get_nb_mecaMAILx                        , &
                        assemb_KT_mecaMAILx                     , &
                        apply_drvdof_KT_mecaMAILx               , &
                        assemb_RHS_mecaMAILx                    , &
                        compute_bulk_mecaMAILx                  , &
                        externalFEM_compute_bulk_mecaMAILx      , &
                        compute_mass_mecaMAILx                  , & 
                        compute_fext_mecaMAILx                  , &
                        compute_forces_mecaMAILx                , &
                        compute_free_vlocy_mecaMAILx            , &
                        externalFEM_compute_free_vlocy_mecaMAILx, &
                        compute_dof_mecaMAILx                   , &
                        externalFEM_compute_dof_mecaMAILx       , &
                        update_dof_mecaMAILx                    , &
                        update_bulk_mecaMAILx                   , &
                        externalFEM_update_bulk_mecaMAILx       , &
                        clean_memory_mecaMAILx                  , &
                        check_properties_mecaMAILx

  use therMAILx, only : increment_therMAILx           , &
                        read_in_driven_dof_therMAILx  , &
                        read_in_gpv_therMAILx         , &
                        read_in_dof_therMAILx         , &
                        get_write_dof_therMAILx       , &
                        write_out_driven_dof_therMAILx, &
                        write_xxx_dof_therMAILx       , &
                        read_behaviours_therMAILx     , &
                        read_models_therMAILx         , &
                        push_ppset_therMAILx          , &
                        get_nb_therMAILx              , &
                        assemb_KT_therMAILx           , &
                        apply_drvdof_KT_therMAILx     , &
                        assemb_RHS_therMAILx          , &
                        compute_ttFint_therMAILx      , &
                        compute_fext_therMAILx        , &
                        compute_capacity_therMAILx    , & 
                        compute_conductivity_therMAILx, & 
                        comp_dof_therMAILx            , &
                        update_dof_therMAILx          , &
                        update_bulk_therMAILx         , &
                        clean_memory_therMAILx

  use poroMAILx, only : increment_poroMAILx               , &
                        read_in_driven_dof_poroMAILx      , &
                        read_in_gpv_poroMAILx             , &
                        read_in_dof_poroMAILx             , &
                        get_write_dof_poroMAILx           , &
                        write_out_driven_dof_poroMAILx    , &
                        write_xxx_dof_poroMAILx           , &
                        load_behaviours_poroMAILx         , &
                        load_models_poroMAILx             , &
                        update_existing_entities_poroMAILx, &
                        push_ppset_poroMAILx              , &
                        get_nb_poroMAILx                  , &
                        assemb_KT_poroMAILx               , &
                        apply_drvdof_KT_poroMAILx         , &
                        assemb_RHS_poroMAILx              , &
                        compute_damping_poroMAILx         , &
                        compute_bulk_poroMAILx            , &
                        compute_mass_poroMAILx            , & 
                        compute_fext_poroMAILx            , &
                        compute_free_vlocy_poroMAILx      , &
                        compute_dof_poroMAILx             , &
                        update_dof_poroMAILx              , &
                        update_bulk_poroMAILx             , &
                        clean_memory_poroMAILx

  use RBDY2, only: increment_RBDY2               , &
                   read_in_bodies_RBDY2          , &
                   read_in_dof_RBDY2             , &
                   read_in_driven_dof_RBDY2      , &
                   read_behaviours_RBDY2         , &
                   update_existing_entities_RBDY2, &
                   get_write_DOF_RBDY2           , &
                   write_out_bodies_RBDY2        , &
                   write_xxx_dof_RBDY2           , &
                   write_out_driven_dof_RBDY2    , &
                   get_nb_RBDY2                  , &
                   is_periodic_RBDY2             , &
                   comp_mass_RBDY2               , &
                   comp_free_vlocy_RBDY2         , &
                   comp_Fext_RBDY2               , &
                   comp_Fint_RBDY2               , &
                   comp_dof_RBDY2                , &
                   update_dof_RBDY2              , &
                   check_periodic_RBDY2 => check_periodic, &
                   clean_memory_RBDY2

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

  use ALpxx, only : read_bodies_ALpxx, &
                    clean_memory_ALpxx
  use ASpxx, only : load_tactors_ASpxx, &
                    clean_memory_ASpxx
  use CLxxx, only : read_bodies_CLxxx, &
                    clean_memory_CLxxx
  use CSxxx, only : load_tactors_CSxxx, &
                    clean_memory_CSxxx
  use PT2DL, only : read_bodies_PT2DL       , &
                    add_convection2KT_PT2DL , &
                    add_convection2RHS_PT2DL, &
                    clean_memory_PT2DL
  use DISKL, only : read_bodies_DISKL, &
                    clean_memory_DISKL

  use DISKx, only : read_bodies_DISKx, &
                    clean_memory_DISKx
  use JONCx, only : read_bodies_JONCx, &
                    clean_memory_JONCx
  use POLYG, only : read_bodies_POLYG, &
                    clean_memory_POLYG
  use PT2Dx, only : read_bodies_PT2Dx, &
                    clean_memory_PT2Dx
  use xKSID, only : read_bodies_xKSID, &
                    clean_memory_xKSID

  use CYLND, only : read_bodies_CYLND, &
                    clean_memory_CYLND
  use DNLYC, only : read_bodies_DNLYC, &
                    clean_memory_DNLYC
  use PLANx, only : read_bodies_PLANx, &
                    clean_memory_PLANx
  use POLYR, only : read_bodies_POLYR, &
                    clean_memory_POLYR
  use PT3Dx, only : read_bodies_PT3Dx, &
                    clean_memory_PT3Dx
  use SPHER, only : read_bodies_SPHER, &
                    clean_memory_SPHER

  use CLALp, only: RUN_CLALp                , &
                   CHECK_CLALp              , &
                   read_ini_Vloc_Rloc_CLALp , &
                   write_xxx_Vloc_Rloc_CLALp, &
                   get_write_Vloc_Rloc_CLALp, &
                   compute_box_CLALp        , &
                   coor_prediction_CLALp    , &
                   creation_tab_visu_CLALp  , &
                   compute_contact_CLALp    , &
                   stock_rloc_CLALp         , &
                   recup_rloc_CLALp         , &
                   clean_memory_CLALp

  use CLJCx, only: RUN_CLJCx                , &
                   CHECK_CLJCx              , &
                   read_ini_Vloc_Rloc_CLJCx , &
                   write_xxx_Vloc_Rloc_CLJCx, &
                   get_write_Vloc_Rloc_CLJCx, &
                   compute_box_CLJCx        , &
                   coor_prediction_CLJCx    , &
                   creation_tab_visu_CLJCx  , &
                   compute_contact_CLJCx    , &
                   stock_rloc_CLJCx         , &
                   recup_rloc_CLJCx         , &
                   clean_memory_CLJCx

  use DKALp, only: RUN_DKALp                , &
                   CHECK_DKALp              , &
                   read_ini_Vloc_Rloc_DKALp , &
                   write_xxx_Vloc_Rloc_DKALp, &
                   get_write_Vloc_Rloc_DKALp, &
                   compute_box_DKALp        , &
                   coor_prediction_DKALp    , &
                   creation_tab_visu_DKALp  , &
                   compute_contact_DKALp    , &
                   stock_rloc_DKALp         , &
                   recup_rloc_DKALp         , &
                   clean_memory_DKALp

  use DKDKL, only: RUN_DKDKL                , &
                   CHECK_DKDKL              , &
                   read_ini_Vloc_Rloc_DKDKL , &
                   write_xxx_Vloc_Rloc_DKDKL, &
                   get_write_Vloc_Rloc_DKDKL, &
                   compute_box_DKDKL        , &
                   coor_prediction_DKDKL    , &
                   creation_tab_visu_DKDKL  , &
                   compute_contact_DKDKL    , &
                   stock_rloc_DKDKL         , &
                   recup_rloc_DKDKL         , &
                   clean_memory_DKDKL

  use DKDKx, only: RUN_DKDKx                , &
                   CHECK_DKDKx              , &
                   read_ini_Vloc_Rloc_DKDKx , &
                   write_xxx_Vloc_Rloc_DKDKx, &
                   get_write_Vloc_Rloc_DKDKx, &
                   compute_box_DKDKx        , &
                   coor_prediction_DKDKx    , &
                   creation_tab_visu_DKDKx  , &
                   compute_contact_DKDKx    , &
                   stock_rloc_DKDKx         , &
                   recup_rloc_DKDKx         , &
                   clean_memory_DKDKx

  use DKJCx, only: RUN_DKJCx                , &
                   CHECK_DKJCx              , &
                   read_ini_Vloc_Rloc_DKJCx , &
                   write_xxx_Vloc_Rloc_DKJCx, &
                   get_write_Vloc_Rloc_DKJCx, &
                   compute_box_DKJCx        , &
                   coor_prediction_DKJCx    , &
                   creation_tab_visu_DKJCx  , &
                   compute_contact_DKJCx    , &
                   stock_rloc_DKJCx         , &
                   recup_rloc_DKJCx         , &
                   clean_memory_DKJCx

  use DKKDx, only: RUN_DKKDx                , &
                   CHECK_DKKDx              , &
                   read_ini_Vloc_Rloc_DKKDx , &
                   write_xxx_Vloc_Rloc_DKKDx, &
                   get_write_Vloc_Rloc_DKKDx, &
                   compute_box_DKKDx        , &
                   coor_prediction_DKKDx    , &
                   creation_tab_visu_DKKDx  , &
                   compute_contact_DKKDx    , &
                   stock_rloc_DKKDx         , &
                   recup_rloc_DKKDx         , &
                   clean_memory_DKKDx

  use DKPLx, only: RUN_DKPLx                , &
                   CHECK_DKPLx              , &
                   read_ini_Vloc_Rloc_DKPLx , &
                   write_xxx_Vloc_Rloc_DKPLx, &
                   get_write_Vloc_Rloc_DKPLx, &
                   compute_box_DKPLx        , &
                   coor_prediction_DKPLx    , &
                   creation_tab_visu_DKPLx  , &
                   compute_contact_DKPLx    , &
                   stock_rloc_DKPLx         , &
                   recup_rloc_DKPLx         , &
                   clean_memory_DKPLx

  use P2P2L, only: RUN_P2P2L                , &
                   CHECK_P2P2L              , &
                   read_ini_Vloc_Rloc_P2P2L , &
                   write_xxx_Vloc_Rloc_P2P2L, &
                   get_write_Vloc_Rloc_P2P2L, &
                   compute_box_P2P2L        , &
                   coor_prediction_P2P2L    , &
                   compute_contact_P2P2L    , &
                   stock_rloc_P2P2L         , &
                   recup_rloc_P2P2L         , &
                   clean_memory_P2P2L

  use PLALp, only: RUN_PLALp                , &
                   CHECK_PLALp              , &
                   read_ini_Vloc_Rloc_PLALp , &
                   write_xxx_Vloc_Rloc_PLALp, &
                   get_write_Vloc_Rloc_PLALp, &
                   compute_box_PLALp        , &
                   coor_prediction_PLALp    , &
                   creation_tab_visu_PLALp  , &
                   compute_contact_PLALp    , &
                   stock_rloc_PLALp         , &
                   recup_rloc_PLALp         , &
                   clean_memory_PLALp

  use PLJCx, only: RUN_PLJCx                , &
                   CHECK_PLJCx              , &
                   read_ini_Vloc_Rloc_PLJCx , &
                   write_xxx_Vloc_Rloc_PLJCx, &
                   get_write_Vloc_Rloc_PLJCx, &
                   compute_box_PLJCx        , &
                   coor_prediction_PLJCx    , &
                   creation_tab_visu_PLJCx  , &
                   compute_contact_PLJCx    , &
                   stock_rloc_PLJCx         , &
                   recup_rloc_PLJCx         , &
                   clean_memory_PLJCx

  use PLPLx, only: RUN_PLPLx                  , &
                   CHECK_PLPLx                , &
                   read_ini_Vloc_Rloc_PLPLx   , &
                   write_xxx_Vloc_Rloc_PLPLx  , &
                   get_write_Vloc_Rloc_PLPLx  , &
                   compute_box_PLPLx          , &
                   coor_prediction_PLPLx      , &
                   creation_tab_visu_PLPLx    , &
                   compute_contact_PLPLx      , &
                   stock_rloc_PLPLx           , &
                   recup_rloc_PLPLx           , &
                   recup_rloc_ByPosition_PLPLx, &
                   clean_memory_PLPLx

  use PTPT2, only: RUN_PTPT2                , &
                   CHECK_PTPT2              , &
                   read_ini_Vloc_Rloc_PTPT2 , &
                   write_xxx_Vloc_Rloc_PTPT2, &
                   get_write_Vloc_Rloc_PTPT2, &
                   compute_box_PTPT2        , &
                   coor_prediction_PTPT2    , &
                   compute_contact_PTPT2    , &
                   stock_rloc_PTPT2         , &
                   recup_rloc_PTPT2         , &
                   clean_memory_PTPT2

  use CDCDx, only: RUN_CDCDx                , &
                   CHECK_CDCDx              , &
                   read_ini_Vloc_Rloc_CDCDx , &
                   write_xxx_Vloc_Rloc_CDCDx, &
                   get_write_Vloc_Rloc_CDCDx, &
                   compute_box_CDCDx        , &
                   coor_prediction_CDCDx    , &
                   creation_tab_visu_CDCDx  , &
                   compute_contact_CDCDx    , &
                   stock_rloc_CDCDx         , &
                   recup_rloc_CDCDx         , &
                   clean_memory_CDCDx

  use CDPLx, only: RUN_CDPLx                , &
                   CHECK_CDPLx              , &
                   read_ini_Vloc_Rloc_CDPLx , &
                   write_xxx_Vloc_Rloc_CDPLx, &
                   get_write_Vloc_Rloc_CDPLx, &
                   compute_box_CDPLx        , &
                   coor_prediction_CDPLx    , &
                   creation_tab_visu_CDPLx  , &
                   compute_contact_CDPLx    , &
                   stock_rloc_CDPLx         , &
                   recup_rloc_CDPLx         , &
                   clean_memory_CDPLx

  use CSASp, only: RUN_CSASx                , &
                   CHECK_CSASx              , &
                   read_ini_Vloc_Rloc_CSASx , &
                   write_xxx_Vloc_Rloc_CSASx, &
                   get_write_Vloc_Rloc_CSASx, &
                   initialize_CSASp         , &
                   coor_prediction_CSASx    , &
                   creation_tab_visu_CSASx  , &
                   compute_contact_CSASx    , &
                   stock_rloc_CSASx         , &
                   recup_rloc_CSASx         , &
                   clean_memory_CSASp

  use CSPRx, only: RUN_CSPRx                , &
                   CHECK_CSPRx              , &
                   read_ini_Vloc_Rloc_CSPRx , &
                   write_xxx_Vloc_Rloc_CSPRx, &
                   get_write_Vloc_Rloc_CSPRx, &
                   coor_prediction_CSPRx    , &
                   creation_tab_visu_CSPRx  , &
                   compute_contact_CSPRx    , &
                   stock_rloc_CSPRx         , &
                   recup_rloc_CSPRx         , &
                   clean_memory_CSPRx

  use PRASp, only: RUN_PRASp                , &
                   CHECK_PRASp              , &
                   read_ini_Vloc_Rloc_PRASp , &
                   write_xxx_Vloc_Rloc_PRASp, &
                   get_write_Vloc_Rloc_PRASp, &
                   initialize_PRASp         , &
                   coor_prediction_PRASp    , &
                   creation_tab_visu_PRASp  , &
                   compute_contact_PRASp    , &
                   stock_rloc_PRASp         , &
                   recup_rloc_PRASp         , &
                   clean_memory_PRASp

  use PRPLx, only: RUN_PRPLx                , &
                   CHECK_PRPLx              , &
                   read_ini_Vloc_Rloc_PRPLx , &
                   write_xxx_Vloc_Rloc_PRPLx, &
                   get_write_Vloc_Rloc_PRPLx, &
                   coor_prediction_PRPLx    , &
                   creation_tab_visu_PRPLx  , &
                   compute_contact_PRPLx    , &
                   stock_rloc_PRPLx         , &
                   recup_rloc_PRPLx         , &
                   clean_memory_PRPLx

  use PRPRx, only: RUN_PRPRx                    , &
                   CHECK_PRPRx                  , &
                   read_ini_Vloc_Rloc_PRPRx     , &
                   write_xxx_Vloc_Rloc_PRPRx    , &
                   get_write_Vloc_Rloc_PRPRx    , &
                   compute_box_PRPRx            , &
                   coor_prediction_PRPRx        , &
                   creation_tab_visu_PRPRx      , &
                   wcp_compute_contact_PRPRx    , &
                   nc_compute_contact_PRPRx     , &
                   f2f4all_compute_contact_PRPRx, &
                   stock_rloc_PRPRx             , &
                   recup_rloc_PRPRx             , &
                   set_cundall_iteration_PRPRx  , &
                   set_f2f_tol_PRPRx            , &
                   clean_memory_PRPRx

  use PTPT3, only: RUN_PTPT3                , &
                   CHECK_PTPT3              , &
                   read_ini_Vloc_Rloc_PTPT3 , &
                   write_xxx_Vloc_Rloc_PTPT3, &
                   get_write_Vloc_Rloc_PTPT3, &
                   compute_box_PTPT3        , &
                   coor_prediction_PTPT3    , &
                   compute_contact_PTPT3    , &
                   stock_rloc_PTPT3         , &
                   recup_rloc_PTPT3         , &
                   clean_memory_PTPT3

  use SPCDx, only: RUN_SPCDx                , &
                   CHECK_SPCDx              , &
                   read_ini_Vloc_Rloc_SPCDx , &
                   write_xxx_Vloc_Rloc_SPCDx, &
                   get_write_Vloc_Rloc_SPCDx, &
                   compute_box_SPCDx        , &
                   coor_prediction_SPCDx    , &
                   creation_tab_visu_SPCDx  , &
                   compute_contact_SPCDx    , &
                   stock_rloc_SPCDx         , &
                   recup_rloc_SPCDx         , &
                   clean_memory_SPCDx

  use SPDCx, only: RUN_SPDCx                , &
                   CHECK_SPDCx              , &
                   read_ini_Vloc_Rloc_SPDCx , &
                   write_xxx_Vloc_Rloc_SPDCx, &
                   get_write_Vloc_Rloc_SPDCx, &
                   compute_box_SPDCx        , &
                   coor_prediction_SPDCx    , &
                   creation_tab_visu_SPDCx  , &
                   compute_contact_SPDCx    , &
                   stock_rloc_SPDCx         , &
                   recup_rloc_SPDCx         , &
                   clean_memory_SPDCx

  use SPPLx, only: RUN_SPPLx                , &
                   CHECK_SPPLx              , &
                   read_ini_Vloc_Rloc_SPPLx , &
                   write_xxx_Vloc_Rloc_SPPLx, &
                   get_write_Vloc_Rloc_SPPLx, &
                   compute_box_SPPLx        , &
                   coor_prediction_SPPLx    , &
                   creation_tab_visu_SPPLx  , &
                   compute_contact_SPPLx    , &
                   stock_rloc_SPPLx         , &
                   recup_rloc_SPPLx         , &
                   clean_memory_SPPLx

  use SPSPx, only: RUN_SPSPx                , &
                   CHECK_SPSPx              , &
                   read_ini_Vloc_Rloc_SPSPx , &
                   write_xxx_Vloc_Rloc_SPSPx, &
                   get_write_Vloc_Rloc_SPSPx, &
                   compute_box_SPSPx        , &
                   coor_prediction_SPSPx    , &
                   creation_tab_visu_SPSPx  , &
                   compute_contact_SPSPx    , &
                   stock_rloc_SPSPx         , &
                   recup_rloc_SPSPx         , &
                   clean_memory_SPSPx

  use nlgs, only : set_nlgs_parameter    , &
                   prep_nlgs             , &
                   solve_nlgs            , &
                   prep_check_nlgs       , &
                   comp_check_nlgs       , &
                   quick_scramble_nlgs   , &
                   RnodHRloc_nlgs        , &
                   update_tact_behav_nlgs, &
                   nullify_entitylist_nlgs

  use nlgs_3D, only : set_nlgs_3D_parameter      => set_nlgs_parameter    , &
                      prep_nlgs_3D               => prep_nlgs             , &
                      solve_nlgs_3D              => solve_nlgs            , &
                      prep_check_nlgs_3D         => prep_check_nlgs       , &
                      comp_check_nlgs_3D         => comp_check_nlgs       , &
                      quick_scramble_nlgs_3D     => quick_scramble_nlgs   , &
                      RnodHRloc_nlgs_3D          => RnodHRloc_nlgs        , &
                      update_tact_behav_nlgs_3D  => update_tact_behav_nlgs, &
                      nullify_entitylist_nlgs_3D => nullify_entitylist_nlgs

  use postpro, only : init_postpro_command      , &
                      start_postpro             , &
                      messages_for_users        , &
                      postpro_during_computation, &
                      close_postpro_files

  use postpro_3D, only : init_postpro_command_3D       => init_postpro_command      , &
                         start_postpro_3D              => start_postpro             , &
                         messages_for_users_3D         => messages_for_users        , &
                         postpro_during_computation_3D => postpro_during_computation, &
                         close_postpro_files_3D        => close_postpro_files

  implicit none

  ! space dimension
  integer(kind=4) :: space_dim = 3
  character(len=10) :: mhyp = '3D        '
  !character(len=10) :: mhyp = '2D PSTRAIN'
  
  ! time evolution parameters
  real(kind=8) :: dt = 1e-3
  integer(kind=4) :: nb_steps = 100
  
  ! theta integrator parameter
  real(kind=8) :: theta = 0.501
  
  ! deformable  yes=1, no=0
  logical :: deformable = .true.
  
  ! interaction parameters
  integer(kind=4) :: freq_detec = 1
  real(kind=8) :: Rloc_tol = 5.e-3
  
  ! nlgs parameters
  logical :: with_quick_scramble = .true.
  logical :: SDLactif = .true. !stored delassus loop or exchange local/global
  real(kind=8) :: tol = 1e-4
  real(kind=8) :: relax = 1.0
  character(len=5) :: checktype= 'Quad '
  integer(kind=4) :: gs_it1 = 50
  integer(kind=4) :: gs_it2 = 1000
  
  ! write parameter
  integer(kind=4) :: freq_write = 1
  
  ! Cp Cundall : 2 + prprx_iter
  ! Cp f2f explicit : 2 + prprx_f2f_tol
  ! CP f2f : 2 + prprx_iter + prprx_f2f_tol
  ! Nc : 3 + prprx_alert
  ! Nc f2f : 3 + prprx_alert + prprx_f2f_tol
  ! Nc f2f explicit : 4 + prprx_alert + prprx_f2f_tol
  integer(kind=4) :: prprx_detection_method = 2 ! 2: common plane, 3: non convex, 4: face to face for all
  logical         :: plplx_convex_detection = .true.
  real(kind=8)    :: prprx_f2f_tol = 0.d0
  real(kind=8)    :: prprx_alert   = 0.d0
  integer(kind=4) :: prprx_iter    = 40


  logical :: is_externalFEM = .false. 
  logical :: is_detec_init  = .false. 
  integer(kind=4) :: i, j, k, ifrom, ito, iter, iconv, postpro_unit

  character(len=32) :: cin

  write(*,*) 'Enter space dim [3]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) space_dim

  write(*,*) 'With deformable ([y],n):'
  read(*,'(A)') cin
  if( cin == 'n' ) deformable = .false.

  if( space_dim == 2 ) then
    mhyp = '2D PSTRAIN'
    if( deformable ) then
      write(*,*) 'Enter hypothesis ([2D PSTRAIN], 2D PSTRESS, 2D AXI):'
      read(*,'(A)') cin
      if( cin /= '' ) then
        read(cin,*) mhyp
      end if
    end if
  end if
  
  write(*,*) 'Enter time step [1.e-3]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) dt

  write(*,*) 'Enter number of time steps [100]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) nb_steps

  write(*,*) 'Enter theta [0.501]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) theta

  write(*,*) 'Enter detection frequency [1]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) freq_detec

  if( space_dim == 2 ) then
    write(*,*) 'Enter recup rloc tolerance (PLPL only) [5.e-3]:'
    read(*,'(A)') cin
    if( cin /= '' ) read(cin,*) Rloc_tol
  end if

  write(*,*) 'Enter nlgs norm type ([Quad ], QuaN , Maxm , QM/16):'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) checktype

  write(*,*) 'Enter nlgs tolerance [1.e-4]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) tol

  write(*,*) 'Enter nlgs relaxation [1.0]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) relax

  write(*,*) 'Enter nlgs number of iteration in one block [50]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) gs_it1

  write(*,*) 'Enter nlgs number of blocks [1000]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) gs_it2

  write(*,*) 'With quick scramble ([y],n):'
  read(*,'(A)') cin
  if( cin == 'n' ) with_quick_scramble = .false.

  write(*,*) 'Enter file writing frequency [1]:'
  read(*,'(A)') cin
  if( cin /= '' ) read(cin,*) freq_write

  if( space_dim == 3 ) then
    write(*,*) 'Set PRPR detection ([Cp_Cundall]'
    write(*,*) '                     Cp_f2f, Cp_f2f_explicit,'
    write(*,*) '                     Nc, Nc_f2f, Nc_f2f_explicit):'
    read(*,'(A)') cin
    select case( cin )
    case( '', 'Cp_Cundall' )
      prprx_detection_method = 2
      write(*,*) '    cundall iter [40]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_iter
    case( 'Cp_f2f' )
      prprx_detection_method = 2
      write(*,*) '    cundall iter [40]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_iter
      write(*,*) '    face 2 face tol [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_f2f_tol
    case( 'Cp_f2f_explicit' )
      prprx_detection_method = 2
      write(*,*) '    face 2 face tol [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_f2f_tol
    case( 'Nc' )
      prprx_detection_method = 3
      write(*,*) '    alert [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_alert
    case( 'Nc_f2f' )
      prprx_detection_method = 3
      write(*,*) '    alert [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_alert
      write(*,*) '    face 2 face tol [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_f2f_tol
    case( 'Nc_f2f_explicit' )
      prprx_detection_method = 4
      write(*,*) '    alert [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_alert
      write(*,*) '    face 2 face tol [0.]:'
      read(*,'(A)') cin
      if( cin /= '' ) read(cin,*) prprx_f2f_tol
    case default
      ! ignoring...
    end select
  else
    write(*,*) 'PLPL convex detection ([y],n):'
    read(*,'(A)') cin
    if( cin == 'n' ) plplx_convex_detection = .false.
  end if


  ifrom = 1  

  if( prprx_iter /= 0 ) call set_cundall_iteration_PRPRx(prprx_iter)
  if( prprx_f2f_tol /= 0.d0 ) call set_f2f_tol_PRPRx(prprx_f2f_tol)

  call init_MPI
  call start_MPI_time

  call initialize_utimer

  !checkDirectories()
  !call disable_logmes

  !PT3Dx_SetReferenceRadius(5.e-3)
  
  call init_dimension(mhyp)
  
  call set_time_step(dt)        
  call init_theta_integrator(theta)       
  call init_CN_integrator(theta)
  
  call logmes('READ BEHAVIOURS')
  call read_in_bulk_behav 
  call open_tact_behav_ll()
  call open_see_ll()
  call read_xxx_tact_behav(1)
  call close_tact_behav_ll()
  call close_see_ll()
  if( deformable ) then
    call read_models
    call init_models
    call init_external_Models
  end if
  
  call logmes('WRITE BEHAVIOURS')
  call write_out_bulk_behav
  call write_xxx_tact_behav(2)
  
  if( deformable ) call write_models
    
  
  call logmes('READ BODIES')
  call read_in_bodies_MAILx(2,1)
  if( space_dim == 2 ) then
    call read_in_bodies_RBDY2
    call update_existing_entities_RBDY2
  else if( space_dim == 3 ) then
    call read_in_bodies_RBDY3(1)
    call update_existing_entities_RBDY3
  end if
  
  call logmes('LOAD BEHAVIOURS')
  call load_behaviours_mecaMAILx 
  call read_behaviours_therMAILx 
  call load_behaviours_poroMAILx 
  if( space_dim == 2 ) then
    call read_behaviours_RBDY2
  else if( space_dim == 3 ) then
    call read_behaviours_RBDY3
  end if
  
  if( deformable ) then
    call init_ppset
    call load_models_mecaMAILx
    call update_existing_entities_mecaMAILx
    call push_ppset_mecaMAILx
    call read_models_therMAILx
    call push_ppset_therMAILx
    call load_models_poroMAILx
    call update_existing_entities_poroMAILx
    call push_ppset_poroMAILx
    call store_ppset
    call check_properties_mecaMAILx
  end if
  !
  call logmes('READ INI DOF')
  call read_in_dof_ol(0)
  if( space_dim == 2) then
    call read_in_dof_RBDY2(0)
  else if( space_dim == 3 ) then
    call read_in_dof_RBDY3(0)
  end if
  call read_in_dof_mecaMAILx(0)
  call read_in_dof_therMAILx(0) 
  call read_in_dof_poroMAILx(0)
  !
  if( deformable ) then
    call logmes('READ INI GPV')
    call read_in_gpv_ol(0)
    call read_in_gpv_mecaMAILx(0)
    call read_in_gpv_therMAILx(0) 
    call read_in_gpv_poroMAILx(0)
  end if
  !
  call logmes('READ DRIVEN DOF')
  if( space_dim == 2) then
    call read_in_driven_dof_RBDY2
  else if( space_dim == 3 ) then
    call read_in_driven_dof_RBDY3
  end if
  call read_in_driven_dof_mecaMAILx
  call read_in_driven_dof_therMAILx
  call read_in_driven_dof_poroMAILx
  
  call logmes('LOAD TACTORS')
  if( space_dim == 2) then
    call read_bodies_DISKx
    call read_bodies_JONCx
    call read_bodies_POLYG
    call read_bodies_PT2Dx
    call read_bodies_xKSID
    if( deformable ) then
      call read_bodies_ALpxx
      call read_bodies_CLxxx
      call read_bodies_DISKL
      call read_bodies_PT2DL
    end if
  else if( space_dim == 3 ) then
    if( deformable ) then
      call load_tactors_ASpxx
      call load_tactors_CSxxx
    end if
    call read_bodies_CYLND
    call read_bodies_DNLYC
    call read_bodies_PLANx
    call read_bodies_POLYR
    call read_bodies_PT3Dx
    call read_bodies_SPHER
  end if
  call Init_EntityList
  
  call logmes('READ INI Vloc Rloc')
  if( space_dim == 2) then
    if( check_CLALp() ) call read_ini_Vloc_Rloc_CLALp(0)
    if( check_CLJCx() ) call read_ini_Vloc_Rloc_CLJCx(0)
    if( check_DKALp() ) call read_ini_Vloc_Rloc_DKALp(0)
    if( check_DKDKL() ) call read_ini_Vloc_Rloc_DKDKL(0)
    if( check_DKDKx() ) call read_ini_Vloc_Rloc_DKDKx(0)
    if( check_DKJCx() ) call read_ini_Vloc_Rloc_DKJCx(0)
    if( check_DKKDx() ) call read_ini_Vloc_Rloc_DKKDx(0)
    if( check_DKPLx() ) call read_ini_Vloc_Rloc_DKPLx(0)
    if( check_P2P2L() ) call read_ini_Vloc_Rloc_P2P2L(0)
    if( check_PLALp() ) call read_ini_Vloc_Rloc_PLALp(0)
    if( check_PLJCx() ) call read_ini_Vloc_Rloc_PLJCx(0)
    if( check_PLPLx() ) call read_ini_Vloc_Rloc_PLPLx(0)
    if( check_PTPT2() ) call read_ini_Vloc_Rloc_PTPT2(0)
  else if( space_dim == 3 ) then
    if( check_CDCDx() ) call read_ini_Vloc_Rloc_CDCDx(0)
    if( check_CDPLx() ) call read_ini_Vloc_Rloc_CDPLx(0)
    if( check_CSASx() ) call read_ini_Vloc_Rloc_CSASx(0)
    if( check_PRASp() ) call read_ini_Vloc_Rloc_PRASp(0)
    if( check_CSPRx() ) call read_ini_Vloc_Rloc_CSPRx(0)
    if( check_PRPLx() ) call read_ini_Vloc_Rloc_PRPLx(0)
    if( check_PRPRx() ) call read_ini_Vloc_Rloc_PRPRx(0)
    if( check_PTPT3() ) call read_ini_Vloc_Rloc_PTPT3(0)
    if( check_SPCDx() ) call read_ini_Vloc_Rloc_SPCDx(0)
    if( check_SPDCx() ) call read_ini_Vloc_Rloc_SPDCx(0)
    if( check_SPPLx() ) call read_ini_Vloc_Rloc_SPPLx(0)
    if( check_SPSPx() ) call read_ini_Vloc_Rloc_SPSPx(0)
  end if
  
  !
  ! paranoid writes
  !
  call logmes('WRITE BODIES')
  call write_out_bodies_ol
  call write_out_bodies_MAILx(2,1)
  if( space_dim == 2) then
    call write_out_bodies_RBDY2
  else if( space_dim == 3 ) then
    call write_out_bodies_RBDY3(1)
  end if
  
  call logmes('WRITE DRIVEN DOF')
  call write_out_driven_dof_Ol
  if( space_dim == 2) then
    call write_out_driven_dof_RBDY2
  else if( space_dim == 3 ) then
    call write_out_driven_dof_RBDY3
  end if
  call write_out_driven_dof_mecaMAILx
  call write_out_driven_dof_therMAILx
  call write_out_driven_dof_poroMAILx
  
  !CLxxx_SetNbNodesByCLxxx(1)
  
  if( space_dim == 2) then
    postpro_unit = init_postpro_command()
    call start_postpro(postpro_unit, 0)
    call messages_for_users
  else if( space_dim == 3 ) then
    postpro_unit = init_postpro_command_3D()
    call start_postpro_3D(postpro_unit, 0)
    call messages_for_users_3D
  end if
  
  ! since constant compute elementary mass matrices once
  call logmes('COMPUTE MASS')
  if( space_dim == 2) then
    call comp_mass_RBDY2
  else if( space_dim == 3 ) then
    call comp_mass_RBDY3
  end if
  do i = 1, get_nb_mecaMAILx()
    call compute_mass_mecaMAILx(i)
  end do
  call compute_mass_poroMAILx()
  
  ! since constant compute elementary stiffness matrices once
  call logmes('COMPUTE STIFFNESS')
  if( space_dim == 2) then
    call comp_Fint_RBDY2
  else if( space_dim == 3 ) then
    call comp_Fint_RBDY3
  end if
  if( is_externalFEM ) then
    call externalFEM_compute_bulk_mecaMAILx
  else
    do i = 1, get_nb_mecaMAILx()
      call compute_bulk_mecaMAILx(i,0)
    end do
  end if
  do i = 1, get_nb_therMAILx()
    call compute_ttFint_therMAILx(i)
  end do
  call compute_bulk_poroMAILx(0)
  
  ! since constant compute iteration matrix once
  do i = 1, get_nb_mecaMAILx()
    call assemb_KT_mecaMAILx(i)
    call apply_drvdof_KT_mecaMAILx(i)
  end do
  if( space_dim == 2) call add_convection2KT_PT2DL
  do i = 1, get_nb_therMAILx()
    call assemb_KT_therMAILx(i)
    call apply_drvdof_KT_therMAILx(i)
  end do
  call assemb_KT_poroMAILx()
  call apply_drvdof_KT_poroMAILx()
  
  
  do k = 1, nb_steps
    call logmes('INCREMENT STEP')
    call Clean_writing_flags
    call time_increment
    call display_time
    if( space_dim == 2) then
      call increment_RBDY2
    else if( space_dim == 3 ) then
      call increment_RBDY3
    end if
    call increment_mecaMAILx
    call increment_therMAILx
    call increment_poroMAILx

    !TimeEvolution_DisplayStep()

    call logmes('COMPUTE Fext')
    if( space_dim == 2) then
      call comp_Fext_RBDY2
    else if( space_dim == 3 ) then
      call comp_Fext_RBDY3
    end if
    do i = 1, get_nb_mecaMAILx()
      call compute_Fext_mecaMAILx(i)
    end do
    do i = 1, get_nb_therMAILx()
      call compute_Fext_therMAILx(i)
    end do
    call compute_Fext_poroMAILx()
      
    do i = 1, get_nb_therMAILx()
      call compute_capacity_therMAILx(i)
      call compute_conductivity_therMAILx(i,0)
    end do
  
    call logmes('COMPUTE Fint')
    if( space_dim == 2) then
      call comp_Fint_RBDY2
    else if( space_dim == 3 ) then
      call comp_Fint_RBDY3
    end if
    if( is_externalFEM ) then
      call externalFEM_compute_bulk_mecaMAILx
    else
      do i = 1, get_nb_mecaMAILx()
        call compute_bulk_mecaMAILx(i,0)
      end do
    end if
    do i = 1, get_nb_therMAILx()
      call compute_ttFint_therMAILx(i)
    end do
    call compute_bulk_poroMAILx(0)
    call compute_damping_poroMAILx()
    
    !! ??
    !!call compute_convection_matrix_PT2DL
    !!call compute_convection_RHS_PT2DL

    call logmes('ASSEMBLAGE')
    do i = 1, get_nb_mecaMAILx()
      call assemb_RHS_mecaMAILx(i)
    end do
    if( space_dim == 2) call add_convection2RHS_PT2DL
    do i = 1, get_nb_therMAILx()
      CALL assemb_RHS_therMAILx(i)
    end do
    call assemb_RHS_poroMAILx()
    
    call logmes('COMPUTE Free Vlocy')
    if( space_dim == 2) then
      call comp_free_vlocy_RBDY2
    else if( space_dim == 3 ) then
      call comp_free_vlocy_RBDY3
    end if
    if( is_externalFEM ) then
      call externalFEM_compute_free_vlocy_mecaMAILx
    else
      do i = 1, get_nb_mecaMAILx()
        call compute_free_vlocy_mecaMAILx(i)
      end do
    endif
    call compute_free_vlocy_poroMAILx 
    
    call logmes('SELECT PROX TACTORS')
    call Init_EntityList
    call set_run_contactor(freq_detec)
    
    if( space_dim == 2) then
      if( .not. is_detec_init ) then
        call compute_box_CLALp
        call compute_box_CLJCx
        call compute_box_DKALp
        call compute_box_DKDKL
        call compute_box_DKDKx
        call compute_box_DKJCx
        call compute_box_DKKDx
        call compute_box_DKPLx
        call compute_box_P2P2L
        call compute_box_PLALp
        call compute_box_PLJCx
        call compute_box_PLPLx
        call compute_box_PTPT2
        is_detec_init = .true.
      end if
      if( check_CLALp() ) then  
        call coor_prediction_CLALp
        if( run_CLALp() ) call creation_tab_visu_CLALp
        call compute_contact_CLALp
      end if
      if( check_CLJCx() ) then  
        call coor_prediction_CLJCx
        if( run_CLJCx() ) call creation_tab_visu_CLJCx
        call compute_contact_CLJCx
      end if
      if( check_DKALp() ) then  
        call coor_prediction_DKALp
        if( run_DKALp() ) call creation_tab_visu_DKALp
        call compute_contact_DKALp
      end if
      if( check_DKDKL() ) then  
        call coor_prediction_DKDKL
        if( run_DKDKL() ) call creation_tab_visu_DKDKL
        call compute_contact_DKDKL
      end if
      if( check_DKDKx() ) then  
        call coor_prediction_DKDKx
        if( run_DKDKx() ) call creation_tab_visu_DKDKx
        call compute_contact_DKDKx
      end if
      if( check_DKJCx() ) then  
        call coor_prediction_DKJCx
        if( run_DKJCx() ) call creation_tab_visu_DKJCx
        call compute_contact_DKJCx
      end if
      if( check_DKKDx() ) then  
        call coor_prediction_DKKDx
        if( run_DKKDx() ) call creation_tab_visu_DKKDx
        call compute_contact_DKKDx
      end if
      if( check_DKPLx() ) then  
        call coor_prediction_DKPLx
        if( run_DKPLx() ) call creation_tab_visu_DKPLx
        call compute_contact_DKPLx
      end if
      if( check_P2P2L() ) then  
        call coor_prediction_P2P2L
        call compute_contact_P2P2L
      end if
      if( check_PLALp() ) then  
        call coor_prediction_PLALp
        if( run_PLALp() ) call creation_tab_visu_PLALp
        call compute_contact_PLALp
      end if
      if( check_PLJCx() ) then  
        call coor_prediction_PLJCx
        if( run_PLJCx() ) call creation_tab_visu_PLJCx
        call compute_contact_PLJCx
      end if
      if( check_PLPLx() ) then  
        call coor_prediction_PLPLx
        if( run_PLPLx() ) call creation_tab_visu_PLPLx(plplx_convex_detection)
        call compute_contact_PLPLx
      end if
      if( check_PTPT2() ) then  
        call coor_prediction_PTPT2
        call compute_contact_PTPT2
      end if
    else if( space_dim == 3 ) then
      if( .not. is_detec_init ) then
        call compute_box_CDCDx
        call compute_box_CDPLx
        call initialize_CSASp
        call initialize_PRASp
        if( check_PRPrx() ) call compute_box_PRPRx
        call compute_box_PTPT3
        call compute_box_SPCDx
        call compute_box_SPDCx
        call compute_box_SPPLx
        call compute_box_SPSPx
        is_detec_init = .true.
      end if
      if( check_CDCDx() ) then  
        call coor_prediction_CDCDx
        if( run_CDCDx() ) call creation_tab_visu_CDCDx
        call compute_contact_CDCDx
      end if
      if( check_CDPLx() ) then  
        call coor_prediction_CDPLx
        if( run_CDPLx() ) call creation_tab_visu_CDPLx
        call compute_contact_CDPLx
      end if
      if( check_CSASx() ) then  
        call coor_prediction_CSASx
        if( run_CSASx() ) call creation_tab_visu_CSASx
        call compute_contact_CSASx
      end if
      if( check_PRASp() ) then  
        call coor_prediction_PRASp
        if( run_PRASp() ) call creation_tab_visu_PRASp
        call compute_contact_PRASp
      end if
      if( check_CSPRx() ) then  
        call coor_prediction_CSPRx
        if( run_CSPRx() ) call creation_tab_visu_CSPRx
        call compute_contact_CSPRx
      end if
      if( check_PRPLx() ) then  
        call coor_prediction_PRPLx
        if( run_PRPLx() ) call creation_tab_visu_PRPLx
        call compute_contact_PRPLx
      end if
      if( check_PRPRx() ) then  
        call coor_prediction_PRPRx
        if( run_PRPRx() ) call creation_tab_visu_PRPRx
        select case(prprx_detection_method)
        case(2)
          call wcp_compute_contact_PRPRx
        case(3)
          call nc_compute_contact_PRPRx(prprx_alert)
        case(4)
          call f2f4all_compute_contact_PRPRx(prprx_alert)
        case default
          call faterr('prprx select prox tactor','Detection method unknown')
        end select
      end if
      if( check_PTPT3() ) then  
        call coor_prediction_PTPT3
        call compute_contact_PTPT3
      end if
      if( check_SPCDx() ) then  
        call coor_prediction_SPCDx
        if( run_SPCDx() ) call creation_tab_visu_SPCDx
        call compute_contact_SPCDx
      end if
      if( check_SPDCx() ) then  
        call coor_prediction_SPDCx
        if( run_SPDCx() ) call creation_tab_visu_SPDCx
        call compute_contact_SPDCx
      end if
      if( check_SPPLx() ) then  
        call coor_prediction_SPPLx
        if( run_SPPLx() ) call creation_tab_visu_SPPLx
        call compute_contact_SPPLx
      end if
      if( check_SPSPx() ) then  
        call coor_prediction_SPSPx
        if( run_SPSPx() ) call creation_tab_visu_SPSPx
        call compute_contact_SPSPx
      end if
    end if
  
    call logmes('RECUP' )
    if( space_dim == 2) then
      if( check_CLALp() ) call recup_rloc_CLALp
      if( check_CLJCx() ) call recup_rloc_CLJCx
      if( check_DKALp() ) call recup_rloc_DKALp
      if( check_DKDKL() ) call recup_rloc_DKDKL
      if( check_DKDKx() ) call recup_rloc_DKDKx
      if( check_DKJCx() ) call recup_rloc_DKJCx
      if( check_DKKDx() ) call recup_rloc_DKKDx
      if( check_DKPLx() ) call recup_rloc_DKPLx
      if( check_P2P2L() ) call recup_rloc_P2P2L
      if( check_PLALp() ) call recup_rloc_PLALp
      if( check_PLJCx() ) call recup_rloc_PLJCx
      if( check_PTPT2() ) call recup_rloc_PTPT2
      if( check_PLPLx() ) then
        if( Rloc_tol /= 0.d0 ) then
          call recup_rloc_ByPosition_PLPLx(Rloc_tol)
        else
          call recup_rloc_PLPLx
        end if
      end if
    else if( space_dim == 3 ) then
      if( check_CDCDx() ) call recup_rloc_CDCDx
      if( check_CDPLx() ) call recup_rloc_CDPLx
      if( check_CSASx() ) call recup_rloc_CSASx
      if( check_PRASp() ) call recup_rloc_PRASp
      if( check_CSPRx() ) call recup_rloc_CSPRx
      if( check_PRPLx() ) call recup_rloc_PRPLx
      if( check_PRPRx() ) call recup_rloc_PRPRx
      if( check_PTPT3() ) call recup_rloc_PTPT3
      if( check_SPCDx() ) call recup_rloc_SPCDx
      if( check_SPDCx() ) call recup_rloc_SPDCx
      if( check_SPPLx() ) call recup_rloc_SPPLx
      if( check_SPSPx() ) call recup_rloc_SPSPx
    end if
    
  
    call logmes('RESOLUTION' )
    if( space_dim == 2) then
      call set_nlgs_parameter(checktype,tol,relax)
      call prep_nlgs(SDLactif)
      iter = 0
      do i = 1, gs_it2
        if( with_quick_scramble ) call quick_scramble_nlgs

        do j = 1, gs_it1
           iter = iter + 1
           call solve_nlgs(1)
        end do

        call prep_check_nlgs(iconv)
        !rm: c'est quoi ce test exactement ?
        !if (iconv == 0 ) stop 'prep_nlgs not converged'

        call solve_nlgs(2)
        call comp_check_nlgs(iconv)
        if (iconv == 0) exit

      end do

      call RnodHRloc_nlgs
      call solve_nlgs(3)
      call Nullify_EntityList_nlgs

      call update_tact_behav_nlgs

    else if( space_dim == 3 ) then
      call set_nlgs_3D_parameter(checktype,tol,RELAX)
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

    end if
    
    if( space_dim == 2) then
      if( check_CLALp() ) call stock_rloc_CLALp
      if( check_CLJCx() ) call stock_rloc_CLJCx
      if( check_DKALp() ) call stock_rloc_DKALp
      if( check_DKDKL() ) call stock_rloc_DKDKL
      if( check_DKDKx() ) call stock_rloc_DKDKx
      if( check_DKJCx() ) call stock_rloc_DKJCx
      if( check_DKKDx() ) call stock_rloc_DKKDx
      if( check_DKPLx() ) call stock_rloc_DKPLx
      if( check_P2P2L() ) call stock_rloc_P2P2L
      if( check_PLALp() ) call stock_rloc_PLALp
      if( check_PLJCx() ) call stock_rloc_PLJCx
      if( check_PLPLx() ) call stock_rloc_PLPLx
      if( check_PTPT2() ) call stock_rloc_PTPT2
    else if( space_dim == 3 ) then
      if( check_CDCDx() ) call stock_rloc_CDCDx
      if( check_CDPLx() ) call stock_rloc_CDPLx
      if( check_CSASx() ) call stock_rloc_CSASx
      if( check_PRASp() ) call stock_rloc_PRASp
      if( check_CSPRx() ) call stock_rloc_CSPRx
      if( check_PRPLx() ) call stock_rloc_PRPLx
      if( check_PRPRx() ) call stock_rloc_PRPRx
      if( check_PTPT3() ) call stock_rloc_PTPT3
      if( check_SPCDx() ) call stock_rloc_SPCDx
      if( check_SPDCx() ) call stock_rloc_SPDCx
      if( check_SPPLx() ) call stock_rloc_SPPLx
      if( check_SPSPx() ) call stock_rloc_SPSPx
    end if
    
    call logmes('COMPUTE DOF, FIELDS, etc.')
    if( space_dim == 2) then
      call comp_dof_RBDY2
      if( is_periodic_RBDY2() ) call check_periodic_RBDY2
    else if( space_dim == 3 ) then
      call comp_dof_RBDY3
    end if
    if( is_externalFEM ) then
      call externalFEM_compute_dof_mecaMAILx
    else
      do i = 1, get_nb_mecaMAILx()
        call compute_dof_mecaMAILx(i)
        call compute_bulk_mecaMAILx(i,1)
        call compute_forces_mecaMAILx(i)
      end do
    end if
    do i = 1, get_nb_therMAILx()
      call comp_dof_therMAILx(i)
    end do
    call compute_dof_poroMAILx 
    
    call logmes('UPDATE DOF, FIELDS')
    call Updt_time_begin
    if( space_dim == 2) then
      call update_dof_RBDY2
    else if( space_dim == 3 ) then
      call update_dof_RBDY3
    end if
    if( is_externalFEM ) then
      call externalFEM_update_bulk_mecaMAILx
    else
      do i = 1, get_nb_mecaMAILx()
        call update_dof_mecaMAILx(i)
        call compute_bulk_mecaMAILx(i,1)
        call update_bulk_mecaMAILx(i)
      end do
    end if
    do i = 1, get_nb_therMAILx()
      call update_dof_therMAILx(i)
      call compute_conductivity_therMAILx(i,1)
      call update_bulk_therMAILx(i)
    end do
    call update_dof_poroMAILx 
    call compute_bulk_poroMAILx(1)
    call update_bulk_poroMAILx 
    
    call logmes('WRITE OUT DOF')
    if( modulo(get_NSTEP(),freq_write) == 0 ) call write_xxx_dof_ol(1)

    if( space_dim == 2) then
      ito   = get_nb_RBDY2()
      if (get_write_DOF_RBDY2()) call write_xxx_dof_RBDY2(1,ifrom,ito)
    else if( space_dim == 3 ) then
      ito   = get_nb_RBDY3()
      if (get_write_DOF_RBDY3()) call write_xxx_dof_RBDY3(1,ifrom,ito)
    end if
    if (get_write_DOF_mecaMAILx()) then
        ito   = get_nb_mecaMAILx()
        call write_xxx_dof_mecaMAILx(1,ifrom,ito)
    end if
    if (get_write_DOF_therMAILx()) then
        ito   = get_nb_therMAILx()
        call write_xxx_dof_therMAILx(1,ifrom,ito)
    end if
    if (get_write_DOF_poroMAILx()) then
        ito   = get_nb_poroMAILx()
        call write_xxx_dof_poroMAILx(1,ifrom,ito)
    end if
    
    call logmes('WRITE OUT GPV')
    if( modulo(get_NSTEP(),freq_write) == 0 ) call write_xxx_gpv_ol(1)
    if( get_write_GPV_actif_MAILx() ) call write_xxx_gpv_MAILx(1)
    !
    call logmes('WRITE OUT Rloc')
    if( modulo(get_NSTEP(),freq_write) == 0 ) call write_xxx_Vloc_Rloc_ol(1)
    if( space_dim == 2) then
      if( check_CLALp() ) then
        if( get_write_Vloc_Rloc_CLALp() ) call write_xxx_Vloc_Rloc_CLALp(1)
      end if
      if( check_CLJCx() ) then
        if( get_write_Vloc_Rloc_CLJCx() ) call write_xxx_Vloc_Rloc_CLJCx(1)
      end if
      if( check_DKALp() ) then
        if( get_write_Vloc_Rloc_DKALp() ) call write_xxx_Vloc_Rloc_DKALp(1)
      end if
      if( check_DKDKL() ) then
        if( get_write_Vloc_Rloc_DKDKL() ) call write_xxx_Vloc_Rloc_DKDKL(1)
      end if
      if( check_DKDKx() ) then
        if( get_write_Vloc_Rloc_DKDKx() ) call write_xxx_Vloc_Rloc_DKDKx(1)
      end if
      if( check_DKJCx() ) then
        if( get_write_Vloc_Rloc_DKJCx() ) call write_xxx_Vloc_Rloc_DKJCx(1)
      end if
      if( check_DKKDx() ) then
        if( get_write_Vloc_Rloc_DKKDx() ) call write_xxx_Vloc_Rloc_DKKDx(1)
      end if
      if( check_DKPLx() ) then
        if( get_write_Vloc_Rloc_DKPLx() ) call write_xxx_Vloc_Rloc_DKPLx(1)
      end if
      if( check_P2P2L() ) then
        if( get_write_Vloc_Rloc_P2P2L() ) call write_xxx_Vloc_Rloc_P2P2L(1)
      end if
      if( check_PLALp() ) then
        if( get_write_Vloc_Rloc_PLALp() ) call write_xxx_Vloc_Rloc_PLALp(1)
      end if
      if( check_PLJCx() ) then
        if( get_write_Vloc_Rloc_PLJCx() ) call write_xxx_Vloc_Rloc_PLJCx(1)
      end if
      if( check_PLPLx() ) then
        if( get_write_Vloc_Rloc_PLPLx() ) call write_xxx_Vloc_Rloc_PLPLx(1)
      end if
      if( check_PTPT2() ) then
        if( get_write_Vloc_Rloc_PTPT2() ) call write_xxx_Vloc_Rloc_PTPT2(1)
      end if
    else if( space_dim == 3 ) then
      if( check_CDCDx() ) then
        if( get_write_Vloc_Rloc_CDCDx() ) call write_xxx_Vloc_Rloc_CDCDx(1)
      end if
      if( check_CDPLx() ) then
        if( get_write_Vloc_Rloc_CDPLx() ) call write_xxx_Vloc_Rloc_CDPLx(1)
      end if
      if( check_CSASx() ) then
        if( get_write_Vloc_Rloc_CSASx() ) call write_xxx_Vloc_Rloc_CSASx(1)
      end if
      if( check_PRASp() ) then
        if( get_write_Vloc_Rloc_PRASp() ) call write_xxx_Vloc_Rloc_PRASp(1)
      end if
      if( check_CSPRx() ) then
        if( get_write_Vloc_Rloc_CSPRx() ) call write_xxx_Vloc_Rloc_CSPRx(1)
      end if
      if( check_PRPLx() ) then
        if( get_write_Vloc_Rloc_PRPLx() ) call write_xxx_Vloc_Rloc_PRPLx(1)
      end if
      if( check_PRPRx() ) then
        if( get_write_Vloc_Rloc_PRPRx() ) call write_xxx_Vloc_Rloc_PRPRx(1)
      end if
      if( check_PTPT3() ) then
        if( get_write_Vloc_Rloc_PTPT3() ) call write_xxx_Vloc_Rloc_PTPT3(1)
      end if
      if( check_SPCDx() ) then
        if( get_write_Vloc_Rloc_SPCDx() ) call write_xxx_Vloc_Rloc_SPCDx(1)
      end if
      if( check_SPDCx() ) then
        if( get_write_Vloc_Rloc_SPDCx() ) call write_xxx_Vloc_Rloc_SPDCx(1)
      end if
      if( check_SPPLx() ) then
        if( get_write_Vloc_Rloc_SPPLx() ) call write_xxx_Vloc_Rloc_SPPLx(1)
      end if
      if( check_SPSPx() ) then
        if( get_write_Vloc_Rloc_SPSPx() ) call write_xxx_Vloc_Rloc_SPSPx(1)
      end if
    end if
    
    if( space_dim == 2) then
      call postpro_during_computation
    else if( space_dim == 3 ) then
      call postpro_during_computation_3D
    end if

  end do
  
  call write_utimer     
  if( space_dim == 2) then
    call close_postpro_files
  else if( space_dim == 3 ) then
    call close_postpro_files_3D
  end if

  call stop_MPI_time
  call mpi_finalize_process
  
  call clean_memory_models()
  call clean_memory_ExternalModels()
  call clean_memory_bulk_behav
  call clean_memory_tact_behav
  
  call clean_memory_CLALp
  call clean_memory_CSASp
  call clean_memory_PRASp
  call clean_memory_CLJCx
  call clean_memory_DKALp
  call clean_memory_PLALp
  call clean_memory_DKDKL
  call clean_memory_P2P2L
  
  call clean_memory_CDCDx
  call clean_memory_CDPLx
  call clean_memory_CSPRx
  call clean_memory_PRPLx
  call clean_memory_PRPRx
  call clean_memory_PTPT3
  call clean_memory_SPCDx
  call clean_memory_SPDCx
  call clean_memory_SPPLx
  call clean_memory_SPSPx
  
  call clean_memory_DKDKx
  call clean_memory_DKJCx
  call clean_memory_DKKDx
  call clean_memory_DKPLx
  call clean_memory_PLJCx
  call clean_memory_PLPLx
  call clean_memory_PTPT2
  
  call clean_memory_ALpxx
  call clean_memory_ASpxx
  call clean_memory_CLxxx
  call clean_memory_CSxxx
  call clean_memory_DISKL
  call clean_memory_PT2DL
  
  call clean_memory_mecaMAILx
  call clean_memory_therMAILx
  call clean_memory_poroMAILx
  call clean_memory_MAILx
  
  call clean_memory_CYLND
  call clean_memory_DNLYC
  call clean_memory_PLANx
  call clean_memory_POLYR
  call clean_memory_PT3Dx
  call clean_memory_SPHER
  
  call clean_memory_RBDY3
  
  call clean_memory_DISKx
  call clean_memory_JONCx
  call clean_memory_POLYG
  call clean_memory_PT2Dx
  call clean_memory_xKSID
  
  call clean_memory_RBDY2
  
  !display_3D_CleanMemory()

end program
