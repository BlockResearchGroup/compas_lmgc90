program SPSP_standalone_DDM_MPI_DECENT

  use DDM_MPI_3D_DECENT

  use LMGC90_MPI

  use utilities 

  USE RBDY3, only:                         &
       increment_RBDY3,                    &
       comp_dof_RBDY3,                     & 
       comp_V_RBDY3,                       & 
       comp_X_localFrame_RBDY3,            &
       update_dof_RBDY3,                   &
       comp_free_vlocy_RBDY3,              &
       comp_Fext_RBDY3,                    &
       comp_Fint_RBDY3,                    &
       set_skip_invisible_RBDY3,           &
       check_source_point_RBDY3,           &
       out_of_bounds_RBDY3,                &
       fatal_damping_RBDY3,                &
       partial_damping_RBDY3,              &
       read_in_bodies_RBDY3,               &
       update_existing_entities_RBDY3,     &
       read_in_dof_RBDY3,                  &
       read_in_driven_dof_RBDY3,           &
       read_behaviours_RBDY3,              &
       write_out_bodies_RBDY3,             &
       write_out_driven_dof_RBDY3,         &
       write_xxx_dof_RBDY3,                &
       write_xxx_Rnod_RBDY3,               &
       write_out_driven_dof_RBDY3,         &
       comp_mass_RBDY3,                    &
       set_xperiodic_data_RBDY3,           &
       set_yperiodic_data_RBDY3,           &
       init_source_point_RBDY3,            &
       set_init_boundary_RBDY3,            &
       get_nb_RBDY3,                       &
       get_write_Rnod_RBDY3,               &
       get_write_DOF_RBDY3,                &
       put_vector_RBDY3, get_vector_RBDY3, &
       get_volume,                         &
       copy_bodies_RBDY3,                  &
       set_visibility_4all_RBDY3,          &
       !TEST
       get_X,                              &
       get_Xbegin,                         &
       get_V,                              &
       get_Vbegin,                         &
       get_coor,                           &
       get_visible,                        &
       set_skip_invisible_RBDY3,           &
       compute_configurationTT_RBDY3

  USE bulk_behaviour

  USE tact_behaviour

  USE SPHER,ONLY:&
      read_bodies_SPHER, &
      get_nb_SPHER, &
      get_radius_SPHER, &
      get_min_radius_SPHER, &
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
      write_out_gmv_Ol, &
      write_out_MP_values_Ol, &
      set_working_directory, &
      Init_EntityList, &
      Clean_writing_flags, &
      set_with_experimental_dev, &
      i_real_tactor, &
      i_rough_tactor, &
      i_recup_tactor, &
      iIaux_

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
       Nullify_EntityList_nlgs, &
       compute_local_free_vlocy

  USE timer, ONLY: &
       initialize_itimer, &
       write_itimer, &
       get_new_itimer_ID, &
       start_itimer,stop_itimer 

  implicit none

  integer :: i,istep,ib,ik,iconv1,iconv2,iter,iterTT

  ! Indices des timers
  integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_crea_dom,id_detect_fine, &
             id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_updt,id_post, &
             id_nlgs_check, id_ddm_merg, id_nlgs_Rnod, id_nlgs_Compt, id_sol_intrf, &
             id_nlgs_vfree, id_ddm_recoll, id_scatter_DFg, id_Vfree_list, id_scatt_cr_d, &
             id_bdd_locale

  real(kind=8)     :: TimeStep         ! Pas de temps
  real(kind=8)     :: Theta            ! parametre de la theta-methode
  integer          :: nb_time_steps

  integer          :: DDMfreq
  integer          :: DDMtype          ! Type de strategie DDM => NSCDD (1) ou Schwartz (0)
  logical          :: NSCDD=.false.    ! Type de strategie DDM : NSCDD => NSCDD=.true.  ou Schwartz => NSCDD=.false.
  integer          :: Nsdm1,Nsdm2,Nsdm3! nombre de sous-domaines suivant chaque direction
  integer          :: Nsdm             ! nombre total de sous-domaines

  CHARACTER(len=3) :: solving_scheme   ! Type de schema de resolution : SDL ou ELG
  CHARACTER(len=5) :: CVTYPE           ! type de norme utilisee pour tester la 
                                       ! convergence du gauss-Seidel
  real(kind=8)     :: RELAX            ! parametre de relaxation pour le Gauss-Seidel
  real(kind=8)     :: TOL              ! tolerance pour le Gauss-Seidel
  integer          :: itloop1, itloop2 ! Nombre de sous-iterations DDM et nombre max d'iterations 
                                       ! avec check de covergence

  integer          :: bavardage        ! 0 active disable_logmess et desactive print_iter
  integer          :: freq_display     ! frequece d'ecriture des fichiers de visualisation
  integer          :: freq_write       ! frequece d'ecriture des fichiers DOF et VlocRloc
  integer          :: freq_postpro     ! frequece d'ecriture des fichiers POSTPRO
  integer          :: freq_last        ! frequece d'ecriture des fichiers BACKUPS

  integer          :: nb_RBDY3         ! nombre total de RBDY3 (lus  + copies)
  integer          :: nb_SPHER         ! nombre total de SPHER (lues + copies)
  integer          :: nbody            ! nombre initial de RBDY3 (i.e. lus dans le fichier)

  integer          :: ERR              ! pour recuperer le code d'erreur renvoye lors de la lecture du fichier
  character(len=200):: cout             ! pour faire call logmes

  ! indices de boucle
  integer          :: isdm             ! indice d'iteration de la boucle sur les sous-domaines
  integer          :: iddm             ! indice d'itÃ©ration ddm

  ! Pour le pilotage en temps
  logical          :: pilotage_en_temps
  real(kind=8)     :: Temps_initial    ! Valeur du temps initial
  real(kind=8)     :: TimeStep_ini     ! Pas de temps initial
  real(kind=8)     :: Temps_simu_total ! TimeStep_ini*nb_time_steps + time_ini
  real(kind=8)     :: dt_freq_DISPLAY  ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: dt_freq_write    ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: dt_freq_last     ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: dt_freq_ddm      ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: i_freq_DISPLAY   ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: i_freq_write     ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: i_freq_last      ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: i_freq_ddm       ! Pour gerer les ecritures en temps "physique"
  real(kind=8)     :: mean_violation, max_violation ! pour l'adaptation du dt/nb_TStep
  real(kind=8)     :: old_max_violation, evol_violation ! pour l'adaptation du dt/nb_TStep
  real(kind=8)     :: min_radius
  integer(kind=4)  :: config_correcte

  !---------------!
  !      MPI      !
  !---------------!
  logical          :: is_converged4all

       ! Initialisation de l'environnement MPI
       call init_MPI

       ! On utilise le working directory associe au sdm 
       ! pour les ecritures (DOF.OUT, VlocRloc.OUT, ...)
       call set_working_directory_in_DDM

       ! on initialise le code d'erreur a 0
       ERR = 0

       ! ouverture du fichier de parametres
       OPEN(UNIT=10,FILE='standalone.in',STATUS='OLD',IOSTAT=ERR)

       ! si l'ouverture a echoue
       if (ERR.NE.0) then
          ! on affiche un message d'erreur
          WRITE(*,*) 'file standalone.in is missing'
          ! on quitte le programme
          STOP
       end if

       ! lecture :
       !    * du nombre de pas de temps
       READ(10,*) nb_time_steps
       !    * du pas de temps
       READ(10,*) TimeStep
       !    * du parametre de la theta-methode
       READ(10,*) Theta
       !    * supprime certaines sorties a l'ecran
       READ(10,*) bavardage
       !     * de la frequence d'ecriture des fichiers de visualisation
       READ(10,*) freq_display
       !     * de la frequence d'ecriture des fichiers de sortie
       READ(10,*) freq_write
       !     * de la frequence d'ecriture des fichiers de postpro
       READ(10,*) freq_postpro
       !     * de la frequence d'ecriture des fichiers de sauvegarde
       READ(10,*) freq_last
       !    * de la grequence de detection/decomposition en sous-domaines
       READ(10,*) DDMfreq
       !    * du nombre de sous-domaines dans chaque direction
       READ(10,*) Nsdm1, Nsdm2, Nsdm3
       READ(10,*) DDMtype
       !    * type de strategie : SDL ou ELG
       READ(10,'(A3)') solving_scheme
       !    * du type de norme de convergence et de la tolerance
       !      pour le Gauss-Seidel
       READ(10,'(A5,E10.1)') CVTYPE,TOL
       !    * du parametere de relaxation
       READ(10,*) RELAX
       !    * du nombre de boucle avant test de la convergence et
       !      du nombre maximum de paquets d'iterations de Gauss-Seidel.
       READ(10,*) itloop1, itloop2

       ! fermeture du fichier de parametres
       CLOSE(10)
  
       if (DDMtype .eq. 1)  NSCDD     = .true.

       ! Affichage des donnees lues
       !---> Fonction rang0 <---
       call print_fichier_entree(nb_time_steps,TimeStep,Theta,bavardage,freq_DISPLAY,freq_write,freq_postpro,freq_last, &
                        DDMfreq,Nsdm1,Nsdm2,Nsdm3,DDMtype,solving_scheme,CVTYPE,TOL,RELAX,itloop1,itloop2)

       
       ! </read
       call start_itimer(id_read)
       if (bavardage == 0) call disable_logmes

       ! TODO Tester avec et sans 
       !call set_with_experimental_dev
      
       ! Active la prise en compte de la visibilite des corps 
       ! dans les fonctions write_bodies et write_out_dof.
       call set_skip_invisible_RBDY3

       call init_dimension("3D        ")

       TimeStep_ini = TimeStep
       call set_time_step(TimeStep)        
       call init_theta_integrator(Theta)       

       call read_in_bodies_RBDY3(1)
       call update_existing_entities_RBDY3

       call read_in_dof_ol(0)
       call read_in_dof_RBDY3(0)

       call read_in_driven_dof_RBDY3
       call stop_itimer(id_read)
       ! read/>


       ! </DDM
       call start_itimer(id_crea_dom)
       !-------------------------------------------
       ! recuperation du nombre de corps
       nb_RBDY3 = get_nb_RBDY3()

       ! Toutes les particules sont maintenant visibles
       ! N.B. : necessaire pour que les CL soient calculees
       ! sur toutes les copies des corps lors de l'increment_step 
       call set_visibility_4all_RBDY3((/ (.true., i=1, nb_RBDY3) /),nb_RBDY3)
       !-------------------------------------------
       call stop_itimer(id_crea_dom)
       ! DDM/>


       ! </read
       call start_itimer(id_read)
       !-------------------------------------------
       call read_bodies_SPHER
       ! recuperation du nombre de spheres
       nb_SPHER   = get_nb_SPHER()
       min_radius = get_min_radius_SPHER()
       if (nb_SPHER == 0) call faterr("SPSP_standalone_DDM", &
                     "l'echantillon ne contient aucune sphere!")

       call read_in_bulk_behav 
       !am: lecture du fichier TACT_BEHAV.DAT
       call open_tact_behav_ll()
       call open_see_ll()
       call read_xxx_tact_behav(1)
       call close_tact_behav_ll()
       call close_see_ll()
       ! fin lecture du fichier TACT_BEHAV.DAT
       call read_behaviours_RBDY3
       ! Pour des Vloc_Rloc avec des I7 et non des I5
       call Acti_large_computation
       call read_in_Vloc_Rloc_ol(0)
       if ( check_SPSPx() ) call read_ini_Vloc_Rloc_SPSPx(0)
       ! ecriture :
       !    * des corps
       call Write_out_bodies_Ol
       call write_out_bodies_RBDY3(1) 
       !    * des conditions limites
       call Write_out_driven_dof_Ol
       call write_out_driven_dof_RBDY3
       !    * des conditions initiales
       call Write_xxx_dof_Ol(2)
       call write_xxx_dof_RBDY3(2,1,get_nb_RBDY3())

       if ( check_SPSPx() ) call compute_box_SPSPx
       call comp_mass_RBDY3
       !------------------------------------------
       call stop_itimer(id_read)
       ! read/>

       ! Initialisation des timers
       call initialize_itimer
                                       !12345678901234567890
       id_read       = get_new_itimer_ID('READ                ')
       id_free       = get_new_itimer_ID('FREE                ')
       id_ddm_merg   = get_new_itimer_ID('DDM Merge           ')
       id_crea_dom   = get_new_itimer_ID('Creation_domaines   ')
       id_scatt_cr_d = get_new_itimer_ID('Scatter Crea_dom    ')
       id_bdd_locale = get_new_itimer_ID('Topologie+prep+PUM  ')
       id_select     = get_new_itimer_ID('Select Prox Tactors ')
       id_nlgs_prep  = get_new_itimer_ID('NLGS PREP           ')
       id_nlgs_iter  = get_new_itimer_ID('NLGS ITER           ')
       id_nlgs_Rnod  = get_new_itimer_ID('NLGS RnodHRloc      ')
       id_nlgs_Compt = get_new_itimer_ID('NLGS Compt_dof_list ')
       id_sol_intrf  = get_new_itimer_ID('X DF = [|V|]        ')
       id_Vfree_list = get_new_itimer_ID('NLGS cmpt_Vfree_list')
       id_nlgs_vfree = get_new_itimer_ID('NLGS cmpt_loc_free_v')
       id_nlgs_check = get_new_itimer_ID('NLGS CHECK          ')
       id_nlgs_post1 = get_new_itimer_ID('NLGS POST 1         ')
       id_nlgs_post2 = get_new_itimer_ID('NLGS POST 2         ')
       id_nlgs_post3 = get_new_itimer_ID('NLGS POST 3         ')
       id_ddm_recoll = get_new_itimer_ID('DDM Recollement     ')
       id_updt       = get_new_itimer_ID('Update              ')
       id_post       = get_new_itimer_ID('POST                ')

       ! </DDM
       call start_itimer(id_crea_dom)
       !-------------------------------------------
       ! Partie propre a la decomposition de domaine
       call start_MPI_time
       !---> Fonction rang0 <---
       Nsdm = init_dd(Nsdm1, Nsdm2, Nsdm3)
       !-------------------------------------------
       call stop_itimer(id_crea_dom)
       ! DDM/>

       ! </DDM
       call start_itimer(id_crea_dom)
       !-------------------------------------------
       !   * allocation des tableaux communs a tous les processus,
       call new_dd_slave(nb_RBDY3, ntact_SPHER_=nb_SPHER)
       !   * allocation a 0, sur les esclaves, des tableaux geres par le maitre
       call allocations_fantome_slave
       !   * allocation des tableaux du processus hote seul.
       !---> Fonction rang0 <---
       call new_dd_host 

       !-------------------------------------------
       !           OPTIONS "A LA MAIN"            !
       !-------------------------------------------

       ! Choix des fichiers postproDDM a sortir (frequence de sortie fixee dans le standalone.in)
       call init_postpro_in_DDM(solv_info_=.true., viol_evol_=.true., &
            triax_compac_=.false., sdm_info_=.true.,mean_sdm_info_=.true., &
            sample_info_=.true., mean_sample_info_=.true.)

       ! Pour supprimer l'afichage des clusters lors de la visualisation
       ! NB : le premier contacteur sera cependant affiche
       call set_skip_display_cluster(.false.)

       ! Pour piloter en temps decommenter/commeter passages (rechercher PILOTAGE)
       !pilotage_en_temps=.true.
       pilotage_en_temps=.false.

       ! Chargement triaxial (determination des parametre geometriques)
       ! Rq : valeur de la pression imposee a mettre en dur dans la routine
       ! suppose que les parois sont des clusters de spheres et
       ! sont rangee en dernier dans le bodies.dat suivant l'ordre :
       ! Up, Down, Left, Rigth, Front, Rear !!
       !call triaxial_loading_DDM(0)
       !-------------------------------------------
       !-------------------------------------------
       !-------------------------------------------
       call stop_itimer(id_crea_dom)
       ! DDM/>


       iterTT=0
       istep=0

       if (pilotage_en_temps) then
          Temps_initial = get_time()
          ! Temps de simulation total souhaite (moins un shouia
          ! pour etre ""sur"" que le test fasse bien ce que l'on veut)
          Temps_simu_total = (real(nb_Time_steps, kind=8) - 0.1d0) * TimeStep_ini
          dt_freq_DISPLAY  = real(freq_DISPLAY, kind=8) * TimeStep_ini
          dt_freq_write    = real(freq_write, kind=8) * TimeStep_ini
          dt_freq_last     = real(freq_last, kind=8) * TimeStep_ini
          dt_freq_ddm      = real(DDMfreq, kind=8) * TimeStep_ini
          i_freq_DISPLAY   = 1.d0 
          i_freq_write     = 1.d0
          i_freq_last      = 1.d0
          i_freq_ddm       = 1.d0
          config_correcte  = 0
          evol_violation   = 0.d0
          old_max_violation= 0.d0
       end if

  step:do istep=1,nb_Time_steps
  ! PILOTAGE EN TEMPS
  ! Si on passe en pilotage en temps, il faut
  ! de-commenter les DEUX lignes ci dessous (sinon istep ne sera pas incremente)
  !step:do while ((get_time() - Temps_initial) < Temps_simu_total)
  !        istep = istep+1

          ! </free
          call start_itimer(id_free)
          call time_increment
          !call fatal_damping_RBDY3(1000) ! Pour annuler la vitesse des corps, a une frequence donnee
          !call partial_damping_RBDY3(2,0.1d0) ! Pour limiter la vitesse des corps, a une frequence donnee

          !!! A utiliser avec parcimonie !!!
          !! Ouverture des fichiers pour 
          !! etudier le comportement statistique des Fg
          !call open_stat_files_ddm(istep)

          !---> Fonction rang0 <---
          call print_step(get_Nstep())
          ! N.B. : la frequence de decoupage en sous-domaine
          !        donne la frequence de detection grossiere
          call set_run_contactor(DDMfreq)
          call Init_entitylist
          call stop_itimer(id_free)
          ! free/>


          ! Nouvelle decomposition en sous-domaines
          if ( RUN_SPSPx() ) then
          ! PILOTAGE EN TEMPS
          !if ((istep==1) .or. ((get_time() - Temps_initial) >= i_freq_ddm * dt_freq_ddm)) then

             ! On ne peut merger les donnees que si des calcul
             ! ont deja ete faits et on ne veut merger les donnees
             ! que lorsqu'une nouvelle repartition en sous-domaine
             ! vas avoir lieu.

             !am: au premier pas de temps tous les corps sont visibles sur tous 
             !   les processus, et on n'a donc pas besoin de faire un 
             !   gather_Xbeg_Vbeg...


             ! </DDM mergage dans sur l'hote
             call start_itimer(id_ddm_merg)
             ! La fonction suivante envoie les deplacements (translations)
             ! et les vitesses au debut du pas de temps + l'orientation
             ! du repere principal d'inertie dans la configuration 
             ! de detection (calculee durant l'increment step).
             ! Pour la detection grossiere on utilise les coordonnees
             ! au debut pas du pas de temps, a partir desquelles on calcule
             ! les deplacements dans la configuration de detection.
             if (istep > 1) then
                call gather_X_V_begin
                call stock_Fg_liste_old ! Stockage pour recuperation
                                        ! des Fg du pas prededent.
             end if
             call stop_itimer(id_ddm_merg)
             ! DDM mergage dans le sdm de l'hote/>


             ! </DDM Creations des domaines
             call start_itimer(id_crea_dom)
             ! On rend visible l'ensemble des RBDY3 pour le
             ! processus hote
             call set_visibility_4all_in_DDM(0)

             !----------------------------------------
             ! Routine effectuant les operations :
             !   * detection grossiere (methode des boites generique)
             !   * decoupage en sous-domaines
             !   * creation du body_particip
             !   * split du graphe de contact
             !   * compute_interface
             !   * migration_treatment
             !---> Fonction rang0 <---
             call creation_domaines

             !---> Fonction rang0 <---
             call erase_splitted_rough
             !---> Fonction rang0 <---
             call erase_rough_contact

             call stop_itimer(id_crea_dom)
             ! DDM Creations des domaines/>


             ! </DDM Scatter Creations des domaines
             call start_itimer(id_scatt_cr_d)
             call scatter_creation_domaines ! Scatters et mise en donnee des donnees
                                            ! calculees dans creation_domaines
             call stop_itimer(id_scatt_cr_d)
             ! DDM Scatter Creations des domaines/>


             ! </DDM Topo + prep + bdd locale (PUM, etc...)
             call start_itimer(id_bdd_locale)
             ! Construction des interfaces elementaires et de la 
             ! topologie de communication (InLoop) non-structuree
             call compute_shared_interfaces
             ! Etapes generiques du probleme d'interface
             call prep_compute_DF_gamma
             ! Allocation de la bdd pour les echanges MPI InLoop
             call decentralise_prep_exchange_inloop

             if (istep > 1) then
                ! Mise en donnee des nouveaux arrivants
                call fix_migrants
             end if

             ! Applique les masques de visibilites par sous-domaines
             call set_visibility_4all_in_DDM

             ! Calcul et set des masses modifiees
             if (NSCDD) call set_modified_mass_in_DDM
       
             call stop_itimer(id_bdd_locale)
             ! DDM Topo + prep + bdd locale (PUM, etc...)/>

          end if

          ! </free
          call start_itimer(id_free)
          !am : l'appel a increment_RBDY3 est fait apres la 
          ! decomposition en sous-domaines, pour s'assurer que
          ! les migrants, avec vitesses imposees, sont bien geres
          call increment_RBDY3
          call comp_Fext_RBDY3
          call comp_Fint_RBDY3

          ! Initialisation des Fgamma pour les particules d'interface avec
          ! les Fgamma de la fin du pas de temps precedent (particules restees 
          ! dans une meme liste de RBDY3 d'interface par sous domaine).
          if (istep > 1) call set_F_gamma

          ! Chargement triaxial (calcul et application des forces aux parois)
          ! Rq : valeur de la pression imposee a mettre en dur dans la routine
          ! suppose que les parois sont des clusters de spheres et
          ! sont rangee en dernier dans le bodies.dat suivant l'ordre :
          ! Up, Down, Left, Rigth, Front, Rear !!
          !call triaxial_loading_DDM(1)

          ! Realise compute_configurationTT_RBDY3, pour une raison inconnue...
          call comp_free_vlocy_RBDY3
          !call Init_entitylist
          call stop_itimer(id_free)
          ! free/>


          ! </detect
          call start_itimer(id_select)
          if ( check_SPSPx() ) then
             call coor_prediction_SPSPx
             ! Mise en donnee dans mod_SPSPx du graphe de contact grossier
             ! N.B. elle n'est necessaire que si on a refait
             ! une decomposition en sdm au debut du pas
             if (pilotage_en_temps) then
                if ((istep==1) .or. ((get_time() - Temps_initial) >= i_freq_ddm * dt_freq_ddm)) then
                   i_freq_ddm = i_freq_ddm + 1.d0
                   call set_interactions_to_rough_in_DDM
                end if
             else
                if ( RUN_SPSPx() ) call set_interactions_to_rough_in_DDM
             end if
          end if

          if ( check_SPSPx() ) call compute_contact_SPSPx
          ! Recueration dans le module DDM de la liste des
          ! contacts tagges 'INTRF'.
          call get_list_INTRF_in_DDM
          call stop_itimer(id_select)
          ! detect />


          !</ nlgs prep
          call start_itimer(id_nlgs_prep)
          if ( check_SPSPx() ) call recup_rloc_SPSPx
          call set_nlgs_parameter(CVTYPE, tol, RELAX)
          if (solving_scheme .eq. 'ELG') then
             !ELG
             call prep_nlgs(.FALSE.)
          elseif (solving_scheme .eq. 'SDL') then
             !SDL        
             call prep_nlgs(.TRUE.)
          else
             call faterr("DKDK_standalone_DDM_DECENT",&
             "solving_scheme doit etre SDL ou ELG !")
          endif
          call stop_itimer(id_nlgs_prep)
          ! nlgs prep />

          iter = 0

          !---------------------------------------------------------------
          ! Boucle DDM
     ddm2:do iddm=1,itloop2

        ddm1:do ik=1,itloop1

                iter = iter + 1

                !</solve_nlgs
                call start_itimer(id_nlgs_iter)
                call solve_nlgs(1)
                call stop_itimer(id_nlgs_iter)
                ! solve_nlgs />

 
                if (Nsdm == 1) cycle
                !</RnodHRloc
                call start_itimer(id_nlgs_Rnod)
                ! Calcul des torseurs de reaction de contact sur
                ! les corps (stockage dans Iaux),
                ! pour les seuls corps d'interface
                call RnodHRloc_list_in_DDM
                call stop_itimer(id_nlgs_Rnod)
                ! RnodHRloc/>


                if (NSCDD) then 
                   !-------------------------------------------
                   ! Calcul des vitesses (dans Vaux a partir des 
                   ! reactions stockees dans Iaux),
                   ! pour les seuls corps d'interface
                   ! </nlgs_Compt
                   call start_itimer(id_nlgs_Compt)
                   call comp_V_list_RBDY3_in_DDM
                   call stop_itimer(id_nlgs_Compt)
                   ! nlgs_Compt/>

                   ! Echanges decentralises des torseurs
                   ! cinetiques des corps d'interface
                   ! ps: timers internes au module ddm
                   call exchange_interface('Vaux_')

                   ! </DDM XDF=[|V|]
                   call start_itimer(id_sol_intrf)
                   call decentralise_compute_DF_gamma 
                   call stop_itimer(id_sol_intrf)
                   ! DDM XDF=[|V|]/>

                   ! </DDM vfree
                   call start_itimer(id_Vfree_list) 
                   !-------------------------------------------
                   ! Calcul de la nouvelle vitesse libre (au sens de l'absence de contact),
                   ! tenant compte de l'actualisation des F_gamma
                   ! (F_ext <- F_ext + delta F_gamma).
                   call comp_Vfree_Fg_list_RBDY3_in_DDM
                   call stop_itimer(id_Vfree_list) 
                   ! DDM vfree/>

                   call start_itimer(id_nlgs_vfree) 
                   ! Execution des parties generiques de prep_nlgs
                   !   * Envoi des vitesses libres globales dans 
                   !     les reperes locaux de chaque contact,
                   !     pour les seuls contacts impliquant un corps d'interface
                   call compute_local_free_vlocy_in_DDM
                   call stop_itimer(id_nlgs_vfree)
                   ! DDM vfree/>
                else
                   ! </nlgs_Compt
                   call start_itimer(id_nlgs_Compt)
                   call nullify_Iaux_for_vlc_drv_dof_RBDY3_in_DDM
                   call stop_itimer(id_nlgs_Compt)
                   ! nlgs_Compt/>

                   ! Echanges decentralises des torseurs
                   ! steriques des corps d'interface
                   ! ps: timers internes au module ddm
                   call exchange_interface('Iaux_')

                   call start_itimer(id_sol_intrf)
                   call compute_Rddm_in_DDM
                   call stop_itimer(id_sol_intrf)

                   ! </DDM vfree
                   call start_itimer(id_Vfree_list) 
                   !-------------------------------------------
                   ! Calcul de la nouvelle vitesse libre (au sens de l'absence de contact),
                   ! tenant compte de l'actualisation des F_gamma
                   ! (F_ext <- F_ext + delta F_gamma).
                   call comp_Vfree_Rddm_list_RBDY3_in_DDM
                   call stop_itimer(id_Vfree_list) 
                   ! DDM vfree/>

                   call start_itimer(id_nlgs_vfree) 
                   ! Execution des parties generiques de prep_nlgs
                   !   * Envoi des vitesses libres globales dans 
                   !     les reperes locaux de chaque contact,
                   !     pour les seuls contacts impliquant un corps d'interface
                   call compute_local_free_vlocy_in_DDM
                   call stop_itimer(id_nlgs_vfree)
                   ! DDM vfree/>
                end if
                !-------------------------------------------

             end do ddm1

             ! </NLGS check
             call start_itimer(id_nlgs_check) 
             !am: calcul de la convergence avec reduction des normes
             !    calculees dans chaque sdm
             call check_convergence_in_DDM(is_converged4all)
             if (is_converged4all) exit 
             call stop_itimer(id_nlgs_check)
             ! NLGS Check/>

          end do ddm2

          !!! A utiliser avec parcimonie !!!
          ! Ecriture des statistiques sur les FG
          !call write_stat_FG_CV

          iterTT=iterTT+iter
          if (bavardage /= 0) call print_iter(0,iter)
          ! Affichage du nombre d'iterations effectuees
          !---> Fonction rang0 <---
          !call print_iter(0,iter)


          !</nlgs_fin  
          call start_itimer(id_nlgs_post1)
          call RnodHRloc_nlgs
          call stop_itimer(id_nlgs_post1)

          call start_itimer(id_nlgs_post2)
          call solve_nlgs(3)
          call Nullify_EntityList_nlgs
          call update_tact_behav_nlgs
          call stop_itimer(id_nlgs_post2)

          call start_itimer(id_nlgs_post3)
          if ( check_SPSPx() ) call stock_rloc_SPSPx 
          call stop_itimer(id_nlgs_post3)
          ! nlgs fin/>

          ! * version "avec recollement propre et intelligent"
          !</Updt
          call start_itimer(id_updt)
          ! Calcul des vitesses dans chaque sdm
          ! N.B.: les grains d'interface n'ont pas necessairement
          !       la meme dans tous les sdm
          call comp_V_RBDY3
          call stop_itimer(id_updt)
          ! Updt/> 


          ! </ddm recollement de l'interface
          ! 1- Echanges des vitesses des grains d'interfaces
          call exchange_interface('V____')

          ! 2- Calcul de la moyenne des vitesses des grains d'interface
          call start_itimer(id_ddm_recoll)
          call decentralise_fix_V_interface
          call stop_itimer(id_ddm_recoll)
          ! ddm recollement de l'interface/>
 

          !</Updt
          call start_itimer(id_updt)
          ! Calcul des deplacements et des orientations
          ! des reperes principaux d'inertie
          ! a partir des vitesses recollees, dans chaque sdm
          call comp_X_localFrame_RBDY3

          call Updt_time_begin
          call update_dof_RBDY3 
          call stop_itimer(id_updt)
          ! Updt/>


          ! </POST + OUT + DISP + LAST
          call start_itimer(id_post)
          !-------------------------------------------

          if (pilotage_en_temps) then
             if ((get_time() - Temps_initial) >= i_freq_write * dt_freq_write) then
                i_freq_write = i_freq_write + 1.d0 
                call write_OUT_in_DDM
             end if
             ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
             if ((get_time() - Temps_initial) >= i_freq_last * dt_freq_last) then
                i_freq_last = i_freq_last + 1.d0
                call write_LAST_in_DDM
                call write_itimer
             end if
          else 
             ! Sorties postpro post-mortem
             if (MODULO(istep, freq_write)==0) call write_OUT_in_DDM
             ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
             if (MODULO(istep, freq_last)==0) then
                call write_LAST_in_DDM
                call write_itimer
             end if
          end if

          ! Sorties postpro during computation
          if (MODULO(istep, freq_postpro)==0) then
             call postpro_during_in_DDM(iter)

             if (pilotage_en_temps) then
                call get_violation_values_in_DDM(mean_violation,max_violation)

                ! Si la violation moyenne ou maximale devient critique
                if ((mean_violation > 0.009d0 * min_radius) .or. &
                     (max_violation > 0.08d0 * min_radius)) then 

                   ! Si la config. n'est pas correcte, que le pas de temps est superieur a 0.5*TimeStep_ini 
                   ! et que la violation max ne diminue pas, on divise par 2 le pas de temps courant.
                   if ((TimeStep > TimeStep_ini*0.6d0) .and. (evol_violation >= 0.d0)) then
                      TimeStep = TimeStep * 0.5d0
                      call set_time_step(TimeStep)
                   end if

                else
                  ! Config correcte
                   config_correcte = config_correcte + 1
                   ! Si la config. est correcte depuis "suffisamment" longtemps, que 
                   ! le pas de temps est inferieur a 4*TimeStep_ini et que la violation max diminue,
                   ! on multiplie par 2 le pas de temps courant.
                   if ((config_correcte >= 100) .and. (TimeStep < 2.1d0*TimeStep_ini) &
                       .and. (evol_violation < 0.d0)) then
                      TimeStep = TimeStep * 2.d0
                      call set_time_step(TimeStep)
                      config_correcte = 0
                   end if
                end if
                evol_violation = max_violation - old_max_violation
                old_max_violation = max_violation
             end if
          end if

          call clean_writing_flags
          !-------------------------------------------
          call stop_itimer(id_post)
          ! POST + DISP + OUT + LAST/>
         
       end do step

       ! </post
       call start_itimer(id_post)
       !-------------------------------------------
       call write_LAST_in_DDM
       call postpro_last_in_DDM(nb_time_steps,freq_postpro)
       call add_dof2bodies_in_DDM
       !-------------------------------------------
       call stop_itimer(id_post)
       ! post />


       !---> Fonction rang0 <---
       call print_iter(1,iterTT)

       call write_itimer

       ! Desactivation de l'environnement MPI
       call stop_MPI_time
       call mpi_finalize_process

       ! nettoyage de la memoire encore allouee
       call clean_ddm

end program SPSP_standalone_DDM_MPI_DECENT
