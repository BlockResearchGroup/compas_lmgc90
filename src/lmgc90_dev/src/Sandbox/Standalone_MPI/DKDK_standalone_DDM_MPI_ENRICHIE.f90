program DKDK_standalone_DDM_MPI_ENRICHIE

  use DDM_MPI_2D_ENRICHIE

  use LMGC90_MPI

  use utilities 

  USE RBDY2, only:                           &
       increment_RBDY2,                      &
       comp_dof_RBDY2,                       &
       comp_X_RBDY2,                         &
       comp_V_RBDY2,                         &
       comp_dof_RBDY2,                       &
       update_dof_RBDY2,                     &
       comp_free_vlocy_RBDY2,                &
       comp_Fext_RBDY2,                      &
       comp_Fint_RBDY2,                      &
       check_equilibrium_state_RBDY2,        & 
       ghost2invisible_RBDY2,                &
       check_source_point_RBDY2,             &
       out_of_bounds_RBDY2,                  &
       read_in_bodies_RBDY2,                 &
       update_existing_entities_RBDY2,       &
       read_in_dof_RBDY2,                    &
       read_in_driven_dof_RBDY2,             &
       read_behaviours_RBDY2,                &
       write_out_bodies_RBDY2,               &
       write_out_cleared_bodies_RBDY2,       &
       write_xxx_dof_RBDY2,                  &
       write_xxx_Rnod_RBDY2,                 &
       write_out_driven_dof_RBDY2,           &
       comp_mass_RBDY2,                      & 
       set_periodic_data_RBDY2,              &
       resize_RBDY2,                         &
       nullify_X_dof_RBDY2,                  &
       nullify_V_dof_RBDY2,                  &
       init_source_point_RBDY2,              &
       set_init_boundary_RBDY2,              &
       set_data_equilibrium_RBDY2,           &
       add_dof2bodies_RBDY2,                 &
       get_nb_RBDY2,                         &
       get_coor,                             &
       get_inv_mass,                         &
       get_write_Rnod_RBDY2,                 &
       get_write_DOF_RBDY2,                  &
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2,    &
       get_area,                             &
       set_visibility_4all_RBDY2,            &
       set_skip_invisible_RBDY2,             &
       biaxial_def_walls,                    &
       partial_damping_RBDY2,                &
       add_dof2bodies_RBDY2,                 &
       copy_bodies_RBDY2

  USE bulk_behaviour

  USE tact_behaviour

  USE diskx,ONLY:           &
      read_bodies_DISKx,    &
      get_nb_DISKx,         &
      get_DISKx2RBDY2,      &
      get_radius_DISKx,     &
      get_min_radius_DISKx, &
      get_max_radius_DISKx, &
      get_mean_radius_DISKx

  USE overall

  USE DKDKx,ONLY:                   &
       get_nb_DKDKx,                &
       get_nb_recup_DKDKx,          &
       stock_rloc_DKDKx,            & 
       recup_rloc_DKDKx,            &
       smooth_computation_DKDKx,    &
       compute_box_DKDKx,           & 
       read_ini_Vloc_Rloc_DKDKx,    &
       write_xxx_Vloc_Rloc_DKDKx,   &
       set_periodic_data_DKDKx,     &
       coor_prediction_DKDKx,       &
       creation_tab_visu_DKDKx,     &
       compute_contact_DKDKx,       &
       display_prox_tactors_DKDKx,  &
       RUN_DKDKx,                   &
       CHECK_DKDKx,                 &
       get_write_Vloc_Rloc_DKDKx

  USE post2D,ONLY:                  &
       init_GMV_post2D,             &
       update_post2D,               &
       write_GMV_post2D,            &
       set_ref_reac_max_post2D,     &
       set_displ_ampl_post2D,       &
       set_ref_radius_post2D,       &
       set_periodic_data_post2D,    &
       set_extra_material_post2D,   &
       active_GMV,                  &
       put_material_ID_post2D,      &
       GMV_circular_selection,      &
       get_write_gmv_post2D

  USE nlgs,ONLY :                       &
       solve_nlgs,                      &
       comp_check_nlgs,                 &
       write_norm_check_nlgs,           &
       scramble_nlgs,                   &
       quick_scramble_nlgs,             &
       reverse_nlgs,                    &
       bimodal_list_nlgs,               &
       display_check_nlgs,              &
       scale_rloc_nlgs,                 &
       RnodHRloc_nlgs,                  &
       display_rlocn_sum_nlgs,          &
       update_tact_behav_nlgs,          &
       set_nlgs_parameter,              &
       prep_nlgs,                       &
       prep_check_nlgs,                 &
       compute_local_free_vlocy,        &
       put_status_in_DKDKx_nlgs,        &
       Nullify_EntityList_nlgs
 
  USE timer, ONLY:       &
       initialize_itimer, &
       write_itimer,      &
       get_new_itimer_ID, &
       start_itimer,stop_itimer 

  implicit none

  !---------------!
  !    DIVERS     !
  !---------------!
  integer         :: i,errare,iter,iterTT 
  !logical         :: is_Fg_converged
  integer(kind=4) :: nb_iter_CGLP
  !integer(kind=4) :: nb_passes_lineaires

  !----------------------------!
  !      LMGC90 "classique"    !
  !----------------------------!
  integer          :: nb_RBDY2         ! Nombre de RBDY2
  integer          :: nb_DISKx         ! Nombre de disques dupliqués
  integer          :: bavardage        ! 0 active disable_logmess et desactive print_iter
  integer          :: freq_DISPLAY     ! Fréquence d'écriture dans DISPLAY
  integer          :: freq_OUTBOX      ! Fréquence d'écriture dans OUTBOX
  integer          :: freq_postpro     ! frequece d'ecriture des fichiers POSTPRO
  integer          :: freq_last        ! frequece d'ecriture des fichiers LAST
  integer          :: nb_time_steps    ! Nombre de pas de temps désirés
  integer          :: istep            ! Indice du pas de temps courant
  real(kind=8)     :: TimeStep,Theta_  ! Pas de temps et paramètre de la theta méthode
  real(kind=8)     :: TOL,RELAX        ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
  CHARACTER(len=3) :: solving_scheme   ! Type de schema de resolution : SDL ou ELG
  CHARACTER(len=5) :: CVTYPE           ! Type de convergence : QUAD, MAX, QUAD/16, etc...
  integer          :: iconv            ! Valeurs retournées par check_nlgs
  
  !------------------!
  !      NSCDD       !
  !------------------!
  real(kind=8)     :: egluing          ! Erreur de recollement des grains d'interface normalisée 
  integer          :: itloop1,itloop2  ! Nombre de sous-itérations DDM et nombre max d'itérations 
                                       ! avec check de covergence
  integer          :: DDMfreq          ! Fréquence de détection grossière et de répartition en sous-domaines
  integer          :: DDMtype          ! Type de stratégie DDM => NSCDD (1) ou Schwartz (0)
  logical          :: NSCDD=.false.    ! Type de stratégie DDM : NSCDD => NSCDD=.true.  ou Schwartz => NSCDD=.false.
  integer          :: Nsdm1, Nsdm2     ! Nombre de sous-domaines suivant les directions x et y
  integer          :: Nsdm             ! Nombre total de sous-domaines
  integer          :: isdm             ! Indice d'itération des boucles sur les sous-domaines
  integer          :: iddm1,iddm2      ! Indice d'itération de l'agorithme NSCDD
  real(kind=8)     :: left_bound, right_bound ! Frontières gauche et droite des SDM pour l'essai biaxial.
                                              ! On contraint donc le découpage sous-jacent suivant x.
  real(kind=8)     :: d1_eta_, d1_eps_        ! Parametres numeriques pour la version enrichie

  !--------------!
  !    TIMERS    !
  !--------------!
  ! Indices des timers
  integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_crea_dom,id_detect_fine, &
             id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_updt,id_post, &
             id_nlgs_check, id_merg, id_nlgs_Rnod, id_nlgs_Compt, id_sol_intrf, &
             id_nlgs_vfree, id_recoll, id_scatter_DFg, id_Vfree_list,id_CGPA_prep

  !vv&mr: periodic conditions
  logical         :: xperiodic  =.false.
  integer(kind=4) :: ixperiodic = 0
  real(kind=8)    :: xperiode   = 0.d0

  ! Gravite tournante
  real(kind=8), parameter    :: pi_trigo = 3.141592653d0
  real(kind=8), dimension(3) :: imposed_gravity
  real(kind=8)               :: thetag_init, dthetag, thetag


  !---------------!
  !      MPI      !
  !---------------!
  logical                            :: is_converged4all

  ! Initialisation de l'environnement MPI
  call init_MPI

  ! On utilise le working directory associé au sdm
  ! pour les écritures (DOF.OUT, VlocRloc.OUT, ...)
  call set_working_directory_in_DDM
 
  !-------------------------------------------------------------------------------
  ! Lecture du fichier standalone.in
  !-------------------------------------------------------------------------------
  errare = 0

  OPEN(UNIT=1,FILE='standalone.in',STATUS='OLD',IOSTAT=errare)

  if (errare.NE.0) then
     WRITE(*,*) 'file standalone.in is missing'
     STOP
  end if

  READ(1,*) nb_time_steps
  READ(1,*) TimeStep
  READ(1,*) Theta_
  READ(1,*) bavardage
  READ(1,*) freq_DISPLAY
  READ(1,*) freq_OUTBOX
  READ(1,*) freq_postpro
  READ(1,*) freq_last
  ! paramètres de découpages en sous_domaines
  READ(1,*) DDMfreq
  READ(1,*) Nsdm1,Nsdm2
  READ(1,*) DDMtype
  READ(1,*) d1_eta_, d1_eps_
  READ(1,*) ixperiodic,xperiode
  ! paramètre d'itération de la boucle DDM
  READ(1,'(A3)') solving_scheme
  READ(1,'(A5,E10.1)') CVTYPE,TOL
  READ(1,*) RELAX
  READ(1,*) itloop1,itloop2
  CLOSE(1)
  !-------------------------------------------------------------------------------

  if (ixperiodic.eq.1) xperiodic = .true.
  if (DDMtype .eq. 1)  NSCDD     = .true.
  if (DDMtype .ne. 1) call faterr("DKDK_ENRICHIE::","Schwarz pas encore disponible en vesion enrichie")

  !-------------------------------------------------------------------------------
  ! Affichage des données d'entrée
  !---> Fonction rang0 <---
  call print_info(nb_time_steps,TimeStep,Theta_,bavardage,freq_DISPLAY,freq_OUTBOX,freq_postpro,freq_last, &
                        DDMfreq,Nsdm1,Nsdm2,DDMtype,d1_eta_,d1_eps_,ixperiodic,xperiode,solving_scheme,    &
                        CVTYPE,TOL,RELAX,itloop1,itloop2)
  !-------------------------------------------------------------------------------

  ! Initialisation des timers
  call initialize_itimer
                                  !12345678901234567890
  id_read       = get_new_itimer_ID('READ                ')
  id_free       = get_new_itimer_ID('FREE                ')
  id_merg       = get_new_itimer_ID('DDM Merge           ')
  id_crea_dom   = get_new_itimer_ID('Creation_domaines   ')
  id_select     = get_new_itimer_ID('Select Prox Tactors ')
  id_nlgs_prep  = get_new_itimer_ID('NLGS PREP           ')
  id_CGPA_prep  = get_new_itimer_ID('CGPA PREP           ')
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
  id_recoll     = get_new_itimer_ID('DDM Recollement     ')
  id_updt       = get_new_itimer_ID('Update              ')
  id_post       = get_new_itimer_ID('POST                ')

  ! </DDM
  call start_itimer(id_crea_dom)
  !-------------------------------------------
  ! Partie propre à la décomposition de domaine
  !---> Fonction rang0 <---
  Nsdm = init_dd(Nsdm1, Nsdm2)
  call set_enrichissement_parameters(d1_eta_,d1_eps_)
  !-------------------------------------------
  call stop_itimer(id_crea_dom)
  ! DDM/>


  ! </read
  call start_itimer(id_read)
  !-------------------------------------------
  if (bavardage == 0) call disable_logmes

  ! TODO Tester avec et sans 
  !call set_with_experimental_dev

  ! Active la prise en compte de la visibilité des corps 
  ! dans les fonctions write_bodies et write_out_dof.
  call set_skip_invisible_RBDY2

  call set_time_step(TimeStep)        
  call init_theta_integrator(Theta_)       

  call read_in_bodies_RBDY2
  call update_existing_entities_RBDY2
  call read_in_dof_ol
  call read_in_dof_RBDY2
  call read_in_driven_dof_RBDY2
  !-------------------------------------------
  call stop_itimer(id_read)
  ! read/>


  ! </DDM
  call start_itimer(id_crea_dom)
  !-------------------------------------------
  nb_RBDY2 = get_nb_RBDY2()
  ! Toutes les particules sont maintenant visibles
  call set_visibility_4all_RBDY2((/ (.true., i=1, nb_RBDY2) /),nb_RBDY2)
  !-------------------------------------------
  call stop_itimer(id_crea_dom)
  ! DDM/>


  ! </read
  call start_itimer(id_read)
  !-------------------------------------------
  call read_bodies_DISKx
  nb_DISKx = get_nb_DISKx()
  if (nb_DISKx == 0) call faterr("DKDK_standalone_DDM",&
  "l'echantillon ne contient aucun disque!")

  call read_in_bulk_behav 
  !am: lecture du fichier TACT_BEHAV.DAT
  call open_tact_behav_ll()
  call open_see_ll()
  call read_xxx_tact_behav(1)
  call close_tact_behav_ll()
  call close_see_ll()
  ! fin lecture du fichier TACT_BEHAV.DAT
  call read_behaviours_RBDY2   
  call read_in_Vloc_Rloc_ol
  if ( check_DKDKx() ) call read_ini_Vloc_Rloc_DKDKx
  ! ecriture :
  !    * des corps
  call Write_out_bodies_Ol
  call write_out_bodies_RBDY2 
  !    * des conditions limites
  call Write_out_driven_dof_Ol
  call write_out_driven_dof_RBDY2
  !    * des conditions initiales
  call Write_xxx_dof_Ol(2)
  call write_xxx_dof_RBDY2(2,1,get_nb_RBDY2())

  ! Dimensionnement des tableaux de la méthode des boites
  ! une fois pour toute la simulation
  call compute_box_DKDKx

  call comp_mass_RBDY2

  if (xperiodic) then
     print *, xperiode,xperiodic
     call set_periodic_data_RBDY2(xperiode)
     call set_periodic_data_DKDKx(xperiode,xperiodic)
     call set_periodic_data_post2D(xperiode,xperiodic)
  end if

  !-------------------------------------------
  call stop_itimer(id_read)
  ! read/>


  ! </DDM
  call start_itimer(id_crea_dom)
  !-------------------------------------------
  !   * allocation des tableaux communs à tous les processus,
  call new_dd_slave(nb_RBDY2,nb_DISKx,xperiodic,xperiode) 
  !   * allocation a 0, sur les esclaves, des tableaux geres par le maitre
  call allocations_fantome_slave
  !   * allocation des tableaux du processus hôte seul.
  !---> Fonction rang0 <---
  call new_dd_host 

  call init_postpro_in_DDM(solv_info_=.true., viol_evol_=.true., &
       sdm_info_=.true.,mean_sdm_info_=.true.,                   &
       sample_info_=.true., mean_sample_info_=.true.)

  call set_skip_display_cluster(.false.)
  !-------------------------------------------
  call stop_itimer(id_crea_dom)
  ! DDM/>

  iterTT=0

  call start_MPI_time

  ! Gravite tournante
  ! Initialisation de l'orientation de la gravite
  !thetag_init = pi_trigo/8.d0
  thetag_init = 0.d0
  !dthetag     = 2.*thetag_init/real(nb_time_steps,kind=8)
  dthetag     = pi_trigo/(real(nb_time_steps,kind=8))

step:do istep=1,nb_time_steps

     call time_increment                  
     !---> Fonction rang0 <---
     call print_step(get_Nstep())

     ! N.B. : la frequence de decoupage en sous-domaine
     ! donne la frequence de detection
     call set_run_contactor(DDMfreq)

     ! </DDM Domain partitioning
     if ( RUN_DKDKx() ) then

        ! </DDM mergage dans le sdm de l'hôte
        call start_itimer(id_merg)
        !-------------------------------------------
        ! Gathe de Xbeg et Vbeg sur l'hôte
        if (istep > 1) then
           call gather_X_V_begin
           call stock_Fg_liste_old ! Stockage pour récupération
                                   ! des Fg du pas précédent.
        end if
        !-------------------------------------------
        call stop_itimer(id_merg)
        ! DDM mergage dans le sdm de l'hôte/>

        ! </DDM Créations des domaines
        call start_itimer(id_crea_dom)
        !-------------------------------------------
        ! On rend visible l'ensemble des RBDY2 pour le
        ! processus hôte (-->0)
        call set_visibility_4all_in_DDM(0)

        !----------------------------------------
        ! Routine effectuant les opérations :
        !   * détection grossière (méthode des boites générique)
        !   * decoupage en sous-domaines
        !   * création du body_particip
        !   * shift (ou split) du graphe de contact
        !   * compute_modified mass
        !   * compute_interface
        !   * migration_treatment
        !---> Fonction rang0 <---
        call creation_domaines()!left_bound=left_bound, right_bound=right_bound)

        !---> Fonction rang0 <---
        call erase_rough_contact
        !---> Fonction rang0 <---
        call erase_splitted_rough

        call scatter_creation_domaines ! Scatters et leurs préparation de la BDD
                                       ! calculée dans creation_domaines

        call compute_shared_interfaces
        call prep_compute_DF_gamma
        call decentralise_prep_exchange_inloop

        if (istep > 1) then
           ! Mise en donnée des nouveaux arrivants
           call fix_migrants
        end if

        ! Applique les masques de visibilités des différents processus
        call set_visibility_4all_in_DDM

        if (NSCDD) call set_modified_mass_in_DDM
        !-------------------------------------------
        call stop_itimer(id_crea_dom)
        ! DDM Créations des domaines/>
     end if


     ! </free
     call start_itimer(id_free)
     !-------------------------------------------
     call increment_RBDY2

     ! Gravite tournante
     thetag=thetag_init+(real((1-istep),kind=8)*dthetag)
     if (abs(thetag) > (pi_trigo/6.)) thetag = - pi_trigo/6.
     imposed_gravity=9.81d0*(/sin(thetag),-cos(thetag),0.d0/)
     call set_gravity(imposed_gravity)

     call comp_Fext_RBDY2
     call comp_Fint_RBDY2

     call comp_free_vlocy_RBDY2
     call Init_entitylist
     !-------------------------------------------
     call stop_itimer(id_free)
     ! free/>


     ! </detect
     call start_itimer(id_select)
     !-------------------------------------------
     if ( check_DKDKx() ) then
        ! stockage des coordonnees des contacteurs disques dans le module DKDK
        call coor_prediction_DKDKx
        ! Mise en donnée dans mod_DKDKx du graphe de contact grossier
        if ( RUN_DKDKx() ) call set_interactions_to_rough_in_DDM
        ! detection fine
        call compute_contact_DKDKx
        ! récupération dans DDM_MPI de la liste des
        ! contacts taggés 'INTRF'.
        call get_list_INTRF_in_DDM
     end if
     !-------------------------------------------
     call stop_itimer(id_select)
     ! detect/>


     !</prep_nlgs
     call start_itimer(id_nlgs_prep)
     !-------------------------------------------
     if ( check_DKDKx() ) call recup_rloc_DKDKx
     call set_nlgs_parameter(CVTYPE,TOL,RELAX)

     if (solving_scheme .eq. 'ELG') then
        !ELG
        call prep_nlgs(.FALSE.)
     else if (solving_scheme .eq. 'SDL') then
        !SDL        
        call prep_nlgs(.TRUE.)
     else
        call faterr("DKDK_standalone_DDM_DECENT",&
        "solving_scheme doit etre SDL ou ELG !")
     endif
     !-------------------------------------------
     call stop_itimer(id_nlgs_prep)
     ! prep_nlgs/>


     !</prep_CGPA
     call start_itimer(id_CGPA_prep)
     !-------------------------------------------
     ! Constrution (une seule fois par pas de temps, en stockage morse)
     ! de la matrice PAR SOUS-DOMAINE :
     !                                     M_E + l_E * H_E H^T_E
     if (NSCDD) then
        ! Initialisation des Fgamma pour les particules d'interface avec
        ! les Fgamma de la fin du pas de temps précédent (particules restées 
        ! dans une même liste de RBDY2 d'interface par sous domaine).
        call set_F_gamma_NI
     end if
     !-------------------------------------------
     call stop_itimer(id_CGPA_prep)
     ! prep_CGPA/>

     iter = 0
     !nb_passes_lineaires = 0
     !is_Fg_converged = .false.

     !---------------------------------------------------------------
     ! Boucle DDM
ddm2:do iddm2=1,itloop2

        
   ddm1:do iddm1=1,itloop1
           iter = iter + 1


           !</solve_nlgs
           call start_itimer(id_nlgs_iter)
           !-------------------------------------------
           call solve_nlgs(1) ! W_E.r_E - v_E = - v^d_E + v^G_E
           !-------------------------------------------
           call stop_itimer(id_nlgs_iter)
           ! solve_nlgs />

           if (Nsdm /= 1) then
              ! Résolution du problème d'interface
              if (NSCDD .and. (iter==1)) then

                 !nb_passes_lineaires = nb_passes_lineaires + 1

                 call start_itimer(id_CGPA_prep)
                 call put_status_in_DKDKx_nlgs
                 call compute_g2g
                 call MUMPS_analysis_factorization_g2g
                 call stop_itimer(id_CGPA_prep)

                 ! Gradient conjuge parallele, resolvat le pb :
                 !   sum_E A_GE (M_E + l_E * H_E H^T_E)^-1 A^T_GE F_G = 
                 !         sum_E A_GE (M_E + l_E * H_E H^T_E)^-1 * (R_E + l_E * H_E v_E + R^d_E)
                 ! et imposant v^G_E.
                 ! Rq: timers internes a cette fonctions actives dans mod_DDM_MPI_ENRICHIE_2D.f90
                 call Conjugate_Gradient_Linear_Prediction(TOL,nb_iter_CGLP)

                 call close_MUMPS
              end if
           end if

        end do ddm1

        ! </NLGS check
        call start_itimer(id_nlgs_check) 
        !-------------------------------------------
        !am: calcul de la convergence avec reduction des normes calculees dans chaque sdm
        call check_convergence_in_DDM(is_converged4all)
        if (is_converged4all) exit
        !-------------------------------------------
        call stop_itimer(id_nlgs_check)
        ! NLGS Check/>

     end do ddm2


     iterTT=iterTT+iter
     call print_iter(0,iter)
     call print_iter(3,nb_iter_CGLP)
     !call write_lE_Iter_IterLin(iter,nb_passes_lineaires)
     !if (bavardage /= 0) call print_iter(0,iter)
     !if (bavardage /= 0) call print_iter(2,nb_passes_lineaires)

     ! </nlgs fin
     call start_itimer(id_nlgs_post1)
     !-------------------------------------------
     
     call RnodHRloc_nlgs
     call stop_itimer(id_nlgs_post1)
     call solve_nlgs(3)
     call start_itimer(id_nlgs_post2)
     call Nullify_EntityList_nlgs
     call stop_itimer(id_nlgs_post2)
     call start_itimer(id_nlgs_post3)
     if ( check_DKDKx() ) call stock_rloc_DKDKx 
     !-------------------------------------------
     call stop_itimer(id_nlgs_post3)
     ! nlgs fin/>


     !</Updt
     call start_itimer(id_updt)
     !-------------------------------------------
     ! calcul des vitesses dans chaque sdm
     ! N.B.: les grains d'interface n'ont pas necessairement la meme dans tous
     !       les sdm
     call comp_V_RBDY2_in_NI_DDM
     !-------------------------------------------
     call stop_itimer(id_updt)
     ! Updt/> 

     ! </ddm recollement de l'interface
     !-------------------------------------------
     ! 1- recuperation des vitesses des grains d'interface dans la base de 
     !    donnees du maitre
     call exchange_interface('V____')
     ! 2- calcul de la moyenne des vitesses des grains d'interface
     call start_itimer(id_recoll)
     call decentralise_fix_V_interface
     !-------------------------------------------
     call stop_itimer(id_recoll)
     ! ddm recollement de l'interface/>


     !</Updt
     call start_itimer(id_updt)
     !-------------------------------------------
     call comp_X_RBDY2

     call Updt_time_begin
     call update_dof_RBDY2 
     !-------------------------------------------
     call stop_itimer(id_updt)
     ! Updt/>


     ! </post
     ! Sorties postpro post-mortem
     if (istep == 1 .or. MODULO(istep, freq_OUTBOX)==0) call write_OUT_in_DDM
     ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
     if (MODULO(istep, freq_last)==0) then
        call write_LAST_in_DDM
        call write_itimer
     end if
     ! Sorties postpro during computation
     if (MODULO(istep, freq_postpro)==0) call postpro_during_in_DDM(iter)
     call clean_writing_flags
     !-------------------------------------------
     call stop_itimer(id_post)
     ! post />

  end do step

  ! </post
  call start_itimer(id_post)
  !-------------------------------------------
  call write_LAST_in_DDM
  call postpro_last_in_DDM(nb_time_steps,freq_postpro)
  !-------------------------------------------
  call stop_itimer(id_post)
  ! post />

  !---> Fonction rang0 <---
  call print_iter(1,iterTT)

  call write_itimer

  ! Nettoyage de la memoire encore allouee
  call clean_ddm

  ! Désactivation de l'environnement MPI
  call stop_MPI_time
  call mpi_finalize_process

end program DKDK_standalone_DDM_MPI_ENRICHIE
