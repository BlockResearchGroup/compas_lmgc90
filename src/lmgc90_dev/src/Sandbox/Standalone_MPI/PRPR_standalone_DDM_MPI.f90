program PRPR_standalone_DDM_MPI

  use DDM_MPI_3D

  use utilities 

  USE RBDY3, only: &
       increment_RBDY3, &
       comp_dof_RBDY3, &
       comp_V_RBDY3, &
       comp_X_localFrame_RBDY3, &
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
       write_out_driven_dof_RBDY3, &
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
       get_volume, &
       copy_bodies_RBDY3, &
       set_visibility_4all_RBDY3, &
       !TEST
       get_X, &
       get_Xbegin, &
       get_V, &
       get_Vbegin, &
       get_coor, &
       get_visible, &
       compute_configurationTT_RBDY3, &
       !obsolete:compute_frameTT
       check_bounds_RBDY3

  USE bulk_behaviour

  USE tact_behaviour

  USE POLYR,ONLY:&
      read_bodies_POLYR, &
      get_nb_POLYR, &
      get_mean_radius_POLYR, &
      move_POLYR

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

  USE PRPRx,ONLY: &
       get_nb_PRPRx,&
       stock_rloc_PRPRx, &
       recup_rloc_PRPRx, &
       compute_box_PRPRx, &
       read_ini_Vloc_Rloc_PRPRx, &
       write_xxx_Vloc_Rloc_PRPRx, &
       coor_prediction_PRPRx, &
       creation_tab_visu_PRPRx, &
       !am: on utilise la detection avec un plan separateur
       wcp_compute_contact_PRPRx, &
       display_prox_tactors_PRPRx, &
       RUN_PRPRx, &
       CHECK_PRPRx, &
       get_write_Vloc_Rloc_PRPRx, &
       !am : focntions pour parametrer la methode de detection
       set_f2f_tol_PRPRx, &
       set_cundall_iteration_PRPRx, &
       set_shrink_polyr_faces_PRPRx, &
       set_size_factor_polyr_PRPRx

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
       active_diagonal_resolution, &
       compute_local_free_vlocy

  USE timer, ONLY: &
       initialize_utimer, &
       write_utimer, &
       get_new_utimer_ID, &
       start_utimer,stop_utimer 

  implicit none



integer :: i,istep,ib,ik,iconv1,iconv2,iter,iterTT
! indices des timers
integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_crea_dom,id_detect_fine, &
           id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_updt,id_post, &
           id_nlgs_check, id_ddm_merg, id_nlgs_Rnod, id_nlgs_Compt, id_sol_intrf, &
           id_nlgs_vfree, id_ddm_recoll, id_scatter_DFg, id_Vfree_list

  real(kind=8)     :: TimeStep         ! Pas de temps
  real(kind=8)     :: Theta            ! parametre de la theta-methode
  integer          :: nb_time_steps

  integer          :: DDMfreq
  integer          :: Nsdm1, Nsdm2, Nsdm3 ! nombre de sous-domaines suivant chaque direction
  integer          :: Nsdm         ! nombre total de sous-domaines

  CHARACTER(len=5) :: CVTYPE ! type de norme utilisee pour tester la convergence du gauss-Seidel
  real(kind=8)     :: RELAX  ! parametre de relaxation pour le Gauss-Seidel
  real(kind=8)     :: TOL    ! tolerance pour le Gauss-Seidel
  !integer          :: itloop1, itloop2 ! nombre d'iteration des boucles internes et externes du Gauss-Seidel
  integer          :: itloop1, itloop2  ! Nombre de sous-itérations DDM et nombre max d'itérations 
                                       ! avec check de covergence

  integer          :: freq_display     ! frequece d'ecriture des fichier de visualisation
  integer          :: freq_write       ! frequece d'ecriture des fichier DOF et VlocRloc
  integer          :: freq_postpro     ! frequece d'ecriture des fichier POSTPRO
  real(kind=8)     :: RefRadius        ! Rayon de reference

  real(kind=8)     :: Vref             ! Vitesse de référence du problème considéré
 
  real(kind=8)     :: egluing          ! Erreur de recollement des grains d'interface normalisée 

  logical, dimension(:), allocatable :: mask ! masque utilise pour donner la visibilite des corps

  integer          :: nb_RBDY3         ! nombre total de RBDY3 (lus  + copies)
  integer          :: nb_POLYR         ! nombre total de POLYR (lues + copies)

  integer          :: ERR              ! pour recuperer le code d'erreur renvoye lors de la lecture du fichier

  ! indices de boucle
  integer          :: isdm         ! indice d'itération de la boucle sur les sous-domaines
  integer          :: iddm         ! indice d'itération ddm

  !real(kind=8)     :: Xleft_bound, Xright_bound ! Frontières gauche, droite de la boite anglobante 
  !                                              ! On contraint donc le découpage sous-jacent suivant x.

  !---------------!
  !      MPI      !
  !---------------!
  logical          :: is_converged4all

       ! Initialisation de l'environnement MPI
       call init_MPI

       ! on utilise le working directory associe au sdm pour les ecriture (DOF.OUT, VlocRloc.OUT, ...)
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
       !    * du rayon de reference pour la visu
       !READ(10,*) RefRadius
       !!    * de la frequence d'ecriture des fichiers de visualisation
       READ(10,*) freq_display
       !!    * de la frequence d'ecriture des fichiers de sortie
       READ(10,*) freq_write
       !     * de la frequence d'ecriture des fichiers de postpro
       READ(10,*) freq_postpro
       !    * de la grequence de detection/decomposition en sous-domaines
       READ(10,*) DDMfreq
       !    * du nombre de sous-domaines dans chaque direction
       READ(10,*) Nsdm1, Nsdm2, Nsdm3
       !    * du type de norme de convergence et de la tolerance pour le Gauss-Seidel
       READ(10,'(A5,E10.1)') CVTYPE,TOL
       !    * du parametere de relaxation
       READ(10,*) RELAX
       !    * du nombre de boucle avant test de la convergence et du nombre maximum de paquets d'iterations de Gauss-Seidel
       READ(10,*) itloop1, itloop2

       ! fermeture du fichier de parametres
       CLOSE(10)


       !---> Fonction rang0 <---
       call print_info(nb_time_steps,TimeStep,Theta,freq_DISPLAY,freq_write,DDMfreq, &
                        Nsdm1,Nsdm2,Nsdm3,CVTYPE,TOL,RELAX,itloop1,itloop2)
       ! fin affichage des donnees lues

       !am: on initialise les donnees non lues
       !   * le rayon de reference pour la visu
       RefRadius=1.d0

       CALL initialize_utimer
                                       !12345678901234567890
       id_read       = get_new_utimer_ID('READ                ')
       id_free       = get_new_utimer_ID('FREE                ')
       id_ddm_merg   = get_new_utimer_ID('DDM Merge           ')
       id_crea_dom   = get_new_utimer_ID('Creation_domaines   ')
       id_select     = get_new_utimer_ID('Select Prox Tactors ')
       id_nlgs_prep  = get_new_utimer_ID('NLGS PREP           ')
       id_nlgs_iter  = get_new_utimer_ID('NLGS ITER           ')
       id_nlgs_Rnod  = get_new_utimer_ID('NLGS RnodHRloc      ')
       id_nlgs_Compt = get_new_utimer_ID('NLGS Compt_dof_list ')
       id_sol_intrf  = get_new_utimer_ID('X DF = [|V|]        ')
       !id_scatter_DFg= get_new_utimer_ID('SCATTER DFg         ')
       id_Vfree_list = get_new_utimer_ID('NLGS cmpt_Vfree_list')
       id_nlgs_vfree = get_new_utimer_ID('NLGS cmpt_loc_free_v')
       id_nlgs_check = get_new_utimer_ID('NLGS CHECK          ')
       id_nlgs_post1 = get_new_utimer_ID('NLGS POST 1         ')
       id_nlgs_post2 = get_new_utimer_ID('NLGS POST 2         ')
       id_nlgs_post3 = get_new_utimer_ID('NLGS POST 3         ')
       id_ddm_recoll = get_new_utimer_ID('DDM Recollement     ')
       id_updt       = get_new_utimer_ID('Update              ')
       id_post       = get_new_utimer_ID('POST                ')

       !-------------------------------------------
       ! Partie propre à la décomposition de domaine
       !---> Fonction rang0 <---
       Nsdm = init_dd(Nsdm1, Nsdm2, Nsdm3)
       !-------------------------------------------

!!!!!!!
! </read
       call start_utimer(id_read)
       call disable_logmes
       !call set_with_experimental_dev

       !am: indispensable pour importer les polyedres!
       call init_dimension("3D        ")

       ! Pour l'écriture des DOF, active la prise en compte de
       ! la visibilité des corps dans la fonction write_out_dof.
       call set_skip_invisible_RBDY3

       CALL set_time_step(TimeStep)        
       CALL init_theta_integrator(Theta)       

       CALL read_in_bodies_RBDY3(1)
       CALL update_existing_entities_RBDY3

       CALL read_in_dof_ol(0)
       CALL read_in_dof_RBDY3(0)

       CALL read_in_driven_dof_RBDY3

       call stop_utimer(id_read)
! read/>

       ! </DDM
       call start_utimer(id_crea_dom)
       !-------------------------------------------
       ! récupération du nombre de corps
       nb_RBDY3 = get_nb_RBDY3()

       ! Toutes les particules sont maintenant visibles
       ! N.B. : necessaire pour que les CL soient calculees sur toutes les copies des corps lors de l'increment_step 
       call set_visibility_4all_RBDY3((/ (.true., i=1, nb_RBDY3) /),nb_RBDY3)
       !-------------------------------------------
       call stop_utimer(id_crea_dom)
       ! DDM/>

       ! </read
       call start_utimer(id_read)

       CALL read_bodies_POLYR

       !-------------------------------------------
       ! recuperation du nombre de polyedres
       nb_POLYR = get_nb_POLYR()
       !-------------------------------------------

       if (nb_POLYR == 0) call faterr("PRPR_standalone_DDM", "l'echantillon ne contient aucune polyedre!")

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
       if ( check_PRPRx() ) CALL read_ini_Vloc_Rloc_PRPRx(0)

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

       if ( check_PRPRx() ) CALL compute_box_PRPRx

       CALL comp_mass_RBDY3

       !am : parametres propres a la detection polyedre-polyedre
       !   * on utilise la detection face a face
       call set_f2f_tol_PRPRx(1.d-3)
       !   * on utilise la detection "a la Cundall"
       !call set_cundall_iteration_PRPRx(200)
       !   *  avec un shrink
       CALL set_shrink_polyr_faces_PRPRx(5.d-2)
       !CALL set_shrink_polyr_faces_PRPRx(1.d-3)
       !   * on surdimensionne le tableau des contacts
       call set_size_factor_polyr_PRPRx(10)

       !am : on utilise la resolution diagonale
       call active_diagonal_resolution

       call stop_utimer(id_read)
! read/>

       ! </DDM
       !-------------------------------------------
       ! Partie propre à la décomposition de domaine
       !
       !   * allocation des tableaux communs à tous les processus,
       call new_dd_slave(nb_RBDY3, ntact_POLYR_=nb_POLYR)
       call allocations_fantome_slave
      
       !   * allocation des tableaux du processus hôte.
       !---> Fonction rang0 <---
       call new_dd_host 

       call init_postpro_in_DDM(solv_info_=.true., viol_evol_=.true., &
            triax_compac_=.false., sdm_info_=.true.,mean_sdm_info_=.true., &
            sample_info_=.true., mean_sample_info_=.true.)

       call set_skip_display_cluster(.false.)
       !-------------------------------------------
       ! DDM/>

       iterTT=0

  step:do istep=1,nb_time_steps
! </free
          call start_utimer(id_free)

          CALL  time_increment                  !Updt_time

          !---> Fonction rang0 <---
          call print_step(get_Nstep())

          !am : on calcule le repere de detection (localFrameTT), en vue de la prochaine decomposition en sous-domaines
          call compute_configurationTT_RBDY3
          !obsolete: 
          !call compute_frameTT

          !am : on met a jour l'orientation des contacteurs polyedres 
          call move_POLYR

          call stop_utimer(id_free)
! free/>

          ! N.B. : la frequence de decoupage en sous-domaine donne la frequence de detection
          call set_run_contactor(DDMfreq)

          ! calcul de la nouvelle decomposition en sous-domaines
          if ( RUN_PRPRx() ) then

             ! </DDM mergage dans le sdm de l'hôte

             call start_utimer(id_ddm_merg)

             if (istep == 1) then

                ! Copie des boites englobantes de chaque contaceur polyedre
                ! dans les tableaux du  maitre afin de pouvoir faire le
                ! test en norme de Manhattan lors de la detection grossiere 
                ! au premier pas de temps.
                !---> Fonction rang0 <---
                call init_POLYR_TT 

             else
                ! On ne peut merger les données que si des calculs
                ! ont déjà été faits et on ne veut merger les données
                ! que lorsqu'une nouvelle répartition en sous-domaines 
                ! vas avoir lieu.

                !am: au premier pas de temps tous les corps sont visibles sur tous 
                !   les processus, et on n'a donc pas besoin de faire un 
                !   gather_Xbeg_Vbeg...


                ! La fonction suivante envoie les deplacements (translations)
                ! et les vitesses au debut du pas de temps + l'orientation
                ! du repere principal d'inertie dans la configuration 
                ! de detetcion (calculee durant l'increment step).
                ! Pour la detetcion grossiere on utilise les coordonnees
                ! au debut pas du pas de temps, a partir desquels on calcule
                ! les deplacements dans la configuration de detection.
                call gather_X_V_begin
            
                ! envoi des boites englobantes de chaque contaceur polyedre
                ! sur le maitre afin de pouvoir faire le test en norme de 
                ! Manhattan lors de la detection grossiere 
                call gather_POLYR_TT

                call stock_Fg_liste_old ! Stockage pour récupération
                                        ! des Fg du pas précédent.
             end if


             ! La fonction suivante envoie les positions des sommets des polyedres dans la configuration
             ! de detection

             call stop_utimer(id_ddm_merg)

             ! DDM mergage dans le sdm de l'hôte/>

             ! </DDM Créations des domaines
             call start_utimer(id_crea_dom)

             ! On rend visible la première tranche en vue de la nouvelle répartition en sous-domaines
             ! => récupération des bonnes coordonnées des centre d'intertie des corps
             ! processus hôte (-->0)
             call set_visibility_4all_in_DDM(0)
             !----------------------------------------
             ! Partie propre à la décomposition de domaine
             !   * détection grossière (méthode des boites générique)
             !   * decoupage en sous-domaines
             !   * création du body_particip
             !   * shift (ou split) du graphe de contact
             !   * compute_modified mass
             !   * compute_interface
             !   * migration_treatment
             !---> Fonction rang0 <---
             call creation_domaines

             !---> Fonction rang0 <---
             call erase_splitted_rough
             !---> Fonction rang0 <---
             call erase_rough_contact

             call scatter_creation_domaines ! Scatters et préparations de la BDD
                                            ! calculée dans creation_domaines

             if (istep > 1) then
                ! Mise en donnée des nouveaux arrivants
                call fix_migrants
             end if

             ! Applique les masques de visibilités par sous-domaines
             call set_visibility_4all_in_DDM

             ! Calcul et set des masses modifiées par les différents processus
             call set_modified_mass_in_DDM

             call stop_utimer(id_crea_dom)
             ! DDM Créations des domaines/>
          end if

          ! </free
          call start_utimer(id_free)

          ! * gestion des corps sortant des limites :
          ! on rend invisible des corps ayant depasses les limites
          ! a la fin du pas de temps precedent
          if (check_bounds_RBDY3()) call out_of_bounds_RBDY3

          CALL increment_RBDY3
          CALL comp_Fext_RBDY3
          CALL comp_Fint_RBDY3

          ! Initialisation des Fgamma pour les particules d'interface avec
          ! les Fgamma de la fin du pas de temps précédent (particules restées 
          ! dans une même liste de RBDY3 d'interface par sous domaine).
          if (istep > 1) then
             call set_F_gamma
          end if

          CALL comp_free_vlocy_RBDY3

          CALL Init_entitylist

          call stop_utimer(id_free)
          ! free/>

! </detect
          call start_utimer(id_select)

          ! N.B. : la frequence de decoupage en sous-domaine donne la frequence de detection

          if ( check_PRPRx() ) then
             CALL coor_prediction_PRPRx

             ! Mise en donnée dans mod_PRPRx du graphe de contact grossier
             ! N.B. elle n'est necessaire que si on a refait une decomposition en sdm au debut du pas
             if ( RUN_PRPRx() ) call set_interactions_to_rough_in_DDM

             !am : on utilise la methode detection utilisant un plan separateur
             CALL wcp_compute_contact_PRPRx
          end if

          ! récupération dans le module DDM de la liste des contacts taggés 'INTRF'.
          CALL get_list_INTRF_in_DDM

          call stop_utimer(id_select)
! detect />

!</ nlgs
          call start_utimer(id_nlgs_prep)

          if ( check_PRPRx() ) CALL recup_rloc_PRPRx
          !WRITE(*,'(1X,I10,A12)') get_nb_PRPRx(i_recup_tactor),' recup PRPRx'

          CALL set_nlgs_parameter(CVTYPE, tol, RELAX)

!ELG
          CALL prep_nlgs(.FALSE.)
!SDL      
!          CALL prep_nlgs(.TRUE.)

          call prep_exchange_inloop

          call stop_utimer(id_nlgs_prep)

          iter = 0

          !---------------------------------------------------------------
          ! Boucle DDM
     ddm2:do iddm=1,itloop2

        ddm1:do ik=1,itloop1

                iter = iter + 1

                !print *, "---------------------------------"
                !print *, "iter=", iter

                !</solve_nlgs
                call start_utimer(id_nlgs_iter)

                call solve_nlgs(1)

                call stop_utimer(id_nlgs_iter)
                ! solve_nlgs />
   
      
                !</RnodHRloc
                call start_utimer(id_nlgs_Rnod)

                ! calcul des torseurs de reaction de contact sur les corps (stockage dans Iaux),
                ! pour les seuls corps d'interface
                call RnodHRloc_list_in_DDM

                call stop_utimer(id_nlgs_Rnod)
                ! RnodHRloc/>


                ! </nlgs_Compt
                call start_utimer(id_nlgs_Compt)

                ! calcul des vitesses (dans Vaux a partir des reactions stockees dans Iaux),
                ! pour les seuls corps d'interface
                call comp_V_list_RBDY3_in_DDM
    
                ! stockage Vaux/Iaux:
                call gather_V_interface('Vaux_')

                call stop_utimer(id_nlgs_Compt)
                ! nlgs_Compt/>
    
                ! </DDM XDF=[|V|]
                call start_utimer(id_sol_intrf)

                ! Résolution du problème d'interface
                !---> Fonction rang0 <---
                call compute_DF_gamma 

                call stop_utimer(id_sol_intrf)
                ! DDM XDF=[|V|]/>

                ! </DDM vfree
                !call start_utimer(id_scatter_DFg) 
                ! Envoi des DFg aux processus et
                ! actualisation des F_gamma (F_ext <- F_ext + delta F_gamma)
                call scatter_DFg
                !call stop_utimer(id_scatter_DFg) 

                call start_utimer(id_Vfree_list) 
                ! Calcul de la nouvelle vitesse libre (au sens
                ! de l'absence de contact), tenant compte de
                ! l'actualisation des F_gamma
                call comp_Vfree_list_RBDY3_in_DDM
                call stop_utimer(id_Vfree_list) 

                call start_utimer(id_nlgs_vfree) 
                ! Execution des parties generiques de prep_nlgs
                !   * Envoi des vitesses libres globales dans 
                !     les reperes locaux de chaque contact,
                !     pour les seuls contacts impliquant un corps d'interface
                call compute_local_free_vlocy_in_DDM

                call stop_utimer(id_nlgs_vfree)
                ! DDM vfree/>
             end do ddm1

             ! </NLGS check
             call start_utimer(id_nlgs_check) 

             !am: calcul de la convergence avec reduction des normes calculees dans chaque sdm
             call check_convergence_in_DDM(is_converged4all)

             if (is_converged4all) exit 

             call stop_utimer(id_nlgs_check)
             ! NLGS Check/>

          end do ddm2

          iterTT=iterTT+iter
          ! Affichage du nombre d'itérations éffectuées
          !---> Fonction rang0 <---
          !call print_iter(0,iter)

!</nlgs_fin  
          call start_utimer(id_nlgs_post1)

          CALL RnodHRloc_nlgs

          call stop_utimer(id_nlgs_post1)

          call start_utimer(id_nlgs_post2)

          CALL solve_nlgs(3)

          CALL Nullify_EntityList_nlgs

          !am: pour gerer les lois g0
          CALL update_tact_behav_nlgs

          call stop_utimer(id_nlgs_post2)

          call start_utimer(id_nlgs_post3)

          if ( check_PRPRx() ) CALL stock_rloc_PRPRx 

          call stop_utimer(id_nlgs_post3)
! nlgs fin/>

! * version "avec recollement propre et intelligent"
!</Updt
          call start_utimer(id_updt)

          ! calcul des vitesses dans chaque sdm
          ! N.B.: les grains d'interface n'ont pas necessairement la meme dans tous
          !       les sdm
          call comp_V_RBDY3
          call stop_utimer(id_updt)
! Updt/> 

! </DDM recollement de l'interface
          call start_utimer(id_ddm_recoll)
          ! 1- recuperation des vitesses des grains d'interface dans la base de 
          !    donnees du maitre
          call gather_V_interface('V____')

          ! 2- clacul de la moyenne des vitesses des grains d'interface
          !---> Fonction rang0 <---
          call fix_V_interface

          ! 3- envoi des vitesses des grains d'interface moyennees dans la base de 
          !    donnees des esclaves
          call scatter_V_interface
          call stop_utimer(id_ddm_recoll)
! DDM recollement de l'interface/>
 
!</Updt
          call start_utimer(id_updt)
          ! calcul des deplacements et des orientations des reperes principaux d'inertie
          ! a partir des vitesses recollees, dans chaque sdm
          call comp_X_localFrame_RBDY3

          call Updt_time_begin
          call update_dof_RBDY3 

          ! * gestion des corps sortant des limites :
          ! fin de la phase de calcul : on applique les masques de visibilite de la DDM
          ! => les corps ayant depasses les limites redeviennent visibles, afin que les
          !    echanges fonctionnent correctement
          if (check_bounds_RBDY3()) call set_visibility_4all_in_DDM

          call stop_utimer(id_updt)
! Updt/>

! * version "sans recollement"
!! </updt
!          ! call start_utimer(id_updt)
!
!          CALL comp_dof_RBDY3
!          call Updt_time_begin
!          call update_dof_RBDY3
!
!          ! call start_utimer(id_updt)
!! updt/>

           ! call start_utimer(id_post)

! </post
          ! Sorties postpro post-mortem
          IF (MODULO(istep, freq_write)==0) call write_OUT_in_DDM
          ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
          IF (MODULO(istep, 50)==0) call write_LAST_in_DDM
          ! Sorties postpro during computation
          IF (MODULO(istep, freq_postpro)==0) call postpro_during_in_DDM(iter)
          call stop_utimer(id_post)
! post />
          
          call clean_writing_flags

       end do step

       call start_utimer(id_post)
       call write_LAST_in_DDM
       call postpro_last_in_DDM(nb_time_steps,freq_postpro)
       call stop_utimer(id_post)

       !---> Fonction rang0 <---
       call print_iter(1,iterTT)

       CALL write_utimer

       ! Désactivation de l'environnement MPI
       call mpi_finalize_process

       ! nettoyage de la memoire encore allouee
       call clean_ddm

end program PRPR_standalone_DDM_MPI

