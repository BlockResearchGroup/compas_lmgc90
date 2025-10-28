program DKDK_standalone_DDM_MPI

  use DDM_MPI_2D

  use utilities 

  USE RBDY2, only:                           &
       increment_RBDY2,                      &
       comp_dof_RBDY2,                       &
       comp_X_RBDY2,                         &
       comp_V_RBDY2,                         &
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
       write_out_bodies_RBDY2,               &
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
       Nullify_EntityList_nlgs
 
  USE timer, ONLY:       &
       initialize_utimer, &
       write_utimer,      &
       get_new_utimer_ID

  implicit none

  !---------------!
  !    DIVERS     !
  !---------------!
  integer :: i,errare,iter,iterTT 

  !----------------------------!
  !      LMGC90 "classique"    !
  !----------------------------!
  integer          :: nb_RBDY2         ! Nombre de RBDY2
  integer          :: nb_DISKx         ! Nombre de disques dupliqués
  integer          :: freq_DISPLAY     ! Fréquence d'écriture dans DISPLAY
  integer          :: freq_OUTBOX      ! Fréquence d'écriture dans OUTBOX
  integer          :: freq_postpro     ! frequece d'ecriture des fichiers POSTPRO
  integer          :: nb_time_steps    ! Nombre de pas de temps désirés
  integer          :: istep            ! Indice du pas de temps courant
  real(kind=8)     :: TimeStep,Theta_  ! Pas de temps et paramètre de la theta méthode
  real(kind=8)     :: TOL,RELAX        ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
  CHARACTER(len=5) :: CVTYPE           ! Type de convergence : QUAD, MAX, QUAD/16, etc...
  integer          :: iconv1, iconv2   ! Valeurs retournées par check_nlgs
  
  !------------------!
  !      NSCDD       !
  !------------------!
  real(kind=8)     :: egluing          ! Erreur de recollement des grains d'interface normalisée 
  integer          :: itloop1,itloop2  ! Nombre de sous-itérations DDM et nombre max d'itérations 
                                       ! avec check de covergence
  integer          :: DDMfreq          ! Fréquence de détection grossière et de répartition en sous-domaines
  integer          :: Nsdm1, Nsdm2     ! Nombre de sous-domaines suivant les directions x et y
  integer          :: Nsdm             ! Nombre total de sous-domaines
  integer          :: isdm             ! Indice d'itération des boucles sur les sous-domaines
  integer          :: iddm1,iddm2      ! Indice d'itération de l'agorithme NSCDD
  real(kind=8)     :: left_bound, right_bound ! Frontières gauche et droite des SDM pour l'essai biaxial.
                                              ! On contraint donc le découpage sous-jacent suivant x.

  !---------------!
  !      MPI      !
  !---------------!
  logical                            :: is_converged, is_converged4all
  !

  !vv&mr: periodic conditions
  logical         :: xperiodic  =.false.
  integer(kind=4) :: ixperiodic = 0
  real(kind=8)    :: xperiode   = 0.d0

  ! Initialisation de l'environnement MPI
  call init_MPI

  ! </read
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
  READ(1,*) freq_DISPLAY
  READ(1,*) freq_OUTBOX
  READ(1,*) freq_postpro
  ! paramètres de découpages en sous_domaines
  READ(1,*) DDMfreq
  READ(1,*) Nsdm1,Nsdm2
  ! paramètre d'itération de la boucle DDM
  READ(1,'(A5,E10.1)') CVTYPE,TOL
  READ(1,*) RELAX
  READ(1,*) itloop1,itloop2
  READ(1,*) ixperiodic,xperiode
  CLOSE(1)

  if(ixperiodic.eq.1) xperiodic=.true.

  !-------------------------------------------------------------------------------
  !---> Fonction rang0 <---
  call print_info(nb_time_steps,TimeStep,Theta_,freq_DISPLAY,freq_OUTBOX,freq_postpro, &
                        DDMfreq,Nsdm1,Nsdm2,CVTYPE,TOL,RELAX,itloop1,itloop2)
  !-------------------------------------------------------------------------------

  !-------------------------------------------
  ! Partie propre à la décomposition de domaine
  !---> Fonction rang0 <---
  Nsdm = init_dd(Nsdm1, Nsdm2)
  !-------------------------------------------
  call disable_logmes

  ! TODO Tester avec et sans 
  !call set_with_experimental_dev

  ! Active la prise en compte de la visibilité des corps 
  ! dans les fonctions write_bodies et write_out_dof.
  call set_skip_invisible_RBDY2

  call set_time_step(TimeStep)        
  call init_theta_integrator(Theta_)       

  call read_in_bodies_RBDY2
  call update_existing_entities_RBDY2
  call read_in_dof_ol(0)
  call read_in_dof_RBDY2(0)
  call read_in_driven_dof_RBDY2
  ! read/>


  ! </DDM
  !-------------------------------------------
  nb_RBDY2 = get_nb_RBDY2()

  ! Toutes les particules sont maintenant visibles
  call set_visibility_4all_RBDY2((/ (.true., i=1, nb_RBDY2) /),nb_RBDY2)
  !-------------------------------------------
  ! DDM/>


  ! </read
  !-------------------------------------------
  call read_bodies_DISKx
  nb_DISKx = get_nb_DISKx()
  if (nb_DISKx == 0) call faterr("DKDK_standalone_DDM", &
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
  call read_in_Vloc_Rloc_ol(0)
  if ( check_DKDKx() ) call read_ini_Vloc_Rloc_DKDKx(0)
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
     call set_periodic_data_RBDY2(xperiode)
     call set_periodic_data_DKDKx(xperiode,xperiodic)
  end if
  ! read/>


  ! </DDM
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
  ! DDM/>

  iterTT=0

step:do istep=1,nb_time_steps

     ! </free
     call time_increment                  

     !---> Fonction rang0 <---
     call print_step(get_Nstep())
     ! free/>

     ! N.B. : la frequence de decoupage en sous-domaine
     ! donne la frequence de detection
     call set_run_contactor(DDMfreq)

     ! </DDM Domain partitioning
     if ( RUN_DKDKx() ) then

        ! On ne peut merger les données que si un pas de temps a déjà été fait (i>1)
        ! et on ne veut merger les données que lorsqu'une nouvelle répartition en
        ! sous-domaines vas avoir lieu (modulo ci-dessus).
        if (istep > 1) then
           call gather_X_V_begin
           call stock_Fg_liste_old ! Stockage pour récupération
                                   ! des Fg du pas précédent.
        end if

        ! On rend visible l'ensemble des RBDY2 pour le
        ! processus hôte (-->0)
        call set_visibility_4all_in_DDM(0)

        !----------------------------------------
        ! "Pseudo-main" du module DDM_MPI_2D :
        !   * caracteristique de la grille sous-jacente
        !   * ventillation des centres d'inertie
        !   * détection grossière des contacts (générique)
        !   * ventillation des centres des contacts
        !   * création du body_particip
        !   * split du graphe de contacts
        !   * compute_interface
        !   * migration_treatment

        !---> Fonction rang0 <---
        call creation_domaines()!left_bound=left_bound, right_bound=right_bound)

        !---> Fonction rang0 <---
        call erase_splitted_rough
        !---> Fonction rang0 <---
        call erase_rough_contact

        call scatter_creation_domaines ! Scatters et leurs préparation de la BDD
                                       ! calculée dans creation_domaines

        call prep_exchange_inloop

        if (istep > 1) then
           ! Mise en donnée des nouveaux arrivants
           call fix_migrants
        end if

        ! Applique les masques de visibilités des différents processus
        call set_visibility_4all_in_DDM

        call set_modified_mass_in_DDM
     end if
     ! DDM Créations des domaines/>


     ! </free
     call increment_RBDY2
     !call  Set_newton_loop(0)
     call comp_Fext_RBDY2
     call comp_Fint_RBDY2
 
     ! Initialisation des Fgamma pour les particules d'interface avec
     ! les Fgamma de la fin du pas de temps précédent (particules restées 
     ! dans une même liste de RBDY2 d'interface par sous domaine).
     if (istep > 1) then
        call set_F_gamma
     end if

     call comp_free_vlocy_RBDY2
     call Init_entitylist
     ! free/>


     ! </detect
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
     ! detect/>


     !</prep_nlgs
     if ( check_DKDKx() ) call recup_rloc_DKDKx
     call set_nlgs_parameter(CVTYPE,tol,RELAX)
     !ELG
     call prep_nlgs(.FALSE.)
     !SDL        
     !call prep_nlgs(.TRUE.)
     ! prep_nlgs/>

     iter = 0

     !---------------------------------------------------------------
     ! Boucle DDM
ddm2:do iddm2=1,itloop2

   ddm1:do iddm1=1,itloop1

           iter = iter + 1

           !</solve_nlgs
           call solve_nlgs(1)
           ! solve_nlgs />

           if (Nsdm > 1) then 
              ! S'il existe plus d'un sous-domaine, alors on va
              ! effectuer les étapes supplémentaires suivantes :
              !  * injection des impulsions de contact dans
              !    l'espace des particules d'interface,
              !  * résolution du problème de recollement,
              !  * projection de la nouvelle vitesse libre
              !    sur les "contacts d'interface".

              !</RnodHRloc
              !call RnodHRloc_nlgs
              ! Restriction de RnodHRloc aux contacts faisant intervenir
              ! au moins une particule d'interface.
              ! stockage Vaux/Raux:
              call RnodHRloc_list_in_DDM
              ! RnodHRloc/>

              ! </nlgs_Compt
              ! stockage Vaux/Raux:
              call comp_V_list_RBDY2_in_DDM

              ! stockage Vaux/Raux:
              call gather_V_interface('Vaux_')
              ! nlgs_Compt/>
  
              ! </DDM XDF=[|V|]
              ! Résolution du problème d'interface
              !---> Fonction rang0 <---
              call compute_DF_gamma 
              ! DDM XDF=[|V|]/>

              ! </DDM vfree
              call scatter_DFg

              ! Calcul de la nouvelle vitesse libre
              ! (au sens de l'absence de contact),
              ! tenant compte de l'actualisation des F_gamma
              ! (F_ext <- F_ext + delta F_gamma).
              ! Execution des parties generiques de prep_nlgs
              !   * projection des vitesses libres dans 
              !     les repères locaux de chaque contact.
              !call compute_local_free_vlocy
              ! Opération duale de RnodHRloc_group : 
              ! calcul de la nouvelle vitesse
              ! libre uniquement pour les contacts dont au moins
              ! une particule est de multiplicité > 1.
              call compute_local_free_vlocy_in_DDM
              ! DDM vfree/>
            end if

        end do ddm1

        !am: calcul de la convergence avec reduction des normes
        !    calculees dans chaque sdm
        call check_convergence_in_DDM(is_converged4all)

        if (is_converged4all) exit 
        ! NLGS Check/>

     end do ddm2

     iterTT=iterTT+iter
     call print_iter(0,iter)
 
     ! </nlgs fin
     call RnodHRloc_nlgs
     call solve_nlgs(3)
     call Nullify_EntityList_nlgs
     if ( check_DKDKx() ) call stock_rloc_DKDKx 
     ! nlgs fin/>


     !</Updt
     ! calcul des vitesses dans chaque sdm
     ! N.B.: les grains d'interface n'ont pas necessairement
     !       la meme dans tous les sdm
     call comp_V_RBDY2
     ! Updt/> 

     ! </DDM recollement de l'interface
     ! 1- recuperation des vitesses des grains d'interface 
     !    dans la base de donnees du maitre
     call gather_V_interface('V____')

     ! 2- clacul de la moyenne des vitesses des grains d'interface
     !---> Fonction rang0 <---
     call fix_V_interface

     ! 3- envoi des vitesses des grains d'interface moyennees 
     !    dans la base de donnees des esclaves
     call scatter_V_interface
     ! DDM recollement de l'interface/>
 
     !</Updt
     call comp_X_RBDY2

     call Updt_time_begin
     call update_dof_RBDY2 
     ! Updt/>


     ! Sorties postpro post-mortem
     IF (MODULO(istep, freq_OUTBOX)==0) call write_OUT_in_DDM
     ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
     if (MODULO(istep, 100)==0) then
        call write_LAST_in_DDM
        call write_utimer
     end if
     ! Sorties postpro during computation
     IF (MODULO(istep, freq_postpro)==0) call postpro_during_in_DDM(iter)
     call clean_writing_flags

  end do step

  call write_LAST_in_DDM
  call postpro_last_in_DDM(nb_time_steps,freq_postpro)

  !---> Fonction rang0 <---
  call print_iter(1,iterTT)

  ! Désactivation de l'environnement MPI
  call mpi_finalize_process

  ! Nettoyage de la memoire encore allouee
  call clean_ddm

end program DKDK_standalone_DDM_MPI
