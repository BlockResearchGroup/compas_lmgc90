program DKDK_standalone_DDM_MDS


  use DDM_MDS_2D

  use utilities 

  USE RBDY2, only:                           &
       increment_RBDY2,                      &
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
       get_write_Rnod_RBDY2,                 &
       get_write_DOF_RBDY2,                  &
       put_invmass_RBDY2,put_precon_W_RBDY2, &
       put_vector_RBDY2,get_vector_RBDY2,    &
       get_area,                             &
       set_visibility_4all_RBDY2,            &
       biaxial_def_walls,                    &
       write_out_bodies_RBDY2,               &
       add_dof2bodies_RBDY2,                 &
       copy_bodies_RBDY2

  USE bulk_behaviour

  USE tact_behaviour

  USE diskx,ONLY:           &
      read_bodies_DISKx,    &
      get_nb_DISKx,         &
      get_DISKx2BDYTY,      &
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
 
  USE POSTPRO,ONLY:                &
       messages_for_users,         &
       init_postpro_command,       &
       start_postpro,              &
       close_postpro_files,        &  
       postpro_during_computation, &
       circular_selection_postpro, &
       selection_translation_postpro

  USE timer, ONLY:       &
       initialize_utimer, &
       write_utimer,      &
       get_new_utimer_ID, &
       start_utimer,stop_utimer 

  implicit none

  !---------------!
  !    DIVERS     !
  !---------------!
  integer :: i,errare,err,iter,iterTT 
  integer :: id_read,id_free,id_nlgs_prep,id_nlgs_iter,id_crea_dom,id_detect_fine,   &
             id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_updt,id_post,    &
             id_nlgs_check, id_ddm_merg, id_nlgs_Rnod, id_nlgs_Compt, id_sol_intrf,  &
             id_nlgs_vfree
  integer :: postpro_unit

  !-------------------------------!
  !    BBD "classique" LMGC90     !
  !-------------------------------!
  integer          :: nb_RBDY2,nbody   ! Nombre de corps dupliqués et réels
  integer          :: nb_DISKx         ! Nombre de disques dupliqués
  integer          :: freq_DISPLAY     ! Fréquence d'écriture dans DISPLAY
  integer          :: freq_OUTBOX      ! Fréquence d'écriture dans OUTBOX
  integer          :: nb_time_steps    ! Nombre de pas de temps désirés
  integer          :: istep            ! Indice du pas de temps courant
  real(kind=8)     :: TimeStep,Theta_  ! Pas de temps et paramètre de la theta méthode
  real(kind=8)     :: TOL,RELAX        ! Valeur numérique de la tolérance choisie, et paramètre de relaxation
  CHARACTER(len=5) :: CVTYPE           ! Type de convergence : QUAD, MAX, QUAD/16, etc...
  integer          :: iconv1, iconv2   ! Valeurs retournées par check_nlgs
  
  !------------------!
  !    BBD NSCDD     !
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
  logical, dimension(:), allocatable :: mask  ! Masque permettant de rendre visible toute la première 
                                              ! tranche lors de la création des sous-domaines

  !vv&mr: periodic conditions
  logical         :: xperiodic  =.false.
  integer(kind=4) :: ixperiodic = 0
  real(kind=8)    :: xperiode   = 0.d0

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
  READ(1,*) DDMfreq
  ! paramètres de découpages en sous_domaines
  READ(1,*) Nsdm1,Nsdm2
  READ(1,'(A5,E10.1)') CVTYPE,TOL
  READ(1,*) RELAX
  ! paramètre d'itération de la boucle DDM
  READ(1,*) itloop1,itloop2
  !-------------------------------------------------------------------------------

  !-------------------------------------------------------------------------------
  ! Ecriture à l'écran des mêmes paramètres
  !-------------------------------------------------------------------------------
  WRITE(*,*) '! loading step            : ',nb_time_steps
  WRITE(*,*) '! TIME STEP               : ',TimeStep
  WRITE(*,*) '! THETA                   : ',Theta_
  WRITE(*,*) '! WRITE DISPLAY FREQUENCY : ',freq_DISPLAY
  WRITE(*,*) '! WRITE OUTBOX FREQUENCY  : ',freq_OUTBOX
  WRITE(*,*) '! FREQUENCE DETECT/REPART : ',DDMfreq
  ! paramètres de découpages en sous_domaines
  WRITE(*,*) '! Nsdm1 et Nsdm2          : ',Nsdm1,Nsdm2
  WRITE(*,*) '! NLGS CHECK TYPE         : ',CVTYPE,TOL
  WRITE(*,*) '! RELAX                   : ',RELAX
  ! paramètre d'itération de la boucle DDM
  WRITE(*,*) '! iteration & more        : ',itloop1,itloop2

  CLOSE(1)
  !-------------------------------------------------------------------------------


  !-------------------------------------------------------------------------------
  ! Initialisation des timers
  !-------------------------------------------------------------------------------
  CALL initialize_utimer
                                  !12345678901234567890
  id_read       = get_new_utimer_ID('READ                ')
  id_free       = get_new_utimer_ID('FREE                ')
  id_crea_dom   = get_new_utimer_ID('Creation_domaines   ')
  id_select     = get_new_utimer_ID('Select Prox Tactors ')
  id_nlgs_prep  = get_new_utimer_ID('NLGS PREP           ')
  id_nlgs_iter  = get_new_utimer_ID('NLGS ITER           ')
  id_nlgs_Rnod  = get_new_utimer_ID('NLGS RnodHRloc      ')
  id_nlgs_Compt = get_new_utimer_ID('NLGS Compt_dof_list ')
  id_sol_intrf  = get_new_utimer_ID('X DF = [|V|]        ')
  id_nlgs_vfree = get_new_utimer_ID('NLGS cmpt_loc_free_v')
  id_nlgs_check = get_new_utimer_ID('NLGS CHECK          ')
  id_nlgs_post1 = get_new_utimer_ID('NLGS POST 1         ')
  id_nlgs_post2 = get_new_utimer_ID('NLGS POST 2         ')
  id_nlgs_post3 = get_new_utimer_ID('NLGS POST 3         ')
  id_ddm_merg   = get_new_utimer_ID('DDM Merge           ')
  id_updt       = get_new_utimer_ID('Update              ')
  id_post       = get_new_utimer_ID('POST                ')


  !-------------------------------------------
  ! Partie propre à la décomposition de domaine
  ! Plus de paramètre "parallel" à donner,
  ! on est en MDS un point c'est tout.
  Nsdm = init_dd(Nsdm1, Nsdm2, TimeStep)
  
  print *, "Nsdm=", Nsdm 
  !-------------------------------------------


  !call set_with_experimental_dev


  ! </read
  call start_utimer(id_read)
  CALL set_time_step(TimeStep)        
  CALL init_theta_integrator(Theta_)       
  ! lecture des corps + surdimmensionnement (séquentiel multidomaine)
  CALL read_in_bodies_RBDY2(Nsdm)
  CALL update_existing_entities_RBDY2
  CALL read_in_dof_ol(0)
  CALL read_in_dof_RBDY2(0)
  CALL read_in_driven_dof_RBDY2
  call stop_utimer(id_read)
  ! read/>


  ! </DDM
  call start_utimer(id_crea_dom)
  ! copie des corps pour representer chaque sous-domaine
  call copy_bodies_RBDY2(Nsdm) ! les copies sont invisibles par défaut 

  ! récupération du nombre de corps après duplication
  nb_RBDY2 = get_nb_RBDY2()
  nbody = nb_RBDY2 / Nsdm

  ! Toutes les particules sont maintenant visibles
  call set_visibility_4all_RBDY2((/ (.true., i=1, nb_RBDY2) /),nb_RBDY2)
  call stop_utimer(id_crea_dom)
  ! DDM/>


  ! </read
  call start_utimer(id_read)

  CALL read_bodies_DISKx
  CALL init_entitylist
  nb_DISKx = get_nb_DISKx()
  if (nb_DISKx == 0) call faterr("DKDK_standalone_DDM", "l'echantillon ne contient aucun disque!")

  CALL read_in_bulk_behav 
  !am: lecture du fichier TACT_BEHAV.DAT
  CALL open_tact_behav_ll()
  CALL open_see_ll()
  CALL read_xxx_tact_behav(1)
  CALL close_tact_behav_ll()
  CALL close_see_ll()
  ! fin lecture du fichier TACT_BEHAV.DAT
  CALL read_behaviours_RBDY2   
  CALL read_in_Vloc_Rloc_ol(0)
  if ( check_DKDKx() ) CALL read_ini_Vloc_Rloc_DKDKx(0)

  ! init postpro
  postpro_unit = init_postpro_command()
  CALL start_postpro(postpro_unit, 0)
  CALL messages_for_users

  ! Dimensionnement des tableaux de la méthode des boites
  ! une fois pour toute la simulation
  CALL compute_box_DKDKx

  CALL comp_mass_RBDY2
  call stop_utimer(id_read)
  ! read/>


  ! </DDM
  call start_utimer(id_crea_dom)
  !-------------------------------------------
  ! Partie propre à la décomposition de domaine
  !
  !   * allocation des tableaux du module
  call new_dd(nb_RBDY2,nb_DISKx)
  
  if (allocated(mask)) deallocate(mask)
  allocate(mask(nb_RBDY2), stat=errare)
  if (err/=0) call faterr("DKDK_standalone_DDM", "erreur d'allocation de mask")
  !-------------------------------------------
  call stop_utimer(id_crea_dom)
  ! DDM/>


  iterTT=0

  do istep=1,nb_time_steps

     ! </free
     call start_utimer(id_free)
     CALL time_increment                  
     print *, "Nstep =", get_Nstep()
     call stop_utimer(id_free)
     ! free/>

     if (istep == 1 .or. modulo(istep,DDMfreq) == 0) then

        ! On ne peut merger les données que si un pas de temps a déjà été fait (i>1)
        ! et on ne veut merger les données que lorsqu'une nouvelle répartition en
        ! sous-domaines vas avoir lieu (modulo ci-dessus).
        if (istep > 1) then
           ! </DDM mergage dans le sdm de l'hôte
           call start_utimer(id_ddm_merg)
           call gather_Xbeg_Vbeg
           call stop_utimer(id_ddm_merg)
           ! DDM mergage dans le sdm de l'hôte/>
        end if

        ! </DDM Création des domaines
        call start_utimer(id_crea_dom)
        ! On rend visible la première tranche en vue de la
        ! nouvelle répartition en sous-domaines
        mask=.false.
        mask(1:nbody)=.true.
        call set_visibility_4all_RBDY2(mask,nb_RBDY2)

        !----------------------------------------
        ! "Pseudo-main" du module DDM_2d :
        !   * détection grossière (méthode des boites générique)
        !   * découpage en sous-domaines
        !   * création du body_particip
        !   * shift (ou split) du graphe de contact
        !   * compute_modified_mass
        !   * compute_interface
        !   * migration_treatment
        call creation_domaines()!left_bound=left_bound, right_bound=right_bound)

        ! Mise en donnée dans mod_DKDKx du graphe de contact grossier
        call launch_set_anonymous_to_rough_4all
        call erase_shiftted_rough
        call erase_rough_contact

        if (istep > 1) then
           ! Mise en donnée des nouveaux arrivants
           call fix_migrants
        end if

        ! Applique les masques de visibilités pour simuler la DDM
        call set_visibility_4all_in_DDM(nb_RBDY2)
        !call out_of_bounds_RBDY2      

        do isdm=1,Nsdm
           call set_modified_mass_in_DDM(isdm)
        end do
        call stop_utimer(id_crea_dom)
        ! DDM Créations des domaines/>
     end if


     ! </free
     call start_utimer(id_free)
     CALL increment_RBDY2
     !CALL  Set_newton_loop(0)
     CALL comp_Fext_RBDY2
     CALL comp_Fint_RBDY2
 
     ! Initialisation des Fgamma pour les particules d'interface avec
     ! les Fgamma de la fin du pas de temps précédent (particules restées 
     ! dans une même liste de RBDY2 d'interface par sous domaine).
     if (istep > 1) then
        call set_F_gamma
     end if

     CALL comp_free_vlocy_RBDY2
     CALL Init_entitylist
     call stop_utimer(id_free)
     ! free/>


     ! </detect
     call start_utimer(id_select)
     if ( check_DKDKx() ) then
        ! stockage des coordonnees des contacteurs disques dans le module DKDK
        CALL coor_prediction_DKDKx
        ! detection fine
        CALL compute_contact_DKDKx
        ! récupération dans DDM_MDS de la liste des
        ! contacts taggés 'INTRF'.
        CALL get_list_INTRF_in_DDM
     end if
     call stop_utimer(id_select)
     ! detect/>


     !</prep_nlgs
     call start_utimer(id_nlgs_prep)
     if ( check_DKDKx() ) CALL recup_rloc_DKDKx
     CALL set_nlgs_parameter(CVTYPE,tol,RELAX)
     !ELG
     CALL prep_nlgs(.FALSE.)
     !SDL        
     !CALL prep_nlgs(.TRUE.)
     call stop_utimer(id_nlgs_prep)
     ! prep_nlgs/>

     iter = 0

     !---------------------------------------------------------------
     ! Boucle DDM
ddm2:do iddm2=1,itloop2

   ddm1:do iddm1=1,itloop1

           iter = iter + 1

           !print *, "---------------------------------"
           !print *, "iter=", iter

           !</solve_nlgs
           call start_utimer(id_nlgs_iter)
           call solve_nlgs(1)
           call stop_utimer(id_nlgs_iter)
           ! solve_nlgs />

           if (Nsdm > 1) then
              ! Si on est en multidomaines,
              ! les trois étapes supplémentaires sont  : 
              ! Injection dans l'espace des particules d'interface
              ! Résolution du problème de recollement.
              ! Projection de la nouvelle vitesse libre sur les 
              ! "contacts d'interface".

              !</RnodHRloc
              call start_utimer(id_nlgs_Rnod)
              !call RnodHRloc_nlgs
              ! Restriction de RnodHRloc aux contacts faisant intervenir
              ! au moins une particule d'interface.
              ! stockage Vaux/Raux:
              call RnodHRloc_list_in_DDM
              call stop_utimer(id_nlgs_Rnod)
              ! RnodHRloc/>
              ! </nlgs_Compt
              call start_utimer(id_nlgs_Compt)
              ! stockage Vaux/Raux:
              call comp_V_list_RBDY2_in_DDM
              ! stockage Vaux/Raux:
              call stock_V_interface
              call stop_utimer(id_nlgs_Compt)
              ! nlgs_Compt/>
  
  
              ! </DDM XDF=[|V|]
              call start_utimer(id_sol_intrf)
              ! Résolution du problème d'interface
              call compute_F_gamma 
              call stop_utimer(id_sol_intrf)
              ! DDM XDF=[|V|]/>


              ! </DDM vfree
              call start_utimer(id_nlgs_vfree) 
              ! Calcul de la nouvelle vitesse libre (au sens de l'absence de contact)
              ! tenant compte de l'actualisation des F_gamma
              ! (F_ext <- F_ext + delta F_gamma).
              call set_DF_gamma
              ! Execution des parties generiques de prep_nlgs
              !   * Envoi des vitesses libres globales dans 
              !     les repères locaux de chaque contact.
              !call compute_local_free_vlocy
              ! Opération duale de RnodHRloc_group : calcul de la nouvelle vitesse
              ! libre pour les contacts dont au moins une particule est de 
              ! multiplicité > 1.
              call compute_local_free_vlocy_in_DDM
              call stop_utimer(id_nlgs_vfree)
              ! DDM vfree/>
        end if

        end do ddm1

        ! </NLGS check
        call start_utimer(id_nlgs_check) 
        call prep_check_nlgs(iconv1)
        ! Si le nombre de CDAN est zéro, on sort
        if ( iconv1 == 0 ) EXIT
        call solve_nlgs(2)

        call comp_check_nlgs(iconv2)
        call compute_egluing(iddm2, egluing)
        ! Si le problème d'interface est convergé et
        ! le nlgs dans les sdm est convergé, on sort.
        if ( egluing < TOL .and. iconv2 == 0 ) exit
        call stop_utimer(id_nlgs_check)
        ! NLGS Check/>

     end do ddm2


     iterTT=iterTT+iter

 
     ! </nlgs fin
     call start_utimer(id_nlgs_post1)
     ! Affichage du nombre d'itérations éffectuées
     print *, "nb iterations effectuees=", iter
     call RnodHRloc_nlgs
     call stop_utimer(id_nlgs_post1)

     call start_utimer(id_nlgs_post2)
     call solve_nlgs(3)
     call Nullify_EntityList_nlgs
     call stop_utimer(id_nlgs_post2)

     call start_utimer(id_nlgs_post3)
     if ( check_DKDKx() ) call stock_rloc_DKDKx 
     call stop_utimer(id_nlgs_post3)
     ! nlgs fin/>
  
 
     !</Updt début
     call start_utimer(id_updt)
     call comp_V_RBDY2
     call stop_utimer(id_updt)
     ! Updt début/>


     ! </DDM recollement de l'interface
     call start_utimer(id_ddm_merg)
     call fix_V_interface         ! Calcul + set dans le sdm n°1 de la vitesse et 
                                  ! position des grains d'interface à convergence 
                                  ! de la boucle DDM.
     call compute_V_interface_sdm ! Construction des vecteurs à passer aux "esclaves"
     call stop_utimer(id_ddm_merg)
     ! DDM recollement complet/>

 
     ! </DDM fin du recollement complet
     call start_utimer(id_ddm_merg)
     call set_V_interface 
     call stop_utimer(id_ddm_merg)
     ! DDM fin du recollement complet/>


     !</ Updt
     call start_utimer(id_updt)
     call comp_X_RBDY2
     call Updt_time_begin
     call update_dof_RBDY2 
     call stop_utimer(id_updt)
     ! Updt/>

     ! </DDM
     call start_utimer(id_crea_dom)
     ! On impose le masque global de visibilite sur une seule tranche
     call set_visibility_4all_in_DDM(nb_RBDY2)
     !call out_of_bounds_RBDY2
     call stop_utimer(id_crea_dom)
     ! DDM/>
   
  
     ! </post GMV + POSTRO
     call start_utimer(id_post)

     CALL postpro_during_computation

     IF (MODULO(istep, freq_OUTBOX) == 0) then 
        call Write_xxx_Vloc_Rloc_Ol(1)
        if ( check_DKDKx() ) call Write_xxx_Vloc_Rloc_DKDKx(1)
        call Write_xxx_dof_Ol(1)
        call write_xxx_dof_RBDY2(1,1,nb_RBDY2)
        call write_DDM_INFO
     ENDIF

     IF (MODULO(istep, nb_time_steps) == 0) then 
        call Write_xxx_Vloc_Rloc_Ol(2)
        if ( check_DKDKx() ) call Write_xxx_Vloc_Rloc_DKDKx(2)
        call Write_xxx_dof_Ol(2)
        call write_xxx_dof_RBDY2(2,1,nb_RBDY2)
        call Write_out_bodies_Ol
        call write_out_bodies_RBDY2
        call Write_out_driven_dof_Ol
        call write_out_driven_dof_RBDY2
        !call add_dof2bodies_RBDY2
        !call Write_out_bodies_Ol
        !call write_out_bodies_RBDY2
     ENDIF
     call stop_utimer(id_post)
     ! post GMV + POSTRO/>

     call clean_writing_flags

  end do

  print *,  "Nombre d'iterations totales=", iterTT

  CALL close_postpro_files
  CALL write_utimer

  ! nettoyage de la memoire encore allouee
  call clean_ddm

end program DKDK_standalone_DDM_MDS
