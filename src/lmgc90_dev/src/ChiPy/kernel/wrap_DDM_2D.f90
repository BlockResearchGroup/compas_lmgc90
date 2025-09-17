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
module wrap_DDM_2D

  use ISO_C_BINDING

  use timer
  use overall
  use DISKx
  use DKDKx
  use RBDY2
  use nlgs

  use DDM_2D


  private

  !> Type de strategie DDM : NSCDD => NSCDD=.true.  ou Schwartz => NSCDD=.false.
  logical          :: NSCDD = .false.

  !> timers' id
  integer(kind=4) :: id_free,id_nlgs_prep,id_nlgs_iter,id_crea_dom,id_detect_fine,        &
                     id_nlgs_post1,id_nlgs_post2,id_nlgs_post3,id_select,id_comp,id_post, &
                     id_nlgs_check, id_merg, id_nlgs_Rnod, id_nlgs_Compt, id_sol_intrf,   &
                     id_nlgs_vfree, id_recoll, id_scatter_DFg, id_Vfree_list

  integer(kind=4) :: iterTT
  integer(kind=4) :: iter

  logical :: is_initialized = .false.
  logical :: itchatche      = .false.

  !> Frequence de partionnement en sous-domaines
  integer(kind=4) :: freq_ddm
  !> Frequence d'ecriture dans DISPLAY
  integer(kind=4) :: freq_DISPLAY
  !> Frequence d'ecriture des fichiers POSTPRO
  integer(kind=4) :: freq_postpro
  !> Frequence d'ecriture dans OUT
  integer(kind=4) :: freq_OUT
  !> Frequence d'ecriture des fichiers LAST
  integer(kind=4) :: freq_last

  !> Nombre total de sous-domaines
  integer(kind=4) :: Nsdm

contains

!!!--------------------------------------------------------

  subroutine SetDDWorkingDirectory() bind(C, name = 'DDM_2D_SetDDWorkingDirectory')
    implicit none

    call set_working_directory_in_DDM

  end subroutine SetDDWorkingDirectory

  subroutine Initialize(nb_sdmx, nb_sdmy, DDM_type) bind(C, name = 'DDM_2D_Initialize')
    implicit none
    integer(kind=c_int), intent(in), value :: nb_sdmx, nb_sdmy, DDM_type
    !
    integer(kind=4) :: i

    ! Dimensionnement des tableaux de la méthode des boites
    ! une fois pour toute la simulation
    call compute_box_DKDKx

    if (DDM_type .eq. 1)  NSCDD = .true.

    WRITE(*,*) '! Nsdm1 et Nsdm2          : ',nb_sdmx, nb_sdmy
    if (DDM_type == 0) then
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'SCHWARZ'
    elseif (DDM_type == 1) then
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'NSCDD'
    else
       WRITE(*,*) "! TYPE D'ALGO DDM         : ",'INCONNU, CA VA BUGGER !!!'
    end if 

    call initialize_itimer
                                    !12345678901234567890
    id_free       = get_new_itimer_ID('FREE                ')
    id_merg       = get_new_itimer_ID('DDM Merge           ')
    id_crea_dom   = get_new_itimer_ID('Creation_domaines   ')
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
    id_recoll     = get_new_itimer_ID('DDM Recollement     ')
    id_comp       = get_new_itimer_ID('Compute Dof         ')
    id_post       = get_new_itimer_ID('OUT+LAST+postpro    ')


    ! </DDM
    call start_itimer(id_crea_dom)
    !---> Fonction rang0 <---
    Nsdm = init_dd(nb_sdmx, nb_sdmy,DDM_type)
    !-------------------------------------------

    !!-------------------------------------------
    !!> \todo : verifier que le set_visibility est utile
    !! Toutes les particules sont maintenant visibles
    !call set_visibility_4all_RBDY2((/ (.true., i=1, get_nb_RBDY2()) /),get_nb_RBDY2())
    !-------------------------------------------
    !   * allocation des tableaux communs a tous les processus,
    call new_dd_slave(get_nb_RBDY2(),get_nb_DISKx(),is_periodic_RBDY2(),get_xperiode_RBDY2()) 
    !   * allocation a 0, sur les esclaves, des tableaux geres par le maitre
    call allocations_fantome_slave
    !   * allocation des tableaux du processus hôte seul.
    !---> Fonction rang0 <---
    call new_dd_host 

    call init_postpro_in_DDM(solv_info_=.true., viol_evol_=.true., &
         sdm_info_=.true.,mean_sdm_info_=.true.,                   &
         sample_info_=.true., mean_sample_info_=.true.)
  
    iterTT = 0
    itchatche = .true.

    call stop_itimer(id_crea_dom)

  end subroutine Initialize

  subroutine SetParameters(f_ddm, f_out, f_last, f_postpro, f_display) bind(c, name='DDM_2D_SetParameters')
    implicit none
    integer(kind=c_int), intent(in), value :: f_postpro, f_display, f_ddm, f_out, f_last

    freq_postpro = f_postpro
    freq_display = f_display
    freq_ddm     = f_ddm
    freq_out     = f_out
    freq_last    = f_last

    !-------------------------------------------------------------------------------
    ! Ecriture a l'ecran des parametres
    !-------------------------------------------------------------------------------
    WRITE(*,*) '! BAVARDAGE               : ',itchatche
    WRITE(*,*) '! FREQUENCE DETECT/REPART : ',freq_ddm
    WRITE(*,*) '! WRITE POSTPRO FREQUENCY : ',freq_postpro
    WRITE(*,*) '! WRITE DISPLAY FREQUENCY : ',freq_display
    WRITE(*,*) '! WRITE OUTBOX FREQUENCY  : ',freq_out
    WRITE(*,*) '! WRITE BACKUPS FREQUENCY : ',freq_last
    WRITE(*,*) '! iperiodic & xperiode    : ',is_periodic_RBDY2(),get_xperiode_RBDY2()

  end subroutine SetParameters

  subroutine Partitioning() bind(C, name = 'DDM_2D_Partitioning')
    implicit none
    
    !---> Fonction rang0 <---
    call print_step(get_Nstep())

    ! N.B. : la frequence de decoupage en sous-domaine
    ! donne la frequence de detection
    !call set_run_contactor(freq_ddm)

    ! </DDM Domain partitioning
    !> \todo : test DKDKx contacts only before use !!!
    if ( RUN_DKDKx() ) then

       ! </DDM merge dans le sdm de l'hote
       call start_itimer(id_merg)
       !-------------------------------------------
       ! Gather de Xbeg et Vbeg sur l'hote
       if( is_initialized ) then
          call gather_X_V_begin
          call stock_Fg_liste_old ! Stockage pour recupération
                                  ! des Fg du pas precédent.
       end if
       !-------------------------------------------
       call stop_itimer(id_merg)
       ! DDM merge dans le sdm de l'hote/>

       ! </DDM Creations des domaines
       call start_itimer(id_crea_dom)
       !-------------------------------------------
       ! On rend visible l'ensemble des RBDY2 pour le
       ! processus hôte (-->0)
       call set_visibility_4all_in_DDM(0)

       !----------------------------------------
       ! Routine effectuant les operations :
       !   * detection grossiere (méthode des boites générique)
       !   * decoupage en sous-domaines
       !   * creation du body_particip
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

       call scatter_creation_domaines ! Scatters et leurs preparation de la BDD
                                       ! calculee dans creation_domaines

       call compute_shared_interfaces
       call prep_compute_DF_gamma
       call decentralise_prep_exchange_inloop

       if( is_initialized ) then
          ! Mise en donnee des nouveaux arrivants
          call fix_migrants
       end if

       ! Applique les masques de visibilites des différents processus
       call set_visibility_4all_in_DDM

       if( NSCDD ) call set_modified_mass_in_DDM
       !-------------------------------------------
       call stop_itimer(id_crea_dom)
       ! DDM Creations des domaines/>

    end if

  end subroutine Partitioning

  subroutine AddToFext() bind(C, name = 'DDM_2D_AddToFext')
    implicit none

    if( is_initialized ) call set_F_gamma

  end subroutine AddToFext

  subroutine ExSolver(cvalue1_c,cvalue2_c,rvalue1,rvalue2,ivalue1,ivalue2) bind(c, name = 'DDM_2D_ExSolver')
    implicit none
    character(c_char), dimension(30)  :: cvalue1_c
    character(c_char), dimension(5 )  :: cvalue2_c  
    real(c_double), intent(in), value :: rvalue1,rvalue2
    integer(c_int), intent(in), value :: ivalue1,ivalue2
    !
    logical                           :: SDLactif, is_converged4all
    character(len=30) :: cvalue1
    character(len=5 ) :: cvalue2
    integer(kind=4) :: i,iddm1,iddm2

    cvalue1 = ''
    cvalue2 = ''
    do i=1,30
      cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    do i=1,5
      cvalue2 = cvalue2(1:i-1) // cvalue2_c(i)
    end do

    is_converged4all = .false.

    if (.not. is_initialized) then
       !-------------------------------------------------------------------------------
       ! Parametres d'iteration de la boucle DDM
       !-------------------------------------------------------------------------------
       WRITE(*,*) '! SDL ou ELG              : ',cvalue1_c
       WRITE(*,*) '! NLGS CHECK TYPE         : ',cvalue2_c,rvalue1
       WRITE(*,*) '! RELAX                   : ',rvalue2
       WRITE(*,*) '! iteration & more        : ',ivalue1,ivalue2
    end if 

    ! </detect
    call start_itimer(id_select)
    !-------------------------------------------
    if ( check_DKDKx() ) then
       call coor_prediction_DKDKx
       if ( RUN_DKDKx()) call set_interactions_to_rough_in_DDM
       call compute_contact_DKDKx
       ! recupération dans DDM_MPI de la liste des
       ! contacts tagges 'INTRF'.
       call get_list_INTRF_in_DDM
    end if
    !-------------------------------------------
    call stop_itimer(id_select)
    ! detect/>

    !</prep_nlgs
    call start_itimer(id_nlgs_prep)
    !-------------------------------------------
    if ( check_DKDKx() ) call recup_rloc_DKDKx

    call set_nlgs_parameter(cvalue2,rvalue1,rvalue2)

    SDLactif = .FALSE.
    if(cvalue1 == 'Stored_Delassus_Loops         ') SDLactif =.TRUE.
    call prep_nlgs(SDLactif)
    !-------------------------------------------
    call stop_itimer(id_nlgs_prep)
    ! prep_nlgs/>

    iter = 0
    !---------------------------------------------------------------
    ! Boucle DDM
    do iddm2=1,ivalue2

       do iddm1=1,ivalue1

          iter = iter + 1

          !</solve_nlgs
          call start_itimer(id_nlgs_iter)
          !-------------------------------------------
          call solve_nlgs(1)
          !-------------------------------------------
          call stop_itimer(id_nlgs_iter)
          ! solve_nlgs />

          if (Nsdm == 1) cycle
          ! S'il existe plus d'un sous-domaine, alors on va
          ! effectuer les etapes supplémentaires suivantes :
          !  * injection des impulsions de contact dans
          !    l'espace des particules d'interface,
          !  * resolution du probleme de recollement,
          !  * projection de la nouvelle vitesse libre
          !    sur les "contacts d'interface".

          !</RnodHRloc
          call start_itimer(id_nlgs_Rnod)
          !-------------------------------------------
          ! Calcul des torseurs de reaction de contact sur
          ! les corps (stockage dans Iaux),
          ! pour les seuls corps d'interface
          call RnodHRloc_list_in_DDM(NSTEP,iter)
          !-------------------------------------------
          call stop_itimer(id_nlgs_Rnod)
          ! RnodHRloc/>

          if (NSCDD) then 
             !-------------------------------------------
             ! Calcul des vitesses (dans Vaux a partir des 
             ! reactions stockees dans Iaux),
             ! pour les seuls corps d'interface
             ! </nlgs_Compt
             call start_itimer(id_nlgs_Compt)
             call comp_V_list_RBDY2_in_DDM
             call stop_itimer(id_nlgs_Compt)
             ! nlgs_Compt/>

             ! Échanges decentralisés des torseurs
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
             call comp_Vfree_Fg_list_RBDY2_in_DDM
             call stop_itimer(id_Vfree_list) 
             ! DDM vfree/>
             call start_itimer(id_nlgs_vfree) 
             ! Operation duale de RnodHRloc_list_in_DDM : 
             ! calcul de la nouvelle vitesse
             ! libre uniquement pour les contacts dont au moins
             ! une particule est de multiplicite > 1.
             call compute_local_free_vlocy_in_DDM
             !-------------------------------------------
             call stop_itimer(id_nlgs_vfree)
             ! DDM vfree/>

          else ! SCHWARZ
             ! </nlgs_Compt
             call start_itimer(id_nlgs_Compt)
             call nullify_Iaux_for_vlc_drv_dof_RBDY2_in_DDM
             call stop_itimer(id_nlgs_Compt)
             ! nlgs_Compt/>

             ! Échanges decentralisés des torseurs
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
             call comp_Vfree_Rddm_list_RBDY2_in_DDM
             call stop_itimer(id_Vfree_list) 
             ! DDM vfree/>
             call start_itimer(id_nlgs_vfree) 
             ! Operation duale de RnodHRloc_list_in_DDM : 
             ! calcul de la nouvelle vitesse
             ! libre uniquement pour les contacts dont au moins
             ! une particule est de multiplicite > 1.
             call compute_local_free_vlocy_in_DDM
             !-------------------------------------------
             call stop_itimer(id_nlgs_vfree)
             ! DDM vfree/>
          end if
          !-------------------------------------------
       end do

       ! </NLGS check
       call start_itimer(id_nlgs_check) 
       !-------------------------------------------
       ! check de la convergence globale (reduction des normes calculees dans chaque sdm)
       call check_convergence_in_DDM(is_converged4all)
       !-------------------------------------------
       call stop_itimer(id_nlgs_check)
       if (is_converged4all) exit
       ! NLGS Check/>

    end do

    iterTT=iterTT+iter
    if( itchatche ) call print_iter(0,iter)
 
    ! </nlgs fin
    call start_itimer(id_nlgs_post1)
    !-------------------------------------------
    call RnodHRloc_nlgs
    call solve_nlgs(3)
    call stop_itimer(id_nlgs_post1)
    call start_itimer(id_nlgs_post2)
    call Nullify_EntityList_nlgs
    call stop_itimer(id_nlgs_post2)
    ! nlgs fin />

  end subroutine ExSolver

  subroutine ComputeDof() bind(c, name='DDM_2D_ComputeDof')
    implicit none

    !</Updt
    call start_itimer(id_comp)
    !-------------------------------------------
    ! calcul des vitesses dans chaque sdm
    ! N.B.: les grains d'interface n'ont pas necessairement la meme dans tous
    !       les sdm
    call comp_V_RBDY2
    !-------------------------------------------
    call stop_itimer(id_comp)
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
    call start_itimer(id_comp)
    !-------------------------------------------
    call comp_X_RBDY2
    !-------------------------------------------
    call stop_itimer(id_comp)
    ! Updt/>

  end subroutine ComputeDof

  subroutine Post() bind(c, name='DDM_2D_Post')
    implicit none

    ! </post OUT + postpro
    call start_itimer(id_post)
    ! Sorties postpro post-mortem
    if (NSTEP == 1 .or. MODULO(NSTEP, freq_OUT)==0) call write_OUT_in_DDM

    ! Sauvegarde des Vloc et des DOF pour relancer si necessaire
    if (MODULO(NSTEP, freq_last)==0) then
       call write_LAST_in_DDM
       call write_itimer
    end if

    ! Sorties postpro during computation
    if (MODULO(NSTEP, freq_postpro)==0) call postpro_during_in_DDM(iter)
    !-------------------------------------------
    call stop_itimer(id_post)
    ! post/>

    is_initialized = .true.

  end subroutine Post

  subroutine WriteLast() bind(c, name='DDM_2D_WriteLast')
    implicit none

    ! </post
    call start_itimer(id_post)
    !-------------------------------------------
    call write_LAST_in_DDM
    call postpro_last_in_DDM(NSTEP,freq_postpro)
    !-------------------------------------------
    call stop_itimer(id_post)
    ! post/>

  end subroutine WriteLast

  subroutine Finalize() bind(c, name='DDM_2D_Finalize')
    implicit none

    !---> Fonction rang0 <---
    call print_iter(1,iterTT)

    call write_itimer

    ! Nettoyage de la memoire encore allouee
    call clean_ddm

  end subroutine Finalize

end module wrap_DDM_2D
