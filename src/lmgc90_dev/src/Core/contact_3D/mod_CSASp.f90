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

!> This modulus deals with geoemetric and kinematic operations
!> between contactors CSxxx and ASpxx.
!> In this modulus candidate contactors are CSxxx and antagonist 
!> contactors are ASpxx.
 MODULE CSASp                                          

 USE overall
 USE tact_behaviour
 USE DiscreteGeometry
 USE algebra 
 USE CSxxx
 USE ASpxx 

 use MAILx, only : get_color_MAILx
 use RBDY3, only : get_color_RBDY3 => get_color
 use MBS3D, only : get_color_MBS3D => get_color
 
 use parameters, only : i_csasp, i_mailx, i_rbdy3, i_mbs3

 use ann, ann_clean_memory => clean_memory

 use inter_meca_3D

 use ExternalFEM
 use ExternalDetection


 implicit none

 private

 logical :: is_externalDetection = .FALSE.
 

 CHARACTER(len=5) :: BOBO='CSASp'
 !fd le nb de ASpxx
 INTEGER          :: nb_ASpxx
 !fd le nb de ASxxx dans chaque ASpxx
 integer,dimension(:),allocatable :: nb_ASxxx 
 !fd le nb de CSxxx
 INTEGER          :: nb_CSxxx

 type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 INTEGER,PRIVATE :: nb_CSASx=0 , nb_vCSASx=0 ,nb_recup_CSASx  ! nb_CSASx = number of selected candidates POLYR against PLANx
                                                              ! <= size(this).

!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj ! nb_adj(icdtac): number of adjacent pairs POLYR-POLYR
                                                        ! to candidate contactor POLYR icdtac.

!------------------------------------------------------------------------ 


 type(T_verlet), dimension(:), allocatable, target ::verlt

!-------------------------------------------------------------------------
 TYPE T_rough_CSASx                                   ! définit le type de la liste des plus proches voisins

   INTEGER                   :: cd                    ! le candidat, l'antagoniste et isee pour la loi de contact
   INTEGER                   :: an                    ! l'antagoniste
   INTEGER                   :: ias                   ! la face de l'antagoniste            
   INTEGER                   :: isee

   real(kind=8),dimension(3) :: cd_normalTT           ! pour eliminer des faces de polyedres

   integer                    :: pn_id                ! rank of the proximal node in the ASpxx object         

 END TYPE T_rough_CSASx

 TYPE(T_rough_CSASx),DIMENSION(:),ALLOCATABLE   :: rough_CSASx        ! table  de visibilité
 INTEGER                                        :: nb_rough_CSASx     ! nombre de paire de polygone à analyser pour déterminer
                                                                      ! s'il y a contact ou pas par détect

 TYPE T_link_rough_CSASx                                              ! liste chainée pour determiner les listes de cand_ant car
                                                                      ! on ne connait pas a priori le nb de cand-ant 
    TYPE(T_link_rough_CSASx), POINTER :: p                            ! pointeur sur le precedent
    TYPE(T_rough_CSASx)               :: val                          ! les valeurs
    TYPE(T_link_rough_CSASx), POINTER :: n                            ! pointeur sur le suivant

 END TYPE T_link_rough_CSASx

 TYPE(T_link_rough_CSASx),POINTER     :: Root,Current,Previous

!------------------------------------------------------------------------
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: CScoor

 type T_ASp
   REAL(kind=8),DIMENSION(:,:),pointer :: AScoor
 end type T_ASp
 type(T_ASp),DIMENSION(:),ALLOCATABLE,PRIVATE :: ASp

!------------------------------------------------------------------------
 REAL(kind=8) :: Reac_CSASx_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------
 INTEGER,PRIVATE   :: ii,l_ii,iv
 INTEGER,PRIVATE   :: Nstep_creation_tab_visu=1,restart=0
 LOGICAL,PRIVATE   :: write_creation_tab_visu
!------------------------------------------------------------------------
 logical :: is_initialized = .false.
!------------------------------------------------------------------------
 logical :: skip_autocontact = .false.
!------------------------------------------------------------------------
! usefull when both cd/an on skin 
 logical :: is_nonsymmetric_detection=.false.
!------------------------------------------------------------------------
! remove contact with surface edge 
 logical :: trim_contact = .FALSE.
 real(kind=8) :: trim_angle=87. 
!------------------------------------------------------------------------

 logical      :: module_checked_ = .FALSE.
 logical      :: check_CSASx_    = .FALSE.

!------------------------------------------------------------------------
! necessary to read old ini files (the one with the CSpxx rank instead of CSxxx one) 
 logical            :: old_way=.FALSE.
 
! liste des fonctions publiques 
!
 PUBLIC &
       coor_prediction_CSASx,&
       CHECK_CSASx,&
       RUN_CSASx, &
       get_write_Vloc_Rloc_CSASx, &
       read_ini_Vloc_Rloc_CSASx,&
       write_xxx_Vloc_Rloc_CSASx,&
       stock_rloc_CSASx, &
       recup_rloc_CSASx, &
       recup_rloc_by_position_CSASx, &
       creation_tab_visu_CSASx, &
       compute_contact_CSASx, &
       display_prox_tactors_CSASx,&
       get_nb_CSASx, &
       skip_autocontact_CSASp, &
       clean_memory_CSASp, &
       get_nodes_CSASx, &
       get_dof_CSASx, &
       get_external_pressure_CSASx,&
       print_info_CSASp, &
       add_reac_CSASp, &
       assume_old_files_CSASp

 PUBLIC &
      nullify_reac_CSASx,  &
      nullify_vlocy_CSASx, &
      injj_CSASx,          &
      prjj_CSASx,          &
      vitrad_CSASx,        &
      get_g2l_CSASx,       &
      CSASx2ENT,           &
      CSASx2ASpxx,         &
      CSASx2CSxxx,         &
      initialize_CSASp,    &
      get_surf_CSASx,      &
      display_vlocy_CSASx, &
      set_nonsymmetric_detection_CSASp, &
      trim_CSASp,&
      set_trim_angle_CSASp,&      
      is_external_detection_CSASp, &
      get_bulk_strain_csasp, &
      get_bulk_stress_csasp, &
      get_bulk_strain_triaxiality_csasp, &
      get_bulk_stress_triaxiality_csasp, &
      get_bulk_strain_rate_triaxiality_csasp, &
      get_bulk_temperature_csasp

  !rm for handler
  public get_this    , &
         set_nb_CSASx, &
         redo_nb_adj_CSASx, &
         get_an_tacty     , &
         get_verlet_tact_lawnb

 CONTAINS

  include 'interaction_common.f90'
  ! defines the following subroutines
  !subroutine get_behaviour_( icdan, see, tact_behav )
  !subroutine get_this(this_inter, verlet_inter, violation_inter)
  !function get_an_tacty(i_mdl, i_bdy, i_tac)
  !subroutine redo_nb_adj_( nb_cd )
  !subroutine new_verlet_(icdtac, size, errare)
  !subroutine free_verlet_(icdtac)
  !subroutine nullify_verlet_(icdtac)
  !subroutine clean_memory_inter_meca_()
  include 'interaction_common_3D.f90'
  ! defines the following subroutines
  !function get_verlet_tact_lawnb( icdtac, iadj )


!------------------------------------------------------------------------
!------------------------------------------------------------------------

!> dimensionnement de tableaux internes
!> recuperation du nb de CS/AS ...
  subroutine initialize_CSASp
    IMPLICIT NONE  
    integer :: errare,iasp,itacty,nb_vertex
                                !123456789012345678901
    CHARACTER(len=21)  :: IAM = 'mod_CSASp::initialize'

     nb_CSxxx = get_nb_CSxxx()
     nb_ASpxx = get_nb_ASpxx()

     IF (.NOT. ALLOCATED(CScoor)) then
       ALLOCATE(CScoor(3,nb_CSxxx),stat=errare)
        if (errare /=0 ) THEN
          call FATERR(IAM,' error in allocating CScoor')
        endif
     endif

     IF (.NOT. ALLOCATED(ASp)) then
       ALLOCATE(ASp(nb_ASpxx),stat=errare)
        if (errare /=0 ) THEN
          call FATERR(IAM,' error in allocating ASp')
        endif
     endif

     allocate(nb_ASxxx(nb_ASpxx),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating nb_ASxxx')
     endif

     do iasp=1,nb_ASpxx
       nb_ASxxx(iasp) = get_nb_ASxxx(iasp) 
       nb_vertex = get_nb_vertex_ASpxx(iasp) 

       !print*,iasp,nb_ASxxx(iasp),nb_vertex

       ALLOCATE(ASp(iasp)%AScoor(3,nb_vertex),stat=errare)
       if (errare /=0 ) THEN
         call FATERR(IAM,' error in allocating AScoor')
       endif
     enddo

    IF (.NOT. ALLOCATED(adjac)) THEN
      ALLOCATE(adjac(nb_CSxxx),stat=errare)
      IF (errare /=0 ) THEN
        call FATERR(IAM,' error in allocating adjac')
      END IF
      DO itacty=1,nb_CSxxx
        NULLIFY(adjac(itacty)%icdan)
      ENDDO 
    ENDIF

    IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
    ALLOCATE(nb_adj(nb_CSxxx),stat=errare)
    IF (errare /=0 ) THEN
      call faterr(IAM,' error allocating nb_adj')
    END IF    

    is_initialized = .true.

    !print*,'module initialized'

  end subroutine
!-------------------------------------------------------------------------- 
! Subroutine pour actualiser les positions des vertex des polyèdres
! au cours du temps
!--------------------------------------------------------------------------
  SUBROUTINE coor_prediction_CSASx

    IMPLICIT NONE  

    INTEGER :: errare 
    INTEGER :: itacty,ias
                                !12345678901234567890123456
    CHARACTER(len=26)  :: IAM = 'mod_CSASp::coor_prediction'

    character(len=90) :: cout
    integer :: err_

    if (.not. is_initialized ) then
      call FATERR(IAM,' module not initialized')
    endif

    ! actualisation de la structure CSpxx courante
    call increment_CSpxx()

    ! 
    DO itacty=1,nb_CSxxx
      CScoor(1:3,itacty) = get_coorTT_CSxxx(itacty)
    END DO

    DO itacty=1,nb_ASpxx

      !print*,'en entree:',itacty,size(ASp(itacty)%AScoor,dim=1),size(ASp(itacty)%AScoor,dim=2)

      call get_coorTT_ASpxx(itacty,ASp(itacty)%AScoor)

      call update_HE_Hdl(get_HE_Hdl_ASpxx(itacty),ASp(itacty)%AScoor,err_)

      if (err_ > 0) then
        write(cout,'("antagonist patch",I0)') itacty
        call faterr(IAM,'unexpected problem while updating HE') 
      endif   
      
    END DO

 END SUBROUTINE coor_prediction_CSASx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_CSASx(step)
    implicit none
    integer(kind=4), intent(in) :: step
    
    G_nfich=get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
    else if(step > 0) then
      open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(:))))
    else
      open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
    end if

    call read_ini_Vloc_Rloc
    close(G_nfich)
    
  end subroutine read_ini_Vloc_Rloc_CSASx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_CSASx(which)
    
    IMPLICIT NONE
    
    INTEGER :: which,nfich,lc
    
    nfich = get_io_unit()
    
    SELECT CASE(which)
    CASE(1)
       lc = LEN_TRIM(out_Vloc_Rloc)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(out_Vloc_Rloc(1:lc))))
       CALL write_out_Vloc_Rloc(nfich)
       CLOSE(nfich)
    CASE(2)
       lc = LEN_TRIM(last_Vloc_Rloc)
       OPEN(unit=nfich,STATUS='OLD',POSITION='APPEND',file=trim(location(last_Vloc_Rloc(1:lc))))
       CALL write_out_Vloc_Rloc(nfich)
       CLOSE(nfich)
    CASE(6)
       CALL write_out_Vloc_Rloc(6)
    END SELECT
    
  END SUBROUTINE write_xxx_Vloc_Rloc_CSASx
!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_CSASx

  IMPLICIT NONE

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,icdtac,iantac,itac
  INTEGER                               :: isee,isee_min,itacty,i
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol

  real(kind=8) ::  dist_min,dist,dist2,adist2,gdist2,vec(3),dir(3)
  integer      ::  iasxx, iaspxx_min, iasxxx_min, icheck, ppp

  ! pas utilise ....
  integer :: f_out
  real(kind=8),dimension(3) ::  point,weight_out,t,n,s
  ! ... !

  character(len=80) :: cout

  logical :: lcheck

  integer :: cd_ent, an_ent

  integer :: err_

  logical :: oldies
  
  ! Detecting contact roughly 

  DO icdtac=1,nb_CSxxx
    IF (ASSOCIATED(adjac(icdtac)%icdan)) DEALLOCATE(adjac(icdtac)%icdan) 
    NULLIFY(adjac(icdtac)%icdan)
  ENDDO 

  nb_adj=0
  nb_rough_CSASx=0

  ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)

  !fd pre-detection par: 
  !fd   - distance(vertex CSxxx, sphere englobante des vertex du ASpxx)

  !fd todo: virer recherche n^2 

  DO icdtac=1,nb_CSxxx
    IF (.NOT. get_visible_CSxxx(icdtac)) CYCLE

    cdcol=get_color_CSxxx(icdtac)

    dist_min=1.d+20
    iaspxx_min = 0
    iasxxx_min = 0

    dir = get_normalTT_CSxxx(icdtac)

    cd_ent = get_ent_CSxxx(icdtac)

    ! print*,'icdtac= ',icdtac
    ! write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")')  CScoor(:,icdtac)
    ! write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")') dir
    ! print*,cdcol
    ! print*,cd_ent

    DO iantac=1,nb_ASpxx
       
      IF (.NOT.get_visible_ASpxx(iantac)) CYCLE

      if (is_nonsymmetric_detection .and. aspxx2bdyty(1,iantac) > csxxx2bdyty(1,icdtac))  cycle
      
      an_ent = get_ent_ASpxx(iantac)

      if (skip_autocontact .and. cd_ent == an_ent) cycle

      ! print*,'iantac= ',iantac
      ! print*,ancol
      ! print*,an_ent
      
      ancol=get_color_ASpxx(iantac)

      isee = get_isee('MAILx','CSxxx',cdcol,'MAILx','ASpxx',ancol)

      !print*,'isee',isee

      IF (isee/=0) THEN

        ! print*,'iantac= ',iantac
         
        adist2=see(isee)%alert**2
        gdist2=max(see(isee)%alert,see(isee)%global_alert)**2

!!$        do iasxx=1,size(ASp(iantac)%AScoor,dim=2)
!!$          vec = CScoor(:,icdtac) - ASp(iantac)%AScoor(:,iasxx)
!!$          dist2 = dot_product(vec,vec)
!!$
!!$
!!$          !print*,icdtac,iantac,iasxx
!!$          !print*,dist2,adist2
!!$
!!$          if (dist2 > adist2) cycle
!!$
!!$          if (dist2 <= dist_min) then
!!$            dist_min = dist2
!!$            iaspxx_min = iantac
!!$            iasxxx_min = iasxx
!!$            isee_min=isee
!!$          endif
!!$        enddo

        ! cas d'une face a 1 element T3 ou Q4 (cas classique czm)
        if (is_singleton_ASpxx(iantac)) then

         oldies=.FALSE.  

         if (oldies) then 
          ! on parcourt les noeuds de la face (i.e. de l'element) 
          ! a la recherche d un noeud assez proche
          lcheck = .false.
          do iasxx=1,size(ASp(iantac)%AScoor,dim=2)
            vec = CScoor(:,icdtac) - ASp(iantac)%AScoor(:,iasxx)
            dist2 = dot_product(vec,vec)

            ! print*,'singleton',dist2,gdist2

            IF (dist2 < gdist2) lcheck = .true.

            if (lcheck) exit 

          enddo

          ! il y a un noeud qui va bien on teste 
          ! que le candidat se projette dans une face T3 sous jacente et que ca ne soit
          ! pas trop loin
          ! TODO: gerer ca avec les normales 
          if (lcheck) then
             
            ppp = 0
            icheck = node_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
                                           CScoor(:,icdtac),&
                                           see(isee)%global_alert, &
                                           dir,.true.,ppp,dist,point,t,n,s,f_out, &
                                           weight_out,.false.,err_,trim_angle=trim_angle)
            if (err_ > 0) then
              write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
              call logmes(cout)
              call faterr('CSASp:: creation_tab_visu','unexpected problem in node HE proximity')
            endif   

            ! print*,'icheck= ',icheck
            ! print*,dist,iantac,ppp

            if (icheck < 3 ) cycle

            dist2 = dist * dist

            ! cas ou ca n'est pas les bonnes surfaces qui se voient (trop dehors ou trop dedans)
            if (dist2 > adist2) cycle
            
            ! do iasxx=1,size(ASp(iantac)%AScoor,dim=2)
            !    write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")')ASp(iantac)%AScoor(:,iasxx)
            ! enddo   

            ! print*,'ok',dist,dist_min
            ! print*,n
            ! print*,f_out
            ! print*,weight_out
            !ppp

            IF (dist <= dist_min) THEN
               ! dist_min = dist2
               dist_min = dist
               iaspxx_min = iantac
               iasxxx_min = ppp !<- a voir
               isee_min=isee
            ENDIF
          endif
         else
          ! recherche d'un ppp dans le halo
          ! on regarde si le CS est dans le cone de la face ASp <- desactive, a reprendre
          ! TODO: gerer ca aussi avec les normales
          icheck = node_HE_Hdl_rough_proximity(get_HE_Hdl_ASpxx(iantac), &
                                               CScoor(:,icdtac),&
                                               see(isee)%global_alert, &
                                               dir, &
                                               ppp,dist,.false.,err_,trim_angle=trim_angle)
          if (err_ > 0) then
            write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
            call logmes(cout)
            call faterr('CSASp:: creation_tab_visu','unexpected problem in rough node HE proximity')
          endif   

          if (icheck == 0) cycle

          if (dist <= dist_min) then
            dist_min = dist
            iaspxx_min = iantac
            iasxxx_min = ppp !<- a voir
            isee_min=isee
          endif
         endif

        ! face avec plusieurs elements ( cas contact entre objets mailles) 
        else

          ! recherche d'un ppp dans le halo
          ! on regarde si le CS est dans le cone de la face ASp <- desactive, a reprendre
          ! TODO: gerer ca aussi avec les normales
          icheck = node_HE_Hdl_rough_proximity(get_HE_Hdl_ASpxx(iantac), &
                                               CScoor(:,icdtac),&
                                               see(isee)%global_alert, &
                                               dir, &
                                               ppp,dist,.false.,err_,trim_angle=trim_angle,with_extend=.TRUE.)
          if (err_ > 0) then
            write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
            call logmes(cout)
            call faterr('CSASp:: creation_tab_visu','unexpected problem in rough node HE proximity')
          endif   

          if (icheck == 0) cycle

          if (dist <= dist_min) then
            dist_min = dist
            iaspxx_min = iantac
            iasxxx_min = ppp !<- a voir
            isee_min=isee
          endif
        ENDIF
      ENDIF
    ENDDO

    if (iaspxx_min /= 0) then
      nb_rough_CSASx=nb_rough_CSASx+1
      IF ( nb_rough_CSASx == 1) THEN
        ALLOCATE(Root)
        Current => Root
        NULLIFY(Root%p)
      ELSE
        ALLOCATE(Current)
        Previous%n => Current
      ENDIF

      ! print*,'icdtac= ',icdtac
      ! write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")')  CScoor(:,icdtac)
      ! write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")') dir

      ! print*,'iantac= ',iaspxx_min
      ! do iasxx=1,size(ASp(iaspxx_min)%AScoor,dim=2)
      !    write(*,'("[",E12.5,",",E12.5,",",E12.5,"]")')ASp(iaspxx_min)%AScoor(:,iasxx)
      ! enddo   

     
      Current%val%cd    = icdtac
      Current%val%an    = iaspxx_min
      Current%val%ias   = iasxxx_min
      Current%val%isee  = isee_min
      Current%val%cd_normalTT = get_normalTT_CSxxx(icdtac)

      ! print*,'rough'
      ! print*,icdtac, get_ent_CSxxx(icdtac)      
      ! print*,iaspxx_min,iasxxx_min,get_ent_ASpxx(iaspxx_min)
      ! print*,Current%val%cd_normalTT
      
      Current%p => Previous
      NULLIFY(Current%n)
      Previous => Current
    endif
  ENDDO     

  WRITE(cout,'(4X,I10,A20)') nb_rough_CSASx,' CSASx roughly found'
  call logmes(cout)

  IF (ALLOCATED(rough_CSASx)) DEALLOCATE(rough_CSASx)
  ! on s'alloue la table de visibilité utilisée dans compute_contact
  ALLOCATE(rough_CSASx(nb_rough_CSASx))              

  IF (ALLOCATED(this)) DEALLOCATE(this)
  ! on s'alloue un tableau temporaire de contact.On lui donne une taille 3*nb_paire_phphx
  ! car il y a au maximun trois points de contact entre un candidat - antagoniste
  ALLOCATE(this(nb_rough_CSASx))     
                                       
  DO i=nb_rough_CSASx,1,-1

    Previous => Current%p
    rough_CSASx(i)%cd          = Current%val%cd
    rough_CSASx(i)%an          = Current%val%an
    rough_CSASx(i)%ias         = Current%val%ias
    rough_CSASx(i)%isee        = Current%val%isee

    rough_CSASx(i)%cd_normalTT = Current%val%cd_normalTT
    DEALLOCATE(Current)
    Current => Previous
  END DO 

  NULLIFY(Root)

END SUBROUTINE creation_tab_visu_CSASx
!------------------------------------------------------------------------
SUBROUTINE compute_contact_CSASx
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,ibdy,icdtac,iantac,isee,itacty    
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  REAL(kind=8)                          :: adist,dist
  INTEGER                               :: i,j
  REAL(kind=8)                          :: ovlap                               ! les gaps en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: t,n,s                               ! la normale en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: anlev                               ! les vecteurs centre -> point de contact

  REAL(kind=8),DIMENSION(3)             :: cd_Vbegin
  REAL(kind=8),DIMENSION(6)             :: an_Vbegin
  REAL(kind=8),DIMENSION(3)             :: point
  REAL(kind=8),DIMENSION(3,3)           :: localframe_an, Rc
  REAL(kind=8)                          :: vln_cst,vls_cst,vlt_cst
 
  integer :: cd_ent,an_ent

  integer :: v,f,fj,vd,vf,f0,k,ibad,ibad_k,vd_aux
  
  real(kind=8),dimension(3) :: dir,weight_out,nn

!  real(kind=8),dimension(3) :: orien,vec,vv,vec_aux,interc,dir,weight_out
!  real(kind=8) :: sens,apab,norm

  !fd real(kind=8),dimension(3) :: cd_normalTT

  logical :: all_dof_cd, all_dof_an

  logical :: itchatche=.true. !.false.

  integer :: icheck,ppp,f_out,iasxxx

  character(len=80) :: cout

  integer :: err_,ii

  logical :: bavard=.FALSE.
  
  icdan=0        
  nb_CSASx=0
  nb_adj=0

  IF (nb_rough_CSASx /= 0 ) THEN
     !
     ! Fine Detection
     !
     DO i=1,nb_rough_CSASx

       icdtac=rough_CSASx(i)%cd                  ! CSxxx
       iantac=rough_CSASx(i)%an                  ! ASpxx
       iasxxx=rough_CSASx(i)%ias                 ! ppp in the ASpxx 
       isee=rough_CSASx(i)%isee
       !fd cd_normalTT = rough_CSASx(i)%cd_normalTT
       adist=see(isee)%alert 

       if (bavard) then
       
         print*,'                       '
         print*,'detection fine ========'       
         print*,'Rough contact'
         print*,'cd '
         print*,icdtac,get_ent_CSxxx(icdtac)
         print*,'cd coor'
         write(*,'("[",3(1x,E12.5,","),"]")') CScoor(:,icdtac)
         print*,'cd normal (dir)'
         nn=get_normalTT_CSxxx(icdtac)
         write(*,'("[",3(1x,E12.5,","),"]")') nn
         print*,'an '
         print*,iantac,iasxxx,get_ent_ASpxx(iantac)
         print*,'an coor'
         do ii=1,size(ASp(iantac)%AScoor,2)
           write(*,'("[",3(1x,E12.5,","),"],")') ASp(iantac)%AScoor(:,ii)
         enddo   
         print*,'alert distances'       
         print*,see(isee)%alert,see(isee)%global_alert
       endif 
       !
       ! recompute HEorien
       !

       dir = get_normalTT_CSxxx(icdtac)
       ppp=0

       if (bavard) then
         !debug
         icheck = new_node_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
                                        CScoor(:,icdtac),&
                                        see(isee)%global_alert, &
                                        dir,.true., &
                                        ppp,ovlap,point,t,n,s,f_out,weight_out,.true.,err_,with_extend=.TRUE.)
       else          
         icheck = new_node_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
                                        CScoor(:,icdtac),&
                                        see(isee)%global_alert, &
                                        dir,.true., &
                                        ppp,ovlap,point,t,n,s,f_out,weight_out,.false.,err_,with_extend=.TRUE.)
       endif        

       if (err_ > 0) then
         write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
         call logmes(cout)
         call faterr('CSASp::compute_contact','unexpected problem in node HE proximity')
       endif   

       if (bavard) then
         print*,'icheck: ',icheck
         if (icheck > 0) then
           print*,'ppp ',ppp,' face ',f_out
           print*,'gap ',ovlap,' alert ',adist
           print*,'point de contact'
           write(*,'("[",3(1x,E12.5,","),"],")') point
           print*,'normale au contact'
           write(*,'("[",3(1x,E12.5,","),"],")') n
           print*,'poids'
           print*,weight_out
         endif  
       endif     

       !f_out/weight_out la face/les coordonnees reduites dans la face
       ! si trop penetre on vire
       if (icheck <= 0 .or. ovlap > adist .or. ovlap < -2.d0*adist) then
         if (bavard) print*,'on ne garde pas !!!'
         cycle 
       else
         if (bavard) then
           print*,'on garde !!!'
           print*,'===='
         endif  
       endif         
       
       !! checking that not all dof involved in contact are driven dof
       all_dof_cd = all_dof_driven_CSxxx(icdtac)
       ! it is up to ASxxx to map f_out, which is Half-Edge face id,
       ! to the correct element iasxxx which is a face id
       all_dof_an = all_dof_driven_ASxxx(iantac, f_out)
       if( all_dof_cd .and. all_dof_an ) cycle

       icdan = icdan + 1

       nb_adj(icdtac) = nb_adj(icdtac) + 1

       iadj = nb_adj(icdtac)

       this(icdan)%iadj    = iadj
       this(icdan)%icdbdy  = csxxx2bdyty(1, icdtac)
       this(icdan)%icdtac  = icdtac
       this(icdan)%ianbdy  = aspxx2bdyty(1, iantac)
       this(icdan)%iantac  = iantac

       this(icdan)%icdbtac = csxxx2bdyty(2, icdtac)
       this(icdan)%ianbtac = aspxx2bdyty(2, iantac)

       this(icdan)%icdbtyp = csxxx2bdyty(3, icdtac)
       this(icdan)%ianbtyp = aspxx2bdyty(3, iantac)

       this(icdan)%icdctyp = i_csxxx
       this(icdan)%ianctyp = i_aspxx

       this(icdan)%icdsci  = get_sci_CSxxx(icdtac)

       this(icdan)%iansci  = f_out        ! facette (fictive) de projection
       this(icdan)%weight  = weight_out   ! ponderation dans cette facette

       this(icdan)%isee    = isee
       this(icdan)%nuc     = n
       this(icdan)%tuc     = t
       this(icdan)%suc     = s

       cd_ent = get_ent_CSxxx(icdtac)
       an_ent = get_ent_ASpxx(iantac)

       if (cd_ent /= an_ent) then
         entity(cd_ent)%nb = entity(cd_ent)%nb+1
         entity(an_ent)%nb = entity(an_ent)%nb+1
       else
         entity(cd_ent)%nb = entity(cd_ent)%nb+1
       end if

       this(icdan)%icdent = cd_ent
       this(icdan)%ianent = an_ent

       this(icdan)%coor       = point
       this(icdan)%gapTTbegin = ovlap

       ! Computation of relatives velocities
       ! Constante quantities for every contact

       call get_vlocy_CSxxx(icdtac,                     iVbeg_, cd_Vbegin)

       call get_vlocy_ASxxx(iantac, f_out , weight_out, iVbeg_, an_Vbegin)

       this(icdan)%vlsBEGIN=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ & 
                            (cd_Vbegin(2)-an_Vbegin(2))*s(2)+ &
                            (cd_Vbegin(3)-an_Vbegin(3))*s(3)
       this(icdan)%vltBEGIN=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ &
                            (cd_Vbegin(2)-an_Vbegin(2))*t(2)+ &
                            (cd_Vbegin(3)-an_Vbegin(3))*t(3)
       this(icdan)%vlnBEGIN=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ &
                            (cd_Vbegin(2)-an_Vbegin(2))*n(2)+ &
                            (cd_Vbegin(3)-an_Vbegin(3))*n(3)

       this(icdan)%rls=0.D0
       this(icdan)%rlt=0.D0
       this(icdan)%rln=0.D0
       this(icdan)%vls=this(icdan)%vlsBEGIN
       this(icdan)%vlt=this(icdan)%vltBEGIN
       this(icdan)%vln=this(icdan)%vlnBEGIN

       this(icdan)%status=i_nknow

     ENDDO
     nb_CSASx=icdan
   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_CSASx,' CSASx found'
   call logmes(cout)

   DO ibdy=1,nb_CSxxx
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating adjac(',icdtac,')%.....'
         call faterr('mod_CSASp::compute_contact',cout)
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_CSASx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 


   do icdan = 1, nb_CSASx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_CSASx),stat=errare)

END SUBROUTINE compute_contact_CSASx

!------------------------------------------------------------------------
 SUBROUTINE display_prox_tactors_CSASx

   IMPLICIT NONE
   INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   INTEGER :: nb_CSxxx
   real(kind=8),dimension(:),pointer :: interp
   character(len=5) :: cdmodel, anmodel

   nb_CSxxx=get_nb_CSxxx()

   IF (nb_CSASx==0) RETURN
   
   DO icdtact=1,nb_CSxxx    
     DO iadj=1,nb_adj(icdtact)
       icdan  = adjac(icdtact)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
       icdtac = this(icdan)%icdtac
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( csxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( aspxx2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,csxxx2bdyty(1,icdtac),'CSxxx',csxxx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,aspxx2bdyty(1,iantac),'ASpxx',aspxx2bdyty(2,iantac)
       ! cdmodel,icdbdy,'CSxxx',icdtac,see(this(icdan)%isee)%behav,  &
       ! anmodel,ianbdy,'ASpxx',iantac
                    
       WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
       WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
       WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
       WRITE(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
       WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               

       if ( .false. ) then

         WRITE(*,104) 'x(1)=',this(icdan)%coor(1)  ,'x(2)=',this(icdan)%coor(2)  ,'x(3)=',this(icdan)%coor(3)
         print*,'weight = ', get_weight_CSxxx(icdtac)

         nullify(interp)
         interp => get_interp_CSxxx(this(icdan)%icdtac)
         print*,' cd_interp ',interp
         deallocate(interp)
       
         nullify(interp)
         interp => get_interp_ASpxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight)
         print*,' an_interp ',interp
         deallocate(interp)
         nullify(interp)
       endif
      
     END DO                           
   END DO

104  FORMAT(27X,3(2X,A5,D14.7))
   
 END SUBROUTINE display_prox_tactors_CSASx
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_CSASx
   !
   ! get data from this and put into verlt
   !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare

    character(len=80) :: cout
                               !123456789012345678901
    character(len=21) :: IAM = 'mod_CSASp::stock_rloc'

   nb_CSxxx=get_nb_CSxxx()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_CSxxx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_CSxxx
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       ELSE
         call nullify_verlet_(icdtac)
       END IF
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_CSxxx
       verlt(icdtac)%adjsz=0
       call free_verlet_(icdtac)

       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       ELSE
         call nullify_verlet_(icdtac)
       END IF
     END DO
   END IF

   ! filling data:
   DO icdan=1,nb_CSASx
     icdtac = this(icdan)%icdtac ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj   ! serial adjacent number of pair contactor 
                                 ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = csxxx2bdyty( 1, icdtac )
     verlt(icdtac)%cdtac           = csxxx2bdyty( 2, icdtac )
     verlt(icdtac)%cdmodel         = csxxx2bdyty( 3, icdtac )
     verlt(icdtac)%anbdy(iadj)     = aspxx2bdyty( 1, iantac )
     verlt(icdtac)%antac(iadj)     = aspxx2bdyty( 2, iantac )
     verlt(icdtac)%anmodel(iadj)   = aspxx2bdyty( 3, iantac )
     verlt(icdtac)%rls(iadj)       = this(icdan)%rls/H
     verlt(icdtac)%rlt(iadj)       = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)       = this(icdan)%rln/H
     verlt(icdtac)%vls(iadj)       = this(icdan)%vls
     verlt(icdtac)%vlt(iadj)       = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)       = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)     = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)    = this(icdan)%status
     verlt(icdtac)%coor(1:3, iadj) = this(icdan)%coor(1:3)

     verlt(icdtac)%tuc(:, iadj)    = this(icdan)%tuc(:)
     verlt(icdtac)%nuc(:, iadj)    = this(icdan)%nuc(:)
     verlt(icdtac)%suc(:, iadj)    = this(icdan)%suc(:)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

     verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
     verlt(icdtac)%ansci(iadj)    = this(icdan)%iansci
   END DO

   nb_vCSASx = nb_CSASx

   WRITE(cout,'(1X,I10,A12)') nb_vCSASx,' stock CSASx'
   call logmes(cout)
   
 END SUBROUTINE stock_rloc_CSASx
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_CSASx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER           :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   character(len=80) :: cout
   logical           :: cdcheck

   real(kind=8)      :: t(3),s(3) 

   
   if (.not. allocated(verlt)) then
      call logmes('[mod_CSASp::recup_rloc] Warning: verlt not allocated, no recup done')
      return
   end if
   IF (.NOT. ALLOCATED(verlt)) THEN
     call faterr('mod_CSASp::recup_rloc','verlt not allocated, illegal to recup')
   ENDIF
   nb_recup_CSASx=0 
   DO icdan=1,nb_CSASx
     this(icdan)%rls=0.D0
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow

     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac

     IF (verlt(icdtac)%adjsz /= 0) THEN

       if ( verlt(icdtac)%cdbdy  == csxxx2bdyty(1,icdtac) .and. &
            verlt(icdtac)%cdtac  == csxxx2bdyty(2,icdtac) .and. &
            verlt(icdtac)%cdmodel== csxxx2bdyty(3,icdtac)       &
          ) then

         do iadj = 1, verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == aspxx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == aspxx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== aspxx2bdyty(3,iantac) .and. &
                verlt(icdtac)%cdsci(iadj)  == get_sci_CSxxx(icdtac)       &
              ) then
              this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
              this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
              this(icdan)%rln = verlt(icdtac)%rln(iadj)*H
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
              this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)

              !fd 2022-04-11
              ! on reordonne le nouveaux repere pour qu'il soit proche de l'ancien
              s = cross_product(verlt(icdtac)%tuc(:, iadj),this(icdan)%nuc(:))
              s = s/length3(s)

              t = cross_product(this(icdan)%nuc(:),s)
              t = t/length3(t)

              this(icdan)%tuc(:)=t
              this(icdan)%suc(:)=s

              nb_recup_CSASx=nb_recup_CSASx+1
              exit
           end if
         end do
       end if
     ENDIF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_CSASx,' recup CSASx'
   call logmes(cout)

 END SUBROUTINE recup_rloc_CSASx

 !> Get data from verlet list verlt and put into this
 subroutine recup_rloc_by_position_CSASx(tol)
   implicit none
   !> max distance to accept identification
   real(kind=8), intent(in) :: tol
   !
   real(kind=8) :: dist
   integer(kind=4) :: icdan, icdtac, iadj, i_old, itree
   real(kind=8), dimension(:)  , pointer :: point
   real(kind=8), dimension(:,:), pointer :: verlt_coor
   integer     , dimension(:,:), pointer :: imap
   !
   character(len=33), parameter :: IAM = 'mod_CSASp::recup_rloc_by_position'
   character(len=31) :: cout

   nb_recup_CSASx = 0
   if( nb_vCSASx < 1 ) then
     do icdan=1,nb_CSASx

       this(icdan)%rlt = 0.d0
       this(icdan)%rln = 0.d0
       this(icdan)%rls = 0.d0
       this(icdan)%statusBEGIN = i_nknow

     end do
     write(cout,'(1X,I10,A12)') nb_recup_CSASx,' recup CSASx'
     call logmes(cout)
     return
   end if

   call set_nb_kd(1)  

   allocate( verlt_coor(nbDIME, nb_vCSASx) )
   allocate( point(nbDIME) )

   ! map to link index in ann tree and candidate and adjacent id
   ! and if this interaction has already been got back
   allocate( imap(3, nb_vCSASx) )
   imap = 0

   itree = 0
   do icdtac = 1, min(nb_CSxxx, size(verlt))
     if (verlt(icdtac)%adjsz /= 0) then
       do iadj = 1, verlt(icdtac)%adjsz
         itree = itree + 1
         verlt_coor(1:3,itree) = verlt(icdtac)%coor(1:3,iadj)

         imap(1,itree) = icdtac
         imap(2,itree) = iadj
       end do
     end if
   end do

   call add_kd_tree(1, verlt_coor, nb_vCSASx, nbDIME)

   do icdan = 1, nb_CSASx

     this(icdan)%rlt = 0.d0
     this(icdan)%rln = 0.d0
     this(icdan)%rls = 0.d0
     this(icdan)%statusBEGIN = i_nknow

     point(1:3) = this(icdan)%coor(1:3)
     call search_nearest_kd_tree(1, point, i_old, dist)

     ! check if nearest index found, and close enough
     ! AND if not already used by another interaction
     if( i_old>0 .and. imap(3,i_old) == 0 .and. dist<tol ) then

       icdtac = imap(1,i_old)
       iadj   = imap(2,i_old)

       imap(3,i_old) = 1

       this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H
       this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H
       this(icdan)%rls         = verlt(icdtac)%rls(iadj)*H
       this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

       this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)

       nb_recup_CSASx = nb_recup_CSASx + 1

     end if ! if ann found a nearest

   end do ! loop on this

   write(cout,'(1X,I10,A12)') nb_recup_CSASx,' recup CSASx'
   call logmes(cout)

   call ann_clean_memory()

   deallocate( verlt_coor )
   deallocate( point )
   deallocate( imap )

 end subroutine recup_rloc_by_position_CSASx

!------------------------------------------------------------------------ 
 subroutine read_ini_Vloc_Rloc
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   implicit none

   character(len=103) :: clin
   integer            :: icdan,icdbdy,icdtac,icdtact,ianbdy,iantac,iadj
   integer            :: cdmodel, anmodel
   real(kind=8)       :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
   character(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   integer            :: errare,i_internal,nb_internal,ibehav
   character(len=29)  :: IAM = 'mod_CSASx::read_ini_Vloc_Rloc'
   character(len=103) :: cout
   logical            :: to_read
   ! print*,IAM
   
   nb_CSxxx=get_nb_CSxxx()
   errare=0
  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_CSxxx),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating nb_adj')
   END IF

   !DO icdtac=1,nb_CSxxx
   !  nb_adj(icdtac)=0
   !END DO

   nb_adj = 0

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'CSASx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,                                          &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
     IF (cdtac == 'CSxxx' .AND. antac == 'ASpxx') THEN
        
       cdmodel = get_body_model_id_from_name( cdbdy )

       DO icdtact=1,nb_CSxxx
          if (old_way) then 
            IF ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                 csxxx2bdyty(2,icdtact) == icdtac .and. &
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               nb_adj(icdtact) = nb_adj(icdtact) + 1
               EXIT
            END IF
          else  
            IF ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                               icdtact  == icdtac .and. &
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               nb_adj(icdtact) = nb_adj(icdtact) + 1
               EXIT
            END IF
          endif          
       END DO
     END IF
   END DO


   ! DO icdtact=1,nb_CSxxx
   !   print*,csxxx2bdyty(:,icdtact)
   !   print*,icdtact, nb_adj(icdtact)
   ! END DO

   
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_CSxxx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtact=1,nb_CSxxx
       verlt(icdtact)%adjsz=0
       iadj=nb_adj(icdtact)
       IF (iadj > 0) THEN
         verlt(icdtact)%adjsz=iadj
         call new_verlet_(icdtact, iadj, errare)
       ELSE
         call nullify_verlet_(icdtact)
       END IF
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       END IF
       
     END DO
   ELSE 
     DO icdtact=1,nb_CSxxx
       verlt(icdtact)%adjsz=0
       call free_verlet_(icdtact)

       iadj=nb_adj(icdtact)
       IF (iadj > 0) THEN
         verlt(icdtact)%adjsz=iadj
         call new_verlet_(icdtact, iadj, errare)
       ELSE
         call nullify_verlet_(icdtact)
       END IF
     END DO
   END IF


   ! DO icdtact=1,nb_CSxxx
   !   if (nb_adj(icdtact) > 0) print*,icdtact,nb_adj(icdtact),verlt(icdtact)%adjsz
   ! END DO

   nb_adj=0
   icdan = 0

   ! second reading: filling data
   REWIND(G_nfich)
   
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'CSASx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac, &
          behav,                     &
          anbdy,ianbdy,antac,iantac, &
          sttus
     IF (cdtac == 'CSxxx' .AND. antac == 'ASpxx') THEN

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       DO icdtact=1,nb_CSxxx
          to_read=.FALSE.
          if (old_way) then 
            IF ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                 csxxx2bdyty(2,icdtact) == icdtac .and. &
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               to_read=.TRUE.
            END IF
          else  
            IF ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                               icdtact  == icdtac .and. &
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               to_read=.TRUE.
            END IF
          endif          
          
          IF ( to_read ) THEN

            icdan = icdan + 1

            nb_adj(icdtact) = nb_adj(icdtact) + 1

            ! print*, 'read'
            ! print*,icdbdy,icdtac,ianbdy,iantac,cdmodel,anmodel
            ! print*,icdtact,csxxx2bdyty(1:3,icdtact)
            ! print*,verlt(icdtact)%adjsz,nb_adj(icdtact)

            verlt(icdtact)%icdan( nb_adj(icdtact) ) = icdan

            verlt(icdtact)%cdbdy   = icdbdy
            verlt(icdtact)%cdtac   = csxxx2bdyty(2, icdtact)
            verlt(icdtact)%cdmodel = cdmodel
            verlt(icdtact)%cdsci(nb_adj(icdtact))  = get_sci_CSxxx(icdtact)
            verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
            verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
            verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
            verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
            IF( .NOT. read_G_clin()) CYCLE
            READ(G_clin(1:90),'(27X,3(7X,D14.7))')rls,rlt,rln
            verlt(icdtact)%rls(nb_adj(icdtact))=rls
            verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
            verlt(icdtact)%rln(nb_adj(icdtact))=rln
            IF( .NOT. read_G_clin()) CYCLE
            READ(G_clin(1:90),'(27X,3(7X,D14.7))')vls,vlt,vln
            verlt(icdtact)%vls(nb_adj(icdtact))=vls
            verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
            verlt(icdtact)%vln(nb_adj(icdtact))=vln
            IF( .NOT. read_G_clin()) CYCLE 
            READ(G_clin(1:90),'(27X,2(7X,D14.7))')gapTT
            verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
            IF( .NOT. read_G_clin()) CYCLE
            IF (G_clin(30:34)== 'coo1=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%coor(1,nb_adj(icdtact))=PTx
                verlt(icdtact)%coor(2,nb_adj(icdtact))=PTy
                verlt(icdtact)%coor(3,nb_adj(icdtact))=PTz
            ELSE 
                BACKSPACE(G_nfich)
            END IF
            IF( .NOT. read_G_clin()) CYCLE
            IF (G_clin(30:34)== 't(1)=') THEN
              READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
              verlt(icdtact)%tuc(1,nb_adj(icdtact))=PTx
              verlt(icdtact)%tuc(2,nb_adj(icdtact))=PTy
              verlt(icdtact)%tuc(3,nb_adj(icdtact))=PTz
            ELSE 
              BACKSPACE(G_nfich)
            ENDIF
            IF( .NOT. read_G_clin()) CYCLE
            IF (G_clin(30:34)== 'n(1)=') THEN
              READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
              verlt(icdtact)%nuc(1,nb_adj(icdtact))=PTx
              verlt(icdtact)%nuc(2,nb_adj(icdtact))=PTy
              verlt(icdtact)%nuc(3,nb_adj(icdtact))=PTz
            ELSE 
              BACKSPACE(G_nfich)
            ENDIF
            IF( .NOT. read_G_clin()) CYCLE
            IF (G_clin(30:34)== 's(1)=') THEN
              READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
              verlt(icdtact)%suc(1,nb_adj(icdtact))=PTx
              verlt(icdtact)%suc(2,nb_adj(icdtact))=PTy
              verlt(icdtact)%suc(3,nb_adj(icdtact))=PTz
            ELSE 
              BACKSPACE(G_nfich)
            ENDIF

            verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
            ibehav = get_ibehav(behav)
            nb_internal = get_nb_internal(ibehav)
            IF (nb_internal /= 0 ) THEN  
              IF( .NOT. read_G_clin()) EXIT
              DO i_internal=1, nb_internal
                READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
              ENDDO
            ENDIF
            EXIT
          ENDIF
       ENDDO
     endif  
   enddo     

   nb_vCSASx=0
   DO icdtact=1,nb_CSxxx
      nb_vCSASx = nb_vCSASx + nb_adj(icdtact)
      
      IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
         WRITE(cout,'(A,I7,1X,A,1X,I7,A,I7)') 'Very strange for the contactor ',icdtact, &
              ' value of nb_adj is ',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         CALL FATERR(IAM,cout)
      END IF
   END DO

   write(cout,'(A,I0,A)') 'read ',nb_vCSASx,' CSASx'
   call logmes(cout)

   
104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
 END SUBROUTINE read_ini_Vloc_Rloc
!------------------------------------------------------------------------   
 SUBROUTINE write_out_Vloc_Rloc(nfich)
   !
   ! write into file out_Vloc_Rloc data from this, in verlt style
   !
   IMPLICIT NONE
   INTEGER :: iadj,icdtact
   INTEGER :: nfich,icdan,icdtac,iantac

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_CSxxx=get_nb_CSxxx()

   IF (nb_CSASx==0) RETURN

   DO icdtact=1,nb_CSxxx    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         cdmodel = get_body_model_name_from_id( csxxx2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( aspxx2bdyty(3,iantac) )
         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CSASx',icdan  
                             !123456789012345678901234567890123456789012345678901234567890123456789'
         WRITE(nfich,'(A69)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus'
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdmodel,csxxx2bdyty(1,icdtac),'CSxxx',icdtac, & !csxxx2bdyty(2,icdtac),  & <- pas bon car c est le rang du CSpxx
              see(this(icdan)%isee)%behav,  &
              anmodel,aspxx2bdyty(1,iantac),'ASpxx',aspxx2bdyty(2,iantac),  &
              get_contact_status_name_from_id(this(icdan)%status)
         WRITE(nfich,104) 'rls/H',this(icdan)%rls/H      ,'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H
         WRITE(nfich,104) 'vls =',this(icdan)%vls        ,'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  
         WRITE(nfich,103) 'gapTT',this(icdan)%gapTT
         WRITE(nfich,104) 'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',this(icdan)%coor(3)
         WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
         WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
         WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)

         IF (this(icdan)%nb_internal /= 0) THEN
           CALL write_internal_comment(nfich,this(icdan)%lawnb)
           write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
           write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
         END IF
         WRITE(nfich,'(A1)')' '

      END DO
   END DO
   
104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)   

 END SUBROUTINE write_out_Vloc_Rloc
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_reac_CSASx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: storage
    
   CALL nullify_reac_CSxxx(this(icdan)%icdtac,storage)
   
   CALL nullify_reac_ASpxx(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_reac_CSASx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_CSASx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
    
   CALL nullify_vlocy_CSxxx(this(icdan)%icdtac,storage)
   
   CALL nullify_vlocy_ASpxx(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_vlocy_CSASx
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_CSASx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy

   CALL comp_vlocy_CSxxx(this(icdan)%icdtac,storage,need_full_vlocy)

   CALL comp_vlocy_ASpxx(this(icdan)%iantac,this(icdan)%iansci,storage,need_full_vlocy)
    
 END SUBROUTINE vitrad_CSASx
!------------------------------------------------------------------------  
 SUBROUTINE injj_CSASx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER                    :: storage

   REAL(kind=8),DIMENSION(3)  :: reac

   character(len=11),parameter :: IAM = 'CSASp::injj'

   reac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   reac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   reac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)

   !if (storage == iIaux_) then
   !  print*,'< ',IAM
   !  print*,icdan,get_weight_CSxxx(this(icdan)%icdtac)
   !  print*,reac
   !  print*,IAM,' >'
   !endif   
   
   CALL add_reac_CSxxx(this(icdan)%icdtac,reac,storage)

   reac(:) = -(reac(:)*get_weight_CSxxx(this(icdan)%icdtac))
   CALL add_reac_ASxxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight,reac,storage)
   
 END SUBROUTINE injj_CSASx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_CSASx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: storage

   REAL(kind=8),DIMENSION(6) :: Vcd,Van

   character(len=11),parameter :: IAM = 'CSASp::prjj'
   
   call get_vlocy_CSxxx(this(icdan)%icdtac,storage,Vcd)

   call get_vlocy_ASxxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight,storage,Van)

   ! if (storage == iVfree) then
   !  print*,'prjj csas' 
   !  print*,this(icdan)%weight
   !  print*,vcd
   !  print*,van
   ! endif

   VSIK = Vcd(1)*this(icdan)%suc(1) + Vcd(2)*this(icdan)%suc(2) + Vcd(3)*this(icdan)%suc(3)        &
        - Van(1)*this(icdan)%suc(1) - Van(2)*this(icdan)%suc(2) - Van(3)*this(icdan)%suc(3)        

   VTIK = Vcd(1)*this(icdan)%tuc(1) + Vcd(2)*this(icdan)%tuc(2) + Vcd(3)*this(icdan)%tuc(3)        &
        - Van(1)*this(icdan)%tuc(1) - Van(2)*this(icdan)%tuc(2) - Van(3)*this(icdan)%tuc(3)        

   VNIK = Vcd(1)*this(icdan)%nuc(1) + Vcd(2)*this(icdan)%nuc(2) + Vcd(3)*this(icdan)%nuc(3)        &
        - Van(1)*this(icdan)%nuc(1) - Van(2)*this(icdan)%nuc(2) - Van(3)*this(icdan)%nuc(3)        

   !if (storage == iVaux_) then
   !  print*,'< ',IAM
   !  print*,vsik,vnik,vtik
   !  print*,IAM,' >'
   !endif   

   
 END SUBROUTINE prjj_CSASx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_CSASx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_CSASx = nb_CSASx
    CASE(i_verlet_tactor)
       get_nb_CSASx = nb_vCSASx
    CASE(i_rough_tactor)
       get_nb_CSASx = nb_rough_CSASx
    CASE(i_recup_tactor)
       get_nb_CSASx = nb_recup_CSASx
    END SELECT

  END FUNCTION get_nb_CSASx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
  SUBROUTINE CSASx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_CSxxx(this(icdan)%icdtac)
   ianent = get_ENT_ASpxx(this(icdan)%iantac)

  END SUBROUTINE CSASx2ENT
!!!------------------------------------------------------------------------ 
  SUBROUTINE CSASx2CSxxx(icdan,icdtac)

    IMPLICIT NONE
    INTEGER :: icdan,icdtac
   
    icdtac = this(icdan)%icdtac

  END SUBROUTINE CSASx2CSxxx
!!!------------------------------------------------------------------------ 
  SUBROUTINE CSASx2ASpxx(icdan,iantac)

    IMPLICIT NONE
    INTEGER :: icdan,iantac
   
    iantac = this(icdan)%iantac

  END SUBROUTINE CSASx2ASpxx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_CSASx()

    IMPLICIT NONE
    
    RUN_CSASx = RUN_TACTOR

  END FUNCTION RUN_CSASx
!!!------------------------------------------------------------------------
  logical function CHECK_CSASx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_CSASx = check_CSASx_
      return
    end if

    con_pedigree%module_name = 'CDASp'

    con_pedigree%id_cdan  = i_csasp
    con_pedigree%id_cdtac = i_csxxx
    con_pedigree%id_antac = i_aspxx

    cdtact2bdyty => csxxx2bdyty
    antact2bdyty => aspxx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    if( get_nb_ASpxx() == 0 .or. get_nb_CSxxx() == 0) then
      CHECK_CSASx = check_CSASx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'CSxxx' .and. see(isee)%antac == 'ASpxx') then
        check_CSASx_ = .true.
        exit
      end if
    end do

    CHECK_CSASx = check_CSASx_
    return

  end function CHECK_CSASx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_CSASx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_CSASx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_CSASx

!!!------------------------------------------------------------------------ 
 REAL(kind=8) FUNCTION get_surf_CSASx(icdan)

   IMPLICIT NONE
   INTEGER :: icdan 
    
   get_surf_CSASx=get_surf_CSxxx(this(icdan)%icdtac)
   
 END FUNCTION get_surf_CSASx

!!!------------------------------------------------------------------------ 

 subroutine skip_autocontact_CSASp
   implicit none
   skip_autocontact = .true.
 end subroutine

!!!------------------------------------------------------------------------ 

 SUBROUTINE display_vlocy_CSASx(icdan,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   INTEGER                   :: storage
   CHARACTER(len=120)        :: cout 

   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   
   call get_vlocy_CSxxx(this(icdan)%icdtac,storage,Vcd)

   call get_vlocy_ASxxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight,storage,Van)


   write(cout,'(A,3(1x,D12.5))') 'vcd= ',vcd(1:3)
   CALL LOGMES(cout)
   write(cout,'(A,3(1x,D12.5))') 'van= ',van(1:3)
   CALL LOGMES(cout)
   write(cout,'(A,3(1x,D12.5))') 't  = ',this(icdan)%tuc
   CALL LOGMES(cout)
   write(cout,'(A,3(1x,D12.5))') 'n  = ',this(icdan)%nuc
   CALL LOGMES(cout)
   write(cout,'(A,3(1x,D12.5))') 's  = ',this(icdan)%suc
   CALL LOGMES(cout)

 END SUBROUTINE display_vlocy_CSASx 

!!!------------------------------------------------------------------------ 

 subroutine clean_memory_CSASp
   implicit none
   integer(kind=4) :: i

   call clean_memory_inter_meca_()

   nb_ASpxx = 0
   nb_CSxxx = 0

   if( allocated(nb_ASxxx) ) deallocate(nb_ASxxx)

   nb_CSASx       = 0
   nb_vCSASx      = 0
   nb_recup_CSASx = 0

   nb_rough_CSASx = 0
   if( allocated(rough_CSASx) ) deallocate(rough_CSASx)

   ! Root, Current and Previous should always be null outside creation_tab_visu

   if( allocated(CScoor) ) deallocate(CScoor)
   if( allocated(ASp)    ) then
     do i = 1, size(ASp)
       if( associated(ASp(i)%AScoor) ) deallocate(ASp(i)%AScoor)
     end do
     deallocate(ASp)
   end if

   Reac_CSASx_MAX = 0.D0

   module_checked_ = .FALSE.
   check_CSASx_    = .FALSE.

   is_nonsymmetric_detection = .false.
   trim_contact = .false.
   
 end subroutine

!!!------------------------------------------------------------------------ 

 !> recuperation noeuds support d une interaction
 subroutine get_nodes_CSASx(icdan,cd_nodes)
   implicit none
   integer(kind=4) :: icdan
   integer(kind=4),dimension(:),pointer :: cd_nodes

   cd_nodes => get_nodes_CSxxx(this(icdan)%icdtac) 

 end subroutine
 
!!!------------------------------------------------------------------------ 

 !> recuperation ddl support d une interaction
 subroutine get_dof_CSASx(icdan,cd_dof,an_dof)
   implicit none
   integer(kind=4) :: icdan
   integer(kind=4),dimension(:),pointer :: cd_dof,an_dof

   cd_dof => get_dof_CSxxx(this(icdan)%icdtac)
   an_dof => get_dof_ASpxx(this(icdan)%iantac,this(icdan)%iansci)

 end subroutine

!!!------------------------------------------------------------------------ 

 !> recuperation matrice (sparse) d'interpolation
 subroutine get_g2l_CSASx(icdan,g2l,cd_dof,an_dof)
  implicit none
  integer(kind=4) :: icdan
  real(kind=8),dimension(:,:) :: g2l
  integer(kind=4),dimension(:),pointer :: cd_dof,an_dof
  ! ***
  integer(kind=4) :: nbdof_cd,nbdof_an,i,j,k
  real(kind=8),dimension(:),pointer :: cd_interp=>NULL() ,an_interp=>NULL()
                           !12345678901234
  character(len=14) :: IAM='CSASp::get_g2l' 

  if ((size(g2l,dim=1) /= 3) .or. &  
      (size(g2l,dim=2) /= size(cd_dof)+size(an_dof))) then

    call faterr(IAM,'mismatch in g2l shape' )

  endif


  if (associated(cd_interp)) deallocate(cd_interp); nullify(cd_interp) 
  cd_interp => get_interp_CSxxx(this(icdan)%icdtac)
  if (associated(an_interp)) deallocate(an_interp); nullify(an_interp) 
  an_interp => get_interp_ASpxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight)

  ! n
  do i=1,size(cd_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(1, 3*(i-1) + j ) = cd_interp(i)*this(icdan)%nuc(j)
    enddo 
  enddo
  k = 3*size(cd_interp)
  do i=1,size(an_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(1, k+3*(i-1) + j ) = -an_interp(i)*this(icdan)%nuc(j)
    enddo 
  enddo
  ! t
  do i=1,size(cd_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(2, 3*(i-1) + j ) = cd_interp(i)*this(icdan)%tuc(j)
    enddo 
  enddo
  k = 3*size(cd_interp)
  do i=1,size(an_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(2, k+3*(i-1) + j ) = -an_interp(i)*this(icdan)%tuc(j)
    enddo 
  enddo
  ! s
  !fd attempt to correct tns <-> nts ; no effect
  do i=1,size(cd_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(3, 3*(i-1) + j ) = cd_interp(i)*this(icdan)%suc(j)
!fd      g2l(3, 3*(i-1) + j ) = -cd_interp(i)*this(icdan)%suc(j)
    enddo
  enddo
  k = 3*size(cd_interp)
  do i=1,size(an_interp)
    ! boucle sur les noeuds supports
    do j=1,3  
      !boucle sur la dimension d'espace
      g2l(3, k+3*(i-1) + j ) = -an_interp(i)*this(icdan)%suc(j)
!fd      g2l(3, k+3*(i-1) + j ) =  an_interp(i)*this(icdan)%suc(j)
    enddo
  enddo

 end subroutine


subroutine print_info_CSASp(icdan)
   implicit none
   integer          :: icdan,icdtac,iantac,icdbdy,ianbdy

   character(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   write(cout,1) icdtac,iantac
   call LOGMES(cout)

1  format(1X,'CSxxx:',1x,I0,1x,'ASpxx:',1x,I0)

!   icdbdy=this(icdan)%icdbdy
!   ianbdy=this(icdan)%ianbdy

!   call print_info_POLYR(icdtac)
!   call print_info_POLYR(iantac)

end subroutine print_info_CSASp

 subroutine set_nb_CSASx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CSASx = nb

 end subroutine

 subroutine redo_nb_adj_CSASx()
   implicit none

   call redo_nb_adj_( get_nb_CSxxx() )

 end subroutine

!!!------------------------------------------------------------------------
  subroutine set_nonsymmetric_detection_CSASp()
    
    implicit none
    
    is_nonsymmetric_detection = .true.
    
  end subroutine set_nonsymmetric_detection_CSASp
!!!------------------------------------------------------------------------
  subroutine trim_CSASp
    implicit none

    trim_contact = .TRUE.

  end subroutine trim_CSASp
!!!------------------------------------------------------------------------  
  subroutine set_trim_angle_CSASp(angle)
    implicit none
    real(kind=8) :: angle

    trim_angle = angle

  end subroutine
!!!------------------------------------------------------------------------

 !------------------------------------------------------------------------ 
 subroutine is_external_detection_CSASp()
   implicit none

   is_externalDetection = .True.
   
 end subroutine is_external_detection_CSASp
 
 !------------------------------------------------------------------------
 subroutine get_bulk_stress_csasp(icdan,vec)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: vec(:)

   if (is_externalFEM) then

      call externalFEM_get_stress( icdan, vec )

   else

!    call get_bulk_stress_csxxx(this(icdan)%icdtac,vec)

   endif

 end subroutine

 !------------------------------------------------------------------------ 
 subroutine get_bulk_strain_csasp(icdan,vec)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: vec(:)

   if (is_externalFEM) then

     call externalFEM_get_strain( icdan, vec )
  
   else

     !call get_bulk_strain_csxxx(this(icdan)%icdtac,vec)

   endif

 end subroutine

 !------------------------------------------------------------------------  
 subroutine get_bulk_strain_triaxiality_csasp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

   if (is_externalFEM) then

     call externalFEM_get_straintriaxiality( icdan, value )

   else

     !call get_bulk_strain_triaxiality_csxxx(this(icdan)%icdtac,value)

   endif

 end subroutine
 
 !------------------------------------------------------------------------  
 subroutine get_bulk_stress_triaxiality_csasp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

   if (is_externalFEM) then

     call externalFEM_get_stresstriaxiality( icdan, value )

   else

     call get_bulk_stress_triaxiality_csxxx(this(icdan)%icdtac,value)

   endif

 end subroutine

 !------------------------------------------------------------------------  
 subroutine get_bulk_strain_rate_triaxiality_csasp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

   if (is_externalFEM) then

     call externalFEM_get_strainratetriaxiality( icdan, value )

   else

     !call get_bulk_strain_rate_triaxiality_csxxx(this(icdan)%icdtac,value)

   endif

 end subroutine

 !------------------------------------------------------------------------  
 subroutine get_bulk_temperature_csasp(icdan,value)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: value

   if (is_externalFEM) then

     call externalFEM_get_temperature( icdan, value )

   else

     ! fonction à coder
     ! call get_bulk_temperature_csxxx(this(icdan)%icdtac,value)

   endif

 end subroutine

 !------------------------------------------------------------------------  
 subroutine get_external_pressure_csasx(icdan,pext)
   implicit none
   integer(kind=4) :: icdan
   real(kind=8)    :: pext

   if (is_externalFEM) then
     call externalFEM_get_pressure(icdan,pext)
   else    
     pext=0.d0
   endif
     
 end subroutine get_external_pressure_csasx

 subroutine add_reac_CSASp() 
   implicit none

                            !123456789012345
   character(len=15) :: IAM='CSASp::add_reac'
   integer :: icdan

   do icdan=1,nb_CSASx
   
      call injj_CSASx(icdan,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln,iIreac)

   enddo   

 end subroutine add_reac_CSASp
 
 subroutine assume_old_files_CSASp() 
   implicit none

                            !12345678901234567890123
   character(len=23) :: IAM='CSASp::assume_old_files'

   old_way=.TRUE.

 end subroutine assume_old_files_CSASp
 
 
 
END MODULE CSASp
