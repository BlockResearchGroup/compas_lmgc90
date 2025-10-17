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


!fd module experimentale
!fd actuellement on duplique trop de choses
!fd on stoque les noeuds des faces plusieurs fois ...
!fd dans la detection grossiere on ne gere pas les arretes vives comme il faut
!fd on ne fait pas de compute box


 MODULE PRASp                                          

 USE overall
 USE tact_behaviour
 USE DiscreteGeometry
 USE Algebra
 USE POLYR
 USE ASpxx
 
 use MAILx, only : get_color_MAILx
 use RBDY3, only : get_color_RBDY3 => get_color
 use MBS3D, only : get_color_MBS3D => get_color

 use parameters, only : i_prasp, i_mailx, i_rbdy3, i_mbs3

 use inter_meca_3D

 implicit none

 private

 CHARACTER(len=5) :: BOBO='PRASp'
 INTEGER          :: nb_POLYR
 INTEGER          :: nb_ASpxx

 type(T_interaction), dimension(:), allocatable, target :: this

 !fd < a merger
  
 type(T_con),target :: con_pedigree 

 integer, dimension(:,:), pointer :: cdtact2bdyty => null()
 integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 ! nb_PRASp = number of selected candidates POLYR against ASpxx <= size(this).
 INTEGER,PRIVATE :: nb_PRASp=0 , nb_vPRASp=0 ,nb_recup_PRASp 

!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj ! nb_adj(icdtac): number of adjacent pairs POLYR-POLYR
                                                        ! to candidate contactor POLYR icdtac.

!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target ::verlt

!-------------------------------------------------------------------------
 TYPE T_rough_PRASp                                                   ! définit le type de la liste des plus proches voisins

   INTEGER                   :: cd                                    ! le candidat, l'antagoniste et isee pour la loi de contact
   integer                   :: ppp_cd
   INTEGER                   :: an 
   INTEGER                   :: ppp_an                               
   REAL(kind=8),DIMENSION(3) :: N                                     ! normale au point cd
   INTEGER                   :: isee
 END TYPE T_rough_PRASp

 TYPE(T_rough_PRASp),DIMENSION(:),ALLOCATABLE   :: rough_PRASp        ! table  de visibilité
 INTEGER                                        :: nb_rough_PRASp     ! nombre de paire de polygone à analyser pour déterminer
                                                                      ! s'il y a contact ou pas par détect

 TYPE T_link_rough_PRASp                                              ! liste chainée pour determiner les listes de cand_ant car
                                                                      ! on ne connait pas a priori le nb de cand-ant 
    TYPE(T_link_rough_PRASp), POINTER :: p                            ! pointeur sur le precedent
    TYPE(T_rough_PRASp)               :: val                          ! les valeurs
    TYPE(T_link_rough_PRASp), POINTER :: n                            ! pointeur sur le suivant

 END TYPE T_link_rough_PRASp

 TYPE(T_link_rough_PRASp),POINTER     :: Root,Current,Previous

!------------------------------------------------------------------------

 logical :: is_initialized = .FALSE.

 TYPE T_PR
   REAL(kind=8),DIMENSION(3) :: coor
   REAL(kind=8),DIMENSION(:,:),pointer :: normal
 END TYPE T_PR 

 TYPE(T_PR),DIMENSION(:),ALLOCATABLE  :: PR      

 TYPE T_ASp
    REAL(kind=8),DIMENSION(:,:),POINTER :: coor          ! 3,nb_vertex
    real(kind=8),dimension(:)  ,pointer :: radius        ! nb_vertex
    integer,dimension(:),pointer        :: good_vertices ! nb_vertex
 END TYPE T_ASp

 ! coordinates of body owning ASpxx to be used in selecting prox tactors
 TYPE(T_Asp),DIMENSION(:),ALLOCATABLE  :: ASp      

 integer,dimension(:),allocatable :: nb_ASxxx

 REAL(kind=8) :: Reac_PRASp_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

 INTEGER,PRIVATE   :: ii,l_ii,iv
 INTEGER,PRIVATE   :: Nstep_creation_tab_visu=1,restart=0
 LOGICAL,PRIVATE   :: write_creation_tab_visu
!------------------------------------------------------------------------
 logical      :: module_checked_ = .FALSE.
 logical      :: check_PRASp_    = .FALSE.


! liste des fonctions publiques 
!
 PUBLIC                           &
       initialize_PRASp,          &
       coor_prediction_PRASp,     &
       CHECK_PRASp,               &
       RUN_PRASp,                 &
       get_write_Vloc_Rloc_PRASp, &
       read_ini_Vloc_Rloc_PRASp,  &
       write_xxx_Vloc_Rloc_PRASp, &
       stock_rloc_PRASp,          &
       recup_rloc_PRASp,          &
       creation_tab_visu_PRASp,   &
       compute_contact_PRASp,     &
       display_prox_tactors_PRASp,&
       get_nb_PRASx

 PUBLIC &
      nullify_reac_PRASx, nullify_vlocy_PRASx,injj_PRASx, prjj_PRASx, vitrad_PRASx, & 
      PRASx2ENT, PRASx2POLYR,                   &
      get_corresponding_polyr_radius_PRASx, get_surf_PRASx

  public clean_memory_PRASp

  !rm for handler
  public get_this    , &
         set_nb_PRASx, &
         redo_nb_adj_PRASx, &
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
 SUBROUTINE initialize_PRASp
   IMPLICIT NONE  
   integer :: errare,iasp,itacty,nb_vertex
                               !123456789012345678901
   CHARACTER(len=21)  :: IAM = 'mod_PRASp::initialize'

   nb_POLYR = get_nb_POLYR()
   nb_ASpxx = get_nb_ASpxx()

   IF (.NOT. ALLOCATED(PR)) then
     ALLOCATE(PR(nb_POLYR),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating PR')
     endif
   ENDIF

   do itacty=1,nb_POLYR
     ALLOCATE(PR(itacty)%normal(3,S_POLYR(itacty)%nb_vertex),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating PR%normal')
     endif
   enddo
    
   IF (.NOT. ALLOCATED(ASp)) then 
     ALLOCATE(Asp(nb_ASpxx),stat=errare)
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

     ALLOCATE(ASp(iasp)%coor(3,nb_vertex),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating ASp%coor')
     endif

     ALLOCATE(ASp(iasp)%radius(nb_vertex),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating ASp%radius')
     endif

     ALLOCATE(ASp(iasp)%good_vertices(nb_vertex),stat=errare)
     if (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating ASp%good_vertices')
     endif

   enddo

   IF (.NOT. ALLOCATED(adjac)) THEN
     ALLOCATE(adjac(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call FATERR(IAM,' error in allocating adjac')
     END IF
     DO itacty=1,nb_POLYR
       NULLIFY(adjac(itacty)%icdan)
     ENDDO 
   ENDIF

   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_POLYR),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,' error allocating nb_adj')
   END IF    

   is_initialized = .true.

 END SUBROUTINE


!-------------------------------------------------------------------------- 
! Subroutine pour actualiser les positions des vertex des polyedres
! au cours du temps
!--------------------------------------------------------------------------

 SUBROUTINE coor_prediction_PRASp

  IMPLICIT NONE  

  INTEGER :: itacty

                              !12345678901234567890123456
  CHARACTER(len=26)  :: IAM = 'mod_PRASp::coor_prediction'

  character(len=90) :: cout
  integer :: err_
  
  if (.not. is_initialized ) then
    call FATERR(IAM,' module not initialized')
  endif

  call move_polyr

  DO itacty=1,nb_POLYR
     PR(itacty)%coor(1:3) = get_coorTT_POLYR(itacty)

     call get_nodal_normals_HE_Hdl(S_POLYR(itacty)%HE_Hdl,PR(itacty)%normal,err_)
     if (err_ > 0) then
       write(cout,'("POLYR ",I0)') itacty
       call logmes(cout)
       call faterr('PRASp:: coor_prediction','unexpected problem while getting nodal normals')
     endif   

     
  END DO

  DO itacty=1,nb_ASpxx

    !print*,'en entree:',itacty,size(ASp(itacty)%coor,dim=1),size(ASp(itacty)%coor,dim=2)

    call get_coorTT_ASpxx(itacty,ASp(itacty)%coor)

    call update_HE_Hdl(get_HE_Hdl_ASpxx(itacty),ASp(itacty)%coor,err_)
    if (err_ > 0) then
       write(cout,'("an ",I0)') itacty
       call logmes(cout)
       call faterr('PRASp:: coor_prediction','unexpected problem while updating HE')
    endif   

    
    call get_nodal_radius_HE_Hdl(get_HE_Hdl_ASpxx(itacty),ASp(itacty)%radius,err_)
    if (err_ > 0) then
       write(cout,'("an ",I0)') itacty
       call logmes(cout)
       call faterr('PRASp:: coor_prediction','unexpected problem while getting nodal radius')
    endif   

  END DO

END SUBROUTINE coor_prediction_PRASp
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PRASp(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PRASp
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PRASp(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PRASp

!!!------------------------------------------------------------------------

  SUBROUTINE creation_tab_visu_PRASp

  IMPLICIT NONE

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,icdtac,iantac,ias
  INTEGER                               :: isee,itacty,i,j,min_index
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  ! vecteurs reliant les centres des polygones
  REAL(kind=8),DIMENSION(3)             :: sep,axe,point,vect                  
  ! real pour calculer la norme au carrée de sep
  REAL(kind=8)                          :: dist,min_dist,adist,gdist,vec(3)         

  integer :: itouche
  character(len=80) :: cout

  integer :: err_

  ! Detecting contacts

  DO icdtac=1,nb_POLYR
    IF (ASSOCIATED(adjac(icdtac)%icdan)) DEALLOCATE(adjac(icdtac)%icdan) 
    NULLIFY(adjac(icdtac)%icdan)
  ENDDO 

  nb_adj=0
  nb_rough_PRASp=0

  ! creation de la liste de paire de polygones a examiner

  ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
  ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)

  !fd pre-detection par: 
  !fd   - distance(sphere englobante du POLYR, vertex AS)
  !fd   - distance(sphere englobante du POLYR, centre facette )


  DO icdtac=1,nb_POLYR
    IF (.NOT. get_visible_POLYR(icdtac)) CYCLE
    
    cdcol=get_color_POLYR(icdtac)

    DO iantac=1,nb_ASpxx
      ancol=get_color_ASpxx(iantac)

      isee = get_isee('RBDY3','POLYR',cdcol,'MAILx','ASpxx',ancol)

      IF (isee/=0) THEN

        ! distance fine
        adist=see(isee)%alert 
        ! distance grossiere
        gdist = max(see(isee)%alert,see(isee)%global_alert)

        ASp(iantac)%good_vertices=0

        !fd recherche d'intersection de sphere englobantes
        itouche = pinball_HE_Hdl_rough_proximity(get_HE_Hdl_ASpxx(iantac),ASp(iantac)%radius, &
                                                 PR(icdtac)%coor(:),S_POLYR(icdtac)%radius,adist, &
                                                 ASp(iantac)%good_vertices,.FALSE.,err_)

        if (err_ > 0) then
          write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
          call logmes(cout)
          call faterr('PRASp:: creation_tab_visu','unexpected problem in rough pinball HE proximity')
        endif   

        
        if (itouche > 0) then
          ! pour chaque sommet du POLYR on cherche le ppp sur la surface ASpxx parmi les good_vertices
          do j=1,S_POLYR(icdtac)%nb_vertex 
            min_dist = 1.d20 
            min_index=0
            do i=1,size(ASp(iantac)%good_vertices)
              if (ASp(iantac)%good_vertices(i) == 0) cycle

                vec = S_POLYR(icdtac)%vertex(:,j) - ASp(iantac)%coor(:,i)
                dist=length3(vec)
              
                if (dist > gdist) cycle
                if (dist < min_dist) then  
                  min_dist=dist
                  min_index=i
               endif
            enddo
            if (min_index > 0) then  
              nb_rough_PRASp=nb_rough_PRASp+1
              IF ( nb_rough_PRASp == 1) THEN
                ALLOCATE(Root)
                Current => Root
                NULLIFY(Root%p)
              ELSE
                ALLOCATE(Current)
                Previous%n => Current
              ENDIF
              Current%val%cd      = icdtac
              Current%val%ppp_cd  = j
              Current%val%an      = iantac
              Current%val%ppp_an  = min_index
              Current%val%isee    = isee
              Current%val%N       = PR(icdtac)%normal(:,j)
              Current%p => Previous
              NULLIFY(Current%n)
              Previous => Current
            endif
          enddo
        ENDIF
      ENDIF
    ENDDO
  ENDDO     

  WRITE(cout,'(4X,I10,A20)') nb_rough_PRASp,' PRASp roughly found'
  call logmes(cout)

  ! on s'alloue la table de visibilité utilisée dans compute_contact
  IF (ALLOCATED(rough_PRASp)) DEALLOCATE(rough_PRASp)
  ALLOCATE(rough_PRASp(nb_rough_PRASp))              

  DO i=nb_rough_PRASp,1,-1
    Previous => Current%p
    rough_PRASp(i)%cd     = Current%val%cd
    rough_PRASp(i)%ppp_cd = Current%val%ppp_cd
    rough_PRASp(i)%an     = Current%val%an
    rough_PRASp(i)%ppp_an = Current%val%ppp_an
    rough_PRASp(i)%isee   = Current%val%isee
    rough_PRASp(i)%N      = Current%val%N
    DEALLOCATE(Current)
    Current => Previous
  END DO 

  NULLIFY(Root)

  ! on s'alloue un tableau de contact.
  ! On lui donne une taille 4*nb_rough_PRASp
  ! car il y a au maximun quatre points de contact entre un candidat - antagoniste
  IF (ALLOCATED(this)) DEALLOCATE(this)
  ALLOCATE(this(nb_rough_PRASp))     

END SUBROUTINE creation_tab_visu_PRASp
!------------------------------------------------------------------------
SUBROUTINE compute_contact_PRASp
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac,icdtac,iantac,isee,itacty    
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  REAL(kind=8)                          :: raycd,rayan,adist,dist,min_dist,nonuc,gap
  INTEGER                               :: r_icdbdy,r_ianbdy,r_icdtac,r_iantac ! real ...
  INTEGER                               :: i,id,j,nb_ctc,nb_ctc2
  REAL(kind=8),DIMENSION(3,4)           :: xco                                 ! points de contact
  REAL(kind=8)                          :: ovlap                               ! les gaps en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: t,n,s                               ! la normale en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: cdlev,anlev                         ! les vecteurs centre -> point de contact
  REAL(kind=8),DIMENSION(3)             :: sep,axe                             ! vecteur reliant les centres des polyèdres
  REAL(kind=8)                          :: norm,den                            ! scalaire contenant la norme de sep
  REAL(kind=8),DIMENSION(6)             :: cd_Vbegin,an_Vbegin
  REAL(kind=8),DIMENSION(3)             :: axepl,point
  INTEGER,DIMENSION(4)                  :: vertex_candidat
  REAL(kind=8),DIMENSION(3,3)           :: localframe_cd,localframe_an, Rc
  REAL(kind=8)                          :: vln_cst,vls_cst,vlt_cst
  character(len=80) :: cout

  integer :: p_cd,ppp,icheck,f_out,cd_ent,an_ent
  real(kind=8) :: dir(3),weight_out(3)

  integer :: err_
  
  icdan=0        
  nb_PRASp=0
  nb_adj=0

  !print*,'2 NB POLYR=',nb_polyr

  IF (nb_rough_PRASp /= 0 ) THEN
     !
     ! Fine Detection
     !
     DO i=1,nb_rough_PRASp

       icdtac=rough_PRASp(i)%cd      ! POLYR
       p_cd  =rough_PRASp(i)%ppp_cd  ! p POLYR
       dir   =rough_PRASp(i)%N       ! normal at p 

       iantac=rough_PRASp(i)%an      ! ASpxx
       ppp=rough_PRASp(i)%ppp_an     ! ppp ASpxx 

       isee  =rough_PRASp(i)%isee
       adist=see(isee)%alert 

       ! print*,'<-xxxxxxxxxxxx'
       ! print*,'icd ',icdtac,' pt  ',p_cd,' dir ',dir
       ! print*,'ian ',iantac,' ppp ',ppp


       ! call info_HE_Hdl(get_HE_Hdl_ASpxx(iantac))
       
       icheck = node_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
                                      S_POLYR(icdtac)%vertex(:,p_cd),&
                                      see(isee)%global_alert, &
                                      dir,.true.,ppp,dist,point,t,n,s,f_out, &
                                      weight_out,.false.,err_) !,trim_angle=trim_angle)


       
       ! icheck = node_with_ppp_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
       !                                S_POLYR(icdtac)%vertex(:,p_cd),&
       !                                dir, &
       !                                ppp,ovlap,point,t,n,s,f_out,weight_out,.false.,err_)


       ! print*,'xxxxxxxxxxxx->'


       
       if (err_ > 0) then
         write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
         call logmes(cout)
         call faterr('PRASp:: compute_contact','unexpected problem in node ppp HE proximity')
       endif   

       !print*,'statut ',icheck,' gap ',ovlap,' pt ',point,' n ',n
       !print*,'xxxxxxxxxxxx->'
       
       !icheck = node_HE_Hdl_proximity(get_HE_Hdl_ASpxx(iantac), &
       !                               S_POLYR(icdtac)%vertex(:,p_cd),&
       !                               see(isee)%global_alert, &
       !                               dir, &
       !                               ppp,ovlap,point,t,n,s,f_out,weight_out,.false.)
       !

       if (icheck <= 0 .or. ovlap > adist .or. ovlap < -2.d0*adist) cycle 

       icdan          = icdan + 1
       nb_adj(icdtac) = nb_adj(icdtac) + 1
       iadj           = nb_adj(icdtac)

       this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
       this(icdan)%ianbtac = aspxx2bdyty(2, iantac)

       this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
       this(icdan)%ianbtyp = aspxx2bdyty(3, iantac)

       this(icdan)%icdctyp = i_polyr
       this(icdan)%ianctyp = i_aspxx

       this(icdan)%iadj    = iadj
       this(icdan)%icdbdy  = polyr2bdyty(1, icdtac)
       this(icdan)%icdtac  = icdtac
       this(icdan)%icdsci  = p_cd

       this(icdan)%ianbdy  = aspxx2bdyty(1, iantac)
       this(icdan)%iantac  = iantac

       ! currently it is p_cd that is needed to recup
       ! is this ppp important to recup ?
       !this(icdan)%icdsci  = ppp          ! point le plus proche dans le asp pour le pr
       this(icdan)%iansci  = f_out        ! facette (fictive) de projection
       this(icdan)%weight  = weight_out   ! ponderation dans cette facette

       this(icdan)%isee    = isee
       this(icdan)%nuc     = n
       this(icdan)%tuc     = t
       this(icdan)%suc     = s

       cd_ent = get_ent_POLYR(icdtac)
       entity(cd_ent)%nb = entity(cd_ent)%nb+1

       an_ent = get_ent_ASpxx(iantac)
       entity(an_ent)%nb = entity(an_ent)%nb+1

       this(icdan)%icdent = cd_ent
       this(icdan)%ianent = an_ent

       this(icdan)%coor       = point
       this(icdan)%gapTTbegin = ovlap

       ! Computation of relatives velocities
       ! Constante quantities for every contact

       cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)

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

       cdlev = point(1:3) - PR(icdtac)%coor(1:3)
       localframe_cd = get_inertia_frameTT_POLYR(icdtac)

       ! Compute the mapping: inertia frame to general frame for candidate

       Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
       Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
       Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

       Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
       Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
       Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

       Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
       Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
       Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)



         this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + &
                              Rc(1,2)*this(icdan)%suc(2) + &
                              Rc(1,3)*this(icdan)%suc(3) 

         this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + &
                              Rc(2,2)*this(icdan)%suc(2) + &
                              Rc(2,3)*this(icdan)%suc(3) 

         this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + &
                              Rc(3,2)*this(icdan)%suc(2) + &
                              Rc(3,3)*this(icdan)%suc(3) 

         this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                              Rc(1,2)*this(icdan)%tuc(2) + &
                              Rc(1,3)*this(icdan)%tuc(3)
 
         this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + &
                              Rc(2,2)*this(icdan)%tuc(2) + &
                              Rc(2,3)*this(icdan)%tuc(3) 

         this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + &
                              Rc(3,2)*this(icdan)%tuc(2) + &
                              Rc(3,3)*this(icdan)%tuc(3) 

         this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + &
                              Rc(1,2)*this(icdan)%nuc(2) + &
                              Rc(1,3)*this(icdan)%nuc(3)
 
         this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + &
                              Rc(2,2)*this(icdan)%nuc(2) + &
                              Rc(2,3)*this(icdan)%nuc(3)
 
         this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + &
                              Rc(3,2)*this(icdan)%nuc(2) + &
                              Rc(3,3)*this(icdan)%nuc(3) 

         this(icdan)%vlsBEGIN= this(icdan)%vlsBEGIN             + &
                               cd_Vbegin(4)*this(icdan)%Gcds(1) + &
                               cd_Vbegin(5)*this(icdan)%Gcds(2) + &
                               cd_Vbegin(6)*this(icdan)%Gcds(3) 

         this(icdan)%vltBEGIN= this(icdan)%vltBEGIN             + &
                               cd_Vbegin(4)*this(icdan)%Gcdt(1) + &
                               cd_Vbegin(5)*this(icdan)%Gcdt(2) + &
                               cd_Vbegin(6)*this(icdan)%Gcdt(3)

         this(icdan)%vlsBEGIN= this(icdan)%vlsBEGIN             + &
                               cd_Vbegin(4)*this(icdan)%Gcdn(1) + & 
                               cd_Vbegin(5)*this(icdan)%Gcdn(2) + &
                               cd_Vbegin(6)*this(icdan)%Gcdn(3)

     ENDDO
     nb_PRASp=icdan
   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_PRASp,' PRASp found'
   call logmes(cout)

   DO ibdy=1,nb_POLYR
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating adjac(',icdtac,')%.....'
         call faterr('mod_PRASp::compute_contact',cout)
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_PRASp
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 


   do icdan = 1, nb_PRASp
      call get_behaviour_( icdan, see, tact_behav )
   end do

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PRASp),stat=errare)

END SUBROUTINE compute_contact_PRASp

!------------------------------------------------------------------------
 subroutine display_prox_tactors_PRASp

   implicit none

   integer :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdver,nb_POLYR
   character(len=5) :: cdmodel, anmodel

   nb_POLYR=get_nb_POLYR()
   IF (nb_PRASp==0) RETURN
   DO icdtac=1,nb_POLYR    
     DO iadj=1,nb_adj(icdtac)         
       icdan  = adjac(icdtac)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
      !icdtac = this(icdan)%icdtac
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac
       icdver = this(icdan)%icdsci
       cdmodel = get_body_model_name_from_id( polyr2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( aspxx2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,icdbdy,'POLYR',icdtac,icdver,see(this(icdan)%isee)%behav,  &
       anmodel,ianbdy,'ASpxx',iantac
                    
       WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
       WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
       WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
       WRITE(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
       WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(27X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_PRASp
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_PRASp
   !
   ! get data from this and put into verlt
   !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,nb_POLYR
   INTEGER :: errare

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PRASp::stock_rloc'

   nb_POLYR=get_nb_POLYR()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_POLYR
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
     DO icdtac=1,nb_POLYR
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
   DO icdan=1,nb_PRASp
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor
     iadj   = this(icdan)%iadj
     ! adjacent to candidate contactor for contact icdan
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = polyr2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = polyr2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel         = polyr2bdyty(3,icdtac)
     verlt(icdtac)%anbdy(iadj)     = aspxx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = aspxx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)   = aspxx2bdyty(3,iantac)
     verlt(icdtac)%cdsci(iadj)     = this(icdan)%icdsci
     verlt(icdtac)%ansci(iadj)     = this(icdan)%iansci
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
   END DO

   nb_vPRASp = nb_PRASp

   WRITE(cout,'(1X,I10,A12)') nb_vPRASp,' stock PRASp'
   call logmes(cout)

 END SUBROUTINE stock_rloc_PRASp
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_PRASp
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdver
   character(len=80) :: cout
   
   if (.not. allocated(verlt)) then
      call logmes('[mod_PRASp::recup_rloc] Warning: verlt not allocated, no recup done')
      return
   end if
   nb_recup_PRASp=0 
   DO icdan=1,nb_PRASp
     this(icdan)%rls=0.D0
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     ! serial number of candidate body for contact icdan
     icdbdy = this(icdan)%icdbdy
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist body for contact icdan
     ianbdy = this(icdan)%ianbdy
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac
     ! serial number of candidate vertex contactor for contact icdan 
     icdver = this(icdan)%icdsci
     IF (verlt(icdtac)%adjsz /= 0) THEN
       if ( verlt(icdtac)%cdbdy  == polyr2bdyty(1,icdtac) .and. &
            verlt(icdtac)%cdtac  == polyr2bdyty(2,icdtac) .and. &
            verlt(icdtac)%cdmodel== polyr2bdyty(3,icdtac)  ) then
         do iadj = 1, verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == aspxx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == aspxx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== aspxx2bdyty(3,iantac) .and. &
                verlt(icdtac)%cdsci(iadj)  == icdver                 ) then
              this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
              this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
              this(icdan)%rln = verlt(icdtac)%rln(iadj)*H
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
              this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
              nb_recup_PRASp = nb_recup_PRASp + 1
              exit
           end if
         end do
       end if
     ENDIF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_PRASp,' recup PRASp'
   call logmes(cout)

 END SUBROUTINE recup_rloc_PRASp
!------------------------------------------------------------------------ 
 subroutine read_ini_Vloc_Rloc 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   implicit none

   character(len=103) :: clin
   integer            :: icdan,icdbdy,icdtac,icdtact,ianbdy,iantac,iadj,icdver
   integer            :: cdmodel, anmodel
   real(kind=8)       :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
   character(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   integer            :: errare,i_internal,nb_internal,ibehav
   character(len=29)  :: IAM = 'mod_PRASp::read_ini_Vloc_Rloc'
   character(len=103) :: cout
   nb_POLYR=get_nb_POLYR()

   errare=0
  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating nb_adj')
     END IF
   end if

   DO icdtac=1,nb_POLYR
     nb_adj(icdtac)=0
   END DO

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRASp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
     IF (cdtac == 'POLYR' .AND. antac == 'ASpxx') THEN
       cdmodel = get_body_model_id_from_name( cdbdy )
       do icdtact = 1, nb_POLYR
          if ( polyr2bdyty(1,icdtact) == icdbdy .and. &
               polyr2bdyty(2,icdtact) == icdtac .and. &
               polyr2bdyty(3,icdtact) == cdmodel ) then
             nb_adj(icdtact) = nb_adj(icdtact) + 1
             exit
          end if
       end do
     END IF
   END DO

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_POLYR),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtact=1,nb_POLYR
       verlt(icdtact)%adjsz=0
       iadj=nb_adj(icdtact)
       IF (iadj > 0) THEN
         verlt(icdtact)%adjsz=iadj
         call new_verlet_(icdtact, iadj, errare)
       ELSE
         call nullify_verlet_(icdtact)
       END IF
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtact,')%.....'
         call faterr(IAM,cout)
       END IF
     END DO
   ELSE 
     DO icdtact=1,nb_POLYR
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
  
   DO icdtact=1,nb_POLYR
     nb_adj(icdtact)=0
   END DO
   icdan = 0
  
  ! second reading: filling data
   REWIND(G_nfich)

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRASp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
     IF (cdtac == 'POLYR' .AND. antac == 'ASpxx') THEN
       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )
       do icdtact = 1, nb_POLYR
         if ( polyr2bdyty(1,icdtact) == icdbdy .and. &
              polyr2bdyty(2,icdtact) == icdtac .and. &
              polyr2bdyty(3,icdtact) == cdmodel ) then
       
           icdan = icdan + 1

           nb_adj(icdtact)=nb_adj(icdtact)+1 
           verlt(icdtact)%icdan(nb_adj(icdtact))  = icdan
           verlt(icdtact)%cdbdy                   = icdbdy
           verlt(icdtact)%cdtac                   = icdtac
           verlt(icdtact)%cdmodel                 = cdmodel
           verlt(icdtact)%cdsci(nb_adj(icdtact))  = icdver
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
           exit
         endif
       enddo   
     ENDIF
   ENDDO

   nb_vPRASp=0
    
   DO icdtact=1,nb_POLYR
      nb_vPRASp = nb_vPRASp + nb_adj(icdtact)
      
      IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
         WRITE(cout,'(A,I7,1X,A,1X,I7,A,I7)') 'Very strange for the contactor ',icdtact, &
              ' value of nb_adj is ',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         CALL FATERR(IAM,cout)
      END IF
   END DO

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
   INTEGER :: nfich,icdan,icdtac,iantac,icdver

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_POLYR=get_nb_POLYR()

   IF (nb_PRASp==0) RETURN

   DO icdtact=1,nb_POLYR    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         icdver = this(icdan)%icdsci
         cdmodel = get_body_model_name_from_id( polyr2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( aspxx2bdyty(3,iantac) )
         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PRASp',icdan  
         !1234567890123456789012345678901234567890123456789012345678901234567890123456'
         WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdmodel,get_visibleID_POLYR(icdtac),'POLYR',polyr2bdyty(2,icdtac),icdver,  &
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
 SUBROUTINE nullify_reac_PRASx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: icdtac,iantac
   INTEGER           :: storage
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_POLYR(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_ASpxx(iantac,storage)
    
 END SUBROUTINE nullify_reac_PRASx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_PRASx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac,storage
    
   icdtac = this(icdan)%icdtac
   CALL nullify_vlocy_POLYR(icdtac,storage)
   
   iantac = this(icdan)%iantac
   CALL nullify_vlocy_ASpxx(iantac,storage)
    
 END SUBROUTINE nullify_vlocy_PRASx
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_PRASx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
 
   CALL comp_vlocy_POLYR(this(icdan)%icdtac,storage)
    
   CALL comp_vlocy_ASpxx(this(icdan)%iantac,this(icdan)%iansci,storage,need_full_vlocy)
    
 END SUBROUTINE vitrad_PRASx
!------------------------------------------------------------------------  
 SUBROUTINE injj_PRASx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: cdccdof,anccdof
   REAL(kind=8),DIMENSION(6)  :: cdreac, anreac
   INTEGER                    :: icdtac,iantac
   INTEGER                    :: storage

   icdtac    = this(icdan)%icdtac
   iantac    = this(icdan)%iantac

   cdccdof(1:6)= (/ 1, 2, 3, 4, 5, 6 /)
   cdreac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)

   cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
   cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
   cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK

   CALL add_reac_POLYR(icdtac,cdccdof,cdreac,storage)

   anreac(1:3) = -cdreac(1:3)
   CALL add_reac_ASxxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight,anreac,storage)

 END SUBROUTINE injj_PRASx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_PRASx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: storage
   REAL(kind=8)              :: Vcd(6),Van(3)
   
   Vcd = get_vlocy_POLYR(this(icdan)%icdtac,storage)

   call get_vlocy_ASxxx(this(icdan)%iantac,this(icdan)%iansci,this(icdan)%weight,storage,Van)      


   VSIK = Vcd(1)*this(icdan)%suc(1)  + &
          Vcd(2)*this(icdan)%suc(2)  + &
          Vcd(3)*this(icdan)%suc(3)  + &
          Vcd(4)*this(icdan)%Gcds(1) + &
          Vcd(5)*this(icdan)%Gcds(2) + &
          Vcd(6)*this(icdan)%Gcds(3) - &
          Van(1)*this(icdan)%suc(1)  - &
          Van(2)*this(icdan)%suc(2)  - & 
          Van(3)*this(icdan)%suc(3)

   VTIK = Vcd(1)*this(icdan)%tuc(1)  + &
          Vcd(2)*this(icdan)%tuc(2)  + &
          Vcd(3)*this(icdan)%tuc(3)  + &
          Vcd(4)*this(icdan)%Gcdt(1) + &
          Vcd(5)*this(icdan)%Gcdt(2) + &
          Vcd(6)*this(icdan)%Gcdt(3) - &
          Van(1)*this(icdan)%tuc(1)  - &
          Van(2)*this(icdan)%tuc(2)  - &
          Van(3)*this(icdan)%tuc(3)

   VNIK = Vcd(1)*this(icdan)%nuc(1)  + &
          Vcd(2)*this(icdan)%nuc(2)  + & 
          Vcd(3)*this(icdan)%nuc(3)  + &
          Vcd(4)*this(icdan)%Gcdn(1) + &
          Vcd(5)*this(icdan)%Gcdn(2) + &
          Vcd(6)*this(icdan)%Gcdn(3) - &
          Van(1)*this(icdan)%nuc(1)  - &
          Van(2)*this(icdan)%nuc(2)  - &
          Van(3)*this(icdan)%nuc(3)
   
 END SUBROUTINE prjj_PRASx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PRASx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_PRASx = nb_PRASp
    CASE(i_verlet_tactor)
       get_nb_PRASx = nb_vPRASp
    CASE(i_rough_tactor)
       get_nb_PRASx = nb_rough_PRASp
    CASE(i_recup_tactor)
       get_nb_PRASx = nb_recup_PRASp
    END SELECT

  END FUNCTION get_nb_PRASx
!------------------------------------------------------------------------ 
SUBROUTINE PRASx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_POLYR(this(icdan)%icdtac)
   ianent = get_ENT_ASpxx(this(icdan)%iantac)

 END SUBROUTINE PRASx2ENT
!!!------------------------------------------------------------------------ 
SUBROUTINE PRASx2POLYR(icdan,icdtac)

  IMPLICIT NONE
  INTEGER :: icdan,icdtac
   
  icdtac = this(icdan)%icdtac

END SUBROUTINE PRASx2POLYR
!!!------------------------------------------------------------------------ 
SUBROUTINE PRASx2ASpxx(icdan,iantac)

  IMPLICIT NONE
  INTEGER :: icdan,iantac
   
  iantac = this(icdan)%iantac

END SUBROUTINE PRASx2ASpxx
!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_PRASp()

    IMPLICIT NONE
    
    RUN_PRASp = RUN_TACTOR

  END FUNCTION RUN_PRASp
!!!------------------------------------------------------------------------
  logical function CHECK_PRASp()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PRASp = check_PRASp_
      return
    end if

    con_pedigree%module_name = 'PRASp'

    con_pedigree%id_cdan  = i_prasp
    con_pedigree%id_cdtac = i_polyr
    con_pedigree%id_antac = i_aspxx

    cdtact2bdyty => polyr2bdyty
    antact2bdyty => aspxx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_POLYR = get_nb_POLYR()
    nb_ASpxx = get_nb_ASpxx()
    if( nb_POLYR == 0 .or. nb_ASpxx == 0 ) then
      CHECK_PRASp = check_PRASp_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'POLYR' .and. see(isee)%antac == 'ASpxx') then
        check_PRASp_ = .true.
        exit
      end if
    end do

    CHECK_PRASp = check_PRASp_
    return

  end function CHECK_PRASp
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_PRASp()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_PRASp = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_PRASp

!!!-----------------------------------------------------------------------

!!!-----------------------------------------------------------------------

  subroutine clean_memory_PRASp
    implicit none
    integer(kind=4) :: i, j, k

    call clean_memory_inter_meca_()

    ! \todo violation
    nb_POLYR       = 0
    nb_ASpxx       = 0
    nb_PRASp       = 0
    nb_vPRASp      = 0
    nb_recup_PRASp = 0

    nb_rough_PRASp = 0
    if( allocated(rough_PRASp) ) deallocate(rough_PRASp)

    ! Root, Current and Previous should always be null outside creation_tab_visu

    if ( allocated(PR) ) then
      do i=1,size(PR)
        deallocate(PR(i)%normal)
      enddo
      deallocate(PR)
    endif

    if( allocated(ASp) ) then
      do i = 1, size(ASp)
        if( associated(ASp(i)%coor)  ) deallocate(ASp(i)%coor)
        if( associated(ASp(i)%radius)) deallocate(ASp(i)%radius)
        if( associated(ASp(i)%good_vertices)) deallocate(ASp(i)%good_vertices)
      end do
      deallocate(ASp)
    end if

    if ( allocated(nb_ASxxx) ) deallocate(nb_ASxxx)

    Reac_Prasp_MAX = 0.D0

    module_checked_ = .FALSE.
    check_PRASp_    = .FALSE.

  end subroutine

!!!--pta--27/09/2013----------------------------------------------------------------- 
  REAL(kind=8) FUNCTION get_corresponding_polyr_radius_PRASx(icdan) 

    IMPLICIT NONE   
    INTEGER :: icdan
    
    get_corresponding_polyr_radius_PRASx = get_radius_POLYR(this(icdan)%icdbdy)
    
  END FUNCTION get_corresponding_polyr_radius_PRASx
!!!------------------------------------------------------------------------ 

 REAL(kind=8) function get_surf_PRASx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan

   get_surf_PRASx = 0.d0 ! todo this(icdan)%surf

 END function get_surf_PRASx

 subroutine set_nb_PRASx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PRASp = nb

 end subroutine

 subroutine redo_nb_adj_PRASx()
   implicit none

   call redo_nb_adj_( get_nb_POLYR() )

 end subroutine

END MODULE Prasp

