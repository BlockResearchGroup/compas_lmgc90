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
 MODULE CSPRx                                          

  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors POLYR and CSpxx.
  !!  In this modulus candidate contactors are CSpxx and antagonist 
  !!  contactors are POLYR


 USE overall
 USE tact_behaviour
 USE DiscreteGeometry
 USE POLYR
 USE CSxxx
 
 use MAILx, only : get_color_MAILx
 use RBDY3, only : get_color_RBDY3 => get_color
 use MBS3D, only : get_color_MBS3D => get_color

 use inter_meca_3D

 use parameters, only : i_csprx, i_mailx, i_rbdy3, i_mbs3

 use ann, ann_clean_memory => clean_memory
 
 implicit none

 private

 CHARACTER(len=5) :: BOBO='CSPRx'
 INTEGER          :: nb_POLYR
 INTEGER          :: nb_CSxxx

 type(T_interaction), dimension(:), allocatable, target :: this

 !fd < a merger
  
 type(T_con),target :: con_pedigree 

 integer, dimension(:,:), pointer :: cdtact2bdyty => null()
 integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 INTEGER,PRIVATE :: nb_CSPRx=0 , nb_vCSPRx=0 ,nb_recup_CSPRx  ! nb_CSPRx = number of selected candidates CSxxx against POLYR 
                                                              ! <= size(this).

!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj ! nb_adj(icdtac): number of adjacent pairs POLYR-POLYR
                                                        ! to candidate contactor POLYR icdtac.

!------------------------------------------------------------------------ 


 type(T_verlet), dimension(:), allocatable, target ::verlt

!-------------------------------------------------------------------------
 TYPE T_rough_CSPRx                                                   ! définit le type de la liste des plus proches voisins

   INTEGER                   :: cd                                    ! le candidat, l'antagoniste et isee pour la loi de contact
   INTEGER                   :: an 
                              
   INTEGER                   :: isee

   real(kind=8),dimension(3) :: cd_normalTT                           ! pour eliminer des faces de polyedres
 END TYPE T_rough_CSPRx

 TYPE(T_rough_CSPRx),DIMENSION(:),ALLOCATABLE   :: rough_CSPRx        ! table  de visibilité
 INTEGER                                        :: nb_rough_CSPRx     ! nombre de paire de polygone à analyser pour déterminer
                                                                      ! s'il y a contact ou pas par détect

 TYPE T_link_rough_CSPRx                                              ! liste chainée pour determiner les listes de cand_ant car
                                                                      ! on ne connait pas a priori le nb de cand-ant 
    TYPE(T_link_rough_CSPRx), POINTER :: p                            ! pointeur sur le precedent
    TYPE(T_rough_CSPRx)               :: val                          ! les valeurs
    TYPE(T_link_rough_CSPRx), POINTER :: n                            ! pointeur sur le suivant

 END TYPE T_link_rough_CSPRx

 TYPE(T_link_rough_CSPRx),POINTER     :: Root,Current,Previous

!------------------------------------------------------------------------
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: CScoor
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PRcoor

 REAL(kind=8) :: Reac_CSPRx_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

 INTEGER,PRIVATE   :: ii,l_ii,iv
 INTEGER,PRIVATE   :: Nstep_creation_tab_visu=1,restart=0
 LOGICAL,PRIVATE   :: write_creation_tab_visu
!------------------------------------------------------------------------

 logical :: trim_contact = .FALSE. 

 logical      :: module_checked_ = .FALSE.
 logical      :: check_CSPRx_    = .FALSE.

! necessary to read old ini files (the one with the CSpxx rank instead of CSxxx one) 
 logical            :: old_way=.FALSE.

 
! liste des fonctions publiques 
!
 PUBLIC &
       coor_prediction_CSPRx,&
       CHECK_CSPRx,&
       RUN_CSPRx, &
       get_write_Vloc_Rloc_CSPRx, &
       read_ini_Vloc_Rloc_CSPRx,&
       write_xxx_Vloc_Rloc_CSPRx,&
       stock_rloc_CSPRx, &
       recup_rloc_CSPRx, &
       recup_rloc_by_position_CSPRx, &       
       creation_tab_visu_CSPRx, &
       compute_contact_CSPRx, &
       display_prox_tactors_CSPRx,&
       get_nb_CSPRx, &
       trim_CSPRx, &
       get_info_CSPRx, &
       smoothing_CSPRx, &
       add_reac_CSPRx

 PUBLIC &
      nullify_reac_CSPRx, nullify_vlocy_CSPRx,injj_CSPRx, prjj_CSPRx, vitrad_CSPRx,  & 
      CSPRx2ENT, CSPRx2POLYR,                    &
      display_vlocy_CSPRx, get_surf_CSPRx

  public clean_memory_CSPRx

  !rm for handler
  public get_this    , &
         set_nb_CSPRx, &
         redo_nb_adj_CSPRx, &
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
!-------------------------------------------------------------------------- 
! Subroutine pour actualiser les positions des vertex des polyèdres
! au cours du temps
!--------------------------------------------------------------------------
  SUBROUTINE coor_prediction_CSPRx

    IMPLICIT NONE  

    INTEGER :: errare 
    INTEGER :: itacty

    IF (.NOT. ALLOCATED(PRcoor))   ALLOCATE(PRcoor(3,nb_POLYR))

    call move_polyr

    ! -> fait lors du move_polyr  call update_HE_Hdl(S_POLYR(iantac)%HE_Hdl,S_POLYR(iantac)%vertex)

    DO itacty=1,nb_POLYR
       PRcoor(1:3,itacty) = get_coorTT_POLYR(itacty)
    END DO

    ! actualisation de la structure CSpxx courante
    call increment_CSpxx()

    IF (.NOT. ALLOCATED(CScoor)) ALLOCATE(CScoor(3,nb_CSxxx))

    DO itacty=1,nb_CSxxx
      CScoor(1:3,itacty) = get_coorTT_CSxxx(itacty)
    END DO

 END SUBROUTINE coor_prediction_CSPRx
!!!---------------------------------------------------------------
 !> \brief Read a VlocRloc file to initialize database
 subroutine read_ini_Vloc_Rloc_CSPRx(step)
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
   
 end subroutine read_ini_Vloc_Rloc_CSPRx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_CSPRx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_CSPRx
!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_CSPRx

  IMPLICIT NONE

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,icdtac,iantac,itac
  INTEGER                               :: isee,itacty,i
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol

  REAL(kind=8),DIMENSION(3)             :: sep,axe,point,vect                  ! vecteurs reliant les centres des polygones
  REAL(kind=8)                          :: norm,dist,adist,norm1,dist1,norm2         ! real pour calculer la norme au carrée de sep

  LOGICAL :: visible

  integer :: itouche
  character(len=80) :: cout
                             !1234567890123456789012345678
  character(len=28) :: IAM = 'mod_CSPRx::creation_tab_visu'


  ! Detecting contacts; 

  ! first reading: sizing array adjac
   
  nb_CSxxx = get_nb_CSxxx() 

  IF (.NOT. ALLOCATED(adjac)) THEN
    ALLOCATE(adjac(nb_CSxxx),stat=errare)
    IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating adjac')
    END IF
    DO itacty=1,nb_CSxxx
      NULLIFY(adjac(itacty)%icdan)
    ENDDO 
  ENDIF

  DO icdtac=1,nb_CSxxx
    IF (ASSOCIATED(adjac(icdtac)%icdan)) DEALLOCATE(adjac(icdtac)%icdan) 
    NULLIFY(adjac(icdtac)%icdan)
  ENDDO 

  IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
  ALLOCATE(nb_adj(nb_CSxxx),stat=errare)
  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating nb_adj')
  END IF    

  nb_adj=0
  nb_rough_CSPRx=0

! creation de la liste de paire de polygones a examiner

! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)

!fd pre-detection par: 
!fd   - distance(sphere englobante du POLYR, vertex CS)

!fd todo: recherche n^2 => pas bon, a reprendre

  DO icdtac=1,nb_CSxxx
    visible=get_visible_CSxxx(icdtac)
    IF (.NOT.visible) CYCLE
    
    cdcol=get_color_CSxxx(icdtac)

    DO iantac=1,nb_POLYR
      visible=get_visible_POLYR(iantac)
      IF (.NOT.visible) CYCLE

      ancol=get_color_POLYR(iantac)

      isee = get_isee('MAILx','CSxxx',cdcol,'RBDY3','POLYR',ancol)

      ! print*,'MAILx','CSxxx',cdcol,'RBDY3','POLYR',ancol
      ! print*,isee

      IF (isee/=0) THEN

        !fd on se sert volontairement de la distance d'alerte locale
        !   pour le calcul de la boite englobante. 
        adist=see(isee)%alert 

        !print*,S_POLYR(iantac)%maxpos(1:3)
        !print*,S_POLYR(iantac)%minpos(1:3)


        IF ((S_POLYR(iantac)%maxpos(1)-CScoor(1,icdtac)+adist)<0.D0) CYCLE
        IF ((S_POLYR(iantac)%maxpos(2)-CScoor(2,icdtac)+adist)<0.D0) CYCLE
        IF ((S_POLYR(iantac)%maxpos(3)-CScoor(3,icdtac)+adist)<0.D0) CYCLE

!fd @@@ je rajoute ....

        IF ((CScoor(1,icdtac)-S_POLYR(iantac)%minpos(1)+adist)<0.D0) CYCLE
        IF ((CScoor(2,icdtac)-S_POLYR(iantac)%minpos(2)+adist)<0.D0) CYCLE
        IF ((CScoor(3,icdtac)-S_POLYR(iantac)%minpos(3)+adist)<0.D0) CYCLE


        nb_rough_CSPRx=nb_rough_CSPRx+1
        IF ( nb_rough_CSPRx == 1) THEN
          ALLOCATE(Root)
          Current => Root
          NULLIFY(Root%p)
        ELSE
          ALLOCATE(Current)
          Previous%n => Current
        ENDIF
        Current%val%cd    = icdtac
        Current%val%an    = iantac
        Current%val%isee  = isee
        Current%val%cd_normalTT = get_normalTT_CSxxx(icdtac)
        Current%p => Previous
        NULLIFY(Current%n)
        Previous => Current
      ENDIF
    ENDDO
  ENDDO     

  WRITE(cout,'(4X,I10,A20)') nb_rough_CSPRx,' CSPRx roughly found'       
  call logmes(cout)

  IF (ALLOCATED(rough_CSPRx)) DEALLOCATE(rough_CSPRx)
  ALLOCATE(rough_CSPRx(nb_rough_CSPRx))              ! on s'alloue la table de visibilité utilisée dans compute_contact

  IF (ALLOCATED(this)) DEALLOCATE(this)
  ALLOCATE(this(nb_rough_CSPRx))     ! on s'alloue un tableau temporaire de contact.On lui donne une taille 3*nb_paire_phphx
                                       ! car il y a au maximun trois points de contact entre un candidat - antagoniste
  DO i=nb_rough_CSPRx,1,-1

    Previous => Current%p
    rough_CSPRx(i)%cd          = Current%val%cd
    rough_CSPRx(i)%an          = Current%val%an
    rough_CSPRx(i)%isee        = Current%val%isee
    rough_CSPRx(i)%cd_normalTT = Current%val%cd_normalTT
    DEALLOCATE(Current)
    Current => Previous
  END DO 

  NULLIFY(Root)

END SUBROUTINE creation_tab_visu_CSPRx
!------------------------------------------------------------------------
SUBROUTINE compute_contact_CSPRx
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,ibdy,icdtac,iantac,isee,itacty    
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  REAL(kind=8)                          :: adist,dist,min_dist
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

  integer    :: v,f,fj,vd,vf,f0,min_vert,k,ibad,ibad_k,vd_aux
  

  type(T_HE) :: HE,HE_aux,min_HE,HE_null
  real(kind=8),dimension(3) :: orien,vec,vv,vec_aux,interc,dir,weight_out
  real(kind=8) :: sens,apab,norm

  real(kind=8),dimension(3) :: cd_normalTT

  logical :: itchatche=.false.

  integer :: icheck,ppp,f_out
  character(len=80) :: cout

  integer :: err_
  
  icdan=0        
  nb_CSPRx=0
  nb_adj=0

  IF (nb_rough_CSPRx /= 0 ) THEN
     !
     ! Fine Detection
     !
     DO i=1,nb_rough_CSPRx

       icdtac=rough_CSPRx(i)%cd  ! CSxxx
       iantac=rough_CSPRx(i)%an  ! Polyhedron
       isee=rough_CSPRx(i)%isee
       cd_normalTT = rough_CSPRx(i)%cd_normalTT
       adist=see(isee)%alert 

       min_dist=1.D20
       min_vert=0

       !
       ! recompute HEorien
       !
       !-> fait lors du move_polyr call update_HE_Hdl(S_POLYR(iantac)%HE_Hdl,S_POLYR(iantac)%vertex)


       ! on recupere la normale du CSxxx
       dir = get_normalTT_CSxxx(icdtac)
       ppp=0

       !print*,'ok'

       !
       !print*,'=================='
       !print*,'CSxxx ',icdtac,' POLYR ',iantac
       !print*,'mecamailx ', csxxx2bdyty(1,icdtac),' rank csxxx ', csxxx2bdyty(2,icdtac)
       !print*,'normale face cd'
       !print*,dir

       itchatche = .false.


       !if (icdtac == 18 .and. iantac == 1) itchatche = .true.
       !if (icdtac == 19 .and. iantac == 1) itchatche = .true.
       !print*,isee,see(isee)%global_alert

       icheck = node_HE_Hdl_proximity(S_POLYR(iantac)%HE_Hdl,CScoor(:,icdtac),see(isee)%global_alert,dir, &
                                      .true.,ppp,ovlap,point,t,n,s,f_out,weight_out,itchatche,err_)
       !
       if (err_ > 0) then
         write(cout,'("cd ",I0," an ",I0)') icdtac,iantac
         call logmes(cout)
         call faterr('CSPRx:: compute_contact','unexpected problem in node HE proximity')
       endif   

       !print*,icheck,icdtac,iantac
       !print*,CScoor(:,icdtac)
       !fd 
       if (trim_contact .and. icheck < 3) cycle

       !print*,'icheck: ',icheck
       !print*,ovlap,adist

       !if (icheck > 0 ) then
       !
       !  print*,'ppp ',ppp,' gap ',ovlap
       !  print*,'point de contact ',point
       !
       !  print*,'face ',f_out
       !  print*,'ponderation ',weight_out
       !  print*,'normale face an'
       !  print*,S_POLYR(iantac)%HE_Hdl%normal(:,f_out) 
       !endif

       !print*,'ok'
       
       !f_out/weight_out la face/les coordonnees reduites dans la face

       if (icheck <= 0 .or. ovlap > adist) cycle 

       icdan          = icdan + 1
       nb_adj(icdtac) = nb_adj(icdtac) + 1

       this(icdan)%icdbtac = csxxx2bdyty(2, icdtac)
       this(icdan)%ianbtac = polyr2bdyty(2, iantac)

       this(icdan)%icdbtyp = csxxx2bdyty(3, icdtac)
       this(icdan)%ianbtyp = polyr2bdyty(3, iantac)

       this(icdan)%icdctyp = i_csxxx
       this(icdan)%ianctyp = i_polyr

       iadj = nb_adj(icdtac)

       this(icdan)%iadj   = iadj
       this(icdan)%icdbdy = csxxx2bdyty(1, icdtac)
       this(icdan)%icdtac = icdtac
       this(icdan)%ianbdy = polyr2bdyty(1, iantac)
       this(icdan)%iantac = iantac
       this(icdan)%isee   = isee
       this(icdan)%nuc    = n
       this(icdan)%tuc    = t
       this(icdan)%suc    = s

       this(icdan)%icdsci  = get_sci_CSxxx(icdtac)
       this(icdan)%iansci  = 0

       cd_ent = get_ent_CSxxx(icdtac)
       entity(cd_ent)%nb = entity(cd_ent)%nb + 1

       an_ent            = get_ent_POLYR(iantac)
       entity(an_ent)%nb = entity(an_ent)%nb + 1

       this(icdan)%icdent = cd_ent
       this(icdan)%ianent = an_ent

       this(icdan)%coor       = point
       this(icdan)%gapTTbegin = ovlap

       ! Compute the mapping: inertia frame to general frame for antagonist
   
       anlev         = point(:) - PRcoor(:, iantac)
       localframe_an = get_inertia_frameTT_POLYR(iantac)

       Rc(1,1)=localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
       Rc(2,1)=localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
       Rc(3,1)=localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

       Rc(1,2)=localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
       Rc(2,2)=localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
       Rc(3,2)=localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

       Rc(1,3)=localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
       Rc(2,3)=localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
       Rc(3,3)=localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)


       this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
       this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
       this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

       this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
       this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
       this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

       this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
       this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
       this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

 
!       print*,this(icdan)%Gans
!       print*,this(icdan)%Gant
!       print*,this(icdan)%Gann

       ! Computation of relatives velocities
       ! Constante quantities for every contact


       call get_vlocy_CSxxx(icdtac, iVbeg_, cd_Vbegin)
       an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)

       vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ & 
               (cd_Vbegin(2)-an_Vbegin(2))*s(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*s(3)
       vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ &
               (cd_Vbegin(2)-an_Vbegin(2))*t(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*t(3)
       vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ &
               (cd_Vbegin(2)-an_Vbegin(2))*n(2)+ &
               (cd_Vbegin(3)-an_Vbegin(3))*n(3)


       this(icdan)%vlsBEGIN= vls_cst &     
                - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)

       this(icdan)%vltBEGIN = vlt_cst &
                - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

       this(icdan)%vlnBEGIN = vln_cst &     
                - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

       this(icdan)%rls=0.D0
       this(icdan)%rlt=0.D0
       this(icdan)%rln=0.D0
       this(icdan)%vls=this(icdan)%vlsBEGIN
       this(icdan)%vlt=this(icdan)%vltBEGIN
       this(icdan)%vln=this(icdan)%vlnBEGIN

       this(icdan)%status=i_nknow

     ENDDO
     nb_CSPRx=icdan
   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_CSPRx,' CSPRx found'       
   call logmes(cout)

   DO ibdy=1,nb_CSxxx
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating adjac(',icdtac,')%.....'
         call faterr('mod_CSPRx::compute_contact',cout)
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_CSPRx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 


   do icdan = 1, nb_CSPRx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_CSPRx),stat=errare)

END SUBROUTINE compute_contact_CSPRx

!------------------------------------------------------------------------
 subroutine display_prox_tactors_CSPRx

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee
   integer          :: nb_CSxxx
   character(len=5) :: cdmodel, anmodel

   nb_CSxxx=get_nb_CSxxx()

   IF (nb_CSPRx==0) RETURN
   DO icdtac=1,nb_CSxxx    
     DO iadj=1,nb_adj(icdtac)         
       icdan  = adjac(icdtac)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
      !icdtac = this(icdan)%icdtac
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( csxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,icdbdy,'CSxxx',icdtac,see(this(icdan)%isee)%behav,  &
       anmodel,ianbdy,'POLYR',iantac
                    
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
   
 end subroutine display_prox_tactors_CSPRx
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_CSPRx
   !
   ! get data from this and put into verlt
   !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,nb_POLYR
   INTEGER :: errare

   character(len=80) :: cout
                              !12345678901234567890
   character(len=20) :: IAM = 'mod_CSPR::stock_rloc'

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
   do icdan = 1, nb_CSPRx

     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan 
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor 
     iadj   = this(icdan)%iadj
     ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = csxxx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = csxxx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel         = csxxx2bdyty(3,icdtac)
     verlt(icdtac)%anbdy(iadj)     = polyr2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = polyr2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)   = polyr2bdyty(3,iantac)
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
   end do

   nb_vCSPRx = nb_CSPRx

   WRITE(cout,'(1X,I10,A12)') nb_vCSPRx,' stock CSPRx'
   call logmes(cout)

 END SUBROUTINE stock_rloc_CSPRx
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_CSPRx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER           :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   character(len=80) :: cout
   logical           :: cdcheck

                           !12345678901234567
   character(len=17):: IAM='CSPRx::recup_rloc'
   
   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_CSPRx=0 
   DO icdan=1,nb_CSPRx
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
     IF (verlt(icdtac)%adjsz /= 0) THEN
        
       if ( verlt(icdtac)%cdbdy  == csxxx2bdyty(1,icdtac) .and. &
            verlt(icdtac)%cdtac  == csxxx2bdyty(2,icdtac) .and. &
            verlt(icdtac)%cdmodel== csxxx2bdyty(3,icdtac)       &
           ) then
         do iadj = 1, verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == polyr2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == polyr2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== polyr2bdyty(3,iantac) .and. &
                verlt(icdtac)%cdsci(iadj)  == get_sci_CSxxx(icdtac)       &
              ) then
              this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
              this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
              this(icdan)%rln = verlt(icdtac)%rln(iadj)*H
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
              this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
              nb_recup_CSPRx=nb_recup_CSPRx+1
              exit
           end if
         end do
       end if
     ENDIF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_CSPRx,' recup CSPRx'
   call logmes(cout)

 END SUBROUTINE recup_rloc_CSPRx

 !------------------------------------------------------------------------  
 !> Get data from verlet list verlt and put into this
 subroutine recup_rloc_by_position_CSPRx(tol)
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
   character(len=33), parameter :: IAM = 'mod_CSPRx::recup_rloc_by_position'
   character(len=31) :: cout
   
   nb_recup_CSPRx = 0
   if( nb_vCSPRx < 1 ) then
     do icdan=1,nb_CSPRx

       this(icdan)%rlt = 0.d0
       this(icdan)%rln = 0.d0
       this(icdan)%rls = 0.d0
       this(icdan)%statusBEGIN = i_nknow

     end do
     write(cout,'(1X,I10,A12)') nb_recup_CSPRx,' recup CSPRx'
     call logmes(cout)
     return
   end if

   call set_nb_kd(1)  

   allocate( verlt_coor(nbDIME, nb_vCSPRx) )
   allocate( point(nbDIME) )

   ! map to link index in ann tree and candidate and adjacent id
   ! and if this interaction has already been got back
   allocate( imap(3, nb_vCSPRx) )
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

   call add_kd_tree(1, verlt_coor, nb_vCSPRx, nbDIME)

   do icdan = 1, nb_CSPRx

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

       nb_recup_CSPRx = nb_recup_CSPRx + 1

     end if ! if ann found a nearest

   end do ! loop on this

   write(cout,'(1X,I10,A12)') nb_recup_CSPRx,' recup CSPRx'
   call logmes(cout)

   call ann_clean_memory()

   deallocate( verlt_coor )
   deallocate( point )
   deallocate( imap )

 end subroutine recup_rloc_by_position_CSPRx


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
   character(len=29)  :: IAM = 'mod_CSPRx::read_ini_Vloc_Rloc'
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
     IF (G_clin(9:13)/= 'CSPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,                                          &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus

     IF (cdtac == 'CSxxx' .AND. antac == 'POLYR') THEN
        
       cdmodel = get_body_model_id_from_name( cdbdy )
 
       do icdtact=1,nb_CSxxx
          if (old_way) then
            if ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                 csxxx2bdyty(2,icdtact) == icdtac .and. &
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               nb_adj(icdtact)=nb_adj(icdtact)+1    
               exit
            end if
          else   
            if ( csxxx2bdyty(1,icdtact) == icdbdy .and. &
                               icdtact  == icdtac .and. &               
                 csxxx2bdyty(3,icdtact) == cdmodel ) then
               nb_adj(icdtact)=nb_adj(icdtact)+1    
               exit
            end if
          endif  
       end do
     END IF
   END DO

   ! print*,'csxx2bdyty ',nb_CSxxx
   ! DO icdtact=1,nb_CSxxx
   !   print*,'CSxxx ',icdtact, csxxx2bdyty(:,icdtact)
   !   print*,nb_adj(icdtact)
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

   !DO icdtac=1,nb_CSxxx
   !  nb_adj(icdtac)=0
   !END DO

   nb_adj = 0
   icdan  = 0

   ! second reading: filling data
   REWIND(G_nfich)
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'CSPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:103),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
     IF (cdtac == 'CSxxx' .AND. antac == 'POLYR') THEN

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )
       
       do icdtact=1,nb_CSxxx
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
         
         if (to_read) then
           icdan = icdan + 1

           nb_adj(icdtact)=nb_adj(icdtact)+1 

           verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

           verlt(icdtact)%cdbdy                   = icdbdy
           verlt(icdtact)%cdtac                   = csxxx2bdyty(2,icdtact)
           verlt(icdtact)%cdmodel                 = cdmodel
           verlt(icdtact)%cdsci(nb_adj(icdtact))  = get_sci_CSxxx(icdtac)
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
               READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
                  verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
             ENDDO
           ENDIF
           exit
         endif
       enddo
     ENDIF
   ENDDO

   nb_vCSPRx=0
    
   DO icdtact=1,nb_CSxxx
      nb_vCSPRx = nb_vCSPRx + nb_adj(icdtact)
      
      IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
         WRITE(cout,'(A,I7,1X,A,1X,I7,A,I7)') 'Very strange for the contactor ',icdtact, &
              'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
         CALL FATERR(IAM,cout)
      END IF
   END DO

   write(cout,'(A,I0,A)') 'read ',nb_vCSPRx,' CSPRx'
   call logmes(cout)
   
104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
 end subroutine read_ini_Vloc_Rloc
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

   IF (nb_CSPRx==0) RETURN

   DO icdtact=1,nb_CSxxx    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         cdmodel = get_body_model_name_from_id( csxxx2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )
         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CSPRx',icdan  
         !1234567890123456789012345678901234567890123456789012345678901234567890123456'
         WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus'
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdmodel,csxxx2bdyty(1,icdtac),'CSxxx',icdtac, &   ! csxxx2bdyty(2,icdtac),  & <- pas bon car c est le rang du CSpxx
              see(this(icdan)%isee)%behav,  &
              !pta old fashion 'RBDY3',polyr2bdyty(1,iantac),'POLYR',polyr2bdyty(2,iantac),  &
              anmodel,get_visibleID_POLYR(iantac),'POLYR',polyr2bdyty(2,iantac),  &
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
 SUBROUTINE nullify_reac_CSPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: icdtac,iantac
   INTEGER           :: storage
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_CSxxx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_POLYR(iantac,storage)
    
 END SUBROUTINE nullify_reac_CSPRx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_CSPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac,storage
    
   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_CSxxx(icdtac,storage)
   
   iantac = this(icdan)%iantac
   CALL nullify_vlocy_POLYR(iantac,storage)
    
 END SUBROUTINE nullify_vlocy_CSPRx
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_CSPRx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
    
   icdtac=this(icdan)%icdtac
   CALL comp_vlocy_CSxxx(icdtac,storage,need_full_vlocy)
    
   iantac=this(icdan)%iantac
   CALL comp_vlocy_POLYR(iantac,storage)
    
 END SUBROUTINE vitrad_CSPRx
!------------------------------------------------------------------------  
 SUBROUTINE injj_CSPRx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: anccdof = (/ 1,2,3,4,5,6 /)
   REAL(kind=8),DIMENSION(3)  :: cdreac
   REAL(kind=8),DIMENSION(6)  :: anreac
   INTEGER                    :: icdtac,iantac
   INTEGER                    :: storage

   icdtac    = this(icdan)%icdtac
   cdreac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   CALL add_reac_CSxxx(icdtac,cdreac,storage)

   iantac    = this(icdan)%iantac
   anreac(1) =-cdreac(1)
   anreac(2) =-cdreac(2)
   anreac(3) =-cdreac(3)
   anreac(4) =-this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
   anreac(5) =-this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
   anreac(6) =-this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK

   anreac(:) = anreac(:)*get_weight_CSxxx(icdtac)

   CALL add_reac_POLYR(iantac,anccdof,anreac,storage)

 END SUBROUTINE injj_CSPRx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_CSPRx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: icdtac,iantac
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   
   icdtac=this(icdan)%icdtac
   call get_vlocy_CSxxx(icdtac,storage,Vcd)

   iantac=this(icdan)%iantac
   Van = get_vlocy_POLYR(iantac,storage)      

   VSIK = Vcd(1)*this(icdan)%suc(1) + Vcd(2)*this(icdan)%suc(2) + Vcd(3)*this(icdan)%suc(3)        &
        - Van(1)*this(icdan)%suc(1) - Van(2)*this(icdan)%suc(2) - Van(3)*this(icdan)%suc(3)        &
        - Van(4)*this(icdan)%Gans(1)- Van(5)*this(icdan)%Gans(2)- Van(6)*this(icdan)%Gans(3)

   VTIK = Vcd(1)*this(icdan)%tuc(1) + Vcd(2)*this(icdan)%tuc(2) + Vcd(3)*this(icdan)%tuc(3)        &
        - Van(1)*this(icdan)%tuc(1) - Van(2)*this(icdan)%tuc(2) - Van(3)*this(icdan)%tuc(3)        &
        - Van(4)*this(icdan)%Gant(1)- Van(5)*this(icdan)%Gant(2)- Van(6)*this(icdan)%Gant(3)

   VNIK = Vcd(1)*this(icdan)%nuc(1) + Vcd(2)*this(icdan)%nuc(2) + Vcd(3)*this(icdan)%nuc(3)        &
        - Van(1)*this(icdan)%nuc(1) - Van(2)*this(icdan)%nuc(2) - Van(3)*this(icdan)%nuc(3)        &
        - Van(4)*this(icdan)%Gann(1)- Van(5)*this(icdan)%Gann(2)- Van(6)*this(icdan)%Gann(3)

 END SUBROUTINE prjj_CSPRx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_CSPRx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_CSPRx = nb_CSPRx
    CASE(i_verlet_tactor)
       get_nb_CSPRx = nb_vCSPRx
    CASE(i_rough_tactor)
       get_nb_CSPRx = nb_rough_CSPRx
    CASE(i_recup_tactor)
       get_nb_CSPRx = nb_recup_CSPRx
    END SELECT

  END FUNCTION get_nb_CSPRx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
  SUBROUTINE CSPRx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent

   icdent = get_ENT_CSxxx(this(icdan)%icdtac)
   ianent = get_ENT_POLYR(this(icdan)%iantac)

  END SUBROUTINE CSPRx2ENT

!!!------------------------------------------------------------------------ 

  SUBROUTINE CSPRx2CSxxx(icdan,icdtac)

    IMPLICIT NONE
    INTEGER :: icdan,icdtac
   
    icdtac = this(icdan)%icdtac

  END SUBROUTINE CSPRx2CSxxx

!!!------------------------------------------------------------------------ 

  SUBROUTINE CSPRx2POLYR(icdan,iantac)

    IMPLICIT NONE
    INTEGER :: icdan,iantac
   
    iantac = this(icdan)%iantac

  END SUBROUTINE CSPRx2POLYR

!------------------------------------------------------------------------ 

  LOGICAL FUNCTION RUN_CSPRx()

    IMPLICIT NONE
    
    RUN_CSPRx = RUN_TACTOR

  END FUNCTION RUN_CSPRx

!!!------------------------------------------------------------------------

  logical function CHECK_CSPRx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_CSPRx = check_CSPRx_
      return
    end if

    con_pedigree%module_name = 'CSPRx'

    con_pedigree%id_cdan  = i_csprx
    con_pedigree%id_cdtac = i_csxxx
    con_pedigree%id_antac = i_polyr

    cdtact2bdyty => csxxx2bdyty
    antact2bdyty => polyr2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_POLYR = get_nb_POLYR()
    nb_CSxxx = get_nb_CSxxx()

    if( nb_POLYR == 0 .or. nb_CSxxx == 0 ) then
      CHECK_CSPRx = check_CSPRx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'CSxxx' .and. see(isee)%antac == 'POLYR') then
        check_CSPRx_ = .true.
        exit
      end if
    end do

    CHECK_CSPRx = check_CSPRx_

    return

  end function CHECK_CSPRx

!!!------------------------------------------------------------------------ 

  LOGICAL FUNCTION get_write_Vloc_Rloc_CSPRx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_CSPRx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_CSPRx

!!!------------------------------------------------------------------------ 

  subroutine trim_CSPRx
    implicit none

    trim_contact = .TRUE.

  end subroutine

!!!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE display_vlocy_CSPRx(icdan,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   INTEGER                   :: icdtac,iantac
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   
   icdtac=this(icdan)%icdtac
   call get_vlocy_CSxxx(icdtac,storage,Vcd)

   iantac=this(icdan)%iantac
   Van = get_vlocy_POLYR(iantac,storage)      

   write(*,'(A,3(1x,D12.5))') 'vcd= ',vcd(1:3)
   write(*,'(A,6(1x,D12.5))') 'van= ',van
   write(*,'(A,3(1x,D12.5))') 't  = ',this(icdan)%tuc
   write(*,'(A,3(1x,D12.5))') 'n  = ',this(icdan)%nuc
   write(*,'(A,3(1x,D12.5))') 's  = ',this(icdan)%suc


 END SUBROUTINE display_vlocy_CSPRx 
!!!------------------------------------------------------------------------ 
 function get_all_CSPRx()
   !11 coor(3),fn,n(3),ft,t(3)
   implicit none
   real(kind=8),dimension(:,:),pointer ::  get_all_CSPRx

   ! ***
   real(kind=8)              :: rt
   real(kind=8),dimension(3) :: t

                            !12345678901234
   character(len=14) :: IAM='CSPRx::get_all'
   integer :: nb_CSxxx,icdtac,iadj,icdan

   get_all_CSPRx=> null()

   nb_CSxxx = get_nb_CSxxx()

   if (nb_CSxxx <= 0) return 

   allocate( get_all_CSPRx(12,nb_vCSPRx) )

   icdan=0
   do icdtac=1,nb_CSxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj=1,verlt(icdtac)%adjsz
       icdan = icdan + 1
       if (icdan > nb_vCSPRx)  then
         call FATERR(IAM,'number of contact incompatible')
       endif

       rt = dsqrt(verlt(icdtac)%rlt(iadj)**2 + verlt(icdtac)%rls(iadj)**2)
       if ( rt /= 0.d0 ) then
         t = ((verlt(icdtac)%rlt(iadj) * verlt(icdtac)%tuc(:,iadj)) + &
              (verlt(icdtac)%rls(iadj) * verlt(icdtac)%suc(:,iadj)))/rt 
       else
         t = 0.5 * (verlt(icdtac)%tuc(:,iadj) + verlt(icdtac)%suc(:,iadj))
       endif        

       get_all_CSPRx(1:3,icdan)   = verlt(icdtac)%coor(:,iadj)
       get_all_CSPRx(4,icdan)     = verlt(icdtac)%rln(iadj)
       get_all_CSPRx(5:7,icdan)   = verlt(icdtac)%nuc(:,iadj)
       get_all_CSPRx(8,icdan)     = rt
       get_all_CSPRx(9:11,icdan)  = t
       get_all_CSPRx(12,icdan)   = verlt(icdtac)%gapTT(iadj)
     enddo
   enddo

 end function

!!!------------------------------------------------------------------------ 

 subroutine smoothing_CSPRx()
   implicit none
   ! ***
   ! t n s
   real(kind=8),dimension(3) :: r
   real(kind=8)              :: rt,rn,rs

                            !12345678901234
   character(len=14) :: IAM='CSPRx::interpolation'

   integer :: nb_CSxxx,icdtac,iadj,icdan


   nb_CSxxx = get_nb_CSxxx()
   if (nb_CSxxx <= 0) return 


   ! n'a d'interet que si on fait du contact sur element/element 
   ! mais devrait marcher pour noeud/element

   !gather
   ! on a le reac aux noeuds interpolation
   ! on passe du noeud au point de contact

   !scatter 
   ! on recupere la valeur au point de contact
   ! on met ca dans le verlet ...

   icdan=0
   do icdtac=1,nb_CSxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj=1,verlt(icdtac)%adjsz
       icdan = icdan + 1
       if (icdan > nb_vCSPRx)  then
         call FATERR(IAM,'number of contact incompatible')
       endif

       call interpolate_reac_CSpxx(icdtac,r,iIreac)        

       rt = dot_product(r,verlt(icdtac)%tuc(:,iadj))
       rn = dot_product(r,verlt(icdtac)%nuc(:,iadj))
       rs = dot_product(r,verlt(icdtac)%suc(:,iadj))

       !print*,'contact: ',icdan
       !print*,verlt(icdtac)%rlt(iadj),verlt(icdtac)%rln(iadj),verlt(icdtac)%rls(iadj)
       !print*,rt,rn,rs
       !print*,'--'

       verlt(icdtac)%rlt(iadj)=rt/H
       verlt(icdtac)%rln(iadj)=rn/H
       verlt(icdtac)%rls(iadj)=rs/H 

     enddo
   enddo

 end subroutine

!!!------------------------------------------------------------------------ 

 function get_info_CSPRx(icdan)
   !4 icdbdy,ianbdy,icdtac,iantac
   implicit none
   integer                      :: icdan
   integer,dimension(:),pointer ::  get_info_CSPRx

   allocate(get_info_CSPRx(4))

   if (.not. allocated(this) .or. icdan > nb_CSPRx ) then
     get_info_CSPRx = (/ 0.d0,0.d0,0.d0,0.d0 /)
     return
   endif

   get_info_CSPRx = (/ this(icdan)%icdbdy , &
                       this(icdan)%ianbdy , &
                       this(icdan)%icdtac , & 
                       this(icdan)%iantac /)

 end function

!!!------------------------------------------------------------------------ 

 REAL(kind=8) FUNCTION get_surf_CSPRx(icdan)

   IMPLICIT NONE
   INTEGER :: icdan 
    
   get_surf_CSPRx=get_surf_CSxxx(this(icdan)%icdtac)
   
 END FUNCTION get_surf_CSPRx

!!!------------------------------------------------------------------------
 
 subroutine clean_memory_CSPRx
   implicit none
   integer(kind=4) :: i, j, k

   call clean_memory_inter_meca_()

   nb_CSxxx       = 0
   nb_POLYR       = 0
   nb_CSPRx       = 0
   nb_vCSPRx      = 0
   nb_recup_CSPRx = 0

   ! if( allocated(this) ) then
   !   deallocate(this)
   ! end if

   ! if( allocated(adjac) ) then
   !   do i = 1, size(adjac)
   !     if( associated(adjac(i)%icdan) ) deallocate(adjac(i)%icdan)
   !   end do
   !   deallocate(adjac)
   ! end if

   ! if( allocated(nb_adj) ) deallocate(nb_adj)

   ! if( allocated(verlt) ) then
   !   do i = 1, size(verlt)
   !     if( associated(verlt(i)%icdan) ) deallocate(verlt(i)%icdan)
   !     if( associated(verlt(i)%cdbdy) ) deallocate(verlt(i)%cdbdy)
   !     if( associated(verlt(i)%anbdy) ) deallocate(verlt(i)%anbdy)
   !     if( associated(verlt(i)%cdtac) ) deallocate(verlt(i)%cdtac)
   !     if( associated(verlt(i)%antac) ) deallocate(verlt(i)%antac)

   !     if( associated(verlt(i)%rls) ) deallocate(verlt(i)%rls)
   !     if( associated(verlt(i)%rlt) ) deallocate(verlt(i)%rlt)
   !     if( associated(verlt(i)%rln) ) deallocate(verlt(i)%rln)
   !     if( associated(verlt(i)%vls) ) deallocate(verlt(i)%vls)
   !     if( associated(verlt(i)%vlt) ) deallocate(verlt(i)%vlt)
   !     if( associated(verlt(i)%vln) ) deallocate(verlt(i)%vln)

   !     if( associated(verlt(i)%gapTT) ) deallocate(verlt(i)%gapTT)

   !     if( associated(verlt(i)%coor) ) deallocate(verlt(i)%coor)
   !     if( associated(verlt(i)%suc)  ) deallocate(verlt(i)%suc)
   !     if( associated(verlt(i)%tuc)  ) deallocate(verlt(i)%tuc)
   !     if( associated(verlt(i)%nuc)  ) deallocate(verlt(i)%nuc)
   !     if( associated(verlt(i)%internal) ) deallocate(verlt(i)%internal)

   !     if( associated(verlt(i)%status) ) deallocate(verlt(i)%status)
   !   end do
   !   deallocate(verlt)
   ! end if

   nb_rough_CSPRx = 0
   if( allocated(rough_CSPRx) ) deallocate(rough_CSPRx)

   ! Root, Current and Previous should always be null outside creation_tab_visu

   if( allocated(CScoor) ) deallocate(CScoor)
   if( allocated(PRcoor) ) deallocate(PRcoor)

   ! if( allocated(violation) ) deallocate(violation)

   Reac_CSPRx_MAX = 0.D0

    module_checked_ = .FALSE.
    check_CSPRx_    = .FALSE.

 end subroutine

 subroutine set_nb_CSPRx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CSPRx = nb

 end subroutine

 subroutine redo_nb_adj_CSPRx()
   implicit none

   call redo_nb_adj_( get_nb_CSxxx() )

 end subroutine

 subroutine add_reac_CSPRx() 
   implicit none

                            !123456789012345
   character(len=15) :: IAM='CSPRx::add_reac'
   integer :: icdan

   do icdan=1,nb_CSPRx
      
      call injj_CSPRx(icdan,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln,iIreac)

   enddo   

 end subroutine add_reac_CSPRx

 subroutine assume_old_files_CSPRx() 
   implicit none

                            !12345678901234567890123
   character(len=23) :: IAM='CSPRx::assume_old_files'

   old_way=.TRUE.

 end subroutine assume_old_files_CSPRx

 
END MODULE CSPRx
