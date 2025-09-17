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
MODULE PRPLx                                          

  !!****h* LMGC90.CORE/PRPLx
  !! NAME
  !!  module PRPLx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors POLYR and PLANx.
  !!  In this modulus candidate contactors are POLYR and antagonist 
  !!  contactors are PLANx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/POLYR
  !!  LMGC90.CORE/PLANx
  !!****

  USE overall
  USE tact_behaviour
  USE POLYR
  USE PLANx
  
  use MAILx, only : get_color_MAILx
  use RBDY3, only : get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color

  use parameters, only : i_prplx, i_mailx, i_rbdy3, i_mbs3

  use inter_meca_3D

  implicit none
  
  private
  
  CHARACTER(len=5) :: BOBO='PRPLx'
  INTEGER          :: nb_POLYR
  INTEGER          :: nb_PLANx
  
  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  !------------------------------------------------------------------------ 
  ! nb_PRPLx = number of selected candidates POLYR against PLANx
  
  INTEGER,PRIVATE :: nb_PRPLx=0 , nb_vPRPLx=0 ,nb_recup_PRPLx =0
  
!!!------------------------------------------------------------------------ 
  ! serial number in this for adjacent contactor iadj
  ! to candidate contactor icdtac.
  ! For the definition of adjacent see below in 
  ! type T_verlt.
  ! When performing stock_rloc, verlt type is filled in
  ! according to adjac order, i.e.
  ! adjac(icdtac)%icdan(iadj):
  ! verlt(icdtac)%icdan(iadj)=adjac(icdtac)%icdan(iadj):
  ! nb_adj(icdtac): number of adjacent pairs SPHER-SPHER
  ! to candidate contactor SPHER icdtac.
  
  type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   
  integer             , dimension( : ), allocatable, target :: nb_adj
  
!!!------------------------------------------------------------------------   
  ! Let be some candidate contactor SPHER icdtac supported by some candidate 
  ! body icdbdy and some antagonist contactor SPHER iantac supported by some
  ! antagonist body ianbdy. The contactors may be close enough, within some 
  ! alert distance so that the the antagonist contactor is said 'adjacent' to
  ! the candidate contactor (the antagonist body is said as well adjacent to 
  ! the candidate body).
  !  
  ! A list of candidate antagonist pairs contactor-contactor
  ! adjacent to a given contactor is useful for quick access to
  ! data. Such a list is a generalisation of Verlet lists.
  ! verlt(icdtac)%adjsz: size of below arrays
  ! verlt(icdtac)%icdan(iadj): serial number in this for adjacent contactor iadj to candidate contactor icdtac.
  ! verlt(icdtac)%cdbdy(iadj): serial number of candidate body for adjacent contactor iadj. 
  ! verlt(icdtac)%cdtac(iadj): serial number of candidate contactor for adjacent contactor iadj. By definition verlt(icdtac)%cdtac(iadj)=icdtac;
  ! verlt(icdtac)%anbdy(iadj): serial number of antagonist body for adjacent contactor iadj. 
  ! verlt(icdtac)%antac(iadj): serial number of antagonist contactor for adjacent contactor iadj.
  ! verlt(icdtac)%rls(iadj): first tangential components of reaction;
  ! verlt(icdtac)%rlt(iadj): second tangential components of reaction;
  ! verlt(icdtac)%rln(iadj): normal component of reaction;
  ! verlt(icdtac)%vls(iadj): first tangential components of local velocy;
  ! verlt(icdtac)%vlt(iadj): second tangential components of local velocy;
  ! verlt(icdtac)%vln(iadj): normal component of local velocy;
  ! verlt(icdtac)%status(iadj): status of contact labelled iadj;   
  ! components of antagonist contact point;
  ! verlt(icdtac)%tuc(iadj): first tangential vector;

  type(T_verlet), dimension(:), allocatable, target ::verlt
  
!!!------------------------------------------------------------------------
  ! effective mass and radius for md method 
  ! définit le type de la liste des plus proches voisins
  ! le candidat, l'antagoniste et isee pour la loi de contact
  TYPE T_rough_PRPLx 
     
     INTEGER                   :: cd,an,isee
     INTEGER                   :: xperiodic,yperiodic
     REAL(kind=8)              :: meff,reff
     REAL(kind=8),DIMENSION(3) :: N,point
     
  END TYPE T_rough_PRPLx

  TYPE(T_rough_PRPLx),DIMENSION(:),ALLOCATABLE   :: rough_PRPLx
  INTEGER                                        :: nb_rough_PRPLx

  !s'il y a contact ou pas par détect  
  ! liste chainée pour determiner les listes de cand_ant car
  ! on ne connait pas a priori le nb de cand-ant 
  ! pointeur sur le precedent
  ! les valeurs
  ! pointeur sur le suivant
  TYPE T_link_rough_PRPLx         
     TYPE(T_link_rough_PRPLx), POINTER :: p
     TYPE(T_rough_PRPLx)               :: val
     TYPE(T_link_rough_PRPLx), POINTER :: n 
  END TYPE T_link_rough_PRPLx

  TYPE(T_link_rough_PRPLx),POINTER :: Root,Current,Previous

  TYPE T_tmp_plan
     REAL(kind=8),DIMENSION(3) :: N
     REAL(kind=8),DIMENSION(3) :: T
     REAL(kind=8),DIMENSION(3) :: S  
  END TYPE T_tmp_plan

  !------------------------------------------------------------------------
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PLcoor,PRcoor
  TYPE(T_tmp_plan),DIMENSION(:),ALLOCATABLE,PRIVATE :: tmp_plan
  REAL(kind=8) :: Reac_PRPLx_MAX=0.D0
  REAL(kind=8),DIMENSION(:),ALLOCATABLE,PRIVATE, target :: violation
  
  INTEGER,PRIVATE   :: ii,l_ii,iv
  INTEGER,PRIVATE   :: Nstep_creation_tab_visu=1,restart=0
  LOGICAL,PRIVATE   :: write_creation_tab_visu

  logical      :: module_checked_ = .FALSE.
  logical      :: check_PRPLx_    = .FALSE.

  !------------------------------------------------------------------------

  ! liste des fonctions publiques 
  !
  PUBLIC &
       coor_prediction_PRPLx,&
       CHECK_PRPLx,&
       RUN_PRPLx, &
       get_write_Vloc_Rloc_PRPLx, &
       read_ini_Vloc_Rloc_PRPLx,&
       write_xxx_Vloc_Rloc_PRPLx,&
       stock_rloc_PRPLx, &
       recup_rloc_PRPLx, &
       creation_tab_visu_PRPLx, &
       compute_contact_PRPLx, &
       display_prox_tactors_PRPLx,&
       get_nb_PRPLx

 PUBLIC &
      nullify_reac_PRPLx, nullify_vlocy_PRPLx,injj_PRPLx, prjj_PRPLx, vitrad_PRPLx, & 
      PRPLx2ENT, PRPLx2POLYR, PRPLx2PLANx, &
      get_type_PRPLx, &
      get_surf_PRPLx

  public clean_memory_PRPLx

  !rm for handler
  public get_this    , &
         set_nb_PRPLx, &
         redo_nb_adj_PRPLx, &
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
 SUBROUTINE coor_prediction_PRPLx

  IMPLICIT NONE  

  INTEGER                                  :: errare 
  INTEGER                                  :: itac,itacty   
  REAL(kind=8),DIMENSION(3,3)              :: localframe

  IF (.NOT. ALLOCATED(PRcoor))   ALLOCATE(PRcoor(3,nb_POLYR))
  IF (.NOT. ALLOCATED(PLcoor))   ALLOCATE(PLcoor(3,nb_PLANx))
  IF (.NOT. ALLOCATED(tmp_plan)) ALLOCATE(tmp_plan(nb_PLANx))
   
  call move_polyr

  DO itac=1,nb_POLYR
     PRcoor(1:3,itac) = get_coorTT_POLYR(itac)
  END DO

  DO itac=1,nb_PLANx
    PLcoor(1:3,itac) = get_coorTT_PLANx(itac)
    localframe = matmul(get_inertia_frameTT_PLANx(itac), &
                        get_embeded_frame_PLANx(itac))

!fd @@@ modifs pour etre en t,n,s

    tmp_plan(itac)%T(1:3)= localframe(1:3,1)
    tmp_plan(itac)%N(1:3)= localframe(1:3,3)
    tmp_plan(itac)%S(1:3)=-localframe(1:3,2) 

  END DO

END SUBROUTINE coor_prediction_PRPLx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PRPLx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PRPLx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PRPLx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PRPLx
!!!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_PRPLx

  IMPLICIT NONE

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,itac
  INTEGER                               :: icdtac,iantac,isee,itacty,i
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol

  REAL(kind=8),DIMENSION(3)             :: sep,axe,point,vect                  ! vecteurs reliant les centres des polygones
  REAL(kind=8)                          :: norm,dist,adist,norm1,dist1,norm2   ! real pour calculer la norme au carrée de sep

  LOGICAL :: visible, visible_planx
  character(len=80) :: cout

                             !1234567890123456789012345678
  character(len=28) :: IAM = 'mod_PRPLx::creation_tab_visu'

  ! Detecting contacts; 
  ! contacts are being detected within a box and immediate surrounding boxes;  
     
  ! first reading: sizing array adjac

  nb_POLYR = get_nb_POLYR() 

  !print*,'1 NB POLYR=',nb_polyr

  IF (.NOT. ALLOCATED(adjac)) THEN
    ALLOCATE(adjac(nb_POLYR),stat=errare)
    IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating adjac')
    END IF
    DO itac=1,nb_POLYR
      NULLIFY(adjac(itac)%icdan)
    ENDDO 
  ENDIF

  DO itac=1,nb_POLYR
    IF (ASSOCIATED(adjac(itac)%icdan)) DEALLOCATE(adjac(itac)%icdan) 
    NULLIFY(adjac(itac)%icdan)
  ENDDO 

  IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
  ALLOCATE(nb_adj(nb_POLYR),stat=errare)
  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating nb_adj')
  END IF    

  nb_adj=0
  nb_rough_PRPLx=0

! création de la liste de paire de polygones à examiner

! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)

  DO icdtac=1,nb_POLYR
     visible=get_visible_POLYR(icdtac)
     IF (.NOT.visible) CYCLE
    
    cdcol=get_color_POLYR(icdtac)


    DO iantac=1,nb_PLANx

      visible_planx=get_visible_PLANx(iantac)
      IF (.NOT.visible_planx) CYCLE

      ancol= get_color_PLANx(iantac)

      isee = get_isee(get_body_model_name_from_id(polyr2bdyty(3,icdtac)),'POLYR',cdcol, &
                      get_body_model_name_from_id(planx2bdyty(3,iantac)),'PLANx',ancol)

      IF (isee /= 0) THEN

        adist=see(isee)%alert 
        vect= PLcoor(1:3,iantac)-PRcoor(1:3,icdtac)
        axe = get_axes_PLANx(iantac)
        dist=S_POLYR(icdtac)%radius+axe(3)+adist
        norm2=vect(1)*tmp_plan(iantac)%N(1)+vect(2)*tmp_plan(iantac)%N(2)+vect(3)*tmp_plan(iantac)%N(3)

        ! Consider projection on the normal of PLAN 
        ! (alert distance is available only in this direction)

        IF (dabs(norm2)<dist) THEN
          dist = S_POLYR(icdtac)%radius + axe(1)
          norm = vect(1)*tmp_plan(iantac)%T(1)+vect(2)*tmp_plan(iantac)%T(2)+vect(3)*tmp_plan(iantac)%T(3)
          dist1= S_POLYR(icdtac)%radius + axe(2)
          norm1 = vect(1)*tmp_plan(iantac)%S(1)+vect(2)*tmp_plan(iantac)%S(2)+vect(3)*tmp_plan(iantac)%S(3)
          IF ((dabs(norm)< dist).AND.(ABS(norm1) <dist1)) THEN
            nb_rough_PRPLx=nb_rough_PRPLx+1
            IF ( nb_rough_PRPLx == 1) THEN
              ALLOCATE(Root)
                Current => Root
                NULLIFY(Root%p)
              ELSE
                ALLOCATE(Current)
                Previous%n => Current
            ENDIF
            Current%val%cd       = icdtac
            Current%val%an       = iantac
            Current%val%isee     = isee
            IF (norm2>=0.D0) THEN
              Current%val%N     = -tmp_plan(iantac)%N
              Current%val%point =  PRcoor(1:3,icdtac)+(norm2-axe(3))*tmp_plan(iantac)%N
             ELSE
              Current%val%N     =  tmp_plan(iantac)%N
              Current%val%point =  PRcoor(1:3,icdtac)+(norm2+axe(3))*tmp_plan(iantac)%N
            ENDIF
            Current%p => Previous
            NULLIFY(Current%n)
            Previous => Current
          ENDIF
        ENDIF
      ENDIF
    ENDDO
  ENDDO     

  WRITE(cout,'(4X,I10,A20)') nb_rough_PRPLx,' PRPLx roughly found'
  call logmes(cout)

  IF (ALLOCATED(rough_PRPLx)) DEALLOCATE(rough_PRPLx)
  ALLOCATE(rough_PRPLx(nb_rough_PRPLx))              ! on s'alloue la table de visibilité utilisée dans compute_contact

  IF (ALLOCATED(this)) DEALLOCATE(this)
  ALLOCATE(this(4*nb_rough_PRPLx))     ! on s'alloue un tableau temporaire de contact.On lui donne une taille 3*nb_paire_phphx
                                       ! car il y a au maximun trois points de contact entre un candidat - antagoniste
  DO i=nb_rough_PRPLx,1,-1

    Previous => Current%p
    rough_PRPLx(i)%cd     = Current%val%cd
    rough_PRPLx(i)%an     = Current%val%an
    rough_PRPLx(i)%isee   = Current%val%isee
    rough_PRPLx(i)%point  = Current%val%point
    rough_PRPLx(i)%N      = Current%val%N
    DEALLOCATE(Current)
    Current => Previous
  END DO 

  NULLIFY(Root)


END SUBROUTINE creation_tab_visu_PRPLx
!------------------------------------------------------------------------
SUBROUTINE compute_contact_PRPLx
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,itac,icdtac,iantac,isee,itacty,imax    
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  REAL(kind=8)                          :: raycd,rayan,adist,dist,min_dist,nonuc,gap
  INTEGER                               :: i,id,j,nb_ctc,nb_ctc2
  REAL(kind=8),DIMENSION(3,4)           :: xco                                 ! points de contact
  REAL(kind=8),DIMENSION(4)             :: ovlap                               ! les gaps en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: t,n,s                               ! la normale en sortie detect()
  REAL(kind=8),DIMENSION(3)             :: cdlev,anlev                         ! les vecteurs centre -> point de contact
  REAL(kind=8),DIMENSION(3)             :: sep,axe,vect                             ! vecteur reliant les centres des polyèdres
  REAL(kind=8)                          :: norm,den                            ! scalaire contenant la norme de sep
  REAL(kind=8),DIMENSION(6)             :: cd_Vbegin,an_Vbegin
  REAL(kind=8),DIMENSION(3)             :: axepl,point
  INTEGER,DIMENSION(4)                  :: vertex_candidat
  REAL(kind=8),DIMENSION(3,3)           :: localframe_cd,localframe_an, Rc
  REAL(kind=8)                          :: vln_cst,vls_cst,vlt_cst
  REAL(kind=8)                          :: tmp

  integer :: cd_ent,an_ent
  character(len=80) :: cout

  icdan=0        
  nb_PRPLx=0
  nb_adj=0


  !print*,'2 NB POLYR=',nb_polyr

  IF (nb_rough_PRPLx /= 0 ) THEN
     !
     ! Fine Detection
     !
     DO i=1,nb_rough_PRPLx

       icdtac=rough_PRPLx(i)%cd  ! Polyedra
       iantac=rough_PRPLx(i)%an  ! Plan
       isee=rough_PRPLx(i)%isee
       point=rough_PRPLx(i)%point
       n=rough_PRPLx(i)%N
       norm=n(1)*n(1)+n(2)*n(2)+n(3)*n(3)
       n=n/norm
       nb_ctc=0
       adist=see(isee)%alert 
       min_dist=1.D20

       axe = get_axes_PLANx(iantac)

       DO j=1,S_POLYR(icdtac)%nb_vertex
         vect= S_POLYR(icdtac)%vertex(1:3,j) - PLcoor(1:3,iantac)

         tmp = vect(1)*tmp_plan(iantac)%T(1)+vect(2)*tmp_plan(iantac)%T(2)+vect(3)*tmp_plan(iantac)%T(3)
         if (dabs(tmp) > axe(1)+adist) cycle

         tmp = vect(1)*tmp_plan(iantac)%S(1)+vect(2)*tmp_plan(iantac)%S(2)+vect(3)*tmp_plan(iantac)%S(3)
         if (dabs(tmp) > axe(2)+adist) cycle

         dist = n(1)*vect(1) + n(2)*vect(2) + n(3)*vect(3) - axe(3)

!         dist=n(1)*(S_POLYR(icdtac)%vertex(1,j)-point(1))+&
!              n(2)*(S_POLYR(icdtac)%vertex(2,j)-point(2))+&
!              n(3)*(S_POLYR(icdtac)%vertex(3,j)-point(3))


         IF (dist < adist) THEN
           IF (nb_ctc < 4) THEN
              min_dist=min(dist,min_dist)              
              nb_ctc=nb_ctc+1
              xco(1:3,nb_ctc)=S_POLYR(icdtac)%vertex(1:3,j)
              ovlap(nb_ctc)=dist
              vertex_candidat(nb_ctc)=j
           ELSE
              imax = maxloc(ovlap,dim=1)
              IF (dist < ovlap(imax)) THEN
                min_dist=min(dist,min_dist)              
                xco(1:3,imax)=S_POLYR(icdtac)%vertex(1:3,j)
                ovlap(imax)=dist
                vertex_candidat(imax)=j
              ENDIF
           ENDIF     
         ENDIF
       ENDDO

       ! Constante quantities for every contact

       localframe_cd = get_inertia_frameTT_POLYR(icdtac)
       localframe_an = get_inertia_frameTT_PLANx(iantac)

       cd_Vbegin = get_vlocy_POLYR(icdtac,iVbeg_)
       an_Vbegin = get_vlocy_PLANx(iantac,iVbeg_)

!fd @@@ c'est quand meme un truc de sagouin !!!
!fd @@@ avant tu peux tourner le repere et apres tu oublies 

       IF ((tmp_plan(iantac)%n(1)*n(1)+tmp_plan(iantac)%n(2)*n(2)+tmp_plan(iantac)%n(3)*n(3)) > 0) THEN
         t=tmp_plan(iantac)%T
         s=tmp_plan(iantac)%S
       ELSE
         t=-tmp_plan(iantac)%T
         s=-tmp_plan(iantac)%S
       ENDIF

       vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ (cd_Vbegin(2)-an_Vbegin(2))*s(2)+(cd_Vbegin(3)-an_Vbegin(3))*s(3)
       vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ (cd_Vbegin(2)-an_Vbegin(2))*t(2)+(cd_Vbegin(3)-an_Vbegin(3))*t(3)
       vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ (cd_Vbegin(2)-an_Vbegin(2))*n(2)+(cd_Vbegin(3)-an_Vbegin(3))*n(3)

       do j = 1, nb_ctc

         icdan          = icdan + 1
         nb_adj(icdtac) = nb_adj(icdtac) + 1
         iadj           = nb_adj(icdtac)

         this(icdan)%icdbtac = polyr2bdyty(2, icdtac)
         this(icdan)%ianbtac = planx2bdyty(2, iantac)

         this(icdan)%icdbtyp = polyr2bdyty(3, icdtac)
         this(icdan)%ianbtyp = planx2bdyty(3, iantac)

         this(icdan)%icdctyp = i_polyr
         this(icdan)%ianctyp = i_planx

         this(icdan)%iadj    = iadj
         this(icdan)%icdbdy  = polyr2bdyty(1, icdtac)
         this(icdan)%icdtac  = icdtac
         this(icdan)%icdsci  = vertex_candidat(j)
         this(icdan)%ianbdy  = planx2bdyty(1, iantac)
         this(icdan)%iantac  = iantac
         this(icdan)%iansci  = 0
         this(icdan)%isee    = isee
         this(icdan)%nuc     = n
         this(icdan)%tuc     = t
         this(icdan)%suc     = s

         cd_ent = get_ent_POLYR(this(icdan)%icdtac)
         an_ent = get_ent_PLANx(this(icdan)%iantac) 

         this(icdan)%icdent = cd_ent
         this(icdan)%ianent = an_ent

         entity(cd_ent)%nb = entity(cd_ent)%nb + 1
         entity(an_ent)%nb = entity(an_ent)%nb + 1

         this(icdan)%type_ctc  = nb_ctc
         this(icdan)%coor(1:3) = xco(1:3, j)
         cdlev                 = xco(1:3, j) &
                                -( PRcoor(1:3,icdtac)-get_shiftTT_POLYR(icdtac) )
         anlev                 = xco(1:3, j) &
                                -  PLcoor(1:3,iantac)

         ! Compute the mapping: inertia frame to general frame for antagonist
 
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



         this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
         this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
         this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

         this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
         this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
         this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

         this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
         this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
         this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

         ! Computation of relatives velocities

         this(icdan)%gapTTbegin      = ovlap(j)


         this(icdan)%vlsBEGIN= vls_cst &     
                + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)

         this(icdan)%vltBEGIN = vlt_cst &
                + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

         this(icdan)%vlnBEGIN = vln_cst &     
                + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

         this(icdan)%rls=0.D0
         this(icdan)%rlt=0.D0
         this(icdan)%rln=0.D0
         this(icdan)%vls=this(icdan)%vlsBEGIN
         this(icdan)%vlt=this(icdan)%vltBEGIN
         this(icdan)%vln=this(icdan)%vlnBEGIN
         this(icdan)%gapTT=this(icdan)%gapTTbegin
         this(icdan)%status=i_nknow
       ENDDO
     ENDDO
     nb_PRPLx=icdan
   ENDIF

   WRITE(cout,'(1X,I10,A12)') nb_PRPLx,' PRPLx found'
   call logmes(cout)

   DO itac=1,nb_POLYR
     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
     IF (nb_adj(itac) /= 0) THEN
       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating adjac(',icdtac,')%.....'
         call faterr('mod_PRPLx::compute_contact',cout)
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_PRPLx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 


   do icdan = 1, nb_PRPLx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PRPLx),stat=errare)

END SUBROUTINE compute_contact_PRPLx

!------------------------------------------------------------------------
 subroutine display_prox_tactors_PRPLx

   implicit none

   integer :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdver,nb_POLYR
   character(len=5) :: cdmodel, anmodel

   
   nb_POLYR=get_nb_POLYR()
   IF (nb_PRPLx==0) RETURN
   DO icdtac=1,nb_POLYR

     DO iadj=1,nb_adj(icdtac)         
       icdan  = adjac(icdtac)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
       !icdtac = this(icdan)%icdtac
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac
       icdver = this(icdan)%icdsci
       
       cdmodel = get_body_model_name_from_id( polyr2bdyty(3,this(icdan)%icdtac) )
       anmodel = get_body_model_name_from_id( planx2bdyty(3,this(icdan)%iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,icdbdy,'POLYR',icdtac,icdver,see(this(icdan)%isee)%behav,  &
       anmodel,ianbdy,'PLANx',iantac
                    
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
   
 end subroutine display_prox_tactors_PRPLx
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_PRPLx
   !
   ! get data from this and put into verlt
   !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdver,nb_POLYR
   INTEGER :: errare

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PRPLx::stock_rloc'

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
   DO icdan=1,nb_PRPLx
     ! serial number of candidate body for contact icdan
     icdbdy = this(icdan)%icdbdy
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist body for contact icdan
     ianbdy = this(icdan)%ianbdy
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor
     ! adjacent to candidate contactor for contact icdan
     iadj   = this(icdan)%iadj
     verlt(icdtac)%icdan(iadj)     = icdan

     verlt(icdtac)%cdbdy           = polyr2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = polyr2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel         = polyr2bdyty(3,icdtac)
     verlt(icdtac)%anbdy(iadj)     = planx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = planx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)   = planx2bdyty(3,iantac)

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

   nb_vPRPLx = nb_PRPLx

   WRITE(cout,'(1X,I10,A12)') nb_vPRPLx,' stock PRPLx'
   call logmes(cout)

 END SUBROUTINE stock_rloc_PRPLx
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_PRPLx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdver
   character(len=80) :: cout
   
   if (.not. allocated(verlt)) then
      call logmes('[mod_PRPLx::recup_rloc] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_PRPLx=0 
   DO icdan=1,nb_PRPLx
     this(icdan)%rls=0.D0
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac
     ! serial number of candidate vertex contactor for contact icdan
     icdver = this(icdan)%icdsci

     IF (verlt(icdtac)%adjsz /= 0) THEN

       if ( verlt(icdtac)%cdbdy  == polyr2bdyty(1,icdtac) .and. &
            verlt(icdtac)%cdtac  == polyr2bdyty(2,icdtac) .and. &
            verlt(icdtac)%cdmodel== polyr2bdyty(3,icdtac) ) then

         do iadj = 1, verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == planx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == planx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== planx2bdyty(3,iantac) .and. &
                verlt(icdtac)%cdsci(iadj)  == icdver                ) then

             this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
             this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
             this(icdan)%rln = verlt(icdtac)%rln(iadj)*H
             this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
             this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
             nb_recup_PRPLx = nb_recup_PRPLx + 1
             EXIT
           END IF
         END DO
       ENDIF
     endif      
   END DO

  
   WRITE(cout,'(1X,I10,A12)') nb_recup_PRPLx,' recup PRPLx'
   call logmes(cout)

 END SUBROUTINE recup_rloc_PRPLx
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   IMPLICIT NONE

   CHARACTER(len=103)               :: clin
   INTEGER                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdver
   REAL(kind=8)                     :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
   CHARACTER(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER                          :: errare,i_internal,nb_internal,ibehav
   CHARACTER(len=29)  :: IAM = 'mod_PRPLx::read_ini_Vloc_Rloc'
   CHARACTER(len=103) :: cout

   integer:: cdmodel,anmodel,icdtact,nb_read
   
   errare=0
   nb_read=0
   nb_POLYR=get_nb_POLYR()


  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_POLYR),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating nb_adj')
   END IF

   DO icdtac=1,nb_POLYR
     nb_adj(icdtac)=0
   END DO

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRPLx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     
     IF (cdtac == 'POLYR' .AND. antac == 'PLANx') THEN
       do icdtact = 1, nb_POLYR
         if (polyr2bdyty(1,icdtact) == icdbdy .and. &
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
  ! second reading: filling data
   REWIND(G_nfich)
   DO icdtac=1,nb_POLYR
     nb_adj(icdtac)=0
   END DO

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PRPLx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,icdver,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus

     IF (cdtac == 'POLYR' .AND. antac == 'PLANx') THEN

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       do icdtact = 1, nb_POLYR
         if (polyr2bdyty(1,icdtact) == icdbdy .and. &
             polyr2bdyty(2,icdtact) == icdtac .and. &
             polyr2bdyty(3,icdtact) == cdmodel ) then

           nb_read=nb_read+1
        
           nb_adj(icdtact)=nb_adj(icdtact)+1

           verlt(icdtact)%icdan( nb_adj(icdtact) ) = nb_read

           verlt(icdtact)%cdmodel = cdmodel
           verlt(icdtact)%cdbdy   = icdbdy
           verlt(icdtact)%cdtac   = icdtac

           verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
           verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
           verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
           verlt(icdtact)%cdsci(nb_adj(icdtact))  = icdver
           verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
           
           IF( .NOT. read_G_clin()) CYCLE
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') rls,rlt,rln
           verlt(icdtact)%rls(nb_adj(icdtact)) = rls
           verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
           verlt(icdtact)%rln(nb_adj(icdtact)) = rln

           IF( .NOT. read_G_clin()) CYCLE
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') vls,vlt,vln
           verlt(icdtact)%vls(nb_adj(icdtact)) = vls
           verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
           verlt(icdtact)%vln(nb_adj(icdtact)) = vln

           IF( .NOT. read_G_clin()) CYCLE 
           READ(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
           verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 'coo1=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%coor(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%coor(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%coor(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 't(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%tuc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%tuc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%tuc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           ENDIF
         
           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 'n(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%nuc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%nuc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%nuc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           ENDIF
        
           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 's(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           ENDIF

           verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact)) = 0.d0
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
     ENDIF 
   ENDDO

   nb_vPRPLx=0
    
   DO icdtac=1,nb_POLYR
      nb_vPRPLx = nb_vPRPLx + nb_adj(icdtac)
      
      IF ( nb_adj(icdtac) /= verlt(icdtac)%adjsz ) THEN 
         WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtac, &
              'value of nb_adj is',nb_adj(icdtac),' and value of verlet%adjsz is ',verlt(icdtac)%adjsz
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
   character(len=5) :: cdmodel,anmodel

   character(len=20) :: fmt
   
   nb_POLYR=get_nb_POLYR()

   IF (nb_PRPLx==0) RETURN

   DO icdtact=1,nb_POLYR    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         icdver = this(icdan)%icdsci

         cdmodel = get_body_model_name_from_id( polyr2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( planx2bdyty(3,iantac) )
         
         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PRPLx',icdan  
                             !1234567890123456789012345678901234567890123456789012345678901234567890123456'
         WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              !pta old fashion 'RBDY3',polyr2bdyty(1,icdtac),'POLYR',polyr2bdyty(2,icdtac),icdver,  &
              cdmodel,get_visibleID_POLYR(icdtac),'POLYR',polyr2bdyty(2,icdtac),icdver,  &
              see(this(icdan)%isee)%behav,  &
              !pta old fashion 'RBDY3',planx2bdyty(1,iantac),'PLANx',planx2bdyty(2,iantac),  &
              anmodel,get_visibleID_PLANx(iantac),'PLANx',planx2bdyty(2,iantac),  &
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
 SUBROUTINE nullify_reac_PRPLx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: storage
    
   CALL nullify_reac_POLYR(this(icdan)%icdtac,storage)
   
   CALL nullify_reac_PLANx(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_reac_PRPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_PRPLx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
    
   CALL nullify_vlocy_POLYR(this(icdan)%icdtac,storage)
   
   CALL nullify_vlocy_PLANx(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_vlocy_PRPLx
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_PRPLx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
    
   CALL comp_vlocy_POLYR(this(icdan)%icdtac,storage)
    
   CALL comp_vlocy_PLANx(this(icdan)%iantac,storage)
    
 END SUBROUTINE vitrad_PRPLx
!------------------------------------------------------------------------  
 SUBROUTINE injj_PRPLx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: ccdof= (/ 1,2,3,4,5,6 /)
   REAL(kind=8),DIMENSION(6)  :: cdreac, anreac

   INTEGER                    :: storage

   cdreac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
   cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
   cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK

   anreac(1) =-cdreac(1)
   anreac(2) =-cdreac(2)
   anreac(3) =-cdreac(3)
   anreac(4) =-this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
   anreac(5) =-this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
   anreac(6) =-this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK

   !print*,'===='
   !print*,'injj prpl'

   CALL add_reac_POLYR(this(icdan)%icdtac,ccdof,cdreac,storage)
   CALL add_reac_PLANx(this(icdan)%iantac,ccdof,anreac,storage)

 END SUBROUTINE injj_PRPLx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_PRPLx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   
   Vcd = get_vlocy_POLYR(this(icdan)%icdtac,storage)
   Van = get_vlocy_PLANx(this(icdan)%iantac,storage)      

   !print*,'prjj prpl'
   !print*,'==='

   VSIK = Vcd(1)*this(icdan)%suc(1) + Vcd(2)*this(icdan)%suc(2) + Vcd(3)*this(icdan)%suc(3)        &
        + Vcd(4)*this(icdan)%Gcds(1)+ Vcd(5)*this(icdan)%Gcds(2)+ Vcd(6)*this(icdan)%Gcds(3)       &
        - Van(1)*this(icdan)%suc(1) - Van(2)*this(icdan)%suc(2) - Van(3)*this(icdan)%suc(3)        &
        - Van(4)*this(icdan)%Gans(1)- Van(5)*this(icdan)%Gans(2)- Van(6)*this(icdan)%Gans(3)

   VTIK = Vcd(1)*this(icdan)%tuc(1) + Vcd(2)*this(icdan)%tuc(2) + Vcd(3)*this(icdan)%tuc(3)        &
        + Vcd(4)*this(icdan)%Gcdt(1)+ Vcd(5)*this(icdan)%Gcdt(2)+ Vcd(6)*this(icdan)%Gcdt(3)       &
        - Van(1)*this(icdan)%tuc(1) - Van(2)*this(icdan)%tuc(2) - Van(3)*this(icdan)%tuc(3)        &
        - Van(4)*this(icdan)%Gant(1)- Van(5)*this(icdan)%Gant(2)- Van(6)*this(icdan)%Gant(3)

   VNIK = Vcd(1)*this(icdan)%nuc(1) + Vcd(2)*this(icdan)%nuc(2) + Vcd(3)*this(icdan)%nuc(3)        &
        + Vcd(4)*this(icdan)%Gcdn(1)+ Vcd(5)*this(icdan)%Gcdn(2)+ Vcd(6)*this(icdan)%Gcdn(3)       &
        - Van(1)*this(icdan)%nuc(1) - Van(2)*this(icdan)%nuc(2) - Van(3)*this(icdan)%nuc(3)        &
        - Van(4)*this(icdan)%Gann(1)- Van(5)*this(icdan)%Gann(2)- Van(6)*this(icdan)%Gann(3)

 END SUBROUTINE prjj_PRPLx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PRPLx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_PRPLx = nb_PRPLx
    CASE(i_verlet_tactor)
       get_nb_PRPLx = nb_vPRPLx
    CASE(i_rough_tactor)
       get_nb_PRPLx = nb_rough_PRPLx
    CASE(i_recup_tactor)
       get_nb_PRPLx = nb_recup_PRPLx
    END SELECT

  END FUNCTION get_nb_PRPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE PRPLx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_POLYR(this(icdan)%icdtac)
   ianent = get_ENT_PLANx(this(icdan)%iantac)

 END SUBROUTINE PRPLx2ENT
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
SUBROUTINE PRPLx2POLYR(icdan,icdtac)

  IMPLICIT NONE
  INTEGER :: icdan,icdtac
   
  icdtac = this(icdan)%icdtac

END SUBROUTINE PRPLx2POLYR
!!!------------------------------------------------------------------------ 
SUBROUTINE PRPLx2PLANx(icdan,iantac)

  IMPLICIT NONE
  INTEGER :: icdan,iantac
   
  iantac = this(icdan)%iantac

END SUBROUTINE PRPLx2PLANx
!-----------------------------------------------------------------------
 INTEGER FUNCTION get_type_PRPLx(icdan)

   IMPLICIT NONE
   INTEGER       :: icdan
   
   get_type_PRPLx = this(icdan)%type_ctc
   
 END FUNCTION get_type_PRPLx
!------------------------------------------------------------------------
  LOGICAL FUNCTION RUN_PRPLx()

    IMPLICIT NONE
    
    RUN_PRPLx = RUN_TACTOR

  END FUNCTION RUN_PRPLx
!!!------------------------------------------------------------------------
  logical function CHECK_PRPLx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PRPLx = check_PRPLx_
      return
    end if

    con_pedigree%module_name = 'PRPLx'

    con_pedigree%id_cdan  = i_prplx
    con_pedigree%id_cdtac = i_polyr
    con_pedigree%id_antac = i_planx

    cdtact2bdyty => polyr2bdyty
    antact2bdyty => planx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_POLYR = get_nb_POLYR()
    nb_PLANx = get_nb_PLANx()
    IF( nb_POLYR == 0 .or. nb_PLANx == 0 ) then
      CHECK_PRPLx = check_PRPLx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'POLYR' .and. see(isee)%antac == 'PLANx') then
        check_PRPLx_ = .true.
        exit
      end if
    end do

    CHECK_PRPLx = check_PRPLx_
    return

  end function CHECK_PRPLx
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_PRPLx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_PRPLx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_PRPLx
!!!-----------------------------------------------------------------------

 REAL(kind=8) function get_surf_PRPLx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan

   get_surf_PRPLx = 0.d0 ! todo this(icdan)%surf

 END function get_surf_PRPLx



 subroutine clean_memory_PRPLx
   implicit none
   integer(kind=4) :: i, j, k

   call clean_memory_inter_meca_()

   ! \todo: violation

   nb_POLYR       = 0
   nb_PLANx       = 0
   nb_PRPLx       = 0
   nb_vPRPLx      = 0
   nb_recup_PRPLx = 0

   nb_rough_PRPLx = 0
   if( allocated(rough_PRPLx) ) deallocate(rough_PRPLx)

   ! Root, Current and Previous should always be null outside creation_tab_visu

   if( allocated(PRcoor) ) deallocate(PRcoor)
   if( allocated(PLcoor) ) deallocate(PLcoor)

   if( allocated(tmp_plan) ) deallocate(tmp_plan)

   Reac_PRPLx_MAX=0.D0

   module_checked_ = .FALSE.
   check_PRPLx_    = .FALSE.

 end subroutine

 subroutine set_nb_PRPLx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PRPLx = nb

 end subroutine

 subroutine redo_nb_adj_PRPLx()
   implicit none

   call redo_nb_adj_( get_nb_POLYR() )

 end subroutine

END MODULE PRPLx
