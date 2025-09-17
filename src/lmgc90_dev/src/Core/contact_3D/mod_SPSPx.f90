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
MODULE SPSPx 
                                         
  !!****h* LMGC90.CORE/SPSPx
  !! NAME
  !!  module SPSPx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations  between contactors SPHER.
  !!  In this modulus candidate and antagonist contactors are SPHER.
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/SPHER
  !!****

  use overall
  use algebra, only : cross_product

  use tact_behaviour
  use SPHER

  use MAILx, only : get_color_MAILx
  use RBDY3, only : get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color

  !am : modules utilises par les fonctions gerant la DDM
  use anonymous_ptr_container, only : get_object               => get_data                   , &
                                      get_nb_objects           => get_nb_data                , &
                                      close_container          => close_ptr_container        , &
                                      add_object_to_container  => add_object_to_ptr_container, &
                                      display_object_container => display_ptr_container      , &
                                      ptr_container
  use anonymous

  use parameters, only : i_spspx, i_spher, i_mailx, i_rbdy3, i_mbs3
                         !i_undefined, i_free, i_sheared, i_stationary

  use inter_meca_3D

  implicit none

  private

  integer(kind=4), private  :: nb_SPHER

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  INTEGER :: nb_SPSPx=0,nb_vSPSPx=0,nb_recup_SPSPx=0
                               
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
  ! For quick sorting, disks are owned by boxes, sorting being 
  ! performed within a box and immediate surrounding boxes, see
  ! subroutine enumerate_PLPLx.
  ! box(ibox1,ibox2)%popul: number of disks in box ibox1,ibox2;
  ! box(ibox1,ibox2)%which(ipopul): 
  ! rank in the list of contactors of disk labelled ipopul
  ! in box ibox1,ibox2;
  ! box(ibox1,ibox2,ibox3): box with integer coordinates ibox1,ibox2,ibox3.

  TYPE T_box
     INTEGER                      :: popul
     INTEGER,DIMENSION(:),POINTER :: which
  END TYPE T_box
  
  TYPE(T_box), DIMENSION(:,:,:),ALLOCATABLE :: box

  !------------------------------------------------------------------------
  ! variables attached to surrounding boxes
  
  REAL (kind=8)  :: maxray, minray, maxalert, meanradius, Rn_max
  REAL (kind=8)  :: Lbox,LBox_1,norm
  INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,minibox3,maxibox3,maxpopul
      
!!!------------------------------------------------------------------------
  ! effective mass and radius for md method 
  ! définit le type de la liste des plus proches voisins
  ! le candidat, l'antagoniste et isee pour la loi de contact
  TYPE T_rough_SPSPx                        
     INTEGER      :: cd,an,isee
     REAL(kind=8) :: meff,reff
     INTEGER      :: xperiodic,yperiodic
!fd A VOIR le 03/01/08     INTEGER      :: nonuc0 !c'est quoi cette merde ?

     !am: group to which belong an interaction. For ddm group could be
     !   - INTRF: for a body belonging to an interface between two sub-domains
     !   - NOINT: for a body living inside a sub-domain
     integer :: group
  END TYPE T_rough_SPSPx
  
  TYPE(T_rough_SPSPx),DIMENSION(:),ALLOCATABLE   :: rough_SPSPx 
  INTEGER                                        :: nb_rough_SPSPx

  !s'il y a contact ou pas par détect  
  ! liste chainée pour determiner les listes de cand_ant car
  ! on ne connait pas a priori le nb de cand-ant 
  ! pointeur sur le precedent
  ! les valeurs
  ! pointeur sur le suivant
  TYPE T_link_rough_SPSPx 
     TYPE(T_link_rough_SPSPx), POINTER :: p
     TYPE(T_rough_SPSPx)               :: val
     TYPE(T_link_rough_SPSPx), POINTER :: n
  END TYPE T_link_rough_SPSPx
  
  TYPE(T_link_rough_SPSPx),POINTER :: Root,Current,Previous

!!!---------------------------------------------------------
  
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: SPcoor
  REAL(kind=8)                                    :: Reac_SPSPx_MAX=0.D0
  INTEGER,PRIVATE                                 :: ii,l_ii,iv
  INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
  LOGICAL,PRIVATE                                 :: write_creation_tab_visu
  
  real(kind=8), dimension(:), allocatable, target :: violation
!!!---------------------------------------------------------
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(KIND=8) :: XPERIODE = 0.D0,YPERIODE = 0.D0
!!!---------------------------------------------------------


!fd experimental pour la generation de plusieurs interactions par points de contact  

  INTEGER      :: NbInteractionByContact = 1
  REAL(kind=8) :: ContactRadius = 0.d0

  logical      :: module_checked_ = .FALSE.
  logical      :: check_SPSPx_    = .FALSE.

  PUBLIC &
       coor_prediction_SPSPx,&
       CHECK_SPSPx,&
       RUN_SPSPx, &
       get_write_Vloc_Rloc_SPSPx, &
       read_ini_Vloc_Rloc_SPSPx,&
       write_xxx_Vloc_Rloc_SPSPx,&
       !write_out_one_Vloc_Rloc_SPSPx,&
       set_xperiodic_data_SPSPx, &
       set_yperiodic_data_SPSPx, &
       stock_rloc_SPSPx, &
       recup_rloc_SPSPx, &
       smooth_computation_SPSPx, &
       compute_box_SPSPx, &
       creation_tab_visu_SPSPx, &
       compute_contact_SPSPx, &
       display_prox_tactors_SPSPx, &
       update_cohe_SPSPx, &
       !am DDM : declaration des fonctions necessaires a la DDM
       set_interactions_to_rough_SPSPx, &
       set_anonymous_to_rough_SPSPx, &
       get_nb_INTRF_SPSPx, &
       get_list_INTRF_SPSPx, &
       put_icdan_group_SPSPx

  PUBLIC &
       nullify_reac_SPSPx, nullify_vlocy_SPSPx,injj_SPSPx, prjj_SPSPx, vitrad_SPSPx, & 
       get_nb_SPSPx , &
       SPSPx2ENT, SPSPx2SPHER, &
       get_xperiode_SPSPx,get_yperiode_SPSPx,get_surf_SPSPx

  !!public get_rough_SPSPx      , &
  !!       reset_violation_SPSPx, &
  !!       reset_nb_adj_SPSPx   , &
  !!       add_adj_SPSPx        , &
  !!       get_nb_adj_SPSPx     , &
  !!       compute_contacts_in_t2t_SPSPx

!!! EXPERIMENTAL FUNCTION

  PUBLIC &
       Set_NbInteractionByContact,Set_ContactRadius,fd_compute_contact_spspx, &
       get_heat_sources_SPSPx,put_heat_sources_SPSPx

  public clean_memory_SPSPx

  private write_out_vloc_rloc

  !rm for handler
  public get_this    , &
         set_nb_SPSPx, &
         redo_nb_adj_SPSPx, &
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
  SUBROUTINE coor_prediction_SPSPx

    IMPLICIT NONE  
    INTEGER :: itacty 
       
    IF (smooth_method) THEN
       DO itacty=1,nb_SPHER
          SPcoor(1:3,itacty) = get_coor_SPHER(spher2bdyty(1,itacty),spher2bdyty(2,itacty))
       END DO
    ELSE 
       DO itacty=1,nb_SPHER
          SPcoor(1:3,itacty) = get_coorTT_SPHER(spher2bdyty(1,itacty),spher2bdyty(2,itacty))
       enddo   
    END IF

    DO itacty=1,nb_SPHER          
      IF ( XPERIODIC ) THEN
        IF ( SPcoor(1,itacty)  > xperiode ) THEN
          SPcoor(1,itacty) = SPcoor(1,itacty) - xperiode
        ELSE IF ( SPcoor(1,itacty) < 0.D0 ) THEN
          SPcoor(1,itacty) = SPcoor(1,itacty) + xperiode
        END IF
      END IF
      IF ( YPERIODIC ) THEN
        IF ( SPcoor(2,itacty)  > yperiode ) THEN
          SPcoor(2,itacty) = SPcoor(2,itacty) - yperiode
        ELSE IF ( SPcoor(2,itacty) < 0.D0 ) THEN
          SPcoor(2,itacty) = SPcoor(2,itacty) + yperiode
        END IF
      END IF
    END DO

    
  END SUBROUTINE coor_prediction_SPSPx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_SPSPx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_SPSPx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_SPSPx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    xperiode  = per
    XPERIODIC = FLAG
    
  END SUBROUTINE set_xperiodic_data_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_SPSPx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    yperiode  = per
    YPERIODIC = FLAG
    
  END SUBROUTINE set_yperiodic_data_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_box_SPSPx

    IMPLICIT NONE

    INTEGER :: isee,errare,ibdy
                               !1234567890123456789012
    character(len=22) :: IAM = 'mod_SPSPx::compute_box'

    minray     = get_min_radius_SPHER()
    maxray     = get_max_radius_SPHER()
    meanradius = get_mean_radius_SPHER()

    IF (minray > maxray ) THEN
       call faterr(IAM,'Messing error computing minray and maxray')
    END IF

    ! computing largest alert distance between disks 
    maxalert=0.D0  
    DO isee=1,SIZE(see)
       IF (see(isee)%cdtac == 'SPHER' .AND. see(isee)%antac == 'SPHER') THEN
          maxalert=MAX(maxalert,see(isee)%alert)
       END IF
    END DO
   
    Lbox   = 1.01D0*(2.D0*maxray + maxalert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/minray

    IF (.NOT. ALLOCATED(adjac))THEN
       ALLOCATE(adjac(nb_SPHER),stat=errare)
       IF (errare /=0 ) THEN
          call faterr(IAM,'Error allocating adjac')
       END IF
       DO ibdy=1,nb_SPHER
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    ELSE
       DO ibdy=1,nb_SPHER
          IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    END IF
  
    IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
    ALLOCATE(nb_adj(nb_SPHER),stat=errare)
    IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating nb_adj')
    END IF

    nb_adj=0

    ! SPcoor are coordinates of bodies owning SPHER to be used in selecting prox tactors
    IF (ALLOCATED(SPcoor)) DEALLOCATE(SPcoor)
    ALLOCATE(SPcoor(3,nb_SPHER),stat=errare)

  END SUBROUTINE compute_box_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_SPSPx

    IMPLICIT NONE

    INTEGER                    :: errare,icdtac,iantac,isee
    INTEGER                    :: icdan,ibdy
    CHARACTER(len=5)           :: cdcol,ancol
    REAL(kind=8),DIMENSION(3)  :: coorcd,cooran
    REAL(kind=8)               :: rayan,raycd,adist
    INTEGER                    :: ibox1,ibox2,ibox3
    INTEGER                    :: ibox1cd,ibox2cd,ibox3cd
    INTEGER                    :: ibox1an,ibox2an,ibox3an,icdpop,ianpop
    REAL(kind=8)               :: Xleft,Xright,Yleft,Yright,Zup,Zdown

    LOGICAL                    :: visible
    REAL(kind=8)               :: masscd,massan
    CHARACTER(len=30)          :: IAM='mod_SPSPx::select_prox_tactors'
    character(len=80) :: cout

    IF (ALLOCATED(box)) THEN
       DO ibox3 = minibox3,maxibox3
          DO ibox2 = minibox2,maxibox2
             DO ibox1 = minibox1,maxibox1
                IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%which)) DEALLOCATE(box(ibox1,ibox2,ibox3)%which)
             END DO
          END DO
       END DO
       DEALLOCATE(box)
    END IF

    ! Building boxes for quick sorting

    Xleft   =  1.D24
    Xright  = -1.D24
    Yleft   =  1.D24
    Yright  = -1.D24
    Zup     = -1.D24
    Zdown   =  1.D24

    DO ibdy=1,nb_SPHER
       visible=get_visible_SPHER(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = SPcoor(1:3,ibdy)
       Xleft = MIN(coorcd(1),Xleft )
       Xright= MAX(coorcd(1),Xright)
       Yleft = MIN(coorcd(2),Yleft )
       Yright= MAX(coorcd(2),Yright)
       Zup   = MAX(coorcd(3),Zup   )
       Zdown = MIN(coorcd(3),Zdown )
    END DO

    IF(XPERIODIC)THEN
       IF(Xright>xperiode)THEN
          print*,Xright,xperiode
          print*,maxloc(SPcoor(1,:))
          CALL FATERR(IAM,'The xmax right coordinate is greater than the periode')
       END IF
       IF(Xleft<0.D0)THEN
          CALL FATERR(IAM,'The xmin left coordinate is less than zero')
       END IF
       Xright = xperiode
       Xleft  = 0.D0
    END IF

    IF(YPERIODIC)THEN
       IF(Yright>yperiode)THEN
          CALL FATERR(IAM,'The ymax right coordinnate is greater than the periode')
       END IF
       IF(Yleft<0.D0)THEN
          CALL FATERR(IAM,'The ymin left coordinate is less than zero')
       END IF
       Yright = yperiode
       Yleft  = 0.D0
    END IF

    minibox1 = 1
    maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
    minibox2 = 1
    maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
    minibox3 = 1
    maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)
    maxpopul = 4*(1+INT(norm))*(1+INT(norm))*(1+INT(norm))

    maxpopul = MIN(maxpopul,nb_SPHER)

    ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
   
    IF (errare /=0 ) THEN
       CALL FATERR(IAM,'error allocating box')
    END IF
    DO ibox3=minibox3,maxibox3
       DO ibox2=minibox2,maxibox2
          DO ibox1=minibox1,maxibox1
             box(ibox1,ibox2,ibox3)%popul=0
             ALLOCATE(box(ibox1,ibox2,ibox3)%which(maxpopul),stat=errare)
             IF (errare /=0 ) THEN
                CALL FATERR(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%which')
             END IF
          END DO
       END DO
    END DO
   
    DO ibdy=1,nb_SPHER
       visible=get_visible_SPHER(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = SPcoor(1:3,ibdy)
       ibox1 = 1+INT((coorcd(1)-Xleft )*Lbox_1)
       ibox2 = 1+INT((coorcd(2)-Yleft )*Lbox_1)
       ibox3 = 1+INT((coorcd(3)-Zdown )*Lbox_1)
       IF (      ibox1 < minibox1 .OR. ibox1 > maxibox1 &
            .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2 &
            .OR. ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
          write(cout,'(A,I0,A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
          write(cout,'(A,I0,A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
          write(cout,'(A,I0,A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
          write(cout,'(A13,I5,A13)')'  body SPHER ',ibdy,' out of boxes'
          call faterr(IAM,cout)
       END IF
   
       box(ibox1,ibox2,ibox3)%popul=box(ibox1,ibox2,ibox3)%popul+1
       if( box(ibox1,ibox2,ibox3)%popul > size(box(ibox1,ibox2,ibox3)%which) ) then
           call faterr(IAM, "Estimated max popul limit reached.")
       end if
       box(ibox1,ibox2,ibox3)%which(box(ibox1,ibox2,ibox3)%popul)=ibdy

    END DO
     
    nb_rough_SPSPx = 0

    NULLIFY(Root) 
    NULLIFY(Current)
    NULLIFY(Previous)

    DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = minibox2,maxibox2
          DO ibox1cd = minibox1,maxibox1 
             DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                cdcol = get_color_SPHER(icdtac)

                DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                   DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
                      DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)            
                         DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                            iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)

                            IF (iantac .LE. icdtac) CYCLE
                            IF (is_SPHER_same_RBDY3(icdtac,iantac) ) CYCLE
                            IF (is_SPHER_same_THREAD(icdtac,iantac)) CYCLE

                            ancol = get_color_SPHER(iantac)
                            isee = get_isee_specific('SPHER',cdcol,ancol)
                            IF (isee.EQ.0) CYCLE
                            adist=see(isee)%alert 
                               
                            coorcd = SPcoor(1:3,icdtac)
                            cooran = SPcoor(1:3,iantac)
                            raycd = get_radius_SPHER(icdtac)
                            rayan = get_radius_SPHER(iantac)
                            
                            adist=0.1005D+01*adist+raycd+rayan
                               
                            IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                 .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                 .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN
                               nb_rough_SPSPx = nb_rough_SPSPx+1
                               IF ( nb_rough_SPSPx == 1) THEN
                                  ALLOCATE(Root)
                                  Current => Root
                                  NULLIFY(Root%p)
                               ELSE
                                  ALLOCATE(Current)
                                  Previous%n => Current
                               END IF
                               Current%val%cd   = icdtac
                               Current%val%an   = iantac
                               Current%val%isee = isee
                               Current%val%xperiodic = 0
                               Current%val%yperiodic = 0
                               Current%p => Previous
                               NULLIFY(Current%n)
                               Previous => Current
                            END IF
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO
    END DO

    IF (XPERIODIC) THEN
       DO ibox3cd = minibox3,maxibox3
          DO ibox2cd = minibox2,maxibox2
             !DO ibox1cd = maxibox1-1,maxibox1
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1 
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_SPHER(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
                         !DO ibox1an = minibox1,minibox1+1
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)       
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
!fd le 03/01/08 je vire le teste sur l'ordre qui risque de masquer des contacts, il
!fd n'est pas licite car ici on ne passe pas 2 fois sur toutes cases 
!fd                               IF ((iantac .LE. icdtac).OR. is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               IF (is_SPHER_same_THREAD(icdtac,iantac)) CYCLE
                               IF (is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               ancol = get_color_SPHER(iantac)
                               isee = get_isee_specific('SPHER',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = SPcoor(1:3,icdtac)
                               cooran = SPcoor(1:3,iantac)
                               raycd = get_radius_SPHER(icdtac)
                               rayan = get_radius_SPHER(iantac)

                               cooran(1) = cooran(1) + xperiode

                               adist=0.1005D+01*adist+raycd+rayan
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN
                                  nb_rough_SPSPx = nb_rough_SPSPx+1

                                  IF ( nb_rough_SPSPx == 1) THEN
                                     ALLOCATE(Root)
                                     Current => Root
                                     NULLIFY(Root%p)
                                  ELSE
                                     ALLOCATE(Current)
                                     Previous%n => Current
                                  END IF
                                  Current%val%cd        = icdtac
                                  Current%val%an        = iantac
                                  Current%val%isee      = isee
                                  Current%val%xperiodic = 1
                                  Current%val%yperiodic = 0
                                  Current%p => Previous
                                  NULLIFY(Current%n)
                                  Previous => Current
                               END IF
                            END DO
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO
    END IF

    IF(YPERIODIC)THEN
       DO ibox3cd = minibox3,maxibox3
          !DO ibox2cd = maxibox2-1,maxibox2
          DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2 
             DO ibox1cd = minibox1,maxibox1 
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_SPHER(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      !DO ibox2an = minibox2,minibox2+1                   
                      DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
                         DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)  
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
!fd le 03/01/08 je vire le teste sur l'ordre qui risque de masquer des contacts, il
!fd n'est pas licite car ici on ne passe pas 2 fois sur toutes cases 
!fd                               IF ((iantac .LE. icdtac).OR. is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               IF (is_SPHER_same_THREAD(icdtac,iantac)) CYCLE
                               IF (is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               ancol = get_color_SPHER(iantac)
                               isee = get_isee_specific('SPHER',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = SPcoor(1:3,icdtac)
                               cooran = SPcoor(1:3,iantac)
                               raycd = get_radius_SPHER(icdtac)
                               rayan = get_radius_SPHER(iantac)

                               cooran(2) = cooran(2) + yperiode

                               adist=0.1005D+01*adist+raycd+rayan
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_SPSPx = nb_rough_SPSPx+1

                                  IF ( nb_rough_SPSPx == 1) THEN
                                     ALLOCATE(Root)
                                     Current => Root
                                     NULLIFY(Root%p)
                                  ELSE
                                     ALLOCATE(Current)
                                     Previous%n => Current
                                  END IF
                                  Current%val%cd        = icdtac
                                  Current%val%an        = iantac
                                  Current%val%isee      = isee
                                  Current%val%xperiodic = 0
                                  Current%val%yperiodic = 1
                                  Current%p => Previous
                                  NULLIFY(Current%n)
                                  Previous => Current
                               END IF
                            END DO
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO
    END IF

!fd le 04/01/08 contrairement au 2D il faut traiter les coins !! 

!fd A VOIR si une seule boite ca va deconner

    IF (XPERIODIC .AND. YPERIODIC) THEN

!fd on commence par le coin extreme en haut qui voit le coin initial en bas

       DO ibox3cd = minibox3,maxibox3
          !DO ibox2cd = maxibox2-1,maxibox2
          DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2
             !DO ibox1cd = maxibox1-1,maxibox1
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_SPHER(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      !DO ibox2an = minibox2,minibox2+1
                      DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
                         !DO ibox1an = minibox1,minibox1+1
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)   
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
!fd le 03/01/08 je vire le teste sur l'ordre qui risque de masquer des contacts, il
!fd n'est pas licite car ici on ne passe pas 2 fois sur toutes cases 
!fd                               IF ((iantac .LE. icdtac).OR. is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               IF (is_SPHER_same_THREAD(icdtac,iantac)) CYCLE
                               IF (is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE
                               ancol = get_color_SPHER(iantac)
                               isee = get_isee_specific('SPHER',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = SPcoor(1:3,icdtac)
                               cooran = SPcoor(1:3,iantac)
                               raycd = get_radius_SPHER(icdtac)
                               rayan = get_radius_SPHER(iantac)

                               cooran(1) = cooran(1) + xperiode
                               cooran(2) = cooran(2) + yperiode

                               adist=0.1005D+01*adist+raycd+rayan
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_SPSPx = nb_rough_SPSPx+1

                                  IF ( nb_rough_SPSPx == 1) THEN
                                     ALLOCATE(Root)
                                     Current => Root
                                     NULLIFY(Root%p)
                                  ELSE
                                     ALLOCATE(Current)
                                     Previous%n => Current
                                  END IF
                                  Current%val%cd        = icdtac
                                  Current%val%an        = iantac
                                  Current%val%isee      = isee
                                  Current%val%xperiodic = 1
                                  Current%val%yperiodic = 1
                                  Current%p => Previous
                                  NULLIFY(Current%n)
                                  Previous => Current
                               END IF
                            END DO
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO

!fd on termine par le coin extreme en bas qui voit le coin initial en haut

       DO ibox3cd = minibox3,maxibox3
          !DO ibox2cd = minibox2,minibox2+1
          DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
             !DO ibox1cd = maxibox1-1,maxibox1
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1 
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_SPHER(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      !DO ibox2an = maxibox2-1,maxibox2
                      DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2
                         !DO ibox1an = minibox1,minibox1+1
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)  
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
!fd le 03/01/08 je vire le teste sur l'ordre qui risque de masquer des contacts, il
!fd n'est pas licite car ici on ne passe pas 2 fois sur toutes cases 
!fd                               IF ((iantac .LE. icdtac).OR. is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               IF (is_SPHER_same_THREAD(icdtac,iantac)) CYCLE
                               IF (is_SPHER_same_RBDY3(icdtac,iantac)) CYCLE

                               ancol = get_color_SPHER(iantac)
                               isee = get_isee_specific('SPHER',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = SPcoor(1:3,icdtac)
                               cooran = SPcoor(1:3,iantac)
                               raycd = get_radius_SPHER(icdtac)
                               rayan = get_radius_SPHER(iantac)

                               cooran(1) = cooran(1) + xperiode
                               cooran(2) = cooran(2) - yperiode

                               adist=0.1005D+01*adist+raycd+rayan
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_SPSPx = nb_rough_SPSPx+1

                                  IF ( nb_rough_SPSPx == 1) THEN
                                     ALLOCATE(Root)
                                     Current => Root
                                     NULLIFY(Root%p)
                                  ELSE
                                     ALLOCATE(Current)
                                     Previous%n => Current
                                  END IF
                                  Current%val%cd        = icdtac
                                  Current%val%an        = iantac
                                  Current%val%isee      = isee
                                  Current%val%xperiodic =  1
                                  Current%val%yperiodic = -1
                                  Current%p => Previous
                                  NULLIFY(Current%n)
                                  Previous => Current
                               END IF
                            END DO
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          END DO
       END DO

    END IF

    WRITE(cout,'(4X,I10,A20)') nb_rough_SPSPx,' SPSPx roughly found'
    call logmes(cout)

    IF (ALLOCATED(rough_SPSPx)) DEALLOCATE(rough_SPSPx)
    ALLOCATE(rough_SPSPx(nb_rough_SPSPx))
    
    IF (ALLOCATED(this)) DEALLOCATE(this)
    ALLOCATE(this(NbInteractionByContact*nb_rough_SPSPx))

    DO icdan=nb_rough_SPSPx,1,-1
     
       Previous => Current%p
       rough_SPSPx(icdan)%cd   = Current%val%cd
       rough_SPSPx(icdan)%an   = Current%val%an
       rough_SPSPx(icdan)%isee = Current%val%isee

       !am: each rough interaction belongs to the defalut group
       rough_SPSPx(icdan)%group = NOINT

       rough_SPSPx(icdan)%xperiodic = Current%val%xperiodic
       rough_SPSPx(icdan)%yperiodic = Current%val%yperiodic

       masscd = get_mass_SPHER(spher2bdyty(1,Current%val%cd))
       massan = get_mass_SPHER(spher2bdyty(1,Current%val%an))

       rough_SPSPx(icdan)%meff = masscd*massan/(masscd+massan)

       raycd = get_radius_SPHER(Current%val%cd)
       rayan = get_radius_SPHER(Current%val%an)

       rough_SPSPx(icdan)%reff = raycd*rayan/(raycd+rayan)

       DEALLOCATE(Current)
       Current => Previous
    END DO
   
    NULLIFY(Root)

    nb_SPSPx = NbInteractionByContact*nb_rough_SPSPx

  END SUBROUTINE creation_tab_visu_SPSPx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_contact_spspx
    implicit none  
    logical :: to_keep, all_dof_cd, all_dof_an
    integer(kind=4)    :: i4_input(5), i4_output(5)
    integer(kind=4)    :: icdan, itac, i, ibdy
    real(kind=8)       :: gapTT, r8_vec_out(3,11)
    character(len=103) :: cout
    character(len=32)  :: IAM

    IAM = 'mod_SPSPx::compute_contact_SPSPx'
    
    icdan   = 0        
    nb_SPSPx= 0
    nb_adj  = 0
    
    if (nb_rough_SPSPx == 0 ) return
       
    do itac = 1, nb_rough_SPSPx
      
      all_dof_cd = all_dof_driven_SPHER(rough_SPSPx(itac)%cd)
      all_dof_an = all_dof_driven_SPHER(rough_SPSPx(itac)%an)
      if( all_dof_cd .and. all_dof_an ) cycle

      i4_input(1) = rough_SPSPx(itac)%cd
      i4_input(2) = rough_SPSPx(itac)%an

      i4_input(3) = rough_SPSPx(itac)%isee
      i4_input(4) = rough_SPSPx(itac)%xperiodic
      i4_input(5) = rough_SPSPx(itac)%yperiodic

      call compute_one_contact_SPSPx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)

      if( to_keep ) then
        icdan = icdan+1

        this(icdan)%icdbtac = spher2bdyty(2, i4_input(1))
        this(icdan)%ianbtac = spher2bdyty(2, i4_input(2))

        this(icdan)%icdbtyp = spher2bdyty(3, i4_input(1))
        this(icdan)%ianbtyp = spher2bdyty(3, i4_input(2))

        this(icdan)%icdctyp = i_spher
        this(icdan)%ianctyp = i_spher

        this(icdan)%iadj   = i4_output(1)
        this(icdan)%icdbdy = i4_output(2)
        this(icdan)%icdtac = i4_output(3)
        this(icdan)%ianbdy = i4_output(4)
        this(icdan)%iantac = i4_output(5)

        this(icdan)%icdsci = 0
        this(icdan)%iansci = 0

        this(icdan)%isee  = i4_input(3)
        this(icdan)%group = rough_SPSPx(itac)%group

        this(icdan)%tuc    = r8_vec_out(:, 1)
        this(icdan)%nuc    = r8_vec_out(:, 2)
        this(icdan)%suc    = r8_vec_out(:, 3)

        this(icdan)%gapTTbegin =  gapTT
        this(icdan)%xperiodic  = i4_input(4)
        this(icdan)%yperiodic  = i4_input(5)

        this(icdan)%icdent = get_ent_SPHER(i4_output(2))
        this(icdan)%ianent = get_ent_SPHER(i4_output(4))

        this(icdan)%Gcdt = r8_vec_out(1:3, 4)
        this(icdan)%Gcdn = r8_vec_out(1:3, 5)
        this(icdan)%Gcds = r8_vec_out(1:3, 6)
        this(icdan)%Gant = r8_vec_out(1:3, 7)
        this(icdan)%Gann = r8_vec_out(1:3, 8)
        this(icdan)%Gans = r8_vec_out(1:3, 9)

        this(icdan)%vltBEGIN = r8_vec_out(1, 10)
        this(icdan)%vlnBEGIN = r8_vec_out(2, 10)
        this(icdan)%vlsBEGIN = r8_vec_out(3, 10)

        this(icdan)%rlt = 0.d0
        this(icdan)%rln = 0.d0
        this(icdan)%rls = 0.d0
        this(icdan)%vlt = this(icdan)%vltBEGIN
        this(icdan)%vln = this(icdan)%vlnBEGIN
        this(icdan)%vls = this(icdan)%vlsBEGIN
        this(icdan)%gapTT  = this(icdan)%gapTTbegin
        this(icdan)%status = i_nknow
    
        this(icdan)%reff = rough_SPSPx(itac)%reff
        this(icdan)%meff = rough_SPSPx(itac)%meff
         
        this(icdan)%coor(1:3) = r8_vec_out(:,11)

        call get_behaviour_( icdan, see, tact_behav )

        !123456789012345678901234567890
        select case(tact_behav(this(icdan)%lawnb)%lawty)
        case('WET_3C                        ')
          if (this(icdan)%internal(1) .eq. 0.d0) then
            this(icdan)%internal(2)   = gapTT + get_radius_SPHER(i4_input(1)) + get_radius_SPHER(i4_input(2))
            this(icdan)%internal(4:6) = this(icdan)%coor(1:3)
            this(icdan)%internal(1)   = 1.d0
            this(icdan)%internal(3)   = 0.d0
            print*,'CASE-0'
            print*,'n: ',this(icdan)%internal(2),'- t: ',this(icdan)%internal(3)
            print*,this(icdan)%internal(4:6)
          else
            !\todo : il s'appelle pas meuporg...
            !sep  = this(icdan)%coor(1:3) - this(icdan)%internal(4:6)
            !sepn = dot_product(sep,this(icdan)%uc(:,2))
            !sept = sep(1:3) - sepn(1:3)

            !this(icdan)%internal(3) = sqrt(dot_product(sept,sept))
            print*,'CASE-1'
            print*,'n: ',this(icdan)%internal(2),'- t: ',this(icdan)%internal(3)
            print*,this(icdan)%internal(4:6)
          end if
        case('KV_WET                        ')
          if (this(icdan)%internal(1) .eq. 0.d0) then
             this(icdan)%internal(3) = gapTT + get_radius_SPHER(i4_input(1)) + get_radius_SPHER(i4_input(2))
             this(icdan)%internal(2) = gapTT
             this(icdan)%internal(1) = 1.d0
          end if
        case default
        end select

        !mr exp
        this(icdan)%QSij = 0.D0
        this(icdan)%QCij = 0.D0

      end if
      
    end do

    nb_SPSPx = icdan

    write(cout,'(1X,I10,A12)') nb_SPSPx,' SPSPx found'
    call logmes(cout)

    do ibdy = 1, get_nb_SPHER()
      if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
      if (nb_adj(ibdy) /= 0) allocate(adjac(ibdy)%icdan(nb_adj(ibdy)))
    end do
    
    do icdan = 1, nb_SPSPx
      adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
    end do
    
    if (allocated(violation)) deallocate(violation)
    allocate(violation(nb_SPSPx))
    
  end subroutine compute_contact_spspx
!------------------------------------------------------------------------
  subroutine compute_one_contact_SPSPx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)
    implicit none
    !> integer input parameters of the contact computation
    integer(kind=4), dimension(5)   , intent(in)  :: i4_input
    !> integer output parameters of the contact computation
    integer(kind=4), dimension(5)   , intent(out) :: i4_output
    !> real output parameters of the contact computation
    real(kind=8)   , dimension(3,11), intent(out) :: r8_vec_out
    !> is rough contact close enough
    logical, intent(out) :: to_keep
    !> computed gap of the contact
    real(kind=8), intent(out) :: gapTT
    !
    integer(kind=4) :: icdtac, iantac, isee, ixprd, iyprd, icdbdy, ianbdy, cd_ent, an_ent
    real(kind=8)    :: adist, raycd, rayan, nonuc, den
    real(kind=8), dimension(6) :: cd_Vbegin, an_Vbegin
    real(kind=8), dimension(3) :: cdlev,anlev,cd_shift,an_shift,coordcd, coordan, sep
    real(kind=8), dimension(3,3) :: uc, RC, cdframe, anframe, Gan, Gcd
    character(len=30)            :: IAM
    character(len=103)           :: cout

    !     123456789012345678901234567890
    IAM= 'mod_SPSPx::compute_one_contact'

    to_keep = .false.

    icdtac = i4_input(1)
    iantac = i4_input(2)
    isee   = i4_input(3)
    ixprd  = i4_input(4)
    iyprd  = i4_input(5)

    !if (WITH_VAV.and..not.FIRST_TIME_VAV) then
    !  icdbdy = i4_input(5)
    !  ianbdy = i4_input(6)
    !end if
         
    adist   = see(isee)%alert 
    coordcd = SPcoor(1:3,icdtac)
    coordan = SPcoor(1:3,iantac)

    coordan(1) = coordan(1) + (real(ixprd,8)*xperiode)
    coordan(2) = coordan(2) + (real(iyprd,8)*yperiode)

    raycd   = get_radius_SPHER(icdtac)
    rayan   = get_radius_SPHER(iantac)
   
    sep   = coordcd - coordan
    nonuc = sqrt((sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)))
           
    if (nonuc < 1.D-18) then
       write(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of spher',icdtac, &
            'within 1.e-18 from center of spher',iantac,'in comp_local_frame_SPSPx' 
       call faterr(IAM,cout)
    end if
           
    gapTT = nonuc - (raycd+rayan)

    if (gapTT .gt. adist) return

    to_keep = .true.

    nb_adj(icdtac) = nb_adj(icdtac)+1
    !iadj           = nb_adj(icdtac)
    
    if (smooth_method) then
       cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iV____)
       an_Vbegin = get_vlocy_SPHER(spher2bdyty(1,iantac),iV____)
    else
       cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iVbeg_)
       an_Vbegin = get_vlocy_SPHER(spher2bdyty(1,iantac),iVbeg_)
    end if
              
    i4_output(1) = nb_adj(icdtac)
    i4_output(2) = spher2bdyty(1,icdtac)
    i4_output(3) = icdtac
    i4_output(4) = spher2bdyty(1,iantac)
    i4_output(5) = iantac

    uc(1:3,2) = sep/nonuc

    cd_ent = get_ent_SPHER(i4_output(2))
    an_ent = get_ent_SPHER(i4_output(4))

    entity(cd_ent)%nb = entity(cd_ent)%nb+1
    entity(an_ent)%nb = entity(an_ent)%nb+1             

    r8_vec_out(1:3,11) = 0.5 * ((coordcd+coordan) + uc(1:3,2)*(rayan-raycd))

    if(  ( uc(1,2)*uc(2,2) == 0. ).or. &
         ( uc(2,2)*uc(3,2) == 0. ).or. &
         ( uc(3,2)*uc(1,2) == 0. )) then
       
       uc(1,1) = uc(3,2)
       uc(2,1) = uc(1,2)
       uc(3,1) = uc(2,2)
    else
       den = sqrt(  (uc(2,2)*uc(3,2))**2 + (uc(1,2)*uc(3,2))**2 &
                  + 4.d0*(uc(1,2)*uc(2,2))**2 )
       
       uc(1,1) =  -uc(2,2)*uc(3,2)/den
       uc(2,1) =  -uc(3,2)*uc(1,2)/den
       uc(3,1) = 2.d0*uc(1,2)*uc(2,2)/den
    end if
              
    uc(:,3) = cross_product(uc(:,1),uc(:,2))

    r8_vec_out(1:3,1:3) = uc
 
    cd_shift = get_shiftTT_SPHER(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))
    an_shift = get_shiftTT_SPHER(spher2bdyty(1,iantac),spher2bdyty(2,iantac))

    cdframe = get_inertia_frameTT_SPHER(spher2bdyty(1,icdtac))
    anframe = get_inertia_frameTT_SPHER(spher2bdyty(1,iantac))

    cdlev = (-raycd*uc(:,2)) + cd_shift
    anlev = ( rayan*uc(:,2)) + an_shift

    ! mapping of the contact coordinate in the inertial frame
    ! for the antagonist
    Rc(:,1)= anframe(2,:)*anlev(3) - anframe(3,:)*anlev(2)
    Rc(:,2)= anframe(3,:)*anlev(1) - anframe(1,:)*anlev(3)
    Rc(:,3)= anframe(1,:)*anlev(2) - anframe(2,:)*anlev(1)

    Gan = matmul(RC,uc)

    ! for the candidate
    Rc(:,1)=cdframe(2,:)*cdlev(3) - cdframe(3,:)*cdlev(2)
    Rc(:,2)=cdframe(3,:)*cdlev(1) - cdframe(1,:)*cdlev(3)
    Rc(:,3)=cdframe(1,:)*cdlev(2) - cdframe(2,:)*cdlev(1)

    Gcd = matmul(RC,uc)

    !---
    r8_vec_out(1:3,4:6) = Gcd
    r8_vec_out(1:3,7:9) = Gan
              
    !gapTTbegin    = gapTT

    
    r8_vec_out(1,10) = dot_product(cd_Vbegin(1:3) - an_Vbegin(1:3),uc(:,1)) &
                     + dot_product(cd_Vbegin(4:6),Gcd(:,1)) - dot_product(an_Vbegin(4:6),Gan(:,1))
    r8_vec_out(2,10) = dot_product(cd_Vbegin(1:3) - an_Vbegin(1:3),uc(:,2)) &
                     + dot_product(cd_Vbegin(4:6),Gcd(:,2)) - dot_product(an_Vbegin(4:6),Gan(:,2))
    r8_vec_out(3,10) =  dot_product(cd_Vbegin(1:3) - an_Vbegin(1:3),uc(:,3)) &
                     + dot_product(cd_Vbegin(4:6),Gcd(:,3)) - dot_product(an_Vbegin(4:6),Gan(:,3))

  end subroutine

  SUBROUTINE smooth_computation_SPSPx

    IMPLICIT NONE
    INTEGER          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    DO icdan=1,nb_SPSPx
       
       CALL compute_3D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vlsBEGIN,this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            this(icdan)%reff,this(icdan)%meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vls,this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    DO icdan=1,nb_SPSPx  
       CALL nullify_reac_SPSPx(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_SPSPx
       CALL injj_SPSPx(icdan,H*this(icdan)%rls,H*this(icdan)%rlt,H*this(icdan)%rln,iIreac)
    END DO
    
  END SUBROUTINE smooth_computation_SPSPx
!!!------------------------------------------------------------------------ 
  subroutine display_prox_tactors_SPSPx

    implicit none
    integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
    character(len=5) :: cdmodel, anmodel
   
    nb_SPHER = get_nb_SPHER()

    DO icdtac=1,nb_SPHER    
       DO iadj=1,nb_adj(icdtac)         
          icdan  = adjac(icdtac)%icdan(iadj)
          icdbdy = this(icdan)%icdbdy
          ianbdy = this(icdan)%ianbdy
          iantac = this(icdan)%iantac
          cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
          anmodel = get_body_model_name_from_id( spher2bdyty(3,iantac) )
          WRITE(*,'(A1)')' '
          WRITE(*,'(A6,2X,I5)')'$icdan',icdan
          !123456789012345678901234567890123456789012345678901234567890123456789012
          WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
          WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               cdmodel,icdbdy,'SPHER',icdtac,see(this(icdan)%isee)%behav,  &
               anmodel,ianbdy,'SPHER',iantac
          
          WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
          WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
          WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
          !o write(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
          WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
          WRITE(*,'(27X,2X,A5,D14.7)')'gTT-=',this(icdan)%gapTTBegin
          WRITE(*,'(A1)')' '               
       END DO
    END DO
    
104 FORMAT(27X,3(2X,A5,D14.7))
    
  end subroutine display_prox_tactors_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE stock_rloc_SPSPx
    
    IMPLICIT NONE
    INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac
    INTEGER            :: errare,iadj
    CHARACTER(len=80)  :: cout
    CHARACTER(len=20)  :: IAM = 'mod_SPSPx::stoc_rloc'

    nb_SPHER=get_nb_SPHER()

    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_SPHER),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,'error allocating verlt')
       END IF
       DO icdtac=1,nb_SPHER
          verlt(icdtac)%adjsz=0
          iadj=nb_adj(icdtac)

          IF (iadj > 0) THEN
             verlt(icdtac)%adjsz=iadj
             call new_verlet_(icdtac, iadj, errare)
          ELSE
             call nullify_verlet_(icdtac)
          END IF
          IF (errare /=0 ) THEN
             CALL FATERR(IAM,'error in allocating verlt(icdtac)%.....')
          END IF
       END DO
    ELSE 
       DO icdtac=1,nb_SPHER
          verlt(icdtac)%adjsz=0
 
          call free_verlet_(icdtac)

          iadj = nb_adj(icdtac)
          IF (iadj > 0) THEN
             verlt(icdtac)%adjsz=iadj
             call new_verlet_(icdtac, iadj, errare)
          ELSE
             call nullify_verlet_(icdtac)
          END IF
       END DO
    END IF

    Rn_max = 0.D0

    ! filling data:
    ! icdtac : serial number of candidate contactor for contact icdan
    ! iantac : serial number of antagonist contactor for contact icdan 
    ! iadj   : serial adjacent number of pair contactor adjacent to candidate contactor for contact icdan 
    DO icdan=1,nb_SPSPx
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       iadj   = this(icdan)%iadj

       verlt(icdtac)%icdan(iadj)   = icdan
       verlt(icdtac)%cdbdy         = spher2bdyty(1,icdtac)
       verlt(icdtac)%cdtac         = spher2bdyty(2,icdtac)
       verlt(icdtac)%cdmodel       = spher2bdyty(3,icdtac)
       verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
       verlt(icdtac)%anbdy(iadj)   = spher2bdyty(1,iantac)
       verlt(icdtac)%antac(iadj)   = spher2bdyty(2,iantac)
       verlt(icdtac)%anmodel(iadj) = spher2bdyty(3,iantac)
       verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
       verlt(icdtac)%status(iadj)  = this(icdan)%status

       verlt(icdtac)%rls(iadj)     = this(icdan)%rls/H
       verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
       verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
       verlt(icdtac)%vls(iadj)     = this(icdan)%vls
       verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
       verlt(icdtac)%vln(iadj)     = this(icdan)%vln
       verlt(icdtac)%suc(1:3,iadj) = this(icdan)%suc(1:3)
       verlt(icdtac)%tuc(1:3,iadj) = this(icdan)%tuc(1:3)
       verlt(icdtac)%nuc(1:3,iadj) = this(icdan)%nuc(1:3)
       verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT

       verlt(icdtac)%coor(1:3,iadj) = this(icdan)%coor
       
       verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
       
       Rn_max = MAX(Rn_max,this(icdan)%rln)
    END DO

    IF (Rn_max.EQ.0.D0) Rn_max=1.D0

    nb_vSPSPx = nb_SPSPx

    WRITE(cout,'(1X,I10,A12)') nb_vSPSPx,' stock SPSPx'
    call logmes(cout)

  END SUBROUTINE stock_rloc_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_SPSPx

    IMPLICIT NONE
    
    INTEGER            :: icdan,icdtac,iantac,iadj
    CHARACTER(len=21)  :: IAM = 'mod_SPSPx::recup_rloc'
    character(len=80)  :: cout
    
    if (.not. allocated(verlt)) then
       call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
       return
    end if
    
    nb_recup_SPSPx = 0
    
    DO icdan=1,nb_SPSPx
       this(icdan)%rls = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%rln = 0.D0
       this(icdan)%statusBEGIN=i_nknow

       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       
       IF (verlt(icdtac)%adjsz /= 0) THEN
         if ( verlt(icdtac)%cdbdy  == spher2bdyty(1,icdtac) .and. &
              verlt(icdtac)%cdtac  == spher2bdyty(2,icdtac) .and. &
              verlt(icdtac)%cdmodel== spher2bdyty(3,icdtac) ) then
            do iadj = 1, verlt(icdtac)%adjsz
              if ( verlt(icdtac)%anbdy(iadj)  == spher2bdyty(1,iantac) .and. &
                   verlt(icdtac)%antac(iadj)  == spher2bdyty(2,iantac) .and. &
                   verlt(icdtac)%anmodel(iadj)== spher2bdyty(3,iantac) ) then
                 this(icdan)%rls         = verlt(icdtac)%rls(iadj)*H
                 this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H
                 this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H

                 this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

                 this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                 
                 nb_recup_SPSPx = nb_recup_SPSPx + 1
                 exit
              end if
            end do
         end if
       ENDIF
    END DO

    WRITE(cout,'(1X,I10,A12)') nb_recup_SPSPx,' recup SPSPx'
    call logmes(cout)

  END SUBROUTINE recup_rloc_SPSPx
!!!------------------------------------------------------------------------ 
  subroutine read_ini_Vloc_Rloc 

    implicit none
    integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    integer            :: cdmodel, anmodel
    real(kind=8)       :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
    character(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
    integer            :: errare,icdtact
    integer            :: ibehav,nb_internal,i_internal

    character(len=103) :: cout
    character(len=29)  :: IAM = 'mod_SPSPx::read_ini_Vloc_Rloc'

    errare=0
    nb_SPHER = get_nb_SPHER()

    ! first reading: sizing verlt
    ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
    ! For this purpose nb_adj is introduced.
    
    IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_SPHER),stat=errare)
    IF (errare /=0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF

    nb_adj = 0

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) .NE. 'icdan') CYCLE
       IF (G_clin(9:13).NE. 'SPSPx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       IF (xxl_check) THEN
          READ(G_clin(1:84),'(1X,A5,2X,I7,2X,A5,2X,I7,9X,A5,2X,A5,2X,I7,2X,A5,2X,I7,2X,A5)')  &
               cdbdy,icdbdy,cdtac,icdtac,                                          &
               behav,                                                              &
               anbdy,ianbdy,antac,iantac,                                          &
               sttus
       ELSE
          READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
               cdbdy,icdbdy,cdtac,icdtac,                                          &
               behav,                                                              &
               anbdy,ianbdy,antac,iantac,                                          &
               sttus
       END IF
       IF (cdtac.NE.'SPHER' .OR. antac.NE.'SPHER') CYCLE
       cdmodel = get_body_model_id_from_name( cdbdy )
       do icdtact=1,nb_SPHER
          if ( spher2bdyty(1,icdtact) == icdbdy .and. &
               spher2bdyty(2,icdtact) == icdtac .and. &
               spher2bdyty(3,icdtact) == cdmodel ) then
             nb_adj(icdtact) = nb_adj(icdtact) + 1
             exit
          end if
       end do
    END DO
    
    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_SPHER),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,'error allocating verlt')
       END IF
       DO icdtac=1,nb_SPHER
          verlt(icdtac)%adjsz=0
          iadj=nb_adj(icdtac)
    
          IF (iadj > 0) THEN
             verlt(icdtac)%adjsz=iadj
             call new_verlet_(icdtac, iadj, errare)
          ELSE
             call nullify_verlet_(icdtac)
          END IF
          IF (errare /=0 ) THEN
             CALL FATERR(IAM,'error in allocating verlt(icdtac)%.....')
          END IF
       END DO
    ELSE 
       DO icdtac=1,nb_SPHER
          call free_verlet_(icdtac)
          verlt(icdtac)%adjsz = 0
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

    nb_adj = 0
    icdan  = 0

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13)/= 'SPSPx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       IF (xxl_check) THEN
          READ(G_clin(1:84),'(1X,A5,2X,I7,2X,A5,2X,I7,9X,A5,2X,A5,2X,I7,2X,A5,2X,I7,2X,A5)')  &
               cdbdy,icdbdy,cdtac,icdtac,                                                     &
               behav,                                                                         &
               anbdy,ianbdy,antac,iantac,                                                     &
               sttus
       ELSE
          READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
               cdbdy,icdbdy,cdtac,icdtac,                                                     &
               behav,                                                                         &
               anbdy,ianbdy,antac,iantac,                                                     &
               sttus
       END IF
       IF (cdtac.NE.'SPHER' .OR. antac.NE.'SPHER') CYCLE
       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )
       DO icdtact=1,nb_SPHER
          IF ( spher2bdyty(1,icdtact) == icdbdy .and. &
               spher2bdyty(2,icdtact) == icdtac .and. &
               spher2bdyty(3,icdtact) == cdmodel  ) then

             icdan = icdan + 1

             nb_adj(icdtact) = nb_adj(icdtact) + 1

             verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

             verlt(icdtact)%cdbdy                   = icdbdy
             verlt(icdtact)%cdtac                   = icdtac
             verlt(icdtact)%cdmodel                 = cdmodel
             verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
             verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
             verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
             verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
             IF( .NOT. read_G_clin()) EXIT
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') rls,rlt,rln
             verlt(icdtact)%rls(nb_adj(icdtact)) = rls
             verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
             verlt(icdtact)%rln(nb_adj(icdtact)) = rln
             IF( .NOT. read_G_clin()) EXIT
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') vls,vlt,vln
             verlt(icdtact)%vls(nb_adj(icdtact)) = vls
             verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
             verlt(icdtact)%vln(nb_adj(icdtact)) = vln
             IF( .NOT. read_G_clin()) EXIT
             READ(G_clin(1:90),'(27X,2(7X,D14.7))') gapTT
             verlt(icdtact)%gapTT(nb_adj(icdtact)) = gapTT
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(30:34)== 's(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             END IF
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(30:34)== 't(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%tuc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%tuc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%tuc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             END IF
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(30:34)== 'n(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%nuc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%nuc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%nuc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             END IF
             IF( .NOT. read_G_clin()) EXIT
             IF (G_clin(30:34)== 'coo1=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%coor(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%coor(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%coor(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             END IF
             verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
             ibehav      = get_ibehav(behav)
             nb_internal = get_nb_internal(ibehav)
             
             IF (nb_internal /= 0 ) THEN  
                IF( .NOT. read_G_clin()) EXIT
                DO i_internal=1, nb_internal
                   READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') &
                        verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
                END DO
             END IF
          END IF
       END DO
       CYCLE
    END DO
    
    nb_vSPSPx=0
    
    DO icdtact=1,nb_SPHER
       nb_vSPSPx = nb_vSPSPx + nb_adj(icdtact)
       
       IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
          WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
               'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
          CALL FATERR(IAM,cout)
       END IF
    END DO
    
104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
  END SUBROUTINE read_ini_Vloc_Rloc
!!!------------------------------------------------------------------------ 
  SUBROUTINE write_out_Vloc_Rloc(nfich)
    
    IMPLICIT NONE
    
    INTEGER           :: iadj,icdtact
    INTEGER           :: nfich,icdan,icdtac,iantac

    character(len=20) :: fmt
    character(len=5)  :: cdmodel, anmodel

    nb_SPHER = get_nb_SPHER()

   if( nb_vSPSPx > 99999 ) then
       xxl_check = .true.
   end if

    IF (xxl_check) THEN
       DO icdtact=1,nb_SPHER    
          DO iadj=1,nb_adj(icdtact)         
             icdan  = adjac(icdtact)%icdan(iadj)
             icdtac = this(icdan)%icdtac
             iantac = this(icdan)%iantac

             cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
             anmodel = get_body_model_name_from_id( spher2bdyty(3,iantac) )
             WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','SPSPx',icdan  
                                 !123456789012345678901234567890123456789012345678901234567890123456789012345678901234
             WRITE(nfich,'(A84)')' cdbdy    numbr  cdtac    numbr  CDVER  behav  anbdy    numbr  antac    numbr  sttus'
             WRITE(nfich,'(1X,A5,2X,I7,2X,A5,2X,I7,9X,A5,2X,A5,2X,I7,2X,A5,2X,I7,2X,A5,2X,I7)') &
                  !pta old fashion 'RBDY3',spher2bdyty(1,icdtac),'SPHER',spher2bdyty(2,icdtac),  &
                  cdmodel,get_visibleID_SPHER(icdtac),'SPHER',spher2bdyty(2,icdtac),  &
                  see(this(icdan)%isee)%behav,  &
                  !pta old fashion 'RBDY3',spher2bdyty(1,iantac),'SPHER',spher2bdyty(2,iantac),  &
                  anmodel,get_visibleID_SPHER(iantac),'SPHER',spher2bdyty(2,iantac),  &
                  get_contact_status_name_from_id(this(icdan)%status),iantac
             WRITE(nfich,104) 'rls/H',this(icdan)%rls/H,'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H
             WRITE(nfich,104) 'vls =',this(icdan)%vls  ,'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  
             WRITE(nfich,103) 'gTT =',this(icdan)%gapTT
             WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)
             WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
             WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
             WRITE(nfich,104) 'coo1=',this(icdan)%coor(1)    ,'coo2=',this(icdan)%coor(2)    ,'coo3=',this(icdan)%coor(3)
             
             IF (this(icdan)%nb_internal /= 0) THEN
               CALL write_internal_comment(nfich,this(icdan)%lawnb)
               write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
               write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
             END IF
             WRITE(nfich,'(A1)')' '
             
          END DO
       END DO
   ELSE 
       DO icdtact=1,nb_SPHER    
          DO iadj=1,nb_adj(icdtact)         
             icdan  = adjac(icdtact)%icdan(iadj)
             icdtac = this(icdan)%icdtac
             iantac = this(icdan)%iantac

             cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
             anmodel = get_body_model_name_from_id( spher2bdyty(3,iantac) )

             WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','SPSPx',icdan  
                                 !1234567890123456789012345678901234567890123456789012345678901234567890123456
             WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
             WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)') &
                  cdmodel,spher2bdyty(1,icdtac),'SPHER',spher2bdyty(2,icdtac),  &
                  see(this(icdan)%isee)%behav,  &
                  anmodel,spher2bdyty(1,iantac),'SPHER',spher2bdyty(2,iantac),  &
                  get_contact_status_name_from_id(this(icdan)%status),iantac
             WRITE(nfich,104) 'rls/H',this(icdan)%rls/H,'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H
             WRITE(nfich,104) 'vls =',this(icdan)%vls  ,'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  
             WRITE(nfich,103) 'gTT =',this(icdan)%gapTT
             WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)
             WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
             WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
             WRITE(nfich,104) 'coo1=',this(icdan)%coor(1)    ,'coo2=',this(icdan)%coor(2)    ,'coo3=',this(icdan)%coor(3)
             
             IF (this(icdan)%nb_internal /= 0) THEN
               CALL write_internal_comment(nfich,this(icdan)%lawnb)
               write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
               write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
             END IF
             WRITE(nfich,'(A1)')' '
             
          END DO
       END DO
   END IF 
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

  END SUBROUTINE write_out_Vloc_Rloc
!!!------------------------------------------------------------------------ 
!!  !> \brief Write the local velocity and reaction of an interaction to a file
!!  subroutine write_out_one_Vloc_Rloc_SPSPx(nfich, inter)
!!    implicit none
!!    !> [in] unit file in which to write
!!    integer(kind=4), intent(in) :: nfich
!!    !> the interaction to write
!!    type(T_interaction) :: inter
!!      
!!    write(nfich,'(A6,2X,A5,2X,I7)')'$icdan', 'SPSPx', inter%icdan     
!!    !1234567890123456789012345678901234567890123456789012345678901234567890123456  
!!    write(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
!!    write(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)') &
!!         'RBDY3', inter%icdbdy, 'SPHER', spher2bdyty(2,inter%icdtac), see(inter%isee)%behav, &
!!         'RBDY3', inter%ianbdy, 'SPHER', spher2bdyty(2,inter%iantac), inter%status,inter%iantac
!!    write(nfich,104) 'rls/H',inter%rl(3)/H,'rlt/H',inter%rl(1)/H,'rln/H',inter%rl(2)/H
!!    write(nfich,104) 'vls =',inter%vl(3)  ,'vlt =',inter%vl(1)  ,'vln =',inter%vl(2)  
!!    write(nfich,103) 'gTT =',inter%gapTT
!!    write(nfich,104) 's(1)=',inter%uc(1,3), 's(2)=',inter%uc(2,3), 's(3)=',inter%uc(3,3)
!!    write(nfich,104) 't(1)=',inter%uc(1,1), 't(2)=',inter%uc(2,1), 't(3)=',inter%uc(3,1)
!!    write(nfich,104) 'n(1)=',inter%uc(1,2), 'n(2)=',inter%uc(2,2), 'n(3)=',inter%uc(3,2)
!!    write(nfich,104) 'coo1=',inter%coor(1), 'coo2=',inter%coor(2), 'coo3=',inter%coor(3)
!!    
!!    if (inter%nb_internal /= 0) then
!!      call write_internal_comment(nfich,inter%lawnb)
!!      write(nfich,'(5(1x,D14.7))')  inter%internal(1:inter%nb_internal)
!!    end if
!!    write(nfich,'(A1)')' '               
!!
!!103  format(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
!!104  format(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
!!
!!  end subroutine write_out_one_Vloc_Rloc_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_reac_SPSPx(icdan,storage)

    IMPLICIT NONE

    INTEGER,INTENT(in):: icdan 
    INTEGER           :: icdbdy,ianbdy
    INTEGER           :: storage
    
    icdbdy=this(icdan)%icdbdy
    CALL nullify_reac_SPHER(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL nullify_reac_SPHER(ianbdy,storage)
    
  END SUBROUTINE nullify_reac_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_SPSPx(icdan,storage)
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy,storage
    
    icdbdy = this(icdan)%icdbdy
    CALL nullify_vlocy_SPHER(icdbdy,storage)
    
    ianbdy = this(icdan)%ianbdy
    CALL nullify_vlocy_SPHER(ianbdy,storage)
    
  END SUBROUTINE nullify_vlocy_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE vitrad_SPSPx( icdan, storage, need_full_vlocy)

    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy
    INTEGER            :: storage
    logical, optional  :: need_full_vlocy
    
    icdbdy=this(icdan)%icdbdy
    CALL comp_vlocy_SPHER(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL comp_vlocy_SPHER(ianbdy,storage)
    
  END SUBROUTINE vitrad_SPSPx
!!!------------------------------------------------------------------------  
  SUBROUTINE injj_SPSPx(icdan,RSIK,RTIK,RNIK,storage)
    
    IMPLICIT NONE
    
    INTEGER     ,INTENT(in)    :: icdan
    REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
    INTEGER,     DIMENSION(6)  :: cdccdof,anccdof
    REAL(kind=8),DIMENSION(6)  :: cdreac, anreac
    INTEGER                    :: icdbdy,ianbdy
    INTEGER                    :: storage
    
    icdbdy    = this(icdan)%icdbdy
    ianbdy    = this(icdan)%ianbdy
    
    cdccdof(1)= 1
    anccdof(1)= 1
    cdreac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
    anreac(1) = -cdreac(1)
    cdccdof(2)= 2
    anccdof(2)= 2
    cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
    anreac(2) = -cdreac(2)
    cdccdof(3)= 3
    anccdof(3)= 3
    cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
    anreac(3) = -cdreac(3)
    
    cdccdof(4)= 4
    anccdof(4)= 4
    cdccdof(5)= 5
    anccdof(5)= 5
    cdccdof(6)= 6
    anccdof(6)= 6
    
    cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
    cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
    cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK
    
    anreac(4) =-this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
    anreac(5) =-this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
    anreac(6) =-this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK

    CALL add_reac_SPHER(icdbdy,cdccdof,cdreac,storage)
    CALL add_reac_SPHER(ianbdy,anccdof,anreac,storage)
    
  END SUBROUTINE injj_SPSPx
!!!------------------------------------------------------------------------  
  SUBROUTINE prjj_SPSPx(icdan,VSIK,VTIK,VNIK,storage)
    
    IMPLICIT NONE
    
    INTEGER     ,INTENT(in)   :: icdan
    REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
    INTEGER                   :: icdbdy,ianbdy
    INTEGER                   :: storage
    REAL(kind=8),DIMENSION(6) :: Vcd,Van
    
    icdbdy=this(icdan)%icdbdy
    ianbdy=this(icdan)%ianbdy
    
    Vcd = get_vlocy_SPHER(icdbdy,storage)
    Van = get_vlocy_SPHER(ianbdy,storage)    
    
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
    
  END SUBROUTINE prjj_SPSPx
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
!!!
!!! GETTER : USE ALPHABETIC ORDER
!!!
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------
  SUBROUTINE get_heat_sources_SPSPx(icdan,QC,QS)

    IMPLICIT NONE
    INTEGER      :: icdan
    REAL(kind=8) :: QC,QS

    QC = this(icdan)%QCij
    QS = this(icdan)%QSij

  END SUBROUTINE get_heat_sources_SPSPx
!!!------------------------------------------------------------------------
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_SPSPx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_SPSPx = nb_SPSPx
    CASE(i_verlet_tactor)
       get_nb_SPSPx = nb_vSPSPx
    CASE(i_rough_tactor)
       get_nb_SPSPx = nb_rough_SPSPx
    CASE(i_recup_tactor)
       get_nb_SPSPx = nb_recup_SPSPx
    END SELECT

  END FUNCTION get_nb_SPSPx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_surf_SPSPx(icdan)

    IMPLICIT NONE
    INTEGER      :: icdan
    REAL(kind=8) :: raycd,rayan

    raycd   = get_radius_SPHER(this(icdan)%icdtac)
    rayan   = get_radius_SPHER(this(icdan)%iantac)

    get_surf_SPSPx = PI_g*(raycd*rayan)*(raycd*rayan) &
         /((raycd+rayan)*(raycd+rayan))

  END FUNCTION get_surf_SPSPx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_SPSPx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_SPSPx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_SPSPx
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_xperiode_SPSPx(icdan)

    IMPLICIT NONE
    INTEGER :: icdan
    
    get_xperiode_SPSPx = this(icdan)%xperiodic
   
  END FUNCTION get_xperiode_SPSPx
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_yperiode_SPSPx(icdan)

    IMPLICIT NONE
    INTEGER :: icdan

    get_yperiode_SPSPx = this(icdan)%yperiodic

  END FUNCTION get_yperiode_SPSPx
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
!!!
!!! GETTER : USE ALPHABETIC ORDER
!!!
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------
  SUBROUTINE put_heat_sources_SPSPx(icdan,QC,QS)

    IMPLICIT NONE
    INTEGER      :: icdan
    REAL(kind=8) :: QC,QS

    this(icdan)%QCij = QC
    this(icdan)%QSij = QS

  END SUBROUTINE put_heat_sources_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE SPSPx2ENT(icdan,icdent,ianent)

    IMPLICIT NONE
    INTEGER :: icdan,icdent,ianent
    
    icdent = get_ENT_SPHER(this(icdan)%icdbdy)
    ianent = get_ENT_SPHER(this(icdan)%ianbdy)

  END SUBROUTINE SPSPx2ENT
!!!------------------------------------------------------------------------ 
  SUBROUTINE SPSPx2SPHER(icdan,icdtac,iantac)
    
    IMPLICIT NONE
    INTEGER :: icdan,icdtac,iantac
    
    icdtac = this(icdan)%icdtac
    iantac = this(icdan)%iantac
    
  END SUBROUTINE SPSPx2SPHER
!!!------------------------------------------------------------------------
  LOGICAL FUNCTION RUN_SPSPx()

    IMPLICIT NONE
    
    RUN_SPSPx = RUN_TACTOR

  END FUNCTION RUN_SPSPx
!!!------------------------------------------------------------------------
  logical function CHECK_SPSPx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_SPSPx = check_SPSPx_
      return
    end if

    con_pedigree%module_name = 'SPSPx'

    con_pedigree%id_cdan  = i_spspx
    con_pedigree%id_cdtac = i_spher
    con_pedigree%id_antac = i_spher

    cdtact2bdyty => spher2bdyty
    antact2bdyty => spher2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_SPHER = get_nb_SPHER()
    if( nb_SPHER < 2 ) then
      CHECK_SPSPx = check_SPSPx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'SPHER' .and. see(isee)%antac == 'SPHER') then
        check_SPSPx_ = .true.
        exit
      end if
    end do

    CHECK_SPSPx = check_SPSPx_
    return

  end function CHECK_SPSPx
!!!------------------------------------------------------------------------ 
  SUBROUTINE update_cohe_SPSPx(icdan,cohe)

   IMPLICIT NONE
   INTEGER      :: icdan,itact
   REAL(kind=8) :: cohe,WScd,WSan

   itact = this(icdan)%icdtac

   WScd = get_WS_SPHER(spher2bdyty(1,itact),spher2bdyty(2,itact))

   itact = this(icdan)%iantac

   WSan = get_WS_SPHER(spher2bdyty(1,itact),spher2bdyty(2,itact))

   IF (ABS(WScd+WSan).LT.1.D-16) THEN
      cohe = 0.D0
   ELSE
      cohe = (WSan*WScd)/(WScd+WSan)
   END IF

  END SUBROUTINE update_cohe_SPSPx

!am: declarations fonctions necessaires a la DDM

!!!------------------------------------------------------------------------ 
!am: all trivial tests (autocontact, etc) are supposed to be made, so that using a linked list is not necessary ^^
  subroutine set_anonymous_to_rough_SPSPx(anonymous_rough, nb_anonymous_rough)
   implicit none

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! inputs 
   type(PTR_CONTAINER), intent(in)   :: anonymous_rough
   integer(kind=4)    , intent(in)   :: nb_anonymous_rough
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! implicit outputs (global variables set by this function) :
   ! type(T_rough_SPSPx), dimension(:), allocatable :: rough_SPSPx 
   ! integer(kind=4)                                :: nb_rough_SPSPx 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! local variables
   integer                                 :: icdan,icdtac,iantac,isee,group
   integer                                 :: nb_rough_tmp
   real(kind=8)                            :: masscd,massan,raycd,rayan
   character(len=5)                        :: cdcol,ancol
   integer(kind=4),  dimension(:), pointer :: cdan              ! "i4" vector of the anonymous object
   type(T_object)                          :: anonymous_contact ! used to visit "anonymous_rough"
   character(len=100)                      :: cout

   ! The number of rough contacts is exactly the size of anonyous_contact
   nb_rough_SPSPx = nb_anonymous_rough

   IF (ALLOCATED(rough_SPSPx)) DEALLOCATE(rough_SPSPx)
   ALLOCATE(rough_SPSPx(nb_rough_SPSPx))     ! the visibility array used in compute_contact is allocated

   ! This step will store roughly detetcted interactions in rough_SPSPx.

   ! This step will eliminate autocontacts or interactions associated to no interaction law
   ! That's why nb_rough_SPSPx <= nb_anonymous_rough
   do icdan=1,nb_anonymous_rough
      ! Object associated to icdan index is got
      anonymous_contact = get_object(anonymous_rough,icdan)    
      ! The corresponding "candidate index"/"natagoniste index" couple is got from "anonymous_contact"
      cdan => get_i4_vector(anonymous_contact)
      icdtac=cdan(1)
      iantac=cdan(2)

      ! The group to which belongs the interaction is got from "anonymous_contact"
      group=cdan(3)
      
      ! Looking for the interaction law corresponding to the current interaction
      cdcol = get_color_SPHER(icdtac)
      ancol = get_color_SPHER(iantac)
      isee  = get_isee_specific('SPHER', cdcol, ancol)

      ! paranoid check
      !if (isee==0) ...

      ! The new interaction is stored
      rough_SPSPx(icdan)%cd         = icdtac
      rough_SPSPx(icdan)%an         = iantac
      rough_SPSPx(icdan)%isee       = isee
      rough_SPSPx(icdan)%group      = group
      ! Periodic case is not supported yet!
      rough_SPSPx(icdan)%xperiodic  = 0
      rough_SPSPx(icdan)%yperiodic  = 0
      
      raycd = get_radius_SPHER(icdtac)
      rayan = get_radius_SPHER(iantac)
      
      rough_SPSPx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_SPHER(spher2bdyty(1, icdtac))
      massan=get_mass_SPHER(spher2bdyty(1, iantac))
      
      rough_SPSPx(icdan)%meff = masscd*massan/(masscd+massan)
   end do

   WRITE(cout, '(4X,I10,A20)') nb_rough_SPSPx,' SPSPx roughly found'
   call logmes(cout)

   ! "this" array is allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(NbInteractionByContact*nb_rough_SPSPx))            ! the oversized array this is temporaly allocated

   nb_SPSPx = NbInteractionByContact*nb_rough_SPSPx

  end subroutine set_anonymous_to_rough_SPSPx
!!!------------------------------------------------------------------------ 

!!!------------------------------------------------------------------------ 
!am: all trivial tests (autocontact, etc) are supposed to be made, so that using a linked list is not necessary ^^
  subroutine set_interactions_to_rough_SPSPx(interactions, nb_interactions)
   implicit none

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! inputs 
   integer(kind=4)                              , intent(in)   :: nb_interactions
   integer(kind=4), dimension(3*nb_interactions), intent(in)   :: interactions
   
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! implicit outputs (global variables set by this function) :
   ! type(T_rough_SPSPx), dimension(:), allocatable :: rough_SPSPx 
   ! integer(kind=4)                                :: nb_rough_SPSPx 

   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
   ! local variables
   integer                                 :: icdan,icdtac,iantac,isee,group
   real(kind=8)                            :: masscd,massan,raycd,rayan
   character(len=5)                        :: cdcol,ancol
   character(len=100)                      :: cout

   ! The number of rough contacts is exactly the size of anonyous_contact
   nb_rough_SPSPx = nb_interactions

   WRITE(cout, '(4X,I10,A20)') nb_rough_SPSPx,' SPSPx roughly found'
   call logmes(cout)

   IF (ALLOCATED(rough_SPSPx)) DEALLOCATE(rough_SPSPx)
   ALLOCATE(rough_SPSPx(nb_rough_SPSPx))     ! the visibility array used in compute_contact is allocated

   ! This step will store roughly detetcted interactions in rough_SPSPx.

   ! This step will eliminate autocontacts or interactions associated to no interaction law
   ! That's why nb_rough_SPSPx <= nb_anonymous_rough
   do icdan=1,nb_rough_SPSPx

      icdtac=interactions(3*(icdan-1)+1)
      iantac=interactions(3*(icdan-1)+2)

      ! The group to which belongs the interaction is got from "anonymous_contact"
      group=interactions(3*(icdan-1)+3)
      
      ! Looking for the interaction law corresponding to the current interaction
      cdcol = get_color_SPHER(icdtac)
      ancol = get_color_SPHER(iantac)
      isee  = get_isee_specific('SPHER', cdcol, ancol)

      ! paranoid check
      !if (isee==0) ...

      ! The new interaction is stored
      rough_SPSPx(icdan)%cd         = icdtac
      rough_SPSPx(icdan)%an         = iantac
      rough_SPSPx(icdan)%isee       = isee
      rough_SPSPx(icdan)%group      = group

      ! still not supported 
      rough_SPSPx(icdan)%xperiodic  = 0
      rough_SPSPx(icdan)%yperiodic  = 0
      
      raycd = get_radius_SPHER(icdtac)
      rayan = get_radius_SPHER(iantac)
      
      rough_SPSPx(icdan)%reff = raycd*rayan/(raycd+rayan)
      
      masscd=get_mass_SPHER(spher2bdyty(1, icdtac))
      massan=get_mass_SPHER(spher2bdyty(1, iantac))
      
      rough_SPSPx(icdan)%meff = masscd*massan/(masscd+massan)
   end do

   ! "this" array is allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(NbInteractionByContact*nb_rough_SPSPx))            ! the oversized array this is temporaly allocated

   nb_SPSPx = NbInteractionByContact*nb_rough_SPSPx

  end subroutine set_interactions_to_rough_SPSPx
!!!------------------------------------------------------------------------ 

!--------------------------------------------------------------------------------------------------
 subroutine put_icdan_group_SPSPx(icdan,group)

   implicit none

   integer, intent(in) :: icdan, group

   this(icdan)%group=group 

 end subroutine put_icdan_group_SPSPx
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
 SUBROUTINE get_nb_INTRF_SPSPx(nb_INTRF)

   implicit none

   integer, intent(out) :: nb_INTRF ! Nombre de "contacts d'interface",
                                    ! i.e. de contacts dont le %cd et/ou
                                    ! le %an sont taggés "INTRF".
   integer :: i

   nb_INTRF=0

   if (nb_SPSPx == 0) return

   do i = 1, nb_SPSPx
      if ( this(i)%group == INTRF ) nb_INTRF=nb_INTRF+1
   end do

 END SUBROUTINE get_nb_INTRF_SPSPx
!--------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------
 SUBROUTINE get_list_INTRF_SPSPx(nb_INTRF, liste_INTRF)

   implicit none

   integer, intent(in) :: nb_INTRF ! Nombre de "contacts d'interface",
                                   ! i.e. de contacts dont le %cd et/ou
                                   ! le %an sont taggés "INTRF".

   integer, dimension(nb_INTRF), intent(out) :: liste_INTRF

   integer           :: i, compteur
                             !1234567890123456789012345
   CHARACTER(len=25) :: IAM= 'mod_SPSPx::get_list_INTRF'

   compteur=0
   do i = 1, nb_SPSPx
      if ( this(i)%group == INTRF ) then
         compteur = compteur + 1
         if (compteur>nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")
         liste_INTRF(compteur) = i
      end if
   end do

   if (compteur/=nb_INTRF) call FATERR(IAM,"nb_INTRF does not fit with compteur")

 END SUBROUTINE get_list_INTRF_SPSPx
!--------------------------------------------------------------------------------------------------

!------------------------------------------------------------------------ 
!fd 26/02/08 experimental 
! -routine pour fixer le nombre d'interactions
! -algo de detection qui genere plusiers points de contact une fois pour toute
! 
!!!------------------------------------------------------------------------
  SUBROUTINE Set_NbInteractionByContact(ivalue)

    IMPLICIT NONE
    INTEGER,INTENT(IN) :: ivalue

    NbInteractionByContact = ivalue 


  END SUBROUTINE Set_NbInteractionByContact

  SUBROUTINE Set_ContactRadius(rvalue)

    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN) :: rvalue

    ContactRadius = rvalue 


  END SUBROUTINE Set_ContactRadius

  SUBROUTINE fd_compute_contact_spspx

    IMPLICIT NONE  

    INTEGER                     :: errare,isee,ixprd,iyprd
    INTEGER                     :: icdan,iadj,ibdy,icdtac,iantac,itac
    INTEGER                     :: cd_ent,an_ent
    REAL(kind=8)                :: raycd,rayan,adist,nonuc,gapTT
    REAL(kind=8)                :: den
    REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
    REAL(kind=8),DIMENSION(3)   :: sep,coorcd,cooran,sept,sepn
    REAL(kind=8),DIMENSION(3)   :: cdlev,anlev,cd_shift,an_shift
    REAL(kind=8),DIMENSION(3,3) :: Rc,cdframe,anframe
    CHARACTER(len=103)          :: cout
    CHARACTER(len=35)           :: IAM='mod_SPSPx::fd_compute_contact_SPSPx'

    !   
    LOGICAL,SAVE                :: is_first_time=.TRUE.
    INTEGER                     :: ii
    REAL(kind=8)                :: angle
    REAL(kind=8),DIMENSION(3)   :: lvec

    IF ( is_first_time ) THEN

       icdan   = 0        
       nb_SPSPx= 0
       nb_adj  = 0


       IF (NbInteractionByContact == 1) THEN
         angle = 0.d0
       ELSE
         angle = 2.d0*PI_g / REAL(NbInteractionByContact,8)
       ENDIF

       PRINT*,angle,COS(angle),SIN(angle)


       IF (nb_rough_SPSPx /= 0 ) THEN

          DO itac=1,nb_rough_SPSPx

             icdtac = rough_SPSPx(itac)%cd
             iantac = rough_SPSPx(itac)%an
             isee   = rough_SPSPx(itac)%isee
             ixprd  = rough_SPSPx(itac)%xperiodic
             iyprd  = rough_SPSPx(itac)%yperiodic

             adist  = see(isee)%alert 
             raycd  = get_radius_SPHER(icdtac)
             rayan  = get_radius_SPHER(iantac)

             coorcd = SPcoor(1:3,icdtac)
             cooran = SPcoor(1:3,iantac)

             cooran(1) = cooran(1) + (REAL(ixprd,8)*xperiode)
             cooran(2) = cooran(2) + (REAL(iyprd,8)*yperiode)

             sep = coorcd - cooran
             nonuc = SQRT((sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)))

             IF (nonuc < 1.D-18) THEN
                WRITE(cout,'(A14,1X,I5,1X,A33,1X,I5,1X,A25)') 'center of spher',icdtac, &
                     'within 1.e-18 from center of spher',iantac,'in comp_local_frame_SPSPx' 
                CALL FATERR(IAM,cout)
             END IF

             gapTT = nonuc - (raycd+rayan)

             IF (gapTT .LE. adist) THEN

                IF (smooth_method) THEN
                   cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iV____)
                   an_Vbegin = get_vlocy_SPHER(spher2bdyty(1,iantac),iV____)
                ELSE
                   cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iVbeg_)
                   an_Vbegin = get_vlocy_SPHER(spher2bdyty(1,iantac),iVbeg_)
                END IF

                DO ii=1,NbInteractionByContact

                   icdan          = icdan + 1
                   nb_adj(icdtac) = nb_adj(icdtac)+1
                   iadj           = nb_adj(icdtac)

                   this(icdan)%iadj   = iadj  
                   this(icdan)%icdbdy = spher2bdyty(1,icdtac)
                   this(icdan)%icdtac = icdtac
                   this(icdan)%ianbdy = spher2bdyty(1,iantac)
                   this(icdan)%iantac = iantac
                   this(icdan)%isee   = isee     
                   this(icdan)%nuc    = sep/nonuc

                   cd_ent = get_ent_SPHER(this(icdan)%icdbdy)
                   an_ent = get_ent_SPHER(this(icdan)%ianbdy)

                   this(icdan)%icdent = cd_ent
                   this(icdan)%ianent = an_ent

                   entity(cd_ent)%nb = entity(cd_ent)%nb+1
                   entity(an_ent)%nb = entity(an_ent)%nb+1             

                   this(icdan)%xperiodic = ixprd
                   this(icdan)%yperiodic = iyprd

                   IF(  ( this(icdan)%nuc(1)*this(icdan)%nuc(2) == 0. ).OR. &
                        ( this(icdan)%nuc(2)*this(icdan)%nuc(3) == 0. ).OR. &
                        ( this(icdan)%nuc(3)*this(icdan)%nuc(1) == 0. )) THEN

                      this(icdan)%tuc(1)     = this(icdan)%nuc(3)
                      this(icdan)%tuc(2)     = this(icdan)%nuc(1)
                      this(icdan)%tuc(3)     = this(icdan)%nuc(2)
                   ELSE
                      den=SQRT((this(icdan)%nuc(2)*this(icdan)%nuc(3))**2  + &
                           (this(icdan)%nuc(1)*this(icdan)%nuc(3))**2  + &
                           4.D0*(this(icdan)%nuc(1)*this(icdan)%nuc(2))**2)

                      this(icdan)%tuc(1)     =   -this(icdan)%nuc(2)*this(icdan)%nuc(3)/den
                      this(icdan)%tuc(2)     =   -this(icdan)%nuc(3)*this(icdan)%nuc(1)/den
                      this(icdan)%tuc(3)     =  2.D0*this(icdan)%nuc(1)*this(icdan)%nuc(2)/den
                   END IF

                   this(icdan)%suc(1)        =  &
                        this(icdan)%tuc(3)*this(icdan)%nuc(2) &
                        -this(icdan)%tuc(2)*this(icdan)%nuc(3)
                   this(icdan)%suc(2)        =  &
                        this(icdan)%tuc(1)*this(icdan)%nuc(3) &
                        -this(icdan)%tuc(3)*this(icdan)%nuc(1)              
                   this(icdan)%suc(3)        =  &
                        this(icdan)%tuc(2)*this(icdan)%nuc(1) &
                        -this(icdan)%tuc(1)*this(icdan)%nuc(2)

                   ! calcul du bras de levier (le rayon + le shift + le decalage dans le plan tangent) dans le repere global

                   cd_shift = get_shiftTT_SPHER(spher2bdyty(1,icdtac),spher2bdyty(2,icdtac))
                   an_shift = get_shiftTT_SPHER(spher2bdyty(1,iantac),spher2bdyty(2,iantac))

                   cdlev = (-raycd*this(icdan)%nuc) + cd_shift
                   anlev = ( rayan*this(icdan)%nuc) + an_shift

                   lvec = 0.d0
                   IF ( angle /= 0.d0) lvec = ContactRadius*(COS((ii-1)*angle)*this(icdan)%tuc + SIN((ii-1)*angle)*this(icdan)%suc)

                   cdlev = cdlev + lvec
                   anlev = anlev + lvec

                   ! position du point de contact, qu'on visualise

                   this(icdan)%coor = (0.5*((coorcd+cooran) + this(icdan)%nuc(1:3)*(rayan-raycd))) + lvec

                   !

                   cdframe = get_inertia_frameTT_SPHER(spher2bdyty(1,icdtac))
                   anframe = get_inertia_frameTT_SPHER(spher2bdyty(1,iantac))


                   ! mapping of the contact coordinate in the inertial frame
                   ! for the antagonist

                   Rc(1,1)= anframe(2,1)*anlev(3) - anframe(3,1)*anlev(2)
                   Rc(2,1)= anframe(2,2)*anlev(3) - anframe(3,2)*anlev(2)
                   Rc(3,1)= anframe(2,3)*anlev(3) - anframe(3,3)*anlev(2)

                   Rc(1,2)= anframe(3,1)*anlev(1) - anframe(1,1)*anlev(3)
                   Rc(2,2)= anframe(3,2)*anlev(1) - anframe(1,2)*anlev(3)
                   Rc(3,2)= anframe(3,3)*anlev(1) - anframe(1,3)*anlev(3)

                   Rc(1,3)= anframe(1,1)*anlev(2) - anframe(2,1)*anlev(1)
                   Rc(2,3)= anframe(1,2)*anlev(2) - anframe(2,2)*anlev(1)
                   Rc(3,3)= anframe(1,3)*anlev(2) - anframe(2,3)*anlev(1)

                   this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
                   this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
                   this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

                   this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
                   this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
                   this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

                   this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
                   this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
                   this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

                   ! for the candidate

                   Rc(1,1)=cdframe(2,1)*cdlev(3) - cdframe(3,1)*cdlev(2)
                   Rc(2,1)=cdframe(2,2)*cdlev(3) - cdframe(3,2)*cdlev(2)
                   Rc(3,1)=cdframe(2,3)*cdlev(3) - cdframe(3,3)*cdlev(2)

                   Rc(1,2)=cdframe(3,1)*cdlev(1) - cdframe(1,1)*cdlev(3)
                   Rc(2,2)=cdframe(3,2)*cdlev(1) - cdframe(1,2)*cdlev(3)
                   Rc(3,2)=cdframe(3,3)*cdlev(1) - cdframe(1,3)*cdlev(3)

                   Rc(1,3)=cdframe(1,1)*cdlev(2) - cdframe(2,1)*cdlev(1)
                   Rc(2,3)=cdframe(1,2)*cdlev(2) - cdframe(2,2)*cdlev(1)
                   Rc(3,3)=cdframe(1,3)*cdlev(2) - cdframe(2,3)*cdlev(1)


                   this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
                   this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
                   this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

                   this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
                   this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
                   this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

                   this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
                   this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
                   this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

                   !---

                   this(icdan)%gapTTbegin    = gapTT

                   this(icdan)%vlsBEGIN = &
                        (cd_Vbegin(1) - an_Vbegin(1))*this(icdan)%suc(1) &
                        + (cd_Vbegin(2) - an_Vbegin(2))*this(icdan)%suc(2) &
                        + (cd_Vbegin(3) - an_Vbegin(3))*this(icdan)%suc(3) &
                        + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                        - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)

                   this(icdan)%vltBEGIN = &
                        (cd_Vbegin(1) - an_Vbegin(1))*this(icdan)%tuc(1) &
                        + (cd_Vbegin(2) - an_Vbegin(2))*this(icdan)%tuc(2) &
                        + (cd_Vbegin(3) - an_Vbegin(3))*this(icdan)%tuc(3) &
                        + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                        - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

                   this(icdan)%vlnBEGIN = &
                        (cd_Vbegin(1) - an_Vbegin(1))*this(icdan)%nuc(1) &
                        + (cd_Vbegin(2) - an_Vbegin(2))*this(icdan)%nuc(2) &
                        + (cd_Vbegin(3) - an_Vbegin(3))*this(icdan)%nuc(3) &
                        + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                        - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

                   this(icdan)%rls   = 0.D0
                   this(icdan)%rlt   = 0.D0
                   this(icdan)%rln   = 0.D0
                   this(icdan)%vls   = this(icdan)%vlsBEGIN
                   this(icdan)%vlt   = this(icdan)%vltBEGIN
                   this(icdan)%vln   = this(icdan)%vlnBEGIN
                   this(icdan)%gapTT = this(icdan)%gapTTbegin
                   this(icdan)%status= i_nknow
                   this(icdan)%meff  = rough_SPSPx(itac)%meff
                   this(icdan)%reff  = rough_SPSPx(itac)%reff

                   call get_behaviour_( icdan, see, tact_behav )

!!$             !123456789012345678901234567890
!!$             SELECT CASE(tact_behav(this(icdan)%lawnb)%lawty)
!!$             CASE('WET_3C                        ')
!!$                IF (this(icdan)%internal(1).EQ.0.D0) THEN
!!$                   this(icdan)%internal(2)   = raycd + rayan + gapTT
!!$                   this(icdan)%internal(4:6) = this(icdan)%coor(1:3)
!!$                   this(icdan)%internal(1) = 1.D0
!!$                   this(icdan)%internal(3) = 0.D0
!!$                   PRINT*,'CASE-0'
!!$                   PRINT*,'n: ',this(icdan)%internal(2),'- t: ',this(icdan)%internal(3)
!!$                   PRINT*,this(icdan)%internal(4:6)
!!$                ELSE
!!$                   sep = this(icdan)%coor(1:3) - this(icdan)%internal(4:6)
!!$                   sepn = sep(1)*this(icdan)%nuc(1)+sep(2)*this(icdan)%nuc(2)+sep(3)*this(icdan)%nuc(3)
!!$                   sept(1:3) = sep(1:3) - sepn(1:3)
!!$                   this(icdan)%internal(3) = SQRT(sept(1)*sept(1)+sept(2)*sept(2)+sept(3)*sept(3))
!!$                   PRINT*,'CASE-1'
!!$                   PRINT*,'n: ',this(icdan)%internal(2),'- t: ',this(icdan)%internal(3)
!!$                   PRINT*,this(icdan)%internal(4:6)
!!$                END IF
!!$             CASE('KV_WET                        ')
!!$                IF (this(icdan)%internal(1).EQ.0.D0) THEN
!!$                   this(icdan)%internal(3) = raycd + rayan + gapTT
!!$                   this(icdan)%internal(2) = gapTT
!!$                   this(icdan)%internal(1) = 1.D0
!!$                END IF
!!$             CASE DEFAULT
!!$             END SELECT

                   !fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
                   !fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

                   this(icdan)%icdcoor(1)=cdlev(1)*cdframe(1,1)+cdlev(2)*cdframe(2,1)+cdlev(3)*cdframe(3,1)
                   this(icdan)%icdcoor(2)=cdlev(1)*cdframe(1,2)+cdlev(2)*cdframe(2,2)+cdlev(3)*cdframe(3,2)
                   this(icdan)%icdcoor(3)=cdlev(1)*cdframe(1,3)+cdlev(2)*cdframe(2,3)+cdlev(3)*cdframe(3,3)
                                  
                   this(icdan)%iancoor(1)=anlev(1)*anframe(1,1)+anlev(2)*anframe(2,1)+anlev(3)*anframe(3,1)
                   this(icdan)%iancoor(2)=anlev(1)*anframe(1,2)+anlev(2)*anframe(2,2)+anlev(3)*anframe(3,2)
                   this(icdan)%iancoor(3)=anlev(1)*anframe(1,3)+anlev(2)*anframe(2,3)+anlev(3)*anframe(3,3)

                ENDDO

             END IF


          END DO

          nb_SPSPx = icdan

       END IF

       DO ibdy = 1,nb_SPHER
          IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
          IF (nb_adj(ibdy) /= 0) THEN
             ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
             IF (errare /=0 ) THEN
                CALL FATERR(IAM,'error in allocating adjac(icdbdy)%.....,')
             END IF
          END IF
       END DO

       DO icdan=1,nb_SPSPx
          adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
       END DO

       IF (ALLOCATED(violation)) DEALLOCATE(violation)
       ALLOCATE(violation(nb_SPSPx),stat=errare)


       is_first_time = .FALSE.

    ELSE





    ENDIF


  END SUBROUTINE fd_compute_contact_spspx
!------------------------------------------------------------------------

 subroutine clean_memory_SPSPx
   implicit none
   integer(kind=4) :: i, j, k

   call clean_memory_inter_meca_()

   nb_SPHER       = 0
   nb_SPSPx       = 0
   nb_vSPSPx      = 0
   nb_recup_SPSPx = 0

   if( allocated(box) ) then
     do i = 1, size(box,3)
       do j = 1, size(box,2)
         do k = 1, size(box,1)
           if( associated(box(k,j,i)%which) ) deallocate(box(k,j,i)%which)
         end do
       end do
     end do
     deallocate(box)
   end if

   nb_rough_SPSPx = 0
   if( allocated(rough_SPSPx) ) deallocate(rough_SPSPx)

   ! Root, Current and Previous should always be null outside creation_tab_visu

   if( allocated(SPcoor) ) deallocate(SPcoor)

   Reac_SPSPx_MAX = 0.D0

   module_checked_ = .FALSE.
   check_SPSPx_    = .FALSE.

 end subroutine

!! ! rm : getter on a rough spspx for testing itrHdl
!! subroutine get_rough_SPSPx(icdan, icdtac, iantac, isee, xperiodic, yperiodic)
!!   implicit none
!!   integer(kind=4), intent(in) :: icdan
!!   integer(kind=4), intent(out) :: icdtac, iantac, isee, xperiodic, yperiodic
!!
!!   icdtac    = rough_SPSPx(icdan)%cd
!!   iantac    = rough_SPSPx(icdan)%an
!!   isee      = rough_SPSPx(icdan)%isee
!!   xperiodic = rough_SPSPx(icdan)%xperiodic
!!   yperiodic = rough_SPSPx(icdan)%yperiodic
!!
!! end subroutine
!!
!! ! rm : allocating violation array to test itrHdl
!! subroutine reset_violation_SPSPx(nb)
!!   implicit none
!!   integer(kind=4), intent(in) :: nb
!!
!!   if( allocated(violation) ) deallocate(violation)
!!   allocate( violation(nb) )
!!
!! end subroutine
!!
!! subroutine reset_nb_adj_SPSPx()
!!   implicit none
!!
!!   if (.not. allocated(nb_adj)) allocate(nb_adj(get_nb_SPHER()))
!!
!!   nb_adj = 0
!!
!! end subroutine
!!
!! !> add an adjacent
!! subroutine add_adj_SPSPx(icdbdy, icdtac)
!!   implicit none
!!   !> body number of candidat to add adjacent to
!!   integer(kind=4), intent(in) :: icdbdy
!!   !> contactor number of candidat to add adjacent to
!!   integer(kind=4), intent(in) :: icdtac
!!   !
!!   integer(kind=4) :: i_tact
!!
!!   do i_tact =1, get_nb_SPHER()
!!     if (spher2bdyty(1,i_tact) == icdbdy .and. &
!!         spher2bdyty(2,i_tact) == icdtac       ) then
!!       nb_adj(i_tact) = nb_adj(i_tact) + 1
!!       exit
!!     end if
!!   end do
!!
!! end subroutine
!!
!! function get_nb_adj_SPSPx(i_tact)
!!   implicit none
!!   !> contactor number
!!   integer(kind=4), intent(in) :: i_tact
!!   !> number of interactions attached to contactor
!!   integer(kind=4) :: get_nb_adj_SPSPx
!!
!!   get_nb_adj_SPSPx = nb_adj(i_tact)
!!
!! end function
!!
!! !> \brief Create interaction of SPSP in a generic t2t
!! subroutine compute_contacts_in_t2t_SPSPx(t2t, icdan)
!!   implicit none  
!!   !> t2t in which to compute interactions
!!   type(T_tact2tact) :: t2t
!!   !> current number of dkdkx
!!   integer(kind=4), intent(in) :: icdan
!!   !
!!   integer(kind=4)    :: i, itest, ibehav, errare, i4_input(7), i4_output(5)
!!   real(kind=8)       :: gapTT, raycd, rayan, r8_vec_out(3,11)
!!   logical            :: to_keep
!!   character(len=40)  :: IAM
!!   character(len=103) :: cout
!!   !     1234567890123456789012345678901234567890
!!   IAM= 'mod_SPSPx::compute_contacts_in_t2t_SPSPx'
!!
!!   i4_input(1) = t2t%icdtac
!!   i4_input(2) = t2t%iantac
!!   i4_input(3) = t2t%isee
!!   i4_input(4) = t2t%xperiodic
!!   i4_input(5) = t2t%yperiodic
!!
!!   call compute_one_contact_SPSPx(i4_input, gapTT, i4_output, r8_vec_out, to_keep)
!!
!!   if( to_keep ) then
!!
!!     call set_nb_face2faces(t2t, 1)
!!
!!     t2t%f2f(1)%nb_ctc = 1
!!     call initialize_interactions(t2t%f2f(1), 1)
!!
!!     t2t%f2f(1)%ctc(1)%inter%cdan   = i_spspx
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdent = get_ent_SPHER(i4_output(2))
!!     t2t%f2f(1)%ctc(1)%inter%ianent = get_ent_SPHER(i4_output(4))
!!
!!     t2t%f2f(1)%ctc(1)%inter%icdtyp = i_spher
!!     t2t%f2f(1)%ctc(1)%inter%iantyp = i_spher
!!
!!     t2t%f2f(1)%ctc(1)%inter%iadj   = i4_output(1)
!!     t2t%f2f(1)%ctc(1)%inter%icdbdy = i4_output(2)
!!     t2t%f2f(1)%ctc(1)%inter%icdtac = i4_output(3)
!!     t2t%f2f(1)%ctc(1)%inter%ianbdy = i4_output(4)
!!     t2t%f2f(1)%ctc(1)%inter%iantac = i4_output(5)
!!
!!     t2t%f2f(1)%ctc(1)%inter%isee  = i4_input(3)
!!     !t2t%ctc(1)%inter%group = t2t%group
!!
!!     t2t%f2f(1)%ctc(1)%inter%uc(1:nbDIME,1:nbDIME) = r8_vec_out(:,1:3)
!!
!!     t2t%f2f(1)%ctc(1)%inter%gapTTbegin =  gapTT
!!     !xperiodic_SPSPx(icdan+1) =  t2t%xperiodic
!!     !yperiodic_SPSPx(icdan+1) =  t2t%yperiodic
!!
!!     t2t%f2f(1)%ctc(1)%inter%Gcd(1:nbDIME,1:3) = r8_vec_out(:,4:6)
!!     t2t%f2f(1)%ctc(1)%inter%Gan(1:nbDIME,1:3) = r8_vec_out(:,7:9)
!!
!!     t2t%f2f(1)%ctc(1)%inter%vlBEGIN(1:nbDIME) = r8_vec_out(:,10)
!!
!!     t2t%f2f(1)%ctc(1)%inter%rl     = 0.d0
!!     t2t%f2f(1)%ctc(1)%inter%vl     = t2t%f2f(1)%ctc(1)%inter%vlBEGIN
!!     t2t%f2f(1)%ctc(1)%inter%gapTT  = t2t%f2f(1)%ctc(1)%inter%gapTTbegin
!!     t2t%f2f(1)%ctc(1)%inter%status = 'nknow'
!!    
!!     !t2t%ctc(1)%inter%reff = t2t%reff
!!     !t2t%ctc(1)%inter%meff = t2t%meff
!!      
!!     t2t%f2f(1)%ctc(1)%inter%coor(1:3) = r8_vec_out(:,11)
!!
!!     itest = 0
!!     do ibehav = 1, size(tact_behav)
!!       if( see(t2t%isee)%behav == tact_behav(ibehav)%behav ) then
!!         t2t%f2f(1)%ctc(1)%inter%lawnb       = ibehav
!!         t2t%f2f(1)%ctc(1)%inter%i_law       = tact_behav(ibehav)%ilaw
!!         t2t%f2f(1)%ctc(1)%inter%nb_internal = get_nb_internal(ibehav)
!!         t2t%f2f(1)%ctc(1)%inter%internal    = init_internal(ibehav)
!!         itest = 1
!!         exit
!!       end if
!!     end do
!!     if( itest == 0 ) then
!!        write(cout,'(A,1x,I0,1x,A)') 'nickname',see(t2t%isee)%behav,'unknown in lawty'
!!        call logmes('check TACT-BEHAV.DAT in DATBOX')
!!        call faterr(IAM,cout)
!!     end if
!!
!!     raycd   = get_radius_SPHER(t2t%icdtac)
!!     rayan   = get_radius_SPHER(t2t%iantac)
!!
!!     t2t%f2f(1)%ctc(1)%inter%area = PI_g *   (raycd*rayan)*(raycd*rayan)  &
!!                                           / ( (raycd+rayan)*(raycd+rayan) )
!!
!!   end if
!!         
!!   !write(cout,'(1X,I10,A12)') nb_SPSPx,' SPSPx found'
!!   !call logmes(cout)
!!
!!   !nb_SPHER = get_nb_SPHER()
!!   !do ibdy=1,nb_SPHER
!!
!!   !   if (associated(adjac(ibdy)%icdan))  deallocate(adjac(ibdy)%icdan)
!!
!!   !   if (nb_adj(ibdy) /= 0) then
!!   !      allocate(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
!!   !      if (errare /=0 ) then
!!   !         call FATERR(IAM,'error in allocating adjac(icdbdy)%.....')
!!   !      end if
!!   !   else 
!!   !      nullify(adjac(ibdy)%icdan)
!!   !   end if
!!
!!   !end do
!!   !
!!   !do icdan=1,nb_SPSPx
!!   !   adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
!!   !end do
!!
!! end subroutine compute_contacts_in_t2t_SPSPx
 
 subroutine set_nb_SPSPx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_SPSPx = nb

 end subroutine

 subroutine redo_nb_adj_SPSPx()
   implicit none

   if (allocated(SPcoor)) deallocate(SPcoor)
   allocate( SPcoor( 3, get_nb_SPHER() ) )
   call redo_nb_adj_( get_nb_SPHER() )

 end subroutine

END MODULE SPSPx

