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
MODULE SPPRx

  USE overall
  USE utilities
  USE tact_behaviour
  USE SPHER
  USE POLYR
  USE DiscreteGeometry

  USE ExternalDetection

  use algebra    

  use MAILx, only : get_color_MAILx
  use RBDY3, only : get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color

  USE parameters, only : i_spprx, i_mailx, i_rbdy3, i_mbs3

  !am : modules utilises par les fonctions gerant la DDM
  use anonymous_ptr_container, only : get_object               => get_data                   , &
                                      get_nb_objects           => get_nb_data                , &
                                      close_container          => close_ptr_container        , &
                                      add_object_to_container  => add_object_to_ptr_container, &
                                      display_object_container => display_ptr_container      , &
                                      ptr_container
  USE anonymous

  use inter_meca_3D

  implicit none

  private

  CHARACTER(len=5) :: BOBO='SPPRx'
  INTEGER          :: nb_SPHER=0,nb_POLYR=0

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 ! nb_SPPRx = number of selected candidates SPHER against POLYR
 INTEGER,PRIVATE :: nb_SPPRx=0 ,nb_vSPPRx=0 ,nb_recup_SPPRx=0                         

!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj ! nb_adj(icdtac): number of adjacent pairs POLYR-POLYR
                                                        ! to candidate contactor POLYR icdtac.

!------------------------------------------------------------------------ 


 type(T_verlet), dimension(:), allocatable, target ::verlt

 integer, dimension(:,:), allocatable :: this2verlet

!------------------------------------------------------------------------ 

 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_SPPRx.

   INTEGER                               :: pr_popul  ! box(ibox1,ibox2)%pr_popul: number of disks in box ibox1,ibox2;
   INTEGER, DIMENSION(:), POINTER        :: pr_which  ! box(ibox1,ibox2)%pr_which(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;

   ! SPPRx
   INTEGER                               :: sp_popul  ! box(ibox1,ibox2)%SPpopul: number of disks in box ibox1,ibox2;
   INTEGER, DIMENSION(:), POINTER        :: sp_which  ! box(ibox1,ibox2)%SPwhich(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 END TYPE T_box 

 TYPE(T_box), DIMENSION(:,:,:),ALLOCATABLE  :: box    ! box(ibox1,ibox2,ibox3): box with integer coordinates ibox1,ibox2,ibox3.



!------------------------------------------------------------------------

 TYPE T_rough_SPPRx                                   ! définit le type de la liste des plus proches voisins

   ! le candidat, l'antagoniste et isee pour la loi de contact 
   INTEGER                   :: cd                                  
   INTEGER                   :: an
   INTEGER                   :: isee
   REAL(kind=8),DIMENSION(3) :: Vsep

   INTEGER      :: xperiodic,yperiodic

   !am: group to which belong an interaction. For ddm group could be
   !   - INTRF: for a body belonging to an interface between two sub-domains
   !   - NOINT: for a body living inside a sub-domain
   integer :: group
 END TYPE T_rough_SPPRx

 TYPE(T_rough_SPPRx),DIMENSION(:),ALLOCATABLE   :: rough_SPPRx        ! table  de visibilité
 INTEGER                                        :: nb_rough_SPPRx     ! nombre de paire de polyedre a analyser pour determiner
                                                                      ! s'il y a contact ou pas
 TYPE T_link_rough_SPPRx                                           ! liste chainee pour determiner les listes de cand_ant car
                                                                   ! on ne connait pas a priori le nb de cand-ant 
    TYPE(T_link_rough_SPPRx), POINTER :: p                         ! pointeur sur le precedent
    TYPE(T_rough_SPPRx)               :: val                       ! les valeurs
    TYPE(T_link_rough_SPPRx), POINTER :: n                         ! pointeur sur le suivant

 END TYPE T_link_rough_SPPRx

 TYPE(T_link_rough_SPPRx),POINTER                  :: Root,Current,Previous
 
!------------------------------------------------------------------------
! variables attached to surrounding boxes

 REAL (kind=8)  :: maxray, minray, maxalert, meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,minibox3,maxibox3,pr_maxpopul,sp_maxpopul
!------------------------------------------------------------------------

!------------------------------------------------------------------------
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PRcoor
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: SPcoor
 REAL(kind=8)                                    :: Reac_SPPRx_MAX=0.D0
 INTEGER,PRIVATE                                 :: ii,l_ii,iv,restart=0,prox_tactors_to_file=0,prox_tactors_from_file=0
 INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
 LOGICAL,PRIVATE                                 :: write_creation_tab_visu
 real(kind=8), dimension(:), allocatable, target :: violation

!!!---------------------------------------------------------
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(KIND=8) :: XPERIODE = 0.D0,YPERIODE = 0.D0
  REAL(KIND=8),dimension(3) :: perio_shift ! vector containing the translation of antagonist body when
                                           ! using periodic conditions 
!!!---------------------------------------------------------

 real(kind=8) :: tol_recup_rloc = 1.d-6

 logical      :: module_checked_ = .FALSE.
 logical      :: check_SPPRx_    = .FALSE.

 ! fd fix for bug in recup at restart
 logical, public :: verlet_from_file = .TRUE.

 PUBLIC &
       coor_prediction_SPPRx,&
       CHECK_SPPRx,&
       RUN_SPPRx, &
       get_write_Vloc_Rloc_SPPRx, &
       read_ini_Vloc_Rloc_SPPRx,&
       write_xxx_Vloc_Rloc_SPPRx,&
       stock_rloc_SPPRx, &
       recup_rloc_SPPRx, &
       compute_box_SPPRx, &
       creation_tab_visu_SPPRx, &
       compute_contact_SPPRx, &
       display_prox_tactors_SPPRx,&
       get_nb_SPPRx,&
       set_xperiodic_data_SPPRx, &
       set_yperiodic_data_SPPRx, &
       set_tol_recup_rloc_SPPRx, &
       print_info_SPPRx

! liste des fonctions publiques 
!
 PUBLIC &
      nullify_reac_SPPRx      ,&
      nullify_vlocy_SPPRx     ,&
      injj_SPPRx              ,&
      prjj_SPPRx              ,&
      vitrad_SPPRx            ,& 
      SPPRx2ENT               ,&
      SPPRx2POLYR             ,&
      get_surf_SPPRx

  public clean_memory_SPPRx

  !rm for handler
  public get_this             , &
         set_nb_SPPRx         , &
         redo_nb_adj_SPPRx    , &
         get_an_tacty         , &
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
  SUBROUTINE compute_box_SPPRx
    IMPLICIT NONE

    INTEGER                     :: isee,errare,ibdy
    REAL(kind=8)                :: ksi,eta

                             !123456789012345678
    character(len=18) :: IAM='SPPRx::compute_box' 


    nb_POLYR = get_nb_POLYR()
    nb_SPHER = get_nb_SPHER()
  
    ! on ne fait ici que les choses qui changent lorsque nb_POLYR change

    minray     = min(get_min_radius_POLYR(),get_min_radius_SPHER())
    maxray     = max(get_max_radius_POLYR(),get_max_radius_SPHER())

    IF (minray > maxray ) CALL FATERR(IAM,'issue computing minray and maxray')

    IF (minray == 0.d0 ) CALL FATERR(IAM,' minray can t be equal to zero')

    ! computing largest alert distance between disks 
    maxalert=0.D0  
    DO isee=1,SIZE(see)
      IF ((see(isee)%cdtac == 'SPHER' .AND. see(isee)%antac == 'POLYR')) THEN
        maxalert=MAX(maxalert,see(isee)%alert)
      END IF
    END DO

    Lbox   = 1.05D0*(2.D0*maxray + maxalert)

    norm   = Lbox/get_min_radius_POLYR()

    Lbox_1 = 1.D0/Lbox
   
    minibox1= 1
    minibox2= 1
    minibox3= 1
  
    pr_maxpopul = (1+INT(norm))*(1+INT(norm))*(1+INT(norm))

    if (pr_maxpopul < 0) pr_maxpopul = nb_POLYR

    
    norm   = Lbox/get_min_radius_SPHER()

    Lbox_1 = 1.D0/Lbox
   
    minibox1= 1
    minibox2= 1
    minibox3= 1
  
    sp_maxpopul = (1+INT(norm))*(1+INT(norm))*(1+INT(norm))

    !fd le 16/12/07 au cas ou maxpopul passe en neg car c'est un i4 et que
    !fd sur des grands domaines ca ne suffise pas:

    if (sp_maxpopul < 0) sp_maxpopul = nb_SPHER
        
    ! print*,"sp_maxpopul= ",sp_maxpopul,"pr_maxpopul= ",pr_maxpopul

    IF (.NOT. ALLOCATED(adjac)) THEN
      ALLOCATE(adjac(nb_SPHER),stat=errare)
      IF (errare /=0 ) CALL FATERR(IAM,'error in allocating adjac')

      DO ibdy=1,nb_SPHER
        NULLIFY(adjac(ibdy)%icdan)
      END DO

    ELSE
      DO ibdy=1,nb_SPHER
        IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
        NULLIFY(adjac(ibdy)%icdan)
      ENDDO
    ENDIF  
  
    IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
    ALLOCATE(nb_adj(nb_SPHER),stat=errare)
    IF (errare /=0 ) CALL FATERR(IAM,' error allocating nb_adj')

    nb_adj=0

    ! PRcoor are coordinates of bodies to be used in selecting prox tactors

    IF (ALLOCATED(PRcoor)) DEALLOCATE(PRcoor)
    ALLOCATE(PRcoor(3,nb_POLYR),stat=errare)

    IF (ALLOCATED(SPcoor)) DEALLOCATE(SPcoor)
    ALLOCATE(SPcoor(3,nb_SPHER),stat=errare)

  END SUBROUTINE compute_box_SPPRx
  !------------------------------------------------------------------------ 

  !---------------------------------------------------------------------------
  ! Subroutine pour actualiser les positions des vertex des polyedres au cours du temps
  SUBROUTINE coor_prediction_SPPRx

    IMPLICIT NONE  
    INTEGER :: itacty 
    INTEGER :: errare 

                             !1234567890123456789012 
    character(len=22) :: IAM='SPPRx::coor_prediction'
       
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

    call move_polyr

    IF (smooth_method) THEN
      DO itacty=1,nb_POLYR
        PRcoor(1:3,itacty) = get_coor_POLYR(itacty)
      END DO
    ELSE
      DO itacty=1,nb_POLYR
        PRcoor(1:3,itacty) =0.d0
       
        IF (.NOT. get_visible_POLYR(itacty)) CYCLE
       
        PRcoor(1:3,itacty) = get_coorTT_POLYR(itacty)

        IF ( XPERIODIC ) THEN
          IF ( PRcoor(1,itacty) < 0.D0 .or. PRcoor(1,itacty)  > xperiode ) call faterr(IAM,'x perio not verified') 

          !fd a virer il me semble
          IF ( PRcoor(1,itacty)  > xperiode ) THEN
            PRcoor(1,itacty) = PRcoor(1,itacty) - xperiode
            !fd dbg
            !print*,'X- ',itacty
            call faterr(IAM,' X-')          
          ELSE IF( PRcoor(1,itacty) < 0.D0 ) THEN
            PRcoor(1,itacty) = PRcoor(1,itacty) + xperiode
            !fd dbg
            !print*,'X+ ',itacty
            call faterr(IAM,' X+')          
          END IF
        END IF

        IF ( YPERIODIC ) THEN

          if (PRcoor(2,itacty) < 0.D0 .or. PRcoor(2,itacty)  > yperiode ) call faterr(IAM,'y perio not verified') 

          !fd a virer il me semble
          IF ( PRcoor(2,itacty)  > yperiode ) THEN
            PRcoor(2,itacty) = PRcoor(2,itacty) - yperiode
            !fd dbg
            !print*,'Y- ',itacty
            call faterr(IAM,' Y-')
          ELSE IF ( PRcoor(2,itacty) < 0.D0 ) THEN
            PRcoor(2,itacty) = PRcoor(2,itacty) + yperiode
            !fd dbg
            !print*,'Y+ ',itacty
            call faterr(IAM,' Y+')           
          END IF
        END IF
      END DO
    END IF
           

  END SUBROUTINE coor_prediction_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_SPPRx(step)
  implicit none
  integer(kind=4), intent(in) :: step
  !
  integer(kind=4) :: nb_read
  
  G_nfich=get_io_unit()

  if(step == 0) then
    open(unit=G_nfich,file=trim(location(in_Vloc_Rloc(:))))
  else if(step > 0) then
    open(unit=G_nfich,file=trim(location(out_Vloc_Rloc(:))))
  else
    open(unit=G_nfich,file=trim(location(last_Vloc_Rloc(:))))
  end if

  call read_ini_Vloc_Rloc(nb_read)
  close(G_nfich)
  
  end subroutine read_ini_Vloc_Rloc_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_SPPRx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_SPPRx
  !------------------------------------------------------------------------------------------------

  !------------------------------------------------------------------------------------------------
  SUBROUTINE display_prox_tactors_SPPRx

   IMPLICIT NONE

   INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee
   character(len=5) :: cdmodel,anmodel

   DO icdtac=1,get_nb_SPHER()
     DO iadj=1,nb_adj(icdtac)         
       icdan  = adjac(icdtac)%icdan(iadj)
       icdbdy = this(icdan)%icdbdy
       ianbdy = this(icdan)%ianbdy
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,icdbdy,'SPHER',icdtac,see(this(icdan)%isee)%behav,  &
       anmodel,ianbdy,'POLYR',iantac
                    
       WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
       WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
       WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
       WRITE(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
       WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
       WRITE(*,104) 'PTCx=',this(icdan)%coor(1),'PTCy=',this(icdan)%coor(2),'PTCz=',this(icdan)%coor(3)
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(27X,3(2X,A5,D14.7))
   
  END SUBROUTINE display_prox_tactors_SPPRx
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE stock_rloc_SPPRx
  !
  ! get data from this and put into verlt
  !            
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare

   character(len=80) :: cout
                            !12345678901234567 
   character(len=17) :: IAM='SPPRx::stock_rloc'

   nb_SPHER=get_nb_SPHER()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_SPHER),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,' error allocating verlt')
     END IF
     DO icdtac=1,nb_SPHER
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0
         
       ELSE

         call nullify_verlet_(icdtac) 
           
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_SPHER
       verlt(icdtac)%adjsz=0

       call free_verlet_(icdtac)

       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

         verlt(icdtac)%id_f_cd = 0
         verlt(icdtac)%id_f_an = 0
         
       ELSE

         call free_verlet_(icdtac)  
           
       END IF
     END DO
   END IF

   ! filling data:
   DO icdan=1,nb_SPPRx

     !   PRINT*,'yyy'
     !   PRINT*,icdan,this(icdan)%status
     !   PRINT*,this(icdan)%vls,this(icdan)%vlt,this(icdan)%vln
     !   PRINT*,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln
     !   PRINT*,this(icdan)%gapTT


     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan 
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor 
     iadj   = this(icdan)%iadj
     ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)    = icdan

     verlt(icdtac)%cdbdy          = spher2bdyty(1,icdtac)
     verlt(icdtac)%cdtac          = spher2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel        = spher2bdyty(3,icdtac)
     verlt(icdtac)%anbdy(iadj)    = polyr2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)    = polyr2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)  = polyr2bdyty(3,iantac)
     
     !fd @@@ il manque clairement des choses pour retrouver les faces ....

     verlt(icdtac)%cdsci(iadj)    = 0
     verlt(icdtac)%ansci(iadj)    = 0

     verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
     verlt(icdtac)%rls(iadj)      = this(icdan)%rls/H

     verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)      = this(icdan)%vln
     verlt(icdtac)%vls(iadj)      = this(icdan)%vls

     verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)   = this(icdan)%status
     verlt(icdtac)%coor(1:3,iadj) = this(icdan)%coor(1:3)
     verlt(icdtac)%tuc(:,iadj)    = this(icdan)%tuc(:)
     verlt(icdtac)%nuc(:,iadj)    = this(icdan)%nuc(:)
     verlt(icdtac)%suc(:,iadj)    = this(icdan)%suc(:)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

     verlt(icdtac)%icdcoor(:,iadj)   = this(icdan)%icdcoor(:)
     verlt(icdtac)%iancoor(:,iadj)   = this(icdan)%iancoor(:)

     !fd dbg
     !print*, icdtac, iantac, iadj
     !print*, verlt(icdtac)%icdcoor(:,iadj)
     !print*, verlt(icdtac)%iancoor(:,iadj)
     
   END DO
   
   nb_vSPPRx = nb_SPPRx

   WRITE(cout,'(1X,I10,A12)') nb_vSPPRx,' stock SPPRx'
   call logmes(cout)

   verlet_from_file = .FALSE.

  END SUBROUTINE stock_rloc_SPPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_SPPRx
   !
   ! get data from Verlet list verlt and put into this
   !                                         
   IMPLICIT NONE

   INTEGER     :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   REAL(kind=8),DIMENSION(3) :: sep
   REAL(kind=8) :: val   
   logical:: is_found
   character(len=80) :: cout
                            !12345678901234567
   character(len=17) :: IAM='SPPRx::recup_rloc'

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if
   nb_recup_SPPRx=0

   !fd cette merde suppose que le nombre de POLYR n'a pas change 
   !fd par contre on pourrait avoir swappe cd et an ?

   DO icdan=1,nb_SPPRx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%rls=0.D0
     this(icdan)%statusBEGIN=i_nknow
     ! serial number of candidate contactor for contact icdan     
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan             
     iantac = this(icdan)%iantac 

     !print*,'recherche par cd'
     is_found=.false.
     IF (verlt(icdtac)%adjsz /= 0) THEN

       if (verlt(icdtac)%cdbdy  == spher2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == spher2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== spher2bdyty(3,icdtac) ) then

         !print*,' par cd possible'
         !print*,verlt(icdtac)%adjsz

         do iadj = 1, verlt(icdtac)%adjsz
           !print*,'verlet ref global adjacent: ',iadj
           !print*,verlt(icdtac)%anmodel(iadj), &
           !       verlt(icdtac)%anbdy(iadj),  &
           !       verlt(icdtac)%antac(iadj)

           !print*,'faces en vis a vis cd et an'
           !print*,verlt(icdtac)%id_f_cd(iadj),verlt(icdtac)%id_f_an(iadj)

           if (verlt(icdtac)%anbdy(iadj)  == polyr2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == polyr2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== polyr2bdyty(3,iantac) ) then

             !print*,'an possible'

               if ( verlet_from_file ) then
                 !test par la position du point de contact (cdcoor+ancoor)/2   
              
                 !fd @@@ c'est pas mal comme facon de retrouver les points ... mais faudrait etre
                 !fd @@@ plus precis, la norme devrait dependre de la taille des particules ...
               
                 sep=verlt(icdtac)%coor(1:3,iadj)-this(icdan)%coor(1:3)
                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 

               else
                 !test par la position du point candidat  

                 sep = verlt(icdtac)%icdcoor(1:3,iadj)-this(icdan)%icdcoor(1:3)
                 val = (sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)) 
                
               endif
              
               !print*,'icdan',icdan              
               !print*,icdtac,iantac,val

               IF (val < tol_recup_rloc .or. &
                   (xperiodic .and. dabs(xperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. &
                   (yperiodic .and. dabs(yperiode-dsqrt(val))< dsqrt(tol_recup_rloc)) .or. & 
                   (xperiodic .and. yperiodic .and. dabs(dsqrt(xperiode**2+yperiode**2)-dsqrt(val))< dsqrt(tol_recup_rloc)) &
                  ) THEN
                 nb_recup_SPPRx = nb_recup_SPPRx+1
                 this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
                 this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
                 this(icdan)%rls    = verlt(icdtac)%rls(iadj)*H
                 this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
                 this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                 is_found=.true.
                 EXIT
               ENDIF
           ENDIF
         END DO
       ENDIF
     ENDIF

     if (is_found) cycle

   END DO
  
   WRITE(cout,'(1X,I10,A12)') nb_recup_SPPRx,' recup SPPRx'
   call logmes(cout)

  END SUBROUTINE recup_rloc_SPPRx
  !------------------------------------------------------------------------
  
  !------------------------------------------------------------------------ 
  SUBROUTINE read_ini_Vloc_Rloc(nb_read) 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   IMPLICIT NONE

   CHARACTER(len=103)               :: clin
   INTEGER                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   REAL(kind=8)                     :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
   CHARACTER(len=5)                 :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER                          :: errare


   INTEGER :: ibehav,nb_internal,i_internal
   INTEGER :: icdtact,nb_read
   CHARACTER(len=103) :: cout
                               !1234567890123456789012345
   CHARACTER(len=25)  :: IAM = 'SPPRx::read_ini_Vloc_Rloc'

   integer:: cdmodel,anmodel,j,k

   errare=0
   nb_read=0
   nb_SPHER=get_nb_SPHER()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_SPHER),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,' error allocating nb_adj')
   END IF

   DO icdtac=1,nb_SPHER
     nb_adj(icdtac)=0
   END DO

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'SPPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5, 2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,                                          &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                          &
          sttus
 
     cdmodel = get_body_model_id_from_name( cdbdy )

     IF (cdtac == 'SPHER' .AND. antac == 'POLYR') THEN
       do icdtact = 1, nb_POLYR
         if (spher2bdyty(1,icdtact) == icdbdy .and. &
             spher2bdyty(2,icdtact) == icdtac .and. &
             spher2bdyty(3,icdtact) == cdmodel ) then
           nb_adj(icdtact) = nb_adj(icdtact) + 1
           exit
         end if
       end do
     END IF
   END DO

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_SPHER),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,' error allocating verlt')
     END IF
     DO icdtac=1,nb_SPHER
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)

         IF (errare /=0 ) THEN
          call faterr(IAM,'error in allocating verlt(icdtac)%.....')
         END IF
         
       ELSE

         call nullify_verlet_(icdtac)
           
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_SPHER
         
       call free_verlet_(icdtac)    

       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj

         call new_verlet_(icdtac, iadj, errare)
         
         IF (errare /=0 ) THEN
           call faterr(IAM,' error in allocating verlt(icdtac)%....')
         END IF

       ELSE

         call nullify_verlet_(icdtac)

       END IF
     END DO
   END IF    
  ! second reading: filling data
   REWIND(G_nfich)
   DO icdtac=1,nb_SPHER
     nb_adj(icdtac)=0
   END DO
   icdan=0
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'SPPRx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
          cdbdy,icdbdy,cdtac,icdtac,                                   &
          behav,                                                              &
          anbdy,ianbdy,antac,iantac,                                                 &
          sttus

     IF (cdtac == 'SPHER' .AND. antac == 'POLYR') THEN

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       do icdtact = 1, nb_POLYR
         IF (spher2bdyty(1,icdtact) == icdbdy .and. &
             spher2bdyty(2,icdtact) == icdtac .and. &
             spher2bdyty(3,icdtact) == cdmodel ) then

           nb_read=nb_read+1

           nb_adj(icdtact)=nb_adj(icdtact)+1

           verlt(icdtact)%icdan( nb_adj(icdtact) ) = nb_read

           verlt(icdtact)%cdmodel = cdmodel
           verlt(icdtact)%cdbdy   = icdbdy
           verlt(icdtact)%cdtac   = icdtac
 
           verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
           verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
           verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
           verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)


           IF( .NOT. read_G_clin()) EXIT
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') rlt,rln,rls
           verlt(icdtact)%rlt(nb_adj(icdtact)) = rlt
           verlt(icdtact)%rln(nb_adj(icdtact)) = rln
           verlt(icdtact)%rls(nb_adj(icdtact)) = rls
           
           IF( .NOT. read_G_clin()) CYCLE
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') vlt,vln,vls
           verlt(icdtact)%vlt(nb_adj(icdtact)) = vlt
           verlt(icdtact)%vln(nb_adj(icdtact)) = vln
           verlt(icdtact)%vls(nb_adj(icdtact)) = vls
           
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
           END IF

           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 'n(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%nuc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%nuc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%nuc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF
          
           IF( .NOT. read_G_clin()) CYCLE
           IF (G_clin(30:34)== 's(1)=') THEN
             READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
             verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
             verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
             verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
           ELSE 
             BACKSPACE(G_nfich)
           END IF

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
     ENDIF
   ENDDO
 
   nb_vSPPRx=0
    
   DO icdtact=1,nb_SPHER
     nb_vSPPRx = nb_vSPPRx + nb_adj(icdtact)
       
     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
            'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
       CALL FATERR(IAM,cout)
     END IF
   END DO

   !xxxx

   if (allocated(this2verlet)) deallocate(this2verlet)
   allocate(this2verlet(2,nb_vSPPRx))

   k=0
   DO icdtact=1,nb_SPHER
     do j=1,nb_adj(icdtact)
       k=k+1
       this2verlet(1,k) = icdtact
       this2verlet(2,k) = j
     enddo 
   enddo 

104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
  END SUBROUTINE read_ini_Vloc_Rloc
  !------------------------------------------------------------------------   

  !------------------------------------------------------------------------   
  SUBROUTINE write_out_Vloc_Rloc(nfich)
   !
   ! write into file out_Vloc_Rloc data from this, in verlt style
   !
   IMPLICIT NONE

   INTEGER :: iadj,icdtact
   INTEGER :: nfich,icdan,icdtac,iantac
   character(len=5) :: cdmodel,anmodel

   character(len=20) :: fmt
   
   nb_SPHER=get_nb_SPHER()

   IF (nb_SPPRx==0) RETURN

   DO icdtact=1,nb_SPHER    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac

         cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( polyr2bdyty(3,iantac) )

         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','SPPRx',icdan  
         !1234567890123456789012345678901234567890123456789012345678901234567890124567
         WRITE(nfich,'(A69)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus'
         
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
              cdmodel,get_visibleID_SPHER(icdtac),'SPHER',spher2bdyty(2,icdtac), &
              see(this(icdan)%isee)%behav,  &
              anmodel,get_visibleID_POLYR(iantac),'POLYR',polyr2bdyty(2,iantac), &
              get_contact_status_name_from_id(this(icdan)%status)
         WRITE(nfich,104) 'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls/H',this(icdan)%rls/H
         WRITE(nfich,104) 'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',this(icdan)%vls  
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
  SUBROUTINE nullify_reac_SPPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in):: icdan 
   INTEGER           :: storage
    
   CALL nullify_reac_SPHER(this(icdan)%icdbdy,storage)
   
   CALL nullify_reac_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_reac_SPPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_SPPRx(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
    
   CALL nullify_vlocy_SPHER(this(icdan)%icdbdy,storage)
   
   CALL nullify_vlocy_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_vlocy_SPPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  SUBROUTINE vitrad_SPPRx( icdan, storage, need_full_vlocy )

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
    
   CALL comp_vlocy_SPHER(this(icdan)%icdbdy,storage)
    
   CALL comp_vlocy_POLYR(this(icdan)%iantac,storage)
    
  END SUBROUTINE vitrad_SPPRx
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE injj_SPPRx(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: ccdof = (/ 1,2,3,4,5,6 /)
   REAL(kind=8),DIMENSION(6)  :: cdreac, anreac

   INTEGER                    :: storage

   cdreac(1)  = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   cdreac(2)  = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   cdreac(3)  = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
   cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
   cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK

   anreac(1)  = -cdreac(1)
   anreac(2)  = -cdreac(2)
   anreac(3)  = -cdreac(3)
   anreac(4) =-this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
   anreac(5) =-this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
   anreac(6) =-this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK


   CALL add_reac_SPHER(this(icdan)%icdbdy,ccdof,cdreac,storage)

   CALL add_reac_POLYR(this(icdan)%iantac,ccdof,anreac,storage)

  END SUBROUTINE injj_SPPRx 
  !------------------------------------------------------------------------  

  !------------------------------------------------------------------------  
  SUBROUTINE prjj_SPPRx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van

   Vcd = get_vlocy_SPHER(this(icdan)%icdbdy,storage)
   Van = get_vlocy_POLYR(this(icdan)%iantac,storage)      

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

  END SUBROUTINE prjj_SPPRx 
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_SPPRx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_SPPRx = nb_SPPRx
    CASE(i_verlet_tactor)
       get_nb_SPPRx = nb_vSPPRx
    CASE(i_rough_tactor)
       get_nb_SPPRx = nb_rough_SPPRx
    CASE(i_recup_tactor)
       get_nb_SPPRx = nb_recup_SPPRx
    END SELECT

  END FUNCTION get_nb_SPPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  SUBROUTINE SPPRx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_SPHER(this(icdan)%icdbdy)
   ianent = get_ENT_POLYR(this(icdan)%iantac)

  END SUBROUTINE SPPRx2ENT
  !------------------------------------------------------------------------ 
  !------------------------------------------------------------------------ 
  SUBROUTINE SPPRx2SPHER(icdan,icdtac)

    IMPLICIT NONE
    INTEGER :: icdan,icdtac
    
    icdtac = this(icdan)%icdtac
    
  END SUBROUTINE SPPRx2SPHER
  !------------------------------------------------------------------------ 
  SUBROUTINE SPPRx2POLYR(icdan,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,iantac
   
   iantac = this(icdan)%iantac

  END SUBROUTINE SPPRx2POLYR
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------ 
  REAL(kind=8) function get_surf_SPPRx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan

   get_surf_SPPRx = this(icdan)%area

  END function get_surf_SPPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------
  LOGICAL FUNCTION RUN_SPPRx()

    IMPLICIT NONE
    
    RUN_SPPRx = RUN_TACTOR

  END FUNCTION RUN_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  logical function CHECK_SPPRx()
  implicit none
  !   
  integer :: isee

  ! if check already made just return result
  if( module_checked_ ) then
    CHECK_SPPRx = check_SPPRx_
    return
  end if

  con_pedigree%module_name = 'SPPRx'

  con_pedigree%id_cdan  = i_spprx
  con_pedigree%id_cdtac = i_spher
  con_pedigree%id_antac = i_polyr

  cdtact2bdyty => spher2bdyty
  antact2bdyty => polyr2bdyty

  ! check only once if module may be used
  module_checked_ = .TRUE.

  ! checking if enough cd/an
  nb_SPHER = get_nb_SPHER()
  nb_POLYR = get_nb_POLYR()
  if( nb_SPHER < 1 .or.  nb_POLYR < 1 ) then
    CHECK_SPPRx = check_SPPRx_ ! still false
    return
  end if
  
  ! checking if any seetable with the good cd/an type
  do isee = 1, size(see)
    if (see(isee)%cdtac == 'SPHER' .and. see(isee)%antac == 'POLYR') then
      check_SPPRx_ = .true.
      exit
    end if
  end do

  CHECK_SPPRx = check_SPPRx_
  return

  end function CHECK_SPPRx
  !------------------------------------------------------------------------ 

  !------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_SPPRx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_SPPRx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_SPPRx
  !-----------------------------------------------------------------------

  !-----------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_SPPRx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    xperiode  = per
    XPERIODIC = FLAG
    
  END SUBROUTINE set_xperiodic_data_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_SPPRx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    yperiode  = per
    YPERIODIC = FLAG
    
  END SUBROUTINE set_yperiodic_data_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  !--- routines de pre-detection ----
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_SPPRx

   IMPLICIT NONE

   INTEGER                               :: errare,icdtac,iantac,isee,itacty,i,k
   INTEGER                               :: icdan,iadj,itac
   CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
   REAL(kind=8),DIMENSION(3)             :: sep,axe,point
   REAL(kind=8)                          :: dist,norm1,dist1,norm2,dist2,nonuc,gap,rayan,raycd,adist
   REAL(kind=8),DIMENSION(3)             :: coordcd,coordan
   INTEGER                               :: ibox1,ibox2,ibox3
   INTEGER                               :: ibox1cd,ibox2cd,ibox3cd,size_of_this
   INTEGER                               :: ibox1an,ibox2an,ibox3an,icdpop,ianpop
   REAL(kind=8)                          :: Xleft,Xright,Yleft,Yright,Zup,Zdown

                              !123456789012345678901234
   CHARACTER(len=24) :: IAM = 'SPPRx::creation_tab_visu'
   character(len=120):: cout

   logical :: bavard=.false.

  ! Since the list of proximate contactors may not be updated at every time step,
  ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
  ! A warning condition prevents undue deallocation. 

   ! Building boxes for quick sorting 
   
   ! Computing maximal boundary radius of disks and largest box containing disks.
   !
   ! The computation of maximal radius of disks allows to size a grid of boxes,
   ! the purpose of which is quick sorting of candidates to contact. The following
   ! sorting method is reasonnably fast. It is not really efficacious when the 
   ! ratio between maximal and minimal radius is too large (>10), since the box 
   ! size becomes too large. Ratio less than 3 or even 5 are fair.
   ! The minimum radius of disks is also used, in order to estimate the maximal 
   ! number of disks per box.   
   ! This quick sorting method may be applied to bodies other than disks, such as 
   ! ellipsoidal or polygonal bodies with a reasonable aspect ratio, less than 
   ! 3 or even 5. Such bodies are enclosed in disks with radius max_radius, and 
   ! enclosing a disk with radius min_radius. The sorting algorithm may then be 
   ! straightforwardly applied.
   ! In the case of disks, max_radius and min_radius should be merely bry_radius. 
   ! Since the data file allows, for further generalization purposes, other 
   ! contactors than BDARY (the true boundary), extracting min_radius and 
   ! max_radius as bry_radius might seem to be somewhat tortuous, though simple.


   !boxes are shared
   
   Xleft   =  1.D24
   Xright  = -1.D24
   Yleft   =  1.D24
   Yright  = -1.D24
   Zdown   =  1.D24
   Zup     = -1.D24

   DO itac=1,nb_SPHER

     IF (.NOT. get_visible_SPHER(itac)) CYCLE

     coordcd = SPcoor(1:3,itac)
     Xleft = MIN(coordcd(1),Xleft )
     Xright= MAX(coordcd(1),Xright)
     Yleft = MIN(coordcd(2),Yleft )
     Yright= MAX(coordcd(2),Yright)
     Zdown = MIN(coordcd(3),Zdown )
     Zup   = MAX(coordcd(3),Zup   )

   END DO
  
   DO itac=1,nb_POLYR

     IF (.NOT. get_visible_POLYR(itac)) CYCLE

     !cycling if tactor is a big polyr
     if (nb_big_polyr /= 0) then
        if ( any(big_polyr == itac) ) cycle
     endif   

     coordcd = PRcoor(1:3,itac)
     Xleft = MIN(coordcd(1),Xleft )
     Xright= MAX(coordcd(1),Xright)
     Yleft = MIN(coordcd(2),Yleft )
     Yright= MAX(coordcd(2),Yright)
     Zdown = MIN(coordcd(3),Zdown )
     Zup   = MAX(coordcd(3),Zup   )

   END DO

   IF (XPERIODIC) THEN
     IF (Xright>xperiode) THEN
       write(cout,*) 'The max right coordinate ', Xright, ' is greater than the periode'
       CALL FATERR(IAM,cout)
     END IF

     IF (Xleft<0.D0) THEN
       write(cout,*) 'The min left coordinate ', Xleft, ' is less than zero'
       CALL FATERR(IAM,cout)
     END IF

     Xright = xperiode
     Xleft  = 0.D0

     if (bavard) print*,'-------------XPERIODIC--------------------'

   END IF

   IF (YPERIODIC) THEN
     IF (Yright>yperiode) THEN
       write(cout,*) 'The max right coordinate ', Yright, ' is greater than the periode'
       CALL FATERR(IAM,cout)
     END IF

     IF (Yleft<0.D0) THEN
       write(cout,*) 'The min left coordinate ', Yleft, ' is less than zero'
       CALL FATERR(IAM,cout)
     END IF

     Yright = yperiode
     Yleft  = 0.D0

     if (bavard) print*,'-------------YPERIODIC--------------------'
   END IF

   ! if (bavard) then
     ! print*,'X- ',Xleft,' X+ ',Xright
     ! print*,'Y- ',Yleft,' Y+ ',Yright
     ! print*,'Z- ',Zdown,' Z+ ',Zup
   ! endif

   
   !
   ! Box size is defined to be largest (1%) than maxray+maxalert so as to
   ! ensure that all contacts may be detected.
   !
   ! Lbox1=0.101D+01*(maxray+0.5D0*maxalert)
   ! Lbox2=0.101D+01*0.5D0*dsqrt(2.D0)*(maxray+minray+maxalert)
   ! Lbox=max(Lbox1,Lbox2)
   !
   ! A box is located by pairs of integer numbers (ibox1,ibox2), where 
   ! ibox1 is the column number of the box, ibox2 the layer number of the box.
   ! 
   !
   !      ____ ____ ____ ____ ____ 
   !     |    |    |    |    |    |
   !  3  |    |    |    |    |    |   
   !     |____|____|____|____|____| 
   !     |    |    |    |    |    |
   !  2  |    |    |    |    |    |    ibox2
   !     |____|____|____|____|____| 
   !     |    |    |    |    |    |     
   !  1  |    |    |    |    |    |    
   !     |____|____|____|____|____| 
   !     
   !       1    2    3    4    5
   !
   !             ibox1
   !
   ! Coordinates of lower left and upper right corners of a (ibox1,ibox2) box are:
   !
   ! lowerleft  = ( Bleft + (ibox1-1)*Lbox , Bleft + (ibox2-1)*Lbox )
   ! upperright = ( Bdown +  ibox2   *Lbox , Bdown +  ibox2   *Lbox
   !
   ! An oversize covering of the big box (Bright,Bdown,Bleft,Bup) containing all disks 
   ! is the collection of elementary boxes such that, 
   !
   ! -1 .le. ibox1 .le. 1 + AINT((Bleft-Bright)/Lbox) ,
   ! -1 .le. ibox2 .le. 1 + AINT((Bup  -Bdown )/Lbox) . 
   !  
   
   maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
   maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
   maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)

  ! print*,"==========================="
  ! print*,Xright,Xleft,Lbox_1,maxibox1
  ! print*,Yright,Yleft,Lbox_1,maxibox2
  ! print*,Zup,Zdown,Lbox_1,maxibox3
  ! print*,"==========================="

   ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
   IF (errare /=0 ) call FATERR(IAM,'error allocating box')

   DO ibox3=minibox3,maxibox3
   DO ibox2=minibox2,maxibox2
   DO ibox1=minibox1,maxibox1

     box(ibox1,ibox2,ibox3)%pr_popul=0
     ALLOCATE(box(ibox1,ibox2,ibox3)%pr_which(pr_maxpopul),stat=errare)
     IF (errare /=0 ) call faterr(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%pr_which')

     box(ibox1,ibox2,ibox3)%sp_popul=0
     ALLOCATE(box(ibox1,ibox2,ibox3)%sp_which(sp_maxpopul),stat=errare)
     IF (errare /=0 ) call faterr(IAM,'error in allocating box(1+maxibox1,1+maxibox2)%sp_which')

   END DO  
   END DO
   END DO
   
   ! filling boxes
   ! box(ibox1,ibox2)%pr_popul is the number of polyr into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%pr_which(ipopul) is the rank of body POLYR labelled ipopul in the box


   ! filling boxes with SPHER  
   DO itac=1,nb_SPHER

     IF (.NOT. get_visible_SPHER(itac) ) CYCLE

     coordcd=SPcoor(1:3,itac)
     ibox1=1+INT((coordcd(1)-Xleft )*Lbox_1)
     ibox2=1+INT((coordcd(2)-Yleft )*Lbox_1)
     ibox3=1+INT((coordcd(3)-Zdown )*Lbox_1)

     !print*,"polyr:",itac
     !print*,coordcd(1),Xleft,Lbox_1,ibox1
     !print*,coordcd(2),Yleft,Lbox_1,ibox2
     !print*,coordcd(3),Zdown,Lbox_1,ibox3

     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. &
         ibox2 < minibox2 .OR. ibox2 > maxibox2 .OR. &
         ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
       write(cout,*)' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
       call logmes(cout,.true.)
       write(cout,*)'    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
       call logmes(cout,.true.)
       write(cout,*)' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
       call logmes(cout,.true.)
       write(cout,'(A13,I5,A13)')'  body SPHER ',itac,' out of boxes'
       call logmes(cout,.true.)
       write(cout,'(A38)')'  see select_prox_tactors in mod_SPPRx'
       call logmes(cout,.true.)
       write(cout,*) 'Nstep',Nstep,'X',coordcd(1),'Y',coordcd(2),'Z',coordcd(3)
       call logmes(cout,.true.)
       call faterr(IAM,'issue filling boxes')
     END IF

     box(ibox1,ibox2,ibox3)%sp_popul = box(ibox1,ibox2,ibox3)%sp_popul+1
     if( box(ibox1,ibox2,ibox3)%sp_popul > size(box(ibox1,ibox2,ibox3)%sp_which) ) then
         call faterr(IAM, "Estimated max popul limit reached.")
     end if
     box(ibox1,ibox2,ibox3)%sp_which(box(ibox1,ibox2,ibox3)%sp_popul) = itac
     
   END DO  
   
   ! filling boxes with POLYR
   DO itac=1,nb_POLYR

     IF (.NOT. get_visible_POLYR(itac) ) CYCLE

     ! cycling if big POLYR
     if (nb_big_polyr /= 0) then
       if (any(big_polyr == itac) ) cycle
     endif
    
     coordcd=PRcoor(1:3,itac)
     ibox1=1+INT((coordcd(1)-Xleft )*Lbox_1)
     ibox2=1+INT((coordcd(2)-Yleft )*Lbox_1)
     ibox3=1+INT((coordcd(3)-Zdown )*Lbox_1)

     ! if (polyr2bdyty(1,itac) == 192) print*,'ian in ',ibox1,ibox2,ibox3
     
     !print*,"polyr:",itac
     !print*,coordcd(1),Xleft,Lbox_1,ibox1
     !print*,coordcd(2),Yleft,Lbox_1,ibox2
     !print*,coordcd(3),Zdown,Lbox_1,ibox3

     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. &
         ibox2 < minibox2 .OR. ibox2 > maxibox2 .OR. &
         ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
       write(cout,*)' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
       call logmes(cout,.true.)
       write(cout,*)'    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
       call logmes(cout,.true.)
       write(cout,*)' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
       call logmes(cout,.true.)
       write(cout,'(A13,I5,A13)')'  body POLYR ',itac,' out of boxes'
       call logmes(cout,.true.)
       write(cout,'(A38)')'  see select_prox_tactors in mod_SPPRx'
       call logmes(cout,.true.)
       write(cout,*) 'Nstep',Nstep,'X',coordcd(1),'Y',coordcd(2),'Z',coordcd(3)
       call logmes(cout,.true.)
       call faterr(IAM,'issue filling boxes')
     END IF

     box(ibox1,ibox2,ibox3)%pr_popul = box(ibox1,ibox2,ibox3)%pr_popul+1
     if( box(ibox1,ibox2,ibox3)%pr_popul > size(box(ibox1,ibox2,ibox3)%pr_which) ) then
         call faterr(IAM, "Estimated max popul limit reached.")
     end if
     box(ibox1,ibox2,ibox3)%pr_which(box(ibox1,ibox2,ibox3)%pr_popul) = itac
     
   END DO  

   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
     
   nb_rough_SPPRx=0

   ! création de la liste de paire de polygones à examiner

   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

   NULLIFY(Root) 
   NULLIFY(Current)
   NULLIFY(Previous)

   DO ibox3cd=minibox3,maxibox3
   DO ibox2cd=minibox2,maxibox2
   DO ibox1cd=minibox1,maxibox1 

     DO icdpop=1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
       icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
       cdcol = get_color_SPHER(icdtac)
       ! box loop investigating antagonist POLYR
       DO ibox3an=MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
       DO ibox2an=MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
       DO ibox1an=MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)            

         DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
           iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)

           !
           ancol=get_color_POLYR(iantac)

           !fd todo a revoir pour traiter le cas MAILx
           ! en utilisant   cd_mdl = get_mdl_POLYR(icdtac)
  
           !print*,get_body_model_name_from_id(polyr2bdyty(3,icdtac)),cdcol,get_body_model_name_from_id(polyr2bdyty(3,iantac)),ancol

           isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                           get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

           !print*,isee

           
           IF (isee /= 0) THEN
             adist=see(isee)%alert 
             ! checking ROUGHLY distance against alert distance           
             raycd = get_radius_SPHER(icdtac)
             rayan = get_radius_POLYR(iantac)

             ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
             ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
             ! results might be different up to some non significant figures, but when comparing to
             ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

             adist=0.1005D+01*(adist+raycd+rayan)

             sep = SPcoor(:,icdtac)-PRcoor(:,iantac)
             dist = DOT_PRODUCT(sep,sep)
 
             IF (dist<adist*adist) THEN
               nb_rough_SPPRx=nb_rough_SPPRx+1

               IF ( nb_rough_SPPRx == 1) THEN
                 ALLOCATE(Root)
                 Current => Root
                 NULLIFY(Root%p)
               ELSE
                 ALLOCATE(Current)
                 Previous%n => Current
               ENDIF
               Current%val%cd       =icdtac
               Current%val%an       =iantac
               Current%val%isee     =isee
               Current%val%Vsep     = sep/dsqrt(dist)
               Current%val%xperiodic = 0
               Current%val%yperiodic = 0
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current
             ENDIF
           END IF
         END DO
       END DO
       END DO
       END DO
     END DO
   END DO
   END DO
   END DO

   !fd dbg
   !print*,'detection classique ',nb_rough_SPPRx

   ! on est oblige de faire ca car les big ne sont pas dans les boites

   !fd le big est un antagoniste 
   !fd => meilleur gestion du shrink

   DO i=1,nb_big_polyr
     iantac=big_polyr(i)

     IF (.NOT. get_visible_POLYR(iantac)) CYCLE

     ancol=get_color_POLYR(iantac)
     coordan = PRcoor(1:3,iantac)
     rayan = get_radius_POLYR(iantac)

      DO icdtac=1,nb_SPHER 

        IF (.NOT. get_visible_SPHER(icdtac)) CYCLE
         
        cdcol=get_color_SPHER(icdtac)
        
        isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                        get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

        IF (isee /= 0) THEN
          adist=see(isee)%alert 
          coordcd = SPcoor(1:3,icdtac)
          raycd = get_radius_SPHER(icdtac)

          ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
          ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
          ! results might be different up to some non significant figures, but when comparing to
          ! alert distance, extra candidates to contact might be selected in ambiguous situations.   

          adist=0.1005D+01*(adist+raycd+rayan)
          sep = coordcd(:)-coordan(:)
          dist = DOT_PRODUCT(sep,sep)

          IF (dist < adist*adist) THEN
            nb_rough_SPPRx=nb_rough_SPPRx+1

            IF ( nb_rough_SPPRx == 1) THEN
              ALLOCATE(Root)
              Current => Root
              NULLIFY(Root%p)
            ELSE
              ALLOCATE(Current)
              Previous%n => Current
            ENDIF

            Current%val%cd       =icdtac
            Current%val%an       =iantac
            Current%val%isee     =isee                  
            Current%val%Vsep     =sep/dsqrt(dist)                
            Current%val%xperiodic = 0
            Current%val%yperiodic = 0

            Current%p => Previous
            NULLIFY(Current%n)
            Previous => Current
          ENDIF
        END IF
      ENDDO
   ENDDO

   !fd dbg
   !print*,'+detection big polyr ',nb_rough_SPPRx
   
   IF (XPERIODIC) THEN

     ! on cherche SP premiere colonne avec PR derniere colonne (sens +)
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = minibox2,maxibox2
     DO ibox1cd = minibox1,MIN(minibox1+1,maxibox1)
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
         cdcol = get_color_SPHER(icdtac)

         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)
         DO ibox1an = MAX(maxibox1-1,minibox1),maxibox1        

           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
             iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
             ancol = get_color_POLYR(iantac)

             isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                             get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)


             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                              
             coordcd = SPcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_SPHER(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(1) = coordan(1) - xperiode

             adist=0.1005D+01*(adist+raycd+rayan)
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN

               nb_rough_SPPRx = nb_rough_SPPRx+1

               IF ( nb_rough_SPPRx == 1) THEN
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
               Current%val%Vsep      = sep/dsqrt(dist)
               Current%val%xperiodic = -1
               Current%val%yperiodic = 0
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current

               if (bavard) then
                 print *,icdtac,' voit ', iantac
                 print *,'posi cd : ',coordcd
                 print *,'posi an : ',coordan
                 print *,'dist    : ',dsqrt(dist) 
                 print *,Current%val%xperiodic,Current%val%yperiodic
                 print *,maxibox1,maxibox2,maxibox3
                 print *,ibox1cd,ibox2cd,ibox3cd
                 print *,ibox1an,ibox2an,ibox3an
               end if
             END IF
           END DO
         END DO
         END DO
         END DO
       END DO
     END DO
     END DO
     END DO


     ! on cherche SP derniere colonne avec PR premiere colonne (sens -)
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = minibox2,maxibox2
     DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1        
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
         cdcol = get_color_SPHER(icdtac)

         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)
         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)

           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
             iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
             ancol = get_color_POLYR(iantac)

             isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                             get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                              
             coordcd = SPcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_SPHER(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(1) = coordan(1) + xperiode

             adist=0.1005D+01*(adist+raycd+rayan)
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN

               nb_rough_SPPRx = nb_rough_SPPRx+1

               IF ( nb_rough_SPPRx == 1) THEN
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
               Current%val%Vsep      = sep/dsqrt(dist)
               Current%val%xperiodic = 1
               Current%val%yperiodic = 0
               Current%p => Previous
               NULLIFY(Current%n)
               Previous => Current

               if (bavard) then
                 print *,icdtac,' voit ', iantac
                 print *,'posi cd : ',coordcd
                 print *,'posi an : ',coordan
                 print *,'dist    : ',dsqrt(dist) 
                 print *,Current%val%xperiodic,Current%val%yperiodic
                 print *,maxibox1,maxibox2,maxibox3
                 print *,ibox1cd,ibox2cd,ibox3cd
                 print *,ibox1an,ibox2an,ibox3an
               end if
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

   !fd dbg
   !print*,'+xperiodic ',nb_rough_SPPRx

   
  IF (YPERIODIC) THEN

     ! on cherche SP premiere colonne PR derniere colonne (sens +)   
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
     DO ibox1cd = minibox1,maxibox1
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
         cdcol = get_color_SPHER(icdtac)

         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2    
         DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)  

           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul

             iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
              
             ancol = get_color_POLYR(iantac)

             isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                             get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                               
             coordcd = SPcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_SPHER(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(2) = coordan(2) - yperiode

             adist=0.1005D+01*adist+raycd+rayan
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN
               nb_rough_SPPRx = nb_rough_SPPRx+1

               IF ( nb_rough_SPPRx == 1) THEN
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
               Current%val%Vsep      = sep/dsqrt(dist)
               Current%val%xperiodic = 0
               Current%val%yperiodic =-1
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

     ! on cherche SP derniere colonne PR pemiere colonne (sens -)
     DO ibox3cd = minibox3,maxibox3
     DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2    
     DO ibox1cd = minibox1,maxibox1
           
       DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
         icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
         cdcol = get_color_SPHER(icdtac)

         DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
         DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)                       
         DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)  

           DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul

             iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
              
             ancol = get_color_POLYR(iantac)

             isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                             get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

             IF (isee == 0) CYCLE
             adist=see(isee)%alert 
                               
             coordcd = SPcoor(1:3,icdtac)
             coordan = PRcoor(1:3,iantac)
             raycd = get_radius_SPHER(icdtac)
             rayan = get_radius_POLYR(iantac)

             coordan(2) = coordan(2) + yperiode

             adist=0.1005D+01*adist+raycd+rayan
             sep = coordcd(:)-coordan(:)
             dist = DOT_PRODUCT(sep,sep)

             IF (dist<adist*adist) THEN
               nb_rough_SPPRx = nb_rough_SPPRx+1

               IF ( nb_rough_SPPRx == 1) THEN
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
               Current%val%Vsep      = sep/dsqrt(dist)
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

   !fd dbg
   !print*,'+yperiodic ',nb_rough_SPPRx
   
   !fd le 04/01/08 contrairement au 2D il faut traiter les coins !! 
   !fd A VOIR si une seule boite ca va deconner

    IF (XPERIODIC .AND. YPERIODIC) THEN

       !fd on commence par le coin extreme en haut qui voit le coin initial en bas
       !fd sens -
       DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
       DO ibox1cd = minibox1,MIN(minibox1+1,maxibox1)
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
            icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
            cdcol = get_color_SPHER(icdtac)

            DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
            DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2  
            DO ibox1an = MAX(maxibox1-1,minibox1),maxibox1        

              DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
                iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
                ancol = get_color_POLYR(iantac)

                isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                                get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

                IF (isee.EQ.0) CYCLE
                adist=see(isee)%alert 
                               
                coordcd = PRcoor(1:3,icdtac)
                coordan = PRcoor(1:3,iantac)
                raycd = get_radius_POLYR(icdtac)
                rayan = get_radius_POLYR(iantac)

                coordan(1) = coordan(1) - xperiode
                coordan(2) = coordan(2) - yperiode

                adist=0.1005D+01*adist+raycd+rayan
                               
                sep = coordcd(:)-coordan(:)
                dist = DOT_PRODUCT(sep,sep)

                IF (dist<adist*adist) THEN
                  nb_rough_SPPRx = nb_rough_SPPRx+1

                  IF ( nb_rough_SPPRx == 1) THEN
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
                  Current%val%Vsep      = sep/dsqrt(dist)
                  Current%val%xperiodic = -1
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


       !fd sens +    
       DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2  
       DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1        
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
            icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
            cdcol = get_color_SPHER(icdtac)

            DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
            DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
            DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)

              DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
                iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
                ancol = get_color_POLYR(iantac)

                isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                                get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

                IF (isee.EQ.0) CYCLE
                adist=see(isee)%alert 
                               
                coordcd = PRcoor(1:3,icdtac)
                coordan = PRcoor(1:3,iantac)
                raycd = get_radius_POLYR(icdtac)
                rayan = get_radius_POLYR(iantac)

                coordan(1) = coordan(1) + xperiode
                coordan(2) = coordan(2) + yperiode

                adist=0.1005D+01*adist+raycd+rayan
                               
                sep = coordcd(:)-coordan(:)
                dist = DOT_PRODUCT(sep,sep)

                IF (dist<adist*adist) THEN
                  nb_rough_SPPRx = nb_rough_SPPRx+1

                  IF ( nb_rough_SPPRx == 1) THEN
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
                  Current%val%Vsep      = sep/dsqrt(dist)
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
       !fd sens -
       DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2
       DO ibox1cd = minibox1,MIN(minibox1+1,maxibox1)
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
           icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
           cdcol = get_color_SPHER(icdtac)
                   
           DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
           DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
           DO ibox1an = MAX(maxibox1-1,minibox1),maxibox1 
             DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
               iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
               ancol = get_color_POLYR(iantac)

               isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                               get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

               IF (isee.EQ.0) CYCLE
               adist=see(isee)%alert 
                               
               coordcd = PRcoor(1:3,icdtac)
               coordan = PRcoor(1:3,iantac)
               raycd = get_radius_POLYR(icdtac)
               rayan = get_radius_POLYR(iantac)
               coordan(1) = coordan(1) - xperiode
               coordan(2) = coordan(2) + yperiode

               adist=0.1005D+01*adist+raycd+rayan
                               
               sep = coordcd(:)-coordan(:)
               dist = DOT_PRODUCT(sep,sep)

               IF (dist<adist*adist) THEN

                 nb_rough_SPPRx = nb_rough_SPPRx+1

                 IF ( nb_rough_SPPRx == 1) THEN
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
                 Current%val%Vsep      = sep/dsqrt(dist)
                 Current%val%xperiodic = -1
                 Current%val%yperiodic =  1
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

       !fd sens +
       DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
       DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1 
         DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%sp_popul
           icdtac = box(ibox1cd,ibox2cd,ibox3cd)%sp_which(icdpop)
           cdcol = get_color_SPHER(icdtac)
                   
           DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
           DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2
           DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
             DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%pr_popul
               iantac=box(ibox1an,ibox2an,ibox3an)%pr_which(ianpop)
               ancol = get_color_POLYR(iantac)

               isee = get_isee(get_body_model_name_from_id(spher2bdyty(3,icdtac)),'SPHER',cdcol, &
                               get_body_model_name_from_id(polyr2bdyty(3,iantac)),'POLYR',ancol)

               IF (isee.EQ.0) CYCLE
               adist=see(isee)%alert 
                               
               coordcd = PRcoor(1:3,icdtac)
               coordan = PRcoor(1:3,iantac)
               raycd = get_radius_POLYR(icdtac)
               rayan = get_radius_POLYR(iantac)
               coordan(1) = coordan(1) + xperiode
               coordan(2) = coordan(2) - yperiode

               adist=0.1005D+01*adist+raycd+rayan
                               
               sep = coordcd(:)-coordan(:)
               dist = DOT_PRODUCT(sep,sep)

               IF (dist<adist*adist) THEN

                 nb_rough_SPPRx = nb_rough_SPPRx+1

                 IF ( nb_rough_SPPRx == 1) THEN
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
                 Current%val%Vsep      = sep/dsqrt(dist)
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

   IF (ALLOCATED(box)) THEN
      DO ibox3=minibox3,maxibox3
      DO ibox2=minibox2,maxibox2
      DO ibox1=minibox1,maxibox1
        IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%pr_which)) DEALLOCATE(box(ibox1,ibox2,ibox3)%pr_which)
        IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%sp_which)) DEALLOCATE(box(ibox1,ibox2,ibox3)%sp_which)         
      ENDDO
      ENDDO
      ENDDO
      DEALLOCATE(box)
   ENDIF


   ! the visibility array used in compute_contact is allocated
   IF (ALLOCATED(rough_SPPRx)) DEALLOCATE(rough_SPPRx)
   ALLOCATE(rough_SPPRx(nb_rough_SPPRx),stat=errare)      
   IF (errare /=0 ) call FATERR(IAM,'error in allocating rough_SPPRx')

   size_of_this = nb_rough_SPPRx

   WRITE(cout,'(4X,I10,A20)') nb_rough_SPPRx,' SPPRx roughly found'       
   call logmes(cout)
   WRITE(cout,'(4X,I10,A25)') size_of_this, ' size of this array' 
   call logmes(cout)
   
   ! the oversized array this is temporaly allocated
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(size_of_this),stat=errare) 
   IF (errare /=0 ) call faterr(IAM,'error in allocating this')

   DO icdan=nb_rough_SPPRx,1,-1
     
     Previous => Current%p
     rough_SPPRx(icdan)%cd        = Current%val%cd
     rough_SPPRx(icdan)%an        = Current%val%an
     rough_SPPRx(icdan)%isee      = Current%val%isee
     rough_SPPRx(icdan)%Vsep(1:3) = Current%val%Vsep

     !am: each rough interaction belongs to the default group
     rough_SPPRx(icdan)%group     = NOINT

     rough_SPPRx(icdan)%xperiodic = Current%val%xperiodic
     rough_SPPRx(icdan)%yperiodic = Current%val%yperiodic

     DEALLOCATE(Current)
     Current => Previous

     !print*,'rough detection'
     !print*,'rough contact ',icdan,' between cd ',rough_SPPRx(icdan)%cd,' and an ',rough_SPPRx(icdan)%an
 

   END DO 
   
   NULLIFY(Root)

  END SUBROUTINE creation_tab_visu_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------

  SUBROUTINE compute_contact_SPPRx()
 
   IMPLICIT NONE  

   !***
   INTEGER                             :: errare 
   INTEGER                             :: icdan,iadj,itac,i,j
   INTEGER                             :: icdtac,iantac,isee   
   REAL(kind=8)                        :: raycd,rayan,adist,dist,gdist
   INTEGER                             :: nb_ctc
   INTEGER                             :: size_of_array_this
   REAL(kind=8),DIMENSION(3)           :: xco,t,n,s 
   REAL(kind=8)                        :: ovlap,surf
   REAL(kind=8),DIMENSION(3)           :: sep,cdlev,anlev
   REAL(kind=8)                        :: norme
   REAL(kind=8),DIMENSION(6)           :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3,3)         :: Rc,localframe_cd,localframe_an
   REAL(kind=8)                        :: t1,t2,vls_cst,vln_cst,vlt_cst

   integer :: cd_ent,an_ent

                            !1234567890123456789012345
   character(len=22) :: IAM='SPPRx::compute_contact' 
   character(len=80) :: mes
   character(len=450):: lcout
 
   icdan   = 0        
   nb_SPPRx= 0
   nb_adj  = 0

   IF (nb_rough_SPPRx /= 0 ) THEN

     size_of_array_this=SIZE(this)
  
   !
   ! preparation de la detection 
   !
     DO i=1,nb_rough_SPPRx 

       icdtac    = rough_SPPRx(i)%cd
       iantac    = rough_SPPRx(i)%an 
       isee      = rough_SPPRx(i)%isee

       adist=see(isee)%alert
       
       dist=get_radius_SPHER(icdtac) + S_POLYR(iantac)%radius + adist

       gdist=see(isee)%global_alert

       perio_shift = 0.d0
       perio_shift(1) = real(rough_SPPRx(i)%xperiodic,8) * xperiode
       perio_shift(2) = real(rough_SPPRx(i)%yperiodic,8) * yperiode

       sep=SPcoor(1:3,icdtac) - (S_POLYR(iantac)%center + perio_shift)
       norme=sep(1)*sep(1)+sep(2)*sep(2)+sep(3)*sep(3)

       !fd il faut eviter ca car avec des corps non convexes emboites ca ne marche pas

!fd @@@ ca a pas deja ete teste avant dans la partie rough ? 
!fd @@@ faut il l'actualiser en cas de step ou alors on estime qu'on sait ce qu'on fait ?
!fd @@@ et il en manque je pense ...

       IF (norme < dist*dist) THEN

         CALL DETECTION_spprx(icdtac,iantac,gdist,adist, &
                              nb_ctc,xco,ovlap, &
                              t,n,s,surf,.true.)

         IF (nb_ctc==0) CYCLE

           localframe_cd = get_inertia_frameTT_SPHER(spher2bdyty(1,icdtac))
           localframe_an = get_inertia_frameTT_POLYR(iantac)

           cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iVbeg_)
           an_Vbegin = get_vlocy_POLYR(iantac,iVbeg_)
           
           icdan = icdan+1

             IF (icdan>size_of_array_this) THEN
                            !123456789012345678901234567890123456789012345
                call logmes('---------------------------------------------', .true.)
                call logmes('ERROR filling this                           ', .true.)
                call logmes('---------------------------------------------', .true.)

                call faterr(IAM,'Error')
             ENDIF   

             vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*t(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*t(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*t(3)
             vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*n(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*n(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*n(3)
             vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*s(1)+ &
                     (cd_Vbegin(2)-an_Vbegin(2))*s(2)+ &
                     (cd_Vbegin(3)-an_Vbegin(3))*s(3)

             this(icdan)%icdbtac = spher2bdyty(2, icdtac)
             this(icdan)%ianbtac = polyr2bdyty(2, iantac)

             this(icdan)%icdbtyp = spher2bdyty(3, icdtac)
             this(icdan)%ianbtyp = polyr2bdyty(3, iantac)

             this(icdan)%icdctyp = i_spher
             this(icdan)%ianctyp = i_polyr

             this(icdan)%icdsci  = 0
             this(icdan)%iansci  = 0

             nb_adj(icdtac)      = nb_adj(icdtac) + 1
             iadj                = nb_adj(icdtac)
             this(icdan)%iadj    = iadj
             this(icdan)%icdbdy  = spher2bdyty(1, icdtac)
             this(icdan)%icdtac  = icdtac
             this(icdan)%ianbdy  = polyr2bdyty(1, iantac)
             this(icdan)%iantac  = iantac
             this(icdan)%isee    = isee
             this(icdan)%tuc(:)  = t(:)
             this(icdan)%nuc(:)  = n(:)
             this(icdan)%suc(:)  = s(:)

             this(icdan)%coor     = xco(1:3)
             this(icdan)%type_ctc = nb_ctc

             cd_ent = get_ent_SPHER(this(icdan)%icdbdy)
             an_ent = get_ent_POLYR(this(icdan)%iantac) 
         
             this(icdan)%icdent = cd_ent
             this(icdan)%ianent = an_ent

             entity(cd_ent)%nb = entity(cd_ent)%nb + 1
             entity(an_ent)%nb = entity(an_ent)%nb + 1

!fd le 11/09/08 manquait le shift. PRcoor c'est le centre du polyr pas le centre d'inertie
             cdlev = xco(1:3)  &
                   - (SPcoor(1:3,icdtac)-get_shiftTT_SPHER(spher2bdyty(1, icdtac),spher2bdyty(2, icdtac)))
             anlev = xco(1:3) &
                   - (PRcoor(1:3,iantac)-get_shiftTT_POLYR(iantac) + perio_shift)


!fd @@@ calcul de la coordonnee du point de contact dans le repere principal d'inertie
!fd @@@ on calcule la coordonnee dans le repere principal d'inertie actuel

             this(icdan)%icdcoor(1)=cdlev(1)*localframe_cd(1,1)+ &
                                    cdlev(2)*localframe_cd(2,1)+ &
                                    cdlev(3)*localframe_cd(3,1)
             this(icdan)%icdcoor(2)=cdlev(1)*localframe_cd(1,2)+ &
                                    cdlev(2)*localframe_cd(2,2)+ &
                                    cdlev(3)*localframe_cd(3,2)
             this(icdan)%icdcoor(3)=cdlev(1)*localframe_cd(1,3)+ &
                                    cdlev(2)*localframe_cd(2,3)+ & 
                                    cdlev(3)*localframe_cd(3,3)

             this(icdan)%iancoor(1)=anlev(1)*localframe_an(1,1)+ &
                                    anlev(2)*localframe_an(2,1)+ &
                                    anlev(3)*localframe_an(3,1)
             this(icdan)%iancoor(2)=anlev(1)*localframe_an(1,2)+ &
                                    anlev(2)*localframe_an(2,2)+ &
                                    anlev(3)*localframe_an(3,2)
             this(icdan)%iancoor(3)=anlev(1)*localframe_an(1,3)+ &
                                    anlev(2)*localframe_an(2,3)+ &
                                    anlev(3)*localframe_an(3,3)


             ! On va calculer le passage rep inertie -> rep général pour l'antagoniste

             Rc(1,1)= localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
             Rc(2,1)= localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
             Rc(3,1)= localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)

             Rc(1,2)= localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
             Rc(2,2)= localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
             Rc(3,2)= localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)

             Rc(1,3)= localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
             Rc(2,3)= localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
             Rc(3,3)= localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)

             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 


             ! On va calculer le passage rep inertie -> rep général pour le candidat

             Rc(1,1)=localframe_cd(2,1)*cdlev(3) - localframe_cd(3,1)*cdlev(2)
             Rc(2,1)=localframe_cd(2,2)*cdlev(3) - localframe_cd(3,2)*cdlev(2)
             Rc(3,1)=localframe_cd(2,3)*cdlev(3) - localframe_cd(3,3)*cdlev(2)

             Rc(1,2)=localframe_cd(3,1)*cdlev(1) - localframe_cd(1,1)*cdlev(3)
             Rc(2,2)=localframe_cd(3,2)*cdlev(1) - localframe_cd(1,2)*cdlev(3)
             Rc(3,2)=localframe_cd(3,3)*cdlev(1) - localframe_cd(1,3)*cdlev(3)

             Rc(1,3)=localframe_cd(1,1)*cdlev(2) - localframe_cd(2,1)*cdlev(1)
             Rc(2,3)=localframe_cd(1,2)*cdlev(2) - localframe_cd(2,2)*cdlev(1)
             Rc(3,3)=localframe_cd(1,3)*cdlev(2) - localframe_cd(2,3)*cdlev(1)


             this(icdan)%Gcdt(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
             this(icdan)%Gcdt(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

             this(icdan)%Gcdn(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
             this(icdan)%Gcdn(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

             this(icdan)%Gcds(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
             this(icdan)%Gcds(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 
           
             ! Calcul des vitesses relatives


             this(icdan)%gapTTbegin      = ovlap


             this(icdan)%vltBEGIN = vlt_cst &
                  + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                  - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

             this(icdan)%vlnBEGIN = vln_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                  - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

             this(icdan)%vlsBEGIN= vls_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                  - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)


             this(icdan)%rls      = 0.D0
             this(icdan)%rlt      = 0.D0
             this(icdan)%rln      = 0.D0
             this(icdan)%vls      = this(icdan)%vlsBEGIN
             this(icdan)%vlt      = this(icdan)%vltBEGIN
             this(icdan)%vln      = this(icdan)%vlnBEGIN
             this(icdan)%gapTT    = this(icdan)%gapTTbegin
             this(icdan)%status   = i_nknow

             this(icdan)%area     = surf

         ENDIF  
     ENDDO
     nb_SPPRx=icdan

   ENDIF

   WRITE(mes,'(1X,I10,A12)') nb_SPPRx,' SPPRx found'       
   call logmes(mes)


   do icdan = 1, nb_SPPRx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   DO itac=1,nb_SPHER
     IF (ASSOCIATED(adjac(itac)%icdan))  DEALLOCATE(adjac(itac)%icdan)
     IF (nb_adj(itac) /= 0) THEN
       ALLOCATE(adjac(itac)%icdan(nb_adj(itac)),stat=errare)
       IF (errare /=0 ) THEN
         call faterr(IAM,' error in allocating adjac(icdtac)%.....')
       END IF
     ENDIF
   ENDDO 
 
   DO icdan=1,nb_SPPRx
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
   END DO 

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_SPPRx),stat=errare)

  END SUBROUTINE compute_contact_SPPRx
  !------------------------------------------------------------------------

  !------------------------------------------------------------------------
  SUBROUTINE DETECTION_SPPRx(id_sp,id_pr,global_adist,local_adist, &
                                 nb_ctc,PT_CTC,overlap, &
                                 t,n,s,surfaces,do_trim)

! I
! id_sp : id solide 1 (candidat)
! id_pr : id solide 2 (antagoniste)
! global_adist : distance d'alerte globale qui aide dans la recherche des 
!                contacts (notion de halo) 
! local_adist : distance d'alerte locale pour decider si un contact est actif
! do_trim: a flag to say if some triming on normals is performed (.true.) or not (.false.)
!           when using large triming should be avoided
   
!
! O 
! nb_ctc        : nombre reel de points de contact
! nbf           : taille des tableaux qu'on remonte (nombre de faces | nombre de sommets du cd)
! status        : =0 pas contact, =1 noeud, =2 arete, =3 face  
! PT_CTC(nbf)   : coordonnees des points de contact
! overlap(nbf)  : les gaps aux points de contact
! tt(3,nbf)     : tangentes aux points de contact si il y a contact
! nn(3,nbf)     : normales aux points de contact si il y a contact
! ss(3,nbf)     : tangentes aux points de contact si il y a contact
! surfaces(nbf) : aire du point de contact 

   IMPLICIT NONE

   INTEGER                                 :: id_sp,id_pr,nb_ctc
   REAL(kind=8)                            :: local_adist,global_adist
   integer                                 :: status
   REAL(kind=8)                            :: overlap,surfaces
   REAL(kind=8),DIMENSION(3)               :: PT_CTC,t,n,s
   logical                                 :: do_trim
   !***
                                                !1234567890123456789012
   CHARACTER(len=22)                   :: IAM = 'SPPRx::detection_SPPRx'

   TYPE(T_POLYR)                       :: PRan

   ! pour chaque face le centre et sa normale
   real(kind=8),dimension(3)           :: coor_sp,sep,point,weight
   real(kind=8)                        :: ray_sp    

   integer                             :: ff,ppp
   real(kind=8)                        :: gap

   integer,dimension(:),allocatable    :: good_nodes 

   character(len=90) :: cout
   integer           :: err_

   nb_ctc=0


   ray_sp   = get_radius_SPHER(id_sp)
   coor_sp = get_coorTT_SPHER(spher2bdyty(1,id_sp),spher2bdyty(2,id_sp))       

   !fd perio_shift is a global vector filled before calling this function 
   coor_sp = coor_sp - perio_shift
   
   PRan    = S_POLYR(id_pr)

   sep = coor_sp  - PRan%center
   sep = -sep/SQRT(dot_product(sep,sep))
   
   allocate(good_nodes(PRan%nb_vertex))

   good_nodes = 1

   ! we adapt the use of this function ; gdist + raycd
   ppp = 0
   status = node_HE_Hdl_proximity(PRan%HE_Hdl,coor_sp,global_adist+ray_sp, &
                                  sep,do_trim,ppp,gap, &
                                  point,t,n,s,ff,weight,.false.,err_,good_nodes=good_nodes)
   
   if (err_ > 0) then
     write(cout,'("PRan ",I0)') PRan%id
     call logmes(cout, .true.)
     call faterr(IAM,'unexpected problem while getting proximal node to HE')
   endif   

   ! le cas degenere ou le ppp n'est pas dans une face
   if ( minval(weight) == 0.d0 ) then
     sep = coor_sp(:) - point(:)
     n   = sep / length3(sep)
     gap = dot_product(sep,n)
   endif

   ! print*,'statut: ',status
   ! print*,weight
   ! print*,'distance et rep local'
   ! print*,gap
   ! print*,t
   ! print*,n
   ! print*,s

   if (status > 0 .and. gap-ray_sp < local_adist) then
     nb_ctc = nb_ctc + 1
     overlap =gap-ray_sp
     !fd pour etre coherent avec nc_compute_contact         
     pt_ctc=point(:)+perio_shift(:)
   else
     ! on remet a 0 pour virer les cas avec local_adist trop grand
     status  = 0           
     overlap = 0.d0
     pt_ctc  = 0.d0
     t = 0.d0
     n = 0.d0
     s = 0.d0
   endif       

   deallocate(good_nodes)


  END SUBROUTINE DETECTION_SPPRx
  !------------------------------------------------------------------------
  
  !--------------------------------------------------------------------------------------------------
  subroutine set_tol_recup_rloc_SPPRx(tol)
   implicit none
   real(kind=8), intent(in) :: tol

   tol_recup_rloc = tol
  end subroutine
  !--------------------------------------------------------------------------------------------------

  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_vector_SPPRx(name,icdan,vect,sz)
   IMPLICIT NONE

   INTEGER :: icdan,sz
   REAL(kind=8),DIMENSION(sz) :: vect
   CHARACTER(len=5) :: name
   CHARACTER(len=80) :: cout

                              !12345678901234567890123456780'
   CHARACTER(len=30) :: IAM = 'SPPRx::get_interaction_vector'

   if (icdan > nb_SPPRx) call FATERR(IAM,'given SPPRx index greater than number of SPPRx')

   SELECT CASE(name)
   CASE('Coor_')
     vect=this(icdan)%coor
   CASE('N____')
     vect=this(icdan)%nuc
   CASE DEFAULT
     WRITE(cout,'(A,1x,A)') 'Sorry unknown id:',name
     CALL faterr(IAM,cout)
   END SELECT

  end subroutine 
  !--------------------------------------------------------------------------------------------------  
 
  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_internal_SPPRx(i,icdan,rvalue)
   IMPLICIT NONE

   INTEGER :: i,icdan
   REAL(kind=8) :: rvalue

   ! ***
                              !1234567890123456789012345678012'
   CHARACTER(len=32) :: IAM = 'SPPRx::get_interaction_internal'

   if (icdan > nb_SPPRx) call FATERR(IAM,'given SPPRx index greater than number of SPPRx')
   if (i > max_internal_tact ) call FATERR(IAM,'given internal index greater than max number of internal')

   rvalue = this(icdan)%internal(i)

  end subroutine 
  !--------------------------------------------------------------------------------------------------
 
  !--------------------------------------------------------------------------------------------------
  subroutine set_interaction_internal_SPPRx(i,icdan,rvalue)
   IMPLICIT NONE

   INTEGER :: i,icdan
   REAL(kind=8) :: rvalue

   ! ***
                              !1234567890123456789012345678012'
   CHARACTER(len=32) :: IAM = 'SPPRx::set_interaction_internal'

   if (icdan > nb_SPPRx) call FATERR(IAM,'given SPPRx index greater than number of SPPRx')
   if (i > max_internal_tact ) call FATERR(IAM,'given internal index greater than max number of internal')

   this(icdan)%internal(i)=rvalue 

  end subroutine 
  !--------------------------------------------------------------------------------------------------
 
  !--------------------------------------------------------------------------------------------------
  subroutine get_interaction_internal_comment_SPPRx(icdan,cvect)
   IMPLICIT NONE

   INTEGER :: icdan
   CHARACTER(len=100) :: cvect

   ! ***
                              !123456789012345678901234567801234567890'
   CHARACTER(len=40) :: IAM = 'SPPRx::get_interaction_internal_comment'

   if (icdan > nb_SPPRx) call FATERR(IAM,'given SPPRx index greater than number of SPPRx')

   cvect=''
   IF (this(icdan)%nb_internal /= 0) cvect=get_internal_comment(this(icdan)%lawnb)
 
  end subroutine get_interaction_internal_comment_SPPRx
  !--------------------------------------------------------------------------------------------------
   
  !--------------------------------------------------------------------------------------------------  
  subroutine print_info_SPPRx(icdan)
     implicit none
     integer          :: icdan,icdtac,iantac,icdbdy,ianbdy
  
     character(len=80) :: cout
  
     icdtac=this(icdan)%icdtac
     iantac=this(icdan)%iantac
  
     write(cout,1) icdtac,iantac
     call LOGMES(cout)
  
1    format(1X,'SPHER:',1x,I0,1x,'POLYR:',1x,I0)
  
     icdbdy=this(icdan)%icdbdy
     ianbdy=this(icdan)%ianbdy
  
     call print_info_SPHER(icdtac)
     call print_info_POLYR(iantac)
  
  end subroutine print_info_SPPRx
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------    
  subroutine set_nb_SPPRx(nb)
    implicit none
    integer(kind=4), intent(in) :: nb

    if( allocated(this) ) then
      deallocate(this)
    end if

    allocate( this(nb) )

    nb_SPPRx = nb

  end subroutine
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine redo_nb_adj_SPPRx()
    implicit none

    call redo_nb_adj_( get_nb_SPHER() )

  end subroutine
  !--------------------------------------------------------------------------------------------------  
  
  !--------------------------------------------------------------------------------------------------  
  subroutine clean_memory_SPPRx
    implicit none
    integer(kind=4) :: i, j, k

    call clean_memory_inter_meca_()

    nb_SPHER       = 0
    nb_POLYR       = 0
    nb_SPPRx       = 0
    nb_vSPPRx      = 0
    nb_recup_SPPRx = 0

    if( allocated(box) ) then
      do i = 1, size(box,3)
        do j = 1, size(box,2)
          do k = 1, size(box,1)
            if( associated(box(k,j,i)%sp_which) ) deallocate(box(k,j,i)%sp_which)
            if( associated(box(k,j,i)%pr_which) ) deallocate(box(k,j,i)%pr_which)             
          end do
        end do
      end do
      deallocate(box)
    end if

    nb_rough_SPPRx = 0
    if( allocated(rough_SPPRx) ) deallocate(rough_SPPRx)

    ! Root, Current and Previous should always be null outside creation_tab_visu

    if( allocated(PRcoor) ) deallocate(PRcoor)

    Reac_SPPRx_MAX = 0.D0

    module_checked_ = .FALSE.
    check_SPPRx_    = .FALSE.

  end subroutine
  !--------------------------------------------------------------------------------------------------  
 
END MODULE SPPRx

