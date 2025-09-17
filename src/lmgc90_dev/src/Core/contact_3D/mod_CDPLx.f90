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
MODULE CDPLx                                         

  !!****h* LMGC90.CORE/CDPLx
  !! NAME
  !!  module CDPLx
  !! PURPOSE
  !!  
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/CYLND
  !!  LMGC90.CORE/PLANx
  !!****

  USE overall
  USE Algebra
  USE tact_behaviour
 
  USE CYLND
  USE PLANx
  
  use RBDY3, only : get_data, &
                    get_embeded_frame, &
                    get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color
  use MAILx, only : get_color_MAILx

  use parameters, only : i_cdplx, i_mailx, i_rbdy3, i_mbs3

  use inter_meca_3D

  implicit none
  
  private
  
  INTEGER :: nb_CYLND=0,nb_PLANx=0
  
  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!!!------------------------------------------------------------------------ 
  
  INTEGER :: nb_CDPLx=0,nb_vCDPLx,nb_recup_CDPLx
                                 
!!!------------------------------------------------------------------------ 
  ! adjac(icdtac)%icdan(iadj):
  ! serial number in this for adjacent contactor iadj
  ! to candidate contactor icdtac.
  ! For the definition of adjacent see below in 
  ! type T_verlt.
  ! When performing stock_rloc, verlt type is filled in
  ! according to adjac order, i.e.
  ! verlt(icdtac)%icdan(iadj)=adjac(icdtac)%icdan(iadj):
  
  type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   
  
  !------------------------------------------------------------------------  
  ! nb_adj(icdtac): number of adjacent pairs CYLND-PLANx
  ! to candidate contactor CYLND icdtac.
  
  integer             , dimension( : ), allocatable, target :: nb_adj
  
  !------------------------------------------------------------------------ 
  ! Let be some candidate contactor CYLND icdtac supported by some candidate 
  ! body icdbdy and some antagonist contactor CYLND iantac supported by some
  ! antagonist body ianbdy. The contactors may be close enough, within some 
  ! alert distance so that the the antagonist contactor is said 'adjacent' to
  ! the candidate contactor (the antagonist body is said as well adjacent to 
  ! the candidate body).
  
  ! A list of candidate antagonist pairs contactor-contactor
  ! adjacent to a given contactor is useful for quick access to
  ! data. Such a list is a generalisation of Verlet lists.
  ! verlt(icdtac)%adjsz: size of below arrays
  ! verlt(icdtac)%icdan(iadj): serial number in this for adjacent contactor iadj
  ! to candidate contactor icdtac.
  ! verlt(icdtac)%cdbdy(iadj): serial number of candidate body for adjacent contactor iadj. 
  ! verlt(icdtac)%cdtac(iadj): serial number of candidate contactor for adjacent contactor iadj.
  ! By definition verlt(icdtac)%cdtac(iadj)=icdtac;
  ! verlt(icdtac)%anbdy(iadj): serial number of antagonist body for adjacent contactor iadj. 
  ! verlt(icdtac)%antac(iadj): serial number of antagonist contactor for adjacent contactor iadj.
  ! verlt(icdtac)%rls(iadj): first tangential components of reaction;
  ! verlt(icdtac)%rlt(iadj): second tangential components of reaction;
  ! verlt(icdtac)%rln(iadj): normal component of reaction;
  ! verlt(icdtac)%vls(iadj): first tangential components of local velocy;
  ! verlt(icdtac)%vlt(iadj): second tangential components of local velocy;
  ! verlt(icdtac)%vln(iadj): normal component of local velocy;   
  ! verlt(icdtac)%status(iadj): status of contact labelled iadj;
  ! coor components of antagonist contact point;  
  ! verlt(icdtac)%suc(iadj): second tangential vector;
  ! verlt(icdtac)%tuc(iadj): first tangential vector;
  ! verlt(icdtac)%nuc(iadj): normal vector;

  type(T_verlet), dimension(:), allocatable, target ::verlt
  
!!!------------------------------------------------------------------------ 
  ! For quick sorting, disks are owned by boxes, sorting being 
  ! performed within a box and immediate surrounding boxes, see
  ! subroutine enumerate_CDPLx.
  ! box(ibox1,ibox2,ibox3)%popul: number of CYLND in box ibox1,ibox2;
  ! box(ibox1,ibox2,ibox3)%which(ipopul):
  ! rank in the list of contactors of CYLND labelled ipopul
  ! in box ibox1,ibox2;
  ! box(ibox1,ibox2,ibox3)%popul: number of PLANx in box ibox1,ibox2; 
  ! box(ibox1,ibox2,ibox3)%which(ipopul): 
  ! rank in the list of contactors of PLANx labelled ipopul
  ! in box ibox1,ibox2;

  TYPE T_box
     INTEGER                               :: SPpopul
     INTEGER, DIMENSION(:), POINTER        :: SPwhich
     INTEGER                               :: PLpopul
     INTEGER, DIMENSION(:), POINTER        :: PLwhich
  END TYPE T_box
  
  TYPE(T_box), DIMENSION(:,:,:),ALLOCATABLE  :: box

  REAL (kind=8)  :: maxray, minray, maxalert, meanradius
  REAL (kind=8)  :: Lbox,LBox_1,norm
  INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,minibox3,maxibox3,maxpopul

  !-------------------------------------------------------------------------
  !fd :-( c'est super crado ca !
  
  TYPE T_PLframe
     REAL(kind=8),DIMENSION(3) :: N
     REAL(kind=8),DIMENSION(3) :: T
     REAL(kind=8),DIMENSION(3) :: S  
  END TYPE T_PLframe
  
  TYPE(T_PLframe),DIMENSION(:),ALLOCATABLE,PRIVATE :: PLframe
  
  !------------------------------------------------------------------------
  
  TYPE T_rough_CDPLx   
     INTEGER      :: cd
     INTEGER      :: an
     INTEGER      :: isee
     REAL(kind=8) :: meff
     REAL(kind=8) :: reff            
  END TYPE T_rough_CDPLx
  
  TYPE(T_rough_CDPLx),DIMENSION(:),ALLOCATABLE   :: rough_CDPLx
  INTEGER                                        :: nb_rough_CDPLx
  
  TYPE T_link_rough_CDPLx
     TYPE(T_link_rough_CDPLx), POINTER :: p
     TYPE(T_rough_CDPLx)               :: val
     TYPE(T_link_rough_CDPLx), POINTER :: n
  END TYPE T_link_rough_CDPLx
  
  TYPE(T_link_rough_CDPLx),POINTER                  :: Root,Current,Previous

!!!------------------------------------------------------------------------

  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: CDcoor,PLcoor
  REAL(kind=8)                                    :: Reac_CDPLx_MAX=0.D0
  INTEGER,PRIVATE                                 :: ii,l_ii,iv
  INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
  LOGICAL,PRIVATE                                 :: write_creation_tab_visu
  
  real(kind=8), dimension(:), allocatable, target :: violation

  logical      :: module_checked_ = .FALSE.
  logical      :: check_CDPLx_    = .FALSE.

!!!-------------------------------------------------------------------------

  PUBLIC &
       coor_prediction_CDPLx,&
       CHECK_CDPLx,&
       RUN_CDPLx, &
       get_write_Vloc_Rloc_CDPLx, &
       read_ini_Vloc_Rloc_CDPLx,&
       write_xxx_Vloc_Rloc_CDPLx,&
       stock_rloc_CDPLx, &
       recup_rloc_CDPLx, &
       smooth_computation_CDPLx, &
       compute_box_CDPLx, &
       creation_tab_visu_CDPLx, &
       compute_contact_CDPLx, &
       display_prox_tactors_CDPLx
!       update_cohe_CDPLx

  PUBLIC &
       nullify_reac_CDPLx, nullify_vlocy_CDPLx,injj_CDPLx, prjj_CDPLx, vitrad_CDPLx,   & 
       get_nb_CDPLx, get_surf_CDPLx

  public clean_memory_CDPLx

  !rm for handler
  public get_this    , &
         set_nb_CDPLx, &
         redo_nb_adj_CDPLx, &
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


  !fd not used
  ! !------------------------------------------------------------------------
  ! subroutine get_antac_CDPLx( icdtac, iadj, iantac )
  !   implicit none

  !   integer, intent(in)  :: icdtac
  !   integer, intent(in)  :: iadj
  !   integer, intent(out) :: iantac
  
  !   iantac = verlt(icdtac)%lantac(iadj)

  ! end subroutine get_antac_CDPLx

  !fd not used
  ! !------------------------------------------------------------------------
  ! subroutine put_internal_CDPLx( icdan, internal )
  !   implicit none

  !   integer                   , intent(in) :: icdan
  !   real(kind=8), dimension(:), intent(in) :: internal

  !   this(icdan)%internal(1:max_internal_tact) = internal(1:max_internal_tact)

  ! end subroutine put_internal_CDPLx

  !fd not used
  ! !------------------------------------------------------------------------ 
  ! subroutine get_internal_CDPLx( icdan, internal )
  !   implicit none

  !   integer                   , intent(in)  :: icdan
  !   real(kind=8), dimension(:), intent(out) :: internal

  !   internal(1:max_internal_tact) = this(icdan)%internal(1:max_internal_tact)

  ! end subroutine get_internal_CDPLx

  !------------------------------------------------------------------------ 
  SUBROUTINE coor_prediction_CDPLx

    IMPLICIT NONE  
    INTEGER                     :: ibdy,itacty  
    REAL(kind=8),DIMENSION(3,3) :: localframe

    IF (smooth_method) THEN
       DO itacty=1,nb_CYLND
          CDcoor(1:3,itacty) = get_coor_CYLND(CYLND2bdyty(1,itacty),CYLND2bdyty(2,itacty))
       END DO
       DO ibdy=1,nb_PLANx
          PLcoor(:,ibdy) = get_coor_PLANx(ibdy)

          localframe = MATMUL(get_inertia_frameIni_PLANx(ibdy), &
               get_embeded_frame_PLANx(ibdy))

          PLframe(ibdy)%T(1:3)= localframe(1:3,1)
          PLframe(ibdy)%S(1:3)=-localframe(1:3,2)
          PLframe(ibdy)%N(1:3)= localframe(1:3,3)
       END DO
    ELSE 
       DO itacty=1,nb_CYLND
          CDcoor(1:3,itacty) = get_coorTT_CYLND(CYLND2bdyty(1,itacty),CYLND2bdyty(2,itacty))
       END DO
       DO ibdy=1,nb_PLANx
          PLcoor(1:3,ibdy) = get_coorTT_PLANx(ibdy)

          localframe = MATMUL(get_inertia_frameTT_PLANx(ibdy), &
               get_embeded_frame_PLANx(ibdy))
          
          PLframe(ibdy)%T(1:3)=  localframe(1:3,1)
          PLframe(ibdy)%N(1:3)=  localframe(1:3,3)
          PLframe(ibdy)%S(1:3)= -localframe(1:3,2)
     END DO
    END IF
    
  END SUBROUTINE coor_prediction_CDPLx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_CDPLx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_CDPLx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_CDPLx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_CDPLx
!!!-----------------------------------------------------------------------
  SUBROUTINE compute_box_CDPLx

   IMPLICIT NONE

   INTEGER                     :: isee,ibdy,errare
   REAL(kind=8)                :: radius_PLANx

                               !1234567890123456789012
    character(len=22) :: IAM = 'mod_CDPLx::compute_box'

   ! on fait ici les choses qui ne doivent que lorsque nb_CYLND change
   radius_PLANx = get_min_radius_PLANx()
   minray       = get_min_radius_CYLND()
   minray       = MIN(minray,radius_PLANx)

   radius_PLANx = get_max_radius_PLANx()
   maxray       = get_max_radius_CYLND()
   maxray       = MAX(maxray,radius_PLANx)

   !meanradius   = get_mean_radius_CYLND()

   IF (minray > maxray ) THEN
    CALL FATERR('CDPLx','compute_box: messing error computing minray and maxray')
   END IF

   ! computing largest alert distance between disks 
   maxalert=0.D0  
   DO isee=1,SIZE(see)
     IF (see(isee)%cdtac == 'CYLND' .AND. see(isee)%antac == 'PLANx') THEN
       maxalert=MAX(maxalert,see(isee)%alert)
     END IF
   END DO

   
   Lbox   = 1.01D0*(2.D0*maxray + maxalert)
   Lbox_1 = 1.D0/Lbox
   norm   = Lbox/minray

   IF (.NOT. ALLOCATED(adjac))THEN
     ALLOCATE(adjac(nb_CYLND),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO ibdy=1,nb_CYLND
       NULLIFY(adjac(ibdy)%icdan)
     END DO
   ELSE
     DO ibdy=1,nb_CYLND
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       NULLIFY(adjac(ibdy)%icdan)
     ENDDO
   ENDIF  
  
   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_CYLND),stat=errare)
   IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating nb_adj')
   END IF

   nb_adj=0

   ! CDcoor are coordinates of bodies owning CYLND to be used in selecting prox tactors
   IF (ALLOCATED(CDcoor)) DEALLOCATE(CDcoor)
   ALLOCATE(CDcoor(3,nb_CYLND),stat=errare)

   IF (ALLOCATED(PLcoor)) DEALLOCATE(PLcoor)
   ALLOCATE(PLcoor(3,nb_PLANx),stat=errare)

   IF (ALLOCATED(PLframe)) DEALLOCATE(PLframe)
   ALLOCATE(PLframe(nb_PLANx),stat=errare)
   

 END SUBROUTINE compute_box_CDPLx
!!!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_CDPLx

    IMPLICIT NONE
    
    INTEGER                   :: errare,icdtac,iantac,isee
    INTEGER                   :: icdan,ibdy
    INTEGER                   :: ibox1,ibox2,ibox3
    INTEGER                   :: ibox1cd,ibox2cd,ibox3cd
    INTEGER                   :: ibox1an,ibox2an,ibox3an,icdpop,ianpop
    REAL(kind=8)              :: Xleft,Xright,Yleft,Yright,Zup,Zdown
    REAL(kind=8)              :: rayan,raycd,adist,dist,masscd,data_cd(2)
    REAL(kind=8),DIMENSION(3) :: coorcd,cooran,axes
    LOGICAL                   :: visible
    CHARACTER(len=5)          :: cdcol,ancol
    CHARACTER(len=80)         :: cout
                               !1234567890123456789012345678
    character(len=28) :: IAM = 'mod_CDPLx::creation_tab_visu'


    ! Since the list of proximate contactors may not be updated at every time step,
    ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
    ! A warning condition prevents undue deallocation. 
    
    IF (ALLOCATED(box)) THEN
       DO ibox3=minibox3,maxibox3
          DO ibox2=minibox2,maxibox2
             DO ibox1=minibox1,maxibox1
                IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%SPwhich)) DEALLOCATE(box(ibox1,ibox2,ibox3)%SPwhich)
                IF (ASSOCIATED(box(ibox1,ibox2,ibox3)%PLwhich)) DEALLOCATE(box(ibox1,ibox2,ibox3)%PLwhich)
             ENDDO
          ENDDO
       ENDDO
       DEALLOCATE(box)
    ENDIF
    
    ! Building boxes for quick sorting 

    Xleft   =  1.D24
    Xright  = -1.D24
    Yleft   =  1.D24
    Yright  = -1.D24
    Zup     = -1.D24
    Zdown   =  1.D24
    
    DO ibdy=1,nb_CYLND
       visible=get_visible_CYLND(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = CDcoor(1:3,ibdy)
       Xleft = MIN(coorcd(1),Xleft )
       Xright= MAX(coorcd(1),Xright)
       Yleft = MIN(coorcd(2),Yleft )
       Yright= MAX(coorcd(2),Yright)
       Zup   = MAX(coorcd(3),Zup   )
       Zdown = MIN(coorcd(3),Zdown )
    END DO
    DO ibdy=1,nb_PLANx
       visible=get_visible_PLANx(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = PLcoor(1:3,ibdy)
       Xleft = MIN(coorcd(1),Xleft )
       Xright= MAX(coorcd(1),Xright)
       Yleft = MIN(coorcd(2),Yleft )
       Yright= MAX(coorcd(2),Yright)
       Zup   = MAX(coorcd(3),Zup   )
       Zdown = MIN(coorcd(3),Zdown )
    END DO
    
    minibox1 = 1
    maxibox1 = 1 + INT((Xright-Xleft)*Lbox_1)
    minibox2 = 1
    maxibox2 = 1 + INT((Yright-Yleft)*Lbox_1)
    minibox3 = 1
    maxibox3 = 1 + INT((Zup - Zdown )*Lbox_1)
    maxpopul = (1+INT(norm))*(1+INT(norm))*(1+INT(norm))
    
    maxpopul=MIN(maxpopul,nb_CYLND+nb_PLANx)
    
    ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2,minibox3:maxibox3),stat=errare)
    
    IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating box')
    END IF

    DO ibox3=minibox3,maxibox3
       DO ibox2=minibox2,maxibox2
          DO ibox1=minibox1,maxibox1
             
             box(ibox1,ibox2,ibox3)%SPpopul=0
             ALLOCATE(box(ibox1,ibox2,ibox3)%SPwhich(maxpopul),stat=errare)
             IF (errare /=0 ) THEN
                write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%SPwhich'
                call faterr(IAM,cout)
             END IF
             box(ibox1,ibox2,ibox3)%PLpopul=0
             ALLOCATE(box(ibox1,ibox2,ibox3)%PLwhich(maxpopul),stat=errare)
             IF (errare /=0 ) THEN
                write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%PLwhich'
                call faterr(IAM,cout)
             END IF
          END DO
       END DO
    END DO
    
    ! filling boxes   
    DO ibdy=1,nb_CYLND
       visible=get_visible_CYLND(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = CDcoor(1:3,ibdy)
       ibox1=1+INT((coorcd(1)-Xleft )*Lbox_1)
       ibox2=1+INT((coorcd(2)-Yleft )*Lbox_1)
       ibox3=1+INT((coorcd(3)-Zdown )*Lbox_1)
       IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2   &
            .OR. ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
          write(cout,'(A,I0,A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
          call logmes(cout)
          write(cout,'(A,I0,A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
          call logmes(cout)
          write(cout,'(A,I0,A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
          call logmes(cout)
          write(cout,'(A13,I5,A13)')'  body CYLND ',ibdy,' out of boxes'
          call faterr(IAM,cout)
       END IF
       box(ibox1,ibox2,ibox3)%SPpopul=box(ibox1,ibox2,ibox3)%SPpopul+1
       if( box(ibox1,ibox2,ibox3)%SPpopul > size(box(ibox1,ibox2,ibox3)%SPwhich) ) then
           call faterr(IAM, "Estimated max popul limit reached for CD.")
       end if
       box(ibox1,ibox2,ibox3)%SPwhich(box(ibox1,ibox2,ibox3)%SPpopul)=ibdy
    END DO
    
    DO ibdy=1,nb_PLANx
       visible=get_visible_PLANx(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd=PLcoor(1:3,ibdy)
       ibox1=1+INT((coorcd(1)-Xleft )*Lbox_1)
       ibox2=1+INT((coorcd(2)-Yleft )*Lbox_1)
       ibox3=1+INT((coorcd(3)-Zdown )*Lbox_1)
       IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2   &
            .OR. ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
          write(cout,'(A,I0,A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
          call logmes(cout)
          write(cout,'(A,I0,A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
          call logmes(cout)
          write(cout,'(A,I0,A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
          call logmes(cout)
          write(cout,'(A13,I5,A13)')'  body CYLND ',ibdy,' out of boxes'
          call faterr(IAM,cout)
       END IF
       
       box(ibox1,ibox2,ibox3)%PLpopul=box(ibox1,ibox2,ibox3)%PLpopul+1
       if( box(ibox1,ibox2,ibox3)%PLpopul > size(box(ibox1,ibox2,ibox3)%PLwhich) ) then
           call faterr(IAM, "Estimated max popul limit reached for PL.")
       end if
       box(ibox1,ibox2,ibox3)%PLwhich(box(ibox1,ibox2,ibox3)%PLpopul)=ibdy
       
    END DO
    
    nb_rough_CDPLx=0

    NULLIFY(Root) 
    NULLIFY(Current)
    NULLIFY(Previous)

    DO ibox3cd=minibox3,maxibox3
       DO ibox2cd=minibox2,maxibox2
          DO ibox1cd=minibox1,maxibox1 
             DO icdpop=1,box(ibox1cd,ibox2cd,ibox3cd)%SPpopul
                icdtac=box(ibox1cd,ibox2cd,ibox3cd)%SPwhich(icdpop)
                cdcol=get_color_CYLND(icdtac)
                ! box loop investigating antagonist PLANx
                DO ibox3an=MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                   DO ibox2an=MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
                      DO ibox1an=MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)            
                         DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%PLpopul
                            iantac=box(ibox1an,ibox2an,ibox3an)%PLwhich(ianpop)
                            !          if (iantac .le. icdtac) cycle
                            ancol=get_color_PLANx(iantac)
                            isee=get_isee('RBDY3','CYLND',cdcol, &
                                          get_body_model_name_from_id(planx2bdyty(3,iantac)),'PLANx',ancol)
                            IF (isee /= 0) THEN
                               adist=see(isee)%alert 
                               ! checking ROUGHLY distance against alert distance           
                               coorcd = CDcoor(1:3,icdtac)
                               CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)

                               cooran = PLcoor(1:3,iantac)
                               axes    = get_axes_PLANx(iantac)
                               rayan   = axes(3)
                               
                               ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                               ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                               ! results might be different up to some non significant figures, but when comparing to
                               ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                               
                               
                               adist=0.1005D+01*adist + &
                                  data_cd(1) + data_cd(2) + &
                                  rayan 
                                  
                               dist=  (coorcd(1)-cooran(1))*(coorcd(1)-cooran(1)) &
                                    + (coorcd(2)-cooran(2))*(coorcd(2)-cooran(2)) &
                                    + (coorcd(3)-cooran(3))*(coorcd(3)-cooran(3))
                               
!                            IF (       dabs(coorcd(1)-cooran(1)) <= adist &
!                                 .AND. dabs(coorcd(2)-cooran(2)) <= adist &
!                                 .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN
!                                 nb_rough_CDPLx = nb_rough_CDPLx+1
!                               IF ( nb_rough_CDPLx == 1) THEN
!                                  ALLOCATE(Root)
!                                  Current => Root
!                                  NULLIFY(Root%p)
!                               ELSE
!                                  ALLOCATE(Current)
!                                  Previous%n => Current
!                               END IF

!                               !if (dist<adist**2) then
                               nb_rough_CDPLx=nb_rough_CDPLx+1
                               IF ( nb_rough_CDPLx == 1) THEN
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
                               
                               Current%p => Previous
                               NULLIFY(Current%n)
                               Previous => Current
!                            END IF
                            END IF
                         END DO
                      END DO
                   END DO
                END DO
             END DO
          ENDDO
       ENDDO
    ENDDO
    
    WRITE(cout,'(4X,I10,A20)') nb_rough_CDPLx,' CDPLx roughly found'
    call logmes(cout)

    IF (ALLOCATED(rough_CDPLx)) DEALLOCATE(rough_CDPLx)
    ALLOCATE(rough_CDPLx(nb_rough_CDPLx))     ! the visibility array used in compute_contact is allocated
    
    IF (ALLOCATED(this)) DEALLOCATE(this)
    ALLOCATE(this(2*nb_rough_CDPLx))          ! the oversized array this is temporaly allocated
    
    DO icdan=nb_rough_CDPLx,1,-1
       
       Previous => Current%p
       rough_CDPLx(icdan)%cd     = Current%val%cd
       rough_CDPLx(icdan)%an     = Current%val%an
       rough_CDPLx(icdan)%isee   = Current%val%isee
       
       masscd=get_mass_CYLND(CYLND2bdyty(1,Current%val%cd))
       rough_CDPLx(icdan)%meff = masscd
       
       CALL get_data(cylnd2bdyty(1,Current%val%cd),cylnd2bdyty(2,Current%val%cd),data_cd)
       rough_CDPLx(icdan)%reff = data_cd(1)                    ! A DEMANDER A FRED
              
       DEALLOCATE(Current)
       Current => Previous
    END DO
    
    NULLIFY(Root)
    
    nb_CDPLx = nb_rough_CDPLx
    
  END SUBROUTINE creation_tab_visu_CDPLx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_contact_CDPLx
    
    IMPLICIT NONE  
    INTEGER                     :: errare,icdtac,iantac,isee
    INTEGER                     :: icdan,iadj,ibdy,itac
    REAL(kind=8)                :: raycd,adist,ovlap,data_cd(2),gap
    REAL(kind=8)                :: norm2,dist2,norm1,dist1,norm3,dist3
    REAL(kind=8),DIMENSION(3)   :: xco,sep,axe,coorcd,cooran,sept,sepn
    REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
    REAL(kind=8),DIMENSION(3,3) :: Rc,cdframe,anframe
    REAL(kind=8),DIMENSION(3)   :: cdlev,anlev,cd_shift,an_shift
    REAL(kind=8),DIMENSION(3)   :: coor_cd_S1,coor_cd_S2
    REAL(kind=8),DIMENSION(3,3) :: localframe_cd,localframe_an,oframe,eframe
    character(len=80)           :: cout
    
    
    icdan   = 0        
    nb_CDPLx= 0
    nb_adj  = 0
    
    IF (nb_rough_CDPLx.NE.0) THEN
       
       DO itac=1,nb_rough_CDPLx
          
          icdtac = rough_CDPLx(itac)%cd
          iantac = rough_CDPLx(itac)%an
          isee   = rough_CDPLx(itac)%isee
          
          adist=see(isee)%alert 
          
          coorcd = CDcoor(1:3,icdtac)
          oframe = get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac))
          eframe = get_embeded_frame(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac))
          localframe_cd = matmul(oframe,eframe)

          !call GRAMM_SCHMIDT(localframe_cd(1,:),localframe_cd(2,:),localframe_cd(3,:))
          
          CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
          raycd = data_cd(1)

          cooran = PLcoor(1:3,iantac)
          axe     = get_axes_PLANx(iantac)
          localframe_an = get_inertia_frameTT_PLANx(iantac)
          
          ! Position des centres de chaque spheres du cylindre cd dans le repere global
          coor_cd_S1(:) = coorcd(:) + data_cd(1)*localframe_cd(:,3)
          coor_cd_S2(:) = coorcd(:) - data_cd(1)*localframe_cd(:,3)

          dist3 = data_cd(2) + axe(3)
          
          ! on teste la sphere 1 du cylindre cd
          sep = coor_cd_S1 - cooran

          norm3 =  sep(1)*PLframe(iantac)%N(1) + sep(2)*PLframe(iantac)%N(2) + sep(3)*PLframe(iantac)%N(3)
          
!          print*,'candidat :',icdtac, 'antagoniste :',icdtac
!          print*, 'norm3',norm3, ' dist3 : ',dist3
!          print*, 'Normal : ', PLframe(iantac)%N,localframe_an(:,3)
!          print*, 'nb_rough : ',nb_rough_CDPLx
          
          if ((DABS(norm3)-dist3) < adist) then                     ! Alors la sphere 1 est au meme niveau que le plan, reste a savoir si elle est bornee
            dist1 = raycd + axe(1) 
            norm1 =  sep(1)*PLframe(iantac)%T(1) + sep(2)*PLframe(iantac)%T(2) + sep(3)*PLframe(iantac)%T(3)
             
            dist2 = raycd + axe(2)
            norm2 =  sep(1)*PLframe(iantac)%S(1) + sep(2)*PLframe(iantac)%S(2) + sep(3)*PLframe(iantac)%S(3)
             
            if ((DABS(norm1) < dist1).AND.(DABS(norm2) < dist2)) then      ! Ben oui, y a contact
              gap = dabs(norm3)-dist3
              xco = coor_cd_S1 - PLframe(iantac)%N*(data_cd(2)+gap)
              call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
              
!              print*, 'CONTACT CDPL 1'
!              print*, 'norm3',norm3, ' dist3 : ',dist3
!              print*, 'norm2',norm2, ' dist3 : ',dist2
!              print*, 'norm1',norm2, ' dist1 : ',dist1
              
!              print*,'gap : ' , gap
!              print*,'xco ' , xco
              
            end if
          end if
          
          ! on teste la sphere 2 du cylindre cd
          sep = coor_cd_S2 - cooran

          norm3 =  sep(1)*PLframe(iantac)%N(1) + sep(2)*PLframe(iantac)%N(2) + sep(3)*PLframe(iantac)%N(3)
          if ((DABS(norm3)-dist3) < adist) then                     ! Alors la sphere 2 est au meme niveau que le plan, reste a savoir si elle est bornee
            dist1 = raycd + axe(1) 
            norm1 =  sep(1)*PLframe(iantac)%T(1) + sep(2)*PLframe(iantac)%T(2) + sep(3)*PLframe(iantac)%T(3)
             
            dist2 = raycd + axe(2)
            norm2 =  sep(1)*PLframe(iantac)%S(1) + sep(2)*PLframe(iantac)%S(2) + sep(3)*PLframe(iantac)%S(3)
             
            if ((DABS(norm1) < dist1).AND.(DABS(norm2) < dist2)) then      ! Ben oui, y a contact
              gap = dabs(norm3)-dist3
              xco = coor_cd_S2 - PLframe(iantac)%N*(data_cd(2)+gap)
              call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
            end if
          end if

       END DO
       nb_CDPLx=icdan
    END IF

    WRITE(cout,'(1X,I10,A12)') nb_CDPLx,' CDPLx found'
    call logmes(cout)

    DO ibdy=1,nb_CYLND
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       IF (nb_adj(ibdy) /= 0) THEN
          ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
          IF (errare /=0 ) THEN
             write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
             call faterr('mod_CDPLx::compute_cotact',cout)
          END IF
       END IF
    END DO
    
    DO icdan=1,nb_CDPLx
       adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
    END DO

    IF (ALLOCATED(violation)) DEALLOCATE(violation)
    ALLOCATE(violation(nb_CDPLx),stat=errare)
    
  END SUBROUTINE compute_contact_CDPLx
!!!------------------------------------------------------------------------
  SUBROUTINE smooth_computation_CDPLx

    IMPLICIT NONE
    INTEGER          :: icdan

    DO icdan=1,nb_CDPLx
       
       CALL compute_3D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vlsBEGIN,this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            this(icdan)%reff,this(icdan)%meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vls,this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

    END DO
    
    DO icdan=1,nb_CDPLx  
       CALL nullify_reac_CDPLx(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_CDPLx
       CALL injj_CDPLx(icdan,H*this(icdan)%rls,H*this(icdan)%rlt,H*this(icdan)%rln,iIreac)
    END DO
    
  END SUBROUTINE smooth_computation_CDPLx
!!!------------------------------------------------------------------------
  subroutine display_prox_tactors_CDPLx
    implicit none
    integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
    character(len=5) :: cdmodel, anmodel

    nb_CYLND=get_nb_CYLND()

    do icdtac = 1,nb_CYLND    

       icdbdy = this(icdan)%icdbdy
       cdmodel = get_body_model_name_from_id( cylnd2bdyty(3,icdtac) )

       do iadj=1,nb_adj(icdtac)         

          icdan  = adjac(icdtac)%icdan(iadj)

          ianbdy = this(icdan)%ianbdy
          iantac = this(icdan)%iantac
          anmodel = get_body_model_name_from_id( planx2bdyty(3,iantac) )
          WRITE(*,'(A1)')' '
          WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
          !123456789012345678901234567890123456789012345678901234567890123456789012
          WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
          WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               cdmodel,icdbdy,'CYLND',icdtac,see(this(icdan)%isee)%behav,  &
               anmodel,ianbdy,'PLANx',iantac
          
          WRITE(*,104) 's(1)=',this(icdan)%suc(1)  ,'t(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)
          WRITE(*,104) 's(2)=',this(icdan)%suc(2)  ,'t(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)
          WRITE(*,104) 's(3)=',this(icdan)%suc(3)  ,'t(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)
          WRITE(*,104) 'rls =',this(icdan)%rls     ,'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln
          WRITE(*,104) 'vls-=',this(icdan)%vlsBEGIN,'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN
          WRITE(*,'(29X,A5,D14.7)')'gTT-=',this(icdan)%gapTTbegin
          WRITE(*,'(A1)')' '               
       END DO
    END DO
    
104 FORMAT(27X,3(2X,A5,D14.7))
    
  end subroutine display_prox_tactors_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE stock_rloc_CDPLx

    IMPLICIT NONE
    INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    INTEGER            :: errare
    CHARACTER(len=80)  :: cout
    CHARACTER(len=21)  :: IAM = 'mod_CDPLx::stock_rloc'
    
    nb_CYLND = get_nb_CYLND()
    
    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_CYLND),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,'error allocating verlt')
       END IF
       DO icdtac=1,nb_CYLND
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
       DO icdtac=1,nb_CYLND
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
    ! icdbdy : serial number of candidate body for contact icdan
    ! icdtac : serial number of candidate contactor for contact icdan
    ! ianbdy : serial number of antagonist body for contact icdan	   
    ! iantac : serial number of antagonist contactor for contact icdan 
    ! iadj   : serial adjacent number of pair contactor adjacent to 
    !          candidate contactor for contact icdan 

    DO icdan=1,nb_CDPLx
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       iadj   = this(icdan)%iadj
       
       verlt(icdtac)%icdan(iadj)    = icdan
       verlt(icdtac)%cdbdy          = cylnd2bdyty(1,icdtac)
       verlt(icdtac)%cdtac          = cylnd2bdyty(2,icdtac)
       verlt(icdtac)%cdmodel        = cylnd2bdyty(3,icdtac)
       verlt(icdtac)%anbdy(iadj)    = planx2bdyty(1,iantac)
       verlt(icdtac)%antac(iadj)    = planx2bdyty(2,iantac)
       verlt(icdtac)%anmodel(iadj)  = planx2bdyty(3,iantac)

       verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
       verlt(icdtac)%ansci(iadj)    = this(icdan)%iansci

       verlt(icdtac)%status(iadj)   = this(icdan)%status

       verlt(icdtac)%rls(iadj)      = this(icdan)%rls/H
       verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
       verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
       verlt(icdtac)%vls(iadj)      = this(icdan)%vls
       verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
       verlt(icdtac)%vln(iadj)      = this(icdan)%vln

       verlt(icdtac)%tuc(:,iadj)    = this(icdan)%tuc(:)
       verlt(icdtac)%nuc(:,iadj)    = this(icdan)%nuc(:)
       verlt(icdtac)%suc(:,iadj)    = this(icdan)%suc(:)

       verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
       verlt(icdtac)%coor(1:3,iadj) = this(icdan)%coor(1:3)

       verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
    END DO

    nb_vCDPLx = nb_CDPLx

    WRITE(cout,'(1X,I10,A12)') nb_vCDPLx,' stock CDPLx'
    call logmes(cout)

  END SUBROUTINE stock_rloc_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_CDPLx

    IMPLICIT NONE
    INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    CHARACTER(len=21)  :: IAM = 'mod_CDPLx::recup_rloc'
    CHARACTER(len=80)  :: cout
    
    if (.not. allocated(verlt)) then
       call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
       return
    end if
    
    nb_recup_CDPLx = 0
    
    DO icdan=1,nb_CDPLx
       this(icdan)%rls = 0.D0
       this(icdan)%rlt = 0.D0
       this(icdan)%rln = 0.D0
       this(icdan)%statusBEGIN= i_nknow

       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       IF (verlt(icdtac)%adjsz /= 0) THEN

          if ( verlt(icdtac)%cdbdy  == cylnd2bdyty(1,icdtac) .and. &
               verlt(icdtac)%cdtac  == cylnd2bdyty(2,icdtac) .and. &
               verlt(icdtac)%cdmodel== cylnd2bdyty(3,icdtac) ) then
            do iadj=1,verlt(icdtac)%adjsz
               if ( verlt(icdtac)%anbdy(iadj)  == planx2bdyty(1,iantac) .and. &
                    verlt(icdtac)%antac(iadj)  == planx2bdyty(2,iantac) .and. &
                    verlt(icdtac)%anmodel(iadj)== planx2bdyty(3,iantac)  ) then

                  this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
                  this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
                  this(icdan)%rln = verlt(icdtac)%rln(iadj)*H

                  this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

                  this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)

                  nb_recup_CDPLx = nb_recup_CDPLx + 1
                  EXIT
               END IF
            END DO
         end if
       END IF
    END DO

    WRITE(cout,'(1X,I10,A12)') nb_recup_CDPLx,' recup CDPLx'
    call logmes(cout)

  END SUBROUTINE recup_rloc_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE read_ini_Vloc_Rloc 
    
    IMPLICIT NONE
    INTEGER                          :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    integer                          :: cdmodel, anmodel
    INTEGER                          :: errare,ibehav,nb_internal,i_internal,icdtact
    REAL(kind=8)                     :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
    character(len=10)                :: cdbdy,anbdy
    character(len=5)                 :: cdtac,antac,behav,sttus
    CHARACTER(len=103) :: cout
    CHARACTER(len=29)                :: IAM = 'mod_CDPLx::read_ini_Vloc_Rloc'
    
    nb_CYLND = get_nb_CYLND()
    
    ! first reading: sizing verlt
    ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
    ! For this purpose nb_adj is introduced.
    errare=0
    IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_CYLND),stat=errare)
    IF (errare /=0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF

    nb_adj = 0

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13)/= 'CDPLx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
            cdbdy(1:5),icdbdy,cdtac,icdtac, &
            behav,                          &
            anbdy(1:5),ianbdy,antac,iantac, &
            sttus
       IF (cdtac.NE.'CYLND'.OR.antac.NE.'PLANx') CYCLE
       DO icdtact=1,nb_CYLND
          cdmodel = get_body_model_id_from_name( cdbdy )
          if ( cylnd2bdyty(1,icdtact) == icdbdy .and. &
               cylnd2bdyty(2,icdtact) == icdtac .and. &
               cylnd2bdyty(3,icdtact) == cdmodel ) then
             nb_adj(icdtact)=nb_adj(icdtact)+1       
             exit
          end if
       END DO
    END DO
    
    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_CYLND),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,'error allocating verlt')
       END IF
       DO icdtac=1,nb_CYLND
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
       DO icdtac=1,nb_CYLND
          call free_verlet_(icdtac)
          verlt(icdtac)%adjsz=0
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
    icdan = 0

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13)/= 'CDPLx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
            cdbdy,icdbdy,cdtac,icdtac,                                                 &
            behav,                                                              &
            anbdy,ianbdy,antac,iantac,                                                 &
            sttus
       IF (cdtac.NE.'CYLND'.OR. antac.NE.'PLANx') CYCLE

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       DO icdtact=1,nb_CYLND
          if ( cylnd2bdyty(1,icdtact) == icdbdy .and. &
               cylnd2bdyty(2,icdtact) == icdtac .and. &
               cylnd2bdyty(3,icdtact) == cdmodel ) then

             icdan = icdan + 1

             nb_adj(icdtact) = nb_adj(icdtact) + 1 

             verlt(icdtact)%icdan( nb_adj(icdtact) ) = icdan

             verlt(icdtact)%cdbdy  = icdbdy
             verlt(icdtact)%cdtac  = icdtac
             verlt(icdtact)%cdmodel= cdmodel

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
    END DO
 
    nb_vCDPLx=0
    
    DO icdtact=1,nb_CYLND
       nb_vCDPLx = nb_vCDPLx + nb_adj(icdtact)
       
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
    INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
    INTEGER :: nfich

    character(len=20) :: fmt
    character(len=10) :: cdmodel, anmodel
    
    do icdtac = 1, nb_CYLND    

       cdmodel = get_body_model_name_from_id( cylnd2bdyty(3,icdtac) )

       do iadj = 1, nb_adj(icdtac)         

          icdan  = adjac(icdtac)%icdan(iadj)
          iantac = this(icdan)%iantac
          anmodel = get_body_model_name_from_id( planx2bdyty(3,iantac) )

          WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CDPLx',icdan
          WRITE(nfich,'(A76)') ' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
          WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
               !pta old fashion: 'RBDY3',CYLND2bdyty(1,icdtac),'CYLND',CYLND2bdyty(2,icdtac),  &
               cdmodel, get_visibleID_CYLND(icdtac),'CYLND',cylnd2bdyty(2,icdtac),  &
               see(this(icdan)%isee)%behav,  &
               !pta old fashion 'RBDY3',planx2bdyty(1,iantac),'PLANx',planx2bdyty(2,iantac),  &
               anmodel, get_visibleID_PLANx(iantac),'PLANx',planx2bdyty(2,iantac),  &
               get_contact_status_name_from_id(this(icdan)%status)
          WRITE(nfich,104) 'rls/H',this(icdan)%rls/H  ,'rlt/H',this(icdan)%rlt/H  ,'rln/H',this(icdan)%rln/H
          WRITE(nfich,104) 'vls =',this(icdan)%vls    ,'vlt =',this(icdan)%vlt    ,'vln =',this(icdan)%vln  
          WRITE(nfich,103) 'gTT =',this(icdan)%gapTT
          WRITE(nfich,104) 's(1)=',this(icdan)%suc(1) ,'s(2)=',this(icdan)%suc(2) ,'s(3)=',this(icdan)%suc(3)
          WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1) ,'t(2)=',this(icdan)%tuc(2) ,'t(3)=',this(icdan)%tuc(3)
          WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1) ,'n(2)=',this(icdan)%nuc(2) ,'n(3)=',this(icdan)%nuc(3)
          WRITE(nfich,104) 'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',this(icdan)%coor(3)
          
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
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_reac_CDPLx(icdan,storage)

    IMPLICIT NONE
    INTEGER,INTENT(in):: icdan 
    INTEGER           :: icdbdy,ianbdy,storage
    
    icdbdy = this(icdan)%icdbdy
    ianbdy = this(icdan)%ianbdy
    
    CALL nullify_reac_CYLND(icdbdy,storage)   
    CALL nullify_reac_PLANx(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_reac_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_CDPLx(icdan,storage)
    
    IMPLICIT NONE
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy,storage
    
    icdbdy = this(icdan)%icdbdy
    ianbdy = this(icdan)%ianbdy

    CALL nullify_vlocy_CYLND(icdbdy,storage)
    CALL nullify_vlocy_PLANx(this(icdan)%iantac,storage)
    
  END SUBROUTINE nullify_vlocy_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE vitrad_CDPLx( icdan, storage, need_full_vlocy )

    IMPLICIT NONE
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy,storage
    logical, optional  :: need_full_vlocy
    
    icdbdy = this(icdan)%icdbdy
    ianbdy = this(icdan)%ianbdy
    
    CALL comp_vlocy_CYLND(icdbdy,storage)
    CALL comp_vlocy_PLANx(this(icdan)%iantac,storage)
    
  END SUBROUTINE vitrad_CDPLx
!!!------------------------------------------------------------------------  
  SUBROUTINE injj_CDPLx(icdan,rsik,rtik,rnik,storage)
 
    IMPLICIT NONE
    INTEGER     ,INTENT(in)    :: icdan
    REAL(kind=8),INTENT(in)    :: rsik,rtik,rnik
    INTEGER,     DIMENSION(6)  :: cdccdof,anccdof
    REAL(kind=8),DIMENSION(6)  :: cdreac, anreac
    INTEGER                    :: icdbdy,ianbdy
    INTEGER                    :: storage
    
    icdbdy    = this(icdan)%icdbdy
    ianbdy    = this(icdan)%ianbdy
    cdccdof(1)= 1
    anccdof(1)= 1
    cdreac(1) = rsik*this(icdan)%suc(1)+rtik*this(icdan)%tuc(1)+rnik*this(icdan)%nuc(1)
    anreac(1) =-cdreac(1)
    cdccdof(2)= 2
    anccdof(2)= 2
    cdreac(2) = rsik*this(icdan)%suc(2)+rtik*this(icdan)%tuc(2)+rnik*this(icdan)%nuc(2)
    anreac(2) =-cdreac(2)
    cdccdof(3)= 3
    anccdof(3)= 3
    cdreac(3) = rsik*this(icdan)%suc(3)+rtik*this(icdan)%tuc(3)+rnik*this(icdan)%nuc(3)
    anreac(3) =-cdreac(3)

    cdccdof(4)= 4
    anccdof(4)= 4
    cdccdof(5)= 5
    anccdof(5)= 5
    cdccdof(6)= 6
    anccdof(6)= 6
    
    cdreac(4) = this(icdan)%Gcds(1)*rsik+this(icdan)%Gcdt(1)*rtik+this(icdan)%Gcdn(1)*rnik
    cdreac(5) = this(icdan)%Gcds(2)*rsik+this(icdan)%Gcdt(2)*rtik+this(icdan)%Gcdn(2)*rnik
    cdreac(6) = this(icdan)%Gcds(3)*rsik+this(icdan)%Gcdt(3)*rtik+this(icdan)%Gcdn(3)*rnik
    
    anreac(4) =-this(icdan)%Gans(1)*rsik-this(icdan)%Gant(1)*rtik-this(icdan)%Gann(1)*rnik
    anreac(5) =-this(icdan)%Gans(2)*rsik-this(icdan)%Gant(2)*rtik-this(icdan)%Gann(2)*rnik
    anreac(6) =-this(icdan)%Gans(3)*rsik-this(icdan)%Gant(3)*rtik-this(icdan)%Gann(3)*rnik
    
    CALL add_reac_CYLND(icdbdy,cdccdof,cdreac,storage)
    CALL add_reac_PLANx(this(icdan)%iantac,anccdof,anreac,storage)

  END SUBROUTINE injj_CDPLx
!!!------------------------------------------------------------------------  
  SUBROUTINE prjj_CDPLx(icdan,vsik,vtik,vnik,storage)
 
    IMPLICIT NONE
    INTEGER     ,INTENT(in)   :: icdan
    REAL(kind=8),INTENT(out)  :: vsik,vtik,vnik
    INTEGER                   :: icdbdy,ianbdy
    INTEGER                   :: storage
    REAL(kind=8),DIMENSION(6) :: Vcd,Van

    icdbdy = this(icdan)%icdbdy
    ianbdy = this(icdan)%ianbdy
    Vcd    = get_vlocy_CYLND(icdbdy,storage)
    Van    = get_vlocy_PLANx(this(icdan)%iantac,storage)      

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

 END SUBROUTINE prjj_CDPLx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_CDPLx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_CDPLx = nb_CDPLx
    CASE(i_verlet_tactor)
       get_nb_CDPLx = nb_vCDPLx
    CASE(i_rough_tactor)
       get_nb_CDPLx = nb_rough_CDPLx
    CASE(i_recup_tactor)
       get_nb_CDPLx = nb_recup_CDPLx
    END SELECT

  END FUNCTION get_nb_CDPLx
!!!------------------------------------------------------------------------ 
  SUBROUTINE CDPLx2ENT(icdan,icdent,ianent)

    IMPLICIT NONE
    INTEGER :: icdan,icdent,ianent
   
    icdent = get_ENT_CYLND(this(icdan)%icdbdy)
    ianent = get_ENT_PLANx(this(icdan)%iantac)

  END SUBROUTINE CDPLx2ENT
!!!------------------------------------------------------------------------ 
  SUBROUTINE CDPLx2CYLND(icdan,icdtac)

    IMPLICIT NONE
    INTEGER :: icdan,icdtac
    
    icdtac = this(icdan)%icdtac
    
  END SUBROUTINE CDPLx2CYLND
!!!------------------------------------------------------------------------ 
  SUBROUTINE CDPLx2PLANx(icdan,iantac)

    IMPLICIT NONE
    INTEGER :: icdan,iantac
    
    iantac = this(icdan)%iantac
    
  END SUBROUTINE CDPLx2PLANx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_CDPLx()

    IMPLICIT NONE
    
    RUN_CDPLx = RUN_TACTOR

  END FUNCTION RUN_CDPLx
!!!------------------------------------------------------------------------
  logical function CHECK_CDPLx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_CDPLx = check_CDPLx_
      return
    end if

    con_pedigree%module_name = 'CDPLx'

    con_pedigree%id_cdan  = i_cdplx
    con_pedigree%id_cdtac = i_cylnd
    con_pedigree%id_antac = i_planx

    cdtact2bdyty => cylnd2bdyty
    antact2bdyty => planx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_CYLND = get_nb_CYLND()
    nb_PLANx = get_nb_PLANx()
    if( nb_CYLND == 0 .OR. nb_PLANx == 0 ) then
      CHECK_CDPLx = check_CDPLx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'CYLND' .and. see(isee)%antac == 'PLANx') then
        check_CDPLx_ = .true.
        exit
      end if
    end do

    CHECK_CDPLx = check_CDPLx_
    return

  end function CHECK_CDPLx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_CDPLx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_CDPLx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_CDPLx
!!!------------------------------------------------------------------------ 
  REAL(kind=8) FUNCTION get_surf_CDPLx(icdan)

    IMPLICIT NONE
    INTEGER      :: icdan
    !***
    ! tableau qui contient half-height, radius
    REAL(kind=8) :: data_cd(2)

    CALL get_data(cylnd2bdyty(1,this(icdan)%icdtac),cylnd2bdyty(2,this(icdan)%icdtac),data_cd)
    
    get_surf_CDPLx = data_cd(1)*data_cd(2)

  END FUNCTION get_surf_CDPLx
!!!--------------------------------------------------------------------
!  SUBROUTINE update_cohe_CDPLx(icdan,cohe)!

!   IMPLICIT NONE
!   INTEGER      :: icdan,itact
!   REAL(kind=8) :: cohe,WScd,WSan

!   itact = this(icdan)%icdtac

!   WScd = get_WS_CYLND(CYLND2bdyty(1,itact),CYLND2bdyty(2,itact))

!   itact = this(icdan)%iantac

!   WSan = get_WS_PLANx(itact)

!   IF (ABS(WScd+WSan).LT.1.D-16) THEN
!      cohe = 0.D0
!   ELSE
!      cohe = (WSan*WScd)/(WScd+WSan)
!   END IF

! END SUBROUTINE update_cohe_CDPLx
!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
  FUNCTION comp_provec(a,b)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: a,b,comp_provec 

    comp_provec(1)=(a(2)*b(3)) - (a(3)*b(2))
    comp_provec(2)=(a(3)*b(1)) - (a(1)*b(3))
    comp_provec(3)=(a(1)*b(2)) - (a(2)*b(1))

  END FUNCTION comp_provec

  REAL(kind=8) FUNCTION comp_norm(x)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: x 

    comp_norm = dsqrt(DOT_PRODUCT(x,x))

  END FUNCTION comp_norm

  SUBROUTINE comp_rep(t,n,s)
    IMPLICIT NONE
    REAL(kind=8),DIMENSION(3) :: n,t,s
    REAL(kind=8)              :: norm

    IF (dabs(n(1)) < 1.d-15) THEN

      ! fd on est dans le plan y-z

      t(1) =  n(1)
      t(2) = -n(3)
      t(3) =  n(2)

      norm= comp_norm(t)
      t = t/norm

      s=comp_provec(t,n)

      RETURN

    ENDIF

    IF (dabs(n(2)) < 1.d-15) THEN

      ! fd on est dans le plan z-x
      t(2) =  n(2)
      t(1) = -n(3)
      t(3) =  n(1)

      norm= comp_norm(t)
      t = t/norm

      s=comp_provec(t,n)

      RETURN

    ENDIF

    IF (dabs(n(3)) < 1.d-15) THEN

      ! fd on est dans le plan x-y
      t(1) = -n(2)
      t(2) =  n(1)
      t(3) =  n(3)

      norm= comp_norm(t)
      t = t/norm

      s=comp_provec(t,n)

      RETURN

    ENDIF

    !fd cas general 

    !fd on genere un premier vecteur perpendiculaire
    t(1) = 1.; t(2) = 0. ; t(3) = 0.
    s = comp_provec(t,n)
    norm = comp_norm(s)
    t=s/norm

    s=comp_provec(t,n)

  END SUBROUTINE comp_rep

!!!------------------------------------------------------------------------ 
  subroutine add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
    implicit none
    integer                     :: itac,icdtac,iantac,icdan,iadj,an_ent,cd_ent
    real(kind=8)                :: sep(3),sept(3),sepn(3),gap,xco(3),coorcd(3),cooran(3)
    real(kind=8)                :: vls_cst,vln_cst,vlt_cst
    REAL(kind=8),DIMENSION(3,3) :: localframe_cd,localframe_an,eframe,oframe
    REAL(kind=8),DIMENSION(3,3) :: Rc
    REAL(kind=8),DIMENSION(3)   :: cdlev,anlev
    REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin

    icdan = icdan + 1

    nb_adj(icdtac) = nb_adj(icdtac) + 1
    iadj           = nb_adj(icdtac)

    this(icdan)%icdbtac = cylnd2bdyty(2, icdtac)
    this(icdan)%ianbtac = planx2bdyty(2, iantac)

    this(icdan)%icdbtyp = cylnd2bdyty(3, icdtac)
    this(icdan)%ianbtyp = planx2bdyty(3, iantac)

    this(icdan)%icdctyp = i_cylnd
    this(icdan)%ianctyp = i_planx

    this(icdan)%icdsci  = 0
    this(icdan)%iansci  = 0

    this(icdan)%iadj    = iadj
    this(icdan)%icdbdy  = cylnd2bdyty(1, icdtac)
    this(icdan)%icdtac  = icdtac
    this(icdan)%ianbdy  = planx2bdyty(1, iantac)
    this(icdan)%iantac  = iantac
    this(icdan)%isee    = rough_CDPLx(itac)%isee
    this(icdan)%coor    = xco

    this(icdan)%nuc     = sep

    this(icdan)%gapTTbegin = gap
          
    this(icdan)%tuc  = PLframe(iantac)%T
    this(icdan)%nuc  = PLframe(iantac)%N
    this(icdan)%suc  = PLframe(iantac)%S

    coorcd = CDcoor(1:3,icdtac)
    cooran = PLcoor(1:3,iantac)
    !oframe = get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac))
    !eframe = get_embeded_frame(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac))

    !localframe_cd = matmul(oframe,eframe)
    localframe_cd = get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac))
    
    !call GRAMM_SCHMIDT(localframe_cd(1,:),localframe_cd(2,:),localframe_cd(3,:))
    
    localframe_an = get_inertia_frameTT_PLANx(iantac)
    
    ! Je fait le shift dans le doute mais a priori il est nul... 
    cdlev = xco(1:3)- (coorcd(1:3)-get_shiftTT_CYLND(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac)))
    anlev = xco(1:3)- (cooran(1:3)-get_shiftTT_CYLND(planx2bdyty(1,iantac),planx2bdyty(2,iantac)))

    cd_ent = get_ent_CYLND(this(icdan)%icdbdy)
    an_ent = get_ent_PLANx(this(icdan)%iantac)

    this(icdan)%icdent = cd_ent
    this(icdan)%ianent = an_ent

    entity(cd_ent)%nb = entity(cd_ent)%nb+1
    entity(an_ent)%nb = entity(an_ent)%nb+1             
             
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
           
    ! On va calculer le passage rep inertie -> rep général pour an
    
    Rc(1,1)=localframe_an(2,1)*anlev(3) - localframe_an(3,1)*anlev(2)
    Rc(2,1)=localframe_an(2,2)*anlev(3) - localframe_an(3,2)*anlev(2)
    Rc(3,1)=localframe_an(2,3)*anlev(3) - localframe_an(3,3)*anlev(2)
    
    Rc(1,2)=localframe_an(3,1)*anlev(1) - localframe_an(1,1)*anlev(3)
    Rc(2,2)=localframe_an(3,2)*anlev(1) - localframe_an(1,2)*anlev(3)
    Rc(3,2)=localframe_an(3,3)*anlev(1) - localframe_an(1,3)*anlev(3)
    
    Rc(1,3)=localframe_an(1,1)*anlev(2) - localframe_an(2,1)*anlev(1)
    Rc(2,3)=localframe_an(1,2)*anlev(2) - localframe_an(2,2)*anlev(1)
    Rc(3,3)=localframe_an(1,3)*anlev(2) - localframe_an(2,3)*anlev(1)
    
    this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + Rc(1,2)*this(icdan)%tuc(2) + Rc(1,3)*this(icdan)%tuc(3) 
    this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + Rc(2,2)*this(icdan)%tuc(2) + Rc(2,3)*this(icdan)%tuc(3) 
    this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + Rc(3,2)*this(icdan)%tuc(2) + Rc(3,3)*this(icdan)%tuc(3) 

    this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + Rc(1,2)*this(icdan)%nuc(2) + Rc(1,3)*this(icdan)%nuc(3) 
    this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + Rc(2,2)*this(icdan)%nuc(2) + Rc(2,3)*this(icdan)%nuc(3) 
    this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + Rc(3,2)*this(icdan)%nuc(2) + Rc(3,3)*this(icdan)%nuc(3) 

    this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + Rc(1,2)*this(icdan)%suc(2) + Rc(1,3)*this(icdan)%suc(3) 
    this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + Rc(2,2)*this(icdan)%suc(2) + Rc(2,3)*this(icdan)%suc(3) 
    this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + Rc(3,2)*this(icdan)%suc(2) + Rc(3,3)*this(icdan)%suc(3) 

    ! Calcul des vitesses relatives,
    
    cd_Vbegin = get_vlocy_CYLND(cylnd2bdyty(1,icdtac),iVbeg_)
    an_Vbegin = get_vlocy_PLANx(iantac,iVbeg_)
    
    vlt_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) + &
            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2) + &
            (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%tuc(3)
    
    vln_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) + &
            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2) + &
            (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%nuc(3)
    
    vls_cst=(cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%suc(1) + &
            (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%suc(2) + &
            (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%suc(3)
           
    this(icdan)%vltBEGIN = vlt_cst &
                  + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                  - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

    this(icdan)%vlnBEGIN = vln_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                  - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

    this(icdan)%vlsBEGIN= vls_cst &     
                  + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                  - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)
             
    this(icdan)%rls    = 0.D0
    this(icdan)%rlt    = 0.D0
    this(icdan)%rln    = 0.D0
    this(icdan)%vls    = this(icdan)%vlsBEGIN
    this(icdan)%vlt    = this(icdan)%vltBEGIN
    this(icdan)%vln    = this(icdan)%vlnBEGIN
    this(icdan)%gapTT  = this(icdan)%gapTTbegin
    this(icdan)%status = i_nknow

    this(icdan)%meff    = rough_CDPLx(itac)%meff
    this(icdan)%reff    = rough_CDPLx(itac)%reff


    call get_behaviour_( icdan, see, tact_behav )

    !123456789012345678901234567890
!    IF (tact_behav(this(icdan)%lawnb)%lawty == 'WET_3C                        ') THEN
!       IF (this(icdan)%internal(1).EQ.0.D0) THEN
!          this(icdan)%internal(2)   = raycd + gap !fd dist
!          this(icdan)%internal(4:6) = this(icdan)%coor(1:3)
!          this(icdan)%internal(1) = 1.D0
!          this(icdan)%internal(3) = 0.D0
!       ELSE
!          sep = this(icdan)%coor(1:3) - this(icdan)%internal(4:6)
!          sepn = sep(1)*this(icdan)%nuc(1)+sep(2)*this(icdan)%nuc(2)+sep(3)*this(icdan)%nuc(3)
!          sept(1:3) = sep(1:3) - sepn(1:3)
!          this(icdan)%internal(3) = SQRT(sept(1)*sept(1)+sept(2)*sept(2)+sept(3)*sept(3))
!       END IF
!    END IF

end subroutine

subroutine clean_memory_CDPLx
  implicit none
  integer(kind=4) :: i, j, k

  call clean_memory_inter_meca_()

  nb_CYLND       = 0
  nb_PLANx       = 0
  nb_CDPLx       = 0
  nb_vCDPLx      = 0
  nb_recup_CDPLx = 0

  if( allocated(box) ) then
    do i = 1, size(box,3)
      do j = 1, size(box,2)
        do k = 1, size(box,1)
          if( associated(box(k,j,i)%SPwhich) ) deallocate(box(k,j,i)%SPwhich)
          if( associated(box(k,j,i)%PLwhich) ) deallocate(box(k,j,i)%PLwhich)
        end do
      end do
    end do
    deallocate(box)
  end if

  if( allocated(PLframe) ) deallocate(PLframe)

  nb_rough_CDPLx = 0
  if( allocated(rough_CDPLx) ) deallocate(rough_CDPLx)

  ! Root, Current and Previous should always be null outside creation_tab_visu

  if( allocated(CDcoor) ) deallocate(CDcoor)
  if( allocated(PLcoor) ) deallocate(PLcoor)

  Reac_CDPLx_MAX = 0.D0

  module_checked_ = .FALSE.
  check_CDPLx_    = .FALSE.

end subroutine

 subroutine set_nb_CDPLx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CDPLx = nb

 end subroutine

 subroutine redo_nb_adj_CDPLx()
   implicit none

   call redo_nb_adj_( get_nb_CYLND() )

 end subroutine

END MODULE CDPLx
