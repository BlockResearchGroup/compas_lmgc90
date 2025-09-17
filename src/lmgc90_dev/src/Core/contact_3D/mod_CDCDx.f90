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
!> between contactors CYLDx and CYLDx.
!> In this modulus candidate contactors are CYLDx and 
!> antagonist contactors are CYLDx.
!> Apex are closed by CYLNDes

MODULE CDCDx                                          

  USE overall
  USE tact_behaviour
  USE CYLND

  use algebra, only : cross_product
  
  use RBDY3, only : get_data, &
                    get_embeded_frame, &
                    get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color
  use MAILx, only : get_color_MAILx


  use parameters, only : i_cdcdx, i_mailx, i_rbdy3, i_mbs3

  use inter_meca_3D

  implicit none
  
  private

  INTEGER :: nb_CYLND

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 
  
  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!!!---------------------------------------------------------------------

  INTEGER,PRIVATE :: nb_CDCDx=0,nb_vCDCDx=0,nb_recup_CDCDx=0

!!!---------------------------------------------------------------------


  type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   
  integer             , dimension( : ), allocatable, target :: nb_adj

!!!------------------------------------------------------------------------ 

  type(T_verlet), dimension(:), allocatable, target ::verlt

!!!------------------------------------------------------------------------

  TYPE T_box
     INTEGER                      :: popul
     INTEGER,DIMENSION(:),POINTER :: which
  END TYPE T_box
  
  TYPE(T_box), DIMENSION(:,:,:),ALLOCATABLE :: box

  REAL (kind=8)  :: Lbox,LBox_1,norm
  INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,minibox3,maxibox3,maxpopul

!!!------------------------------------------------------------------------

  TYPE T_rough_CDCDx
     INTEGER      :: cd
     INTEGER      :: an
     INTEGER      :: isee
     REAL(kind=8) :: meff
     REAL(kind=8) :: reff
     INTEGER      :: xperiodic,yperiodic
  END TYPE T_rough_CDCDx

  TYPE(T_rough_CDCDx),DIMENSION(:),ALLOCATABLE :: rough_CDCDx
  INTEGER                                      :: nb_rough_CDCDx

  TYPE T_link_rough_CDCDx
     TYPE(T_link_rough_CDCDx), POINTER :: p  
     TYPE(T_rough_CDCDx)               :: val
     TYPE(T_link_rough_CDCDx), POINTER :: n  
  END TYPE T_link_rough_CDCDx

  TYPE(T_link_rough_CDCDx),POINTER :: Root,Current,Previous

!!!------------------------------------------------------------------------

  REAL (kind=8)  :: maxray, minray, maxalert, meanradius
  REAL (kind=8)  :: Upper_limit
  
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: CDcoor
  REAL(kind=8)                                    :: Reac_CDCDx_MAX=0.D0
  INTEGER,PRIVATE                                 :: ii,l_ii,iv
  INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
  LOGICAL,PRIVATE                                 :: write_creation_tab_visu

  real(kind=8), dimension(:), allocatable, target :: violation
!!!--------------------------------------------------------- ATTENTION PAS ENCORE ACTIVEES
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(KIND=8) :: XPERIODE = 0.D0,YPERIODE = 0.D0
!!!---------------------------------------------------------

  INTEGER      :: NbInteractionByContact = 1

  logical      :: module_checked_ = .FALSE.
  logical      :: check_CDCDx_    = .FALSE.

!!!------------------------------------------------------------------------

  PUBLIC &
       coor_prediction_CDCDx,&
       CHECK_CDCDx,&
       RUN_CDCDx, &
       get_write_Vloc_Rloc_CDCDx, &
       read_ini_Vloc_Rloc_CDCDx,&
       write_xxx_Vloc_Rloc_CDCDx,&
       set_xperiodic_data_CDCDx, &
       set_yperiodic_data_CDCDx, &
       stock_rloc_CDCDx, &
       recup_rloc_CDCDx, &
       smooth_computation_CDCDx, &
       compute_box_CDCDx, &
       creation_tab_visu_CDCDx, &
       compute_contact_CDCDx, &
       display_prox_tactors_CDCDx,&
       get_nb_CDCDx

  PUBLIC &
       nullify_reac_CDCDx, nullify_vlocy_CDCDx,injj_CDCDx, prjj_CDCDx, vitrad_CDCDx, & 
       get_surf_CDCDx

  public clean_memory_CDCDx

!!! EXPERIMENTAL FUNCTION

  PUBLIC &
       Set_NbInteractionByContact,Set_ContactRadius

  !rm for handler
  public get_this    , &
         set_nb_CDCDx, &
         redo_nb_adj_CDCDx, &
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
!!------------------------------------------------------------------------
  SUBROUTINE coor_prediction_CDCDx

    IMPLICIT NONE  
    INTEGER :: itacty  
    
    IF (smooth_method) THEN
       DO itacty=1,nb_CYLND
          CDcoor(1:3,itacty) = get_coor_CYLND(cylnd2bdyty(1,itacty),cylnd2bdyty(2,itacty))
       END DO
    ELSE
       DO itacty=1,nb_CYLND
          CDcoor(1:3,itacty) = get_coorTT_CYLND(cylnd2bdyty(1,itacty),cylnd2bdyty(2,itacty))
       END DO
    END IF

    DO itacty=1,nb_CYLND
      IF ( XPERIODIC ) THEN
        IF ( CDcoor(1,itacty)  > xperiode ) THEN
          CDcoor(1,itacty) = CDcoor(1,itacty) - xperiode
        ELSE IF ( CDcoor(1,itacty) < 0.D0 ) THEN
          CDcoor(1,itacty) = CDcoor(1,itacty) + xperiode
        END IF
      END IF
      IF ( YPERIODIC ) THEN
        IF ( CDcoor(2,itacty)  > yperiode ) THEN
          CDcoor(2,itacty) = CDcoor(2,itacty) - yperiode
        ELSE IF ( CDcoor(2,itacty) < 0.D0 ) THEN
          CDcoor(2,itacty) = CDcoor(2,itacty) + yperiode
        END IF
      END IF
    END DO



    
  END SUBROUTINE coor_prediction_CDCDx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_CDCDx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_CDCDx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_CDCDx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_CDCDx
!!!------------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_CDCDx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG

    xperiode  = per
    XPERIODIC = FLAG
    
  END SUBROUTINE set_xperiodic_data_CDCDx
!!!------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_CDCDx(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    yperiode  = per
    YPERIODIC = FLAG

    
  END SUBROUTINE set_yperiodic_data_CDCDx

!!!------------------------------------------------------------------------
  SUBROUTINE compute_box_CDCDx

    IMPLICIT NONE

    INTEGER                     :: isee,errare,ibdy
    CHARACTER(len=22)           :: IAM='mod_CDCDx::compute_box'

    maxalert=0.D0  

    DO isee=1,SIZE(see)
       IF (see(isee)%cdtac == 'CYLND' .AND. see(isee)%antac == 'CYLND') THEN
         maxalert=MAX(maxalert,see(isee)%alert)
       END IF
    END DO
   
    minray = get_min_radius_CYLND()
    maxray = get_max_radius_CYLND()

    IF (minray > maxray ) THEN
       call faterr(IAM,'Messing error computing minray and maxray')
    END IF
 
    maxray = (1.005*maxray) + maxalert

    Lbox   = 1.01D0*(2.D0*maxray + maxalert)
    Lbox_1 = 1.D0/Lbox
    norm   = Lbox/minray

    IF (.NOT. ALLOCATED(adjac))THEN
       ALLOCATE(adjac(nb_CYLND),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,'error in allocating adjac')
       END IF
       DO ibdy=1,nb_CYLND
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    ELSE
       DO ibdy=1,nb_CYLND
          IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    END IF
  
    IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
    ALLOCATE(nb_adj(nb_CYLND),stat=errare)
    IF (errare /=0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF

    nb_adj = 0

    IF (ALLOCATED(CDcoor)) DEALLOCATE(CDcoor)
    ALLOCATE(CDcoor(3,nb_CYLND),stat=errare)

  END SUBROUTINE compute_box_CDCDx

!!!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_CDCDx

    IMPLICIT NONE

    INTEGER                    :: errare,icdtac,iantac,isee
    INTEGER                    :: icdan,ibdy
    CHARACTER(len=5)           :: cdcol,ancol
    REAL(kind=8),DIMENSION(3)  :: coorcd,cooran
    REAL(kind=8)               :: data_cd(2),data_an(2),adist
    INTEGER                    :: ibox1,ibox2,ibox3
    INTEGER                    :: ibox1cd,ibox2cd,ibox3cd
    INTEGER                    :: ibox1an,ibox2an,ibox3an,icdpop,ianpop
    REAL(kind=8)               :: Xleft,Xright,Yleft,Yright,Zup,Zdown

    LOGICAL                    :: visible
    REAL(kind=8)               :: masscd,massan
    REAL(kind=8),DIMENSION(3,3) :: localframe_cd,localframe_an
    REAL(kind=8),DIMENSION(3)   :: vec_cdcd
    
                                      !1234567890123456789012345678
    CHARACTER(len=28)          :: IAM='mod_CDCDx::creation_tab_visu'
    CHARACTER(len=80)          :: mes

!       Height = (0.5D0*data(1)+maxalert)
!       rayan = data(2)



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

    IF(XPERIODIC)THEN
       IF(Xright>xperiode)THEN
          CALL FATERR(IAM,'The max right coordinnate is greater than the periode')
       END IF
       IF(Xleft<0.D0)THEN
          CALL FATERR(IAM,'The min left coordinate is less than zero')
       END IF
       Xright = xperiode
       Xleft  = 0.D0
    END IF

    IF(YPERIODIC)THEN
       IF(Yright>yperiode)THEN
          CALL FATERR(IAM,'The max right coordinnate is greater than the periode')
       END IF
       IF(Yleft<0.D0)THEN
          CALL FATERR(IAM,'The min left coordinate is less than zero')
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

    maxpopul = MIN(maxpopul,nb_CYLND)
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
   
    DO ibdy=1,nb_CYLND
       visible=get_visible_CYLND(ibdy)
       IF (.NOT.visible) CYCLE
       coorcd = CDcoor(1:3,ibdy)
       ibox1 = 1+INT((coorcd(1)-Xleft )*Lbox_1)
       ibox2 = 1+INT((coorcd(2)-Yleft )*Lbox_1)
       ibox3 = 1+INT((coorcd(3)-Zdown )*Lbox_1)
       IF (      ibox1 < minibox1 .OR. ibox1 > maxibox1 &
            .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2 &
            .OR. ibox3 < minibox3 .OR. ibox3 > maxibox3) THEN
          WRITE(*,*)' maxibox1=',maxibox1,'maxibox2=',maxibox2,'maxibox3=',maxibox3
          WRITE(*,*)'    ibox1=',ibox1,   '   ibox2=',ibox2,   '   ibox3=',ibox3
          WRITE(*,*)' minibox1=',minibox1,'minibox2=',minibox2,'minibox3=',minibox3
          WRITE(*,'(A13,I5,A13)')'  body CYLND ',ibdy,' out of boxes'
          call faterr(IAM,'Unexpected situation')
       END IF
   
       box(ibox1,ibox2,ibox3)%popul=box(ibox1,ibox2,ibox3)%popul+1
       if( box(ibox1,ibox2,ibox3)%popul > size(box(ibox1,ibox2,ibox3)%which) ) then
           call faterr(IAM, "Estimated max popul limit reached.")
       end if
       box(ibox1,ibox2,ibox3)%which(box(ibox1,ibox2,ibox3)%popul)=ibdy

    END DO
     
    nb_rough_CDCDx = 0

    NULLIFY(Root) 
    NULLIFY(Current)
    NULLIFY(Previous)

    DO ibox3cd = minibox3,maxibox3
       DO ibox2cd = minibox2,maxibox2
          DO ibox1cd = minibox1,maxibox1 
             DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                cdcol = get_color_CYLND(icdtac)

                DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                   DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
                      DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)            
                         DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                            iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                            IF ((iantac .LE. icdtac).OR. is_CYLND_same_RBDY3(icdtac,iantac)) CYCLE
                            ancol = get_color_CYLND(iantac)
                            isee = get_isee_specific('CYLND',cdcol,ancol)
                            IF (isee.EQ.0) CYCLE
                            adist=see(isee)%alert 
                               
                            coorcd = CDcoor(1:3,icdtac)
                            cooran = CDcoor(1:3,iantac)
                                                        
                            CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
                            CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

                            adist=0.1005D+01*adist + &
                                  data_cd(1) + data_cd(2) + &
                                  data_an(1) + data_an(2) 
                               
                            IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                 .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                 .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN
                                 nb_rough_CDCDx = nb_rough_CDCDx+1
                               IF ( nb_rough_CDCDx == 1) THEN
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
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1  
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_CYLND(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)

                               IF (is_CYLND_same_RBDY3(icdtac,iantac)) CYCLE
                               ancol = get_color_CYLND(iantac)
                               isee = get_isee_specific('CYLND',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = CDcoor(1:3,icdtac)
                               cooran = CDcoor(1:3,iantac)

                               cooran(1) = cooran(1) + xperiode

                               CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
                               CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

                               adist=0.1005D+01*adist + &
                                    data_cd(1) + data_cd(2) + &
                                    data_an(1) + data_an(2) 
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN
                                  nb_rough_CDCDx = nb_rough_CDCDx+1

                                  IF ( nb_rough_CDCDx == 1) THEN
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
          DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2
             DO ibox1cd = minibox1,maxibox1 
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_CYLND(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
                         DO ibox1an = MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)  
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                               IF (is_CYLND_same_RBDY3(icdtac,iantac)) CYCLE
                               ancol = get_color_CYLND(iantac)
                               isee = get_isee_specific('CYLND',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = CDcoor(1:3,icdtac)
                               cooran = CDcoor(1:3,iantac)

                               cooran(2) = cooran(2) + yperiode

                               CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
                               CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

                               adist=0.1005D+01*adist + &
                                    data_cd(1) + data_cd(2) + &
                                    data_an(1) + data_an(2) 

                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_CDCDx = nb_rough_CDCDx+1

                                  IF ( nb_rough_CDCDx == 1) THEN
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

!fd A VOIR si une seule boite ca va deconner

    IF (XPERIODIC .AND. YPERIODIC) THEN

       !fd on commence par le coin extreme en haut qui voit le coin initial en bas

       DO ibox3cd = minibox3,maxibox3
          DO ibox2cd = MAX(maxibox2-1,minibox2),maxibox2 
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_CYLND(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      DO ibox2an = minibox2,MIN(minibox2+1,maxibox2)
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                               IF (is_CYLND_same_RBDY3(icdtac,iantac)) CYCLE
                               ancol = get_color_CYLND(iantac)
                               isee = get_isee_specific('CYLND',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = CDcoor(1:3,icdtac)
                               cooran = CDcoor(1:3,iantac)

                               cooran(1) = cooran(1) + xperiode
                               cooran(2) = cooran(2) + yperiode

                               CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
                               CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

                               adist=0.1005D+01*adist + &
                                    data_cd(1) + data_cd(2) + &
                                    data_an(1) + data_an(2) 
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_CDCDx = nb_rough_CDCDx+1

                                  IF ( nb_rough_CDCDx == 1) THEN
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
          DO ibox2cd = minibox2,MIN(minibox2+1,maxibox2)
             DO ibox1cd = MAX(maxibox1-1,minibox1),maxibox1 
                DO icdpop = 1,box(ibox1cd,ibox2cd,ibox3cd)%popul
                   icdtac = box(ibox1cd,ibox2cd,ibox3cd)%which(icdpop)
                   cdcol = get_color_CYLND(icdtac)
                   
                   DO ibox3an = MAX(minibox3,ibox3cd-1),MIN(maxibox3,ibox3cd+1)                        
                      DO ibox2an = MAX(maxibox2-1,minibox2),maxibox2
                         DO ibox1an = minibox1,MIN(minibox1+1,maxibox1)
                            DO ianpop=1,box(ibox1an,ibox2an,ibox3an)%popul
                               iantac=box(ibox1an,ibox2an,ibox3an)%which(ianpop)
                               IF (is_CYLND_same_RBDY3(icdtac,iantac)) CYCLE
                               ancol = get_color_CYLND(iantac)
                               isee = get_isee_specific('CYLND',cdcol,ancol)
                               IF (isee.EQ.0) CYCLE
                               adist=see(isee)%alert 
                               
                               coorcd = CDcoor(1:3,icdtac)
                               cooran = CDcoor(1:3,iantac)

                               cooran(1) = cooran(1) + xperiode
                               cooran(2) = cooran(2) - yperiode

                               CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
                               CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

                               adist=0.1005D+01*adist + &
                                    data_cd(1) + data_cd(2) + &
                                    data_an(1) + data_an(2) 
                               
                               IF (       dabs(coorcd(1)-cooran(1)) <= adist &
                                    .AND. dabs(coorcd(2)-cooran(2)) <= adist &
                                    .AND. dabs(coorcd(3)-cooran(3)) <= adist) THEN

                                  nb_rough_CDCDx = nb_rough_CDCDx+1

                                  IF ( nb_rough_CDCDx == 1) THEN
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

    write(mes,'(I0,1x,A)') nb_rough_CDCDx,' CDCDx roughly found'
    call logmes('create_tab_visu::'//trim(mes)) 
    IF (ALLOCATED(rough_CDCDx)) DEALLOCATE(rough_CDCDx)
    ALLOCATE(rough_CDCDx(nb_rough_CDCDx))
    
    IF (ALLOCATED(this)) DEALLOCATE(this)
    ALLOCATE(this(2*nb_rough_CDCDx))

    DO icdan=nb_rough_CDCDx,1,-1
     
       Previous => Current%p
       rough_CDCDx(icdan)%cd   = Current%val%cd
       rough_CDCDx(icdan)%an   = Current%val%an
       rough_CDCDx(icdan)%isee = Current%val%isee

       rough_CDCDx(icdan)%xperiodic = Current%val%xperiodic
       rough_CDCDx(icdan)%yperiodic = Current%val%yperiodic

       masscd = get_mass_CYLND(CYLND2bdyty(1,Current%val%cd))
       massan = get_mass_CYLND(CYLND2bdyty(1,Current%val%an))

       rough_CDCDx(icdan)%meff = masscd*massan/(masscd+massan)

       CALL get_data(cylnd2bdyty(1,Current%val%cd),cylnd2bdyty(2,Current%val%cd),data_cd)
       CALL get_data(cylnd2bdyty(1,Current%val%an),cylnd2bdyty(2,Current%val%an),data_an)

       rough_CDCDx(icdan)%reff = data_cd(2)*data_an(2)/(data_cd(2)+data_an(2))

       DEALLOCATE(Current)
       Current => Previous
    END DO
   
    NULLIFY(Root)

    !nb_CDCDx = NbInteractionByContact*nb_rough_CDCDx

  END SUBROUTINE creation_tab_visu_CDCDx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_contact_CDCDx
 
    IMPLICIT NONE  
    
    INTEGER                     :: errare,icdtac,iantac,isee
    INTEGER                     :: icdan,ibdy,itac
    REAL(kind=8)                :: adist,dist
    REAL(kind=8),DIMENSION(3,3) :: localframe_cd,localframe_an
    REAL(kind=8),DIMENSION(2)   :: data_cd,data_an
    REAL(kind=8)                :: gap,dist_cd_S1,dist_cd_S2,dist_an_S1,dist_an_S2
    REAL(kind=8),DIMENSION(3)   :: vec_cdan,P_an,P_cd,coor_cd_S1,coor_cd_S2,coor_an_S1,coor_an_S2,coor_an,coor_cd,xco
    REAL(kind=8),DIMENSION(3)   :: r,v1,v2,n1,n2,sep
    REAL(kind=8)                :: beta,alpha_an,alpha_cd,lambda_cd,lambda_an,dd
    real(kind=8)                :: lcd,lan,tol
    INTEGER                     :: nbc
    logical                     :: end2end
    
                                      !12345678901234567890123456
    CHARACTER(len=28)          :: IAM='mod_CDCDx::compute_contact'

    CHARACTER(len=80)          :: mes

    real(kind=8)               :: s,t,a,b,c,d,e,det,bte,ctd,ate,btd 
    

    icdan    = 0        
    nb_CDCDx = 0
    nb_adj   = 0

    IF (nb_rough_CDCDx /= 0 ) THEN
       DO itac=1,nb_rough_CDCDx

          icdtac = rough_CDCDx(itac)%cd
          iantac = rough_CDCDx(itac)%an
          isee   = rough_CDCDx(itac)%isee
          adist   = see(isee)%alert

          coor_cd = CDcoor(1:3,icdtac)
          localframe_cd = MATMUL(get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac)), &
                                 get_embeded_frame(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac)))
          
          CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),data_cd)
          
          coor_an = CDcoor(1:3,iantac)

          coor_an(1) = coor_an(1) + (real(rough_CDCDx(itac)%xperiodic,8)*xperiode)
          coor_an(2) = coor_an(2) + (real(rough_CDCDx(itac)%yperiodic,8)*yperiode)
          
          localframe_an = MATMUL(get_inertia_frameTT_CYLND(cylnd2bdyty(1,iantac)), &
                                 get_embeded_frame(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac)))
           
          CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),data_an)

          lcd = 2.d0*data_cd(1)
          lan = 2.d0*data_an(1)
          tol = spacing(max(lcd,lan)) 


          ! par la suite on est dans le cylindre si dans [tol,2*data_xx(1)-tol] sinon sphere 
          
          !fd verifier que l'axe soit bien suivant 3  !!!

          ! print *,localframe_cd(:,3)
          ! print *,localframe_an(:,3)
          
          ! Position des centres de chaque spheres de chaque cylindre dans le repere global
          coor_cd_S1(:) = coor_cd(:) - data_cd(1)*localframe_cd(:,3)
          coor_cd_S2(:) = coor_cd(:) + data_cd(1)*localframe_cd(:,3)
          coor_an_S1(:) = coor_an(:) - data_an(1)*localframe_an(:,3)
          coor_an_S2(:) = coor_an(:) + data_an(1)*localframe_an(:,3)

          ! produit scalaire           
          beta = DOT_PRODUCT( localframe_an(:,3) , localframe_cd(:,3) )

          nbc=0 
          end2end=.FALSE.

          !fd valeur tolerance pifometrique !!
          if (1.d0-abs(beta*beta) < 0.001d0) then   
            ! Les cylindres sont alignees
             
             !             print*,'Particules alignees'

            ! Oui ils sont alignes, il y a donc 2 points de contacts de type cylindres-spheres ou sphere-sphere
            !     1) soit c est 2 points de an sur cd ou l inverse, dans ce cas l'un est 'inclu' dans l autre
            !     2) soit c est 1 de an et 1 de cd ou l inverse, dans ce cas ils 'debordent'
            !     3) soit 2 cylindres ont la meme longueur et si ils sont confondus, il s agit
            !        alors de deux contacts spheres-spheres


            !fd on calcule tous les cas des qu'on a 2 pt de contact on arrete     
             
            ! On projette les centres des spheres de cd sur an
            dist_cd_S1 = DOT_PRODUCT( coor_cd_S1(:) - coor_an_S1(:) , localframe_an(:,3) )
            dist_cd_S2 = DOT_PRODUCT( coor_cd_S2(:) - coor_an_S1(:) , localframe_an(:,3) )

            if (dist_cd_S1 < tol .and. dist_cd_S2 < tol) then
              !cd x-x-x 
              !an      1->-2

              end2end=.TRUE.
              nbc = nbc +1
              
              if (dist_cd_S1 > dist_cd_S2) then                 
                ! cd_S1 voit an_S1
                !cd 2-<-1 
                !an       1->-2

                ! print*,'cd_S1 voit an_S1' 
                 
                vec_cdan(:)   =  coor_cd_S1(:) - coor_an_S1(:)
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S1(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S1(:) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              else  
                ! cd_S2 voit an_S1                  
                !cd 1->-2 
                !an       1->-2

                ! print*,'cd_S2 voit an_S1'  
                 
                vec_cdan(:)   =  coor_cd_S2(:) - coor_an_S1(:)
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S2(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S1(:) + data_an(2)*sep(:)
                xco(:)        =   0.5* ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              endif
                 
            else if (dist_cd_S1 > lan-tol  .and. dist_cd_S2 > lan-tol) then
              !cd       x-x-x 
              !an 1->-2
               
              end2end=.TRUE.
              nbc = nbc +1

              if (dist_cd_S1 < dist_cd_S2) then                 
                ! cd_S1 voit an_S2
                !cd        1->-2 
                !an  1->-2

                ! print*,'cd_S1 voit an_S2' 
                 
                vec_cdan(:)   =  coor_cd_S1(:) - coor_an_S2(:)
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S1(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S2(:) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              else  
                ! cd_S2 voit an_S2                  
                !cd       2-<-1 
                !an 1->-2

                ! print*,'cd_S2 voit an_S2' 

                vec_cdan(:)   =  coor_cd_S2(:) - coor_an_S2(:)
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S2(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S2(:) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              endif
              
            else
              
              if (dist_cd_S1 >= -tol .and. dist_cd_S1 <= lan+tol) then  
                !cd   1 
                !an 1->-2
                nbc = nbc +1

                ! print*,'cd_S1 voit an seg'
                 
                vec_cdan(:)   =  coor_cd_S1(:) - (coor_an_S1(:)+dist_cd_S1*localframe_an(:,3)) 
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S1(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S1(:) + dist_cd_S1*localframe_an(:,3) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
              endif 
                 
              if (dist_cd_S2 >= -tol .and. dist_cd_S2 <= lan+tol) then  
                !cd   2 
                !an 1->-2
                nbc = nbc +1

                ! print*,'cd_S2 voit an seg'
                
                vec_cdan(:)   =  coor_cd_S2(:) - (coor_an_S1(:)+dist_cd_S2*localframe_an(:,3)) 
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =   coor_cd_S2(:) - data_cd(2)*sep(:)
                P_an(:)       =   coor_an_S1(:) + dist_cd_S2*localframe_an(:,3) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                 
              endif   
            endif
           
            ! on cherche les autres cas 

            if (.not. end2end .and. nbc < 2) then

              ! On projette les centres des spheres de an sur cd (on les calcule l meme si on a besoin plus tard)             
              dist_an_S1 = DOT_PRODUCT( coor_an_S1(:) - coor_cd_S1(:) , localframe_cd(:,3) )
              dist_an_S2 = DOT_PRODUCT( coor_an_S2(:) - coor_cd_S1(:) , localframe_cd(:,3) )

              ! print*,dist_an_S1,dist_an_S2,tol
              
              if (dist_an_S1 >= -tol .and. dist_an_S1 <= lcd+tol) then  
                !cd 1->-2 
                !an   1
                nbc = nbc +1

                ! print*,'an_S1 voit cd seg'

                vec_cdan(:)   =  coor_cd_S1(:) + dist_an_S1*localframe_cd(:,3) - coor_an_S1(:) 
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =  coor_cd_S1(:) + dist_an_S1*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)       =  coor_an_S1(:) + data_an(2)*sep(:)
                xco(:)        =  0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =  DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                 
              endif 
            endif
           
            if (.not. end2end .and. nbc < 2) then
              
              if (dist_an_S2 >= -tol .and. dist_an_S2 <= lcd+tol) then  
                !cd 1->-2 
                !an   2
                nbc = nbc +1

                ! print*,'an_S2 voit cd seg'
                
                vec_cdan(:)   =  coor_cd_S1(:) + dist_an_S2*localframe_cd(:,3) - coor_an_S2(:)  
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =  coor_cd_S1(:) + dist_an_S2*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)       =  coor_an_S2(:) + data_an(2)*sep(:)
                xco(:)        =  0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =  DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                 
              endif   
            endif
          else
            
             !             print*, 'Particules non alignees'

            v1 = coor_an_S1 - coor_cd_S1
            v2 = coor_an_S2 - coor_cd_S1

            n1 = cross_PRODUCT( v1 , localframe_cd(:,3) )   
            n2 = cross_PRODUCT( v2 , localframe_cd(:,3) )

            if (comp_norm(cross_PRODUCT( n1 , n2 )) < 1e-5) then   

               !               print*,'particules coplanaires'

              ! ici on gre le contact sphere - cylindre et sphere - sphere 

              dd=dot_product(v1,localframe_cd(:,3))

              ! print*,'an_S1 sur cd',dd,2*data_cd(1)
              
              if ( dd < tol) then

                nbc=nbc+1
                
                ! contact cd_S1 - an_S1 
                vec_cdan(:)= coor_cd_S1 - coor_an_S1           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S1 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S1 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print *,'contact cd_S1 - an_S1' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              else if (dd >= tol .and. dd <= lcd -tol ) then
                ! contact cd - an_S1

                nbc=nbc+1 
                 
                vec_cdan(:)= (coor_cd_S1 + dd*localframe_cd(:,3)) - coor_an_S1           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S1 + dd*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S1 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
              
                ! print *,'contact cd  - an_S1' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
               
              else if (dd > lcd-tol) then
                ! contact cd_S2 - an_S1

                nbc=nbc+1
                
                vec_cdan(:)= coor_cd_S2 - coor_an_S1           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S2 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S1 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
              
                ! print *,'contact cd_S2 - an_S1' 
                ! print *,sep,gap

                if (gap.LE.adist) then
                  call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                endif
              endif   

              dd = dot_product(v2,localframe_cd(:,3))

              ! print*,'an_S2 sur cd',dd
              
              if (dd < tol) then 
                ! contact cd_S1 - an_S2

                nbc=nbc+1
                 
                vec_cdan(:)= coor_cd_S1 - coor_an_S2           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S1 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S2 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
              
                ! print *,'contact cd_S1 - an_S2' 
                ! print *,sep,gap

                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
              else if (dd >= tol .and. dd <= lcd - tol ) then
                ! contact cd - an_S1

                nbc=nbc+1
                 
                vec_cdan(:)= (coor_cd_S1 + dd*localframe_cd(:,3)) - coor_an_S2           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S1 + dd*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S2 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print *,'contact cd - an_S1' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              else if (dd > lcd - tol) then
                ! contact cd_S2 - an_S2

                nbc=nbc+1
                 
                vec_cdan(:)= coor_cd_S2 - coor_an_S2           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S2 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S2 + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print *,'contact cd_S2 - an_S2' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
              endif   

              dd = dot_product(coor_cd_S1 - coor_an_S1,localframe_an(:,3))

              ! print*,'cd_S1  sur an ',dd
              
              if (dd >= 0.d0 .and. dd <= 2.d0*data_an(1) ) then
                ! contact cd_S1 - an

                nbc=nbc+1

                 
                vec_cdan(:)= coor_cd_S1  - (coor_an_S1 + dd*localframe_an(:,3))           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S1 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S1 + dd*localframe_an(:,3) + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print *,'contact cd_S1 - an' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              endif  

              dd = dot_product(coor_cd_S2 - coor_an_S1,localframe_an(:,3))

              ! print*,'cd_S2  sur an ',dd

              if (dd >= 0.d0 .and. dd <= 2.d0*data_an(1) ) then
                ! contact cd_S2 - an

                nbc=nbc+1
                 
                vec_cdan(:)= coor_cd_S2  - (coor_an_S1 + dd*localframe_an(:,3))           
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd_S2 - data_cd(2)*sep(:)
                P_an(:)    = coor_an_S1 + dd*localframe_an(:,3) + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print *,'contact cd_S2 - an' 
                ! print *,sep,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              endif  
              
            else  

               !               print*,'skew'


              if (.TRUE.) then

                ! la methode de eberly "non robuste"
                 
                a=dot_product(coor_cd_S2-coor_cd_S1,coor_cd_S2-coor_cd_S1)
                b=dot_product(coor_cd_S2-coor_cd_S1,coor_an_S2-coor_an_S1)
                c=dot_product(coor_an_S2-coor_an_S1,coor_an_S2-coor_an_S1)
                d=dot_product(coor_cd_S2-coor_cd_S1,coor_cd_S1-coor_an_S1)
                e=dot_product(coor_an_S2-coor_an_S1,coor_cd_S1-coor_an_S1)
                det=a*c-b*b


                ! print*,data_cd(1),data_an(1)
                ! print*,a,b,c,d,e
                ! print*,'det',det  
                
                if ( det > 0.d0 ) then ! non parallel segments

                  bte = b*e
                  ctd = c*d
                  if ( bte <= ctd ) then  ! s <= 0
                    if ( e <= 0.d0 ) then ! t <= 0 ( r e g i o n 6 )
                      ! print*,'region 6'     
                      if (-d >= a ) then
                          s=1.d0
                      elseif (-d > 0.d0) then
                          s=-d/a
                      else
                          s=0.d0
                      endif   
                      t = 0.d0
                    elseif ( e < c ) then ! 0 < t < 1 ( r e g i o n 5 )
                      ! print*,'region 5'     
                      s = 0.d0
                      t = e/c 
                    else                  ! t >= 1 ( r e g i o n 4 )
                      ! print*,'region 4'     
                      if (b-d >= a) then
                          s=1.d0
                      elseif (b-d > 0) then
                          s= (b-d) / a
                      else
                          s=0.d0
                      endif   
                      t = 1.d0
                    endif
                else ! s > 0
                  s = bte - ctd
                  if ( s >= det ) then    ! s >= 1
                    if ( b+e <= 0 ) then  ! t <= 0 ( r e g i o n 8 )
                      ! print*,'region 8'     
                      if (-d <= 0) then
                         s=0.d0
                      elseif (-d < a) then
                         s=-d/ a
                      else
                         s=1.d0
                      endif   
                      t = 0.d0
 
                    elseif ( b+e < c ) then ! 0 < t < 1 ( r e g i o n 1 )
                      ! print*,'region 1'     
                      s = 1.d0
                      t = ( b+e ) / c
                    else                    ! t >= 1 ( r e g i o n 2 )
                      ! print*,'region 2'     
                      if ((b-d) <= 0.d0) then
                        s=0.d0
                      elseif (b-d < a) then
                        s= (b-d)/a
                      else
                        s=1.d0
                      endif   
                      t = 1.d0
                    endif
                  else  ! 0 < s < 1
                    ate = a*e
                    btd = b*d
                    if ( ate <= btd ) then ! t <= 0 ( r e g i o n 7 )
                       ! print*,'region 7'     
                       if (-d <= 0.d0 ) then 
                          s=0.d0
                       elseif (-d >= a) then
                          s=1.d0
                       else
                          s=-d/ a
                       endif   
                       t = 0.d0
                    else ! t > 0
                      t = ate - btd 
                      if ( t >= det ) then ! t >= 1 ( r e g i o n 3 )
                         ! print*,'region 3'     
                         if ( b-d <= 0) then
                            s= 0.d0
                         elseif ( b-d >= a) then
                            s=1.d0
                         else
                            s=(b-d) / a
                         endif   
                         t = 1.d0
                      else ! 0 < t < 1 ( r e g i o n 0 )
                         ! print*,'region 0'     
                         s = s/det
                         t = t/det
                      endif
                    endif
                  endif 
                endif


                ! print*,s,t
                
                nbc = nbc +1


                lambda_cd = ((s-1.d0)*data_cd(1))+(s*data_cd(1)) 
                lambda_an = ((t-1.d0)*data_an(1))+(t*data_an(1)) 


                ! print*,lambda_cd,lambda_an
                
                vec_cdan(:)   =  (coor_cd + lambda_cd*localframe_cd(:,3)) - (coor_an + lambda_an*localframe_an(:,3))
                dist          =  comp_norm( vec_cdan(:) )
                sep(:)        =  vec_cdan(:) / dist
                 
                P_cd(:)       =  coor_cd + lambda_cd*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)       =  coor_an + lambda_an*localframe_an(:,3) + data_an(2)*sep(:)
                xco(:)        =   0.5d0 * ( P_an(:)+P_cd(:) )
                gap           =   DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                ! print*,gap
                
                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

              else    
               
              ! Ils ne sont pas alignes, il n y a que 1 contact possible avec 3 possibilites:
              ! sphere-sphere,  spheres-cylindre et cylindre-cylindre

              ! On cherche d abord le contact ligne-ligne

              ! Pcd = coor_cd + lambda_cd * localframe_cd(:,3)
              ! Pan = coor_an + lambda_an * localframe_an(:,3)
             
              ! PcdPan = r + lambda_an * localframe_an(:,3) - lambda_cd * localframe_cd(:,3)
              ! le point solution verifie
              !   PcdPan . localframe_cd(:3) = 0
              !   PcdPan . localframe_an(:3) = 0
            
              r = coor_an(:) - coor_cd(:)
            
              alpha_cd    =     DOT_PRODUCT( r , localframe_cd(:,3) )   
              alpha_an    =     DOT_PRODUCT( r , localframe_an(:,3) )   
 
              lambda_cd   =     (alpha_cd - (alpha_an*beta)) / ( 1.d0 - (beta*beta))
              lambda_an   =     ((alpha_cd*beta) - alpha_an) / ( 1.d0 - (beta*beta))


              print*,'cd',lambda_cd,data_cd(1)!,coor_cd(:)+lambda_cd*localframe_cd(:,3)
              print*,'an',lambda_an,data_an(1)!,coor_an(:)+lambda_an*localframe_an(:,3)              
              
              if ( ( abs(lambda_cd) <= data_cd(1) ) .and. ( (abs(lambda_an) <= data_an(1)) ) ) then
                 !
                 print*,'cas cylindre-cylindre'

                nbc=nbc+1
                 
                ! la normale de an -> cd 
                vec_cdan(:)= lambda_cd*localframe_cd(:,3) - (r + lambda_an*localframe_an(:,3)) 
                dist       = comp_norm( vec_cdan(:) )
                sep(:)     = vec_cdan(:) / dist
                            
                P_cd(:)    = coor_cd(:) + lambda_cd*localframe_cd(:,3) - data_cd(2)*sep(:)
                P_an(:)    = coor_an(:) + lambda_an*localframe_an(:,3) + data_an(2)*sep(:)
              
                xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
              else

                 !
                 print*,'un bout'  
              
                ! les autres cas
                if (lambda_cd < -data_cd(1)) then
                  if (lambda_an < -data_an(1)) then
                     !
                     print*,'cas sphere cd_S1 - sphere an_S1'
                    nbc=nbc+1 

                    vec_cdan(:)= coor_cd_S1(:) - coor_an_S1(:)
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd_S1(:) - data_cd(2)*sep(:) 
                    P_an(:)    = coor_an_S1(:) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd S1 - an S1' 
                    ! print*, lambda_cd,P_cd(:)
                    ! print*, lambda_an,P_an(:)
                    ! print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
                  else if (lambda_an > data_an(1)) then
                     !
                     print*,'cas sphere cd_S1 - sphere an_S2'
                    nbc=nbc+1 

                    vec_cdan(:)= coor_cd_S1(:) - coor_an_S2(:)
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd_S1(:) - data_cd(2)*sep(:) 
                    P_an(:)    = coor_an_S2(:) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd S1 - an S2' 
                    ! print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

                  else
                     !
                     print*,'cas sphere cd_S1 - cylindre an'

                    ! correction du lambda 
                    !!dd = dot_product(coor_cd_S1 - coor_an_S1,localframe_an(:,3))


                    lambda_an = -dot_product(localframe_an(:,3),r+data_cd(1)*localframe_cd(:,3))

                    print*,lambda_an
                    
                    nbc=nbc+1
                    

                    vec_cdan(:)= coor_cd_S1(:) - (coor_an(:) + lambda_an*localframe_an(:,3))
                    !!vec_cdan(:)= coor_cd_S1(:) - (coor_an_S1(:) + dd*localframe_an(:,3))
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                              
                    P_cd(:)    = coor_cd_S1(:) - data_cd(2)*sep(:)
                    P_an(:)    = coor_an (:) + lambda_an*localframe_an(:,3) + data_an(2)*sep(:)                    
                    !!P_an(:)    = coor_an_S1(:) + dd*localframe_an(:,3) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )


                    ! print*,' cd S1 - an' 
                    ! print*,gap,sep

                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

                  
                  endif

                else if (lambda_cd > data_cd(1)) then
                  if (lambda_an < -data_an(1)) then
                     !
                     print*,'cas sphere cd_S2 - sphere an_S1'                 

                    nbc=nbc+1

                    vec_cdan(:)= coor_cd_S2(:) - coor_an_S1(:)
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd_S2(:) - data_cd(2)*sep(:)
                    P_an(:)    = coor_an_S1(:) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd S2 - an S1' 
                    ! print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
                  else if (lambda_an > data_an(1)) then
                    !
                    print*,'cas sphere cd_S2 - sphere an_S2'                  

                    !recherche correction lambda
                    ! cd_s2 an                   
                    lambda_an = data_an(1) !min(data_an(1),-dot_product(localframe_an(:,3),r-data_cd(1)*localframe_cd(:,3)))
                    ! cd  an_s2                                       
                    lambda_cd = max(-data_cd(1),min(data_cd(1),dot_product(localframe_cd(:,3),r+data_an(1)*localframe_an(:,3))))

                    print*,'cd ',lambda_cd
                    print*,'an ',lambda_an
                    
                    nbc=nbc+1

                    vec_cdan(:)= (coor_cd + lambda_cd*localframe_cd(:,3)) - (coor_an + lambda_an*localframe_an(:,3))
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd_S2(:) - data_cd(2)*sep(:)
                    P_an(:)    = coor_an_S2(:) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )
                  
                    ! print*,' cd S2 - an S2' 
                    !
                    print*,gap,sep

                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                  
                  else
                     !
                     print*,'cas sphere cd_S2 - cylindre an'

                    ! correction du lambda 
                    !!dd = dot_product(coor_cd_S2 - coor_an_S1,localframe_an(:,3))

                    lambda_an = -dot_product(localframe_an(:,3),r-data_cd(1)*localframe_cd(:,3))

                    print*,lambda_an
                    
                    nbc=nbc+1
                    
                    vec_cdan(:)= coor_cd_S2(:) - (coor_an(:) + lambda_an*localframe_an(:,3))
                    !!vec_cdan(:)= coor_cd_S2(:) - (coor_an_S1(:) + dd*localframe_an(:,3))                    
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd_S2(:) - data_cd(2)*sep(:)
                    P_an(:)    = coor_an(:) + lambda_an*localframe_an(:,3) + data_an(2)*sep(:)
                    !!P_an(:)    = coor_an_S1(:) + dd*localframe_an(:,3) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd S2 - an' 
                    ! print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

                   
                  endif
                else
                  if (lambda_an < -data_an(1)) then
                     !
                     print*,'cas sphere an_S1 - cylindre cd'                 

                    !correction du lambda
                    !!dd = dot_product(coor_an_S1 - coor_cd_S1,localframe_cd(:,3))
                     
                    lambda_cd = dot_product(localframe_cd(:,3),r-data_an(1)*localframe_an(:,3))

                    print*,lambda_cd  
                    
                    nbc=nbc+1 
                    
                    vec_cdan(:)= (coor_cd(:) + lambda_cd*localframe_cd(:,3)) - coor_an_S1(:)
                    !!vec_cdan(:)= (coor_cd_S1(:) + dd*localframe_cd(:,3)) - coor_an_S1(:)                    
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd(:) + lambda_cd*localframe_cd(:,3) - data_cd(2)*sep(:)
                    !!P_cd(:)    = coor_cd_S1(:) + dd*localframe_cd(:,3) - data_cd(2)*sep(:)                    
                    P_an(:)    = coor_an_S1(:) + data_an(2)*sep(:)
              
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd - an S1'
                    ! print*, dd,lambda_cd,P_cd(:)
                    ! print*, lambda_an,P_an(:)
                    ! print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)
                
                  else if (lambda_an > data_an(1)) then
                     !
                     print*,'cas sphere an_S2 - cylindre cd'                   

                    !correction du lambda
                    !!dd = dot_product(coor_an_S2 - coor_cd_S1,localframe_cd(:,3))

                    lambda_cd = dot_product(localframe_cd(:,3),r+data_an(1)*localframe_an(:,3))

                    print*,lambda_cd 
                    
                    nbc=nbc+1
                    
                    vec_cdan(:)= (coor_cd(:) + lambda_cd*localframe_cd(:,3)) - coor_an_S2(:)
                    !!vec_cdan(:)= (coor_cd_S1(:) + dd*localframe_cd(:,3)) - coor_an_S2(:)                    
                    dist       = comp_norm( vec_cdan(:) )
                    sep(:)     = vec_cdan(:) / dist
                            
                    P_cd(:)    = coor_cd(:) + lambda_cd*localframe_cd(:,3) - data_cd(2)*sep(:)
                    !!P_cd(:)    = coor_cd_S1(:) + dd*localframe_cd(:,3) - data_cd(2)*sep(:)
                    P_an(:)    = coor_an_S2(:) + data_an(2)*sep(:)
                                                      
                    xco(:)     = 0.5d0 * (P_an(:)+P_cd(:))
                    gap        = DOT_PRODUCT( P_cd(:)-P_an(:) , sep(:) )

                    ! print*,' cd - an S2' 
                    !
                    !print*,gap,sep
                    
                    if (gap.LE.adist) call add_contact(itac,icdtac,iantac,sep,gap,xco,icdan)

                  endif
                endif
              endif
              endif ! NEW TRUE
            endif 
          endif
          endif
       
          if (end2end .and. nbc>1) call faterr(IAM,'end to end only one contact point expected')

          !if (nbc>2) call faterr(IAM,'no more than two contact points expected')

          
       END DO
       nb_CDCDx=icdan
    END IF

    write(mes,'(I0,1x,A)') nb_CDCDx,' CDCDx found'
    call logmes('compute_contact::'//trim(mes)) 
    
    DO ibdy=1,nb_CYLND
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       IF (nb_adj(ibdy) /= 0) THEN
          ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
          IF (errare /=0 ) THEN
            call faterr(IAM,'error in allocating adjac(icdbdy)%icdan')
          END IF
       END IF
    END DO
    
    
    DO icdan=1,nb_CDCDx
       adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
    END DO
    

    IF (ALLOCATED(violation)) DEALLOCATE(violation)
    ALLOCATE(violation(nb_CDCDx),stat=errare)
    IF (errare /=0 ) THEN
      call faterr(IAM,'error in allocating violation') 
    END IF
    
  END SUBROUTINE compute_contact_CDCDx
  
!!!------------------------------------------------------------------------
  SUBROUTINE smooth_computation_CDCDx

    IMPLICIT NONE
    INTEGER          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    DO icdan=1,nb_CDCDx
       
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
    
    DO icdan=1,nb_CDCDx  
       CALL nullify_reac_CDCDx(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_CDCDx
       CALL injj_CDCDx(icdan,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln,iIreac)
    END DO
    
  END SUBROUTINE smooth_computation_CDCDx
!!!------------------------------------------------------------------------
  SUBROUTINE display_prox_tactors_CDCDx

    IMPLICIT NONE
    INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
   character(len=5) :: cdmodel,anmodel
    
    nb_CYLND=get_nb_CYLND()
    
    DO icdtac=1,nb_CYLND    
       DO iadj=1,nb_adj(icdtac)         
          icdan  = adjac(icdtac)%icdan(iadj)
          icdbdy = this(icdan)%icdbdy
          !icdtac = this(icdan)%icdtac
          ianbdy = this(icdan)%ianbdy
          iantac = this(icdan)%iantac

          cdmodel = get_body_model_name_from_id( cylnd2bdyty(3,icdtac) )
          anmodel = get_body_model_name_from_id( cylnd2bdyty(3,iantac) )

          WRITE(*,'(A1)')' '
          WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
          !123456789012345678901234567890123456789012345678901234567890123456789012
          WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
          WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               cdmodel,icdbdy,'CYLND',icdtac,see(this(icdan)%isee)%behav,  &
               anmodel,ianbdy,'CYLND',iantac
          
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
    
  END SUBROUTINE display_prox_tactors_CDCDx
!!!------------------------------------------------------------------------  
  SUBROUTINE stock_rloc_CDCDx

    IMPLICIT NONE

    INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    INTEGER :: errare

    character(len=80) :: cout
                               !123456789012345678901
    character(len=21) :: IAM = 'mod_CDCDx::stock_rloc'

    nb_CYLND=get_nb_CYLND()

    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_CYLND),stat=errare)
       IF (errare /=0 ) THEN
          call faterr(IAM,'Error allocating verlt')
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
             write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
             call faterr(IAM,cout)
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
    DO icdan=1,nb_CDCDx
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       iadj   = this(icdan)%iadj

       verlt(icdtac)%icdan(iadj)    = icdan
       verlt(icdtac)%cdbdy          = cylnd2bdyty(1,icdtac)
       verlt(icdtac)%cdtac          = cylnd2bdyty(2,icdtac)
       verlt(icdtac)%cdmodel        = cylnd2bdyty(3,icdtac)
       verlt(icdtac)%anbdy(iadj)    = cylnd2bdyty(1,iantac)
       verlt(icdtac)%antac(iadj)    = cylnd2bdyty(2,iantac)
       verlt(icdtac)%anmodel(iadj)  = cylnd2bdyty(3,iantac)

       verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
       verlt(icdtac)%ansci(iadj)    = this(icdan)%iansci

       verlt(icdtac)%status(iadj)   = this(icdan)%status

       verlt(icdtac)%rls(iadj)      = this(icdan)%rls/H
       verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
       verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
       verlt(icdtac)%vls(iadj)      = this(icdan)%vls
       verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
       verlt(icdtac)%vln(iadj)      = this(icdan)%vln

       verlt(icdtac)%tuc(:, iadj)   = this(icdan)%tuc(:)
       verlt(icdtac)%nuc(:, iadj)   = this(icdan)%nuc(:)
       verlt(icdtac)%suc(:, iadj)   = this(icdan)%suc(:)
       
       verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
       verlt(icdtac)%coor(1:3,iadj) = this(icdan)%coor(1:3)

       verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
    END DO
    
    nb_vCDCDx = nb_CDCDx

    WRITE(cout,'(1X,I10,A12)') nb_vCDCDx,' stock CDCDx'
    call logmes(cout)

  END SUBROUTINE stock_rloc_CDCDx
!!!------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_CDCDx

    IMPLICIT NONE

    INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    character(len=80) :: cout
   
   if (.not. allocated(verlt)) then
      call logmes('[mod_CDCDx::recup_rloc] Warning: verlt not allocated, no recup done')
      return
   end if

    nb_recup_CDCDx = 0

    DO icdan=1,nb_CDCDx
       this(icdan)%rls=0.D0
       this(icdan)%rlt=0.D0
       this(icdan)%rln=0.D0
       this(icdan)%statusBEGIN=i_nknow

       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       
       IF (verlt(icdtac)%adjsz /= 0) THEN

         if ( verlt(icdtac)%cdbdy  == cylnd2bdyty(1,icdtac) .and. &      
              verlt(icdtac)%cdtac  == cylnd2bdyty(2,icdtac) .and. &
              verlt(icdtac)%cdmodel== cylnd2bdyty(3,icdtac) ) then

           do iadj = 1, verlt(icdtac)%adjsz
             if (                                           &
                  verlt(icdtac)%anbdy(iadj)  == cylnd2bdyty(1,iantac) .and. &
                  verlt(icdtac)%antac(iadj)  == cylnd2bdyty(2,iantac) .and. &
                  verlt(icdtac)%anmodel(iadj)== cylnd2bdyty(3,iantac)       &
                ) then
                this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
                this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H 
                this(icdan)%rln = verlt(icdtac)%rln(iadj)*H

                this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
                this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                nb_recup_CDCDx = nb_recup_CDCDx + 1
                EXIT
             end if
           end do
         end if
       ENDIF
    END DO

    WRITE(cout,'(1X,I10,A12)') nb_recup_CDCDx,' recup CDCDx'
    call logmes(cout)

  END SUBROUTINE recup_rloc_CDCDx
!!!------------------------------------------------------------------------ 
  SUBROUTINE read_ini_Vloc_Rloc 
    
    IMPLICIT NONE
    integer            :: icdan,icdbdy,icdtac,ianbdy,iantac
    integer            :: cdmodel, anmodel, iadj
    REAL(kind=8)       :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
    CHARACTER(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
    INTEGER            :: errare,icdtact
    INTEGER            :: ibehav,nb_internal,i_internal
    CHARACTER(len=103) :: cout
    CHARACTER(len=29)  :: IAM = 'mod_CDCDx::read_ini_Vloc_Rloc' 

    nb_CYLND = get_nb_CYLND()
    errare=0
    IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_CYLND),stat=errare)
    IF (errare /=0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF
 
    nb_adj = 0
    
    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) .NE. 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13).NE. 'CDCDx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)') &
            cdbdy,icdbdy,cdtac,icdtac, &
            behav,                     &
            anbdy,ianbdy,antac,iantac, &
            sttus

       cdmodel = get_body_model_id_from_name( cdbdy )
       IF (cdtac .NE. 'CYLND' .OR. antac .NE. 'CYLND') CYCLE
       DO icdtact=1,nb_CYLND
          IF ( cylnd2bdyty(1,icdtact) == icdbdy .and. &
               cylnd2bdyty(2,icdtact) == icdtac .and. &
               cylnd2bdyty(3,icdtact) == cdmodel  ) then
             nb_adj(icdtact)=nb_adj(icdtact)+1    
             EXIT
          END IF
       END DO
    END DO
    
    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_CYLND),stat=errare)
       IF (errare /=0 ) THEN
          CALL FATERR(IAM,' error allocating verlt')
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
       IF (G_clin(9:13)/= 'CDCDx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)') &
            cdbdy,icdbdy,cdtac,icdtac, &
            behav,                     &
            anbdy,ianbdy,antac,iantac, &
            sttus
       IF (cdtac .NE. 'CYLND' .OR. antac .NE. 'CYLND') CYCLE

       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )

       DO icdtact=1,nb_CYLND

          if ( cylnd2bdyty(1,icdtact) == icdbdy .and. &
               cylnd2bdyty(2,icdtact) == icdtac .and. &
               cylnd2bdyty(3,icdtact) == cdmodel ) then

             icdan = icdan + 1
             nb_adj(icdtact) = nb_adj(icdtact)+1 

             verlt(icdtact)%icdan( nb_adj(icdtact) ) = icdan

             verlt(icdtact)%cdbdy  = icdbdy
             verlt(icdtact)%cdtac  = icdtac
             verlt(icdtact)%cdmodel= cdmodel

             verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
             verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
             verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
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
             IF (G_clin(30:34)== 's(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             ENDIF
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
                  READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)')  &
                    verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
                ENDDO
             ENDIF
          ENDIF
       END DO
    END DO

    nb_vCDCDx=0
    
    DO icdtac=1,nb_CYLND
       nb_vCDCDx = nb_vCDCDx + nb_adj(icdtac)
       
       IF ( nb_adj(icdtac) /= verlt(icdtac)%adjsz ) THEN 
          WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtac, &
               'value of nb_adj is',nb_adj(icdtac),' and value of verlet%adjsz is ',verlt(icdtac)%adjsz
          CALL FATERR(IAM,cout)
       END IF
    END DO

104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)

  END SUBROUTINE read_ini_Vloc_Rloc
!!!------------------------------------------------------------------------ 
  SUBROUTINE write_out_Vloc_Rloc(nfich)

    IMPLICIT NONE
    
    INTEGER :: iadj,icdtact
    INTEGER :: nfich,icdan,icdtac,iantac

    character(len=20) :: fmt
    character(len=5) :: cdmodel, anmodel
    
    nb_CYLND=get_nb_CYLND()
    
    IF (nb_CDCDx==0) RETURN
    
    do icdtact = 1, nb_CYLND    

       do iadj = 1, nb_adj(icdtact)

          icdan  = adjac(icdtact)%icdan(iadj)
          icdtac = this(icdan)%icdtac
          iantac = this(icdan)%iantac

          cdmodel = get_body_model_name_from_id( cylnd2bdyty(3,icdtac) )
          anmodel = get_body_model_name_from_id( cylnd2bdyty(3,iantac) )

          WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CDCDx',icdan  
          !1234567890123456789012345678901234567890123456789012345678901234567890124567
          WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
          WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               !pta old fashion 'RBDY3',CYLND2bdyty(1,icdtac),'CYLND',CYLND2bdyty(2,icdtac),  &
               cdmodel,get_visibleID_CYLND(icdtac),'CYLND',CYLND2bdyty(2,icdtac),  &
               see(this(icdan)%isee)%behav,  &
               !pta 'RBDY3',cylnd2bdyty(1,iantac),'CYLND',cylnd2bdyty(2,iantac),  &
               anmodel,get_visibleID_CYLND(iantac),'CYLND',cylnd2bdyty(2,iantac),  &
               get_contact_status_name_from_id(this(icdan)%status)
          WRITE(nfich,104) 'rls/H',this(icdan)%rls/H,'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H
          WRITE(nfich,104) 'vls =',this(icdan)%vls  ,'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  
          WRITE(nfich,103) 'gTT =',this(icdan)%gapTT
          WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)
          WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
          WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
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
  SUBROUTINE nullify_reac_CDCDx(icdan,storage)
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in):: icdan 
    INTEGER           :: icdbdy,ianbdy
    INTEGER           :: storage
    
    icdbdy=this(icdan)%icdbdy
    CALL nullify_reac_CYLND(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL nullify_reac_CYLND(ianbdy,storage)
    
  END SUBROUTINE nullify_reac_CDCDx
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_CDCDx(icdan,storage)
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy,storage
    
    icdbdy = this(icdan)%icdbdy
    CALL nullify_vlocy_CYLND(icdbdy,storage)
    
    ianbdy = this(icdan)%ianbdy
    CALL nullify_vlocy_CYLND(ianbdy,storage)
    
  END SUBROUTINE nullify_vlocy_CDCDx
  !------------------------------------------------------------------------ 
  !------------------------------------------------------------------------ 
  SUBROUTINE vitrad_CDCDx( icdan, storage, need_full_vlocy )
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy
    INTEGER            :: storage
    logical, optional  :: need_full_vlocy
    
    icdbdy=this(icdan)%icdbdy
    CALL comp_vlocy_CYLND(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL comp_vlocy_CYLND(ianbdy,storage)
    
  END SUBROUTINE vitrad_CDCDx
  !------------------------------------------------------------------------  
  SUBROUTINE injj_CDCDx(icdan,RSIK,RTIK,RNIK,storage)
    
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
   anreac(1) =-cdreac(1)
   cdccdof(2)= 2
   anccdof(2)= 2
   cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   anreac(2) =-cdreac(2)
   cdccdof(3)= 3
   anccdof(3)= 3
   cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   anreac(3) =-cdreac(3)
   cdccdof(4)= 4
   anccdof(4)= 4
   cdccdof(5)= 5
   anccdof(5)= 5
   cdccdof(6)= 6
   anccdof(6)= 6


   cdreac(4) = this(icdan)%Gcds(1)*RSIK+this(icdan)%Gcdt(1)*RTIK+this(icdan)%Gcdn(1)*RNIK
   cdreac(5) = this(icdan)%Gcds(2)*RSIK+this(icdan)%Gcdt(2)*RTIK+this(icdan)%Gcdn(2)*RNIK
   cdreac(6) = this(icdan)%Gcds(3)*RSIK+this(icdan)%Gcdt(3)*RTIK+this(icdan)%Gcdn(3)*RNIK

   anreac(4) = -this(icdan)%Gans(1)*RSIK-this(icdan)%Gant(1)*RTIK-this(icdan)%Gann(1)*RNIK
   anreac(5) = -this(icdan)%Gans(2)*RSIK-this(icdan)%Gant(2)*RTIK-this(icdan)%Gann(2)*RNIK
   anreac(6) = -this(icdan)%Gans(3)*RSIK-this(icdan)%Gant(3)*RTIK-this(icdan)%Gann(3)*RNIK

   CALL add_reac_CYLND(icdbdy,cdccdof,cdreac,storage)
   CALL add_reac_CYLND(ianbdy,anccdof,anreac,storage)

 END SUBROUTINE injj_CDCDx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_CDCDx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: icdbdy,ianbdy
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy
   Vcd = get_vlocy_CYLND(icdbdy,storage)
   Van = get_vlocy_CYLND(ianbdy,storage)      

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

 END SUBROUTINE prjj_CDCDx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_CDCDx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_CDCDx = nb_CDCDx
    CASE(i_verlet_tactor)
       get_nb_CDCDx = nb_vCDCDx
    CASE(i_rough_tactor)
       get_nb_CDCDx = nb_rough_CDCDx
    CASE(i_recup_tactor)
       get_nb_CDCDx = nb_recup_CDCDx
    END SELECT

  END FUNCTION get_nb_CDCDx
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
SUBROUTINE CDCDx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_CYLND(this(icdan)%icdbdy)
   ianent = get_ENT_CYLND(this(icdan)%ianbdy)

 END SUBROUTINE CDCDx2ENT
!------------------------------------------------------------------------ 
SUBROUTINE CDCDx2CYLND(icdan,icdtac,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac
   
 END SUBROUTINE CDCDx2CYLND
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------
!!! < md < !!!
!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_CDCDx()

    IMPLICIT NONE
    
    RUN_CDCDx = RUN_TACTOR

  END FUNCTION RUN_CDCDx
!!!------------------------------------------------------------------------
  logical function CHECK_CDCDx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_CDCDx = check_CDCDx_
      return
    end if

    con_pedigree%module_name = 'CDCDx'

    con_pedigree%id_cdan  = i_cdcdx
    con_pedigree%id_cdtac = i_cylnd
    con_pedigree%id_antac = i_cylnd

    cdtact2bdyty => cylnd2bdyty
    antact2bdyty => cylnd2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_CYLND = get_nb_CYLND()
    if( nb_CYLND < 2 ) then
      CHECK_CDCDx = check_CDCDx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'CYLND' .and. see(isee)%antac == 'CYLND') then
        check_CDCDx_ = .true.
        exit
      end if
    end do

    CHECK_CDCDx = check_CDCDx_
    return

  end function CHECK_CDCDx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_CDCDx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_CDCDx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_CDCDx
!!!--------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_surf_CDCDx(icdan)

    IMPLICIT NONE
    INTEGER      :: icdan
    ! ***
    ! half heigh , radius
    REAL(Kind=8),dimension(2)    :: DATA_cylnd_cd, DATA_cylnd_an
    !
    integer(kind=4) :: icdtac,iantac
    !
    real(kind=8) :: reff
    !
    ! cette surface n'a de sens que si les cylindres sont alignes
    !
    icdtac = this(icdan)%icdtac
    iantac = this(icdan)%iantac

    CALL get_data(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac),DATA_cylnd_cd)
    CALL get_data(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac),DATA_cylnd_an)

    reff = DATA_cylnd_cd(2)*DATA_cylnd_an(2)/(DATA_cylnd_cd(2)+DATA_cylnd_an(2))
    
    get_surf_CDCDx = reff*min(DATA_cylnd_cd(1),DATA_cylnd_an(1))

  END FUNCTION get_surf_CDCDx
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
!!!------------------------------------------------------------------------ 
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
    REAL(kind=8),DIMENSION(3,3) :: localframe_cd,localframe_an
    REAL(kind=8),DIMENSION(3,3) :: Rc
    REAL(kind=8),DIMENSION(3)   :: cdlev,anlev
    REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin

    icdan = icdan + 1

    nb_adj(icdtac) = nb_adj(icdtac) + 1
    iadj           = nb_adj(icdtac)

    this(icdan)%icdbtac = cylnd2bdyty(2, icdtac)
    this(icdan)%ianbtac = cylnd2bdyty(2, iantac)

    this(icdan)%icdbtyp = cylnd2bdyty(3, icdtac)
    this(icdan)%ianbtyp = cylnd2bdyty(3, iantac)

    this(icdan)%icdctyp = i_cylnd
    this(icdan)%ianctyp = i_cylnd

    this(icdan)%icdsci  = 0
    this(icdan)%iansci  = 0

    this(icdan)%iadj    = iadj
    this(icdan)%icdbdy  = cylnd2bdyty(1, icdtac)
    this(icdan)%icdtac  = icdtac
    this(icdan)%ianbdy  = cylnd2bdyty(1, iantac)
    this(icdan)%iantac  = iantac
    this(icdan)%isee    = rough_CDCDx(itac)%isee
    this(icdan)%coor    = xco

    this(icdan)%nuc     = sep

    this(icdan)%gapTTbegin = gap
          
    call comp_rep(this(icdan)%tuc,this(icdan)%nuc,this(icdan)%suc)

    coorcd = CDcoor(1:3,icdtac)
    cooran = CDcoor(1:3,iantac)

    cooran(1) = cooran(1) + (real(rough_CDCDx(itac)%xperiodic,8)*xperiode)
    cooran(2) = cooran(2) + (real(rough_CDCDx(itac)%yperiodic,8)*yperiode)
    
    localframe_cd = get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac))
    !localframe_cd = MATMUL(get_inertia_frameTT_CYLND(cylnd2bdyty(1,icdtac)), &
    !                       get_embeded_frame(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac)))
   
    localframe_an = get_inertia_frameTT_CYLND(cylnd2bdyty(1,iantac))
    !localframe_an = MATMUL(get_inertia_frameTT_CYLND(cylnd2bdyty(1,iantac)), &
    !                       get_embeded_frame(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac)))
   
    
    ! Je fait le shift dans le doute mais a priori il est nul... 
    cdlev = xco(1:3)- (coorcd(1:3)-get_shiftTT_CYLND(cylnd2bdyty(1,icdtac),cylnd2bdyty(2,icdtac)))
    anlev = xco(1:3)- (cooran(1:3)-get_shiftTT_CYLND(cylnd2bdyty(1,iantac),cylnd2bdyty(2,iantac)))

    cd_ent = get_ent_CYLND(this(icdan)%icdbdy)
    an_ent = get_ent_CYLND(this(icdan)%ianbdy)

    this(icdan)%icdent = cd_ent
    this(icdan)%ianent = an_ent

    entity(cd_ent)%nb = entity(cd_ent)%nb+1
    entity(an_ent)%nb = entity(an_ent)%nb+1             
             
    ! On va calculer le passage rep inertie -> rep gnral pour le candidat

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
           
    ! On va calculer le passage rep inertie -> rep gnral pour an
    
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
    an_Vbegin = get_vlocy_CYLND(cylnd2bdyty(1,iantac),iVbeg_)
    
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

    this(icdan)%meff    = rough_CDCDx(itac)%meff
    this(icdan)%reff    = rough_CDCDx(itac)%reff


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





!------------------------------------------------------------------------ 
!fd 26/02/08 experimental 
! -routine pour fixer le nombre d'interactions
! -algo de detection qui genere plusiers points de contact une fois pour toute
! 
!!!------------------------------------------------------------------------
  SUBROUTINE Set_NbInteractionByContact(ivalue)

    IMPLICIT NONE
    INTEGER,INTENT(IN) :: ivalue
  ! A COMPLETER

  END SUBROUTINE Set_NbInteractionByContact

  SUBROUTINE Set_ContactRadius(rvalue)

    IMPLICIT NONE
    REAL(KIND=8),INTENT(IN) :: rvalue
  ! A COMPLETER


  END SUBROUTINE Set_ContactRadius

!------------------------------------------------------------------------

  subroutine clean_memory_CDCDx
    implicit none
    integer(kind=4) :: i, j, k

    call clean_memory_inter_meca_()

    nb_CYLND       = 0
    nb_CDCDx       = 0
    nb_vCDCDx      = 0
    nb_recup_CDCDx = 0

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

    nb_rough_CDCDx = 0
    if( allocated(rough_CDCDx) ) deallocate(rough_CDCDx)

    ! Root, Current and Previous should always be null outside creation_tab_visu

    if( allocated(CDcoor) ) deallocate(CDcoor)

    Reac_CDCDx_MAX = 0.D0

    module_checked_ = .FALSE.
    check_CDCDx_    = .FALSE.

  end subroutine

 subroutine set_nb_CDCDx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CDCDx = nb

 end subroutine

 subroutine redo_nb_adj_CDCDx()
   implicit none

   call redo_nb_adj_( get_nb_CYLND() )

 end subroutine

END MODULE CDCDx

