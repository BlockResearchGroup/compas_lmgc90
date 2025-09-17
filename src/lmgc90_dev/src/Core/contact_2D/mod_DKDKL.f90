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
MODULE DKDKL                                          

  !!****h* LMGC90.CORE/DKDKL
  !! NAME
  !!  module DKDKL
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors DISKx and DISKL.
  !!  In this modulus candidate contactors are DISKx and antagonist 
  !!  contactors are DISKL
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/DISKx
  !!  LMGC90.CORE/MAILx
  !!  LMGC90.CORE/DISKL
  !!****

  USE overall
  USE tact_behaviour
  USE DISKx, only : get_nb_diskx, diskx2bdyty           , &
                    print_info_diskx, get_radius_DISKx  , &
                    get_ent_diskx       => get_ent      , &
                    get_color_diskx     => get_color    , &
                    get_visible_diskx   => get_visible  , &
                    get_coor_diskx      => get_coor     , &
                    get_coorTT_diskx    => get_coorTT   , &
                    get_Vbegin_DISKx    => get_Vbegin   , &
                    add_reac_diskx      => add_reac     , &
                    get_vlocy_diskx     => get_vlocy    , &
                    comp_vlocy_diskx    => comp_vlocy   , &
                    nullify_reac_diskx  => nullify_reac , &
                    nullify_vlocy_diskx => nullify_vlocy, &
                    get_mean_radius_DISKx               , &
                    get_max_radius_DISKx                , &
                    get_min_radius_DISKx                , &
                    get_mass_DISKx
  USE DISKL, only : get_nb_antac        => get_nb_diskl        ,&
                    nullify_reac_antac  => nullify_reac_diskl  ,&
                    nullify_vlocy_antac => nullify_vlocy_diskl , &
                    comp_vlocy_antac    => comp_vlocy_diskl    , &
  !
  ! RIP
  !
                    get_nb_diskl,nullify_reac_diskl,nullify_vlocy_diskl,comp_vlocy_diskl, &
                    get_coorTT_diskl,diskl2bdyty, &
                    get_visible_diskl,get_ent_diskl,get_radius_diskl,get_min_radius_diskl, &
                    get_max_radius_diskl,get_vlocy_diskl,add_reac_diskl
  
  
  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color
 
  use inter_meca_2D

  use parameters, only : i_dkdkl, i_diskx, i_diskl, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

  type( T_interaction ), dimension( : ), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 
  
  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  !------------------------------------------------------------------------ 

  INTEGER :: nb_DKDKL                  ! nb_DKKDx = number of selected candidates DISKx against xKSID
                                       ! <= size(this).
  INTEGER :: nb_vDKDKL

  !------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs DISKx-xKSID
                                                         ! to candidate contactor DISKx icdtac.

!------------------------------------------------------------------------ 


 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------
!------------------------------------------------------------------------ 

 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                              ! performed within a box and immediate surrounding boxes, see
                              ! subroutine enumerate_DKKDx.
                              
   INTEGER                               :: DKpopul   ! box(ibox1,ibox2)%popul: number of DISKx in box ibox1,ibox2;
   
   INTEGER, DIMENSION(:), POINTER        :: DKwhich   ! box(ibox1,ibox2)%which(ipopul):
                                                      ! rank in the list of contactors of DISKx labelled ipopul
                              ! in box ibox1,ibox2;
 
   INTEGER                               :: DKLpopul   ! box(ibox1,ibox2)%popul: number of xKSID in box ibox1,ibox2;
   
   INTEGER, DIMENSION(:), POINTER        :: DKLwhich   ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of xKSID labelled ipopul
                              ! in box ibox1,ibox2;
   
 END TYPE T_box 

 TYPE(T_box), DIMENSION(:,:),ALLOCATABLE  :: box      ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

 TYPE T_rough_DKDKL
                                                      ! définit le type de la liste des plus proches voisins
    INTEGER :: cd                                     ! le candidat, l'antagoniste et isee pour la loi de contact
    INTEGER :: an
    INTEGER :: isee

!!! > md > !!!
   REAL(kind=8)               :: meff,reff            ! effective mass and radius for md method 
!!! < md < !!!

    INTEGER      :: periodic                          ! periodic contact flag

 END TYPE T_rough_DKDKL
 
 TYPE(T_rough_DKDKL),DIMENSION(:),ALLOCATABLE   :: rough_DKDKL            ! table  de visibilité

 TYPE T_link_rough_DKDKL                                                 ! liste chainée pour déterminer les listes de cand anta car
                                                                        ! on ne connait pas le nb de cand -ant à priori
    TYPE(T_link_rough_DKDKL), POINTER :: p                               ! pointeur sur le precedent
    TYPE(T_rough_DKDKL)               :: val                             ! les valeurs
    TYPE(T_link_rough_DKDKL), POINTER :: n                               ! pointeur sur le suivant

 END TYPE T_link_rough_DKDKL

 TYPE(T_link_rough_DKDKL),POINTER              :: Root,Current,Previous

 INTEGER                                       :: Nstep_rough_seek_DKDKL=1
 LOGICAL                                       :: write_creation_tab_visu

!------------------------------------------------------------------------
! variables attached to surrounding boxes

 REAL (kind=8)  :: maxray, minray, maxalert, meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: nb_rough_DKDKL
 integer        :: nb_recup_DKDKL
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul
!------------------------------------------------------------------------

 REAL(kind=8)   :: Reac_DKDKL_MAX=0.D0
 real(kind=8), dimension(:)  , allocatable, target :: violation 
 real(kind=8), dimension(:,:), allocatable, target :: DKcoor  ! coordinates of body owning DISKx to be used in selecting prox tactors
 real(kind=8), dimension(:,:), allocatable, target :: DKLcoor ! coordinates of body owning xKSID to be used in selecting prox tactors

!------------------------------------------------------------------------

 LOGICAL      :: PERIODIC=.FALSE.
 REAL(KIND=8) :: PERIODE = 0.d0
 INTEGER      :: nb_PERIODIC_DKDKL

 INTEGER,DIMENSION(:),ALLOCATABLE   :: periodic_DKDKL

!------------------------------------------------------------------------
 LOGICAL :: RUN=.FALSE.

 logical :: module_checked_ = .FALSE.
 logical :: check_DKDKL_    = .FALSE.

!------------------------------------------------------------------------
! public functions 
  PUBLIC &
      stock_rloc_DKDKL, &
      recup_rloc_DKDKL, &
      smooth_computation_DKDKL, &
      set_periodic_data_DKDKL, &
      compute_box_DKDKL, &
      read_ini_Vloc_Rloc_DKDKL, &
      write_xxx_Vloc_Rloc_DKDKL, &
      coor_prediction_DKDKL, &
      creation_tab_visu_DKDKL, &
      compute_contact_DKDKL, &
      display_prox_tactors_DKDKL, &
      RUN_DKDKL, &
      CHECK_DKDKL, &
      get_write_Vloc_Rloc_DKDKL

 PUBLIC &
      nullify_reac_DKDKL, &
      nullify_vlocy_DKDKL, &
      injj_DKDKL, prjj_DKDKL, vitrad_DKDKL, & 
      get_nb_DKDKL, &
      DKDKL2DISKx, DKDKL2DISKL, &
      print_info_DKDKL, &
      get_length_DKDKL, &
      get_periode_DKDKL, &
      get_icdtac_DKDKL, &
      get_iantac_DKDKL, &
      clean_memory_DKDKL

 !rm for handler
 public get_this    , &
        set_nb_DKDKL, &
        redo_nb_adj_DKDKL, &
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
  include 'interaction_common_2D.f90'
  ! defines the following subroutines
  !function get_verlet_tact_lawnb( icdtac, iadj )

!------------------------------------------------------------------------
!!!------------------------------------------------------------------------
  SUBROUTINE coor_prediction_DKDKL

    IMPLICIT NONE
    
    INTEGER      :: itact,errare
    INTEGER      :: nb_DISKx,nb_DISKL

    nb_DISKx=get_nb_DISKx()
    nb_DISKL=get_nb_DISKL()

    IF (smooth_method) THEN
       
       DO itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coor_DISKx(itact)
       END DO
       DO itact=1,nb_DISKL
          DKLcoor(1:3,itact) = get_coorTT_DISKL(itact)
       END DO
    ELSE 
       DO itact=1,nb_DISKx
          DKcoor(1:3,itact) = get_coorTT_DISKx(itact)
          
          IF( PERIODIC ) THEN
             IF( DKcoor(1,itact)  > periode ) THEN
                !print*,'on corrige le DISKx ',itact,' qui sort par x+'
                DKcoor(1,itact) = DKcoor(1,itact) - periode
             ELSE IF( DKcoor(1,itact) < 0.D0 ) THEN
                !print*,'on corrige le DISKx ',itact,' qui sort par x-'
                DKcoor(1,itact) = DKcoor(1,itact) + periode
             END IF
          END IF
          
       END DO
       
       DO itact=1,nb_DISKL
          DKLcoor(1:3,itact) = get_coorTT_DISKL(itact)

          IF( PERIODIC ) THEN
             IF( DKLcoor(1,itact)  > periode ) THEN
                !print*,'on corrige le DISKL ',itact,' qui sort par x+'
                DKLcoor(1,itact) = DKLcoor(1,itact) - periode
             ELSE IF( DKcoor(1,itact) < 0.D0 ) THEN
                !print*,'on corrige le DISKx ',itact,' qui sort par x-'
                DKLcoor(1,itact) = DKLcoor(1,itact) + periode
             END IF
          END IF
       END DO
    END IF

  END SUBROUTINE coor_prediction_DKDKL
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_DKDKL(step)
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
    
  end subroutine read_ini_Vloc_Rloc_DKDKL
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_DKDKL(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_DKDKL
!!!------------------------------------------------------------------------
  SUBROUTINE set_periodic_data_DKDKL(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    periode  = per
    PERIODIC = FLAG
    
  END SUBROUTINE set_periodic_data_DKDKL
!!!------------------------------------------------------------------------
 SUBROUTINE compute_box_DKDKL

   IMPLICIT NONE

   INTEGER      :: isee,errare,ibdy
   INTEGER      :: nb_DISKx,nb_DISKL

                              !1234567890123456789012
   character(len=22) :: IAM = 'mod_DKDKL::compute_box'

   nb_DISKx=get_nb_DISKx()
   nb_DISKL=get_nb_DISKL()

   ! on fait ici les choses qui ne doivent que lorsque nb_DISKx change

   minray     = get_min_radius_DISKx()
   maxray     = get_max_radius_DISKx()
   meanradius = get_mean_radius_DISKx()
   minray     = MIN(minray,get_min_radius_DISKL())
   maxray     = MAX(maxray,get_max_radius_DISKL())

   IF (minray > maxray ) THEN
    call faterr(IAM,'Messing error computing minray and maxray')
   END IF

   ! computing largest alert distance between disks 
   maxalert=0.D0  
   DO isee=1,SIZE(see)
     IF (see(isee)%cdtac == 'DISKx' .AND. see(isee)%antac == 'DISKL') THEN
       maxalert=MAX(maxalert,see(isee)%alert)
     END IF
   END DO

   Lbox   = 1.01D0*(2.D0*maxray + maxalert)
   Lbox_1 = 1.D0/Lbox
   norm   = Lbox/minray

   IF (.NOT. ALLOCATED(adjac))THEN
     ALLOCATE(adjac(nb_DISKx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO ibdy=1,nb_DISKx
       NULLIFY(adjac(ibdy)%icdan)
     ENDDO
   ELSE
     DO ibdy=1,nb_DISKx
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       NULLIFY(adjac(ibdy)%icdan)
     ENDDO
   ENDIF  
  
   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_DISKx),stat=errare)
   IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating nb_adj')
   END IF

   nb_adj=0

   ! DKcoor are coordinates of bodies owning DISKx to be used in selecting prox tactors
   IF (ALLOCATED(DKcoor)) DEALLOCATE(DKcoor)
   ALLOCATE(DKcoor(3,nb_DISKx),stat=errare)
   ! DKLcoor are coordinates of bodies owning DISKL to be used in selecting prox tactors
   IF (ALLOCATED(DKLcoor)) DEALLOCATE(DKLcoor)
   ALLOCATE(DKLcoor(3,nb_DISKL),stat=errare)


 END SUBROUTINE compute_box_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 SUBROUTINE creation_tab_visu_DKDKL
 
   IMPLICIT NONE 
 
   INTEGER                               :: errare 

   INTEGER                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
   INTEGER                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty   
   REAL(kind=8)                          :: Bleft,Bright,Bup,Bdown
   CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol,cdbdyty
   REAL(kind=8),DIMENSION(3)             :: coord,coordcd,coordan 
   REAL(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gapTT,masscd
   INTEGER      :: nb_DISKx,nb_DISKL

   character(len=80) :: cout
   character(len=28) :: IAM = 'mod_DKDKL::creation_tab_visu'

   nb_DISKx=get_nb_DISKx()
   nb_DISKL=get_nb_DISKL()

! Since the list of proximate contactors may not be updated at every time step,
! boxes data would be lost if deallocated. When starting the program, boxes are not created.
! A warning condition prevents undue deallocation. 

   IF (ALLOCATED(box)) THEN
     DO ibox1=minibox1,maxibox1
       DO ibox2=minibox2,maxibox2
         IF (ASSOCIATED(box(ibox1,ibox2)%DKwhich)) DEALLOCATE(box(ibox1,ibox2)%DKwhich)
         IF (ASSOCIATED(box(ibox1,ibox2)%DKLwhich))DEALLOCATE(box(ibox1,ibox2)%DKLwhich)
       ENDDO
     ENDDO
     DEALLOCATE(box)
   ENDIF
    
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

   Bleft    =  1.D24
   Bright   = -1.D24
   Bup      = -1.D24
   Bdown    =  1.D24

   DO ibdy=1,nb_DISKx
     coord = DKcoor(1:3,ibdy)
     Bleft = MIN(coord(1),Bleft )
     Bright= MAX(coord(1),Bright)
     Bup   = MAX(coord(2),Bup   )
     Bdown = MIN(coord(2),Bdown )
   END DO

   DO ibdy=1,nb_DISKL
     coord = DKLcoor(1:3,ibdy)
     Bleft = MIN(coord(1),Bleft )
     Bright= MAX(coord(1),Bright)
     Bup   = MAX(coord(2),Bup   )
     Bdown = MIN(coord(2),Bdown )
   END DO
 
   !if (minray < 0.1D-02*(Bright-Bleft) .or. minray < 0.1D-02*(Bup-Bdown)) then
   !   print*,' minray is quite small, in select_prox_tactors in mod_DKDKL'
   !  write(*,'(A9,D14.7)')'  minray=',minray
   !  !stop
   !end if   
   !if (maxray > (Bright-Bleft) .or. maxray > (Bup-Bdown)) then
   !  print*,' maxray is quite large, in select_prox_tactors in mod_DKDKL'
   !  write(*,'(A9,D14.7)')'  maxray=',maxray
   !end if 

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

   IF ( PERIODIC ) THEN
      !     print*,'Bright',Bright,'Bleft',Bleft,' Lbox ',Lbox
      !     print*,INT(periode*Lbox_1)*Lbox,(1+(INT(periode)*Lbox_1))*Lbox
      
      IF (Bright > periode) THEN
         write(cout,'(2(A,D14.7))') 'Bright ',Bright,' Periode ',periode
         call faterr(IAM,cout)
         !     else if (Bright < periode - LBox) then
         !       print*,'Bright ',Bright,' Periode ',periode,' Lbox ',Lbox
         !       call FATERR(IAM,'the max right column will be empty !')
      ENDIF
      
      IF (Bleft < 0.d0) THEN
         write(cout,'(A,D14.7)') 'Bleft ',Bleft
         call faterr(IAM,cout)
         !     else if (Bleft > LBox) then
         !       print*,'Bleft ',Bleft,' Lbox ',Lbox
         !       call FATERR(IAM,'the min left column will be empty !')
      ENDIF
      
      !fd pas sur que ca soit si malin ...
      !     if (INT(periode*Lbox_1)*Lbox > Bright) then
      !       Bright = periode - LBox !+ maxalert
      !     else
      Bright = periode
      !     endif
      Bleft  = 0.d0    !- maxalert
   END IF

   minibox1 =-1
   maxibox1 = 1 + INT((Bright-Bleft)*Lbox_1)
   minibox2 =-1
   maxibox2 = 1 + INT((Bup - Bdown )*Lbox_1)
   maxpopul = (1+INT(norm))*(1+INT(norm))
   !
   ! for each box maxpopul is less than the total number of DISKx 
   !
   maxpopul=MIN(maxpopul,nb_DISKx+nb_DISKL)
   !   
   ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)
  
   IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating box')
   END IF

   DO ibox1=minibox1,maxibox1
     DO ibox2=minibox2,maxibox2
       box(ibox1,ibox2)%DKpopul=0
       box(ibox1,ibox2)%DKLpopul=0
       ALLOCATE(box(ibox1,ibox2)%DKwhich(maxpopul),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%DKwhich'
         call faterr(IAM,cout)
       END IF
       ALLOCATE(box(ibox1,ibox2)%DKLwhich(maxpopul),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A,I0,A)') 'Error allocating box(',ibox1,',',ibox2,')%KDwhich'
         call faterr(IAM,cout)
       END IF
     END DO
   END DO
  
   ! filling boxes with disks
   ! box(ibox1,ibox2)%DKpopul is the number of disks into the box (ibox1,ibox2)
   ! box(ibox1,ibox2)%DKLwhich(ipopul) is the rank of body DISKx labelled ipopul in the box
  
   ! filling boxes   

   DO ibdy=1,nb_DISKx
     coord=DKcoor(1:3,ibdy)
     ibox1=1+INT((coord(1)-Bleft )*Lbox_1)
     ibox2=1+INT((coord(2)-Bdown )*Lbox_1)
     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2) THEN
       write(cout,'(A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       write(cout,'(A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2
       write(cout,'(A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2
       write(cout,'(A13,I5,A13)')'  body DISKx ',ibdy,' out of boxes'
       call faterr(IAM,cout)
     END IF
     box(ibox1,ibox2)%DKpopul=box(ibox1,ibox2)%DKpopul+1
     if( box(ibox1,ibox2)%DKpopul > size(box(ibox1,ibox2)%DKwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for DK.")
     end if
     box(ibox1,ibox2)%DKwhich(box(ibox1,ibox2)%DKpopul)=ibdy
   END DO

   DO ibdy=1,nb_DISKL
     coord=DKLcoor(1:3,ibdy)
     ibox1=1+INT((coord(1)-Bleft )*Lbox_1)
     ibox2=1+INT((coord(2)-Bdown )*Lbox_1)
     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2) THEN
       write(cout,'(A,I0,A,I0)') ' maxibox1=',maxibox1,'maxibox2=',maxibox2
       write(cout,'(A,I0,A,I0)') '    ibox1=',ibox1,   '   ibox2=',ibox2
       write(cout,'(A,I0,A,I0)') ' minibox1=',minibox1,'minibox2=',minibox2
       write(cout,'(A13,I5,A13)')'  body DISKL ',ibdy,' out of boxes'
       call faterr(IAM,cout)
     END IF
     box(ibox1,ibox2)%DKLpopul=box(ibox1,ibox2)%DKLpopul+1
     if( box(ibox1,ibox2)%DKLpopul > size(box(ibox1,ibox2)%DKLwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for DKL.")
     end if
     box(ibox1,ibox2)%DKLwhich(box(ibox1,ibox2)%DKLpopul)=ibdy
   END DO
  
   ! Detecting contacts; 
   ! contacts are being detected within a box and immediate surrounding boxes;  
  
   nb_rough_DKDKL=0
  
   ! création de la liste de paire à examiner
  
   ! on esalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on alloue un zone memoire au fur et à mesure que l'on determine un candidat - antagoniste

   NULLIFY(Root)
   NULLIFY(Current)
   NULLIFY(Previous)
  
   DO ibox1cd=minibox1,maxibox1  
   DO ibox2cd=minibox2,maxibox2
    DO icdpop=1,box(ibox1cd,ibox2cd)%DKpopul
      icdtac=box(ibox1cd,ibox2cd)%DKwhich(icdpop)
      cdcol=get_color_DISKx(icdtac)
      ! box loop investigating antagonist DISKL
      DO ibox1an=MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)                        
      DO ibox2an=MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
        DO ianpop=1,box(ibox1an,ibox2an)%DKLpopul
          iantac=box(ibox1an,ibox2an)%DKLwhich(ianpop)
          ancol=get_color_MAILx(diskl2bdyty(1,iantac),diskl2bdyty(2,iantac))
          cdbdyty = get_body_model_name_from_id(diskx2bdyty(3,icdtac))
          isee=get_isee(cdbdyty,'DISKx',cdcol,'MAILx','DISKL',ancol)
          IF (isee /= 0) THEN
            adist=see(isee)%alert 
            ! checking ROUGHLY distance against alert distance           
            coordcd = DKcoor(1:3,icdtac)
            coordan = DKLcoor(1:3,iantac)
            raycd = get_radius_DISKx(icdtac)
            rayan = get_radius_DISKL(iantac)
            ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
            ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
            ! results might be different up to some non significant figures, but when comparing to
            ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
            adist=0.1005D+01*adist
            nonuc=dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)                 
            gapTT=nonuc-(raycd+rayan)
        ! checking distance against alert distance 
            IF (gapTT .LE. adist) THEN
              nb_rough_DKDKL=nb_rough_DKDKL+1
              IF ( nb_rough_DKDKL == 1) THEN
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
              Current%val%periodic = 0
                  
              Current%p => Previous
              NULLIFY(Current%n)
              Previous => Current
            ENDIF
          END IF
        ENDDO
      ENDDO
      ENDDO
    ENDDO
  END DO
  END DO

  nb_PERIODIC_DKDKL = 0
   
  IF ( PERIODIC ) THEN

     DO ibox1cd = maxibox1-1,maxibox1
     DO ibox2cd = minibox2,maxibox2

       DO icdpop = 1,box(ibox1cd,ibox2cd)%DKpopul
            
         icdtac = box(ibox1cd,ibox2cd)%DKwhich(icdpop)
         cdcol  = get_color_DISKx(icdtac)
         
         DO ibox1an = minibox1,minibox1+1
         DO ibox2an = MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   

           DO ianpop = 1,box(ibox1an,ibox2an)%DKLpopul
                  
             iantac = box(ibox1an,ibox2an)%DKLwhich(ianpop)
             ancol=get_color_MAILx(diskl2bdyty(1,iantac),diskl2bdyty(2,iantac))
             cdbdyty = get_body_model_name_from_id(diskx2bdyty(3,icdtac))
             isee  = get_isee(cdbdyty,'DISKx',cdcol,'MAILx','DISKL',ancol)
                  
             IF (isee /= 0 ) THEN
                adist   = see(isee)%alert 
                ! checking ROUGHLY distance against alert distance           
                coordcd = DKcoor(1:3,icdtac)
                coordan = DKcoor(1:3,iantac)
                raycd   = get_radius_DISKx(icdtac)
                rayan   = get_radius_DISKL(iantac)
                
                coordan(1) = coordan(1) + periode
                     
                ! Here, alert distance is 0.5% OVERESTIMATED so as to ensure a secure oversizing 
                ! of arrays. Indeed, the distance beeing computed twice, here and further on, the
                ! results might be different up to some non significant figures, but when comparing to
                ! alert distance, extra candidates to contact might be selected in ambiguous situations.   
                adist = 0.1005D+01*adist+raycd+rayan
                IF (       dabs(coordcd(1)-coordan(1)) <= adist &
                     .AND. dabs(coordcd(2)-coordan(2)) <= adist ) THEN
                   
                   nb_rough_DKDKL    = nb_rough_DKDKL+1
                   nb_PERIODIC_DKDKL = nb_PERIODIC_DKDKL + 1
                   
                   IF ( nb_rough_DKDKL == 1) THEN
                      ALLOCATE(Root)
                      Current => Root
                      NULLIFY(Root%p)
                   ELSE
                      ALLOCATE(Current)
                      Previous%n => Current
                   END IF
                   
                   Current%val%cd       = icdtac
                   Current%val%an       = iantac
                   Current%val%isee     = isee
                   Current%val%periodic = 1                  
                   Current%p => Previous
                   NULLIFY(Current%n)
                   Previous => Current
                END IF
             END IF
          END DO
        END DO
      END DO
      END DO
    END DO
    END DO
  END IF

  WRITE(cout,'(4X,I10,A20)') nb_rough_DKDKL,' DKDKL roughly found'
  call logmes(cout)

  IF (ALLOCATED(periodic_DKDKL)) DEALLOCATE(periodic_DKDKL)
  ALLOCATE(periodic_DKDKL(nb_rough_DKDKL)) 
  
  IF (ALLOCATED(rough_DKDKL)) DEALLOCATE(rough_DKDKL)
  ALLOCATE(rough_DKDKL(nb_rough_DKDKL))      ! the visibility array used in compute_contact is allocated
  
  IF (ALLOCATED(this)) DEALLOCATE(this)
  ALLOCATE(this(nb_rough_DKDKL))             ! the oversized array this is temporaly allocated 

  DO icdan=nb_rough_DKDKL,1,-1
     
    Previous => Current%p
    rough_DKDKL(icdan)%cd       = Current%val%cd
    rough_DKDKL(icdan)%an       = Current%val%an
    rough_DKDKL(icdan)%isee     = Current%val%isee
    rough_DKDKL(icdan)%periodic = Current%val%periodic

    raycd = get_radius_DISKx(Current%val%cd)
    
    rough_DKDKL(icdan)%reff = raycd

    masscd=get_mass_DISKx(diskx2bdyty(1,Current%val%cd))

    rough_DKDKL(icdan)%meff = masscd

    DEALLOCATE(Current)
    Current => Previous
  END DO 
   
  NULLIFY(Root)
   
END SUBROUTINE creation_tab_visu_DKDKL
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
SUBROUTINE compute_contact_DKDKL
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: icdan,iadj,ibdy,icdtac,iantac,isee,itacty    
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
  REAL(kind=8),DIMENSION(3)             :: coordcd,coordan,cd_Vbegin,an_Vbegin
  REAL(kind=8)                          :: raycd,rayan,adist,dist,gapTT,ut1,ut2,un1,un2,Gant3,Gann3
  REAL(kind=8)                          :: cd_shift,an_shift
  INTEGER                               :: i,id,j
  REAL(kind=8)                          :: norm,nonuc                                ! scalaire contenant la norme de sep

  INTEGER                               :: cd_ent,an_ent,iprd

  INTEGER           :: lawnb
  integer(kind=4)   :: status
  REAL(kind=8)      :: kn,dampn,gamma,finet,mu
  REAL(kind=8)      :: gap,meff,reff,fadhs
  REAL(kind=8)      :: vijn,vijs,fijn,fijs
  REAL(kind=8)      :: fext1,fext2,fext3
  REAL(kind=8),DIMENSION(2) :: n,t,cdlev,anlev
  integer :: nb_DISKx

  character(len=80) :: cout

  icdan=0        
  nb_DKDKL=0

  nb_adj=0

  IF (nb_rough_DKDKL /= 0 ) THEN
!
! preparing detection 
!
    icdtac=1  !fd pour l'instant, c'est ok...
    iantac=1

    DO i=1,nb_rough_DKDKL
      icdtac=rough_DKDKL(i)%cd
      iantac=rough_DKDKL(i)%an
      isee=rough_DKDKL(i)%isee  
      iprd   = rough_DKDKL(i)%periodic
      adist=see(isee)%alert
!      if (Nstep == 1) adist = adist/100.D0   !!! modif mado       
      coordcd = DKcoor(1:3,icdtac)
      coordan = DKLcoor(1:3,iantac) + (REAL(iprd,8)*periode)
      raycd= get_radius_DISKx(icdtac)
      rayan= get_radius_DISKL(iantac)
      nonuc=dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)
      gapTT=nonuc-(raycd+rayan)
      ! checking distance against alert distance           
      IF (gapTT .LE. adist) THEN    

        icdan = icdan + 1
        nb_adj(icdtac) = nb_adj(icdtac) + 1

        iadj = nb_adj(icdtac)

!        if (smooth_method) then
!          cd_Vbegin = get_V_DISKx(diskx2bdyty(1,icdtac))
!          an_Vbegin = get_V_xKSID(xksid2bdyty(1,iantac))
!        else
          cd_Vbegin = get_Vbegin_DISKx(icdtac)
          CALL get_vlocy_DISKL(iantac,iVbeg_,an_Vbegin)
!        endif

        this(icdan)%icdbtac = diskx2bdyty(2, icdtac)
        this(icdan)%ianbtac = diskl2bdyty(2, iantac)

        this(icdan)%icdbtyp = diskx2bdyty(3, icdtac)
        this(icdan)%ianbtyp = diskl2bdyty(3, iantac)

        this(icdan)%icdctyp = i_diskx
        this(icdan)%ianctyp = i_diskl

        this(icdan)%iadj      =  iadj
        this(icdan)%icdbdy    =  diskx2bdyty(1,icdtac)
        this(icdan)%icdtac    =  icdtac
        this(icdan)%icdsci    = 0

        this(icdan)%ianbdy    =  diskl2bdyty(1,iantac)
        this(icdan)%iantac    =  iantac
        this(icdan)%iansci    = 0
        this(icdan)%isee      =  isee                 
        this(icdan)%nuc(1:2)  =  (coordcd(1:2)-coordan(1:2))/nonuc
        this(icdan)%tuc(1)    =  this(icdan)%nuc(2)
        this(icdan)%tuc(2)    = -this(icdan)%nuc(1)   
     
        cd_ent = get_ent_DISKx(this(icdan)%icdtac)
        an_ent = get_ent_DISKL(this(icdan)%iantac) 

        this(icdan)%icdent = cd_ent
        this(icdan)%ianent = an_ent

        entity(cd_ent)%nb = entity(cd_ent)%nb+1
        entity(an_ent)%nb = entity(an_ent)%nb+1

        this(icdan)%gapTTbegin =  gapTT

        periodic_DKDKL(icdan)  =  iprd

!fd beuuuuuuuuuuuuuuuuuuurk

        cd_shift = 0.D0
        an_shift = 0.D0

        cdlev= (-raycd*this(icdan)%nuc) + cd_shift
        anlev= ( rayan*this(icdan)%nuc) + an_shift
         
        n(1) = this(icdan)%nuc(1)
        n(2) = this(icdan)%nuc(2)               
        t(1)=n(2);t(2)=-n(1)

        this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
        this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)

        this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
        this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)

        this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdt3 &
                               - an_Vbegin(3)*this(icdan)%Gant3

        this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdn3 &
                               - an_Vbegin(3)*this(icdan)%Gann3

!fd modifs pour tenir compte de l'excentrement >


        this(icdan)%rlt       = 0.D0
        this(icdan)%rln       = 0.D0
        this(icdan)%vlt       = this(icdan)%vltBEGIN
        this(icdan)%vln       = this(icdan)%vlnBEGIN
        this(icdan)%gapTT     = this(icdan)%gapTTbegin
        this(icdan)%status    = i_nknow

        this(icdan)%coor      = DKcoor(1:2,icdtac) - raycd*this(icdan)%nuc(1:2)

        this(icdan)%reff    = rough_DKDKL(i)%reff
        this(icdan)%meff    = rough_DKDKL(i)%meff
        
      END IF
    END DO

    nb_DKDKL = icdan

  ENDIF

  WRITE(cout,'(1X,I10,A12)') nb_DKDKL,' DKDKL found'
  call logmes(cout)

  nb_DISKx=get_nb_DISKx()
  DO ibdy=1,nb_DISKx
    IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
    IF (nb_adj(ibdy) /= 0) THEN
      ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      IF (errare /=0 ) THEN
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_DKDKL::compute_contact_DKDKL',cout)
      END IF
    ELSE 
      NULLIFY(adjac(ibdy)%icdan)
    END IF
  END DO
  
  DO icdan=1,nb_DKDKL
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
  END DO
   
   
  do icdan = 1, nb_DKDKL
     call get_behaviour_( icdan, see, tact_behav )
  end do

  IF (ALLOCATED(violation)) DEALLOCATE(violation)
  ALLOCATE(violation(nb_DKDKL),stat=errare)

END SUBROUTINE compute_contact_DKDKL
!!!------------------------------------------------------------------------
  SUBROUTINE smooth_computation_DKDKL

    IMPLICIT NONE
    INTEGER          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    DO icdan=1,nb_DKDKL
       
       CALL compute_2D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            this(icdan)%reff,this(icdan)%meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    DO icdan=1,nb_DKDKL  
       CALL nullify_reac_DKDKL(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_DKDKL
       CALL injj_DKDKL(icdan,this(icdan)%rlt,this(icdan)%rln,iIreac)
    END DO
    
  END SUBROUTINE smooth_computation_DKDKL
!!!------------------------------------------------------------------------
 subroutine display_prox_tactors_DKDKL

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact,nb_DISKx
   character(len=5) :: cdmodel, anmodel

   nb_DISKx=get_nb_DISKx()

   DO icdtact=1,nb_DISKx    
     DO iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( diskl2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                          !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '       
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,diskl2bdyty(1,iantac),'DISKL',diskl2bdyty(2,iantac)
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       WRITE(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
     ! write(*,104)'rlt =',this(icdan)%rlt,'rln =',this(icdan)%rln,'rls =',0.D0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 end subroutine display_prox_tactors_DKDKL
!------------------------------------------------------------------------  

!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_DKDKL
 
   
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,nb_DISKx
   INTEGER :: errare

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_DKDKL::stock_rloc'
   nb_DISKx=get_nb_DISKx()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_DISKx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_DISKx
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
     DO icdtac=1,nb_DISKx
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
   DO icdan=1,nb_DKDKL
     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                        ! serial adjacent number of pair contactor 
                                                      ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)   = icdan
     verlt(icdtac)%cdbdy         = diskx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac         = diskx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel       = diskx2bdyty(3,icdtac)
     verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)   = diskl2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)   = diskl2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj) = diskl2bdyty(3,iantac)
     verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
     verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)     = this(icdan)%vln
     verlt(icdtac)%status(iadj)  = this(icdan)%status
     verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj)= this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   END DO

   nb_vDKDKL = nb_DKDKL

   WRITE(cout,'(1X,I10,A12)') nb_vDKDKL,' stock DKDKL'
   call logmes(cout)

 END SUBROUTINE stock_rloc_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_DKDKL
 
   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE

   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   CHARACTER(len=21) :: IAM = 'mod_DKDKL::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_DKDKL=0

   DO icdan=1,nb_DKDKL
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        

     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac)       &
          ) then
          do iadj = 1, verlt(icdtac)%adjsz
            if (verlt(icdtac)%anbdy(iadj)  == diskl2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == diskl2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== diskl2bdyty(3,iantac)       &
               ) then
               this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
               this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
               this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)

               nb_recup_DKDKL = nb_recup_DKDKL + 1
               exit
            end if
          end do
       end if
     END IF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_DKDKL,' recup DKDKL'
   call logmes(cout)

 END SUBROUTINE recup_rloc_DKDKL
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE
   CHARACTER(len=103) :: clin
   INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   INTEGER            :: cdmodel, anmodel
   REAL(kind=8)       :: rlt,rln,vlt,vln,gapTT
   CHARACTER(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   REAL(kind=8),DIMENSION(2) :: coor,nuc,tuc
   INTEGER            :: errare 
  
   INTEGER :: ibehav,nb_internal,i_internal
   INTEGER      :: nb_DISKx,nb_DISKL

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_DKDKL::read_ini_Vloc_Rloc'

   nb_DISKx=get_nb_DISKx()
   nb_DISKL=get_nb_DISKL()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_DISKx),stat=errare)
     IF (errare /=0 ) call faterr(IAM,'error allocating nb_adj')
   END IF
    
   DO icdtac=1,nb_DISKx
     nb_adj(icdtac)=0
   END DO

   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKDKL') CYCLE     
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT

     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     do icdtact = 1, nb_DISKx
       if (diskx2bdyty(1,icdtact) == icdbdy .and. &
           diskx2bdyty(2,icdtact) == icdtac .and. &
           diskx2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
     cycle
   END DO 
  
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_DISKx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtac=1,nb_DISKx
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
     DO icdtac=1,nb_DISKx
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

   DO icdtac=1,nb_DISKx
     nb_adj(icdtac)=0
   END DO
   icdan = 0

   DO    
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKDKL') CYCLE
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     DO icdtact=1,nb_DISKx
       IF (diskx2bdyty(1,icdtact) == icdbdy .and. &
           diskx2bdyty(2,icdtact) == icdtac .and. &
           diskx2bdyty(3,icdtact) == cdmodel ) then

         icdan = icdan + 1

         nb_adj(icdtact) = nb_adj(icdtact) + 1
         verlt(icdtact)%icdan(nb_adj(icdtact))  = icdan
         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,2(7X,D14.7))') rlt,rln
         verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
         verlt(icdtact)%rln(nb_adj(icdtact))=rln
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,2(7X,D14.7))')vlt,vln
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         IF( .NOT. read_G_clin()) EXIT 
         READ(G_clin(1:90),'(27X,1(7X,D14.7))')gapTT
         verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
         IF( .NOT. read_G_clin()) EXIT
         IF (G_clin(30:34)== 'n(1)=') THEN
           READ(G_clin(1:90),'(27X,2(7X,D14.7))')nuc(1),nuc(2)
           verlt(icdtact)%nuc(1,nb_adj(icdtact))=nuc(1)
           verlt(icdtact)%nuc(2,nb_adj(icdtact))=nuc(2)
         ELSE 
           BACKSPACE(G_nfich)
         END IF
         IF( .NOT. read_G_clin()) EXIT
         IF (G_clin(30:34)== 'coo1=') THEN
           READ(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
           verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
           verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)
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
       END IF
     END DO
     CYCLE
   END DO   

   nb_vDKDKL=0

   DO icdtact=1,nb_DISKx
     nb_vDKDKL = nb_vDKDKL + nb_adj(icdtact)
     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       write(cout,'(A,1x,I0)')   'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')   'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     ENDIF
   ENDDO

 END SUBROUTINE read_ini_Vloc_Rloc
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 SUBROUTINE write_out_Vloc_Rloc(nfich)

  !
  ! write into file out_Vloc_Rloc data from this, in verlt style
  !

   IMPLICIT NONE
   INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nfich,icdtact
   INTEGER :: lc
   REAL(kind=8),DIMENSION(2) :: coor
   integer :: nb_DISKL,nb_DISKx

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_DISKL=get_nb_DISKL()
   nb_DISKx=get_nb_DISKx()

   DO icdtact=1,nb_DISKx    
     DO iadj=1,nb_adj(icdtact)    
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       !mj Should rather be the coordinates of the mid gap point 
       ! (coordinates of the contact point if contact is active)
       coor(1) = DKcoor(1,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(1))
       coor(2) = DKcoor(2,icdtac)-(get_radius_DISKx(icdtac)*this(icdan)%nuc(2))

       cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( diskl2bdyty(3,iantac) )
       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKDKL',icdan 
                              !1234567890123456789012345678901234567890123456789012345678901234567890123456
       WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'       
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),  &
       see(this(icdan)%isee)%behav,  &
       anmodel,diskl2bdyty(1,iantac),'DISKL',diskl2bdyty(2,iantac),  &
       get_contact_status_name_from_id(this(icdan)%status),iantac
       WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls =',0.D0
       WRITE(nfich,104)'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',0.D0
       WRITE(nfich,103)'gapTT',this(icdan)%gapTT 
       WRITE(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
       WRITE(nfich,104)'coo1=',coor(1)           ,'coo2=',coor(2)           ,'coo3=',0.D0

       IF (this(icdan)%nb_internal /= 0) THEN
         CALL write_internal_comment(nfich,this(icdan)%lawnb)
         write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
         write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
       ENDIF
       WRITE(nfich,'(A1)')' '
       
     END DO                              
   END DO
 
103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 END SUBROUTINE write_out_Vloc_Rloc
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_reac_DKDKL(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdtac,iantac,storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_DISKx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_DISKL(iantac,storage)
    
 END SUBROUTINE nullify_reac_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_DKDKL(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdtac,iantac,storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_DISKx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_vlocy_DISKL(iantac,storage)
    
 END SUBROUTINE nullify_vlocy_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 subroutine vitrad_DKDKL(icdan,storage,need_full_vlocy)
   implicit none
   integer(kind=4), intent(in) :: icdan 
   integer(kind=4)             :: storage   
   logical                    ,  optional :: need_full_vlocy
   !
   integer(kind=4) :: icdtac,iantac
    
   icdtac=this(icdan)%icdtac
   CALL comp_vlocy_DISKx(icdtac,storage)
    
   iantac=this(icdan)%iantac
   CALL comp_vlocy_DISKL(iantac,storage,need_full_vlocy)
    
 END SUBROUTINE vitrad_DKDKL
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE injj_DKDKL(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RTIK,RNIK
   INTEGER,     DIMENSION(3)  :: cdccdof,anccdof
   REAL(kind=8),DIMENSION(3)  :: cdreac,anreac
   INTEGER                    :: icdtac,iantac,storage   
   
   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac
   cdccdof(1)=1
   anccdof(1)=1
   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)
   cdccdof(2)=2
   anccdof(2)=2
   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)=-cdreac(2)
   cdccdof(3)=3
   anccdof(3)=3
   cdreac(3)= this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK
   anreac(3)=-this(icdan)%Gant3*RTIK-this(icdan)%Gann3*RNIK

   CALL add_reac_DISKx(icdtac,cdccdof,cdreac,storage)
   CALL add_reac_DISKL(iantac,anreac,storage)

 END SUBROUTINE injj_DKDKL 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 subroutine prjj_DKDKL(icdan,VTIK,VNIK,storage)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   real(kind=8)   , intent(out) :: VTIK,VNIK
   integer(kind=4), intent(in)  :: storage
   !
   integer(kind=4)               :: icdtac,iantac
   real(kind=8)   , dimension(3) :: Vcd,Van
   
   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   CALL get_vlocy_DISKx(icdtac,storage,Vcd)
   CALL get_vlocy_DISKL(iantac,storage,Van) 

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3  &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3
   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3

 END SUBROUTINE prjj_DKDKL 
!------------------------------------------------------------------------ 
 integer function get_nb_DKDKL(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_DKDKL = nb_DKDKL
   case(i_verlet_tactor)
      get_nb_DKDKL = nb_vDKDKL
   case(i_rough_tactor)
      get_nb_DKDKL = nb_rough_DKDKL
   case(i_recup_tactor)
      get_nb_DKDKL = nb_recup_DKDKL
   end select

 end function get_nb_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE DKDKL2DISKx(icdan,icdtac)

   IMPLICIT NONE
   INTEGER          :: icdan,icdtac

   icdtac = this(icdan)%icdtac

 END SUBROUTINE DKDKL2DISKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE DKDKL2DISKL(icdan,iantac)

   IMPLICIT NONE
   INTEGER          :: icdan,iantac

   iantac = this(icdan)%iantac

 END SUBROUTINE DKDKL2DISKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE print_info_DKDKL(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'DISKx:',1x,I5,1x,'DISKL:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

!   call print_info_DISKx(icdbdy)
!   call print_info_DISKL(ianbdy)

END SUBROUTINE print_info_DKDKL
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
real(kind=8) function get_length_DKDKL(icdan)
  implicit none
  !
  integer(kind=4), intent(in) :: icdan 
  !
  integer(kind=4) :: icdtac
  real(kind=8)    :: raycd,rayan

  raycd = get_radius_DISKx(this(icdan)%icdtac)
  rayan = get_radius_DISKL(this(icdan)%iantac)

  get_length_DKDKL = (raycd*rayan)/(rayan+raycd)
  
end function get_length_DKDKL
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
LOGICAL FUNCTION RUN_DKDKL(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_DKDKL = RUN_TACTOR

END FUNCTION RUN_DKDKL
!------------------------------------------------------------------------
  logical function CHECK_DKDKL()
    implicit none
    !   
    integer :: isee, nb_DISKx, nb_DISKL
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_DKDKL = check_DKDKL_
      return
    end if

    con_pedigree%module_name = 'DKDKL'

    con_pedigree%id_cdan  = i_dkdkl
    con_pedigree%id_cdtac = i_diskx
    con_pedigree%id_antac = i_diskl

    cdtact2bdyty => diskx2bdyty
    antact2bdyty => diskl2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_DISKL = get_nb_DISKL()
    nb_DISKx = get_nb_DISKx()
    IF( nb_DISKL == 0 .or. nb_DISKx == 0 ) then
      CHECK_DKDKL = check_DKDKL_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'DISKL') then
        check_DKDKL_ = .true.
        exit
      end if
    end do
  
    CHECK_DKDKL = check_DKDKL_
    return
  
  end function CHECK_DKDKL
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_DKDKL(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome
 
    get_write_Vloc_Rloc_DKDKL = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_DKDKL
!------------------------------------------------------------------------ 
 INTEGER FUNCTION get_periode_DKDKL(icdtac)

   IMPLICIT NONE

   INTEGER :: icdtac

   IF(.NOT.ALLOCATED(periodic_DKDKL)) then
        get_periode_DKDKL = 0
        RETURN
   ENDIF

   get_periode_DKDKL = periodic_DKDKL(icdtac)

 END FUNCTION get_periode_DKDKL
!!!------------------------------------------------------------------------

 function get_icdtac_DKDKL(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_DKDKL
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_DISKx
   logical :: found

   found = .false.

   nb_DISKx = get_nb_DISKx()

   icc = 0
   do icdtac = 1, nb_DISKx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_DKDKL = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKDKL::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_DKDKL(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_DKDKL
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_DISKx
   logical :: found

   found = .false.

   nb_DISKx = get_nb_DISKx()

   icc = 0
   do icdtac = 1, nb_DISKx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_DKDKL =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKDKL::get_icdtac','unknown contact index')
   

   get_iantac_DKDKL = this(icdan)%iantac

 end function

 subroutine clean_memory_DKDKL
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_DKDKL  = 0
   nb_vDKDKL = 0

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%DKwhich ) ) deallocate(box(i,j)%DKwhich)
         if( associated(box(i,j)%DKLwhich) ) deallocate(box(i,j)%DKLwhich)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_DKDKL) ) deallocate(rough_DKDKL)

   nb_rough_DKDKL = 0
   nstep_rough_seek_DKDKL = 1
   nb_recup_DKDKL = 0

   RUN = .false.

   if( allocated(DKcoor) ) deallocate(DKcoor)
   if( allocated(DKLcoor)) deallocate(DKLcoor)

   Reac_DKDKL_MAX = 0.D0

   !PERIODIC=.FALSE.
   !PERIODE = 0.d0
   nb_PERIODIC_DKDKL = 0

   if( allocated(periodic_DKDKL) ) deallocate(periodic_DKDKL)
   
   module_checked_ = .FALSE.
   check_DKDKL_    = .FALSE.

 end subroutine

 subroutine set_nb_DKDKL(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_DKDKL = nb

 end subroutine

 subroutine redo_nb_adj_DKDKL()
   implicit none

   call redo_nb_adj_( get_nb_DISKx() )

 end subroutine

END MODULE DKDKL
