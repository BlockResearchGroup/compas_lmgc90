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
MODULE DKPLx                                          

  !!****
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors DISKx and POLYG.
  !!  In this modulus candidate contactors are DISKx and antagonist 
  !!  contactors are POLYG.
  !!****

  USE overall
  USE tact_behaviour
  use POLYG, only : T_POLYG, get_nb_polyg, polyg2bdyty  , &
                    get_l_polyg, get_radius_polyg       , &
                    get_min_radius_polyg                , &
                    get_max_radius_polyg                , &
                    get_mean_radius_polyg               , &
                    move_bdary_polyg, print_info_polyg  , &
                    get_inv_mass_polyg                  , &
                    get_ent_polyg       => get_ent      , &
                    get_color_polyg     => get_color    , &
                    get_visible_polyg   => get_visible  , &
                    get_coorTT_polyg    => get_coorTT   , &
                    get_shiftTT_polyg   => get_shiftTT  , &
                    get_Vbegin_polyg    => get_Vbegin   , &
                    get_V_polyg         => get_V        , &
                    add_reac_polyg      => add_reac     , &
                    get_vlocy_polyg     => get_vlocy    , &
                    comp_vlocy_polyg    => comp_vlocy   , &
                    nullify_reac_polyg  => nullify_reac , &
                    nullify_vlocy_polyg => nullify_vlocy, &
                    get_Vd_polyg                                            

  USE DISKx, only : get_nb_diskx, diskx2bdyty           , &
                    print_info_diskx, get_radius_DISKx  , &
                    get_ent_diskx       => get_ent      , &
                    get_color_diskx     => get_color    , &
                    get_visible_diskx   => get_visible  , &
                    get_coorTT_diskx    => get_coorTT   , &
                    get_shiftTT_diskx   => get_shiftTT  , &
                    get_V_DISKx         => get_V        , &
                    get_Vbegin_DISKx    => get_Vbegin   , &
                    add_reac_diskx      => add_reac     , &
                    get_vlocy_diskx     => get_vlocy    , &
                    comp_vlocy_diskx    => comp_vlocy   , &
                    nullify_reac_diskx  => nullify_reac , &
                    nullify_vlocy_diskx => nullify_vlocy, &
                    get_max_radius_DISKx                , &
                    get_min_radius_DISKx                , &
                    get_Vd_diskx                        

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use inter_meca_2D

  use parameters, only : i_dkplx, i_diskx, i_polyg, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

 INTEGER         :: nb_DKPLx                          ! nb_DKPLx = number of selected candidate POLYG against POLYG
                                                      ! due to the fact that their might be 2 node_segment for each
                                                      ! entry in this it should be higher than size(this)
 INTEGER         :: nb_vDKPLx                         ! nb_vDKPLx = number of selected candidates DISKx against DISKx
                                                      ! <= size(this).


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdbdy): number of adjacent pairs body-contactor
                                                         ! to candidate body ibdycd.

!------------------------------------------------------------------------  


!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------ 

 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_DKJCx.
                              
   INTEGER                               :: PLpopul   ! box(ibox1,ibox2)%popul: number of polyg in box ibox1,ibox2;
   INTEGER, DIMENSION(:), POINTER        :: PLwhich   ! box(ibox1,ibox2)%which(ipopul): 

   INTEGER                               :: DKpopul   ! box(ibox1,ibox2)%popul: number of disks in box ibox1,ibox2;
   INTEGER, DIMENSION(:), POINTER        :: DKwhich   ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of disk labelled ipopul
                                                      ! in box ibox1,ibox2;
   
 END TYPE T_box 
!------------------------------------------------------------------------

 TYPE(T_box), DIMENSION(:,:),ALLOCATABLE  :: box    ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.


 real(kind=8)                                    :: Reac_DKPLx_MAX = 0.d0
 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------
TYPE T_rough_DKPLx                                                 ! définit le type de la liste des plus proches voisins
   INTEGER :: cd                                                   ! le candidat, l'antagoniste et isee pour la loi de contact
   INTEGER :: an
   INTEGER :: isee

   integer :: periodic                                             ! periodic contact flag 
END TYPE T_rough_DKPLx

TYPE(T_rough_DKPLx),DIMENSION(:),ALLOCATABLE   :: rough_DKPLx        ! table  de visibilité

!------------------------------------------------------------------------
TYPE T_link_rough_DKPLx                        ! liste chainée pour déterminer les listes de cand anta car onne connait pas le nb
                                               ! de cand -ant à priori
   TYPE(T_link_rough_DKPLx), POINTER :: p      ! pointeur sur le precedent

   TYPE(T_rough_DKPLx) :: val                  ! les valeurs
  
   TYPE(T_link_rough_DKPLx ), POINTER :: n     ! pointeur sur le suivant

END TYPE T_link_rough_DKPLx

TYPE(T_link_rough_DKPLx),POINTER                  :: Root,Current,Previous

REAL(kind=8),DIMENSION(:,:),ALLOCATABLE            :: PL_coor          ! tableau (3,nb_POLYG) contenant les coordonnées 
                                                                       ! des centres au cours du calcul
REAL(kind=8),DIMENSION(:,:),ALLOCATABLE            :: DK_coor          ! tableau (3,nb_DISKx) contenant les coordonnées
                                                                       ! des centres au cours du calcul

 INTEGER                                       :: Nstep_rough_seek_DKPLx=1
 LOGICAL                                       :: write_creation_tab_visu

!------------------------------------------------------------------------
! variables attached to surrounding boxes

 REAL (kind=8)  :: maxray, minray,maxalert,meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: nb_rough_DKPLx
 integer        :: nb_recup_DKPLx
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul
!------------------------------------------------------------------------
 LOGICAL :: RUN=.FALSE.

 logical :: module_checked_ = .FALSE.
 logical :: check_DKPLx_    = .FALSE.

!------------------------------------------------------------------------
 
 logical                                    :: PERIODIC=.false.
 real(KIND=8)                               :: PERIODE = 0.d0

!------------------------------------------------------------------------
 
! liste des fonctions publiques
!

  PUBLIC &
      stock_rloc_DKPLx, &
      recup_rloc_DKPLx, &
      compute_box_DKPLx, &
      read_ini_Vloc_Rloc_DKPLx, &
      write_xxx_Vloc_Rloc_DKPLx, &
      coor_prediction_DKPLx, &
      creation_tab_visu_DKPLx, &
      compute_contact_DKPLx, &
      display_prox_tactors_DKPLx, &
      RUN_DKPLx, &
      CHECK_DKPLx, &
      get_write_Vloc_Rloc_DKPLx, &
      set_periodic_data_DKPLx      

 PUBLIC &
      nullify_reac_DKPLx,&
      nullify_vlocy_DKPLx,&
      injj_DKPLx, prjj_DKPLx, vitrad_DKPLx, &
      get_nb_DKPLx, &
      DKPLx2POLYG, DKPLx2DISKx,&
!!$   compute_Wikik_DKPLx,compute_Wikjl_DKPLx,get_Wik_DKPLx, &
      print_info_DKPLx, &
      get_length_DKPLx, &
      get_g2l_DKPLx, get_old_index_DKPLx, &
      get_icdtac_DKPLx, &
      get_iantac_DKPLx, &
      clean_memory_DKPLx

  !rm for handler
  public get_this    , &
         set_nb_DKPLx, &
         redo_nb_adj_DKPLx, &
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
!------------------------------------------------------------------------
  SUBROUTINE coor_prediction_DKPLx

    IMPLICIT NONE
    
    INTEGER      :: itact,errare
    INTEGER      :: nb_DISKx,nb_POLYG

    nb_POLYG=get_nb_POLYG()
    nb_DISKx=get_nb_DISKx()

    DO itact=1,nb_POLYG
       PL_coor(1:3,itact) = get_coorTT_POLYG(itact)
       if (PERIODIC) then
         if (PL_coor(1,itact)  > periode) then
           !print*,'on corrige le POLYG ',itact,' qui sort par x+'
           PL_coor(1,itact) = PL_coor(1,itact) - periode
         else if (PL_coor(1,itact) < 0.D0) then
           !print*,'on corrige le POLYG ',itact,' qui sort par x-'
           PL_coor(1,itact) = PL_coor(1,itact) + periode
         end if
       end if       
       CALL move_BDARY_POLYG(itact,PL_coor(1:3,itact))
    END DO

    DO itact=1,nb_DISKx
       DK_coor(1:3,itact) = get_coorTT_DISKx(itact)
       if (PERIODIC) then
         if (DK_coor(1,itact)  > periode) then
           !print*,'on corrige le DISKx ',itact,' qui sort par x+'
           DK_coor(1,itact) = DK_coor(1,itact) - periode
         else if (DK_coor(1,itact) < 0.D0) then
           !print*,'on corrige le DISKx ',itact,' qui sort par x-'
           DK_coor(1,itact) = DK_coor(1,itact) + periode
         end if
       end if
       
    END DO

  END SUBROUTINE coor_prediction_DKPLx
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_DKPLx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_DKPLx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_DKPLx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_DKPLx
!!!------------------------------------------------------------------------
 SUBROUTINE compute_box_DKPLx

   IMPLICIT NONE

   INTEGER        :: isee,errare,ibdy
   REAL(kind=8)   :: minray_polyg,maxray_polyg,meanradius_polyg
   REAL(kind=8)   :: minray_diskx,maxray_diskx,meanradius_diskx
   INTEGER        :: nb_POLYG, nb_DISKx


   nb_POLYG=get_nb_POLYG()
   nb_DISKx=get_nb_DISKx()

   minray_polyg     = get_min_radius_POLYG()
   maxray_polyg     = get_max_radius_POLYG()
   meanradius_polyg = get_mean_radius_POLYG()

   minray_diskx     = get_min_radius_DISKx()
   maxray_diskx     = get_max_radius_DISKx()

   minray=MIN(minray_polyg,minray_diskx)
   maxray=MAX(maxray_polyg,maxray_diskx)

   meanradius=meanradius_polyg

   IF (minray > maxray ) THEN
    call faterr('mod_DKPLx::compute_box','Messing error computing minray and maxray')
   END IF

   ! computing largest alert distance between disks 
   maxalert=0.D0  
   DO isee=1,SIZE(see)
     IF (see(isee)%cdtac == 'DISKx' .AND. see(isee)%antac == 'POLYG') THEN
       maxalert=MAX(maxalert,see(isee)%alert)
     END IF
   END DO

   
   Lbox   = 1.01D0*(2.D0*maxray + maxalert)
   Lbox_1 = 1.D0/Lbox
   norm   = Lbox/minray

   IF (.NOT. ALLOCATED(adjac))THEN
     ALLOCATE(adjac(nb_DISKx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr('mod_DKPLx::compute_box','Error allocating adjac')
     END IF
     DO ibdy=1,nb_DISKx
       NULLIFY(adjac(ibdy)%icdan)
     END DO
   ELSE
     DO ibdy=1,nb_DISKx
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       NULLIFY(adjac(ibdy)%icdan)
     END DO
   ENDIF  
  
   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_DISKx),stat=errare)
   IF (errare /=0 ) THEN
      call faterr('mod_DKPLx::compute_box','Error allocating nb_adj')
   END IF
  
   nb_adj=0

   ! DKcoor are coordinates of bodies owning DISKx to be used in selecting prox tactors
   ! PLcoor are coordinates of bodies owning POLYG to be used in selecting prox tactors
   IF (ALLOCATED(DK_coor)) DEALLOCATE(DK_coor)
   ALLOCATE(DK_coor(3,nb_DISKx),stat=errare)
   IF (ALLOCATED(PL_coor)) DEALLOCATE(PL_coor)
   ALLOCATE(PL_coor(3,nb_POLYG),stat=errare)   
  
 END SUBROUTINE compute_box_DKPLx
!------------------------------------------------------------------------ 
!--------------------------------------------------------------------------------------------------
SUBROUTINE creation_tab_visu_DKPLx
 
  IMPLICIT NONE 
 
  TYPE(T_POLYG)                         :: PLibdy,PLicdbdy
  INTEGER                               :: errare 

  INTEGER                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
  INTEGER                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty,i   
  REAL(kind=8)                          :: Bleft,Bright,Bup,Bdown,Lbox
  CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol,cdbdyty,anbdyty
  REAL(kind=8),DIMENSION(3)             :: coord,coordcd,coordan 
  REAL(kind=8)                          :: raycd,rayan,adist,dist,nonuc,gap,lnorm
  LOGICAL                               :: visible

  INTEGER         :: nb_POLYG, nb_DISKx, nb_PERIODIC_DKPLx

  character(len=108) :: cout
                             !1234567890123456789012345678
  character(len=28) :: IAM = 'mod_DKPLx::creation_tab_visu'

  nb_POLYG=get_nb_POLYG()
  nb_DISKx=get_nb_DISKx()

  ! Since the list of proximate contactors may not be updated at every time step,
  ! boxes data would be lost if deallocated. When starting the program, boxes are not created.
  ! A warning condition prevents undue deallocation.

  IF (ALLOCATED(box)) THEN
     DO ibox1=minibox1,maxibox1
        DO ibox2=minibox2,maxibox2
           IF (ASSOCIATED(box(ibox1,ibox2)%PLwhich)) DEALLOCATE(box(ibox1,ibox2)%PLwhich)
           IF (ASSOCIATED(box(ibox1,ibox2)%DKwhich)) DEALLOCATE(box(ibox1,ibox2)%DKwhich)
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

  DO ibdy=1,nb_POLYG
     IF (.NOT.get_visible_POLYG(ibdy)) CYCLE
     coord = PL_coor(:,ibdy)
     Bleft = MIN(coord(1),Bleft )
     Bright= MAX(coord(1),Bright)
     Bup   = MAX(coord(2),Bup   )
     Bdown = MIN(coord(2),Bdown )
  END DO

   DO ibdy=1,nb_DISKx
     IF (.NOT.get_visible_DISKx(ibdy)) CYCLE
     coord = DK_coor(1:3,ibdy)
     Bleft = MIN(coord(1),Bleft )
     Bright= MAX(coord(1),Bright)
     Bup   = MAX(coord(2),Bup   )
     Bdown = MIN(coord(2),Bdown )
   END DO



   if ( PERIODIC ) then
      if (Bright > periode) then
          print*,'Bright ',Bright,' Periode ',periode
          call FATERR(IAM,'the max right coordinate is greater than the periode')
      endif
       
      if (Bleft < 0.d0) then
          print*,'Bleft ',Bleft
          call FATERR(IAM,'the min left coordinate is less than zero')
      endif
       
      Bright = periode
      Bleft  = 0.d0    
    end if

   
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

  minibox1 = 1
  maxibox1 = 1 + INT((Bright-Bleft)*Lbox_1)
  minibox2 = 1
  maxibox2 = 1 + INT((Bup - Bdown )*Lbox_1)
  !
  maxpopul = (1+INT(norm))*(1+INT(norm))
  !
  ! for each box maxpopul is less than the total number of DISKx 
  !

  !print *,Lbox_1
  !print *,Bleft,Bright,maxibox1
  !print *,Bdown,Bup,maxibox2
  !print *,norm,maxpopul

  maxpopul=MIN(maxpopul,nb_POLYG+nb_DISKx)

  !   
  !print *,maxpopul

  !
  ALLOCATE(box(minibox1:maxibox1,minibox2:maxibox2),stat=errare)

  IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating box')
  END IF
  DO ibox1=minibox1,maxibox1
  DO ibox2=minibox2,maxibox2
     box(ibox1,ibox2)%PLpopul=0
     box(ibox1,ibox2)%DKpopul=0

     ALLOCATE(box(ibox1,ibox2)%PLwhich(maxpopul),stat=errare)
     IF (errare /=0 ) THEN
        write(cout,'(A,I0,A,I0,A)') 'Error in allocating box(',ibox1,',',ibox2,')%which'
        call faterr(IAM,cout)
     END IF

     ALLOCATE(box(ibox1,ibox2)%DKwhich(maxpopul),stat=errare)
     IF (errare /=0 ) THEN
       write(cout,'(A,I0,A,I0,A)') 'Error in allocating box(',ibox1,',',ibox2,')%DKwhich'
       call faterr(IAM,cout)
     END IF   
  END DO
  END DO
  ! filling boxes with POLYG
  ! box(ibox1,ibox2)%popul is the number of disks into the box (ibox1,ibox2)
  ! box(ibox1,ibox2)%which(ipopul) is the rank of body DISKx labelled ipopul in the box

  ! filling boxes   
  DO ibdy=1,nb_POLYG
     IF (.NOT.get_visible_POLYG(ibdy)) CYCLE
     coord=PL_coor(:,ibdy)
     ibox1=1+INT((coord(1)-Bleft )*Lbox_1)
     ibox2=1+INT((coord(2)-Bdown )*Lbox_1)
     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2) THEN
        WRITE(cout,'(A,I0,A,I0)')' maxibox1=',maxibox1,'maxibox2=',maxibox2
        WRITE(cout,'(A,I0,A,I0)')'    ibox1=',ibox1,   '   ibox2=',ibox2
        WRITE(cout,'(A,I0,A,I0)')' minibox1=',minibox1,'minibox2=',minibox2
        WRITE(cout,'(A13,I5,A13)')'  body POLYG ',ibdy,' out of boxes'
        call faterr(IAM,cout)
     END IF
     box(ibox1,ibox2)%PLpopul=box(ibox1,ibox2)%PLpopul+1
     if( box(ibox1,ibox2)%PLpopul > size(box(ibox1,ibox2)%PLwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for PL.")
     end if
     box(ibox1,ibox2)%PLwhich(box(ibox1,ibox2)%PLpopul)=ibdy
  END DO

   DO ibdy=1,nb_DISKx
     IF (.NOT.get_visible_DISKx(ibdy)) CYCLE
     coord=DK_coor(1:3,ibdy)
     ibox1=1+INT((coord(1)-Bleft )*Lbox_1)
     ibox2=1+INT((coord(2)-Bdown )*Lbox_1)
     IF (ibox1 < minibox1 .OR. ibox1 > maxibox1 .OR. ibox2 < minibox2 .OR. ibox2 > maxibox2) THEN
       WRITE(cout,'(A,I0,A,I0)')' maxibox1=',maxibox1,'maxibox2=',maxibox2
       WRITE(cout,'(A,I0,A,I0)')'    ibox1=',ibox1,   '   ibox2=',ibox2
       WRITE(cout,'(A,I0,A,I0)')' minibox1=',minibox1,'minibox2=',minibox2
       WRITE(cout,'(A13,I5,A13)')'  body DISKx ',ibdy,' out of boxes'
       call faterr(IAM,cout)
     END IF
     box(ibox1,ibox2)%DKpopul=box(ibox1,ibox2)%DKpopul+1
     if( box(ibox1,ibox2)%DKpopul > size(box(ibox1,ibox2)%DKwhich) ) then
         call faterr(IAM, "Estimated max popul limit reached for DK.")
     end if
     box(ibox1,ibox2)%DKwhich(box(ibox1,ibox2)%DKpopul)=ibdy
   END DO  
  
  ! Detecting contacts; 
  ! contacts are being detected within a box and immediate surrounding boxes;  
  
  ! first reading: sizing array this
  
   nb_rough_DKPLx=0
  
  ! création de la liste de paire à examiner
  
  ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
  ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)

  DO ibox1cd=minibox1,maxibox1  
  DO ibox2cd=minibox2,maxibox2
    DO icdpop=1,box(ibox1cd,ibox2cd)%DKpopul
      icdtac=box(ibox1cd,ibox2cd)%DKwhich(icdpop)   
      cdcol=get_color_DISKx(icdtac)
      ! box loop investigating antagonist polyg
      DO ibox1an=MAX(minibox1,ibox1cd-1),MIN(maxibox1,ibox1cd+1)                        
      DO ibox2an=MAX(minibox2,ibox2cd-1),MIN(maxibox2,ibox2cd+1)                   
        DO ianpop=1,box(ibox1an,ibox2an)%PLpopul
          iantac=box(ibox1an,ibox2an)%PLwhich(ianpop)
          IF ( (polyg2bdyty(1,iantac) == diskx2bdyty(1,icdtac)) .AND. (polyg2bdyty(3,iantac) == diskx2bdyty(3,icdtac)) ) CYCLE
          ancol   = get_color_POLYG(iantac)
          anbdyty = get_body_model_name_from_id(polyg2bdyty(3,iantac))
          cdbdyty = get_body_model_name_from_id(diskx2bdyty(3,icdtac))
          isee=get_isee(cdbdyty,'DISKx',cdcol,anbdyty,'POLYG',ancol)
          ! if contactors are seeing each other
          IF (isee /= 0) THEN
            adist=see(isee)%alert 
            ! checking ROUGHLY distance against alert distance           
            coordcd = DK_coor(:,icdtac)
            coordan = PL_coor(:,iantac)
            raycd = get_radius_DISKx(icdtac)
            rayan = get_radius_POLYG(iantac)
            dist=raycd+rayan+adist
            lnorm= (coordan(1)-coordcd(1))*(coordan(1)-coordcd(1)) &
                  +(coordan(2)-coordcd(2))*(coordan(2)-coordcd(2))
            IF (lnorm<dist*dist) THEN
                nb_rough_DKPLx=nb_rough_DKPLx+1
                IF ( nb_rough_DKPLx == 1) THEN
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
      END DO
    END DO
  END DO
  END DO

  ! print*,PERIODIC,minibox1,maxibox1,minibox2,maxibox2
  
  if ( PERIODIC ) then

      nb_PERIODIC_DKPLx = 0
     
      ! perio +
      ! PL dans 2 premieres colonnes
      ! DK dans 2 dernieres colonnes
      do ibox1cd = maxibox1-1,maxibox1
      do ibox2cd = minibox2,maxibox2

         do icdpop = 1,box(ibox1cd,ibox2cd)%DKpopul
            
            icdtac = box(ibox1cd,ibox2cd)%DKwhich(icdpop)
            cdcol  = get_color_DISKx(icdtac)

            ! print*,'cd',icdtac,cdcol

            ! print*,'x',minibox1,minibox1+1
            ! print*,'y',max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)
            
            do ibox1an = minibox1,minibox1+1
            do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)
                  
               ! print*,ibox1an,ibox2an,box(ibox1an,ibox2an)%PLpopul
               
               do ianpop = 1,box(ibox1an,ibox2an)%PLpopul
                  
                  iantac = box(ibox1an,ibox2an)%PLwhich(ianpop)
                  ancol = get_color_POLYG(iantac)

                  ! print*,'an',iantac,ancol

                  ! print*,diskx2bdyty(1:3,icdtac)
                  ! print*,polyg2bdyty(1:3,iantac)
                  
                  ! skip autocontact
                  IF ( (polyg2bdyty(1,iantac) == diskx2bdyty(1,icdtac)) .AND. (polyg2bdyty(3,iantac) == diskx2bdyty(3,icdtac)) ) CYCLE
                  

                  isee = get_isee(get_body_model_name_from_id(diskx2bdyty(3,icdtac)),'DISKx',cdcol, &
                                  get_body_model_name_from_id(polyg2bdyty(3,iantac)),'POLYG',ancol)

                  ! print*,isee
                  
                  if (isee /= 0 ) then
                     adist   = see(isee)%alert 
                     ! checking ROUGHLY distance against alert distance           
                     coordcd = DK_coor(1:3,icdtac)
                     coordan = PL_coor(1:3,iantac)
                     coordan(1) = coordan(1) + periode
                     
                     raycd   = get_radius_DISKx(icdtac)
                     rayan   = get_radius_POLYG(iantac)
                     
                     dist=raycd+rayan+adist
                     lnorm= (coordan(1)-coordcd(1))*(coordan(1)-coordcd(1)) &
                          +(coordan(2)-coordcd(2))*(coordan(2)-coordcd(2))


                     ! print*,diskx2bdyty(1,icdtac),polyg2bdyty(1,iantac),raycd,rayan,lnorm,dist*dist
                     
                     IF (lnorm<dist*dist) THEN
                        
                        nb_rough_DKPLx    = nb_rough_DKPLx+1
                        nb_PERIODIC_DKPLx = nb_PERIODIC_DKPLx+1                        
                        
                        if ( nb_rough_DKPLx == 1) then
                           allocate(Root)
                           Current => Root
                           nullify(Root%p)
                        else
                           allocate(Current)
                           Previous%n => Current
                        end if
                        
                        Current%val%cd       = icdtac
                        Current%val%an       = iantac
                        Current%val%periodic = 1                  

                        Current%val%isee     = isee
                        Current%p => Previous
                        nullify(Current%n)
                        Previous => Current
                     end if
                  end if
               end do
            end do
            end do
         end do
      end do
      end do
   
      ! perio -
      ! PL dans 2 dernieres colonnes
      ! DK dans 2 premieres colonnes
      do ibox1cd = maxibox1-1,maxibox1
      do ibox2cd = minibox2,maxibox2

         do icdpop = 1,box(ibox1cd,ibox2cd)%PLpopul
            
            icdtac = box(ibox1cd,ibox2cd)%PLwhich(icdpop)
            cdcol  = get_color_POLYG(icdtac)

            do ibox1an = minibox1,minibox1+1
            do ibox2an = max(minibox2,ibox2cd-1),min(maxibox2,ibox2cd+1)                   

               do ianpop = 1,box(ibox1an,ibox2an)%DKpopul
                  
                  iantac = box(ibox1an,ibox2an)%DKwhich(ianpop)

                  ! skip autocontact
                  IF ((polyg2bdyty(1,icdtac) == diskx2bdyty(1,iantac)) .AND. (polyg2bdyty(3,icdtac) == diskx2bdyty(3,iantac)) ) CYCLE

                  ancol = get_color_DISKx(iantac)
                  isee = get_isee(get_body_model_name_from_id(polyg2bdyty(3,icdtac)),'POLYG',cdcol, &
                                  get_body_model_name_from_id(diskx2bdyty(3,iantac)),'DISKx',ancol)
                  
                  if (isee /= 0 ) then
                     adist   = see(isee)%alert 
                     ! checking ROUGHLY distance against alert distance           
                     coordcd = PL_coor(1:3,icdtac)
                     coordan = DK_coor(1:3,iantac)
                     coordan(1) = coordan(1) + periode
                     
                     raycd   = get_radius_POLYG(icdtac)
                     rayan   = get_radius_DISKx(iantac)
                     
                     dist=raycd+rayan+adist
                     lnorm= (coordan(1)-coordcd(1))*(coordan(1)-coordcd(1)) &
                           +(coordan(2)-coordcd(2))*(coordan(2)-coordcd(2))
                     
                     IF (lnorm<dist*dist) THEN
                        
                        nb_rough_DKPLx    = nb_rough_DKPLx+1
                        nb_PERIODIC_DKPLx = nb_PERIODIC_DKPLx+1
                        
                        if ( nb_rough_DKPLx == 1) then
                           allocate(Root)
                           Current => Root
                           nullify(Root%p)
                        else
                           allocate(Current)
                           Previous%n => Current
                        end if
                        Current%val%cd       = iantac
                        Current%val%an       = icdtac
                        Current%val%periodic = -1                  

                        Current%val%isee     = isee
                        Current%p => Previous
                        nullify(Current%n)
                        Previous => Current
                     end if
                  end if
               end do
            end do
            end do
         end do
      end do
      end do
   
      WRITE(cout,'(4X,I0,A)') nb_PERIODIC_DKPLx,' periodic DKPLx roughly found'
      call logmes(cout)

  endif
  
  WRITE(cout,'(4X,I10,A20)') nb_rough_DKPLx,' DKPLx roughly found'
  call logmes(cout)

  ! on s'alloue la table de visibilité utilisée dans compute_contact
  IF (ALLOCATED(rough_DKPLx)) DEALLOCATE(rough_DKPLx)
  ALLOCATE(rough_DKPLx(nb_rough_DKPLx))              
  
  IF (ALLOCATED(this)) DEALLOCATE(this)
  ALLOCATE(this(nb_rough_DKPLx))     
                                            
  DO i=nb_rough_DKPLx,1,-1
     Previous => Current%p
     rough_DKPLx(i)%cd       = Current%val%cd
     rough_DKPLx(i)%an       = Current%val%an
     rough_DKPLx(i)%isee     = Current%val%isee
     rough_DKPLx(i)%periodic = Current%val%periodic     
     DEALLOCATE(Current)
     Current => Previous
  END DO 
   
  NULLIFY(Root)

END SUBROUTINE creation_tab_visu_DKPLx
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
SUBROUTINE compute_contact_DKPLx
 
   IMPLICIT NONE  

   INTEGER                               :: errare 

   TYPE(T_POLYG)                         :: PLiantac

   INTEGER                               :: ibox1,ibox2,ibox1cd,ibox2cd,ibox1an,ibox2an,icdpop,ianpop
   INTEGER                               :: icdan,iadj,ibdy,icdbdy,ianbdy,itac, &
                                            icdtac,iantac,isee,itacty,ipr
   CHARACTER(len=5)                      :: cdtac,cdcol,antac,ancol
   REAL(kind=8),DIMENSION(3)             :: coord,coordcd,coordan,cd_Vbegin,an_Vbegin
   REAL(kind=8)                          :: ax1,ax2,yan,adist,dist,nonuc,gap,ut1,ut2,un1,un2,Gant3,Gann3,raycd,rayan
   INTEGER                               :: i,id,j,nb_ctc
   REAL(kind=8)                          :: norme,norm1,gapTT,ovlap        ! scalaire contenant la norme de sep
   REAL(kind=8),DIMENSION(2)             :: xco,sep
   REAL(kind=8),DIMENSION(2)             :: cdlev,anlev,t,N,pt   
   INTEGER,DIMENSION(2)                  :: vertex_candidat
   INTEGER                               :: cd_ent,an_ent
   REAL(kind=8),DIMENSION(2)             :: cd_shift,an_shift ! local barycenter shift vector 
   INTEGER                               :: i1,i2, an_na, an_nb, ianseg
   REAL(kind=8)                          :: norme1,norme2,xi
   INTEGER(kind=4) :: nb_DISKx

   character(len=80) :: cout

   logical :: debug
   
   icdan=0        
   nb_DKPLx=0
   nb_adj=0

   IF (nb_rough_DKPLx /= 0 ) THEN
!
! preparation de la detection 
!
    icdtac=1 ! pour l'instant, c'est ok...
    iantac=1

    DO i=1,nb_rough_DKPLx

      icdtac   = rough_DKPLx(i)%cd
      iantac   = rough_DKPLx(i)%an
      ipr      = rough_DKPLx(i)%periodic
      PLiantac = get_l_POLYG(iantac)
      raycd    = get_radius_DISKx(icdtac)
      rayan    = get_radius_POLYG(iantac)
      isee     = rough_DKPLx(i)%isee
      adist    = see(isee)%alert
      coordcd  = DK_coor(:,icdtac)
      coordan  = PL_coor(:,iantac)
      coordan(1) = coordan(1) + (real(ipr,8)*periode)      

      if (ipr .ne. 0) call move_BDARY_POLYG(iantac,coordan)   
      
      ! Overestimated distance between antagonist and candidate
      norm1=adist+raycd+rayan

      dist=(coordcd(1)-coordan(1))*(coordcd(1)-coordan(1)) &
          +(coordcd(2)-coordan(2))*(coordcd(2)-coordan(2))

      IF (dist>norm1*norm1) CYCLE

      sep=coordcd(1:2)-coordan(1:2)
      norme=SQRT(dist)
      sep=sep/norme
      
      debug = .FALSE.
      ! if (diskx2bdyty(1,icdtac) == 33 .and. polyg2bdyty(1,iantac) == 22) debug=.TRUE.
      
      CALL detect(PLiantac,raycd,coordcd,xco,sep,N,ovlap,adist,nb_ctc,ianseg,debug)

      if (ipr .ne. 0) call move_BDARY_POLYG(iantac,PL_coor(1:3,iantac))
      
      IF (nb_ctc==1) THEN

        icdtac   = rough_DKPLx(i)%cd
        iantac   = rough_DKPLx(i)%an
        icdan=icdan+1
        cd_Vbegin = get_Vbegin_DISKx(icdtac)
        an_Vbegin = get_Vbegin_POLYG(iantac)
        nb_adj(icdtac) = nb_adj(icdtac)+1
        iadj=nb_adj(icdtac)

        this(icdan)%iadj     =  iadj
        this(icdan)%icdbdy   =  diskx2bdyty(1,icdtac)
        this(icdan)%icdtac   =  icdtac
        this(icdan)%icdsci   = 0
        this(icdan)%ianbdy   =  polyg2bdyty(1,iantac)
        this(icdan)%iantac   =  iantac
        this(icdan)%iansci   = 0
        this(icdan)%isee     =  isee
        this(icdan)%nuc(1:2) =  N

        t(1) = n(2); t(2) = -n(1)
        this(icdan)%tuc(1:2) =  t
     
        this(icdan)%icdbtac = diskx2bdyty(2, icdtac)
        this(icdan)%ianbtac = polyg2bdyty(2, iantac)

        this(icdan)%icdbtyp = diskx2bdyty(3, icdtac)
        this(icdan)%ianbtyp = polyg2bdyty(3, iantac)

        this(icdan)%icdctyp = i_diskx
        this(icdan)%ianctyp = i_polyg

        cd_ent = get_ent_DISKx(this(icdan)%icdtac)
        an_ent = get_ent_POLYG(this(icdan)%iantac)

        this(icdan)%icdent = cd_ent
        this(icdan)%ianent = an_ent

        if (cd_ent /= an_ent) then
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
          entity(an_ent)%nb = entity(an_ent)%nb+1
        else
          entity(cd_ent)%nb = entity(cd_ent)%nb+1
        end if

        cd_shift = get_shiftTT_DISKx(icdtac)
        an_shift = get_shiftTT_POLYG(iantac)

        this(icdan)%coor(1:2) = xco(1:2)

        cdlev= xco(1:2) + cd_shift(1:2) - DK_coor(1:2,icdtac)
        anlev= xco(1:2) + an_shift(1:2) - PL_coor(1:2,iantac)
        anlev(1) = anlev(1) - (real(ipr,8)*periode)

        this(icdan)%Gcdt3     = -cdlev(2)*t(1)+cdlev(1)*t(2)
        this(icdan)%Gcdn3     = -cdlev(2)*n(1)+cdlev(1)*n(2)
        this(icdan)%Gant3     = -anlev(2)*t(1)+anlev(1)*t(2)
        this(icdan)%Gann3     = -anlev(2)*n(1)+anlev(1)*n(2)

        this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdt3  &
                               - an_Vbegin(3)*this(icdan)%Gant3

        this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                               +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                               + cd_Vbegin(3)*this(icdan)%Gcdn3  &
                               - an_Vbegin(3)*this(icdan)%Gann3
  

        this(icdan)%gapTTBEGIN  = -ovlap

        this(icdan)%rlt       = 0.d0
        this(icdan)%rln       = 0.d0
        this(icdan)%vlt       = this(icdan)%vltBEGIN
        this(icdan)%vln       = this(icdan)%vlnBEGIN
        this(icdan)%gapTT     = this(icdan)%gapTTBEGIN
        this(icdan)%status    = i_nknow
        this(icdan)%iansci    = ianseg


        !rm copy/paste from PLPL detection
        an_na = ianseg
        an_nb = ianseg+1
        if (an_nb > PLiantac%nb_vertex) an_nb=1          

        xi =( ( this(icdan)%coor(1)-PLiantac%vertex(1, an_na) ) * t(1) +   &
              ( this(icdan)%coor(2)-PLiantac%vertex(2, an_na) ) * t(2) ) / &
            ( ( PLiantac%vertex(1, an_nb)-PLiantac%vertex(1, an_na) ) * t(1) + &
              ( PLiantac%vertex(2, an_nb)-PLiantac%vertex(2, an_na) ) * t(2)   )
        
        this(icdan)%iancoor(1) = (1.d0-xi) * PLiantac%vertex_ref(1, an_na) + &
                                       xi  * PLiantac%vertex_ref(1, an_nb)
        this(icdan)%iancoor(2) = (1.d0-xi) * PLiantac%vertex_ref(2, an_na) + &
                                       xi  * PLiantac%vertex_ref(2, an_nb)
      ENDIF
    ENDDO

    nb_DKPLx=icdan

  ENDIF

  WRITE(cout,'(1X,I10,A12)') nb_DKPLx,' DKPLx found'
  call logmes(cout)

  nb_DISKx=get_nb_DISKx()

  DO ibdy=1,nb_DISKx
    IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
    IF (nb_adj(ibdy) /= 0) THEN
      ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      IF (errare /=0 ) THEN
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_DKPLx::compute_contact_DKPLx',cout)
      END IF
    ENDIF
  ENDDO

  DO icdan=1,nb_DKPLx     
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
  END DO

  
   do icdan = 1, nb_DKPLx
      call get_behaviour_( icdan, see, tact_behav )
   end do

  IF (ALLOCATED(violation)) DEALLOCATE(violation)
  ALLOCATE(violation(nb_DKPLx),stat=errare)
   
END SUBROUTINE compute_contact_DKPLx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 SUBROUTINE display_prox_tactors_DKPLx

   IMPLICIT NONE
   INTEGER :: iadj,icdan,icdbdy,jbdycd,icdtac,ianbdy,iantac,isee,icdver,itact
   integer :: nb_diskx
   character(len=5) :: cdmodel, anmodel

   IF (nb_DKPLx == 0) RETURN
   icdver=0

   nb_DISKx = get_nb_DISKx()
   DO itact=1,nb_DISKx
     DO iadj=1,nb_adj(itact)
       icdan  = adjac(itact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyg2bdyty(3,iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan
                        !123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
       WRITE(*,'(A103)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  vertx  numbr      sttus iadj '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,16X,A5,1X,I5)')   &
       cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac), &
       see(this(icdan)%isee)%behav,  &
       anmodel,polyg2bdyty(1,iantac),'POLYG',polyg2bdyty(2,iantac),'CDVER',icdver, &
       get_contact_status_name_from_id(this(icdan)%status),iadj
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       WRITE(*,104)'t(3)=',0.D0              ,'n(3)=',0.D0              ,'s(3)=',0.D0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBEGIN
       WRITE(*,'(A1)')' '               
     END DO
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 END SUBROUTINE display_prox_tactors_DKPLx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_DKPLx
 
   !  
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE

   INTEGER                               :: errare 
   INTEGER :: icdan,icdbdy,icdtac,icdver,ianbdy,iantac,iadj
   integer(kind=4) :: nb_diskx

   character(len=80) :: cout
                              !123456789012345678901
   character(len=20) :: IAM = 'mod_DKPLx::stock_rloc'

   nb_DISKx = get_nb_DISKx()

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

         IF (errare /=0 ) THEN
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         END IF
       ELSE 
         call nullify_verlet_(icdtac)
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
         IF (errare /=0 ) THEN
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         END IF
       ELSE 
         call nullify_verlet_(icdtac)
       END IF
     END DO
   END IF

  ! filling data:
   DO icdan=1,nb_DKPLx

     icdtac = this(icdan)%icdtac  ! serial number of candidate contactor for contact icdan
     icdver = this(icdan)%icdsci
     iantac = this(icdan)%iantac  ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj    ! serial adjacent number of pair body-contactor 
                                  ! adjacent to candidate body for contact icdan 
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdmodel         = diskx2bdyty(3,icdtac)
     verlt(icdtac)%cdbdy           = diskx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = diskx2bdyty(2,icdtac)
     verlt(icdtac)%cdsci(iadj)     = this(icdan)%icdsci
     verlt(icdtac)%anmodel(iadj)   = polyg2bdyty(3,iantac)
     verlt(icdtac)%anbdy(iadj)     = polyg2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = polyg2bdyty(2,iantac)
     verlt(icdtac)%ansci(iadj)     = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)       = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)       = this(icdan)%rln/H
     verlt(icdtac)%status(iadj)    = this(icdan)%status
     verlt(icdtac)%vlt(iadj)       = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)       = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)     = this(icdan)%gapTT
     verlt(icdtac)%nuc(1:2,iadj)   = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj)  = this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   END DO

   nb_vDKPLx = nb_DKPLx

   WRITE(cout,'(1X,I10,A12)') nb_vDKPLx,' stock DKPLx'
   call logmes(cout)

 END SUBROUTINE stock_rloc_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_DKPLx

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE
   INTEGER :: icdan,icdtac,icdver,iantac,iadj
   CHARACTER(len=21)  :: IAM = 'mod_DKPLx::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   IF (nb_DKPLx == 0) RETURN  

   nb_recup_DKPLx=0

   DO icdan=1,nb_DKPLx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac          ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac          ! serial number of antagonist contactor for contact icdan        
     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac)       &
          ) then
          do iadj = 1, verlt(icdtac)%adjsz
            IF (verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,iantac)       &
               ) then
              this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
              this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
              this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
              nb_recup_DKPLx=nb_recup_DKPLx+1
              exit
            end if
          end do
       end if
     ENDIF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_DKPLx,' recup DKPLx'
   call logmes(cout)

 END SUBROUTINE recup_rloc_DKPLx
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE

   INTEGER                           :: icdan,icdbdy,icdtac,icdver,ianbdy,iantac,iadj
   INTEGER                           :: icdtact,cdmodel,anmodel
   REAL(kind=8)                      :: rlt,rln,vlt,vln,gapTT
   REAL(kind=8),DIMENSION(2)         :: nuc,coor
   CHARACTER(len=5)                  :: cdbdy,cdtac,cdver,anbdy,antac,behav,sttus
   INTEGER                           :: errare 
  
   INTEGER :: ibehav,nb_internal,i_internal
   integer(kind=4) :: nb_diskx

   character(len=150) :: cout
   !                            12345678901234567890123456789
   character(len=29)  :: IAM = 'mod_DKPLx::read_ini_Vloc_Rloc'


   nb_DISKx=get_nb_DISKx()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contacts have to be selected.
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_DISKx),stat=errare)
     IF (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   END IF

   nb_adj=0

   DO
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKPLx') CYCLE
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,16X,A5)') &
                          cdbdy,icdbdy,cdtac,icdtac,               &
                          behav,                                   &
                          anbdy,ianbdy,antac,iantac,cdver,icdver,  &
                          sttus
     IF (cdtac /= 'DISKx' .OR. antac /= 'POLYG') CYCLE
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
     DO icdbdy=1,nb_DISKx
       verlt(icdbdy)%adjsz=0
       iadj=nb_adj(icdbdy)
       IF (iadj > 0) THEN
         verlt(icdbdy)%adjsz=iadj
         call new_verlet_(icdbdy, iadj, errare)
         IF (errare /=0 ) THEN
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdbdy,')%.....'
           call faterr(IAM,cout)
         END IF
       ELSE
         call nullify_verlet_(icdbdy)
       ENDIF
     END DO
   ELSE 
     DO icdbdy=1,nb_DISKx
       call free_verlet_(icdbdy)
       verlt(icdbdy)%adjsz=0
       iadj=nb_adj(icdbdy)

       IF (iadj > 0) THEN
         verlt(icdbdy)%adjsz=iadj
         call new_verlet_(icdbdy, iadj, errare)
       ELSE
         call nullify_verlet_(icdbdy)
       END IF
     END DO
   END IF

  ! second reading: filling data
   REWIND(G_nfich)
   nb_adj=0
   icdan = 0

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKPLx') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT

     READ(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,16X,A5)') &
                          cdbdy,icdbdy,cdtac,icdtac,               &
                          behav,                                   &
                          anbdy,ianbdy,antac,iantac,cdver,icdver,  &
                          sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )

     IF (cdtac /= 'DISKx' .AND. antac /= 'POLYG') CYCLE
     do icdtact = 1, nb_DISKx
       if (diskx2bdyty(1,icdtact) == icdbdy .and. &
           diskx2bdyty(2,icdtact) == icdtac .and. &
           diskx2bdyty(3,icdtact) == cdmodel ) then

         icdan = icdan + 1

         nb_adj(icdtact) = nb_adj(icdtact) + 1

         verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
         verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)

         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,2(7X,D14.7))')rlt,rln
         verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
         verlt(icdtact)%rln(nb_adj(icdtact))=rln
         IF( .NOT. read_G_clin()) EXIT 
         READ(G_clin(1:90),'(27X,2(7X,D14.7))')vlt,vln
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         IF( .NOT. read_G_clin()) EXIT 
         READ(G_clin(1:90),'(27X,2(7X,D14.7))')gapTT
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
         EXIT 
         verlt(icdtact)%internal(1:max_internal_tact,nb_adj(icdtact))=0.d0
         ibehav = get_ibehav(behav)
         nb_internal = get_nb_internal(ibehav)
         IF (nb_internal /= 0 ) THEN  
           IF( .NOT. read_G_clin()) EXIT
           DO i_internal=1, nb_internal
             READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
           ENDDO
         ENDIF



       END IF
     ENDDO
     CYCLE
   END DO

   nb_vDKPLx=0

   DO icdtact=1,nb_DISKx
     nb_vDKPLx = nb_vDKPLx + nb_adj(icdtact)

     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       write(cout,'(A,1x,I0)')   'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')   'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     ENDIF

   END DO

 END SUBROUTINE read_ini_Vloc_Rloc
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 SUBROUTINE write_out_Vloc_Rloc(nfich)

  !
  ! write into file out_Vloc_Rloc data from this, in verlt style
  !

   IMPLICIT NONE
   INTEGER :: iadj,icdan,icdbdy,icdtac,icdver,ianbdy,iantac,isee,nfich,icdtact
   INTEGER :: lc
   REAL(kind=8),DIMENSION(2) :: coor
   integer :: nb_diskx
   character(len=5) :: cdmodel, anmodel

   character(len=20) :: fmt
   
   IF (nb_DKPLx==0) RETURN

   nb_DISKx=get_nb_DISKx()
   icdver=0

   DO icdtact=1,nb_DISKx
     DO iadj=1,nb_adj(icdtact)
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       icdver=0

       cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( polyg2bdyty(3,iantac) )

       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKPLx',icdan
       !1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
       ! RBDY2  12345  POLYG  12345  CDVER  12345  BEHAV  RBDY2  12345  DISKx  12345                STTUS 12345
       WRITE(nfich,'(A103)') &
       ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  vertx  numbr                sttus iadj '
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,16X,A5,1X,I5)')   &
       cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac), &
       see(this(icdan)%isee)%behav,  &
       anmodel,polyg2bdyty(1,iantac),'POLYG',polyg2bdyty(2,iantac),'CDVER',icdver, &
       get_contact_status_name_from_id(this(icdan)%status),iadj

       WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',0.D0
       WRITE(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',0.D0
       WRITE(nfich,103)'gapTT',this(icdan)%gapTT 
       WRITE(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',0.D0
       WRITE(nfich,104)'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',0.D0

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
 SUBROUTINE nullify_reac_DKPLx(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: iantac,icdtac
   INTEGER            :: storage

   iantac=this(icdan)%iantac
   CALL nullify_reac_POLYG(iantac,storage)

   icdtac=this(icdan)%icdtac
   CALL nullify_reac_DISKx(icdtac,storage)

 END SUBROUTINE nullify_reac_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_DKPLx(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER            :: iantac,icdtac
   INTEGER            :: storage

   iantac=this(icdan)%iantac
   CALL nullify_vlocy_POLYG(iantac,storage)

   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_DISKx(icdtac,storage)

 END SUBROUTINE nullify_vlocy_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_DKPLx( icdan, storage, need_full_V )

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER            :: iantac,icdtac
   INTEGER            :: storage
   logical, optional  :: need_full_V

   iantac=this(icdan)%iantac
   CALL comp_vlocy_POLYG(iantac,storage)

   icdtac=this(icdan)%icdtac
   CALL comp_vlocy_DISKx(icdtac,storage)

 END SUBROUTINE vitrad_DKPLx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 SUBROUTINE injj_DKPLx(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RTIK,RNIK
   INTEGER,     DIMENSION(3)  :: cdccdof,anccdof
   REAL(kind=8),DIMENSION(3)  :: cdreac, anreac
   INTEGER                    :: storage

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

   CALL add_reac_DISKx(this(icdan)%icdtac,cdccdof,cdreac,storage)
   CALL add_reac_POLYG(this(icdan)%iantac,anccdof,anreac,storage)

 END SUBROUTINE injj_DKPLx 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_DKPLx(icdan,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VTIK,VNIK
   REAL(kind=8),DIMENSION(3) :: Vcd,Van
   integer(kind=4)           :: ianbdy
   integer(kind=4),intent(in):: storage
   real(kind=8)              :: Vdcd,Vdan(2)   

   ianbdy=this(icdan)%ianbdy
   
   if (storage == iVfree) then
     ! fd dila
     call get_Vd_DISKx(this(icdan)%icdtac,Vdcd)
     ! todo passer le num du contacteur 
     call get_Vd_POLYG(ianbdy,this(icdan)%iancoor,Vdan)
   else 
     Vdcd=0.d0
     Vdan=0.d0      
   endif
   
   CALL get_vlocy_DISKx(this(icdan)%icdtac,storage,Vcd)
   CALL get_vlocy_POLYG(this(icdan)%iantac,storage,Van)

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3 &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3 &
        -Vdan(1)*this(icdan)%tuc(1)-Vdan(2)*this(icdan)%tuc(2)
   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3 &
        -Vdcd-Vdan(1)*this(icdan)%nuc(1)+Vdan(2)*this(icdan)%nuc(2)

 END SUBROUTINE prjj_DKPLx
!!$!------------------------------------------------------------------------
!!$ SUBROUTINE compute_Wikik_DKPLx(icdan,WTT,WTN,WNT,WNN)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,icdbdy,ianbdy
!!$  REAL(kind=8)              :: WTT,WTN,WNT,WNN
!!$  REAL(kind=8),DIMENSION(3) :: icdmass,ianmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$
!!$  icdmass = get_inv_mass_DISKx(icdbdy)
!!$  ianmass = get_inv_mass_POLYG(ianbdy)
!!$
!!$  WTT =  icdmass(1)+icdmass(3)*this(icdan)%Gcdt3*this(icdan)%Gcdt3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gant3*this(icdan)%Gant3
!!$  WNN =  icdmass(1)+icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdn3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gann3*this(icdan)%Gann3
!!$  WTN =  icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdt3 &
!!$       + ianmass(3)*this(icdan)%Gann3*this(icdan)%Gant3
!!$  WNT = WTN
!!$
!!$ END SUBROUTINE compute_Wikik_DKPLx
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE get_Wik_DKPLx(icdan,ikcd,tik,nik,ikcdmass,ikGcdt,ikGcdn)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,ikcd
!!$  REAL(kind=8)              :: ikGcdt,ikGcdn
!!$  REAL(kind=8),DIMENSION(3) :: ikcdmass
!!$  REAL(kind=8),DIMENSION(2) :: tik,nik
!!$
!!$  ikcd    = this(icdan)%ianbdy
!!$  ikGcdt  = this(icdan)%Gcdt3
!!$  ikGcdn  = this(icdan)%Gcdn3
!!$  ikcdmass= get_inv_mass_POLYG(ikcd)
!!$  tik     = this(icdan)%tuc
!!$  nik     = this(icdan)%nuc
!!$
!!$ END SUBROUTINE get_Wik_DKPLx
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE compute_Wikjl_DKPLx(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy
!!$  REAL(kind=8)              :: WTT,WTN,WNT,WNN
!!$  REAL(kind=8),DIMENSION(3) :: icdmass,ianmass,jcdmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$  jcdbdy=this(jcdan)%icdbdy
!!$
!!$  icdmass = get_inv_mass_POLYG(icdbdy)
!!$  ianmass = get_inv_mass_DISKx(ianbdy)
!!$  jcdmass = get_inv_mass_POLYG(icdbdy)
!!$
!!$!cas ij-ik
!!$  IF (icdbdy == jcdbdy) THEN
!!$
!!$    WTT =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdt3
!!$
!!$    WTN =  icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gcdn3
!!$
!!$    WNT =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdt3
!!$
!!$    WNN =  icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          +icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gcdn3
!!$!cas ij-kj
!!$  ELSE
!!$
!!$    WTT =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gant3
!!$
!!$    WTN =  ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gann3
!!$
!!$    WNT =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gant3
!!$
!!$    WNN =  ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          +ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gann3
!!$  ENDIF
!!$
!!$ END SUBROUTINE compute_Wikjl_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_nb_DKPLx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_DKPLx = nb_DKPLx
   case(i_verlet_tactor)
      get_nb_DKPLx = nb_vDKPLx
   case(i_rough_tactor)
      get_nb_DKPLx = nb_rough_DKPLx
   case(i_recup_tactor)
      get_nb_DKPLx = nb_recup_DKPLx
   end select

 end function get_nb_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE DKPLx2POLYG(icdan,icdtac,iantac)

   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

END SUBROUTINE DKPLx2POLYG
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE DKPLx2DISKx(icdan,iantac)

   IMPLICIT NONE
   INTEGER          :: icdan,iantac
   
   iantac = this(icdan)%iantac

END SUBROUTINE DKPLx2DISKx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE get_type_DKPLx(icdan,TYPE)
 IMPLICIT NONE
  INTEGER          :: icdan,TYPE
  TYPE = this(icdan)%dct
END SUBROUTINE get_type_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE get_numcorps_DKPLx(icdan,icdbdy,ianbdy)

   IMPLICIT NONE

   INTEGER          :: icdan,icdbdy,ianbdy

   icdbdy   = this(icdan)%icdbdy
   ianbdy   = this(icdan)%ianbdy

 END SUBROUTINE get_numcorps_DKPLx

!------------------------------------------------------------------------ 
SUBROUTINE print_info_DKPLx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'POLYG:',1x,I5,1x,'DISKx:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   CALL print_info_DISKx(icdbdy)
   CALL print_info_POLYG(ianbdy)

END SUBROUTINE print_info_DKPLx
!------------------------------------------------------------------
!------------------------------------------------------------------------
real(kind=8) function get_length_DKPLx(icdan)
  implicit none
  !
  integer(kind=4), intent(in) :: icdan 
  !
  real(kind=8)    :: raycd

  raycd = get_radius_DISKx(this(icdan)%icdtac)

  get_length_DKPLx = raycd
  
end function get_length_DKPLx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE detect(PGa,rayb,coordb,xco,sep,n,ovlap,adist,contact,crita,debug)

   IMPLICIT NONE

   TYPE(T_POLYG)               :: PGa                 ! corps a (polyg) notation JJM
   REAL(kind=8)                :: rayb                ! radius body b
   REAL(kind=8),DIMENSION(2)   :: coordb              ! Coordinate of center of b (diskx)
   REAL(kind=8),DIMENSION(2)   :: xco                 ! coordonnées des points de contacts
   REAL(kind=8),DIMENSION(2)   :: sep                 ! vecteur separateur   
   REAL(kind=8),DIMENSION(2)   :: n                   ! normale du contact
   REAL(kind=8)                :: ovlap               ! overlap pour le point 1 et éventuellement 2
   REAL(kind=8)                :: adist
   INTEGER                     :: nb_vertex_PGa
  
   REAL(kind=8)                :: scal,dist1,dist2,scal1
   REAL(kind=8)                :: proda,prodb,prodi   ! valeurs des projections sur la ligne des centres sep
   REAL(kind=8)                :: norm,over0          ! valeur de la norme de sep et valeur de l'overlap
   INTEGER                     :: sens,contact        ! sens pour le parcours des sommets des polygones et 
   INTEGER                     :: preceda,crita,suiva
   INTEGER                     :: num_vertex  ! variables pour parcourir les sommets
   INTEGER                     :: iverta,i,crita0,critb0
   REAL(kind=8),DIMENSION(2)   :: tgn,del,sep0
   REAL(kind=8),DIMENSION(2)   :: centre              ! coordonnées du milieu des centres actifs
   REAL(kind=8),DIMENSION(2)   :: vect            
   logical                     :: debug
   real(kind=8)                :: dist

   ! if (debug) print*,'-----'
   
   ovlap  = 0.d0
   xco    = 0.d0
   contact=0
   nb_vertex_PGa=PGa%nb_vertex

   
   !** shadow ovelap **
   !------------------------------------------------------------------------------------------
   ! Recherche du sommet critique par projection sur la ligne des centres
   ! en fait le vecteur sep et la determination de l'overlap, traduisent le recouvrement eventuel des projections
   !------------------------------------------------------------------------------------------
   proda=-1.d+20
   DO iverta=1,nb_vertex_PGa
     prodi=  sep(1)*PGa%vertex(1,iverta) + sep(2)*PGa%vertex(2,iverta) 
     IF ( prodi > proda ) THEN
       crita=iverta
       proda=prodi
     ENDIF
   ENDDO

   vect = coordb-rayb*sep
   prodb= sep(1)*vect(1)+sep(2)*vect(2)

   over0= proda-prodb
   
   ovlap=over0   

   ! if (debug) then
   !   print*,proda,prodb,ovlap
   !   print*,'crita ',crita 
   ! endif   
   
   !------------------------------------------------------------------------------------------
   ! Si l'overlap est negatif les projections des sommets des deux polygones ne se recoupent
   ! pas donc on est sur qu'il n'y a pas contact. Dans le cas contraire il faut faire une
   ! analyse plus fine.
   !------------------------------------------------------------------------------------------
   ! truc a la con ... over0 est un recouvrement (-gap)
   ! donc ici on teste -over0 > adist
   IF (over0 + adist < 0.d0) RETURN

   !fd le 2022-07-21
   ! dans certains cas utiliser crita comme point le plus proche pose pb
   ! recherche d'un point le plus proche
   proda=1d20
   crita=0
   DO iverta=1,nb_vertex_PGa
     prodi =  sep(1)*PGa%normale(1,iverta) + sep(2)*PGa%normale(2,iverta)
     !on ne prend que les bords dont la normale est dans le meme sens que sep  (angle maxi ~80 deg)
     IF ( prodi > 0.1d0 ) THEN
       dist = sqrt((PGa%vertex(1,iverta)-coordb(1))**2 + (PGa%vertex(2,iverta)-coordb(2))**2)
       if (dist < proda) then
         proda=dist  
         crita=iverta 
       endif   
     ENDIF
   ENDDO

   ! if (debug) print*,'crita recalcule',crita

   !------------------------------------------------------------------------------------------
   ! Determination des rotations du vecteur reliant les centres possibles pour diminuer
   ! l'overlap et eventuellement le rendre negatif.
   ! Si aucune position trouvee alors il y a contact
   !------------------------------------------------------------------------------------------

   del=PGa%vertex(1:2,crita)-vect   

   sep0=PGa%vertex(1:2,crita)-coordb
   norm=SQRT(sep0(1)*sep0(1)+sep0(2)*sep0(2))
   sep0=sep0/norm


   !fd pas logique qu'on teste avec sep ; prendre normal au segment ?
   ! sens=-1
   ! IF (sep(1)*del(2)-sep(2)*del(1) > 0.d0) sens=1

   ! if (debug) then 
   !    if (crita == 1) then
   !       print*,'n ',PGa%normale(:,crita)
   !       print*,'v ',del           
   !      stop 
   !    endif
   ! endif   
   
   sens=-1
   ! fd avec del ca introduit un biais dans certains cas   
   ! IF (PGa%normale(1,crita)*del(2)-PGa%normale(2,crita)*del(1) > 0.d0) sens=1
   IF (PGa%normale(1,crita)*sep0(2)-PGa%normale(2,crita)*sep0(1) > 0.d0) sens=1
   
   IF (sens == 1) THEN

       ! on est dans le segment precedent
       ! if (debug) print*,'sens +1'

       IF (crita==1) THEN
         preceda=nb_vertex_PGa
       ELSE
         preceda=crita-1
       ENDIF      

       !fd critere pour savoir si on est dans le segment ou sur le coin ?
       ! scal = sep(1)*PGa%normale(1,preceda)+sep(2)*PGa%normale(2,preceda) 
       ! scal1= sep(1)*sep0(1)+sep(2)*sep0(2)
       ! IF (scal > -scal1) THEN

       ! fd avec del ca introduit un biais dans certains cas
       ! IF (PGa%normale(1,preceda)*del(2)-PGa%normale(2,preceda)*del(1) > 0.d0) THEN
       IF (PGa%normale(1,preceda)*sep0(2)-PGa%normale(2,preceda)*sep0(1) > 0.d0) THEN          

         ! if (debug) then
         !     print*,'segment'
         !     print*,'sep  ',sep
         !     print*,'scal ',scal,scal1
         !     print*,'preced ',preceda
         ! endif
             
         sep=PGa%normale(:,preceda)
         vect = coordb-rayb*sep
         crita=preceda
       ELSE

         ! if (debug) print*,'coin'

         ! la normale   
         sep=-sep0
         ! le bras de levier
         vect = coordb-rayb*sep

       ENDIF
       
       del=PGa%vertex(1:2,crita)-vect
       over0=sep(1)*del(1)+sep(2)*del(2)
      
   ELSE

     ! on est dans ce segment 
     ! if (debug) print*,'sens -1'

       !je ne comprends pas ça 
     
       ! ! determination de la plus petite des deux rotations possibles
       ! scal = sep(1)*PGa%normale(1,crita)+sep(2)*PGa%normale(2,crita)
       ! scal1= sep(1)*sep0(1)+sep(2)*sep0(2)
       
       ! IF (scal>-scal1) THEN

       !   if (debug) print*,'segment'
          
       !   sep=PGa%normale(:,crita)
       !   vect = coordb-rayb*sep
       !   IF (crita==nb_vertex_PGa) THEN
       !     crita=1
       !   ELSE
       !     crita=crita+1
       !   ENDIF         
       ! ELSE

       !   if (debug) print*,'coin'
          
       !   ! la normale au contact est passee    
       !   sep=-sep0
       !   ! le bras de levier
       !   !? bug ? 
       !   vect = coordb-rayb*sep
         
       ! ENDIF

       sep=PGa%normale(:,crita)
       vect = coordb-rayb*sep
       del=PGa%vertex(1:2,crita)-vect
       over0=sep(1)*del(1)+sep(2)*del(2)
   ENDIF

   ovlap=over0

   ! truc a la con ... over0 est un recouvrement (-gap)
   ! donc ici on teste -over0 > adist
   IF (over0 + adist < 0.d0) then
      ! if (debug) then
      !    print*,'lost'
      !    print*,'-----'
      ! endif   
      RETURN ! on sort de la routine
   endif

   
   xco = vect + ovlap*sep
   n   = sep
   contact = 1

   ! if (debug) then
   !   print*,'adist ',adist 
   !   print*,'ovlap ',ovlap 
   !   print*,'sep   ',sep
   !   print*,'vect  ',vect
   !   print*,'---'
   ! endif   
 END SUBROUTINE detect
!------------------------------------------------------------------------ 
LOGICAL FUNCTION RUN_DKPLx(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_DKPLx = RUN_TACTOR

END FUNCTION RUN_DKPLx
!------------------------------------------------------------------------
  logical function CHECK_DKPLx()
    implicit none
    !   
    integer :: isee, nb_DISKx, nb_POLYG
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_DKPLx = check_DKPLx_
      return
    end if

    con_pedigree%module_name = 'DKPLx'

    con_pedigree%id_cdan  = i_dkplx
    con_pedigree%id_cdtac = i_diskx
    con_pedigree%id_antac = i_polyg

    cdtact2bdyty => diskx2bdyty
    antact2bdyty => polyg2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_POLYG = get_nb_POLYG()
    nb_DISKx = get_nb_DISKx()
    if( nb_POLYG == 0 .or. nb_DISKx == 0 ) then
      CHECK_DKPLx = check_DKPLx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'POLYG') then
        check_DKPLx_ = .true.
        exit
      end if
    end do
  
    CHECK_DKPLx = check_DKPLx_
    return
  
  end function CHECK_DKPLx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_DKPLx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_DKPLx = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_DKPLx
!------------------------------------------------------------------------
 SUBROUTINE get_g2l_DKPLx(icdan,g2l)

   IMPLICIT NONE
   INTEGER      :: icdan
   REAL(kind=8) :: g2l(2,6)

   !fd 
   ! construction de la matrice qui permet de calculer 
   ! la vitesse relative t,n (l comme locale)
   ! a partir de 
   ! la vitesse des objets candidat et antagoniste
   ! x_c,y_c,w_c,x_a,y_a,w_a (g comme globale) 

   g2l = 0.d0

   !vlt =  (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
   !     + (cd_Vbegin(2)-an_Vbegin(2))*t(2) &
   !     + cd_Vbegin(3)*this(icdan)%Gcdt3   &
   !     - an_Vbegin(3)*this(icdan)%Gant3

   g2l(1,1) = this(icdan)%tuc(1)
   g2l(1,2) = this(icdan)%tuc(2) 
   g2l(1,3) = this(icdan)%Gcdt3 
   g2l(1,4) =-this(icdan)%tuc(1)
   g2l(1,5) =-this(icdan)%tuc(2) 
   g2l(1,6) =-this(icdan)%Gant3 
   
            
   !vln =  (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
   !     + (cd_Vbegin(2)-an_Vbegin(2))*n(2) &
   !     + cd_Vbegin(3)*this(icdan)%Gcdn3   &
   !     -  an_Vbegin(3)*this(icdan)%Gann3

   g2l(2,1) = this(icdan)%nuc(1)
   g2l(2,2) = this(icdan)%nuc(2) 
   g2l(2,3) = this(icdan)%Gcdn3 
   g2l(2,4) =-this(icdan)%nuc(1)
   g2l(2,5) =-this(icdan)%nuc(2) 
   g2l(2,6) =-this(icdan)%Gann3 
   
 END SUBROUTINE get_g2l_DKPLx

 ! rm : functions for siconos wrapper

 FUNCTION get_old_index_DKPLx(icdan)
   IMPLICIT NONE
   INTEGER :: icdan
   INTEGER :: get_old_index_DKPLx
   !
   INTEGER :: icdtac,iantac,iadj

   get_old_index_DKPLx = 0

   IF (.NOT. ALLOCATED(verlt)) THEN
      RETURN
   ENDIF
   
   icdtac = this(icdan)%icdtac                ! serial number of candidate contactor for contact icdan
   iantac = this(icdan)%iantac                ! serial number of antagonist contactor for contact icdan 

   IF (verlt(icdtac)%adjsz /= 0) THEN
      if ( verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac) .and. &
           verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac)       &
         ) then
         do iadj=1,verlt(icdtac)%adjsz
            if ( verlt(icdtac)%anbdy(iadj)  == polyg2bdyty(1,iantac) .and. &
                 verlt(icdtac)%antac(iadj)  == polyg2bdyty(2,iantac) .and. &
                 verlt(icdtac)%anmodel(iadj)== polyg2bdyty(3,iantac)       &
                 ) then
               get_old_index_DKPLx = verlt(icdtac)%icdan(iadj)
               exit
            end if
         end do
      end if
   END IF

 END FUNCTION get_old_index_DKPLx


 function get_icdtac_DKPLx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_DKPLx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_POLYG
   logical :: found

   found = .false.

   nb_POLYG = get_nb_POLYG()

   icc = 0
   do icdtac = 1, nb_POLYG
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_DKPLx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKPLx::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_DKPLx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_DKPLx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_POLYG
   logical :: found

   found = .false.

   nb_POLYG = get_nb_POLYG()

   icc = 0
   do icdtac = 1, nb_POLYG
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_DKPLx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKPLx::get_icdtac','unknown contact index')
   

   get_iantac_DKPLx = this(icdan)%iantac

 end function


  !> \brief Set period value for periodic conditions in the x direction
  subroutine set_periodic_data_DKPLx(per,FLAG)
    implicit none
    real(kind=8) :: per
    logical      :: FLAG
    
    periode  = per
    PERIODIC = FLAG
    
  end subroutine set_periodic_data_DKPLx

 
 subroutine clean_memory_DKPLx
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_DKPLx  = 0
   nb_vDKPLx = 0

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%DKwhich) ) deallocate(box(i,j)%DKwhich)
         if( associated(box(i,j)%PLwhich) ) deallocate(box(i,j)%PLwhich)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_DKPLx) ) deallocate(rough_DKPLx)

   nb_rough_DKPLx = 0
   nstep_rough_seek_DKPLx = 1
   nb_recup_DKPLx = 0

   RUN = .false.

   if( allocated(DK_coor) ) deallocate(DK_coor)
   if( allocated(PL_coor) ) deallocate(PL_coor)

   Reac_DKPLx_MAX = 0.D0

   !maxray, minray, maxalert, meanradius
   !Lbox,LBox_1,norm
   !maxpopul

   module_checked_ = .FALSE.
   check_DKPLx_    = .FALSE.

 end subroutine
 
 subroutine set_nb_DKPLx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_DKPLx = nb

 end subroutine

 subroutine redo_nb_adj_DKPLx()
   implicit none

   call redo_nb_adj_( get_nb_DISKx() )

 end subroutine

END MODULE DKPLx
