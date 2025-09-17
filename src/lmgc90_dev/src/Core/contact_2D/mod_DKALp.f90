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
MODULE DKALp                                          

  !!****h* LMGC90.CORE/DKALp
  !! NAME
  !!  module DKALp
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors DISKx and ALxxx.
  !!  In this modulus candidate contactors are DISKx and antagonist 
  !!  contactors are ALxxx
  !! USES
  !!  LMGC90.CORE/MAILx
  !!  LMGC90.CORE/DISKx
  !!  LMGC90.CORE/ALpxx
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!****

  USE overall
  USE tact_behaviour

  USE DISKx, only : get_nb_diskx, diskx2bdyty           , &
                    print_info_diskx, get_radius_DISKx  , &
                    get_ent_diskx       => get_ent      , &
                    get_color_diskx     => get_color    , &
                    get_visible_diskx   => get_visible  , &
                    get_coorTT_diskx    => get_coorTT   , &
                    get_Vbegin_DISKx    => get_Vbegin   , &
                    add_reac_diskx      => add_reac     , &
                    get_vlocy_diskx     => get_vlocy    , &
                    comp_vlocy_diskx    => comp_vlocy   , &
                    nullify_reac_diskx  => nullify_reac , &
                    nullify_vlocy_diskx => nullify_vlocy

  USE ALpxx, only: get_nb_antac         => get_nb_alpxx        , &
                    nullify_reac_antac  => nullify_reac_ALpxx  , &
                    nullify_vlocy_antac => nullify_vlocy_ALpxx , &
                    comp_vlocy_antac    => comp_vlocy_ALpxx    , &
  !
  ! RIP
  !
                    get_nb_alpxx,nullify_reac_ALpxx,nullify_vlocy_ALpxx,comp_vlocy_ALpxx, &
                    get_nb_node_alpxx,get_nodes_alxxx,get_coorTT_alpxx,l_alpxx,alpxx2bdyty, &
                    get_visible_alpxx,get_ent_alpxx,get_vlocy_alpxx,add_reac_alpxx
  

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use inter_meca_2D

  use parameters, only : i_dkalp, i_diskx, i_alpxx, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

  logical :: bavard=.false.

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 
  
  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  INTEGER         :: nb_DKALp                          ! nb_DKALp = number of selected candidate DISKx against ALPxx
                                                       ! due to the fact that their might be 2 node_segment for each
                                                       ! entry in this it should be higher than size(this)
  INTEGER         :: nb_vDKALp                         ! nb_DKALp = number of selected candidates DISKx against JONCx
                                                       ! <= size(this).


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdbdy): number of adjacent pairs body-contactor
                                                         ! to candidate body ibdycd.

!------------------------------------------------------------------------  


!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------ 

 real(kind=8) :: Reac_DKALp_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------
 TYPE T_rough_DKALp                       ! définit le type de la liste des plus proches voisins
   INTEGER                   :: cd, &
                                ivert 
   REAL(KIND=8),DIMENSION(2) :: point 

   INTEGER                   :: an, &     !les fontieres les plus proches
                                inode, &  !les noeuds de ces fontieres les plus proches 
                                isee      ! 

   REAL(KIND=8)              :: adist     ! distance d'alerte
   REAL(KIND=8),DIMENSION(2) :: n

 END TYPE T_rough_DKALp

 TYPE(T_rough_DKALp),DIMENSION(:),ALLOCATABLE   :: rough_DKALp        ! table  de visibilité

!------------------------------------------------------------------------
 TYPE T_link_rough_DKALp                   ! liste chainée pour déterminer les listes de cand anta car on ne 
                                           ! connait pas le nb de cand -ant à priori
   TYPE(T_link_rough_DKALp), POINTER :: p  ! pointeur sur le precedent

   TYPE(T_rough_DKALp) :: val              ! les valeurs
  
   TYPE(T_link_rough_DKALp ), POINTER :: n ! pointeur sur le suivant

 END TYPE T_link_rough_DKALp

 TYPE(T_link_rough_DKALp),POINTER               :: Root,Current,Previous
!------------------------------------------------------------------------

 REAL(KIND=8),DIMENSION(:,:),ALLOCATABLE        :: DK_coor          ! tableau (3,nb_DISKx) contenant les coordonnées 
                                                                    ! des centres au cours du calcul


 TYPE T_coor
    REAL(KIND=8),DIMENSION(:,:),POINTER :: coor
 END TYPE T_coor

 TYPE(T_coor),DIMENSION(:),ALLOCATABLE  :: ALp      !  coordinates of body owning ALxxx to be used in selecting prox tactors

 TYPE T_l_ALp
    REAL(KIND=8),DIMENSION(:,:),POINTER :: n        ! normal to the ALxxx 
    REAL(KIND=8),DIMENSION(:,:),POINTER :: c        ! center of the ALxxx
    REAL(KIND=8),DIMENSION(:)  ,POINTER :: radius                     
 END TYPE T_l_ALp

 TYPE(T_l_ALp),DIMENSION(:),ALLOCATABLE  :: ALp_l    !  local inFORMATion of ALxxx to be used in selecting prox tactors

 INTEGER :: nb_ALxxx

!
 INTEGER      :: Nstep_rough_seek_DKALp=1
 LOGICAL      :: write_creation_tab_visu
 INTEGER      :: nb_rough_DKALp
 integer      :: nb_recup_DKALp

 logical      :: module_checked_ = .FALSE.
 logical      :: check_DKALp_    = .FALSE.

 logical      :: isnot_trimmed   = .TRUE.

!------------------------------------------------------------------------
! PUBLIC FUNCTIONS LIST
!
 PUBLIC &
      stock_rloc_DKALp, &
      recup_rloc_DKALp, &
      compute_box_DKALp, &
      read_ini_Vloc_Rloc_DKALp, &
      write_xxx_Vloc_Rloc_DKALp, &
      coor_prediction_DKALp, &
      creation_tab_visu_DKALp, &
      compute_contact_DKALp, &
      display_prox_tactors_DKALp, &
      RUN_DKALp, &
      CHECK_DKALp, &
      get_write_Vloc_Rloc_DKALp

 PUBLIC &
      nullify_reac_DKALp, nullify_vlocy_DKALp    , &
      injj_DKALp  , prjj_DKALp, vitrad_DKALp, &
      get_nb_DKALp, &
      DKALp2DISKx, &
      print_info_DKALp, &
      get_length_DKALp, &
      get_icdtac_DKALp, &
      get_iantac_DKALp, &
      get_iannodes_DKALp, &
      get_old_index_DKALp, &
      clean_memory_DKALp

 !rm for handler
 public get_this    , &
        set_nb_DKALp, &
        redo_nb_adj_DKALp, &
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


!***********************************************************************
  SUBROUTINE coor_prediction_DKALp

    IMPLICIT NONE
    
    INTEGER      :: itact,errare,ialpxx,inod
    INTEGER      :: ia,ib
    REAL(kind=8) :: norm
    REAL(kind=8),DIMENSION(2) :: coor
    INTEGER         :: nb_DISKx
    INTEGER         :: nb_ALpxx


    nb_DISKx = get_nb_DISKx()
    nb_ALpxx = get_nb_ALpxx()

    DO itact = 1,nb_DISKx
       DK_coor(1:3,itact) = get_coorTT_DISKx(itact)
    END DO

    DO ialpxx=1,nb_ALpxx
       CALL get_coorTT_ALpxx(ialpxx,get_nb_node_ALpxx(ialpxx),ALp(ialpxx)%coor)
    END DO

    DO ialpxx = 1,nb_ALpxx
       DO inod = 1,get_nb_node_ALpxx(ialpxx)-1
          
          ia = l_ALpxx(ialpxx)%idata(inod  )
          ib = l_ALpxx(ialpxx)%idata(inod+1)
          
          coor(1:2) = ALp(ialpxx)%coor(1:2,inod+1) - ALp(ialpxx)%coor(1:2,inod)
          norm = SQRT(coor(1)*coor(1) + coor(2)*coor(2))

          ALp_l(ialpxx)%radius(inod) = norm*0.5
          
          ALp_l(ialpxx)%N(1,inod) = -coor(2)/norm
          ALp_l(ialpxx)%N(2,inod) =  coor(1)/norm
          
          ALp_l(ialpxx)%c(1,inod) = (ALp(ialpxx)%coor(1,inod) + ALp(ialpxx)%coor(1,inod+1))*0.5
          ALp_l(ialpxx)%c(2,inod) = (ALp(ialpxx)%coor(2,inod) + ALp(ialpxx)%coor(2,inod+1))*0.5

       END DO
    END DO

  END SUBROUTINE coor_prediction_DKALp
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_DKALp(step)
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
    
  end subroutine read_ini_Vloc_Rloc_DKALp
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_DKALp(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_DKALp
!!!------------------------------------------------------------------------
 
SUBROUTINE compute_box_DKALp

  IMPLICIT NONE

  INTEGER  :: isee,errare,ibdy
  INTEGER  :: ialpxx,iplal
  INTEGER         :: nb_DISKx
  INTEGER         :: nb_ALpxx

                             !1234567890123456789012
  character(len=22) :: IAM = 'mod_DKALp::compute_box'

  nb_DISKx = get_nb_DISKx()
  nb_ALpxx = get_nb_ALpxx()

  ! on fait ici les choses qui ne doivent etre modifiees que 
  ! lorsque nb_DISKx change
  ! on DIMENSIONne le adjac/rough sur le nombre de DISKx

  IF (.NOT. ALLOCATED(adjac))THEN
    ALLOCATE(adjac(nb_DISKx),stat=errare)
    IF (errare /=0 ) THEN
      call faterr(IAM,'Error in allocating adjac')
    ENDIF
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
  ENDIF
  
  nb_adj = 0

  ! DK_coor are coordinates of bodies owning DISKx to be used in selecting prox tactors
   
  IF (ALLOCATED(DK_coor)) DEALLOCATE(DK_coor)
  ALLOCATE(DK_coor(3,nb_DISKx),stat=errare)
   
  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating DK_coor')
  ENDIF    

  ! ALp%coor are coordinates of bodies owning ALpxx to be used in selecting prox tactors
  ! for each ALpxx you may have several ALxxx, indeed you need 2 loops

  IF (ALLOCATED(ALp)) THEN
    DO ialpxx = 1,SIZE(Alp) 
      DEALLOCATE(Alp(ialpxx)%coor)
    ENDDO
    DEALLOCATE(ALp)
  ENDIF

  nb_ALpxx = get_nb_ALpxx()
  ALLOCATE(ALp(nb_ALpxx),stat=errare)

  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating ALp')
  ENDIF    
 
  DO ialpxx=1,nb_ALpxx
    ALLOCATE(ALp(ialpxx)%coor(2,get_nb_node_ALpxx(ialpxx)),stat=errare)     
  ENDDO

  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating ALp%coor')
  ENDIF    

  ! ALp_l%n,... are coordinates of normals owning ALpxx to be used in selecting prox tactors
  ! for each ALpxx you may have several normal (one for each ALxxx), indeed you need 2 loops

  IF (ALLOCATED(ALp_l)) THEN
    DO ialpxx = 1,SIZE(Alp_l) 
      DEALLOCATE(Alp_l(ialpxx)%n,Alp_l(ialpxx)%c,Alp_l(ialpxx)%radius)
    ENDDO
    DEALLOCATE(ALp_l)
  ENDIF

  ALLOCATE(ALp_l(nb_ALpxx),stat=errare)

  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating ALp_l')
  ENDIF    
  
  DO ialpxx=1,nb_ALpxx
    ALLOCATE(ALp_l(ialpxx)%n(2,get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
    ALLOCATE(ALp_l(ialpxx)%c(2,get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
    ALLOCATE(ALp_l(ialpxx)%radius(get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
  ENDDO

  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating ALp_l%xxx')
  ENDIF    

  IF (ALLOCATED(rough_DKALp)) DEALLOCATE(rough_DKALp)
  ALLOCATE(rough_DKALp(nb_DISKx),stat=errare)   

  IF (errare /=0 ) THEN
    call faterr(IAM,'Error allocating rough_DKALp')
  ENDIF    

  
END SUBROUTINE compute_box_DKALp

!********************************************************************************
!* SUBROUTINE CREATION_TAB_VISU
!*
!********************************************************************************

SUBROUTINE creation_tab_visu_DKALp
 
  IMPLICIT NONE 
 
  INTEGER                   :: errare 
  INTEGER                   :: icdtac,ivertex,iantac,inod,isee,i
  REAL(KIND=8)              :: adist,raycd,rayan,dist,pscal,seuil
  REAL(KIND=8),DIMENSION(2) :: coorcd,cooran
  CHARACTER(LEN=5)          :: cdcol,ancol,cdtac,antac,cdbdyty
  INTEGER         :: nb_DISKx
  INTEGER         :: nb_ALpxx

  character(len=80) :: cout

   nb_DISKx=get_nb_DISKx()
   nb_ALpxx=get_nb_ALpxx()


  !* Detecting contacts; 
  !* contacts are being detected within a circle centered on an ALxxx
  
  !* first reading: determine the size of array THIS
  
  nb_rough_DKALp = 0
  
  !* create list of potential contact
  
  NULLIFY(Root) 
  NULLIFY(Current)
  NULLIFY(Previous)


  DO icdtac = 1,nb_DISKx   
            
    IF ( .NOT.get_visible_DISKx(icdtac) ) CYCLE
    
    cdcol   = get_color_DISKx(icdtac)
    cdbdyty = get_body_model_name_from_id(diskx2bdyty(3,icdtac))
    raycd   = get_radius_DISKx(icdtac)
    
    DO iantac = 1,nb_ALpxx

      ancol = get_color_MAILx(alpxx2bdyty(1,iantac),alpxx2bdyty(2,iantac))
      isee = get_isee(cdbdyty,'DISKx',cdcol,'MAILx','ALpxx',ancol)
      
      IF ( isee /= 0 ) THEN
  
        adist = see(isee)%alert 

        coorcd(1:2) = DK_coor(1:2,icdtac)
       
        DO inod = 1,get_nb_node_ALpxx(iantac)-1
         
          cooran(1:2) = ALp_l(iantac)%c(1:2,inod)

          rayan       = ALp_l(iantac)%radius(inod)
     
          dist  = (cooran(1)-coorcd(1))*(cooran(1)-coorcd(1)) + (cooran(2)-coorcd(2))*(cooran(2)-coorcd(2))
          seuil = (raycd + rayan + adist)*(raycd + rayan + adist)

          IF ( dist < seuil ) THEN

            nb_rough_DKALp = nb_rough_DKALp + 1
                  
            IF ( nb_rough_DKALp == 1 ) THEN
              ALLOCATE(Root)
              Current => Root
              NULLIFY(Root%p)
            ELSE 
              ALLOCATE(Current)
              Previous%n => Current
            ENDIF
                  
            Current%val%cd     = icdtac
            Current%val%an     = iantac
            Current%val%inode  = inod
            Current%val%n(1:2) = ALp_l(iantac)%n(1:2,inod)
            Current%val%isee   = isee
            Current%val%adist  = adist
             
            Current%p => Previous
            NULLIFY(Current%n)
            Previous => Current

          ENDIF
        ENDDO
      ENDIF
    ENDDO
  ENDDO

  WRITE(cout,'(4X,I10,A20)') nb_rough_DKALp,' DKALp roughly found'
  call logmes(cout)

  IF ( ALLOCATED(rough_DKALp) ) DEALLOCATE(rough_DKALp)
  ALLOCATE( rough_DKALp(nb_rough_DKALp) )  ! on s'alloue la table de visibilité utilisée dans compute_contact
  
  IF ( ALLOCATED(this) ) DEALLOCATE(this)
  ALLOCATE(this(nb_rough_DKALp))         ! on s'alloue un tableau de contact.

  DO i = nb_rough_DKALp,1,-1
     
    Previous => Current%p
    rough_DKALp(i)%cd         = Current%val%cd
    rough_DKALp(i)%an         = Current%val%an
    rough_DKALp(i)%inode      = Current%val%inode
    rough_DKALp(i)%n(1:2)     = Current%val%n(1:2)
    rough_DKALp(i)%isee       = Current%val%isee
    rough_DKALp(i)%adist      = Current%val%adist
    DEALLOCATE(Current)
    Current => Previous
  ENDDO
   
  NULLIFY(Root)

END SUBROUTINE creation_tab_visu_DKALp

!************************************************************************

SUBROUTINE compute_contact_DKALp
 
  IMPLICIT NONE  

  INTEGER                               :: errare,icdan,iadj,ibdy
  INTEGER                               :: i,icdtac,iantac,ialxxx,isee
  REAL(KIND=8)                          :: xp1,yp1,xp2,yp2,nx,ny,tx,ty,adist
  REAL(KIND=8)                          :: xa,ya,xb,yb,AB,APAB,gap,raycd,dist,APn,APt,norm
  REAL(KIND=8),DIMENSION(2)             :: cdlev,anlev,an_Vbegin,xco
  REAL(KIND=8),DIMENSION(3)             :: cd_Vbegin

  INTEGER                               :: cd_ent,an_ent
  INTEGER :: nb_DISKx

  character(len=80) :: cout

  icdan    = 0        
  iadj     = 0
  nb_DKALp = 0
  nb_adj   = 0

  IF ( nb_rough_DKALp /= 0 ) THEN
     
     DO i = 1,nb_rough_DKALp
        
        icdtac   = rough_DKALp(i)%cd
        iantac   = rough_DKALp(i)%an
        ialxxx   = rough_DKALp(i)%inode
        
        Nx       = rough_DKALp(i)%n(1)
        Ny       = rough_DKALp(i)%n(2)

        !fd  repere T,N direct 
        Tx = Ny
        Ty =-Nx
        
        !print*,'on teste le contact potentiel: ',i
        !print*,'candidat :',icdtac,'antagoniste :',iantac
        !print*,'normale :',Nx,Ny

        isee     = rough_DKALp(i)%isee  
        adist    = rough_DKALp(i)%adist
        
        raycd = get_radius_DISKx(icdtac)
        
        xp1 = DK_coor(1,icdtac)
        yp1 = DK_coor(2,icdtac)
        
        xa = ALp(iantac)%coor(1,ialxxx)
        ya = ALp(iantac)%coor(2,ialxxx) 
        
        xb = ALp(iantac)%coor(1,ialxxx+1)
        yb = ALp(iantac)%coor(2,ialxxx+1)
        
        AB = SQRT((xa-xb)*(xa-xb)+(ya-yb)*(ya-yb)) 
        
        APn = (xp1-xa)*Nx + (yp1-ya)*Ny
        APt = (xp1-xa)*Tx + (yp1-ya)*Ty
        
        !print*,'longueur du segment :',AB
        !print*,'projection :',APt

        APAB=0.d0

        !fd si le centre se projete sur la face            
        IF ( (APt >= 0.d0) .AND. ( APt <= AB ) )  THEN
        !fd IF ( (APt > -adist) .AND. ( APt < ( AB + adist ) ) )  THEN
              
          !print*,'on est dans la face'

          gap = APn - raycd
        
          APAB = APt/AB

          ! le repere locale est celui deja construit

          xco(1) = xa + Apt*Tx
          xco(2) = ya + Apt*Ty

        ELSE IF (isnot_trimmed .and. (APt < 0.d0 .AND. APt > -(raycd+adist))) THEN  
        !fd ELSE IF (APt <= -adist .AND. APt > -(raycd+adist)) THEN
           
          !print*,'on est dans le cone gauche'

          !fd le contact potentiel va donc etre sur la ligne A P1           

          APAB = 0.d0
           
          norm = SQRT((xa-xp1)*(xa-xp1)+(ya-yp1)*(ya-yp1))

          Nx = (xp1-xa) / norm
          Ny = (yp1-ya) / norm

          Tx = Ny
          Ty =-Nx

          xco(1) = xa 
          xco(2) = ya 

          APn = (xp1-xa)*Nx + (yp1-ya)*Ny
          
          gap = APn - raycd

          
        ELSE IF (isnot_trimmed .and. (APt > AB .AND. APt < (AB+raycd+adist))) THEN  

          !print*,'on est dans le cone droit'

          !fd le contact potentiel va donc etre sur la ligne B P1           

          APAB = 1.d0
           
          norm = SQRT((xb-xp1)*(xb-xp1)+(yb-yp1)*(yb-yp1))
          
          Nx = (xp1-xb) / norm
          Ny = (yp1-yb) / norm

          Tx = Ny
          Ty =-Nx

          xco(1) = xb 
          xco(2) = yb 

          APn = (xp1-xb)*Nx + (yp1-yb)*Ny
          
          gap = APn - raycd
          
        ELSE

          !print*,'else'

          gap = 10.*adist

        ENDIF


        !print*,'le gap: ',gap
        !print*,'le repere:',Nx,Ny,Tx,Ty
        !print*,'la coordonnee reduite: ',APAB

        IF( gap .LE. adist .AND. gap .GE. -raycd ) THEN

              icdan = icdan + 1
              
              nb_adj(icdtac) = nb_adj(icdtac) + 1 
              iadj = nb_adj(icdtac)
              
              this(icdan)%iadj    = iadj 
              this(icdan)%icdbdy  = diskx2bdyty(1,icdtac)
              this(icdan)%icdtac  = icdtac
              this(icdan)%icdsci  = 0
              
              this(icdan)%ianbdy  = alpxx2bdyty(1,iantac)
              
              this(icdan)%iantac  = iantac
              this(icdan)%iansci  = ialxxx
              
              this(icdan)%icdbtac = diskx2bdyty(2, icdtac)
              this(icdan)%ianbtac = alpxx2bdyty(2, iantac)

              this(icdan)%icdbtyp = diskx2bdyty(3, icdtac)
              this(icdan)%ianbtyp = alpxx2bdyty(3, iantac)

              this(icdan)%icdctyp = i_diskx
              this(icdan)%ianctyp = i_alpxx

              cd_ent = get_ent_DISKx(this(icdan)%icdtac)
              an_ent = get_ent_ALpxx(this(icdan)%iantac)

              this(icdan)%icdent = cd_ent
              this(icdan)%ianent = an_ent

              entity(cd_ent)%nb  = entity(cd_ent)%nb+1
              entity(an_ent)%nb  = entity(an_ent)%nb+1
              
              this(icdan)%isee   = isee    
              
              this(icdan)%nuc(:) = (/ nx,ny /)  
              this(icdan)%tuc(:) = (/ tx,ty /)
              
              this(icdan)%gapTTBegin = gap
              this(icdan)%cpcd       = APAB
              
              this(icdan)%coor(1) = xco(1)
              this(icdan)%coor(2) = xco(2)
              
              cdlev(1) = this(icdan)%coor(1) - xp1
              cdlev(2) = this(icdan)%coor(2) - yp1
              
              this(icdan)%Gcdt3 = cdlev(1)*Ty - cdlev(2)*Tx
              this(icdan)%Gcdn3 = cdlev(1)*Ny - cdlev(2)*Nx
              
              cd_Vbegin         = get_Vbegin_DISKx(icdtac)
          
              CALL get_vlocy_ALpxx(iantac,ialxxx,iVbeg_,an_Vbegin,APAB)
              
              this(icdan)%vltBEGIN      = (cd_Vbegin(1)-an_Vbegin(1))*Tx &
                                        + (cd_Vbegin(2)-an_Vbegin(2))*Ty &
                                        + cd_Vbegin(3)*this(icdan)%Gcdt3

              this(icdan)%vlnBEGIN      = (cd_Vbegin(1)-an_Vbegin(1))*nx &
                                        + (cd_Vbegin(2)-an_Vbegin(2))*ny &
                                        + cd_Vbegin(3)*this(icdan)%Gcdn3

              this(icdan)%rlt           = 0.D0
              this(icdan)%rln           = 0.D0
              this(icdan)%vlt           = this(icdan)%vltBEGIN
              this(icdan)%vln           = this(icdan)%vlnBEGIN
              this(icdan)%gapTT         = this(icdan)%gapTTbegin
              this(icdan)%status        = i_nknow

        ENDIF
     ENDDO
     nb_DKALp = icdan
  ENDIF
  
  WRITE(cout,'(1X,I10,A12)') nb_DKALp,' DKALp found'
  call logmes(cout)

  nb_DISKx=get_nb_DISKx()
 
  DO ibdy=1,nb_DISKx
      
    IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
    IF (nb_adj(ibdy) /= 0) THEN
      ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      IF (errare /=0 ) THEN
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_DKALp::compute_contact',cout)
      ENDIF
    ENDIF
  ENDDO

  DO icdan=1,nb_DKALp
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
  ENDDO

  
  do icdan = 1, nb_DKALp
     call get_behaviour_( icdan, see, tact_behav )
  end do

  IF (ALLOCATED(violation)) DEALLOCATE(violation)
  ALLOCATE(violation(nb_DKALp),stat=errare)
   
END SUBROUTINE compute_contact_DKALp

!*****************************************************************************************

subroutine display_prox_tactors_DKALp

  implicit none

  integer :: iadj,icdan,icdbdy,jbdycd,icdtac,ianbdy,iantac,isee,itact
  integer :: nb_DISKx
  character(len=5) :: cdmodel, anmodel

   nb_DISKx=get_nb_DISKx()



  IF (nb_DKALp == 0) RETURN
   
  DO itact=1,nb_DISKx
    DO iadj=1,nb_adj(itact)         

      icdan  = adjac(itact)%icdan(iadj)
      icdtac = this(icdan)%icdtac
      iantac = this(icdan)%iantac

      cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
      anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )

      WRITE(*,'(A1)')' '
      WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                       !1234567890123456789012345678901234567890123456789012345678901234567890123456
      WRITE(*,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr'       
      WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,12x)')   &
       cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac), &
       see(this(icdan)%isee)%behav,  &
       anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac)
      WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
      WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
      WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
      WRITE(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBegin
      WRITE(*,'(A1)')' '               
    ENDDO                           
  ENDDO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
end subroutine display_prox_tactors_DKALp

!************************************************************************

SUBROUTINE stock_rloc_DKALp
 
   !  
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE

   INTEGER                               :: errare 
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER         :: nb_DISKx

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_DKALp::stock_rloc'

   nb_DISKx=get_nb_DISKx()

   ! sizing verlt:
     IF (.NOT. ALLOCATED(verlt)) THEN
      ALLOCATE(verlt(nb_DISKx),stat=errare)
      IF (errare /=0 ) THEN
       write(cout,'(A,I0,A)') 'Error allocating verlt'
       call faterr(IAM,cout)
     ENDIF
 
     DO icdtac=1,nb_DISKx
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
         IF (errare /=0 ) THEN
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
           call faterr(IAM,cout)
         ENDIF
       ELSE 
         call nullify_verlet_(icdtac)
       ENDIF
     ENDDO
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
         ENDIF
       ELSE 
         call nullify_verlet_(icdtac)
       ENDIF
     ENDDO
   ENDIF

  ! filling data:
   DO icdan=1,nb_DKALp

     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                  ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                        ! serial adjacent number of pair body-contactor 
                                                      ! adjacent to candidate body for contact icdan 
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = diskx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = diskx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel         = diskx2bdyty(3,icdtac)
     verlt(icdtac)%cdsci(iadj)     = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)     = alpxx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)     = alpxx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)   = alpxx2bdyty(3,iantac)
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
   ENDDO

   nb_vDKALp = nb_DKALp

   WRITE(cout,'(1X,I10,A12)') nb_vDKALp,' stock DKALp'
   call logmes(cout)

 END SUBROUTINE stock_rloc_DKALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 

!************************************************************************

 SUBROUTINE recup_rloc_DKALp

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   !> \todo : Il faudrait pouvoir faire le tri entre le cas ou un DK voit plusieur AL d'un ALp
   !>         et le cas ou un DK ne voit qu'un seul AL dans l'ALp, mais passe au suivant !

   IMPLICIT NONE

   INTEGER :: icdan,icdtac,iantac,iadj
   CHARACTER(len=21)  :: IAM = 'mod_DKALp::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   IF (nb_DKALp == 0) RETURN  

   nb_recup_DKALp=0

   DO icdan=1,nb_DKALp
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
           if (verlt(icdtac)%anbdy(iadj)  == alpxx2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == alpxx2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== alpxx2bdyty(3,iantac) .and. &
               verlt(icdtac)%ansci(iadj)  == this(icdan)%iansci          &
           ) then
             this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H 
             this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H 
             this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
             this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
             nb_recup_DKALp = nb_recup_DKALp + 1
             exit
           end if
         end do
       end if
     ENDIF
   ENDDO

   WRITE(cout,'(1X,I10,A12)') nb_recup_DKALp,' recup DKALp'
   call logmes(cout)

 END SUBROUTINE recup_rloc_DKALp

!************************************************************************

SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE

   INTEGER                           :: icdan,icdbdy,icdtac,ianbdy,iantac,ialxxx,iadj,icdtact
   INTEGER                           :: cdmodel, anmodel
   REAL(KIND=8)                      :: rlt,rln,vlt,vln,gapTT
   REAL(KIND=8),DIMENSION(2)         :: nuc,coor
   CHARACTER(LEN=5)                  :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER                           :: errare 

   INTEGER :: ibehav,nb_internal,i_internal
   INTEGER         :: nb_DISKx
 
   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_DKALp::read_ini_Vloc_Rloc' 

   nb_DISKx=get_nb_DISKx()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contacts have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_DISKx),stat=errare)
     IF (errare /=0 ) call faterr(IAM,'error allocating nb_adj')
   ENDIF    

   nb_adj=0

   DO
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKALp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:83),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,9X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,ialxxx,        & ! no anseg and ianseg for DKALp contrary to PLPLx
                      sttus
     IF (cdtac /= 'DISKx' .OR. antac /= 'ALpxx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )
     do icdtact = 1, nb_DISKx
       if (diskx2bdyty(1,icdtact) == icdbdy .and. &
           diskx2bdyty(2,icdtact) == icdtac .and. &
           diskx2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
     CYCLE
   ENDDO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_DISKx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     ENDIF
     DO icdbdy=1,nb_DISKx
       verlt(icdbdy)%adjsz=0
       iadj=nb_adj(icdbdy)
       IF (iadj > 0) THEN
         verlt(icdbdy)%adjsz=iadj
         call new_verlet_(icdbdy, iadj, errare)
         IF (errare /=0 ) THEN
           write(cout,'(A,I0,A)') 'Error allocating verlt(',icdbdy,')%.....'
           call faterr(IAM,cout)
         ENDIF
       ELSE
         call nullify_verlet_(icdbdy)
       ENDIF
     ENDDO
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
       ENDIF
     ENDDO
   ENDIF
    
  ! second reading: filling data
   REWIND(G_nfich)
   nb_adj=0
   icdan = 0

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'DKALp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT

     READ(G_clin(1:83),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,9X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,        &
                      behav,                            &
                      anbdy,ianbdy,antac,iantac,ialxxx, &
                      sttus

     IF (cdtac /= 'DISKx' .AND. antac /= 'ALpxx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     DO icdtact=1,nb_DISKx
       IF (diskx2bdyty(1,icdtact) == icdbdy .and. &
           diskx2bdyty(2,icdtact) == icdtac .and. &
           diskx2bdyty(3,icdtact) == cdmodel ) then

         icdan = icdan + 1

         nb_adj(icdtact)=nb_adj(icdtact)+1 

         verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%status(nb_adj(icdtact)) = get_contact_status_id_from_name(sttus)
         verlt(icdtact)%ansci(nb_adj(icdtact))  = ialxxx

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
         ENDIF
         IF( .NOT. read_G_clin()) EXIT
         IF (G_clin(30:34)== 'coo1=') THEN
           READ(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
           verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
           verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)
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
     CYCLE
   ENDDO

   nb_vDKALp=0

   DO icdtact=1,nb_DISKx
     nb_vDKALp = nb_vDKALp + nb_adj(icdtact)

     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       write(cout,'(A,1x,I0)')   'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')   'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     ENDIF

   ENDDO

 END SUBROUTINE read_ini_Vloc_Rloc

!************************************************************************

SUBROUTINE write_out_Vloc_Rloc(nfich)

  !
  ! write into file out_Vloc_Rloc data from this, in verlt style
  !

  IMPLICIT NONE

  INTEGER                   :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
  INTEGER                   :: ialxxx,isee,nfich,icdtact,lc
  REAL(KIND=8),DIMENSION(2) :: coor
  integer :: nb_DISKx

  character(len=20) :: fmt
  character(len=5)  :: cdmodel, anmodel
  
  IF ( nb_DKALp == 0 ) RETURN

  nb_DISKx = get_nb_DISKx()

  DO icdtact=1,nb_DISKx

!     print*,'nb_adj(',icdtact,')= ',nb_adj(icdtact) 

    DO iadj=1,nb_adj(icdtact)         

!       print*,iadj,adjac(icdtact)%icdan(iadj)


      icdan  = adjac(icdtact)%icdan(iadj)
      icdtac = this(icdan)%icdtac
      iantac = this(icdan)%iantac
      ialxxx = this(icdan)%iansci

      cdmodel = get_body_model_name_from_id( diskx2bdyty(3,icdtac) )
      anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )

      WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','DKALp',icdan     
      !12345678901234567890123456789012345678901234567890123456789012345678901234567890123456789
      ! RBDY2  12345  DISKx  12345  BEHAV  MAILx  12345  ALPxx  12345 IALxx          STTUS 12345
      WRITE(nfich,'(A89)') &
      ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj '
      WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,9X,A5,1x,I5)')   &
      cdmodel,diskx2bdyty(1,icdtac),'DISKx',diskx2bdyty(2,icdtac),  &
      see(this(icdan)%isee)%behav,  &
      anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac),ialxxx, &
      get_contact_status_name_from_id(this(icdan)%status), iadj
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
    ENDDO                           
  ENDDO

103  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
END SUBROUTINE write_out_Vloc_Rloc

!************************************************************************


!************************************************************************

SUBROUTINE nullify_reac_DKALp(icdan,storage)

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: icdan 
  INTEGER            :: icdtac,iantac,storage
    
  icdtac = this(icdan)%icdtac
  CALL nullify_reac_DISKx(icdtac,storage)
   
  iantac = this(icdan)%iantac
  CALL nullify_reac_ALpxx(iantac,storage)

END SUBROUTINE nullify_reac_DKALp

!************************************************************************

SUBROUTINE nullify_vlocy_DKALp(icdan,storage)

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: icdan 
  INTEGER            :: icdtac,iantac,storage
    
   icdtac = this(icdan)%icdtac
   CALL nullify_vlocy_DISKx(icdtac,storage)
   
   iantac = this(icdan)%iantac
   CALL nullify_vlocy_ALpxx(iantac,storage)

END SUBROUTINE nullify_vlocy_DKALp

!************************************************************************

SUBROUTINE vitrad_DKALp(icdan,storage,need_full_vlocy)

  IMPLICIT NONE

  INTEGER,INTENT(IN) :: icdan
  INTEGER            :: storage
  INTEGER            :: icdtac,iantac,ialxxx
  logical, optional  :: need_full_vlocy

  icdtac = this(icdan)%icdtac
  CALL comp_vlocy_DISKx(icdtac,storage)
  
  iantac = this(icdan)%iantac
  ialxxx=this(icdan)%iansci
  CALL comp_vlocy_ALpxx(iantac,ialxxx,storage,need_full_vlocy)
  
END SUBROUTINE vitrad_DKALp

!************************************************************************

SUBROUTINE injj_DKALp(icdan,RTIK,RNIK,storage)
 
  IMPLICIT NONE

  INTEGER     ,INTENT(IN)    :: icdan
  REAL(KIND=8),INTENT(IN)    :: RTIK,RNIK
  
  INTEGER                    :: icdtac,iantac,ialxxx,storage
  INTEGER,     DIMENSION(3)  :: cdccdof
  REAL(KIND=8),DIMENSION(2)  :: anreac
  REAL(KIND=8),DIMENSION(3)  :: cdreac
  
  icdtac = this(icdan)%icdtac

  iantac = this(icdan)%iantac
  ialxxx = this(icdan)%iansci
   
  cdccdof(1) = 1

  cdreac(1)  = RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
  anreac(1)  =-cdreac(1)
  
  cdccdof(2) = 2
   
  cdreac(2)  = RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
  anreac(2)  =-cdreac(2)
  
  cdccdof(3) = 3
  cdreac(3)  = this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK

  CALL add_reac_DISKx(icdtac,cdccdof,cdreac,storage)
  
  CALL add_reac_ALpxx(iantac,ialxxx,anreac(1:2),this(icdan)%cpcd,storage)

END SUBROUTINE injj_DKALp

!************************************************************************

SUBROUTINE prjj_DKALp(icdan,VTIK,VNIK,storage)
 
  IMPLICIT NONE

  INTEGER     ,INTENT(IN)   :: icdan
  REAL(KIND=8),INTENT(OUT)  :: VTIK,VNIK

  INTEGER                   :: icdtac,iantac,ialxxx
  integer     ,intent(in)   :: storage
  REAL(KIND=8),DIMENSION(3) :: Vcd,Van
   
  icdtac=this(icdan)%icdtac
  
  iantac=this(icdan)%iantac
  ialxxx=this(icdan)%iansci
  
  CALL get_vlocy_DISKx(icdtac,storage,Vcd)
  
  CALL get_vlocy_ALpxx(iantac,ialxxx,storage,Van,this(icdan)%cpcd)   
  
  VTIK = Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3 &
       - Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)
  VNIK = Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
       - Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)

 END SUBROUTINE prjj_DKALp 

!************************************************************************
!************************************************************************

 integer function get_nb_DKALp(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_DKALp = nb_DKALp
   case(i_verlet_tactor)
      get_nb_DKALp = nb_vDKALp
   case(i_rough_tactor)
      get_nb_DKALp = nb_rough_DKALp
   case(i_recup_tactor)
      get_nb_DKALp = nb_recup_DKALp
   end select

 end function get_nb_DKALp
!************************************************************************

SUBROUTINE DKALp2DISKx(icdan,icdtac)

  IMPLICIT NONE
  
  INTEGER :: icdan,icdtac
   
  icdtac = this(icdan)%icdtac

END SUBROUTINE DKALp2DISKx

!************************************************************************

 SUBROUTINE DKALp2ALpxx(icdan,iantac)

   IMPLICIT NONE

   INTEGER :: icdan,iantac
   
   iantac = this(icdan)%iantac

END SUBROUTINE DKALp2ALpxx

!************************************************************************

SUBROUTINE DKALp2ENT(icdan,icdent,ianent)

  IMPLICIT NONE

  INTEGER :: icdan,icdent,ianent
   
  icdent = get_ENT_DISKx(this(icdan)%icdbdy)
  ianent = get_ENT_ALpxx(this(icdan)%ianbdy)

END SUBROUTINE DKALp2ENT

!************************************************************************
!************************************************************************
!************************************************************************

SUBROUTINE get_numcorps_DKALp(icdan,icdbdy,ianbdy)

  IMPLICIT NONE

  INTEGER :: icdan,icdbdy,ianbdy

  icdbdy = this(icdan)%icdbdy
  ianbdy = this(icdan)%ianbdy

END SUBROUTINE get_numcorps_DKALp

!************************************************************************

SUBROUTINE print_info_DKALp(icdan)

  IMPLICIT NONE

  INTEGER           :: icdan,icdtac,iantac,ianal,icdbdy,ianbdy
  CHARACTER(LEN=80) :: cout

  icdtac=this(icdan)%icdtac
  iantac=this(icdan)%iantac
  ianal=this(icdan)%iansci

  WRITE(cout,1) icdtac,iantac
  CALL LOGMES(cout)

1 FORMAT(1X,'DISKx:',1x,I5,1x,'ALpxx:',1x,I5,1x,'segment:',1x,I5)

  icdbdy = this(icdan)%icdbdy
  ianbdy = this(icdan)%ianbdy
  
  CALL print_info_DISKx(icdbdy)

END SUBROUTINE print_info_DKALp
!------------------------------------------------------------------------
real(kind=8) function get_length_DKALp(icdan)
  implicit none
  !
  integer(kind=4), intent(in) :: icdan 
  !
  integer(kind=4) :: icdtac
  real(kind=8)    :: raycd

  raycd = get_radius_DISKx(this(icdan)%icdtac)

  get_length_DKALp = raycd
  
end function get_length_DKALp
!------------------------------------------------------------------------
LOGICAL FUNCTION RUN_DKALp(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_DKALp = RUN_TACTOR

END FUNCTION RUN_DKALp
!------------------------------------------------------------------------
  logical function CHECK_DKALp()
    implicit none
    !   
    integer :: isee, nb_DISKx, nb_ALpxx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_DKALp = check_DKALp_
      return
    end if

    con_pedigree%module_name = 'DKALp'

    con_pedigree%id_cdan  = i_dkalp
    con_pedigree%id_cdtac = i_diskx
    con_pedigree%id_antac = i_alpxx

    cdtact2bdyty => diskx2bdyty
    antact2bdyty => alpxx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_ALpxx = get_nb_ALpxx()
    nb_DISKx = get_nb_DISKx()
    if( nb_ALpxx == 0 .or. nb_DISKx == 0 ) then
      CHECK_DKALp = check_DKALp_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'DISKx' .and. see(isee)%antac == 'ALpxx') then
        check_DKALp_ = .true.
        exit
      end if
    end do
  
    CHECK_DKALp = check_DKALp_
    return
  
  end function CHECK_DKALp
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_DKALp(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_DKALp = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_DKALp


!!!------------------------------------------------------------------------ 

 function get_icdtac_DKALp(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_DKALp
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
         get_icdtac_DKALp = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKALp::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_DKALp(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_DKALp
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
         get_iantac_DKALp =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('DKALp::get_icdtac','unknown contact index')
   

   get_iantac_DKALp = this(icdan)%iantac

 end function

 ! rm : functions for siconos wrapper

 function get_iannodes_DKALp(icdan)
   implicit none
   integer(kind=4), intent(in)   :: icdan
   integer(kind=4), dimension(2) :: get_iannodes_DKALp

   get_iannodes_DKALp = get_nodes_ALxxx(this(icdan)%iantac, this(icdan)%iansci)
 end function

 FUNCTION get_old_index_DKALp(icdan)
   IMPLICIT NONE
   INTEGER :: icdan
   INTEGER :: get_old_index_DKALp
   !
   INTEGER :: icdtac,iantac,iadj

   get_old_index_DKALp = 0

   IF (.NOT. ALLOCATED(verlt)) THEN
      RETURN
   ENDIF
   
   icdtac = this(icdan)%icdtac                ! serial number of candidate contactor for contact icdan
   iantac = this(icdan)%iantac                ! serial number of antagonist contactor for contact icdan 

   IF (verlt(icdtac)%adjsz /= 0) THEN
      if (verlt(icdtac)%cdbdy  == diskx2bdyty(1,icdtac) .and. &
          verlt(icdtac)%cdtac  == diskx2bdyty(2,icdtac) .and. &
          verlt(icdtac)%cdmodel== diskx2bdyty(3,icdtac)       &
         ) then
         do iadj=1,verlt(icdtac)%adjsz
           if (verlt(icdtac)%anbdy(iadj)  == alpxx2bdyty(1,iantac) .and. &
               verlt(icdtac)%antac(iadj)  == alpxx2bdyty(2,iantac) .and. &
               verlt(icdtac)%anmodel(iadj)== alpxx2bdyty(3,iantac) .and. &
               verlt(icdtac)%ansci(iadj)  == this(icdan)%iansci          &
              ) then
              get_old_index_DKALp = verlt(icdtac)%icdan(iadj)
              exit
           end if
         end do
      end if
   END IF

 END FUNCTION get_old_index_DKALp

 subroutine get_g2l_DKALp(icdan,g2l)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   real(kind=8)   , intent(out) :: g2l(2,7)

   g2l(1:2,1:7) = 0.d0

   g2l(1,1) = this(icdan)%tuc(1)
   g2l(1,2) = this(icdan)%tuc(2) 
   g2l(1,3) = this(icdan)%Gcdt3 
   g2l(1,4) = - this(icdan)%cpcd * this(icdan)%tuc(1)
   g2l(1,5) = - this(icdan)%cpcd * this(icdan)%tuc(2) 
   g2l(1,6) = (this(icdan)%cpcd-1.d0) * this(icdan)%tuc(1)
   g2l(1,7) = (this(icdan)%cpcd-1.d0) * this(icdan)%tuc(2) 
   
   g2l(2,1) = this(icdan)%nuc(1)
   g2l(2,2) = this(icdan)%nuc(2) 
   g2l(2,3) = this(icdan)%Gcdn3 
   g2l(2,4) = - this(icdan)%cpcd * this(icdan)%nuc(1)
   g2l(2,5) = - this(icdan)%cpcd * this(icdan)%nuc(2) 
   g2l(2,6) = (this(icdan)%cpcd-1.d0) * this(icdan)%nuc(1)
   g2l(2,7) = (this(icdan)%cpcd-1.d0) * this(icdan)%nuc(2) 
   
 end subroutine get_g2l_DKALp

 subroutine clean_memory_DKALp
   implicit none
   integer(kind=4) :: i

   call clean_memory_inter_meca_()

   nb_DKALp  = 0
   nb_vDKALp = 0

   if( allocated(rough_DKALp) ) deallocate(rough_DKALp)

   nb_rough_DKALp = 0
   nstep_rough_seek_DKALp = 1
   nb_recup_DKALp = 0

   if( allocated(DK_coor) ) deallocate(DK_coor)

   nb_ALxxx = 0
   if( allocated(ALp) ) then
     do i = 1, size(ALp)
       if( associated(ALp(i)%coor) ) deallocate(ALp(i)%coor)
     end do
     deallocate(ALp)
   end if
   if( allocated(ALp_l) ) then
     do i = 1, size(ALp_l)
       if( associated(ALp_l(i)%n     ) ) deallocate(ALp_l(i)%n     )
       if( associated(ALp_l(i)%c     ) ) deallocate(ALp_l(i)%c     )
       if( associated(ALp_l(i)%radius) ) deallocate(ALp_l(i)%radius)
     end do
     deallocate(ALp_l)
   end if

   Reac_DKALp_MAX = 0.D0

   module_checked_ = .FALSE.
   check_DKALp_    = .FALSE.

 end subroutine

 subroutine set_nb_DKALp(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_DKALp = nb

 end subroutine

 subroutine redo_nb_adj_DKALp()
   implicit none

   call redo_nb_adj_( get_nb_DISKx() )

 end subroutine

END MODULE DKALp
