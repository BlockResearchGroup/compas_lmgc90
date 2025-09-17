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
MODULE PLALp

  !!****h* LMGC90.CORE/PLALp
  !! NAME
  !!  module PLALp
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors POLYG and ALpxx.
  !!  In this modulus candidate contactors are POLYG and antagonist 
  !!  contactors are ALPxx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/POLYG
  !!  LMGC90.CORE/MAILx
  !!  LMGC90.CORE/ALpxx
  !!****

  USE overall
  USE tact_behaviour
  use POLYG, only : T_POLYG, get_nb_polyg, polyg2bdyty  , &
                    get_l_polyg, get_radius_polyg       , &
                    move_bdary_polyg, print_info_polyg  , &
                    get_ent_polyg       => get_ent      , &
                    get_color_polyg     => get_color    , &
                    get_visible_polyg   => get_visible  , &
                    get_coorTT_polyg    => get_coorTT   , &
                    get_Vbegin_polyg    => get_Vbegin   , &
                    add_reac_polyg      => add_reac     , &
                    get_vlocy_polyg     => get_vlocy    , &
                    comp_vlocy_polyg    => comp_vlocy   , &
                    nullify_reac_polyg  => nullify_reac , &
                    nullify_vlocy_polyg => nullify_vlocy

  USE ALpxx

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use inter_meca_2D

  use parameters, only : i_plalp, i_polyg, i_alpxx, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

  logical :: bavard=.false.

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

 INTEGER         :: nb_PLALp                          ! nb_PLALp = number of selected candidate POLYG against POLYG
                                                      ! due to the fact that their might be 2 node_segment for each
                                                      ! entry in this it should be higher than size(this)
 INTEGER         :: nb_vPLALp                         ! nb_PLALp = number of selected candidates DISKx against JONCx
                                                      ! <= size(this).




 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdbdy): number of adjacent pairs body-contactor
                                                         ! to candidate body ibdycd.

!------------------------------------------------------------------------  

!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------ 

 real(kind=8)                                    :: Reac_PLALp_MAX=0.D0
 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------
 TYPE T_rough_PLALp                       ! définit le type de la liste des plus proches voisins
   INTEGER                   :: cd, &
                                ivert 
   REAL(kind=8),DIMENSION(2) :: point 

   INTEGER                   :: an, &     !les fontieres les plus proches
                                inode, &  !les noeuds de ces fontieres les plus proches 
                                isee      ! 

   REAL(kind=8)              :: adist     ! distance d'alerte
   REAL(kind=8),DIMENSION(2) :: n

 END TYPE T_rough_PLALp

 TYPE(T_rough_PLALp),DIMENSION(:),ALLOCATABLE   :: rough_PLALp        ! table  de visibilité

!------------------------------------------------------------------------
 TYPE T_link_rough_PLALp                   ! liste chainée pour déterminer les listes de cand anta car on ne 
                                           ! connait pas le nb de cand -ant à priori
   TYPE(T_link_rough_PLALp), POINTER :: p  ! pointeur sur le precedent

   TYPE(T_rough_PLALp) :: val              ! les valeurs
  
   TYPE(T_link_rough_PLALp ), POINTER :: n ! pointeur sur le suivant

 END TYPE T_link_rough_PLALp

 TYPE(T_link_rough_PLALp),POINTER               :: Root,Current,Previous
!------------------------------------------------------------------------

 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE        :: PL_coor          ! tableau (3,nb_POLYG) contenant les coordonnées 
                                                                    ! des centres au cours du calcul


 TYPE T_coor
    REAL(kind=8),DIMENSION(:,:),POINTER :: coor
 END TYPE T_coor

 TYPE(T_coor),DIMENSION(:),ALLOCATABLE  :: ALp      !  coordinates of body owning ALxxx to be used in selecting prox tactors

 TYPE T_l_ALp
    REAL(kind=8),DIMENSION(:,:),POINTER :: n        ! normal to the ALxxx 
    REAL(kind=8),DIMENSION(:,:),POINTER :: c        ! center of the ALxxx
    REAL(kind=8),DIMENSION(:)  ,POINTER :: radius                     
 END TYPE T_l_ALp

 TYPE(T_l_ALp),DIMENSION(:),ALLOCATABLE  :: ALp_l    !  local information of ALxxx to be used in selecting prox tactors

 INTEGER :: nb_ALxxx

!
 INTEGER        :: Nstep_rough_seek_PLALp=1
 LOGICAL        :: write_creation_tab_visu
 INTEGER        :: nb_rough_PLALp
 integer        :: nb_recup_PLALp
!------------------------------------------------------------------------
 LOGICAL :: RUN=.FALSE.
 logical :: module_checked_ = .FALSE.
 logical :: check_PLALp_    = .FALSE.

!------------------------------------------------------------------------
! liste des fonctions publiques
!
  PUBLIC &
       stock_rloc_PLALp, &
       recup_rloc_PLALp, &
       compute_box_PLALp, &
       read_ini_Vloc_Rloc_PLALp, &
       write_xxx_Vloc_Rloc_PLALp, &
       coor_prediction_PLALp, &
       creation_tab_visu_PLALp, &
       compute_contact_PLALp, &
       display_prox_tactors_PLALp, &
       RUN_PLALp, &
       CHECK_PLALp, &
       get_write_Vloc_Rloc_PLALp

  PUBLIC &
       nullify_reac_PLALp,&
       nullify_vlocy_PLALp,&
       injj_PLALp, prjj_PLALp, vitrad_PLALp, &
       get_nb_PLALp, &
       PLALp2POLYG, &
       print_info_PLALp, &
       get_length_PLALp, &
       get_icdtac_PLALp, &
       get_iantac_PLALp, &
       clean_memory_PLALp

 !rm for handler
 public get_this    , &
        set_nb_PLALp, &
        redo_nb_adj_PLALp, &
        get_an_tacty     , &
        get_verlet_tact_lawnb

CONTAINS

  include 'interaction_common.f90'
  ! defines the following subroutines
  !subroutine get_behaviour_( icdan, see, tact_behav )
  !subroutine get_this_(this_inter, verlet_inter, violation_inter)
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
  SUBROUTINE coor_prediction_PLALp

    IMPLICIT NONE
    
    INTEGER :: itact,errare,ialpxx
    INTEGER                   :: inod,ia,ib
    REAL(kind=8),DIMENSION(2) :: coor
    REAL(kind=8)              :: norm

    INTEGER :: nb_POLYG
    INTEGER :: nb_ALpxx

    nb_POLYG=get_nb_POLYG()
    nb_ALpxx=get_nb_ALpxx()
    
    DO itact=1,nb_POLYG
       PL_coor(1:3,itact) = get_coorTT_POLYG(itact)
       CALL move_BDARY_POLYG(itact,PL_coor(1:3,itact))
    END DO
    
    DO ialpxx=1,nb_ALpxx
       CALL get_coorTT_ALpxx(ialpxx,get_nb_node_ALpxx(ialpxx),ALp(ialpxx)%coor)
    END DO
    

    DO ialpxx=1,nb_ALpxx
       DO inod=1,get_nb_node_ALpxx(ialpxx)-1
          
          ia = l_ALpxx(ialpxx)%idata(inod)
          ib = l_ALpxx(ialpxx)%idata(inod+1)
          
          coor(:) = ALp(ialpxx)%coor(:,inod+1) - ALp(ialpxx)%coor(:,inod)
          norm=dsqrt(coor(1)**2 + coor(2)**2)

          ALp_l(ialpxx)%radius(inod) = norm*0.5
          
          ALp_l(ialpxx)%n(1,inod) = -coor(2)/norm
          ALp_l(ialpxx)%n(2,inod) =  coor(1)/norm
          
          ALp_l(ialpxx)%c(1,inod) = (ALp(ialpxx)%coor(1,inod) + ALp(ialpxx)%coor(1,inod+1))*0.5
          ALp_l(ialpxx)%c(2,inod) = (ALp(ialpxx)%coor(2,inod) + ALp(ialpxx)%coor(2,inod+1))*0.5
          
       END DO
    END DO


  END SUBROUTINE coor_prediction_PLALp
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PLALp(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PLALp
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PLALp(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PLALp
!!!---------------------------------------------
  SUBROUTINE compute_box_PLALp

    IMPLICIT NONE

    INTEGER  :: isee,errare,ibdy
    INTEGER  :: ialpxx,iplal
    INTEGER :: nb_POLYG
    INTEGER :: nb_ALpxx

                               !1234567890123456789012
    character(len=22) :: IAM = 'mod_PLALp::compute_box'

    nb_POLYG=get_nb_POLYG()
    nb_ALpxx=get_nb_ALpxx()

   ! on fait ici les choses qui ne doivent etre modifiees que 
   ! lorsque nb_POLYG change
   ! on dimensionne le adjac/rough sur le nombre de POLYG 

   IF (.NOT. ALLOCATED(adjac))THEN
     ALLOCATE(adjac(nb_POLYG),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO ibdy=1,nb_POLYG
       NULLIFY(adjac(ibdy)%icdan)
     END DO
   ELSE
     DO ibdy=1,nb_POLYG
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       NULLIFY(adjac(ibdy)%icdan)
     END DO
   ENDIF  
  
   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_POLYG),stat=errare)
   IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating nb_adj')
   END IF
  
   nb_adj=0

   ! PL_coor are coordinates of bodies owning POLYG to be used in selecting prox tactors
   
   IF (ALLOCATED(PL_coor)) DEALLOCATE(PL_coor)
   ALLOCATE(PL_coor(3,nb_POLYG),stat=errare)
   
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating PL_coor')
   END IF    

   ! ALp%coor are coordinates of bodies owning ALpxx to be used in selecting prox tactors
   ! for each ALpxx you may have several ALxxx, indeed you need 2 loops

   IF (ALLOCATED(ALp)) THEN
     DO ialpxx = 1,SIZE(Alp) 
       DEALLOCATE(Alp(ialpxx)%coor)
     ENDDO
     DEALLOCATE(ALp)
   ENDIF

   nb_ALpxx=get_nb_ALpxx()
   ALLOCATE(ALp(nb_ALpxx),stat=errare)

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating ALp')
   END IF    
 
   DO ialpxx=1,nb_ALpxx
     ALLOCATE(ALp(ialpxx)%coor(2,get_nb_node_ALpxx(ialpxx)),stat=errare)     
   ENDDO

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating ALp%coor')
   END IF    

   ! ALp_l%n,... are coordinates of normals owning ALpxx to be used in selecting prox tactors
   ! for each ALpxx you may have several normal (one for each ALxxx), indeed you need 2 loops

   IF (ALLOCATED(ALp_l)) THEN
     DO ialpxx = 1,SIZE(Alp_l) 
       DEALLOCATE(Alp_l(ialpxx)%n,Alp_l(ialpxx)%c,Alp_l(ialpxx)%radius)
     ENDDO
     DEALLOCATE(ALp_l)
   ENDIF

   nb_ALpxx=get_nb_ALpxx()
   ALLOCATE(ALp_l(nb_ALpxx),stat=errare)

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating ALp_l')
   END IF    
 
   DO ialpxx=1,nb_ALpxx
     ALLOCATE(ALp_l(ialpxx)%n(2,get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
     ALLOCATE(ALp_l(ialpxx)%c(2,get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
     ALLOCATE(ALp_l(ialpxx)%radius(get_nb_node_ALpxx(ialpxx)-1),stat=errare)     
   ENDDO

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating ALp_l%xxx')
   END IF    

   IF (ALLOCATED(rough_PLALp)) DEALLOCATE(rough_PLALp)
   ALLOCATE(rough_PLALp(nb_POLYG),stat=errare)   

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating rough_PLALp')
   END IF    

 END SUBROUTINE compute_box_PLALp
!------------------------------------------------------------------------ 
!--------------------------------------------------------------------------------------------------
 SUBROUTINE creation_tab_visu_PLALp
 
   IMPLICIT NONE 
 
   TYPE(T_POLYG)                         :: PGcd

   INTEGER                               :: errare 

   INTEGER                               :: icdtac,ivertex,iantac,inod,isee,i

   CHARACTER(len=5)                      :: cdcol,ancol,cdtac,antac,cdbdyty
   REAL(kind=8)                          :: adist,raycd,rayan,dist,pscal
   REAL(kind=8),DIMENSION(2)             :: coorcd,cooran
   INTEGER :: nb_POLYG
   INTEGER :: nb_ALpxx

   character(len=80) :: cout

    nb_ALpxx=get_nb_ALpxx()
    nb_POLYG=get_nb_POLYG()

   ! Detecting contacts; 
   ! contacts are being detected within a circle centered on an ALxxx
  
   ! first reading: sizing array this
  
   nb_rough_PLALp=0
  
   ! création de la liste de paire à examiner
  
   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

   NULLIFY(Root) 
   NULLIFY(Current)
   NULLIFY(Previous)

   DO iantac=1,nb_ALpxx
   DO inod=1,get_nb_node_ALpxx(iantac)-1

     ancol=get_color_MAILx(alpxx2bdyty(1,iantac),alpxx2bdyty(2,iantac))

     DO icdtac=1,nb_POLYG   

       IF (.NOT.get_visible_POLYG(icdtac)) CYCLE

       cdcol   = get_color_POLYG(icdtac)
       cdbdyty = get_body_model_name_from_id(polyg2bdyty(3,icdtac))

       isee=get_isee(cdbdyty,'POLYG',cdcol,'MAILx','ALpxx',ancol)

       ! if contactors are seeing each other
       IF (isee /= 0) THEN

         adist=see(isee)%alert 
         ! checking ROUGHLY distance against alert distance           
         coorcd(1:2) = PL_coor(1:2,icdtac)
         raycd = get_radius_POLYG(icdtac)
         cooran(1:2) = ALp_l(iantac)%c(1:2,inod)
         rayan = ALp_l(iantac)%radius(inod)

         dist= dsqrt((cooran(1)-coorcd(1))**2 + (cooran(2)-coorcd(2))**2)
         dist=dist - raycd - rayan - adist

         IF (dist < 0.d0) THEN
         ! ils se voient .... mais ou  
         ! recherche des vertex du POLYG concerne

           PGcd= get_l_POLYG(icdtac)

           DO ivertex=1,SIZE(PGcd%vertex,dim=2)

             pscal =  ALp_l(iantac)%n(1,inod)*(PGcd%vertex(1,ivertex) - coorcd(1)) + &
                      ALp_l(iantac)%n(2,inod)*(PGcd%vertex(2,ivertex) - coorcd(2))

             IF (pscal < 0.d0) THEN

!             print*,ivertex,inod
!             print*,PGcd%vertex(1,ivertex) - coorcd(1),PGcd%vertex(2,ivertex) - coorcd(2)
!             print*,ALp_l(iantac)%n(1,inod),ALp_l(iantac)%n(2,inod)
!             print*,ALp(iantac)%coor(1,inod+1),ALp(iantac)%coor(2,inod+1)
!             print*,ALp(iantac)%coor(1,inod),ALp(iantac)%coor(2,inod)


             ! faces en vis a vis

               dist= dsqrt((cooran(1)-PGcd%vertex(1,ivertex))**2 + &
                           (cooran(2)-PGcd%vertex(2,ivertex))**2)

               dist=dist - rayan - adist

               IF ( dist < 0.d0) THEN
               !
               ! c est dans la boite
               !
                 nb_rough_PLALp=nb_rough_PLALp+1
                 IF ( nb_rough_PLALp == 1) THEN
                   ALLOCATE(Root)
                   Current => Root
                   NULLIFY(Root%p)
                 ELSE 
                   ALLOCATE(Current)
                   Previous%n => Current
                 ENDIF
   
                 Current%val%cd          =icdtac
                 Current%val%ivert       =ivertex
                 Current%val%point(1:2)  =PGcd%vertex(1:2,ivertex)

                 Current%val%an          =iantac
                 Current%val%inode       =inod
                 Current%val%n(1:2)      = ALp_l(iantac)%n(1:2,inod)

                 Current%val%isee        =isee
                 Current%val%adist       =adist

                 Current%p => Previous
                 NULLIFY(Current%n)
                 Previous => Current
               ENDIF
             ENDIF
           ENDDO
         ENDIF
       ENDIF
     ENDDO
   ENDDO
   ENDDO

   WRITE(cout,'(4X,I10,A20)') nb_rough_PLALp,' PLALp roughly found'
   call logmes(cout)

   IF (ALLOCATED(rough_PLALp)) DEALLOCATE(rough_PLALp)
   ALLOCATE(rough_PLALp(nb_rough_PLALp))  ! on s'alloue la table de visibilité utilisée dans compute_contact
  
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(nb_rough_PLALp))         ! on s'alloue un tableau de contact.

   DO i=nb_rough_PLALp,1,-1
     
     Previous => Current%p
     rough_PLALp(i)%cd         = Current%val%cd
     rough_PLALp(i)%ivert      = Current%val%ivert
     rough_PLALp(i)%point(1:2) = Current%val%point(1:2)
     rough_PLALp(i)%an         = Current%val%an
     rough_PLALp(i)%inode      = Current%val%inode
     rough_PLALp(i)%n(1:2)     = Current%val%n(1:2)
     rough_PLALp(i)%isee       = Current%val%isee
     rough_PLALp(i)%adist      = Current%val%adist
     DEALLOCATE(Current)
     Current => Previous
   END DO 
   
   NULLIFY(Root)

 END SUBROUTINE creation_tab_visu_PLALp
!--------------------------------------------------------------------------------------------------
!--------------------------------------------------------------------------------------------------
 SUBROUTINE compute_contact_PLALp
 
   IMPLICIT NONE  

   INTEGER                               :: errare 
   INTEGER                               :: i,icdtac,icdver,iantac,ialxxx,isee, &
                                            icdan,iadj,ibdy
   REAL(kind=8)                          :: xp1,yp1,xp2,yp2,nx,ny,tx,ty,adist
   REAL(kind=8)                          :: xa,ya,xb,yb,AB,APAB,gap
   REAL(kind=8),DIMENSION(2)             :: cdlev,an_Vbegin
   REAL(kind=8),DIMENSION(3)             :: cd_Vbegin

   INTEGER                               :: cd_ent,an_ent
   INTEGER :: nb_POLYG

   character(len=80) :: cout

   icdan=0        
   iadj=0
   nb_PLALp=0
   nb_adj=0

   IF (nb_rough_PLALp /= 0 ) THEN

     DO i=1,nb_rough_PLALp

       icdtac   = rough_PLALp(i)%cd
       icdver   = rough_PLALp(i)%ivert
       xp1      = rough_PLALp(i)%point(1)
       yp1      = rough_PLALp(i)%point(2)

       iantac   = rough_PLALp(i)%an
       ialxxx   = rough_PLALp(i)%inode
       Nx       = rough_PLALp(i)%n(1)
       Ny       = rough_PLALp(i)%n(2)

       isee     = rough_PLALp(i)%isee  
       adist    = rough_PLALp(i)%adist

       IF (bavard) THEN

       PRINT*,'=========predetection==================='
       PRINT*,'Le candidat ',icdtac,'vertex ',icdver
       PRINT*,'qui est sur le RBDY2', polyg2bdyty(1,icdtac)
       PRINT*,'voit la ligne:',iantac, &
              'segment le plus proche:',ialxxx
       PRINT*,'qui est sur le MAILx:',l_alpxx(iantac)%ibdyty
       PRINT*,'=========fin predetection==============='

       ENDIF

       xa=ALp(iantac)%coor(1,ialxxx)
       ya=ALp(iantac)%coor(2,ialxxx) 

       xb=ALp(iantac)%coor(1,ialxxx+1)
       yb=ALp(iantac)%coor(2,ialxxx+1)

       AB = dsqrt((xa-xb)**2+(ya-yb)**2) 
  
       tx =  ny
       ty = -nx
       APAB = (((xp1-xa)*tx) + ((yp1-ya)*ty))/AB     

       xp2 = xa + (APAB*AB*tx)
       yp2 = ya + (APAB*AB*ty)

       gap = ((xp1-xa)*nx) + ((yp1-ya)*ny)

       IF (gap .LE. adist .AND. &
           (apab >= -1.D-5) .AND. ((apab - 1.d0) <= 1.d-5)) THEN


         IF (bavard) THEN
         PRINT*,'==================================================================='
         PRINT*,'candidat: ',icdtac,'corps: ',polyg2bdyty(1,icdtac)
         PRINT*,'antagoniste: ',iantac,'corps: ',l_ALpxx(iantac)%ibdyty
         PRINT*,'numnoda:',l_ALpxx(iantac)%idata(ialxxx)
         PRINT*,'numnodb:',l_ALpxx(iantac)%idata(ialxxx+1)
         PRINT*,'p1',xp1,yp1
         PRINT*,'pa',xa,ya
         PRINT*,'pb',xb,yb
         PRINT*,'p2',xp2,yp2
         PRINT*,'cpcd',apab,'gap',gap,'adist',adist
         PRINT*,'==================================================================='
         ENDIF
         icdan=icdan+1

         nb_adj(icdtac) = nb_adj(icdtac) + 1 
         iadj=nb_adj(icdtac)

         this(icdan)%icdbtac = polyg2bdyty(2, icdtac)
         this(icdan)%ianbtac = alpxx2bdyty(2, iantac)

         this(icdan)%icdbtyp = polyg2bdyty(3, icdtac)
         this(icdan)%ianbtyp = alpxx2bdyty(3, iantac)

         this(icdan)%icdctyp = i_polyg
         this(icdan)%ianctyp = i_alpxx

         this(icdan)%iadj    = iadj
         this(icdan)%icdbdy  = polyg2bdyty(1, icdtac)
         this(icdan)%icdtac  = icdtac
         this(icdan)%icdsci  = icdver

         this(icdan)%ianbdy  = l_alpxx(iantac)%ibdyty
         this(icdan)%iantac  = iantac
         this(icdan)%iansci  = ialxxx

         cd_ent = get_ent_POLYG(this(icdan)%icdtac)
         an_ent = get_ent_ALpxx(iantac) 

         this(icdan)%icdent = cd_ent
         this(icdan)%ianent = an_ent

         entity(cd_ent)%nb = entity(cd_ent)%nb+1
         entity(an_ent)%nb = entity(an_ent)%nb+1

         this(icdan)%isee       = isee    

         this(icdan)%nuc(:)     = (/ nx, ny /)  
         this(icdan)%tuc(:)     = (/ tx, ty /)

         this(icdan)%gapTTBegin = gap
         this(icdan)%cpcd       = apab

         this(icdan)%coor(1)    = (xp1+xp2) * 0.5
         this(icdan)%coor(2)    = (yp1+yp2) * 0.5
                    
         cdlev(1)               = xp1 - PL_coor(1, icdtac)
         cdlev(2)               = yp1 - PL_coor(2, icdtac)

         this(icdan)%Gcdt3      = -cdlev(2)*tx + cdlev(1)*ty
         this(icdan)%Gcdn3      = -cdlev(2)*nx + cdlev(1)*ny

         cd_Vbegin              = get_Vbegin_POLYG(icdtac)
         CALL get_vlocy_ALpxx(iantac,ialxxx,iVbeg_,an_Vbegin,apab)


         this(icdan)%vltBEGIN   =  ( cd_Vbegin(1)-an_Vbegin(1) ) * tx &
                                 + ( cd_Vbegin(2)-an_Vbegin(2) ) * ty &
                                 + cd_Vbegin(3)*this(icdan)%Gcdt3

         this(icdan)%vlnBEGIN   =  ( cd_Vbegin(1)-an_Vbegin(1) ) * nx &
                                 + ( cd_Vbegin(2)-an_Vbegin(2) ) * ny &
                                 + cd_Vbegin(3)*this(icdan)%Gcdn3

         this(icdan)%rlt        = 0.d0
         this(icdan)%rln        = 0.d0
         this(icdan)%vlt        = this(icdan)%vltBEGIN
         this(icdan)%vln        = this(icdan)%vlnBEGIN
         this(icdan)%gapTT      = this(icdan)%gapTTbegin
         this(icdan)%status     = i_nknow

       ENDIF
     ENDDO
    nb_PLALp=icdan
  ENDIF

  WRITE(cout,'(1X,I10,A12)') nb_PLALp,' PLALp found'
  call logmes(cout)

  nb_POLYG= get_nb_POLYG()

  ! look for double contacts
  ! (useful to compute contact length)
  do icdan = 1, nb_PLALp-1

    ! doublet already found
    if( this(icdan)%dct /= 0 ) cycle

    do iadj = icdan+1, nb_PLALp
      ! not same body... exit
      if( this(icdan)%icdbdy /= this(iadj)%icdbdy   .or. &
          this(icdan)%ianbdy /= this(iadj)%ianbdy   ) exit
      ! not same patch or polyg... exit
      if( this(icdan)%icdtac /= this(iadj)%icdtac   .or. &
          this(icdan)%iantac /= this(iadj)%iantac   ) exit
      ! not adjacent vertices of a polyg... next !
      if( this(icdan)%icdsci /= this(iadj)%icdsci+1 .or. &
          this(icdan)%icdsci /= this(iadj)%icdsci-1 ) cycle

      ! otherwise store adjacent
      this(icdan)%dct = 1; this(icdan)%icocdan = iadj
      this(iadj)%dct  = 1; this(iadj)%icocdan  = icdan

    end do

  end do


  DO ibdy=1,nb_POLYG

    IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
    IF (nb_adj(ibdy) /= 0) THEN
      ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
      IF (errare /=0 ) THEN
        write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
        call faterr('mod_PLALp::compute_contact',cout)
      END IF
    ENDIF
  ENDDO

  DO icdan=1,nb_PLALp
     adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
  END DO

  
   do icdan = 1, nb_PLALp
      call get_behaviour_( icdan, see, tact_behav )
   end do

  IF (ALLOCATED(violation)) DEALLOCATE(violation)
  ALLOCATE(violation(nb_PLALp),stat=errare)
   
END SUBROUTINE compute_contact_plalp
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 SUBROUTINE display_prox_tactors_PLALp

   IMPLICIT NONE
   INTEGER :: iadj,icdan,icdbdy,jbdycd,icdtac,ianbdy,iantac,isee,icdver,itact
   integer :: nb_polyg
   character(len=5) :: cdmodel, anmodel

   IF (nb_PLALp == 0) RETURN

   nb_POLYG= get_nb_POLYG()   
   DO itact=1,nb_POLYG    
     DO iadj=1,nb_adj(itact)         

       icdan  = adjac(itact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       icdver = this(icdan)%icdsci
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan
                       !123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
       WRITE(*,'(A90)')' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr  segmt  numbr'
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,12x)')   &
       cdmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',icdver, &
       see(this(icdan)%isee)%behav,  &
       anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac)
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 END SUBROUTINE display_prox_tactors_PLALp
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_PLALp
 
   !  
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE

   INTEGER                               :: errare 
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   integer :: nb_polyg

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PLALp::stock_rloc'

   nb_POLYG= get_nb_POLYG()   

   ! sizing verlt:
     IF (.NOT. ALLOCATED(verlt)) THEN
      ALLOCATE(verlt(nb_POLYG),stat=errare)
      IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
 
     DO icdtac=1,nb_POLYG
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
     DO icdtac=1,nb_POLYG
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
   DO icdan=1,nb_PLALp

     icdtac = this(icdan)%icdtac  ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac  ! serial number of antagonist contactor for contact icdan
     iadj   = this(icdan)%iadj    ! serial adjacent number of pair body-contactor
                                  ! adjacent to candidate body for contact icdan
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = polyg2bdyty(1,icdtac)
     verlt(icdtac)%cdtac           = polyg2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel         = polyg2bdyty(3,icdtac)
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
     verlt(icdtac)%nuc(1:2, iadj)  = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2, iadj) = this(icdan)%coor(1:2)
     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
   END DO

   nb_vPLALp = nb_PLALp

   WRITE(cout,'(1X,I10,A12)') nb_vPLALp,' stock PLALp'
   call logmes(cout)

 END SUBROUTINE stock_rloc_PLALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_PLALp

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE
   INTEGER :: icdan,icdtac,icdver,iantac,iadj
   CHARACTER(len=21)  :: IAM = 'mod_PLALp::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   IF (nb_PLALp == 0) RETURN  

   nb_recup_PLALp=0

   DO icdan=1,nb_PLALp
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     icdver = this(icdan)%icdsci 
     iantac = this(icdan)%iantac                 ! serial number of antagonist contactor for contact icdan        
     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdmodel== polyg2bdyty(3,icdtac) .and. &
           verlt(icdtac)%cdbdy  == polyg2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == polyg2bdyty(2,icdtac)       &
          ) then
          do iadj = 1, verlt(icdtac)%adjsz
            if (verlt(icdtac)%cdsci(iadj)  == icdver                .and. &
                verlt(icdtac)%anmodel(iadj)== alpxx2bdyty(3,iantac) .and. &
                verlt(icdtac)%anbdy(iadj)  == alpxx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == alpxx2bdyty(2,iantac)       &
               ) then
               this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H 
               this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H 
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)
               this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
               nb_recup_PLALp = nb_recup_PLALp + 1
               exit
            end if
          end do
       end if
     ENDIF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_PLALp,' recup PLALp'
   call logmes(cout)

 END SUBROUTINE recup_rloc_PLALp
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE

   INTEGER                           :: icdan,icdbdy,icdtac,icdver,ianbdy,iantac,ialxxx
   INTEGER                           :: iadj,icdtact,cdmodel,anmodel
   REAL(kind=8)                      :: rlt,rln,vlt,vln,gapTT
   REAL(kind=8),DIMENSION(2)         :: nuc,coor
   CHARACTER(len=5)                  :: cdbdy,cdtac,cdver,anbdy,antac,behav,sttus
   INTEGER                           :: errare 

   INTEGER :: ibehav,nb_internal,i_internal
   integer :: nb_polyg
  
   character(len=80) :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_PLALp                    '


   nb_POLYG=get_nb_POLYG()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contacts have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_POLYG),stat=errare)
     IF (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   END IF    

   nb_adj=0

   DO
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PLALp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,5X,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,ialxxx,        & ! no anseg and ianseg for PLALp contrary to PLPLx
                      sttus
     cdmodel = get_body_model_id_from_name( cdbdy )

     IF (cdtac /= 'POLYG' .OR. antac /= 'ALpxx') CYCLE
     do icdtact = 1, nb_POLYG
       if (polyg2bdyty(3,icdtact) == cdmodel .and. &
           polyg2bdyty(1,icdtact) == icdbdy  .and. &
           polyg2bdyty(2,icdtact) == icdtac  ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
     cycle
   END DO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_POLYG),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdbdy=1,nb_POLYG
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
     DO icdbdy=1,nb_POLYG
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
     IF (G_clin(9:13)/= 'PLALp') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT

     READ(G_clin(1:97),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,2X,5X,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,cdver,icdver,  &
                      behav,                                   &
                      anbdy,ianbdy,antac,iantac,ialxxx,        & !anseg,ianseg,  &
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )

     IF (cdtac /= 'POLYG' .AND. antac /= 'ALpxx') CYCLE
     do icdtact = 1, nb_POLYG
       if (polyg2bdyty(3,icdtact) == cdmodel .and. &
           polyg2bdyty(1,icdtact) == icdbdy  .and. &
           polyg2bdyty(2,icdtact) == icdtac  ) then

         icdan = icdan + 1

         nb_adj(icdtact) = nb_adj(icdtact) + 1

         verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

         verlt(icdtact)%cdmodel                 = cdmodel
         verlt(icdtact)%cdbdy                   = icdbdy
         verlt(icdtact)%cdtac                   = icdtac
         verlt(icdtact)%cdsci(nb_adj(icdtact))  = icdver
         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
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
     ENDDO
     CYCLE
   END DO

   nb_vPLALp=0

   DO icdtact=1,nb_POLYG
     nb_vPLALp = nb_vPLALp + nb_adj(icdtact)

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
   INTEGER :: iadj,icdan,icdbdy,icdtac,icdver,ianbdy,iantac,ialxxx,isee,nfich,icdtact
   INTEGER :: lc
   REAL(kind=8),DIMENSION(2) :: coor
   integer nb_polyg

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel

   IF (nb_PLALp==0) RETURN

   nb_POLYG=get_nb_POLYG()

   DO icdtact=1,nb_POLYG   

!     print*,'nb_adj(',icdtact,')= ',nb_adj(icdtact) 

     DO iadj=1,nb_adj(icdtact)         

!       print*,iadj,adjac(icdtact)%icdan(iadj)


       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       ialxxx = this(icdan)%iansci

       cdmodel = get_body_model_name_from_id( polyg2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( alpxx2bdyty(3,iantac) )

       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PLALp',icdan
       !1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890123
       ! RBDY2  12345  POLYG  12345  CDVER  12345  BEHAV  MAILx  12345  ALPxx  12345 IALxx          STTUS 12345
       WRITE(nfich,'(A103)') &
       ' cdbdy  numbr  cdtac  numbr  vertx  numbr  behav  anbdy  numbr  antac  numbr                sttus iadj '
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,I5,9X,A5,1X,I5)')   &
       cdmodel,polyg2bdyty(1,icdtac),'POLYG',polyg2bdyty(2,icdtac),'CDVER',this(icdan)%icdsci, &
       see(this(icdan)%isee)%behav,  &
       anmodel,alpxx2bdyty(1,iantac),'ALpxx',alpxx2bdyty(2,iantac),ialxxx, &
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
 SUBROUTINE nullify_reac_PLALp(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac
   INTEGER            :: storage
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_ALpxx(iantac,storage)

 END SUBROUTINE nullify_reac_PLALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_PLALp(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac
   INTEGER            :: storage
    
   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_POLYG(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_vlocy_ALpxx(iantac,storage)

 END SUBROUTINE nullify_vlocy_PLALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_PLALp(icdan,storage,need_full_vlocy)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac,ialxxx
   INTEGER            :: storage
   logical, optional  :: need_full_vlocy
    
  icdtac=this(icdan)%icdtac
  CALL comp_vlocy_POLYG(icdtac,storage)
    
   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci
   CALL comp_vlocy_ALpxx(iantac,ialxxx,storage,need_full_vlocy)
    
 END SUBROUTINE vitrad_PLALp
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE injj_PLALp(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RTIK,RNIK
   INTEGER,     DIMENSION(3)  :: cdccdof
   REAL(kind=8),DIMENSION(3)  :: cdreac
   REAL(kind=8),DIMENSION(2)  :: anreac
   INTEGER                    :: icdtac,iantac,ialxxx
   INTEGER                    :: storage
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci

   cdccdof(1)=1

   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)

   cdccdof(2)=2

   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      
   anreac(2)=-cdreac(2)

   cdccdof(3)=3
   cdreac(3)= this(icdan)%Gcdt3*RTIK+this(icdan)%Gcdn3*RNIK

   CALL add_reac_POLYG(icdtac,cdccdof,cdreac,storage)

   CALL add_reac_ALpxx(iantac,ialxxx,anreac(1:2),this(icdan)%cpcd,storage)

 END SUBROUTINE injj_PLALp 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_PLALp(icdan,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VTIK,VNIK
   REAL(kind=8),DIMENSION(3) :: Vcd,Van

   integer(kind=4)             :: icdtac,iantac,ialxxx
   integer(kind=4), intent(in) :: storage
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac
   ialxxx=this(icdan)%iansci
   
   CALL get_vlocy_POLYG(icdtac,storage,Vcd)

   CALL get_vlocy_ALpxx(iantac,ialxxx,storage,Van,this(icdan)%cpcd)   

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3 &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)
   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)

 END SUBROUTINE prjj_PLALp 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_nb_PLALp(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_PLALp = nb_PLALp
   case(i_verlet_tactor)
      get_nb_PLALp = nb_vPLALp
   case(i_rough_tactor)
      get_nb_PLALp = nb_rough_PLALp
   case(i_recup_tactor)
      get_nb_PLALp = nb_recup_PLALp
   end select

 end function get_nb_PLALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE PLALp2POLYG(icdan,icdtac)

   IMPLICIT NONE
   INTEGER          :: icdan,icdtac
   
   icdtac = this(icdan)%icdtac

 END SUBROUTINE PLALp2POLYG
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE PLALp2ALpxx(icdan,iantac)

   IMPLICIT NONE
   INTEGER          :: icdan,iantac
   
   iantac = this(icdan)%iantac

 END SUBROUTINE PLALp2ALpxx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------------------------------
 SUBROUTINE get_numcorps_PLALp(icdan,icdbdy,ianbdy)

   IMPLICIT NONE

   INTEGER          :: icdan,icdbdy,ianbdy

   icdbdy   = this(icdan)%icdbdy
   ianbdy   = this(icdan)%ianbdy

 END SUBROUTINE get_numcorps_PLALp
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE print_info_PLALp(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,ianal,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac
   ianal=this(icdan)%iansci

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'POLYG:',1x,I5,1x,'ALpxx:',1x,I5,1x,'segment:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   CALL print_info_POLYG(icdbdy)
!   call print_info_ALpxx(ianbdy)

END SUBROUTINE print_info_PLALp
!------------------------------------------------------------------------
real(kind=8) function get_length_PLALp(icdan)
   ! fd 
   ! compute the length of a PLALp contact
   ! 2 points -> distance between the 2 points /2
   ! 1 point  -> radius (idem spheres)
   implicit none
   !
   integer(kind=4), intent(in) :: icdan 
   !
   integer(kind=4)            :: icdtac
   real(kind=8), dimension(2) :: bord

   if (this(icdan)%dct /= 0) then

     bord = this(icdan)%coor - this(this(icdan)%icocdan)%coor

     get_length_PLALp = 0.5 * dsqrt(dot_product(bord,bord))

   else

     get_length_PLALp = get_radius_POLYG(this(icdan)%icdtac)

   endif
  
end function get_length_PLALp
!------------------------------------------------------------------------ 
LOGICAL FUNCTION RUN_PLALp(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_PLALp = RUN_TACTOR

END FUNCTION RUN_PLALp
!------------------------------------------------------------------------
  logical function CHECK_PLALp()
    implicit none
    !   
    integer :: isee, nb_POLYG, nb_ALpxx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PLALp = check_PLALp_
      return
    end if

    con_pedigree%module_name = 'PLALp'

    con_pedigree%id_cdan  = i_plalp
    con_pedigree%id_cdtac = i_polyg
    con_pedigree%id_antac = i_alpxx

    cdtact2bdyty => polyg2bdyty
    antact2bdyty => alpxx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_ALpxx = get_nb_ALpxx()
    nb_POLYG = get_nb_POLYG()
    if( nb_ALpxx == 0 .or. nb_POLYG == 0 ) then
      CHECK_PLALp = check_PLALp_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'POLYG' .and. see(isee)%antac == 'ALpxx') then
        check_PLALp_ = .true.
        exit
      end if
    end do
  
    CHECK_PLALp = check_PLALp_
    return
  
  end function CHECK_PLALp
!------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_Vloc_Rloc_PLALp(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_PLALp = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_PLALp

 function get_icdtac_PLALp(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_PLALp
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
         get_icdtac_PLALp = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLALp::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_PLALp(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_PLALp
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
         get_iantac_PLALp =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PLALp::get_icdtac','unknown contact index')
   

   get_iantac_PLALp = this(icdan)%iantac

 end function

 subroutine clean_memory_PLALp
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_PLALp  = 0
   nb_vPLALp = 0

   nb_rough_PLALp = 0
   nstep_rough_seek_PLALp = 1
   nb_recup_PLALp = 0

   RUN = .false.

   if( allocated(PL_coor) ) deallocate(PL_coor)
   if( allocated(ALp)    ) then
     do i =1, size(ALp)
       if( associated(ALp(i)%coor)   ) deallocate(ALp(i)%coor)
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

   Reac_PLALp_MAX = 0.D0

   module_checked_ = .FALSE.
   check_PLALp_    = .FALSE.

 end subroutine
 
 subroutine set_nb_PLALp(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PLALp = nb

 end subroutine

 subroutine redo_nb_adj_PLALp()
   implicit none

   call redo_nb_adj_( get_nb_POLYG() )

 end subroutine

END MODULE PLALp
