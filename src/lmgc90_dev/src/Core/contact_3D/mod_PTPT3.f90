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
 MODULE PTPT3                                         

  !!****h* LMGC90.CORE/PTPT3
  !! NAME
  !!  module PTPT3
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors PT3Dx
  !!  In this modulus candidate contactors are PT3Dx and antagonist 
  !!  contactors are PT3Dx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/PT3Dx
  !!****
                                                      
 USE overall
 USE tact_behaviour
 USE PT3Dx

 use MAILx, only : get_color_MAILx
 use RBDY3, only : get_color_RBDY3 => get_color
 use MBS3D, only : get_color_MBS3D => get_color

 use inter_meca_3D

 use parameters, only : i_ptpt3, i_mailx, i_rbdy3, i_mbs3

!$ use timer

 implicit none

 private

 CHARACTER(len=5) :: BOBO='PTPT3'
 INTEGER          :: nb_PT3Dx

 type(T_interaction), dimension(:), allocatable, target :: this

 !fd < a merger
  
 type(T_con),target :: con_pedigree 

 integer, dimension(:,:), pointer :: cdtact2bdyty => null()
 integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 INTEGER :: nb_PTPT3,nb_vPTPT3,nb_recup_PTPT3                                  ! nb_PTPT3 = number of selected candidates POINT against POINT
                                                      ! <= size(this).
!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs POINT-POINT
                                                         !                  to candidate contactor POINT icdtac.

!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target ::verlt

!------------------------------------------------------------------------
 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                                                      ! performed within a box and immediate surrounding boxes, see
                                                      ! subroutine enumerate_PTPT3.
                        
   INTEGER                               :: popul     ! box(ibox1,ibox2)%popul: number of PT3Dx in box ibox1,ibox2;
   
   INTEGER, DIMENSION(:), POINTER        :: which     ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of PT3Dx labelled ipopul
                        ! in box ibox1,ibox2;
   
 END TYPE T_box 

 TYPE(T_box), DIMENSION(:,:),ALLOCATABLE    :: box    ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

 TYPE T_rough_PTPT3  
                                                                        ! definit le type de la liste des plus proches voisins
    INTEGER :: cd                                                       ! le candidat, l'antagoniste et isee pour la loi de contact
    INTEGER :: an
    INTEGER :: isee
! pb
    INTEGER :: xperiodic,yperiodic
    LOGICAL :: loop
! pb
    REAL(kind=8) :: nonuc0

    ! nard
    REAL(kind=8) :: surf
    REAL(kind=8) :: l0
    REAL(kind=8) :: mass


 END TYPE T_rough_PTPT3
 
 TYPE(T_rough_PTPT3),DIMENSION(:),ALLOCATABLE   :: rough_PTPT3          ! table  de visibilite

 TYPE T_link_rough_PTPT3                                                ! liste chainee pour determiner les listes de cand anta car
                                                                        ! on ne connait pas le nb de cand -ant a priori
    TYPE(T_link_rough_PTPT3), POINTER :: p                              ! pointeur sur le precedent
    TYPE(T_rough_PTPT3)               :: val                            ! les valeurs
    TYPE(T_link_rough_PTPT3), POINTER :: n                              ! pointeur sur le suivant

 END TYPE T_link_rough_PTPT3

 TYPE(T_link_rough_PTPT3),POINTER     :: Root,Current,Previous
 
 INTEGER,PRIVATE   :: ii,l_ii,iv
 INTEGER,PRIVATE   :: Nstep_creation_tab_visu=1,restart=0
 LOGICAL,PRIVATE   :: write_creation_tab_visu
 
!------------------------------------------------------------------------
! variables pour le calcul des boites englobantes

 REAL (kind=8)  :: maxray, minray, maxalert, meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: nb_rough_PTPT3
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul
!------------------------------------------------------------------------

 REAL(kind=8) :: Reac_PTPT3_MAX=0.D0
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PTcoor  !  coordinates of bodies owning POINT to be used in selecting prox tactors
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: PTcoor0 !  reference coordinates of bodies owning POINT

 real(kind=8), dimension(:) ,allocatable, target :: violation
!------------------------------------------------------------------------

!pb
!!!---------------------------------------------------------
  LOGICAL      :: XPERIODIC=.FALSE.,YPERIODIC=.FALSE.
  REAL(KIND=8) :: XPERIODE = 0.D0,YPERIODE = 0.D0
!!!---------------------------------------------------------

 LOGICAL :: RUN=.FALSE.
 logical :: module_checked_ = .FALSE.
 logical :: check_PTPT3_    = .FALSE.

!------------------------------------------------------------------------
!variables pour le display
 REAL(kind=8)        :: max_nonuc0=0.D0
!------------------------------------------------------------------------
  LOGICAL :: given_network  = .FALSE.
  logical :: current_nonuc0 = .false.


!--- pour pouvoir ajouter des ptpt3 depuis l'exterieur
! -- utilise par mecacell
   
  INTEGER :: nb_extra=0


  !nard
   LOGICAL :: given_params = .FALSE.  
   LOGICAL :: explicit_local_frame = .FALSE.
   LOGICAL :: init_local_frame = .TRUE.
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: local_frame_n
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: local_frame_t
   REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: local_frame_s

  
  PUBLIC &
       coor_prediction_PTPT3,&
       CHECK_PTPT3,&
       RUN_PTPT3, &
       get_write_Vloc_Rloc_PTPT3, &
       read_ini_Vloc_Rloc_PTPT3,&
       write_xxx_Vloc_Rloc_PTPT3,&
       stock_rloc_PTPT3, &
       recup_rloc_PTPT3, &
       smooth_computation_PTPT3, &
       compute_box_PTPT3, &
       compute_contact_PTPT3, &
       display_prox_tactors_PTPT3,&
       get_nb_PTPT3, &
       load_network_PTPT3, &
       set_xperiodic_data_PTPT3, &
       set_yperiodic_data_PTPT3, &
       get_xperiode_PTPT3    , &
       get_yperiode_PTPT3    , &
       get_surf_PTPT3        , &
       set_explicit_local_frame_PTPT3, &
       load_params_ptpt3             , &
       use_current_nonuc0_PTPT3

! liste des fonctions publiques 

 PUBLIC &
      nullify_reac_PTPT3, nullify_vlocy_PTPT3,&
      injj_PTPT3, prjj_PTPT3, vitrad_PTPT3, & 
      get_max_radius_PTPT3, &
      PTPT32ENT, PTPT32PT3Dx, &
      set_nb_extra_ptpt3, &
      clean_memory_ptpt3

  !rm for handler
  public get_this    , &
         set_nb_PTPT3, &
         redo_nb_adj_PTPT3, &
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
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PTPT3(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PTPT3
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PTPT3(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PTPT3
!!!------------------------------------------------------------------------
   SUBROUTINE load_network_PTPT3()
     IMPLICIT NONE

     given_network = .TRUE.

   END SUBROUTINE load_network_PTPT3
!!!------------------------------------------------------------------------
  SUBROUTINE set_xperiodic_data_PTPT3(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    xperiode  = per
    XPERIODIC = FLAG
    
  END SUBROUTINE set_xperiodic_data_PTPT3
!!!------------------------------------------------------------------------
  SUBROUTINE set_yperiodic_data_PTPT3(per,FLAG)
    
    IMPLICIT NONE
    
    REAL(kind=8) :: per
    LOGICAL      :: FLAG
    
    yperiode  = per
    YPERIODIC = FLAG
    
  END SUBROUTINE set_yperiodic_data_PTPT3
!!!------------------------------------------------------------------------
   SUBROUTINE compute_box_PTPT3

     IMPLICIT NONE
     
     INTEGER                          :: isee,errare,ibdy,itac,itact
     INTEGER                          :: icdan,icdtac,iantac,iadj
     CHARACTER(len=5)                 :: cdcol,ancol
     REAL(kind=8)                     :: adist,nonuc0
     REAL(kind=8),DIMENSION(3)        :: coor0cd,coor0an
     INTEGER                          :: nb_PT3Dx

     INTEGER :: xper,yper,iloop
     INTEGER :: given_fich 
     CHARACTER(len=5) :: law 
     LOGICAL :: visible

                                !1234567890123456789012
     character(len=22) :: IAM = 'mod_PTPT3::compute_box'


     ! nard
     real(kind=8) :: surf,mass
     
     nb_PT3Dx=get_nb_PT3Dx()

     IF (.NOT. ALLOCATED(adjac))THEN
       ALLOCATE(adjac(nb_PT3Dx),stat=errare)
       IF (errare /=0 ) THEN
         call faterr(IAM,'Error allocating adjac')
       END IF
       DO ibdy=1,nb_PT3Dx
         NULLIFY(adjac(ibdy)%icdan)
       ENDDO
     ELSE
       DO ibdy=1,nb_PT3Dx
         IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
         NULLIFY(adjac(ibdy)%icdan)
       ENDDO
     ENDIF  

     IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
     ALLOCATE(nb_adj(nb_PT3Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating nb_adj')
     END IF

    nb_adj=0

   IF (ALLOCATED(PTcoor)) DEALLOCATE(PTcoor)
   ALLOCATE(PTcoor(3,nb_PT3Dx),stat=errare)
   IF (ALLOCATED(PTcoor0)) DEALLOCATE(PTcoor0)
   ALLOCATE(PTcoor0(3,nb_PT3Dx),stat=errare)


   ! a voir si merde des le debut

   DO itact=1,nb_PT3Dx
      PTcoor0(1:3,itact) = get_cooref_PT3Dx(pt3dx2bdyty(1,itact),pt3dx2bdyty(2,itact))

      ! cooref pas bon avec perio ; on corrige ici et pas dans rbdy3
   
      if (XPERIODIC .and. (PTcoor0(1,itact) /= modulo(PTcoor0(1,itact),xperiode))) call faterr(IAM,'PT3Dx out of the X periodic domain')
   
      if (YPERIODIC .and. (PTcoor0(2,itact) /= modulo(PTcoor0(2,itact),yperiode))) call faterr(IAM,'PT3Dx out of the Y periodic domain')
      
   END DO


   !fd dans le cas des PTPT3 il n'y a rien de dynamique ...
   !fd ce sont des liaisons cinematiques lineaires ou non lineaires

   nb_rough_PTPT3=0

   ! creation de la liste de paire a examiner
  
   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   NULLIFY(Root)
   NULLIFY(Current)
   NULLIFY(Previous)

   IF (given_network) THEN

     CALL logmes('----')
     CALL logmes('load ptpt3 network from file')
     IF (given_params) CALL LOGMES('LOAD PTPT3 PARAMS FROM FILE')     

     given_fich=get_io_unit()
     OPEN(unit=given_fich,file=trim(location('DATBOX/PTPT3_NETWORK.DAT')),status='OLD')
     DO 

       IF (given_params) THEN
         READ(given_fich,*,END=99) icdtac,iantac,xper,yper,iloop,surf,nonuc0,mass
       ELSE    
         READ(given_fich,*,END=99) icdtac,iantac,xper,yper,iloop,nonuc0
       ENDIF

       cdcol=get_color_PT3Dx(icdtac)
       ancol=get_color_PT3Dx(iantac)
       isee=get_isee('RBDY3','PT3Dx',cdcol,'RBDY3','PT3Dx',ancol)

       nb_rough_PTPT3 = nb_rough_PTPT3 + 1
       IF (nb_rough_PTPT3 == 1) THEN
         ALLOCATE(Root)
         Current => Root
         NULLIFY(Root%p)
       ELSE
         ALLOCATE(Current)
         Previous%n => Current
       ENDIF  

       Current%val%cd       =icdtac  
       Current%val%an       =iantac
       Current%val%isee     = isee   

       IF (given_params) THEN
          
          Current%val%surf   = surf
          Current%val%l0     = nonuc0
          Current%val%mass   = mass
          
       END IF

       !pb
       Current%val%xperiodic = xper
       Current%val%yperiodic = yper
       IF (iloop.EQ.1) THEN
         Current%val%loop = .TRUE.
       ELSE
         Current%val%loop = .FALSE.
       ENDIF
       !pb
       
       if( .not. current_nonuc0 ) then
         coor0cd = PTcoor0(1:3,icdtac)
         coor0an = PTcoor0(1:3,iantac)
         nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)
       end if

       ! coor0cd = PTcoor0(1:3,icdtac)
       ! coor0an = PTcoor0(1:3,iantac)
       ! nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)

       Current%val%nonuc0 = nonuc0                  
       Current%p => Previous
       NULLIFY(Current%n)
       Previous => Current

     ENDDO
 99  CLOSE(given_fich)
     
   ELSE

      ! loop investigating candidate PT3Dx
      DO icdtac=1,nb_PT3Dx
         !pta
         visible=get_visible_PT3Dx(icdtac) 
         IF (.NOT.visible) CYCLE
         !pta
         cdcol=get_color_PT3Dx(icdtac)

         ! loop investigating antagonist POINT
         DO iantac=icdtac+1,nb_PT3Dx
         
            IF ( is_PT3Dx_same_RBDY3(icdtac,iantac) ) CYCLE
            
            !pta
            visible=get_visible_PT3Dx(iantac)
            IF (.NOT.visible) CYCLE
            !pta
            ancol=get_color_PT3Dx(iantac)
            isee=get_isee('RBDY3','PT3Dx',cdcol,'RBDY3','PT3Dx',ancol)

            IF (isee /= 0) THEN

               adist=see(isee)%alert 
               if( .not. current_nonuc0 ) then
                 coor0cd = PTcoor0(1:3,icdtac)
                 coor0an = PTcoor0(1:3,iantac)
               else
                 coor0cd = PTcoor(1:3,icdtac)
                 coor0an = PTcoor(1:3,iantac)
               end if
               nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)

               IF (nonuc0 > max_nonuc0) max_nonuc0=nonuc0

               !fd
               !fd   les deux PT3Dx se voient de maniere "statique" 
               !fd
               !
               IF (nonuc0 .LE. adist) THEN

                  nb_rough_PTPT3=nb_rough_PTPT3+1
                  !
                  IF (nb_rough_PTPT3 == 1) THEN
                     ALLOCATE(Root)
                     Current => Root
                     NULLIFY(Root%p)
                  ELSE
                     ALLOCATE(Current)
                     Previous%n => Current
                  ENDIF
                  Current%val%cd        = icdtac  
                  Current%val%an        = iantac
                  Current%val%isee      = isee
                  Current%val%nonuc0    = nonuc0
                  Current%val%xperiodic = 0
                  Current%val%yperiodic = 0
                  Current%val%loop      = .FALSE.                 
                  Current%p => Previous
                  NULLIFY(Current%n)
                  Previous => Current
               END IF
               !
               !! X PERIODIC CASE
               IF(XPERIODIC) THEN
                  coor0cd = PTcoor0(1:3,icdtac)
                  coor0an = PTcoor0(1:3,iantac)
                  coor0an(1) = coor0an(1) + xperiode
                  
                  nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)

                  IF (nonuc0 > max_nonuc0) max_nonuc0 = nonuc0
                  IF (nonuc0 .LE. adist) THEN
                     nb_rough_PTPT3 = nb_rough_PTPT3 + 1
         !
                     IF (nb_rough_PTPT3 == 1) THEN
                        ALLOCATE(Root)
                        Current => Root
                        NULLIFY(Root%p)
                     ELSE
                        ALLOCATE(Current)
                        Previous%n => Current
                     ENDIF
                     Current%val%cd        = icdtac
                     Current%val%an        = iantac
                     Current%val%isee      = isee
                     Current%val%nonuc0    = nonuc0
                     Current%val%xperiodic = 1
                     Current%val%yperiodic = 0
                     Current%val%loop      = .TRUE.
                     Current%p => Previous
                     NULLIFY(Current%n)
                     Previous => Current
                  END IF
               END IF
               !!
               !! Cas YPERIODIC
               !!
               IF(YPERIODIC) THEN
                  coor0cd = PTcoor0(1:3,icdtac)
                  coor0an = PTcoor0(1:3,iantac)
                  coor0an(2) = coor0an(2) + yperiode
                  
                  nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)

                  IF (nonuc0 > max_nonuc0) max_nonuc0=nonuc0
                  IF (nonuc0 .LE. adist) THEN
                     nb_rough_PTPT3=nb_rough_PTPT3+1
         !
                     IF (nb_rough_PTPT3 == 1) THEN
                        ALLOCATE(Root)
                        Current => Root
                        NULLIFY(Root%p)
                     ELSE
                        ALLOCATE(Current)
                        Previous%n => Current
                     ENDIF
                     Current%val%cd        = icdtac
                     Current%val%an        = iantac
                     Current%val%isee      = isee
                     Current%val%nonuc0    = nonuc0
                     Current%val%xperiodic = 0
                     Current%val%yperiodic = 1
                     Current%val%loop      = .TRUE.
                     Current%p => Previous
                     NULLIFY(Current%n)
                     Previous => Current
                  END IF
               END IF
               !!
               !! Cas XPERIODIC et YPERIODIC
               !!
               IF(XPERIODIC .AND. YPERIODIC) THEN
                  coor0cd = PTcoor0(1:3,icdtac)
                  coor0an = PTcoor0(1:3,iantac)
                  coor0an(1) = coor0an(1) + xperiode
                  coor0an(2) = coor0an(2) + yperiode
                  nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2+(coor0cd(3)-coor0an(3))**2)

                  IF (nonuc0 > max_nonuc0) max_nonuc0=nonuc0
                  IF (nonuc0 .LE. adist) THEN
                     nb_rough_PTPT3=nb_rough_PTPT3+1
                     !
                     IF (nb_rough_PTPT3 == 1) THEN
                        ALLOCATE(Root)
                        Current => Root
                        NULLIFY(Root%p)
                     ELSE
                        ALLOCATE(Current)
                        Previous%n => Current
                     ENDIF
                     Current%val%cd        = icdtac
                     Current%val%an        = iantac
                     Current%val%isee      = isee
                     Current%val%nonuc0    = nonuc0
                     Current%val%xperiodic = 1
                     Current%val%yperiodic = 1
                     Current%val%loop      = .TRUE.
                     Current%p => Previous
                     NULLIFY(Current%n)
                     Previous => Current
                  END IF
               END IF
            ENDIF
         END DO
      END DO
      
   ENDIF

   IF (ALLOCATED(rough_PTPT3)) DEALLOCATE(rough_PTPT3)
   ALLOCATE(rough_PTPT3(nb_rough_PTPT3 + nb_extra))      ! on s'alloue la table de visibilite utilisee dans compute_contact
  
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(nb_rough_PTPT3 + nb_extra))             ! on s'alloue le tableau this surdimensionne a la taille temporaire 

   DO icdan=nb_rough_PTPT3,1,-1
      
     Previous => Current%p

     rough_PTPT3(icdan)%cd        = Current%val%cd
     rough_PTPT3(icdan)%an        = Current%val%an
     rough_PTPT3(icdan)%isee      = Current%val%isee
     rough_PTPT3(icdan)%nonuc0    = Current%val%nonuc0
     rough_PTPT3(icdan)%xperiodic = Current%val%xperiodic
     rough_PTPT3(icdan)%yperiodic = Current%val%yperiodic
     rough_PTPT3(icdan)%loop      = Current%val%loop

     IF (given_params) THEN
        rough_PTPT3(icdan)%surf = Current%val%surf
        rough_PTPT3(icdan)%l0   = Current%val%l0
        rough_PTPT3(icdan)%mass = Current%val%mass
     END IF

     
     DEALLOCATE(Current)
     Current => Previous
   END DO 
   
   NULLIFY(Root)

 END SUBROUTINE compute_box_PTPT3
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 SUBROUTINE set_nb_extra_ptpt3(nb)
   IMPLICIT NONE
   INTEGER :: nb

   nb_extra = nb  

 END SUBROUTINE
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE add_extra_rough_ptpt3(cd,an,isee,nonuc0)
   IMPLICIT NONE
   INTEGER :: cd,an,isee
   REAL(kind=8) :: nonuc0

   nb_rough_PTPT3 = nb_rough_PTPT3 +1

   rough_PTPT3(nb_rough_PTPT3)%cd     = cd
   rough_PTPT3(nb_rough_PTPT3)%an     = an
   rough_PTPT3(nb_rough_PTPT3)%isee   = isee
   rough_PTPT3(nb_rough_PTPT3)%nonuc0 = nonuc0

 END SUBROUTINE
!!!------------------------------------------------------------------------ 
!!!------------------------------------------------------------------------ 
 SUBROUTINE add_extra_ptpt3(icdtac,iantac,isee,nonuc0,nonuc)

   IMPLICIT NONE

   INTEGER :: icdbdy,ianbdy,icdtac,iantac,isee,icdan
   INTEGER :: iadj,cd_ent,an_ent,k

   REAL(kind=8)               :: nonuc0,nonuc
   REAL(kind=8),DIMENSION(3)  :: coordcd,coordan
   REAL(kind=8)               :: gapTT,fortuc,normtuc
   REAL(kind=8),DIMENSION(6)  :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3)  :: t,n,s,cdlev,anlev,cd_shift,an_shift
   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an

   character(len=80) :: cout
                              !12345678901234567890
   character(len=20) :: IAM = 'mod_CDCDx::add_extra'

   nb_PTPT3 = nb_PTPT3 + 1
   icdan    = nb_PTPT3

   nb_adj(icdtac) = nb_adj(icdtac) + 1
   iadj           = nb_adj(icdtac)

   this(icdan)%icdbtac = pt3dx2bdyty(2, icdtac)
   this(icdan)%ianbtac = pt3dx2bdyty(2, iantac)

   this(icdan)%icdbtyp = pt3dx2bdyty(3, icdtac)
   this(icdan)%ianbtyp = pt3dx2bdyty(3, iantac)

   this(icdan)%icdctyp = i_pt3dx
   this(icdan)%ianctyp = i_pt3dx

   this(icdan)%iadj    = iadj
   this(icdan)%icdbdy  = pt3dx2bdyty(1, icdtac)
   this(icdan)%icdtac  = icdtac
   this(icdan)%ianbdy  = pt3dx2bdyty(1, iantac)
   this(icdan)%iantac  = iantac
   this(icdan)%isee    = isee

   call get_behaviour_( icdan, see, tact_behav )

   CALL PTPT32ENT(icdan,cd_ent,an_ent)

   entity(cd_ent)%nb = entity(cd_ent)%nb + 1
   entity(an_ent)%nb = entity(an_ent)%nb + 1

   IF ( nonuc0 .LT. 1.d-20) THEN
     IF (nonuc .GE. 1e-10) THEN
       write(cout,'(A)')                  'On a 2 noeuds decolles dans une situation de noeuds colles'
       write(cout,'(A,I0,1x,I0)')         'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
       write(cout,'(A,1x,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
       call faterr(IAM,cout)
     ENDIF

     !fd cas de noeuds colles 

     this(icdan)%tuc(:)     =  (/ 1.d0, 0.d0 , 0.d0/)
     this(icdan)%nuc(:)     =  (/ 0.d0, 1.d0 , 0.d0/)
     this(icdan)%suc(:)     =  (/ 0.d0, 0.d0 , 1.d0/)
     this(icdan)%gapTTbegin =  0.d0
     this(icdan)%nonuc0     =  0.d0
     this(icdan)%internal   =  0.d0
 
     !fd Pour les lois sachant quoi faire on laisse faire: 
     !fd COUPLED_DOF        
     !fd TEX_SOL
                                                   !123456789012345678901234567890
     IF (tact_behav(this(icdan)%lawnb)%lawty /= 'COUPLED_DOF                   ' .AND. &
         tact_behav(this(icdan)%lawnb)%lawty /= 'TEX_SOL                       ' &
        ) THEN
        write(cout,'(A)')                  'On a 2 noeuds colles mais la loi d''interaction ne sait pas le traiter'
        write(cout,'(A,I0,1x,I0)')         'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
        write(cout,'(A,1x,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
        call faterr(IAM,cout)
     ENDIF

   ELSE

     IF (nonuc .LT. 1d-20) THEN
       write(cout,'(A)')                  'On a 2 noeuds  colles dans une situation de noeuds decolles'
       write(cout,'(A,I0,1x,I0)')         'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
       write(cout,'(A,1x,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
       call faterr(IAM,cout)
     ENDIF 

     this(icdan)%nuc(:) = (coordcd(1:3)-coordan(1:3))/nonuc
     
     this(icdan)%coor(1:3) = (coordcd(1:3) + coordan(1:3))*0.5

     ! Dans le cas general, on sait que l'antagoniste fait partie du plan tangent donc on prend un point M de ce plan
     ! parametre : Ym = Yan +1 et Zm = Zan +1 pour obtenir le vecteur t. Si n(1)=0, on parametre Xm et Ym

     !Cas ou la normale est alignee sur un axe

     DO k=1,3
       IF (dabs(this(icdan)%nuc(k)) .LT. 1.D-18) this(icdan)%nuc(k) = 0.D0
     ENDDO         

     IF (( this(icdan)%nuc(1)*this(icdan)%nuc(2) == 0.D0 ).AND. &
         ( this(icdan)%nuc(2)*this(icdan)%nuc(3) == 0.D0 ).AND. &
         ( this(icdan)%nuc(3)*this(icdan)%nuc(1) == 0.D0 )) THEN

         IF (this(icdan)%nuc(1) /= 0.d0) THEN  
           this(icdan)%tuc = (/ 0., 0., 1. /)
         ELSE IF (this(icdan)%nuc(2) /= 0.d0) THEN
           this(icdan)%tuc = (/ 1., 0., 0. /)
         ELSE IF (this(icdan)%nuc(3) /= 0.d0) THEN
            this(icdan)%tuc = (/ 0., 1., 0. /)
         ENDIF

     ELSE 

         IF (this(icdan)%nuc(1) == 0.D0) THEN

           fortuc  = -this(icdan)%nuc(2)/this(icdan)%nuc(3)
           normtuc = dsqrt(fortuc**2+2)

           this(icdan)%tuc(1) = 1/normtuc
           this(icdan)%tuc(2) = this(icdan)%tuc(1)
           this(icdan)%tuc(3) = fortuc/normtuc

         ELSE

           fortuc  = -(this(icdan)%nuc(2)+this(icdan)%nuc(3))/this(icdan)%nuc(1)
           normtuc = dsqrt(fortuc**2+2)

           this(icdan)%tuc(1) = fortuc/normtuc
           this(icdan)%tuc(2) = 1/normtuc
           this(icdan)%tuc(3) = this(icdan)%tuc(2)

         ENDIF

     ENDIF

     this(icdan)%suc(1) = this(icdan)%tuc(2)*this(icdan)%nuc(3) - this(icdan)%tuc(3)*this(icdan)%nuc(2)
     this(icdan)%suc(2) = this(icdan)%tuc(3)*this(icdan)%nuc(1) - this(icdan)%tuc(1)*this(icdan)%nuc(3)
     this(icdan)%suc(3) = this(icdan)%tuc(1)*this(icdan)%nuc(2) - this(icdan)%tuc(2)*this(icdan)%nuc(1)

     this(icdan)%gapTTBEGIN  =  nonuc
     this(icdan)%nonuc0      =  nonuc0
     this(icdan)%internal    =  0.d0  

                                                    !123456789012345678901234567890
     IF (tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_ROD                   ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_ROD                     ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_WIRE                  ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'BRITTLE_ELASTIC_WIRE          ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_WIRE                    ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'RIGID_WIRE                    ' .OR. &
         tact_behav(this(icdan)%lawnb)%lawty == 'TEX_SOL_UNILAT                ' &
        ) THEN

        this(icdan)%internal(1) = nonuc0

     ENDIF

   ENDIF

   t = this(icdan)%tuc
   n = this(icdan)%nuc
   s = this(icdan)%suc

!fd @@@ calcul des bras de levier pour une resolution dans le repere principal d'inertie.
!fd @@@ a voir la compatibilte avec les spheres !?


   cdlev= get_shiftTT_PT3Dx(pt3dx2bdyty(1,icdtac),pt3dx2bdyty(2,icdtac))
   anlev= get_shiftTT_PT3Dx(pt3dx2bdyty(1,iantac),pt3dx2bdyty(2,iantac))

   localframe_cd = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,icdtac))
   localframe_an = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,iantac))

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
           


   this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                           +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                           +(cd_Vbegin(3)-an_Vbegin(3))*t(3) &
                            + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                              - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

   this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                              +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                              +(cd_Vbegin(3)-an_Vbegin(3))*n(3) &
                              + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                              - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

   this(icdan)%vlsBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*s(1) &
                              +(cd_Vbegin(2)-an_Vbegin(2))*s(2) &
                              +(cd_Vbegin(3)-an_Vbegin(3))*s(3) &
                              + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                              - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)


   this(icdan)%rlt       = 0.D0
   this(icdan)%rln       = 0.D0
   this(icdan)%rls       = 0.D0
   this(icdan)%vlt       = this(icdan)%vltBEGIN
   this(icdan)%vln       = this(icdan)%vlnBEGIN
   this(icdan)%vls       = this(icdan)%vlsBEGIN
   this(icdan)%gapTT     = this(icdan)%gapTTBEGIN
   this(icdan)%status    = i_nknow

 END SUBROUTINE
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE coor_prediction_PTPT3

   IMPLICIT NONE  

   INTEGER  :: errare 
   INTEGER  :: itacty   
 
   IF(smooth_method)THEN
      DO itacty=1,nb_PT3Dx
         PTcoor(1:3,itacty) = get_coor_PT3Dx(pt3dx2bdyty(1,itacty),pt3dx2bdyty(2,itacty))
      END DO
   ELSE
      DO itacty=1,nb_PT3Dx
         PTcoor(1:3,itacty) = get_coorTT_PT3Dx(pt3dx2bdyty(1,itacty),pt3dx2bdyty(2,itacty),.FALSE.)
      END DO
   END IF

   ! DO itacty=1,nb_PT3Dx
      
   !    IF( XPERIODIC ) THEN
   !       IF( PTcoor(1,itacty)  > xperiode ) THEN
   !          call faterr('xx','yy')
   !          PTcoor(1,itacty) = PTcoor(1,itacty) - xperiode
   !       ELSE IF( PTcoor(1,itacty) < 0.D0 ) THEN
   !          call faterr('xx','yy')            
   !          PTcoor(1,itacty) = PTcoor(1,itacty) + xperiode
   !       END IF
   !    END IF
   !    IF( YPERIODIC ) THEN
   !       IF( PTcoor(2,itacty)  > yperiode ) THEN
   !          call faterr('xx','yy')            
   !          PTcoor(2,itacty) = PTcoor(2,itacty) - yperiode
   !       ELSE IF( PTcoor(2,itacty) < 0.D0 ) THEN
   !          call faterr('xx','yy')            
   !          PTcoor(2,itacty) = PTcoor(2,itacty) + yperiode
   !       END IF
   !    END IF

   ! END DO

 END SUBROUTINE coor_prediction_PTPT3
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE compute_contact_PTPT3

   use algebra
   
   IMPLICIT NONE  
   INTEGER                   :: errare, humanum, est,k
   INTEGER                   :: nb_PT3Dx
   INTEGER                   :: i,icdtac,iantac,isee,icdan,iadj,ibdy
   CHARACTER(len=5)          :: cdtac,cdcol,antac,ancol
   REAL(kind=8)              :: nonuc,nonuc0,fortuc,normtuc
   REAL(kind=8),DIMENSION(3) :: coordcd,coordan
   REAL(kind=8),DIMENSION(6) :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(3) :: t,n,s,cdlev,anlev,cd_shift,an_shift
   INTEGER                   :: cd_ent,an_ent,ixprd,iyprd

   REAL(kind=8),DIMENSION(3,3) :: Rc,localframe_cd,localframe_an
   CHARACTER(len=80) :: cout
                              !12345678901234567890123456
   character(len=26) :: IAM = 'mod_PTPT3::compute_contact'


   ! nard
   real(kind=8) :: n_proj(3),t_proj(3),s_proj(3)
   integer      :: err
   REAL(kind=8),DIMENSION(3,3) :: M_inertia
   
   icdan=0        
   nb_PTPT3=0
   nb_adj=0
   nb_PT3Dx=get_nb_PT3Dx()

   IF (nb_rough_PTPT3 /= 0 ) THEN

    IF (init_local_frame) THEN

       IF (.NOT. ALLOCATED(local_frame_n)) THEN
          ALLOCATE(local_frame_n(nb_rough_PTPT3,3),stat=errare)
       END IF

       IF (.NOT. ALLOCATED(local_frame_t)) THEN
          ALLOCATE(local_frame_t(nb_rough_PTPT3,3),stat=errare)
       END IF

       IF (.NOT. ALLOCATED(local_frame_s)) THEN
          ALLOCATE(local_frame_s(nb_rough_PTPT3,3),stat=errare)
       END IF

    END IF

    DO i=1,nb_rough_PTPT3
       icdtac=rough_PTPT3(i)%cd
       iantac=rough_PTPT3(i)%an
       isee=rough_PTPT3(i)%isee  
       nonuc0 = rough_PTPT3(i)%nonuc0
       
       if ( .NOT. get_visible_PT3Dx(icdtac) .or. .NOT. get_visible_PT3Dx(iantac) ) CYCLE

       coordcd = PTcoor(1:3,icdtac)
       coordan = PTcoor(1:3,iantac)

       ixprd  = rough_PTPT3(i)%xperiodic
       iyprd  = rough_PTPT3(i)%yperiodic


       !fd n'a aucun sens !!
       IF(rough_PTPT3(i)%loop) THEN ! on inverse cd et an !!
         coordcd(1) = coordcd(1) + REAL(ixprd,8)*XPERIODE
         coordcd(2) = coordcd(2) + REAL(iyprd,8)*YPERIODE
       ELSE
         coordan(1) = coordan(1) + REAL(ixprd,8)*XPERIODE
         coordan(2) = coordan(2) + REAL(iyprd,8)*YPERIODE
       ENDIF

       nonuc =dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2+(coordcd(3)-coordan(3))**2)
       
       icdan=icdan+1
       nb_adj(icdtac)=nb_adj(icdtac)+1
       iadj=nb_adj(icdtac)

       !fd 
       !fd la vitesse du centre d'inertie du RBDY3 auquel est acroche le PT3Dx
       !fd

       cd_Vbegin = get_Vbegin_PT3Dx( pt3dx2bdyty(1, icdtac) )
       an_Vbegin = get_Vbegin_PT3Dx( pt3dx2bdyty(1, iantac) )

       this(icdan)%icdbtac = pt3dx2bdyty(2, icdtac)
       this(icdan)%ianbtac = pt3dx2bdyty(2, iantac)

       this(icdan)%icdbtyp = pt3dx2bdyty(3, icdtac)
       this(icdan)%ianbtyp = pt3dx2bdyty(3, iantac)

       this(icdan)%icdctyp = i_pt3dx
       this(icdan)%ianctyp = i_pt3dx

       this(icdan)%iadj    = iadj
       this(icdan)%icdbdy  = pt3dx2bdyty(1, icdtac)
       this(icdan)%icdtac  = icdtac
       this(icdan)%icdsci  = 0
       this(icdan)%ianbdy  = pt3dx2bdyty(1, iantac)
       this(icdan)%iantac  = iantac
       this(icdan)%iansci  = 0
       this(icdan)%isee    = isee
       !pb
       this(icdan)%xperiodic = ixprd
       this(icdan)%yperiodic = iyprd
       !pb
       call get_behaviour_( icdan, see, tact_behav )

       CALL PTPT32ENT(icdan,cd_ent,an_ent)

       this(icdan)%icdent = cd_ent
       this(icdan)%ianent = an_ent

       entity(cd_ent)%nb = entity(cd_ent)%nb + 1
       entity(an_ent)%nb = entity(an_ent)%nb + 1

       IF ( nonuc0 .LT. 1.d-20) THEN
         IF (nonuc .GE. 1e-10) THEN
           write(cout,'(A)')                'On a 2 noeuds decolles dans une situation de noeuds colles'
           call logmes(cout)
           write(cout,'(A,I0,1x,I0)')       'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
           call logmes(cout)
           write(cout,'(A,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
           call faterr(IAM,cout)
         ENDIF

         !fd cas de noeuds colles 

         this(icdan)%tuc(:)      =  (/ 1.d0, 0.d0 , 0.d0/)
         this(icdan)%nuc(:)      =  (/ 0.d0, 1.d0 , 0.d0/)
         this(icdan)%suc(:)      =  (/ 0.d0, 0.d0 , 1.d0/)
         this(icdan)%gapTTbegin  =  0.d0
         this(icdan)%nonuc0      =  0.d0
         this(icdan)%internal    =  0.d0
 
         !fd Pour les lois sachant quoi faire on laisse faire: 
         !fd COUPLED_DOF        
         !fd TEX_SOL
                                                   !123456789012345678901234567890
         IF (tact_behav(this(icdan)%lawnb)%lawty /= 'COUPLED_DOF                   ' .AND. &
             tact_behav(this(icdan)%lawnb)%lawty /= 'TEX_SOL                       ' &
            ) THEN
           write(cout,'(A)')                'On a 2 noeuds colles mais la loi d''interaction ne sait pas le traiter'
           call logmes(cout)
           write(cout,'(A,I0,1x,I0)')       'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
           call logmes(cout)
           write(cout,'(A,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
           call faterr(IAM,cout)
         ENDIF

       ELSE

         ! nard 
         IF (explicit_local_frame) THEN

           IF (init_local_frame) THEN


             ! voir si on stocke t ou si on le recalcul a chaque fois

             ! calcul du repere local intial

             this(icdan)%nuc(:) = (coordcd(1:3)-coordan(1:3))/nonuc
             
             DO k=1,3
               IF (dabs(this(icdan)%nuc(k)) .LT. 1.D-18) this(icdan)%nuc(k) = 0.D0
             ENDDO         

             IF (( this(icdan)%nuc(1)*this(icdan)%nuc(2) == 0.D0 ).AND. &
                 ( this(icdan)%nuc(2)*this(icdan)%nuc(3) == 0.D0 ).AND. &
                 ( this(icdan)%nuc(3)*this(icdan)%nuc(1) == 0.D0 )) THEN

               IF (this(icdan)%nuc(1) /= 0.d0) THEN  
                 this(icdan)%tuc = (/ 0., 0., 1. /)
               ELSE IF (this(icdan)%nuc(2) /= 0.d0) THEN
                 this(icdan)%tuc = (/ 1., 0., 0. /)
               ELSE IF (this(icdan)%nuc(3) /= 0.d0) THEN
                 this(icdan)%tuc = (/ 0., 1., 0. /)
               ENDIF

             ELSE 

               IF (this(icdan)%nuc(1) == 0.D0) THEN

                 fortuc  = -this(icdan)%nuc(2)/this(icdan)%nuc(3)
                 normtuc = dsqrt(fortuc**2+2)

                 this(icdan)%tuc(1) = 1/normtuc
                 this(icdan)%tuc(2) = this(icdan)%tuc(1)
                 this(icdan)%tuc(3) = fortuc/normtuc

               ELSE

                 fortuc  = -(this(icdan)%nuc(2)+this(icdan)%nuc(3))/this(icdan)%nuc(1)
                 normtuc = dsqrt(fortuc**2+2)

                 this(icdan)%tuc(1) = fortuc/normtuc
                 this(icdan)%tuc(2) = 1/normtuc
                 this(icdan)%tuc(3) = this(icdan)%tuc(2)

               ENDIF

             ENDIF

             
             this(icdan)%suc(1) = this(icdan)%tuc(2)*this(icdan)%nuc(3) - this(icdan)%tuc(3)*this(icdan)%nuc(2)
             this(icdan)%suc(2) = this(icdan)%tuc(3)*this(icdan)%nuc(1) - this(icdan)%tuc(1)*this(icdan)%nuc(3)
             this(icdan)%suc(3) = this(icdan)%tuc(1)*this(icdan)%nuc(2) - this(icdan)%tuc(2)*this(icdan)%nuc(1)


             ! on projet dans repere inertie
             M_inertia = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,iantac))

             local_frame_n(i,:) = matmul(this(icdan)%nuc(:),M_inertia)
             local_frame_t(i,:) = matmul(this(icdan)%tuc(:),M_inertia)
             local_frame_s(i,:) = matmul(this(icdan)%suc(:),M_inertia)

             this(icdan)%gapTTBEGIN  =  nonuc
             this(icdan)%nonuc0      =  nonuc0
                                                           !123456789012345678901234567890
             IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'NARD_ROD                      ') THEN

               this(icdan)%internal(2) = 0.d0 ! pas utile si on le fait dans le solveur ! suivant t 
               this(icdan)%internal(3) = 0.d0 ! pas utile si on le fait dans le solveur ! suivant s 

             END IF

           ELSE


             ! on recupere le repere local (exprime dans le repere d inertie)

             n(:) = local_frame_n(i,:) 
             t(:) = local_frame_t(i,:) 
             s(:) = local_frame_s(i,:)

             M_inertia = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,iantac))

             call inverse33(M_inertia,err)

             n_proj = matmul(n,M_inertia)
             t_proj = matmul(t,M_inertia)
             s_proj = matmul(s,M_inertia)

             this(icdan)%nuc(:) = n_proj(:)
             this(icdan)%tuc(:) = t_proj(:)
             this(icdan)%suc(:) = s_proj(:)

!             s(1) = this(icdan)%tuc(2)*this(icdan)%nuc(3) - this(icdan)%tuc(3)*this(icdan)%nuc(2)
!             s(2) = this(icdan)%tuc(3)*this(icdan)%nuc(1) - this(icdan)%tuc(1)*this(icdan)%nuc(3)
!             s(3) = this(icdan)%tuc(1)*this(icdan)%nuc(2) - this(icdan)%tuc(2)*this(icdan)%nuc(1)

!             this(icdan)%suc(:) = s(:)


             this(icdan)%gapTTBEGIN  = n_proj(1)*(coordcd(1)-coordan(1)) + n_proj(2)*(coordcd(2)-coordan(2)) &
                                       + n_proj(3)*(coordcd(3)-coordan(3))

             this(icdan)%nonuc0      =  nonuc0
                                                           !123456789012345678901234567890
             IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'NARD_ROD                      ') THEN

               this(icdan)%internal(2) = t_proj(1)*(coordcd(1)-coordan(1)) + t_proj(2)*(coordcd(2)-coordan(2)) &
                                       + t_proj(3)*(coordcd(3)-coordan(3))
               this(icdan)%internal(3) = s_proj(1)*(coordcd(1)-coordan(1)) + s_proj(2)*(coordcd(2)-coordan(2)) &
                                       + s_proj(3)*(coordcd(3)-coordan(3))
!               this(icdan)%internal(3) = s(1)*(coordcd(1)-coordan(1)) + s(2)*(coordcd(2)-coordan(2)) &
!                                       + s(3)*(coordcd(3)-coordan(3))

             END IF

           END IF   

           this(icdan)%coor(1:3) = (coordcd(1:3) + coordan(1:3))*0.5
!! --an
         ELSE
          
           IF (nonuc .LT. 1d-20) THEN
             write(cout,'(A)')                'On a 2 noeuds colles  dans une situation de noeuds decolles'
             call logmes(cout)
             write(cout,'(A,I0,1x,I0)')       'corps cd/an concernes: ', pt3dx2bdyty(1,icdtac), pt3dx2bdyty(1,iantac)
             call logmes(cout)
             write(cout,'(A,D14.7,A,D14.7)') 'distances initiale:', this(icdan)%nonuc0,' et actuelle: ',nonuc
             call faterr(IAM,cout)
           ENDIF 

           this(icdan)%nuc(:) = (coordcd(1:3)-coordan(1:3))/nonuc
     
           this(icdan)%coor(1:3) = (coordcd(1:3) + coordan(1:3))*0.5

           !fd a voir ...
           if (XPERIODIC) this(icdan)%coor(1) = modulo(this(icdan)%coor(1),xperiode)
           if (YPERIODIC) this(icdan)%coor(2) = modulo(this(icdan)%coor(2),yperiode)  

           ! Dans le cas general, on sait que l'antagoniste fait partie du plan tangent donc on prend un point M de ce plan
           ! parametre : Ym = Yan +1 et Zm = Zan +1 pour obtenir le vecteur t. Si n(1)=0, on parametre Xm et Ym

           !Cas ou la normale est alignee sur un axe

           DO k=1,3
             IF (dabs(this(icdan)%nuc(k)) .LT. 1.D-18) this(icdan)%nuc(k) = 0.D0
           ENDDO         

           IF (( this(icdan)%nuc(1)*this(icdan)%nuc(2) == 0.D0 ).AND. &
               ( this(icdan)%nuc(2)*this(icdan)%nuc(3) == 0.D0 ).AND. &
               ( this(icdan)%nuc(3)*this(icdan)%nuc(1) == 0.D0 )) THEN

              IF (this(icdan)%nuc(1) /= 0.d0) THEN  
                this(icdan)%tuc = (/ 0., 0., 1. /)
              ELSE IF (this(icdan)%nuc(2) /= 0.d0) THEN
                this(icdan)%tuc = (/ 1., 0., 0. /)
              ELSE IF (this(icdan)%nuc(3) /= 0.d0) THEN
                this(icdan)%tuc = (/ 0., 1., 0. /)
              ENDIF

           ELSE 

             IF (this(icdan)%nuc(1) == 0.D0) THEN

                fortuc  = -this(icdan)%nuc(2)/this(icdan)%nuc(3)
                normtuc = dsqrt(fortuc**2+2)

                this(icdan)%tuc(1) = 1/normtuc
                this(icdan)%tuc(2) = this(icdan)%tuc(1)
                this(icdan)%tuc(3) = fortuc/normtuc

             ELSE

                fortuc  = -(this(icdan)%nuc(2)+this(icdan)%nuc(3))/this(icdan)%nuc(1)
                normtuc = dsqrt(fortuc**2+2)

                this(icdan)%tuc(1) = fortuc/normtuc
                this(icdan)%tuc(2) = 1/normtuc
                this(icdan)%tuc(3) = this(icdan)%tuc(2)

             ENDIF

           ENDIF

           this(icdan)%suc(1) = this(icdan)%tuc(2)*this(icdan)%nuc(3) - this(icdan)%tuc(3)*this(icdan)%nuc(2)
           this(icdan)%suc(2) = this(icdan)%tuc(3)*this(icdan)%nuc(1) - this(icdan)%tuc(1)*this(icdan)%nuc(3)
           this(icdan)%suc(3) = this(icdan)%tuc(1)*this(icdan)%nuc(2) - this(icdan)%tuc(2)*this(icdan)%nuc(1)

           this(icdan)%gapTTBEGIN  =  nonuc
           this(icdan)%nonuc0      =  nonuc0
           this(icdan)%internal    =  0.d0  

                                                      !123456789012345678901234567890
           IF (tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_ROD                   ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_ROD                     ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_WIRE                  ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'BRITTLE_ELASTIC_WIRE          ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_WIRE                    ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'RIGID_WIRE                    ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'TEX_SOL_UNILAT                ' &
              ) THEN

             this(icdan)%internal(1) = nonuc0

           ENDIF

         ENDIF
                                                    !123456789012345678901234567890
         IF (tact_behav(this(icdan)%lawnb)%lawty == 'NARD_ROD                      ' .AND. &
             (given_network .eqv. .false.) .AND. (given_params .eqv. .false.)) THEN

              write(cout,'(A)')                'ATTENTION: Params (surf,l0) non charges !!!'
              call faterr(IAM,cout)

         END IF


         IF (given_params) THEN
             this(icdan)%internal(4) = rough_PTPT3(i)%surf
             this(icdan)%internal(5) = rough_PTPT3(i)%l0
             this(icdan)%internal(6) = rough_PTPT3(i)%mass
         END IF

       endif
       t = this(icdan)%tuc
       n = this(icdan)%nuc
       s = this(icdan)%suc

       !fd @@@ calcul des bras de levier pour une resolution dans le repere principal d'inertie.
       !fd @@@ a voir la compatibilte avec les spheres !?

       cdlev= get_shiftTT_PT3Dx(pt3dx2bdyty(1,icdtac),pt3dx2bdyty(2,icdtac))
       anlev= get_shiftTT_PT3Dx(pt3dx2bdyty(1,iantac),pt3dx2bdyty(2,iantac))

       localframe_cd = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,icdtac))
       localframe_an = get_inertia_frameTT_PT3Dx(pt3dx2bdyty(1,iantac))

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
           


       this(icdan)%vltBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*t(1) &
                              +(cd_Vbegin(2)-an_Vbegin(2))*t(2) &
                              +(cd_Vbegin(3)-an_Vbegin(3))*t(3) &
                              + cd_Vbegin(4)*this(icdan)%Gcdt(1)+cd_Vbegin(5)*this(icdan)%Gcdt(2)+cd_Vbegin(6)*this(icdan)%Gcdt(3) &
                              - an_Vbegin(4)*this(icdan)%Gant(1)-an_Vbegin(5)*this(icdan)%Gant(2)-an_Vbegin(6)*this(icdan)%Gant(3)

       this(icdan)%vlnBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*n(1) &
                              +(cd_Vbegin(2)-an_Vbegin(2))*n(2) &
                              +(cd_Vbegin(3)-an_Vbegin(3))*n(3) &
                              + cd_Vbegin(4)*this(icdan)%Gcdn(1)+cd_Vbegin(5)*this(icdan)%Gcdn(2)+cd_Vbegin(6)*this(icdan)%Gcdn(3) &
                              - an_Vbegin(4)*this(icdan)%Gann(1)-an_Vbegin(5)*this(icdan)%Gann(2)-an_Vbegin(6)*this(icdan)%Gann(3)

       this(icdan)%vlsBEGIN  = (cd_Vbegin(1)-an_Vbegin(1))*s(1) &
                              +(cd_Vbegin(2)-an_Vbegin(2))*s(2) &
                              +(cd_Vbegin(3)-an_Vbegin(3))*s(3) &
                              + cd_Vbegin(4)*this(icdan)%Gcds(1)+cd_Vbegin(5)*this(icdan)%Gcds(2)+cd_Vbegin(6)*this(icdan)%Gcds(3) &
                              - an_Vbegin(4)*this(icdan)%Gans(1)-an_Vbegin(5)*this(icdan)%Gans(2)-an_Vbegin(6)*this(icdan)%Gans(3)


       this(icdan)%rlt       = 0.D0
       this(icdan)%rln       = 0.D0
       this(icdan)%rls       = 0.D0
       this(icdan)%vlt       = this(icdan)%vltBEGIN
       this(icdan)%vln       = this(icdan)%vlnBEGIN
       this(icdan)%vls       = this(icdan)%vlsBEGIN
       this(icdan)%gapTT     = this(icdan)%gapTTBEGIN
       this(icdan)%status    = i_nknow

    ENDDO
    nb_PTPT3=icdan

    IF (init_local_frame) init_local_frame = .FALSE.
    
  END IF

   WRITE(cout,'(1X,I10,A12)') nb_PTPT3,' PTPT3 found'
   CALL logmes(cout)

   ! Since selection of candidates for contact has been refined, nb_PTPT3 is less or equal size(this). 
   ! Loops are now to run from 1 to nb_PTPT3 where data are available.

   DO ibdy=1,nb_PT3Dx
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
         call faterr('mod_PTPT3::compute_contact',cout)
       END IF
     ELSE 
       NULLIFY(adjac(ibdy)%icdan)
     ENDIF
   !
   ENDDO
  
   DO icdan=1,nb_PTPT3
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   END DO

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PTPT3),stat=errare)

 END SUBROUTINE compute_contact_PTPT3
!------------------------------------------------------------------------

!!!------------------------------------------------------------------------
 subroutine display_prox_tactors_PTPT3

   implicit none
   integer :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nb_PT3Dx
   integer :: icdtact

   character(len=5) :: cdmodel, anmodel

   nb_PT3Dx=get_nb_PT3Dx()

   DO icdtact=1,nb_PT3Dx    
     DO iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( pt3dx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( pt3dx2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
                         !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '      
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,pt3dx2bdyty(1,icdtac),'PT3Dx',pt3dx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,pt3dx2bdyty(1,iantac),'PT3Dx',pt3dx2bdyty(2,iantac)

       WRITE(*,104) 't(1)=',this(icdan)%tuc(1)  ,'n(1)=',this(icdan)%nuc(1)  ,'s(1)=',this(icdan)%suc(1)
       WRITE(*,104) 't(2)=',this(icdan)%tuc(2)  ,'n(2)=',this(icdan)%nuc(2)  ,'s(2)=',this(icdan)%suc(2)
       WRITE(*,104) 't(3)=',this(icdan)%tuc(3)  ,'n(3)=',this(icdan)%nuc(3)  ,'s(3)=',this(icdan)%suc(3)
       WRITE(*,104) 'rlt =',this(icdan)%rlt     ,'rln =',this(icdan)%rln     ,'rls =',this(icdan)%rls
       WRITE(*,104) 'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',this(icdan)%vlsBEGIN                    
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(27X,3(2X,A5,D14.7))

 end subroutine display_prox_tactors_PTPT3
!------------------------------------------------------------------------ 
   SUBROUTINE smooth_computation_PTPT3

    IMPLICIT NONE
    INTEGER          :: icdan
    REAL(kind=8)     :: reff,meff

    reff = 1.0
    meff = 1.0

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    DO icdan=1,nb_PTPT3
       
       CALL compute_3D_smooth_forces(this(icdan)%lawnb,&
            this(icdan)%vlsBEGIN,this(icdan)%vltBEGIN,this(icdan)%vlnBEGIN, &
            this(icdan)%gapTTBEGIN,this(icdan)%statusBEGIN,this(icdan)%internal, &
            reff,meff,this(icdan)%status,this(icdan)%gapTT, &
            this(icdan)%vls,this(icdan)%vlt,this(icdan)%vln, &
            this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln)

       violation(icdan) = this(icdan)%gapTT

    END DO
    !$OMP END DO
    !$OMP END PARALLEL
    
    DO icdan=1,nb_PTPT3  
       CALL nullify_reac_PTPT3(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_PTPT3
       CALL injj_PTPT3(icdan,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln,iIreac)
    END DO
    
    
  END SUBROUTINE smooth_computation_PTPT3
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_PTPT3
 
   
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,nb_PT3Dx
   INTEGER :: errare

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PTPT3::stock_rloc'

   nb_PT3Dx=get_nb_PT3Dx()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT3Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_PT3Dx
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
     DO icdtac=1,nb_PT3Dx
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

  ! filling data:
   DO icdan=1,nb_PTPT3
     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                      ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                        ! serial adjacent number of pair contactor 
                                                      ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)     = icdan
     verlt(icdtac)%cdbdy           = pt3dx2bdyty(1, icdtac)
     verlt(icdtac)%cdtac           = pt3dx2bdyty(2, icdtac)
     verlt(icdtac)%cdmodel         = pt3dx2bdyty(3, icdtac)
     verlt(icdtac)%cdsci           = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)     = pt3dx2bdyty(1, iantac)
     verlt(icdtac)%antac(iadj)     = pt3dx2bdyty(2, iantac)
     verlt(icdtac)%anmodel(iadj)   = pt3dx2bdyty(3, iantac)
     verlt(icdtac)%ansci           = this(icdan)%iansci
     verlt(icdtac)%status(iadj)    = this(icdan)%status

     verlt(icdtac)%rlt(iadj)       = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)       = this(icdan)%rln/H
     verlt(icdtac)%rls(iadj)       = this(icdan)%rls/H

     verlt(icdtac)%vlt(iadj)       = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)       = this(icdan)%vln
     verlt(icdtac)%vls(iadj)       = this(icdan)%vls

     verlt(icdtac)%gapTT(iadj)     = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)    = this(icdan)%status
     verlt(icdtac)%coor(1:3, iadj) = this(icdan)%coor(1:3)
     verlt(icdtac)%tuc(:, iadj)    = this(icdan)%tuc(:)
     verlt(icdtac)%nuc(:, iadj)    = this(icdan)%nuc(:)
     verlt(icdtac)%suc(:, iadj)    = this(icdan)%suc(:)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   END DO

   nb_vPTPT3 = nb_PTPT3

   WRITE(cout,'(1X,I10,A12)') nb_vPTPT3,' stock PTPT3'
   call logmes(cout)

 END SUBROUTINE stock_rloc_PTPT3
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_PTPT3
   
   IMPLICIT NONE
   INTEGER           :: icdan,iantac,icdtac,iadj
   CHARACTER(len=21) :: IAM = 'mod_PTPT3::recup_rloc'
   CHARACTER(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_PTPT3 =0 

   DO icdan=1,nb_PTPT3
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%rls=0.D0
     this(icdan)%statusBEGIN=i_nknow

     icdtac = this(icdan)%icdtac
     iantac = this(icdan)%iantac

     IF (verlt(icdtac)%adjsz /= 0) THEN
       if ( verlt(icdtac)%cdbdy  == pt3dx2bdyty(1,icdtac) .and. &
            verlt(icdtac)%cdtac  == pt3dx2bdyty(2,icdtac) .and. &
            verlt(icdtac)%cdmodel== pt3dx2bdyty(3,iantac) ) then
         do iadj = 1, verlt(icdtac)%adjsz
           if ( verlt(icdtac)%anbdy(iadj)  == pt3dx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == pt3dx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== pt3dx2bdyty(3,iantac) ) then

              this(icdan)%rlt         = verlt(icdtac)%rlt(iadj)*H
              this(icdan)%rln         = verlt(icdtac)%rln(iadj)*H
              this(icdan)%rls         = verlt(icdtac)%rls(iadj)*H
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

              this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
              nb_recup_PTPT3 = nb_recup_PTPT3 + 1
              exit
           end if
         end do
       end if
     ENDIF
   END DO
   
   WRITE(cout,'(1X,I10,A12)') nb_recup_PTPT3,' recup PTPT3'
   CALL logmes(cout)

 END SUBROUTINE recup_rloc_PTPT3
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 subroutine read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   implicit none
   integer                    :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact,nb_PT3Dx
   integer                    :: cdmodel, anmodel
   real(kind=8)               :: rlt,rln,rls,vlt,vln,vls,gapTT
   real(kind=8), dimension(3) :: nuc,coor
   character(len=5)           :: cdbdy,cdtac,anbdy,antac,behav,sttus
   integer                    :: errare
   integer                    :: ibehav,nb_internal,i_internal
   character(len=29)          :: IAM = 'mod_PTPT3::read_ini_Vloc_Rloc'
   character(len=103)         :: cout

   nb_PT3Dx=get_nb_PT3Dx()

   errare=0
  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.
   IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_PT3DX),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating nb_adj')
   END IF    

   nb_adj=0
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PTPT3') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     IF (cdtac /= 'PT3Dx' .OR. antac /= 'PT3Dx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )     
     do icdtact = 1, nb_PT3Dx
       if (pt3dx2bdyty(1,icdtact) == icdbdy .and. &
           pt3dx2bdyty(2,icdtact) == icdtac .and. &
           pt3dx2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
     CYCLE
   END DO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT3Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtac=1,nb_PT3Dx
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
     DO icdtac=1,nb_PT3Dx
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

   nb_adj=0
   icdan = 0

   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PTPT3') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus
     IF (cdtac /= 'PT3Dx' .OR. antac /= 'PT3Dx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     DO icdtact=1,nb_PT3Dx
       if (pt3dx2bdyty(1,icdtact) == icdbdy .and. &
           pt3dx2bdyty(2,icdtact) == icdtac .and. &
           pt3dx2bdyty(3,icdtact) == cdmodel ) then

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
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,3(7X,D14.7))')rlt,rln,rls
         verlt(icdtact)%rlt(nb_adj(icdtact))=rlt
         verlt(icdtact)%rln(nb_adj(icdtact))=rln
         verlt(icdtact)%rls(nb_adj(icdtact))=rls
         IF( .NOT. read_G_clin()) EXIT 
         READ(G_clin(1:90),'(27X,3(7X,D14.7))')vlt,vln,vls
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         verlt(icdtact)%vls(nb_adj(icdtact))=vls
         IF( .NOT. read_G_clin()) EXIT 
         READ(G_clin(1:90),'(27X,2(7X,D14.7))')gapTT
         verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
         IF( .NOT. read_G_clin()) EXIT
         IF (G_clin(30:34)== 'n(1)=') THEN
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') nuc(1),nuc(2),nuc(3)
           verlt(icdtact)%nuc(1,nb_adj(icdtact))=nuc(1)
           verlt(icdtact)%nuc(2,nb_adj(icdtact))=nuc(2)
           verlt(icdtact)%nuc(3,nb_adj(icdtact))=nuc(3)
         ELSE 
           BACKSPACE(G_nfich)
         END IF
         IF( .NOT. read_G_clin()) EXIT
         IF (G_clin(30:34)== 'coo1=') THEN
           READ(G_clin(1:90),'(27X,3(7X,D14.7))') coor(1),coor(2),coor(3)
           verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
           verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)
           verlt(icdtact)%coor(3,nb_adj(icdtact))=coor(3)
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

   DO icdtact=1,nb_PT3Dx

     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       write(cout,'(A,1x,I0)')      'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,1x,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')      'value of verlet%adjsz is',verlt(icdtact)%adjsz
       call faterr(IAM,cout)
     ENDIF

   END DO

    nb_vPTPT3=0
    
    DO icdtact=1,nb_PT3Dx
       nb_vPTPT3 = nb_vPTPT3 + nb_adj(icdtact)
       
       IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
          WRITE(cout,'(A31,I7,1X,A17,1X,I7,A30,I7)') 'Very strange for the contactor ',icdtact, &
               'value of nb_adj is',nb_adj(icdtact),' and value of verlet%adjsz is ',verlt(icdtact)%adjsz
          CALL FATERR(IAM,cout)
       END IF
    END DO

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

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   IF (nb_PTPT3.EQ.0) RETURN

   DO icdtact=1,nb_PT3DX    
      DO iadj=1,nb_adj(icdtact)         
         icdan  = adjac(icdtact)%icdan(iadj)
         icdtac = this(icdan)%icdtac
         iantac = this(icdan)%iantac
         
         cdmodel = get_body_model_name_from_id( pt3dx2bdyty(3,icdtac) )
         anmodel = get_body_model_name_from_id( pt3dx2bdyty(3,iantac) )
         WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PTPT3',icdan     
         !1234567890123456789012345678901234567890123456789012345678901234567890123456  
         WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'      
         WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
              !pta old fashion 'RBDY3',pt3dx2bdyty(1,icdtac),'PT3Dx',pt3dx2bdyty(2,icdtac),  &
              cdmodel,get_visibleID_PT3Dx(icdtac),'PT3Dx',pt3dx2bdyty(2,icdtac),  &
              see(this(icdan)%isee)%behav,  &
              !pta old fashion 'RBDY3',pt3dx2bdyty(1,iantac),'PT3Dx',pt3dx2bdyty(2,iantac),  &
              anmodel,get_visibleID_PT3Dx(iantac),'PT3Dx',pt3dx2bdyty(2,iantac),  &
              get_contact_status_name_from_id(this(icdan)%status),iantac
         WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',this(icdan)%rls/H
         WRITE(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',this(icdan)%vls
         WRITE(nfich,103)'gapTT',this(icdan)%gapTT 
         WRITE(nfich,104)'n(1)=',this(icdan)%nuc(1),'n(2)=',this(icdan)%nuc(2),'n(3)=',this(icdan)%nuc(3)
         WRITE(nfich,104)'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',this(icdan)%coor(3)
         
         IF (this(icdan)%nb_internal /= 0) THEN
           CALL write_internal_comment(nfich,this(icdan)%lawnb)
           write(fmt,"('(',I0,'(1x,D14.7))')") this(icdan)%nb_internal
           write(nfich,trim(fmt)) this(icdan)%internal(1:this(icdan)%nb_internal)
         ENDIF
         WRITE(nfich,'(A1)')' '
         
      END DO
   END DO
   
103 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,2X,A5,D14.7)
104 FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

 END SUBROUTINE write_out_Vloc_Rloc
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_reac_PTPT3(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdbdy,ianbdy
   INTEGER :: storage
    
   icdbdy=this(icdan)%icdbdy
   CALL nullify_reac_PT3Dx(icdbdy,storage)
   
   ianbdy=this(icdan)%ianbdy
   CALL nullify_reac_PT3Dx(ianbdy,storage)
    
 END SUBROUTINE nullify_reac_PTPT3
!------------------------------------------------------------------------ 
 !------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_PTPT3(icdan,storage)

   IMPLICIT NONE

   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdbdy,ianbdy,storage
    
   icdbdy = this(icdan)%icdbdy
   CALL nullify_vlocy_PT3Dx(icdbdy,storage)
   
   ianbdy = this(icdan)%ianbdy
   CALL nullify_vlocy_PT3Dx(ianbdy,storage)
    
 END SUBROUTINE nullify_vlocy_PTPT3
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_PTPT3( icdan, storage, need_full_vlocy )

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdbdy,ianbdy
   INTEGER :: storage
   logical, optional  :: need_full_vlocy
    
  icdbdy=this(icdan)%icdbdy
  CALL comp_vlocy_PT3Dx(icdbdy,storage)
    
  ianbdy=this(icdan)%ianbdy
  CALL comp_vlocy_PT3Dx(ianbdy,storage)
    
 END SUBROUTINE vitrad_PTPT3
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE injj_PTPT3(icdan,RSIK,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)    :: icdan
   REAL(kind=8),INTENT(in)    :: RSIK,RTIK,RNIK
   INTEGER,     DIMENSION(6)  :: cdccdof,anccdof
   REAL(kind=8),DIMENSION(6)  :: cdreac, anreac
   INTEGER                    :: icdbdy,ianbdy
   INTEGER                    :: storage

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy
   cdccdof(1)=1
   anccdof(1)=1
   cdreac(1) = RSIK*this(icdan)%suc(1)+RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)
   anreac(1)=-cdreac(1)

   cdccdof(2)=2
   anccdof(2)=2     
   cdreac(2) = RSIK*this(icdan)%suc(2)+RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)
   anreac(2)=-cdreac(2)

   cdccdof(3)=3
   anccdof(3)=3
   cdreac(3) = RSIK*this(icdan)%suc(3)+RTIK*this(icdan)%tuc(3)+RNIK*this(icdan)%nuc(3)
   anreac(3)=-cdreac(3)

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

   CALL add_reac_PT3Dx(icdbdy,cdccdof,cdreac,storage)
   CALL add_reac_PT3Dx(ianbdy,anccdof,anreac,storage)

 END SUBROUTINE injj_PTPT3 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_PTPT3(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: icdbdy,ianbdy
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van
   
   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy
   
   Vcd = get_vlocy_PT3Dx(icdbdy,storage)
   Van = get_vlocy_PT3Dx(ianbdy,storage)

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

 END SUBROUTINE prjj_PTPT3
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_PTPT3(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_PTPT3 = nb_PTPT3
    CASE(i_verlet_tactor)
       get_nb_PTPT3 = nb_vPTPT3
    CASE(i_rough_tactor)
       get_nb_PTPT3 = nb_rough_PTPT3
    CASE(i_recup_tactor)
       get_nb_PTPT3 = nb_recup_PTPT3
    END SELECT

  END FUNCTION get_nb_PTPT3
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 FUNCTION get_max_radius_PTPT3(fantome)

   IMPLICIT NONE
   INTEGER,OPTIONAL :: fantome
   REAL(kind=8)     ::  get_max_radius_PTPT3

   get_max_radius_PTPT3=max_nonuc0

 END FUNCTION get_max_radius_PTPT3
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE put_violation_PTPT3(icdan,vltonK)

   IMPLICIT NONE
   INTEGER     :: icdan
   REAL(kind=8):: vltonK

   violation(icdan)=vltonK
   
 END SUBROUTINE put_violation_PTPT3
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE PTPT32ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_PT3Dx(this(icdan)%icdbdy)
   ianent = get_ENT_PT3Dx(this(icdan)%ianbdy)

 END SUBROUTINE PTPT32ENT
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
SUBROUTINE PTPT32PT3Dx(icdan,icdtac,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

END SUBROUTINE PTPT32PT3Dx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_PTPT3()

    IMPLICIT NONE
    
    RUN_PTPT3 = RUN_TACTOR

  END FUNCTION RUN_PTPT3
!!!-----------------------------------------------------------------------
  INTEGER FUNCTION get_yperiode_PTPT3(icdan)
    
    IMPLICIT NONE
    INTEGER :: icdan
    
    get_yperiode_PTPT3 = this(icdan)%yperiodic
   
  END FUNCTION get_yperiode_PTPT3
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_xperiode_PTPT3(icdan)

    IMPLICIT NONE
    INTEGER :: icdan
    
    get_xperiode_PTPT3 = this(icdan)%xperiodic
   
  END FUNCTION get_xperiode_PTPT3
!!!------------------------------------------------------------------------
  logical function CHECK_PTPT3()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PTPT3 = check_PTPT3_
      return
    end if

    con_pedigree%module_name = 'PTPT3'

    con_pedigree%id_cdan  = i_ptpt3
    con_pedigree%id_cdtac = i_pt3dx
    con_pedigree%id_antac = i_pt3dx

    cdtact2bdyty => pt3dx2bdyty
    antact2bdyty => pt3dx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_PT3Dx = get_nb_PT3Dx()
    if( nb_PT3Dx < 2 ) then
      CHECK_PTPT3 = check_PTPT3_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'PT3Dx' .and. see(isee)%antac == 'PT3Dx') then
        check_PTPT3_ = .true.
        exit
      end if
    end do

    CHECK_PTPT3 = check_PTPT3_
    return

  end function CHECK_PTPT3
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_PTPT3()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_PTPT3 = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_PTPT3

!!!-----------------------------------------------------------------------

  REAL(kind=8) function get_surf_PTPT3(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan

   get_surf_PTPT3 = 0.d0 ! todo this(icdan)%surf

 END function get_surf_PTPT3

!!!------------------------------------------------------------------------
   SUBROUTINE load_params_PTPT3
     IMPLICIT NONE

     given_params = .TRUE.

   END SUBROUTINE load_params_PTPT3

   subroutine use_current_nonuc0_PTPT3(no0)
     implicit none
     integer, intent(in) :: no0

     if( no0 == 0 ) then
       current_nonuc0 = .false.
     else
       current_nonuc0 = .true.
     end if

   end subroutine use_current_nonuc0_PTPT3

!!!-----------------------------------------------------------------------
  SUBROUTINE set_explicit_local_frame_PTPT3

    IMPLICIT NONE
      
    explicit_local_frame = .TRUE.

  END SUBROUTINE set_explicit_local_frame_PTPT3
!!!-----------------------------------------------------------------------

  subroutine clean_memory_PTPT3
    implicit none
    integer(kind=4) :: i, j

    call clean_memory_inter_meca_()

    nb_PT3Dx       = 0
    nb_PTPT3       = 0
    nb_vPTPT3      = 0
    nb_recup_PTPT3 = 0

    if( allocated(box) ) then
      do i = 1, size(box,2)
        do j = 1, size(box,1)
          if( associated(box(j,i)%which) ) deallocate(box(j,i)%which)
        end do
      end do
      deallocate(box)
    end if

    nb_rough_PTPT3 = 0
    if( allocated(rough_PTPT3) ) deallocate(rough_PTPT3)

    ! Root, Current and Previous should always be null outside creation_tab_visu

    if( allocated(PTcoor) ) deallocate(PTcoor)
    if( allocated(PTcoor0)) deallocate(PTcoor0)

    Reac_PTPT3_MAX = 0.D0

    module_checked_ = .FALSE.
    check_PTPT3_    = .FALSE.

  end subroutine

 subroutine set_nb_PTPT3(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PTPT3 = nb

 end subroutine

 subroutine redo_nb_adj_PTPT3()
   implicit none

   call redo_nb_adj_( get_nb_PT3Dx() )

 end subroutine

END MODULE PTPT3

