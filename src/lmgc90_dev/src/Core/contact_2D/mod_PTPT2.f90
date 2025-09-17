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
MODULE PTPT2

  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors PT2Dx.
  !!  In this modulus candidate contactors are PT2Dx and antagonist 
  !!  contactors are PT2Dx.

  USE overall
  USE tact_behaviour
  USE PT2Dx

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use inter_meca_2D

  use parameters, only : i_ptpt2, i_pt2dx, i_mailx, i_rbdy2, i_mbs2

  implicit none

  private

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 INTEGER :: nb_PTPT2                                  ! nb_PTPT2 = number of selected candidates POINT against POINT
                                                      ! <= size(this).

 INTEGER :: nb_vPTPT2
!------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs POINT-POINT
                                                         !                  to candidate contactor POINT icdtac.

!------------------------------------------------------------------------ 

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------
 TYPE T_box
                                                      ! For quick sorting, disks are owned by boxes, sorting being 
                        ! performed within a box and immediate surrounding boxes, see
                        ! subroutine enumerate_PTPT2.
                        
   INTEGER                               :: popul     ! box(ibox1,ibox2)%popul: number of DISKx in box ibox1,ibox2;
   
   INTEGER, DIMENSION(:), POINTER        :: which     ! box(ibox1,ibox2)%which(ipopul): 
                                                      ! rank in the list of contactors of DISKx labelled ipopul
                        ! in box ibox1,ibox2;
   
 END TYPE T_box 

 TYPE(T_box), DIMENSION(:,:),ALLOCATABLE    :: box    ! box(ibox1,ibox2): box with integer coordinates ibox1,ibox2.

 TYPE T_rough_PTPT2  
                                                                        ! definit le type de la liste des plus proches voisins
    INTEGER :: cd                                                       ! le candidat, l'antagoniste et isee pour la loi de contact
    INTEGER :: an
    INTEGER :: isee
    REAL(kind=8) :: nonuc0

    ! nard
    REAL(kind=8) :: surf
    REAL(kind=8) :: l0
    REAL(kind=8) :: mass

    
 END TYPE T_rough_PTPT2
 
 TYPE(T_rough_PTPT2),DIMENSION(:),ALLOCATABLE   :: rough_PTPT2          ! table  de visibilite

 TYPE T_link_rough_PTPT2                                                ! liste chainee pour determiner les listes de cand anta car
                                                                        ! on ne connait pas le nb de cand -ant a priori
    TYPE(T_link_rough_PTPT2), POINTER :: p                              ! pointeur sur le precedent
    TYPE(T_rough_PTPT2)               :: val                            ! les valeurs
    TYPE(T_link_rough_PTPT2), POINTER :: n                              ! pointeur sur le suivant

 END TYPE T_link_rough_PTPT2

 

 INTEGER                                       :: Nstep_rough_seek_PTPT2=1
 LOGICAL                                       :: write_creation_tab_visu
 

!------------------------------------------------------------------------
! variables pour le calcul des boites englobantes

 REAL (kind=8)  :: maxray, minray, maxalert, meanradius
 REAL (kind=8)  :: Lbox,LBox_1,norm
 INTEGER        :: nb_rough_PTPT2
 integer        :: nb_recup_PTPT2
 INTEGER        :: minibox1,maxibox1,minibox2,maxibox2,maxpopul
!------------------------------------------------------------------------

 REAL(kind=8) :: Reac_PTPT2_MAX=0.D0
 real(kind=8), dimension(:)  , allocatable, target :: violation
 real(kind=8), dimension(:,:), allocatable, target :: PTcoor  ! coordinates of bodies owning POINT to be used in selecting prox tactors
 real(kind=8), dimension(:,:), allocatable, target :: PTcoor0 !  reference coordinates of bodies owning POINT

!------------------------------------------------------------------------

 LOGICAL :: RUN=.FALSE.
 logical :: module_checked_ = .FALSE.
 logical :: check_PTPT2_    = .FALSE.

 LOGICAL :: given_network  = .FALSE.
 logical :: current_nonuc0 = .false.

!------------------------------------------------------------------------

 REAL(kind=8) :: ref_size=1.d0, tol=1d-4

!------------------------------------------------------------------------

! nard
 LOGICAL :: given_params = .FALSE. 
 LOGICAL :: explicit_local_frame = .FALSE.
 LOGICAL :: init_local_frame = .TRUE.
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE  :: local_frame_normal
 REAL(kind=8),DIMENSION(:),ALLOCATABLE  :: theta_ini

 
! liste des fonctions publiques 
 PUBLIC &
       stock_rloc_PTPT2, &
       recup_rloc_PTPT2, &
       compute_box_PTPT2, &
       read_ini_Vloc_Rloc_PTPT2, &
       write_xxx_Vloc_Rloc_PTPT2, &
       coor_prediction_PTPT2, &
       compute_contact_PTPT2, &
       display_prox_tactors_PTPT2, &
       RUN_PTPT2, &
       CHECK_PTPT2, &
       get_write_Vloc_Rloc_PTPT2, &
       load_network_PTPT2, &
       set_tol_PTPT2, &
       set_explicit_local_frame_PTPT2, &
       load_params_PTPT2, &
       use_current_nonuc0_PTPT2

 PUBLIC &
      nullify_reac_PTPT2, &
      nullify_vlocy_PTPT2, &
      injj_PTPT2, prjj_PTPT2, vitrad_PTPT2, &
      get_nb_PTPT2, &
!!$   get_Wik_PTPT2, compute_Wikik_PTPT2, compute_Wikjl_PTPT2, &
      PTPT22PT2Dx, &
      print_info_PTPT2, &
      get_length_PTPT2, &
      get_g2l_PTPT2, &
      get_icdtac_PTPT2, &
      get_iantac_PTPT2, &
      clean_memory_PTPT2
 
 !rm for handler
 public get_this    , &
        set_nb_PTPT2, &
        redo_nb_adj_PTPT2, &
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
  SUBROUTINE coor_prediction_PTPT2

    IMPLICIT NONE
    
    INTEGER :: ibdy,itac,itact,errare
    INTEGER :: nb_PT2Dx


    nb_PT2Dx=get_nb_PT2Dx()

    DO itact=1,nb_PT2Dx
       PTcoor(1:3,itact) = get_coorTT(itact)
    END DO

  END SUBROUTINE coor_prediction_PTPT2
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_PTPT2(step)
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
    
  end subroutine read_ini_Vloc_Rloc_PTPT2
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_PTPT2(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_PTPT2
!!!------------------------------------------------------------------------
   SUBROUTINE load_network_PTPT2
     IMPLICIT NONE

     given_network = .TRUE.

   END SUBROUTINE load_network_PTPT2
!!!------------------------------------------------------------------------
 SUBROUTINE compute_box_PTPT2

   IMPLICIT NONE

   INTEGER                          :: isee,errare,ibdy,itac,itact
   INTEGER                          :: icdan,icdtac,iantac,iadj
   CHARACTER(len=5)                 :: cdcol,ancol
   REAL(kind=8)                     :: adist,nonuc0
   REAL(kind=8),DIMENSION(2)        :: coor0cd,coor0an
   TYPE(T_link_rough_PTPT2),POINTER :: Root,Current,Previous
   INTEGER :: nb_PT2Dx

   integer :: given_fich 
   character(len=5) :: law 

                              !1234567890123456789012
   character(len=22) :: IAM = 'mod_PTPT2::compute_box'

   !nard
   integer :: i
   REAL(kind=8) :: surf
   REAL(kind=8) :: l0
   REAL(kind=8) :: mass

   
   nb_PT2Dx=get_nb_PT2Dx()

   IF (.NOT. ALLOCATED(adjac))THEN
     ALLOCATE(adjac(nb_PT2Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO ibdy=1,nb_PT2Dx
       NULLIFY(adjac(ibdy)%icdan)
     ENDDO
   ELSE
     DO ibdy=1,nb_PT2Dx
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       NULLIFY(adjac(ibdy)%icdan)
     ENDDO
   ENDIF  

   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_PT2Dx),stat=errare)
   IF (errare /=0 ) THEN
      call faterr(IAM,'Error allocating nb_adj')
   END IF

   nb_adj=0

   IF (ALLOCATED(PTcoor)) DEALLOCATE(PTcoor)
   ALLOCATE(PTcoor(3,nb_PT2Dx),stat=errare)
   
   IF (ALLOCATED(PTcoor0)) DEALLOCATE(PTcoor0)
   ALLOCATE(PTcoor0(3,nb_PT2Dx),stat=errare)

   DO itact=1,nb_PT2Dx
     PTcoor0(1:3,itact) = get_cooref(itact)
   END DO



!fd dans le cas des PTPT2 il n'y a rien de dynamique ...
!fd ce sont des liaisons cinematiques lineaires ou non lineaires

   nb_rough_PTPT2=0

   ! creation de la liste de paire a examiner
  
   ! on desalloue la liste chainee pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone memoire au fur et a mesure que l'on determine un candidat - antagoniste

   NULLIFY(Root)
   NULLIFY(Current)
   NULLIFY(Previous)


   ! nard
   !                                              123456789012345678901234567890  

   do i=1,get_nb_tact_behav() 

     IF ( tact_behav(i)%lawty .EQ. 'NARD_ROD                      ') THEN

       IF (.not. given_params) THEN

          call faterr(IAM,'NARD_ROD requires givens_params (PTPT2_LoadParams()) !!!')
  
       END IF

     END IF
   enddo 
   
   IF (given_network) THEN

      CALL LOGMES('EXTRA FILE READING')
      CALL LOGMES('LOAD PTPT2 NETWORK FROM FILE')

      !nard
      
      
      IF (given_params) CALL LOGMES('LOAD PTPT2 PARAMS FROM FILE')
      
      given_fich=get_io_unit()

      OPEN(UNIT=given_fich,file=trim(location('DATBOX/PTPT2_NETWORK.DAT')),status='OLD')

      DO 
         !nard
         IF (given_params) THEN
            READ(given_fich,*,END=99) isee,icdtac,iantac,surf,l0,mass,nonuc0
         else   
            READ(given_fich,*,END=99) isee,icdtac,iantac,nonuc0
            print *, 'read : ', isee, icdtac, iantac, nonuc0
         endif   
         nb_rough_PTPT2 = nb_rough_PTPT2 + 1
         IF (nb_rough_PTPT2 == 1) THEN
            ALLOCATE(Root)
            Current => Root
            NULLIFY(Root%p)
         ELSE
            ALLOCATE(Current)
            Previous%n => Current
         END IF

         Current%val%cd       =icdtac
         Current%val%an       =iantac
         Current%val%isee     = isee   


         !nard 
         IF (given_params) THEN
            Current%val%surf   = surf
            Current%val%l0     = l0
            Current%val%mass   = mass
         END IF
         
         if( .not. current_nonuc0 ) then
           coor0cd = PTcoor0(1:2,icdtac)
           coor0an = PTcoor0(1:2,iantac)
           nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2)
         end if
         
         Current%val%nonuc0   =nonuc0                  
         Current%p => Previous
         NULLIFY(Current%n)
         Previous => Current
         
      END DO
99    CLOSE(given_fich)

   ELSE
      ! loop investigating candidate PT2Dx
      DO icdtac=1,nb_PT2Dx
         cdcol=get_color(icdtac)
         ! loop investigating antagonist POINT
         DO iantac=icdtac+1,nb_PT2Dx
            ancol=get_color(iantac)
            isee=get_isee('RBDY2','PT2Dx',cdcol,'RBDY2','PT2Dx',ancol)

            IF (isee /= 0) THEN
               adist=see(isee)%alert 
               if( .not. current_nonuc0 ) then
                 coor0cd = PTcoor0(1:2,icdtac)
                 coor0an = PTcoor0(1:2,iantac)
               else
                 coor0cd = PTcoor(1:2,icdtac)
                 coor0an = PTcoor(1:2,iantac)
               end if
               nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2)
               
               !fd
               !fd   les deux PT2Dx se voient de maniere "statique" 
               !fd

               
                !print*,'PT2Dx',icdtac,'PT2Dx',iantac, isee
                !print*,coorcd
                !print*,cooran
                !print*,adist,nonuc0

               
               IF (nonuc0 .LE. adist) THEN
                  
                  nb_rough_PTPT2=nb_rough_PTPT2+1
                  !
                  IF (nb_rough_PTPT2 == 1) THEN
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
                  Current%val%nonuc0   =nonuc0                  
                  Current%p => Previous
                  NULLIFY(Current%n)
                  Previous => Current
               END IF
            ENDIF
         END DO
      END DO
   END IF

   IF (ALLOCATED(rough_PTPT2)) DEALLOCATE(rough_PTPT2)
   ALLOCATE(rough_PTPT2(nb_rough_PTPT2))      ! on s'alloue la table de visibilite utilisee dans compute_contact
  
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(nb_rough_PTPT2))            ! on s'alloue le tableau this surdimensionne a la taille temporaire 

   DO icdan=nb_rough_PTPT2,1,-1
      
     Previous => Current%p
     rough_PTPT2(icdan)%cd     = Current%val%cd
     rough_PTPT2(icdan)%an     = Current%val%an
     rough_PTPT2(icdan)%isee   = Current%val%isee
     rough_PTPT2(icdan)%nonuc0 = Current%val%nonuc0

     !nard
     IF (given_params) THEN
        rough_PTPT2(icdan)%surf = Current%val%surf
        rough_PTPT2(icdan)%l0   = Current%val%l0
        rough_PTPT2(icdan)%mass = Current%val%mass
     END IF
     
     DEALLOCATE(Current)
     Current => Previous
   END DO 
   
   NULLIFY(Root)
   
 END SUBROUTINE compute_box_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
 SUBROUTINE compute_contact_PTPT2
 
   IMPLICIT NONE  
   INTEGER                   :: errare, humanum, est
   INTEGER                   :: i,icdtac,iantac,isee,icdan,iadj,ibdy
   CHARACTER(len=5)          :: cdtac,cdcol,antac,ancol
   REAL(kind=8)              :: nonuc,nonuc0,gapTT
   REAL(kind=8),DIMENSION(2) :: coordcd,coordan
   REAL(kind=8),DIMENSION(3) :: cd_Vbegin,an_Vbegin
   REAL(kind=8),DIMENSION(2) :: n,t,cdlev,anlev,cd_shift,an_shift

   INTEGER                               :: cd_ent,an_ent
   INTEGER :: nb_PT2Dx

   character(len=80) :: cout
                              !12345678901234567890123456
   character(len=26) :: IAM = 'mod_PTPT2::compute_contact'

   !nard 
   REAL(kind=8) :: n_rot(2)

   

   icdan=0        
   nb_PTPT2=0

   nb_adj=0

   nb_PT2Dx=get_nb_PT2Dx()

   IF (nb_rough_PTPT2 /= 0 ) THEN

     ! nard
     IF (explicit_local_frame) THEN

       IF (.NOT. ALLOCATED(local_frame_normal)) THEN
       ALLOCATE(local_frame_normal(nb_rough_PTPT2,2),stat=errare)
       END IF

       IF (.NOT. ALLOCATED(theta_ini)) THEN
       ALLOCATE(theta_ini(nb_rough_PTPT2),stat=errare)
       END IF

     END IF
      
     DO i=1,nb_rough_PTPT2
       icdtac=rough_PTPT2(i)%cd
       iantac=rough_PTPT2(i)%an
       isee=rough_PTPT2(i)%isee  
       nonuc0 = rough_PTPT2(i)%nonuc0
!fd 
!fd    coordonnees des PT2Dx (dans R0) en tenant compte du shift
!fd 
       coordcd = PTcoor(1:2,icdtac)
       coordan = PTcoor(1:2,iantac)

       nonuc =dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)

       icdan=icdan+1
       nb_adj(icdtac)=nb_adj(icdtac)+1
       iadj=nb_adj(icdtac)

!fd 
!fd la vitesse du centre d'inertie du RBDY2 auquel est acroche le PT2Dx
!fd

       cd_Vbegin = get_Vbegin(icdtac)
       an_Vbegin = get_Vbegin(iantac)

       this(icdan)%icdbtac = pt2dx2bdyty(2, icdtac)
       this(icdan)%ianbtac = pt2dx2bdyty(2, iantac)

       this(icdan)%icdbtyp = pt2dx2bdyty(3, icdtac)
       this(icdan)%ianbtyp = pt2dx2bdyty(3, iantac)

       this(icdan)%icdctyp = i_pt2dx
       this(icdan)%ianctyp = i_pt2dx

       this(icdan)%iadj    = iadj
       this(icdan)%icdbdy  = pt2dx2bdyty(1, icdtac)
       this(icdan)%icdtac  = icdtac
       this(icdan)%icdsci  = 0
       this(icdan)%ianbdy  = pt2dx2bdyty(1, iantac)
       this(icdan)%iantac  = iantac
       this(icdan)%iansci  = 0
       this(icdan)%isee    = isee


       call get_behaviour_( icdan, see, tact_behav )

       cd_ent = get_ent(this(icdan)%icdtac)
       an_ent = get_ent(this(icdan)%iantac) 

       this(icdan)%icdent = cd_ent
       this(icdan)%ianent = an_ent

       if (cd_ent /= an_ent) then
         entity(cd_ent)%nb = entity(cd_ent)%nb+1
         entity(an_ent)%nb = entity(an_ent)%nb+1
       else
         entity(cd_ent)%nb = entity(cd_ent)%nb+1
       end if

       !fd 
       !fd on peut avoir 2 cas:
       !fd  *points colles (nonuc0 = 0) c'est pour un texsol
       !fd  *points non colles (nonuc0 /= 0) c'est pour visco-elas qqch
       !fd

       !print*,'-----'
       !print*,icdan,nonuc0,nonuc

       IF ( nonuc0 .LT. 1.d-14*ref_size) THEN

          !                                              123456789012345678901234567890
          IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'NARD_ROD                      ') call faterr(IAM,'IQS_AN_CLB ne sait pas traiter les cas de noeuds superposes!!!')

          !print*,'glued'

          !                                              123456789012345678901234567890
          IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'BROKEN_DOF                    ') THEN
             
             this(icdan)%tuc(:)      =  (/ 1.d0, 0.d0 /)
             this(icdan)%nuc(:)      =  (/ 0.d0, 1.d0 /)
             this(icdan)%gapTTbegin  =  0.d0
             this(icdan)%nonuc0      =  0.d0
             
          ELSE  

             IF (nonuc > tol*ref_size) THEN
                call faterr(IAM,'On a 2 noeuds decolles dans une situation de noeuds colles')
             ENDIF
          
             !fd Pour les lois sachant quoi faire on laisse faire: 
             !fd COUPLED_DOF        
             !fd TEX_SOL
             !                                            123456789012345678901234567890
             IF ( tact_behav(this(icdan)%lawnb)%lawty /= 'COUPLED_DOF                   ' .AND. &
                  tact_behav(this(icdan)%lawnb)%lawty /= 'TEX_SOL                       ' &
                  ) THEN
                call faterr(IAM,'The 2 PT2Dx are at the position which it not managed by the interaction law')
             ENDIF
             
             !fd cas de noeuds colles 
             
             this(icdan)%tuc(:)      =  (/ 1.d0, 0.d0 /)
             this(icdan)%nuc(:)      =  (/ 0.d0, 1.d0 /)
             this(icdan)%gapTTbegin  =  0.d0
             this(icdan)%nonuc0      =  0.d0
             
             this(icdan)%internal    = 0.d0

          END IF

       ELSE

          !print*,'not glued'
          
          IF (nonuc < 1d-14*ref_size) THEN
             call faterr(IAM,'The 2 PT2Dx are at the same position which is not possible') 
          ENDIF
          
          IF (explicit_local_frame) THEN

             IF (init_local_frame) THEN

                ! calcul repere local                                                                                                                                                 

                this(icdan)%nuc(:) =  (coordcd(1:2)-coordan(1:2))/nonuc
                this(icdan)%tuc(1) = this(icdan)%nuc(2)
                this(icdan)%tuc(2) =-this(icdan)%nuc(1)

                theta_ini(i) = PTcoor(3,iantac)

                ! on stock la normale                                                                                                                                                 

                local_frame_normal(i,:) = this(icdan)%nuc(:)

                this(icdan)%gapTTBEGIN  =  nonuc

                this(icdan)%nonuc0 = nonuc0

                ! on initialise deplacement tangent a 0? pour iqs_an uniquement                                                                                                       
                !                                              123456789012345678901234567890
                IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'NARD_ROD                      ') THEN

                   this(icdan)%internal(2) = 0.d0 ! pas utile si on le fait dans le solveur                                                                                           
                END IF

             ELSE

                n(:) = local_frame_normal(i,:)

                ! on applique la rotation du corps                                                                                                                                    

                ! on recup la rotation                                                                                                                                                

                theta = PTcoor(3,iantac)-theta_ini(i) !??? probleme si on redemarre a partir du dof                                                                                   

                ! on peut lire le theta 0 et le retrancher                                                                                                                            

                ! on applique une petite matrice de rotation qui va bien                                                                                                              

                n_rot(1) = cos(theta)*n(1) - sin(theta)*n(2)
                n_rot(2) = sin(theta)*n(1) + cos(theta)*n(2)

                t(1)=n_rot(2);t(2)=-n_rot(1)

                ! on calcul le gap : projette vecteur reliant les points sur la normale du repere                                                                                     

                gapTT = n_rot(1)*(coordcd(1)-coordan(1)) + n_rot(2)*(coordcd(2)-coordan(2))

                this(icdan)%gapTTBEGIN  = gapTT

                this(icdan)%nuc(:)  = n_rot(:)
                this(icdan)%tuc(1)= this(icdan)%nuc(2)
                this(icdan)%tuc(2)=-this(icdan)%nuc(1)

                this(icdan)%nonuc0 = nonuc0 !???                                                                                                                                      
                !                                              123456789012345678901234567890
                IF ( tact_behav(this(icdan)%lawnb)%lawty .EQ. 'NARD_ROD                      ') THEN

                   ! on calcul le deplacement tangent = projection sur tuc                                                                                                            

                   this(icdan)%internal(2) = t(1)*(coordcd(1)-coordan(1)) + t(2)*(coordcd(2)-coordan(2))

                END IF

             END IF

          else


            this(icdan)%nuc(:)      =  (coordcd(1:2)-coordan(1:2))/nonuc
            this(icdan)%tuc(1)= this(icdan)%nuc(2)
            this(icdan)%tuc(2)=-this(icdan)%nuc(1)
            gapTT=nonuc
            this(icdan)%gapTTBEGIN  =  gapTT
            this(icdan)%nonuc0      =  nonuc0
          
            this(icdan)%internal    = 0.d0

          endif 

          ! nard
          IF (given_params) THEN
             this(icdan)%internal(4) = rough_PTPT2(i)%surf
             this(icdan)%internal(5) = rough_PTPT2(i)%l0
             this(icdan)%internal(6) = rough_PTPT2(i)%mass
          END IF
          
            
          !fd attention a le gestion du internal qui est plutot qqch propre au solveur
          !fd cette valeur est ecrasee par ce qui est contenu dans le verlet lorsque qu'on fait
          !fd un RECUP Rloc donc si on veut initialiser qqch il faut penser a forcer le STOCK Rloc     
          
          !123456789012345678901234567890
          IF (tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_ROD                   ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_ROD                     ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_WIRE                  ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_WIRE                    ' .OR. &
               tact_behav(this(icdan)%lawnb)%lawty == 'TEX_SOL_UNILAT                ' &
               ) THEN
             
             this(icdan)%internal(1) = nonuc0
             
          ENDIF
          
       END IF
    

       cdlev= get_shiftTT(icdtac)
       anlev= get_shiftTT(iantac)
         
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


       this(icdan)%rlt       = 0.D0
       this(icdan)%rln       = 0.D0
       this(icdan)%vlt       = this(icdan)%vltBEGIN
       this(icdan)%vln       = this(icdan)%vlnBEGIN
       this(icdan)%gapTT     = this(icdan)%gapTTBEGIN
       this(icdan)%status    = i_nknow

       this(icdan)%coor(1:2) = 0.5 * ( PTcoor(1:2, icdtac) + PTcoor(1:2, iantac) )

     ENDDO
     nb_PTPT2=icdan

     ! nard
     IF (init_local_frame) init_local_frame = .FALSE.
     
   END IF 

  WRITE(cout,'(1X,I10,A12)') nb_PTPT2,' PTPT2 found'
  call logmes(cout)

  ! Since selection of candidates for contact has been refined, nb_PTPT2 is less or equal size(this). 
  ! Loops are now to run from 1 to nb_PTPT2 where data are available.

   nb_pt2dx = get_nb_pt2dx()

   DO ibdy=1,nb_PT2Dx
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'error allocating adjac(',ibdy,')%.....'
         call faterr(IAM,cout)
       END IF
     ELSE 
       NULLIFY(adjac(ibdy)%icdan)
     ENDIF
!
   ENDDO
  
   DO icdan=1,nb_PTPT2
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   END DO

!fd 21/02/04 out la mierda   call get_behaviour
     
   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_PTPT2),stat=errare)

 END SUBROUTINE compute_contact_PTPT2
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine display_prox_tactors_PTPT2

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   integer          :: nb_PT2Dx
   character(len=5) :: cdmodel, anmodel


   nb_PT2Dx=get_nb_PT2Dx()

   DO icdtact=1,nb_PT2Dx
     DO iadj=1,nb_adj(icdtact)
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( pt2dx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( pt2dx2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan
                         !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,pt2dx2bdyty(1,icdtac),'PT2Dx',pt2dx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,pt2dx2bdyty(1,iantac),'PT2Dx',pt2dx2bdyty(2,iantac)
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.d0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.d0
       WRITE(*,104)'t(3)=',0.d0              ,'n(3)=',0.d0              ,'s(3)=',0.d0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,2X,A5,D14.7)')'gapTT-=',this(icdan)%gapTTbegin
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))

 END SUBROUTINE display_prox_tactors_PTPT2
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_PTPT2
 
   
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare
   INTEGER :: nb_PT2Dx

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_PTPT2::stock_rloc'

   nb_PT2Dx=get_nb_PT2Dx()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT2Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_PT2Dx
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
     DO icdtac=1,nb_PT2Dx
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
   DO icdan=1,nb_PTPT2
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac
     ! serial adjacent number of pair contactor
     ! adjacent to candidate contactor for contact icdan
     iadj   = this(icdan)%iadj
     verlt(icdtac)%icdan(iadj)   = icdan
     verlt(icdtac)%cdbdy         = pt2dx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac         = pt2dx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel       = pt2dx2bdyty(3,icdtac)
     verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)   = pt2dx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)   = pt2dx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj) = pt2dx2bdyty(3,iantac)
     verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
     verlt(icdtac)%status(iadj)  = this(icdan)%status
     verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
     verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)     = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT
     verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
!fd
!fd a voir cette histoire de definition du point d'application de la force
!fd
     verlt(icdtac)%coor(1:2,iadj)= this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)
   END DO
   nb_vPTPT2 = nb_PTPT2
   WRITE(cout,'(1X,I10,A12)') nb_vPTPT2,' stock PTPT2'
   call logmes(cout)

 END SUBROUTINE stock_rloc_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_PTPT2

   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   CHARACTER(len=21) :: IAM = 'mod_PTPT2::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_PTPT2 =0 

   DO icdan=1,nb_PTPT2
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     ! serial number of candidate contactor for contact icdan
     icdtac = this(icdan)%icdtac
     ! serial number of antagonist contactor for contact icdan
     iantac = this(icdan)%iantac

     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdbdy  == pt2dx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == pt2dx2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== pt2dx2bdyty(3,icdtac)       &
          ) then
          do iadj = 1 , verlt(icdtac)%adjsz
            IF (                                           &
                verlt(icdtac)%anbdy(iadj)  == pt2dx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == pt2dx2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== pt2dx2bdyty(3,iantac)       &
               ) then
               this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
               this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

               this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
               nb_recup_PTPT2 = nb_recup_PTPT2 +1
              exit
            end if
          end do
       end if
     ENDIF
   END DO
   
   WRITE(cout,'(1X,I10,A12)') nb_recup_PTPT2,' recup PTPT2'
   call logmes(cout)

 END SUBROUTINE recup_rloc_PTPT2
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE
   INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac
   INTEGER            :: iadj,icdtact,cdmodel,anmodel
   REAL(kind=8)       :: rlt,rln,vlt,vln,gapTT
   REAL(kind=8),DIMENSION(2) :: nuc,coor
   CHARACTER(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER            :: errare
   
   INTEGER :: ibehav,nb_internal,i_internal
   INTEGER :: nb_PT2Dx

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_PTPT2::read_ini_Vloc_Rloc'


   nb_PT2DX=get_nb_PT2DX()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is a record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.
   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_PT2DX),stat=errare)
     IF (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   END IF    

   nb_adj=0
   DO    
     IF ( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'PTPT2') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus
     IF (cdtac /= 'PT2Dx' .OR. antac /= 'PT2Dx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )
     do icdtact = 1, nb_PT2Dx
       if (pt2dx2bdyty(1,icdtact) == icdbdy .and. &
           pt2dx2bdyty(2,icdtact) == icdtac .and. &
           pt2dx2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact)=nb_adj(icdtact)+1       
         exit
       end if
     end do
     CYCLE
   END DO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT2Dx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtac=1,nb_PT2Dx
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
     DO icdtac=1,nb_PT2Dx
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
     IF (G_clin(9:13)/= 'PTPT2') CYCLE     
     IF ( .NOT. read_G_clin()) EXIT
     IF ( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
                      cdbdy,icdbdy,cdtac,icdtac,                                          &
                      behav,                                                              &
                      anbdy,ianbdy,antac,iantac,                                          &
                      sttus
     IF (cdtac /= 'PT2Dx' .OR. antac /= 'PT2Dx') CYCLE
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     do icdtact = 1, nb_PT2Dx
       if (pt2dx2bdyty(1,icdtact) == icdbdy .and. &
           pt2dx2bdyty(2,icdtact) == icdtac .and. &
           pt2dx2bdyty(3,icdtact) == cdmodel ) then
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
           READ(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
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

   nb_vPTPT2=0

   DO icdtact=1,nb_PT2Dx
     nb_vPTPT2 = nb_vPTPT2 + nb_adj(icdtact)

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
   INTEGER :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nfich,icdtact
   INTEGER :: lc
   REAL(kind=8),DIMENSION(2) :: coor
   integer :: nb_pt2Dx

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_PT2DX=get_nb_PT2DX()

   DO icdtact=1,nb_PT2DX    
     DO iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       ! coordinates of the contact point if contact is active
       !fd tient compte du shift

       coor(1) = (PTcoor(1,icdtac) + PTcoor(1,iantac))*0.5
       coor(2) = (PTcoor(2,icdtac) + PTcoor(2,iantac))*0.5

       cdmodel = get_body_model_name_from_id( pt2dx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( pt2dx2bdyty(3,iantac) )

       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','PTPT2',icdan     
                             !1234567890123456789012345678901234567890123456789012345678901234567890123456  
       WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'      
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,pt2dx2bdyty(1,icdtac),'PT2Dx',pt2dx2bdyty(2,icdtac),  &
       see(this(icdan)%isee)%behav,  &
       anmodel,pt2dx2bdyty(1,iantac),'PT2Dx',pt2dx2bdyty(2,iantac),  &
       get_contact_status_name_from_id(this(icdan)%status),iantac
       WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H ,'rln/H',this(icdan)%rln/H ,'rls/H',0.D0
       WRITE(nfich,104)'vlt =',this(icdan)%vlt   ,'vln =',this(icdan)%vln   ,'vls =',0.D0
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
 SUBROUTINE nullify_reac_PTPT2(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: storage
    
   CALL nullify_reac(this(icdan)%icdtac,storage)
   
   CALL nullify_reac(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_reac_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_PTPT2(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: storage
    
   CALL nullify_vlocy(this(icdan)%icdtac,storage)
   
   CALL nullify_vlocy(this(icdan)%iantac,storage)
    
 END SUBROUTINE nullify_vlocy_PTPT2
!------------------------------------------------------------------------ 

!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_PTPT2( icdan, storage, need_full_V )

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: storage
   logical, optional  :: need_full_V
    

  CALL comp_vlocy(this(icdan)%icdtac,storage)
    
  CALL comp_vlocy(this(icdan)%iantac,storage)
    
 END SUBROUTINE vitrad_PTPT2
!------------------------------------------------------------------------  

!------------------------------------------------------------------------  
 SUBROUTINE injj_PTPT2(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in) :: icdan
   REAL(kind=8),INTENT(in) :: RTIK,RNIK
   INTEGER,     DIMENSION(3)  :: cdccdof,anccdof
   REAL(kind=8),DIMENSION(3)  :: cdreac, anreac
   INTEGER :: storage
   
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

   CALL add_reac(this(icdan)%icdtac,cdccdof,cdreac,storage)
   CALL add_reac(this(icdan)%iantac,anccdof,anreac,storage)

 END SUBROUTINE injj_PTPT2 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_PTPT2(icdan,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VTIK,VNIK
   INTEGER     ,intent(in)   :: storage
   REAL(kind=8),DIMENSION(3) :: Vcd,Van
   
   CALL get_vlocy(this(icdan)%icdtac,storage,Vcd)
   CALL get_vlocy(this(icdan)%iantac,storage,Van)     

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2)+Vcd(3)*this(icdan)%Gcdt3  &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3

   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2)+Vcd(3)*this(icdan)%Gcdn3 &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3


 END SUBROUTINE prjj_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_nb_PTPT2(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_PTPT2 = nb_PTPT2
   case(i_verlet_tactor)
      get_nb_PTPT2 = nb_vPTPT2
   case(i_rough_tactor)
      get_nb_PTPT2 = nb_rough_PTPT2
   case(i_recup_tactor)
      get_nb_PTPT2 = nb_recup_PTPT2
   end select

 end function get_nb_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE PTPT22PT2Dx(icdan,icdtac,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,icdtac,iantac
   
   icdtac = this(icdan)%icdtac
   iantac = this(icdan)%iantac

END SUBROUTINE PTPT22PT2Dx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE compute_Wikik_PTPT2(icdan,WTT,WTN,WNT,WNN)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,icdbdy,ianbdy
!!$  REAL(kind=8)              :: WTT,WTN,WNT,WNN
!!$  REAL(kind=8),DIMENSION(3) :: icdmass,ianmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$
!!$!  icdmass = get_inv_mass_DISKx(icdbdy)
!!$!  ianmass = get_inv_mass_DISKx(ianbdy)
!!$
!!$  icdmass = 0.D0
!!$  ianmass = 0.D0
!!$
!!$  WTT =  icdmass(1)+icdmass(3)*this(icdan)%Gcdt3*this(icdan)%Gcdt3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gant3*this(icdan)%Gant3
!!$  WNN =  icdmass(1)+icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdn3 &
!!$       + ianmass(1)+ianmass(3)*this(icdan)%Gann3*this(icdan)%Gann3
!!$  WTN =  icdmass(3)*this(icdan)%Gcdn3*this(icdan)%Gcdt3 &
!!$       + ianmass(3)*this(icdan)%Gann3*this(icdan)%Gant3
!!$  WNT = WTN
!!$
!!$ END SUBROUTINE compute_Wikik_PTPT2
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE get_Wik_PTPT2(icdan,ikcd,ikan,tik,nik,ikcdmass,ikanmass,ikGcdt,ikGcdn,ikGant,ikGann)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,ikcd,ikan
!!$  REAL(kind=8)              :: ikGcdt,ikGcdn,ikGant,ikGann
!!$  REAL(kind=8),DIMENSION(3) :: ikcdmass,ikanmass
!!$  REAL(kind=8),DIMENSION(2) :: tik,nik
!!$
!!$  ikcd    = this(icdan)%icdbdy
!!$  ikan    = this(icdan)%ianbdy
!!$  ikGcdt  = this(icdan)%Gcdt3
!!$  ikGcdn  = this(icdan)%Gcdn3
!!$  ikGant  = this(icdan)%Gant3
!!$  ikGann  = this(icdan)%Gann3
!!$!  ikcdmass= get_inv_mass_DISKx(ikcd)
!!$!  ikanmass= get_inv_mass_DISKx(ikan)
!!$  tik     = this(icdan)%tuc
!!$  nik     = this(icdan)%nuc
!!$
!!$ END SUBROUTINE get_Wik_PTPT2
!!$!------------------------------------------------------------------------ 
!!$ SUBROUTINE compute_Wikjl_PTPT2(icdan,jcdan,WTT,WTN,WNT,WNN)
!!$
!!$  IMPLICIT NONE
!!$  INTEGER                   :: icdan,jcdan,icdbdy,ianbdy,jcdbdy,janbdy
!!$  REAL(kind=8)              :: WTT,WTN,WNT,WNN
!!$  REAL(kind=8),DIMENSION(3) :: icdmass,ianmass,jcdmass,janmass
!!$
!!$  icdbdy=this(icdan)%icdbdy
!!$  ianbdy=this(icdan)%ianbdy
!!$  jcdbdy=this(jcdan)%icdbdy
!!$  janbdy=this(jcdan)%ianbdy
!!$
!!$!  icdmass = get_inv_mass_DISKx(icdbdy)
!!$!  ianmass = get_inv_mass_DISKx(ianbdy)
!!$!  jcdmass = get_inv_mass_DISKx(jcdbdy)
!!$!  janmass = get_inv_mass_DISKx(janbdy)
!!$
!!$
!!$   icdmass = 0.d0
!!$   ianmass = 0.d0
!!$!cas ik-il
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
!!$!cas ik-jk
!!$  ELSEIF (ianbdy == janbdy) THEN
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
!!$!cas ik-kl
!!$  ELSEIF (ianbdy == jcdbdy) THEN
!!$
!!$    WTT = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdt3
!!$
!!$    WTN = -ianmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gant3*this(jcdan)%Gcdn3
!!$
!!$    WNT = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdt3
!!$
!!$    WNN = -ianmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          -ianmass(3)*this(icdan)%Gann3*this(jcdan)%Gcdn3
!!$  ELSE
!!$!cas ik-ji
!!$
!!$    WTT = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%tuc(1)+this(icdan)%tuc(2)*this(jcdan)%tuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gant3
!!$
!!$    WTN = -icdmass(1)*(this(icdan)%tuc(1)*this(jcdan)%nuc(1)+this(icdan)%tuc(2)*this(jcdan)%nuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdt3*this(jcdan)%Gann3
!!$
!!$    WNT = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%tuc(1)+this(icdan)%nuc(2)*this(jcdan)%tuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gant3
!!$
!!$    WNN = -icdmass(1)*(this(icdan)%nuc(1)*this(jcdan)%nuc(1)+this(icdan)%nuc(2)*this(jcdan)%nuc(2)) &
!!$          -icdmass(3)*this(icdan)%Gcdn3*this(jcdan)%Gann3
!!$  ENDIF
!!$
!!$ END SUBROUTINE compute_Wikjl_PTPT2
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE print_info_PTPT2(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'PT2Dx:',1x,I5,1x,'PT2Dx:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

   CALL print_info_PT2Dx(icdbdy)
   CALL print_info_PT2Dx(ianbdy)

END SUBROUTINE print_info_PTPT2
!------------------------------------------------------------------------
real(kind=8) function get_length_PTPT2(icdan)
  implicit none
  !
  integer(kind=4), intent(in) :: icdan 

  get_length_PTPT2 = 1.d0
  
end function get_length_PTPT2
!------------------------------------------------------------------------ 
LOGICAL FUNCTION RUN_PTPT2(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_PTPT2 = RUN_TACTOR

END FUNCTION RUN_PTPT2
!------------------------------------------------------------------------
  logical function CHECK_PTPT2()
    implicit none
    !   
    integer :: isee, nb_PT2Dx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_PTPT2 = check_PTPT2_
      return
    end if

    con_pedigree%module_name = 'PTPT2'

    con_pedigree%id_cdan  = i_ptpt2
    con_pedigree%id_cdtac = i_pt2dx
    con_pedigree%id_antac = i_pt2dx

    cdtact2bdyty => pt2dx2bdyty
    antact2bdyty => pt2dx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_PT2Dx = get_nb_PT2Dx()
    if( nb_PT2Dx < 2 ) then
      CHECK_PTPT2 = check_PTPT2_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'PT2Dx' .and. see(isee)%antac == 'PT2Dx') then
        check_PTPT2_ = .true.
        exit
      end if
    end do
  
    CHECK_PTPT2 = check_PTPT2_
    return
  
  end function CHECK_PTPT2
!------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_Vloc_Rloc_PTPT2(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_PTPT2 = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_PTPT2
!------------------------------------------------------------------------
 SUBROUTINE get_g2l_PTPT2(icdan,g2l)

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
   
 END SUBROUTINE get_g2l_PTPT2
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 function get_icdtac_PTPT2(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_PTPT2
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_PT2Dx
   logical :: found

   found = .false.

   nb_PT2Dx = get_nb_PT2Dx()

   icc = 0
   do icdtac = 1, nb_PT2Dx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_PTPT2 = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PTPT2::get_icdtac','unknown contact index')
   
 end function
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 function get_iantac_PTPT2(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_PTPT2
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_PT2Dx
   logical :: found

   found = .false.

   nb_PT2Dx = get_nb_PT2Dx()

   icc = 0
   do icdtac = 1, nb_PT2Dx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_PTPT2 =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('PTPT2::get_icdtac','unknown contact index')
   

   get_iantac_PTPT2 = this(icdan)%iantac

 end function get_iantac_PTPT2

 !!!------------------------------------------------------------------------ 
  SUBROUTINE set_tol_PTPT2(val)

    IMPLICIT NONE
    REAL(kind=8) :: val

    tol = val

  END SUBROUTINE set_tol_PTPT2
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine clean_memory_PTPT2
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_PTPT2  = 0
   nb_vPTPT2 = 0

   if( allocated(box) ) then
     do j = lbound(box,2), ubound(box,2)
       do i = lbound(box,1), ubound(box,1)
         if( associated(box(i,j)%which) ) deallocate(box(i,j)%which)
       end do
     end do
     deallocate(box)
   end if

   if( allocated(rough_PTPT2) ) deallocate(rough_PTPT2)

   nb_rough_PTPT2 = 0
   nstep_rough_seek_PTPT2 = 1
   nb_recup_PTPT2 = 0

   RUN = .false.

   if( allocated(PTcoor) ) deallocate(PTcoor)
   if( allocated(PTcoor0)) deallocate(PTcoor0)

   Reac_PTPT2_MAX = 0.D0

   module_checked_ = .FALSE.
   check_PTPT2_    = .FALSE.

 end subroutine

!!!-----------------------------------------------------------------------
 
 subroutine set_nb_PTPT2(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_PTPT2 = nb

 end subroutine

!!!-----------------------------------------------------------------------
 
 subroutine redo_nb_adj_PTPT2()
   implicit none

   call redo_nb_adj_( get_nb_PT2Dx() )

   ! because PTcoor is needed in this case
   ! to write vloc_rloc
   if (allocated(PTcoor)) deallocate(PTcoor)
   allocate( PTcoor( 3, get_nb_PT2Dx() ) )
   call coor_prediction_PTPT2()

 end subroutine

!!!-----------------------------------------------------------------------
 
 subroutine load_params_PTPT2()
   IMPLICIT NONE

   given_params = .TRUE.

 END SUBROUTINE load_params_PTPT2

 subroutine use_current_nonuc0_PTPT2(no0)
   implicit none
   integer, intent(in) :: no0

   if( no0 == 0 ) then
     current_nonuc0 = .false.
   else
     current_nonuc0 = .true.
   end if

 end subroutine use_current_nonuc0_PTPT2

!!!-----------------------------------------------------------------------
 SUBROUTINE set_explicit_local_frame_PTPT2

   IMPLICIT NONE
      
   explicit_local_frame = .TRUE.

 END SUBROUTINE set_explicit_local_frame_PTPT2

 
END MODULE PTPT2

