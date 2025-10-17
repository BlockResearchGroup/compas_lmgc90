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
MODULE P2P2L    

  USE overall
  USE tact_behaviour
  USE JONCx
  USE PT2DL

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use parameters, only : i_p2p2l, i_pt2dl, i_mailx, i_rbdy2, i_mbs2

  use inter_meca_2D

  implicit none

  private

  type(T_interaction), dimension(:), allocatable, target :: this


  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!------------------------------------------------------------------------ 

 INTEGER          :: nb_P2P2L                  ! nb_P2P2L = number of selected candidates PT2DL against PT2DL
                                               ! <= size(this).

 INTEGER          :: nb_vP2P2L=0
 integer          :: nb_recup_P2P2L = 0

!------------------------------------------------------------------------ 

 ! TYPE T_this_adjac

 !   PRIVATE
 !   INTEGER,DIMENSION(:),POINTER          :: icdan     ! adjac(icdtac)%icdan(iadj):
 !                                                      ! serial number in this for adjacent contactor iadj
 !                                                      ! to candidate contactor icdtac.
 !                                                      ! For the definition of adjacent see below in 
 !                                                      ! type T_verlt.
 !                                                      ! When performing stock_rloc, verlt type is filled in
 !                                                      ! according to adjac order, i.e.
 !                                                      ! verlt(icdtac)%icdan(iadj)=adjac(icdtac)%icdan(iadj):
 ! END TYPE T_this_adjac

 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs PT2DL-PT2DL
                                                         ! to candidate contactor PT2DL icdtac.

!------------------------------------------------------------------------

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------

TYPE T_rough_P2P2L

                                                  ! définit le type de la liste des plus proches voisins
    INTEGER :: cd                                 ! le candidat, l'antagoniste et isee pour la loi de contact
    INTEGER :: an
    INTEGER :: isee
    REAL(kind=8) :: nonuc0

END TYPE T_rough_P2P2L

TYPE(T_rough_P2P2L),DIMENSION(:),ALLOCATABLE :: rough_P2P2L

   TYPE T_link_rough_P2P2L                        ! liste chainée pour déterminer les listes de cand anta car
                                                  ! on ne connait pas le nb de cand -ant à priori
     TYPE(T_link_rough_P2P2L), POINTER :: p       ! pointeur sur le precedent
     TYPE(T_rough_P2P2L)               :: val     ! les valeurs
     TYPE(T_link_rough_P2P2L), POINTER :: n       ! pointeur sur le suivant

   END TYPE T_link_rough_P2P2L

INTEGER :: nb_rough_P2P2L

!------------------------------------------------------------------------ 

 real(kind=8) :: Reac_P2P2L_MAX=0.D0
 real(kind=8), dimension(:)  , allocatable, target :: violation
 real(kind=8), dimension(:,:), allocatable         :: PTcoor,PTcoor0 !  coordinates of PT2DL candidate and antagoniste
 
 logical      :: module_checked_ = .FALSE.
 logical      :: check_P2P2L_    = .FALSE.

!------------------------------------------------------------------------

! liste des fonctions publiques 
 PUBLIC &
      stock_rloc_P2P2L, &
      recup_rloc_P2P2L, &
      compute_box_P2P2L, &
      read_ini_Vloc_Rloc_P2P2L, &
      write_xxx_Vloc_Rloc_P2P2L, &
      coor_prediction_P2P2L, &
      compute_contact_P2P2L, &
      display_prox_tactors_P2P2L, &
      RUN_P2P2L, &
      CHECK_P2P2L, &
      get_write_Vloc_Rloc_P2P2L

 PUBLIC &
      nullify_reac_P2P2L, &
      nullify_vlocy_P2P2L, &
      injj_P2P2L, prjj_P2P2L, vitrad_P2P2L, & 
      get_nb_P2P2L, &
      print_info_P2P2L, &
      get_length_P2P2L, &
      get_icdtac_P2P2L, &
      get_iantac_P2P2L, &
      clean_memory_P2P2L

 !rm for handler
 public get_this    , &
        set_nb_P2P2L, &
        redo_nb_adj_P2P2L, &
        get_an_tacty     , &
        get_verlet_tact_lawnb

!------------------------------------------------------------------------  

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
! The following are standard chic commands 
!------------------------------------------------------------------------
 SUBROUTINE coor_prediction_P2P2L

   IMPLICIT NONE

   INTEGER :: ipt2dl
   INTEGER :: nb_PT2DL
 
   nb_PT2DL= get_nb_PT2DL()

   DO ipt2dl=1,nb_PT2DL

      PTcoor(1:2,ipt2dl) = get_coorTT_PT2DL(ipt2dl)

   END DO  

 END SUBROUTINE coor_prediction_P2P2L
!!!------------------------------------------------------------------------
 !> \brief Read a VlocRloc file to initialize database
 subroutine read_ini_Vloc_Rloc_P2P2L(step)
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
   
 end subroutine read_ini_Vloc_Rloc_P2P2L
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_P2P2L(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_P2P2L
!------------------------------------------------------------------------
SUBROUTINE compute_box_P2P2L

   IMPLICIT NONE

   INTEGER :: errare,isee
   INTEGER :: itact,ipt

   INTEGER :: icdan,icdtac,iantac,iadj
   CHARACTER(len=5) :: cdcol,ancol

   REAL(kind=8)              :: adist,nonuc0
   REAL(kind=8),DIMENSION(2) :: coor0cd,coor0an
   TYPE(T_link_rough_P2P2L),POINTER              :: Root,Current,Previous

   integer :: nb_pt2Dl

                              !1234567890123456789012
   character(len=22) :: IAM = 'mod_P2P2L::compute_box'

   nb_PT2DL=get_nb_PT2DL()

   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_PT2DL),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating nb_adj')
   END IF    

   nb_adj=0
        
   IF (.NOT. ALLOCATED(adjac)) THEN
     ALLOCATE(adjac(nb_PT2DL),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO itact=1,nb_PT2DL
       NULLIFY(adjac(itact)%icdan)

!!$       ! on gagne du temps dans les P2-P2 
!!$       ! car on n'aura tjs qu'un seul antagoniste pour un PT2DL (?!)
!!$
!!$       ALLOCATE(adjac(itact)%icdan(1))
     END DO
   ELSE
     DO itact=1,nb_PT2DL
       IF (ASSOCIATED(adjac(itact)%icdan))  DEALLOCATE(adjac(itact)%icdan)
!!$       NULLIFY(adjac(itact)%icdan)
!!$       !
!!$       ! pour gagner du temps ... un PT2DL ne vera jamais qu'un iantac
!!$       !
!!$       ALLOCATE(adjac(itact)%icdan(1))
     ENDDO
   ENDIF  
  
   IF (ALLOCATED(PTcoor)) DEALLOCATE(PTcoor)
   ALLOCATE(PTcoor(2,nb_PT2DL),stat=errare)   
   IF (ALLOCATED(PTcoor0)) DEALLOCATE(PTcoor0)
   ALLOCATE(PTcoor0(2,nb_PT2DL),stat=errare)

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating PT_coor')
   END IF    

   DO ipt=1,nb_PT2DL
     PTcoor0(1:2,ipt) = get_coorefTT_PT2DL(ipt)
   END DO  


!fd dans le cas des P2P2L il n'y a rien de dynamique ...
!fd ce sont des liaisons cinematiques lineaires ou non linéaires

   nb_rough_p2p2l=0

   ! création de la liste de paire à examiner
  
   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

   NULLIFY(Root)
   NULLIFY(Current)
   NULLIFY(Previous)

   ! loop investigating candidate PT2DL
   DO icdtac=1,nb_PT2DL
     cdcol=get_color_MAILx(pt2dl2bdyty(1,icdtac),pt2dl2bdyty(2,icdtac))
     ! loop investigating antagonist POINT
     DO iantac=icdtac+1,nb_PT2DL
       ancol=get_color_MAILx(pt2dl2bdyty(1,iantac),pt2dl2bdyty(2,iantac))
       isee=get_isee('MAILx','PT2DL',cdcol,'MAILx','PT2DL',ancol)
       IF (isee /= 0) THEN
         adist=see(isee)%alert 
         coor0cd = PTcoor0(1:2,icdtac)
         coor0an = PTcoor0(1:2,iantac)
         nonuc0=dsqrt((coor0cd(1)-coor0an(1))**2+(coor0cd(2)-coor0an(2))**2)
!fd
!fd   les deux PT2DL se voient de maniere "statique" 
!fd
         IF (nonuc0 .LE. adist) THEN
           nb_rough_p2p2l=nb_rough_p2p2l+1
         !
           IF (nb_rough_p2p2l == 1) THEN
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

   IF (ALLOCATED(rough_P2P2L)) DEALLOCATE(rough_P2P2L)
   ALLOCATE(rough_P2P2L(nb_rough_p2p2l))      ! on s'alloue la table de visibilité utilisée dans compute_contact
  
   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(nb_rough_p2p2l))            ! on s'alloue le tableau this surdimensionne a la taille temporaire 

   DO icdan=nb_rough_p2p2l,1,-1
      
     Previous => Current%p
     rough_P2P2L(icdan)%cd     = Current%val%cd
     rough_P2P2L(icdan)%an     = Current%val%an
     rough_P2P2L(icdan)%isee   = Current%val%isee
     rough_P2P2L(icdan)%nonuc0 = Current%val%nonuc0
    
     DEALLOCATE(Current)
     Current => Previous
   END DO 
   
   NULLIFY(Root)

 END SUBROUTINE compute_box_P2P2L
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE compute_contact_P2P2L
 
  IMPLICIT NONE  

  INTEGER                   :: errare 
  INTEGER                   :: icdan,iadj,itact,icdbdy,ianbdy,icdtac,iantac,isee   

  REAL(kind=8),DIMENSION(2) :: coordcd,coordan
  REAL(kind=8),DIMENSION(2) :: cd_Vbegin,an_Vbegin

  INTEGER                   :: i,ibdy
  REAL(kind=8)              :: nonuc,nonuc0,gapTT

  INTEGER                   :: cd_ent,an_ent 
  integer                   :: nb_pt2dl
   
  character(len=80) :: cout
                             !12345678901234567890123456
  character(len=26) :: IAM = 'mod_P2P2L::compute_contact'

   icdan=0        
   nb_P2P2L=0
   nb_adj=0

   nb_PT2DL=get_nb_PT2DL()

   IF (nb_rough_p2p2l /= 0 ) THEN

     DO i=1,nb_rough_p2p2l
       icdtac=rough_p2p2l(i)%cd
       iantac=rough_p2p2l(i)%an
       isee=rough_p2p2l(i)%isee  
       nonuc0 = rough_p2p2l(i)%nonuc0
!fd 
!fd    coordonnees des PT2DL (dans R0)
!fd 
       coordcd = PTcoor(1:2,icdtac)
       coordan = PTcoor(1:2,iantac)

       nonuc =dsqrt((coordcd(1)-coordan(1))**2+(coordcd(2)-coordan(2))**2)

       icdan=icdan+1
       nb_adj(icdtac)=nb_adj(icdtac)+1
       iadj=nb_adj(icdtac)

       this(icdan)%icdent = get_ENT_PT2DL(icdtac)
       this(icdan)%ianent = get_ENT_PT2DL(iantac)


       this(icdan)%iadj   = iadj
       this(icdan)%icdbdy = l_pt2dl(icdtac)%ibdyty
       this(icdan)%icdtac = icdtac
       this(icdan)%icdsci = 0
       this(icdan)%ianbdy = l_pt2dl(iantac)%ibdyty
       this(icdan)%iantac = iantac
       this(icdan)%iansci = 0
       this(icdan)%isee   = isee

       this(icdan)%icdbtac = pt2dl2bdyty(2, icdtac)
       this(icdan)%ianbtac = pt2dl2bdyty(2, iantac)

       this(icdan)%icdbtyp = pt2dl2bdyty(3, icdtac)
       this(icdan)%ianbtyp = pt2dl2bdyty(3, iantac)

       this(icdan)%icdctyp = i_pt2dl
       this(icdan)%ianctyp = i_pt2dl

       call get_behaviour_( icdan, see, tact_behav )

       cd_ent = get_ent_PT2DL(icdtac)
       an_ent = get_ent_PT2DL(iantac) 

       entity(cd_ent)%nb = entity(cd_ent)%nb+1
!fd auto contact
       if (cd_ent /= an_ent) entity(an_ent)%nb = entity(an_ent)%nb+1

       !fd 
       !fd on peut avoir 2 cas:
       !fd  *points colles (nonuc0 = 0) c'est pour un texsol
       !fd  *points non colles (nonuc0 /= 0) c'est pour visco-elas qqch
       !fd

       IF ( nonuc0 .LT. 1.d-20) THEN
         IF (nonuc .GE. 1e-10) THEN
           call faterr(IAM,'On a 2 noeuds decolles dans une situation de noeuds colles')
         ENDIF

         !fd cas de noeuds colles 

         this(icdan)%tuc(:)      =  (/ 1.d0, 0.d0 /)
         this(icdan)%nuc(:)      =  (/ 0.d0, 1.d0 /)
         this(icdan)%gapTTbegin  =  0.d0
         this(icdan)%nonuc0      =  0.d0

         this(icdan)%internal    = 0.d0

!fd Pour les lois sachant quoi faire on laisse faire: 
!fd COUPLED_DOF        
!fd TEX_SOL
                                                   !123456789012345678901234567890
         IF (tact_behav(this(icdan)%lawnb)%lawty /= 'COUPLED_DOF                   ' .AND. &
             tact_behav(this(icdan)%lawnb)%lawty /= 'TEX_SOL                       ' &
            ) THEN
           call faterr(IAM,'On a 2 noeuds colles mais la loi d''interaction ne sait pas le traiter')
         ENDIF

       ELSE

         IF (nonuc .LT. 1d-20) THEN
           PRINT*,'On a 2 noeuds colles dans une situation de noeuds decolles'              
         ENDIF 

         this(icdan)%nuc(:)=  (coordcd(1:2)-coordan(1:2))/nonuc
         this(icdan)%tuc(1)= this(icdan)%nuc(2)
         this(icdan)%tuc(2)=-this(icdan)%nuc(1)
         this(icdan)%gapTTBEGIN  =  nonuc
         this(icdan)%nonuc0      =  nonuc0

         this(icdan)%internal    = 0

!fd attention a le gestion du internal qui est plutot qqch propre au solveur
!fd cette valeur est ecrasee par ce qui est contenu dans le verlet lorsque qu'on fait
!fd un RECUP Rloc donc si on veut initialiser qqch il faut penser a forcer le STOCK Rloc     

                                                    !123456789012345678901234567890
         IF (tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_ROD                   ' .OR. &
             tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_ROD                     ' .OR. &
             tact_behav(this(icdan)%lawnb)%lawty == 'ELASTIC_WIRE                  ' .OR. &
             tact_behav(this(icdan)%lawnb)%lawty == 'BRITTLE_ELASTIC_WIRE          ' .OR. &
             tact_behav(this(icdan)%lawnb)%lawty == 'VOIGT_WIRE                    ' &
            ) THEN

           this(icdan)%internal(1) = nonuc0

         ENDIF





       ENDIF

       CALL get_vlocy_PT2DL(icdtac,iVbeg_,cd_Vbegin)
       CALL get_vlocy_PT2DL(iantac,iVbeg_,an_Vbegin)

       this(icdan)%vltBEGIN  =  (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2)
       this(icdan)%vlnBEGIN  =  (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2)

       this(icdan)%rlt=0.D0
       this(icdan)%rln=0.D0
       this(icdan)%vlt=this(icdan)%vltBEGIN
       this(icdan)%vln=this(icdan)%vlnBEGIN
       this(icdan)%gapTT=this(icdan)%gapTTBEGIN
       this(icdan)%status=i_nknow

       this(icdan)%coor= (coordcd(1:2)+coordan(1:2))*0.5

     ENDDO
     nb_P2P2L=icdan
   END IF 
                            
   WRITE(cout,'(1X,I10,A12)') nb_P2P2L,' P2P2L found'
   call logmes(cout)

  ! Since selection of candidates for contact has been refined, nb_P2P2L is less or equal size(this). 
  ! Loops are now to run from 1 to nb_P2P2L where data are available.

   DO ibdy=1,nb_PT2DL
     IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
     IF (nb_adj(ibdy) /= 0) THEN
       ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error in allocating adjac(',icdbdy,')%.....'
         call faterr(IAM,cout)
       END IF
     ELSE 
       NULLIFY(adjac(ibdy)%icdan)
     ENDIF
!
   ENDDO
  
   DO icdan=1,nb_P2P2L
    adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan      
   END DO

!fd 23/02/04 out la mierda   call get_behaviour
     
   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_P2P2L),stat=errare)


END SUBROUTINE compute_contact_P2P2L
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine display_prox_tactors_P2P2L

   implicit none
   integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   integer          :: nb_pt2dl
   character(len=5) :: cdmodel, anmodel

   nb_PT2DL=get_nb_PT2DL()

   DO icdtact=1,nb_PT2DL    
     DO iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( pt2dl2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( pt2dl2bdyty(3,iantac) )
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan
                      !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,pt2dl2bdyty(1,icdtac),'PT2DL',pt2dl2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,pt2dl2bdyty(1,iantac),'PT2DL',pt2dl2bdyty(2,iantac)
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,A7,D14.7)')'gapTT-=',this(icdan)%gapTTBEGIN
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 END SUBROUTINE display_prox_tactors_P2P2L
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_P2P2L
 
   !
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare
   integer :: nb_pt2dl

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_P2P2L::stock_rloc'

   nb_PT2DL=get_nb_PT2DL()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT2DL),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_PT2DL
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

     DO icdtac=1,nb_PT2DL
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
   DO icdan=1,nb_P2P2L
     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac              ! serial number of antagonist contactor for contact icdan 
     iadj   = this(icdan)%iadj                        ! serial adjacent number of pair contactor 
                                                      ! adjacent to candidate contactor for contact icdan 
     verlt(icdtac)%icdan(iadj)    = icdan
     verlt(icdtac)%cdbdy          = pt2dl2bdyty(1,icdtac)
     verlt(icdtac)%cdtac          = pt2dl2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel        = pt2dl2bdyty(3,icdtac)
     verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
     verlt(icdtac)%anbdy(iadj)    = pt2dl2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)    = pt2dl2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj)  = pt2dl2bdyty(3,iantac)
     verlt(icdtac)%ansci(iadj )   = this(icdan)%iansci
     verlt(icdtac)%rlt(iadj)      = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)      = this(icdan)%rln/H
     verlt(icdtac)%vlt(iadj)      = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)      = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)    = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)   = this(icdan)%status
     verlt(icdtac)%nuc(1:2,iadj)  = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj) = this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   END DO

   nb_vP2P2L = nb_P2P2L

   WRITE(cout,'(1X,I10,A12)') nb_vP2P2L,' stock P2P2L'
   call logmes(cout)

 END SUBROUTINE stock_rloc_P2P2L
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_P2P2L
 
   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   character(len=80) :: cout


   if (.not. allocated(verlt)) then
      call logmes('[mod_P2P2L::recup_rloc] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_P2P2L = 0

   DO icdan=1,nb_P2P2L
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac             ! serial number of antagonist contactor for contact icdan        

     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdbdy  == pt2dl2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac  == pt2dl2bdyty(2,icdtac) .and. &
           verlt(icdtac)%cdmodel== pt2dl2bdyty(3,icdtac)       &
          ) then
          do iadj = 1, verlt(icdtac)%adjsz
            if (verlt(icdtac)%anbdy(iadj)  == pt2dl2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == pt2dl2bdyty(2,iantac) .and. &
                verlt(icdtac)%anmodel(iadj)== pt2dl2bdyty(3,iantac)       &
               ) then
               this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H
               this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H 
               this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

               this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
               nb_recup_P2P2L = nb_recup_P2P2L + 1
               exit
            end if
          end do
       end if
     END IF
   END DO
   
   WRITE(cout,'(1X,I10,A12)') nb_recup_P2P2L,' recup P2P2L'
   call logmes(cout)

 END SUBROUTINE recup_rloc_P2P2L
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE
   CHARACTER(len=103) :: clin
   INTEGER            :: icdan,icdbdy,icdtac,ianbdy,iantac
   INTEGER            :: iadj,icdtact, cdmodel, anmodel
   REAL(kind=8)       :: rlt,rln,vlt,vln,gapTT
   REAL(kind=8),DIMENSION(2) :: nuc,coor
   CHARACTER(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER            :: errare 

   INTEGER :: ibehav,nb_internal,i_internal
   integer :: nb_pt2dl

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_P2P2L::read_ini_Vloc_Rloc'


   nb_PT2DL=get_nb_PT2DL()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_PT2DL),stat=errare)
     IF (errare /=0 ) call faterr(IAM,' error allocating nb_adj')
   END IF    

   DO icdtac=1,nb_PT2DL
     nb_adj(icdtac)=0
   END DO

   DO    
    IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'P2P2L') CYCLE     
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,                                          &
     sttus
     cdmodel = get_body_model_id_from_name( cdbdy )
     do icdtact = 1, nb_PT2DL
       if (pt2dl2bdyty(1,icdtact) == icdbdy .and. &
           pt2dl2bdyty(2,icdtact) == icdtac .and. &
           pt2dl2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
   END DO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_PT2DL),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtac=1,nb_PT2DL
       verlt(icdtac)%adjsz=0
       iadj=nb_adj(icdtac)
       IF (iadj > 0) THEN
         verlt(icdtac)%adjsz=iadj
         call new_verlet_(icdtac, iadj, errare)
       ELSE
         call nullify_verlet_(icdtac)
       END IF
!
       IF (errare /=0 ) THEN
         write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_PT2DL
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
   DO icdtac=1,nb_PT2DL
     nb_adj(icdtac)=0
   END DO
   icdan = 0
   DO
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'P2P2L') CYCLE     
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,                                          &
     sttus
     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )
     DO icdtact=1,nb_PT2DL
       if (pt2dl2bdyty(1,icdtact) == icdbdy .and. &
           pt2dl2bdyty(2,icdtact) == icdtac .and. &
           pt2dl2bdyty(3,icdtact) == cdmodel ) then
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
         READ(G_clin(1:90),'(27X,2(7X,D14.7))') vlt,vln
         verlt(icdtact)%vlt(nb_adj(icdtact))=vlt
         verlt(icdtact)%vln(nb_adj(icdtact))=vln
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,1(7X,D14.7))') gapTT
         verlt(icdtact)%gapTT(nb_adj(icdtact))=gapTT
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,2(7X,D14.7))') nuc(1),nuc(2)
         verlt(icdtact)%nuc(1,nb_adj(icdtact))=nuc(1)
         verlt(icdtact)%nuc(2,nb_adj(icdtact))=nuc(2)
         IF( .NOT. read_G_clin()) EXIT
         READ(G_clin(1:90),'(27X,2(7X,D14.7))') coor(1),coor(2)
         verlt(icdtact)%coor(1,nb_adj(icdtact))=coor(1)
         verlt(icdtact)%coor(2,nb_adj(icdtact))=coor(2)

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
   END DO   

 nb_vP2P2L=0

 DO icdtact=1,nb_PT2DL
    nb_vP2P2L = nb_vP2P2L + nb_adj(icdtact)

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
   integer :: nb_pt2dl

   character(len=20) :: fmt
   character(len=5)  :: cdmodel, anmodel
   
   nb_PT2DL=get_nb_PT2DL()
 
   DO icdtact=1,nb_PT2DL    
     DO iadj=1,nb_adj(icdtact)    
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       cdmodel = get_body_model_name_from_id( pt2dl2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( pt2dl2bdyty(3,iantac) )
       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','P2P2L',icdan     
                            !1234567890123456789012345678901234567890123456789012345678901234567890123456
       WRITE(nfich,'(A76)') ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'   
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,pt2dl2bdyty(1,icdtac),'PT2DL',pt2dl2bdyty(2,icdtac),  &
       see(this(icdan)%isee)%behav,  &
       anmodel,pt2dl2bdyty(1,iantac),'PT2DL',pt2dl2bdyty(2,iantac),  &
       get_contact_status_name_from_id(this(icdan)%status),iantac
       WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls/H',0.D0
       WRITE(nfich,104)'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',0.D0
       WRITE(nfich,103)'gap =',this(icdan)%gapTT
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
 SUBROUTINE nullify_reac_P2P2L(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdtac,iantac
   INTEGER :: storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_PT2DL(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_PT2DL(iantac,storage)
    
 END SUBROUTINE nullify_reac_P2P2L
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_P2P2L(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER :: icdtac,iantac
   INTEGER :: storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_PT2DL(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_vlocy_PT2DL(iantac,storage)
    
 END SUBROUTINE nullify_vlocy_P2P2L
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_P2P2L(icdan,storage,need_full_vlocy)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan 
   INTEGER            :: icdtac,iantac
   INTEGER            :: storage   
   logical, optional  :: need_full_vlocy
    
   icdtac=this(icdan)%icdtac
   CALL comp_vlocy_PT2DL(icdtac,storage,need_full_vlocy)
    
   iantac=this(icdan)%iantac
   CALL comp_vlocy_PT2DL(iantac,storage,need_full_vlocy)
    
 END SUBROUTINE vitrad_P2P2L
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE injj_P2P2L(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in) :: icdan
   REAL(kind=8),INTENT(in) :: RTIK,RNIK
   REAL(kind=8),DIMENSION(2)  :: cdreac,anreac

   INTEGER :: icdtac
   INTEGER :: iantac
   INTEGER :: storage   
   
   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)

   anreac(1)=-cdreac(1)

   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      

   anreac(2)=-cdreac(2)

   icdtac=this(icdan)%icdtac
   CALL add_reac_PT2DL(icdtac,cdreac(1:2),storage)

   iantac=this(icdan)%iantac
   CALL add_reac_PT2DL(iantac,anreac(1:2),storage)

 END SUBROUTINE injj_P2P2L 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_P2P2L(icdan,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VTIK,VNIK
   INTEGER    , intent(in) :: storage
   !
   REAL(kind=8),DIMENSION(2) :: Vcd,Van
   INTEGER :: icdtac,iantac
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac

   SELECT CASE(storage) 
   CASE (iV____)
     CALL get_vlocy_PT2DL(icdtac,iV____,Vcd)
     CALL get_vlocy_PT2DL(iantac,iV____,Van) 
   CASE (iVfree)
     CALL get_vlocy_PT2DL(icdtac,iVfree,Vcd)
     CALL get_vlocy_PT2DL(iantac,iVfree,Van)     
   CASE (iVaux_)
     CALL get_vlocy_PT2DL(icdtac,iVaux_,Vcd)
     CALL get_vlocy_PT2DL(iantac,iVaux_,Van)   
   END SELECT

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2) &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)
   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2) &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)
   
 END SUBROUTINE prjj_P2P2L 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 integer function get_nb_P2P2L(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_P2P2L = nb_P2P2L
   case(i_verlet_tactor)
      get_nb_P2P2L = nb_vP2P2L
   case(i_rough_tactor)
      get_nb_P2P2L = nb_rough_P2P2L
   case(i_recup_tactor)
      get_nb_P2P2L = nb_recup_P2P2L
   end select

 end function get_nb_P2P2L
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE print_info_P2P2L(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'PT2DL:',1x,I5,1x,'PT2DL:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

!   call print_info_PT2DL(icdbdy)
!   call print_info_PT2DL(ianbdy)

END SUBROUTINE print_info_P2P2L
!------------------------------------------------------------------------
!------------------------------------------------------------------------
real(kind=8) function get_length_P2P2L(icdan)
  implicit none
  !
  integer(kind=4), intent(in) :: icdan 

  get_length_P2P2L = 1.d0
  
end function get_length_P2P2L
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
LOGICAL FUNCTION RUN_P2P2L(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_P2P2L = RUN_TACTOR

END FUNCTION RUN_P2P2L
!------------------------------------------------------------------------
  logical function CHECK_P2P2L()
    implicit none
    !   
    integer :: isee, nb_PT2DL
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_P2P2L = check_P2P2L_
      return
    end if

    con_pedigree%module_name = 'P2P2L'

    con_pedigree%id_cdan  = i_p2p2l
    con_pedigree%id_cdtac = i_pt2dl
    con_pedigree%id_antac = i_pt2dl

    cdtact2bdyty => pt2dl2bdyty
    antact2bdyty => pt2dl2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_PT2DL = get_nb_PT2DL()
    if( nb_PT2DL < 2 ) then
      CHECK_P2P2L = check_P2P2L_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'PT2DL' .and. see(isee)%antac == 'PT2DL') then
        check_P2P2L_ = .true.
        exit
      end if
    end do
  
    CHECK_P2P2L = check_P2P2L_
    return
  
  end function CHECK_P2P2L
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_P2P2L(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_P2P2L = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_P2P2L

 function get_icdtac_P2P2L(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_P2P2L
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_PT2DL
   logical :: found

   found = .false.

   nb_PT2DL = get_nb_PT2DL()

   icc = 0
   do icdtac = 1, nb_PT2DL
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_P2P2L = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('P2P2L::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_P2P2L(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_P2P2L
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_PT2DL
   logical :: found

   found = .false.

   nb_PT2DL = get_nb_PT2DL()

   icc = 0
   do icdtac = 1, nb_PT2DL
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_P2P2L =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('P2P2L::get_icdtac','unknown contact index')
   

   get_iantac_P2P2L = this(icdan)%iantac

 end function

 subroutine clean_memory_P2P2L
   implicit none
   integer(kind=4) :: i, j

   call clean_memory_inter_meca_()

   nb_P2P2L  = 0
   nb_vP2P2L = 0

   if( allocated(rough_P2P2L) ) deallocate(rough_P2P2L)

   nb_rough_P2P2L = 0

   if( allocated(PTcoor) ) deallocate(PTcoor)
   if( allocated(PTcoor0)) deallocate(PTcoor0)

   Reac_P2P2L_MAX = 0.D0

   module_checked_ = .FALSE.
   check_P2P2L_    = .FALSE.

 end subroutine
 
 subroutine set_nb_P2P2L(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_P2P2L = nb

 end subroutine

 subroutine redo_nb_adj_P2P2L()
   implicit none

   call redo_nb_adj_( get_nb_PT2DL() )

 end subroutine

END MODULE P2P2L

