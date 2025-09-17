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
MODULE SPDCx                                          

  !!****h* LMGC90.CORE/SPDCx
  !! NAME
  !!  module SPDCx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors SPHER and CYLDx.
  !!  In this modulus candidate contactors are SPHER and antagonist 
  !!  contactors are CYLDx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/SPHER
  !!  LMGC90.CORE/DNLYC
  !!****

  USE overall
  USE tact_behaviour
  USE SPHER
  USE DNLYC

  use RBDY3, only : get_data, &
                    get_color_RBDY3 => get_color
  use MBS3D, only : get_color_MBS3D => get_color
  use MAILx, only : get_color_MAILx

  use parameters, only : i_spdcx, i_mailx, i_rbdy3, i_mbs3

  use inter_meca_3D

  implicit none
  
  private

  INTEGER :: nb_SPHER
  INTEGER :: nb_DNLYC

  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

!!!---------------------------------------------------------------------

  INTEGER,PRIVATE :: nb_SPDCx=0,nb_vSPDCx=0,nb_recup_SPDCx=0

!!!---------------------------------------------------------------------


  type( T_this_adjac ), dimension( : ), allocatable, target :: adjac   
  integer             , dimension( : ), allocatable, target :: nb_adj

!!!------------------------------------------------------------------------ 

  type(T_verlet), dimension(:), allocatable, target ::verlt

!!!------------------------------------------------------------------------

  TYPE T_rough_SPDCx
    INTEGER      :: cd,an,isee
     REAL(kind=8) :: meff,reff
  END TYPE T_rough_SPDCx

  TYPE(T_rough_SPDCx),DIMENSION(:),ALLOCATABLE :: rough_SPDCx
  INTEGER                                      :: nb_rough_SPDCx

  TYPE T_link_rough_SPDCx
     TYPE(T_link_rough_SPDCx), POINTER :: p  
     TYPE(T_rough_SPDCx)               :: val
     TYPE(T_link_rough_SPDCx), POINTER :: n  
  END TYPE T_link_rough_SPDCx

  TYPE(T_link_rough_SPDCx),POINTER :: Root,Current,Previous

!!!------------------------------------------------------------------------

  REAL (kind=8)  :: maxray, minray, maxalert, meanradius
  REAL (kind=8)  :: Upper_limit
  
  REAL(kind=8),DIMENSION(:,:),ALLOCATABLE,PRIVATE :: SPcoor,CDcoor
  REAL(kind=8)                                    :: Reac_SPDCx_MAX=0.D0
  INTEGER,PRIVATE                                 :: ii,l_ii,iv
  INTEGER,PRIVATE                                 :: Nstep_creation_tab_visu=1
  LOGICAL,PRIVATE                                 :: write_creation_tab_visu

  real(kind=8), dimension(:), allocatable, target :: violation
!!!------------------------------------------------------------------------
  logical      :: module_checked_ = .FALSE.
  logical      :: check_SPDCx_    = .FALSE.


  PUBLIC &
       coor_prediction_SPDCx,&
       CHECK_SPDCx,&
       RUN_SPDCx, &
       get_write_Vloc_Rloc_SPDCx, &
       read_ini_Vloc_Rloc_SPDCx,&
       write_xxx_Vloc_Rloc_SPDCx,&
       stock_rloc_SPDCx, &
       recup_rloc_SPDCx, &
       smooth_computation_SPDCx, &
       compute_box_SPDCx, &
       creation_tab_visu_SPDCx, &
       compute_contact_SPDCx, &
       display_prox_tactors_SPDCx,&
       get_nb_SPDCx, &
       update_cohe_SPDCx

  PUBLIC &
       nullify_reac_SPDCx, nullify_vlocy_SPDCx,injj_SPDCx, prjj_SPDCx, vitrad_SPDCx, & 
       SPDCx2ENT,SPDCx2SPHER,SPDCx2DNLYC, &
       get_surf_SPDCx

  public clean_memory_SPDCx

  !rm for handler
  public get_this    , &
         set_nb_SPDCx, &
         redo_nb_adj_SPDCx, &
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
!!!------------------------------------------------------------------------
  SUBROUTINE coor_prediction_SPDCx

    IMPLICIT NONE  
    INTEGER :: itacty  
    
    IF (smooth_method) THEN
       DO itacty=1,nb_SPHER
          SPcoor(1:3,itacty) = get_coor_SPHER(spher2bdyty(1,itacty),spher2bdyty(2,itacty))
       END DO

       DO itacty=1,nb_DNLYC
          CDcoor(1:3,itacty) = get_coor_DNLYC(dnlyc2bdyty(1,itacty),dnlyc2bdyty(2,itacty))
       END DO
    ELSE
       DO itacty=1,nb_SPHER
          SPcoor(1:3,itacty) = get_coorTT_SPHER(spher2bdyty(1,itacty),spher2bdyty(2,itacty))
       END DO

       DO itacty=1,nb_DNLYC
          CDcoor(1:3,itacty) = get_coorTT_DNLYC(dnlyc2bdyty(1,itacty),dnlyc2bdyty(2,itacty))
       END DO
    END IF

  END SUBROUTINE coor_prediction_SPDCx
!!!---------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_SPDCx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_SPDCx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_SPDCx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_SPDCx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_box_SPDCx

    IMPLICIT NONE

    INTEGER                     :: isee,errare,ibdy
    CHARACTER(len=22)           :: IAM='mod_SPDCx::compute_box'

    maxalert=0.D0  

    DO isee=1,SIZE(see)
       IF (see(isee)%cdtac .EQ. 'SPHER' .AND. see(isee)%antac .EQ. 'DNLYC') THEN
          maxalert=MAX(maxalert,see(isee)%alert)
       END IF
    END DO
    
    minray       = 0.d0
    maxray       = get_max_radius_SPHER()

    maxray = (1.005*maxray) + maxalert

    IF (.NOT. ALLOCATED(adjac))THEN
       ALLOCATE(adjac(nb_SPHER),stat=errare)
       IF (errare .NE.0 ) THEN
          CALL FATERR(IAM,'error in allocating adjac')
       END IF
       DO ibdy=1,nb_SPHER
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    ELSE
       DO ibdy=1,nb_SPHER
          IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
          NULLIFY(adjac(ibdy)%icdan)
       END DO
    END IF
  
    IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
    ALLOCATE(nb_adj(nb_SPHER),stat=errare)
    IF (errare .NE.0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF

    nb_adj = 0

    ! SPcoor are coordinates of bodies owning SPHER to be used in selecting prox tactors
    IF (ALLOCATED(SPcoor)) DEALLOCATE(SPcoor)
    ALLOCATE(SPcoor(3,nb_SPHER),stat=errare)
    
    IF (ALLOCATED(CDcoor)) DEALLOCATE(CDcoor)
    ALLOCATE(CDcoor(3,nb_DNLYC),stat=errare)

  END SUBROUTINE compute_box_SPDCx
!!!------------------------------------------------------------------------
  SUBROUTINE creation_tab_visu_SPDCx

    IMPLICIT NONE

    INTEGER                     :: icdtac,iantac,isee,icdan,test
    CHARACTER(len=5)            :: cdcol,ancol
    REAL(kind=8)                :: Hi,dist,raycd,adist,masscd
    REAL(kind=8),DIMENSION(3)   :: coorcd,cooran,vect_CDSP
    REAL(kind=8),DIMENSION(3,3) :: localframe
    LOGICAL                     :: visible
    REAL(kind=8),DIMENSION(2)   :: DATA
    REAL(kind=8)                :: Height,rayan,intray
    character(len=80)           :: cout

    nb_rough_SPDCx=0
    test=0

   ! création de la liste de paire de polygones à examiner

   ! on désalloue la liste chainée pour le stockage temporaire des paires candidats antagonistes
   ! on s'alloue un zone mémoire au fur et à mesure que l'on détermine un candidat - antagoniste

    NULLIFY(Root) 
    NULLIFY(Current)
    NULLIFY(Previous)
    
    DO iantac=1,nb_DNLYC
       !pta
       visible = get_visible_DNLYC(iantac)
       IF (.NOT.visible) CYCLE
       !pta
       cooran = CDcoor(1:3,iantac)
       localframe = get_inertia_frameTT_DNLYC(dnlyc2bdyty(1,iantac))
       CALL get_data(dnlyc2bdyty(1,iantac),dnlyc2bdyty(2,iantac),DATA)

!fd on ne garde que les spheres qui sont dans une tranche interieure tolerable
!fd demi hauteur tolerable
       !Height = (0.5D0*data(1)+maxalert)
       Height = (data(1)+maxalert)
       rayan = data(2)

!fd tranche interieure au cylindre tolerable (si c est 0 on garde tout)

       intray  = (max(0.d0,rayan - (2.d0*maxray)))**2

       DO icdtac=1,nb_SPHER

          visible = get_visible_SPHER(icdtac)
          IF (.NOT.visible) CYCLE
          coorcd = SPcoor(1:3,icdtac)
          raycd   = get_radius_SPHER(icdtac)
          cdcol   = get_color_SPHER(icdtac)
          ancol   = get_color_DNLYC(iantac)
          
          isee = get_isee('RBDY3','SPHER',cdcol,'RBDY3','DNLYC',ancol)
          
          IF (isee .NE. 0) THEN
             adist = see(isee)%alert 
             vect_CDSP(1:3) = coorcd(1:3) - cooran(1:3)
             

!fd projection du vecteur des centres sur l'axe
!fd attention ici le centre de l'antagoniste c'est l'axe

             Hi  = vect_CDSP(1)*localframe(1,3) + &
                   vect_CDSP(2)*localframe(2,3) + &
                   vect_CDSP(3)*localframe(3,3)

             vect_CDSP(:)=vect_CDSP(:) - Hi*localframe(:,3)

!fd calcul de la distance a l'axe
             dist=vect_CDSP(1)*vect_CDSP(1) + &
                  vect_CDSP(2)*vect_CDSP(2) + &
                  vect_CDSP(3)*vect_CDSP(3)

!fd -si on est dans la hauteur du cylindre et 
!fd  la distance a l'axe depasse le rayon interieure: contact sphere-cylindre 
!fd -si on est hors de la hauteur du cylindre et 
!fd  la distance a l'axe depasse le rayon exterieure: contact sphere-plan 

             if ((abs(Hi) < Height+(2.d0*maxray) .and. dist > intray)) then

                nb_rough_SPDCx = nb_rough_SPDCx + 1
                IF ( nb_rough_SPDCx .EQ. 1) THEN
                   ALLOCATE(Root)
                   Current => Root
                   NULLIFY(Root%p)
                ELSE
                   ALLOCATE(Current)
                   Previous%n => Current
                END IF
                Current%val%cd    = icdtac
                Current%val%an    = iantac
                Current%val%isee  = isee
                
                Current%p => Previous
                NULLIFY(Current%n)
                Previous => Current
             END IF
          END IF
       END DO
    END DO
    
    WRITE(cout,'(4X,I10,A20)') nb_rough_SPDCx,' SPDCx roughly found'
    call logmes(cout)

    IF (ALLOCATED(rough_SPDCx)) DEALLOCATE(rough_SPDCx)
    ALLOCATE(rough_SPDCx(nb_rough_SPDCx))
    
    IF (ALLOCATED(this)) DEALLOCATE(this)
    ALLOCATE(this(2*nb_rough_SPDCx))        

    DO icdan=nb_rough_SPDCx,1,-1
       
       Previous => Current%p
       rough_SPDCx(icdan)%cd   = Current%val%cd
       rough_SPDCx(icdan)%an   = Current%val%an
       rough_SPDCx(icdan)%isee = Current%val%isee
       
       masscd = get_mass_SPHER(spher2bdyty(1,Current%val%cd))
       raycd  = get_radius_SPHER(Current%val%cd)
       
       rough_SPDCx(icdan)%meff = masscd
       rough_SPDCx(icdan)%reff = raycd
       
       DEALLOCATE(Current)
       Current => Previous
    END DO
   
    NULLIFY(Root)

    nb_SPDCx = nb_rough_SPDCx

  END SUBROUTINE creation_tab_visu_SPDCx
!!!------------------------------------------------------------------------
  SUBROUTINE compute_contact_SPDCx
 
    IMPLICIT NONE  
    
    INTEGER                     :: errare,icdtac,iantac,isee
    INTEGER                     :: icdan,iadj,ibdy,itac
    REAL(kind=8)                :: raycd,adist,dist
    REAL(kind=8)                :: den,norm2
    REAL(kind=8),DIMENSION(3)   :: cdlev,anlev,sep,point,coorcd,cooran,vect_CDSP
    REAL(kind=8),DIMENSION(6)   :: cd_Vbegin,an_Vbegin
    REAL(kind=8),DIMENSION(3,3) :: Rc
    REAL(kind=8),DIMENSION(3,3) :: localframe
    REAL(kind=8),DIMENSION(2)   :: DATA
    
    REAL(kind=8)                :: Hi,Height,rayan,tmp,gap
    REAL(kind=8),DIMENSION(3)   :: xco,sept,sepn
    logical                     :: iskip
    integer :: cd_ent,an_ent
    character(len=80) :: cout

    icdan    = 0        
    nb_SPDCx = 0
    nb_adj   = 0

    IF (nb_rough_SPDCx .NE. 0 ) THEN

       DO itac=1,nb_rough_SPDCx

          icdtac = rough_SPDCx(itac)%cd
          iantac = rough_SPDCx(itac)%an

          !fd 01/04/2010 si on n'actualise pas la liste verlet
          !fd            mais qu'on rend invisible des particules
          !fd            il ne faut plus tester le contact  
          if (.not. get_visible_SPHER(icdtac)) cycle 
          !pta
          if (.not. get_visible_DNLYC(iantac)) cycle 


          isee   = rough_SPDCx(itac)%isee
          adist   = see(isee)%alert
          raycd  = get_radius_SPHER(icdtac)
          
          localframe = get_inertia_frameTT_DNLYC(dnlyc2bdyty(1,iantac))

          coorcd = SPcoor(1:3,icdtac)
          cooran = CDcoor(1:3,iantac)
          
          CALL get_data(dnlyc2bdyty(1,iantac),dnlyc2bdyty(2,iantac),DATA)

          Height = DATA(1); rayan = DATA(2)
          vect_CDSP(1:3) = coorcd(1:3) - cooran(1:3)

          Hi  = vect_CDSP(1)*localframe(1,3) + &
                vect_CDSP(2)*localframe(2,3) + &
                vect_CDSP(3)*localframe(3,3)

          vect_CDSP(:)=vect_CDSP(:) - (Hi*localframe(:,3))

          dist=vect_CDSP(1)*vect_CDSP(1)+ &
               vect_CDSP(2)*vect_CDSP(2)+ &
               vect_CDSP(3)*vect_CDSP(3)

          norm2 = sqrt(dist)

          iskip = .true.

!fd on est dans la partie creuse
          dist  = rayan-(norm2+raycd)
          IF (dabs(Hi) < Height .AND. dist < adist  ) THEN

!fd si grain sur l'axe on ne sait pas faire
             if (norm2 /= 0.d0) then
               sep   = -vect_CDSP/norm2
               gap   = dist
               iskip = .false.
             endif

          endif

!fd on est dans la partie coin
          IF (dabs(Hi) >= Height .AND. &
              dabs(Hi) < Height+raycd+adist .and. &
              dist < adist  .and. &
              norm2 <= rayan ) THEN

             ! partie verticale
             sepn  = sign(1.d0,Hi)*(dabs(Hi)-Height)*localframe(:,3)
             ! partie horizontale
             sept  = vect_CDSP*(1.d0 - (rayan/norm2))  

             sep = sepn + sept 
             gap = sqrt(dot_product(sep,sep))

             !fd petite securite si ca degenere
             if (gap /= 0.d0) then
                sep = sep / gap
                iskip = .false.
             else
                sep   = sign(1.d0,Hi)*localframe(:,3)
                iskip = .false.
             endif

          endif

!fd on est dans la partie plate

          IF (dabs(Hi) < Height+raycd+adist .and. &
              norm2 > rayan  ) THEN

             sep   = sign(1.d0,Hi)*localframe(:,3)
             gap   = dabs(Hi) - rayan

             iskip = .false.

          endif 


          if (iskip) cycle

             icdan          = icdan + 1
             nb_adj(icdtac) = nb_adj(icdtac) + 1
             iadj           = nb_adj(icdtac)

             xco(1:3)       = coorcd(1:3) - raycd*sep(1:3)

             this(icdan)%icdbtac = spher2bdyty(2, icdtac)
             this(icdan)%ianbtac = dnlyc2bdyty(2, iantac)

             this(icdan)%icdbtyp = spher2bdyty(3, icdtac)
             this(icdan)%ianbtyp = dnlyc2bdyty(3, iantac)

             this(icdan)%icdctyp = i_spher
             this(icdan)%ianctyp = i_dnlyc

             this(icdan)%iadj    = iadj
             this(icdan)%icdbdy  = spher2bdyty(1, icdtac)
             this(icdan)%icdtac  = icdtac
             this(icdan)%icdsci  = 0
             this(icdan)%ianbdy  = dnlyc2bdyty(1, iantac)
             this(icdan)%iantac  = iantac
             this(icdan)%iansci  = 0
             this(icdan)%isee    = rough_SPDCx(itac)%isee
             this(icdan)%coor    = xco
             this(icdan)%nuc     = sep

             this(icdan)%gapTTbegin = gap
          
             IF(  ( this(icdan)%nuc(1)*this(icdan)%nuc(2) .EQ. 0. ).OR. &
                  ( this(icdan)%nuc(2)*this(icdan)%nuc(3) .EQ. 0. ).OR. &
                  ( this(icdan)%nuc(3)*this(icdan)%nuc(1) .EQ. 0. )) THEN
                
                this(icdan)%tuc(1) = this(icdan)%nuc(3)
                this(icdan)%tuc(2) = this(icdan)%nuc(1)
                this(icdan)%tuc(3) = this(icdan)%nuc(2)
             ELSE
                den=SQRT((this(icdan)%nuc(2)*this(icdan)%nuc(3))*(this(icdan)%nuc(2)*this(icdan)%nuc(3)) &
                        +(this(icdan)%nuc(1)*this(icdan)%nuc(3))*(this(icdan)%nuc(1)*this(icdan)%nuc(3)) &
                   +4.D0*(this(icdan)%nuc(1)*this(icdan)%nuc(2))*(this(icdan)%nuc(1)*this(icdan)%nuc(2)))
                den = 1./den
                
                this(icdan)%tuc(1) =     -this(icdan)%nuc(2)*this(icdan)%nuc(3)*den
                this(icdan)%tuc(2) =     -this(icdan)%nuc(3)*this(icdan)%nuc(1)*den
                this(icdan)%tuc(3) = 2.D0*this(icdan)%nuc(1)*this(icdan)%nuc(2)*den
             END IF
             
             this(icdan)%suc(1) =  this(icdan)%tuc(3)*this(icdan)%nuc(2) &
                                  -this(icdan)%tuc(2)*this(icdan)%nuc(3)
             this(icdan)%suc(2) =  this(icdan)%tuc(1)*this(icdan)%nuc(3) &
                                  -this(icdan)%tuc(3)*this(icdan)%nuc(1)              
             this(icdan)%suc(3) =  this(icdan)%tuc(2)*this(icdan)%nuc(1) &
                                  -this(icdan)%tuc(1)*this(icdan)%nuc(2)
             
             cdlev = xco(1:3) - coorcd(1:3)
             anlev = xco(1:3) - cooran(1:3)
             
             cd_ent = get_ent_SPHER(this(icdan)%icdbdy)
             an_ent = get_ent_DNLYC(this(icdan)%ianbdy)

             this(icdan)%icdent = cd_ent
             this(icdan)%ianent = an_ent

             entity(cd_ent)%nb = entity(cd_ent)%nb + 1
             entity(an_ent)%nb = entity(an_ent)%nb + 1

             ! Bras de levier pour la sphère: pas de changement de repère
             ! car symétrie sphérique

             this(icdan)%Gcds(1)= cdlev(2)*this(icdan)%suc(3)-cdlev(3)*this(icdan)%suc(2)
             this(icdan)%Gcds(2)= cdlev(3)*this(icdan)%suc(1)-cdlev(1)*this(icdan)%suc(3)
             this(icdan)%Gcds(3)= cdlev(1)*this(icdan)%suc(2)-cdlev(2)*this(icdan)%suc(1)
             
             this(icdan)%Gcdt(1)= cdlev(2)*this(icdan)%tuc(3)-cdlev(3)*this(icdan)%tuc(2)
             this(icdan)%Gcdt(2)= cdlev(3)*this(icdan)%tuc(1)-cdlev(1)*this(icdan)%tuc(3)
             this(icdan)%Gcdt(3)= cdlev(1)*this(icdan)%tuc(2)-cdlev(2)*this(icdan)%tuc(1)
             
             this(icdan)%Gcdn(1)= cdlev(2)*this(icdan)%nuc(3)-cdlev(3)*this(icdan)%nuc(2)
             this(icdan)%Gcdn(2)= cdlev(3)*this(icdan)%nuc(1)-cdlev(1)*this(icdan)%nuc(3)
             this(icdan)%Gcdn(3)= cdlev(1)*this(icdan)%nuc(2)-cdlev(2)*this(icdan)%nuc(1)
             
             ! On va calculer le passage rep inertie -> rep général

             localframe = get_inertia_frameTT_DNLYC(dnlyc2bdyty(1,iantac))
             
             Rc(1,1)=localframe(2,1)*anlev(3) - localframe(3,1)*anlev(2)
             Rc(2,1)=localframe(2,2)*anlev(3) - localframe(3,2)*anlev(2)
             Rc(3,1)=localframe(2,3)*anlev(3) - localframe(3,3)*anlev(2)
             
             Rc(1,2)=localframe(3,1)*anlev(1) - localframe(1,1)*anlev(3)
             Rc(2,2)=localframe(3,2)*anlev(1) - localframe(1,2)*anlev(3)
             Rc(3,2)=localframe(3,3)*anlev(1) - localframe(1,3)*anlev(3)
             
             Rc(1,3)=localframe(1,1)*anlev(2) - localframe(2,1)*anlev(1)
             Rc(2,3)=localframe(1,2)*anlev(2) - localframe(2,2)*anlev(1)
             Rc(3,3)=localframe(1,3)*anlev(2) - localframe(2,3)*anlev(1)
             
             this(icdan)%Gans(1)= Rc(1,1)*this(icdan)%suc(1) + &
                                  Rc(1,2)*this(icdan)%suc(2) + &
                                  Rc(1,3)*this(icdan)%suc(3)
 
             this(icdan)%Gans(2)= Rc(2,1)*this(icdan)%suc(1) + &
                                  Rc(2,2)*this(icdan)%suc(2) + &
                                  Rc(2,3)*this(icdan)%suc(3) 

             this(icdan)%Gans(3)= Rc(3,1)*this(icdan)%suc(1) + &
                                  Rc(3,2)*this(icdan)%suc(2) + &
                                  Rc(3,3)*this(icdan)%suc(3) 
             
             this(icdan)%Gant(1)= Rc(1,1)*this(icdan)%tuc(1) + &
                                  Rc(1,2)*this(icdan)%tuc(2) + &
                                  Rc(1,3)*this(icdan)%tuc(3)
 
             this(icdan)%Gant(2)= Rc(2,1)*this(icdan)%tuc(1) + &
                                  Rc(2,2)*this(icdan)%tuc(2) + &
                                  Rc(2,3)*this(icdan)%tuc(3)
 
             this(icdan)%Gant(3)= Rc(3,1)*this(icdan)%tuc(1) + &
                                  Rc(3,2)*this(icdan)%tuc(2) + &
                                  Rc(3,3)*this(icdan)%tuc(3) 
             
             this(icdan)%Gann(1)= Rc(1,1)*this(icdan)%nuc(1) + &
                                  Rc(1,2)*this(icdan)%nuc(2) + &
                                  Rc(1,3)*this(icdan)%nuc(3) 

             this(icdan)%Gann(2)= Rc(2,1)*this(icdan)%nuc(1) + &
                                  Rc(2,2)*this(icdan)%nuc(2) + &
                                  Rc(2,3)*this(icdan)%nuc(3)
 
             this(icdan)%Gann(3)= Rc(3,1)*this(icdan)%nuc(1) + &
                                  Rc(3,2)*this(icdan)%nuc(2) + &
                                  Rc(3,3)*this(icdan)%nuc(3) 
             
             ! Calcul des vitesses relatives, pas besoin de recalculer la vitesse de rotation (4,5,6) dans le rep
             ! d'inertie car cette opération est contenue dans le calcul des bras de leviers.
             
             cd_Vbegin = get_vlocy_SPHER(spher2bdyty(1,icdtac),iVbeg_)
             an_Vbegin = get_vlocy_DNLYC(dnlyc2bdyty(1,iantac),iVbeg_)
             
             this(icdan)%vlsBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%suc(1) &
                  + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%suc(2) &
                  + (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%suc(3) &
                  +  cd_Vbegin(4)*this(icdan)%Gcds(1) - an_Vbegin(4)*this(icdan)%Gans(1) &
                  +  cd_Vbegin(5)*this(icdan)%Gcds(2) - an_Vbegin(5)*this(icdan)%Gans(2) &
                  +  cd_Vbegin(6)*this(icdan)%Gcds(3) - an_Vbegin(6)*this(icdan)%Gans(3)
             
             this(icdan)%vltBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) &
                  + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2) &
                  + (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%tuc(3) &
                  +  cd_Vbegin(4)*this(icdan)%Gcdt(1) - an_Vbegin(4)*this(icdan)%Gant(1) &
                  +  cd_Vbegin(5)*this(icdan)%Gcdt(2) - an_Vbegin(5)*this(icdan)%Gant(2) &
                  +  cd_Vbegin(6)*this(icdan)%Gcdt(3) - an_Vbegin(6)*this(icdan)%Gant(3)
             
             this(icdan)%vlnBEGIN = (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) &
                  + (cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2) &
                  + (cd_Vbegin(3)-an_Vbegin(3))*this(icdan)%nuc(3) &
                  +  cd_Vbegin(4)*this(icdan)%Gcdn(1) - an_Vbegin(4)*this(icdan)%Gann(1) &
                  +  cd_Vbegin(5)*this(icdan)%Gcdn(2) - an_Vbegin(5)*this(icdan)%Gann(2) &
                  +  cd_Vbegin(6)*this(icdan)%Gcdn(3) - an_Vbegin(6)*this(icdan)%Gann(3)
             
             this(icdan)%rls    = 0.D0
             this(icdan)%rlt    = 0.D0
             this(icdan)%rln    = 0.D0
             this(icdan)%vls    = this(icdan)%vlsBEGIN
             this(icdan)%vlt    = this(icdan)%vltBEGIN
             this(icdan)%vln    = this(icdan)%vlnBEGIN
             this(icdan)%gapTT  = this(icdan)%gapTTbegin
             this(icdan)%status = i_nknow

             this(icdan)%meff    = rough_SPDCx(itac)%meff
             this(icdan)%reff    = rough_SPDCx(itac)%reff

             call get_behaviour_( icdan, see, tact_behav )

             !123456789012345678901234567890
             IF (tact_behav(this(icdan)%lawnb)%lawty .EQ. 'WET_3C                        ') THEN
                IF (this(icdan)%internal(1).EQ.0.D0) THEN
                   this(icdan)%internal(2)   = raycd + gap  !fd dist ?
                   this(icdan)%internal(4:6) = this(icdan)%coor(1:3)
                   this(icdan)%internal(1) = 1.D0
                   this(icdan)%internal(3) = 0.D0
                ELSE
                   sep = this(icdan)%coor(1:3) - this(icdan)%internal(4:6)
                   sepn = sep(1)*this(icdan)%nuc(1)+sep(2)*this(icdan)%nuc(2)+sep(3)*this(icdan)%nuc(3)
                   sept(1:3) = sep(1:3) - sepn(1:3)
                   this(icdan)%internal(3) = SQRT(sept(1)*sept(1)+sept(2)*sept(2)+sept(3)*sept(3))
                END IF
             END IF

       END DO
       nb_SPDCx=icdan
    END IF
    
    WRITE(cout,'(1X,I10,A12)') nb_SPDCx,' SPDCx found'
    call logmes(cout)

    DO ibdy=1,nb_SPHER
       IF (ASSOCIATED(adjac(ibdy)%icdan))  DEALLOCATE(adjac(ibdy)%icdan)
       IF (nb_adj(ibdy) .NE. 0) THEN
          ALLOCATE(adjac(ibdy)%icdan(nb_adj(ibdy)),stat=errare)
          IF (errare .NE.0 ) THEN
             write(cout,'(A,I0,A)') 'Error allocating adjac(',ibdy,')%.....'
             call faterr('mod_SPDCx::compute_contact',cout)
          END IF
       END IF
    END DO
    
    DO icdan=1,nb_SPDCx
       adjac(this(icdan)%icdtac)%icdan(this(icdan)%iadj) = icdan
    END DO
    
    IF (ALLOCATED(violation)) DEALLOCATE(violation)
    ALLOCATE(violation(nb_SPDCx),stat=errare)
    IF (errare .NE.0 ) THEN
       call faterr('mod_SPDCx::compute_contact','Error allocating violation')
    END IF
    
  END SUBROUTINE compute_contact_SPDCx
!!!------------------------------------------------------------------------ 
  SUBROUTINE smooth_computation_SPDCx

    IMPLICIT NONE
    INTEGER          :: icdan

    !$OMP PARALLEL DEFAULT(SHARED) &
    !$OMP          PRIVATE(icdan)
    !$OMP DO SCHEDULE(RUNTIME)
    DO icdan=1,nb_SPDCx
       
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
    
    DO icdan=1,nb_SPDCx  
       CALL nullify_reac_SPDCx(icdan,iIreac)
    END DO
    
    DO icdan=1,nb_SPDCx
       CALL injj_SPDCx(icdan,this(icdan)%rls,this(icdan)%rlt,this(icdan)%rln,iIreac)
    END DO
    
  END SUBROUTINE smooth_computation_SPDCx

!!!------------------------------------------------------------------------
  subroutine display_prox_tactors_SPDCx

    implicit none
    integer          :: iadj,icdan,icdbdy,icdtac,ianbdy,iantac
    character(len=5) :: cdmodel, anmodel
    
    nb_SPHER=get_nb_SPHER()
    
    DO icdtac=1,nb_SPHER    
       DO iadj=1,nb_adj(icdtac)         
          icdan  = adjac(icdtac)%icdan(iadj)
          icdbdy = this(icdan)%icdbdy
          !icdtac = this(icdan)%icdtac
          ianbdy = this(icdan)%ianbdy
          iantac = this(icdan)%iantac
          cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
          anmodel = get_body_model_name_from_id( dnlyc2bdyty(3,iantac) )
          WRITE(*,'(A1)')' '
          WRITE(*,'(A6,2X,I5)')'$icdan',icdan     
          !123456789012345678901234567890123456789012345678901234567890123456789012
          WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '
          WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               cdmodel,icdbdy,'SPHER',icdtac,see(this(icdan)%isee)%behav,  &
               anmodel,ianbdy,'DNLYC',iantac
          
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
    
  end subroutine display_prox_tactors_SPDCx
!!!------------------------------------------------------------------------  
  SUBROUTINE stock_rloc_SPDCx

    IMPLICIT NONE

    INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    INTEGER :: errare

    character(len=80) :: cout
                               !123456789012345678901
    character(len=21) :: IAM = 'mod_SPCDx::stock_rloc'

    nb_SPHER=get_nb_SPHER()

    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_SPHER),stat=errare)
       IF (errare .NE.0 ) THEN
          call faterr(IAM,'Error allocating verlt')
       END IF
       DO icdtac=1,nb_SPHER
          verlt(icdtac)%adjsz=0
          iadj=nb_adj(icdtac)
          IF (iadj > 0) THEN
             verlt(icdtac)%adjsz=iadj
             call new_verlet_(icdtac, iadj, errare)
          ELSE
             call nullify_verlet_(icdtac)
          END IF
          IF (errare .NE.0 ) THEN
             write(cout,'(A,I0,A)') 'Error allocating verlt(',icdtac,')%.....'
             call faterr(IAM,cout)
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
          ELSE
             call nullify_verlet_(icdtac)
          END IF
       END DO
    END IF
    
    ! filling data:
    DO icdan=1,nb_SPDCx
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       iadj   = this(icdan)%iadj

       verlt(icdtac)%icdan(iadj)    = icdan
       verlt(icdtac)%cdbdy          = spher2bdyty(1,icdtac)
       verlt(icdtac)%cdtac          = spher2bdyty(2,icdtac)
       verlt(icdtac)%cdmodel        = spher2bdyty(3,icdtac)
       verlt(icdtac)%cdsci(iadj)    = this(icdan)%icdsci
       verlt(icdtac)%anbdy(iadj)    = dnlyc2bdyty(1,iantac)
       verlt(icdtac)%antac(iadj)    = dnlyc2bdyty(2,iantac)
       verlt(icdtac)%anmodel(iadj)  = dnlyc2bdyty(3,iantac)
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

    nb_vSPDCx = nb_SPDCx

    WRITE(cout,'(1X,I10,A12)') nb_vSPDCx,' stock SPDCx'
    call logmes(cout)
  END SUBROUTINE stock_rloc_SPDCx
!!!------------------------------------------------------------------------ 
  SUBROUTINE recup_rloc_SPDCx

    IMPLICIT NONE

    INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    character(len=80) :: cout
   
    if (.not. allocated(verlt)) then
       call logmes('[mod_SPDCx::recup_rloc] Warning: verlt not allocated, no recup done')
       return
    end if

    nb_recup_SPDCx = 0

    DO icdan=1,nb_SPDCx
       this(icdan)%rls=0.D0
       this(icdan)%rlt=0.D0
       this(icdan)%rln=0.D0
       this(icdan)%statusBEGIN=i_nknow

       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac
       
       IF (verlt(icdtac)%adjsz .NE. 0) THEN
         if ( verlt(icdtac)%cdbdy  == spher2bdyty(1,icdtac) .and. &
              verlt(icdtac)%cdtac  == spher2bdyty(2,icdtac) .and. &
              verlt(icdtac)%cdmodel== spher2bdyty(3,icdtac) ) then
           do iadj = 1, verlt(icdtac)%adjsz
             if ( verlt(icdtac)%anbdy(iadj)  == dnlyc2bdyty(1,iantac) .and. &
                  verlt(icdtac)%antac(iadj)  == dnlyc2bdyty(2,iantac) .and. &
                  verlt(icdtac)%anmodel(iadj)== dnlyc2bdyty(3,iantac) ) then
                this(icdan)%rls = verlt(icdtac)%rls(iadj)*H
                this(icdan)%rlt = verlt(icdtac)%rlt(iadj)*H
                this(icdan)%rln = verlt(icdtac)%rln(iadj)*H

                this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

                this(icdan)%internal(1:max_internal_tact) = verlt(icdtac)%internal(1:max_internal_tact,iadj)
                nb_recup_SPDCx = nb_recup_SPDCx + 1
                exit
             end if
           end do
         end if
       ENDIF
    END DO

    WRITE(cout,'(1X,I10,A12)') nb_recup_SPDCx,' recup SPDCx'
    call logmes(cout)

  END SUBROUTINE recup_rloc_SPDCx
!!!------------------------------------------------------------------------ 
  subroutine read_ini_Vloc_Rloc 
    
    implicit none
    integer            :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
    integer            :: cdmodel, anmodel
    real(kind=8)       :: rls,rlt,rln,PTx,PTy,PTz,gapTT,vls,vlt,vln
    character(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
    integer            :: errare,icdtact
    integer            :: ibehav,nb_internal,i_internal

    character(len=29)  :: IAM = 'mod_SPDC::read_ini_Vloc_Rloc'
    character(len=103) :: cout

    nb_SPHER = get_nb_SPHER()
    errare=0
    IF (.NOT. ALLOCATED(nb_adj)) ALLOCATE(nb_adj(nb_SPHER),stat=errare)
    IF (errare .NE.0 ) THEN
       CALL FATERR(IAM,'error allocating nb_adj')
    END IF
    
    nb_adj = 0
        
    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) .NE. 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13).NE. 'SPDCx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
            cdbdy,icdbdy,cdtac,icdtac,                                          &
            behav,                                                              &
            anbdy,ianbdy,antac,iantac,                                          &
            sttus
       IF (cdtac .NE. 'SPHER' .OR. antac .NE. 'DNLYC') CYCLE
       cdmodel = get_body_model_id_from_name( cdbdy )
       do icdtact = 1, nb_SPHER
          if ( spher2bdyty(1,icdtact) == icdbdy .and. &
               spher2bdyty(2,icdtact) == icdtac .and. &
               spher2bdyty(3,icdtact) == cdmodel ) then
             nb_adj(icdtact) = nb_adj(icdtact) + 1
             exit
          end if
       end do
    END DO
    
    IF (.NOT. ALLOCATED(verlt)) THEN
       ALLOCATE(verlt(nb_SPHER),stat=errare)
       IF (errare .NE.0 ) THEN
          CALL FATERR(IAM,' error allocating verlt')
       END IF
       DO icdtac=1,nb_SPHER
          verlt(icdtac)%adjsz=0
          iadj=nb_adj(icdtac)
          IF (iadj > 0) THEN
             verlt(icdtac)%adjsz=iadj
             call new_verlet_(icdtac, iadj, errare)
          ELSE
             call nullify_verlet_(icdtac)
          END IF
          IF (errare .NE.0 ) THEN
             CALL FATERR(IAM,'error in allocating verlt(icdtac)%.....')
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
          ELSE
             call nullify_verlet_(icdtac)
          END IF
       END DO
    END IF

    ! second reading: filling data
    REWIND(G_nfich)
    nb_adj = 0
    icdan  = 0

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) .NE. 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
       IF (G_clin(9:13).NE. 'SPDCx') CYCLE     
       IF ( .NOT. read_G_clin()) EXIT
       IF ( .NOT. read_G_clin()) EXIT
       READ(G_clin(1:76),'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')  &
            cdbdy,icdbdy,cdtac,icdtac,                                                 &
            behav,                                                              &
            anbdy,ianbdy,antac,iantac,                                                 &
            sttus
       IF (cdtac .NE. 'SPHER' .OR. antac .NE. 'DNLYC') CYCLE
       cdmodel = get_body_model_id_from_name( cdbdy )
       anmodel = get_body_model_id_from_name( anbdy )
       do icdtact = 1, nb_SPHER
          if ( spher2bdyty(1,icdtact) == icdbdy .and. &
               spher2bdyty(2,icdtact) == icdtac .and. &
               spher2bdyty(3,icdtact) == cdmodel ) then

             icdan = icdan + 1

             nb_adj(icdtact) = nb_adj(icdtact) + 1

             verlt(icdtact)%icdan( nb_adj(icdtact) )= icdan

             verlt(icdtact)%cdbdy                   = icdbdy
             verlt(icdtact)%cdtac                   = icdtac
             verlt(icdtact)%cdmodel                 = cdmodel
             verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
             verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
             verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
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
             IF (G_clin(30:34).EQ. 's(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%suc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%suc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%suc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             ENDIF
             IF( .NOT. read_G_clin()) CYCLE
             IF (G_clin(30:34).EQ. 't(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%tuc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%tuc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%tuc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             ENDIF
             IF( .NOT. read_G_clin()) CYCLE
             IF (G_clin(30:34).EQ. 'n(1)=') THEN
                READ(G_clin(1:90),'(27X,3(7X,D14.7))') PTx,PTy,PTz
                verlt(icdtact)%nuc(1,nb_adj(icdtact)) = PTx
                verlt(icdtact)%nuc(2,nb_adj(icdtact)) = PTy
                verlt(icdtact)%nuc(3,nb_adj(icdtact)) = PTz
             ELSE 
                BACKSPACE(G_nfich)
             ENDIF
             IF( .NOT. read_G_clin()) CYCLE
             IF (G_clin(30:34).EQ. 'coo1=') THEN
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
             IF (nb_internal .NE. 0 ) THEN  
               IF( .NOT. read_G_clin()) EXIT
               DO i_internal=1, nb_internal
                 READ(G_clin(((i_internal-1)*15)+1:i_internal*15),'(1X,D14.7)') verlt(icdtact)%internal(i_internal,nb_adj(icdtact))
               END DO
            END IF
          END IF
       END DO
    END DO
    nb_vSPDCx=0
    
    DO icdtact = 1,nb_SPHER
       nb_vSPDCx = nb_vSPDCx + nb_adj(icdtact)
       
       IF ( nb_adj(icdtact) .NE. verlt(icdtact)%adjsz ) THEN 
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
    INTEGER :: iadj,icdtact
    INTEGER :: nfich,icdan,icdtac,iantac

    character(len=20) :: fmt
    character(len=5)  :: cdmodel, anmodel
    
    nb_SPHER=get_nb_SPHER()
    
    IF (nb_SPDCx.EQ.0) RETURN
    
    DO icdtact=1,nb_SPHER    
       DO iadj=1,nb_adj(icdtact)         
          icdan  = adjac(icdtact)%icdan(iadj)
          icdtac = this(icdan)%icdtac
          iantac = this(icdan)%iantac
          cdmodel = get_body_model_name_from_id( spher2bdyty(3,icdtac) )
          anmodel = get_body_model_name_from_id( dnlyc2bdyty(3,iantac) )
          WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','SPDCx',icdan  
          !1234567890123456789012345678901234567890123456789012345678901234567890124567
          WRITE(nfich,'(A76)')' cdbdy  numbr  cdtac  numbr  CDVER  behav  anbdy  numbr  antac  numbr  sttus'
          WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,9X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
               !pta old fashion'RBDY3',spher2bdyty(1,icdtac),'SPHER',spher2bdyty(2,icdtac),  &
               cdmodel,get_visibleID_SPHER(icdtac),'SPHER',spher2bdyty(2,icdtac),  &
               see(this(icdan)%isee)%behav,  &
               !pta old fashion 'RBDY3',dnlyc2bdyty(1,iantac),'DNLYC',dnlyc2bdyty(2,iantac),  &
               anmodel,get_visibleID_DNLYC(iantac),'DNLYC',dnlyc2bdyty(2,iantac),  &
               get_contact_status_name_from_id(this(icdan)%status),iantac
          WRITE(nfich,104) 'rls/H',this(icdan)%rls/H,'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H
          WRITE(nfich,104) 'vls =',this(icdan)%vls  ,'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  
          WRITE(nfich,103) 'gTT =',this(icdan)%gapTT
          WRITE(nfich,104) 't(1)=',this(icdan)%tuc(1)     ,'t(2)=',this(icdan)%tuc(2)     ,'t(3)=',this(icdan)%tuc(3)
          WRITE(nfich,104) 'n(1)=',this(icdan)%nuc(1)     ,'n(2)=',this(icdan)%nuc(2)     ,'n(3)=',this(icdan)%nuc(3)
          WRITE(nfich,104) 's(1)=',this(icdan)%suc(1)     ,'s(2)=',this(icdan)%suc(2)     ,'s(3)=',this(icdan)%suc(3)
          WRITE(nfich,104) 'coo1=',this(icdan)%coor(1),'coo2=',this(icdan)%coor(2),'coo3=',this(icdan)%coor(3)
          
          IF (this(icdan)%nb_internal .NE. 0) THEN
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
  SUBROUTINE nullify_reac_SPDCx(icdan,storage)
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in):: icdan 
    INTEGER           :: icdbdy,ianbdy
    INTEGER           :: storage
    
    icdbdy=this(icdan)%icdbdy
    CALL nullify_reac_SPHER(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL nullify_reac_DNLYC(ianbdy,storage)
    
  END SUBROUTINE nullify_reac_SPDCx
!!!------------------------------------------------------------------------ 
  SUBROUTINE nullify_vlocy_SPDCx(icdan,storage)
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy,storage
    
    icdbdy = this(icdan)%icdbdy
    CALL nullify_vlocy_SPHER(icdbdy,storage)
    
    ianbdy = this(icdan)%ianbdy
    CALL nullify_vlocy_DNLYC(ianbdy,storage)
    
  END SUBROUTINE nullify_vlocy_SPDCx
  !------------------------------------------------------------------------ 
  !------------------------------------------------------------------------ 
  SUBROUTINE vitrad_SPDCx( icdan, storage, need_full_vlocy )
    
    IMPLICIT NONE
    
    INTEGER,INTENT(in) :: icdan 
    INTEGER            :: icdbdy,ianbdy
    INTEGER            :: storage
    logical, optional  :: need_full_vlocy
    
    icdbdy=this(icdan)%icdbdy
    CALL comp_vlocy_SPHER(icdbdy,storage)
    
    ianbdy=this(icdan)%ianbdy
    CALL comp_vlocy_DNLYC(ianbdy,storage)
    
  END SUBROUTINE vitrad_SPDCx
  !------------------------------------------------------------------------  
  SUBROUTINE injj_SPDCx(icdan,RSIK,RTIK,RNIK,storage)
    
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

   CALL add_reac_SPHER(icdbdy,cdccdof,cdreac,storage)
   CALL add_reac_DNLYC(ianbdy,anccdof,anreac,storage)

 END SUBROUTINE injj_SPDCx 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_SPDCx(icdan,VSIK,VTIK,VNIK,storage)
 
   IMPLICIT NONE

   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VSIK,VTIK,VNIK
   INTEGER                   :: icdbdy,ianbdy
   INTEGER                   :: storage
   REAL(kind=8),DIMENSION(6) :: Vcd,Van

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy
   Vcd = get_vlocy_SPHER(icdbdy,storage)
   Van = get_vlocy_DNLYC(ianbdy,storage)      

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

 END SUBROUTINE prjj_SPDCx 
!!!------------------------------------------------------------------------ 
  INTEGER FUNCTION get_nb_SPDCx(itactor)
  
    IMPLICIT NONE
    INTEGER :: itactor

    SELECT CASE(itactor)
    CASE(i_real_tactor)
       get_nb_SPDCx = nb_SPDCx
    CASE(i_verlet_tactor)
       get_nb_SPDCx = nb_vSPDCx
    CASE(i_rough_tactor)
       get_nb_SPDCx = nb_rough_SPDCx
    CASE(i_recup_tactor)
       get_nb_SPDCx = nb_recup_SPDCx
    END SELECT

  END FUNCTION get_nb_SPDCx
!!!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
SUBROUTINE SPDCx2ENT(icdan,icdent,ianent)

   IMPLICIT NONE
   INTEGER :: icdan,icdent,ianent
   
   icdent = get_ENT_SPHER(this(icdan)%icdbdy)
   ianent = get_ENT_DNLYC(this(icdan)%ianbdy)

 END SUBROUTINE SPDCx2ENT
!------------------------------------------------------------------------ 
SUBROUTINE SPDCx2SPHER(icdan,icdtac)

   IMPLICIT NONE
   INTEGER :: icdan,icdtac
   
   icdtac = this(icdan)%icdtac

 END SUBROUTINE SPDCx2SPHER
!------------------------------------------------------------------------ 
SUBROUTINE SPDCx2DNLYC(icdan,iantac)

   IMPLICIT NONE
   INTEGER :: icdan,iantac
   
   iantac = this(icdan)%iantac

 END SUBROUTINE SPDCx2DNLYC
!!!------------------------------------------------------------------------
!------------------------------------------------------------------------
!!! < md < !!!
!------------------------------------------------------------------------ 
  LOGICAL FUNCTION RUN_SPDCx()

    IMPLICIT NONE
    
    RUN_SPDCx = RUN_TACTOR

  END FUNCTION RUN_SPDCx
!!!------------------------------------------------------------------------
  logical function CHECK_SPDCx()
    implicit none
    !   
    integer :: isee

    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_SPDCx = check_SPDCx_
      return
    end if

    con_pedigree%module_name = 'SPDCx'

    con_pedigree%id_cdan  = i_spdcx
    con_pedigree%id_cdtac = i_spher
    con_pedigree%id_antac = i_dnlyc

    cdtact2bdyty => spher2bdyty
    antact2bdyty => dnlyc2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.

    ! checking if enough cd/an
    nb_SPHER = get_nb_SPHER()
    nb_DNLYC = get_nb_DNLYC()
    if( nb_SPHER == 0 .or. nb_DNLYC == 0 ) then
      CHECK_SPDCx = check_SPDCx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'SPHER' .and. see(isee)%antac == 'DNLYC') then
        check_SPDCx_ = .true.
        exit
      end if
    end do

    CHECK_SPDCx = check_SPDCx_
    return

  end function CHECK_SPDCx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_SPDCx()
    
    IMPLICIT NONE
    
    get_write_Vloc_Rloc_SPDCx = write_Vloc_Rloc
    
  END FUNCTION get_write_Vloc_Rloc_SPDCx
!!!--------------------------------------------------------------------
  REAL(kind=8) FUNCTION get_surf_SPDCx(icdan)

    IMPLICIT NONE
    INTEGER      :: icdan
    REAL(kind=8) :: raycd,rayan

    raycd   = 0.1*get_radius_SPHER(this(icdan)%icdtac)

    get_surf_SPDCx = PI_g*raycd*raycd

  END FUNCTION get_surf_SPDCx
!!!------------------------------------------------------------------------ 
  SUBROUTINE update_cohe_SPDCx(icdan,cohe)

   IMPLICIT NONE
   INTEGER      :: icdan,itact
   REAL(kind=8) :: cohe,WScd,WSan

   itact = this(icdan)%icdtac

   WScd = get_WS_SPHER(spher2bdyty(1,itact),spher2bdyty(2,itact))

   itact = this(icdan)%iantac

   WSan = get_WS_DNLYC(dnlyc2bdyty(1,itact),dnlyc2bdyty(2,itact))

   IF (ABS(WScd+WSan).LT.1.D-16) THEN
      cohe = 0.D0
   ELSE
      cohe = (WSan*WScd)/(WScd+WSan)
   END IF

 END SUBROUTINE update_cohe_SPDCx
!------------------------------------------------------------------------ 

 subroutine clean_memory_SPDCx
   implicit none
   integer(kind=4) :: i, j, k

   call clean_memory_inter_meca_()

   nb_DNLYC       = 0
   nb_SPDCx       = 0
   nb_vSPDCx      = 0
   nb_recup_SPDCx = 0

   nb_rough_SPDCx = 0
   if( allocated(rough_SPDCx) ) deallocate(rough_SPDCx)

   ! Root, Current and Previous should always be null outside creation_tab_visu

   if( allocated(SPcoor) ) deallocate(SPcoor)
   if( allocated(CDcoor) ) deallocate(CDcoor)

   Reac_SPDCx_MAX = 0.D0

   module_checked_ = .FALSE.
   check_SPDCx_    = .FALSE.

 end subroutine

 subroutine set_nb_SPDCx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_SPDCx = nb

 end subroutine

 subroutine redo_nb_adj_SPDCx()
   implicit none

   call redo_nb_adj_( get_nb_SPHER() )

 end subroutine

END MODULE SPDCx

