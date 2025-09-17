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
MODULE CLJCx
  
  !!****h* LMGC90.CORE/CLJCx
  !! NAME
  !!  module CLJCx
  !! PURPOSE
  !!  This modulus deals with geoemetric and kinematic operations
  !!  between contactors CLxxx and JONCx.
  !!  In this modulus candidate contactors are CLxxx and antagonist 
  !!  contactors are JONCx
  !! USES
  !!  LMGC90.CORE/OVERALL
  !!  LMGC90.CORE/TACT_BEHAVIOUR
  !!  LMGC90.CORE/JONCx
  !!  LMGC90.CORE/MAILx
  !!  LMGC90.CORE/CLxxx
  !!****

  USE overall
  USE tact_behaviour

  use JONCx, only : get_nb_joncx, joncx2bdyty           , &
                    get_axes_joncx, print_info_joncx    , &
                    get_ent_joncx       => get_ent      , &
                    get_color_joncx     => get_color    , &
                    get_coorTT_joncx    => get_coorTT   , &
                    add_reac_joncx      => add_reac     , &
                    get_vlocy_joncx     => get_vlocy    , &
                    comp_vlocy_joncx    => comp_vlocy   , &
                    nullify_reac_joncx  => nullify_reac , &
                    nullify_vlocy_joncx => nullify_vlocy
  use CLxxx

  use MAILx, only : get_color_MAILx
  use RBDY2, only : get_color_RBDY2 => get_color
  use MBS2D, only : get_color_MBS2D => get_color

  use parameters, only : i_cljcx, i_clxxx, i_joncx, i_mailx, i_rbdy2, i_mbs2

  use inter_meca_2D

  implicit none

  private

  LOGICAL :: bavard = .FALSE.
  
  type(T_interaction), dimension(:), allocatable, target :: this

  !fd < a merger
  
  type(T_con),target :: con_pedigree 

  integer, dimension(:,:), pointer :: cdtact2bdyty => null()
  integer, dimension(:,:), pointer :: antact2bdyty => null()

  !------------------------------------------------------------------------ 
  ! nb_CLJCx = number of selected candidates CLxxx against JONCx <= size(this).
  INTEGER          :: nb_CLJCx                  

  INTEGER          :: nb_vCLJCx=0

  !------------------------------------------------------------------------ 


 type( T_this_adjac ), dimension( : ), allocatable, target :: adjac

!------------------------------------------------------------------------  

 integer, dimension( : ), allocatable, target :: nb_adj  ! nb_adj(icdtac): number of adjacent pairs CLxxx-JONCx
                                                         ! to candidate contactor CLxxx icdtac.

!------------------------------------------------------------------------

 type(T_verlet), dimension(:), allocatable, target :: verlt

!------------------------------------------------------------------------

TYPE T_rough_CLJCx

  INTEGER :: ijoncx, & !la fontiere la plus proche
             isee 
  REAL(kind=8) :: adist 
  REAL(kind=8),DIMENSION(2) :: axes
END TYPE T_rough_CLJCx

TYPE(T_rough_CLJCx),DIMENSION(:),ALLOCATABLE :: rough_CLJCx

INTEGER :: nb_rough_CLJCx,nstep_rough_seek_CLJCx=1
integer :: nb_recup_CLJCx
LOGICAL :: RUN=.FALSE.

logical :: module_checked_ = .FALSE.
logical :: check_CLJCx_    = .FALSE.

!------------------------------------------------------------------------ 

 REAL(kind=8) :: Reac_CLJCx_MAX=0.D0
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: CLcoor  !  coordinates of CL candidate
 REAL(kind=8),DIMENSION(:,:),ALLOCATABLE :: JCcoor  !  coordinates of JC antagoniste

 real(kind=8), dimension(:), allocatable, target :: violation

!------------------------------------------------------------------------

! liste des fonctions publiques 

 PUBLIC &
      stock_rloc_CLJCx, &
      recup_rloc_CLJCx, &
      compute_box_CLJCx, &
      read_ini_Vloc_Rloc_CLJCx, &
      write_xxx_Vloc_Rloc_CLJCx, &
      display_prox_tactors_CLJCx, &
      coor_prediction_CLJCx, &
      creation_tab_visu_CLJCx, &
      compute_contact_CLJCx, &
      RUN_CLJCx, &
      CHECK_CLJCx, &
      get_write_Vloc_Rloc_CLJCx

 PUBLIC &
      nullify_reac_CLJCx, &
      nullify_vlocy_CLJCx, &
      injj_CLJCx, prjj_CLJCx, vitrad_CLJCx, & 
      get_nb_CLJCx, &
      get_length_CLJCx, &
      print_info_CLJCx, &
      clean_memory_CLJCx

 !rm for handler
 public get_this    , &
        set_nb_CLJCx, &
        redo_nb_adj_CLJCx, &
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


!!!------------------------------------------------------------------------
  SUBROUTINE coor_prediction_CLJCx

    IMPLICIT NONE
    
    INTEGER :: nb_CLxxx, nb_JONCx,ibdy,itac,itact,errare
    INTEGER :: iclxxx,ijoncx

    nb_CLxxx=get_nb_CLxxx()
    nb_JONCx=get_nb_JONCx()

    DO iclxxx=1,nb_CLxxx

       CLcoor(1:2,iclxxx) = get_coorTT_CLxxx(iclxxx)

    END DO

    ! coordonnee de a et b

    DO ijoncx=1,nb_JONCx
       JCcoor(1:3,ijoncx) = get_coorTT_JONCx(ijoncx)
    END DO

  END SUBROUTINE coor_prediction_CLJCx
!!!------------------------------------------------------------------------
  !> \brief Read a VlocRloc file to initialize database
  subroutine read_ini_Vloc_Rloc_CLJCx(step)
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
    
  end subroutine read_ini_Vloc_Rloc_CLJCx
!!!------------------------------------------------------------------------
  SUBROUTINE write_xxx_Vloc_Rloc_CLJCx(which)
    
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
    
  END SUBROUTINE write_xxx_Vloc_Rloc_CLJCx
!!!------------------------------------------------------------------------ 
 SUBROUTINE compute_box_CLJCx

   IMPLICIT NONE

   INTEGER :: nb_CLxxx, nb_JONCx,ial,errare
   INTEGER :: itact,ibdy,icl
                              !1234567890123456789012
   character(len=22) :: IAM = 'mod_CLJCx::compute_box'

   nb_CLxxx=get_nb_CLxxx()
   nb_JONCx=get_nb_JONCx()

   IF (ALLOCATED(nb_adj)) DEALLOCATE(nb_adj)
   ALLOCATE(nb_adj(nb_CLxxx),stat=errare)
   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating nb_adj')
   END IF    

   nb_adj=0
        
   IF (.NOT. ALLOCATED(adjac)) THEN
     ALLOCATE(adjac(nb_CLxxx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating adjac')
     END IF
     DO itact=1,nb_CLxxx
       NULLIFY(adjac(itact)%icdan)
! on gagne du temps dans les CL-... on n'aura tjs qu'un seul antagoniste pour un CL (?!)
       ALLOCATE(adjac(itact)%icdan(1))

     END DO
   ELSE
     DO itact=1,nb_CLxxx
       IF (ASSOCIATED(adjac(itact)%icdan))  DEALLOCATE(adjac(itact)%icdan)
       NULLIFY(adjac(itact)%icdan)
!
! pour gagner du temps ... un CL ne vera jamais qu'un iantac
!
       ALLOCATE(adjac(itact)%icdan(1))
!
     ENDDO
   ENDIF  
  
   IF (ALLOCATED(CLcoor)) DEALLOCATE(CLcoor)
   ALLOCATE(CLcoor(2,nb_CLxxx),stat=errare)   

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating CL_coor')
   END IF    


   IF (ALLOCATED(JCcoor)) DEALLOCATE(JCcoor)
   ALLOCATE(JCcoor(3,nb_JONCx),stat=errare)

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating JONCx')
   END IF    
 
   IF (ALLOCATED(rough_CLJCx)) DEALLOCATE(rough_CLJCx)
   ALLOCATE(rough_CLJCx(nb_CLxxx),stat=errare)   

   IF (errare /=0 ) THEN
     call faterr(IAM,'Error allocating rough_CLJCx')
   END IF    

   DO icl=1,nb_CLxxx
     rough_CLJCx(icl)%ijoncx=0
     rough_CLJCx(icl)%isee=0     
     rough_CLJCx(icl)%adist=0.d0     
     rough_CLJCx(icl)%axes(:)=(/ 0.d0, 0.d0 /)     
   ENDDO


 END SUBROUTINE compute_box_CLJCx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE creation_tab_visu_CLJCx
 
   IMPLICIT NONE 
 
   INTEGER      :: errare 
   INTEGER      :: isee
   INTEGER      :: nb_CLxxx, nb_JONCx,icdtac,iantac,iadj
   REAL(kind=8) :: adist
   REAL(kind=8),DIMENSION(2) :: axes
   CHARACTER(len=5) :: cdcol,ancol,anbdyty
   character(len=80) :: cout

   nb_CLxxx=get_nb_CLxxx()
   nb_JONCx=get_nb_JONCx()

   nb_rough_CLJCx=0 

   DO icdtac=1,nb_CLxxx
     if (.not.get_visible_CLxxx(icdtac)) cycle

     cdcol=get_color_MAILx(clxxx2bdyty(1,icdtac),clxxx2bdyty(2,icdtac))
 
     iadj=0

     DO iantac=1,nb_JONCx
       ancol   = get_color_JONCx(iantac)
       anbdyty = get_body_model_name_from_id(joncx2bdyty(3,iantac))
       isee    = get_isee('MAILx','CLxxx',cdcol,anbdyty,'JONCx',ancol)

       IF (isee == 0) THEN
         rough_CLJCx(icdtac)%ijoncx = 0
         rough_CLJCx(icdtac)%isee=0
         rough_CLJCx(icdtac)%adist=0.d0
         rough_CLJCx(icdtac)%axes(:)=(/ 0.d0, 0.d0 /)     
         CYCLE
       ELSE
         axes=get_axes_joncx(iantac)

         adist=see(isee)%alert
         adist=0.1005D+01*adist

         IF (test_roughly_distance(CLcoor(1,icdtac),CLcoor(2,icdtac),&
                                   JCcoor(1,iantac),JCcoor(2,iantac),JCcoor(3,iantac),&
                                   axes(1),axes(2),adist) ) THEN     


           nb_rough_CLJCx=nb_rough_CLJCx+1
           iadj=iadj+1                        

           rough_CLJCx(icdtac)%ijoncx=iantac
           rough_CLJCx(icdtac)%isee=isee
           rough_CLJCx(icdtac)%adist=see(isee)%alert
           rough_CLJCx(icdtac)%axes=axes
           EXIT
         ELSE
           rough_CLJCx(icdtac)%ijoncx = 0
           rough_CLJCx(icdtac)%isee=0
           rough_CLJCx(icdtac)%adist=0.d0
           rough_CLJCx(icdtac)%axes(:)=(/ 0.d0, 0.d0 /)      
           CYCLE
         END IF
       ENDIF
     ENDDO

     IF (iadj > 1) THEN
       call faterr('mod_CLJCx::creation_tab_visu','iadj should be less or equal than 1')
     ENDIF
   ENDDO

   WRITE(cout,'(4X,I10,A20)') nb_rough_CLJCx,' CLJCx roughly found'
   call logmes(cout)

   IF (ALLOCATED(this)) DEALLOCATE(this)
   ALLOCATE(this(nb_rough_CLJCx),stat=errare)
   IF (errare /=0 ) THEN
     call faterr('mod_CLJCx::creation_tab_visu','Error allocating this')
   END IF 


 END SUBROUTINE creation_tab_visu_CLJCx
!------------------------------------------------------------------------
!------------------------------------------------------------------------
SUBROUTINE compute_contact_CLJCx
 
  IMPLICIT NONE  

  INTEGER                               :: errare 
  INTEGER                               :: nb_CLxxx, nb_JONCx,icdan,iadj,itact,icdbdy,ianbdy,icdtac,iantac,isee   

  REAL(kind=8),DIMENSION(2)             :: cd_Vbegin,axes
  REAL(kind=8),DIMENSION(3)             :: an_Vbegin
  REAL(kind=8)                          :: GAP,UN2,UN1,UT2,UT1,adist,Gann3,Gant3,XP2,YP2

  INTEGER                               :: cd_ent,an_ent

  character(len=80) :: cout

   nb_CLxxx=get_nb_CLxxx()
   nb_JONCx=get_nb_JONCx()


   icdan=0

   DO icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   END DO

   DO icdtac=1,nb_CLxxx     
     if (.not.get_visible_CLxxx(icdtac)) cycle
     iadj=0
     iantac=rough_CLJCx(icdtac)%ijoncx
     IF (iantac /= 0) THEN
       adist=rough_CLJCx(icdtac)%adist 

       CALL local_framing(CLcoor(1,icdtac),CLcoor(2,icdtac), &
                          JCcoor(1,iantac),JCcoor(2,iantac),JCcoor(3,iantac), &
                          rough_CLJCx(icdtac)%axes(1),rough_CLJCx(icdtac)%axes(2), &
                          icdtac,iantac, &
                          ut1,ut2,un1,un2,gap,Gant3,Gann3,XP2,YP2)
       ! 
       ! checking distance against alert distance           
       !

       IF (gap .LE. adist) THEN 

         icdan=icdan+1
         iadj=iadj+1

         adjac(icdtac)%icdan(iadj) = icdan

         this(icdan)%iadj          = iadj
         this(icdan)%icdbdy        = l_clxxx(icdtac)%ibdyty
         this(icdan)%icdtac        = icdtac
         this(icdan)%icdsci        = 0
         this(icdan)%ianbdy        = joncx2bdyty(1,iantac)
         this(icdan)%iantac        = iantac
         this(icdan)%iansci        = 0

         this(icdan)%icdbtac = clxxx2bdyty(2, icdtac)
         this(icdan)%ianbtac = joncx2bdyty(2, iantac)

         this(icdan)%icdbtyp = clxxx2bdyty(3, icdtac)
         this(icdan)%ianbtyp = joncx2bdyty(3, iantac)

         this(icdan)%icdctyp = i_clxxx
         this(icdan)%ianctyp = i_joncx

         cd_ent = get_ent_CLxxx(icdtac)
         an_ent = get_ent_JONCx(this(icdan)%iantac)

         this(icdan)%icdent = cd_ent
         this(icdan)%ianent = an_ent

         IF ( bavard ) THEN

           PRINT*,'////////////////////////////'
           PRINT*,'entite candidate:',cd_ent
           PRINT*,'entite antagoniste:',an_ent
           PRINT*,'||||||||||||||||||||||||||||'

         ENDIF

         if (cd_ent /= an_ent) then
           entity(cd_ent)%nb = entity(cd_ent)%nb+1
           entity(an_ent)%nb = entity(an_ent)%nb+1
         else
           entity(cd_ent)%nb = entity(cd_ent)%nb+1
         end if

         this(icdan)%isee          = rough_CLJCx(icdtac)%isee
                 
         this(icdan)%nuc(:)    =  (/ un1,un2 /)
         this(icdan)%tuc(:)    =  (/ ut1,ut2 /)
         this(icdan)%gapTTBEGIN  =  gap

         this(icdan)%Gant3=Gant3
         this(icdan)%Gann3=Gann3

         CALL get_vlocy_CLxxx(icdtac,iVbeg_,cd_Vbegin)

         CALL get_vlocy_JONCx(iantac,iVbeg_,an_Vbegin)

         this(icdan)%vltBEGIN  =  (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%tuc(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%tuc(2) &
                                 - an_Vbegin(3)*this(icdan)%Gant3
         this(icdan)%vlnBEGIN  =  (cd_Vbegin(1)-an_Vbegin(1))*this(icdan)%nuc(1) &
                                 +(cd_Vbegin(2)-an_Vbegin(2))*this(icdan)%nuc(2) &
                                 - an_Vbegin(3)*this(icdan)%Gann3
         this(icdan)%rlt=0.D0
         this(icdan)%rln=0.D0
         this(icdan)%vlt=this(icdan)%vltBEGIN
         this(icdan)%vln=this(icdan)%vlnBEGIN
         this(icdan)%gapTT=this(icdan)%gapTTBEGIN
         this(icdan)%status=i_nknow

         this(icdan)%coor=(/ xp2,yp2 /)

       END IF
     ENDIF

     IF (iadj > SIZE(adjac(icdtac)%icdan)) THEN
       call faterr('mod_CLJCx::compute_contact','Extra adjacent contacts found when refining !')
     END IF
     nb_adj(icdtac)=iadj
     ! Since selection of candidates for contact has been refined, nb_adj(icdtac) is less or equal
     ! size(adjac(icdtac)%icdan).
     ! Loops are now to run from 1 to nb_adj(icdtac) where data are available.
   END DO
  
   nb_CLJCx=icdan

   WRITE(cout,'(1X,I10,A12)') nb_CLJCx,' CLJCx found'
   call logmes(cout)


  ! Since selection of candidates for contact has been refined, nb_CLJCx is less or equal size(this). 
  ! Loops are now to run from 1 to nb_CLJCx where data are available.


   do icdan = 1, nb_CLJCx
      call get_behaviour_( icdan, see, tact_behav )
   end do

   IF (ALLOCATED(violation)) DEALLOCATE(violation)
   ALLOCATE(violation(nb_CLJCx),stat=errare)
 
END SUBROUTINE compute_contact_CLJCx
!------------------------------------------------------------------------
!------------------------------------------------------------------------ 
 subroutine display_prox_tactors_CLJCx

   implicit none
   integer          :: nb_CLxxx, iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,icdtact
   character(len=5) :: cdmodel, anmodel

   nb_CLxxx=get_nb_CLxxx()

   DO icdtact=1,nb_CLxxx    
     if (.not.get_visible_CLxxx(icdtact)) cycle
     DO iadj=1,nb_adj(icdtact)         
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( clxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( joncx2bdyty(3,iantac) )

       WRITE(*,'(A1)')' '
       WRITE(*,'(A1)')' '
       WRITE(*,'(A6,2X,I5)')'$icdan',icdan
                       !123456789012345678901234567890123456789012345678901234567890123456789012
       WRITE(*,'(A72)')' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr          '   
       WRITE(*,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,clxxx2bdyty(1,icdtac),'CLxxx',clxxx2bdyty(2,icdtac),see(this(icdan)%isee)%behav,  &
       anmodel,joncx2bdyty(1,iantac),'JONCx',joncx2bdyty(2,iantac)
       WRITE(*,104)'t(1)=',this(icdan)%tuc(1),'n(1)=',this(icdan)%nuc(1),'s(1)=',0.D0
       WRITE(*,104)'t(2)=',this(icdan)%tuc(2),'n(2)=',this(icdan)%nuc(2),'s(2)=',0.D0
       WRITE(*,104)'vlt-=',this(icdan)%vltBEGIN,'vln-=',this(icdan)%vlnBEGIN,'vls-=',0.D0
       WRITE(*,'(27X,2X,A5,D14.7)')'gap-=',this(icdan)%gapTTBEGIN
       WRITE(*,'(A1)')' '               
     END DO                           
   END DO

104  FORMAT(1X,5X,2X,5X,2X,5X,2X,5X,3(2X,A5,D14.7))
   
 END SUBROUTINE display_prox_tactors_CLJCx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE stock_rloc_CLJCx
 
   
   ! get data from this and put into verlt
   !           
 
   IMPLICIT NONE
   INTEGER :: nb_CLxxx,icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   INTEGER :: errare

   character(len=80) :: cout
                              !123456789012345678901
   character(len=21) :: IAM = 'mod_CLJCx::stock_rloc'
   nb_CLxxx=get_nb_CLxxx()

  ! sizing verlt:
   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_CLxxx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF
     DO icdtac=1,nb_CLxxx
       verlt(icdtac)%adjsz=0
       if (.not.get_visible_CLxxx(icdtac)) cycle
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

     DO icdtac=1,nb_CLxxx
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
   DO icdan=1,nb_CLJCx
     icdtac = this(icdan)%icdtac                      ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac                      ! serial number of antagonist contactor for contact icdan
     iadj   = this(icdan)%iadj                        ! serial adjacent number of pair contactor 
                                                      ! adjacent to candidate contactor for contact icdan
     verlt(icdtac)%icdan(iadj)   = icdan
     verlt(icdtac)%cdbdy         = clxxx2bdyty(1,icdtac)
     verlt(icdtac)%cdtac         = clxxx2bdyty(2,icdtac)
     verlt(icdtac)%cdmodel       = clxxx2bdyty(3,icdtac)
     verlt(icdtac)%cdsci(iadj)   = this(icdan)%icdsci
     verlt(icdtac)%ansci(iadj)   = this(icdan)%iansci
     verlt(icdtac)%anbdy(iadj)   = joncx2bdyty(1,iantac)
     verlt(icdtac)%antac(iadj)   = joncx2bdyty(2,iantac)
     verlt(icdtac)%anmodel(iadj) = joncx2bdyty(3,iantac)
     verlt(icdtac)%rlt(iadj)     = this(icdan)%rlt/H
     verlt(icdtac)%rln(iadj)     = this(icdan)%rln/H
     verlt(icdtac)%vlt(iadj)     = this(icdan)%vlt
     verlt(icdtac)%vln(iadj)     = this(icdan)%vln
     verlt(icdtac)%gapTT(iadj)   = this(icdan)%gapTT
     verlt(icdtac)%status(iadj)  = this(icdan)%status
     verlt(icdtac)%nuc(1:2,iadj) = this(icdan)%nuc(1:2)
     verlt(icdtac)%coor(1:2,iadj)= this(icdan)%coor(1:2)

     verlt(icdtac)%internal(1:max_internal_tact,iadj) = this(icdan)%internal(1:max_internal_tact)

   END DO

   nb_vCLJCx = nb_CLJCx

   WRITE(cout,'(1X,I10,A12)') nb_vCLJCx,' stock CLJCx'
   call logmes(cout)

 END SUBROUTINE stock_rloc_CLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE recup_rloc_CLJCx
 
   !
   ! get data from Verlet list verlt and put into this
   !                                      
   
   IMPLICIT NONE
   INTEGER :: icdan,icdbdy,icdtac,ianbdy,iantac,iadj
   CHARACTER(len=21)  :: IAM = 'mod_CLJCx::recup_rloc'
   character(len=80) :: cout

   if (.not. allocated(verlt)) then
      call logmes('['//IAM//'] Warning: verlt not allocated, no recup done')
      return
   end if

   nb_recup_CLJCx=0

   DO icdan=1,nb_CLJCx
     this(icdan)%rlt=0.D0
     this(icdan)%rln=0.D0
     this(icdan)%statusBEGIN=i_nknow
     icdtac = this(icdan)%icdtac                     ! serial number of candidate contactor for contact icdan
     iantac = this(icdan)%iantac             ! serial number of antagonist contactor for contact icdan

     IF (verlt(icdtac)%adjsz /= 0) THEN
       if (verlt(icdtac)%cdmodel == clxxx2bdyty(3,icdtac) .and. &
           verlt(icdtac)%cdbdy   == clxxx2bdyty(1,icdtac) .and. &
           verlt(icdtac)%cdtac   == clxxx2bdyty(2,icdtac)       &
          ) then
          do iadj = 1, verlt(icdtac)%adjsz
            if (verlt(icdtac)%anmodel(iadj)== joncx2bdyty(3,iantac) .and. &
                verlt(icdtac)%anbdy(iadj)  == joncx2bdyty(1,iantac) .and. &
                verlt(icdtac)%antac(iadj)  == joncx2bdyty(2,iantac)       &
               ) then
              this(icdan)%rlt    = verlt(icdtac)%rlt(iadj)*H 
              this(icdan)%rln    = verlt(icdtac)%rln(iadj)*H 
              this(icdan)%statusBEGIN = verlt(icdtac)%status(iadj)

              this(icdan)%internal(1:max_internal_tact)=verlt(icdtac)%internal(1:max_internal_tact,iadj)
              nb_recup_CLJCx = nb_recup_CLJCx + 1
              exit
            end if
          end do
       end if
     END IF
   END DO

   WRITE(cout,'(1X,I10,A12)') nb_recup_CLJCx,' recup CLJCx'
   call logmes(cout)

 END SUBROUTINE recup_rloc_CLJCx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------ 
 SUBROUTINE read_ini_Vloc_Rloc
 
   !
   ! get data from file Vloc_Rloc.INI and put into a Verlet list
   !                                      
   
   IMPLICIT NONE
   CHARACTER(len=103) :: clin
   INTEGER            :: nb_CLxxx, nb_JONCx,icdan,icdbdy,icdtac,ianbdy,iantac,iadj,icdtact
   REAL(kind=8)       :: rlt,rln,vlt,vln,gapTT
   REAL(kind=8),DIMENSION(2) :: nuc,coor
   CHARACTER(len=5)   :: cdbdy,cdtac,anbdy,antac,behav,sttus
   INTEGER            :: errare 

   INTEGER :: ibehav,nb_internal,i_internal, cdmodel, anmodel

   character(len=80)  :: cout
   !                            12345678901234567890123456789      
   character(len=29)  :: IAM = 'mod_CLJCx::read_ini_Vloc_Rloc' 

   nb_CLxxx=get_nb_CLxxx()
   nb_JONCx=get_nb_JONCx()

  ! first reading: sizing verlt
  ! Since in_Vloc_Rloc is the record, adjacent contactors have to be selected.  
  ! For this purpose nb_adj is introduced.

   IF (.NOT. ALLOCATED(nb_adj)) then
     ALLOCATE(nb_adj(nb_CLxxx),stat=errare)
     IF (errare /=0 ) call FATERR (IAM,' error allocating nb_adj')
   END IF    

   DO icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   END DO

   DO    
    IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'CLJCx') CYCLE     
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,                                          &
     sttus

     cdmodel = get_body_model_id_from_name( cdbdy )

     do icdtact = 1, nb_CLxxx
       if (clxxx2bdyty(1,icdtact) == icdbdy .and. &
           clxxx2bdyty(2,icdtact) == icdtac .and. &
           clxxx2bdyty(3,icdtact) == cdmodel ) then
         nb_adj(icdtact) = nb_adj(icdtact) + 1
         exit
       end if
     end do
   END DO   

   IF (.NOT. ALLOCATED(verlt)) THEN
     ALLOCATE(verlt(nb_CLxxx),stat=errare)
     IF (errare /=0 ) THEN
       call faterr(IAM,'Error allocating verlt')
     END IF

     DO icdtac=1,nb_CLxxx
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
         write(cout,'(A,I0,A)') 'Error in allocating verlt(',icdtac,')%.....'
         call faterr(IAM,cout)
       END IF
     END DO
   ELSE 
     DO icdtac=1,nb_CLxxx
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
   DO icdtac=1,nb_CLxxx
     nb_adj(icdtac)=0
   END DO
   icdan = 0
   DO
     IF( .NOT. read_G_clin()) EXIT
     IF (G_clin(2:6) /= 'icdan') CYCLE                  ! fishing for the keyword 'icdan'
     IF (G_clin(9:13)/= 'CLJCx') CYCLE     
     IF( .NOT. read_G_clin()) EXIT
     IF( .NOT. read_G_clin()) EXIT
     READ(G_clin(1:69),'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5)')   &
     cdbdy,icdbdy,cdtac,icdtac,                                          &
     behav,                                                              &
     anbdy,ianbdy,antac,iantac,                                          &
     sttus

     cdmodel = get_body_model_id_from_name( cdbdy )
     anmodel = get_body_model_id_from_name( anbdy )

     DO icdtact=1,nb_CLxxx
       if (clxxx2bdyty(1,icdtact) == icdbdy .and. &
           clxxx2bdyty(2,icdtact) == icdtac .and. &
           clxxx2bdyty(3,icdtact) == cdmodel) then
         icdan = icdan + 1

         nb_adj(icdtact)=nb_adj(icdtact)+1 

         verlt(icdtact)%icdan(nb_adj(icdtact))  = icdan

         verlt(icdtact)%cdmodel= cdmodel
         verlt(icdtact)%cdbdy  = icdbdy
         verlt(icdtact)%cdtac  = icdtac

         verlt(icdtact)%anmodel(nb_adj(icdtact))= anmodel
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%anbdy(nb_adj(icdtact))  = ianbdy
         verlt(icdtact)%antac(nb_adj(icdtact))  = iantac
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

 nb_vCLJCx=0

 DO icdtact=1,nb_CLxxx
    nb_vCLJCx = nb_vCLJCx + nb_adj(icdtact)

     IF ( nb_adj(icdtact) /= verlt(icdtact)%adjsz ) THEN 
       write(cout,'(A,1x,I0)')      'Very strange for the contactor',icdtact
       write(cout,'(A,1x,I0,1x,A)') 'value of nb_adj is',nb_adj(icdtact),'and'
       write(cout,'(A,1x,I0)')      'value of verlet%adjsz is',verlt(icdtact)%adjsz
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
   INTEGER :: nb_CLxxx, nb_JONCx,iadj,icdan,icdbdy,icdtac,ianbdy,iantac,isee,nfich,icdtact
   INTEGER :: lc
   character(len=5) :: cdmodel, anmodel

   character(len=20):: fmt
   
   nb_JONCx=get_nb_JONCx()
   nb_CLxxx=get_nb_CLxxx()


   DO icdtact=1,nb_CLxxx    
     DO iadj=1,nb_adj(icdtact)    
       icdan  = adjac(icdtact)%icdan(iadj)
       icdtac = this(icdan)%icdtac
       iantac = this(icdan)%iantac

       cdmodel = get_body_model_name_from_id( clxxx2bdyty(3,icdtac) )
       anmodel = get_body_model_name_from_id( joncx2bdyty(3,iantac) )

       WRITE(nfich,'(A6,2X,A5,2X,I7)')'$icdan','CLJCx',icdan
                            !1234567890123456789012345678901234567890123456789012345678901234567890123456
       WRITE(nfich,'(A76)') ' cdbdy  numbr  cdtac  numbr  behav  anbdy  numbr  antac  numbr  sttus   iadj'
       WRITE(nfich,'(1X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,A5,2X,I5,2X,A5,2X,I5,2X,A5,2X,I5)')   &
       cdmodel,clxxx2bdyty(1,icdtac),'CLxxx',clxxx2bdyty(2,icdtac),  &
       see(this(icdan)%isee)%behav,  &
       anmodel,joncx2bdyty(1,iantac),'JONCx',joncx2bdyty(2,iantac),  &
       get_contact_status_name_from_id(this(icdan)%status), iantac
       WRITE(nfich,104)'rlt/H',this(icdan)%rlt/H,'rln/H',this(icdan)%rln/H,'rls/H',0.D0
       WRITE(nfich,104)'vlt =',this(icdan)%vlt  ,'vln =',this(icdan)%vln  ,'vls =',0.D0
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
!------------------------------------------------------------------------   
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_reac_CLJCx(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER :: icdtac,iantac
   INTEGER :: storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_reac_CLxxx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_reac_JONCx(iantac,storage)
    
 END SUBROUTINE nullify_reac_CLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE nullify_vlocy_CLJCx(icdan,storage)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER :: icdtac,iantac
   INTEGER :: storage   
    
   icdtac=this(icdan)%icdtac
   CALL nullify_vlocy_CLxxx(icdtac,storage)
   
   iantac=this(icdan)%iantac
   CALL nullify_vlocy_JONCx(iantac,storage)
    
 END SUBROUTINE nullify_vlocy_CLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE vitrad_CLJCx(icdan,storage,need_full_vlocy)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER            :: icdtac,iantac
   INTEGER            :: storage   
   logical, optional  :: need_full_vlocy
    
   icdtac=this(icdan)%icdtac
   CALL comp_vlocy_CLxxx(icdtac,storage,need_full_vlocy)
    
   iantac=this(icdan)%iantac
   CALL comp_vlocy_JONCx(iantac,storage)
    
 END SUBROUTINE vitrad_CLJCx
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE injj_CLJCx(icdan,RTIK,RNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in) :: icdan
   REAL(kind=8),INTENT(in) :: RTIK,RNIK
   INTEGER,     DIMENSION(3)  :: anccdof
   REAL(kind=8),DIMENSION(2)  :: cdreac
   REAL(kind=8),DIMENSION(3)  :: anreac
   INTEGER :: icdtac
   INTEGER :: iantac
   INTEGER :: storage   
   
   cdreac(1)=RTIK*this(icdan)%tuc(1)+RNIK*this(icdan)%nuc(1)

   anccdof(1)=1
   anreac(1)=-cdreac(1)

   cdreac(2)=RTIK*this(icdan)%tuc(2)+RNIK*this(icdan)%nuc(2)      

   anccdof(2)=2
   anreac(2)=-cdreac(2)

   anccdof(3)=3
   anreac(3)=-this(icdan)%Gant3*RTIK-this(icdan)%Gann3*RNIK

   icdtac=this(icdan)%icdtac
   CALL add_reac_CLxxx(icdtac,cdreac(1:2),storage)

   iantac=this(icdan)%iantac
   CALL add_reac_JONCx(iantac,anccdof,anreac,storage)

 END SUBROUTINE injj_CLJCx 
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------  
 SUBROUTINE prjj_CLJCx(icdan,VTIK,VNIK,storage)
 
   IMPLICIT NONE
   INTEGER     ,INTENT(in)   :: icdan
   REAL(kind=8),INTENT(out)  :: VTIK,VNIK
   REAL(kind=8),DIMENSION(2) :: Vcd
   REAL(kind=8),DIMENSION(3) :: Van
   integer(kind=4) :: icdtac,iantac
   integer(kind=4), intent(in) :: storage
   
   icdtac=this(icdan)%icdtac

   iantac=this(icdan)%iantac

   SELECT CASE(storage) 
   CASE (iV____)
     CALL get_vlocy_CLxxx(icdtac,iV____,Vcd)
     CALL get_vlocy_JONCx(iantac,iV____,Van)
   CASE (iVfree)
     CALL get_vlocy_CLxxx(icdtac,iVfree,Vcd)
     CALL get_vlocy_JONCx(iantac,iVfree,Van)
   CASE (iVaux_)
     CALL get_vlocy_CLxxx(icdtac,iVaux_,Vcd)
     CALL get_vlocy_JONCx(iantac,iVaux_,Van)
   END SELECT

   VTIK= Vcd(1)*this(icdan)%tuc(1)+Vcd(2)*this(icdan)%tuc(2) &
        -Van(1)*this(icdan)%tuc(1)-Van(2)*this(icdan)%tuc(2)-Van(3)*this(icdan)%Gant3 
   VNIK= Vcd(1)*this(icdan)%nuc(1)+Vcd(2)*this(icdan)%nuc(2) &
        -Van(1)*this(icdan)%nuc(1)-Van(2)*this(icdan)%nuc(2)-Van(3)*this(icdan)%Gann3

 END SUBROUTINE prjj_CLJCx 
!------------------------------------------------------------------------ 
 integer function get_nb_CLJCx(itactor)
   implicit none
   integer, intent(in) :: itactor

   select CASE(itactor)
   case(i_real_tactor)
      get_nb_CLJCx = nb_CLJCx
   case(i_verlet_tactor)
      get_nb_CLJCx = nb_vCLJCx
   case(i_rough_tactor)
      get_nb_CLJCx = nb_rough_CLJCx
   case(i_recup_tactor)
      get_nb_CLJCx = nb_recup_CLJCx
   end select

 end function get_nb_CLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 LOGICAL FUNCTION test_roughly_distance(COO1L,COO2L,X1,X2,X3,AX1,AX2,adist)

   IMPLICIT NONE
 
   REAL(kind=8) :: COO1L,COO2L,X1,X2,X3,AX1,AX2,adist
   REAL(kind=8) :: CG,SG,D1,D2,COO1,COO2,Dup,Dright

   LOGICAL :: on_garde
    
   on_garde = .FALSE.
    
   CG=DCOS(X3)
   SG=DSIN(X3)

   D1=COO1L-X1
   D2=COO2L-X2
   COO1= D1*CG+D2*SG
   COO2=-D1*SG+D2*CG
   D1=COO1
   D2=COO2
   Dup   =AX2+adist
   Dright=AX1+Dup
   IF (D1 >= -Dright .AND. D1 <= Dright .AND. D2 >= -Dup .AND. D2 <= Dup) on_garde = .TRUE.
    
   test_roughly_distance=on_garde
   
 END FUNCTION test_roughly_distance
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 SUBROUTINE local_framing(COO1L,COO2L,X1,X2,X3,AX1,AX2,icdtac,iantac, &
                          UT1,UT2,UN1,UN2,gap,Gant3,Gann3,XP2,YP2)

!
!   mjean, 1986,1989, 05.01.91, 28.05.91, 28.02.93, 02.05.01
! 

    IMPLICIT NONE
 
    REAL(kind=8),INTENT(in) :: COO1L,COO2L,X1,X2,X3,AX1,AX2
    INTEGER,INTENT(in) :: icdtac,iantac
    REAL(kind=8) :: CG,SG,D1,D2,COO1,COO2,nonuc
    REAL(kind=8) :: BRAS1,BRAS2
    REAL(kind=8) :: X,UxT1,UxT2,UxN1,UxN2
    REAL(kind=8),INTENT(out) :: UT1,UT2,UN1,UN2,gap,Gant3,Gann3
    
    REAL(kind=8) :: XP2,YP2

    character(len=80) :: cout
                               !123456789012345678901234
    character(len=24) :: IAM = 'mod_CLJCx::local_framing'

    CG=DCOS(X3)
    SG=DSIN(X3)

    D1=COO1L-X1
    D2=COO2L-X2

    COO1= D1*CG+D2*SG
    COO2=-D1*SG+D2*CG

    D1=COO1
    D2=COO2

  
    !fd BRAS1,BRAS2 les coordonnees du bras de levier ...

    !fd Gant3=-BRAS2*UxT1+BRAS1*UxT2
    !fd Gann3=-BRAS2*UxN1+BRAS1*UxN2

    !fd pour Gant3 et Gann3 on se fiche du repertoire dans
    !fd lequel on travaille (local ou global) 


    ! checking upper plane face
    IF (D1 >= -AX1 .AND. D1 <= AX1 .AND. D2 >= 0) THEN
      UxT1= 1.D00
      UxT2= 0.D00
      UxN1= 0.D00
      UxN2= 1.D00

      gap=D2-AX2

      BRAS1=D1
      BRAS2=AX2

      Gant3=-AX2
      Gann3= D1
 
    ! checking lower plane face
    ELSE IF (D1 >= -AX1 .AND. D1 <=AX1 .AND. D2 < 0) THEN
      UxT1=-1.D00
      UxT2= 0.D00
      UxN1= 0.D00
      UxN2=-1.D00

      gap=-D2-AX2

      BRAS1= D1
      BRAS2=-AX2

      Gant3=-AX2

!fd problème de signe !?
!fd       Gann3= D1
      Gann3= -D1

    ! checking right half disk
    ELSE IF (D1 > AX1) THEN
      D1=D1-AX1
      nonuc=dsqrt(D1*D1+D2*D2)
      IF ( nonuc <= 0.) THEN
        write(cout,'(A,1x,I0,A,1x,I0)') 'clxxx',icdtac,' centre sur jonc',iantac
        call faterr(IAM,cout)
      ENDIF

      UxT1= D2/nonuc
      UxT2=-D1/nonuc
      UxN1= D1/nonuc
      UxN2= D2/nonuc

      gap=nonuc-AX2

!fd ca doit etre UxN1 au lieu de UxT1 !?
!fd      BRAS1= AX1+AX2*UxT1

      BRAS1= AX1+AX2*UxN1
      BRAS2=     AX2*UxN2

      Gant3=-BRAS2*UxT1+BRAS1*UxT2
      Gann3=-BRAS2*UxN1+BRAS1*UxN2


    ! checking left half disk
    ELSE IF (D1 < -AX1) THEN
      D1=D1+AX1
      nonuc=dsqrt(D1*D1+D2*D2)
      IF ( nonuc <= 0.) THEN
        write(cout,'(A,1x,I0,A,1x,I0)') 'error : clxxx',icdtac,' centre sur jonc',iantac
        call faterr(IAM,cout)
      ENDIF

      UxT1= D2/nonuc
      UxT2=-D1/nonuc
      UxN1= D1/nonuc
      UxN2= D2/nonuc

      gap=nonuc-AX2

!fd ca doit etre UxN1 au lieu de UxT1 !?
!fd      BRAS1= AX1+AX2*UxT1

      BRAS1=-AX1+AX2*UxN1
      BRAS2=     AX2*UxN2

      Gant3=-BRAS2*UxT1+BRAS1*UxT2
      Gann3=-BRAS2*UxN1+BRAS1*UxN2

    ELSE
      call faterr(IAM,'not possible')
    END IF
  
    UT1=UxT1*CG-UxT2*SG
    UT2=UxT1*SG+UxT2*CG

    UN1=UxN1*CG-UxN2*SG
    UN2=UxN1*SG+UxN2*CG

    XP2=X1+(BRAS1*CG-BRAS2*SG)
    YP2=X2+(BRAS1*SG+BRAS2*CG)

 END SUBROUTINE local_framing
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------ 
 REAL(kind=8) FUNCTION get_length_CLJCx(icdan)

   IMPLICIT NONE
   INTEGER,INTENT(in) :: icdan
   INTEGER :: icdtac
    
   icdtac=this(icdan)%icdtac
   get_length_CLJCx=get_length_CLxxx(icdtac)
   
 END FUNCTION get_length_CLJCx
!------------------------------------------------------------------------ 
!------------------------------------------------------------------------
SUBROUTINE print_info_CLJCx(icdan)
   IMPLICIT NONE
   INTEGER          :: icdan,icdtac,iantac,icdbdy,ianbdy

   CHARACTER(len=80) :: cout

   icdtac=this(icdan)%icdtac
   iantac=this(icdan)%iantac

   WRITE(cout,1) icdtac,iantac
   CALL LOGMES(cout)

1  FORMAT(1X,'CLxxx:',1x,I5,1x,'JONCx:',1x,I5)

   icdbdy=this(icdan)%icdbdy
   ianbdy=this(icdan)%ianbdy

!   call print_info_CLxxx(icdbdy)
   CALL print_info_JONCx(ianbdy)

END SUBROUTINE print_info_CLJCx
!------------------------------------------------------------------------
LOGICAL FUNCTION RUN_CLJCx(fantome)

  IMPLICIT NONE
  INTEGER,optional :: fantome

  RUN_CLJCx = RUN_TACTOR

END FUNCTION RUN_CLJCx
!------------------------------------------------------------------------
  logical function CHECK_CLJCx()
    implicit none
    !   
    integer :: isee, nb_CLxxx, nb_JONCx
  
    ! if check already made just return result
    if( module_checked_ ) then
      CHECK_CLJCx = check_CLJCx_
      return
    end if

    con_pedigree%module_name = 'CLJCx'

    con_pedigree%id_cdan  = i_cljcx
    con_pedigree%id_cdtac = i_clxxx
    con_pedigree%id_antac = i_joncx

    cdtact2bdyty => clxxx2bdyty
    antact2bdyty => joncx2bdyty

    ! check only once if module may be used
    module_checked_ = .TRUE.
  
    ! checking if enough cd/an
    nb_JONCx = get_nb_JONCx()
    nb_CLxxx = get_nb_CLxxx()
    if( nb_JONCx == 0 .or. nb_CLxxx == 0 ) then
      CHECK_CLJCx = check_CLJCx_ ! still false
      return
    end if
    
    ! checking if any seetable with the good cd/an type
    do isee = 1, size(see)
      if (see(isee)%cdtac == 'CLxxx' .and. see(isee)%antac == 'JONCx') then
        check_CLJCx_ = .true.
        exit
      end if
    end do
  
    CHECK_CLJCx = check_CLJCx_
    return
  
  end function CHECK_CLJCx
!!!------------------------------------------------------------------------ 
  LOGICAL FUNCTION get_write_Vloc_Rloc_CLJCx(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_Vloc_Rloc_CLJCx = write_Vloc_Rloc

  END FUNCTION get_write_Vloc_Rloc_CLJCx
!------------------------------------------------------------------------

 function get_icdtac_CLJCx(icdan)
   implicit none
   integer(kind=4), intent(in)  :: icdan
   integer(kind=4) :: get_icdtac_CLJCx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_CLxxx
   logical :: found

   found = .false.

   nb_CLxxx=get_nb_CLxxx()

   icc = 0
   do icdtac = 1, nb_CLxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_icdtac_CLJCx = icdtac
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('CLJCx::get_icdtac','unknown contact index')
   
 end function

 function get_iantac_CLJCx(icdan)
   implicit none
   integer, intent(in)  :: icdan
   integer :: get_iantac_CLJCx
   !
   integer(kind=4) :: icc, icdtac, iadj, nb_CLxxx
   logical :: found

   found = .false.

   nb_CLxxx=get_nb_CLxxx()

   icc = 0
   do icdtac = 1, nb_CLxxx
     if (verlt(icdtac)%adjsz == 0) cycle

     do iadj = 1, verlt(icdtac)%adjsz
       icc = icc + 1
       if ( icc == icdan ) then
         found = .true.
         get_iantac_CLJCx =  verlt(icdtac)%antac(iadj)
         exit
       end if
     end do
     if ( found ) exit
   end do

   if( .not. found ) call faterr('CLJCx::get_icdtac','unknown contact index')
   

   get_iantac_CLJCx = this(icdan)%iantac

 end function
 !------------------------------------------------------------------------
 subroutine clean_memory_CLJCx
   implicit none
   integer(kind=4) :: i

   call clean_memory_inter_meca_()

   nb_CLJCx  = 0
   nb_vCLJCx = 0

   if( allocated(rough_CLJCx) ) deallocate(rough_CLJCx)

   nb_rough_CLJCx = 0
   nstep_rough_seek_CLJCx = 1
   nb_recup_CLJCx = 0

   RUN = .false.

   if( allocated(CLcoor) ) deallocate(CLcoor)
   if( allocated(JCcoor) ) deallocate(JCcoor)

   Reac_CLJCx_MAX = 0.D0

   module_checked_ = .FALSE.
   check_CLJCx_    = .FALSE.

 end subroutine

 subroutine set_nb_CLJCx(nb)
   implicit none
   integer(kind=4), intent(in) :: nb

   if( allocated(this) ) then
     deallocate(this)
   end if

   allocate( this(nb) )

   nb_CLJCx = nb

 end subroutine

 subroutine redo_nb_adj_CLJCx()
   implicit none

   call redo_nb_adj_( get_nb_CLxxx() )

 end subroutine

END MODULE CLJCx

