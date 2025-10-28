!==========================================================================

! Copyright 2000-2022 CNRS-UM.
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
MODULE nlgs_newarch

! shared modules

  USE overall

  USE parameters

  USE tact_behaviour
  USE utilities

  use algebra, only : determinant33

!  .. statements for LAPACK95
  USE LA_PRECISION, ONLY: WP => DP
  USE F95_LAPACK, ONLY: LA_GESV

! contrib modules

  use soe, only : set_system_rhs, &
                  solve         , &
                  reset_rhs     , &
                  reset_sol     , &
                  add_to_rhs    , &
                  get_sol

  use interaction, only : T_interaction
  use interaction_handler, only : get_nb_interactions, &
                                  get_interaction

  IMPLICIT NONE
  
  PRIVATE

  REAL(kind=8),DIMENSION(:),ALLOCATABLE         :: Wab
  INTEGER, PRIVATE                              :: nb_CDAN,nb_ENTITY
 
  CHARACTER(len=30)  :: Wstorage          ! variable indicating if the W Delassus matrix is built or not

  LOGICAL            :: SDLactif=.FALSE.

  INTEGER,      DIMENSION(:), ALLOCATABLE :: ialeat,ialeatr,iwksg,randomlist
  REAL(kind=8), DIMENSION(:), ALLOCATABLE :: Xvlton,WRRarray

  INTEGER            :: Nnoact,NOKsta,Nactif,Nstick,Nslide
  INTEGER            :: Nvnish,Nhover,Nnoctc,Ntract,Ncompr,Nnknow,Nb_RGR
  INTEGER            :: NOKweak,NOKstrg,SNstick,SNslide,WNstick,WNslide

  !mj randomlist

  INTEGER      :: IAL1
  REAL(kind=8) :: RA

  REAL(kind=8) :: somme_rn
  REAL(kind=8) :: HH
  

  REAL(kind=8) :: DVDV,DVDVRR,DVoR,SumDVDV,MaxDVDV,SumDVDVRR,MaxDVDVRR,SumDVoR,SumWRWR,WRR,SumWRR,dynstat


  REAL(kind=8) :: QuadDV,MaxmDV,QuadDVR,MaxmDVR,QuadWR,MeanWRR,MeanDVoR

  REAL(kind=8) :: rcvltm,Dreac,Dcrac,Scale=1.D0,Enrg=0.D0
  
  REAL(kind=8)      :: tol=0.1666D-03,RELAX=1.D0,RELAX1=0.D0,inv_tol=0.6002401D+04

  INTEGER           :: i_checktype=1                           ! variable indicating the type of iter check test
  CHARACTER(len=5)  :: checktype                               ! variable indicating the type of iter check test
  
!!! parameter table -------------------------------------------------------------

!!! nlgs check keyword

  INTEGER,PARAMETER :: i_Quad = 1 , i_Maxm = 2 , i_QMs16 = 3 , i_QuadN = 4

!!! nlgs keyword

  INTEGER,PARAMETER :: i_iter = 1 , i_check = 2 , i_post = 3

! RGR CRITIC

  REAL(kind=8),PARAMETER :: Oneover4pi2=1.D0/(4.D0*PI_g*PI_g), Oneover2pi=1.D0/(2.D0*PI_g)

  INTEGER      :: nlgs_loop,norm_fich
  LOGICAL      :: norm_check=.FALSE.,diagonal_resolution=.FALSE.

! pour un calcul de l'evolution de la reaction
  REAL(KIND=8), dimension(3) :: sum_,dsum_

  PUBLIC &
       active_diagonal_resolution, &
       bimodal_list_nlgs, &
       comp_check_nlgs, &
       display_check_nlgs, &
       quick_scramble_nlgs, &
       reverse_nlgs, &
       scale_rloc_nlgs, &
       scramble_nlgs, &
       solve_nlgs, &
       write_norm_check_nlgs, &
       RnodHRloc_nlgs, &
       compute_local_free_vlocy, &
       display_rlocn_sum_nlgs, &
       update_tact_behav_nlgs, &
       set_nlgs_parameter, &
       prep_nlgs, &
       prep_check_nlgs, &
       Nullify_EntityList_nlgs, &
       !init_cohe_nlgs, &
       get_error, &
       get_conv, &
       compute_convergence_norms_nlgs, & !am: functions for check convergence in DDM
       check_convergence_nlgs

  PUBLIC &
       !post traitement
       get_nlgs_loop, get_nlgs_network_change,get_nlgs_contact_status,&
       get_after_iter_check, get_somme_rn, &
       get_all_this

CONTAINS
!!!------------------------------------------------------------------------
!!! SUBROUTINE prep_nlgs : preparing input data
!!!
!!! extracting informations from the contribs in order to use 
!!! a shared solver
!!!
!!!------------------------------------------------------------------------
 SUBROUTINE prep_nlgs(actif)

   IMPLICIT NONE

   ! common part
                            !123456789012345
   CHARACTER(len=15) :: IAM='nlgs::prep_nlgs'
   CHARACTER(len=80) :: cout
   INTEGER           :: errare=0
   
   INTEGER           :: ik,ibehav,icdan,i
   
   INTEGER           :: ient !sdl

   REAL(kind=8)      :: forward,backward,nbc,TshVlt, det
   REAL(kind=8), dimension(nbDIME) :: vik,rik

   type(T_interaction), pointer :: this, this_jl

   ! behaviours part

   REAL(kind=8)      :: fric,                         & ! friction coefficient
                        tangalrest,normalrest,        & ! restitution coefficients
                        normalcoh,tangalcoh,Wethk,    &
                        forcePERgap,forcePERstrain,   &
                        forcePERstrainrate,prestrain, &
   ! RGR CRITIC
                        ToverH,OTH,vOVERcv, &
                        gap_tol,meff,reff,etan,etat,vloc, &
                        DW, SW

   LOGICAL           :: pret=.FALSE.,inter,is_present=.FALSE.,ok,actif
   
   INTEGER           :: jl,iadj,jadj,nbadj,jll,icdik,ianik,bandwith,icdent,ianent,istart,n2
   
   INTEGER           :: ikadj,ikstart,jladj,jlstart
   
   REAL(kind=8)      :: cd_length,thickness ! mac_czm
   LOGICAL           :: is_initialized=.FALSE.
   
   ! bimodal list
   REAL(kind=8)      :: reac_mean = 0.D0
   INTEGER           :: nb_WEAK,nb_STRONG
   
   !local periodic computation
   REAL(kind=8)              :: Y_x,Y_y,EY_x,EY_y
   REAL(kind=8),DIMENSION(3) :: E

   !mj randomlist

   INTEGER      :: IAL1
   REAL(kind=8) :: RA
   
   REAL(kind=8) :: un
   REAL(kind=8) :: pre_gap

   SDLactif    = actif
   !newarch_solver = .TRUE.
   nlgs_loop   = 0

   nb_CDAN = get_nb_interactions()

   IF (nb_CDAN == 0) RETURN 
  
   IF( ALLOCATED(ialeat) .and. size(ialeat)<nb_CDAN ) then
     DEALLOCATE(ialeat)
   end if
   if( .not. allocated(ialeat) ) then
     ALLOCATE(ialeat(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating ialeat')
     END IF
   end if
   
   IF( ALLOCATED(ialeatr) .and. size(ialeatr)<nb_CDAN ) then
     DEALLOCATE(ialeatr)
   end if
   if( .not. allocated(ialeatr) ) then
     ALLOCATE(ialeatr(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating ialeatr')
     END IF
   end if
   
   IF( ALLOCATED(iwksg) .and. size(iwksg)<nb_CDAN ) then
     DEALLOCATE(iwksg) 
   end if
   if( .not. allocated(iwksg) ) then
     ALLOCATE(iwksg(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating ialeat')
     END IF
   end if
   
!!!mj randomlist
   
   IF( ALLOCATED(randomlist) .and. size(randomlist)<nb_CDAN ) then
     DEALLOCATE(randomlist)
   end if
   if( .not. allocated(randomlist) ) then
     ALLOCATE(randomlist(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating randomlist')
     END IF
   end if
   
   IF( ALLOCATED(Xvlton) .and. size(Xvlton)<nb_CDAN ) then
     DEALLOCATE(Xvlton)
   end if
   if( .not. allocated(Xvlton) ) then
     ALLOCATE(Xvlton(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating Xvlton')
     END IF
   end if
   Xvlton(1:nb_CDAN) = 0.D0
   
   IF( ALLOCATED(WRRarray) .and. size(WRRarray)<nb_CDAN ) then
     DEALLOCATE(WRRarray)
   end if
   if( .not. allocated(WRRarray) ) then
     ALLOCATE(WRRarray(nb_CDAN),stat=errare)
     IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating WRRarray')
     end if
   end if
   WRRarray(1:nb_CDAN) = 1.D0

   !rm : done when filling adjjl
   !do ik = 1, nb_cdan
   !  this => get_interaction(ik)
   !  if( associated(thise%adjjl) ) deallocate(this%adjjl)
   !end do

   do ik = 1, nb_cdan
      ialeat(ik)  = ik
      ialeatr(ik) = ik

      call random_number(ra)
      ial1 = idint(ra*real(nb_cdan,8))+1
      ial1 = min0(ial1,nb_cdan)
      ial1 = max0(1,ial1)
      randomlist(ik)=ial1
   end do

   nb_ENTITY = get_nb_ENTITY()

   call Create_EntityList   

!!!!
 
   reac_mean = 0.D0
  
   do icdan = 1, nb_cdan
     this => get_interaction(icdan)
     this%icdan = icdan
     reac_mean = reac_mean + this%rl(2)

     icdent = this%icdent
     ianent = this%ianent

     entity(icdent)%ik = entity(icdent)%ik+1
     entity(icdent)%list(entity(icdent)%ik) = icdan
     entity(ianent)%ik = entity(ianent)%ik+1
     entity(ianent)%list(entity(ianent)%ik) = icdan

     call nullify_reac(this,iReac_)
     call nullify_vlocy(this,iVaux_)
   end do

   if( nb_CDAN /= 0 ) reac_mean = reac_mean/REAL(nb_CDAN,8)
  
   !!!!!!!!!!!!!!!!!
   ! Rnod = [H] Rloc
   !fd  + all the Vaux = 0. for sdl
   !!!!!!!!!!!!!!!!!

   nb_STRONG = 0
   nb_WEAK   = 0
   
   do ik = 1, nb_CDAN
     this => get_interaction(ik)
     call injj(this,this%rl(1:nbDIME),iReac_)
      if( this%rl(2) < reac_mean ) then
        nb_WEAK = nb_WEAK+1
        iwksg(nb_CDAN+1-nb_WEAK) = ik
      else
        this%ws = 1
        nb_STRONG = nb_STRONG+1
        iwksg(nb_STRONG) = ik
      end if
   end do
   
!!!*************************************************************************************
!!! computing local dynamic matrix W (Delassus matrix)
!!!
   if( SDLactif) then
      ! In this case the W matrix is going to be built and stored
      bandwith=0
      istart  = 1  
      do ik = 1, nb_CDAN
        this => get_interaction(ik)
        nbadj = 0

        icdik = this%icdent
        ianik = this%ianent

        IF (icdik == ianik) THEN
          !fd & mr autocontact (avec la langue)       
          nbadj = entity(icdik)%nb-1
        ELSE
          nbadj = entity(icdik)%nb+entity(ianik)%nb-2
        ENDIF

        jl = 0
        
        IF (nbadj /= 0) THEN
           
           IF ( ALLOCATED(this%adjjl) .AND. nbadj > SIZE(this%adjjl)) DEALLOCATE(this%adjjl)
           
           IF (.NOT. ALLOCATED(this%adjjl)) ALLOCATE(this%adjjl(nbadj),stat=errare)
           IF (errare /= 0) THEN
              CALL FATERR(IAM,'error allocating this(ik)%adjjl')
           END IF
           this%adjjl = 0
           
           DO iadj = 1, entity(icdik)%nb
             IF(entity(icdik)%list(iadj) == ik) CYCLE
             jl = jl+1
             this%adjjl(jl) = entity(icdik)%list(iadj)
           END DO
           
           DO iadj=1,entity(ianik)%nb
             IF(entity(ianik)%list(iadj) == ik) CYCLE
             
             !!!fd evacuation des contacts partages
             is_present = .FALSE.
             !!! precedemment on a range entity(icdik)%nb-1 contacts on les reparcours 
             !!! a la recherche d'un doublon avec celui qu'on veut poser
             DO jadj=1,entity(icdik)%nb-1
               IF (this%adjjl(jadj) /= entity(ianik)%list(iadj)) CYCLE
               is_present = .TRUE.
               EXIT
             END DO
             IF (is_present) CYCLE
             jl = jl+1
             this%adjjl(jl) = entity(ianik)%list(iadj)
           END DO
        END IF
        
        this%istart = istart
        istart = nbDIME*nbDIME*jl + istart
        
        this%nbadj = jl
        bandwith = bandwith+jl

      END DO
      
      !!!fd paranoiac test
      DO ient=1,nb_ENTITY
        IF (entity(ient)%ik /= entity(ient)%nb) THEN
          CALL LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
          WRITE(cout,'(A7,I5,A4,I5,A4,I5)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
          CALL FATERR(IAM,cout)
        END IF
      END DO
      
      IF (ALLOCATED(Wab)) DEALLOCATE(Wab)
      ALLOCATE(Wab(nbDIME*nbDIME*bandwith),stat=errare)
      Wab=0.d0
      IF (errare /= 0) THEN
        CALL FATERR(IAM,'error allocating Wab')
      END IF
      
   END IF
!!!
!!! end of computing local dynamic matrix W (Delassus matrix)
!!!*************************************************************************************

   n2 = nbDIME*nbDIME

   DO ik = 1, nb_CDAN

     ! --------------------------------------------
     ! Computing the bloc diagonal part of matrix W
     ! --------------------------------------------
     this => get_interaction(ik)


     rik(1)        = 1.d0
     rik(2:nbDIME) = 0.d0

     call nullify_reac(this,iRaux_)
     CALL injj(this,rik(1:nbDIME),iRaux_)
     CALL vitrad(this,iVaux_e_invM_t_Raux_)
     CALL prjj(this,vik(1:nbDIME),iVaux_)

     this%W(1:nbDIME,1) = vik(1:nbDIME)
      
     IF (SDLactif) THEN
       DO ikadj = 1, this%nbadj
            
         jl = this%adjjl(ikadj)
         if (jl == 0) then
           icdik = this%icdent
           ianik = this%ianent
           write(*,'(A,1x,I0,1x,A,1x,I0,1x,A,1x,I0)') 'contact :',ik,' icdent ',icdik,' ianent ',ianik
           print*,entity(icdik)%nb
           print*,entity(icdik)%list
           print*,entity(ianik)%nb
           print*,entity(ianik)%list
           write(*,'(A,1x,I0,1x,A)') ' nbabj ',this%nbadj,'liste :'
           print*,this%adjjl(:)

           write(*,'(A,1x,I0)') 'ca chie pour adj ',ikadj
         endif

         this_jl => get_interaction(jl)
         jlstart = this_jl%istart
            
         CALL prjj(this_jl,vik(1:nbDIME),iVaux_)
            
         ok = .FALSE.
            
         ! shifting index of 1 for storing from 0 to 0+something
         do jladj = 0, this_jl%nbadj-1
           if (this_jl%adjjl(jladj+1) == ik) THEN
             Wab(jlstart + n2*jladj  ) = vik(1) !Wtt
             Wab(jlstart + n2*jladj+1) = vik(2) !Wnt
             if( nbDIME == 3 ) then
             Wab(jlstart + n2*jladj+2) = vik(3) !Wst
             endif
             ok = .TRUE.
           END IF
         END DO
            
         IF (.NOT. ok) THEN
           CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         END IF
            
       END DO
         
       call nullify_vlocy(this,iVaux_)

     END IF
     
     rik(1) = 0.d0
     rik(2) = 1.d0
     if( nbDIME == 3 ) rik(3) = 0.d0

     call nullify_reac(this,iRaux_)
     CALL injj(this,rik(1:nbDIME),iRaux_)
     CALL vitrad(this,iVaux_e_invM_t_Raux_)
     CALL prjj(this,vik(1:nbDIME),iVaux_)

     this%W(1:nbDIME,2) = vik(1:nbDIME)
      
     IF (SDLactif) THEN
       DO ikadj = 1, this%nbadj
            
         jl = this%adjjl(ikadj)

         this_jl => get_interaction(jl)
         jlstart = this_jl%istart
            
         CALL prjj(this_jl,vik(1:nbDIME),iVaux_)

         ok = .FALSE.

         do jladj = 0, this_jl%nbadj-1
           IF (this_jl%adjjl(jladj+1) == ik) THEN
             Wab(jlstart + n2*jladj+nbDIME  ) = vik(1) !Wtn
             Wab(jlstart + n2*jladj+nbDIME+1) = vik(2) !Wnn
             if( nbDIME == 3 ) then
             Wab(jlstart + n2*jladj+nbDIME+2) = vik(3) !Wsn
             endif
             ok = .TRUE.
           END IF
         end do
            
         IF (.NOT. ok) THEN
           CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         END IF
            
       END DO
         
       call nullify_vlocy(this,iVaux_)

     END IF
     
     if( nbDIME == 3 ) then
       rik(1:2) = 0.d0
       rik(3)   = 1.d0

       call nullify_reac(this,iRaux_)
       CALL injj(this,rik(1:nbDIME),iRaux_)
       CALL vitrad(this,iVaux_e_invM_t_Raux_)
       CALL prjj(this,vik(1:nbDIME),iVaux_)

       this%W(1:nbDIME,3) = vik(1:nbDIME)
        
       IF (SDLactif) THEN
         DO ikadj = 1, this%nbadj
              
           jl = this%adjjl(ikadj)

           this_jl => get_interaction(jl)
           jlstart = this_jl%istart
              
           CALL prjj(this_jl,vik(1:nbDIME),iVaux_)
              
           ok = .FALSE.
 
           do jladj = 0, this_jl%nbadj-1
             IF (this_jl%adjjl(jladj+1) == ik) THEN
               Wab(jlstart + n2*jladj+6) = vik(1) !Wts
               Wab(jlstart + n2*jladj+7) = vik(2) !Wns
               if( nbDIME == 3 ) then
               Wab(jlstart + n2*jladj+8) = vik(3) !Wss
               endif
               ok = .TRUE.
             END IF
           end do
              
           IF (.NOT. ok) THEN
             CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
           END IF
              
         END DO
           
         call nullify_vlocy(this,iVaux_)

       END IF
     end if


     ibehav = this%lawnb

!!! --------------------------------------
!!! Warning and coping with critical cases
!!! --------------------------------------

      IF (this%W(2,2) .LE. 1.D-18) THEN
         WRITE(cout,543) ik,this%W(2,2)
543      FORMAT(1X,'  Wnn(',I5,') =',D12.5,' < 1.D-18')
         CALL LOGMES('Error '//IAM//': '//cout)
         WRITE(cout,'(I8,A2,I7,A1,I7,A1,D12.5)') this%CDAN,': ',this%icdent,',',this%ianent,' ',this%gapTTbegin
         CALL LOGMES('Error '//IAM//': '//cout)
      END IF

      IF (this%W(1,1) .LE. 1.D-06*this%W(2,2) .AND. &
!!!                                     123456789012345678901234567890
           tact_behav(ibehav)%lawty /= 'ELASTIC_ROD                   ' .AND. &
           tact_behav(ibehav)%lawty /= 'VOIGT_ROD                     ' ) THEN
         
         WRITE(cout,544)ik,this%W(1,1),ik
544      FORMAT(1X,'   Wtt(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
         CALL LOGMES(cout)
         this%W(1,1)=1.D-06*this%W(2,2)
      END IF

      if( nbDIME == 3) then
        IF (this%W(3,3) .LE. 1.D-06*this%W(2,2).AND. &
!!!                                       123456789012345678901234567890
             tact_behav(ibehav)%lawty /= 'ELASTIC_ROD                   ' .AND. &
             tact_behav(ibehav)%lawty /= 'VOIGT_ROD                     ' ) THEN
           WRITE(cout,545)ik,this%W(3,3),ik
545        FORMAT(1X,'   Wss(',I5,') =',D12.5,' < 1.D-06 * Wnn(',I5,')')
           CALL LOGMES(cout)
           this%W(3,3)=1.D-06*this%W(2,2)
        END IF
      end if
!!!*************************************
!!! Preparing auxiliaries
!!!     
!!!! default values

      fric = get_fric(ibehav,this%statusBEGIN)
      this%fric=fric

      this%WW(1) = this%W(1,1)
      this%WW(2) = this%W(2,2)
      if( nbDIME == 3 ) this%WW(3) = this%W(3,3)

      this%covfree(1:nbDIME) = 0.D0
      this%corl(1:nbDIME)    = 0.D0      

      cd_length=0.d0

      ! customized values
      SELECT CASE(tact_behav(ibehav)%lawty)
!!!---------123456789012345678901234567890--
      CASE('IQS_CLB                       ')
         this%i_law = i_IQS_CLB
         this%covfree(2)=MAX(0.D0,this%gapTTbegin/H)         

!!!-------------------------------------
      CASE('RST_CLB                       ')
         this%i_law = i_RST_CLB
         IF (this%gapTTbegin .LE. 0.D0) THEN
            ! this(ik)%forecast='acton' (default is 'acton')
            !fd needs further checking by !mj
            CALL get_rst(ibehav,tangalrest,normalrest)
            this%covfree(1) = tangalrest*this%vlBEGIN(1)
            this%covfree(2) = normalrest*this%vlBEGIN(2)
            if( nbDIME == 3 ) this%covfree(3) = tangalrest*this%vlBEGIN(3)
         ELSE
            this%forecast= 'noact'
            this%rl(1:nbDIME) = 0.D0
            this%status       = 'noctc'
            normalrest        = 0.D0
            tangalrest        = 0.D0
            this%covfree(2)   = 0.D0
         END IF
      CASE default
         CALL LOGMES('WARNING: default case selected')
         CALL LOGMES(tact_behav(ibehav)%lawty)
      END SELECT
    
555   FORMAT(1X,'  this(',I5,')%gapREF =',D12.5,' < 1.D-18')

!!!--------------------------------------
!!! Warning non uniqueness cases
!!!--------------------------------------
       
      if( nbDIME == 2 ) then
        det      = this%WW(1)*this%WW(2)-this%W(1,2)*this%W(2,1)
        this%det = det
        
        IF (det .LT. 1.D-24 .AND. &
!!!                                       123456789012345678901234567890
             tact_behav(ibehav)%lawty /= 'ELASTIC_ROD                   ' .AND. &
             tact_behav(ibehav)%lawty /= 'VOIGT_ROD                     ' ) THEN

           WRITE(cout,565) ik,det
565        FORMAT(1X,'    WWtt*WWnn-Wtn*Wnt (',I5,') =',D12.5,' < 1.D-24')
           CALL LOGMES(cout)
        END IF

        forward = 1.D0-this%fric*this%W(2,1)/this%WW(2)
        this%forward = forward

        IF (forward .LE. 1.D-18) THEN
           
           WRITE(cout,'(2(1x,D14.7))') this%W(2,2),this%W(2,1)
           CALL LOGMES(cout)
           WRITE(cout,'(2(1x,D14.7))') this%W(1,2),this%W(1,1)
           CALL LOGMES(cout)
           WRITE(cout,'(1(1x,D14.7))') this%fric
           CALL LOGMES(cout)
           CALL print_info(ik) 
           
           WRITE(cout,521) ik,ik,ik
521        FORMAT(1X,'   1 - fric(',I5,')*Wnt(',I5,')/WWnn(',I5,') < 1.D-18')
           CALL FATERR(IAM,cout)
        END IF
        
        backward = 1.D0+this%fric*this%W(2,1)/this%WW(2)
        this%backward=backward
        
        IF (backward .LE. 1.D-18) THEN
           
           WRITE(cout,'(2(1x,D14.7))') this%W(2,2),this%W(2,1)
           CALL LOGMES(cout)
           WRITE(cout,'(2(1x,D14.7))') this%W(1,2),this%W(1,1)
           CALL LOGMES(cout) 
           WRITE(cout,'(1(1x,D14.7))') this%fric
           CALL LOGMES(cout)
          
           CALL print_info(ik) 
           
           WRITE(cout,522) ik,ik,ik
522        FORMAT(1X,'   1 + fric(',I5,')*Wnt(',I5,')/WWnn(',I5,') < 1.D-18')
           CALL FATERR(IAM,cout)
        END IF
       
      else if( nbDIME == 3 ) then
        if(.NOT.diagonal_resolution)THEN
           
          !-----------------------------------------------
          ! Warning non uniqueness cases
          !-----------------------------------------------
          det = determinant33(this%W(1:3,1:3))
          
          this%det = det
          IF (det .LT. 1.D-24) THEN
            WRITE(cout,546)ik,det
            CALL LOGMES(cout)

            print*,ik
            print*,this%W(1,1:nbDIME)
            print*,this%W(2,1:nbDIME)
            print*,this%W(3,1:nbDIME)
            endif
          END IF
          
          this%ron = 1.D0/this%W(2,2)
          
          Sw = this%W(1,1)+this%W(3,3)
          Dw = (Sw*Sw)-4.D0*(this%W(1,1)*this%W(3,3)-this%W(1,3)*this%W(3,1)) 
          
          IF (Dw>0.D0) THEN
             Dw = SQRT(Dw)
          ELSE
             Dw = 0.D0
          END IF
          
          this%rot=2.D0*(Sw-Dw)/((Sw+Dw)*(Sw+Dw))    
           
        ELSE
           
          ! Case of diagonal resolution
          this%invW(1:nbDIME) = 1.D0/this%WW(1:nbDIME)
          !-----------------------------------------------
          ! Warning non uniqueness cases
          !-----------------------------------------------
          det = this%invW(1)*this%invW(2)*this%invW(3)
          
          IF (det .LT. 1.D-24) THEN
             WRITE(cout,546) ik,det
546          FORMAT(1X,'    det(',I5,') =',D12.5,' < 1.D-25')
             CALL LOGMES(cout)
          END IF
        END IF

!!!-------------------------------------
!!! Computing free local vlocy
!!!-------------------------------------

       CALL prjj(this,this%vfree(1:nbDIME),iVfree)
       
!!!-------------------------------------
!!! Computing free local vlocy
!!!-------------------------------------

       this%statuscheck='nknow'
       this%status     ='nknow'

    END DO

    is_initialized = .TRUE.

  end subroutine prep_nlgs

  SUBROUTINE solve_nlgs(i_what)

    IMPLICIT NONE
    !                         1234567890123456
    CHARACTER(len=16) :: IAM='nlgs::solve_nlgs'
    CHARACTER(len=80) :: cout
    INTEGER           :: i_what

    INTEGER          :: ik,ikk,ibehav,iadj,ikjl,ibdy,istart,iistart,ilaw
    CHARACTER(len=5) :: sstatusik
    REAL(kind=8)     :: DET,forward,backward,DFT,DFN,FFN,Cforward,Cbackward
    REAL(kind=8)     :: fricik, modrl

    REAL(kind=8), dimension(nbDIME,nbDIME) :: WWik
    REAL(kind=8), dimension(nbDIME)        :: vlocfreeik, Wrlik, Wrliki, vik, rik, vlik, rlik
    REAL(kind=8), dimension(nbDIME)        :: vvlocfreeik, vvlik, rrlik, vliki, rliki, rloc, vl, Dvl

    REAL(kind=8)     :: normalcoh,tangalcoh,Wethk
    REAL(kind=8)     :: WRRmin,WRRmax,RWR,bR,alphaik,fnik
    
!fd czm 
   REAL(kind=8)     :: Hradh_t,Hradh_n
   LOGICAL          :: is_cohesive
   REAL(kind=8)     :: k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,Ttt,Ttn,Tnt,Tnn
!fd nosldt
   REAL(kind=8)     :: bt
!fd
   REAL(kind=8)     :: snmax

   type(T_interaction), pointer :: this, this_ikjl

   IF (i_what == i_iter) nlgs_loop = nlgs_loop + 1

   RWR    = 0.D0
   bR     = 0.D0
   Enrg   = 0.D0
   WRRmin = 1.D24
   WRRmax = 0.D0

   sum_(1:nbDIME)  = 0.d0
   dsum_(1:nbDIME) = 0.d0

   IF (nb_CDAN == 0) RETURN

   !$OMP PARALLEL DEFAULT(SHARED)                                                            &
   !$OMP PRIVATE(ik,ikk,istart,ikjl,iadj,iistart,                                            &
   !$OMP         rik,rliki,vlik,vlocfreeik,sstatusik,                                        &
   !$OMP         rlik,Wrlik,WWik,                                                            &
   !$OMP         vvlocfreeik,fricik,ibehav,ilaw,rrlik,vvlik,                                 &
   !$OMP         Wrliki,vliki,vik,                                                           &
   !$OMP         rloc,vl,Dvl,DVDV,DVDVRR,DVoR,WRR,                                           &
   !$OMP         Hradh_t,Hradh_n,is_cohesive,k_n,k_t,detJ,Att,Atn,Ant,Ann,vt,vn,ut,un,       &
   !$OMP         Ttt,Ttn,Tnt,Tnn,                                                            &
   !$OMP         modrl,alphaik)

   !$OMP DO SCHEDULE(RUNTIME) REDUCTION(+:SumDVDVRR,SumDVDV,SumWRR,SumWRWR,SumDVoR,RWR,bR,   &
   !$OMP                      Nnoact,Nactif,Nstick,Nslide,NOKsta,Nnoctc,WNslide,SNslide,     &
   !$OMP                      Ncompr,Ntract,dynstat,Nvnish,Nhover,Nb_RGR,WNstick,SNstick,    &
   !$OMP                      sum_,dsum_)                                                    &
   !$OMP                      REDUCTION(MAX:MaxDVDV,MAXDVDVRR,WRRmax) REDUCTION(MIN:WRRmin) 

   DO ikk=1,nb_CDAN

     ! Changing at random computational ordering of contact elements
      
     ik=IALEAT(ikk)
     
     this => get_interaction(ik)

     ! Computations for case i_what = 'i_iter' or case what = 'i_post ' or case what = 'i_check'
     rlik(1:nbDIME)       = 0.D0
     rliki(1:nbDIME)      = 0.D0
     vlik(1:nbDIME)       = 0.D0
     vliki(1:nbDIME)      = 0.D0
     Wrlik(1:nbDIME)      = 0.D0
     Wrliki(1:nbDIME)     = 0.D0
     vlocfreeik(1:nbDIME) = 0.D0

     IF (this%forecast == 'noact') THEN
        
       vlik(1:nbDIME)       = this%vfree(1:nbDIME)
       vlocfreeik(1:nbDIME) = vlik(1:nbDIME)
       !mj sstatusik='noact'
       sstatusik='noctc'

     ELSE IF (this%forecast == 'acton') THEN
        
       IF (i_what == i_iter) THEN
         !rm: pas le meme critere en 2D et 3D...
         !mj if (this(ik)%status == 'noctc') then
         IF (this%rl(1) == 0.D0 .AND. this%rl(2) == 0.D0) THEN
           this%ivnish=this%ivnish+1
           IF (this%ivnish < this%iskip*(this%iskip+1)/2) THEN
             CYCLE
           ELSE
             this%iskip = this%iskip+1
           END IF
         ELSE
           this%ivnish= 0
           this%iskip = 1
         END IF
       ELSE
         this%ivnish= 0
         this%iskip = 1
       END IF
       
       !fd a voir ....
       IF (i_what == i_check) THEN
         this%ivnish= 0
         this%iskip = 1
       END IF

!!! Computing vlocfree ***************************
       
       rlik(1:nbDIME)  = this%rl(1:nbDIME)
       Wrlik(1:nbDIME) = matmul(this%W(1:nbDIME,1:nbDIME),rlik(1:nbDIME))

       ! Computing          _______________________________ H* p* invM p H Rloc(ik)
       IF(.NOT.SDLactif)THEN
         ! Computing                           ______________ H* p* invM p H Rloc
         CALL vitrad(this,iVaux_e_invM_t_Reac_)
         CALL prjj  (this,vik(1:nbDIME),iVaux_)
         vlik(1:nbDIME) = this%vfree(1:nbDIME) + vik(1:nbDIME)
         ! Computing_________________________________________ H* p* invM p H Rloc - H* p* invM p H Rloc(ik)     
         !                                                  = vlocfree(ik)
         vlocfreeik(1:nbDIME) = vlik(1:nbDIME) - Wrlik(1:nbDIME)
         !
       ELSE
         ! Computing contribution of contacts jl
         vlocfreeik(1:nbDIME) = 0.D0
         istart = this%istart
         
         IF (this%nbadj /= 0) THEN
           DO iadj=1,this%nbadj
             ikjl = this%adjjl(iadj)
             this_ikjl => get_interaction(ikjl)

             iistart = istart + nbDIME*nbDIME*(iadj-1)
             !\todo: rm: is written as a matmul... 
             vlocfreeik(1:nbDIME) = vlocfreeik(1:nbDIME) + matmul ( reshape(Wab(iistart:iistart+nbDIME*nbDIME-1), &
                                                                            shape=(/nbDIME,nbDIME/) ), this_ikjl%rl(1:nbDIME) )
           END DO
         END IF

         vlocfreeik(1:nbDIME) = vlocfreeik(1:nbDIME) + this%vfree(1:nbDIME)

         !fd ce signe est normal il vient des 2 constructions differentes
         vlik(1:nbDIME) = vlocfreeik(1:nbDIME) + Wrlik(1:nbDIME)

       END IF

!!! Convert to auxiliary ********************************************************

       WWik(1:nbDIME,1:nbDIME) = this%W(1:nbDIME,1:nbDIME)

       vvlocfreeik(1:nbDIME) = this%covfree(1:nbDIME) + vlocfreeik(1:nbDIME)
       
       fricik = this%fric
       ilaw   = this%i_law
       ibehav = this%lawnb

       select case(ilaw)
       case default
         if( nbDIME == 2 ) then
           CALL mu_SC_std_solver(this%det,this%forward,this%backward, fricik, &
                                 WWik, vvlocfreeik, sstatusik, rrlik)
         else
           call faterr(IAM,'mu_NG_solver not implemented yet')
           !CALL mu_NG_solver(ik,WWnnik,WWttik,WWssik,WWstik,WWtsik,WWnsik,WWsnik,WWtnik,WWntik,  &
           !     vvlocfreenik,vvlocfreetik,vvlocfreesik,rlnik,rltik,rlsik,fricik, &
           !     Wrlnik,Wrltik,Wrlsik,rlniki,rltiki,rlsiki,sstatusik)
         end if
       end select
!!! Restore genuine **********************************************************

       ! default
       rliki(1:nbDIME) = rrlik(1:nbDIME)
       
       ! vltiki, vlniki, this(ik)%gapTT, are so far not to be used; computation is saved.

       ilaw   = this%i_law
       ibehav = this%lawnb

       IF (i_what == i_iter) THEN
         dsum_(1:nbDIME) = dsum_(1:nbDIME) + dabs(rlik(1:nbDIME) - rliki(1:nbDIME))
          sum_(1:nbDIME) =  sum_(1:nbDIME) + dabs(rlik(1:nbDIME))
       ENDIF

     END IF ! end if (this(ik)%forecast == 'noact') else if (this(ik)%forecast == 'acton')
!!!
!!! Updating *********************************************
!!!
     IF (i_what /= i_check) THEN

       IF (this%forecast == 'noact') THEN

         rliki(1:nbDIME)   = 0.D0
         this%rl(1:nbDIME) = rliki(1:nbDIME)

         this%status = sstatusik
         ! No further updating is necessary since noact candidates for contact are, by definition,
         ! assigned not to interfer with other candidates.

       ELSE IF (this%forecast == 'acton') THEN
       
         rliki(1:nbDIME) = RELAX*rliki(1:nbDIME)+RELAX1*rlik(1:nbDIME)
         
         where( DABS(rliki) .LT. 1.D-24 ) rliki = 0.D0

         this%rl(1:nbDIME) = rliki(1:nbDIME)

         this%status = sstatusik

         IF( .NOT.SDLactif ) THEN
           ! Computing_________________________________________  R - H Rloc(ik) + H Rloc(ik)
           rik(1:nbDIME) = this%rl(1:nbDIME) - rlik(1:nbDIME) 
           ! Injecting difference between impulse reactions after and before going through the single
           ! contact solver
           CALL injj(this,rik(1:nbDIME),iReac_)
         END IF
          
       END IF
        
     ELSE
        
       ! Updating is purposedly omitted while check, according to the definition of violations.
       ! Only rltiki, rlniki, (weighting with RELAX is also omitted), rltik, rlnik, sstatusik, 
       ! are used for computing violations.
       ! check is an apart process and does not interfer with iter process values. 
       IF(norm_check)THEN
          RWR = RWR + dot_product(vlik(1:nbDIME),this%rl(1:nbDIME))
          bR  = bR  + dot_product(this%vfree(1:nbDIME)+this%covfree(1:nbDIME),this%rl(1:nbDIME))
       END IF
     END IF


!!! end case i_what = 'i_iter' ********************************
      
!!! Further computations for case i_what = 'i_post '***********

     IF (i_what == i_post) THEN

       ! Rebuilding relative velocities, gap, status, with last known reaction values 

       Wrliki(1:nbDIME) = matmul(this%W(1:nbDIME,1:nbDIME),rliki(1:nbDIME))
       vliki(1:nbDIME)  = Wrliki(1:nbDIME) + vlocfreeik(1:nbDIME)
       
       where( DABS(vliki) .LT. 1.D-24 ) vliki = 0.D0
       this%vl(1:nbDIME) = vliki(1:nbDIME)

       this%gapTT= this%gapTTbegin + H*vliki(2)

       this%status = sstatusik
       IF ( DABS(this%gapTT ) .LT. 1.D-24 ) this%gapTT = 0.D0

     END IF

!!! end case i_what = i_post ******************************************

!!! Further computations for case i_what = i_check ********************

      IF (i_what == i_check) THEN

        ! Computing discrepancies, violations, mean values
        IF( i_checktype == i_QuadN ) THEN

          Wrliki(2) = this%W(2,2)*rliki(2)
          vliki(2)  = Wrliki(2)+vlocfreeik(2)

          IF (DABS(vliki(2)) .LT. 1.D-24) vliki(2)=0.D0
          this%statuscheck=sstatusik

          rloc(2) = 0.5D0*(rlik(2)+rliki(2))
          vl(2)   = 0.5D0*(vlik(2)+vliki(2))      
          Dvl(2)  = vlik(2)-vliki(2)

          DVDV   = Dvl(2)*Dvl(2)
          DVDVRR = DVDV*(rloc(2)*rloc(2))
          DVoR   = rloc(2)*Dvl(2)

          SumDVDV   = SumDVDV+DVDV
          MaxDVDV   = DMAX1(MaxDVDV,DVDV)
          SumDVDVRR = SumDVDVRR+DVDVRR
          MaxDVDVRR = DMAX1(MaxDVDVRR,DVDVRR)
          SumDVoR   = SumDVoR+DVoR

          WRR=0.5D0*(this%W(2,2)*rlik(2)*rlik(2)+Wrliki(2)*rliki(2))
          
          SumWRR = SumWRR+WRR
          SumWRWR= SumWRWR+this%W(2,2)*WRR

        ELSE
          ! Rebuilding relative velocities, gap, status, with last known reaction values 
          Wrliki(1:nbDIME) = matmul(this%W(1:nbDIME,1:nbDIME),rliki(1:nbDIME))
          vliki(1:nbDIME)  = Wrliki(1:nbDIME) + vlocfreeik(1:nbDIME)
          where( DABS(vliki) .LT. 1.D-24) vliki=0.D0

          this%statuscheck=sstatusik

          ! According to above computations

          rloc(1:nbDIME) = 0.5D0*(rlik(1:nbDIME)+rliki(1:nbDIME)) 
          vl(1:nbDIME)   = 0.5D0*(vlik(1:nbDIME)+vliki(1:nbDIME)) 

          Dvl(1:nbDIME) = vlik(1:nbDIME)-vliki(1:nbDIME)

          DVDV   = dot_product(Dvl(1:nbDIME),Dvl(1:nbDIME))

          modrl  = dot_product(rloc(1:nbDIME),rloc(1:nbDIME))

          DVDVRR = DVDV*modrl
          modrl = SQRT(modrl)

          DVor  = dot_product(rloc(1:nbDIME),Dvl(1:nbDIME))

          SumDVDV   = SumDVDV + DVDV
          MaxDVDV   = DMAX1(MaxDVDV,DVDV)
          SumDVDVRR = SumDVDVRR + DVDVRR
          MaxDVDVRR = DMAX1(MaxDVDVRR,DVDVRR)

          SumDVoR = SumDVoR + DVoR

          WRR = 0.5*( dot_product(Wrlik(1:nbDIME),rlik(1:nbDIME))  &
                     +dot_product(Wrliki(1:nbDIME),rliki(1:nbDIME))&
                    )

          SumWRR  = SumWRR + WRR

          SumWRWR = SumWRWR + 0.5D0*( dot_product(Wrlik(1:nbDIME),Wrlik(1:nbDIME))  &
                                     +dot_product(Wrliki(1:nbDIME),Wrliki(1:nbDIME))&
                                    )
!!!mr .... faudrait pas définir un mot clés afin de ne pas avoir à faire ces opérations
!!!        tout le temps?!
          !dynstat = dynstat & 
          !        + dabs( ( this(ik)%Wnn*(vltiki-this(ik)%vltBEGIN)-this(ik)%Wtn*(vlniki-this(ik)%vlnBEGIN)) &
          !        * (vltiki+this(ik)%vltBEGIN)/this(ik)%det &
          !        + (-this(ik)%Wnt*(vltiki-this(ik)%vltBEGIN)+this(ik)%Wtt*(vlniki-this(ik)%vlnBEGIN)) &
          !        * (vlniki+this(ik)%vlnBEGIN)/this(ik)%det)
        END IF
        ! Arrays for fine violation analysis
        Xvlton(ik)   = DVDV  ! Xvlton(ik)=DVDVRR
        WRRarray(ik) = WRR 

        this%statuscheck = sstatusik

        IF( this%forecast == 'noact' ) Nnoact=Nnoact+1

        IF( rloc(2) > 0.D0 ) THEN
           Ncompr=Ncompr+1
        ELSE IF( rloc(2) < 0.D0 ) THEN
           Ntract=Ntract+1
        ENDIF

        IF( modrl == 0.D0 ) THEN
           Nvnish=Nvnish+1
        END IF
        Nactif=Ncompr+Ntract
        
        IF (this%statuscheck == 'noctc'  .OR. &
            this%statuscheck == 'Wnctc'  .OR. &
            this%statuscheck == 'Mnctc') THEN
           Nhover=Nhover+1 
        END IF
        
        IF (this%statuscheck == 'stick'  .OR. &
            this%statuscheck == 'Wstck'  .OR. & 
            this%statuscheck == 'Mstck'  .OR. &
            this%statuscheck == 'Cstck') THEN   
           Nstick=Nstick+1
           IF(this%ws == 0)THEN
              WNstick = WNstick + 1
           ELSE
              SNstick = SNstick + 1
           END IF
        ENDIF
         
         IF(this%statuscheck == 'slifw' .OR. this%statuscheck == 'slibw' .OR. &
            this%statuscheck == 'Wslfw' .OR. this%statuscheck == 'Wslbw' .OR. &
            this%statuscheck == 'Mslfw' .OR. this%statuscheck == 'Mslbw' .OR. &
            this%statuscheck == 'Cslfw' .OR. this%statuscheck == 'Cslbw' .or. &
            this%statuscheck == 'slide' .OR. this%statuscheck == 'Wslid' .or. &
            this%statuscheck == 'Cslid' ) THEN  
            Nslide=Nslide+1
            IF(this%ws == 0)THEN
               WNslide = WNslide + 1
            ELSE
               SNslide = SNslide + 1
            END IF
         END IF
         
         IF( this%status /= this%statuscheck ) THEN
            NOKsta = NOKsta + 1
            IF(this%ws == 0)THEN
               NOKweak = NOKweak + 1
            ELSE
               NOKstrg = NOKstrg + 1
            END IF
         END IF
         
         IF (this%statuscheck == '_RGR_') Nb_RGR = Nb_RGR+1
         
         !rm: du coup... mj ? ou le meme que dans nlgs_3D ?
         ! Fine violation analysis
         !mj       if (this(ik)%statuscheck .ne. 'noctc') then
         IF (modrl .NE. 0.D0) THEN !mj
            WRRmin=DMIN1(WRRmin,WRRarray(ik))
            WRRmax=DMAX1(WRRmax,WRRarray(ik))
            IF (QuadWR == 0.d0) THEN
               Xvlton(ik)=DSQRT(Xvlton(ik))
            ELSE  
               Xvlton(ik)=DSQRT(Xvlton(ik))/(QuadWR*tol)
            END IF
            ! The violation has the form (sqrt(DVDV)/sqrt(WRWR))/tol
         END IF
         
         ! Sending violations for violation chart
         SELECT CASE (this%CDAN)
         CASE (i_DKDKx)
         !   CALL put_violation_DKDKx(this%icdan,Xvlton(ik))
         !CASE (i_DKKDx)
         !   CALL put_violation_DKKDx(this%icdan,Xvlton(ik)) 
         !CASE (i_DKJCx)
         !   CALL put_violation_DKJCx(this%icdan,Xvlton(ik)) 
         !CASE (i_DKDPx)
         !   CALL put_violation_DKDPx(this%icdan,Xvlton(ik))
         !CASE (i_DKPDx)
         !   CALL put_violation_DKPDx(this%icdan,Xvlton(ik))
         !CASE (i_DKPLx)
         !   CALL put_violation_DKPLx(this%icdan,Xvlton(ik))
         !CASE (i_PTPT2)
         !   CALL put_violation_PTPT2(this%icdan,Xvlton(ik)) 
         !CASE (i_PLPLx) 
         !   CALL put_violation_PLPLx(this%icdan,Xvlton(ik))
         !CASE (i_PLJCx) 
         !   CALL put_violation_PLJCx(this%icdan,Xvlton(ik))
         !CASE (i_CLALp)
         !   CALL put_violation_CLALp(this%icdan,Xvlton(ik)) 
         !CASE (i_CLJCx)
         !   CALL put_violation_CLJCx(this%icdan,Xvlton(ik)) 
         !CASE (i_P2P2L)
         !   CALL put_violation_P2P2L(this%icdan,Xvlton(ik)) 
         !CASE (i_PLALp)
         !   CALL put_violation_PLALp(this%icdan,Xvlton(ik)) 
         !CASE (i_DKALp)
         !   CALL put_violation_DKALp(this%icdan,Xvlton(ik)) 
         !CASE (i_DPALp)
         !   CALL put_violation_DPALp(this%icdan,Xvlton(ik)) 
         !CASE (i_DKDKL)
         !  CALL put_violation_DKDKL(this%icdan,Xvlton(ik)) 
         CASE default
            WRITE(cout,'(I5,A31)') this%CDAN,' is not implemented'
            CALL FATERR(IAM,cout)
         END SELECT
         
      END IF

   END DO

   !$OMP END DO
   !$OMP END PARALLEL   

!!! Summarize rough violation analysis ***********************************

   IF (i_what == i_check) THEN
      
      Nactif = nb_CDAN-Nvnish

      ! compute quantities used to check the convergence (QuadDV, MaxmDV, QuadDVR, MaxmDVR and MeanDVoR)
      call compute_convergence_norms_nlgs(Nactif, SumWRWR, SumDVDV, MaxDVDV, &
                                          SumWRR, SumDVDVRR, MaxDVDVRR, SumDVoR, tol, &
                                          QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR)

      ! compute more check quantities
      IF (Nactif .GE. 1 .AND. SumWRR .GT. 1.D-18 .AND. SumWRWR .GT. 1.D-18) THEN   
         QuadWR   = DSQRT(SumWRWR/REAL(Nactif,8))
         Dreac    = QuadWR*H  
         
         MeanWRR  = SumWRR/REAL(Nactif,8)
         rcvltm = - MeanDVoR*tol  
         dynstat  = 0.5D0*dynstat/SumWRR
      ELSE   
         QuadWR   = 0.111D-11 
         Dreac    = 0.111D-11 
         MeanWRR  = 0.111D-11
         rcvltm =   0.000D+00 
         dynstat  = 0.111D-11
      END IF
      Enrg = 0.5*RWR + bR
   END IF
   
 END SUBROUTINE solve_nlgs

 subroutine compute_convergence_norms_nlgs(Nactif_, SumWRWR_, SumDVDV_, MaxDVDV_, &
                                           SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_, &
                                           QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_)

    implicit none

    ! inputs
    integer :: Nactif_
    real(kind=8), intent(in) :: SumWRWR_, SumDVDV_, MaxDVDV_, SumWRR_, SumDVDVRR_, MaxDVDVRR_, SumDVoR_, tol_

    ! outputs
    real(kind=8), intent(out) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

    ! locals
    real(kind=8) :: QuadWR_, MeanWRR_

    if (Nactif_ >= 1 .and. SumWRR_ > 1.d-18 .and. SumWRWR_ > 1.d-18) then
       QuadWR_   = DSQRT(SumWRWR_/REAL(Nactif_,8))
       QuadDV_   = DSQRT(SumDVDV_/REAL(Nactif_,8))   / (QuadWR_*tol_)
       MaxmDV_   = DSQRT(MaxDVDV_)                  / (QuadWR_*tol_)

       MeanWRR_  = SumWRR_/REAL(Nactif_,8)
       QuadDVR_  = DSQRT(SumDVDVRR_/REAL(Nactif_,8)) / (MeanWRR_*tol_)
       MaxmDVR_  = DSQRT(MaxDVDVRR_)                / (MeanWRR_*tol_)
       MeanDVoR_ = SumDVoR_                         / (SumWRR_*tol_)
    else
       QuadDV_   = 0.111D-11
       MaxmDV_   = 0.111D-11
       
       QuadDVR_  = 0.111D-11
       MaxmDVR_  = 0.111D-11
       MeanDVoR_ = 0.111D-11
    end if

 end subroutine compute_convergence_norms_nlgs

!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 subroutine compute_local_free_vlocy(list_INTRF)
   IMPLICIT NONE
   INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: list_INTRF
   !
   INTEGER :: i,ik
   type(T_interaction), pointer :: this

   IF (.NOT. PRESENT(list_INTRF)) THEN

      IF (nb_CDAN == 0) RETURN

      DO ik=1,nb_CDAN
         this => get_interaction(ik)
         CALL prjj(this,this%vfree(1:nbDIME),iVfree)
      END DO
   
   ELSE

      DO i = 1, size(list_INTRF)
         ik=list_INTRF(i)
         this => get_interaction(ik)
         CALL prjj(this,this%vfree(1:nbDIME),iVfree)
      END DO

   END IF

 end subroutine compute_local_free_vlocy
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 subroutine prjj(this,vik,storage)
   implicit none
   type(T_interaction), intent(in) :: this    !< [in] interaction
   real(kind=8), dimension(nbDIME) :: vik     !< [in] velocity
   integer(kind=4)    , intent(in) :: storage !< [in] what velocity to get
   !
   real(kind=8), dimension(6) :: Vcd, Van
   integer(kind=4)   :: i_sol
   character(len=18) :: IAM
   character(len=80) :: cout
   !    123456789012345678
   IAM='nlgs_newarch::prjj'
  
   select case( storage )
   case(iVaux_)
     i_sol = 2
   case(iVfree)
     i_sol = 1
   case default
      write(cout, '(A,1x,I0)') 'unknow storage type:',storage
      call faterr(IAM,cout)
   end select
  
   ! Vcd et Van sont alloués ici (automatiquement), mais du coup on se trimballe une copy jusque ici

   select case( this%icdtyp )
   case( i_diskx, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     call get_sol(this%icdbdy,i_sol, Vcd(1:3))
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call get_sol(this%icdbdy,i_sol, Vcd(1:6))
   !rm: trucs differents pour chaque contactor en maillé
   !    en fait il faut pouvoir recuperer juste la section qui concerne le contacteur
   !case( i_clxxx, i_diskl, i_pt2dl, i_csxx3, i_csxx4 )
   !  cdreac(1:nbDIME) = dot_product(rik(1:nbDIME),this%uc(1,(1:nbDIME))
   case default
      write(cout,'(I0,A)') this%icdtyp ,' is not implemented'
      call faterr(IAM,cout)
   end select
 
   select case( this%iantyp )
   case( i_diskx, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     call get_sol(this%ianbdy,i_sol, Van(1:3))
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     call get_sol(this%ianbdy,i_sol, Van(1:6))
   !rm: trucs differents pour chaque contactor en maillé
   !case( i_alpxx, i_diskl, i_pt2dl, i_aspx3, i_aspx4 )
   case default
      write(cout,'(I5,A31)') this%iantyp,' is not implemented'
      call faterr(IAM,cout)
   end select

   vik(1) = dot_product(Vcd(1:nbDIME),this%uc(1:nbDIME,1)) &
           -dot_product(Van(1:nbDIME),this%uc(1:nbDIME,1))
   vik(2) = dot_product(Vcd(1:nbDIME),this%uc(1:nbDIME,2)) &
           -dot_product(Van(1:nbDIME),this%uc(1:nbDIME,2))

   if( nbDIME == 2 ) then
     vik(1) = vik(1) + Vcd(3)*this%Gcd(1,1) - Van(3)*this%Gan(1,1)
     vik(2) = vik(2) + Vcd(3)*this%Gcd(2,1) - Van(3)*this%Gan(2,1)
   else if( nbDIME == 3 ) then
     vik(1) = vik(1) + dot_product(Vcd(4:6),this%Gcd(1,1:nbDIME)) &
                     - dot_product(Van(4:6),this%Gan(1,1:nbDIME))
     vik(2) = vik(2) + dot_product(Vcd(4:6),this%Gcd(2,1:nbDIME)) &
                     - dot_product(Van(4:6),this%Gan(2,1:nbDIME))
     vik(3) = dot_product(Vcd(1:nbDIME),this%uc(3,1:nbDIME)) &
             -dot_product(Van(1:nbDIME),this%uc(3,1:nbDIME)) &
             +dot_product(Vcd(4:6),this%Gcd(3,1:nbDIME))     &
             -dot_product(Van(4:6),this%Gan(3,1:nbDIME))
   end if
 end subroutine prjj
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 subroutine injj(this,rik,storage)
   implicit none
   type(T_interaction), intent(in) :: this    !< [in] interaction
   real(kind=8), dimension(nbDIME) :: rik     !< [in] reaction
   integer(kind=4)    , intent(in) :: storage !< [in] in what reaction to add
   !
   !nteger(kind=4), dimension(6) :: cdccdof, anccdof
   real(kind=8)   , dimension(6) :: cdreac, anreac
   integer(kind=4)   :: i_rhs
   character(len=18) :: IAM
   character(len=80) :: cout
   !    123456789012345678
   IAM='nlgs_newarch::injj'
  
   select case( storage )
   case(iReac_)
     i_rhs = 2
   case(iRaux_)
     i_rhs = 3
   case default
      write(cout, '(A,1x,I0)') 'unknow storage type:',storage
      call faterr(IAM,cout)
   end select
  
   select case( this%icdtyp )
   case( i_diskx, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     !cdccdof(1:nbDIME+1) = (/1,2,3/)
     cdreac(1)  = dot_product(rik(1:nbDIME),this%uc(1,1:nbDIME))
     cdreac(2)  = dot_product(rik(1:nbDIME),this%uc(2,1:nbDIME))
     cdreac(3)  = dot_product(rik(1:nbDIME),this%Gcd(1:nbDIME,1))
     call add_to_rhs(this%icdbdy, i_rhs, cdreac(1:nbDIME+1))
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     !dccdof(1:2*nbDIME) = (/1,2,3,4,5,6/)
     cdreac(1)  = dot_product(rik(1:nbDIME),this%uc(1,1:nbDIME))
     cdreac(2)  = dot_product(rik(1:nbDIME),this%uc(2,1:nbDIME))
     cdreac(3)  = dot_product(rik(1:nbDIME),this%uc(3,1:nbDIME))
     cdreac(4)  = dot_product(rik(1:nbDIME),this%Gcd(1:nbDIME,1))
     cdreac(5)  = dot_product(rik(1:nbDIME),this%Gcd(1:nbDIME,2))
     cdreac(6)  = dot_product(rik(1:nbDIME),this%Gcd(1:nbDIME,3))
     call add_to_rhs(this%icdbdy, i_rhs, cdreac(1:2*nbDIME))
   !rm: trucs differents pour chaque contactor en maillé
   !case( i_clxxx, i_diskl, i_pt2dl, i_csxx3, i_csxx4 )
   !  cdreac(1:nbDIME) = dot_product(rik(1:nbDIME),this%uc(1,(1:nbDIME))
   case default
      write(cout,'(I5,A31)') this%icdtyp ,' is not implemented'
      call faterr(IAM,cout)
   end select
 
   select case( this%iantyp )
   case( i_diskx, i_xksid, i_dispx, i_xpsid, i_polyg, i_joncx, i_pt2dx)
     !anccdof(1:nbDIME+1) = (/1,2,3/)
     anreac(1:2)= - cdreac(1:2)
     anreac(3)  =-dot_product(rik(1:nbDIME),this%Gan(1:nbDIME,1))
     call add_to_rhs(this%ianbdy, i_rhs, anreac(1:nbDIME+1))
   case( i_spher, i_cylnd, i_dnlyc, i_polyr, i_planx, i_pt3dx )
     !anccdof(1:2*nbDIME) = (/1,2,3,4,5,6/)
     anreac(1:3)= -cdreac(1:3)
     anreac(4)  =-dot_product(rik(1:nbDIME),this%Gan(1:nbDIME,1))
     anreac(5)  =-dot_product(rik(1:nbDIME),this%Gan(1:nbDIME,2))
     anreac(6)  =-dot_product(rik(1:nbDIME),this%Gan(1:nbDIME,3))
     call add_to_rhs(this%ianbdy, i_rhs, anreac(1:2*nbDIME))
   !rm: trucs differents pour chaque contactor en maillé
   !case( i_alpxx, i_diskl, i_pt2dl, i_aspx3, i_aspx4 )
   case default
      write(cout,'(I5,A31)') this%iantyp,' is not implemented'
      call faterr(IAM,cout)
   end select

 end subroutine injj
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 subroutine nullify_reac(this,storage)
   implicit none
   type(T_interaction), intent(in) :: this    !< [in] interaction
   integer(kind=4)    , intent(in) :: storage !< [in] what reaction to nullify
   !
   integer(kind=4)   :: i_rhs
   CHARACTER(len=27) :: IAM
   CHARACTER(len=80) :: cout
   !    123456789012345678901234567
   IAM='nlgs_newarch::nullify_reac'
  
   select case( storage )
   case(iReac_)
     i_rhs = 2
   case(iRaux_)
     i_rhs = 3
   case default
      write(cout, '(A,1x,I0)') 'unknow storage type:',storage
      call faterr(IAM,cout)
   end select
  
   call reset_rhs(this%icdbdy, i_rhs)
   call reset_rhs(this%ianbdy, i_rhs)
     
 end subroutine nullify_reac
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 subroutine vitrad(this,storage)
   implicit none
   type(T_interaction), intent(in) :: this    !< [in] interaction
   integer(kind=4)    , intent(in) :: storage !< [in] what reaction to nullify
   !
   integer(kind=4)   :: i_sol, i_rhs
   character(len=21) :: IAM
   character(len=80) :: cout
   !    123456789012345678901
   IAM='nlgs_newarch::vitrad'
   
   select case( storage )
   case(iVaux_e_invM_t_Raux_)
     i_sol = 2
     i_rhs = 3
   case(iVaux_e_invM_t_Reac_)
     i_sol = 2
     i_rhs = 2
   case default
      write(cout, '(A,1x,I0)') 'unknow storage type:',storage
      call faterr(IAM,cout)
   end select
  
   call set_system_rhs(this%icdbdy, i_rhs)
   call solve(this%icdbdy, i_sol, i_rhs)
     
   call set_system_rhs(this%ianbdy, i_rhs)
   call solve(this%ianbdy, i_sol, i_rhs)
     
 end subroutine vitrad
 !------------------------------------------------------------------------
 !------------------------------------------------------------------------
 subroutine nullify_vlocy(this,storage)
   implicit none
   type(T_interaction), intent(in) :: this    !< [in] interaction
   integer(kind=4)    , intent(in) :: storage !< [in] what velocity to nullify
   !
   integer(kind=4)   :: i_sol
   character(len=27) :: IAM
   character(len=80) :: cout
   !    1234567890123456789012345678
   IAM='nlgs_newarch::nullify_vlocy'
  
   select case( storage )
   case(iVaux_)
     i_sol = 2
   case default
      write(cout, '(A,1x,I0)') 'unknow storage type:',storage
      call faterr(IAM,cout)
   end select
  
   call reset_sol(this%icdbdy, i_sol)
   call reset_sol(this%ianbdy, i_sol)
     
 end subroutine nullify_vlocy

 !------------------------------------------------------------------------
 SUBROUTINE print_info(ik)
   
   IMPLICIT NONE
   
                            !1234567890123456
   CHARACTER(len=16) :: IAM='nlgs::print_info'
   CHARACTER(len=80) :: cout
   
   INTEGER :: ik
  
   !call display_interaction(ik)
   
 END SUBROUTINE print_info
 !------------------------------------------------------------------------
 SUBROUTINE mu_SC_wear_solver(internal,vvlt,vvln,vlnBEGIN,gapTTbegin, &
                              fric,kcdwear,kanwear,WWtt,WWtn,WWnt,WWnn, &
                              vvlocfreet,vvlocfreen,  &
                              sstatus,rrlt,rrln)
   IMPLICIT NONE

   INTEGER :: i,j
   REAL(kind=8), DIMENSION(2,2):: B,D
   REAL(kind=8), DIMENSION(2)  :: ulib,delta,rloc,lambda
   REAL(kind=8), DIMENSION(4,4):: MM,Grad
   REAL(kind=8), DIMENSION(4):: y
   REAL(kind=8), DIMENSION(4)  :: e,gy
   REAL(kind=8)                :: p,diff,sign1,sign2,vwear,xnbegin,kw,kcdwear,kanwear
   REAL(kind=8)                :: fric,WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   REAL(kind=8)                :: vvlt,vvln,rrlt,rrln,vlnBEGIN,gapTTbegin,norm
   CHARACTER(len=5)            :: sstatus
   INTEGER, DIMENSION(4)       :: ipiv
   CHARACTER(len=1)            :: trans
   
   REAL(kind=8),DIMENSION(3)   :: internal

   INTEGER :: info


   p = 1.D0/WWnn
   e = 0.D0
   kw = kcdwear+kanwear
   vwear = internal(1)
   e(1) = vvlocfreet
   e(2) = vvlocfreen

   delta(1) = vvlt
   delta(2) = vvln-vwear
   rloc(1)  = rrlt
   rloc(2)  = rrln

   D = 0.D0
   D(1,1) = 1.D0
   D(2,2) = 1.D0
   B(1,1) = -H*WWtt 
   B(1,2) = -H*WWtn
   B(2,1) = -H*WWnt
   B(2,2) = -H*WWnn
  

!!!fd to fj xnbegin et gapTT doivent etre la meme chose !

   xnbegin = gapTTbegin+H*(1-THETA)*vlnBEGIN

!!! newton ou le nombre d'iterations max = 300

   DO i = 1,300
      MM   = 0.D0
      Grad = 0.D0
      gy=0.d0
      MM(1:2,1:2)=D;MM(1:2,3:4)=B;MM(3,3)=-1.D0;MM(4,4)=-1.D0
      lambda(2)=rloc(2)-p*(xnbegin+H*THETA*(delta(2)+kw*rloc(2)*dabs(delta(1))))

! NO CONTACT
    IF (lambda(2)<=0.d0) THEN   
      y=e
      CALL la_gesv(MM,y,ipiv,info)
      sstatus='noctc'
! CONTACT ...
    ELSE                        
      lambda(1)=rloc(1)-p*delta(1)
!     ... STICKING
      IF ( dabs(lambda(1))<=fric*lambda(2)) THEN   
        Grad(4,4)=1.d0-p*kw*dabs(delta(1)); Grad(3,1)=-p
        Grad(3,3)=1.d0;Grad(4,2)=-H*THETA*p
        IF (delta(1)>0.D0) THEN
          Grad(4,1)=-p*H*THETA*kw*rloc(2)
        ELSE IF (delta(1)<0.D0) THEN
          Grad(4,1)=p*H*THETA*kw*rloc(2)
        END IF
        MM=MM+Grad
        gy(1:2)=0.d0;gy(3)=lambda(1);gy(4)=lambda(2)
        y=MATMUL(Grad,y)-gy+e
        CALL la_gesv(MM,y,ipiv,info)
        sstatus='stick'
!     ... SLIDING ...
      ELSE                                 
!             ... BACKWARD
        IF (lambda(1)>fric*lambda(2)) THEN
          Grad(4,4)=1.d0-p*H*THETA*kw*dabs(delta(1));Grad(4,2)=-p*H*THETA
          Grad(3,4)=fric*(1.d0-p*H*THETA*kw*dabs(delta(1)))
          Grad(3,2)=-fric*p*H*THETA
          IF (delta(1)>0.D0) THEN
            Grad(4,1)=-p*H*THETA*kw*rloc(2) ;Grad(3,1)=-p*H*THETA*fric*kw*rloc(2)
          ELSE IF (delta(1)<0.D0) THEN
            Grad(4,1)=p*H*THETA*kw*rloc(2) ;Grad(3,1)=p*H*THETA*fric*kw*rloc(2)
          END IF
          MM=MM+Grad
          gy(1:2)=0.d0;gy(4)=lambda(2);gy(3)=fric*lambda(2)
          y=MATMUL(Grad,y)-gy+e
          CALL la_gesv(MM,y,ipiv,info)
          sstatus='slibw'
!             ... FORWARD
        ELSE IF (lambda(1)<-(fric*lambda(2))) THEN
          Grad(4,4)=1.d0-p*H*THETA*kw*dabs(delta(1));Grad(4,2)=-p*H*THETA
          Grad(3,4)=-fric*(1.d0-p*H*THETA*kw*dabs(delta(1)))
          Grad(3,2)=fric*p*H*THETA
          IF (delta(1)>0.D0) THEN
            Grad(4,1)=-p*H*THETA*kw*rloc(2) ;Grad(3,1)=p*H*THETA*fric*kw*rloc(2)
          ELSE IF (delta(1)<0.D0) THEN
            Grad(4,1)=p*H*THETA*kw*rloc(2) ;Grad(3,1)=-p*H*THETA*fric*kw*rloc(2)
          END IF
          MM=MM+Grad
          gy(1:2)=0.d0;gy(4)=lambda(2);gy(3)=-fric*lambda(2)
          y=MATMUL(Grad,y)-gy+e
          CALL la_gesv(MM,y,ipiv,info)
          sstatus='slifw'
        END IF
      END IF
    END IF

    norm=SQRT(rloc(1)**2+rloc(2)**2+delta(1)**2+delta(2)**2)
    IF (norm/=0.D0) THEN
      diff=dabs(rloc(1)-y(3))+dabs(rloc(2)-y(4))+dabs(delta(1)-y(1))+ &
           dabs(delta(2)-y(2))/norm
    ELSE
      diff=dabs(rloc(1)-y(3))+dabs(rloc(2)-y(4))+dabs(delta(1)-y(1))+dabs(delta(2)-y(2)) 
    END IF
    IF (diff<1.D-4) EXIT
    delta=y(1:2)
    rloc=y(3:4)
  END DO
  delta=y(1:2)
  rloc=y(3:4)
  vwear=kw*rloc(2)*dabs(delta(1))
  vvln=delta(2)+vwear
  vvlt=delta(1)
  rrln=H*rloc(2)
  rrlt=H*rloc(1)

  IF (i>=300) THEN
    WRITE(*,*)'Nombre iterations de Newton =',i
    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*)'Reactions',rloc(1), rloc(2)
    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*)'vitesse locale elastique',delta(1),delta(2)
    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*)'vitesse d usure',vwear
    WRITE(*,*)
    WRITE(*,*)
    WRITE(*,*)'vitesse locale total',vvlt,vvln
    STOP
  END IF

  internal(1)=vwear
  internal(2)= vwear*kcdwear/kw
  internal(3)=-vwear*kanwear/kw
  
 END SUBROUTINE mu_SC_wear_solver  
!------------------------------------------------------------------------
!------------------------------------------------------------------------
 SUBROUTINE coupled_dof_solver(det,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                               sstatus,rrlt,rrln)

   ! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   ! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   ! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   ! They may be computed within the subroutine from input data.
   ! Here they have been precomputed in prep_nlgs in order to save floating point operations.

   IMPLICIT NONE
   REAL(kind=8)     :: WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   REAL(kind=8)     :: DFT,DFN
   REAL(kind=8)     :: vvlt,vvln,rrlt,rrln
   CHARACTER(len=5) :: sstatus

   DFT= WWnn*vvlocfreet-WWtn*vvlocfreen
   DFN=-WWnt*vvlocfreet+WWtt*vvlocfreen

   rrlt=-DFT/det
   rrln=-DFN/det

   vvln=0.d0
   vvlt=0.d0

   sstatus='stick'
 
 END SUBROUTINE coupled_dof_solver
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 SUBROUTINE plastic_coupled_dof_solver(det,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                                       sstatus,rrlt,rrln,seuil)

   ! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   ! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   ! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   ! They may be computed within the subroutine from input data.
   ! Here they have been precomputed in prep_nlgs in order to save floating point operations.

   IMPLICIT NONE
   REAL(kind=8)     :: WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   REAL(kind=8)     :: DFT,DFN
   REAL(kind=8)     :: vvlt,vvln,rrlt,rrln,seuil
   CHARACTER(len=5) :: sstatus

   DFT= WWnn*vvlocfreet-WWtn*vvlocfreen
   DFN=-WWnt*vvlocfreet+WWtt*vvlocfreen

   rrlt=-DFT/det
   rrln=-DFN/det

   vvln=0.d0
   vvlt=0.d0

   IF (rrln < -seuil) THEN

      rrln = -seuil
      rrlt =-(vvlocfreet + (WWtn*rrln))/WWtt 

      vvln = vvlocfreen + (WWnn*rrln) + (WWnt*rrlt)

   ENDIF
   sstatus='stick'
 
 END SUBROUTINE plastic_coupled_dof_solver
!------------------------------------------------------------------------  
!------------------------------------------------------------------------
 SUBROUTINE tangential_coupled_dof_solver(det,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen,  &
                              sstatus,rrlt,rrln)

   ! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
   ! sstatus,vvlt,vvln,rrlt,rrln, are output data.
   ! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
   ! They may be computed within the subroutine from input data.
   ! Here they have been precomputed in prep_nlgs in order to save floating point operations.

   IMPLICIT NONE
   REAL(kind=8)     :: WWtt,WWtn,WWnt,WWnn,det,vvlocfreet,vvlocfreen
   REAL(kind=8)     :: vvlt,vvln,rrlt,rrln
   CHARACTER(len=5) :: sstatus

   rrlt= -vvlocfreet/WWtt 
   rrln= 0.d0

   vvlt=0.d0
   vvln=vvlocfreen + WWnt*rrlt

   sstatus='stick'
 
 END SUBROUTINE tangential_coupled_dof_solver
!!!------------------------------------------------------------------------
!!! NOT TO BE MODIFIED ...
!!!------------------------------------------------------------------------  
 SUBROUTINE mu_SC_viscous_solver(det,forward,backward, &
                                 fric,internal,WWtt,WWnn,vvlocfreet,vvlocfreen,  &
                                 sstatus,rrlt,rrln)

!!! fric,WWtt,WWtn,WWnt,WWnn,vvlocfreet,vvlocfreen, are input data.
!!! sstatus,vvlt,vvln,rrlt,rrln, are output data.
!!! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
!!! They may be computed within the subroutine from input data.
!!! Here they have been precomputed in prep_nlgs in order to save floating point operations.


   IMPLICIT NONE
   REAL(kind=8)     :: fric,WWtt,WWnn,det,vvlocfreet,vvlocfreen
   REAL(kind=8)     :: vvlt,vvln,rrlt,rrln,NUT,internal
   CHARACTER(len=5) :: sstatus
   REAL(kind=8)     :: DFT,DFN,Cforward,Cbackward,forward,backward,FFN

   DFT = WWnn*vvlocfreet
   DFN = WWtt*vvlocfreen
   FFN =-vvlocfreen/WWnn
   Cforward  = DFT+fric*DFN
   Cbackward = DFT-fric*DFN
   
   NUT = vvlocfreet*internal/(1.D0+WWtt*internal)

   IF (vvlocfreen .GE. 0.D0) THEN
      !no contact
      rrln=0.D0
      rrlt=0.D0
      sstatus='noctc'
   ELSE IF (vvlocfreen .LT. 0.D0 .AND. Cforward .GT. 0.D0) THEN
      !sliding forward
      rrln=FFN/forward
      rrlt=-fric*rrln - NUT
      sstatus='slifw'                    
   ELSE IF (vvlocfreen .LT. 0.D0 .AND. Cbackward .LT. 0.D0) THEN
      !sliding backward      
      rrln=FFN/backward
      rrlt=fric*rrln - NUT
      sstatus='slibw'                    
   ELSE IF (vvlocfreen .LT. 0.D0 .AND. Cforward .LE. 0.D0 .AND. Cbackward .GE. 0.D0) THEN
      !sticking      
      rrln=-vvlocfreen/WWnn
      rrlt=-vvlocfreet/WWtt
      sstatus='stick'
   ELSE
      rrln=0.D0
      rrlt=0.D0
      sstatus='vnish'                    
   END IF
   
 END SUBROUTINE mu_SC_viscous_solver
!------------------------------------------------------------------------  
 SUBROUTINE Nullify_EntityList_nlgs

   IMPLICIT NONE

   CALL Free_EntityList

 END SUBROUTINE Nullify_EntityList_nlgs
!------------------------------------------------------------------------  
 SUBROUTINE get_nlgs_loop(compteur,err1,err2,err3,contact)

   IMPLICIT NONE
   INTEGER      :: compteur,contact
   REAL(kind=8) :: err1,err2,err3

   compteur = nlgs_loop
   contact  = nb_CDAN
   err1     = MeanDVoR
   err2     = QuadDV
   err3     = QuadDVR

 END SUBROUTINE get_nlgs_loop
!------------------------------------------------------------------------  
 SUBROUTINE get_nlgs_network_change(nctc,nweak,nstrong)

   IMPLICIT NONE

   INTEGER :: nctc,nweak,nstrong

   nctc    = NOKsta
   nweak   = NOKweak
   nstrong = NOKstrg

 END SUBROUTINE get_nlgs_network_change
!------------------------------------------------------------------------  
 SUBROUTINE get_nlgs_contact_status(noctc,Wslide,Sslide,Wstick,Sstick)

  IMPLICIT NONE

  INTEGER :: noctc,Wslide,Sslide,Sstick,Wstick
  
  noctc  = Nhover !fd obsolete Nnoctc
  Wslide = WNslide
  Sslide = SNslide
  Sstick = SNstick
  Wstick = WNstick

 END SUBROUTINE get_nlgs_contact_status
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE get_after_iter_check(ddynstat,nnb_CDAN,NNnoact,NNvnish,NNhover,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR)


   IMPLICIT NONE
   INTEGER   :: nnb_CDAN,NNnoact,NNvnish,NNhover,NNcompr,NNtract,NNslide,NNstick,NNOKsta,NNb_RGR
   REAL(kind=8) :: ddynstat

   ddynstat=dynstat
   nnb_CDAN=nb_CDAN
   NNnoact=Nnoact
   NNvnish=Nvnish
   NNcompr=Ncompr
   NNtract=Ntract
   NNslide=Nslide
   NNstick=Nstick
   NNOKsta=NOKsta
   NNb_RGR=Nb_RGR
  
 END SUBROUTINE get_after_iter_check  
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE get_somme_rn(ssomme_rn)
   IMPLICIT NONE

   INTEGER :: ik
   REAL(kind=8) :: ssomme_rn,somme_rn

   type(T_interaction), pointer :: this

   somme_rn=0.D0
   DO ik=1,nb_CDAN
     this => get_interaction(ik)
     somme_rn=somme_rn+this%rl(2)
   END DO
   ssomme_rn=somme_rn/H
  
 END SUBROUTINE get_somme_rn  
!------------------------------------------------------------------------  
!------------------------------------------------------------------------  
 SUBROUTINE update_internal(this)
   IMPLICIT NONE
   type(T_interaction) :: this
                            !1234567890123456789012
   CHARACTER(len=22) :: IAM='nlgs::update_internal'
   CHARACTER(len=80) :: cout

   INTEGER :: ibehav,ilaw

   ibehav = this%lawnb
   ilaw   = this%i_law 

   SELECT CASE(ilaw)

   CASE(i_CZM,i_IQS_CZM,i_postGAP_IQS_CZM,i_IQS_WET_CZM)

      IF (this%status(1:1) == 'C') THEN 

         if( nbDIME == 2 ) then
           CALL updt_CZM(ibehav,.TRUE.,this%internal,H*this%vl(1),H*this%vl(2))
         else if( nbDIME == 3 ) then
           CALL updt_CZM_3D(ibehav,.TRUE.,this%internal,H*this%vl(1),H*this%vl(2),H*this%vl(3))
         end if
         
      ELSE
         
         CALL raz_CZM(ibehav,this%internal)
         
      ENDIF

! fd est un pd
!!         if (ilaw == i_postGAP_IQS_MAC_CZM) this(ik)%internal(6) = this(ik)%internal(6) * this(ik)%internal(4)
      IF (ilaw == i_postGAP_IQS_CZM) this%internal(6) =  (-0.002)* (1. - this%internal(4))


   CASE(i_IQS_SGR_CLB_WEAR)

      IF ( this%internal(2) == 0 ) THEN
         IF( this%rl(2) -  this%internal(1) .LE. 0.D0 ) THEN
            this%internal(2) = 1
         END IF
      END IF

   CASE(i_IQS_CLB_nosldt)

      this%internal(2) = this%internal(2) + H*this%vl(1)

   CASE default
      
   END SELECT

 END SUBROUTINE update_internal
 !!!---------------------------------------------------------------
  SUBROUTINE scale_rloc_nlgs

    IMPLICIT NONE    

    INTEGER :: ik
    type(T_interaction), pointer :: this

    IF (nb_CDAN == 0) RETURN

    Scale=DMIN1(Scale,1.1D0)
    Scale=DMAX1(Scale,0.9D0)      

    ! Rnod = [H] Rloc
    DO ik=1,nb_CDAN 
       this => get_interaction(ik)
       this%rl(1:nbDIME) = this%rl(1:nbDIME)*Scale
       CALL nullify_reac(this,iReac_)
    END DO
    DO ik=1,nb_CDAN
       this => get_interaction(ik)
       CALL injj(this,this%rl(1:nbDIME),iReac_)
    END DO

  END SUBROUTINE scale_rloc_nlgs
!!!---------------------------------------------------------------
  SUBROUTINE reverse_nlgs

    IMPLICIT NONE    

    INTEGER :: ik

    IF (nb_CDAN == 0) RETURN

    DO ik=1,nb_CDAN
       ialeatr(ik)=ialeat(ik)
    END DO
    DO ik=1,nb_CDAN
       ialeat(ik)=nb_CDAN-ialeatr(ik)+1
    END DO

  END SUBROUTINE reverse_nlgs
!!!---------------------------------------------------------------
  SUBROUTINE bimodal_list_nlgs

    IMPLICIT NONE
    INTEGER :: ik

    IF (nb_CDAN == 0) RETURN

    DO ik=1,nb_CDAN
       ialeat(ik)=iwksg(ik)
    END DO

  END SUBROUTINE bimodal_list_nlgs
!!!---------------------------------------------------------------
  SUBROUTINE RnodHRloc_nlgs(list_INTRF, storage_reac)

    IMPLICIT NONE

    ! optional inputs, used in DDM
    INTEGER, DIMENSION(:), INTENT(in), OPTIONAL :: list_INTRF
    INTEGER, INTENT(IN), OPTIONAL :: storage_reac

    ! locals
    INTEGER :: i,ik
    !am: storage specifies where to store the reaction torque.
    INTEGER :: storage

    type(T_interaction), pointer :: this

    ! the reaction torque will be stored in Reac...
    storage = iReac_

    ! ... unless the user choose another location
    if (present(storage_reac)) storage = storage_reac

    IF (.NOT. PRESENT(list_INTRF)) THEN

       IF (nb_CDAN == 0) RETURN
   
       DO ik=1,nb_CDAN  
          this => get_interaction(ik)
          CALL nullify_reac(this, storage)
       END DO
       DO ik=1,nb_CDAN
          this => get_interaction(ik)
          CALL injj(this, this%rl(1:nbDIME), storage)
       END DO

    ELSE

       DO i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          this => get_interaction(ik)
          CALL nullify_reac(this, storage)
       END DO
       DO i = 1, size(list_INTRF)
          ik=list_INTRF(i)
          this => get_interaction(ik)
          CALL injj(this, this%rl(1:nbDIME), storage)
       END DO

    END IF

  END SUBROUTINE RnodHRloc_nlgs
!!!---------------------------------------------------------------

!!!---------------------------------------------------------------
  SUBROUTINE set_nlgs_parameter(normtype,tolerence,relaxation)

    CHARACTER(len=5)  :: normtype
    REAL(kind=8)      :: relaxation,tolerence

    tol   = tolerence
    RELAX = relaxation
    RELAX1=1.D0-RELAX

    SELECT CASE(normtype)
    CASE('QuaN ')
       i_checktype = i_QuadN
    CASE('Quad ')
       i_checktype = i_Quad
    CASE('Maxm ')
       i_checktype = i_Maxm
    CASE('QM/16')
       i_checktype = i_QMs16
    CASE DEFAULT
       CALL LOGMES(normtype)
       CALL FATERR('nlgs::set_nlgs_parameter','unknown norm type')
    END SELECT

  END SUBROUTINE set_nlgs_parameter
!!!---------------------------------------------------------------
  SUBROUTINE prep_check_nlgs(iconv)
    
    IMPLICIT NONE
    INTEGER :: ik,iconv
    
    iconv = 0
    conv_contact = .TRUE.
    IF (nb_CDAN == 0) RETURN
    iconv = 1
    conv_contact = .FALSE.
    
    CALL RnodHRloc_nlgs

    Dcrac  = 1.D0    !distance caracteristique

    SumDVDV  = 0.D0
    MaxDVDV  = 0.D0
    SumDVDVRR= 0.D0
    MaxDVDVRR= 0.D0
    SumWRWR  = 0.D0
    SumWRR   = 0.D0
    SumDVoR  = 0.D0
    dynstat  = 0.D0
    
    Dreac  = 0.D0      ! "reacteristic" distance 
    Nnoact = 0         ! number of contacts being forecasted inactive 
    Nactif = 0         ! number of active contacts (contacts where the normal reaction is not vanishing)
    
    Nvnish = 0         ! number of candidates with vanishing reactions
    Nnoctc = 0         ! number of no contacts, obsolete!
    Nhover = 0         ! number of hovering contacts (separated contacts active or not)
    
    Nslide = 0         ! number of sliding contacts
    Nstick = 0         ! number of sticking contacts
    Ncompr = 0         ! number of compressing contacts
    Ntract = 0         ! number of tensile contacts
    NOKsta = 0         ! number of questionable status
    Nb_RGR = 0         ! number of contacts where the "Radjai Gap Rescue" is active
    
    ! mj & fd -> comments ?
    WNslide = 0
    WNstick = 0
    SNstick = 0
    SNslide = 0
    NOKweak = 0
    NOKstrg = 0

    ! nbCDAN-Nnoact-Nnoctc = Nactif = Ncomp+Ntract = Nslide+Nstick+Nb_RGR

  END SUBROUTINE prep_check_nlgs
!!!-------------------------------------------------------------------------------------
  SUBROUTINE comp_check_nlgs(iconv)

    IMPLICIT NONE

    ! locals
    LOGICAL :: converged

    ! outputs
    INTEGER, INTENT(OUT) :: iconv

    iconv = 1
   
    !am: check convergence using stored quantities 
    call check_convergence_nlgs(QuadDV, MaxmDV, QuadDVR, MaxmDVR, MeanDVoR, converged)

    conv_contact = converged

    if (converged) iconv = 0

    if (i_checktype == i_Maxm .and. converged) then
       QuadDV  = MaxmDV
       QuadDVR = MaxmDVR
    end if

    Scale = 1.D0+rcvltm

  END SUBROUTINE comp_check_nlgs

  !am: this function check convergence using given quantities (the stored ones or some external ones)
  !    N.B. this function is used in DDM
  subroutine check_convergence_nlgs(QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_, converged)

     implicit none

     ! inputs
     real(kind=8), intent(in) :: QuadDV_, MaxmDV_, QuadDVR_, MaxmDVR_, MeanDVoR_

     ! outputs
     logical, intent(out) :: converged

     converged = .false.

     SELECT CASE(i_checktype)
        CASE(i_Quad, i_QuadN)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(QuadDV_)   .LT. 1.D0 .AND. &
                DABS(QuadDVR_)  .LT. 1.D0) THEN
              converged = .true.
           END IF
        CASE(i_Maxm)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(MaxmDV_)   .LT. 1.D0 .AND. &
                DABS(MaxmDVR_)  .LT. 1.D0) THEN
              converged = .true.
           END IF
        CASE(i_QMs16)
           IF ( DABS(MeanDVoR_) .LT. 1.D0 .AND. &
                DABS(QuadDV_)   .LT. 1.D0 .AND. &
                DABS(QuadDVR_)  .LT. 1.D0 .AND. &
                DABS(MaxmDV_)   .LT. 16.666D0 .AND. &
                DABS(MaxmDVR_)  .LT. 16.666D0) THEN
              converged = .true.
           END IF
     END SELECT

  end subroutine check_convergence_nlgs

!!!------------------------------------------------------------------
  SUBROUTINE display_check_nlgs
    
    IMPLICIT NONE
    CHARACTER(len=103)         :: cout 

    IF (nb_CDAN == 0) RETURN
       
    CALL LOGMES(' ')
    SELECT CASE(i_checktype)
    CASE(i_Quad,i_QuadN)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Quad ','     Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       CALL LOGMES(cout)
    CASE(i_Maxm)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  Maxm ','     Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV  
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR 
       CALL LOGMES(cout)
    CASE(i_QMs16)
       WRITE(cout,'(1X,A3,3X,A4,15X,A18,14X,A9)')    ' @ ','Quad','checktype =  QM/16','1/16 Maxm'
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDV  /QuadWR  =',QuadDV  ,'MaxmDV  /QuadWR  =',MaxmDV *0.06D0 
       CALL LOGMES(cout)
       WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')      ' @ ','QuadDVR /MeanWRR =',QuadDVR ,'MaxmDVR /MeanWRR =',MaxmDVR*0.06D0 
       CALL LOGMES(cout)
    END SELECT
    
    WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','MeanDVoR/SumWRR  =',MeanDVoR,'Free run length  =',Dreac 
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,2(3X,A18,D10.3,1X))')               ' @ ','dynamic/static   =',dynstat
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Nvnish =',Nvnish,'  nbCDAN =',nb_CDAN, 'Nhover =',Nhover
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ncompr =',Ncompr,'  Nnoact =', Nnoact, 'Nslide =',Nslide
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,(2X,A9,I10,2X,A10,I10,2X,A8,I10))') ' @ ',' Ntract =',Ntract,'  NOKsta =',NOKsta , 'Nstick =',Nstick 
    CALL LOGMES(cout)
    WRITE(cout,'(1X,A3,23X,A10,I10)')                      ' @ ',                   '  Nb_RGR =',Nb_RGR 
    CALL LOGMES(cout)
    CALL LOGMES('  ')

  END SUBROUTINE display_check_nlgs
!!!---------------------------------------------------------------------
  SUBROUTINE display_rlocn_sum_nlgs
    IMPLICIT NONE
    INTEGER :: ik
    type(T_interaction), pointer :: this
    
    somme_rn=0.D0
    DO ik=1,nb_CDAN
       this => get_interaction(ik)
       somme_rn = somme_rn+this%rl(2)
    END DO
    WRITE(6,'(A11,D14.7)') 'RlocN SUM: ', somme_rn/H

  END SUBROUTINE display_rlocn_sum_nlgs
!!!---------------------------------------------------------------------
  SUBROUTINE update_tact_behav_nlgs
    IMPLICIT NONE
    !
    INTEGER :: ik
    type(T_interaction), pointer :: this

    IF (nb_CDAN == 0) RETURN
    
    DO ik=1,nb_CDAN
       this => get_interaction(ik)
       CALL update_internal(this)
    END DO

  END SUBROUTINE update_tact_behav_nlgs
!!!---------------------------------------------------------------------
  SUBROUTINE write_norm_check_nlgs(istat)
    
    IMPLICIT NONE

    INTEGER :: istat

    SELECT CASE(istat)
    CASE(1)
       norm_check = .TRUE.
       norm_fich  = get_io_unit() 
       OPEN(UNIT=norm_fich,FILE=TRIM(location('NORM_NLGS.DAT')),STATUS='REPLACE')
    CASE(2)
       IF (norm_check) THEN
          SELECT CASE(i_checktype)
          CASE(i_Quad,i_QuadN)
             WRITE(norm_fich,'(3(2X,D14.7),3(1X,I8))') ABS(meanDVoR),QuadDVR,QuadDV,NOKsta,NOKweak,NOKstrg
          CASE(i_Maxm)
             WRITE(norm_fich,'(3(2X,D14.7),3(1X,I8))') ABS(meanDVoR),MaxmDVR,MaxmDV,NOKsta,NOKweak,NOKstrg
          END SELECT
       END IF
    CASE(3)
       IF (norm_check) CLOSE(norm_fich)
    END SELECT

  END SUBROUTINE write_norm_check_nlgs
!!!------------------------------------------------------------------------
 SUBROUTINE scramble_nlgs

   ! This subroutine scrambles the ordering of candidates for contact
   
   IMPLICIT NONE

   INTEGER       :: ik
   INTEGER       :: IALEATik,IAL1,IAL2
   REAL(kind=8)  :: RA

   DO ik=1,nb_CDAN/2
      
      CALL RANDOM_NUMBER(RA)
      IAL1 = IDINT(RA*REAL(nb_CDAN,8))+1
      IAL1 = MIN0(IAL1,nb_CDAN)
      IAL1 = MAX0(1,IAL1)
      
      CALL RANDOM_NUMBER(RA)
      IAL2 = IDINT(RA*REAL(nb_CDAN,8))+1
      IAL2 = MIN0(IAL2,nb_CDAN)
      IAL2 = MAX0(1,IAL2)
      
      IALEATik     = IALEAT(IAL1)
      IALEAT(IAL1) = IALEAT(IAL2)
      IALEAT(IAL2) = IALEATik
      
   END DO
   
 END SUBROUTINE scramble_nlgs
!!!mj---------------------------------------------------------------------- 
 SUBROUTINE quick_scramble_nlgs

   ! This subroutine scrambles the ordering of candidates for contact

   IMPLICIT NONE

   INTEGER       :: ik,IALEATik

   DO ik=1,nb_CDAN
      
      IALEATik=IALEAT(ik)
      IALEAT(ik)=IALEAT(randomlist(ik))
      IALEAT(randomlist(ik))=IALEATik
      
   END DO
   
 END SUBROUTINE quick_scramble_nlgs
!!!mr------------------------------------------------------------------------
!!!------------------------------------------------------------------------
 SUBROUTINE update_friction_coefficient(fric)

   IMPLICIT NONE

   INTEGER      :: icdan
   REAL(kind=8) :: fric,thickness

   DO icdan=1,nb_CDAN

      !CALL read_friction_map(fric,thickness)

   END DO

 END SUBROUTINE update_friction_coefficient
!!!------------------------------------------------------------------------
 !am: this function allows to get all, or a part of, summations used to compute the quantities required to check the convergence
 !    N.B. this function is used in DDM 
 SUBROUTINE get_error(SumDVDVRR_, Nactif_, MeanWRR_, &
                      tol_, SumDVDV_, QuadWR_, SumDVoR_, &
                      SumWRR_, SumWRWR_, MaxDVDV_, MaxDVDVRR_) 
    
   IMPLICIT NONE

   !am: all outpout variables are optional in order to allow the user to define which variables he want to collect
   !    N.B. some new variables were added but the order of the first variable did not change, so that callings in Iceta DDM code should still be working!
   INTEGER, INTENT(out), optional :: Nactif_
   REAL(kind=8), INTENT(out), optional :: SumDVDVRR_, &
                                          MeanWRR_, tol_, SumDVDV_, &
                                          QuadWR_, SumDVoR_, SumWRR_, &
                                          SumWRWR_, MaxDVDV_, MaxDVDVRR_
  
   ! only the asked values are returned 
   if (present(SumDVDVRR_)) SumDVDVRR_ = SumDVDVRR
   if (present(Nactif_))    Nactif_    = Nactif
   if (present(MeanWRR_))   MeanWRR_   = MeanWRR
   if (present(tol_))       tol_       = tol
   if (present(SumDVDV_))   SumDVDV_   = SumDVDV
   if (present(QuadWR_))    QuadWR_    = QuadWR
   if (present(SumDVoR_))   SumDVoR_   = SumDVoR
   if (present(SumWRR_))    SumWRR_    = SumWRR
   if (present(SumWRWR_))   SumWRWR_   = SumWRWR
   if (present(MaxDVDV_))   MaxDVDV_   = MaxDVDV
   if (present(MaxDVDVRR_)) MaxDVDVRR_ = MaxDVDVRR
 
 END SUBROUTINE get_error
!!!------------------------------------------------------------------------
 SUBROUTINE get_conv(my_sum_t,my_sum_n,my_dsum_t,my_dsum_n)
   REAL(kind=8) :: my_sum_t,my_sum_n,my_dsum_t,my_dsum_n

   my_sum_t  = sum_(1)
   my_sum_n  = sum_(2)
   my_dsum_t = dsum_(1)
   my_dsum_n = dsum_(2)
   !my_sum_s  = sum_(3)
   !my_dsum_s = dsum_(3)

 END SUBROUTINE

 SUBROUTINE active_diagonal_resolution
   IMPLICIT NONE

   diagonal_resolution = .TRUE.

 END SUBROUTINE active_diagonal_resolution
   
 subroutine mu_SC_std_solver(det,forward,backward,fric,WW,vvlocfree,sstatus,rrl)
!!! The variables det,forward,backward, are auxiliaries to be used in the subroutine.
!!! They may be computed within the subroutine from input data.
!!! Here they have been precomputed in prep_nlgs in order to save floating point operations.
   IMPLICIT NONE
   real(kind=8), dimension(nbDIME,nbDIME) ::WW
   real(kind=8), dimension(nbDIME)        ::vvlocfree, rrl
   real(kind=8)                           :: det, fric
   character(len=5)                       :: sstatus
   !
   real(kind=8), dimension(nbDIME,nbDIME) ::WI
   real(kind=8), dimension(nbDIME)        ::vvl, DF
   real(kind=8)                           :: Cforward,Cbackward,forward,backward,FFN

   WI(1,1) = WW(2,2)
   WI(1,2) =-WW(1,2)
   WI(2,1) =-WW(2,1)
   WI(2,2) = WW(1,1)

   DF(1:2) = matmul(WI(1:2,1:2),vvlocfree(1:2))
   FFN =-vvlocfree(2)/WW(2,2)

   Cforward  = DF(1)+fric*DF(2)
   Cbackward = DF(1)-fric*DF(2)
   
   IF (vvlocfree(2) .GE. 0.D0) THEN
      !no contact
      rrl(1:2)=0.D0
      sstatus='noctc'
   ELSE IF (vvlocfree(2) .LT. 0.D0 .AND. Cforward .GT. 0.D0) THEN
      !sliding forward
      rrl(2)=FFN/forward
      rrl(1)=-fric*rrl(2)
      sstatus='slifw'                    
   ELSE IF (vvlocfree(2) .LT. 0.D0 .AND. Cbackward .LT. 0.D0) THEN
      !sliding backward      
      rrl(2)=FFN/backward
      rrl(1)=fric*rrl(2)
      sstatus='slibw'                    
   ELSE IF (vvlocfree(2) .LT. 0.D0 .AND. Cforward .LE. 0.D0 .AND. Cbackward .GE. 0.D0) THEN
      !sticking      
      rrl(1:2)=-DF(1:2)/det
      sstatus='stick'
   ELSE
      rrl(1:2)=0.D0
      sstatus='vnish'                    
   END IF
   
 end subroutine mu_SC_std_solver

 function get_all_this()
   implicit none
   real(kind=8), dimension(:,:), pointer :: get_all_this
   !
   type(T_interaction), pointer :: this
   integer(kind=4) :: i_cdan, n

   get_all_this => null()

   if( nb_cdan <= 0 ) return

   allocate( get_all_this(10,nb_cdan) )

   do i_cdan = 1, nb_cdan

     this => get_interaction(i_cdan)

     get_all_this(         1:  nbDIME,i_cdan) = this%coor
     get_all_this(  nbDIME+1:2*nbDIME,i_cdan) = this%uc(1:nbDIME,1)
     get_all_this(2*nbDIME+1:3*nbDIME,i_cdan) = this%uc(1:nbDIME,2)
     n = 3
     if ( nbDIME == 3 ) then
       get_all_this(3*nbDIME+1:4*nbDIME,i_cdan) = this%uc(1:nbDIME,3)
       n = 4
     end if
     get_all_this(   n *nbDIME+1:(n+1)*nbDIME,i_cdan) = this%rl(1:nbDIME)
     get_all_this((n+1)*nbDIME+1:(n+2)*nbDIME,i_cdan) = this%vl(1:nbDIME)
     
   end do

 end function

END MODULE nlgs_newarch

