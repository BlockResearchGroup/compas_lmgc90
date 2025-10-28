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
MODULE SiconosNumerics

  use overall
  use parameters

  use lmgc90_mpi, only: lmgc_mpi_world_comm

  USE tact_behaviour
  USE utilities
  USE ALGEBRA

  use inter_meca_handler_3D, only : get_tact_lawnb , &
                                    get_nb_inters    , &
                                    set_loc          , &
                                    get_rloc         , &
                                    get_vlocBEGIN    , &
                                    get_internal     , &
                                    set_internal     , &
                                    inter2ent        , &
                                    injj             , &
                                    prjj             , &
                                    vitrad           , &
                                    nullify_reac     , &
                                    nullify_vlocy    !, &
  USE SPSPx
  USE SPPLx
  USE SPCDx
  USE SPDCx
  USE PRPRx
  USE PRPLx
  USE PTPT3
  USE CSPRx
  USE CSASp
  USE CDCDx
  USE CDPLx

  USE CSxxx
  
  use mecaMAILx, only: get_nb_mecaMAILx         , &
                       get_nb_nz_g_sys_mecaMAILx, &
                       get_nb_dofs_mecaMAILx    , &
                       get_i_indices_mecaMAILx  , &
                       get_j_indices_mecaMAILx  , &
                       get_rhs_vector_mecaMAILx , &
                       get_val_g_sys_mecaMAILx  , &
                       get_vector_mecaMAILx     , &
                       put_vector_mecaMAILx

  use RBDY3, only: get_nb_rbdy3

  use iso_c_binding


  
  IMPLICIT NONE

  PRIVATE

  ! --------------------------------------------------------------------------
  TYPE T_ctct_element
     
     INTEGER                      :: CDAN                           
     INTEGER                      :: icdan                          
     REAL(kind=8)                 :: rlt,  rln ,  rls           
     REAL(kind=8)                 :: vlt,  vln ,  vls

     REAL(kind=8)                 :: corlt,corln ,corls

     integer(kind=4)             :: status,statuscheck                         

     REAL(kind=8)                 :: vfreet,   vfreen,   vfrees
     REAL(kind=8)                 :: covfreet, covfreen, covfrees
                                                                 
     REAL(kind=8)                 :: vltBEGIN, vlnBEGIN, vlsBEGIN
                                                                 
     REAL(kind=8)                 :: gapTTbegin
     REAL(kind=8)                 :: gapREF
     integer(kind=4)              :: statusBEGIN
     integer(kind=4)              :: forecast  !(1=acton, 0=noact)
                                             
                                             
     INTEGER                      :: lawnb
     REAL(kind=8)                 :: fric
     INTEGER                      :: inoctc,iskip
     
     INTEGER                      :: nbadj,icdent,ianent,istart 
     INTEGER                      :: i_law

     INTEGER,DIMENSION(:),POINTER :: adjjl

     REAL(kind=8),DIMENSION(max_internal_tact) :: internal

     ! fd pour le contact global
     integer(kind=4),dimension(:),pointer :: cd_dof, an_dof
     real(kind=8),dimension(:,:),pointer :: g2l

  END TYPE T_ctct_element


  integer(kind=4),parameter :: i_acton=1,i_noact=0

  ! matrice w sparse par bloc a plat, l_a/l_b donnent pour un bloc sa coordonnee ik,jl  

!!!
!!!  W(this(ik)%istart+9*iadj-8) = WNN ; W(this(ik)%istart+9*iadj-5) = WNT ; W(this(ik)%istart+9*iadj-2) = WNS         
!!!  W(this(ik)%istart+9*iadj-7) = WTN ; W(this(ik)%istart+9*iadj-4) = WTT ; W(this(ik)%istart+9*iadj-1) = WST 
!!!  W(this(ik)%istart+9*iadj-6) = WSN ; W(this(ik)%istart+9*iadj-3) = WST ; W(this(ik)%istart+9*iadj  ) = WSS 
!!!

  REAL(kind=8),DIMENSION(:),pointer   :: Wab => null()
  integer,DIMENSION(:),pointer        :: l_a => null(), l_b => null()

  ! second membre ik

  REAL(kind=8),DIMENSION(:),pointer   :: q => null(), z => null(), u => null(), mu => null()

  ! global solver
  real(kind=8)   ,dimension(:), pointer :: all_M  => null(), all_H  => null()
  integer(kind=4),dimension(:), pointer :: all_iM => null(), all_jM => null()
  integer(kind=4),dimension(:), pointer :: all_iH => null(), all_jH => null()
  real(kind=8)   ,dimension(:), pointer :: gu     => null(), all_rhs_free => null(), b => null()
  integer(kind=4),dimension(:)  , allocatable :: cc_gno_zero, cc_gdofs, cc_adj_dof
  integer(kind=4),dimension(:,:), allocatable :: adj_dof

  ! parameter of global sovler
  real(kind=8)   ,dimension(:), pointer :: dparam => null()
  integer(kind=4),dimension(:), pointer :: iparam => null()

                                          !123456789012345678901234567890
  character(len=30) :: solver_name      = 'nlgs                          '
  real(kind=8)      :: solver_tolerance = 1e-6
  integer           :: solver_itermax   = 1000
  integer           :: solver_ritermax  = 1
  integer           :: solver_verbosity = 0
  integer           :: solver_output    = 0
  integer           :: solver_freq_output    = 1
  integer           :: solver_ndof    = 0
 
  INTEGER, PRIVATE                        :: nb_CDAN=0,nb_ENTITY

  TYPE (T_ctct_element),DIMENSION(:),ALLOCATABLE  :: this  
 
  integer           :: nnoctc,nnoact,nactif,ncompr,ntract,nslide,nstick,noksta

  INTEGER,PARAMETER :: i_iter = 1 , i_check = 2 , i_post = 3
  LOGICAL      :: is_initialized=.FALSE.
  LOGICAL      :: is_global

  real(kind=8) :: relax=1.d0,relax1=0.d0

  interface
     subroutine c_fc3d_LmgcDriver(p_z,p_u,p_q,p_mu,p_w,p_la,p_lb, &
                                  nc,nb,ids,stol,itm,verbose,output,freq_output,ndof) &
                                  bind(C, name="fc3d_LmgcDriver")
       import C_PTR, C_INT, C_DOUBLE
       type(c_ptr),value :: p_w,p_q,p_u,p_z,p_mu,p_la,p_lb  
       real(c_double),value :: stol
       integer(c_int),value :: nc,nb,itm,ids,verbose,output,freq_output,ndof

    end subroutine c_fc3d_LmgcDriver
  end interface

  interface
    subroutine c_gfc3d_LmgcDriver(p_z,p_u,p_gu,p_rf,p_b,p_mu         ,&
                                  p_M,nzM,p_iM,p_jM,p_H,nzH,p_iH,p_jH,&
                                  n,nc,ids,ios,iparam,dos,dparam,verbose,output,freq_output) &
                                  bind(C, name="gfc3d_LmgcDriver")
       import C_PTR, C_INT, C_DOUBLE
       type(c_ptr),value :: p_z,p_u,p_gu,p_rf,p_b,p_mu,p_M,p_iM,p_jM,p_H,p_iH,p_jH,iparam,dparam
       integer(c_int),value :: nzM,nzH,n,nc,ids,verbose,output,freq_output,ios,dos

    end subroutine c_gfc3d_LmgcDriver
  end interface

  PUBLIC &
       prep, &
       solve, &
       RnodHRloc, &
       update_tact_behav, &
       set_parameter, &
       Nullify_EntityList, & 
       assume_is_initialized, &
       compute_local_free_vlocy

  PUBLIC  &
       get_contact_status, &
       get_network_change

  private prjj_, injj_  , &
          vitrad_       , &
          nullify_reac_ , &
          nullify_vlocy_

CONTAINS

!!!------------------------------------------------------------------------
  subroutine prep()
    implicit none

    if (is_global) then
      call prep_global()
    else
      call prep_local()
    end if

  end subroutine
!!!------------------------------------------------------------------------
  SUBROUTINE prep_local()
 
    IMPLICIT NONE
    
    ! common part

                              !123456789012345678901234567
    CHARACTER(len=27)  :: IAM='SiconosNumerics::prep_local'
    INTEGER            :: errare
    
    INTEGER            :: ik,ibehav,icdan,ient,itact
    REAL(kind=8)       :: vsik,vtik,vnik,rsik,rtik,rnik

    ! behaviours part

    REAL(kind=8)       :: fric,tangalrest,normalrest

   ! Wab variables
    
    INTEGER            :: jl,iadj,jadj,ikadj,jladj,jlstart
    INTEGER            :: nbadj,icdik,ianik,nb_blocks,icdent,ianent,istart
    LOGICAL            :: is_present=.FALSE., ok=.FALSE.

    integer            :: i_block
    
    integer(kind=4)    :: nb_body, i
    
    ! on remonte tout dans le solveur
    call gather_interactions()
    
    !------------------
    ! Rnod = [H] Rloc
    !------------------
    
    DO ik=1,nb_CDAN  
       CALL nullify_reac_(ik,iIreac)
       CALL nullify_vlocy_(ik,iVaux_)
    END DO
    
    DO ik=1,nb_CDAN
       CALL injj_(ik,this(ik)%rls,this(ik)%rlt,this(ik)%rln,iIreac)
    END DO
    
    ! computing local dynamic matrix W (Delassus matrix) 
    ! with its diagonal terms

    ! sizing
    nb_blocks = 0
    istart   = 0
 
    DO ik=1,nb_CDAN
          
      ! surdimensionnement du nb d adjacent dont lui meme
      nbadj = 0

      icdik = this(ik)%icdent
      ianik = this(ik)%ianent
          
      ! computation of the nb_adj contacts of ik; 
      ! it is equal to the number of active contact for icdbdy and ianbdy
      ! minus the contact ik for each of them

      IF (icdik == ianik) THEN
        ! special case of self contact          
        nbadj = entity(icdik)%nb
      ELSE
        ! -1 car on ne veut pas compter ikik 2 fois
        nbadj = entity(icdik)%nb + entity(ianik)%nb - 1
      ENDIF

      jl = 0
          
      IF (nbadj /= 0) THEN

        IF (ASSOCIATED(this(ik)%adjjl)) DEALLOCATE(this(ik)%adjjl)
        ALLOCATE(this(ik)%adjjl(nbadj),stat=errare)
        IF (errare /= 0) THEN
          CALL FATERR(IAM,'error allocating this(ik)%adjjl')
        END IF
        this(ik)%adjjl = 0
             
        DO iadj=1,entity(icdik)%nb
          jl = jl+1
          this(ik)%adjjl(jl) = entity(icdik)%list(iadj)
        END DO
             
        ! si pas auto contact
        IF (icdik /= ianik) THEN
          DO iadj=1,entity(ianik)%nb
            ! on ne compte pas 2 fois le contact ik
            IF (entity(ianik)%list(iadj) == ik) CYCLE 

            ! on ne veut pas recompter les contacts deja dans la liste
            is_present = .FALSE.
            DO jadj=1,entity(icdik)%nb
              IF (this(ik)%adjjl(jadj) /= entity(ianik)%list(iadj)) CYCLE
              is_present = .TRUE.
              EXIT
            END DO
            IF (is_present) CYCLE

            jl = jl+1
            this(ik)%adjjl(jl) = entity(ianik)%list(iadj)
          END DO
        END IF
          

        this(ik)%istart = istart
        istart = istart + 9*jl
          
        this(ik)%nbadj = jl
        nb_blocks = nb_blocks + jl
          
        !print*,'contact ',ik, 'nb adj', this(ik)%nbadj, 'liste:'
        !print*,this(ik)%adjjl(1:jl)
        !print*,this(ik)%istart

      endif   

    END DO

       
    if (associated(l_a)) deallocate(l_a)
    allocate(l_a(nb_blocks))

    if (associated(l_b)) deallocate(l_b)
    allocate(l_b(nb_blocks))


    IF (AssOCiATED(Wab)) DEALLOCATE(Wab)
    ALLOCATE(Wab(9*nb_blocks),stat=errare)
    IF (errare /= 0) THEN
      CALL FATERR(IAM,'error allocating Wab')
    END IF
    Wab = 0.D0
    

    IF (AssOCiATED(q)) DEALLOCATE(q)
    ALLOCATE(q(3*nb_CDAN),stat=errare)
    q=0.d0

    IF (AssOCiATED(z)) DEALLOCATE(z)
    ALLOCATE(z(3*nb_CDAN),stat=errare)
    z=0.d0

    IF (AssOCiATED(u)) DEALLOCATE(u)
    ALLOCATE(u(3*nb_CDAN),stat=errare)
    u=0.d0

    IF (AssOCiATED(mu)) DEALLOCATE(mu)
    ALLOCATE(mu(nb_CDAN),stat=errare)
    mu=0.d0

    i_block=0

    DO ik=1,nb_CDAN
       
       ! computing local dynamic matrix W
       
       rnik=1.D0
       rtik=0.D0
       rsik=0.D0
       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)
       
       ! on passe les adjacents
       DO ikadj=1,this(ik)%nbadj
             
         jl = this(ik)%adjjl(ikadj)
         jlstart = this(jl)%istart
             
         CALL prjj_(jl,vsik,vtik,vnik,iVaux_)
             
         ok = .FALSE.
             
         DO jladj=1,this(jl)%nbadj
                
           IF (this(jl)%adjjl(jladj) == ik) THEN
                   
             Wab(jlstart + 9*jladj-8) = vnik ! Wnn
             Wab(jlstart + 9*jladj-7) = vtik ! Wtn
             Wab(jlstart + 9*jladj-6) = vsik ! Wsn
                   
             i_block = i_block + 1
             l_a(i_block) = ik
             l_b(i_block) = jl

             ok = .TRUE.
           END IF
                
         END DO
             
         IF (.NOT. ok) THEN
           CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         END IF
             
       END DO
          
       CALL nullify_vlocy_(ik,iVaux_)
       
       rnik=0.D0
       rtik=1.D0
       rsik=0.D0

       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)

       DO ikadj=1,this(ik)%nbadj
             
         jl = this(ik)%adjjl(ikadj)
         jlstart = this(jl)%istart
             
         CALL prjj_(jl,vsik,vtik,vnik,iVaux_)
             
         ok = .FALSE.
             
         DO jladj=1,this(jl)%nbadj
                
           IF (this(jl)%adjjl(jladj) == ik) THEN
                   
              Wab(jlstart + 9*jladj-5) = vnik ! Wnt
              Wab(jlstart + 9*jladj-4) = vtik ! Wtt
              Wab(jlstart + 9*jladj-3) = vsik ! Wts
                   
              ok = .TRUE.
           END IF
                
         END DO
             
         IF (.NOT. ok) THEN
           CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         END IF
             
       END DO
          
       CALL nullify_vlocy_(ik,iVaux_)
          
       
       rnik=0.D0
       rtik=0.D0
       rsik=1.D0

       CALL nullify_reac_(ik,iIaux_)
       CALL injj_(ik,rsik,rtik,rnik,iIaux_)
       CALL vitrad_(ik,iVaux_e_invM_t_Iaux_)
       CALL prjj_(ik,vsik,vtik,vnik,iVaux_)
          
       DO ikadj=1,this(ik)%nbadj
             
         jl = this(ik)%adjjl(ikadj)
         jlstart = this(jl)%istart
             
         CALL prjj_(jl,vsik,vtik,vnik,iVaux_)
             
         ok = .FALSE.
             
         DO jladj=1,this(jl)%nbadj
                
           IF (this(jl)%adjjl(jladj) == ik) THEN
                
             Wab(jlstart + 9*jladj-2) = vnik ! Wns
             Wab(jlstart + 9*jladj-1) = vtik ! Wts
             Wab(jlstart + 9*jladj  ) = vsik ! Wss
                   
             ok = .TRUE.
           END IF
                
         END DO
             
         IF (.NOT. ok) THEN
           CALL FATERR(IAM,'ERROR: unable to find the reverse adjacent !!')
         END IF
             
       END DO
          
       CALL nullify_vlocy_(ik,iVaux_)

       ibehav=this(ik)%lawnb

!!!-----------------------------------------------
!!! Preparing auxiliaries for contact ik.
!!!-----------------------------------------------
       
       this(ik)%fric = get_fric(ibehav,this(ik)%statusBEGIN)

       mu(ik) = this(ik)%fric

       

!!! -- changement de variables pour les lois    

       SELECT CASE(tact_behav(ibehav)%ilaw)
          
            !123456789012345678901234567890
       CASE(i_IQS_CLB)
          this(ik)%i_law = i_IQS_CLB
          this(ik)%covfreen=MAX(0.D0,this(ik)%gapTTbegin/H)

!!!----------------------------------------
            !123456789012345678901234567890
       CASE(i_IQS_CLB_g0)
          this(ik)%i_law = i_IQS_CLB_g0

          IF (.NOT. is_initialized) THEN
            !fd on stoque le max(0.,g0) 
            this(ik)%internal(1) = max(0.d0,this(ik)%gapTTbegin)       
          END IF
          this(ik)%covfreen=MAX(0.D0,(this(ik)%gapTTbegin-this(ik)%internal(1))/H)

!!!----------------------------------------
       CASE(i_GAP_SGR_CLB)
          this(ik)%i_law = i_GAP_SGR_CLB
          this(ik)%covfreen=this(ik)%gapTTbegin/H

!!!----------------------------------------
       CASE(i_GAP_SGR_CLB_g0)
          this(ik)%i_law = i_GAP_SGR_CLB_g0
          this(ik)%covfreen=this(ik)%gapTTbegin/H

          IF (.NOT. is_initialized) THEN

            !am & pta on stoque le max(0.,g0) 
            this(ik)%internal(2) = max(0.d0,this(ik)%gapTTbegin)       

          END IF

          this(ik)%covfreen=(this(ik)%gapTTbegin-this(ik)%internal(2))/H

!!!----------------------------------------
       CASE(i_RST_CLB)
          this(ik)%i_law = i_RST_CLB
          
          IF (this(ik)%gapTTbegin .LE. 0.D0) THEN
             !!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 
             ! this(ik)%forecast='acton' !(default is 'acton')
             CALL get_rst(ibehav,tangalrest,normalrest)
             this(ik)%covfrees=tangalrest*this(ik)%vlsBEGIN
             this(ik)%covfreet=tangalrest*this(ik)%vltBEGIN
             this(ik)%covfreen=normalrest*this(ik)%vlnBEGIN
          ELSE
             this(ik)%forecast=0
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status=i_noctc
             normalrest = 0.D0
             tangalrest = 0.D0
             this(ik)%covfreen = 0.D0
          END IF
!!!----------------------------------------
       CASE(i_VEL_SGR_CLB)
          this(ik)%i_law = i_VEL_SGR_CLB

          IF (this(ik)%gapTTbegin .LE. 0.D0) THEN 
             !!! It is relevant to use this(ik)%gapTTbegin as a forecast criterium. 
             ! this(ik)%forecast='acton' (default is 'acton')
          ELSE
             this(ik)%forecast=0
             this(ik)%rls = 0.D0
             this(ik)%rlt = 0.D0
             this(ik)%rln = 0.D0
             this(ik)%status=i_noctc
          END IF

       CASE default
          CALL LOGMES('ERROR: contact law not available for siconos local solver')
          CALL LOGMES(tact_behav(ibehav)%lawty)
          stop
       END SELECT
       
       !-----------------------------------------------
       ! Computing free local vlocy
       !-----------------------------------------------
       
       CALL prjj_(ik,this(ik)%vfrees,this(ik)%vfreet,this(ik)%vfreen,iVfree)

       q(3*(ik - 1) + 1 : 3*ik) = (/ this(ik)%vfreen + this(ik)%covfreen ,&
                                     this(ik)%vfreet + this(ik)%covfreet ,&
                                     this(ik)%vfrees + this(ik)%covfrees /)

    END DO


    solver_ndof    = 0
    nb_body = get_nb_mecaMAILx()
    do i = 1, nb_body
       solver_ndof    =  solver_ndof    + get_nb_dofs_mecaMAILx(i)
    end do
    !print*,'solver_ndof =============================',solver_ndof
    nb_body = get_nb_RBDY3()
    !print*,'nb_body =============================',nb_body
    do i = 1, nb_body
       solver_ndof    =  solver_ndof    + 6
    end do
    !print*,'solver_ndof =============================',solver_ndof


    
    is_initialized = .TRUE.

!!$    do i_block = 1, nb_blocks
!!$
!!$      write(*,*) i_block,l_a(i_block),l_b(i_block)
!!$      write(*,'(3(1x,D12.5))') wab(9*(i_block - 1) + 1:9*i_block) 
!!$
!!$    enddo
!!$
!!$    do ik = 1, nb_CDAN
!!$
!!$      write(*,*) ik 
!!$      write(*,'(3(1x,D12.5))') q(3*(ik - 1) + 1 : 3*ik) 
!!$
!!$    enddo


  END SUBROUTINE prep_local
!!!------------------------------------------------------------------------
  SUBROUTINE prep_global()
 
    IMPLICIT NONE
    
    ! common part
                              !1234567890123456789012345678
    CHARACTER(len=28)  :: IAM='SiconosNumerics::prep_global'
 
    integer :: errare

    integer(kind=4) :: ik, iki, ibehav !,icdan,ient,itact
    integer(kind=4) :: glob_i1, glob_i2, glob_i3, glob_j
    integer(kind=4) :: nb_mecax, i, i_dof, nb_no_zero, idx

    ! 30 is more than DIME*4 + DIME*4 which is the theoretical current limit
    integer(kind=4) :: nb_max_adj_dof = 30

    if( .not. associated(iparam) ) then
      allocate(iparam(20))
    end if
    iparam = 0
   
    if( .not. associated(dparam) ) then
      allocate(dparam(20))
    end if
    dparam = 0.d0
    
    ! on remonte les infos des interactions dans le solveur

    call gather_interactions()

    ! recuperation de la matrice de masse globale
    nb_mecax = get_nb_mecaMAILx()
    if( .not. is_initialized ) then
      allocate(cc_gno_zero(nb_mecax+1))
      allocate(cc_gdofs(nb_mecax+1))
      cc_gno_zero = 0
      cc_gdofs    = 0
      do i = 1, nb_mecax
        !print*,i,'nz',get_nb_nz_g_sys_mecaMAILx(i),' nbdof ',get_nb_dofs_mecaMAILx(i)
        cc_gno_zero(i+1) = cc_gno_zero(i) + get_nb_nz_g_sys_mecaMAILx(i)
        cc_gdofs(i+1)    = cc_gdofs(i)    + get_nb_dofs_mecaMAILx(i)
      end do

      allocate(all_M(cc_gno_zero(nb_mecax+1)))
      allocate(all_iM(cc_gno_zero(nb_mecax+1)))
      allocate(all_jM(cc_gno_zero(nb_mecax+1)))

      allocate(all_rhs_free(cc_gdofs(nb_mecax+1)))
      allocate(gu(cc_gdofs(nb_mecax+1)))

      do i = 1, nb_mecax
        !print *,'getting indices of mecax : ', i
        call get_i_indices_mecaMAILx(i,all_iM(cc_gno_zero(i)+1:cc_gno_zero(i+1)))
        call get_j_indices_mecaMAILx(i,all_jM(cc_gno_zero(i)+1:cc_gno_zero(i+1)))
        !print *,'all iM indices boundaries : ', cc_gno_zero(i)+1,cc_gno_zero(i+1)
        !print *,'shift to add : ', cc_gdofs(i)
        all_iM(cc_gno_zero(i)+1:cc_gno_zero(i+1)) = cc_gdofs(i) + all_iM(cc_gno_zero(i)+1:cc_gno_zero(i+1))
        all_jM(cc_gno_zero(i)+1:cc_gno_zero(i+1)) = cc_gdofs(i) + all_jM(cc_gno_zero(i)+1:cc_gno_zero(i+1))
      end do
    end if

    do i = 1, nb_mecax
      ! gu is an output of the solver, but it is initialized to help it
      call get_vector_mecaMAILx('V____',i,gu(cc_gdofs(i)+1:cc_gdofs(i+1)),cc_gdofs(i+1)-cc_gdofs(i))
      ! getting matrice
      call get_val_g_sys_mecaMAILx(i,all_M(cc_gno_zero(i)+1:cc_gno_zero(i+1)))
      ! getting rhs => since incremental resolution is used with LMGC90, rhs = rhs + M*V
      call get_rhs_vector_mecaMAILx(i,all_rhs_free(cc_gdofs(i)+1:cc_gdofs(i+1)))
    end do

    !=================================
    if (associated(b)) deallocate(b)
    allocate(b(3*nb_CDAN))
    b = 0.d0
    
    if (allocated(adj_dof)) then
      if (size(adj_dof,1) /= 3*nb_CDAN) deallocate(adj_dof)
    end if
    if (.not. allocated(adj_dof)) allocate(adj_dof(3*nb_CDAN,nb_max_adj_dof))
    adj_dof    = 0
    

    if (allocated(cc_adj_dof)) then
      if (size(cc_adj_dof) /= 3*nb_CDAN+1) deallocate(cc_adj_dof)
    end if
    if (.not. allocated(cc_adj_dof)) allocate(cc_adj_dof(3*nb_CDAN+1))
    cc_adj_dof = 0

    do ik = 1, nb_CDAN

      ! on parcourt les ddl de l'interaction

      glob_i1 = 3*ik-2
      glob_i2 = 3*ik-1
      glob_i3 = 3*ik  


      ! on parcourt les ddl du cd
      do i_dof = 1, size(this(ik)%cd_dof)
        glob_j = cc_gdofs(this(ik)%icdent) + this(ik)%cd_dof(i_dof)

        cc_adj_dof(glob_i1+1)   = cc_adj_dof(glob_i1+1) + 1
        adj_dof(glob_i1,cc_adj_dof(glob_i1+1)) = glob_j

        cc_adj_dof(glob_i2+1)   = cc_adj_dof(glob_i2+1) + 1
        adj_dof(glob_i2,cc_adj_dof(glob_i2+1)) = glob_j

        cc_adj_dof(glob_i3+1)   = cc_adj_dof(glob_i3+1) + 1
        adj_dof(glob_i3,cc_adj_dof(glob_i3+1)) = glob_j
      end do

      ! on parcourt les ddl du an
      do i_dof = 1, size(this(ik)%an_dof)
        glob_j = cc_gdofs(this(ik)%ianent) + this(ik)%an_dof(i_dof)

        cc_adj_dof(glob_i1+1) = cc_adj_dof(glob_i1+1) + 1
        adj_dof(glob_i1,cc_adj_dof(glob_i1+1)) = glob_j

        cc_adj_dof(glob_i2+1) = cc_adj_dof(glob_i2+1) + 1
        adj_dof(glob_i2,cc_adj_dof(glob_i2+1)) = glob_j

        cc_adj_dof(glob_i3+1) = cc_adj_dof(glob_i3+1) + 1
        adj_dof(glob_i3,cc_adj_dof(glob_i3+1)) = glob_j
      end do
    end do

    do iki = 1, 3*nb_CDAN
      cc_adj_dof(iki+1) = cc_adj_dof(iki+1) + cc_adj_dof(iki)
    end do

    nb_no_zero = cc_adj_dof(3*nb_CDAN+1)
    if (associated(all_H)) then
      if (nb_no_zero /= size(all_H)) then
        deallocate(all_H, all_iH, all_jH)
        nullify(all_H, all_iH, all_jH)
      end if
    end if

    if (.not. associated(all_H)) then
      allocate(all_H(nb_no_zero), all_iH(nb_no_zero), all_jH(nb_no_zero))
    end if
   
    all_H  = 0
    all_iH = 0
    all_jH = 0

    do ik = 1, nb_CDAN

      glob_i1 = 3*ik-2
      glob_i2 = 3*ik-1
      glob_i3 = 3*ik  

      do i_dof = 1, size(this(ik)%cd_dof)
        glob_j = cc_gdofs(this(ik)%icdent) + this(ik)%cd_dof(i_dof)

        idx = minloc( adj_dof(glob_i1,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i1,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i1)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i1
        all_H(idx)  = this(ik)%g2l(1,i_dof)

        idx = minloc( adj_dof(glob_i2,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i2,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i2)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i2
        all_H(idx)  = this(ik)%g2l(2,i_dof)

        idx = minloc( adj_dof(glob_i3,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i3,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i3)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i3
        all_H(idx)  = this(ik)%g2l(3,i_dof)
      end do

      do i_dof = 1, size(this(ik)%an_dof)
        glob_j = cc_gdofs(this(ik)%ianent) + this(ik)%an_dof(i_dof)

        idx = minloc( adj_dof(glob_i1,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i1,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i1)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i1
        all_H(idx)  = this(ik)%g2l(1,size(this(ik)%cd_dof)+i_dof)

        idx = minloc( adj_dof(glob_i2,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i2,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i2)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i2
        all_H(idx)  = this(ik)%g2l(2,size(this(ik)%cd_dof)+i_dof)

        idx = minloc( adj_dof(glob_i3,1:nb_max_adj_dof), dim=1, mask=(adj_dof(glob_i3,1:nb_max_adj_dof)>=glob_j) )
        idx = idx + cc_adj_dof(glob_i3)
        all_iH(idx) = glob_j
        all_jH(idx) = glob_i3
        all_H(idx)  = this(ik)%g2l(3,size(this(ik)%cd_dof)+i_dof)
      end do
    end do

    
    !------------------
    ! Rnod = [H] Rloc
    !------------------
    
    DO ik=1,nb_CDAN  

       CALL nullify_reac_(ik,iIreac)
       CALL nullify_vlocy_(ik,iVaux_)

    END DO
    
    IF (AssOCiATED(mu)) DEALLOCATE(mu)
    ALLOCATE(mu(nb_CDAN),stat=errare)
    mu=0.d0

    DO ik=1,nb_CDAN
       
       ibehav=this(ik)%lawnb

!!!-----------------------------------------------
!!! Preparing auxiliaries for contact ik.
!!!-----------------------------------------------
       
       mu(ik) = get_fric(ibehav,this(ik)%statusBEGIN)

!!! -- changement de variables pour les lois    

       SELECT CASE(tact_behav(ibehav)%ilaw)
          
!!!----------------------------------------
       CASE(i_VEL_SGR_CLB)
          this(ik)%i_law = i_VEL_SGR_CLB

       CASE default
          CALL LOGMES('ERROR: contact law not available for siconos global solver')
          CALL LOGMES(tact_behav(ibehav)%lawty)
          stop
       END SELECT
       
    END DO
 
    IF (AssOCiATED(z)) DEALLOCATE(z)
    ALLOCATE(z(3*nb_CDAN),stat=errare)
    z=0.d0

    IF (AssOCiATED(u)) DEALLOCATE(u)
    ALLOCATE(u(3*nb_CDAN),stat=errare)
    u=0.d0

    is_initialized = .TRUE.

  END SUBROUTINE prep_global
!!!------------------------------------------------------------------------  
  SUBROUTINE solve()

   IMPLICIT NONE
   

                            !1234567890123456789012
   CHARACTER(len=22) :: IAM='SiconosNumerics::solve'
   CHARACTER(len=80) :: cout
   INTEGER           :: ik,ibehav,ilaw
   CHARACTER(len=5)  :: sstatusik
   REAL(kind=8)      :: fricik,gapttik


   REAL(kind=8)      :: vlocfreetik,vlocfreenik,vlocfreesik
   REAL(kind=8)      :: vvlocfreesik,vvlocfreetik,vvlocfreenik


   REAL(kind=8)      :: vsik,  vtik,  vnik,   rsik,   rtik,   rnik
   REAL(kind=8)      :: vlsik, vltik, vlnik,  rlsik,  rltik,  rlnik
   REAL(kind=8)      :: vlsiki,vltiki,vlniki, rlsiki, rltiki, rlniki

   INTEGER           :: istart,iistart,ikjl,iadj

   integer           :: nb_mecax,icdtac
   real(kind=8)      :: weight

   
   

   ! resolution u = w z + q 
   if ( is_global ) then
     call SiconosNumerics_global_solve()
     IF (nb_CDAN == 0) RETURN
   else
     IF (nb_CDAN == 0) RETURN   
     call SiconosNumerics_local_solve() 
   end if

   ! push the local velocity and reaction to lmgc90
   
   do ik=1,nb_cdan

     rlnik = this(ik)%rln
     rltik = this(ik)%rlt
     rlsik = this(ik)%rls

     ! !fd attempt to correct tns <-> nts
     ! if (is_global) then
     !   rlniki = z(3*(ik - 1) + 1) 
     !   rltiki = z(3*(ik - 1) + 2) 
     !   rlsiki =-z(3*(ik - 1) + 3) 

     !   vlniki = u(3*(ik - 1) + 1) 
     !   vltiki = u(3*(ik - 1) + 2) 
     !   vlsiki =-u(3*(ik - 1) + 3)
     ! else     
       rlniki = z(3*(ik - 1) + 1) 
       rltiki = z(3*(ik - 1) + 2) 
       rlsiki = z(3*(ik - 1) + 3) 

       vlniki = u(3*(ik - 1) + 1) 
       vltiki = u(3*(ik - 1) + 3) 
       vlsiki = u(3*(ik - 1) + 3) 
     ! endif
    
     this(ik)%rln=RELAX*rlniki+RELAX1*rlnik
     this(ik)%rlt=RELAX*rltiki+RELAX1*rltik
     this(ik)%rls=RELAX*rlsiki+RELAX1*rlsik

     IF (DABS(this(ik)%rls) .LT. 1.D-24) this(ik)%rls=0.D0
     IF (DABS(this(ik)%rlt) .LT. 1.D-24) this(ik)%rlt=0.D0
     IF (DABS(this(ik)%rln) .LT. 1.D-24) this(ik)%rln=0.D0

     ! TODO gestion du statut
     !this(ik)%status='sicon'
     this(ik)%status=i_nknow     
         
     gapTTik=this(ik)%gapTTbegin+H*vlniki
     IF (DABS(gapTTik) .LT. 1.D-24) gapTTik=0.D0

     if (is_global) then
       call CSASx2CSxxx(this(ik)%icdan,icdtac)
       weight=get_weight_CSxxx(icdtac)
       weight = 1.d0/weight

       !print*,this(ik)%icdan,icdtac,weight
       
     else
       weight=1.d0
     endif   

     
     ! Sending local data   
     call set_loc( this(ik)%CDAN      , this(ik)%icdan     ,this(ik)%status     ,  &
                   vltiki             , vlniki             , vlsiki             ,  &
                   weight*this(ik)%rlt, weight*this(ik)%rln, weight*this(ik)%rls,  &
                   gapTTik)

     ! TODO statistique des statuts

     Nactif=Nactif+1
     IF (this(ik)%rln > 0.D0) Ncompr=Ncompr+1
     IF (this(ik)%rln < 0.D0) Ntract=Ntract+1
         
   END DO

   ! if ( is_global ) then
   !   i_b=0
   !   do i=1,get_nb_mecaMAILx()
   !     nbdof =  get_nb_dofs_mecaMAILx(i) 
   !     i_f = i_b + nbdof 
   !     call put_vector_mecaMAILx('V____',i,gu(i_b+1:i_f),nbdof)
   !     i_b = i_f
   !  enddo
   ! endif   
   
  END SUBROUTINE solve
!!!---------------------------------------------------------------
  SUBROUTINE RnodHRloc

    IMPLICIT NONE
    INTEGER :: ik

    IF (nb_CDAN == 0) RETURN

    DO ik=1,nb_CDAN  
       CALL nullify_reac_(ik,iIreac)
    END DO

    DO ik=1,nb_CDAN
       CALL injj_(ik,this(ik)%rls,this(ik)%rlt,this(ik)%rln,iIreac)
    END DO

  END SUBROUTINE RnodHRloc
!!!---------------------------------------------------------------
  SUBROUTINE set_parameter(name,tolerance,itererror,itermax,relaxation,verbose,output,freq_output)

    CHARACTER(len=30) :: name
    REAL(kind=8)      :: relaxation,tolerance
    integer           :: itermax,itererror,verbose,output,freq_output

 
    select case(name)
    case('nlgs','localac')
      !nlgs local Gauss-Seidel
      !localac local Alart-Curnier 
      is_global = .false.
    case('nsgs','nlgs_wr','nlgsv_wr','prox_wr','ds_fp_wr','tfp_wr',&
         'localac_wr','globalac','globaladmm','vi_eg','vi_fpp')
       !nsgs nlgs global
       !nlgs_wr reformulation en nlgs local
       !nlgsv_wr reformulation en nlgs local en iterant sur les vitesses
       !prox_wr reformulation en point proximal
       !ds_fp_wr reformulation en deSaxe avec point fixe
       !tfp_wr reformulation en tresca avec point fixe
       !localac_wr reformulation en local Alart-Curnier
       
      is_global = .true.
    case default
      call faterr('SiconosNumerics::set_parameter','Unknown solver: '//name)
    end select

    solver_name = name
    solver_tolerance   = tolerance
    solver_ritermax = itererror
    solver_itermax  = itermax
    solver_verbosity=verbose 
    solver_output=output
    solver_freq_output=freq_output
    !
    RELAX = relaxation
    RELAX1= 1.D0-RELAX


  END SUBROUTINE set_parameter
!!!---------------------------------------------------------------
  SUBROUTINE prjj_(ik,vsik,vtik,vnik,storage)

   IMPLICIT NONE
   
   INTEGER                  :: ik
   REAL(kind=8),INTENT(out) :: vsik,vtik,vnik   
   INTEGER                  :: storage

  call prjj( this(ik)%CDAN, this(ik)%icdan, vtik, vnik, vsik, storage )
  
  END SUBROUTINE prjj_
!!!---------------------------------------------------------------
  SUBROUTINE injj_(ik,rsik,rtik,rnik,storage)

   IMPLICIT NONE

   INTEGER                 :: ik
   REAL(kind=8),INTENT(in) :: rsik,rtik,rnik
   INTEGER                 :: storage  
  
   call injj( this(ik)%CDAN, this(ik)%icdan, rtik, rnik, rsik, storage)

  END SUBROUTINE injj_
!!!--------------------------------------------------------------- 
  SUBROUTINE nullify_reac_(ik,storage)

   IMPLICIT NONE

   INTEGER           :: ik
   INTEGER           :: storage

   call nullify_reac( this(ik)%CDAN, this(ik)%icdan, storage )
  
  END SUBROUTINE nullify_reac_
!!!---------------------------------------------------------------
  SUBROUTINE vitrad_(ik,storage)
  !
  !computing velocity of adajacent ctct_elements
  !

   INTEGER           :: ik
   INTEGER           :: storage

   logical :: need_full_V
   
   !fd to say if all terms of a deformable body velocity are mandatory, default is no
   need_full_V = .false.
   if (storage == iVaux_e_invM_t_Iaux_) need_full_V = .true.

   call vitrad( this(ik)%CDAN, this(ik)%icdan, storage, need_full_V )
   
  END SUBROUTINE vitrad_
!!!--------------------------------------------------------------- 
  SUBROUTINE nullify_vlocy_(ik,storage)
   
   IMPLICIT NONE
   
   !                         12345678901234567890123
   CHARACTER(len=23) :: IAM='nlgs_3D::nullify_vlocy'
   CHARACTER(len=80) :: cout
   
   INTEGER :: ik,storage

   call nullify_vlocy( this(ik)%CDAN, this(ik)%icdan, storage )
   
  END SUBROUTINE nullify_vlocy_
!!!------------------------------------------------------------------------  
  SUBROUTINE Nullify_EntityList
    
    IMPLICIT NONE
    
    CALL Free_EntityList
    
  END SUBROUTINE Nullify_EntityList
!!!---------------------------------------------------------------
  SUBROUTINE get_contact_status(noctc,stick,slide)

    IMPLICIT NONE
    
    INTEGER :: noctc,stick,slide
    
    noctc = Nnoctc
    stick = Nstick
    slide = Nslide
    
  END SUBROUTINE get_contact_status
!!!------------------------------------------------------------------------  
  SUBROUTINE get_network_change(nctc,nweak,nstrong)

    IMPLICIT NONE
    INTEGER :: ik,nctc,nweak,nstrong
    
    nctc    = 0
    nweak   = 0
    nstrong = 0
    
    DO ik = 1,nb_CDAN
       IF(this(ik)%status == this(ik)%statusBEGIN) CYCLE
       nctc = nctc + 1
    ENDDO
    
  END SUBROUTINE get_network_change
!!!---------------------------------------------------------------------
  SUBROUTINE update_tact_behav

    IMPLICIT NONE

    INTEGER :: ik
       
    IF (nb_CDAN == 0) RETURN
    
    DO ik=1,nb_CDAN
       CALL update_internal_(ik)
    END DO

  END SUBROUTINE update_tact_behav
!!!------------------------------------------------------------------------  
  SUBROUTINE update_internal_(ik)

    IMPLICIT NONE

    INTEGER :: ik

    CALL set_internal(this(ik)%CDAN, this( ik )%icdan, this( ik )%internal )

    
  END SUBROUTINE update_internal_
!!!------------------------------------------------------------------------  
  SUBROUTINE assume_is_initialized()

   IMPLICIT NONE
 
   is_initialized=.TRUE.

  END SUBROUTINE
!!!------------------------------------------------------------------------  
  subroutine compute_local_free_vlocy
   implicit none

   integer :: ik

   do ik=1, nb_CDAN
      CALL prjj_(ik, this(ik)%vfrees, this(ik)%vfreet, this(ik)%vfreen, iVfree)
   end do

  end subroutine compute_local_free_vlocy
!!!------------------------------------------------------------------------  
  subroutine gather_interactions()
   implicit none

   ! contactors part 

   INTEGER            :: nb_SPSPx, nb_SPPLx, nb_SPCDx
   INTEGER            :: nb_PRPLx, nb_PRPRx, nb_PTPT3
   INTEGER            :: nb_SPDCx
   INTEGER            :: nb_CSPRx,nb_CSASx
   INTEGER            :: nb_CDCDx,nb_CDPLx

   integer            :: errare,ient,icdan,icdent,ianent,ik
   CHARACTER(len=18)  :: IAM='SiconosNumerics::gather_interactions'
   CHARACTER(len=120) :: cout


   nb_CDAN=0

   nb_SPSPx = get_nb_inters(i_SPSPx)
   nb_CDAN = nb_CDAN + nb_SPSPx

   nb_SPPLx = get_nb_inters(i_SPPLx)
   nb_CDAN = nb_CDAN + nb_SPPLx

   nb_SPCDx = get_nb_inters(i_SPCDx)
   nb_CDAN = nb_CDAN + nb_SPCDx

   nb_SPDCx = get_nb_inters(i_SPDCx)
   nb_CDAN = nb_CDAN + nb_SPDCx

   nb_PRPLx = get_nb_inters(i_PRPLx)
   nb_CDAN = nb_CDAN + nb_PRPLx

   nb_PRPRx = get_nb_inters(i_PRPRx)
   nb_CDAN = nb_CDAN + nb_PRPRx
    
   nb_PTPT3 = get_nb_inters(i_PTPT3)
   nb_CDAN = nb_CDAN + nb_PTPT3
    
   nb_CSPRx = get_nb_inters(i_CSPRx)
   nb_CDAN = nb_CDAN + nb_CSPRx

   nb_CSASx = get_nb_inters(i_CSASp)
   nb_CDAN = nb_CDAN + nb_CSASx

   nb_CDCDx = get_nb_inters(i_CDCDx)
   nb_CDAN = nb_CDAN + nb_CDCDx

   nb_CDPLx = get_nb_inters(i_CDPLx)
   nb_CDAN = nb_CDAN + nb_CDPLx

   IF (nb_CDAN == 0) RETURN 

   IF (ALLOCATED(this)) THEN
     DO ik=1,SIZE(this)
       IF(ASSOCIATED(this(ik)%adjjl)) DEALLOCATE(this(ik)%adjjl)
       if(ASSOCIATED(this(ik)%cd_dof)) DEALLOCATE(this(ik)%cd_dof)
       if(ASSOCIATED(this(ik)%an_dof)) DEALLOCATE(this(ik)%an_dof)
       if(ASSOCIATED(this(ik)%g2l)) DEALLOCATE(this(ik)%g2l)
     END DO
     DEALLOCATE(this)
   END IF

   ALLOCATE(this(nb_CDAN),stat=errare)
   IF (errare /= 0) THEN
       CALL FATERR(IAM,'error allocating this')
   END IF

   

   nb_ENTITY = get_nb_ENTITY()
   call Create_EntityList

!!!
   DO ik=1,nb_CDAN
       !
       this(ik)%rls        = 0.D0
       this(ik)%rlt        = 0.D0
       this(ik)%rln        = 0.D0
       this(ik)%vls        = 0.D0
       this(ik)%vlt        = 0.D0
       this(ik)%vln        = 0.D0
       this(ik)%corls      = 0.D0
       this(ik)%corlt      = 0.D0
       this(ik)%corln      = 0.D0
       this(ik)%status     =i_nknow
       this(ik)%vlsBEGIN   = 0.D0
       this(ik)%vltBEGIN   = 0.D0
       this(ik)%vlnBEGIN   = 0.D0
       this(ik)%gapTTbegin = 0.D0
       this(ik)%gapREF     = 0.D0
       this(ik)%statusBEGIN=i_nknow

       this(ik)%vfrees     = 0.D0
       this(ik)%vfreet     = 0.D0
       this(ik)%vfreen     = 0.D0
       this(ik)%covfrees   = 0.D0
       this(ik)%covfreet   = 0.D0
       this(ik)%covfreen   = 0.D0     
       this(ik)%lawnb      = 0
       this(ik)%statuscheck=i_nknow
       this(ik)%forecast   =i_acton       ! default status
       this(ik)%fric       = 0.D0

       this(ik)%inoctc     = 0
       this(ik)%iskip      = 1

       this(ik)%icdent     = 0
       this(ik)%ianent     = 0

       this(ik)%istart     = 0
       this(ik)%nbadj      = 0
       NULLIFY(this(ik)%adjjl)
       this(ik)%internal   = 0.D0

       this(ik)%i_law      = 0

       nullify(this(ik)%cd_dof,this(ik)%an_dof,this(ik)%g2l) 

   END DO
   
    nb_CDAN=0

    DO icdan=1,nb_SPSPx
       ik = nb_CDAN + icdan
       this(ik)%CDAN  = i_SPSPx
       this(ik)%icdan = icdan    
       CALL get_rloc(i_SPSPx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_SPSPx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_SPSPx,icdan,this(ik)%internal)
       CALL inter2ENT(i_SPSPx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_SPSPx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_SPSPx
    
    DO icdan=1,nb_SPPLx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_SPPLx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_SPPLx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_SPPLx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_SPPLx,icdan,this(ik)%internal)
       CALL inter2ENT(i_SPPLx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_SPPLx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_SPPLx
    
    DO icdan=1,nb_SPCDx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_SPCDx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_SPCDx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_SPCDx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_SPCDx,icdan,this(ik)%internal)
       CALL inter2ENT(i_SPCDx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_SPCDx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_SPCDx

    DO icdan=1,nb_SPDCx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_SPDCx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_SPDCx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_SPDCx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_SPDCx,icdan,this(ik)%internal)
       CALL inter2ENT(i_SPDCx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_SPDCx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_SPDCx
    
    DO icdan=1,nb_PRPLx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_PRPLx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_PRPLx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_PRPLx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_PRPLx,icdan,this(ik)%internal)
       CALL inter2ENT(i_PRPLx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_PRPLx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_PRPLx
    
    DO icdan=1,nb_PRPRx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_PRPRx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_PRPRx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_PRPRx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_PRPRx,icdan,this(ik)%internal)
       CALL inter2ENT(i_PRPRx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_PRPRx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_PRPRx
    
    DO icdan=1,nb_PTPT3
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_PTPT3
       this(ik)%icdan=icdan    
       CALL get_rloc(i_PTPT3,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_PTPT3,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_PTPT3,icdan,this(ik)%internal)
       CALL inter2ENT(i_PTPT3,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_PTPT3,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_PTPT3

    DO icdan=1,nb_CSPRx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_CSPRx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_CSPRx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_CSPRx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_CSPRx, icdan,this(ik)%internal)
       CALL inter2ENT(i_CSPRx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_CSPRx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_CSPRx

    DO icdan=1,nb_CSASx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_CSASp
       this(ik)%icdan=icdan    

       CALL get_rloc(i_CSASp, icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_CSASp,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_CSASp,icdan,this(ik)%internal)
       CALL inter2ENT(i_CSASp,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_CSASp,icdan)

       ! on remonte la liste des ddl sur lesquels s appuient le contact
       if (associated(this(ik)%cd_dof)) deallocate(this(ik)%cd_dof); nullify(this(ik)%cd_dof) 
       if (associated(this(ik)%an_dof)) deallocate(this(ik)%an_dof); nullify(this(ik)%an_dof)  
       call get_dof_CSASx(icdan,this(ik)%cd_dof,this(ik)%an_dof)


       ! on remonte la map sparse entre ddl de contact et ddl aux noeuds
       if (associated(this(ik)%g2l)) deallocate(this(ik)%g2l); nullify(this(ik)%g2l)
       allocate(this(ik)%g2l(3,size(this(ik)%cd_dof)+size(this(ik)%an_dof)))
       call get_g2l_CSASx(icdan,this(ik)%g2l,this(ik)%cd_dof,this(ik)%an_dof)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_CSASx
    
    DO icdan=1,nb_CDCDx
       ik = nb_CDAN + icdan
       this(ik)%CDAN  = i_CDCDx
       this(ik)%icdan = icdan    
       CALL get_rloc(i_CDCDx,icdan,this(ik)%rlt,this(ik)%rln,this(ik)%rls,this(ik)%status)
       CALL get_vlocBEGIN(i_CDCDx,icdan,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%vlsBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_CDCDx,icdan,this(ik)%internal)
       CALL inter2ENT(i_CDCDx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_CDCDx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_CDCDx

    DO icdan=1,nb_CDPLx
       ik=nb_CDAN+icdan
       this(ik)%CDAN=i_CDPLx
       this(ik)%icdan=icdan    
       CALL get_rloc(i_CDPLx,icdan,this(ik)%rls,this(ik)%rlt,this(ik)%rln,this(ik)%status)
       CALL get_vlocBEGIN(i_CDPLx,icdan,this(ik)%vlsBEGIN,this(ik)%vltBEGIN,this(ik)%vlnBEGIN,this(ik)%gapTTbegin,this(ik)%statusBEGIN)
       CALL get_internal(i_CDPLx,icdan,this(ik)%internal)
       CALL inter2ENT(i_CDPLx,icdan,icdent,ianent)
       this(ik)%lawnb=get_tact_lawnb(i_CDPLx,icdan)

       this(ik)%icdent   = icdent
       this(ik)%ianent   = ianent
       entity(icdent)%ik = entity(icdent)%ik+1
       entity(ianent)%ik = entity(ianent)%ik+1
       entity(icdent)%list(entity(icdent)%ik) = ik
       entity(ianent)%list(entity(ianent)%ik) = ik
    END DO
    nb_CDAN = nb_CDAN + nb_CDPLx

    DO ient=1,nb_ENTITY
      IF (entity(ient)%ik /= entity(ient)%nb) THEN
        CALL LOGMES('Error '//IAM//': mismatch in the entity connectivity for')
        WRITE(cout,'(A7,I5,A4,I5,A4,I5)') 'entity ',ient,' ik= ',entity(ient)%ik,' nb= ',entity(ient)%nb
        CALL FATERR(IAM,cout)
      END IF
    END DO

  end subroutine gather_interactions
!!!------------------------------------------------------------------------  
  subroutine SiconosNumerics_local_solve()

   implicit none

   ! ***                     1234567890123456789012345678
   character(len=28) :: IAM='SiconosNumerics::local_solve'

   type(c_ptr) :: p_w,p_q,p_u,p_z,p_mu,p_la,p_lb  

   real(c_double) :: stol

   integer(c_int) :: nc,nb,itm,ids,output,freq_output,verbose,ndof

   if ( associated(wab) ) then
     p_w  = c_loc(wab(1))
   else
     call faterr(IAM,'w not associated')
   endif 

   if ( associated(q) ) then
     p_q  = c_loc(q(1))
   else
     call faterr(IAM,'q not associated')
   endif 

   if ( associated(u) ) then
     p_u  = c_loc(u(1))
   else
     call faterr(IAM,'u not associated')
   endif 

   if ( associated(z) ) then
     p_z  = c_loc(z(1))
   else
     call faterr(IAM,'z not associated')
   endif 

   if ( associated(mu) ) then
     p_mu  = c_loc(mu(1))
   else
     call faterr(IAM,'mu not associated')
   endif 

   if ( associated(l_a) ) then
     p_la  = c_loc(l_a(1))
   else
     call faterr(IAM,'l_a not associated')
   endif 

   if ( associated(l_b) ) then
     p_lb  = c_loc(l_b(1))
   else
     call faterr(IAM,'l_b not associated')
   endif 

   nc = nb_CDAN
   nb = size(l_a)
   stol = solver_tolerance
   itm=solver_itermax
   verbose=solver_verbosity
   output=solver_output
   freq_output=solver_freq_output
   ndof= solver_ndof
   
   select case(solver_name)
   case('nlgs')
     ids = 500
   case('localac')
     ids = 504
   case default
     call faterr(IAM,'unsupported solver')
   end select

   ! p_z Rloc 
   ! p_u Vloc
   ! p_q 
   ! p_mu coeff de frottement
   ! p_w delassus

   ! pb local
   ! p_u = p_q + p_w p_z
   ! condition de contact frottant
   ! normal: p_u >= 0 perp p_z >= 0
   ! tangent: Coulomb

   call c_fc3d_LmgcDriver(p_z,p_u,p_q,p_mu,p_w,p_la,p_lb,nc,nb,ids, &
                                       stol,itm,verbose,output,freq_output,ndof)

  end subroutine
!!!------------------------------------------------------------------------  
  subroutine SiconosNumerics_global_solve()
   implicit none
   ! ***                     12345678901234567890123456789
   character(len=27) :: IAM='SiconosNumerics::global_solve'

   type(c_ptr)    :: p_u,p_z,p_mu,p_rf,p_gu,p_b,p_M,p_iM,p_jM,p_H,p_iH,p_jH,p_ip,p_dp
   integer(c_int) :: nzM,nzH,n,nc,ids,ios,dos,output,freq_output,verbose

   ! real(kind=8) :: r(3),w(3,3)
   ! integer      :: i,j,k
   ! real(kind=8),allocatable :: colH(:), McolH(:)

  ! debug
   real(kind=8),allocatable :: r(:)
   integer                  :: i
   
   if ( associated(u) ) then
     p_u  = c_loc(u(1))
   else
     call faterr(IAM,'u not associated')
   endif 
   if ( associated(z) ) then
     p_z  = c_loc(z(1))
   else
     call faterr(IAM,'z not associated')
   endif 
   if ( associated(mu) ) then
     p_mu  = c_loc(mu(1))
   else
     call faterr(IAM,'mu not associated')
   endif 

   if ( associated(gu) ) then
     p_gu = c_loc(gu(1))
   else
     call faterr(IAM,'gu not associated')
   endif 

   if ( associated(b) ) then
     p_b  = c_loc(b(1))
   else
     call faterr(IAM,'b not associated')
   endif 

   if ( associated(all_rhs_free) ) then
     p_rf = c_loc(all_rhs_free(1))
   else
     call faterr(IAM,'all_rhs_free not associated')
   endif 

   if ( associated(all_M) ) then
     p_M  = c_loc(all_M(1))
   else
     call faterr(IAM,'all_M not associated')
   endif 
   if ( associated(all_iM) ) then
     p_iM = c_loc(all_iM(1))
   else
     call faterr(IAM,'all_iM not associated')
   endif 
   if ( associated(all_jM) ) then
     p_jM = c_loc(all_jM(1))
   else
     call faterr(IAM,'all_jM not associated')
   endif 

   if ( associated(all_H) ) then
     p_H  = c_loc(all_H(1))
   else
     call faterr(IAM,'all_H not associated')
   endif 
   if ( associated(all_iH) ) then
     p_iH = c_loc(all_iH(1))
   else
     call faterr(IAM,'all_iH not associated')
   endif 
   if ( associated(all_jH) ) then
     p_jH = c_loc(all_jH(1))
   else
     call faterr(IAM,'all_jH not associated')
   endif 

   if( .not. associated(iparam) ) then
     call faterr(IAM,'integer parameters array not allocated')
   else
     ios = size(iparam)
     iparam(1) = solver_itermax
     iparam(8) = solver_ritermax
     p_ip = c_loc(iparam(1))
   end if
   if( .not. associated(dparam) ) then
     call faterr(IAM,'real parameters array not allocated')
   else
     dos = size(dparam)
     dparam(1) = solver_tolerance
     dparam(9) = lmgc_mpi_world_comm
     p_dp = c_loc(dparam(1))
   end if

   !stol = solver_tolerance
   !itm=solver_itermax
   verbose=solver_verbosity
   output=solver_output
   freq_output=solver_freq_output
 
   select case(solver_name)
   case('nsgs')
     ids = 605
   case('nlgs_wr')
     ids = 600
   case('nlgsv_wr')
     ids = 601
   case('prox_wr')
     ids = 602
   case('ds_fp_wr')
     ids = 603
   case('tfp_wr')
     ids = 604
   case('localac_wr')
     ids = 606
   case('globalac')
     ids = 607
   case('globalac_nls')
     ids = 607
   case('vi_fpp')
     ids = 610
   case('vi_eg')
     ids = 611

     dparam(4) = -1.0   !; rho is variable by default
     dparam(5) = 2/3.0  !;  /* tau */
     dparam(6) = 3.0/2.0!;  /*tauinv */
     dparam(7) = 0.9    !;  /* L */
     dparam(8) = 0.3    !;  /* Lmin */
     
   case('globaladmm')
     ids = 613
     ! SICONOS_FRICTION_3D_IPARAM_RESCALING =3
     ! SICONOS_FRICTION_3D_RESCALING_YES=1     
     iparam(3) = 1 
   case default
     call faterr(IAM,'unsupported solver')
   end select
   
   n  = cc_gdofs(get_nb_mecaMAILx()+1)
   nc = nb_CDAN

   nzM = size(all_M)
   nzH = size(all_H)
   ! print *,'nzm : ', nzM
   ! print *,'nzh : ', nzH
   ! print *,'max iM', maxval(all_iM)
   ! print *,'max jM', maxval(all_jM)
   ! print *,'max iH', maxval(all_iH)
   ! print *,'max jH', maxval(all_jH)

   iparam(4) = nzM;
   iparam(8) = 1;

   ! ! ok
   ! r = 0.d0
   ! do i=1,nzH
   !   print*,i,all_iH(i),all_jH(i),all_H(i)
   !   r(all_jH(i)) = r(all_jH(i)) + all_H(i)*all_rhs_free(all_iH(i))
   ! enddo   

   ! print*,r

   ! ! idiot car il faudrait calculer M^-1
   ! w = 0.d0
   ! ! 48 = total_nb_dof
   ! allocate(colH(48))
   ! allocate(McolH(48))
   ! do j=1,3
   !   ! on remplie colH
   !   colH=0.d0
   !   do i=1,nzH
   !     if (all_jH(i) == j) colH(all_iH(i))=all_H(i)
   !   enddo
   !   !print*,colH
   !   ! on calcule colonne par colonne M colH
   !   McolH=0.d0              
   !   do k=1,48
   !     do i=1,nzM
   !       if (all_iM(i) == k) McolH(k) = McolH(k)+all_M(i)*colH(all_iM(i)) 
   !     enddo
   !   enddo
   !   !print*,McolH     
   !   ! on calcule produit H^T M colH 
   !   do i=1,nzH
   !     w(all_jH(i),j) = w(all_jH(i),j) + all_H(i)*McolH(all_iH(i))
   !   enddo   
   ! enddo
   ! print*,w
   ! deallocate(colH,McolH) 


   ! print*,'rfree ',all_rhs_free

   !print*,'M '
   !do i=1,nzM
   !   print*,i,all_iM(i),all_jM(i),all_M(i)
   !enddo
   !
   ! allocate(r(3*nc))
   ! r = 0.d0
   ! print*,'H '   
   ! do i=1,nzH
   !   print*,i,all_iH(i),all_jH(i),all_H(i)
   !   r(all_jH(i)) = r(all_jH(i)) + all_H(i)*all_rhs_free(all_iH(i))
   ! enddo   

   ! print*,'rloc_free'
   ! print*,r
   
   ! deallocate(r)


   
   ! on resoud
   
   ! p_z Rloc 
   ! p_u Vloc
   ! p_b 
   ! p_mu coeff de frottement
   ! p_gu vitesses globales
   ! p_rf second membre
   ! p_M masse
   ! p_H passage global -> local

   ! pb global
   ! p_M p_gu = p_rf + p_H^T p_z
   ! condition de contact frottant
   ! normal p_u + p_b >= 0 perp p_z >= 0  (p_u=p_H p_gu)
   ! tangent coulomb

   
   call c_gfc3d_LmgcDriver(p_z,p_u,p_gu,p_rf,p_b,p_mu           ,&
                           p_M,nzM,p_iM,p_jM,p_H,nzH,p_iH,p_jH  ,&
                           n,nc,ids,ios,p_ip,dos,p_dp,verbose,output, freq_output)


   !print*,z
   !print*,u                          
   !print*,gu
   !
   ! print*,n
   
   ! allocate(r(n))
   ! r = 0.d0
   ! do i=1,n
   !   r(i) = all_H(3*(i-1)+1:3*i)*z(:)
   ! enddo   

   ! print*,'H^T z'
   ! print*,r
   
   ! deallocate(r)
                           
   ! globalac :
   !   iparam(2) = nombre d'iterations faites par le solveur
   !   dparam(2) = erreur obtenue

    do i = 1, get_nb_mecaMAILx()
      ! gu is an output of the solver lets push the solution
      call put_vector_mecaMAILx('V____',i,gu(cc_gdofs(i)+1:cc_gdofs(i+1)),cc_gdofs(i+1)-cc_gdofs(i))
    enddo

  end subroutine

END MODULE SiconosNumerics
