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
! encouraged to load and test the software's suitability as regards their"
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
MODULE MP_SOLVER_3D

  !!****h* LMGC90.CORE/MP_SOLVER_3D
  !! NAME
  !!  module MP_SOLVER_3D
  !! AUTHOR
  !!   M. Renouf (e-mail: Mathieu.Renouf@insa-lyon.fr)
  !! PURPOSE
  !!   This module is dedicated to Thermal Electrical Discrete Element  model
  !!   Contact the author to obtain the module.
  !! USES
  !!  LMGC90.CORE/utilities
  !!  LMGC90.CORE/overall
  !!  LMGC90.CORE/BULK_BEHAVIOUR
  !!  LMGC90.CORE/a_DOF
  !!  LMGC90.CORE/RBDY3
  !!  LMGC90.CORE/SPHER
  !!  LMGC90.CORE/PLANx
  !!  LMGC90.CORE/DNLYC
  !!  LMGC90.CORE/SPSPx
  !!  LMGC90.CORE/SPPLx
  !!  LMGC90.CORE/SPDCx
  !!****

  USE utilities
  USE overall
  USE BULK_BEHAVIOUR
  USE TACT_BEHAVIOUR
  USE a_DOF

  USE RBDY3
  USE SPHER
  USE PLANx
  USE DNLYC
  USE CYLND

  USE SPSPx
  USE SPPLx, only: SPPLx2PLANx, &
                   SPPLx2SPHER
  USE SPDCx, only: SPDCx2DNLYC, &
                   SPDCx2SPHER
  USE SPCDx, only: SPCDx2CYLND, &
                   SPCDx2SPHER

  use inter_meca_handler_3D, only : get_nb_inters         , &
                                    get_rloc              , &
                                    get_vloc              , &
                                    get_vlocBEGIN         , &
                                    get_tact_lawnb        , &
                                    get_eff

  IMPLICIT NONE

  PRIVATE
  
  LOGICAL :: electro_model = .FALSE.
  LOGICAL :: thermo_model  = .FALSE.

  !----------------------------------------------------------------
  !* iheat = 1 : diff
  !* iheat = 2 : conv
  !*
  !* ibound = 1 : adia
  !* ibound = 2 : line
  !* ibound = 3 : cont
  !-----------------------------------------------------------------!

  TYPE T_THERMAL_MODEL
     
     LOGICAL         :: lconv,init
     REAL(kind=8)    :: T0,Alert,Gcond,GTemp
     INTEGER(kind=4) :: iheat,ibound,ilkine,ildiff
     INTEGER(kind=4) :: ilcond
     REAL(kind=8)    :: thickness

  END TYPE T_THERMAL_MODEL
  
  TYPE(T_THERMAL_MODEL)                     :: MODtherm
  REAL(kind=8),ALLOCATABLE,DIMENSION(:,:)   :: TBULK,TBULKi

  !-----------------------------------------------------------------!

  INTEGER(kind=4),PARAMETER :: i_h_diff = 1, i_h_conv = 2
  INTEGER(kind=4),PARAMETER :: i_d_adia = 1, i_d_line = 2, i_d_1D__ = 3 , i_d_3D__ = 4
  INTEGER(kind=4),PARAMETER :: i_lkine_no = 0 , i_lkine_dvn = 1 , i_lkine_dvt = 2 , i_lkine_all = 3
  INTEGER(kind=4),PARAMETER :: i_ldiff_none = 0 , i_ldiff_Hertz = 1 , i_ldiff_Cylnd = 2
  !
  ! FOR EXTRA MODELS
  !
  INTEGER(kind=4),PARAMETER :: i_lc_cylnd = 5
  INTEGER(kind=4),PARAMETER :: i_upxxx = 1 , i_downx = 2 , i_right = 3 , i_leftx = 4 , i_default = 0

  !-----------------------------------------------------------------!

  TYPE T_ELECTRO_MODEL
     
     INTEGER(kind=4) :: current,local

     LOGICAL         :: init,oxide,sliding,breakdown
     REAL(kind=8)    :: A,B,Omega,Phi
     REAL(kind=8)    :: Coxi
     REAL(kind=8)    :: breakdown_threshold,sliding_threshold,breakdown_var
     
     REAL(kind=8)    :: TOL,NLTOL
     INTEGER(kind=4) :: itermax
     INTEGER(kind=4) :: NLitermax

  END TYPE T_ELECTRO_MODEL

  TYPE(T_ELECTRO_MODEL) :: MODelec

  !-----------------------------------------------------------------!

  INTEGER(kind=4),PARAMETER :: i_c_cons = 1, i_c_line = 2, i_c_alte = 3
  INTEGER(kind=4),PARAMETER :: i_l_allno = 1, i_l_Hertz = 2

  !-----------------------------------------------------------------!

  TYPE T_NODES

     INTEGER(kind=4) :: ID_RBDY3,ID_TACTY

     !* thermal purposes

     INTEGER(kind=4) :: iTHsource,iTHbound
     LOGICAL         :: AM_I_TH_BND,AM_I_TH_SRC

     REAL(kind=8)    :: alpha  ! alpha : 1/(rho*Cp*V)
     REAL(kind=8)    :: T      ! T     : Current temperature
     REAL(kind=8)    :: Tini   ! Tini  : Initial temperature
     REAL(kind=8)    :: dT     ! T     : Current temperature velocity
     REAL(kind=8)    :: dTini  ! Tini  : Initial temperature velocity

     !* electrical purposes

     INTEGER(kind=4) :: iELsource,iELbound
     LOGICAL         :: AM_I_EL_BND,AM_I_EL_SRC

     REAL(kind=8)    :: Ic     ! 
     REAL(kind=8)    :: V      ! Electrical strengh
     REAL(kind=8)    :: Vik    ! Electrical strengh

     INTEGER(kind=4)                        :: iadj,nbadj
     REAL(kind=8)                           :: Cii
     INTEGER(kind=4), POINTER, DIMENSION(:) :: adj
     REAL(kind=8), POINTER, DIMENSION(:)    :: Cij

  END TYPE T_NODES

  TYPE(T_NODES),DIMENSION(:),ALLOCATABLE :: Nodes

  INTEGER(kind=4) :: nb_NODES = 0

  !-----------------------------------------------------------------!

  TYPE T_VERLET

     INTEGER(kind=4)                       :: nbadj,iadj
     INTEGER(kind=4) ,POINTER,DIMENSION(:) :: adj
     LOGICAL,POINTER,DIMENSION(:)          :: oxided
     REAL(kind=8),POINTER,DIMENSION(:)     :: threshold
     REAL(kind=8),POINTER,DIMENSION(:)     :: UI

  END TYPE T_VERLET

  TYPE(T_VERLET),ALLOCATABLE,DIMENSION(:) :: VNodes

  !-----------------------------------------------------------------!

  TYPE MULTI_NODE

     INTEGER(kind=4) :: CD,AN
     REAL(kind=8)    :: AREA

  END TYPE MULTI_NODE

  TYPE(MULTI_NODE),DIMENSION(:),ALLOCATABLE :: MultiNodes

  INTEGER(kind=4) :: nb_MLT_NODES = 0

!!!------------------------------------------------------------------!
  
  TYPE T_EXTRA_NODES

     INTEGER(kind=4)                     :: ifirst,ilast,idirection
     REAL(kind=8),DIMENSION(2)           :: internal
     REAL(kind=8)                        :: T,thickness,sgnI
     REAL(kind=8)                        :: lenght,DX,ALPHA

     INTEGER(kind=4)                     :: NX,NY,NZ
     REAL(kind=8),DIMENSION(:),POINTER   :: TSOURCE,TPROFIL
     REAL(kind=8),DIMENSION(:,:),POINTER :: TBULK,TBULKi

  END TYPE T_EXTRA_NODES

  TYPE(T_EXTRA_NODES),DIMENSION(:),ALLOCATABLE :: TH_SRC_NODES,TH_BND_NODES
  TYPE(T_EXTRA_NODES),DIMENSION(:),ALLOCATABLE :: EL_BND_NODES

  INTEGER(kind=4) :: nb_TH_BOUNDS = 0 , nb_TH_SOURCES = 0
  INTEGER(kind=4) :: nb_EL_BOUNDS = 0

  !-----------------------------------------------------------------!

  TYPE T_BRANCHE
     
     INTEGER(kind=4) :: Active
     INTEGER(kind=4) :: icd,ian
     REAL(kind=8)    :: rln,vlt,Ctot,U,UI,Coxi
     REAL(kind=8)    :: threshold,Calpha,reff
     LOGICAL         :: oxided,oxi

  END TYPE T_BRANCHE
  
  TYPE(T_BRANCHE),ALLOCATABLE,DIMENSION(:) :: Branches  

  !-----------------------------------------------------------------!

  INTEGER(kind=4) :: nb_RBDY3 = 0 , nb_SPHER = 0 , nb_PLANx = 0 , nb_CYLND = 0 , nb_DNLYC = 0

  INTEGER(kind=4),ALLOCATABLE,DIMENSION(:) :: spher2nodes,planx2nodes,cylnd2nodes,dnlyc2nodes

  INTEGER(kind=4) :: nb_CDAN = 0 , nb_SPSPx = 0 , nb_SPPLx = 0
  INTEGER(kind=4) :: nb_SPCDx = 0 , nb_SPDCx = 0 

!!!--------------------------------------------

  REAL(kind=8)    :: E_ERR
  INTEGER(kind=4) :: iter , NLiter ,Noxide
  INTEGER(kind=4) :: Isig = 1, nb_OXID=0, nb_changed_OXID = 0 , nb_SLDOXI = 0

  REAL(kind=8) :: V0, Vtmp = 0.D0, Itmp = 0.D0, OMEGA
  REAL(kind=8) :: Req, Cmean, Pmean , Pmax, Pmin , Iinc = 1.0

  REAL(kind=8) :: GLOBAL_DV2,GLOBAL_PV,GLOBAL_DPV,GLOBAL_QIJ,GLOBAL_AQIJ,GLOBAL_AREA

  REAL(kind=8),ALLOCATABLE,DIMENSION(:,:) :: CondMat

  LOGICAL :: first_time=.TRUE.
  LOGICAL :: FREE_BOUNDARY=.FALSE.
  

  PUBLIC &
       active_recup, &
       get_electro_info, &
       get_oxided_tactor, &
       get_write_mp_values, &
       get_global_3D_thermal_variable, &
       get_HEAT_bound_dims, &
       get_HEAT_BOUND_MNODES, &
       get_HEAT_BOUND_TNODES, &
       get_HEAT_BOUND_PROFILE, &
       get_nb_HEAT_bounds, &
       init_mp_solver, &
       init_free_boundary_mp_solver, &
       read_in_mp_behaviour_mp_solver, &
       read_ini_mp_values_mp_solver, &
       solve_electro1G, &
       solve_nl_electro1G, &
       solve_thermo_mp_solver, &
       update_compuctivity_mp_solver, &
       update_thermo_mp_solver, &
       write_out_mp_behaviour_mp_solver, &
       write_xxx_mp_values_mp_solver

CONTAINS
!!!--------------------------------------------------------------------------------------
  SUBROUTINE init_mp_solver
    !!****u* CORE.MPSOLVER/init_mp_solver
    !! NAME
    !!  init_mp_solver
    !! PURPOSE
    !!  
    !!****
    IMPLICIT NONE

    INTEGER :: inode

    nb_RBDY3  = get_nb_RBDY3()
    nb_SPHER  = get_nb_SPHER()
    nb_PLANx  = get_nb_PLANx()
    nb_CYLND  = get_nb_CYLND()
    nb_DNLYC  = get_nb_DNLYC()

    nb_NODES = nb_SPHER + nb_PLANx + nb_CYLND + nb_DNLYC

    IF( nb_NODES .EQ. 0 ) THEN
       CALL LOGMES(' @ No NODES in this problem')
       STOP
    END IF

    IF (ALLOCATED(Nodes)) DEALLOCATE(Nodes)
    ALLOCATE(Nodes(nb_NODES))

    DO inode = 1,nb_NODES

       Nodes(inode)%ID_RBDY3 = 0
       Nodes(inode)%ID_TACTY = 0

       Nodes(inode)%AM_I_TH_SRC = .FALSE.
       Nodes(inode)%AM_I_TH_BND = .FALSE.

       Nodes(inode)%AM_I_EL_SRC = .FALSE.
       Nodes(inode)%AM_I_EL_BND = .FALSE.

       Nodes(inode)%iTHsource = 0
       Nodes(inode)%iTHbound  = 0

       Nodes(inode)%iELsource = 0
       Nodes(inode)%iELbound  = 0

       Nodes(inode)%alpha = 0.D0
       Nodes(inode)%T     = 0.D0
       Nodes(inode)%Tini  = 0.D0
       Nodes(inode)%dT    = 0.D0
       Nodes(inode)%dTini = 0.D0

       Nodes(inode)%V     = 0.D0
       Nodes(inode)%Vik   = 0.D0
       Nodes(inode)%Ic    = 0.D0
       Nodes(inode)%iadj  = 0
       Nodes(inode)%nbadj = 0
       Nodes(inode)%Cii   = 0.0
       NULLIFY(Nodes(inode)%adj)
       NULLIFY(Nodes(inode)%Cij)       

    END DO

    IF (.NOT.ALLOCATED(spher2nodes)) ALLOCATE(spher2nodes(nb_SPHER))
    IF (.NOT.ALLOCATED(planx2nodes)) ALLOCATE(planx2nodes(nb_PLANx))
    IF (.NOT.ALLOCATED(cylnd2nodes)) ALLOCATE(cylnd2nodes(nb_CYLND))
    IF (.NOT.ALLOCATED(dnlyc2nodes)) ALLOCATE(dnlyc2nodes(nb_DNLYC))

    spher2nodes = 0.D0
    planx2nodes = 0.D0
    cylnd2nodes = 0.D0
    dnlyc2nodes = 0.D0

  END SUBROUTINE init_mp_solver
!!!------------------------------------------------------
  SUBROUTINE read_in_mp_behaviour_mp_solver
    !!****u* CORE.MPSOLVER/read_in_mp_behaviour_mp_solver
    !! NAME
    !!  read_in_mp_behaviour_mp_solver
    !! PURPOSE
    !!  
    !!  
    !!****
    IMPLICIT NONE

    INTEGER :: inode,itact

    G_nfich = get_io_unit()

    OPEN(unit=G_nfich,file=trim(location(in_mpdem(:))) )
    CALL read_mp_behaviour
    CLOSE(G_nfich)

!!!-----------------------------------------------------------

    inode = 0

    DO itact = 1,nb_SPHER

       inode = inode + 1
       spher2nodes(itact) = inode

       Nodes(inode)%ID_RBDY3 = spher2bdyty(1,itact)
       Nodes(inode)%ID_TACTY = spher2bdyty(2,itact)

    END DO

    DO itact = 1,nb_PLANx

       inode = inode + 1
       planx2nodes(itact) = inode

       Nodes(inode)%ID_RBDY3 = planx2bdyty(1,itact)
       Nodes(inode)%ID_TACTY = planx2bdyty(2,itact)

    END DO

    DO itact = 1,nb_CYLND

       inode = inode + 1
       cylnd2nodes(itact) = inode

       Nodes(inode)%ID_RBDY3 = cylnd2bdyty(1,itact)
       Nodes(inode)%ID_TACTY = cylnd2bdyty(2,itact)

    END DO

    DO itact = 1,nb_DNLYC

       inode = inode + 1
       dnlyc2nodes(itact) = inode
       
       Nodes(inode)%ID_RBDY3 = dnlyc2bdyty(1,itact)
       Nodes(inode)%ID_TACTY = dnlyc2bdyty(2,itact)

    END DO

!!!-----------------------------------------------------------    

    IF (electro_model) CALL init_behav_electrical_solver
    
    IF (thermo_model)  CALL init_behav_thermal_solver

  END SUBROUTINE read_in_mp_behaviour_mp_solver
!!!------------------------------------------------------
  SUBROUTINE read_mp_behaviour
    !!****u* CORE.MPSOLVER/read_mp_behaviour
    !! NAME
    !!  read_mp_behaviour
    !! PURPOSE
    !!  
    !!  
    !!****
    IMPLICIT NONE

    INTEGER            :: err
    INTEGER            :: imodel,iTHsource,iTHbound,iELbound
    CHARACTER(len=35)  :: IAM = 'mod_mp_solver::read_mp_behaviour'
    CHARACTER(len=30)  :: e_clin
    CHARACTER(len=29)  :: cout
    CHARACTER(len=1)   :: signe

    ! first read model
    err    = 0
    imodel = 0

    DO
       IF( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) .NE. 'model') CYCLE

       SELECT CASE(G_clin(9:13))

!!!****ELECTRO CASE********************************

       CASE('elec_')
          electro_model = .TRUE.
          imodel = imodel + 1
          !*curnt*************************************
          IF( .NOT. read_G_clin()) THEN
             CALL LOGMES('You specified a elec_ model but no keyword are defined')
             CALL LOGMES('Check your MPDEM.DAT file')
             STOP
          END IF
          IF (G_clin(15:19) .NE. 'curnt') THEN
             CALL LOGMES('keyword curnt not defined')
             STOP
          END IF
          SELECT CASE(G_clin(22:25))
          CASE('cons')
             !* I = A
             MODelec%current = i_c_cons
             IF( .NOT. read_G_clin()) THEN
                CALL LOGMES('keyword missing')
                STOP
             END IF
             READ(G_clin(19:34),'(1(2X,D14.7))',iostat=err) MODelec%A
             IF(err.NE.0)THEN
                CALL LOGMES('Error during reading current value')
                STOP
             END IF
          CASE('line')
             !* I = A*MIN(1.,B*t)
             MODelec%current = i_c_line
             IF( .NOT. read_G_clin()) THEN
                CALL LOGMES('keyword missing')
                STOP
             END IF
             READ(G_clin(19:49),'(2(2X,D14.7))',iostat=err) MODelec%A,MODelec%B
             IF(err.NE.0)THEN
                CALL LOGMES('Error during reading current values')
                STOP
             END IF
          CASE('alte')
             !* I = A + B*COS(OMEGA*t+Phi)
             MODelec%current = i_c_alte
             IF( .NOT. read_G_clin()) THEN
                CALL LOGMES('keyword missing')
                STOP
             END IF
             READ(G_clin(19:79),'(4(2X,D14.7))',iostat=err) MODelec%A,MODelec%B,MODelec%Omega,MODelec%Phi
             IF(err.NE.0)THEN
                CALL LOGMES('Error during reading current values')
                STOP
             END IF
          CASE DEFAULT
             CALL LOGMES('current type not defined')
             STOP
          END SELECT
          
          !*local*************************************
          IF( .NOT. read_G_clin()) THEN
             CALL LOGMES('keyword missing')
             STOP
          END IF
          IF (G_clin(15:19) .NE. 'local') THEN
             CALL LOGMES('keyword local not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:26))
             CASE('hertz')
                MODelec%local = i_l_Hertz
             CASE('allno')
                MODelec%local = i_l_allno
             CASE DEFAULT
                CALL LOGMES('local type not defined')
                STOP
             END SELECT
             !* iter_ ********************
             IF( .NOT. read_G_clin()) THEN
                CALL LOGMES('keyword missing')
                STOP
             END IF
             IF (G_clin(22:26) .NE. 'iter_') THEN
                CALL LOGMES('keyword iter_ missing')
                STOP
             ELSE
                READ(G_clin(28:34),'(I7)',iostat=err) MODelec%itermax
                IF(err.NE.0)THEN
                   CALL LOGMES('Error during reading itermax value')
                   STOP
                END IF
             END IF
             !* tol__ ********************
             IF( .NOT. read_G_clin()) THEN
                CALL LOGMES('keyword missing')
                STOP
             END IF
             IF (G_clin(22:26) .NE. 'tol__') THEN
                CALL LOGMES('keyword tol__ missing')
                STOP
             ELSE
                READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%TOL
                IF(err.NE.0)THEN
                   CALL LOGMES('Error during reading tol value')
                   STOP
                END IF
             END IF
          END IF

          !*oxide*************************************
          IF( .NOT. read_G_clin()) THEN
             CALL LOGMES('keyword missing')
             STOP
          END IF
          IF (G_clin(15:19) .NE. 'oxide') THEN
             CALL LOGMES('keyword oxide not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:24))
             CASE('yes')
                MODelec%oxide = .TRUE.
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'Cond_') THEN
                   CALL LOGMES('keyword Cond_ not defined')
                   STOP
                ELSE
                   READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%Coxi
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading Coxi value')
                      STOP
                   END IF
                END IF
                !* iter_ ********************
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'iter_') THEN
                   CALL LOGMES('keyword iter_ missing')
                   STOP
                ELSE
                   READ(G_clin(28:34),'(I7)',iostat=err) MODelec%NLitermax
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading NLitermax value')
                      STOP
                   END IF
                END IF
                !* tol__ ********************
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'tol__') THEN
                   CALL LOGMES('keyword tol__ missing')
                   STOP
                ELSE
                   READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%NLTOL
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading NLtol value')
                      STOP
                   END IF
                END IF
             CASE('no ')
                MODelec%oxide = .FALSE.
             CASE DEFAULT
                CALL LOGMES('problem for the oxide definition')
                STOP
             END SELECT
          END IF

          !*brkdw*************************************
          IF( .NOT. read_G_clin()) THEN
             CALL LOGMES('keyword missing')
             STOP
          END IF
          IF (G_clin(15:19) .NE. 'brkdw') THEN
             CALL LOGMES('keyword brkdw not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:24))
             CASE('yes')
                MODelec%breakdown = .TRUE.
                !---------------------------------
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'thsld') THEN
                   CALL LOGMES('keyword thsld missing')
                   STOP
                ELSE
                   READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%breakdown_threshold
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading value')
                      STOP
                   END IF
                END IF
                !---------------------------------
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'var__') THEN
                   CALL LOGMES('keyword var__ missing')
                   STOP
                ELSE
                   READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%breakdown_var
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading value')
                      STOP
                   END IF
                END IF

                !----------------------------------
             CASE('no ')
                MODelec%breakdown = .FALSE.
             CASE DEFAULT
             END SELECT
          END IF
          !*sldng*************************************
          IF( .NOT. read_G_clin()) THEN
             CALL LOGMES('keyword missing')
             STOP
          END IF
          IF (G_clin(15:19) .NE. 'sldng') THEN
             CALL LOGMES('keyword sldng not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:24))
             CASE('yes')
                MODelec%sliding = .TRUE.
                !---------------------------------
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'sldth') THEN
                   CALL LOGMES('keyword thsld missing')
                   STOP
                ELSE
                   READ(G_clin(28:41),'(D14.7)',iostat=err) MODelec%sliding_threshold
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading sliding threshold value')
                      STOP
                   END IF
                END IF
                !----------------------------------
             CASE('no ')
                MODelec%sliding = .FALSE.
             CASE DEFAULT
             END SELECT
          END IF

!!!****THERMO CASE**********************************

       CASE('therm')

          thermo_model = .TRUE.
          imodel = imodel + 1

          !* T0___
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'T0___') THEN
             CALL LOGMES('keyword T0___ not defined')
             STOP
          ELSE
             READ(G_clin(21:34),'(D14.7)',iostat=err) MODtherm%T0
             IF(err.NE.0)THEN
                CALL LOGMES('Error during reading value')
                STOP
             END IF
          END IF

          !* alert
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'alert') THEN
             CALL LOGMES('keyword alert not defined')
             STOP
          ELSE
             READ(G_clin(21:34),'(D14.7)',iostat=err) MODtherm%Alert
             IF(err.NE.0)THEN
                CALL LOGMES('Error during reading value')
                STOP
             END IF
          END IF

          !* ldiff************************************
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'ldiff') THEN
             CALL LOGMES('keyword ldiff not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:26))
             CASE('Hertz')
                MODtherm%ildiff = i_ldiff_Hertz
             CASE('Cylnd')
                MODtherm%ildiff = i_ldiff_Cylnd
             CASE DEFAULT
                CALL LOGMES('ldiff case not implemented')
                STOP
             END SELECT
          END IF

          !* lconv************************************
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'lconv') THEN
             CALL LOGMES('keyword lconv not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:24))
             CASE('yes')
                MODtherm%lconv = .TRUE.
                !---------------------------------
                IF( .NOT. read_G_clin()) THEN
                   CALL LOGMES('keyword missing')
                   STOP
                END IF
                IF (G_clin(22:26) .NE. 'Gcond') THEN
                   CALL LOGMES('keyword Gcond missing')
                   STOP
                ELSE
                   READ(G_clin(28:63),'(D14.7,8X,D14.7)',iostat=err) MODtherm%Gcond,MODtherm%GTemp
                   PRINT*,'Gcond: ',MODtherm%Gcond,'GTemp:',MODtherm%GTemp
                   IF(err.NE.0)THEN
                      CALL LOGMES('Error during reading sliding threshold value')
                      STOP
                   END IF
                END IF
                !----------------------------------

             CASE('no ')
                MODtherm%lconv = .FALSE.
             CASE DEFAULT
                CALL LOGMES('lconv case not implemented')
                STOP
             END SELECT
          END IF

          !*lkine*************************************
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'lkine') THEN
             CALL LOGMES('keyword lkine not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:24))
             CASE('all')
                MODtherm%ilkine = i_lkine_all
             CASE('dvt')
                MODtherm%ilkine = i_lkine_dvt
             CASE('dvn')
                MODtherm%ilkine = i_lkine_dvn
             CASE('no ')
                MODtherm%ilkine = i_lkine_no
             CASE DEFAULT
                CALL LOGMES('lkine case not implemented')
                STOP
             END SELECT
          END IF

          !*bound*************************************
          IF( .NOT. read_G_clin()) EXIT
          IF (G_clin(15:19) .NE. 'bound') THEN
             CALL LOGMES('keyword bound not defined')
             STOP
          ELSE
             SELECT CASE(G_clin(22:25))
             CASE('adia')
                MODtherm%ibound = i_d_adia
             CASE('line')
                MODtherm%ibound = i_d_line
             CASE('1D__')
                MODtherm%ibound = i_d_1D__
             CASE('3D__')
                MODtherm%ibound = i_d_3D__
             CASE DEFAULT                  !12345678901                 1234567890123456
                WRITE(cout,'(A11,A4,A16)') 'bound case ',G_clin(22:25),' not implemented'
                CALL LOGMES(cout)
                STOP
             END SELECT
          END IF

!!!****DEFAULT CASE*********************************
       CASE DEFAULT
          CALL LOGMES('model not implemented')
          STOP
       END SELECT
    END DO

    REWIND(G_nfich)

    WRITE(6,'(1X,I2,1X,A13)') imodel,' models found'

    ! second read size sources and bounds

    iTHsource = 0
    iTHbound  = 0
    iELbound  = 0

    DO
       IF( .NOT. read_G_clin()) EXIT
       SELECT CASE(G_clin(2:7))

       CASE('source')
          SELECT CASE(G_clin(10:14))
          CASE('therm')
             iTHsource = iTHsource + 1
          CASE DEFAULT
             CALL LOGMES('source unavailable')
             STOP
          END SELECT

       CASE('bounds')
          SELECT CASE(G_clin(10:14))
          CASE('therm')
             iTHbound = iTHbound + 1
          CASE('elec_')
             iELbound = iELbound + 1
          CASE DEFAULT
             CALL LOGMES('bounds unavailable')
             STOP
          END SELECT
          
       CASE DEFAULT

       END SELECT
    END DO
    
    REWIND(G_nfich)
    
    WRITE(6,'(1X,I3,1X,A25)') iTHsource,' thermal sources found  '
    WRITE(6,'(1X,I3,1X,A25)') iTHbound, ' thermal bounds found   '
    WRITE(6,'(1X,I3,1X,A25)') iELbound, ' electrical bounds found'

    nb_TH_SOURCES = iTHsource
    nb_TH_BOUNDS  = iTHbound
    nb_EL_BOUNDS  = iELbound

    !* Allocate bounds and sources *

    IF ( ALLOCATED(TH_SRC_NODES) ) DEALLOCATE(TH_SRC_NODES) 
    ALLOCATE(TH_SRC_NODES(nb_TH_SOURCES))

    IF ( ALLOCATED(TH_BND_NODES) ) DEALLOCATE(TH_BND_NODES) 
    ALLOCATE(TH_BND_NODES(nb_TH_BOUNDS))

    IF ( ALLOCATED(EL_BND_NODES) ) DEALLOCATE(EL_BND_NODES) 
    ALLOCATE(EL_BND_NODES(nb_EL_BOUNDS))

    !* ************************** *

    DO iTHsource = 1,nb_TH_SOURCES
       TH_SRC_NODES(iTHsource)%ifirst    = 0
       TH_SRC_NODES(iTHsource)%ilast     = 0
       TH_SRC_NODES(iTHsource)%internal  = 0.D0
       TH_SRC_NODES(iTHsource)%T         = 0.D0
       TH_SRC_NODES(iTHsource)%sgnI      = 0.D0
       TH_SRC_NODES(iTHsource)%thickness = 0.D0
    END DO

    DO iTHbound = 1,nb_TH_BOUNDS
       TH_BND_NODES(iTHbound)%ifirst    = 0
       TH_BND_NODES(iTHbound)%ilast     = 0
       TH_BND_NODES(iTHbound)%internal  = 0.D0
       TH_BND_NODES(iTHbound)%thickness = 0.D0
       TH_BND_NODES(iTHbound)%T         = 0.D0
       TH_BND_NODES(iTHbound)%sgnI      = 0.D0
       TH_BND_NODES(iTHbound)%NX        = 0
       TH_BND_NODES(iTHbound)%NY        = 0
       NULLIFY(TH_BND_NODES(iTHbound)%TSOURCE)
       NULLIFY(TH_BND_NODES(iTHbound)%TPROFIL)
       NULLIFY(TH_BND_NODES(iTHbound)%TBULK)
       NULLIFY(TH_BND_NODES(iTHbound)%TBULKi)
    END DO

    DO iELbound = 1,nb_EL_BOUNDS
       EL_BND_NODES(iELbound)%ifirst    = 0
       EL_BND_NODES(iELbound)%ilast     = 0
       EL_BND_NODES(iELbound)%internal  = 0.D0
       EL_BND_NODES(iELbound)%thickness = 0.D0
       EL_BND_NODES(iELbound)%sgnI      = 0.D0
       EL_BND_NODES(iELbound)%T         = 0.D0
    END DO

    !* ************************** *

    iTHsource = 0
    iTHbound  = 0
    iELbound  = 0

    DO
       IF( .NOT. read_G_clin()) EXIT

       SELECT CASE(G_clin(2:7))
       CASE('source')

          SELECT CASE(G_clin(10:14))
          CASE('therm')
             iTHsource = iTHsource + 1
          
             IF( .NOT. read_G_clin() )THEN
                CALL LOGMES('Value missing')
                STOP
             END IF
             !********
             IF (G_clin(19:20) .EQ. 'to') THEN
                READ(G_clin(10:29),'(I7,6X,I7)') TH_SRC_NODES(iTHsource)%ifirst,TH_SRC_NODES(iTHsource)%ilast
             ELSE
                READ(G_clin(10:16),'(I7)') TH_SRC_NODES(iTHsource)%ifirst
                TH_SRC_NODES(iTHsource)%ilast = TH_SRC_NODES(iTHsource)%ifirst
             END IF
             !********
             READ(G_clin(37:50),'(D14.7)') TH_SRC_NODES(iTHsource)%T
             !********
          CASE DEFAULT
             CALL LOGMES('source unavailable')
             STOP
          END SELECT

       CASE('bounds')
          
          SELECT CASE(G_clin(10:14))
          CASE('therm')
             iTHbound = iTHbound + 1
             SELECT CASE(G_clin(16:16))
             CASE('U')
                TH_BND_NODES(iTHbound)%idirection = i_upxxx
             CASE('D')
                TH_BND_NODES(iTHbound)%idirection = i_downx
             CASE('R')
                TH_BND_NODES(iTHbound)%idirection = i_right
             CASE('L')
                TH_BND_NODES(iTHbound)%idirection = i_leftx
             CASE DEFAULT
                TH_BND_NODES(iTHbound)%idirection = i_default
             END SELECT

             IF( .NOT. read_G_clin() )THEN
                CALL LOGMES('Value missing')
                STOP
             END IF

             !********
             IF (G_clin(19:20) .EQ. 'to') THEN
                READ(G_clin(10:29),'(I7,6X,I7)') TH_BND_NODES(iTHbound)%ifirst, &
                                                 TH_BND_NODES(iTHbound)%ilast
             ELSE
                READ(G_clin(10:16),'(I7)') TH_BND_NODES(iTHbound)%ifirst
                TH_BND_NODES(iTHbound)%ilast = TH_BND_NODES(iTHbound)%ifirst
             END IF
             !********
             READ(G_clin(37:74),'(D14.7,7X,D14.7)') TH_BND_NODES(iTHbound)%T, &
                                                    TH_BND_NODES(iTHbound)%thickness

             IF( .NOT. read_G_clin() )THEN
                CALL LOGMES('Value missing')
                STOP
             END IF

             READ(G_clin(37:74),'(D14.7,7X,D14.7)') TH_BND_NODES(iTHbound)%lenght, &
                                                    TH_BND_NODES(iTHbound)%ALPHA
             !********
          CASE('elec_')
             iELbound = iELbound + 1

             IF( .NOT. read_G_clin() )THEN
                CALL LOGMES('Value missing')
                STOP
             END IF
             !********

             IF (G_clin(19:20) .EQ. 'to') THEN

                READ(G_clin(10:29),'(I7,6X,I7)') EL_BND_NODES(iELbound)%ifirst,EL_BND_NODES(iELbound)%ilast
             ELSE

                READ(G_clin(10:16),'(I7)') EL_BND_NODES(iELbound)%ifirst
                EL_BND_NODES(iELbound)%ilast = EL_BND_NODES(iELbound)%ifirst
             END IF
             !********

             READ(G_clin(38:38),'(A1)') signe
             IF (signe.EQ.'+') THEN
                EL_BND_NODES(iELbound)%sgnI = 1.D0
             ELSE IF(signe.EQ.'-')THEN
                EL_BND_NODES(iELbound)%sgnI =-1.D0
             END IF
             !********
          CASE DEFAULT
             CALL LOGMES('bounds unavailable')
             STOP
          END SELECT

       CASE DEFAULT
          
       END SELECT

    END DO

    REWIND(G_nfich)

  END SUBROUTINE read_mp_behaviour
!!!-----------------------------------------------------------
  subroutine read_ini_mp_values_mp_solver(step)
    implicit none
    integer(kind=4), intent(in) :: step
    !
    INTEGER :: ibdyty,inode
    
    CHARACTER(len=103) :: cout
    CHARACTER(len=33)  :: IAM
    
    IAM = 'mod_mp_solver::read_ini_mp_values'

    G_nfich=get_io_unit()

    if(step == 0) then
      open(unit=G_nfich,file=trim(location(in_mpv(:))))
    else if(step > 1) then
      open(unit=G_nfich,file=trim(location(out_mpv(:))))
    else
      open(unit=G_nfich,file=trim(location(last_mpv(:))))
    end if

    DO    
       IF ( .NOT. read_G_clin()) EXIT
       IF (G_clin(2:6) /= 'nodty') CYCLE
       READ(G_clin(9:15),'(I7)') inode
       READ(G_clin(24:59),'(D14.7,8X,D14.7)') Nodes(inode)%V,Nodes(inode)%Tini
       WRITE(6,'(D14.7,8X,D14.7)') Nodes(inode)%V,Nodes(inode)%Tini
       Nodes(inode)%T = Nodes(inode)%Tini
       CYCLE
    END DO

    CLOSE(G_nfich)    

    DO inode =1,nb_NODES
       CALL put_electric_potentiel(Nodes(inode)%ID_RBDY3,Nodes(inode)%ID_TACTY,Nodes(inode)%V)
       CALL put_thermal_value(Nodes(inode)%ID_RBDY3,Nodes(inode)%ID_TACTY,Nodes(inode)%Tini)
    END DO

  END SUBROUTINE read_ini_mp_values_mp_solver
!!!--------------------------------------------------------------------------------------
  subroutine write_xxx_mp_values_mp_solver(which)
    implicit none
    integer, intent(in) :: which
    !
    INTEGER :: nfich,ibdyty,inode
    CHARACTER(len=103) :: cout
    CHARACTER(len=35)  :: IAM
    
    IAM = 'mod_mp_solver::write_xxx_mp_values'

    select case(which)
    case(1)
       nfich = get_io_unit()
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',&
            file=trim(location(out_mpv(:))))
    case(2)
       nfich = get_io_unit()
       open(unit=nfich,STATUS='OLD',POSITION='APPEND',&
            file=trim(location(last_mpv(:))))
    case(6)
       nfich = which
    end select
  
    DO inode=1,nb_NODES
       WRITE(nfich,'(A6,2X,I7,2(2X,A5,D14.7))') '$nodty',inode,'PotE=',Nodes(inode)%V,'Temp=',Nodes(inode)%Tini
       WRITE(nfich,'(A6)') '      '
    END DO

    if( nfich /= 6 ) close(nfich)

  end subroutine write_xxx_mp_values_mp_solver
!!!--------------------------------------------------------------------------------------
  SUBROUTINE init_behav_electrical_solver

    IMPLICIT NONE
    
    INTEGER           :: ibdyty,itacty,itact,inode,ibound
    CHARACTER(len=30) :: cout

    REAL(kind=8)      :: Concd,rho,avrd,Hspe,TMP,TCdown

!!!-----------------------------------------------------------

    Cmean = 0.D0

    DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3
       itacty = Nodes(inode)%ID_TACTY

       DO ibound=1,nb_EL_BOUNDS

          IF (ibdyty.LT.EL_BND_NODES(ibound)%ifirst) CYCLE
          IF (ibdyty.GT.EL_BND_NODES(ibound)%ilast) CYCLE
             
          Nodes(inode)%AM_I_EL_BND = .TRUE.
          Nodes(inode)%iELbound  = ibound

          EXIT

       END DO

       CALL put_electric_potentiel(ibdyty,itacty,Nodes(inode)%V)

       Cmean = Cmean + get_elec_cond(ibdyty,itacty)

    END DO

    Cmean = Cmean/REAL(nb_NODES,8)

    IF (MODelec%oxide) THEN

       IF ( ALLOCATED(VNodes) ) DEALLOCATE(VNodes)
       ALLOCATE(VNodes(nb_NODES))

       DO inode = 1,nb_NODES
          Vnodes(inode)%nbadj = 0
          Vnodes(inode)%iadj  = 0
          NULLIFY(Vnodes(inode)%adj)
          NULLIFY(Vnodes(inode)%oxided)
          NULLIFY(Vnodes(inode)%threshold)
          NULLIFY(Vnodes(inode)%UI)
       END DO

    END IF

  END SUBROUTINE init_behav_electrical_solver
!!!-----------------------------------------------------------
  SUBROUTINE init_behav_thermal_solver

    IMPLICIT NONE
    
    INTEGER           :: ibdyty,itacty,itact,inode,isource,ibound
    CHARACTER(len=30) :: cout
    LOGICAL           :: FLAG
    REAL(kind=8)      :: Concd,rho,avrd,Hspe,TMP,TCdown

    INTEGER                   :: icdbdy,ianbdy,icdtac,iantac
    INTEGER                   :: icdnode,iannode,imltnode
    REAL(kind=8)              :: rcd,ran,reff,dist
    REAL(kind=8),DIMENSION(3) :: coorcd,cooran,sep

    inode = 0

!!!-----------------------------------------------------------

     DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3

       DO isource=1,nb_TH_SOURCES

          IF (ibdyty.LT.TH_SRC_NODES(isource)%ifirst) CYCLE
          IF (ibdyty.GT.TH_SRC_NODES(isource)%ilast) CYCLE
          
          Nodes(inode)%AM_I_TH_SRC = .TRUE.
          Nodes(inode)%iTHsource   = isource
          EXIT

       END DO

       DO ibound=1,nb_TH_BOUNDS

          IF (ibdyty.LT.TH_BND_NODES(ibound)%ifirst) CYCLE
          IF (ibdyty.GT.TH_BND_NODES(ibound)%ilast) CYCLE

          Nodes(inode)%AM_I_TH_BND = .TRUE.
          Nodes(inode)%iTHbound = ibound

          TH_BND_NODES(ibound)%NX = TH_BND_NODES(ibound)%NX + 1

          EXIT

       END DO

    END DO

    DO ibound=1,nb_TH_BOUNDS
       IF (TH_BND_NODES(ibound)%NX.EQ.0) CYCLE
       ALLOCATE(TH_BND_NODES(ibound)%TSOURCE(TH_BND_NODES(ibound)%NX))
       TH_BND_NODES(ibound)%TSOURCE = 0
       TH_BND_NODES(ibound)%NX      = 0
    END DO

    DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3

       DO ibound=1,nb_TH_BOUNDS

          IF (ibdyty.LT.TH_BND_NODES(ibound)%ifirst) CYCLE
          IF (ibdyty.GT.TH_BND_NODES(ibound)%ilast) CYCLE

          Nodes(inode)%AM_I_TH_BND = .TRUE.
          Nodes(inode)%iTHbound = ibound

          TH_BND_NODES(ibound)%NX = TH_BND_NODES(ibound)%NX + 1
          TH_BND_NODES(ibound)%TSOURCE(TH_BND_NODES(ibound)%NX) = inode
          EXIT

       END DO

    END DO

    DO ibound=1,nb_TH_BOUNDS
       CALL INIT_BULK_MATRIX(ibound)
    END DO

!!!-----------------------------------------------------------

    DO inode = 1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3
       itacty = Nodes(inode)%ID_TACTY

       rho  = get_rho(get_bulk_behav_number_RBDY3(ibdyty,1))
       Hspe = get_Hspe(get_bulk_behav_number_RBDY3(ibdyty,1))
       avrd = get_avr_radius_tacty(ibdyty,itacty)

       TMP = rho*Hspe*avrd*avrd*avrd*PI_g*4.0/3.0

       IF ( TMP .GT. 1.D-16 ) THEN
          Nodes(inode)%alpha = H/TMP
       ELSE
          Nodes(inode)%alpha = 0.D0
       END IF
       
       IF (Nodes(inode)%AM_I_TH_SRC) THEN
          Nodes(inode)%Tini  = TH_SRC_NODES(Nodes(inode)%iTHsource)%T
          Nodes(inode)%T     = Nodes(inode)%Tini
          Nodes(inode)%dTini = 0.D0
          Nodes(inode)%dT    = 0.D0
       ELSE
          Nodes(inode)%Tini  = MODtherm%T0
          Nodes(inode)%T     = MODtherm%T0
          Nodes(inode)%dTini = 0.D0
          Nodes(inode)%dT    = 0.D0
       END IF

       CALL put_thermal_value(ibdyty,itacty,Nodes(inode)%Tini)

    END DO

    !* MULTI NODES TREATMENT

    !* first sizing vector

    imltnode = 0

    DO icdnode = 1,nb_NODES

       icdbdy = Nodes(icdnode)%ID_RBDY3
       icdtac = Nodes(icdnode)%ID_TACTY

       DO iannode = icdnode+1,nb_NODES 

          ianbdy = Nodes(iannode)%ID_RBDY3

          IF(icdbdy.NE.ianbdy) CYCLE
          !* MULTI-TACTOR CASE

          iantac = Nodes(iannode)%ID_TACTY

          coorcd = get_coor(icdbdy,icdtac)
          cooran = get_coor(ianbdy,iantac)

          sep  = coorcd - cooran
          dist = sep(1)*sep(1) + sep(2)*sep(2) + sep(3)*sep(3)

          IF (SQRT(dist) .GT. MODtherm%alert) CYCLE
          
          imltnode = imltnode + 1

       END DO

    END DO

    !* 
    nb_MLT_NODES = imltnode

    IF (nb_MLT_NODES.EQ.0) THEN
       CALL LOGMES('Warning!')
       CALL LOGMES('No MULTI-NODE according to the definition of MODtherm%alert')
    ELSE
       WRITE(6,'(I7,A18)') nb_MLT_NODES,' MULTI-NODES found' 
       IF (ALLOCATED(MultiNodes)) DEALLOCATE(MultiNodes)
       ALLOCATE(MultiNodes(nb_MLT_NODES))
       DO imltnode = 1,nb_MLT_NODES
          MultiNodes(imltnode)%CD   = 0
          MultiNodes(imltnode)%AN   = 0
          MultiNodes(imltnode)%Area = 0.D0
       END DO
    END IF
    !*

    imltnode = 0

    DO icdnode = 1,nb_NODES

       icdbdy = Nodes(icdnode)%ID_RBDY3
       icdtac = Nodes(icdnode)%ID_TACTY

       DO iannode = icdnode+1,nb_NODES 

          ianbdy = Nodes(iannode)%ID_RBDY3
          iantac = Nodes(iannode)%ID_TACTY

          IF(icdbdy.NE.ianbdy) CYCLE
          !* MULTI-TACTOR CASE

          coorcd = get_coor(icdbdy,icdtac)
          cooran = get_coor(ianbdy,iantac)

          sep  = coorcd - cooran
          dist = sep(1)*sep(1) + sep(2)*sep(2) + sep(3)*sep(3)

          IF (SQRT(dist) .GT. MODtherm%alert) CYCLE

          imltnode = imltnode + 1

          MultiNodes(imltnode)%CD = icdnode
          MultiNodes(imltnode)%AN = iannode

          rcd = get_avr_radius_tacty(icdbdy,icdtac)
          ran = get_avr_radius_tacty(ianbdy,iantac)

          reff = (rcd+ran)*0.5
          MultiNodes(imltnode)%Area = 2.0*reff

       END DO

    END DO

  END SUBROUTINE init_behav_thermal_solver
!!!--------------------------------------------------------------------------------------
  SUBROUTINE solve_electro1G

    IMPLICIT NONE

    INTEGER          :: inode,icdan,ibdyty,itacty,inet,iadj,nbadj
    INTEGER          :: icdnode,iannode,icdtac,iantac,adjsz

    REAL(kind=8)     :: rlt,rln
    REAL(kind=8)     :: Concd,Conan,Conct,Conby,Ctot,Cii,Condini
    REAL(KIND=8)     :: Resct,Resby,Tup,duk
    REAL(KIND=8)     :: Pot,Itot,Apot,PotUPxxx,PotDOWNx

    CHARACTER(len=5) :: status

    Req   = 0.0

    nb_CDAN = 0
    nb_SPSPx = get_nb_inters( i_spspx )
    nb_CDAN = nb_CDAN + nb_SPSPx

    nb_SPPLx = get_nb_inters( i_spplx )
    nb_CDAN = nb_CDAN + nb_SPPLx

    nb_SPDCx = get_nb_inters( i_spdcx )
    nb_CDAN = nb_CDAN + nb_SPDCx

    nb_SPCDx = get_nb_inters( i_spcdx )
    nb_CDAN = nb_CDAN + nb_SPCDx

    IF ( nb_CDAN == 0 ) RETURN 

    IF ( ALLOCATED(Branches) ) DEALLOCATE(Branches)
    ALLOCATE(Branches(nb_CDAN))
!!!*
    Conan = 0.0
    Concd = 0.0
!!!*
    inet  = 0

    DO inode = 1,nb_NODES
       Nodes(inode)%iadj = 0
    END DO

    CALL fill_active_contact(inet)

    IF ( inet == 0 ) RETURN

!!! matrix construction --------------------------------------------------

    CALL conductivity_matrix_construction

!!! Initialization --------------------------------------------------------

!!!* current *

    SELECT CASE(MODelec%current)
    CASE(i_c_cons)
       DO inode = 1,nb_NODES
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE(i_c_line)
       DO inode = 1,nb_NODES
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*MIN(1.D0,MODelec%B*TPS)
          Nodes(inode)%Ic = Nodes(inode)%Ic*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE(i_c_alte)
       DO inode = 1,nb_NODES
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*SIN(MODelec%Omega*TPS+MODelec%Phi)
          Nodes(inode)%Ic = Nodes(inode)%Ic*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE DEFAULT
       CALL LOGMES('problem')
       STOP       
    END SELECT

!!!* Potential *

    IF (MODelec%init) THEN
       DO inode = 1,nb_NODES
          ibdyty = NODES(inode)%ID_RBDY3
          itacty = NODES(inode)%ID_TACTY
          Nodes(inode)%V = get_electric_potentiel(ibdyty,itacty)
       END DO
    ELSE
       DO inode = 1,nb_NODES
          Nodes(inode)%V = 0.0
       END DO
    END IF

!!! start iteration --------------------------------------------------------

    iter = 0
    
    DO
       
       iter = iter + 1
       
       IF( iter > MODelec%itermax ) EXIT
       
       E_ERR = 0.D0
       PRINT*,iter
       DO icdnode = 1,nb_NODES
          PRINT*,icdnode,ABS(Nodes(icdnode)%Cii)
          IF ( ABS(Nodes(icdnode)%Cii) .GE. 1.D-14 ) THEN
             
             Itot  = Nodes(icdnode)%Ic
             nbadj = Nodes(icdnode)%nbadj

             DO iadj = 1,nbadj
                iannode = Nodes(icdnode)%adj(iadj)
                Itot = Itot - Nodes(iannode)%V*Nodes(icdnode)%Cij(iadj)
             END DO
             
             Pot = Itot/Nodes(icdnode)%Cii
             
             IF( ABS(Pot) > 1.D-16 ) THEN
                Apot = ABS( (Nodes(icdnode)%V-Pot )/Pot)
             ELSE
                Apot = ABS( Nodes(icdnode)%V-Pot )
             END IF
             
             Nodes(icdnode)%V = Pot
             E_ERR = MAX( E_ERR , Apot )
             
          END IF
          
       END DO

       IF ( E_ERR < MODelec%TOL ) EXIT
          
    END DO
    
!!! end of iteration ---------------------------------------------------------
       
    DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3
       itacty = Nodes(inode)%ID_TACTY
       PRINT*,inode,Nodes(inode)%V
       CALL put_electric_potentiel(ibdyty,itacty,Nodes(inode)%V )

    END DO

  END SUBROUTINE solve_electro1G
!!!--------------------------------------------------------------------------------------
  SUBROUTINE solve_nl_electro1G

    IMPLICIT NONE

    INTEGER          :: inode,icdan,ibdyty,inet,iadj,nbadj,itacty
    INTEGER          :: icdnode,iannode,icdtac,iantac,adjsz,nb_RECUP
    INTEGER          :: NL_ERR

    REAL(kind=8)     :: rlt,rln,vlt,vln,X
    REAL(kind=8)     :: Concd,Conan,Conct,Conby,Ctot,Cii,Condini,Calpha
    REAL(KIND=8)     :: Resct,Resby,Tup,duk
    REAL(kind=8)     :: gapTT,gap,reff,meff
    REAL(KIND=8)     :: Pot,Itot,Apot,PotUPxxx,PotDOWNx

    CHARACTER(len=5) :: status

    integer( kind = 4 ) :: id_inter

    nb_CDAN = 0
    nb_SPSPx = get_nb_inters( i_spspx )
    nb_CDAN = nb_CDAN + nb_SPSPx

    nb_SPPLx = get_nb_inters( i_spplx )
    nb_CDAN = nb_CDAN + nb_SPPLx

    nb_SPDCx = get_nb_inters( i_spdcx )
    nb_CDAN = nb_CDAN + nb_SPDCx

    nb_SPCDx = get_nb_inters( i_spcdx )
    nb_CDAN = nb_CDAN + nb_SPCDx

    IF ( nb_CDAN == 0 ) RETURN 

    IF ( ALLOCATED(Branches) ) DEALLOCATE(Branches)
    ALLOCATE(Branches(nb_CDAN))

!!! Initialisation ----------------------------------------------

    NL_ERR = MAX(1,INT(nb_CDAN*MODelec%NLTOL))

    WRITE(6,'(A16,I5)') 'NL Error value: ',NL_ERR

!!! Initialization --------------------------------------------------------

!!!* current *

    SELECT CASE(MODelec%current)
    CASE(i_c_cons)
       DO inode = 1,nb_NODES
          Nodes(inode)%Ic = 0.D0
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE(i_c_line)
       DO inode = 1,nb_NODES
          Nodes(inode)%Ic = 0.D0
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*MIN(1.D0,MODelec%B*TPS)
          Nodes(inode)%Ic = Nodes(inode)%Ic*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE(i_c_alte)
       DO inode = 1,nb_NODES
          Nodes(inode)%Ic = 0.D0
          IF(.NOT.Nodes(inode)%AM_I_EL_BND) CYCLE
          Nodes(inode)%Ic = MODelec%A*SIN(MODelec%Omega*TPS+MODelec%Phi)
          Nodes(inode)%Ic = Nodes(inode)%Ic*EL_BND_NODES(Nodes(inode)%iELbound)%sgnI
       END DO
    CASE DEFAULT
       CALL LOGMES('problem')
       STOP       
    END SELECT

    Conan = 0.0
    Concd = 0.0
    inet  = 0

    DO inode = 1,nb_NODES
       Nodes(inode)%iadj = 0
    END DO

!!!--------------------------------------------------------------

    CALL fill_active_contact(inet)
    WRITE(6,'(A18,I5)') 'Network branches: ',inet

    nb_RECUP  = 0
    nb_SLDOXI = 0

    id_inter = i_spspx

    IF (first_time) THEN
       DO icdan = 1, nb_CDAN
          vlt = Branches(icdan)%vlt
          IF ( ABS(vlt) .GT. MODelec%sliding_threshold ) THEN

             Branches(icdan)%oxided   = .FALSE.
             Branches(icdan)%oxi      = .FALSE.

          ELSE

             Branches(icdan)%oxided   = .TRUE.
             Branches(icdan)%oxi      = .TRUE.

             call get_eff( id_inter, icdan, meff, reff )
             Branches(icdan)%threshold = 0.75*MODelec%breakdown_threshold*reff*Branches(icdan)%rln/H
             
          END IF
       END DO
    ELSE
       DO icdan = 1, nb_CDAN

          icdnode = Branches(icdan)%icd
          iannode = Branches(icdan)%ian

          nbadj = VNodes(icdnode)%nbadj
          vlt   = Branches(icdan)%vlt
       
          IF ( ABS(vlt) .GT. MODelec%sliding_threshold ) THEN
             IF (Branches(icdan)%oxided) nb_SLDOXI = nb_SLDOXI + 1
             Branches(icdan)%oxided   = .FALSE.
             Branches(icdan)%oxi      = .FALSE.
          ELSE
             Branches(icdan)%oxided = .TRUE.

             !Modification en fonction de la discussion avec Daniel: R*I^2 = a^3*Cp(Tf-T0)
             call get_eff( id_inter, icdan, meff, reff )
             Branches(icdan)%threshold = 0.75*MODelec%breakdown_threshold*reff*Branches(icdan)%rln/H
             
             DO iadj = 1,nbadj
                IF(iannode.EQ.Vnodes(icdnode)%adj(iadj))THEN
                   Branches(icdan)%oxided   = Vnodes(icdnode)%oxided(iadj)
                   Branches(icdan)%threshold = Vnodes(icdnode)%threshold(iadj)
                   nb_RECUP = nb_RECUP + 1
                   EXIT
                END IF
             END DO
          END IF
       END DO
    END IF
!!!
    IF ( inet == 0 ) RETURN
!!!
    WRITE(*,*) ' @ NB ELECTRO RECUP : ',nb_RECUP
    WRITE(*,*) ' @ NB OXIDED SLIDING: ',nb_SLDOXI
    
!!! matrix construction ------------------------------------------------

    CALL conductivity_matrix_construction

!!! Initialization -----------------------------------------------------

    IF (MODelec%init) THEN
       DO inode = 1,nb_NODES
          ibdyty = NODES(inode)%ID_RBDY3
          itacty = NODES(inode)%ID_TACTY
          Nodes(inode)%V = get_electric_potentiel(ibdyty,itacty)
       END DO
    ELSE
       DO inode = 1,nb_NODES
          Nodes(inode)%V = 0.0
       END DO
    END IF

!!! Start non linear iteration -------------------------------------------

    NLiter = 0

    DO inode = 1,nb_NODES
       Nodes(inode)%Vik = Nodes(inode)%V
    END DO

    DO 

       NLiter = NLiter + 1
       
       IF( NLiter > MODelec%NLitermax ) EXIT

!!!---- Start linear iteration -------------------------------------------

       iter   = 0

       DO
          
          iter = iter + 1
          
          IF( iter > MODelec%itermax ) EXIT
          
          E_ERR = 0.D0
          
          DO icdnode = 1,nb_NODES
             
             IF ( ABS(Nodes(icdnode)%Cii) >= 1.D-14 ) THEN
                
                Itot  = Nodes(icdnode)%Ic
                nbadj = Nodes(icdnode)%nbadj

                DO iadj = 1,nbadj
                   iannode = Nodes(icdnode)%adj(iadj)
                   Itot    = Itot - Nodes(iannode)%Vik*Nodes(icdnode)%Cij(iadj)
                END DO
                
                Pot = Itot/Nodes(icdnode)%Cii
                
                IF( ABS(Pot) > 1.D-16 ) THEN
                   Apot = ABS( (Nodes(icdnode)%Vik-Pot )/Pot)
                ELSE
                   Apot = ABS( Nodes(icdnode)%Vik-Pot )
                END IF
                
                Nodes(icdnode)%Vik = Pot
                E_ERR = MAX( E_ERR , Apot )

             END IF
             
          END DO

          IF ( E_ERR < MODelec%TOL ) EXIT
          
       END DO

!!!---- stop linear iteration ---------------------------------------------------

       Noxide = 0
       Pmean  = 0
       Pmax   =-1.D+24
       Pmin   = 1.D+24
 
       DO inode = 1,nb_NODES
          Nodes(inode)%iadj = 0
       END DO

       DO icdan = 1,nb_CDAN

          icdnode = Branches(icdan)%icd
          iannode = Branches(icdan)%ian
          
          Nodes(icdnode)%iadj = Nodes(icdnode)%iadj + 1
          Nodes(iannode)%iadj = Nodes(iannode)%iadj + 1
          
          IF (Branches(icdan)%Active == 1) THEN
             
             Branches(icdan)%U  = Nodes(iannode)%Vik - Nodes(icdnode)%Vik
             Branches(icdan)%UI = Branches(icdan)%U*Branches(icdan)%U*Branches(icdan)%Calpha

             Pmean = Pmean + Branches(icdan)%UI
             Pmax  = MAX(Pmax,Branches(icdan)%UI)
             Pmin  = MIN(Pmin,Branches(icdan)%UI)

             IF (Branches(icdan)%oxided) THEN
                IF (Branches(icdan)%oxi) THEN
                   IF ( Branches(icdan)%UI .GE. Branches(icdan)%threshold ) THEN 
                      Noxide = Noxide + 1
                      Branches(icdan)%oxi   = .FALSE.
                      Nodes(icdnode)%Cii = Nodes(icdnode)%Cii &
                           - Branches(icdan)%Coxi + Branches(icdan)%Ctot
                      Nodes(iannode)%Cii = Nodes(iannode)%Cii &
                           - Branches(icdan)%Coxi + Branches(icdan)%Ctot
                      
                      Nodes(icdnode)%Cij(Nodes(icdnode)%iadj)  = -Branches(icdan)%Ctot
                      Nodes(iannode)%Cij(Nodes(iannode)%iadj)  = -Branches(icdan)%Ctot
                      Branches(icdan)%Calpha = Branches(icdan)%Ctot
                   END IF
                ELSE
                   IF (.NOT. Branches(icdan)%oxi) THEN
                      IF ( Branches(icdan)%UI .LT. Branches(icdan)%threshold ) THEN 
                         Noxide = Noxide + 1
                         Branches(icdan)%oxi   = .TRUE.
                         Nodes(icdnode)%Cii = Nodes(icdnode)%Cii &
                              + Branches(icdan)%Coxi - Branches(icdan)%Ctot
                         Nodes(iannode)%Cii = Nodes(iannode)%Cii &
                              + Branches(icdan)%Coxi - Branches(icdan)%Ctot
                         
                         Nodes(icdnode)%Cij(Nodes(icdnode)%iadj)  = -Branches(icdan)%Coxi
                         Nodes(iannode)%Cij(Nodes(iannode)%iadj)  = -Branches(icdan)%Coxi
                         Branches(icdan)%Calpha = Branches(icdan)%Coxi
                      END IF
                   END IF
                END IF
             END IF
          END IF
       END DO

       IF ( Noxide .LE. NL_ERR .AND. &
            E_ERR  .LE. MODelec%TOL) EXIT
       
    END DO
    
!!! stop non linear iteration ---------------------------------------------------

   DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3
       itacty = Nodes(inode)%ID_TACTY
       CALL put_electric_potentiel(ibdyty,itacty,Nodes(inode)%Vik)
       Nodes(inode)%V = Nodes(inode)%Vik
    END DO

!!! Stock information for next time step

!!! clean struture ---------------------------------------------------------
    
    IF (first_time) THEN
       first_time = .FALSE.
       DO inode = 1,nb_NODES
          Vnodes(inode)%iadj  = 0
          Vnodes(inode)%nbadj = 0
       END DO
    ELSE
       DO inode = 1,nb_NODES
          IF(ASSOCIATED(Vnodes(inode)%adj))       DEALLOCATE(Vnodes(inode)%adj)
          IF(ASSOCIATED(Vnodes(inode)%oxided))    DEALLOCATE(Vnodes(inode)%oxided)
          IF(ASSOCIATED(Vnodes(inode)%threshold)) DEALLOCATE(Vnodes(inode)%threshold)
          IF(ASSOCIATED(Vnodes(inode)%UI))        DEALLOCATE(Vnodes(inode)%UI)
          NULLIFY(Vnodes(inode)%adj)
          NULLIFY(Vnodes(inode)%oxided)
          NULLIFY(Vnodes(inode)%threshold)
          NULLIFY(Vnodes(inode)%UI)
          Vnodes(inode)%iadj  = 0
          Vnodes(inode)%nbadj = 0
       END DO
    END IF
    
!!! resize structure -------------------------------------------------------

    nb_OXID = 0
    nb_changed_OXID = 0

    DO icdan = 1,nb_CDAN
       icdnode = Branches(icdan)%icd
       Vnodes(icdnode)%iadj = Vnodes(icdnode)%iadj + 1

!fd faux au niveau du langage je pense que c'est pas autorise de tester des logiques
!fd IF (Branches(icdan)%oxided .NE. Branches(icdan)%oxi) nb_changed_OXID = nb_changed_OXID + 1
!mr
       IF ((    Branches(icdan)%oxided     .AND.     Branches(icdan)%oxi)    .or. &
           ((.not. Branches(icdan)%oxided) .AND. (.not. Branches(icdan)%oxi))     &
          ) nb_changed_OXID = nb_changed_OXID + 1

       Branches(icdan)%oxided = Branches(icdan)%oxi
       IF (Branches(icdan)%oxided) nb_OXID = nb_OXID + 1

    END DO

    DO inode=1,nb_NODES
       nbadj = Vnodes(inode)%iadj
       Vnodes(inode)%nbadj = nbadj 
       Vnodes(inode)%iadj  = 0
       IF (nbadj .NE. 0) THEN
          ALLOCATE(Vnodes(inode)%adj(nbadj))
          ALLOCATE(Vnodes(inode)%oxided(nbadj))
          ALLOCATE(Vnodes(inode)%threshold(nbadj))
          ALLOCATE(Vnodes(inode)%UI(nbadj))
          Vnodes(inode)%threshold = 0.0
          Vnodes(inode)%oxided   = .FALSE.
          Vnodes(inode)%adj      = 0
          Vnodes(inode)%UI       = 0.0
       ELSE
          NULLIFY(Vnodes(inode)%adj)
          NULLIFY(Vnodes(inode)%oxided)
          NULLIFY(Vnodes(inode)%threshold)
          NULLIFY(Vnodes(inode)%UI)
       END IF
    END DO

!!! fill structure ---------------------------------------------------------

    DO icdan = 1,nb_CDAN
       icdnode = Branches(icdan)%icd
       iannode = Branches(icdan)%ian
       Vnodes(icdnode)%iadj = Vnodes(icdnode)%iadj + 1
       Vnodes(icdnode)%adj(Vnodes(icdnode)%iadj)       = iannode
       Vnodes(icdnode)%oxided(Vnodes(icdnode)%iadj)    = Branches(icdan)%oxided
       Vnodes(icdnode)%threshold(Vnodes(icdnode)%iadj) = Branches(icdan)%threshold
       Vnodes(icdnode)%UI(Vnodes(icdnode)%iadj) = Branches(icdan)%UI
    END DO

    RETURN

  END SUBROUTINE solve_nl_electro1G
!--------------------------------------------------------------------------------------
  subroutine fill_active_contact_aux( id_inter, contactor1_2nodes, contactor2_2nodes, &
       icdan, icdtac, iantac, itact, &
       Branches, Nodes, inet )

    implicit none

    integer( kind = 4 ) :: id_inter
    integer( kind = 4 ), dimension( : ), allocatable :: contactor1_2nodes
    integer( kind = 4 ), dimension( : ), allocatable :: contactor2_2nodes
    integer( kind = 4 ) :: icdan
    integer( kind = 4 ) :: icdtac
    integer( kind = 4 ) :: iantac
    integer( kind = 4 ) :: itact
    TYPE(T_BRANCHE), dimension( : ), allocatable :: Branches  
    TYPE(T_NODES)  , dimension( : ), allocatable :: Nodes
    integer( kind = 4 ) :: inet

    ! Local variables
    integer( kind = 4 ) :: icdnode
    integer( kind = 4 ) :: iannode
    integer( kind = 4 ) :: status
    real( kind = 8 )    :: rls
    real( kind = 8 )    :: rln
    real( kind = 8 )    :: rlt
    real( kind = 8 )    :: vls
    real( kind = 8 )    :: vlt
    real( kind = 8 )    :: vln
    real( kind = 8 )    :: reff
    real( kind = 8 )    :: meff

    icdnode = contactor1_2nodes( icdtac )
    iannode = contactor2_2nodes( iantac )

    Branches(icdan)%icd = icdnode
    Branches(icdan)%ian = iannode

    Nodes(icdnode)%iadj = Nodes(icdnode)%iadj + 1
    Nodes(iannode)%iadj = Nodes(iannode)%iadj + 1

    call get_rloc( id_inter, itact, rlt, rln, rls, status )
    call get_vloc( id_inter, itact, vlt, vln, vls )

    Branches(icdan)%rln    = rln
    Branches(icdan)%vlt    = sqrt( vlt*vlt + vls*vls )
    Branches(icdan)%Active = 0

    call get_eff( id_inter, itact, meff, reff )
    Branches(icdan)%reff = reff

    select case(MODelec%local)
    case(i_l_allno)
       if ( abs(rln) > 1.D-16 ) then
          Branches(icdan)%Active = 1
          inet = inet + 1
       end if
    case(i_l_Hertz)
       if ( ABS(rln) > 1.D-16 ) then
          Branches(icdan)%Active = 1
          inet = inet + 1
       end if
    end select

  end subroutine fill_active_contact_aux
!!!--------------------------------------------------------------------------------------
  SUBROUTINE fill_active_contact(inet)

    IMPLICIT NONE

    INTEGER      :: inet,icdan,icdnode,iannode
    INTEGER      :: icdtac,iantac,itact

    CHARACTER(len=5) :: status

    REAL(kind=8) :: rls,rln,rlt,vls,vlt,vln,reff,meff

    icdan = 0

    DO itact = 1, nb_SPSPx

       icdan = icdan + 1

       CALL SPSPx2SPHER(itact,icdtac,iantac)

       call fill_active_contact_aux( i_spspx, spher2nodes, spher2nodes, icdan, icdtac, iantac, itact, &
            Branches, Nodes, inet )

    END DO

    DO itact = 1, nb_SPPLx

       icdan = icdan + 1

       CALL SPPLx2SPHER(itact,icdtac)
       CALL SPPLx2PLANx(itact,iantac)

       call fill_active_contact_aux( i_spplx, spher2nodes, planx2nodes, icdan, icdtac, iantac, itact, &
            Branches, Nodes, inet )

    END DO

    DO itact = 1, nb_SPDCx

       icdan = icdan + 1

       CALL SPDCx2SPHER(itact,icdtac)
       CALL SPDCx2DNLYC(itact,iantac)

       call fill_active_contact_aux( i_spdcx, spher2nodes, dnlyc2nodes, icdan, icdtac, iantac, itact, &
            Branches, Nodes, inet )

    END DO

    DO itact = 1, nb_SPCDx

       icdan = icdan + 1

       CALL SPCDx2SPHER(itact,icdtac)
       CALL SPCDx2CYLND(itact,iantac)

       call fill_active_contact_aux( i_spcdx, spher2nodes, cylnd2nodes, icdan, icdtac, iantac, itact, &
            Branches, Nodes, inet )

    END DO

  END SUBROUTINE fill_active_contact
!!!--------------------------------------------------------------------------------------
  SUBROUTINE conductivity_matrix_construction

    IMPLICIT NONE

    INTEGER :: inode,icdan,inet,icdnode,iannode
    INTEGER :: adjsz

    REAL(kind=8) :: Ctot,Conby,Concd,Conan,Conct,Calpha
    REAL(kind=8) :: rln
    
    DO inode = 1,nb_NODES
       
       adjsz = Nodes(inode)%iadj
       Nodes(inode)%nbadj = adjsz

       IF (ASSOCIATED(Nodes(inode)%adj)) DEALLOCATE(Nodes(inode)%adj)
       IF (ASSOCIATED(Nodes(inode)%Cij)) DEALLOCATE(Nodes(inode)%Cij)

       IF ( adjsz /= 0 ) THEN
          ALLOCATE(Nodes(inode)%adj(adjsz))
          ALLOCATE(Nodes(inode)%Cij(adjsz))
       ELSE
          NULLIFY(Nodes(inode)%adj)
          NULLIFY(Nodes(inode)%Cij)
       END IF
       
       Nodes(inode)%Cii  = 0.0
       Nodes(inode)%iadj = 0

    END DO

    inet = 0

    DO icdan = 1,nb_CDAN
       
       icdnode = Branches(icdan)%icd
       iannode = Branches(icdan)%ian

       Ctot = 0.0
       
       IF (Branches(icdan)%Active .EQ. 1) THEN
          
          inet = inet + 1
          
          Concd = get_elec_cond(Nodes(icdnode)%ID_RBDY3,Nodes(icdnode)%ID_TACTY)
          Conan = get_elec_cond(Nodes(iannode)%ID_RBDY3,Nodes(iannode)%ID_TACTY)
          
          SELECT CASE(MODelec%local)

          CASE(i_l_allno)
             Conby = Concd*Conan
             IF ( Conby > 1.D-18 ) Ctot = Conby/(Concd+Conan)
          CASE(i_l_Hertz)
             rln = ABS(Branches(icdan)%rln)
             Conct = (rln/H)**(0.33333)
             Conby = Concd*Conan             
             IF ( Conby > 1.D-18 .AND. Conct > 1.D-18 ) Ctot = Conby/(Concd+Conan+Conby/Conct)
          CASE DEFAULT
             CALL LOGMES('ELECTRO ERROR')
             STOP
          END SELECT
          
       END IF
       
       Branches(icdan)%Ctot = Ctot
       Branches(icdan)%U    = 0.0
       Branches(icdan)%UI   = 0.0
       Branches(icdan)%oxi  = Branches(icdan)%oxided
       
       IF (Branches(icdan)%oxided) THEN
          Branches(icdan)%Coxi     = Ctot*MODelec%Coxi/(Ctot+MODelec%Coxi)
          Calpha = Branches(icdan)%Coxi
       ELSE
          Calpha = Branches(icdan)%Ctot
       END IF
       
       Branches(icdan)%Calpha = Calpha
       
       Nodes(icdnode)%iadj = Nodes(icdnode)%iadj + 1
       Nodes(iannode)%iadj = Nodes(iannode)%iadj + 1
       
       Nodes(icdnode)%Cii = Nodes(icdnode)%Cii + Calpha
       Nodes(iannode)%Cii = Nodes(iannode)%Cii + Calpha
       
       Nodes(icdnode)%adj(Nodes(icdnode)%iadj) = iannode
       Nodes(iannode)%adj(Nodes(iannode)%iadj) = icdnode
       
       Nodes(icdnode)%Cij(Nodes(icdnode)%iadj)  = -Calpha
       Nodes(iannode)%Cij(Nodes(iannode)%iadj)  = -Calpha
       
    END DO

  END SUBROUTINE conductivity_matrix_construction
!--------------------------------------------------------------------------------------
  subroutine solve_thermo_mp_solver_aux( id_inter, icdan, icdtac, iantac, contactor1_2nodes, contactor2_2nodes, &
       QCij, QSij )

    implicit none

    integer :: id_inter
    integer :: icdan
    integer :: icdtac
    integer :: iantac
    integer, dimension(:), allocatable :: contactor1_2nodes
    integer, dimension(:), allocatable :: contactor2_2nodes
    real(kind=8) :: QCij
    real(kind=8) :: QSij

    ! Local variables
    integer       :: icdnode, iannode
    integer       :: lawnb
    integer       :: status
    real(kind= 8) :: rlt, rln, rls
    real(kind= 8) :: vlt, vln, vls
    real(kind= 8) :: vltBegin, vlnBegin, vlsBegin, gapBEGIN
    real(kind= 8) :: meff, reff

    call get_eff( id_inter, icdan, meff, reff )

    icdnode = contactor1_2nodes(icdtac)
    iannode = contactor2_2nodes(iantac)

    call get_vlocBEGIN( id_inter, icdan, vltBegin, vlnBegin, vlsBegin, gapBEGIN, status )
    call get_vloc( id_inter, icdan, vlt, vln, vls)
    call get_rloc( id_inter, icdan, rlt, rln, rls, status )

!!!### GENERATION

    call comp_generation_part( icdan, icdnode, iannode, status, &
                               rln, rlt, rls, vln, vlt, vls, vlnBEGIN, vltBEGIN, vlsBegin, QSij )

!!!### DIFFUSION

    lawnb = get_tact_lawnb( id_inter, icdan )

    call comp_diffusive_part( lawnb, icdnode, iannode, rln, reff, QCij)

!!!### #########

    ! call put_heat_sources_SPSPx( icdan, QCij, QSij )

  end subroutine solve_thermo_mp_solver_aux
!!!--------------------------------------------------------
!!!****f* MP_SOLVER/solve_thermo_mp_solver
!!! NAME
!!!   solve_thermo_mp_solver
!!! PURPOSE
!!!
!!!****
!!!--------------------------------------------------------
  SUBROUTINE solve_thermo_mp_solver

    IMPLICIT NONE

    INTEGER :: ibdyty,inode,icdan,icdtac,iantac,ibound,iadj
    INTEGER :: icdnode,iannode,itact,itacty,setnb,lawnb
    INTEGER :: nb_tacty,ibehav

    REAL(kind=8)     :: Concd,Conan,Coneff,Qij,Hspe,meff,reff,dist,gapBegin,gap,QSij,QCij
    REAL(kind=8)     :: NUTHan,ETHcd,NUTHcd,ETHan,ETHeff,Us,Un,Ut
    REAL(kind=8)     :: rls,rlt,rln,vls,vlsBEGIN,vln,vlnBEGIN,vlt,vltBEGIN,DV2,forcePERgap,area,normalcoh,tangalcoh,Wethk

    CHARACTER(len=5) :: status
    LOGICAL          :: FLAG

!!!--------------------------------------------------------
!    PRINT*,' *** INITIALISATION ***'

    GLOBAL_DV2 = 0.D0
    GLOBAL_PV  = 0.D0
    GLOBAL_DPV = 0.D0
    GLOBAL_QIJ = 0.D0
    GLOBAL_AQIJ = 0.D0
    GLOBAL_AREA = 0.D0

    IF(MODtherm%init)THEN
       DO inode=1,nb_NODES
          Nodes(inode)%Tini = Nodes(inode)%T
          Nodes(inode)%dT   = 0.D0
       END DO
    ELSE
       DO inode=1,nb_NODES
          Nodes(inode)%Tini = 0.D0
          Nodes(inode)%T    = 0.D0
          Nodes(inode)%dT   = 0.D0
       END DO
    END IF

!!!--------------------------------------------------------

    nb_CDAN = 0
    
    nb_SPSPx = get_nb_inters( i_spspx )
    nb_CDAN = nb_CDAN + nb_SPSPx

    nb_SPPLx = get_nb_inters( i_spplx )
    nb_CDAN = nb_CDAN + nb_SPPLx

    nb_SPCDx = get_nb_inters( i_spcdx )
    nb_CDAN = nb_CDAN + nb_SPCDx

    nb_SPDCx = get_nb_inters( i_spdcx )
    nb_CDAN = nb_CDAN + nb_SPDCx

!!!--------------------------------------------------------

    DO icdan = 1, nb_SPSPx


       CALL SPSPx2SPHER( icdan, icdtac, iantac )

       call solve_thermo_mp_solver_aux( i_spspx, icdan, icdtac, iantac, spher2nodes, spher2nodes, QCij, QSij )
     
       CALL put_heat_sources_SPSPx( icdan, QCij, QSij )

    END DO

    DO icdan = 1, nb_SPPLx

       CALL SPPLx2SPHER( icdan, icdtac )
       CALL SPPLx2PLANx( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_spplx, icdan, icdtac, iantac, spher2nodes, planx2nodes, QCij, QSij )

    END DO

    DO icdan = 1, nb_SPDCx

       CALL SPDCx2SPHER( icdan, icdtac )
       CALL SPDCx2DNLYC( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_spdcx, icdan, icdtac, iantac, spher2nodes, dnlyc2nodes, QCij, QSij )

    END DO

    DO icdan = 1, nb_SPCDx

       CALL SPCDx2SPHER( icdan, icdtac )
       CALL SPCDx2CYLND( icdan, iantac )

       call solve_thermo_mp_solver_aux( i_spcdx, icdan, icdtac, iantac, spher2nodes, cylnd2nodes, QCij, QSij )

    END DO

!!! TO DO PLPLx AND PLJCx AND DKPLx

!!!---------------------------------------------------------
!!! Multi-tactors treatment 

    DO inode=1,nb_MLT_NODES

       icdnode = MultiNodes(inode)%CD
       iannode = MultiNodes(inode)%AN

       CALL comp_eff_value(icdnode,iannode,Coneff,ETHeff)

       Qij = 2.0*Coneff*( Nodes(iannode)%Tini - Nodes(icdnode)%Tini )*MultiNodes(inode)%Area

       Nodes(icdnode)%dT = Nodes(icdnode)%dT + Qij
       Nodes(iannode)%dT = Nodes(iannode)%dT - Qij

    END DO

!!!---------------------------------------------------------
!!! BOUNDARY CONDITION

    SELECT CASE(MODtherm%ibound)
!!!* ADIABATIC BEHAVIOUR *
    CASE(i_d_adia)

!!!* LINEAR EVOLUTION IN THE FIRST BODY THICKNESS *
    CASE(i_d_line)

       DO ibound=1,nb_TH_BOUNDS

          DO inode=1,TH_BND_NODES(ibound)%NX
             
             icdnode = TH_BND_NODES(ibound)%TSOURCE(inode)
             
             reff    = get_avr_radius_tacty(Nodes(icdnode)%ID_RBDY3,Nodes(icdnode)%ID_TACTY)
             Coneff  = get_therm_cond(Nodes(icdnode)%ID_RBDY3,Nodes(icdnode)%ID_TACTY)
             
             Qij = PI_g*reff*Coneff*reff*( TH_BND_NODES(ibound)%T - Nodes(inode)%Tini )/TH_BND_NODES(ibound)%thickness
             Nodes(inode)%dT = Nodes(inode)%dT + Qij          
             
             GLOBAL_AQIJ = GLOBAL_AQIJ + Qij
             
          END DO

       END DO

!!!* D. RICHARD APPROACH *
    CASE(i_d_1D__)
      DO ibound=1,nb_TH_BOUNDS
          CALL COMP_T_BULK(ibound)
       END DO
!!!* M. RENOUF APPROACH *
    CASE(i_d_3D__)
       DO ibound=1,nb_TH_BOUNDS
          CALL COMP_T_BULK(ibound)
       END DO
    CASE default
       CALL LOGMES('Boundary conditions not found')
       CALL LOGMES('Please, correct the ELECTRO.DAT file.')
       STOP
    END SELECT

!!!---------------------------------------------------------

    IF(FREE_BOUNDARY)THEN
       DO inode=1,nb_NODES
          
          CALL comp_convexion_part(inode,Qij)

          Nodes(inode)%dT = Nodes(inode)%dT + Qij          
          
          GLOBAL_AQIJ = GLOBAL_AQIJ + Qij

       END DO
    END IF

    IF(MODtherm%lconv)THEN

       DO inode=1,nb_NODES
          
          CALL comp_convexion_part(inode,Qij)
          !PRINT*,inode,Qij
          Nodes(inode)%dT = Nodes(inode)%dT + Qij          
          
          GLOBAL_AQIJ = GLOBAL_AQIJ + Qij
          
       END DO
       
    END IF

!!!---------------------------------------------------------

    DO inode=1,nb_NODES
       Nodes(inode)%dT = Nodes(inode)%dT*Nodes(inode)%alpha
    END DO

!!!---------------------------------------------------------
!!! Note: The time step H is already take into account in the alpha
!    PRINT*,' *** SOLUTION ***'

    DO inode=1,nb_NODES

       ibdyty = Nodes(inode)%ID_RBDY3
       itacty = Nodes(inode)%ID_TACTY

       IF(Nodes(inode)%AM_I_TH_SRC) THEN
          Nodes(inode)%T     = TH_SRC_NODES(Nodes(inode)%iTHsource)%T
       ELSE
          Nodes(inode)%T     = Nodes(inode)%Tini + (1-THETA)*Nodes(inode)%dTini + THETA*Nodes(inode)%dT
          Nodes(inode)%dTini = Nodes(inode)%dT
       END IF

       CALL put_thermal_value(ibdyty,itacty,Nodes(inode)%T)

    END DO

  END SUBROUTINE solve_thermo_mp_solver
!!!--------------------------------------------------------------------------------------
  SUBROUTINE update_compuctivity_mp_solver

    IMPLICIT NONE


  END SUBROUTINE update_compuctivity_mp_solver
!!!--------------------------------------------------------------------------------------
  SUBROUTINE update_thermo_mp_solver

    IMPLICIT NONE

  END SUBROUTINE update_thermo_mp_solver
!!!--------------------------------------------------------------------------------------
  LOGICAL FUNCTION get_write_mp_values(fantome)

    IMPLICIT NONE
    INTEGER,optional :: fantome

    get_write_mp_values = write_mpv
    
  END FUNCTION get_write_mp_values
!!!--------------------------------------------------------------------------------------
  SUBROUTINE get_global_3D_thermal_variable(GPV,GDPV,GDV2,GQIJ,GAQIJ,GA)

    IMPLICIT NONE
    INTEGER(kind=4) :: NA
    REAL(kind=8) :: GPV,GDPV,GDV2,GQIJ,GAQIJ,GA

    GPV  = GLOBAL_PV  
    GDPV = GLOBAL_DPV 
    GDV2 = GLOBAL_DV2 
    GQIJ = GLOBAL_QIJ 
    GAQIJ = GLOBAL_AQIJ

    NA = MAX(1,nb_CDAN)
    GA   = GLOBAL_AREA/REAL(NA,8)


  END SUBROUTINE get_global_3D_thermal_variable
!!!--------------------------------------------------------------------------------------
  LOGICAL FUNCTION get_oxided_tactor(icdan)

    IMPLICIT NONE

    INTEGER :: icdan

    IF ( ALLOCATED(Branches) ) THEN
       get_oxided_tactor = Branches(icdan)%oxided
    ELSE
       get_oxided_tactor = .TRUE.
    END IF

  END FUNCTION get_oxided_tactor
!!!--------------------------------------------------------------------------------------
  SUBROUTINE get_electro_info(it,err,NLit,Cm,nbo,nbco,nbso)

    IMPLICIT NONE

    INTEGER      :: it,NLit,nbo,nbco,nbso
    REAL(kind=8) :: err,NLerr,Cm

    it    = iter 
    err   = E_ERR
    NLit  = NLiter

    Cm    = Cmean

    nbo   = nb_OXID
    nbco  = nb_changed_OXID
    nbso  = nb_SLDOXI

  END SUBROUTINE get_electro_info
!!!--------------------------------------------------------------------------------------
  SUBROUTINE active_recup(FLAG)

    IMPLICIT NONE
    CHARACTER(len=1) :: FLAG

    SELECT CASE(FLAG)
    CASE('T')
       MODtherm%init = .TRUE.
    CASE('P')
       MODelec%init = .TRUE.
    CASE DEFAULT
    END SELECT

  END SUBROUTINE active_recup
!!!--------------------------------------------------------------------------------------
  SUBROUTINE write_out_mp_behaviour_mp_solver
    !!****u* CORE.MPSOLVER/write_out_mp_behaviour_mp_solver
    !! NAME
    !!  write_out_mp_behaviour_mpsolver
    !! PURPOSE
    !!  
    !!  
    !!****
    IMPLICIT NONE

    INTEGER :: inode,itact,nfich

    nfich = get_io_unit()

    OPEN(unit=nfich,file=trim(location(out_mpdem(:))))

    IF(electro_model)THEN
       WRITE(nfich,'(A14)') '$model  elec_:'
       SELECT CASE(MODelec%current)
       CASE(i_c_cons)              !12345678901
          WRITE(nfich,'(14X,A11)') 'curnt: cons'
          WRITE(nfich,'(18X,1(2X,D14.7))') MODelec%A
       CASE(i_c_line)
          WRITE(nfich,'(14X,A11)') 'curnt: line'
          WRITE(nfich,'(18X,2(2X,D14.7))') MODelec%A,MODelec%B
       CASE(i_c_alte)
          WRITE(nfich,'(14X,A11)') 'curnt: alte'
          WRITE(nfich,'(18X,4(2X,D14.7))') MODelec%A,MODelec%B,MODelec%Omega,MODelec%Phi
       END SELECT
       SELECT CASE(MODelec%local)
       CASE(i_l_Hertz)             !123456789012
          WRITE(nfich,'(14X,A12)') 'local: Hertz'
       CASE(i_l_allno)
          WRITE(nfich,'(14X,A12)') 'local: allno'
       END SELECT
       WRITE(nfich,'(21X,A6,I7)')    'iter_:',MODelec%itermax
       WRITE(nfich,'(21X,A6,D14.7)') 'tol__:',MODelec%TOL
       IF (MODelec%oxide) THEN     !1234567890
          WRITE(nfich,'(14X,A10)') 'oxide: yes'
          WRITE(nfich,'(21X,A6,D14.7)') 'Cond_:',MODelec%Coxi
          WRITE(nfich,'(21X,A6,I7)')    'iter_:',MODelec%NLitermax
          WRITE(nfich,'(21X,A6,D14.7)') 'tol__:',MODelec%NLTOL
       ELSE                        
          WRITE(nfich,'(14X,A10)') 'oxide: no '
       END IF
       IF (MODelec%breakdown) THEN !1234567890
          WRITE(nfich,'(14X,A10)') 'brkdw: yes'
          WRITE(nfich,'(21X,A6,D14.7)') 'brkth:',MODelec%breakdown_threshold
          WRITE(nfich,'(21X,A6,D14.7)') 'var__:',MODelec%breakdown_var
       ELSE                        
          WRITE(nfich,'(14X,A10)') 'brkth: no '
       END IF
       IF (MODelec%sliding) THEN   !1234567890
          WRITE(nfich,'(14X,A10)') 'sldng: yes'
          WRITE(nfich,'(21X,A6,D14.7)') 'sldth:',MODelec%sliding_threshold
       ELSE                        
          WRITE(nfich,'(14X,A10)') 'sldng: no '
       END IF
    END IF

    IF(thermo_model)THEN
       WRITE(nfich,'(A14)') '$model  therm:'
       WRITE(nfich,'(14X,A6,D14.7)') 'T0___:',MODtherm%T0
       WRITE(nfich,'(14X,A6,D14.7)') 'alert:',MODtherm%Alert
       IF (MODtherm%lconv) THEN     !1234567890
          WRITE(nfich,'(14X,A10)') 'lconv: yes'
          WRITE(nfich,'(14X,A13)') '       Gcond:',MODtherm%Gcond
       ELSE                        
          WRITE(nfich,'(14X,A10)') 'lconv: no '
       END IF

       SELECT CASE(MODtherm%ilkine)
       CASE(i_lkine_all)              !12345678901
          WRITE(nfich,'(14X,A10)') 'lkine: all'
       CASE(i_lkine_dvn)
          WRITE(nfich,'(14X,A10)') 'lkine: dvn'
       CASE(i_lkine_dvt)
          WRITE(nfich,'(14X,A10)') 'lkine: dvt'
       CASE(i_lkine_no)
          WRITE(nfich,'(14X,A10)') 'lkine: no '
       END SELECT

       SELECT CASE(MODtherm%ibound)
       CASE(i_d_adia)              !12345678901
          WRITE(nfich,'(14X,A11)') 'bound: adia'
       CASE(i_d_line)
          WRITE(nfich,'(14X,A11)') 'bound: line'
       CASE(i_d_1D__)
          WRITE(nfich,'(14X,A11)') 'bound: 1D__'
       CASE(i_d_3D__)
          WRITE(nfich,'(14X,A11)') 'bound: 3D__'
       END SELECT
    END IF

    DO inode = 1,nb_TH_SOURCES
       WRITE(nfich,'(A1)') ' '
       WRITE(nfich,'(A14)') '$source  therm'
       WRITE(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D14.7)') &
            TH_SRC_NODES(inode)%ifirst,'to',TH_SRC_NODES(inode)%ilast,'T0__=',TH_SRC_NODES(inode)%T
    END DO

    DO inode = 1,nb_TH_BOUNDS
       WRITE(nfich,'(A1)') ' '
       WRITE(nfich,'(A14)') '$bounds  therm'
       WRITE(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D14.7,2X,A5,D14.7)') &
            TH_BND_NODES(inode)%ifirst,'to',TH_BND_NODES(inode)%ilast, &
            'T0__=',TH_BND_NODES(inode)%T,'Thck=',TH_BND_NODES(inode)%thickness
    END DO

    DO inode = 1,nb_EL_BOUNDS
       WRITE(nfich,'(A1)') ' '
       WRITE(nfich,'(A14)') '$bounds  elec_'
       WRITE(nfich,'(9X,I7,2X,A2,2X,I7,2X,A5,D8.1)') &
            EL_BND_NODES(inode)%ifirst,'to',EL_BND_NODES(inode)%ilast,'Isgn=',EL_BND_NODES(inode)%sgnI
    END DO

    CLOSE(nfich)

  END SUBROUTINE write_out_mp_behaviour_mp_solver
!!!------------------------------------------------------
  SUBROUTINE comp_eff_value(icd,ian,cond,E)

    IMPLICIT NONE
    INTEGER      :: icd,ian,ibdyty,itacty,ibehav
    REAL(kind=8) :: cond,E
    REAL(kind=8) :: concd,conan,Ean,NUan,Ecd,NUcd

    ibdyty = Nodes(icd)%ID_RBDY3
    itacty = Nodes(icd)%ID_TACTY

    concd  = get_therm_cond(ibdyty,itacty)

    ibehav = get_bulk_behav_number_RBDY3(ibdyty,1)
    CALL get_equivalent_mat_prop(ibehav,Ecd,NUcd)

    ibdyty = Nodes(ian)%ID_RBDY3
    itacty = Nodes(ian)%ID_TACTY
    
    ibehav = get_bulk_behav_number_RBDY3(ibdyty,1)
    CALL get_equivalent_mat_prop(ibehav,Ean,NUan)

    conan  = get_therm_cond(ibdyty,itacty)
    
    IF ( concd + conan < 1.D-16 ) THEN
       cond = 0.0
    ELSE
       cond = 2.0*concd*conan/(concd + conan)
    END IF
    
    IF ( Ecd + Ean < 1.D-16 ) THEN
       E = 0.0
    ELSE
       E = Ecd*Ean/(Ecd*(1-NUan*NUan) + Ean*(1-NUcd*NUcd))
    END IF

  END SUBROUTINE comp_eff_value
!!!------------------------------------------------------
  SUBROUTINE comp_diffusive_part(ilaw,icd,ian,rln,reff,Qij)

    IMPLICIT NONE
    INTEGER      :: ilaw,icd,ian
    REAL(kind=8) :: rln,reff,AREA
    REAL(kind=8) :: cohn,coht,dw,Qij,ETHeff,Coneff

    CALL comp_eff_value(icd,ian,Coneff,ETHeff)

    CALL get_coh(ilaw,cohn,coht,dw)

    AREA = (0.75*reff*MAX(0.D0,rln/H+cohn)/ETHeff)**0.3333333D+00
    Qij = 2.0*Coneff*( Nodes(ian)%Tini - Nodes(icd)%Tini )*AREA
    
    Nodes(icd)%dT = Nodes(icd)%dT + Qij
    Nodes(ian)%dT = Nodes(ian)%dT - Qij

    GLOBAL_QIJ  = GLOBAL_QIJ  + ABS(Qij)
    GLOBAL_AREA = GLOBAL_AREA + AREA/(2.0*reff)

  END SUBROUTINE comp_diffusive_part
!!!------------------------------------------------------
  SUBROUTINE comp_generation_part(icdan,icd,ian,status, &
                                  rln,rlt,rls,vln,vlt,vls,vlni,vlti,vlsi,QSij)

    IMPLICIT NONE
    INTEGER(kind=4) :: icdan,icd,ian
    REAL(kind=8)    :: rln,rlt,rls,vln,vlt,vls,vlni,vlti,vlsi
    REAL(kind=8)    :: dv2,Un,Ut,Us,QSij

    integer(kind=4) :: status

    dv2 = 0.0

    IF ((status.EQ.i_noctc).OR.(status.EQ.i_vnish)) RETURN

    ! Dissipated power: M*L*L/T*T*T
    ! dv2 = 0.5*meff*ABS((vln*vln+vlt*vlt)-(vlnBEGIN*vlnBEGIN+vltBEGIN*vltBEGIN))/H
    
    ! kinetic energy theory: the system is isolated 
    ! => the power dissipated in the system equals the sum of the power of each contact

    SELECT CASE(MODtherm%ilkine)

    CASE(i_lkine_all)
       Un = ((1-THETA)*vlni) + (THETA* vln)
       Ut = ((1-THETA)*vlti) + (THETA* vlt)
       Us = ((1-THETA)*vlsi) + (THETA* vls)

!       IF (rln.LT.0.D0) rln = 0.D0

!       dv2 = -((Un*rln)+(Ut*rlt)+(Us*rls))/H
       dv2 = -(Un*rln)+(Ut*rlt)+(Us*rls)

       Nodes(icd)%dT = Nodes(icd)%dT + dv2*0.5
       Nodes(ian)%dT = Nodes(ian)%dT + dv2*0.5

    CASE(i_lkine_dvn)

       Un = ((1-THETA)*vlni) + (THETA* vln)
       
       dv2 = -Un*rln

       Nodes(icd)%dT = Nodes(icd)%dT + dv2*0.5
       Nodes(ian)%dT = Nodes(ian)%dT + dv2*0.5

    CASE(i_lkine_dvt)

       Ut = ((1-THETA)*vlti) + (THETA* vlt)
       Us = ((1-THETA)*vlsi) + (THETA* vls)

       dv2 = - ( Ut*rlt + Us*rls )

       Nodes(icd)%dT = Nodes(icd)%dT + dv2*0.5
       Nodes(ian)%dT = Nodes(ian)%dT + dv2*0.5

    CASE(i_lkine_no)

       dv2 = 0.0

    END SELECT

    QSij = dv2*0.5/H

    GLOBAL_PV   = GLOBAL_PV   + ABS(rln*vln/H) + ABS(rlt*vlt/H) + ABS(rls*vls/H)
    GLOBAL_DPV  = GLOBAL_DPV  + ABS(rln*(vln-vlni)/H) + ABS(rlt*(vlt-vlti)/H) + ABS(rls*(vls-vlsi)/H)
    GLOBAL_DV2  = GLOBAL_DV2  + dv2

  END SUBROUTINE comp_generation_part
!!!------------------------------------------------------
SUBROUTINE comp_convexion_part(inode,QIJ)

    IMPLICIT NONE
    INTEGER(kind=4) :: inode
    INTEGER(kind=4) :: ibdyty,itacty
    REAL(kind=8)    :: QIJ,radius,condik,AREA

    ibdyty = Nodes(inode)%ID_RBDY3
    itacty = Nodes(inode)%ID_TACTY

    QIJ = 0.D0

!    IF (.NOT.IS_IN_THE_FREE_BOUNDARY(ibdyty,itacty)) RETURN

    radius  = get_avr_radius_tacty(ibdyty,itacty)
    condik  = get_therm_cond(ibdyty,itacty)

    AREA = 4.0d0*PI_g*radius*radius

    QIJ = MODtherm%Gcond*AREA*(MODtherm%GTemp-Nodes(inode)%Tini)


  END SUBROUTINE comp_convexion_part
!!!------------------------------------------------------
  SUBROUTINE INIT_BULK_MATRIX(ibound)

    IMPLICIT NONE
    INTEGER(kind=4) :: ibound,NX,NY
    REAL(kind=8)    :: CST,DX,ALPHA,DX2,HTMP
    
    NX = TH_BND_NODES(ibound)%NX
    
    IF ( NX.EQ.0) THEN
       PRINT*,'WRANING!'
       PRINT*,'BOUND ',ibound,' DO NOT HAVE NODE'
       STOP
    ELSE
       DX = TH_BND_NODES(ibound)%lenght/REAL(NX,8)
       TH_BND_NODES(ibound)%DX = DX
    END IF
    
    NY = INT(TH_BND_NODES(ibound)%thickness/DX)
    
    TH_BND_NODES(ibound)%NY = NY
    
    SELECT CASE(MODtherm%ibound)
    CASE(i_d_1D__)
       
       ALLOCATE(TH_BND_NODES(ibound)%TBULK(1,NY))
       ALLOCATE(TH_BND_NODES(ibound)%TBULKi(1,NY))
       ALLOCATE(TH_BND_NODES(ibound)%TPROFIL(NY))
       
       HTMP = 0.5*DX*DX/TH_BND_NODES(ibound)%ALPHA
       
       PRINT*,' HTMP/H = ',HTMP,'/',H
       
       IF ( H .GT. HTMP ) THEN
          CALL LOGMES('* WARNING *')
          CALL LOGMES('The time step is larger than the maximal value of the thermal step')
       END IF
       
       TH_BND_NODES(ibound)%TBULK   = TH_BND_NODES(ibound)%T
       TH_BND_NODES(ibound)%TBULKi  = TH_BND_NODES(ibound)%T
       TH_BND_NODES(ibound)%TPROFIL = TH_BND_NODES(ibound)%T
       
    CASE(i_d_3D__)
       CALL LOGMES('init bulk 3D unavailable')
       STOP
       
    CASE DEFAULT
       
       NULLIFY(TH_BND_NODES(ibound)%TBULK)
       NULLIFY(TH_BND_NODES(ibound)%TBULKi)
       NULLIFY(TH_BND_NODES(ibound)%TPROFIL)
       
    END SELECT
    
  END SUBROUTINE INIT_BULK_MATRIX
!!!------------------------------------------------------
  SUBROUTINE COMP_T_BULK(ibound)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: inode,ibound,ibdyty,itacty
    INTEGER(kind=4) :: NX,NY,IX,IY
    REAL(kind=8)    :: DX,DX2,DX2_1,ADT,DIAG,QIJ,concd,DELTAT
    
    NX  = TH_BND_NODES(ibound)%NX
    NY  = TH_BND_NODES(ibound)%NY
    
    DX    = TH_BND_NODES(ibound)%DX
    DX2   = DX*DX
    DX2_1 = 1.D0/DX2
    ADT   = TH_BND_NODES(ibound)%ALPHA*H
    
    
    SELECT CASE(MODtherm%ibound)
       !********************!
    CASE(i_d_1D__)
       
       !* LIMIT CONDITIONS *!
       
       DIAG = 1 - 2.0*ADT/DX2
       
       DO IX = 1,NX
          inode = TH_BND_NODES(ibound)%TSOURCE(IX)
          TH_BND_NODES(ibound)%TBULK(1,1) = TH_BND_NODES(ibound)%TBULK(1,1) + Nodes(inode)%T
          
       END DO
       
       TH_BND_NODES(ibound)%TBULK(1,1)  = TH_BND_NODES(ibound)%TBULK(1,1) / REAL(NX,8)
       TH_BND_NODES(ibound)%TBULK(1,NY) = TH_BND_NODES(ibound)%T     
       
       !* BULK *!
       
       DO IY = 2,NY-1
          
          TH_BND_NODES(ibound)%TBULK(1,IY) &
               = DIAG*TH_BND_NODES(ibound)%TBULK(1,IY) &
               + ADT*DX2_1*(TH_BND_NODES(ibound)%TBULK( 1,IY+1) &
               +            TH_BND_NODES(ibound)%TBULK( 1,IY-1))
       END DO
       
       TH_BND_NODES(ibound)%TBULKi = TH_BND_NODES(ibound)%TBULK
       
       !* THERMAL GRADIENT COMPUTATION *!
       
       DO IX = 1,NX
          inode = TH_BND_NODES(ibound)%TSOURCE(IX)
          
          DELTAT = TH_BND_NODES(ibound)%TBULK(1,2) - Nodes(inode)%T 
          
          ibdyty = Nodes(inode)%ID_RBDY3
          itacty = Nodes(inode)%ID_TACTY
          
          concd  = get_therm_cond(ibdyty,itacty)
          
          QIJ = concd*DELTAT!*DX/DX
          
          Nodes(inode)%dT = Nodes(inode)%dT + Qij
          
          GLOBAL_AQIJ = GLOBAL_AQIJ + Qij
          
       END DO
       
    CASE(i_d_3D__)
       
    CASE DEFAULT
       
    END SELECT
    
  END SUBROUTINE COMP_T_BULK
!!!------------------------------------------------------
  INTEGER(kind=4) FUNCTION get_nb_HEAT_bounds()
    
    IMPLICIT NONE
    
    get_nb_HEAT_bounds = nb_TH_BOUNDS
    
  END FUNCTION get_nb_HEAT_bounds
!!!------------------------------------------------------
  SUBROUTINE get_HEAT_bound_dims(ibound,NX,NY,NZ)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: ibound,NX,NY,NZ
    
    NX  = TH_BND_NODES(ibound)%NX
    NY  = TH_BND_NODES(ibound)%NY
    NZ  = TH_BND_NODES(ibound)%NZ
    
  END SUBROUTINE get_HEAT_bound_dims
!!!------------------------------------------------------
  SUBROUTINE GET_HEAT_BOUND_MNODES(ibound,MNODES)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: ibound
    REAL(kind=8),POINTER,DIMENSION(:,:) :: MNODES
    
  END SUBROUTINE GET_HEAT_BOUND_MNODES
!!!------------------------------------------------------
  SUBROUTINE GET_HEAT_BOUND_TNODES(ibound,TNODES)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: ibound
    REAL(kind=8),POINTER,DIMENSION(:) :: TNODES
    
  END SUBROUTINE GET_HEAT_BOUND_TNODES
!!!------------------------------------------------------
  SUBROUTINE GET_HEAT_BOUND_PROFILE(ibound,TPROFIL)
    
    IMPLICIT NONE
    INTEGER(kind=4) :: ibound,IX,IY,NX,NY
    REAL(kind=8),POINTER,DIMENSION(:) :: TPROFIL
    
    NX  = TH_BND_NODES(ibound)%NX
    NY  = TH_BND_NODES(ibound)%NY
    
    SELECT CASE(MODtherm%ibound)
    CASE(i_d_1D__)
       
       DO IY = 1,NY
          TPROFIL(IY) = TH_BND_NODES(ibound)%TBULKi(1,IY)
       END DO
       
    CASE(i_d_3D__)
       
    CASE DEFAULT
       
    END SELECT
    
  END SUBROUTINE GET_HEAT_BOUND_PROFILE
!!!------------------------------------------------------
  SUBROUTINE init_free_boundary_mp_solver
    
    IMPLICIT NONE
    FREE_BOUNDARY = .TRUE.
    
  END SUBROUTINE init_free_boundary_mp_solver
!!!------------------------------------------------------

END MODULE MP_SOLVER_3D

