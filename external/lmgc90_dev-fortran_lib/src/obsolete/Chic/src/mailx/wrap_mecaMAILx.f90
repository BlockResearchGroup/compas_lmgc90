!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_mecaMAILx                                       

  !!****h* LMGC90.CHIC/mecaMAILx  
  !! NAME
  !!  module wrap_mecaMAILx  
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/mecaMAILx  
  !!****

  USE CHIC

  use overall, only: get_time

  USE mecaMAILx,ONLY:&
       get_nb_mecaMAILx, &
       compute_bulk_hpp_mecaMAILx, &
       update_bulk_mecaMAILx, &
       compute_free_vlocy_mecaMAILx, &
       assemb_KT_mecaMAILx, &
       assemb_RHS_mecaMAILx, &
!!$       compute_Fint_mecaMAILx, &
       check_nl_convergence_mecaMAILx, &
       compute_nl_free_vlocy_mecaMAILx, &
       assemb_nl_KT_mecaMAILx, &
       assemb_nl_RHS_mecaMAILx, &
       compute_bulk_gd_mecaMAILx, &
       update_dof_mecaMAILx, &
       compute_dof_mecaMAILx, &
       increment_mecaMAILx, &
       compute_Fext_mecaMAILx, &
       compute_mass_mecaMAILx, &
       fatal_damping_mecaMAILx,&
       fatal_damping_mecaMAILx,&
       check_equilibrium_state_mecaMAILx, &
       set_data_equilibrium_mecaMAILx, &
       read_in_driven_dof_mecaMAILx, &
       write_out_driven_dof_mecaMAILx, &
       read_in_gpv_mecaMAILx, &
       read_in_dof_mecaMAILx, &
       write_xxx_dof_mecaMAILx, &
       write_xxx_Rnod_mecaMAILx, &
       load_behaviours_mecaMAILx, &
       write_xxx_nodforces, &
       update_existing_entities_mecaMAILx, &
       load_models_mecaMAILx, &
       push_ppset_mecaMAILx, &
       CHECK_mecaMAILx, &
       get_write_DOF_mecaMAILx, &
       get_write_Rnod_mecaMAILx, &
       init_precon_W_mecaMAILx, &
       compute_precon_W_mecaMAILx, &
       compute_coro_W_mecaMAILx, &
       set_precon_body_mecaMAILx,&
       set_is_sky_mecaMAILx, &
       set_field_bynode, &
       set_field_byuser, &
       get_field_rank, &
       get_nb_nodes, &
       terminate_mecaMAILx, &
       display_bulk_element_mecamailx,&
       use_new_ppset_mecamailx, &
       set_ortho_frame_byuser, &       
       set_with_renum_mecaMAILx
!       compute_ttFint_mecaMAILx, &
!       compute_stiffness_mecaMAILx, &
!       update_nl_bulk_mecaMAILx, &


CONTAINS

!!!---------------------------------------------------
  SUBROUTINE chic_command_mecaMAILx

    IMPLICIT NONE
    
    INTEGER :: ifrom,ito,IGOZZZ
    INTEGER :: iconv
    INTEGER :: nb_mecaMAILx

    REAL(kind=8)     :: tol
    LOGICAL          :: check_convergence,CHECK,write_DOF,write_Rnod
    CHARACTER(len=5) :: checktype
    INTEGER :: ibdyty,iblmty,ifield


    IF (INDEX(CHZZZ,'SKYLINE STORAGE')==1) THEN
       !!****e* mecaMAILx/SKYLINE STORAGE
       !! NAME
       !!  SET PRECON W
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_precon_W_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL set_is_sky_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'RENUMBERING')==1) THEN
       !!****e* mecaMAILx/set_with_renum_mecaMAILx
       !! NAME
       !!  set_with_renum_mecaMAILx
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_with_renum_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL set_with_renum_mecaMAILx()
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'USE NEW PPSET')==1) THEN
       !!****e* mecaMAILx/USE NEW PPSET
       !! NAME
       !!  USE NEW PPSET
       !! PURPOSE
       !!  Needed when using models with behavior depending on external parameters 
       !! USES
       !!  LMGC90.CORE/mecaMAILx/use_new_ppset_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL use_new_ppset_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF


    CHECK = CHECK_mecaMAILx()

    IF (.NOT.CHECK) RETURN
    

    IF (INDEX(CHZZZ,'SET PRECON BODY')==1) THEN
       !!****e* mecaMAILx/SET PRECON BODY
       !! NAME
       !!  SET PRECON BODY
       !! PURPOSE
       !!  declare a body to be preconditionned 
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_precon_body_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdyty
       CALL set_precon_body_mecaMAILx(ibdyty)
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SET PRECON ALL BODIES')==1) THEN
       !!****e* mecaMAILx/SET PRECON ALL BODIES
       !! NAME
       !!  SET PRECON ALL BODIES
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_precon_W_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       do ibdyty=1,get_nb_mecaMAILx() 
         CALL set_precon_body_mecaMAILx(ibdyty)
       end do
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE PRECON W')==1) THEN
       !!****e* mecaMAILx/COMPUTE PRECON W
       !! NAME
       !!  COMPUTE PRECON W
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_precon_W_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_precon_W_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE CORO W')==1) THEN
       !!****e* mecaMAILx/COMPUTE CORO W
       !! NAME
       !!  COMPUTE CORO W
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_coro_W_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_coro_W_mecaMAILx

       stop

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INIT PRECON W')==1) THEN
       !!****e* mecaMAILx/INIT PRECON W
       !! NAME
       !!  INIT PRECON W
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/init_precon_W_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL init_precon_W_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

!!! LINEAR PROBLEMS ----------------------------------

    IF (INDEX(CHZZZ,'COMPUTE BULK')==1) THEN
       !!****e* mecaMAILx/COMPUTE BULK
       !! NAME
       !!  COMPUTE BULK
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_bulk_hpp_mecaMAILx(0) 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE BULK')==1) THEN
       !!****e* mecaMAILx/UPDATE BULK
       !! NAME
       !!  UPDATE BULK
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_bulk_mecaMAILx
       !!  LMGC90.CORE/mecaMAILx/compute_bulk_hpp_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_bulk_hpp_mecaMAILx(1) 
       CALL update_bulk_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE FREE VELOCITY')==1) THEN
       !!****e* mecaMAILx/COMPUTE FREE VELOCITY
       !! NAME
       !!  COMPUTE FREE VELOCITY
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_free_vlocy_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_free_vlocy_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'ASSEMB KT')==1) THEN
       !!****e* mecaMAILx/ASSEMB KT
       !! NAME
       !!  ASSEMB KT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_KT_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL assemb_KT_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'ASSEMB RHS')==1) THEN
       !!****e* mecaMAILx/ASSEMB RHS
       !! NAME
       !!  ASSEMB RHS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_RHS_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL assemb_RHS_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

!    IF (INDEX(CHZZZ,'COMPUTE Fint')==1) THEN
!       !!****e* mecaMAILx/COMPUTE Fint
!       !! NAME
!       !!  COMPUTE Fint
!       !! PURPOSE
!       !!
!       !! USES
!       !!  LMGC90.CORE/mecaMAILx/compute_Fint_mecaMAILx
!       !!****
!       CALL LOGCHIC('mecaMAILx')
!       CALL compute_Fint_mecaMAILx
!       IETZZZ = 1
!      RETURN      
!    END IF

!    IF (INDEX(CHZZZ,'COMPUTE TTFint')==1) THEN
!       !!****e* mecaMAILx/COMPUTE TTFint
!       !! NAME
!       !!  COMPUTE TTFint
!       !! PURPOSE
!       !!
!       !! USES
!       !!  LMGC90.CORE/mecaMAILx/compute_ttFint_mecaMAILx
!       !!****
!       CALL LOGCHIC('mecaMAILx')
!       print*,'ERROR: obsolete keyword use COMPUTE BULK insteed'
!!       CALL compute_ttFint_mecaMAILx
!       IETZZZ = 1
!       RETURN      
!    END IF

!    IF (INDEX(CHZZZ,'COMPUTE STIFFNESS')==1) THEN
!       !!****e* mecaMAILx/COMPUTE STIFFNESS
!       !! NAME
!       !!  COMPUTE STIFFNESS
!       !! PURPOSE
!       !!
!       !! USES
!       !!  LMGC90.CORE/mecaMAILx/compute_stiffness_mecaMAILx
!       !!****
!       CALL LOGCHIC('mecaMAILx')
!       print*,'ERROR: obsolete keyword use COMPUTE BULK insteed'
!!!       CALL compute_stiffness_mecaMAILx
!       IETZZZ = 1
!       RETURN      
 !   END IF

!!! NON LINEAR PROBLEMS -------------------------------

    IF (INDEX(CHZZZ,'CHECK NL CONVERGENCE')==1) THEN
       !!****e* mecaMAILx/CHECK NL CONVERGENCE
       !! NAME
       !!  CHECK NL CONVERGENCE
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/check_nl_convergence_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       iconv = 1
       CALL check_nl_convergence_mecaMAILx(iconv)
       nb_mecaMAILx = get_nb_mecaMAILx()
       IF (nb_mecaMAILx .NE. 0) IFLZZZ(NZZZ,1) = iconv
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'UPDATE NL BULK')==1) THEN
       !!****e* mecaMAILx/UPDATE NL BULK
       !! NAME
       !!  UPDATE NL BULK
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_nl_bulk_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_bulk_gd_mecaMAILx(1) 
       CALL update_bulk_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE NL FREE VELOCITY')==1) THEN
       !!****e* mecaMAILx/COMPUTE NL FREE VELOCITY
       !! NAME
       !!  COMPUTE NL FREE VELOCITY
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_nl_free_vlocy_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_nl_free_vlocy_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'ASSEMB NL KT')==1) THEN
       !!****e* mecaMAILx/ASSEMB NL KT
       !! NAME
       !!  ASSEMB NL KT
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_nl_KT_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL assemb_nl_KT_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'ASSEMB NL RHS')==1) THEN
       !!****e* mecaMAILx/ASSEMB NL RHS
       !! NAME
       !!  ASSEMB NL RHS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/assemb_nl_RHS_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL assemb_nl_RHS_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'COMPUTE NL BULK')==1) THEN
       !!****e* mecaMAILx/COMPUTE NL BULK
       !! NAME
       !!  COMPUTE NL BULK
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_nl_bulk_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_bulk_gd_mecaMAILx(0)
       IETZZZ = 1
       RETURN      
    END IF

!!! LINEAR AND NON LINEAR PROBLEMS -------------------------------

    IF (INDEX(CHZZZ,'UPDATE DOF')==1) THEN
       !!****e* mecaMAILx/UPDATE DOF
       !! NAME
       !!  UPDATE DOF
       !! PURPOSE
       !!  save d.o.f. of the end of the time step to d.o.f. of the begining 
       !!  of the next one
       !! USES
       !!  LMGC90.CORE/mecaMAILx/update_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL update_dof_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF
    !
    IF (INDEX(CHZZZ,'COMPUTE DOF')==1) THEN
       !!****e* mecaMAILx/COMPUTE DOF
       !! NAME
       !!  COMPUTE DOF
       !! PURPOSE
       !!  correction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_dof_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF
    !
    IF (INDEX(CHZZZ,'INCREMENT STEP')==1) THEN
       !!****e* mecaMAILx/INCREMENT STEP
       !! NAME
       !!  INCREMENT STEP
       !! PURPOSE
       !!  prediction of the configuration parameter using the theta-method
       !! USES
       !!  LMGC90.CORE/mecaMAILx/increment_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL increment_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF
    !
    IF (INDEX(CHZZZ,'COMPUTE Fext')==1) THEN
       !!****e* mecaMAILx/COMPUTE Fext
       !! NAME
       !!  COMPUTE Fext
       !! PURPOSE
       !!  compute external forces
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_Fext_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_Fext_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF
    !
    IF (INDEX(CHZZZ,'COMPUTE MASS')==1) THEN
       !!****e* mecaMAILx/COMPUTE MASS
       !! NAME
       !!  COMPUTE_MASS
       !! PURPOSE
       !!  compute mass and inertia of bodies
       !! USES
       !!  LMGC90.CORE/mecaMAILx/compute_mass_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL compute_mass_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF
    !
    IF (INDEX(CHZZZ,'FATAL DAMPING')==1) THEN
       !!****e* mecaMAILx/FATAL DAMPING
       !! NAME
       !!  FATAL DAMPING
       !! PURPOSE
       !!  Nullify body velocities
       !! USES
       !!  LMGC90.CORE/mecaMAILx/fatal_damping_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL fatal_damping_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'CHECK EQUILIBRIUM STATE')==1) THEN
       !!****e* CHIC_cmd/CHECK EQUILIBRIUM STATE
       !! NAME
       !!   CHECK EQUILIBRIUM STATE 
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !! USES
       !!  LMGC90.CORE/mecaMAILx/check_equilibrium_state_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       IFLZZZ(NZZZ,1)=1
       check_convergence = .FALSE.
       CALL check_equilibrium_state_mecaMAILx(check_convergence)
       IF (check_convergence) IFLZZZ(NZZZ,1) = 0
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'EQUILIBRIUM NORM')==1) THEN
       !!****e* CHIC_cmd/EQUILIBRIUM NORM
       !! NAME
       !!   EQUILIBRIUM NORM 
       !! SYNOPSIS
       !!  CHECK EQUILIBRIUM STATE
       !!  [checktype] [tol]
       !! INPUTS
       !!  [checktype] : 
       !!  [tol]       : 
       !! PURPOSE
       !!  Check if the sample riches its equilibrium state 
       !!  If it's the case it puts the flags 1 to 0
       !!  You must precise the type of check test
       !!   - Qvlcy : quadratic norm of velocy
       !!   - Maxm  : maximum   norm of velocy
       !! USES
       !!  LMGC90.CORE/mecaMAILx/set_data_equilibrium_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),'(A5)') checktype
       IF ( checktype .NE. 'Qvlcy' .AND. checktype .NE. 'Mvlcy') THEN
          WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ)
          CALL LOGMESCHIC(' @ WARNING, NEW from 03.12.16.')
          CALL LOGMESCHIC(' @ You must precise the type of check test,')
          CALL LOGMESCHIC(' @  - Qvlcy : quadratic norm of velocy,')
          CALL LOGMESCHIC(' @  - Maxm  : maximum   norm of velocy.')
          STOP
       END IF
       READ(CHLZZZ(NZZZ+1)(7:30),*) tol
       CALL set_data_equilibrium_mecaMAILx(checktype,tol)
       IETZZZ = 1
       RETURN      
    END IF

!!! READING DATA ----------------------------------------------------

    IF (INDEX(CHZZZ,'READ DRIVEN DOF')==1) THEN
       !!****e* mecaMAILx/READ DRIVEN DOF
       !! NAME
       !!  READ DRIVEN DOF
       !! PURPOSE
       !!  read DRV_DOF.DAT file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_driven_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL read_in_driven_dof_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'READ INI GPV')==1) THEN
       !!****e* mecaMAILx/READ INI GPV
       !! NAME
       !!  READ INI GPV
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_gpv_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL read_in_gpv_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'READ INI DOF')==1) THEN
       !!****e* mecaMAILx/READ INI DOF
       !! NAME
       !!  READ INI DOF
       !! PURPOSE
       !!  read DOF.INI file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_in_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL read_in_dof_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'LOAD BEHAVIOURS')==1) THEN
       !!****e* mecaMAILx/LOAD BEHAVIOURS
       !! NAME
       !!  READ BEHAVIOURS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_behaviours_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL load_behaviours_mecaMAILx 
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'LOAD MODELS')==1) THEN
       !!****e* mecaMAILx/LOAD MODELS
       !! NAME
       !!  READ MODELS
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_models_mecaMAILx
       !!  LMGC90.CORE/mecaMAILx/update_existing_entities_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL load_models_mecaMAILx
       CALL update_existing_entities_mecaMAILx
       
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'PUSH PROPERTIES')==1) THEN
       !!****e* mecaMAILx/PUSH PROPERTIES
       !! NAME
       !!  
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/mecaMAILx/read_behaviours_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       call push_ppset_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

!!! WRITTING DATA --------------------------------------------------

    IF (INDEX(chzzz,'WRITE DRIVEN DOF')==1) THEN
       !!****e* mecaMAILx/WRITE DRIVEN DOF
       !! NAME
       !!  WRITE DRIVEN DOF
       !! PURPOSE
       !!  write DRV_DOF.OUT file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_out_driven_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL write_out_driven_dof_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST DOF')==1) THEN     
       !!****e* mecaMAILx/WRITE LAST DOF
       !! NAME
       !!  WRITE LAST DOF
       !! PURPOSE
       !!  write ascii DOF.LAST file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_mecaMAILx = get_nb_mecaMAILx()
          ifrom = 1  
          ito   = nb_mecaMAILx
          CALL write_xxx_dof_mecaMAILx(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT DOF')==1) THEN     
       !!****e* mecaMAILx/WRITE OUT DOF
       !! NAME
       !!  WRITE OUT DOF
       !! PURPOSE
       !!  write ascii DOF.OUT file. Can be activate only each N step
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')

       write_DOF = get_write_DOF_mecaMAILx()

       IF (write_DOF) THEN
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN 
             nb_mecaMAILx = get_nb_mecaMAILx()
             ifrom = 1  
             ito   = nb_mecaMAILx
             CALL write_xxx_dof_mecaMAILx(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*) ifrom,ito
                      nb_mecaMAILx = get_nb_mecaMAILx()
                      ifrom=max0(ifrom,1)
                      ito=min0(ito,nb_mecaMAILx)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_dof_mecaMAILx(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT DOF')==1) THEN
       !!****e* mecaMAILx/DISPLAY OUT DOF
       !! NAME
       !!  DISPLAY OUT DOF
       !! PURPOSE
       !!  display body degrees of freedom
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_dof_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_mecaMAILx = get_nb_mecaMAILx()
          ifrom = 1  
          ito   = nb_mecaMAILx
          CALL write_xxx_dof_mecaMAILx(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   nb_mecaMAILx = get_nb_mecaMAILx()
                   ifrom=max0(ifrom,1)
                   ito=min0(ito,nb_mecaMAILx)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_dof_mecaMAILx(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE LAST Rnod')==1) THEN     
       !!****e* mecaMAILx/WRITE LAST Rnod
       !! NAME
       !!  WRITE LAST Rnod
       !! PURPOSE
       !!  write ascii Rnod.LAST file
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_mecaMAILx = get_nb_mecaMAILx()
          ifrom = 1  
          ito   = nb_mecaMAILx
          CALL write_xxx_Rnod_mecaMAILx(2,ifrom,ito)
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT Rnod')==1) THEN     
       !!****e* mecaMAILx/WRITE OUT Rnod
       !! NAME
       !!  WRITE OUT Rnod
       !! PURPOSE
       !!   write ascii Rnod.OUT file. Can be activate only each N step.
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****
       write_Rnod = get_write_Rnod_mecaMAILx()
       CALL LOGCHIC('mecaMAILx')
       IF (write_Rnod) THEN
          IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN 
             nb_mecaMAILx = get_nb_mecaMAILx()
             ifrom = 1  
             ito   = nb_mecaMAILx
             CALL write_xxx_Rnod_mecaMAILx(1,ifrom,ito)
          ELSE
             IGOZZZ=0
             DO
                IGOZZZ=IGOZZZ+1
                IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                   IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                      READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*) ifrom,ito
                      nb_mecaMAILx = get_nb_mecaMAILx()
                      ifrom=max0(ifrom,1)
                      ito=min0(ito,nb_mecaMAILx)
                      IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                      CALL write_xxx_Rnod_mecaMAILx(1,ifrom,ito)
                   END IF
                ELSE
                   EXIT
                END IF
                CYCLE
             END DO
          END IF
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT Rnod')==1) THEN
       !!****e* mecaMAILx/DISPLAY OUT Rnod
       !! NAME
       !!  DISPLAY OUT Rnod
       !! PURPOSE
       !!  display body forces.
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_Rnod_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_mecaMAILx = get_nb_mecaMAILx()
          ifrom = 1  
          ito   = nb_mecaMAILx
          CALL write_xxx_Rnod_mecaMAILx(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*) ifrom,ito
                   nb_mecaMAILx = get_nb_mecaMAILx()
                   ifrom=max0(ifrom,1)
                   ito=min0(ito,nb_mecaMAILx)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_Rnod_mecaMAILx(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF

    IF (INDEX(CHZZZ,'DISPLAY OUT nodal forces')==1) THEN
       !!****e* mecaMAILx/DISPLAY OUT nodal forces
       !! NAME
       !!  DISPLAY OUT nodal forces
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/mecaMAILx/write_xxx_nodforces
       !!****
       CALL LOGCHIC('mecaMAILx')
       IF (CHLZZZ(NZZZ+1)(1:7) /= 'FROM TO') THEN
          nb_mecaMAILx = get_nb_mecaMAILx()
          ifrom = 1  
          ito   = nb_mecaMAILx
          CALL write_xxx_nodforces(6,ifrom,ito)
       ELSE
          IGOZZZ=0
          DO
             IGOZZZ=IGOZZZ+1
             IF (CHLZZZ(NZZZ+IGOZZZ)(1:7) == 'FROM TO') THEN
                IF (CHLZZZ(NZZZ+IGOZZZ)(9:13) == 'MAILx') THEN
                   READ(CHLZZZ(NZZZ+IGOZZZ)(14:30),*)ifrom,ito
                   nb_mecaMAILx = get_nb_mecaMAILx()
                   ifrom = max0(ifrom,1) 
                   ito   = min0(ito,nb_mecaMAILx)
                   IF (KHOZZZ.EQ.1) WRITE(*,'(1X,A3,A30)')' @ ',CHLZZZ(NZZZ+IGOZZZ)
                   CALL write_xxx_nodforces(6,ifrom,ito)
                END IF
             ELSE
                EXIT
             END IF
             CYCLE
          END DO
       END IF
       IETZZZ=1
       RETURN 
    END IF


    IF (INDEX(CHZZZ,'TERMINATE')==1) THEN
       !!****e* mecaMAILx/TERMINATE
       !! NAME
       !!  TERMINATE 
       !! PURPOSE
       !!  stop job properly
       !! USES
       !!  LMGC90.CORE/mecaMAILx/terminate_mecaMAILx
       !!****
       CALL LOGCHIC('mecaMAILx')
       CALL terminate_mecaMAILx
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'DISPLAY MECAx ELEMENT')==1) THEN
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ibdyty,iblmty
       CALL DISPLAY_bulk_element_mecaMAILx(ibdyty,iblmty)
       IETZZZ = 1
       RETURN      
    END IF

   IF (INDEX(CHZZZ,'SET ORTHO FRAME')==1) THEN
       !!****e* mecaMAILx/SET ORTHO FRAME
       !! NAME
       !!  SET ORTHO FRAME
       !! PURPOSE
       !!  set the ortho frame if necessary. 
       !!  this method use a routine defined by the user in user.f90
       !! USES
       !!  LMGC90.CORE/mecaMAILx/SET_ORTHO_FRAME_byuser
       !!****
       CALL LOGCHIC('mecaMAILx')
       call SET_ORTHO_FRAME_byuser
       IETZZZ = 1
      RETURN      
   END IF

   IF (INDEX(CHZZZ,'COMPUTE FIELD')==1) THEN
       !!****e* mecaMAILx/COMPUTE FIELD
       !! NAME
       !!  COMPUTE FIELD
       !! PURPOSE
       !!  Computes a user field at gp
       !! USES
       !!  LMGC90.CORE/mecaMAILx/SET_field_byuser
       !!****
       CALL LOGCHIC('mecaMAILx')
       READ(CHLZZZ(NZZZ+1),*) ifield
       call set_field_byuser(ifield)
       IETZZZ = 1
      RETURN      
   END IF

  END SUBROUTINE chic_command_mecaMAILx


!!!----------------------------------------------------

  include '../../more_src/mailx/wrap_mecaMAILx.inc'

END MODULE wrap_mecaMAILx
