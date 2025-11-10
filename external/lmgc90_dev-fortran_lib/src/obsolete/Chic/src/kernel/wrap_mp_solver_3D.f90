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
MODULE wrap_MP_SOLVER_3D

  !!****h* LMGC90.CHIC/MP_SOLVER_3D
  !! NAME
  !!  module WRAP_MP_SOLVER_3D
  !! AUTHOR
  !!   M. Renouf (e-mail: Mathieu.Renouf@insa-lyon.fr)
  !! PURPOSE
  !!   This module is dedicated to Thermal Electrical Discrete Element  model
  !!   Contact the author to obtain the module.
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/MP_SOLVER_3D
  !!****

  USE CHIC
  USE MP_SOLVER_3D,ONLY:&
       init_mp_solver, &
       get_oxided_tactor, &
       get_electro_info, &
       read_in_mp_behaviour_mp_solver, &
       write_out_mp_behaviour_mp_solver, &
       read_ini_mp_values_mp_solver, &
       write_out_mp_values_mp_solver, &
       solve_electro1G, &
       solve_nl_electro1G, &
       solve_thermo_mp_solver, &
       get_write_mp_values, &
       update_compuctivity_mp_solver, &
       update_thermo_mp_solver, &
       active_recup, &
       get_global_3D_thermal_variable

CONTAINS

!!!--------------------------------------------------------
  SUBROUTINE chic_command_mp_solver_3D

    IMPLICIT NONE

    LOGICAL :: write_mp

    IF (INDEX(CHZZZ,'READ MP BEHAVIOURS')==1) THEN
       !!****e* MP_SOLVER_3D/READ MP BEHAVIOUR
       !! NAME
       !!  READ MP BEHAVIOUR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/MP_SOLVER_3D/init_mp_solver
       !!  LMGC90.CORE/MP_SOLVER_3D/read_in_mp_behaviour_mp_solver
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL init_mp_solver
       CALL read_in_mp_behaviour_mp_solver
    END IF

    IF (INDEX(CHZZZ,'WRITE MP BEHAVIOURS')==1) THEN
       !!****e* MP_SOLVER/WRITE_MP_BEHAVIOUR
       !! NAME
       !!  WRITE MP BEHAVIOUR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/write_out_mp_behaviour_mp_solver
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL write_out_mp_behaviour_mp_solver
    END IF

    IF (INDEX(CHZZZ,'READ INI MP VALUES')==1) THEN
       !!****e* MP_SOLVER/READ_INI_MP_VALUES
       !! NAME
       !!  READ INI MP VALUES
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/read_ini_mp_values
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL read_ini_mp_values_mp_solver
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'WRITE OUT MP VALUES')==1) THEN     
       !!****e* MP_SOLVER/WRITE_OUT_MP_VALUES
       !! NAME
       !!   WRITE OUT MP VALUES
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/write_out_mp_values
       !!****
       CALL LOGCHIC('MP_SOLVER')
       write_mp = get_write_mp_values()
       IF (write_mp) THEN
          CALL write_out_mp_values_mp_solver
          IETZZZ=1
          RETURN 
       END IF
    END IF

    IF (INDEX(CHZZZ,'SOLVE ELECTRO 1G')==1) THEN
       !!****e* MP_SOLVER_3D/SOLVE ELECTRO 1G
       !! NAME
       !!  SOLVE ELECTRO 1G
       !! PURPOSE
       !! USES
       !!  LMGC90.CORE/MP_SOLVER_3D/solve_electro1G
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL solve_electro1G
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'SOLVE NL ELECTRO 1G')==1) THEN
       !!****e* MP_SOLVER_3D/SOLVE NL ELECTRO 1G
       !! NAME
       !!  SOLVE NL ELECTRO 1G
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER_3D/solve_nl_electro1G
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL solve_nl_electro1G
       IETZZZ = 1
       RETURN
    END IF

    IF (INDEX(CHZZZ,'SOLVE THERMO PROBLEM')==1) THEN
       !!****e* MP_SOLVER_3D/SOLVE THERMO PROBLEM
       !! NAME
       !!  SOLVE THERMO PROBLEM
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER_3D/solve_thermo_mp_solver
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL solve_thermo_mp_solver
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'UPDATE THERMO PROBLEM')==1) THEN
       !!****e* MP_SOLVER_3D/UPDATE THERMO PROBLEM
       !! NAME
       !!  UPDATE THERMO PROBLEM
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER_3D/update_thermo_mp_solver
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL update_thermo_mp_solver
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'RECUP TEMPERATURE')==1) THEN
       !!****e* MP_SOLVER/RECUP_TEMPERATURE
       !! NAME
       !!  RECUP TEMPERATURE
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/active_recup
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL active_recup('T')
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'RECUP POTENTIAL')==1) THEN
       !!****e* MP_SOLVER/RECUP_POTENTIAL
       !! NAME
       !!  RECUP POTENTIAL
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/active_recup
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL active_recup('P')
       IETZZZ = 1
       RETURN
    ENDIF

    IF (INDEX(CHZZZ,'UPDATE CONDUCTIVITY')==1) THEN
       !!****e* MP_SOLVER/UPDATE_CONDUCTIVITY
       !! NAME
       !!  UPDATE CONDUCTIVITY
       !! PURPOSE
       !!
       !! USES
       !!  LMGC90.CORE/MP_SOLVER/update_compuctivity_mp_solver
       !!****
       CALL LOGCHIC('MP_SOLVER')
       CALL update_compuctivity_mp_solver
       IETZZZ = 1
       RETURN
    ENDIF

  END SUBROUTINE chic_command_mp_solver_3D

END MODULE wrap_MP_SOLVER_3D
