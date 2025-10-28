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
MODULE wrap_PTPT3

  USE ISO_C_BINDING

  USE PTPT3,ONLY:&
       coor_prediction_PTPT3,&
       CHECK_PTPT3,&
       RUN_PTPT3, &
       get_write_Vloc_Rloc_PTPT3, &
       read_ini_Vloc_Rloc_PTPT3,&
       write_xxx_Vloc_Rloc_PTPT3,&
       smooth_computation_PTPT3, &
       compute_box_PTPT3, &
       compute_contact_PTPT3, &
       display_prox_tactors_PTPT3,&
       load_network_PTPT3, &
       set_xperiodic_data_PTPT3, &
       set_yperiodic_data_PTPT3, &
       set_explicit_local_frame_PTPT3, &
       load_params_PTPT3, &
       use_current_nonuc0_PTPT3, &
       clean_memory_PTPT3

  USE utilities, ONLY : faterr

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(C, name = 'PTPT3_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between PT3Dx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       LOGICAL :: is_initialized = .FALSE.
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       IF (.NOT. check_PTPT3() ) RETURN

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PTPT3] select      ')
       call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN
         CALL compute_box_PTPT3
         is_initialized = .TRUE.  
       ENDIF

       CALL coor_prediction_PTPT3

       CALL compute_contact_PTPT3

       call stop_itimer(timer_id)

  END SUBROUTINE SelectProxTactors

!!!--------------------------------------------------

  SUBROUTINE SmoothForceComputation() bind(C, name = 'PTPT3_SmoothForceComputation')
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       IMPLICIT NONE

       IF( .NOT. check_PTPT3() ) RETURN

       CALL smooth_computation_PTPT3

  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() bind(C, name = 'PTPT3_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PTPT3 contacts
       IMPLICIT NONE

       IF( .NOT. check_PTPT3() ) RETURN

       CALL write_xxx_Vloc_Rloc_PTPT3(2)

  END SUBROUTINE

  SUBROUTINE WriteOutVlocRloc() bind(C, name = 'PTPT3_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PTPT3 contacts
       IMPLICIT NONE
       LOGICAL :: write_Vloc_Rloc

       IF( .NOT. check_PTPT3() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_PTPT3()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_PTPT3(1)

  END SUBROUTINE

  SUBROUTINE DisplayOutVlocRloc() bind(C, name = 'PTPT3_DisplayOutVlocRloc')   
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PTPT3 contacts
       IMPLICIT NONE

       IF( .NOT. check_PTPT3() ) RETURN

       CALL write_xxx_Vloc_Rloc_PTPT3(6)

  END SUBROUTINE

  SUBROUTINE DisplayProxTactors() bind(C, name = 'PTPT3_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       IF( .NOT. check_PTPT3() ) RETURN

       CALL display_prox_tactors_PTPT3

  END SUBROUTINE

  SUBROUTINE LoadNetwork() bind(C, name = 'PTPT3_LoadNetwork')
       !! PURPOSE
       !!  read a ptpt3 network from file 
       IMPLICIT NONE

       IF( .NOT. check_PTPT3() ) RETURN

       CALL  load_network_PTPT3

  END SUBROUTINE LoadNetwork

  subroutine ReadIniVlocRloc(step) bind(c, name='PTPT3_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_PTPT3() ) return

     call read_ini_Vloc_Rloc_PTPT3(step)

  end subroutine

!!!---------------------------------------------------------------------
!!!-----------------------

  SUBROUTINE SetXPeriodicCondition(xperiode) bind(C, name = 'PTPT3_SetXPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: xperiode
       LOGICAL :: XPERIODIC

       !IF( .NOT. check_PTPT3() ) RETURN

       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_PTPT3(xperiode,XPERIODIC)

  END SUBROUTINE SetXPeriodicCondition

!!!-----------------------

  SUBROUTINE SetYPeriodicCondition(yperiode) bind(C, name = 'PTPT3_SetYPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: yperiode
       LOGICAL :: YPERIODIC

       !IF( .NOT. check_PTPT3() ) RETURN

       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_PTPT3(yperiode,YPERIODIC)

  END SUBROUTINE SetYPeriodicCondition

!!!-----------------------
  
  SUBROUTINE SetExplicitLocalFrame() BIND(C, name = 'PTPT3_SetExplicitLocalFrame')
       IMPLICIT NONE

       CALL  set_explicit_local_frame_PTPT3

  END SUBROUTINE SetExplicitLocalFrame
  
!!!-----------------------

  SUBROUTINE LoadParams() BIND(C, name = 'PTPT3_LoadParams')
       !! PURPOSE
       !!  read a ptpt3 surface and l0 from file 
       IMPLICIT NONE

       CALL  load_params_PTPT3

  END SUBROUTINE LoadParams

  subroutine useCurrentNonuc0(no0) bind(c, name = 'PTPT3_UseCurrentNonuc0')
       !! purpose
       !! use nonuc0 given in file or use GetCoor to compute nonuc0 
       implicit none
       integer(kind=c_int), intent(in), value :: no0
       
       call  use_current_nonuc0_PTPT3(no0)

  end subroutine useCurrentNonuc0

!!!-----------------------
  
  subroutine CleanMemory() bind(c, name='PTPT3_CleanMemory')
    implicit none

    call clean_memory_PTPT3
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_PTPT3
