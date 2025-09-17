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
MODULE wrap_SPSPx

  USE ISO_C_BINDING

  USE SPSPx,ONLY:&
       coor_prediction_SPSPx,&
       CHECK_SPSPx,&
       RUN_SPSPx, &
       get_write_Vloc_Rloc_SPSPx, &
       read_ini_Vloc_Rloc_SPSPx,&
       write_xxx_Vloc_Rloc_SPSPx,&
       smooth_computation_SPSPx, &
       compute_box_SPSPx, &
       creation_tab_visu_SPSPx, &
       compute_contact_SPSPx, &
       display_prox_tactors_SPSPx,&
       set_xperiodic_data_SPSPx, &
       set_yperiodic_data_SPSPx, &
       Set_NbInteractionByContact,Set_ContactRadius,fd_compute_contact_spspx, &  ! experimental
       clean_memory_SPSPx

  USE utilities, ONLY : faterr
  
  LOGICAL :: XPERIODIC = .FALSE.,YPERIODIC = .FALSE.
 
CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(C, name = 'SPSPx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between SPHER and SPHER tactors.
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

       IF( .NOT. check_SPSPx() ) RETURN

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[SPSPx] select      ')
       call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN
         CALL compute_box_SPSPx
         is_initialized = .TRUE.
       ENDIF

       CALL coor_prediction_SPSPx

       IF (RUN_SPSPx()) CALL creation_tab_visu_SPSPx

       CALL compute_contact_SPSPx

       call stop_itimer(timer_id)

  END SUBROUTINE SelectProxTactors

!!!--------------------------------------------------

  SUBROUTINE SmoothForceComputation() bind(C, name = 'SPSPx_SmoothForceComputation')
       IMPLICIT NONE

       IF( .NOT. check_SPSPx() ) RETURN

       CALL smooth_computation_SPSPx

  END SUBROUTINE SmoothForceComputation

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() bind(C, name = 'SPSPx_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  SPSPx contacts
       IMPLICIT NONE

       IF( .NOT. check_SPSPx() ) RETURN

       CALL write_xxx_Vloc_Rloc_SPSPx(2)
   
  END SUBROUTINE WriteLastVlocRloc

  SUBROUTINE WriteOutVlocRloc() bind(C, name = 'SPSPx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  SPSPx contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       IF( .NOT. check_SPSPx() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_SPSPx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_SPSPx(1)

  END SUBROUTINE WriteOutVlocRloc

  SUBROUTINE DisplayOutVlocRloc() bind(C, name = 'SPSPx_DisplayOutVlocRloc')
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  SPSPx contacts
       IMPLICIT NONE

       IF( .NOT. check_SPSPx() ) RETURN

       CALL write_xxx_Vloc_Rloc_SPSPx(6)

  END SUBROUTINE DisplayOutVlocRloc

  SUBROUTINE DisplayProxTactors() bind(C, name = 'SPSPx_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       IF( .NOT. check_SPSPx() ) RETURN

       CALL display_prox_tactors_SPSPx

  END SUBROUTINE DisplayProxTactors

  subroutine ReadIniVlocRloc(step) bind(c, name='SPSPx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_SPSPx() ) return

     call read_ini_Vloc_Rloc_SPSPx(step)

  end subroutine

  SUBROUTINE SetXPeriodicCondition(xperiode) bind(C, name = 'SPSPx_SetXPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: xperiode

       !IF( .NOT. check_SPSPx() ) RETURN

       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_SPSPx(xperiode,XPERIODIC)

  END SUBROUTINE SetXPeriodicCondition

  SUBROUTINE SetYPeriodicCondition(yperiode) bind(C, name = 'SPSPx_SetYPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: yperiode

       !IF( .NOT. check_SPSPx() ) RETURN

       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_SPSPx(yperiode,YPERIODIC)

  END SUBROUTINE SetYPeriodicCondition

  SUBROUTINE SetNumberInterByContact(nbic) bind(C, name = 'SPSPx_SetNumberInterByContact')
       !! PURPOSE
       !!  define the number of interaction by contact (experimental)
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: nbic

       !IF( .NOT. check_SPSPx() ) RETURN

       CALL set_NbInteractionByContact(nbic)

  END SUBROUTINE SetNumberInterByContact

  SUBROUTINE SetContactRadius(cr) bind(C, name = 'SPSPx_SetContactRadius')
       !! PURPOSE
       !!  define the contact radius (experimental)
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: cr

       !IF( .NOT. check_SPSPx() ) RETURN

       CALL set_ContactRadius(cr)

  END SUBROUTINE SetContactRadius

  SUBROUTINE FdSelectProxTactors() bind(C, name = 'SPSPx_FdSelectProxTactors') 
       !! PURPOSE
       !!  contact detection between SPHER and SPHER tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       LOGICAL :: RUN

       IF( .NOT. check_SPSPx() ) RETURN

       CALL coor_prediction_SPSPx

       RUN = RUN_SPSPx()

       IF (RUN) CALL creation_tab_visu_SPSPx

       CALL fd_compute_contact_SPSPx

  END SUBROUTINE FdSelectProxTactors
!!!---------------------------------------------------------------------

  subroutine CleanMemory() bind(c, name='SPSPx_CleanMemory')
    implicit none

    call clean_memory_SPSPx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_SPSPx
