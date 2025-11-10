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
MODULE wrap_CDCDx

  USE ISO_C_BINDING

  USE CDCDx,ONLY:&
       coor_prediction_CDCDx,&
       CHECK_CDCDx,&
       RUN_CDCDx, &
       get_write_Vloc_Rloc_CDCDx, &
       read_ini_Vloc_Rloc_CDCDx,&
       write_xxx_Vloc_Rloc_CDCDx,&
       smooth_computation_CDCDx, &
       compute_box_CDCDx, &
       creation_tab_visu_CDCDx, &
       compute_contact_CDCDx, &
       display_prox_tactors_CDCDx,&
       get_nb_CDCDx,&
       set_xperiodic_data_CDCDx,&     ! CE QUI SUIT EST EXPERIMENTAL ET NE MARCHE PAS ENCORE !!!!!
       set_yperiodic_data_CDCDx,&
       Set_NbInteractionByContact,&
       Set_ContactRadius, &
       clean_memory_CDCDx

  USE utilities,ONLY : faterr

  LOGICAL :: XPERIODIC = .FALSE.,YPERIODIC = .FALSE.

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) BIND(c, name='CDCDx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between CYLND and CYLND tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       LOGICAL :: is_initialized=.FALSE.
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       IF( .NOT. check_CDCDx() ) RETURN

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CDCDx] select      ')
       call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN 
          CALL compute_box_CDCDx
          is_initialized = .TRUE.
       ENDIF

       CALL coor_prediction_CDCDx

       IF (RUN_CDCDx()) CALL creation_tab_visu_CDCDx

       CALL compute_contact_CDCDx

       call stop_itimer(timer_id)

  END SUBROUTINE

!!!--------------------------------------------------
  SUBROUTINE SmootForceComputation() BIND(c, name='CDCDx_SmoothForceComputation')
       IMPLICIT NONE

       IF( .NOT. check_CDCDx() ) RETURN

       CALL smooth_computation_CDCDx

  END SUBROUTINE
!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() BIND(c, name='CDCDx_WriteLastVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts

       IF( .NOT. check_CDCDx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CDCDx(2)

  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc() BIND(c, name='CDCDx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       IF( .NOT. check_CDCDx() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_CDCDx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_CDCDx(1)

  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc() BIND(c, name='CDCDx_DisplayOutVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CDCDx contacts

       IF( .NOT. check_CDCDx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CDCDx(6)

  END SUBROUTINE

  SUBROUTINE DisplayProxTactors() BIND(c, name='CDCDx_DisplayProxTactors')
       IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       IF( .NOT. check_CDCDx() ) RETURN

       CALL display_prox_tactors_CDCDx

  END SUBROUTINE

  subroutine ReadIniVlocRloc(step) bind(c, name='CDCDx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_CDCDx() ) return

     call read_ini_Vloc_Rloc_CDCDx(step)

  end subroutine

  SUBROUTINE SetXPeriodicCondition(xperiode) BIND(C, name = 'CDCDx_SetXPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: xperiode

       !IF( .NOT. check_CDCDx() ) RETURN

       XPERIODIC=.TRUE.
       CALL set_xperiodic_data_CDCDx(xperiode,XPERIODIC)

  END SUBROUTINE

  SUBROUTINE SetYPeriodicCondition(yperiode) BIND(C, name = 'CDCDx_SetYPeriodicCondition')
       !! PURPOSE
       !!  initialise data for simulation using periodic condition
       !!****
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: yperiode

       !IF( .NOT. check_CDCDx() ) RETURN

       YPERIODIC=.TRUE.
       CALL set_yperiodic_data_CDCDx(yperiode,YPERIODIC)

  END SUBROUTINE

  SUBROUTINE SetNumberInterByContact(nbic) BIND(C, name = 'CDCDx_SetNumberInterByContact')
       !! PURPOSE
       !!  define the number of interaction by contact (experimental)
       IMPLICIT NONE
       INTEGER(C_INT), INTENT(IN), VALUE :: nbic

       !IF( .NOT. check_CDCDx() ) RETURN

       CALL set_NbInteractionByContact(nbic)

  END SUBROUTINE

  SUBROUTINE SetContactRadius(cr) BIND(C, name = 'CDCDx_SetContactRadius')
       !! PURPOSE
       !!  define the contact radius (experimental)
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: cr

       !IF( .NOT. check_CDCDx() ) RETURN

       CALL set_ContactRadius(cr)

  END SUBROUTINE

!!!---------------------------------------------------------------------

  subroutine CleanMemory() bind(c, name='CDCDx_CleanMemory')
    implicit none

    call clean_memory_CDCDx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_CDCDx
