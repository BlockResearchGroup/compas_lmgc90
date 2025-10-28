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
MODULE wrap_CDPLx

  USE ISO_C_BINDING

  USE CDPLx,ONLY:&
       coor_prediction_CDPLx,&
       CHECK_CDPLx,&
       RUN_CDPLx, &
       get_write_Vloc_Rloc_CDPLx, &
       read_ini_Vloc_Rloc_CDPLx,&
       write_xxx_Vloc_Rloc_CDPLx,&
       smooth_computation_CDPLx, &
       compute_box_CDPLx, &
       creation_tab_visu_CDPLx, &
       compute_contact_CDPLx, &
       display_prox_tactors_CDPLx,&
       clean_memory_CDPLx

  USE utilities, ONLY : faterr

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) BIND(c, name='CDPLx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between CYLND and PLANx tactors.
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

       IF( .NOT. check_CDPLx() ) RETURN

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CDPLx] select      ')
       call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN
         CALL compute_box_CDPLx         
         is_initialized = .TRUE.
       ENDIF

       CALL coor_prediction_CDPLx

       IF (RUN_CDPLx()) CALL creation_tab_visu_CDPLx

       CALL compute_contact_CDPLx

       call stop_itimer(timer_id)

  END SUBROUTINE

  SUBROUTINE SmootForceComputation() BIND(c, name='CDPLx_SmoothForceComputation')
       IMPLICIT NONE

       IF( .NOT. check_CDPLx() ) RETURN

       CALL smooth_computation_CDPLx

  END SUBROUTINE
!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() BIND(c, name='CDPLx_WriteLastVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts

       IF( .NOT. check_CDPLx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CDPLx(2)

  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc() BIND(c, name='CDPLx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       IF( .NOT. check_CDPLx() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_CDPLx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_CDPLx(1)

  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc() BIND(c, name='CDPLx_DisplayOutVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CDPLx contacts

       IF( .NOT. check_CDPLx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CDPLx(6)

  END SUBROUTINE

  SUBROUTINE DisplayProxTactors() BIND(c, name='CDPLx_DisplayProxTactors')
       IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       IF( .NOT. check_CDPLx() ) RETURN

       CALL display_prox_tactors_CDPLx

  END SUBROUTINE

  subroutine ReadIniVlocRloc(step) bind(c, name='CDPLx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_CDPLx() ) return

     call read_ini_Vloc_Rloc_CDPLx(step)

  end subroutine

!!!---------------------------------------------------------------------
  subroutine CleanMemory() bind(c, name='CDPLx_CleanMemory')
    implicit none

    call clean_memory_CDPLx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_CDPLx
