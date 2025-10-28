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
MODULE wrap_CSASx

  USE ISO_C_BINDING

  USE CSASp,ONLY:&
       coor_prediction_CSASx,&
       CHECK_CSASx,&
       RUN_CSASx, &
       get_write_Vloc_Rloc_CSASx, &
       read_ini_Vloc_Rloc_CSASx,&
       write_xxx_Vloc_Rloc_CSASx,&
       creation_tab_visu_CSASx, &
       compute_contact_CSASx, &
       display_prox_tactors_CSASx,&
       initialize_CSASp, &
       skip_autocontact_CSASp, &
       set_nonsymmetric_detection_CSASp, &
       trim_CSASp, &
       set_trim_angle_CSASp, &
       is_external_detection_CSASp, &
       clean_memory_CSASp, &
       add_reac_CSASp, &
       assume_old_files_CSASp

   USE utilities, ONLY : faterr

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset,use_external) BIND(c, name='CSASp_SelectProxTactors')
       !! PURPOSE
       !!  contact detection between CSxxx and ASpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       use timer
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       integer(kind=c_int), intent(in), value :: use_external
       LOGICAL :: is_initialized = .FALSE.
       integer(kind=4), save :: timer_id = 0

       if (use_external > 0) call is_external_detection_CSASp()
       
       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       IF( .NOT. check_CSASx() ) RETURN
       
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CSASp] select      ')
      call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN
          CALL initialize_CSASp()
          is_initialized = .TRUE.
       ENDIF

       CALL coor_prediction_CSASx

       IF (RUN_CSASx()) CALL creation_tab_visu_CSASx

       CALL compute_contact_CSASx
       
       call stop_itimer(timer_id)

  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() BIND(c, name='CSASp_WriteLastVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CSASp contacts

       IF( .NOT. check_CSASx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CSASx(2)

  END SUBROUTINE
   
  SUBROUTINE WriteOutVlocRloc() BIND(c, name='CSASp_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CSASp contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       IF( .NOT. check_CSASx() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_CSASx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_CSASx(1)

  END SUBROUTINE
    
  SUBROUTINE DisplayOutVlocRloc() BIND(c, name='CSASp_DisplayOutVlocRloc')
       IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CSASp contacts

       IF( .NOT. check_CSASx() ) RETURN

       CALL write_xxx_Vloc_Rloc_CSASx(6)

  END SUBROUTINE

  SUBROUTINE DisplayProxTactors() BIND(c, name='CSASp_DisplayProxTactors')
       IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       IF( .NOT. check_CSASx() ) RETURN

       CALL display_prox_tactors_CSASx

  END SUBROUTINE

  subroutine ReadIniVlocRloc(step) bind(c, name='CSASp_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_CSASx() ) return

     call read_ini_Vloc_Rloc_CSASx(step)

  end subroutine

!!!---------------------------------------------------------------------

  SUBROUTINE SkipAutoContactCSASp()  BIND(c, name='CSASp_SkipAutoContact')
    IMPLICIT NONE

    CALL skip_autocontact_CSASp

  END SUBROUTINE

!!!---------------------------------------------------------------------
  !> this function allows non symetric detection i.e. only one interaction
  !> is kept when two bodies with candidate and antagonist contactors see
  !> each other 
  subroutine SetNonSymmetricDetection() bind(c, name='CSASp_SetNonSymmetricDetection')
    implicit none      
 
    call set_nonsymmetric_detection_CSASp

  end subroutine SetNonSymmetricDetection


!!!---------------------------------------------------------------------
  SUBROUTINE TrimCSASp() bind(c, name='CSASp_Trim')
       IMPLICIT NONE

       CALL trim_CSASp

  END SUBROUTINE TrimCSASp
!!!---------------------------------------------------------------------
  SUBROUTINE SetTrimAngleCSASp(angle) bind(c, name='CSASp_SetTrimAngle')
       IMPLICIT NONE
       real(kind=c_double), intent(in), value :: angle
       
       CALL set_trim_angle_CSASp(angle)

  END SUBROUTINE SetTrimAngleCSASp

!!!---------------------------------------------------------------------  
  SUBROUTINE AddReac() bind(c, name='CSASp_AddReac')
       IMPLICIT NONE

       CALL add_reac_CSASp

  END SUBROUTINE AddReac

!!!---------------------------------------------------------------------  
  SUBROUTINE AssumeOldFiles() bind(c, name='CSASp_AssumeOldFiles')
       IMPLICIT NONE

       CALL assume_old_files_CSASp

  END SUBROUTINE AssumeOldFiles

!!!---------------------------------------------------------------------  
  subroutine CleanMemory() bind(c, name='CSASp_CleanMemory')
    implicit none

    call clean_memory_CSASp
    call SelectProxTactors(reset=1,use_external=0)
    !call SelectProxTactors(1)

  end subroutine

END MODULE wrap_CSASx
