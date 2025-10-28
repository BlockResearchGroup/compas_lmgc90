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
MODULE wrap_PLALp

  USE ISO_C_BINDING

  USE PLALp,ONLY: &
       compute_box_PLALp, &
       read_ini_Vloc_Rloc_PLALp, &
       write_xxx_Vloc_Rloc_PLALp, &
       coor_prediction_PLALp, &
       creation_tab_visu_PLALp, &
       compute_contact_PLALp, &
       display_prox_tactors_PLALp, &
       RUN_PLALp, &
       CHECK_PLALp, &
       get_write_Vloc_Rloc_PLALp, &
       clean_memory_PLALp

  USE utilities, ONLY : faterr    

CONTAINS
  
    SUBROUTINE SelectProxTactors(reset) BIND(c, name='PLALp_SelectProxTactors')
      use timer
      !! PURPOSE
      !!  contact detection between DISKx and POLYG tactors.
      !!  first recup coordinate prediction, then proceed to a box selection to found rough
      !!  contact list and finally compute the final contact list
      IMPLICIT NONE
      integer(kind=c_int), intent(in), value :: reset
      !
      LOGICAL :: is_initialized = .FALSE.
      integer(kind=4), save :: timer_id = 0

      if( reset /= 0 ) then
        is_initialized = .false.
        return
      end if

      IF( .NOT. check_PLALp() ) RETURN

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PLALp] select      ')
      call start_itimer(timer_id)

      IF (.NOT. is_initialized) THEN
        CALL compute_box_PLALp
        is_initialized = .TRUE.
      ENDIF

      CALL coor_prediction_PLALp

      IF (RUN_PLALp()) CALL creation_tab_visu_PLALp

      CALL compute_contact_PLALp

      call stop_itimer(timer_id)

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE WriteLastVlocRloc() BIND(c, name='PLALp_WriteLastVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts

       IF( .NOT. check_PLALp() ) RETURN

       CALL write_xxx_Vloc_Rloc_PLALp(2)

    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc() BIND(c, name='PLALp_WriteOutVlocRloc')
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts

       IF( .NOT. check_PLALp() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_PLALp()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_PLALp(1)

    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc() BIND(c, name='PLALp_DisplayOutVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLALp contacts

       IF( .NOT. check_PLALp() ) RETURN

       CALL write_xxx_Vloc_Rloc_PLALp(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors() BIND(c, name='PLALp_DisplayProxTactors')
      IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       IF( .NOT. check_PLALp() ) RETURN

       CALL display_prox_tactors_PLALp

    END SUBROUTINE

    subroutine ReadIniVlocRloc(step) bind(c, name='PLALp_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_PLALp() ) return

       call read_ini_Vloc_Rloc_PLALp(step)

    end subroutine

    subroutine CleanMemory() bind(c, name='PLALp_CleanMemory')
      implicit none

      call clean_memory_PLALp
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine

END MODULE wrap_PLALp
