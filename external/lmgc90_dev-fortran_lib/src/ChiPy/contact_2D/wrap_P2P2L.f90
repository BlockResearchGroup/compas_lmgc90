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
MODULE wrap_P2P2L

  USE ISO_C_BINDING

  USE P2P2L,ONLY: &
       compute_box_P2P2L, &
       read_ini_Vloc_Rloc_P2P2L, &
       write_xxx_Vloc_Rloc_P2P2L, &
       coor_prediction_P2P2L, &
       compute_contact_P2P2L, &
       display_prox_tactors_P2P2L, &
       RUN_P2P2L, &
       CHECK_P2P2L, &
       get_write_Vloc_Rloc_P2P2L, &
       clean_memory_P2P2L

  USE utilities, ONLY : faterr    
  
CONTAINS
  
    SUBROUTINE SelectProxTactors(reset) BIND(c, name='P2P2L_SelectProxTactors')
      use timer
      !! PURPOSE
      !!  contact detection between PT2DL tactors.
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

      IF( .NOT. check_P2P2L() ) RETURN

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[P2P2L] select      ')
      call start_itimer(timer_id)

      IF (.NOT. is_initialized) THEN
        CALL compute_box_P2P2L
        is_initialized = .TRUE.
      ENDIF

      CALL coor_prediction_P2P2L
       
      CALL compute_contact_P2P2L

      call stop_itimer(timer_id)

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE WriteLastVlocRloc() BIND(c, name='P2P2L_WriteLastVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts

       IF( .NOT. check_P2P2L() ) RETURN

       CALL write_xxx_Vloc_Rloc_P2P2L(2)
 
    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc() BIND(c, name='P2P2L_WriteOutVlocRloc')
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all

       IF( .NOT. check_P2P2L() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_P2P2L()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_P2P2L(1)

    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc() BIND(c, name='P2P2L_DisplayOutVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  P2P2L contacts

       IF( .NOT. check_P2P2L() ) RETURN

       CALL write_xxx_Vloc_Rloc_P2P2L(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors() BIND(c, name='P2P2L_DisplayProxTactors')
      IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       CALL display_prox_tactors_P2P2L

    END SUBROUTINE

    subroutine ReadIniVlocRloc(step) bind(c, name='P2P2L_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_P2P2L() ) return

       call read_ini_Vloc_Rloc_P2P2L(step)

    end subroutine

    subroutine CleanMemory() bind(c, name='P2P2L_CleanMemory')
      implicit none

      call clean_memory_P2P2L
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine

END MODULE wrap_P2P2L
