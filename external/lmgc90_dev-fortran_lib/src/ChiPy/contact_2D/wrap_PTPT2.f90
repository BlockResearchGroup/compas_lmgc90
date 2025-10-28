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
MODULE wrap_PTPT2

  USE ISO_C_BINDING

  USE PTPT2,ONLY: &
       compute_box_PTPT2, &
       read_ini_Vloc_Rloc_PTPT2, &
       write_xxx_Vloc_Rloc_PTPT2, &
       coor_prediction_PTPT2, &
       compute_contact_PTPT2, &
       display_prox_tactors_PTPT2, &
       RUN_PTPT2, &
       CHECK_PTPT2, &
       load_network_PTPT2, &
       get_write_Vloc_Rloc_PTPT2, &
       set_tol_PTPT2, &
       load_params_PTPT2, &
       set_explicit_local_frame_PTPT2, &
       use_current_nonuc0_PTPT2, &
       clean_memory_PTPT2
  
  USE utilities, ONLY : faterr      

CONTAINS
  
    SUBROUTINE SelectProxTactors(reset) BIND(c, name='PTPT2_SelectProxTactors')
      use timer
      !! PURPOSE
      !!  contact detection between PT2Dx tactors.
      !!  first recup coordinate prediction, then proceed to a box selection to found rough
      !!  contact list and finally compute the final contact list

      IMPLICIT NONE
      integer(kind=c_int), intent(in), value :: reset
      !
      LOGICAL :: is_initialized = .FALSE.
      integer(kind=4), save :: timer_id = 0

      if ( reset /= 0 ) then
        is_initialized = .false.
        return
      end if

      IF( .NOT. check_PTPT2() ) RETURN

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PTPT2] select      ')
      call start_itimer(timer_id)

      IF (.NOT. is_initialized) THEN
        CALL compute_box_PTPT2
        is_initialized = .TRUE.
      ENDIF
     
      CALL coor_prediction_PTPT2
      
      CALL compute_contact_PTPT2

      call stop_itimer(timer_id)

    END SUBROUTINE

!!!--------------------------------------------------

    SUBROUTINE WriteLastVlocRloc() BIND(c, name='PTPT2_WriteLastVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts

       IF( .NOT. check_PTPT2() ) RETURN

       CALL write_xxx_Vloc_Rloc_PTPT2(2)

    END SUBROUTINE
   
    SUBROUTINE WriteOutVlocRloc() BIND(c, name='PTPT2_WriteOutVlocRloc')
      IMPLICIT NONE
      LOGICAL :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts

       IF( .NOT. check_PTPT2() ) RETURN

       write_Vloc_Rloc = get_write_Vloc_Rloc_PTPT2()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_PTPT2(1)

    END SUBROUTINE
    
    SUBROUTINE DisplayOutVlocRloc() BIND(c, name='PTPT2_DisplayOutVlocRloc')
      IMPLICIT NONE
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PTPT2 contacts

       IF( .NOT. check_PTPT2() ) RETURN

       CALL write_xxx_Vloc_Rloc_PTPT2(6)

    END SUBROUTINE

    SUBROUTINE DisplayProxTactors() BIND(c, name='PTPT2_DisplayProxTactors')
      IMPLICIT NONE
       !! PURPOSE
       !!  display contacts

       IF( .NOT. check_PTPT2() ) RETURN

       CALL display_prox_tactors_PTPT2

    END SUBROUTINE

    subroutine ReadIniVlocRloc(step) bind(c, name='PTPT2_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_PTPT2() ) return

       call read_ini_Vloc_Rloc_PTPT2(step)

    end subroutine

  SUBROUTINE LoadNetwork() BIND(C, name = 'PTPT2_LoadNetwork')
       !! PURPOSE
       !!  read a ptpt2 network from file 
       IMPLICIT NONE

       IF( .NOT. check_PTPT2() ) RETURN

       CALL  load_network_PTPT2

  END SUBROUTINE LoadNetwork

    SUBROUTINE SetTol(val) bind(c, name='PTPT2_SetTolerance')
       implicit none
       real(c_double), intent(in), value :: val

       CALL set_tol_PTPT2(val)

  END SUBROUTINE SetTol

  SUBROUTINE LoadParams() BIND(C, name = 'PTPT2_LoadParams')
     !! PURPOSE
     !!  read a ptpt2 surface and l0 from file 
     IMPLICIT NONE

       CALL  load_params_PTPT2

  END SUBROUTINE LoadParams

  SUBROUTINE SetExplicitLocalFrame() BIND(C, name = 'PTPT2_SetExplicitLocalFrame')
       IMPLICIT NONE

       CALL  set_explicit_local_frame_PTPT2

  END SUBROUTINE SetExplicitLocalFrame


  subroutine useCurrentNonuc0(no0) bind(c, name = 'PTPT2_UseCurrentNonuc0')
       !! purpose
       !! use nonuc0 given in file or use GetCoor to compute nonuc0 
       implicit none
       integer(kind=c_int), intent(in), value :: no0

       call  use_current_nonuc0_PTPT2(no0)

  end subroutine useCurrentNonuc0


  subroutine CleanMemory() bind(c, name='PTPT2_CleanMemory')
      implicit none

      call clean_memory_PTPT2
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

  end subroutine

END MODULE wrap_PTPT2
