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
MODULE wrap_SPDCx

  USE ISO_C_BINDING

  USE SPDCx,ONLY:&
       coor_prediction_SPDCx,&
       CHECK_SPDCx,&
       RUN_SPDCx, &
       get_write_Vloc_Rloc_SPDCx, &
       read_ini_Vloc_Rloc_SPDCx,&
       write_xxx_Vloc_Rloc_SPDCx,&
       smooth_computation_SPDCx, &
       compute_box_SPDCx, &
       creation_tab_visu_SPDCx, &
       compute_contact_SPDCx, &
       display_prox_tactors_SPDCx,&
       clean_memory_SPDCx

  use utilities, only : faterr
  
CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(C, name = 'SPDCx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between SPHER and CYLDx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       logical :: is_initialized = .FALSE.
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       if( .not. check_SPDCx() ) return

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[SPDCx] select      ')
       call start_itimer(timer_id)

       if (.not. is_initialized) then
         call compute_box_SPDCx
         is_initialized = .TRUE.
       endif

       CALL coor_prediction_SPDCx

       IF (RUN_SPDCx()) CALL creation_tab_visu_SPDCx

       CALL compute_contact_SPDCx

       call stop_itimer(timer_id)

  END SUBROUTINE

!!!--------------------------------------------------
 
  SUBROUTINE SmoothForceComputation() bind(C, name = 'SPDCx_SmoothForceComputation')
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       IMPLICIT NONE

       if( .not. check_SPDCx() ) return

       CALL smooth_computation_SPDCx

  END SUBROUTINE SmoothForceComputation

!!!----------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() bind(C, name = 'SPDCx_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  SPDCx contacts
       IMPLICIT NONE

       if( .not. check_SPDCx() ) return

       CALL write_xxx_Vloc_Rloc_SPDCx(2)

  END SUBROUTINE WriteLastVlocRloc

  SUBROUTINE WriteOutVlocRloc() bind(C, name = 'SPDCx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  SPDCx contacts
       IMPLICIT NONE


       if( .not. check_SPDCx() ) return

       IF (get_write_Vloc_Rloc_SPDCx()) CALL write_xxx_Vloc_Rloc_SPDCx(1)

  END SUBROUTINE WriteOutVlocRloc

  SUBROUTINE DisplayOutVlocRloc() bind(C, name = 'SPDCx_DisplayOutVlocRloc')
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  SPDCx contacts
       IMPLICIT NONE

       if( .not. check_SPDCx() ) return

       CALL write_xxx_Vloc_Rloc_SPDCx(6)

  END SUBROUTINE DisplayOutVlocRloc

  SUBROUTINE DisplayProxTactors() bind(C, name = 'SPDCx_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       if( .not. check_SPDCx() ) return

       CALL display_prox_tactors_SPDCx

  END SUBROUTINE DisplayProxTactors

  subroutine ReadIniVlocRloc(step) bind(c, name='SPDCx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_SPDCx() ) return

     call read_ini_Vloc_Rloc_SPDCx(step)

  end subroutine

!!!---------------------------------------------------------------------

  subroutine CleanMemory() bind(c, name='SPDCx_CleanMemory')
    implicit none

    call clean_memory_SPDCx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_SPDCx
