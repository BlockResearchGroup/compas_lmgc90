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
MODULE wrap_SPCDx

  USE ISO_C_BINDING

  USE SPCDx,ONLY:&
       coor_prediction_SPCDx,&
       CHECK_SPCDx,&
       RUN_SPCDx, &
       get_write_Vloc_Rloc_SPCDx, &
       read_ini_Vloc_Rloc_SPCDx,&
       write_xxx_Vloc_Rloc_SPCDx,&
       smooth_computation_SPCDx, &
       compute_box_SPCDx, &
       creation_tab_visu_SPCDx, &
       compute_contact_SPCDx, &
       display_prox_tactors_SPCDx,&
       clean_memory_SPCDx
  
  use utilities, only : faterr

CONTAINS

!!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(C, name = 'SPCDx_SelectProxTactors')
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

       if (.not. check_SPCDx() ) return

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[SPCDx] select      ')
       call start_itimer(timer_id)

       if (.not. is_initialized) then 
         call compute_box_SPCDx
         is_initialized = .TRUE.
       endif

       CALL coor_prediction_SPCDx

       IF (RUN_SPCDx()) CALL creation_tab_visu_SPCDx

       CALL compute_contact_SPCDx

       call stop_itimer(timer_id)

  END SUBROUTINE SelectProxTactors

!!!--------------------------------------------------

  SUBROUTINE SmoothForceComputation() bind(C, name = 'SPCDx_SmoothForceComputation')
       !! PURPOSE
       !!  recup values of local contact forces of the last time step
       IMPLICIT NONE

       if( .not. check_SPCDx() ) return

       CALL smooth_computation_SPCDx

  END SUBROUTINE

!!!----------------------------------------------------

  SUBROUTINE  WriteLastVlocRloc() bind(C, name = 'SPCDx_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  SPCDx contacts
       IMPLICIT NONE

       if( .not. check_SPCDx() ) return

       CALL write_xxx_Vloc_Rloc_SPCDx(2)

  END SUBROUTINE

  SUBROUTINE WriteOutVlocRloc() bind(C, name = 'SPCDx_WriteOutVlocRloc')  
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  SPCDx contacts
       IMPLICIT NONE
       LOGICAL :: write_Vloc_Rloc

       if( .not. check_SPCDx() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_SPCDx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_SPCDx(1)

  END SUBROUTINE

  SUBROUTINE DisplayOutVlocRloc() bind(C, name = 'SPCDx_DisplayOutVlocRloc')
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  SPCDx contacts
       IMPLICIT NONE

       if( .not. check_SPCDx() ) return

       CALL write_xxx_Vloc_Rloc_SPCDx(6)

  END SUBROUTINE

  SUBROUTINE DisplayProxTactors() bind(C, name = 'SPCDx_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       if( .not. check_SPCDx() ) return

       CALL display_prox_tactors_SPCDx

  END SUBROUTINE

  subroutine ReadIniVlocRloc(step) bind(c, name='SPCDx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_SPCDx() ) return

     call read_ini_Vloc_Rloc_SPCDx(step)

  end subroutine

!!!---------------------------------------------------------------------

  subroutine CleanMemory() bind(c, name='SPCDx_CleanMemory')
    implicit none
    
    call clean_memory_SPCDx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)
    

  end subroutine

END MODULE wrap_SPCDx
