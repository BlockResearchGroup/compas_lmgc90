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
module wrap_DKALp

  use ISO_C_BINDING

  use DKALp,only: &
       compute_box_DKALp, &
       read_ini_Vloc_Rloc_DKALp, &
       write_xxx_Vloc_Rloc_DKALp, &
       coor_prediction_DKALp, &
       creation_tab_visu_DKALp, &
       compute_contact_DKALp, &
       display_prox_tactors_DKALp, &
       RUN_DKALp, &
       CHECK_DKALp, &
       get_write_Vloc_Rloc_DKALp, &
       clean_memory_DKALp

  use utilities, only : faterr

contains
  
    subroutine SelectProxTactors(reset) bind(c, name='DKALp_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between DISKx and ALpxx tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       implicit none
       integer(kind=c_int), intent(in), value :: reset
       !
       logical :: is_initialized = .false.
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       if( .not. check_DKALp() ) return

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[DKALp] select      ')
       call start_itimer(timer_id)

       if (.not. is_initialized) then
         call compute_box_DKALp
         is_initialized = .true.
       endif

       call coor_prediction_DKALp
       
       if (RUN_DKALp()) call creation_tab_visu_DKALp

       call compute_contact_DKALp

       call stop_itimer(timer_id)

    end subroutine

!!!--------------------------------------------------

    subroutine WriteLastVlocRloc() bind(c, name='DKALp_WriteLastVlocRloc')
      implicit none
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts

       if( .not. check_DKALp() ) return

       call write_xxx_Vloc_Rloc_DKALp(2)

    end subroutine
   
    subroutine WriteOutVlocRloc() bind(c, name='DKALp_WriteOutVlocRloc')
      implicit none
      logical :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts

       if( .not. check_DKALp() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_DKALp()

       if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_DKALp(1)

    end subroutine
    
    subroutine DisplayOutVlocRloc() bind(c, name='DKALp_DisplayOutVlocRloc')
      implicit none
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKALp contacts

       if( .not. check_DKALp() ) return

       call write_xxx_Vloc_Rloc_DKALp(6)

    end subroutine

    subroutine DisplayProxTactors() bind(c, name='DKALp_DisplayProxTactors')
      implicit none
       !! PURPOSE
       !!  display contacts

       if( .not. check_DKALp() ) return

       call display_prox_tactors_DKALp

    end subroutine

    subroutine ReadIniVlocRloc(step) bind(c, name='DKALp_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_DKALp() ) return

       call read_ini_Vloc_Rloc_DKALp(step)

    end subroutine

    subroutine CleanMemory() bind(c, name='DKALp_CleanMemory')
      implicit none

      call clean_memory_DKALp
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine

end module wrap_DKALp
