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
module wrap_CLALp

  use ISO_C_BINDING

  use CLALp,only: &
       compute_box_CLALp, &
       read_ini_Vloc_Rloc_CLALp, &
       write_xxx_Vloc_Rloc_CLALp, &
       display_prox_tactors_CLALp, &
       coor_prediction_CLALp, &
       creation_tab_visu_CLALp, &
       compute_contact_CLALp, &
       external_detection_CLALp, &
       update_wear_CLALp, &
       RUN_CLALp, &
       CHECK_CLALp, &
       get_write_Vloc_Rloc_CLALp, &
       set_nonsymmetric_detection_CLALp, &
       trim_CLALp, &
       is_external_detection_CLALp, &       
       clean_memory_CLALp

  use utilities, only : faterr    
  
contains
  
!!!-------------------------------------------------------

    subroutine SelectProxTactors(reset,use_external) bind(c, name='CLALp_SelectProxTactors')
      !! PURPOSE
      !!  contact detection between CLxxx and ALpxx tactors.
      !!  first recup coordinate prediction, then proceed to a box selection to found rough
      !!  contact list and finally compute the final contact list

      use timer
      implicit none
      integer(kind=c_int), intent(in), value :: reset
      integer(kind=c_int), intent(in), value :: use_external
      !
      logical :: is_initialized = .false.
      integer(kind=4), save :: timer_id = 0

      if (use_external > 0) call is_external_detection_CLALp()
      
      if( reset /= 0 ) then
        is_initialized = .false.
        return
      end if

      if( .not. check_CLALp() ) return
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CLALp] select      ')
      call start_itimer(timer_id)

      if (.not. is_initialized) then
        call compute_box_CLALp
        is_initialized = .true.
      endif

      call coor_prediction_CLALp

      if( use_external > 0 ) then
        call external_detection_CLALp()
        call stop_itimer(timer_id)
        return
      end if
                                                       !12345678901234567890
      if (RUN_CLALp()) call creation_tab_visu_CLALp

      call compute_contact_CLALp

      call stop_itimer(timer_id)

    end subroutine

!!!--------------------------------------------------

    subroutine UpdateWear() bind(c, name='CLALp_UpdateWear')
      implicit none
       !! PURPOSE
       !!  

       if( .not. check_CLALp() ) return

       call update_wear_CLALp

    end subroutine

!!!----------------------------------------------------

    subroutine WriteLastVlocRloc() bind(c, name='CLALp_WriteLastVlocRloc')
      implicit none
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all

       if( .not. check_CLALp() ) return

       call write_xxx_Vloc_Rloc_CLALp(2)

    end subroutine
   
!!!----------------------------------------------------

    subroutine WriteOutVlocRloc() bind(c, name='CLALp_WriteOutVlocRloc')
      implicit none
      logical :: write_Vloc_Rloc

       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CLALp contacts

       if( .not. check_CLALp() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_CLALp()

       if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_CLALp(1)

    end subroutine
    
!!!----------------------------------------------------

    subroutine DisplayOutVlocRloc() bind(c, name='CLALp_DisplayOutVlocRloc')
      implicit none
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CLALp contacts

       if( .not. check_CLALp() ) return

       call write_xxx_Vloc_Rloc_CLALp(6)

    end subroutine

!!!----------------------------------------------------

    subroutine DisplayProxTactors() bind(c, name='CLALp_DisplayProxTactors')
      implicit none
       !! PURPOSE
       !!  display contacts

       if( .not. check_CLALp() ) return

       call display_prox_tactors_CLALp

    end subroutine

!!!----------------------------------------------------

    subroutine ReadIniVlocRloc(step) bind(c, name='CLALp_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_CLALp() ) return

       call read_ini_Vloc_Rloc_CLALp(step)

    end subroutine

!!!---------------------------------------------------------------------
    !> this function allows non symmetric detection i.e. only one interaction
    !> is kept when two bodies with candidate and antagonist contactors see
    !> each other 
    subroutine SetNonSymmetricDetection() bind(c, name='CLALp_SetNonSymmetricDetection')
       implicit none      
 
       call set_nonsymmetric_detection_CLALp

    end subroutine SetNonSymmetricDetection


!!!---------------------------------------------------------------------
  SUBROUTINE TrimCLALp() bind(c, name='CLALp_Trim')
       IMPLICIT NONE

       CALL trim_CLALp

  END SUBROUTINE 

  subroutine CleanMemory() bind(c, name='CLALp_CleanMemory')
    implicit none

    call clean_memory_CLALp
    call SelectProxTactors(1, 0)

  end subroutine

end module wrap_CLALp
