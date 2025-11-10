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
module wrap_PLJCx

  use ISO_C_BINDING

  use PLJCx,only: &
       compute_box_PLJCx, &
       read_ini_Vloc_Rloc_PLJCx, &
       write_xxx_Vloc_Rloc_PLJCx, &
       coor_prediction_PLJCx, &
       creation_tab_visu_PLJCx, &
       compute_contact_PLJCx, &
       display_prox_tactors_PLJCx, &
       RUN_PLJCx, &
       CHECK_PLJCx, &
       get_write_Vloc_Rloc_PLJCx, &
       compute_stress_PLJCx, &
       clean_memory_PLJCx, &
       set_friction_model_PLJCx
  
  use utilities, only : faterr    

contains
  
!!!-------------------------------------------------------

    subroutine SelectProxTactors(reset) bind(c, name='PLJCx_SelectProxTactors')
      use timer
      !! PURPOSE
      !!  contact detection between POLYG and JONCx tactors.
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

      if( .not. check_PLJCx() ) return
      
                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PLJCx] select      ')
      call start_itimer(timer_id)

      if (.not. is_initialized) then
        call compute_box_PLJCx
        is_initialized = .true.
      endif

      call coor_prediction_PLJCx
       
      if (RUN_PLJCx()) call creation_tab_visu_PLJCx

      call compute_contact_PLJCx

      call stop_itimer(timer_id)

    end subroutine

!!!--------------------------------------------------

    subroutine WriteLastVlocRloc() bind(c, name='PLJCx_WriteLastVlocRloc')
      implicit none
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  PLJCx contacts

       if( .not. check_PLJCx() ) return

       call write_xxx_Vloc_Rloc_PLJCx(2)

    end subroutine
   
    subroutine WriteOutVlocRloc() bind(c, name='PLJCx_WriteOutVlocRloc')
      implicit none
      logical :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  PLJCx contacts

       if( .not. check_PLJCx() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_PLJCx()

       if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_PLJCx(1)

    end subroutine
    
    subroutine DisplayOutVlocRloc() bind(c, name='PLJCx_DisplayOutVlocRloc')
      implicit none
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  PLJCx contacts

       if( .not. check_PLJCx() ) return

       call write_xxx_Vloc_Rloc_PLJCx(6)

    end subroutine 

    subroutine DisplayProxTactors() bind(c, name='PLJCx_DisplayProxTactors')
      implicit none
       !! PURPOSE
       !!  display contacts

       if( .not. check_PLJCx() ) return

       call display_prox_tactors_PLJCx

    end subroutine

    subroutine ReadIniVlocRloc(step) bind(c, name='PLJCx_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_PLJCx() ) return

       call read_ini_Vloc_Rloc_PLJCx(step)

    end subroutine

    subroutine ComputeStressPLJCx() bind(c, name='PLJCx_ComputeStress')
      implicit none
      
      call compute_stress_PLJCx()
      
    end subroutine ComputeStressPLJCx
    
    subroutine CleanMemory() bind(c, name='PLJCx_CleanMemory')
      implicit none

      call clean_memory_PLJCx
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine
!!!--------------------------------------------------
  subroutine SetFrictionModel(cvalue1_c) bind(c, name='PLJCx_SetFrictionModel')
    implicit none
    character(C_CHAR), dimension(3)   :: cvalue1_c
    character(len=3) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,3
       cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    if( .not. check_PLJCx() ) return
    
    call set_friction_model_PLJCx(cvalue1)
    
  end subroutine SetFrictionModel  
!!!----------------------------------
end module wrap_PLJCx
