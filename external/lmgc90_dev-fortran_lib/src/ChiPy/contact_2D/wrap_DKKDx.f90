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
module wrap_DKKDx

  use ISO_C_BINDING

  use DKKDx,only: &
       smooth_computation_DKKDx, &
       compute_box_DKKDx, &
       read_ini_Vloc_Rloc_DKKDx, &
       write_xxx_Vloc_Rloc_DKKDx, &
       coor_prediction_DKKDx, &
       creation_tab_visu_DKKDx, &
       compute_contact_DKKDx, &
       display_prox_tactors_DKKDx, &
       RUN_DKKDx, &
       CHECK_DKKDx, &
       get_write_Vloc_Rloc_DKKDx, &
       set_surface_sectors_DKKDx, &
       clean_memory_DKKDx

  use utilities, only : faterr

contains
  
    subroutine SelectProxTactors(reset) bind(c, name='DKKDx_SelectProxTactors')
      use timer
      !! PURPOSE
      !!  contact detection between DISKx tactors.
      !!  first recup coordinate prediction, then proceed to a box selection to found rough
      !!  contact list and finally compute the final contact list

      implicit none
      integer(kind=c_int), intent(in), value :: reset
      !
      logical :: is_initialized=.false.
      integer(kind=4), save :: timer_id = 0

      if( reset /= 0 ) then
        is_initialized = .false.
        return
      end if

      if( .not. check_DKKDx() ) return

                                                       !12345678901234567890
      if( timer_id == 0 ) timer_id = get_new_itimer_ID('[DKKDx] select      ')
      call start_itimer(timer_id)

      if (.not. is_initialized) then
        call compute_box_DKKDx
        is_initialized = .true.
      endif
      
      call coor_prediction_DKKDx
       
      if (RUN_DKKDx()) call creation_tab_visu_DKKDx

      call compute_contact_DKKDx

      call stop_itimer(timer_id)

    end subroutine

!!!--------------------------------------------------

    subroutine SmoothForceComputation() bind(c, name='DKKDx_SmoothForceComputation')
      implicit none
       !! PURPOSE
       !!  recup values of local contact forces of the last time step

       if( .not. check_DKKDx() ) return

       call smooth_computation_DKKDx

    end subroutine

!!!----------------------------------------------------

    subroutine WriteLastVlocRloc() bind(c, name='DKKDx_WriteLastVlocRloc')
      implicit none
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts

       if( .not. check_DKKDx() ) return

       call write_xxx_Vloc_Rloc_DKKDx(2)

    end subroutine
   
    subroutine WriteOutVlocRloc() bind(c, name='DKKDx_WriteOutVlocRloc')
      implicit none
      logical      :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts
       if( .not. check_DKKDx() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_DKKDx()

       if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_DKKDx(1)

    end subroutine
    
    subroutine DisplayOutVlocRloc() bind(c, name='DKKDx_DisplayOutVlocRloc')
      implicit none
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKKDx contacts

       if( .not. check_DKKDx() ) return

       call write_xxx_Vloc_Rloc_DKKDx(6)

    end subroutine

    subroutine DisplayProxTactors() bind(c, name='DKKDx_DisplayProxTactors')
      implicit none
       !! PURPOSE
       !!  display contacts

       if( .not. check_DKKDx() ) return

       call display_prox_tactors_DKKDx

    end subroutine

    subroutine ReadIniVlocRloc(step) bind(c, name='DKKDx_ReadIniVlocRloc')
      implicit none
      integer(c_int), intent(in), value :: step

       if( .not. check_DKKDx() ) return

       call read_ini_Vloc_Rloc_DKKDx(step)

    end subroutine

   subroutine SetSurfaceSectorsDKKDx(nbsect) bind(c, name='DKKDx_SetSurfaceSectors')
     implicit none
     integer(c_int),intent(in), value :: nbsect

     call set_surface_sectors_DKKDx(nbsect)

   end subroutine SetSurfaceSectorsDKKDx

    subroutine CleanMemory() bind(c, name='DKKDx_CleanMemory')
      implicit none

      call clean_memory_DKKDx
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine

end module wrap_DKKDx
