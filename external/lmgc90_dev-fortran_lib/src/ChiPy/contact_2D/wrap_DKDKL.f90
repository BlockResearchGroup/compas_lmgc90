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
module wrap_DKDKL

  use ISO_C_BINDING

  use DKDKL,only: &
       get_nb_DKDKL, &
       smooth_computation_DKDKL, &
       compute_box_DKDKL, &
       read_ini_Vloc_Rloc_DKDKL, &
       set_periodic_data_DKDKL, &
       write_xxx_Vloc_Rloc_DKDKL, &
       coor_prediction_DKDKL, &
       creation_tab_visu_DKDKL, &
       compute_contact_DKDKL, &
       display_prox_tactors_DKDKL, &
       RUN_DKDKL, &
       CHECK_DKDKL, &
       get_write_Vloc_Rloc_DKDKL, &
       clean_memory_DKDKL

  use utilities, only : faterr

contains
  
!!!-------------------------------------------------------

  subroutine SelectProxTactors(reset) bind(c, name='DKDKL_SelectProxTactors')
    use timer
    !! PURPOSE
    !!  contact detection between DISKx and DISKL tactors.
    !!  first recup coordinate prediction, then proceed to a box selection to found rough
    !!  contact list and finally compute the final contact list

    implicit none
    integer(kind=c_int), intent(in), value :: reset
    !
    logical      :: is_initialized = .false.
    integer(kind=4), save :: timer_id = 0

    if( reset /= 0 ) then
      is_initialized = .false.
      return
    end if

    if (.not. check_DKDKL() ) return

                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[DKDKL] select      ')
    call start_itimer(timer_id)

    if (.not. is_initialized) then
      call compute_box_DKDKL
      is_initialized = .true.
    endif


    call coor_prediction_DKDKL
       
    if (RUN_DKDKL()) call creation_tab_visu_DKDKL

    call compute_contact_DKDKL

    call stop_itimer(timer_id)

  end subroutine

!!!--------------------------------------------------

  subroutine SmoothForceComputation() bind(c, name='DKDKL_SmoothForceComputation')
    implicit none
       !! PURPOSE
       !!  recup values of local contact forces of the last time step

       if( .not. check_DKDKL() ) return

       call smooth_computation_DKDKL

  end subroutine

!!!----------------------------------------------------

  subroutine WriteLastVlocRloc() bind(c, name='DKDKL_WriteLastVlocRloc')
    implicit none
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts

       if( .not. check_DKDKL() ) return

       call write_xxx_Vloc_Rloc_DKDKL(2)

  end subroutine
   
  subroutine WriteOutVlocRloc() bind(c, name='DKDKL_WriteOutVlocRloc')
    implicit none
    logical      :: write_Vloc_Rloc
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts

       if( .not. check_DKDKL() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKL()

       if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_DKDKL(1)

  end subroutine
    
  subroutine DisplayOutVlocRloc() bind(c, name='DKDKL_DisplayOutVlocRloc')
    implicit none
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  DKDKL contacts

       if( .not. check_DKDKL() ) return

       call write_xxx_Vloc_Rloc_DKDKL(6)

  end subroutine

  subroutine DisplayProxTactors() bind(c, name='DKDKL_DisplayProxTactors')
    implicit none
       !! PURPOSE
       !!  display contacts

       if( .not. check_DKDKL() ) return

       call display_prox_tactors_DKDKL

  end subroutine

  subroutine ReadIniVlocRloc(step) bind(c, name='DKDKL_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_DKDKL() ) return

     call read_ini_Vloc_Rloc_DKDKL(step)

  end subroutine

!!!--------------------------------------------------
    subroutine SetPeriodicCondition(rvalue1) bind(c, name='DKDKL_SetPeriodicCondition')
      implicit none
      real(c_double), intent(in), value :: rvalue1
      logical      :: periodic
       !! PURPOSE
       !!  initialise data for simulation using periodic condition

       if( .not. check_DKDKL() ) return

       PERIODIC=.true.
       call set_periodic_data_DKDKL(rvalue1,PERIODIC)

    end subroutine

    subroutine CleanMemory() bind(c, name='DKDKL_CleanMemory')
      implicit none

      call clean_memory_DKDKL
      !call SelectProxTactors(reset=1)
      call SelectProxTactors(1)

    end subroutine

end module wrap_DKDKL
