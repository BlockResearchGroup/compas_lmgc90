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
module wrap_CLJCx

  use ISO_C_BINDING

  use CLJCx,only: &
       compute_box_CLJCx, &
       read_ini_Vloc_Rloc_CLJCx, &
       write_xxx_Vloc_Rloc_CLJCx, &
       display_prox_tactors_CLJCx, &
       coor_prediction_CLJCx, &
       creation_tab_visu_CLJCx, &
       compute_contact_CLJCx, &
       RUN_CLJCx, &
       CHECK_CLJCx, &
       get_write_Vloc_Rloc_CLJCx,&
       clean_memory_CLJCx
 
  use utilities, only : faterr    
 
contains
  
  subroutine SelectProxTactors(reset) bind(c, name='CLJCx_SelectProxTactors')
    use timer
    !! PURPOSE
    !!  contact detection between CLxxx and JONCx tactors.
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

    if( .not. CHECK_CLJCx() ) return
    
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CLJCx] select      ')
    call start_itimer(timer_id)

    if (.not. is_initialized) then
       call compute_box_CLJCx
       is_initialized = .true.
    endif
    
    call coor_prediction_CLJCx
    
    if (RUN_CLJCx()) call creation_tab_visu_CLJCx
    
    call compute_contact_CLJCx
    
    call stop_itimer(timer_id)

  end subroutine SelectProxTactors
!!!--------------------------------------------------
  
  subroutine WriteLastVlocRloc() bind(c, name='CLJCx_WriteLastVlocRloc')
    implicit none
    !! PURPOSE
    !!  write last local values (relative velocity, forces, local frame) of all
    
    if( .not. CHECK_CLJCx() ) return
    
    call write_xxx_Vloc_Rloc_CLJCx(2)
    
  end subroutine WriteLastVlocRloc
  
  subroutine WriteOutVlocRloc() bind(c, name='CLJCx_WriteOutVlocRloc')
    implicit none
    logical :: write_Vloc_Rloc
    !! PURPOSE
    !!  write local values (relative velocity, forces, local frame) of all
    !!  CLJCx contacts
    
    if( .not. CHECK_CLJCx() ) return
    
    write_Vloc_Rloc = get_write_Vloc_Rloc_CLJCx()
    
    if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_CLJCx(1)
    
  end subroutine WriteOutVlocRloc
  
  subroutine DisplayOutVlocRloc() bind(c, name='CLJCx_DisplayOutVlocRloc')
    implicit none
    !! PURPOSE
    !!  display local values (relative velocity, forces, local frame) of all
    !!  CLJCx contacts
    
    if( .not. CHECK_CLJCx() ) return
    
    call write_xxx_Vloc_Rloc_CLJCx(6)
    
    return 
  end subroutine DisplayOutVlocRloc
  
  subroutine DisplayProxTactors() bind(c, name='CLJCx_DisplayProxTactors')
    implicit none
    !! PURPOSE
    !!  display contacts
    
    if( .not. CHECK_CLJCx() ) return
    
    call display_prox_tactors_CLJCx
    
  end subroutine DisplayProxTactors
  
  subroutine ReadIniVlocRloc(step) bind(c, name='CLJCx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_CLJCx() ) return

     call read_ini_Vloc_Rloc_CLJCx(step)

  end subroutine

  subroutine CleanMemory() bind(c, name='CLJCx_CleanMemory')
    implicit none

    call clean_memory_CLJCx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

end module wrap_CLJCx
