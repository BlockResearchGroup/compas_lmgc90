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
MODULE wrap_CSPRx

  USE ISO_C_BINDING

  USE CSPRx,ONLY:&
       coor_prediction_CSPRx,&
       CHECK_CSPRx,&
       RUN_CSPRx, &
       get_write_Vloc_Rloc_CSPRx, &
       read_ini_Vloc_Rloc_CSPRx,&
       write_xxx_Vloc_Rloc_CSPRx,&
       creation_tab_visu_CSPRx, &
       compute_contact_CSPRx, &
       display_prox_tactors_CSPRx,&
       get_nb_CSPRx , &
!       set_xperiodic_data_CSPRx, &
!       set_yperiodic_data_CSPRx
       trim_CSPRx, &
       get_info_CSPRx, &
       smoothing_CSPRx, &
       clean_memory_CSPRx, &
       add_reac_CSPRx

  use utilities, only : faterr

CONTAINS

  !!!---------------------------------------------------------------------

  SUBROUTINE SelectProxTactors(reset) bind(c, name='CSPRx_SelectProxTactors')
       use timer
       !! PURPOSE
       !!  contact detection between POLYR tactors.
       !!  first recup coordinate prediction, then proceed to a box selection to found rough
       !!  contact list and finally compute the final contact list
       IMPLICIT NONE
       integer(kind=c_int), intent(in), value :: reset
       LOGICAL :: is_initialized = .false.
       LOGICAL :: RUN
       integer(kind=4), save :: timer_id = 0

       if( reset /= 0 ) then
         is_initialized = .false.
         return
       end if

       if ( .not. check_CSPRx() ) return

                                                        !12345678901234567890
       if( timer_id == 0 ) timer_id = get_new_itimer_ID('[CSPRx] select      ')
       call start_itimer(timer_id)

       IF (.NOT. is_initialized) THEN 
          is_initialized = .TRUE.
       ENDIF

       CALL coor_prediction_CSPRx

       RUN = RUN_CSPRx()
       
       IF (RUN) CALL creation_tab_visu_CSPRx

       CALL compute_contact_CSPRx

       call stop_itimer(timer_id)

  END SUBROUTINE SelectProxTactors
     
  !!!--------------------------------------------------

  SUBROUTINE WriteLastVlocRloc() bind(c, name='CSPRx_WriteLastVlocRloc')
       !! PURPOSE
       !!  write last local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       IMPLICIT NONE

       if( .not. check_CSPRx() ) return

       CALL write_xxx_Vloc_Rloc_CSPRx(2)

  END SUBROUTINE WriteLastVlocRloc
   
  SUBROUTINE WriteOutVlocRloc() bind(c, name='CSPRx_WriteOutVlocRloc')
       !! PURPOSE
       !!  write local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       IMPLICIT NONE
       LOGICAL write_Vloc_Rloc

       if( .not. check_CSPRx() ) return

       write_Vloc_Rloc = get_write_Vloc_Rloc_CSPRx()

       IF (write_Vloc_Rloc) CALL write_xxx_Vloc_Rloc_CSPRx(1)

  END SUBROUTINE WriteOutVlocRloc
    
  SUBROUTINE DisplayOutVlocRloc() bind(c, name='CSPRx_DisplayOutVlocRloc')
       !! PURPOSE
       !!  display local values (relative velocity, forces, local frame) of all
       !!  CSPRx contacts
       IMPLICIT NONE

       if( .not. check_CSPRx() ) return

       CALL write_xxx_Vloc_Rloc_CSPRx(6)

  END SUBROUTINE DisplayOutVlocRloc

  SUBROUTINE DisplayProxTactors() bind(c, name='CSPRx_DisplayProxTactors')
       !! PURPOSE
       !!  display contacts
       IMPLICIT NONE

       if( .not. check_CSPRx() ) return

       CALL display_prox_tactors_CSPRx

  END SUBROUTINE DisplayProxTactors

  subroutine ReadIniVlocRloc(step) bind(c, name='CSPRx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_CSPRx() ) return

     call read_ini_Vloc_Rloc_CSPRx(step)

  end subroutine

!!!---------------------------------------------------------------------
!!!---------------------------------------------------------------------
  SUBROUTINE TrimCSPRx() bind(c, name='CSPRx_Trim')
       IMPLICIT NONE

       CALL trim_CSPRx

  END SUBROUTINE 
!!!---------------------------------------------------------------------
!!!---------------------------------------------------------------------
  subroutine GetInfoCSPRx(i,ptr,dim1) BIND(c, name='CSPRx_GetInfo')
    implicit none
    integer(c_int),value :: i
    type(c_ptr)          :: ptr 
    integer(c_int)       :: dim1

    integer(kind=4), dimension(:),pointer :: info

    info => Get_Info_CSPRx(i)   

    if ( associated(info) ) then
      ptr  = c_loc(info(1))
      dim1 = size(info)
    else
      ptr  = c_null_ptr
      dim1 = 0
    end if

  end subroutine

  SUBROUTINE Smoothing() bind(c, name='CSPRx_Smoothing')
       !! PURPOSE
       !!  stock values of local contact forces for the next time step
       IMPLICIT NONE

       !if( .not. check_CSPRx() ) return

       CALL smoothing_CSPRx

  END SUBROUTINE Smoothing

  SUBROUTINE AddReac() bind(c, name='CSPRx_AddReac')
       IMPLICIT NONE

       CALL add_reac_CSPRx

  END SUBROUTINE AddReac

  
  subroutine CleanMemory() bind(c, name='CSPRx_CleanMemory')
    implicit none

    call clean_memory_CSPRx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)

  end subroutine

END MODULE wrap_CSPRx
