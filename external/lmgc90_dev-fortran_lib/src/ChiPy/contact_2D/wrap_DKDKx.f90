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
module wrap_DKDKx

  use ISO_C_BINDING
  
  use DKDKx,only: &
       smooth_computation_DKDKx, &
       compute_box_DKDKx, &
       read_ini_Vloc_Rloc_DKDKx, &
       write_xxx_Vloc_Rloc_DKDKx, &
       set_periodic_data_DKDKx, &
       set_friction_model_DKDKx, &
       coor_prediction_DKDKx, &
       creation_tab_visu_DKDKx, &
       compute_contact_DKDKx, &
       display_prox_tactors_DKDKx, &
       RUN_DKDKx, &
       CHECK_DKDKx, &
       get_write_Vloc_Rloc_DKDKx, &
       set_vav_DKDKx, &
       set_surface_sectors_DKDKx, &
       update_WS_sector_DKDKx, &
       compute_stress_DKDKx, &
       compute_betai_DKDKx , &
       clean_memory_DKDKx, &
       compute_czm_energy_DKDKx, &
       get_CZM_energy_DKDKx

  use utilities, only : faterr

  public

contains
  
!!!-------------------------------------------------------
  subroutine SelectProxTactors(reset) bind(c, name='DKDKx_SelectProxTactors')
    !! PURPOSE
    !!  contact detection between DISKx tactors.
    !!  first recup coordinate prediction, then proceed to a box selection to found rough
    !!  contact list and finally compute the final contact list
    
    use timer
    implicit none
    integer(kind=c_int), intent(in), value :: reset
    !
    logical :: is_initialized = .false.
    integer(kind=4), save :: timer_id = 0
    
    if( reset /= 0 ) then
       is_initialized = .false.
       return
    end if
    
    if( .not. check_DKDKx() ) return
    
    !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[DKDKx] select      ')
    call start_itimer(timer_id)
    
    if (.not. is_initialized) then
       call compute_box_DKDKx
       is_initialized = .true.
    endif
    
    call coor_prediction_DKDKx
    
    if (RUN_DKDKx()) call creation_tab_visu_DKDKx
    
    call compute_contact_DKDKx
    
    call stop_itimer(timer_id)
    
  end subroutine SelectProxTactors
!!!--------------------------------------------------
  subroutine SmoothForceComputation() bind(c, name='DKDKx_SmoothForceComputation')
    implicit none
    !! PURPOSE
    !!
    if( .not. check_DKDKx() ) return
    
    call smooth_computation_DKDKx
    
  end subroutine SmoothForceComputation
!!!--------------------------------------------------
  subroutine UseVaVDetection(nbN) bind(c, name='DKDKx_UseVaVDetection')
    implicit none
    integer(c_int), intent(in), value :: nbN
    
    if( .not. check_DKDKx() ) return
    
    call set_vav_DKDKx(nbN)
    
  end subroutine UseVaVDetection
!!!----------------------------------------------------
  subroutine WriteLastVlocRloc() bind(c, name='DKDKx_WriteLastVlocRloc')
    implicit none
    !! PURPOSE
    !!  write last local values (relative velocity, forces, local frame) of all
    !!  DKDKx contacts
    
    if( .not. check_DKDKx() ) return
    
    call write_xxx_Vloc_Rloc_DKDKx(2)
    
  end subroutine WriteLastVlocRloc
!!!--------------------------------------------------   
  subroutine WriteOutVlocRloc() bind(c, name='DKDKx_WriteOutVlocRloc')
    implicit none
    logical :: write_Vloc_Rloc
    !! PURPOSE
    !!  write local values (relative velocity, forces, local frame) of all
    !!  DKDKx contacts
    
    if( .not. check_DKDKx() ) return
    
    write_Vloc_Rloc = get_write_Vloc_Rloc_DKDKx()
    
    if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_DKDKx(1)
    
  end subroutine WriteOutVlocRloc
!!!--------------------------------------------------    
  subroutine DisplayOutVlocRloc() bind(c, name='DKDKx_DisplayOutVlocRloc')
    implicit none
    !! PURPOSE
    !!  display local values (relative velocity, forces, local frame) of all
    !!  DKDKx contacts
    
    if( .not. check_DKDKx() ) return
    
    call write_xxx_Vloc_Rloc_DKDKx(6)
    
  end subroutine DisplayOutVlocRloc
!!!--------------------------------------------------
  subroutine DisplayProxTactors() bind(c, name='DKDKx_DisplayProxTactors')
    implicit none
    !! PURPOSE
    !!  display contacts
    
    if( .not. check_DKDKx() ) return
    
    call display_prox_tactors_DKDKx
    
  end subroutine DisplayProxTactors
!!!--------------------------------------------------
  subroutine ReadIniVlocRloc(step) bind(c, name='DKDKx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step
    
    if( .not. check_DKDKx() ) return
    
    call read_ini_Vloc_Rloc_DKDKx(step)
    
  end subroutine ReadIniVlocRloc
  
!!!--------------------------------------------------
  subroutine SetPeriodicCondition(rvalue1) bind(c, name='DKDKx_SetPeriodicCondition')
    implicit none
    real(c_double), intent(in), value :: rvalue1
    logical      :: periodic
    !! PURPOSE
    !!  initialise data for simulation using periodic condition
    
    if( .not. check_DKDKx() ) return
    
    PERIODIC=.true.
    call set_periodic_data_DKDKx(rvalue1,PERIODIC)
    
  end subroutine SetPeriodicCondition
  
!!!--------------------------------------------------
  subroutine SetFrictionModel(cvalue1_c) bind(c, name='DKDKx_SetFrictionModel')
    implicit none
    character(C_CHAR), dimension(3)   :: cvalue1_c
    character(len=3) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,3
       cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    if( .not. check_DKDKx() ) return
    
    call set_friction_model_DKDKx(cvalue1)
    
  end subroutine SetFrictionModel  
!!!----------------------------------
  subroutine SetSurfaceSectorsDKDKx(nbsect) bind(c, name='DKDKx_SetSurfaceSectors')
    implicit none
    integer(c_int),intent(in), value :: nbsect
    
    call set_surface_sectors_DKDKx(nbsect)
    
  end subroutine SetSurfaceSectorsDKDKx
!!!----------------------------------
  subroutine UpdateSurfaceEnergySector() bind(c, name='DKDKx_UpdateSurfaceEnergySector')
    implicit none
     
    call update_WS_sector_DKDKx()
    
  end subroutine UpdateSurfaceEnergySector
!!!----------------------------------
  subroutine ComputeStressDKDKx() bind(c, name='DKDKx_ComputeStress')
    implicit none
    
    call compute_stress_DKDKx()
    
  end subroutine ComputeStressDKDKx
!!!----------------------------------
  subroutine ComputeBetaiDKDKx() bind(c, name='DKDKx_ComputeBetai')
    implicit none
    
    call compute_betai_DKDKx()
    
  end subroutine ComputeBetaiDKDKx
!!!----------------------------------
  subroutine ComputeCZMEnergyDKDKx() bind(c, name='DKDKx_ComputeCZMEnergy')
    implicit none
    
    call compute_czm_energy_DKDKx()
    
  end subroutine ComputeCZMEnergyDKDKx
!!!----------------------------------
  subroutine GetCZMEnergyDKDKx(icdan,energy) bind(c, name='DKDKx_GetCZMEnergy')
    implicit none
    integer(C_INT), intent(in), value :: icdan
    real(C_DOUBLE), dimension(4)      :: energy
    real(C_DOUBLE)                    :: stored,damage,failure,cohesion
    
    call get_CZM_energy_DKDKx(icdan,stored,damage,failure,cohesion)
    
    energy(1) = stored
    energy(2) = damage
    energy(3) = failure
    energy(4) = cohesion
    
  end subroutine GetCZMEnergyDKDKx
!!!----------------------------------
  subroutine CleanMemory() bind(c, name='DKDKx_CleanMemory')
    implicit none
    
    call clean_memory_DKDKx
      !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)
    
  end subroutine CleanMemory
!!!----------------------------------
  
end module wrap_DKDKx
