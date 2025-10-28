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
module wrap_PLPLx

  use ISO_C_BINDING
  
  use PLPLx,only: &
       CHECK_PLPLx, &
       clean_memory_PLPLx, &
       compute_betai_PLPLx, &
       compute_box_PLPLx, &
       compute_contact_PLPLx, &
       compute_contact_nc_PLPLx, &
       compute_stress_PLPLx,&
       coor_prediction_PLPLx, &
       creation_tab_visu_PLPLx, &
       display_prox_tactors_PLPLx, &
       get_write_Vloc_Rloc_PLPLx, &
       read_ini_Vloc_Rloc_PLPLx, &
       RUN_PLPLx, &
       set_big_polyg_tolerance_PLPLx, &
       set_periodic_data_PLPLx, &
       set_friction_model_PLPLx, &
       write_xxx_Vloc_Rloc_PLPLx, &
       compute_czm_energy_PLPLx, &
       get_CZM_energy_PLPLx, &
       set_shrink_polyg_faces_PLPLx

  use utilities, only : faterr,logmes    

  integer      :: detection_method = 1

contains
  
  subroutine SelectProxTactors(reset) bind(c, name='PLPLx_SelectProxTactors')
    use timer
    !! PURPOSE
    !!  contact detection between POLYG tactors.
    !!  first recup coordinate prediction, then proceed to a box selection to found rough
    !!  contact list and finally compute the final contact list
    implicit none
    integer(kind=c_int), intent(in), value :: reset
    !
    logical :: is_initialized = .false.
    character(len=80) :: cout
    integer(kind=4), save :: timer_id = 0
    
    if( reset /= 0 ) then
       is_initialized = .false.
       return
    end if
    
    if( .not. check_PLPLx() ) return
    
                                                     !12345678901234567890
    if( timer_id == 0 ) timer_id = get_new_itimer_ID('[PLPLx] select      ')
    call start_itimer(timer_id)

    if (.not. is_initialized) then
       call compute_box_PLPLx
       is_initialized = .true.
    endif
    
    call coor_prediction_PLPLx

    select case(detection_method)
    case(0)
      call FATERR('PLPLx_SelectProxTactors','You must specify a detection method')
    case(1)
      if (RUN_PLPLx()) call creation_tab_visu_PLPLx(.true.)
      call compute_contact_PLPLx
    case(2)
      if (RUN_PLPLx()) call creation_tab_visu_PLPLx(.false.)
      call compute_contact_nc_PLPLx
    end select
    
    call stop_itimer(timer_id)

  end subroutine SelectProxTactors
  
!!!--------------------------------------------------
  
  subroutine WriteLastVlocRloc() bind(c, name='PLPLx_WriteLastVlocRloc')
    implicit none
    !! PURPOSE
    !!  write last local values (relative velocity, forces, local frame) of all
    !!  PLPLx contacts
    
    if( .not. check_PLPLx() ) return
    
    call write_xxx_Vloc_Rloc_PLPLx(2)
    
  end subroutine WriteLastVlocRloc
  
!!!--------------------------------------------------
  
  subroutine WriteOutVlocRloc() bind(c, name='PLPLx_WriteOutVlocRloc')
    implicit none
    logical write_Vloc_Rloc
    !! PURPOSE
    !!  write local values (relative velocity, forces, local frame) of all
    !!  PLPLx contacts
    
    if( .not. check_PLPLx() ) return
    
    write_Vloc_Rloc = get_write_Vloc_Rloc_PLPLx()
    
    if (write_Vloc_Rloc) call write_xxx_Vloc_Rloc_PLPLx(1)
    
  end subroutine WriteOutVlocRloc
  
!!!--------------------------------------------------
  
  subroutine DisplayOutVlocRloc() bind(c, name='PLPLx_DisplayOutVlocRloc')
    implicit none
    !! PURPOSE
    !!  display local values (relative velocity, forces, local frame) of all
    !!  PLPLx contacts
    
    if( .not. check_PLPLx() ) return
    
    call write_xxx_Vloc_Rloc_PLPLx(6)
    
  end subroutine DisplayOutVlocRloc

!!!--------------------------------------------------

  subroutine DisplayProxTactors() bind(c, name='PLPLx_DisplayProxTactors')
    implicit none
    !! PURPOSE
    !!  display contacts
    
    if( .not. check_PLPLx() ) return
    
    call display_prox_tactors_PLPLx
    
  end subroutine DisplayProxTactors

!!!--------------------------------------------------
  
  subroutine ReadIniVlocRloc(step) bind(c, name='PLPLx_ReadIniVlocRloc')
    implicit none
    integer(c_int), intent(in), value :: step

     if( .not. check_PLPLx() ) return

     call read_ini_Vloc_Rloc_PLPLx(step)

  end subroutine

!!!--------------------------------------------------
  
  subroutine SetPeriodicCondition(rvalue1) bind(c, name='PLPLx_SetPeriodicCondition')
    implicit none
    real(c_double), intent(in), value :: rvalue1
    logical      :: periodic
    !! PURPOSE
    !!  initialise data for simulation using periodic condition
    
    if( .not. check_PLPLx() ) return
    
    PERIODIC=.true.
    call set_periodic_data_PLPLx(rvalue1,PERIODIC)
    
  end subroutine SetPeriodicCondition
  
!!!--------------------------------------------------
  subroutine SetFrictionModel(cvalue1_c) bind(c, name='PLPLx_SetFrictionModel')
    implicit none
    character(C_CHAR), dimension(3)   :: cvalue1_c
    character(len=3) :: cvalue1
    integer :: i
    
    cvalue1 = ''
    do i=1,3
       cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    if( .not. check_PLPLx() ) return
    
    call set_friction_model_PLPLx(cvalue1)
    
  end subroutine SetFrictionModel
  
!!!--------------------------------------------------

  subroutine SetBigPolygTolerance(rvalue1) bind(c, name='PLPLx_SetBigPolygTolerance')
    implicit none
    real(c_double), intent(in), value :: rvalue1
    
    call set_big_polyg_tolerance_PLPLx(rvalue1)

  end subroutine SetBigPolygTolerance

!!!--------------------------------------------------
!!!--------------------------------------------------
  
  subroutine ComputeStressPLPLx() bind(c, name='PLPLx_ComputeStress')
    implicit none
    
    call compute_stress_PLPLx()
    
  end subroutine ComputeStressPLPLx
  
!!!--------------------------------------------------
  
  subroutine ComputeBetaiPLPLx() bind(c, name='PLPLx_ComputeBetai')
    implicit none
    
    call compute_betai_PLPLx()
    
  end subroutine ComputeBetaiPLPLx
!!!----------------------------------
   subroutine ComputeCZMEnergyPLPLx() bind(c, name='PLPLx_ComputeCZMEnergy')
     implicit none

     call compute_czm_energy_PLPLx()
     
   end subroutine ComputeCZMEnergyPLPLx
!----------------------------------
   subroutine GetCZMEnergyPLPLx(icdan,energy) bind(c, name='PLPLx_GetCZMEnergy')
     implicit none
     integer(C_INT), intent(in), value :: icdan
     real(C_DOUBLE), dimension(4)      :: energy
     real(C_DOUBLE)                    :: stored,damage,failure,cohesion

     call get_CZM_energy_PLPLx(icdan,stored,damage,failure,cohesion)

     energy(1) = stored
     energy(2) = damage
     energy(3) = failure
     energy(4) = cohesion

   end subroutine GetCZMEnergyPLPLx
!!!--------------------------------------------------
!!!--------------------------------------------------
  
  subroutine CleanMemory() bind(c, name='PLPLx_CleanMemory')
    implicit none
    
    call clean_memory_PLPLx
    !call SelectProxTactors(reset=1)
    call SelectProxTactors(1)
    
  end subroutine CleanMemory
  
!!!--------------------------------------------------

  subroutine Use_Nc_Detection() bind(C, name='PLPLx_UseNcDetection')
    implicit none

    detection_method = 2

  end subroutine Use_Nc_Detection

!!!--------------------------------------------------

  subroutine ShrinkPolygFaces(shrink) bind(C, name='PLPLx_ShrinkPolygFaces')
    implicit none
    real(c_double), intent(in), value :: shrink

    ! rm: tested in set_shrink_polyg_faces
    !if (shrink < 0.d0 .or. shrink > 1.d0) then
    !  call logmes('shrink should be between 0. and 1.')
    !  call faterr(IAM,'incompatible value of shrink coefficient')
    !endif

    call set_shrink_polyg_faces_PLPLx(SHRINK)

  end subroutine ShrinkPolygFaces

!===========================================================================
end module wrap_PLPLx
