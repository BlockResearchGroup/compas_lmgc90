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
MODULE wrap_MP_SOLVER

  USE ISO_C_BINDING

  USE MP_SOLVER,ONLY:&
       read_in_mp_behaviour_mp_solver, &
       write_out_mp_behaviour_mp_solver, &
       read_ini_mp_values_mp_solver, &
       write_xxx_mp_values_mp_solver, &
       solve_electro1G, &
       solve_nl_electro1G, &
       solve_thermo_mp_solver, &
       get_write_mp_values, &
       update_conductivity_mp_solver, &
       update_thermo_mp_solver, &
       active_recup, &
       init_mp_solver, &
       init_thermal_conductivity_mp_solver, &
       get_branches_values, &
       set_heat_generation_factor, &
       set_heat_conduction_continue_factor

CONTAINS

!!!--------------------------------------------------------

  SUBROUTINE ReadMpBehaviour() bind(C, name = 'mp_solver_ReadMpBehaviour')
       IMPLICIT NONE

       CALL init_mp_solver
       CALL read_in_mp_behaviour_mp_solver

  END SUBROUTINE ReadMpBehaviour

  SUBROUTINE WriteMpBehaviour() bind(C, name = 'mp_solver_WriteMpBehaviour')
       IMPLICIT NONE

       CALL write_out_mp_behaviour_mp_solver

  END SUBROUTINE WriteMpBehaviour

  SUBROUTINE ReadIniMpValues(step) bind(C, name = 'mp_solver_ReadIniMpValues')
       IMPLICIT NONE
       integer(c_int), intent(in), value :: step

       CALL read_ini_mp_values_mp_solver(step)

  END SUBROUTINE ReadIniMpValues

  SUBROUTINE WriteOutMpValues() bind(C, name = 'mp_solver_WriteOutMpValues')
       IMPLICIT NONE

       logical write_mp

       write_mp = get_write_mp_values()
       IF (write_mp) THEN
          CALL write_xxx_mp_values_mp_solver(1)
          RETURN 
       END IF

  END SUBROUTINE WriteOutMpValues

  subroutine WriteLastMpValues() bind(c, name='mp_solver_WriteLastMpValues')
     !! PURPOSE
     !!  write ascii MP_VALUES.LAST file
     implicit none

     call write_xxx_mp_values_mp_solver(2)

  end subroutine

  SUBROUTINE SolveElectro1G() bind(C, name = 'mp_solver_SolveElectro1G')
       IMPLICIT NONE

       CALL solve_electro1G

  END SUBROUTINE SolveElectro1G

  SUBROUTINE SolveNlElectro1G() bind(C, name = 'mp_solver_SolveNlElectro1G')
       IMPLICIT NONE

       CALL solve_nl_electro1G

  END SUBROUTINE SolveNlElectro1G

  SUBROUTINE SolveThermoProblem() bind(C, name = 'mp_solver_SolveThermoProblem')
       IMPLICIT NONE

       CALL solve_thermo_mp_solver

  END SUBROUTINE SolveThermoProblem

  SUBROUTINE UpdateThermoProblem() bind(C, name = 'mp_solver_UpdateThermoProblem')
       IMPLICIT NONE

       CALL update_thermo_mp_solver

  END SUBROUTINE UpdateThermoProblem

  SUBROUTINE RecupTemperature() bind(C, name = 'mp_solver_RecupTemperature')
       IMPLICIT NONE

       CALL active_recup('T')

  END SUBROUTINE RecupTemperature

  SUBROUTINE RecupPotential() bind(C, name = 'mp_solver_RecupPotential')
       IMPLICIT NONE

       CALL active_recup('P')

  END SUBROUTINE RecupPotential

  SUBROUTINE UpdateConductivity() bind(C, name = 'mp_solver_UpdateConductivity')
       IMPLICIT NONE

       CALL update_conductivity_mp_solver

  END SUBROUTINE UpdateConductivity

  subroutine InitThermalConductivity() bind(C, name = 'mp_solver_InitThermalConductivity')
    implicit none
    
    call init_thermal_conductivity_mp_solver

  end subroutine InitThermalConductivity

  subroutine GetBrancheValues_mp_solver(cvalue1_c,ivalue1,rvect,ivalue2) bind(c, name='mp_solver_GetBrancheValues')
    implicit none
    character(c_char), dimension(5),intent(in) :: cvalue1_c
    integer(c_int),intent(in),value            :: ivalue1
    integer(c_int)                             :: ivalue2
    type(c_ptr)                                :: rvect

    real(c_double), dimension(:), pointer :: vector

    real(c_double)   :: Uik,Iik,Cik
    character(len=5) :: cvalue1
    integer          :: i

    ivalue2 = 4
    allocate(vector(ivalue2))

    cvalue1 = ''
    do i=1,5
       cvalue1 = cvalue1(1:i-1) // cvalue1_c(i)
    end do
    
    call get_branches_values(cvalue1,ivalue1,vector)

    rvect = c_loc(vector(1))

  end subroutine GetBrancheValues_mp_solver
!
  subroutine PutHeatGenerationFactor(convgen) bind(c, name='mp_solver_PutHeatGenerationFactor')
    implicit none
    real(c_double),intent(in),value :: convgen

    call set_heat_generation_factor(convgen)

  end subroutine PutHeatGenerationFactor

  subroutine PutHeatConductionContinueFactor(convcond) bind(c, name='mp_solver_PutHeatConductionContinueFactor')
    implicit none
    real(c_double),intent(in),value :: convcond
      
    call set_heat_conduction_continue_factor(convcond)

  end subroutine PutHeatConductionContinueFactor
!
END MODULE wrap_MP_SOLVER
