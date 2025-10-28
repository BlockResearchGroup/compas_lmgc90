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

module wrap_global_thermal_solver

  use global_thermal_solver, only : initialize        , &
                                    prep_global_system, &
                                    assemble_lhs      , &
                                    assemble_rhs      , &
                                    apply_drvdofs     , &
                                    solve             , &
                                    clean_memory 
                                    

!> \todo Decide on a correct module name for wrapping

  implicit none

  private

contains

  subroutine gtsInitialize() bind(c, name='gts_Initialize')
    implicit none

    call initialize()

  end subroutine

  subroutine gtsAssembleSystem() bind(c, name='gts_AssembleSystem')
    implicit none

    call prep_global_system()
    call assemble_lhs()
    call apply_drvdofs()
    call assemble_rhs()
    call solve()

  end subroutine

  subroutine gtsPrepSystem() bind(c, name='gts_PrepSystem')
    implicit none

    call prep_global_system()

  end subroutine
 
  subroutine gtsAssembleLHS() bind(c, name='gts_AssembleLHS')
    implicit none

    call assemble_lhs()
    call apply_drvdofs()

  end subroutine

  subroutine gtsAssembleRHS() bind(c, name='gts_AssembleRHS')
    implicit none

    call assemble_rhs()

  end subroutine

  subroutine gtsSolve() bind(c, name='gts_Solve')
    implicit none

    call solve()

  end subroutine
 
  subroutine gtsFinalize() bind(c, name='gts_Finalize')
    implicit none

    call clean_memory()

  end subroutine

end module
