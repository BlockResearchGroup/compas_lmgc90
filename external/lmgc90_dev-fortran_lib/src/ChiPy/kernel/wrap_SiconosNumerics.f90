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
MODULE wrap_SiconosNumerics

  USE ISO_C_BINDING

  USE SiconosNumerics,ONLY: &
       solve, &
       RnodHRloc, &
       update_tact_behav, &
       set_parameter, &
       prep, &
       Nullify_EntityList, &
       assume_is_initialized

CONTAINS

!!!--------------------------------------------------------

  SUBROUTINE SetParameters(name, tol, iter_error, iter_max, relax, verbose, output, freq_output)&
       bind(C, name = 'SiconosNumerics_SetParameters')
       IMPLICIT NONE

       character(c_char), dimension(30), intent(in) :: name
       real(c_double), intent(in),value :: tol, relax
       integer(c_int), intent(in),value :: iter_error, iter_max, verbose, output, freq_output

       character(len=30) :: solver_name
       integer :: i

       solver_name = ''
       do i=1,30
          if(name(i) == C_NULL_CHAR) exit
         solver_name = solver_name(1:i-1) // name(i)
       end do

       CALL set_parameter(solver_name,tol,iter_error,iter_max,relax,verbose,output,freq_output)

  END SUBROUTINE

  SUBROUTINE ExSolver() bind(C, name = 'SiconosNumerics_ExSolver')
       !! PURPOSE
       !!  solve fully the local contact problem

       IMPLICIT NONE
 
       CALL prep()
       CALL solve()
       CALL Nullify_EntityList()

       call RnodHRloc()


  END SUBROUTINE ExSolver

  SUBROUTINE IsInitialized() bind(C, name = 'SiconosNumerics_IsInitialized')
    IMPLICIT NONE
     
    CALL assume_is_initialized()

  END SUBROUTINE

  SUBROUTINE UpdateTactBehav() bind(C, name = 'SiconosNumerics_UpdateTactBehav')
       !! PURPOSE
       !!  update internal parameters of contact laws for each contact

       IMPLICIT NONE

       CALL update_tact_behav

  END SUBROUTINE UpdateTactBehav

END MODULE wrap_SiconosNumerics
