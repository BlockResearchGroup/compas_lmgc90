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

module LMGC90_MPI

integer(kind=4), public :: lmgc_mpi_world_comm = -1

logical         :: with_MPI = .false.
logical         :: DDM_SCHWARTZ = .false.
! file unit du process pour le debug
integer         :: slave_io=1000
integer         :: nb_procs_COMM_WORLD=1, rang_COMM_WORLD=0

!To compile with no MPI, not to be used !!
integer         :: code_MPI, MPI_DOUBLE_PRECISION, MPI_INTEGER, MPI_SUM, MPI_SUCCESS

!---------------------------------------------------------------------------------------------------
public                  &
   init_MPI,            &
   start_MPI_time,      &
   stop_MPI_time,       &
   mpi_finalize_process
!---------------------------------------------------------------------------------------------------

contains

!---------------------------------------------------------------------------------------------------
 subroutine init_MPI
    implicit none

 end subroutine init_MPI
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
 subroutine start_MPI_time
    implicit none

 end subroutine start_MPI_time
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
  subroutine stop_MPI_time
    implicit none
  
  end subroutine stop_MPI_time
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
  subroutine MPI_BARRIER(comm, code)
    implicit none
    integer(kind=4) :: comm, code
  
  end subroutine MPI_BARRIER
!---------------------------------------------------------------------------------------------------

! Procédure d'arrêt de MPI
!---------------------------------------------------------------------------------------------------
subroutine mpi_finalize_process
   implicit none

end subroutine mpi_finalize_process
!---------------------------------------------------------------------------------------------------

end module LMGC90_MPI
