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

use MPI

!---------------------------------------------------------------------------------------------------
logical         :: with_MPI = .true.
logical         :: DDM_SCHWARTZ = .false.
logical         :: init = .false.

integer         :: nb_procs_COMM_WORLD,rang_COMM_WORLD,code_MPI
integer         :: COMM_MYSELF,nb_procs_myself,rang_myself
! file unit du process pour debug
integer         :: slave_io
real(kind=8)    :: secondsTT,MPI_MAX_time
!---------------------------------------------------------------------------------------------------
integer(kind=4), public :: lmgc_mpi_world_comm = -1

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

    ! code_MPI :: code retour des fonctions MPI (pour gerer les erreurs)
    call MPI_Initialized(init,code_MPI)
    if( .not. init ) then
       call MPI_INIT(code_MPI)
    end if

    call MPI_COMM_SIZE(MPI_COMM_WORLD,nb_procs_COMM_WORLD,code_MPI)
    call MPI_COMM_RANK(MPI_COMM_WORLD,rang_COMM_WORLD,code_MPI)

    ! Pour les sorties (debuggage essentiellement) type "un fichier par processus"
    slave_io=1000+rang_COMM_WORLD

    ! Pour MUMPS
    lmgc_mpi_world_comm = MPI_COMM_WORLD

    ! Creation d'un communicateur, pour chaque processus, derive de COMM_WORLD, tel 
    ! couleur = rang_COMM_WORLD
    ! clef = 0 car un seul processus par communicateur 
    call MPI_COMM_SPLIT(MPI_COMM_WORLD,rang_COMM_WORLD,0,COMM_MYSELF,code_MPI)
    call MPI_COMM_SIZE(COMM_MYSELF,nb_procs_myself,code_MPI)
    call MPI_COMM_RANK(COMM_MYSELF,rang_myself,code_MPI)

 end subroutine init_MPI
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
 subroutine start_MPI_time

    implicit none

    secondsTT = MPI_Wtime ( )

 end subroutine start_MPI_time
!---------------------------------------------------------------------------------------------------

!---------------------------------------------------------------------------------------------------
  subroutine stop_MPI_time
  
    implicit none

    secondsTT = MPI_Wtime ( ) - secondsTT
  
    call MPI_REDUCE(secondsTT,MPI_MAX_time,1,MPI_DOUBLE_PRECISION, &
                    MPI_MAX,0,MPI_COMM_WORLD,code_MPI)
  
    if (rang_COMM_WORLD==0) then
       print *, "Elapsed MPI time=", MPI_MAX_TIME
    end if
  
  end subroutine stop_MPI_time
!---------------------------------------------------------------------------------------------------

! Procédure d'arrêt de MPI
!---------------------------------------------------------------------------------------------------
subroutine mpi_finalize_process

   implicit none

   ! Désactivation de l'environnement MPI
   if( init ) then
     call MPI_FINALIZE(code_MPI)
   end if

end subroutine mpi_finalize_process
!---------------------------------------------------------------------------------------------------

end module LMGC90_MPI
