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
MODULE wrap_postpro

  USE ISO_C_BINDING

  use utilities, only : logmes

  USE POSTPRO,ONLY:&
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &  
       postpro_during_computation, &
       flush_during_computation  , &
       circular_selection_postpro, &
       selection_translation_postpro, &
       clean_memory_postpro, &
       compute_kinetic_energy

  PUBLIC 

  integer, private :: iunit_postpro_dat = 0

CONTAINS

    subroutine PostproReadCommands() bind(c, name='postpro_ReadCommands')
      implicit none

      iunit_postpro_dat = init_postpro_command()

    end subroutine

    SUBROUTINE PostproDuringComputation() bind(c, name='postpro_PostproDuringComputation')
       IMPLICIT NONE
       !! PURPOSE
       !!  scan postprocessing function which should be call during the
       !!  computation process

       if( iunit_postpro_dat /= 0 ) then
         call postpro_during_computation
       else
         call logmes("[WARNING][wrap_postpro_2d::PostproDuringComputation] module not initialized", .true.)
       end if

    END SUBROUTINE

    subroutine FlushDuringComputation() bind(c, name='postpro_FlushDuringComputation')
       implicit none
       !! PURPOSE
       !!  force flush of postpro file writing

       if( iunit_postpro_dat /= 0 ) then
         call flush_during_computation
       else
         call logmes("[WARNING][wrap_postpro_2d::FlushDuringComputation] module not initialized", .true.)
       end if

    end subroutine

!!!-----------------------------------------------------------------
!!!-----------------------------------------------------------------    
    subroutine PostproBeforeComputation(restart) bind(c, name='postpro_PostproBeforeComputation')
       implicit none
       integer(kind=c_int), intent(in), value :: restart
       !! PURPOSE
       !!  data initialization and scan postprocessing function
       !!  which should be call before the computation process

       if( iunit_postpro_dat == 0 ) then
         iunit_postpro_dat = init_postpro_command()
       end if
       call start_postpro( iunit_postpro_dat, restart )
       call messages_for_users

    end subroutine
!!!-----------------------------------------------------------------    
    SUBROUTINE ClosePostproFiles() bind(c, name='postpro_ClosePostproFiles')
       IMPLICIT NONE
       !! PURPOSE
       !!  scan postprocessing function which should be call after the
       !!  computation process

       if( iunit_postpro_dat /= 0 ) then
         call close_postpro_files
       else
         call logmes("[WARNING][wrap_postpro_2d::ClosePostproFiles] module not initialized", .true.)
       end if

    END SUBROUTINE
!!!-----------------------------------------------------------------
    SUBROUTINE SetCircularSelectionZone(rvalue1,rvalue2,rvalue3) bind(c, name='postpro_SetCircularSelectionZone')
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2,rvalue3
       !! PURPOSE
       !!  initialize data for postreatment using a circular selection

       CALL circular_selection_postpro(rvalue1,rvalue2,rvalue3)

    END SUBROUTINE
!!!-----------------------------------------------------------------
    SUBROUTINE MoveCircularSelectionZone(rvalue1,rvalue2) bind(c, name='postpro_MoveCircularSelectionZone')
       IMPLICIT NONE
       REAL(C_DOUBLE), INTENT(IN), VALUE :: rvalue1,rvalue2
       !! PURPOSE
       !!  increment the position of the circular selection define with CIRCULAR_SELECTION

       CALL selection_translation_postpro(rvalue1,rvalue2)

    END SUBROUTINE
    
    subroutine CleanMemory() bind(c, name='postpro_CleanMemory')
      implicit none

      call clean_memory_postpro
      iunit_postpro_dat = 0

    end subroutine


!!!-----------------------------------------------------------------
    FUNCTION GetKineticEnergy() bind(c, name='postpro_2D_GetKineticEnergy')
       IMPLICIT NONE
       REAL(C_double) :: GetKineticEnergy

       ! calcul de l'energie cinetique des corps rigides 2D et defo
       GetKineticEnergy =  compute_kinetic_energy()


    END FUNCTION


END MODULE wrap_postpro
