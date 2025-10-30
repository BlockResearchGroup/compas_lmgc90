!===========================================================================
!
! Copyright 2000-2004 CNRS.
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
! Frederic Dubois or Michel Jean.
!
! dubois@lmgc.univ-montp2.fr
! mjean@mn.esm2.imt-mrs.fr
!
!===========================================================================
MODULE wrap_postpro_3D

  !!****h* LMGC90.CHIPY/POSTPRO_3D
  !! NAME
  !!  module wrap_postpro_3D
  !! USES
  !!  LMGC90.CHIPY/CHIPY
  !!  LMGC90.CORE/POSTPRO_3D
  !!****

  USE POSTPRO_3D,ONLY:&
        messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &
       postpro_during_computation

  USE utilities
  IMPLICIT NONE
  PUBLIC



CONTAINS

!!!------------------------------------------------
  SUBROUTINE PostproDuringComputation
    IMPLICIT NONE

       !!****e* POSTPRO_3D/PostproDuringComputation
       !! NAME 
       !!  PostproDuringComputation
       !! PURPOSE
       !!  scan postprocessing function which should be call during the
       !!  computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO_3D/postpro_during_computation
       !!****
       CALL postpro_during_computation

  END SUBROUTINE
    
  SUBROUTINE PostproBeforeComputation
    IMPLICIT NONE
       !!****e* POSTPRO_3D/PostproBeforeComputation
       !! NAME 
       !!  StartPostpro
       !! PURPOSE
       !!  data initialization and scan postprocessing function
       !!  which should be call before the computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO_3D/init_postpro_command
       !!  LMGC90.CORE/POSTPRO_3D/start_postpro
       !!  LMGC90.CORE/POSTPRO_3D/messages_for_users
       !!****

       CALL init_postpro_command
       CALL start_postpro
       CALL messages_for_users

  END SUBROUTINE

!!!-----------------------------------------------------------------    
    SUBROUTINE ClosePostproFiles
       IMPLICIT NONE
       !!****e* POSTPRO/ClosePostproFiles
       !! NAME 
       !!  ClosePostproFiles
       !! PURPOSE
       !!  scan postprocessing function which should be call after the
       !!  computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO/close_postpro_files
       !!****

       CALL close_postpro_files

    END SUBROUTINE

!!!------------------------------------------------------------------

END MODULE wrap_postpro_3D
