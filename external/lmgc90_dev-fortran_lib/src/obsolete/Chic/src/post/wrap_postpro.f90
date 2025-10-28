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
MODULE wrap_postpro

  !!****h* LMGC90.CHIC/POSTPRO
  !! NAME
  !!  module WRAP_POSTPRO
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/POSTPRO
  !!****

  USE CHIC
  USE POSTPRO,ONLY:&
       messages_for_users, &
       init_postpro_command, &
       start_postpro, &
       close_postpro_files, &
       postpro_during_computation, &
       circular_selection_postpro, &
       selection_translation_postpro

CONTAINS

!!!-----------------------------------------------------------------
  SUBROUTINE chic_command_postpro
    
    IMPLICIT NONE

    REAL(kind=8) :: X,Y,R

    IF (INDEX(CHZZZ,'POSTPRO DURING COMPUTATION')==1) THEN
       CALL LOGCHIC('POSTPRO')
       !!****e* POSTPRO/POSTPRO DURING COMPUTATION
       !! NAME 
       !!  POSTPRO DURING COMPUTATION
       !! PURPOSE
       !!  scan postprocessing function which should be call during the
       !!  computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO/postpro_during_computation
       !!****
       CALL postpro_during_computation
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'START POSTPRO')==1) THEN
       !!****e* POSTPRO/START POSTPRO
       !! NAME 
       !!  START POSTPRO
       !! PURPOSE
       !!  data initialization and scan postprocessing function
       !!  which should be call before the computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO/init_postpro_command
       !!  LMGC90.CORE/POSTPRO/start_postpro
       !!  LMGC90.CORE/POSTPRO/messages_for_users
       !!  LMGC90.CORE/POSTPRO/init_postpro_files
       !!  LMGC90.CORE/POSTPRO/postpro_before_computation
       !!****
       CALL LOGCHIC('POSTPRO')
       CALL init_postpro_command
       CALL start_postpro
       CALL messages_for_users
       IETZZZ = 1
       RETURN      
    END IF
    
    IF (INDEX(CHZZZ,'POSTPRO AFTER COMPUTATION')==1) THEN
       !!****e* POSTPRO/POSTPRO AFTER COMPUTATION
       !! NAME 
       !!  POSTPRO AFTER COMPUTATION
       !! PURPOSE
       !!  scan postprocessing function which should be call after the
       !!  computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO/postpro_after_computation
       !!  LMGC90.CORE/POSTPRO/close_postpro_files
       !!****
       CALL LOGCHIC('POSTPRO')
       CALL LOGCHIC('OBSOLETE KEYWORD')
       CALL LOGCHIC('USE - CLOSE POSTPRO FILES - INSTEAD')

       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CLOSE POSTPRO FILES')==1) THEN
       !!****e* POSTPRO/CLOSE POSTPRO FILES
       !! NAME 
       !!  CLOSE POSTPRO FILES
       !! PURPOSE
       !!  scan postprocessing function which should be call after the
       !!  computation process
       !! USES
       !!  LMGC90.CORE/POSTPRO/close_postpro_files
       !!****
       CALL LOGCHIC('POSTPRO')
       CALL close_postpro_files
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'CIRCULAR SELECTION')==1) THEN
       !!****e* POSTPRO/CIRCULAR SELECTION
       !! NAME 
       !!  CIRCULAR SELECTION
       !! SYNOPSIS
       !!  CIRCULAR SELECTION
       !!  [X]
       !!  [Y]
       !!  [R]
       !! INPUTS
       !!  [X] : X coordinate
       !!  [Y] : Y coordinate
       !!  [R] : radius selection
       !! PURPOSE
       !!  initialize data for postreatment using a circular selection
       !! USES
       !!  LMGC90.CORE/POSTPRO/circular_selection_postpro
       !!****
       CALL LOGCHIC('post2D')
       READ(CHLZZZ(NZZZ+1),*) X
       READ(CHLZZZ(NZZZ+2),*) Y
       READ(CHLZZZ(NZZZ+3),*) R
       CALL circular_selection_postpro(X,Y,R)
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SELECTION TRANSLATION')==1) THEN
       !!****e* POSTPRO/SELECTION TRANSLATION
       !! NAME 
       !!  SELECTION TRANSLATION
       !! SYNOPSIS
       !!  SELECTION TRANSLATION
       !!  [X]
       !!  [Y]
       !! INPUTS
       !!  [X] : X transalation increment
       !!  [Y] : Y transalation increment
       !! PURPOSE
       !!  increment the position of the circular selection define with CIRCULAR_SELECTION
       !! USES
       !!  LMGC90.CORE/POSTPRO/selection_translation_postpro
       !!****
       CALL LOGCHIC('post2D')
       READ(CHLZZZ(NZZZ+1),*) X
       READ(CHLZZZ(NZZZ+2),*) Y
       CALL selection_translation_postpro(X,Y)
       RETURN      
    END IF
    
  END SUBROUTINE chic_command_postpro
!!!-----------------------------------------------------------------

  include '../../more_src/post/wrap_postpro.inc'

END MODULE wrap_postpro
