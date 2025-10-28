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
MODULE wrap_POLYR

  !!****h* LMGC90.CHIC/POLYR
  !! NAME
  !!  module wrap_POLYR
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/POLYR
  !!****

  USE CHIC

  USE POLYR,ONLY: &
       read_bodies_POLYR, &
       MOVE_POLYR, &
       save_vertex_POLYR, &
       set_radius_correction_POLYR

CONTAINS

!!!------------------------------------------------------------------------
  SUBROUTINE chic_command_POLYR

    IMPLICIT NONE

    REAL(kind=8) :: corr,ratio=1.d0
    
    IF (INDEX(CHZZZ,'READ BODIES')==1) THEN
       !!****e* POLYR/READ BODIES
       !! NAME
       !!   READ BODIES
       !! PURPOSE
       !!  read BODIES.DAT file
       !!  Initializes existing_entities variable for POLYR contactors
       !! USES
       !!  LMGC90.CORE/POLYR/read_bodies_POLYR
       !!****
       CALL LOGCHIC('POLYR')
       CALL read_bodies_POLYR
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'INCREMENT STEP')==1) THEN
       !!****e* POLYR/INCREMENT STEP
       !! NAME
       !!  INCREMENT STEP
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/POLYR/MOVE_POLYR
       !!****
       CALL LOGCHIC('POLYR')
       CALL MOVE_POLYR
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'SAVE VERTEX POLYR')==1) THEN
       !!****e* POLYR/SAVE VERTEX POLYR
       !! NAME
       !!  SAVE VERTEX POLYR
       !! PURPOSE
       !!  
       !! USES
       !!  LMGC90.CORE/POLYR/save_vertex_POLYR
       !!****
       CALL LOGCHIC('POLYR')
       CALL save_vertex_POLYR
       IETZZZ = 1
       RETURN      
    END IF

    IF (INDEX(CHZZZ,'MODIFY RADIUS POLYR')==1) THEN
       !!****e* POLYR/MODIFY RADIUS POLYR
       !! NAME
       !!  MODIFY RADIUS POLYR
       !! SYNOPSIS
       !!  MODIFY RADIUS POLYR
       !!  [ratio]
       !! INPUTS
       !!  [ratio] ratio factor (default = 1.d0)
       !! PURPOSE
       !!  apply a amplificatio/reduction size factor 
       !! USES
       !!  LMGC90.CORE/POLYR/set_radius_correction_POLYR
       !!****
       CALL LOGCHIC('POLYR')
       READ(CHLZZZ(NZZZ+1),*) ratio
       CALL set_radius_correction_POLYR(ratio)
       IETZZZ = 1
       RETURN      
    END IF

  END SUBROUTINE chic_command_POLYR
!!!------------------------------------------------------------------------

END MODULE wrap_POLYR
