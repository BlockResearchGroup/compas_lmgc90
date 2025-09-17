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
MODULE wrap_ALpxx

  !!****h* LMGC90.CHIC/ALpxx
  !! NAME
  !!  module wrap_ALpxx
  !! USES
  !!  LMGC90.CHIC/CHIC
  !!  LMGC90.CORE/ALpxx
  !!****

  USE CHIC

  USE ALPxx,ONLY: read_bodies_ALPxx, &
                  set_precon_node_ALpxx

CONTAINS
!!!----------------------------------------------------
  SUBROUTINE chic_command_ALPxx

    IMPLICIT NONE
    
    IF (INDEX(CHZZZ,'LOAD MODELS')==1) THEN
       !!****e* ALpxx/LOAD MODELS
       !! NAME
       !!   READ MODELS
       !! PURPOSE
       !!  read MODELS.DAT file
       !!  Initializes existing_entities variable for ALpxx contactors
       !! USES
       !!  LMGC90.CORE/ALpxx/read_bodies_ALpxx
       !!****
       CALL LOGCHIC('ALPxx')
       CALL read_bodies_ALPxx
       IETZZZ = 1
       RETURN      
    END IF

    if (INDEX(CHZZZ,'PUSH PRECONDENSATION NODES')==1) then
      call LOGCHIC('ALpxx')
      call set_precon_node_ALpxx
      IETZZZ = 1
      return      
    end if



  END SUBROUTINE chic_command_ALPxx
!!!----------------------------------------------------

  include '../../more_src/mailx/wrap_ALpxx.inc'

END MODULE wrap_ALPxx
